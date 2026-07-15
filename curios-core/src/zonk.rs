#[cfg(test)]
mod tests;

use {
    super::{
        Apply, Bound, Carrier, Cases, Context, Definition, Error, Func, FuncType, Inductive,
        InductiveParam, InductiveType, Item, Let, Match, Metavar, MetavarId, MetavarOrigin, Module,
        MotivePattern, MotiveSlot, Nat, Prim, Proj, Rec, Struct, StructType, Structure, Subterm,
        Term, Tuple, TupleType, Variant,
    },
    std::sync::Arc,
};

/// Substitute every solved metavariable in `term` by its (recursively zonked)
/// solution, yielding a meta-free term (§9). An unsolved metavariable is a
/// residual hole the elaborator never pinned down — reported as `cannot_infer`
/// at its occurrence span. The result flows downstream to `erase`, which is then
/// guaranteed never to meet a `Subterm::Metavar`.
///
/// Substitution replaces a metavariable node by its solution. A solution is
/// spelled with the birth telescope's names and is *not* in general a closed
/// term; the occurrence's spine (its delayed substitution) records what each
/// birth binder corresponds to at the splice site, so `zonk_term` resolves
/// by rewriting the solution through it. Every solved occurrence carries its
/// spine — `elaborate_apply` opens telescopes with rebuilt arguments, so no
/// bare copy of a birthed hole survives to be spliced.
pub(crate) fn zonk(context: &Context, term: &Term) -> Result<Term, Error> {
    zonk_term(context, term)
}

/// Zonk a whole [`Module`]: substitute metavariable solutions throughout every
/// top-level item plus the entrypoint body and annotation, yielding a meta-free
/// module for `erase` (§9).
pub fn zonk_module(context: &Context, module: &Module) -> Result<Module, Error> {
    let items = module
        .items
        .iter()
        .map(|item| zonk_item(context, item))
        .collect::<Result<Vec<_>, Error>>()?;

    let body = zonk_term(context, &module.body)?;

    let type_ = module
        .type_
        .as_ref()
        .map(|type_| zonk_term(context, type_))
        .transpose()?;

    // The registry's telescopes flow into `erase`, which runs meta-free with
    // its own (solution-less) context — so they are zonked like everything else.
    let inductives = module
        .inductives
        .iter()
        .map(|(name, inductive)| {
            Ok((
                name.clone(),
                Inductive {
                    params: inductive.params.zonk(context)?,
                    indices: inductive.indices.zonk(context)?,
                    constructors: inductive
                        .constructors
                        .iter()
                        .map(|(tag, param)| {
                            Ok((
                                tag.clone(),
                                InductiveParam {
                                    telescope: param.telescope.zonk(context)?,
                                },
                            ))
                        })
                        .collect::<Result<_, Error>>()?,
                    result_sort: inductive.result_sort.clone(),
                    root: inductive.root,
                },
            ))
        })
        .collect::<Result<_, Error>>()?;

    // Struct field telescopes flow into `erase` the same way — zonk them too.
    let structures = module
        .structures
        .iter()
        .map(|(name, structure)| {
            Ok((
                name.clone(),
                Structure {
                    params: structure.params.zonk(context)?,
                    fields: structure.fields.zonk(context)?,
                    result_sort: structure.result_sort.clone(),
                    module: structure.module.clone(),
                    root: structure.root,
                    rep_public: structure.rep_public,
                },
            ))
        })
        .collect::<Result<_, Error>>()?;

    Ok(Module {
        items,
        inductives,
        structures,
        // Concept metadata and witness markers carry no terms of their own
        // (each concept's telescopes live in `structures`, zonked above).
        concepts: module.concepts.clone(),
        witnesses: module.witnesses.clone(),
        type_,
        body,
    })
}

fn zonk_item(context: &Context, item: &Item) -> Result<Item, Error> {
    match item {
        Item::Let(def) => Ok(Item::Let(zonk_definition(context, def)?)),
        Item::Rec(defs) => Ok(Item::Rec(
            defs.iter()
                .map(|def| zonk_definition(context, def))
                .collect::<Result<Vec<_>, Error>>()?,
        )),
    }
}

fn zonk_definition(context: &Context, def: &Definition) -> Result<Definition, Error> {
    Ok(Definition {
        name: def.name.clone(),
        island: def.island.clone(),
        root: def.root,
        type_: zonk_term(context, &def.type_)?,
        body: zonk_term(context, &def.body)?,
    })
}

/// The report a written goal `?` errors out with: the local scope frozen at
/// its birth, the goal's type, and the solution elaboration committed (if
/// any) — each zonked for *display*, keeping the raw spelling where unsolved
/// holes survive (the same tolerance the no-witness report uses).
fn goal_report(context: &Context, id: MetavarId) -> Error {
    let display = |term: &Term| zonk_term(context, term).unwrap_or_else(|_| term.clone());
    match context.metavar_entry(id) {
        Some(entry) => Error::goal(
            entry
                .telescope
                .iter()
                .map(|(name, type_)| (Term::free_var(name), display(type_)))
                .collect(),
            display(&entry.result),
            context.metavar_solution(id).map(display),
        ),
        // A goal elaboration never reached was never birthed, so there is no
        // scope or type to report — unreachable in practice, since every kept
        // item elaborates.
        None => Error::CannotInfer,
    }
}

pub(crate) fn zonk_term(context: &Context, term: &Term) -> Result<Term, Error> {
    // A metavariable node *is* the substitution site: replace it by its solution,
    // recursively zonked (the solution may itself mention solved metavariables).
    if let Subterm::Metavar(Metavar { id, spine, origin }) = &**term {
        // A written goal `?` never splices — the whole point of writing it was
        // the report. Solved or not, error with what elaboration determined:
        // the frozen scope, the goal's type, and the solution when one landed.
        if matches!(origin, Some(MetavarOrigin::Goal)) {
            return Err(goal_report(context, *id).at_opt(term.span()));
        }

        let solution = context.metavar_solution(*id).ok_or_else(|| {
            // An unsolved metavariable the *elaborator* minted (an omitted
            // implicit or witness argument) is reported by the binder it
            // filled — the provenance rides on the node itself — not as a
            // bare hole: the user never wrote this metavariable, so a generic
            // "cannot infer" would point at nothing they can see.
            let error = match origin {
                Some(MetavarOrigin::Implicit(origin)) => {
                    Error::uninferred_implicit(origin.func.clone(), origin.binder.clone())
                }
                Some(MetavarOrigin::Witness(origin)) => {
                    // The birth record's `result` is the goal type; display it
                    // through whatever solutions landed, keeping the raw
                    // spelling if holes survive.
                    let goal = context
                        .metavar_entry(*id)
                        .map(|entry| entry.result.clone())
                        .unwrap_or_else(Term::type_);
                    let goal = zonk_term(context, &goal).unwrap_or(goal);
                    Error::no_witness(goal, origin.func.clone(), origin.binder.clone())
                }
                None => Error::CannotInfer,
                // Handled by the unconditional report above.
                Some(MetavarOrigin::Goal) => unreachable!("a goal never reaches the splice path"),
            };
            match term.span() {
                Some(span) => error.at(span),
                None => error,
            }
        })?;

        // Resolve the solution in its own (named) frame first: nested solved
        // metavariables are substituted before the spine splice below.
        let resolved = zonk_term(context, solution)?;

        // A contextual occurrence: the spine records, in this site's own
        // (already de-Bruijn-correct) form, what each birth binder
        // corresponds to here. Zonk the spine entries (they may embed
        // solved metavariables), then splice the solution through them —
        // birth names captured, spine terms released.
        let entry = context
            .metavar_entry(*id)
            .expect("a solved metavariable has a birth entry");
        assert_eq!(
            spine.len(),
            entry.telescope.len(),
            "a solved metavariable's occurrence carries its full spine"
        );
        let labels = entry
            .telescope
            .iter()
            .map(|(name, _)| name.as_str())
            .collect::<Vec<_>>();
        let spine = zonk_terms(context, spine)?;
        let refs = spine.iter().collect::<Vec<_>>();
        let zonked = resolved.capture(&labels).release(&refs);

        // Carry the hole's span only if the solution carries none of its own.
        return Ok(match term.span() {
            Some(span) => zonked.with_span(span),
            None => zonked,
        });
    }

    // Fast path: a subtree with no metavariables is already meta-free. Clone it
    // (cheap — `Term` is `Rc`-backed) and keep its span untouched.
    if term.metavars().is_empty() {
        return Ok(term.clone());
    }

    let inner = zonk_subterm(context, term)?;

    Ok(match term.span() {
        Some(span) => Term::spanned(span, inner),
        None => Term::from(inner),
    })
}

fn zonk_terms(context: &Context, terms: &[Term]) -> Result<Vec<Term>, Error> {
    terms.iter().map(|t| zonk_term(context, t)).collect()
}

fn zonk_subterm(context: &Context, term: &Term) -> Result<Subterm, Error> {
    Ok(match &**term {
        Subterm::Type => Subterm::Type,
        Subterm::Prop => Subterm::Prop,
        Subterm::Var(var) => Subterm::Var(var.clone()),

        // `Infix`/`NumLit` are elaboration-transient: `elaborate` replaces every
        // occurrence with a concrete `Prim` before zonk ever runs.
        Subterm::Infix(_) => unreachable!("infix node survived elaboration into zonk"),
        Subterm::NumLit(_) => unreachable!("numeric-literal node survived elaboration into zonk"),

        Subterm::Prim(prim) => Subterm::Prim(zonk_prim(context, prim)?),

        Subterm::Func(Func { telescope }) => Subterm::Func(Func {
            telescope: telescope.zonk(context)?,
        }),

        Subterm::FuncType(FuncType {
            telescope,
            plicities,
        }) => Subterm::FuncType(FuncType {
            telescope: telescope.zonk(context)?,
            plicities: plicities.clone(),
        }),

        Subterm::Apply(Apply {
            head,
            params,
            plicities,
        }) => Subterm::Apply(Apply {
            head: zonk_term(context, head)?,
            params: zonk_terms(context, params)?,
            plicities: plicities.clone(),
        }),

        Subterm::TupleType(TupleType { telescope }) => Subterm::TupleType(TupleType {
            telescope: telescope.zonk(context)?,
        }),

        Subterm::Tuple(Tuple { fields, names }) => Subterm::Tuple(Tuple {
            fields: zonk_terms(context, fields)?,
            names: names.clone(),
        }),

        Subterm::Proj(Proj { head, field }) => Subterm::Proj(Proj {
            head: zonk_term(context, head)?,
            field: field.clone(),
        }),

        Subterm::InductiveType(InductiveType {
            name,
            params,
            indices,
        }) => Subterm::InductiveType(InductiveType {
            name: name.clone(),
            params: zonk_terms(context, params)?,
            indices: zonk_terms(context, indices)?,
        }),

        Subterm::Variant(Variant {
            name,
            params,
            tag,
            payload,
        }) => Subterm::Variant(Variant {
            name: name.clone(),
            params: zonk_terms(context, params)?,
            tag: tag.clone(),
            payload: zonk_terms(context, payload)?,
        }),

        Subterm::StructType(StructType { name, params }) => Subterm::StructType(StructType {
            name: name.clone(),
            params: zonk_terms(context, params)?,
        }),

        Subterm::Struct(Struct {
            name,
            params,
            fields,
            entries,
        }) => Subterm::Struct(Struct {
            name: name.clone(),
            params: zonk_terms(context, params)?,
            fields: zonk_terms(context, fields)?,
            entries: entries.clone(),
        }),

        Subterm::Match(Match {
            head,
            motive,
            cases,
        }) => Subterm::Match(Match {
            head: zonk_term(context, head)?,
            motive: motive.map_body(|b| zonk_term(context, b))?,
            cases: match cases {
                Cases::Bln {
                    false_case,
                    true_case,
                } => Cases::Bln {
                    false_case: zonk_term(context, false_case)?,
                    true_case: zonk_term(context, true_case)?,
                },
                Cases::FreeMonoid { carrier } => Cases::FreeMonoid {
                    carrier: match carrier {
                        Carrier::Nat {
                            empty_case,
                            cons_case,
                        } => Carrier::Nat {
                            empty_case: zonk_term(context, empty_case)?,
                            cons_case: cons_case.map_body(|b| zonk_term(context, b))?,
                        },
                        Carrier::Bin {
                            grain,
                            empty_case,
                            cons_case,
                        } => Carrier::Bin {
                            grain: *grain,
                            empty_case: zonk_term(context, empty_case)?,
                            cons_case: cons_case.map_body(|b| zonk_term(context, b))?,
                        },
                        Carrier::Lst {
                            elem,
                            empty_case,
                            cons_case,
                        } => Carrier::Lst {
                            elem: zonk_term(context, elem)?,
                            empty_case: zonk_term(context, empty_case)?,
                            cons_case: cons_case.map_body(|b| zonk_term(context, b))?,
                        },
                    },
                },
                Cases::Switch { cases, default } => Cases::Switch {
                    cases: cases
                        .iter()
                        .map(|(n, body)| Ok((*n, zonk_term(context, body)?)))
                        .collect::<Result<_, Error>>()?,
                    default: zonk_term(context, default)?,
                },
                Cases::Inductive {
                    cases,
                    pattern,
                    default,
                } => Cases::Inductive {
                    cases: cases
                        .iter()
                        .map(|(atom, scope)| {
                            Ok((atom.clone(), scope.map_body(|b| zonk_term(context, b))?))
                        })
                        .collect::<Result<_, Error>>()?,
                    default: default
                        .as_ref()
                        .map(|d| zonk_term(context, d))
                        .transpose()?,
                    pattern: pattern
                        .as_ref()
                        .map(|p| {
                            Ok(MotivePattern {
                                name: p.name.clone(),
                                slots: p
                                    .slots
                                    .iter()
                                    .map(|slot| {
                                        Ok(match slot {
                                            MotiveSlot::Binder => MotiveSlot::Binder,
                                            MotiveSlot::Term(t) => {
                                                MotiveSlot::Term(zonk_term(context, t)?)
                                            }
                                        })
                                    })
                                    .collect::<Result<_, Error>>()?,
                            })
                        })
                        .transpose()?,
                },
            },
        }),

        Subterm::Let(Let { bindings, tail }) => Subterm::Let(Let {
            bindings: bindings
                .iter()
                .map(|(type_, value)| Ok((zonk_term(context, type_)?, zonk_term(context, value)?)))
                .collect::<Result<_, Error>>()?,
            tail: tail.map_body(|b| zonk_term(context, b))?,
        }),

        Subterm::Rec(Rec { id, items, tail }) => Subterm::Rec(Rec {
            id: *id,
            items: items
                .iter()
                .map(|(type_, body)| {
                    Ok((
                        type_.map_body(|t| zonk_term(context, t))?,
                        body.map_body(|b| zonk_term(context, b))?,
                    ))
                })
                .collect::<Result<_, Error>>()?,
            tail: tail.map_body(|b| zonk_term(context, b))?,
        }),

        // Handled in `zonk_term` before dispatch.
        Subterm::Metavar(_) => unreachable!("metavariable handled by `zonk_term`"),
    })
}

/// Zonk a primitive's term operands. Mirrors `traverse_prim`'s rebuild, but
/// fallibly substitutes metavariable solutions rather than de Bruijn shifting.
fn zonk_prim(context: &Context, prim: &Prim) -> Result<Prim, Error> {
    Ok(match prim {
        Prim::BlnType
        | Prim::Bln(_)
        | Prim::NatType
        | Prim::Nat(Nat::Zero)
        | Prim::ByteType
        | Prim::Byte(_)
        | Prim::IntType
        | Prim::Int(_)
        | Prim::FltType
        | Prim::Flt(_)
        | Prim::BinType(curios_base::Grain::X)
        | Prim::Bin(curios_base::Grain::X, _)
        | Prim::BinType(curios_base::Grain::B)
        | Prim::Bin(curios_base::Grain::B, _)
        | Prim::IoType
        | Prim::Io(_) => prim.clone(),

        Prim::Nat(Nat::Succ(spine, inner)) => {
            Prim::Nat(Nat::Succ(spine.clone(), zonk_term(context, inner)?))
        }

        Prim::NatEql(a, b) => Prim::NatEql(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IoEql(a, b) => Prim::IoEql(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatNeq(a, b) => Prim::NatNeq(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatAdd(a, b) => Prim::NatAdd(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatSub(a, b) => Prim::NatSub(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatMul(a, b) => Prim::NatMul(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatLt(a, b) => Prim::NatLt(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatDiv(a, b) => Prim::NatDiv(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatRem(a, b) => Prim::NatRem(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatGt(a, b) => Prim::NatGt(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatLte(a, b) => Prim::NatLte(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatGte(a, b) => Prim::NatGte(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatAnd(a, b) => Prim::NatAnd(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatOr(a, b) => Prim::NatOr(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatXor(a, b) => Prim::NatXor(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatShl(a, b) => Prim::NatShl(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatShr(a, b) => Prim::NatShr(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::ByteToNat(t) => Prim::ByteToNat(zonk_term(context, t)?),
        Prim::NatToByte(t) => Prim::NatToByte(zonk_term(context, t)?),
        Prim::ByteEql(a, b) => Prim::ByteEql(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::ByteLt(a, b) => Prim::ByteLt(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::ByteLte(a, b) => Prim::ByteLte(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::ByteGt(a, b) => Prim::ByteGt(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::ByteGte(a, b) => Prim::ByteGte(zonk_term(context, a)?, zonk_term(context, b)?),

        Prim::BlnAnd(a, b) => Prim::BlnAnd(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::BlnOr(a, b) => Prim::BlnOr(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::BlnXor(a, b) => Prim::BlnXor(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::BlnEql(a, b) => Prim::BlnEql(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::BlnNeq(a, b) => Prim::BlnNeq(zonk_term(context, a)?, zonk_term(context, b)?),

        Prim::IntEql(a, b) => Prim::IntEql(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntNeq(a, b) => Prim::IntNeq(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntAdd(a, b) => Prim::IntAdd(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntSub(a, b) => Prim::IntSub(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntMul(a, b) => Prim::IntMul(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntDiv(a, b) => Prim::IntDiv(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntRem(a, b) => Prim::IntRem(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntLt(a, b) => Prim::IntLt(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntGt(a, b) => Prim::IntGt(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntLte(a, b) => Prim::IntLte(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntGte(a, b) => Prim::IntGte(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntAnd(a, b) => Prim::IntAnd(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntOr(a, b) => Prim::IntOr(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntXor(a, b) => Prim::IntXor(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntShl(a, b) => Prim::IntShl(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntShr(a, b) => Prim::IntShr(zonk_term(context, a)?, zonk_term(context, b)?),

        Prim::FltAdd(a, b) => Prim::FltAdd(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltSub(a, b) => Prim::FltSub(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltMul(a, b) => Prim::FltMul(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltDiv(a, b) => Prim::FltDiv(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltRem(a, b) => Prim::FltRem(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltEql(a, b) => Prim::FltEql(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltNeq(a, b) => Prim::FltNeq(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltLt(a, b) => Prim::FltLt(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltGt(a, b) => Prim::FltGt(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltLte(a, b) => Prim::FltLte(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltGte(a, b) => Prim::FltGte(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltMin(a, b) => Prim::FltMin(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltMax(a, b) => Prim::FltMax(zonk_term(context, a)?, zonk_term(context, b)?),

        Prim::FltNeg(t) => Prim::FltNeg(zonk_term(context, t)?),
        Prim::FltAbs(t) => Prim::FltAbs(zonk_term(context, t)?),
        Prim::FltSqrt(t) => Prim::FltSqrt(zonk_term(context, t)?),
        Prim::FltFloor(t) => Prim::FltFloor(zonk_term(context, t)?),
        Prim::FltCeil(t) => Prim::FltCeil(zonk_term(context, t)?),
        Prim::FltTrunc(t) => Prim::FltTrunc(zonk_term(context, t)?),
        Prim::FltNearest(t) => Prim::FltNearest(zonk_term(context, t)?),

        Prim::FltToLeBytes(t) => Prim::FltToLeBytes(zonk_term(context, t)?),
        Prim::FltOfLeBytes(t) => Prim::FltOfLeBytes(zonk_term(context, t)?),
        Prim::NatToInt(t) => Prim::NatToInt(zonk_term(context, t)?),
        Prim::NatToFlt(t) => Prim::NatToFlt(zonk_term(context, t)?),
        Prim::IntToNat(t) => Prim::IntToNat(zonk_term(context, t)?),
        Prim::IntToFlt(t) => Prim::IntToFlt(zonk_term(context, t)?),
        Prim::FltToNat(t) => Prim::FltToNat(zonk_term(context, t)?),
        Prim::FltToInt(t) => Prim::FltToInt(zonk_term(context, t)?),

        Prim::BinLen(curios_base::Grain::X, t) => {
            Prim::BinLen(curios_base::Grain::X, zonk_term(context, t)?)
        }
        Prim::BinEql(curios_base::Grain::X, a, b) => Prim::BinEql(
            curios_base::Grain::X,
            zonk_term(context, a)?,
            zonk_term(context, b)?,
        ),
        Prim::BinGet(curios_base::Grain::X, a, b) => Prim::BinGet(
            curios_base::Grain::X,
            zonk_term(context, a)?,
            zonk_term(context, b)?,
        ),
        Prim::BinAppend(curios_base::Grain::X, a, b) => Prim::BinAppend(
            curios_base::Grain::X,
            zonk_term(context, a)?,
            zonk_term(context, b)?,
        ),
        Prim::BinSlice(curios_base::Grain::X, a, b, c) => Prim::BinSlice(
            curios_base::Grain::X,
            zonk_term(context, a)?,
            zonk_term(context, b)?,
            zonk_term(context, c)?,
        ),
        Prim::BinConcat(curios_base::Grain::X, terms) => {
            Prim::BinConcat(curios_base::Grain::X, zonk_terms(context, terms)?)
        }
        Prim::BinLen(curios_base::Grain::B, t) => {
            Prim::BinLen(curios_base::Grain::B, zonk_term(context, t)?)
        }
        Prim::BinEql(curios_base::Grain::B, a, b) => Prim::BinEql(
            curios_base::Grain::B,
            zonk_term(context, a)?,
            zonk_term(context, b)?,
        ),
        Prim::BinGet(curios_base::Grain::B, a, b) => Prim::BinGet(
            curios_base::Grain::B,
            zonk_term(context, a)?,
            zonk_term(context, b)?,
        ),
        Prim::BinAppend(curios_base::Grain::B, a, b) => Prim::BinAppend(
            curios_base::Grain::B,
            zonk_term(context, a)?,
            zonk_term(context, b)?,
        ),
        Prim::BinSlice(curios_base::Grain::B, a, b, c) => Prim::BinSlice(
            curios_base::Grain::B,
            zonk_term(context, a)?,
            zonk_term(context, b)?,
            zonk_term(context, c)?,
        ),
        Prim::BinConcat(curios_base::Grain::B, terms) => {
            Prim::BinConcat(curios_base::Grain::B, zonk_terms(context, terms)?)
        }

        Prim::LstType(t) => Prim::LstType(zonk_term(context, t)?),
        Prim::Lst(elems) => Prim::Lst(zonk_terms(context, elems)?),
        Prim::LstLen(a, b) => Prim::LstLen(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::LstGet(a, b, c) => Prim::LstGet(
            zonk_term(context, a)?,
            zonk_term(context, b)?,
            zonk_term(context, c)?,
        ),
        Prim::LstAppend(a, b, c) => Prim::LstAppend(
            zonk_term(context, a)?,
            zonk_term(context, b)?,
            zonk_term(context, c)?,
        ),
        Prim::LstSlice(a, b, c, d) => Prim::LstSlice(
            zonk_term(context, a)?,
            zonk_term(context, b)?,
            zonk_term(context, c)?,
            zonk_term(context, d)?,
        ),
        Prim::LstConcat(ty, operands) => {
            Prim::LstConcat(zonk_term(context, ty)?, zonk_terms(context, operands)?)
        }
        Prim::LstMap(a, b, f, lst) => Prim::LstMap(
            zonk_term(context, a)?,
            zonk_term(context, b)?,
            zonk_term(context, f)?,
            zonk_term(context, lst)?,
        ),

        Prim::Foreign(function, args) => {
            Prim::Foreign(Arc::clone(function), zonk_terms(context, args)?)
        }
        Prim::IoExit(a, b) => Prim::IoExit(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::CellType(a) => Prim::CellType(zonk_term(context, a)?),
        Prim::Cell(a, b) => Prim::Cell(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::CellSet(a, b, c) => Prim::CellSet(
            zonk_term(context, a)?,
            zonk_term(context, b)?,
            zonk_term(context, c)?,
        ),
        Prim::CellGet(a, b) => Prim::CellGet(zonk_term(context, a)?, zonk_term(context, b)?),
    })
}
