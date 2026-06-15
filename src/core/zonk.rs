use super::{
    Apply, Arity, Bound, Cases, Context, Definition, Error, Func, FuncType, Inductive, Item, Let,
    Match, Metavar, Module, MotivePattern, MotiveSlot, Nat, Prim, Proj, Rec, Scope, Struct,
    StructType, Structure, Subterm, Telescope, Term, Tuple, TupleType, UnionType, Variant,
};

/// Zonk `scope`'s body in place. (The binder stack that used to thread
/// through here fed the pre-spine label realignment; spines made it
/// unnecessary.)
fn enter_scope<A, B>(
    scope: &Scope<A, B>,
    body: impl FnOnce(&B) -> Result<B, Error>,
) -> Result<Scope<A, B>, Error>
where
    A: Arity,
    B: Bound,
{
    scope.map_body(body)
}

/// Substitute every solved metavariable in `term` by its (recursively zonked)
/// solution, yielding a meta-free term (§9). An unsolved metavariable is a
/// residual hole the elaborator never pinned down — reported as `cannot_infer`
/// at its occurrence span. The result flows downstream to `erase`, which is then
/// guaranteed never to meet a `Subterm::Metavar`.
///
/// Substitution replaces a metavariable node by its solution. A solution is
/// spelled with the birth telescope's names and is *not* in general a closed
/// term; the occurrence's spine (its delayed substitution) records what each
/// birth binder corresponds to at the splice site, so [`zonk_term`] resolves
/// by rewriting the solution through it. Every solved occurrence carries its
/// spine — `elaborate_apply` opens telescopes with rebuilt arguments, so no
/// bare copy of a birthed hole survives to be spliced.
pub fn zonk(context: &Context, term: &Term) -> Result<Term, Error> {
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
                        .map(|(tag, signature)| Ok((tag.clone(), signature.zonk(context)?)))
                        .collect::<Result<_, Error>>()?,
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
                    module: structure.module.clone(),
                    rep_public: structure.rep_public,
                },
            ))
        })
        .collect::<Result<_, Error>>()?;

    Ok(Module {
        items,
        inductives,
        structures,
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
        type_: zonk_term(context, &def.type_)?,
        body: zonk_term(context, &def.body)?,
    })
}

fn zonk_term(context: &Context, term: &Term) -> Result<Term, Error> {
    // A metavariable node *is* the substitution site: replace it by its solution,
    // recursively zonked (the solution may itself mention solved metavariables).
    if let Subterm::Metavar(Metavar { id, origin, spine }) = &**term {
        let solution = context.metavar_solution(*id).ok_or_else(|| {
            // An unsolved metavariable the *elaborator* minted (an omitted
            // implicit argument) is reported by the binder it filled — the
            // provenance rides on the node itself — not as a bare hole: the
            // user never wrote this metavariable, so a generic "cannot infer"
            // would point at nothing they can see.
            let error = match origin {
                Some(origin) => {
                    Error::uninferred_implicit(origin.func.clone(), origin.binder.clone())
                }
                None => Error::CannotInfer,
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
        Subterm::Var(var) => Subterm::Var(var.clone()),

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

        Subterm::UnionType(UnionType {
            name,
            params,
            indices,
        }) => Subterm::UnionType(UnionType {
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
            names,
        }) => Subterm::Struct(Struct {
            name: name.clone(),
            params: zonk_terms(context, params)?,
            fields: zonk_terms(context, fields)?,
            names: names.clone(),
        }),

        Subterm::Match(Match {
            head,
            motive,
            cases,
        }) => Subterm::Match(Match {
            head: zonk_term(context, head)?,
            motive: enter_scope(motive, |b| zonk_term(context, b))?,
            cases: match cases {
                Cases::Bln {
                    false_case,
                    true_case,
                } => Cases::Bln {
                    false_case: zonk_term(context, false_case)?,
                    true_case: zonk_term(context, true_case)?,
                },
                Cases::Nat {
                    zero_case,
                    succ_case,
                } => Cases::Nat {
                    zero_case: zonk_term(context, zero_case)?,
                    succ_case: enter_scope(succ_case, |b| zonk_term(context, b))?,
                },
                Cases::Switch { cases, default } => Cases::Switch {
                    cases: cases
                        .iter()
                        .map(|(n, body)| Ok((*n, zonk_term(context, body)?)))
                        .collect::<Result<_, Error>>()?,
                    default: zonk_term(context, default)?,
                },
                Cases::Union { cases, pattern } => Cases::Union {
                    cases: cases
                        .iter()
                        .map(|(atom, scope)| {
                            Ok((atom.clone(), enter_scope(scope, |b| zonk_term(context, b))?))
                        })
                        .collect::<Result<_, Error>>()?,
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

        Subterm::Let(Let { type_, body, tail }) => Subterm::Let(Let {
            type_: zonk_term(context, type_)?,
            body: zonk_term(context, body)?,
            tail: enter_scope(tail, |b| zonk_term(context, b))?,
        }),

        Subterm::Rec(Rec { items, tail }) => Subterm::Rec(Rec {
            items: items
                .iter()
                .map(|(type_, body)| {
                    Ok((
                        enter_scope(type_, |t| zonk_term(context, t))?,
                        enter_scope(body, |b| zonk_term(context, b))?,
                    ))
                })
                .collect::<Result<_, Error>>()?,
            tail: enter_scope(tail, |b| zonk_term(context, b))?,
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
        | Prim::IntType
        | Prim::Int(_)
        | Prim::FltType
        | Prim::Flt(_)
        | Prim::BinType
        | Prim::Bin(_)
        | Prim::StrType
        | Prim::Str(_)
        | Prim::IoType
        | Prim::Io(_) => prim.clone(),

        Prim::Nat(Nat::Succ(spine, inner)) => {
            Prim::Nat(Nat::Succ(spine.clone(), zonk_term(context, inner)?))
        }

        Prim::NatEql(a, b) => Prim::NatEql(zonk_term(context, a)?, zonk_term(context, b)?),
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

        Prim::FltAdd(a, b) => Prim::FltAdd(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltSub(a, b) => Prim::FltSub(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltMul(a, b) => Prim::FltMul(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltDiv(a, b) => Prim::FltDiv(zonk_term(context, a)?, zonk_term(context, b)?),
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

        Prim::NatToStr(t) => Prim::NatToStr(zonk_term(context, t)?),
        Prim::IntToStr(t) => Prim::IntToStr(zonk_term(context, t)?),
        Prim::FltToStr(t) => Prim::FltToStr(zonk_term(context, t)?),
        Prim::FltToLeBin(t) => Prim::FltToLeBin(zonk_term(context, t)?),
        Prim::NatToInt(t) => Prim::NatToInt(zonk_term(context, t)?),
        Prim::NatToFlt(t) => Prim::NatToFlt(zonk_term(context, t)?),
        Prim::IntToNat(t) => Prim::IntToNat(zonk_term(context, t)?),
        Prim::IntToFlt(t) => Prim::IntToFlt(zonk_term(context, t)?),
        Prim::FltToNat(t) => Prim::FltToNat(zonk_term(context, t)?),
        Prim::FltToInt(t) => Prim::FltToInt(zonk_term(context, t)?),

        Prim::BinLen(t) => Prim::BinLen(zonk_term(context, t)?),
        Prim::BinEql(a, b) => Prim::BinEql(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::BinGet(a, b) => Prim::BinGet(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::BinAppend(a, b) => Prim::BinAppend(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::BinSlice(a, b, c) => Prim::BinSlice(
            zonk_term(context, a)?,
            zonk_term(context, b)?,
            zonk_term(context, c)?,
        ),
        Prim::BinConcat(terms) => Prim::BinConcat(zonk_terms(context, terms)?),

        Prim::StrToBin(t) => Prim::StrToBin(zonk_term(context, t)?),
        Prim::StrOfBin(t) => Prim::StrOfBin(zonk_term(context, t)?),
        Prim::StrConcat(a, b) => Prim::StrConcat(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::StrEql(a, b) => Prim::StrEql(zonk_term(context, a)?, zonk_term(context, b)?),

        Prim::ArrType(t) => Prim::ArrType(zonk_term(context, t)?),
        Prim::Arr(elems) => Prim::Arr(zonk_terms(context, elems)?),
        Prim::ArrLen(a, b) => Prim::ArrLen(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::ArrGet(a, b, c) => Prim::ArrGet(
            zonk_term(context, a)?,
            zonk_term(context, b)?,
            zonk_term(context, c)?,
        ),
        Prim::ArrAppend(a, b, c) => Prim::ArrAppend(
            zonk_term(context, a)?,
            zonk_term(context, b)?,
            zonk_term(context, c)?,
        ),
        Prim::ArrSlice(a, b, c, d) => Prim::ArrSlice(
            zonk_term(context, a)?,
            zonk_term(context, b)?,
            zonk_term(context, c)?,
            zonk_term(context, d)?,
        ),
        Prim::ArrConcat(ty, operands) => {
            Prim::ArrConcat(zonk_term(context, ty)?, zonk_terms(context, operands)?)
        }

        Prim::IoRead(a, b) => Prim::IoRead(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IoWrite(a, b) => Prim::IoWrite(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IoOpen(a, b) => Prim::IoOpen(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IoClose(a) => Prim::IoClose(zonk_term(context, a)?),
    })
}

/// Telescopes and the unit body carry term operands too. A small trait threads
/// `zonk` uniformly over `Telescope<Term>` (a `FuncType`) and `Telescope<()>`
/// (a `TupleType`, whose trailing body is `()`), mirroring `CollectMetavars`.
trait Zonk: Sized {
    fn zonk(&self, context: &Context) -> Result<Self, Error>;
}

impl Zonk for () {
    fn zonk(&self, _: &Context) -> Result<Self, Error> {
        Ok(())
    }
}

impl Zonk for Term {
    fn zonk(&self, context: &Context) -> Result<Self, Error> {
        zonk_term(context, self)
    }
}

impl<B: Zonk + Bound> Zonk for Telescope<B> {
    fn zonk(&self, context: &Context) -> Result<Self, Error> {
        match self {
            Telescope::Done(body) => Ok(Telescope::Done(body.zonk(context)?.into())),
            Telescope::Cons(ty, rest) => Ok(Telescope::Cons(
                zonk_term(context, ty)?,
                enter_scope(rest, |inner| inner.zonk(context))?,
            )),
        }
    }
}
