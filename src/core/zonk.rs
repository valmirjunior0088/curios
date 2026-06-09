use super::{
    Apply, Arity, BlnMatch, Bound, Context, Definition, Error, Func, FuncType, Inductive, Item,
    Let, Metavar, Module, Nat, NatMatch, Prim, Proj, Rec, Scope, Subterm, Telescope, Term,
    Tuple, TupleType, UnionCtor, UnionMatch, UnionType,
};

/// Placeholder label pushed onto the binder stack for an unnamed (constant) scope
/// — e.g. a match motive's scrutinee binder. It occupies a de Bruijn slot so depths
/// stay correct; the empty string can never match a real free name (every
/// elaborator-introduced binder name is `fresh`-minted with a `#`, and globals are
/// `/`-joined paths), so `capture` never rewrites anything against it.
const UNNAMED_BINDER: &str = "";

/// Enter `scope`'s body with its binders pushed onto the stack, innermost last.
/// `capture` at a metavariable splice (see [`zonk_term`]) reads this stack to
/// realign a solution's free locals to the de Bruijn indices they now sit under.
fn enter_scope<A, B>(
    binders: &[String],
    scope: &Scope<A, B>,
    body: impl FnOnce(&B, &[String]) -> Result<B, Error>,
) -> Result<Scope<A, B>, Error>
where
    A: Arity,
    B: Bound,
{
    let mut extended = binders.to_vec();
    match scope.names() {
        Some(names) => extended.extend(names.iter().cloned()),
        None => extended.extend(std::iter::repeat_n(
            UNNAMED_BINDER.to_string(),
            scope.arity(),
        )),
    }

    scope.map_body(|inner| body(inner, &extended))
}

/// Substitute every solved metavariable in `term` by its (recursively zonked)
/// solution, yielding a meta-free term (§9). An unsolved metavariable is a
/// residual hole the elaborator never pinned down — reported as `cannot_infer`
/// at its occurrence span. The result flows downstream to `erase`, which is then
/// guaranteed never to meet a `Subterm::Metavar`.
///
/// Substitution replaces a metavariable node by its solution. A solution may
/// capture binders local to where the metavariable was born (e.g. a combinator's
/// type parameter), so it is *not* in general a closed term; [`zonk_term`] realigns
/// it to the de Bruijn indices of its splice site via the threaded binder stack.
pub fn zonk(context: &Context, term: &Term) -> Result<Term, Error> {
    zonk_term(context, term, &[])
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

    let body = zonk_term(context, &module.body, &[])?;

    let type_ = module
        .type_
        .as_ref()
        .map(|type_| zonk_term(context, type_, &[]))
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
                    params: inductive.params.zonk(context, &[])?,
                    constructors: inductive
                        .constructors
                        .iter()
                        .map(|(tag, signature)| Ok((tag.clone(), signature.zonk(context, &[])?)))
                        .collect::<Result<_, Error>>()?,
                },
            ))
        })
        .collect::<Result<_, Error>>()?;

    Ok(Module {
        items,
        inductives,
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
        type_: zonk_term(context, &def.type_, &[])?,
        body: zonk_term(context, &def.body, &[])?,
    })
}

fn zonk_term(context: &Context, term: &Term, binders: &[String]) -> Result<Term, Error> {
    // A metavariable node *is* the substitution site: replace it by its solution,
    // recursively zonked (the solution may itself mention solved metavariables).
    if let Subterm::Metavar(Metavar { id }) = &**term {
        let solution = context.metavar_solution(*id).ok_or_else(|| {
            let error = Error::cannot_infer(term.clone());
            match term.span() {
                Some(span) => error.at(span),
                None => error,
            }
        })?;

        // Resolve the solution in its own (named) frame: with an empty stack every
        // `capture` below is the identity, so nested metavariables are substituted
        // but no realignment happens yet — that is done once, here, for the whole
        // solution.
        let resolved = zonk_term(context, solution, &[])?;

        // Realign the solution to this occurrence. A solution may legitimately
        // mention binders local to where its metavariable was born (e.g. a
        // combinator's type parameter `A`); it is stored in named form, but it is
        // spliced into a context where those binders have been re-closed to de
        // Bruijn indices. `capture` against the enclosing binder labels (innermost
        // first) turns each such free name into its correct index. A closed
        // solution matches no label and is left untouched — the original "solutions
        // are closed" fast case, now a special case rather than an assumption.
        let labels = binders.iter().rev().map(String::as_str).collect::<Vec<_>>();
        let zonked = resolved.capture(&labels);

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

    let inner = zonk_subterm(context, term, binders)?;

    Ok(match term.span() {
        Some(span) => Term::spanned(span, inner),
        None => Term::from(inner),
    })
}

fn zonk_terms(context: &Context, terms: &[Term], binders: &[String]) -> Result<Vec<Term>, Error> {
    terms
        .iter()
        .map(|t| zonk_term(context, t, binders))
        .collect()
}

fn zonk_subterm(context: &Context, term: &Term, binders: &[String]) -> Result<Subterm, Error> {
    Ok(match &**term {
        Subterm::Type => Subterm::Type,
        Subterm::Var(var) => Subterm::Var(var.clone()),

        Subterm::Prim(prim) => Subterm::Prim(zonk_prim(context, prim, binders)?),

        Subterm::Func(Func { telescope }) => Subterm::Func(Func {
            telescope: telescope.zonk(context, binders)?,
        }),

        Subterm::FuncType(FuncType { telescope }) => Subterm::FuncType(FuncType {
            telescope: telescope.zonk(context, binders)?,
        }),

        Subterm::Apply(Apply { head, params }) => Subterm::Apply(Apply {
            head: zonk_term(context, head, binders)?,
            params: zonk_terms(context, params, binders)?,
        }),

        Subterm::TupleType(TupleType { telescope }) => Subterm::TupleType(TupleType {
            telescope: telescope.zonk(context, binders)?,
        }),

        Subterm::Tuple(Tuple { fields }) => Subterm::Tuple(Tuple {
            fields: zonk_terms(context, fields, binders)?,
        }),

        Subterm::Proj(Proj { head, index }) => Subterm::Proj(Proj {
            head: zonk_term(context, head, binders)?,
            index: *index,
        }),

        Subterm::BlnMatch(BlnMatch {
            head,
            motive,
            false_case,
            true_case,
        }) => Subterm::BlnMatch(BlnMatch {
            head: zonk_term(context, head, binders)?,
            motive: enter_scope(binders, motive, |b, binders| zonk_term(context, b, binders))?,
            false_case: zonk_term(context, false_case, binders)?,
            true_case: zonk_term(context, true_case, binders)?,
        }),

        Subterm::NatMatch(NatMatch::Induction {
            head,
            motive,
            zero_case,
            succ_case,
        }) => Subterm::NatMatch(NatMatch::Induction {
            head: zonk_term(context, head, binders)?,
            motive: enter_scope(binders, motive, |b, binders| zonk_term(context, b, binders))?,
            zero_case: zonk_term(context, zero_case, binders)?,
            succ_case: enter_scope(binders, succ_case, |b, binders| {
                zonk_term(context, b, binders)
            })?,
        }),

        Subterm::NatMatch(NatMatch::Dispatch {
            head,
            motive,
            cases,
            default,
        }) => Subterm::NatMatch(NatMatch::Dispatch {
            head: zonk_term(context, head, binders)?,
            motive: enter_scope(binders, motive, |b, binders| zonk_term(context, b, binders))?,
            cases: cases
                .iter()
                .map(|(n, body)| Ok((*n, zonk_term(context, body, binders)?)))
                .collect::<Result<_, Error>>()?,
            default: zonk_term(context, default, binders)?,
        }),

        Subterm::UnionType(UnionType { name, params }) => Subterm::UnionType(UnionType {
            name: name.clone(),
            params: zonk_terms(context, params, binders)?,
        }),

        Subterm::UnionCtor(UnionCtor {
            name,
            params,
            tag,
            payload,
        }) => Subterm::UnionCtor(UnionCtor {
            name: name.clone(),
            params: zonk_terms(context, params, binders)?,
            tag: tag.clone(),
            payload: zonk_terms(context, payload, binders)?,
        }),

        Subterm::UnionMatch(UnionMatch {
            head,
            motive,
            cases,
        }) => Subterm::UnionMatch(UnionMatch {
            head: zonk_term(context, head, binders)?,
            motive: enter_scope(binders, motive, |b, binders| zonk_term(context, b, binders))?,
            cases: cases
                .iter()
                .map(|(atom, scope)| {
                    Ok((
                        atom.clone(),
                        enter_scope(binders, scope, |b, binders| zonk_term(context, b, binders))?,
                    ))
                })
                .collect::<Result<_, Error>>()?,
        }),

        Subterm::Let(Let { type_, body, tail }) => Subterm::Let(Let {
            type_: zonk_term(context, type_, binders)?,
            body: zonk_term(context, body, binders)?,
            tail: enter_scope(binders, tail, |b, binders| zonk_term(context, b, binders))?,
        }),

        Subterm::Rec(Rec { items, tail }) => Subterm::Rec(Rec {
            items: items
                .iter()
                .map(|(type_, body)| {
                    Ok((
                        enter_scope(binders, type_, |t, binders| zonk_term(context, t, binders))?,
                        enter_scope(binders, body, |b, binders| zonk_term(context, b, binders))?,
                    ))
                })
                .collect::<Result<_, Error>>()?,
            tail: enter_scope(binders, tail, |b, binders| zonk_term(context, b, binders))?,
        }),

        // Handled in `zonk_term` before dispatch.
        Subterm::Metavar(_) => unreachable!("metavariable handled by `zonk_term`"),
    })
}

/// Zonk a primitive's term operands. Mirrors `traverse_prim`'s rebuild, but
/// fallibly substitutes metavariable solutions rather than de Bruijn shifting.
fn zonk_prim(context: &Context, prim: &Prim, binders: &[String]) -> Result<Prim, Error> {
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
        | Prim::IoRead => prim.clone(),

        Prim::Nat(Nat::Succ(spine, inner)) => Prim::Nat(Nat::Succ(
            spine.clone(),
            zonk_term(context, inner, binders)?,
        )),

        Prim::NatEql(a, b) => Prim::NatEql(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::NatNeq(a, b) => Prim::NatNeq(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::NatAdd(a, b) => Prim::NatAdd(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::NatSub(a, b) => Prim::NatSub(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::NatMul(a, b) => Prim::NatMul(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::NatLt(a, b) => Prim::NatLt(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::NatDiv(a, b) => Prim::NatDiv(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::NatRem(a, b) => Prim::NatRem(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::NatGt(a, b) => Prim::NatGt(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::NatLte(a, b) => Prim::NatLte(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::NatGte(a, b) => Prim::NatGte(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),

        Prim::IntEql(a, b) => Prim::IntEql(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::IntNeq(a, b) => Prim::IntNeq(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::IntAdd(a, b) => Prim::IntAdd(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::IntSub(a, b) => Prim::IntSub(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::IntMul(a, b) => Prim::IntMul(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::IntDiv(a, b) => Prim::IntDiv(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::IntRem(a, b) => Prim::IntRem(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::IntLt(a, b) => Prim::IntLt(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::IntGt(a, b) => Prim::IntGt(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::IntLte(a, b) => Prim::IntLte(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::IntGte(a, b) => Prim::IntGte(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),

        Prim::FltAdd(a, b) => Prim::FltAdd(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::FltSub(a, b) => Prim::FltSub(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::FltMul(a, b) => Prim::FltMul(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::FltDiv(a, b) => Prim::FltDiv(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::FltEql(a, b) => Prim::FltEql(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::FltNeq(a, b) => Prim::FltNeq(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::FltLt(a, b) => Prim::FltLt(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::FltGt(a, b) => Prim::FltGt(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::FltLte(a, b) => Prim::FltLte(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::FltGte(a, b) => Prim::FltGte(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::FltMin(a, b) => Prim::FltMin(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::FltMax(a, b) => Prim::FltMax(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),

        Prim::FltNeg(t) => Prim::FltNeg(zonk_term(context, t, binders)?),
        Prim::FltAbs(t) => Prim::FltAbs(zonk_term(context, t, binders)?),
        Prim::FltSqrt(t) => Prim::FltSqrt(zonk_term(context, t, binders)?),
        Prim::FltFloor(t) => Prim::FltFloor(zonk_term(context, t, binders)?),
        Prim::FltCeil(t) => Prim::FltCeil(zonk_term(context, t, binders)?),
        Prim::FltTrunc(t) => Prim::FltTrunc(zonk_term(context, t, binders)?),
        Prim::FltNearest(t) => Prim::FltNearest(zonk_term(context, t, binders)?),

        Prim::NatToStr(t) => Prim::NatToStr(zonk_term(context, t, binders)?),
        Prim::IntToStr(t) => Prim::IntToStr(zonk_term(context, t, binders)?),
        Prim::FltToStr(t) => Prim::FltToStr(zonk_term(context, t, binders)?),
        Prim::NatToInt(t) => Prim::NatToInt(zonk_term(context, t, binders)?),
        Prim::NatToFlt(t) => Prim::NatToFlt(zonk_term(context, t, binders)?),
        Prim::IntToNat(t) => Prim::IntToNat(zonk_term(context, t, binders)?),
        Prim::IntToFlt(t) => Prim::IntToFlt(zonk_term(context, t, binders)?),
        Prim::FltToNat(t) => Prim::FltToNat(zonk_term(context, t, binders)?),
        Prim::FltToInt(t) => Prim::FltToInt(zonk_term(context, t, binders)?),

        Prim::BinLen(t) => Prim::BinLen(zonk_term(context, t, binders)?),
        Prim::BinEql(a, b) => Prim::BinEql(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::BinGet(a, b) => Prim::BinGet(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::BinAppend(a, b) => Prim::BinAppend(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::BinSlice(a, b, c) => Prim::BinSlice(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
            zonk_term(context, c, binders)?,
        ),
        Prim::BinConcat(terms) => Prim::BinConcat(zonk_terms(context, terms, binders)?),

        Prim::ArrType(t) => Prim::ArrType(zonk_term(context, t, binders)?),
        Prim::Arr(elems) => Prim::Arr(zonk_terms(context, elems, binders)?),
        Prim::ArrLen(a, b) => Prim::ArrLen(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
        ),
        Prim::ArrGet(a, b, c) => Prim::ArrGet(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
            zonk_term(context, c, binders)?,
        ),
        Prim::ArrAppend(a, b, c) => Prim::ArrAppend(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
            zonk_term(context, c, binders)?,
        ),
        Prim::ArrSlice(a, b, c, d) => Prim::ArrSlice(
            zonk_term(context, a, binders)?,
            zonk_term(context, b, binders)?,
            zonk_term(context, c, binders)?,
            zonk_term(context, d, binders)?,
        ),
        Prim::ArrConcat(ty, operands) => Prim::ArrConcat(
            zonk_term(context, ty, binders)?,
            zonk_terms(context, operands, binders)?,
        ),

        Prim::IoPrint(t) => Prim::IoPrint(zonk_term(context, t, binders)?),
    })
}

/// Telescopes and the unit body carry term operands too. A small trait threads
/// `zonk` uniformly over `Telescope<Term>` (a `FuncType`) and `Telescope<()>`
/// (a `TupleType`, whose trailing body is `()`), mirroring `CollectMetavars`.
trait Zonk: Sized {
    fn zonk(&self, context: &Context, binders: &[String]) -> Result<Self, Error>;
}

impl Zonk for () {
    fn zonk(&self, _: &Context, _: &[String]) -> Result<Self, Error> {
        Ok(())
    }
}

impl Zonk for Term {
    fn zonk(&self, context: &Context, binders: &[String]) -> Result<Self, Error> {
        zonk_term(context, self, binders)
    }
}

impl<B: Zonk + Bound> Zonk for Telescope<B> {
    fn zonk(&self, context: &Context, binders: &[String]) -> Result<Self, Error> {
        match self {
            Telescope::Done(body) => Ok(Telescope::Done(body.zonk(context, binders)?.into())),
            // `ty` is at the current depth; the rest of the telescope is under this
            // binder, so descend it with the binder pushed.
            Telescope::Cons(ty, rest) => Ok(Telescope::Cons(
                zonk_term(context, ty, binders)?,
                enter_scope(binders, rest, |inner, binders| inner.zonk(context, binders))?,
            )),
        }
    }
}
