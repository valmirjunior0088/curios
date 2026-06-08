use super::{
    Apply, Arity, BlnMatch, Bound, Context, Definition, Error, Func, FuncType, Item, Let, Match,
    Metavar, Module, Nat, NatMatch, Prim, Proj, Rec, Scope, Subterm, Telescope, Term, Tuple,
    TupleType,
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
        None => extended.extend(std::iter::repeat(UNNAMED_BINDER.to_string()).take(scope.arity())),
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

    Ok(Module { items, type_, body })
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
    terms.iter().map(|t| zonk_term(context, t, binders)).collect()
}

fn zonk_subterm(context: &Context, term: &Term, binders: &[String]) -> Result<Subterm, Error> {
    Ok(match &**term {
        Subterm::Type => Subterm::Type,
        Subterm::Var(var) => Subterm::Var(var.clone()),
        Subterm::Atom(atom) => Subterm::Atom(atom.clone()),
        Subterm::AtomType(atom_type) => Subterm::AtomType(atom_type.clone()),

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

        Subterm::Match(Match {
            head,
            motive,
            cases,
        }) => Subterm::Match(Match {
            head: zonk_term(context, head, binders)?,
            motive: enter_scope(binders, motive, |b, binders| zonk_term(context, b, binders))?,
            cases: cases
                .iter()
                .map(|(atom, body)| Ok((atom.clone(), zonk_term(context, body, binders)?)))
                .collect::<Result<_, Error>>()?,
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
            succ_case: enter_scope(binders, succ_case, |b, binders| zonk_term(context, b, binders))?,
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
    let z = |t: &Term| zonk_term(context, t, binders);

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

        Prim::Nat(Nat::Succ(spine, inner)) => Prim::Nat(Nat::Succ(spine.clone(), z(inner)?)),

        Prim::NatEql(a, b) => Prim::NatEql(z(a)?, z(b)?),
        Prim::NatNeq(a, b) => Prim::NatNeq(z(a)?, z(b)?),
        Prim::NatAdd(a, b) => Prim::NatAdd(z(a)?, z(b)?),
        Prim::NatSub(a, b) => Prim::NatSub(z(a)?, z(b)?),
        Prim::NatMul(a, b) => Prim::NatMul(z(a)?, z(b)?),
        Prim::NatLt(a, b) => Prim::NatLt(z(a)?, z(b)?),
        Prim::NatDiv(a, b) => Prim::NatDiv(z(a)?, z(b)?),
        Prim::NatRem(a, b) => Prim::NatRem(z(a)?, z(b)?),
        Prim::NatGt(a, b) => Prim::NatGt(z(a)?, z(b)?),
        Prim::NatLte(a, b) => Prim::NatLte(z(a)?, z(b)?),
        Prim::NatGte(a, b) => Prim::NatGte(z(a)?, z(b)?),

        Prim::IntEql(a, b) => Prim::IntEql(z(a)?, z(b)?),
        Prim::IntNeq(a, b) => Prim::IntNeq(z(a)?, z(b)?),
        Prim::IntAdd(a, b) => Prim::IntAdd(z(a)?, z(b)?),
        Prim::IntSub(a, b) => Prim::IntSub(z(a)?, z(b)?),
        Prim::IntMul(a, b) => Prim::IntMul(z(a)?, z(b)?),
        Prim::IntDiv(a, b) => Prim::IntDiv(z(a)?, z(b)?),
        Prim::IntRem(a, b) => Prim::IntRem(z(a)?, z(b)?),
        Prim::IntLt(a, b) => Prim::IntLt(z(a)?, z(b)?),
        Prim::IntGt(a, b) => Prim::IntGt(z(a)?, z(b)?),
        Prim::IntLte(a, b) => Prim::IntLte(z(a)?, z(b)?),
        Prim::IntGte(a, b) => Prim::IntGte(z(a)?, z(b)?),

        Prim::FltAdd(a, b) => Prim::FltAdd(z(a)?, z(b)?),
        Prim::FltSub(a, b) => Prim::FltSub(z(a)?, z(b)?),
        Prim::FltMul(a, b) => Prim::FltMul(z(a)?, z(b)?),
        Prim::FltDiv(a, b) => Prim::FltDiv(z(a)?, z(b)?),
        Prim::FltEql(a, b) => Prim::FltEql(z(a)?, z(b)?),
        Prim::FltNeq(a, b) => Prim::FltNeq(z(a)?, z(b)?),
        Prim::FltLt(a, b) => Prim::FltLt(z(a)?, z(b)?),
        Prim::FltGt(a, b) => Prim::FltGt(z(a)?, z(b)?),
        Prim::FltLte(a, b) => Prim::FltLte(z(a)?, z(b)?),
        Prim::FltGte(a, b) => Prim::FltGte(z(a)?, z(b)?),
        Prim::FltMin(a, b) => Prim::FltMin(z(a)?, z(b)?),
        Prim::FltMax(a, b) => Prim::FltMax(z(a)?, z(b)?),

        Prim::FltNeg(t) => Prim::FltNeg(z(t)?),
        Prim::FltAbs(t) => Prim::FltAbs(z(t)?),
        Prim::FltSqrt(t) => Prim::FltSqrt(z(t)?),
        Prim::FltFloor(t) => Prim::FltFloor(z(t)?),
        Prim::FltCeil(t) => Prim::FltCeil(z(t)?),
        Prim::FltTrunc(t) => Prim::FltTrunc(z(t)?),
        Prim::FltNearest(t) => Prim::FltNearest(z(t)?),

        Prim::NatToStr(t) => Prim::NatToStr(z(t)?),
        Prim::IntToStr(t) => Prim::IntToStr(z(t)?),
        Prim::FltToStr(t) => Prim::FltToStr(z(t)?),
        Prim::NatToInt(t) => Prim::NatToInt(z(t)?),
        Prim::NatToFlt(t) => Prim::NatToFlt(z(t)?),
        Prim::IntToNat(t) => Prim::IntToNat(z(t)?),
        Prim::IntToFlt(t) => Prim::IntToFlt(z(t)?),
        Prim::FltToNat(t) => Prim::FltToNat(z(t)?),
        Prim::FltToInt(t) => Prim::FltToInt(z(t)?),

        Prim::BinLen(t) => Prim::BinLen(z(t)?),
        Prim::BinEql(a, b) => Prim::BinEql(z(a)?, z(b)?),
        Prim::BinGet(a, b) => Prim::BinGet(z(a)?, z(b)?),
        Prim::BinAppend(a, b) => Prim::BinAppend(z(a)?, z(b)?),
        Prim::BinSlice(a, b, c) => Prim::BinSlice(z(a)?, z(b)?, z(c)?),
        Prim::BinConcat(terms) => Prim::BinConcat(zonk_terms(context, terms, binders)?),

        Prim::ArrType(t) => Prim::ArrType(z(t)?),
        Prim::Arr(elems) => Prim::Arr(zonk_terms(context, elems, binders)?),
        Prim::ArrLen(a, b) => Prim::ArrLen(z(a)?, z(b)?),
        Prim::ArrGet(a, b, c) => Prim::ArrGet(z(a)?, z(b)?, z(c)?),
        Prim::ArrAppend(a, b, c) => Prim::ArrAppend(z(a)?, z(b)?, z(c)?),
        Prim::ArrSlice(a, b, c, d) => Prim::ArrSlice(z(a)?, z(b)?, z(c)?, z(d)?),
        Prim::ArrConcat(ty, operands) => Prim::ArrConcat(z(ty)?, zonk_terms(context, operands, binders)?),

        Prim::IoPrint(t) => Prim::IoPrint(z(t)?),
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
