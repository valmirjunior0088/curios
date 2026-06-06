use super::{
    Apply, BlnMatch, Bound, Context, Definition, Error, Func, FuncType, Item, Let, Match, Metavar,
    Module, Nat, NatMatch, Prim, Proj, Rec, Subterm, Telescope, Term, Tuple, TupleType,
};

/// Substitute every solved metavariable in `term` by its (recursively zonked)
/// solution, yielding a meta-free term (§9). An unsolved metavariable is a
/// residual hole the elaborator never pinned down — reported as `cannot_infer`
/// at its occurrence span. The result flows downstream to `erase`, which is then
/// guaranteed never to meet a `Subterm::Metavar`.
///
/// Substitution is direct (a metavariable node is replaced by its solution as
/// is): sound because a bare metavariable is a closed head and its solution is a
/// closed term, so no de Bruijn realignment is needed.
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
        type_: zonk_term(context, &def.type_)?,
        body: zonk_term(context, &def.body)?,
    })
}

fn zonk_term(context: &Context, term: &Term) -> Result<Term, Error> {
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

        let zonked = zonk_term(context, solution)?;

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
        Subterm::Atom(atom) => Subterm::Atom(atom.clone()),
        Subterm::AtomType(atom_type) => Subterm::AtomType(atom_type.clone()),

        Subterm::Prim(prim) => Subterm::Prim(zonk_prim(context, prim)?),

        Subterm::Func(Func { body }) => Subterm::Func(Func {
            body: body.map_body(|b| zonk_term(context, b))?,
        }),

        Subterm::FuncType(FuncType { telescope }) => Subterm::FuncType(FuncType {
            telescope: telescope.zonk(context)?,
        }),

        Subterm::Apply(Apply { head, params }) => Subterm::Apply(Apply {
            head: zonk_term(context, head)?,
            params: zonk_terms(context, params)?,
        }),

        Subterm::TupleType(TupleType { telescope }) => Subterm::TupleType(TupleType {
            telescope: telescope.zonk(context)?,
        }),

        Subterm::Tuple(Tuple { fields }) => Subterm::Tuple(Tuple {
            fields: zonk_terms(context, fields)?,
        }),

        Subterm::Proj(Proj { head, index }) => Subterm::Proj(Proj {
            head: zonk_term(context, head)?,
            index: *index,
        }),

        Subterm::Match(Match {
            head,
            motive,
            cases,
        }) => Subterm::Match(Match {
            head: zonk_term(context, head)?,
            motive: motive.map_body(|b| zonk_term(context, b))?,
            cases: cases
                .iter()
                .map(|(atom, body)| Ok((atom.clone(), zonk_term(context, body)?)))
                .collect::<Result<_, Error>>()?,
        }),

        Subterm::BlnMatch(BlnMatch {
            head,
            motive,
            false_case,
            true_case,
        }) => Subterm::BlnMatch(BlnMatch {
            head: zonk_term(context, head)?,
            motive: motive.map_body(|b| zonk_term(context, b))?,
            false_case: zonk_term(context, false_case)?,
            true_case: zonk_term(context, true_case)?,
        }),

        Subterm::NatMatch(NatMatch::Induction {
            head,
            motive,
            zero_case,
            succ_case,
        }) => Subterm::NatMatch(NatMatch::Induction {
            head: zonk_term(context, head)?,
            motive: motive.map_body(|b| zonk_term(context, b))?,
            zero_case: zonk_term(context, zero_case)?,
            succ_case: succ_case.map_body(|b| zonk_term(context, b))?,
        }),

        Subterm::NatMatch(NatMatch::Dispatch {
            head,
            motive,
            cases,
            default,
        }) => Subterm::NatMatch(NatMatch::Dispatch {
            head: zonk_term(context, head)?,
            motive: motive.map_body(|b| zonk_term(context, b))?,
            cases: cases
                .iter()
                .map(|(n, body)| Ok((*n, zonk_term(context, body)?)))
                .collect::<Result<_, Error>>()?,
            default: zonk_term(context, default)?,
        }),

        Subterm::Let(Let { type_, body, tail }) => Subterm::Let(Let {
            type_: zonk_term(context, type_)?,
            body: zonk_term(context, body)?,
            tail: tail.map_body(|b| zonk_term(context, b))?,
        }),

        Subterm::Rec(Rec { items, tail }) => Subterm::Rec(Rec {
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
    let z = |t: &Term| zonk_term(context, t);

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
        Prim::BinConcat(terms) => Prim::BinConcat(zonk_terms(context, terms)?),

        Prim::ArrType(t) => Prim::ArrType(z(t)?),
        Prim::Arr(elems) => Prim::Arr(zonk_terms(context, elems)?),
        Prim::ArrLen(a, b) => Prim::ArrLen(z(a)?, z(b)?),
        Prim::ArrGet(a, b, c) => Prim::ArrGet(z(a)?, z(b)?, z(c)?),
        Prim::ArrAppend(a, b, c) => Prim::ArrAppend(z(a)?, z(b)?, z(c)?),
        Prim::ArrSlice(a, b, c, d) => Prim::ArrSlice(z(a)?, z(b)?, z(c)?, z(d)?),
        Prim::ArrConcat(ty, operands) => Prim::ArrConcat(z(ty)?, zonk_terms(context, operands)?),

        Prim::IoPrint(t) => Prim::IoPrint(z(t)?),
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
                rest.map_body(|t| t.zonk(context))?,
            )),
        }
    }
}
