use {
    super::{
        Context, Error, ImplicitOrigin, Mode, Prim, Subterm, Term, elaborate, expect, reduce_with,
        wire_term,
    },
    std::sync::Arc,
};

/// Elaborate both operands of a homogeneous binary primitive at `operand`, then
/// rebuild the variant through its constructor (`build`) and pair it with
/// `result`. Lets each arm name itself once instead of destructuring an
/// OR-pattern and re-matching just to reattach the elaborated operands.
fn binary(
    context: &mut Context,
    left: &Term,
    right: &Term,
    operand: &Term,
    result: Term,
    build: fn(Term, Term) -> Prim,
) -> Result<(Prim, Term), Error> {
    let left = elaborate(context, left, Mode::Check(operand.clone()))?.0;
    let right = elaborate(context, right, Mode::Check(operand.clone()))?.0;
    Ok((build(left, right), result))
}

/// The unary counterpart of [`binary`], for the float-unary primitives.
fn unary(
    context: &mut Context,
    inner: &Term,
    operand: &Term,
    result: Term,
    build: fn(Term) -> Prim,
) -> Result<(Prim, Term), Error> {
    let inner = elaborate(context, inner, Mode::Check(operand.clone()))?.0;
    Ok((build(inner), result))
}

/// Elaborate `bin`, requiring its inferred type to be `Bin`, and return the
/// rebuilt operand. The `Bin`-indexing/slicing prims read their shape off the
/// operand's type, so they infer it rather than checking against a known one.
fn infer_bin(context: &mut Context, bin: &Term) -> Result<Term, Error> {
    let (bin, actual) = elaborate(context, bin, Mode::Infer)?;
    match &*reduce_with(context, &actual)? {
        Subterm::Prim(Prim::BinType) => Ok(bin),
        other => Err(Error::type_mismatch(
            other.clone(),
            Subterm::Prim(Prim::BinType),
        )),
    }
}

fn lst_type(elem: Term) -> Term {
    Subterm::Prim(Prim::LstType(elem)).into()
}

/// Check every element of an `Lst` literal against an already-determined element
/// type, returning the rebuilt elements. Shared by the two ways the element type
/// is fixed: borrowed from `expected` when checking, or a fresh metavar when
/// inferring (see [`elaborate_prim`] and [`synth_prim`]'s `Lst` arm).
fn check_arr_elems(
    context: &mut Context,
    elems: &[Term],
    elem_type: &Term,
) -> Result<Vec<Term>, Error> {
    let mut elaborated = Vec::with_capacity(elems.len());

    for elem in elems {
        elaborated.push(elaborate(context, elem, Mode::Check(elem_type.clone()))?.0);
    }

    Ok(elaborated)
}

/// Synthesize a primitive's type, checking *and rebuilding* its operands. Mirrors
/// the old `infer_prim`, but every operand obligation goes through
/// `elaborate(Check)` and the elaborated operand is kept, so the returned `Prim`
/// is the authoritative (rebuilt) one that flows on to `zonk`/`erase` (§9).
fn synth_prim(context: &mut Context, prim: &Prim) -> Result<(Prim, Term), Error> {
    let nat_type: Term = Subterm::Prim(Prim::NatType).into();
    let int_type: Term = Subterm::Prim(Prim::IntType).into();
    let flt_type: Term = Subterm::Prim(Prim::FltType).into();
    let bln_type: Term = Subterm::Prim(Prim::BlnType).into();
    let bin_type: Term = Subterm::Prim(Prim::BinType).into();
    let io_type: Term = Subterm::Prim(Prim::IoType).into();

    Ok(match prim {
        Prim::BlnType => (prim.clone(), Term::type_()),
        Prim::Bln(_) => (prim.clone(), bln_type),
        Prim::NatType => (prim.clone(), Term::type_()),
        Prim::Nat(_) => (prim.clone(), nat_type),
        Prim::NatEql(l, r) => binary(context, l, r, &nat_type, bln_type.clone(), Prim::NatEql)?,
        Prim::IoEql(l, r) => binary(context, l, r, &io_type, bln_type.clone(), Prim::IoEql)?,
        Prim::NatNeq(l, r) => binary(context, l, r, &nat_type, bln_type.clone(), Prim::NatNeq)?,
        Prim::NatLt(l, r) => binary(context, l, r, &nat_type, bln_type.clone(), Prim::NatLt)?,
        Prim::NatGt(l, r) => binary(context, l, r, &nat_type, bln_type.clone(), Prim::NatGt)?,
        Prim::NatLte(l, r) => binary(context, l, r, &nat_type, bln_type.clone(), Prim::NatLte)?,
        Prim::NatGte(l, r) => binary(context, l, r, &nat_type, bln_type.clone(), Prim::NatGte)?,
        Prim::NatAdd(l, r) => binary(context, l, r, &nat_type, nat_type.clone(), Prim::NatAdd)?,
        Prim::NatSub(l, r) => binary(context, l, r, &nat_type, nat_type.clone(), Prim::NatSub)?,
        Prim::NatMul(l, r) => binary(context, l, r, &nat_type, nat_type.clone(), Prim::NatMul)?,
        Prim::NatDiv(l, r) => binary(context, l, r, &nat_type, nat_type.clone(), Prim::NatDiv)?,
        Prim::NatRem(l, r) => binary(context, l, r, &nat_type, nat_type.clone(), Prim::NatRem)?,
        Prim::NatAnd(l, r) => binary(context, l, r, &nat_type, nat_type.clone(), Prim::NatAnd)?,
        Prim::NatOr(l, r) => binary(context, l, r, &nat_type, nat_type.clone(), Prim::NatOr)?,
        Prim::NatXor(l, r) => binary(context, l, r, &nat_type, nat_type.clone(), Prim::NatXor)?,
        Prim::NatShl(l, r) => binary(context, l, r, &nat_type, nat_type.clone(), Prim::NatShl)?,
        Prim::NatShr(l, r) => binary(context, l, r, &nat_type, nat_type.clone(), Prim::NatShr)?,
        Prim::BlnAnd(l, r) => binary(context, l, r, &bln_type, bln_type.clone(), Prim::BlnAnd)?,
        Prim::BlnOr(l, r) => binary(context, l, r, &bln_type, bln_type.clone(), Prim::BlnOr)?,
        Prim::BlnXor(l, r) => binary(context, l, r, &bln_type, bln_type.clone(), Prim::BlnXor)?,
        Prim::BlnEql(l, r) => binary(context, l, r, &bln_type, bln_type.clone(), Prim::BlnEql)?,
        Prim::BlnNeq(l, r) => binary(context, l, r, &bln_type, bln_type.clone(), Prim::BlnNeq)?,
        Prim::IntType => (prim.clone(), Term::type_()),
        Prim::Int(_) => (prim.clone(), int_type),
        Prim::IntEql(l, r) => binary(context, l, r, &int_type, bln_type.clone(), Prim::IntEql)?,
        Prim::IntNeq(l, r) => binary(context, l, r, &int_type, bln_type.clone(), Prim::IntNeq)?,
        Prim::IntLt(l, r) => binary(context, l, r, &int_type, bln_type.clone(), Prim::IntLt)?,
        Prim::IntGt(l, r) => binary(context, l, r, &int_type, bln_type.clone(), Prim::IntGt)?,
        Prim::IntLte(l, r) => binary(context, l, r, &int_type, bln_type.clone(), Prim::IntLte)?,
        Prim::IntGte(l, r) => binary(context, l, r, &int_type, bln_type.clone(), Prim::IntGte)?,
        Prim::IntAdd(l, r) => binary(context, l, r, &int_type, int_type.clone(), Prim::IntAdd)?,
        Prim::IntSub(l, r) => binary(context, l, r, &int_type, int_type.clone(), Prim::IntSub)?,
        Prim::IntMul(l, r) => binary(context, l, r, &int_type, int_type.clone(), Prim::IntMul)?,
        Prim::IntDiv(l, r) => binary(context, l, r, &int_type, int_type.clone(), Prim::IntDiv)?,
        Prim::IntRem(l, r) => binary(context, l, r, &int_type, int_type.clone(), Prim::IntRem)?,
        Prim::IntAnd(l, r) => binary(context, l, r, &int_type, int_type.clone(), Prim::IntAnd)?,
        Prim::IntOr(l, r) => binary(context, l, r, &int_type, int_type.clone(), Prim::IntOr)?,
        Prim::IntXor(l, r) => binary(context, l, r, &int_type, int_type.clone(), Prim::IntXor)?,
        Prim::IntShl(l, r) => binary(context, l, r, &int_type, int_type.clone(), Prim::IntShl)?,
        Prim::IntShr(l, r) => binary(context, l, r, &int_type, int_type.clone(), Prim::IntShr)?,
        Prim::FltType => (prim.clone(), Term::type_()),
        Prim::Flt(_) => (prim.clone(), flt_type),
        Prim::FltAdd(l, r) => binary(context, l, r, &flt_type, flt_type.clone(), Prim::FltAdd)?,
        Prim::FltSub(l, r) => binary(context, l, r, &flt_type, flt_type.clone(), Prim::FltSub)?,
        Prim::FltMul(l, r) => binary(context, l, r, &flt_type, flt_type.clone(), Prim::FltMul)?,
        Prim::FltDiv(l, r) => binary(context, l, r, &flt_type, flt_type.clone(), Prim::FltDiv)?,
        Prim::FltRem(l, r) => binary(context, l, r, &flt_type, flt_type.clone(), Prim::FltRem)?,
        Prim::FltMin(l, r) => binary(context, l, r, &flt_type, flt_type.clone(), Prim::FltMin)?,
        Prim::FltMax(l, r) => binary(context, l, r, &flt_type, flt_type.clone(), Prim::FltMax)?,
        Prim::FltNeg(i) => unary(context, i, &flt_type, flt_type.clone(), Prim::FltNeg)?,
        Prim::FltAbs(i) => unary(context, i, &flt_type, flt_type.clone(), Prim::FltAbs)?,
        Prim::FltSqrt(i) => unary(context, i, &flt_type, flt_type.clone(), Prim::FltSqrt)?,
        Prim::FltFloor(i) => unary(context, i, &flt_type, flt_type.clone(), Prim::FltFloor)?,
        Prim::FltCeil(i) => unary(context, i, &flt_type, flt_type.clone(), Prim::FltCeil)?,
        Prim::FltTrunc(i) => unary(context, i, &flt_type, flt_type.clone(), Prim::FltTrunc)?,
        Prim::FltNearest(i) => unary(context, i, &flt_type, flt_type.clone(), Prim::FltNearest)?,
        Prim::FltEql(l, r) => binary(context, l, r, &flt_type, bln_type.clone(), Prim::FltEql)?,
        Prim::FltNeq(l, r) => binary(context, l, r, &flt_type, bln_type.clone(), Prim::FltNeq)?,
        Prim::FltLt(l, r) => binary(context, l, r, &flt_type, bln_type.clone(), Prim::FltLt)?,
        Prim::FltGt(l, r) => binary(context, l, r, &flt_type, bln_type.clone(), Prim::FltGt)?,
        Prim::FltLte(l, r) => binary(context, l, r, &flt_type, bln_type.clone(), Prim::FltLte)?,
        Prim::FltGte(l, r) => binary(context, l, r, &flt_type, bln_type.clone(), Prim::FltGte)?,
        // `Flt/to_le_bin` exposes the IEEE-754 bytes (`Bin`); `/std/Flt/to_str`
        // renders them to the proof-carrying `/syn/Str` in Curios (Dragon4).
        Prim::FltToLeBin(i) => unary(context, i, &flt_type, bin_type.clone(), Prim::FltToLeBin)?,
        Prim::NatToInt(i) => unary(context, i, &nat_type, int_type.clone(), Prim::NatToInt)?,
        Prim::NatToFlt(i) => unary(context, i, &nat_type, flt_type.clone(), Prim::NatToFlt)?,
        Prim::IntToNat(i) => unary(context, i, &int_type, nat_type.clone(), Prim::IntToNat)?,
        Prim::IntToFlt(i) => unary(context, i, &int_type, flt_type.clone(), Prim::IntToFlt)?,
        Prim::FltToNat(i) => unary(context, i, &flt_type, nat_type.clone(), Prim::FltToNat)?,
        Prim::FltToInt(i) => unary(context, i, &flt_type, int_type.clone(), Prim::FltToInt)?,
        Prim::BinType => (prim.clone(), Term::type_()),
        Prim::Bin(_) => (prim.clone(), bin_type),
        Prim::BinLen(bin) => {
            let bin = infer_bin(context, bin)?;
            (Prim::BinLen(bin), nat_type)
        }
        Prim::BinEql(left, right) => {
            let left = elaborate(context, left, Mode::Check(bin_type.clone()))?.0;
            let right = elaborate(context, right, Mode::Check(bin_type))?.0;
            (Prim::BinEql(left, right), bln_type)
        }
        Prim::BinGet(bin, index) => {
            let bin = infer_bin(context, bin)?;
            let index = elaborate(context, index, Mode::Check(nat_type.clone()))?.0;
            (Prim::BinGet(bin, index), nat_type)
        }
        Prim::BinSlice(bin, start, end) => {
            let bin = infer_bin(context, bin)?;
            let start = elaborate(context, start, Mode::Check(nat_type.clone()))?.0;
            let end = elaborate(context, end, Mode::Check(nat_type))?.0;
            (Prim::BinSlice(bin, start, end), bin_type)
        }
        Prim::BinAppend(bin, byte) => {
            let bin = infer_bin(context, bin)?;
            let byte = elaborate(context, byte, Mode::Check(nat_type))?.0;
            (Prim::BinAppend(bin, byte), bin_type)
        }
        Prim::BinConcat(operands) => {
            let mut elaborated = Vec::with_capacity(operands.len());
            for operand in operands {
                elaborated.push(elaborate(context, operand, Mode::Check(bin_type.clone()))?.0);
            }
            (Prim::BinConcat(elaborated), bin_type)
        }
        Prim::LstType(elem) => {
            let elem = elaborate(context, elem, Mode::Check(Term::type_()))?.0;
            (Prim::LstType(elem), Term::type_())
        }
        // Inferring: the element type is unknown, so mint a fresh metavar — the
        // implicit `@T` a `nil`/`cons` constructor would insert — which the elements
        // solve (an empty `[]` leaves it for a later unification to ground, exactly
        // as the old `Lst/nil()` did). Checking goes through `elaborate_prim`, which
        // borrows the concrete element type from `expected` before reaching here.
        Prim::Lst(elems) => {
            let elem_type = context.fresh_metavar(
                Term::type_(),
                None,
                ImplicitOrigin {
                    func: "Lst".to_string(),
                    binder: "T".to_string(),
                },
            );
            let elaborated = check_arr_elems(context, elems, &elem_type)?;
            (Prim::Lst(elaborated), lst_type(elem_type))
        }
        Prim::LstLen(type_, list) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let list_type = lst_type(type_.clone());
            let list = elaborate(context, list, Mode::Check(list_type))?.0;
            (Prim::LstLen(type_, list), nat_type)
        }
        Prim::LstGet(type_, list, index) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let list_type = lst_type(type_.clone());
            let list = elaborate(context, list, Mode::Check(list_type))?.0;
            let index = elaborate(context, index, Mode::Check(nat_type))?.0;
            let output = type_.clone();
            (Prim::LstGet(type_, list, index), output)
        }
        Prim::LstSlice(type_, list, start, end) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let list_type = lst_type(type_.clone());
            let list = elaborate(context, list, Mode::Check(list_type.clone()))?.0;
            let start = elaborate(context, start, Mode::Check(nat_type.clone()))?.0;
            let end = elaborate(context, end, Mode::Check(nat_type))?.0;
            (Prim::LstSlice(type_, list, start, end), list_type)
        }
        Prim::LstAppend(type_, list, elem) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let list_type = lst_type(type_.clone());
            let list = elaborate(context, list, Mode::Check(list_type.clone()))?.0;
            let elem = elaborate(context, elem, Mode::Check(type_.clone()))?.0;
            (Prim::LstAppend(type_, list, elem), list_type)
        }
        Prim::LstConcat(type_, operands) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let list_type = lst_type(type_.clone());
            let mut elaborated = Vec::with_capacity(operands.len());
            for operand in operands {
                elaborated.push(elaborate(context, operand, Mode::Check(list_type.clone()))?.0);
            }
            (Prim::LstConcat(type_, elaborated), list_type)
        }
        Prim::LstMap(a, b, f, lst) => {
            let a = elaborate(context, a, Mode::Check(Term::type_()))?.0;
            let b = elaborate(context, b, Mode::Check(Term::type_()))?.0;
            let f_type = Term::func_type([("x", a.clone())], b.clone());
            let f = elaborate(context, f, Mode::Check(f_type))?.0;
            let lst_a = lst_type(a.clone());
            let lst = elaborate(context, lst, Mode::Check(lst_a))?.0;
            let lst_b = lst_type(b.clone());
            (Prim::LstMap(a, b, f, lst), lst_b)
        }
        Prim::IoType => (prim.clone(), Term::type_()),
        Prim::Io(_) => (prim.clone(), io_type),
        // `(@A : Type) -> Nat -> A`: exit never returns, so the result type is
        // whatever the caller demands (`/std/Proc/exit` instantiates it at
        // `False`). The type argument keeps the kernel from naming `/std/False`.
        Prim::IoExit(type_, code) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let code = elaborate(context, code, Mode::Check(nat_type))?.0;
            (Prim::IoExit(type_.clone(), code), type_)
        }
        // A store-described host call: each operand checks against its wire
        // type, and the result shape (unit, bare value, named record) is read
        // off the signature. The arity is an invariant of construction (the
        // prelude builds the argument list from the same signature).
        Prim::Foreign(function, args) => {
            let signature = &function.signature;

            assert_eq!(
                args.len(),
                signature.params.len(),
                "{} operand count does not match its signature",
                function.name
            );

            let mut elaborated = Vec::with_capacity(args.len());
            for (arg, (_, wire_type)) in args.iter().zip(&signature.params) {
                elaborated.push(elaborate(context, arg, Mode::Check(wire_term(wire_type)))?.0);
            }

            let result = match signature.results.as_slice() {
                [] => Term::tuple_type_unit(),
                [(_, wire_type)] => wire_term(wire_type),
                results => Term::tuple_type(
                    results
                        .iter()
                        .map(|(label, wire_type)| (label.as_str(), wire_term(wire_type))),
                ),
            };

            (Prim::Foreign(Arc::clone(function), elaborated), result)
        }
        Prim::CellType(elem) => {
            let elem = elaborate(context, elem, Mode::Check(Term::type_()))?.0;
            (Prim::CellType(elem), Term::type_())
        }
        Prim::Cell(type_, init) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let init = elaborate(context, init, Mode::Check(type_.clone()))?.0;
            let cell_type: Term = Subterm::Prim(Prim::CellType(type_.clone())).into();
            (Prim::Cell(type_, init), cell_type)
        }
        Prim::CellSet(type_, cell, value) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let cell_type: Term = Subterm::Prim(Prim::CellType(type_.clone())).into();
            let cell = elaborate(context, cell, Mode::Check(cell_type))?.0;
            let value = elaborate(context, value, Mode::Check(type_.clone()))?.0;
            (Prim::CellSet(type_, cell, value), Term::tuple_type_unit())
        }
        Prim::CellGet(type_, cell) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let cell_type: Term = Subterm::Prim(Prim::CellType(type_.clone())).into();
            let cell = elaborate(context, cell, Mode::Check(cell_type))?.0;
            let output = type_.clone();
            (Prim::CellGet(type_, cell), output)
        }
    })
}

pub(crate) fn elaborate_prim(
    context: &mut Context,
    term: &Term,
    prim: &Prim,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // `Lst` is bidirectional. Checking against a concrete `Lst(T)`, it borrows
    // the element type from `expected` — definitional, so each element is
    // checked against the known type (better errors, and numeric element
    // literals pick the right numeric type). Any other expected shape — a
    // stuck `?M(?A)` awaiting the flex-apply imitation included — falls
    // through to `synth_prim`, which mints a fresh element-type metavar; the
    // check-after-infer unification then equates `Lst(?T)` with the expected
    // type (pinning `?M := Lst`, or reporting the genuine mismatch).
    if let (Prim::Lst(elems), Mode::Check(expected)) = (prim, &mode)
        && let Subterm::Prim(Prim::LstType(elem_type)) = &*reduce_with(context, expected)?
    {
        let elaborated = check_arr_elems(context, elems, elem_type)?;

        return Ok((Term::prim(Prim::Lst(elaborated)), expected.clone()));
    }

    // `LstConcat` mirrors `Lst`'s bidirectionality — and must, because the
    // lowering of a spread list literal `[a, ..xs, b]` mints a fresh metavar
    // for the element-type slot. Checking against a concrete `Lst(T)`, solve
    // the slot against `expected` FIRST (`expect` unifies `Lst(slot)` with
    // it), so the operands — the literal chunks especially — elaborate
    // against the known element type instead of default-solving it from the
    // first element. Any other expected shape falls through to `synth_prim`,
    // exactly as for `Lst`.
    if let (Prim::LstConcat(type_slot, operands), Mode::Check(expected)) = (prim, &mode)
        && let Subterm::Prim(Prim::LstType(_)) = &*reduce_with(context, expected)?
    {
        let type_slot = elaborate(context, type_slot, Mode::Check(Term::type_()))?.0;
        expect(context, term, &lst_type(type_slot.clone()), expected)?;

        let mut elaborated = Vec::with_capacity(operands.len());
        for operand in operands {
            elaborated.push(elaborate(context, operand, Mode::Check(expected.clone()))?.0);
        }

        return Ok((
            Term::prim(Prim::LstConcat(type_slot, elaborated)),
            expected.clone(),
        ));
    }

    let (prim, type_) = synth_prim(context, prim)?;

    if let Mode::Check(expected) = &mode {
        expect(context, term, &type_, expected)?;
    }

    Ok((Term::prim(prim), type_))
}
