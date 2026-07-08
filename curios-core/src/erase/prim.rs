use {
    super::erase,
    crate::{Context, Error, Nat, Prim, Subterm, Term, reduce_with, wire_term},
    curios_base::Int,
    num_bigint::BigUint,
    num_traits::ToPrimitive,
    std::sync::Arc,
};

fn narrow_nat(k: &BigUint) -> Result<u32, Error> {
    k.to_u32().ok_or_else(|| Error::nat_overflow(k.clone()))
}

/// Narrow an unbounded type-level `Int` to `ersd`'s `i32` carrier, like
/// [`narrow_nat`]'s u32. The runtime's own i31 limit is enforced where it
/// appears, in the `cont` → wasm lowering.
fn narrow_int(value: &Int) -> Result<i32, Error> {
    value
        .to_i32()
        .ok_or_else(|| Error::int_overflow(value.clone()))
}

fn prim_type(prim: Prim) -> Term {
    Subterm::Prim(prim).into()
}

fn nat_type() -> Term {
    prim_type(Prim::NatType)
}

fn int_type() -> Term {
    prim_type(Prim::IntType)
}

fn flt_type() -> Term {
    prim_type(Prim::FltType)
}

fn bin_type() -> Term {
    prim_type(Prim::BinType)
}

fn bln_type() -> Term {
    prim_type(Prim::BlnType)
}

fn io_type() -> Term {
    prim_type(Prim::IoType)
}

fn lst_type(elem: Term) -> Term {
    Subterm::Prim(Prim::LstType(elem)).into()
}

fn pure(prim: curios_ersd::PurePrim) -> curios_ersd::Term {
    curios_ersd::Subterm::Prim(curios_ersd::Prim::Pure(prim)).into()
}

fn host(prim: curios_ersd::HostPrim) -> curios_ersd::Term {
    curios_ersd::Subterm::Prim(curios_ersd::Prim::Host(prim)).into()
}

fn cell(prim: curios_ersd::CellPrim) -> curios_ersd::Term {
    curios_ersd::Subterm::Prim(curios_ersd::Prim::Cell(prim)).into()
}

/// Erase both operands of a homogeneous binary primitive at `operand`, then
/// rebuild as the target `curios_ersd::PurePrim` via its constructor (`build`). Lets the
/// scalar arms name themselves once instead of spelling the `pure`/`erase`
/// scaffold out per variant.
fn binary(
    context: &mut Context,
    left: &Term,
    right: &Term,
    operand: fn() -> Term,
    build: fn(curios_ersd::Term, curios_ersd::Term) -> curios_ersd::PurePrim,
) -> Result<curios_ersd::Term, Error> {
    Ok(pure(build(
        erase(context, left, &operand())?,
        erase(context, right, &operand())?,
    )))
}

/// The unary counterpart of [`binary`], for single-operand scalar primitives.
fn unary(
    context: &mut Context,
    inner: &Term,
    operand: fn() -> Term,
    build: fn(curios_ersd::Term) -> curios_ersd::PurePrim,
) -> Result<curios_ersd::Term, Error> {
    Ok(pure(build(erase(context, inner, &operand())?)))
}

/// The host-prim analogue of [`unary`], for single-operand host primitives.
fn host_unary(
    context: &mut Context,
    inner: &Term,
    operand: fn() -> Term,
    build: fn(curios_ersd::Term) -> curios_ersd::HostPrim,
) -> Result<curios_ersd::Term, Error> {
    Ok(host(build(erase(context, inner, &operand())?)))
}

/// Lower a primitive to its `ersd` form. Pure downstream lowering: the term is
/// already well-typed (elaborate discharged every obligation) and meta-free
/// (zonk ran), so there is no checking here. `expected` is consumed only where a
/// runtime shape must be read off the type — the element type of a list literal
/// (§9).
pub(crate) fn erase_prim(
    context: &mut Context,
    _term: &Term,
    prim: &Prim,
    expected: &Term,
) -> Result<curios_ersd::Term, Error> {
    match prim {
        Prim::BlnType => Ok(curios_ersd::Subterm::Erased.into()),
        &Prim::Bln(value) => Ok(pure(curios_ersd::PurePrim::Nat(if value { 1 } else { 0 }))),
        // `Bln` rides the `0`/`1` i31 carrier, so its logic ops are exactly the
        // `Nat` bit ops on a single bit. `not` flips bit 0 with `xor 1`.
        Prim::BlnAnd(l, r) => binary(context, l, r, bln_type, curios_ersd::PurePrim::NatAnd),
        Prim::BlnOr(l, r) => binary(context, l, r, bln_type, curios_ersd::PurePrim::NatOr),
        Prim::BlnXor(l, r) => binary(context, l, r, bln_type, curios_ersd::PurePrim::NatXor),
        Prim::BlnEql(l, r) => binary(context, l, r, bln_type, curios_ersd::PurePrim::NatEql),
        // `bln != bln` is xor on the `0`/`1` carrier, exactly like `BlnXor`.
        Prim::BlnNeq(l, r) => binary(context, l, r, bln_type, curios_ersd::PurePrim::NatXor),
        Prim::NatType => Ok(curios_ersd::Subterm::Erased.into()),
        Prim::Nat(Nat::Zero) => Ok(pure(curios_ersd::PurePrim::Nat(0))),
        Prim::Nat(Nat::Succ(spine, inner)) => {
            if matches!(inner.as_ref(), Subterm::Prim(Prim::Nat(Nat::Zero))) {
                Ok(pure(curios_ersd::PurePrim::Nat(narrow_nat(spine)?)))
            } else {
                let inner_ersd = erase(context, inner, &nat_type())?;
                let spine_term = pure(curios_ersd::PurePrim::Nat(narrow_nat(spine)?));
                Ok(pure(curios_ersd::PurePrim::NatAdd(spine_term, inner_ersd)))
            }
        }
        Prim::NatEql(l, r) => binary(context, l, r, nat_type, curios_ersd::PurePrim::NatEql),
        // Handle identity stays abstract through `ersd`; the `ersd → cont`
        // lowering is the one place that knows a handle is bytes and turns this
        // into the corresponding `Bin` comparison.
        Prim::IoEql(l, r) => binary(context, l, r, io_type, curios_ersd::PurePrim::IoEql),
        Prim::NatAdd(l, r) => binary(context, l, r, nat_type, curios_ersd::PurePrim::NatAdd),
        Prim::NatSub(l, r) => binary(context, l, r, nat_type, curios_ersd::PurePrim::NatSub),
        Prim::NatMul(l, r) => binary(context, l, r, nat_type, curios_ersd::PurePrim::NatMul),
        Prim::NatNeq(l, r) => binary(context, l, r, nat_type, curios_ersd::PurePrim::NatNeq),
        Prim::NatDiv(l, r) => binary(context, l, r, nat_type, curios_ersd::PurePrim::NatDiv),
        Prim::NatRem(l, r) => binary(context, l, r, nat_type, curios_ersd::PurePrim::NatRem),
        Prim::NatLt(l, r) => binary(context, l, r, nat_type, curios_ersd::PurePrim::NatLt),
        Prim::NatGt(l, r) => binary(context, l, r, nat_type, curios_ersd::PurePrim::NatGt),
        Prim::NatLte(l, r) => binary(context, l, r, nat_type, curios_ersd::PurePrim::NatLte),
        Prim::NatGte(l, r) => binary(context, l, r, nat_type, curios_ersd::PurePrim::NatGte),
        Prim::NatAnd(l, r) => binary(context, l, r, nat_type, curios_ersd::PurePrim::NatAnd),
        Prim::NatOr(l, r) => binary(context, l, r, nat_type, curios_ersd::PurePrim::NatOr),
        Prim::NatXor(l, r) => binary(context, l, r, nat_type, curios_ersd::PurePrim::NatXor),
        Prim::NatShl(l, r) => binary(context, l, r, nat_type, curios_ersd::PurePrim::NatShl),
        Prim::NatShr(l, r) => binary(context, l, r, nat_type, curios_ersd::PurePrim::NatShr),
        Prim::IntType => Ok(curios_ersd::Subterm::Erased.into()),
        Prim::Int(value) => Ok(pure(curios_ersd::PurePrim::Int(narrow_int(value)?))),
        Prim::IntEql(l, r) => binary(context, l, r, int_type, curios_ersd::PurePrim::IntEql),
        Prim::IntNeq(l, r) => binary(context, l, r, int_type, curios_ersd::PurePrim::IntNeq),
        Prim::IntAdd(l, r) => binary(context, l, r, int_type, curios_ersd::PurePrim::IntAdd),
        Prim::IntSub(l, r) => binary(context, l, r, int_type, curios_ersd::PurePrim::IntSub),
        Prim::IntMul(l, r) => binary(context, l, r, int_type, curios_ersd::PurePrim::IntMul),
        Prim::IntDiv(l, r) => binary(context, l, r, int_type, curios_ersd::PurePrim::IntDiv),
        Prim::IntRem(l, r) => binary(context, l, r, int_type, curios_ersd::PurePrim::IntRem),
        Prim::IntLt(l, r) => binary(context, l, r, int_type, curios_ersd::PurePrim::IntLt),
        Prim::IntGt(l, r) => binary(context, l, r, int_type, curios_ersd::PurePrim::IntGt),
        Prim::IntLte(l, r) => binary(context, l, r, int_type, curios_ersd::PurePrim::IntLte),
        Prim::IntGte(l, r) => binary(context, l, r, int_type, curios_ersd::PurePrim::IntGte),
        Prim::IntAnd(l, r) => binary(context, l, r, int_type, curios_ersd::PurePrim::IntAnd),
        Prim::IntOr(l, r) => binary(context, l, r, int_type, curios_ersd::PurePrim::IntOr),
        Prim::IntXor(l, r) => binary(context, l, r, int_type, curios_ersd::PurePrim::IntXor),
        Prim::IntShl(l, r) => binary(context, l, r, int_type, curios_ersd::PurePrim::IntShl),
        Prim::IntShr(l, r) => binary(context, l, r, int_type, curios_ersd::PurePrim::IntShr),
        Prim::FltType => Ok(curios_ersd::Subterm::Erased.into()),
        &Prim::Flt(flt) => Ok(pure(curios_ersd::PurePrim::Flt(flt.to_f32()))),
        Prim::FltAdd(l, r) => binary(context, l, r, flt_type, curios_ersd::PurePrim::FltAdd),
        Prim::FltSub(l, r) => binary(context, l, r, flt_type, curios_ersd::PurePrim::FltSub),
        Prim::FltMul(l, r) => binary(context, l, r, flt_type, curios_ersd::PurePrim::FltMul),
        Prim::FltNeg(i) => unary(context, i, flt_type, curios_ersd::PurePrim::FltNeg),
        Prim::FltAbs(i) => unary(context, i, flt_type, curios_ersd::PurePrim::FltAbs),
        Prim::FltSqrt(i) => unary(context, i, flt_type, curios_ersd::PurePrim::FltSqrt),
        Prim::FltFloor(i) => unary(context, i, flt_type, curios_ersd::PurePrim::FltFloor),
        Prim::FltCeil(i) => unary(context, i, flt_type, curios_ersd::PurePrim::FltCeil),
        Prim::FltTrunc(i) => unary(context, i, flt_type, curios_ersd::PurePrim::FltTrunc),
        Prim::FltNearest(i) => unary(context, i, flt_type, curios_ersd::PurePrim::FltNearest),
        Prim::FltDiv(l, r) => binary(context, l, r, flt_type, curios_ersd::PurePrim::FltDiv),
        Prim::FltRem(l, r) => binary(context, l, r, flt_type, curios_ersd::PurePrim::FltRem),
        Prim::FltMin(l, r) => binary(context, l, r, flt_type, curios_ersd::PurePrim::FltMin),
        Prim::FltMax(l, r) => binary(context, l, r, flt_type, curios_ersd::PurePrim::FltMax),
        Prim::FltEql(l, r) => binary(context, l, r, flt_type, curios_ersd::PurePrim::FltEql),
        Prim::FltNeq(l, r) => binary(context, l, r, flt_type, curios_ersd::PurePrim::FltNeq),
        Prim::FltLt(l, r) => binary(context, l, r, flt_type, curios_ersd::PurePrim::FltLt),
        Prim::FltGt(l, r) => binary(context, l, r, flt_type, curios_ersd::PurePrim::FltGt),
        Prim::FltLte(l, r) => binary(context, l, r, flt_type, curios_ersd::PurePrim::FltLte),
        Prim::FltGte(l, r) => binary(context, l, r, flt_type, curios_ersd::PurePrim::FltGte),
        Prim::FltToLeBin(i) => unary(context, i, flt_type, curios_ersd::PurePrim::FltToLeBin),
        Prim::NatToInt(i) => unary(context, i, nat_type, curios_ersd::PurePrim::NatToInt),
        Prim::IntToNat(i) => unary(context, i, int_type, curios_ersd::PurePrim::IntToNat),
        Prim::IntToFlt(i) => unary(context, i, int_type, curios_ersd::PurePrim::IntToFlt),
        Prim::NatToFlt(i) => unary(context, i, nat_type, curios_ersd::PurePrim::NatToFlt),
        Prim::FltToInt(i) => unary(context, i, flt_type, curios_ersd::PurePrim::FltToInt),
        Prim::FltToNat(i) => unary(context, i, flt_type, curios_ersd::PurePrim::FltToNat),
        Prim::BinType => Ok(curios_ersd::Subterm::Erased.into()),
        Prim::Bin(bytes) => Ok(pure(curios_ersd::PurePrim::Bin(bytes.clone()))),
        Prim::BinLen(bin) => unary(context, bin, bin_type, curios_ersd::PurePrim::BinLen),
        Prim::BinEql(l, r) => binary(context, l, r, bin_type, curios_ersd::PurePrim::BinEql),
        Prim::BinGet(bin, index) => Ok(pure(curios_ersd::PurePrim::BinGet(
            erase(context, bin, &bin_type())?,
            erase(context, index, &nat_type())?,
        ))),
        Prim::BinSlice(bin, start, end) => Ok(pure(curios_ersd::PurePrim::BinSlice(
            erase(context, bin, &bin_type())?,
            erase(context, start, &nat_type())?,
            erase(context, end, &nat_type())?,
        ))),
        Prim::BinAppend(bin, byte) => Ok(pure(curios_ersd::PurePrim::BinAppend(
            erase(context, bin, &bin_type())?,
            erase(context, byte, &nat_type())?,
        ))),
        Prim::BinConcat(operands) => {
            let erased = operands
                .iter()
                .map(|e| erase(context, e, &bin_type()))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(pure(curios_ersd::PurePrim::BinConcat(erased)))
        }
        Prim::LstType(_) => Ok(curios_ersd::Subterm::Erased.into()),
        Prim::Lst(elems) => {
            // Elaborate already checked this literal against a list type (§9);
            // the element type is re-derived here only to lower the elements.
            let elem_type = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
                Subterm::Prim(Prim::LstType(elem_type)) => elem_type,
                _ => unreachable!("erase: list literal checked against non-list type"),
            };
            let erased_elems = elems
                .iter()
                .map(|e| erase(context, e, &elem_type))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(pure(curios_ersd::PurePrim::Lst(erased_elems)))
        }
        Prim::LstLen(type_, list) => {
            let expected_list_type = lst_type(type_.clone());
            let list_erased = erase(context, list, &expected_list_type)?;
            Ok(pure(curios_ersd::PurePrim::LstLen(list_erased)))
        }
        Prim::LstGet(type_, list, index) => {
            let expected_list_type = lst_type(type_.clone());
            let list_erased = erase(context, list, &expected_list_type)?;
            let index_erased = erase(context, index, &nat_type())?;
            Ok(pure(curios_ersd::PurePrim::LstGet(
                list_erased,
                index_erased,
            )))
        }
        Prim::LstSlice(type_, list, start, end) => {
            let expected_list_type = lst_type(type_.clone());
            let list_erased = erase(context, list, &expected_list_type)?;
            let start_erased = erase(context, start, &nat_type())?;
            let end_erased = erase(context, end, &nat_type())?;
            Ok(pure(curios_ersd::PurePrim::LstSlice(
                list_erased,
                start_erased,
                end_erased,
            )))
        }
        Prim::LstAppend(type_, list, elem) => {
            let expected_list_type = lst_type(type_.clone());
            let list_erased = erase(context, list, &expected_list_type)?;
            let elem_erased = erase(context, elem, type_)?;
            Ok(pure(curios_ersd::PurePrim::LstAppend(
                list_erased,
                elem_erased,
            )))
        }
        Prim::LstConcat(type_, operands) => {
            let expected_list_type = lst_type(type_.clone());
            let erased = operands
                .iter()
                .map(|e| erase(context, e, &expected_list_type))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(pure(curios_ersd::PurePrim::LstConcat(erased)))
        }
        Prim::LstMap(a, b, f, lst) => {
            let f_type = Term::func_type([("x", a.clone())], b.clone());
            let f_erased = erase(context, f, &f_type)?;
            let lst_ty = lst_type(a.clone());
            let lst_erased = erase(context, lst, &lst_ty)?;
            Ok(pure(curios_ersd::PurePrim::LstMap(lst_erased, f_erased)))
        }
        Prim::IoType => Ok(curios_ersd::Subterm::Erased.into()),
        &Prim::Io(token) => Ok(pure(curios_ersd::PurePrim::Io(token))),
        // The polymorphic result type is type-only; only the code survives.
        Prim::IoExit(_, code) => host_unary(context, code, nat_type, curios_ersd::HostPrim::IoExit),
        // A store-described host call: each operand erases against its wire
        // type, read off the same signature elaboration checked it with.
        Prim::Foreign(function, args) => {
            let mut erased = Vec::with_capacity(args.len());

            for (arg, (_, wire_type)) in args.iter().zip(&function.signature.params) {
                erased.push(erase(context, arg, &wire_term(wire_type))?);
            }

            Ok(host(curios_ersd::HostPrim::Foreign(
                Arc::clone(function),
                erased,
            )))
        }
        Prim::CellType(_) => Ok(curios_ersd::Subterm::Erased.into()),
        Prim::Cell(type_, init) => Ok(cell(curios_ersd::CellPrim::New(erase(
            context, init, type_,
        )?))),
        Prim::CellSet(type_, c, v) => {
            let cell_type: Term = Subterm::Prim(Prim::CellType(type_.clone())).into();
            Ok(cell(curios_ersd::CellPrim::Set(
                erase(context, c, &cell_type)?,
                erase(context, v, type_)?,
            )))
        }
        Prim::CellGet(type_, c) => {
            let cell_type: Term = Subterm::Prim(Prim::CellType(type_.clone())).into();
            Ok(cell(curios_ersd::CellPrim::Get(erase(
                context, c, &cell_type,
            )?)))
        }
    }
}
