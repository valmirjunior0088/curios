use {
    super::erase,
    crate::{
        core::{Context, Error, Nat, Prim, Subterm, Term, reduce_with},
        ersd,
    },
    num_bigint::BigUint,
    num_traits::ToPrimitive,
};

fn narrow_nat(k: &BigUint) -> Result<u32, Error> {
    k.to_u32().ok_or_else(|| Error::nat_overflow(k.clone()))
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

fn pure(prim: ersd::PurePrim) -> ersd::Term {
    ersd::Term::Prim(ersd::Prim::Pure(prim))
}

fn host(prim: ersd::HostPrim) -> ersd::Term {
    ersd::Term::Prim(ersd::Prim::Host(prim))
}

/// Lower a primitive to its `ersd` form. Pure downstream lowering: the term is
/// already well-typed (elaborate discharged every obligation) and meta-free
/// (zonk ran), so there is no checking here. `expected` is consumed only where a
/// runtime shape must be read off the type — the element type of an array literal
/// (§9).
pub fn erase_prim(
    context: &mut Context,
    _term: &Term,
    prim: &Prim,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    match prim {
        Prim::BlnType => Ok(ersd::Term::Erased),
        &Prim::Bln(value) => Ok(pure(ersd::PurePrim::Nat(if value { 1 } else { 0 }))),
        Prim::NatType => Ok(ersd::Term::Erased),
        Prim::Nat(Nat::Zero) => Ok(pure(ersd::PurePrim::Nat(0))),
        Prim::Nat(Nat::Succ(spine, inner)) => {
            if matches!(inner.as_ref(), Subterm::Prim(Prim::Nat(Nat::Zero))) {
                Ok(pure(ersd::PurePrim::Nat(narrow_nat(spine)?)))
            } else {
                let inner_ersd = erase(context, inner, &nat_type())?;
                let spine_term = pure(ersd::PurePrim::Nat(narrow_nat(spine)?));
                Ok(pure(ersd::PurePrim::NatAdd(
                    spine_term.into(),
                    inner_ersd.into(),
                )))
            }
        }
        Prim::NatEql(left, right) => Ok(pure(ersd::PurePrim::NatEql(
            erase(context, left, &nat_type())?.into(),
            erase(context, right, &nat_type())?.into(),
        ))),
        Prim::NatAdd(left, right) => Ok(pure(ersd::PurePrim::NatAdd(
            erase(context, left, &nat_type())?.into(),
            erase(context, right, &nat_type())?.into(),
        ))),
        Prim::NatSub(left, right) => Ok(pure(ersd::PurePrim::NatSub(
            erase(context, left, &nat_type())?.into(),
            erase(context, right, &nat_type())?.into(),
        ))),
        Prim::NatMul(left, right) => Ok(pure(ersd::PurePrim::NatMul(
            erase(context, left, &nat_type())?.into(),
            erase(context, right, &nat_type())?.into(),
        ))),
        Prim::NatNeq(left, right) => Ok(pure(ersd::PurePrim::NatNeq(
            erase(context, left, &nat_type())?.into(),
            erase(context, right, &nat_type())?.into(),
        ))),
        Prim::NatDiv(left, right) => Ok(pure(ersd::PurePrim::NatDiv(
            erase(context, left, &nat_type())?.into(),
            erase(context, right, &nat_type())?.into(),
        ))),
        Prim::NatRem(left, right) => Ok(pure(ersd::PurePrim::NatRem(
            erase(context, left, &nat_type())?.into(),
            erase(context, right, &nat_type())?.into(),
        ))),
        Prim::NatLt(left, right) => Ok(pure(ersd::PurePrim::NatLt(
            erase(context, left, &nat_type())?.into(),
            erase(context, right, &nat_type())?.into(),
        ))),
        Prim::NatGt(left, right) => Ok(pure(ersd::PurePrim::NatGt(
            erase(context, left, &nat_type())?.into(),
            erase(context, right, &nat_type())?.into(),
        ))),
        Prim::NatLte(left, right) => Ok(pure(ersd::PurePrim::NatLte(
            erase(context, left, &nat_type())?.into(),
            erase(context, right, &nat_type())?.into(),
        ))),
        Prim::NatGte(left, right) => Ok(pure(ersd::PurePrim::NatGte(
            erase(context, left, &nat_type())?.into(),
            erase(context, right, &nat_type())?.into(),
        ))),
        Prim::IntType => Ok(ersd::Term::Erased),
        &Prim::Int(value) => Ok(pure(ersd::PurePrim::Int(value.to_i32()))),
        Prim::IntEql(left, right) => Ok(pure(ersd::PurePrim::IntEql(
            erase(context, left, &int_type())?.into(),
            erase(context, right, &int_type())?.into(),
        ))),
        Prim::IntNeq(left, right) => Ok(pure(ersd::PurePrim::IntNeq(
            erase(context, left, &int_type())?.into(),
            erase(context, right, &int_type())?.into(),
        ))),
        Prim::IntAdd(left, right) => Ok(pure(ersd::PurePrim::IntAdd(
            erase(context, left, &int_type())?.into(),
            erase(context, right, &int_type())?.into(),
        ))),
        Prim::IntSub(left, right) => Ok(pure(ersd::PurePrim::IntSub(
            erase(context, left, &int_type())?.into(),
            erase(context, right, &int_type())?.into(),
        ))),
        Prim::IntMul(left, right) => Ok(pure(ersd::PurePrim::IntMul(
            erase(context, left, &int_type())?.into(),
            erase(context, right, &int_type())?.into(),
        ))),
        Prim::IntDiv(left, right) => Ok(pure(ersd::PurePrim::IntDiv(
            erase(context, left, &int_type())?.into(),
            erase(context, right, &int_type())?.into(),
        ))),
        Prim::IntRem(left, right) => Ok(pure(ersd::PurePrim::IntRem(
            erase(context, left, &int_type())?.into(),
            erase(context, right, &int_type())?.into(),
        ))),
        Prim::IntLt(left, right) => Ok(pure(ersd::PurePrim::IntLt(
            erase(context, left, &int_type())?.into(),
            erase(context, right, &int_type())?.into(),
        ))),
        Prim::IntGt(left, right) => Ok(pure(ersd::PurePrim::IntGt(
            erase(context, left, &int_type())?.into(),
            erase(context, right, &int_type())?.into(),
        ))),
        Prim::IntLte(left, right) => Ok(pure(ersd::PurePrim::IntLte(
            erase(context, left, &int_type())?.into(),
            erase(context, right, &int_type())?.into(),
        ))),
        Prim::IntGte(left, right) => Ok(pure(ersd::PurePrim::IntGte(
            erase(context, left, &int_type())?.into(),
            erase(context, right, &int_type())?.into(),
        ))),
        Prim::FltType => Ok(ersd::Term::Erased),
        &Prim::Flt(flt) => Ok(pure(ersd::PurePrim::Flt(flt.to_f32()))),
        Prim::FltAdd(left, right) => Ok(pure(ersd::PurePrim::FltAdd(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::FltSub(left, right) => Ok(pure(ersd::PurePrim::FltSub(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::FltMul(left, right) => Ok(pure(ersd::PurePrim::FltMul(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::FltNeg(inner) => Ok(pure(ersd::PurePrim::FltNeg(
            erase(context, inner, &flt_type())?.into(),
        ))),
        Prim::FltAbs(inner) => Ok(pure(ersd::PurePrim::FltAbs(
            erase(context, inner, &flt_type())?.into(),
        ))),
        Prim::FltSqrt(inner) => Ok(pure(ersd::PurePrim::FltSqrt(
            erase(context, inner, &flt_type())?.into(),
        ))),
        Prim::FltFloor(inner) => Ok(pure(ersd::PurePrim::FltFloor(
            erase(context, inner, &flt_type())?.into(),
        ))),
        Prim::FltCeil(inner) => Ok(pure(ersd::PurePrim::FltCeil(
            erase(context, inner, &flt_type())?.into(),
        ))),
        Prim::FltTrunc(inner) => Ok(pure(ersd::PurePrim::FltTrunc(
            erase(context, inner, &flt_type())?.into(),
        ))),
        Prim::FltNearest(inner) => Ok(pure(ersd::PurePrim::FltNearest(
            erase(context, inner, &flt_type())?.into(),
        ))),
        Prim::FltDiv(left, right) => Ok(pure(ersd::PurePrim::FltDiv(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::FltMin(left, right) => Ok(pure(ersd::PurePrim::FltMin(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::FltMax(left, right) => Ok(pure(ersd::PurePrim::FltMax(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::FltEql(left, right) => Ok(pure(ersd::PurePrim::FltEql(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::FltNeq(left, right) => Ok(pure(ersd::PurePrim::FltNeq(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::FltLt(left, right) => Ok(pure(ersd::PurePrim::FltLt(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::FltGt(left, right) => Ok(pure(ersd::PurePrim::FltGt(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::FltLte(left, right) => Ok(pure(ersd::PurePrim::FltLte(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::FltGte(left, right) => Ok(pure(ersd::PurePrim::FltGte(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::NatToStr(inner) => Ok(pure(ersd::PurePrim::NatToStr(
            erase(context, inner, &nat_type())?.into(),
        ))),
        Prim::IntToStr(inner) => Ok(pure(ersd::PurePrim::IntToStr(
            erase(context, inner, &int_type())?.into(),
        ))),
        Prim::FltToStr(inner) => Ok(pure(ersd::PurePrim::FltToStr(
            erase(context, inner, &flt_type())?.into(),
        ))),
        Prim::NatToInt(inner) => Ok(pure(ersd::PurePrim::NatToInt(
            erase(context, inner, &nat_type())?.into(),
        ))),
        Prim::IntToNat(inner) => Ok(pure(ersd::PurePrim::IntToNat(
            erase(context, inner, &int_type())?.into(),
        ))),
        Prim::IntToFlt(inner) => Ok(pure(ersd::PurePrim::IntToFlt(
            erase(context, inner, &int_type())?.into(),
        ))),
        Prim::NatToFlt(inner) => Ok(pure(ersd::PurePrim::NatToFlt(
            erase(context, inner, &nat_type())?.into(),
        ))),
        Prim::FltToInt(inner) => Ok(pure(ersd::PurePrim::FltToInt(
            erase(context, inner, &flt_type())?.into(),
        ))),
        Prim::FltToNat(inner) => Ok(pure(ersd::PurePrim::FltToNat(
            erase(context, inner, &flt_type())?.into(),
        ))),
        Prim::BinType => Ok(ersd::Term::Erased),
        Prim::Bin(bytes) => Ok(pure(ersd::PurePrim::Bin(bytes.clone()))),
        Prim::BinLen(bin) => Ok(pure(ersd::PurePrim::BinLen(
            erase(context, bin, &bin_type())?.into(),
        ))),
        Prim::BinEql(left, right) => Ok(pure(ersd::PurePrim::BinEql(
            erase(context, left, &bin_type())?.into(),
            erase(context, right, &bin_type())?.into(),
        ))),
        Prim::BinGet(bin, index) => Ok(pure(ersd::PurePrim::BinGet(
            erase(context, bin, &bin_type())?.into(),
            erase(context, index, &nat_type())?.into(),
        ))),
        Prim::BinSlice(bin, start, end) => Ok(pure(ersd::PurePrim::BinSlice(
            erase(context, bin, &bin_type())?.into(),
            erase(context, start, &nat_type())?.into(),
            erase(context, end, &nat_type())?.into(),
        ))),
        Prim::BinAppend(bin, byte) => Ok(pure(ersd::PurePrim::BinAppend(
            erase(context, bin, &bin_type())?.into(),
            erase(context, byte, &nat_type())?.into(),
        ))),
        Prim::BinConcat(operands) => {
            let erased = operands
                .iter()
                .map(|e| erase(context, e, &bin_type()).map(|t| t.into()))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(pure(ersd::PurePrim::BinConcat(erased)))
        }
        Prim::ArrType(_) => Ok(ersd::Term::Erased),
        Prim::Arr(elems) => {
            // Elaborate already checked this literal against an array type (§9);
            // the element type is re-derived here only to lower the elements.
            let elem_type = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
                Subterm::Prim(Prim::ArrType(elem_type)) => elem_type,
                _ => unreachable!("erase: array literal checked against non-array type"),
            };
            let erased_elems = elems
                .iter()
                .map(|e| erase(context, e, &elem_type).map(|t| t.into()))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(pure(ersd::PurePrim::Arr(erased_elems)))
        }
        Prim::ArrLen(type_, list) => {
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            let list_erased = erase(context, list, &expected_list_type)?;
            Ok(pure(ersd::PurePrim::ArrLen(list_erased.into())))
        }
        Prim::ArrGet(type_, list, index) => {
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            let list_erased = erase(context, list, &expected_list_type)?;
            let index_erased = erase(context, index, &nat_type())?;
            Ok(pure(ersd::PurePrim::ArrGet(
                list_erased.into(),
                index_erased.into(),
            )))
        }
        Prim::ArrSlice(type_, list, start, end) => {
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            let list_erased = erase(context, list, &expected_list_type)?;
            let start_erased = erase(context, start, &nat_type())?;
            let end_erased = erase(context, end, &nat_type())?;
            Ok(pure(ersd::PurePrim::ArrSlice(
                list_erased.into(),
                start_erased.into(),
                end_erased.into(),
            )))
        }
        Prim::ArrAppend(type_, list, elem) => {
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            let list_erased = erase(context, list, &expected_list_type)?;
            let elem_erased = erase(context, elem, type_)?;
            Ok(pure(ersd::PurePrim::ArrAppend(
                list_erased.into(),
                elem_erased.into(),
            )))
        }
        Prim::ArrConcat(type_, operands) => {
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            let erased = operands
                .iter()
                .map(|e| erase(context, e, &expected_list_type).map(|t| t.into()))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(pure(ersd::PurePrim::ArrConcat(erased)))
        }
        Prim::IoPrint(inner) => Ok(host(ersd::HostPrim::IoPrint(
            erase(context, inner, &bin_type())?.into(),
        ))),
        Prim::IoRead => Ok(host(ersd::HostPrim::IoRead)),
    }
}
