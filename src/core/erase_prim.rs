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
        &Prim::Bln(value) => Ok(ersd::Term::Prim(ersd::Prim::Nat(if value { 1 } else { 0 }))),
        Prim::NatType => Ok(ersd::Term::Erased),
        Prim::Nat(Nat::Zero) => Ok(ersd::Term::Prim(ersd::Prim::Nat(0))),
        Prim::Nat(Nat::Succ(spine, inner)) => {
            if matches!(inner.as_ref(), Subterm::Prim(Prim::Nat(Nat::Zero))) {
                Ok(ersd::Term::Prim(ersd::Prim::Nat(narrow_nat(spine)?)))
            } else {
                let inner_ersd = erase(context, inner, &nat_type())?;
                let spine_term = ersd::Term::Prim(ersd::Prim::Nat(narrow_nat(spine)?));
                Ok(ersd::Term::Prim(ersd::Prim::NatAdd(
                    spine_term.into(),
                    inner_ersd.into(),
                )))
            }
        }
        Prim::NatEql(left, right) => Ok(ersd::Term::Prim(ersd::Prim::NatEql(
            erase(context, left, &nat_type())?.into(),
            erase(context, right, &nat_type())?.into(),
        ))),
        Prim::NatAdd(left, right) => Ok(ersd::Term::Prim(ersd::Prim::NatAdd(
            erase(context, left, &nat_type())?.into(),
            erase(context, right, &nat_type())?.into(),
        ))),
        Prim::NatSub(left, right) => Ok(ersd::Term::Prim(ersd::Prim::NatSub(
            erase(context, left, &nat_type())?.into(),
            erase(context, right, &nat_type())?.into(),
        ))),
        Prim::NatMul(left, right) => Ok(ersd::Term::Prim(ersd::Prim::NatMul(
            erase(context, left, &nat_type())?.into(),
            erase(context, right, &nat_type())?.into(),
        ))),
        Prim::NatNeq(left, right) => Ok(ersd::Term::Prim(ersd::Prim::NatNeq(
            erase(context, left, &nat_type())?.into(),
            erase(context, right, &nat_type())?.into(),
        ))),
        Prim::NatDiv(left, right) => Ok(ersd::Term::Prim(ersd::Prim::NatDiv(
            erase(context, left, &nat_type())?.into(),
            erase(context, right, &nat_type())?.into(),
        ))),
        Prim::NatRem(left, right) => Ok(ersd::Term::Prim(ersd::Prim::NatRem(
            erase(context, left, &nat_type())?.into(),
            erase(context, right, &nat_type())?.into(),
        ))),
        Prim::NatLt(left, right) => Ok(ersd::Term::Prim(ersd::Prim::NatLt(
            erase(context, left, &nat_type())?.into(),
            erase(context, right, &nat_type())?.into(),
        ))),
        Prim::NatGt(left, right) => Ok(ersd::Term::Prim(ersd::Prim::NatGt(
            erase(context, left, &nat_type())?.into(),
            erase(context, right, &nat_type())?.into(),
        ))),
        Prim::NatLte(left, right) => Ok(ersd::Term::Prim(ersd::Prim::NatLte(
            erase(context, left, &nat_type())?.into(),
            erase(context, right, &nat_type())?.into(),
        ))),
        Prim::NatGte(left, right) => Ok(ersd::Term::Prim(ersd::Prim::NatGte(
            erase(context, left, &nat_type())?.into(),
            erase(context, right, &nat_type())?.into(),
        ))),
        Prim::IntType => Ok(ersd::Term::Erased),
        &Prim::Int(value) => Ok(ersd::Term::Prim(ersd::Prim::Int(value.to_i32()))),
        Prim::IntEql(left, right) => Ok(ersd::Term::Prim(ersd::Prim::IntEql(
            erase(context, left, &int_type())?.into(),
            erase(context, right, &int_type())?.into(),
        ))),
        Prim::IntNeq(left, right) => Ok(ersd::Term::Prim(ersd::Prim::IntNeq(
            erase(context, left, &int_type())?.into(),
            erase(context, right, &int_type())?.into(),
        ))),
        Prim::IntAdd(left, right) => Ok(ersd::Term::Prim(ersd::Prim::IntAdd(
            erase(context, left, &int_type())?.into(),
            erase(context, right, &int_type())?.into(),
        ))),
        Prim::IntSub(left, right) => Ok(ersd::Term::Prim(ersd::Prim::IntSub(
            erase(context, left, &int_type())?.into(),
            erase(context, right, &int_type())?.into(),
        ))),
        Prim::IntMul(left, right) => Ok(ersd::Term::Prim(ersd::Prim::IntMul(
            erase(context, left, &int_type())?.into(),
            erase(context, right, &int_type())?.into(),
        ))),
        Prim::IntDiv(left, right) => Ok(ersd::Term::Prim(ersd::Prim::IntDiv(
            erase(context, left, &int_type())?.into(),
            erase(context, right, &int_type())?.into(),
        ))),
        Prim::IntRem(left, right) => Ok(ersd::Term::Prim(ersd::Prim::IntRem(
            erase(context, left, &int_type())?.into(),
            erase(context, right, &int_type())?.into(),
        ))),
        Prim::IntLt(left, right) => Ok(ersd::Term::Prim(ersd::Prim::IntLt(
            erase(context, left, &int_type())?.into(),
            erase(context, right, &int_type())?.into(),
        ))),
        Prim::IntGt(left, right) => Ok(ersd::Term::Prim(ersd::Prim::IntGt(
            erase(context, left, &int_type())?.into(),
            erase(context, right, &int_type())?.into(),
        ))),
        Prim::IntLte(left, right) => Ok(ersd::Term::Prim(ersd::Prim::IntLte(
            erase(context, left, &int_type())?.into(),
            erase(context, right, &int_type())?.into(),
        ))),
        Prim::IntGte(left, right) => Ok(ersd::Term::Prim(ersd::Prim::IntGte(
            erase(context, left, &int_type())?.into(),
            erase(context, right, &int_type())?.into(),
        ))),
        Prim::FltType => Ok(ersd::Term::Erased),
        &Prim::Flt(flt) => Ok(ersd::Term::Prim(ersd::Prim::Flt(flt.to_f32()))),
        Prim::FltAdd(left, right) => Ok(ersd::Term::Prim(ersd::Prim::FltAdd(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::FltSub(left, right) => Ok(ersd::Term::Prim(ersd::Prim::FltSub(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::FltMul(left, right) => Ok(ersd::Term::Prim(ersd::Prim::FltMul(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::FltNeg(inner) => Ok(ersd::Term::Prim(ersd::Prim::FltNeg(
            erase(context, inner, &flt_type())?.into(),
        ))),
        Prim::FltAbs(inner) => Ok(ersd::Term::Prim(ersd::Prim::FltAbs(
            erase(context, inner, &flt_type())?.into(),
        ))),
        Prim::FltSqrt(inner) => Ok(ersd::Term::Prim(ersd::Prim::FltSqrt(
            erase(context, inner, &flt_type())?.into(),
        ))),
        Prim::FltFloor(inner) => Ok(ersd::Term::Prim(ersd::Prim::FltFloor(
            erase(context, inner, &flt_type())?.into(),
        ))),
        Prim::FltCeil(inner) => Ok(ersd::Term::Prim(ersd::Prim::FltCeil(
            erase(context, inner, &flt_type())?.into(),
        ))),
        Prim::FltTrunc(inner) => Ok(ersd::Term::Prim(ersd::Prim::FltTrunc(
            erase(context, inner, &flt_type())?.into(),
        ))),
        Prim::FltNearest(inner) => Ok(ersd::Term::Prim(ersd::Prim::FltNearest(
            erase(context, inner, &flt_type())?.into(),
        ))),
        Prim::FltDiv(left, right) => Ok(ersd::Term::Prim(ersd::Prim::FltDiv(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::FltMin(left, right) => Ok(ersd::Term::Prim(ersd::Prim::FltMin(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::FltMax(left, right) => Ok(ersd::Term::Prim(ersd::Prim::FltMax(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::FltEql(left, right) => Ok(ersd::Term::Prim(ersd::Prim::FltEql(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::FltNeq(left, right) => Ok(ersd::Term::Prim(ersd::Prim::FltNeq(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::FltLt(left, right) => Ok(ersd::Term::Prim(ersd::Prim::FltLt(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::FltGt(left, right) => Ok(ersd::Term::Prim(ersd::Prim::FltGt(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::FltLte(left, right) => Ok(ersd::Term::Prim(ersd::Prim::FltLte(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::FltGte(left, right) => Ok(ersd::Term::Prim(ersd::Prim::FltGte(
            erase(context, left, &flt_type())?.into(),
            erase(context, right, &flt_type())?.into(),
        ))),
        Prim::NatToStr(inner) => Ok(ersd::Term::Prim(ersd::Prim::NatToStr(
            erase(context, inner, &nat_type())?.into(),
        ))),
        Prim::IntToStr(inner) => Ok(ersd::Term::Prim(ersd::Prim::IntToStr(
            erase(context, inner, &int_type())?.into(),
        ))),
        Prim::FltToStr(inner) => Ok(ersd::Term::Prim(ersd::Prim::FltToStr(
            erase(context, inner, &flt_type())?.into(),
        ))),
        Prim::NatToInt(inner) => Ok(ersd::Term::Prim(ersd::Prim::NatToInt(
            erase(context, inner, &nat_type())?.into(),
        ))),
        Prim::IntToNat(inner) => Ok(ersd::Term::Prim(ersd::Prim::IntToNat(
            erase(context, inner, &int_type())?.into(),
        ))),
        Prim::IntToFlt(inner) => Ok(ersd::Term::Prim(ersd::Prim::IntToFlt(
            erase(context, inner, &int_type())?.into(),
        ))),
        Prim::NatToFlt(inner) => Ok(ersd::Term::Prim(ersd::Prim::NatToFlt(
            erase(context, inner, &nat_type())?.into(),
        ))),
        Prim::FltToInt(inner) => Ok(ersd::Term::Prim(ersd::Prim::FltToInt(
            erase(context, inner, &flt_type())?.into(),
        ))),
        Prim::FltToNat(inner) => Ok(ersd::Term::Prim(ersd::Prim::FltToNat(
            erase(context, inner, &flt_type())?.into(),
        ))),
        Prim::BinType => Ok(ersd::Term::Erased),
        Prim::Bin(bytes) => Ok(ersd::Term::Prim(ersd::Prim::Bin(bytes.clone()))),
        Prim::BinLen(bin) => Ok(ersd::Term::Prim(ersd::Prim::BinLen(
            erase(context, bin, &bin_type())?.into(),
        ))),
        Prim::BinEql(left, right) => Ok(ersd::Term::Prim(ersd::Prim::BinEql(
            erase(context, left, &bin_type())?.into(),
            erase(context, right, &bin_type())?.into(),
        ))),
        Prim::BinGet(bin, index) => Ok(ersd::Term::Prim(ersd::Prim::BinGet(
            erase(context, bin, &bin_type())?.into(),
            erase(context, index, &nat_type())?.into(),
        ))),
        Prim::BinSlice(bin, start, end) => Ok(ersd::Term::Prim(ersd::Prim::BinSlice(
            erase(context, bin, &bin_type())?.into(),
            erase(context, start, &nat_type())?.into(),
            erase(context, end, &nat_type())?.into(),
        ))),
        Prim::BinAppend(bin, byte) => Ok(ersd::Term::Prim(ersd::Prim::BinAppend(
            erase(context, bin, &bin_type())?.into(),
            erase(context, byte, &nat_type())?.into(),
        ))),
        Prim::BinConcat(operands) => {
            let erased = operands
                .iter()
                .map(|e| erase(context, e, &bin_type()).map(|t| t.into()))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(ersd::Term::Prim(ersd::Prim::BinConcat(erased)))
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
            Ok(ersd::Term::Prim(ersd::Prim::Arr(erased_elems)))
        }
        Prim::ArrLen(type_, list) => {
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            let list_erased = erase(context, list, &expected_list_type)?;
            Ok(ersd::Term::Prim(ersd::Prim::ArrLen(list_erased.into())))
        }
        Prim::ArrGet(type_, list, index) => {
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            let list_erased = erase(context, list, &expected_list_type)?;
            let index_erased = erase(context, index, &nat_type())?;
            Ok(ersd::Term::Prim(ersd::Prim::ArrGet(
                list_erased.into(),
                index_erased.into(),
            )))
        }
        Prim::ArrSlice(type_, list, start, end) => {
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            let list_erased = erase(context, list, &expected_list_type)?;
            let start_erased = erase(context, start, &nat_type())?;
            let end_erased = erase(context, end, &nat_type())?;
            Ok(ersd::Term::Prim(ersd::Prim::ArrSlice(
                list_erased.into(),
                start_erased.into(),
                end_erased.into(),
            )))
        }
        Prim::ArrAppend(type_, list, elem) => {
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            let list_erased = erase(context, list, &expected_list_type)?;
            let elem_erased = erase(context, elem, type_)?;
            Ok(ersd::Term::Prim(ersd::Prim::ArrAppend(
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
            Ok(ersd::Term::Prim(ersd::Prim::ArrConcat(erased)))
        }
        Prim::IoPrint(inner) => Ok(ersd::Term::Prim(ersd::Prim::IoPrint(
            erase(context, inner, &bin_type())?.into(),
        ))),
        Prim::IoRead => Ok(ersd::Term::Prim(ersd::Prim::IoRead)),
    }
}
