use {
    super::erase,
    crate::{
        core::{Context, Error, Nat, Prim, Subterm, Term, expect, infer, reduce_with},
        ersd,
    },
    num_bigint::BigUint,
    num_traits::ToPrimitive,
};

fn narrow_nat(k: &BigUint) -> Result<u32, Error> {
    k.to_u32().ok_or_else(|| Error::nat_overflow(k.clone()))
}

fn type_type() -> Term {
    Term::type_()
}

fn prim_type(prim: Prim) -> Term {
    Subterm::Prim(prim).into()
}

fn bln_type() -> Term {
    prim_type(Prim::BlnType)
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

fn unit_type() -> Term {
    Term::tuple_type_unit()
}

pub fn erase_prim(
    context: &mut Context,
    term: &Term,
    prim: &Prim,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    match prim {
        Prim::BlnType => {
            expect(context, term, &type_type(), expected)?;

            Ok(ersd::Term::Erased)
        }
        &Prim::Bln(value) => {
            expect(context, term, &bln_type(), expected)?;

            Ok(ersd::Prim::Nat(if value { 1 } else { 0 }).into())
        }
        Prim::NatType => {
            expect(context, term, &type_type(), expected)?;

            Ok(ersd::Term::Erased)
        }
        Prim::Nat(Nat::Zero) => {
            expect(context, term, &nat_type(), expected)?;

            Ok(ersd::Prim::Nat(0).into())
        }
        Prim::Nat(Nat::Succ(spine, inner)) => {
            expect(context, term, &nat_type(), expected)?;

            if matches!(inner.as_ref(), Subterm::Prim(Prim::Nat(Nat::Zero))) {
                Ok(ersd::Prim::Nat(narrow_nat(spine)?).into())
            } else {
                let inner_ersd = erase(context, inner, &nat_type())?;
                let spine_term: ersd::Term = ersd::Prim::Nat(narrow_nat(spine)?).into();
                Ok(ersd::Prim::NatAdd(spine_term.into(), inner_ersd.into()).into())
            }
        }
        Prim::NatEql(left, right) => {
            expect(context, term, &bln_type(), expected)?;

            Ok(ersd::Prim::NatEql(
                erase(context, left, &nat_type())?.into(),
                erase(context, right, &nat_type())?.into(),
            )
            .into())
        }
        Prim::NatAdd(left, right) => {
            expect(context, term, &nat_type(), expected)?;

            Ok(ersd::Prim::NatAdd(
                erase(context, left, &nat_type())?.into(),
                erase(context, right, &nat_type())?.into(),
            )
            .into())
        }
        Prim::NatSub(left, right) => {
            expect(context, term, &nat_type(), expected)?;

            Ok(ersd::Prim::NatSub(
                erase(context, left, &nat_type())?.into(),
                erase(context, right, &nat_type())?.into(),
            )
            .into())
        }
        Prim::NatMul(left, right) => {
            expect(context, term, &nat_type(), expected)?;

            Ok(ersd::Prim::NatMul(
                erase(context, left, &nat_type())?.into(),
                erase(context, right, &nat_type())?.into(),
            )
            .into())
        }
        Prim::NatNeq(left, right) => {
            expect(context, term, &bln_type(), expected)?;

            Ok(ersd::Prim::NatNeq(
                erase(context, left, &nat_type())?.into(),
                erase(context, right, &nat_type())?.into(),
            )
            .into())
        }
        Prim::NatDiv(left, right) => {
            expect(context, term, &nat_type(), expected)?;

            Ok(ersd::Prim::NatDiv(
                erase(context, left, &nat_type())?.into(),
                erase(context, right, &nat_type())?.into(),
            )
            .into())
        }
        Prim::NatRem(left, right) => {
            expect(context, term, &nat_type(), expected)?;

            Ok(ersd::Prim::NatRem(
                erase(context, left, &nat_type())?.into(),
                erase(context, right, &nat_type())?.into(),
            )
            .into())
        }
        Prim::NatLt(left, right) => {
            expect(context, term, &bln_type(), expected)?;

            Ok(ersd::Prim::NatLt(
                erase(context, left, &nat_type())?.into(),
                erase(context, right, &nat_type())?.into(),
            )
            .into())
        }
        Prim::NatGt(left, right) => {
            expect(context, term, &bln_type(), expected)?;

            Ok(ersd::Prim::NatGt(
                erase(context, left, &nat_type())?.into(),
                erase(context, right, &nat_type())?.into(),
            )
            .into())
        }
        Prim::NatLte(left, right) => {
            expect(context, term, &bln_type(), expected)?;

            Ok(ersd::Prim::NatLte(
                erase(context, left, &nat_type())?.into(),
                erase(context, right, &nat_type())?.into(),
            )
            .into())
        }
        Prim::NatGte(left, right) => {
            expect(context, term, &bln_type(), expected)?;

            Ok(ersd::Prim::NatGte(
                erase(context, left, &nat_type())?.into(),
                erase(context, right, &nat_type())?.into(),
            )
            .into())
        }
        Prim::IntType => {
            expect(context, term, &type_type(), expected)?;

            Ok(ersd::Term::Erased)
        }
        &Prim::Int(value) => {
            expect(context, term, &int_type(), expected)?;

            Ok(ersd::Prim::Int(value.to_i32()).into())
        }
        Prim::IntEql(left, right) => {
            expect(context, term, &bln_type(), expected)?;

            Ok(ersd::Prim::IntEql(
                erase(context, left, &int_type())?.into(),
                erase(context, right, &int_type())?.into(),
            )
            .into())
        }
        Prim::IntNeq(left, right) => {
            expect(context, term, &bln_type(), expected)?;

            Ok(ersd::Prim::IntNeq(
                erase(context, left, &int_type())?.into(),
                erase(context, right, &int_type())?.into(),
            )
            .into())
        }
        Prim::IntAdd(left, right) => {
            expect(context, term, &int_type(), expected)?;

            Ok(ersd::Prim::IntAdd(
                erase(context, left, &int_type())?.into(),
                erase(context, right, &int_type())?.into(),
            )
            .into())
        }
        Prim::IntSub(left, right) => {
            expect(context, term, &int_type(), expected)?;

            Ok(ersd::Prim::IntSub(
                erase(context, left, &int_type())?.into(),
                erase(context, right, &int_type())?.into(),
            )
            .into())
        }
        Prim::IntMul(left, right) => {
            expect(context, term, &int_type(), expected)?;

            Ok(ersd::Prim::IntMul(
                erase(context, left, &int_type())?.into(),
                erase(context, right, &int_type())?.into(),
            )
            .into())
        }
        Prim::IntDiv(left, right) => {
            expect(context, term, &int_type(), expected)?;

            Ok(ersd::Prim::IntDiv(
                erase(context, left, &int_type())?.into(),
                erase(context, right, &int_type())?.into(),
            )
            .into())
        }
        Prim::IntRem(left, right) => {
            expect(context, term, &int_type(), expected)?;

            Ok(ersd::Prim::IntRem(
                erase(context, left, &int_type())?.into(),
                erase(context, right, &int_type())?.into(),
            )
            .into())
        }
        Prim::IntLt(left, right) => {
            expect(context, term, &bln_type(), expected)?;

            Ok(ersd::Prim::IntLt(
                erase(context, left, &int_type())?.into(),
                erase(context, right, &int_type())?.into(),
            )
            .into())
        }
        Prim::IntGt(left, right) => {
            expect(context, term, &bln_type(), expected)?;

            Ok(ersd::Prim::IntGt(
                erase(context, left, &int_type())?.into(),
                erase(context, right, &int_type())?.into(),
            )
            .into())
        }
        Prim::IntLte(left, right) => {
            expect(context, term, &bln_type(), expected)?;

            Ok(ersd::Prim::IntLte(
                erase(context, left, &int_type())?.into(),
                erase(context, right, &int_type())?.into(),
            )
            .into())
        }
        Prim::IntGte(left, right) => {
            expect(context, term, &bln_type(), expected)?;

            Ok(ersd::Prim::IntGte(
                erase(context, left, &int_type())?.into(),
                erase(context, right, &int_type())?.into(),
            )
            .into())
        }
        Prim::FltType => {
            expect(context, term, &type_type(), expected)?;

            Ok(ersd::Term::Erased)
        }
        &Prim::Flt(flt) => {
            expect(context, term, &flt_type(), expected)?;

            Ok(ersd::Prim::Flt(flt.to_f32()).into())
        }
        Prim::FltAdd(left, right) => {
            expect(context, term, &flt_type(), expected)?;

            Ok(ersd::Prim::FltAdd(
                erase(context, left, &flt_type())?.into(),
                erase(context, right, &flt_type())?.into(),
            )
            .into())
        }
        Prim::FltSub(left, right) => {
            expect(context, term, &flt_type(), expected)?;

            Ok(ersd::Prim::FltSub(
                erase(context, left, &flt_type())?.into(),
                erase(context, right, &flt_type())?.into(),
            )
            .into())
        }
        Prim::FltMul(left, right) => {
            expect(context, term, &flt_type(), expected)?;

            Ok(ersd::Prim::FltMul(
                erase(context, left, &flt_type())?.into(),
                erase(context, right, &flt_type())?.into(),
            )
            .into())
        }
        Prim::FltNeg(inner) => {
            expect(context, term, &flt_type(), expected)?;

            Ok(ersd::Prim::FltNeg(erase(context, inner, &flt_type())?.into()).into())
        }
        Prim::FltAbs(inner) => {
            expect(context, term, &flt_type(), expected)?;

            Ok(ersd::Prim::FltAbs(erase(context, inner, &flt_type())?.into()).into())
        }
        Prim::FltSqrt(inner) => {
            expect(context, term, &flt_type(), expected)?;

            Ok(ersd::Prim::FltSqrt(erase(context, inner, &flt_type())?.into()).into())
        }
        Prim::FltFloor(inner) => {
            expect(context, term, &flt_type(), expected)?;

            Ok(ersd::Prim::FltFloor(erase(context, inner, &flt_type())?.into()).into())
        }
        Prim::FltCeil(inner) => {
            expect(context, term, &flt_type(), expected)?;

            Ok(ersd::Prim::FltCeil(erase(context, inner, &flt_type())?.into()).into())
        }
        Prim::FltTrunc(inner) => {
            expect(context, term, &flt_type(), expected)?;

            Ok(ersd::Prim::FltTrunc(erase(context, inner, &flt_type())?.into()).into())
        }
        Prim::FltNearest(inner) => {
            expect(context, term, &flt_type(), expected)?;

            Ok(ersd::Prim::FltNearest(erase(context, inner, &flt_type())?.into()).into())
        }
        Prim::FltDiv(left, right) => {
            expect(context, term, &flt_type(), expected)?;

            Ok(ersd::Prim::FltDiv(
                erase(context, left, &flt_type())?.into(),
                erase(context, right, &flt_type())?.into(),
            )
            .into())
        }
        Prim::FltMin(left, right) => {
            expect(context, term, &flt_type(), expected)?;

            Ok(ersd::Prim::FltMin(
                erase(context, left, &flt_type())?.into(),
                erase(context, right, &flt_type())?.into(),
            )
            .into())
        }
        Prim::FltMax(left, right) => {
            expect(context, term, &flt_type(), expected)?;

            Ok(ersd::Prim::FltMax(
                erase(context, left, &flt_type())?.into(),
                erase(context, right, &flt_type())?.into(),
            )
            .into())
        }
        Prim::FltEql(left, right) => {
            expect(context, term, &bln_type(), expected)?;

            Ok(ersd::Prim::FltEql(
                erase(context, left, &flt_type())?.into(),
                erase(context, right, &flt_type())?.into(),
            )
            .into())
        }
        Prim::FltNeq(left, right) => {
            expect(context, term, &bln_type(), expected)?;

            Ok(ersd::Prim::FltNeq(
                erase(context, left, &flt_type())?.into(),
                erase(context, right, &flt_type())?.into(),
            )
            .into())
        }
        Prim::FltLt(left, right) => {
            expect(context, term, &bln_type(), expected)?;

            Ok(ersd::Prim::FltLt(
                erase(context, left, &flt_type())?.into(),
                erase(context, right, &flt_type())?.into(),
            )
            .into())
        }
        Prim::FltGt(left, right) => {
            expect(context, term, &bln_type(), expected)?;

            Ok(ersd::Prim::FltGt(
                erase(context, left, &flt_type())?.into(),
                erase(context, right, &flt_type())?.into(),
            )
            .into())
        }
        Prim::FltLte(left, right) => {
            expect(context, term, &bln_type(), expected)?;

            Ok(ersd::Prim::FltLte(
                erase(context, left, &flt_type())?.into(),
                erase(context, right, &flt_type())?.into(),
            )
            .into())
        }
        Prim::FltGte(left, right) => {
            expect(context, term, &bln_type(), expected)?;

            Ok(ersd::Prim::FltGte(
                erase(context, left, &flt_type())?.into(),
                erase(context, right, &flt_type())?.into(),
            )
            .into())
        }
        Prim::NatToStr(inner) => {
            expect(context, term, &bin_type(), expected)?;

            Ok(ersd::Prim::NatToStr(erase(context, inner, &nat_type())?.into()).into())
        }
        Prim::IntToStr(inner) => {
            expect(context, term, &bin_type(), expected)?;

            Ok(ersd::Prim::IntToStr(erase(context, inner, &int_type())?.into()).into())
        }
        Prim::FltToStr(inner) => {
            expect(context, term, &bin_type(), expected)?;

            Ok(ersd::Prim::FltToStr(erase(context, inner, &flt_type())?.into()).into())
        }
        Prim::NatToInt(inner) => {
            expect(context, term, &int_type(), expected)?;

            Ok(ersd::Prim::NatToInt(erase(context, inner, &nat_type())?.into()).into())
        }
        Prim::IntToNat(inner) => {
            expect(context, term, &nat_type(), expected)?;

            Ok(ersd::Prim::IntToNat(erase(context, inner, &int_type())?.into()).into())
        }
        Prim::IntToFlt(inner) => {
            expect(context, term, &flt_type(), expected)?;

            Ok(ersd::Prim::IntToFlt(erase(context, inner, &int_type())?.into()).into())
        }
        Prim::NatToFlt(inner) => {
            expect(context, term, &flt_type(), expected)?;

            Ok(ersd::Prim::NatToFlt(erase(context, inner, &nat_type())?.into()).into())
        }
        Prim::FltToInt(inner) => {
            expect(context, term, &int_type(), expected)?;

            Ok(ersd::Prim::FltToInt(erase(context, inner, &flt_type())?.into()).into())
        }
        Prim::FltToNat(inner) => {
            expect(context, term, &nat_type(), expected)?;

            Ok(ersd::Prim::FltToNat(erase(context, inner, &flt_type())?.into()).into())
        }
        Prim::BinType => {
            expect(context, term, &type_type(), expected)?;
            Ok(ersd::Term::Erased)
        }
        Prim::Bin(bytes) => {
            expect(context, term, &bin_type(), expected)?;
            Ok(ersd::Prim::Bin(bytes.clone()).into())
        }
        Prim::BinLen(bin) => {
            expect(context, term, &nat_type(), expected)?;
            let bin_type = infer(context, bin)?;
            let bin_type_reduced = reduce_with(context, &bin_type)?;
            match &*bin_type_reduced {
                Subterm::Prim(Prim::BinType) => {}
                other => {
                    return Err(Error::type_mismatch(
                        term.clone(),
                        other.clone(),
                        Subterm::Prim(Prim::BinType),
                    ));
                }
            }
            Ok(ersd::Prim::BinLen(erase(context, bin, &bin_type_reduced)?.into()).into())
        }
        Prim::BinEql(left, right) => {
            expect(context, term, &bln_type(), expected)?;

            Ok(ersd::Prim::BinEql(
                erase(context, left, &bin_type())?.into(),
                erase(context, right, &bin_type())?.into(),
            )
            .into())
        }
        Prim::BinGet(bin, index) => {
            expect(context, term, &nat_type(), expected)?;
            let bin_type = infer(context, bin)?;
            let bin_type_reduced = reduce_with(context, &bin_type)?;
            match &*bin_type_reduced {
                Subterm::Prim(Prim::BinType) => {}
                other => {
                    return Err(Error::type_mismatch(
                        term.clone(),
                        other.clone(),
                        Subterm::Prim(Prim::BinType),
                    ));
                }
            }
            Ok(ersd::Prim::BinGet(
                erase(context, bin, &bin_type_reduced)?.into(),
                erase(context, index, &nat_type())?.into(),
            )
            .into())
        }
        Prim::BinSlice(bin, start, end) => {
            let bin_type = infer(context, bin)?;
            let bin_type_reduced = reduce_with(context, &bin_type)?;
            match &*bin_type_reduced {
                Subterm::Prim(Prim::BinType) => {}
                other => {
                    return Err(Error::type_mismatch(
                        term.clone(),
                        other.clone(),
                        Subterm::Prim(Prim::BinType),
                    ));
                }
            }
            expect(context, term, &bin_type_reduced, expected)?;
            Ok(ersd::Prim::BinSlice(
                erase(context, bin, &bin_type_reduced)?.into(),
                erase(context, start, &nat_type())?.into(),
                erase(context, end, &nat_type())?.into(),
            )
            .into())
        }
        Prim::BinAppend(bin, byte) => {
            let bin_type = infer(context, bin)?;
            let bin_type_reduced = reduce_with(context, &bin_type)?;
            match &*bin_type_reduced {
                Subterm::Prim(Prim::BinType) => {}
                other => {
                    return Err(Error::type_mismatch(
                        term.clone(),
                        other.clone(),
                        Subterm::Prim(Prim::BinType),
                    ));
                }
            }
            expect(context, term, &bin_type_reduced, expected)?;
            Ok(ersd::Prim::BinAppend(
                erase(context, bin, &bin_type_reduced)?.into(),
                erase(context, byte, &nat_type())?.into(),
            )
            .into())
        }
        Prim::BinConcat(operands) => {
            expect(context, term, &bin_type(), expected)?;
            let erased = operands
                .iter()
                .map(|e| erase(context, e, &bin_type()).map(|t| t.into()))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(ersd::Prim::BinConcat(erased).into())
        }
        Prim::ArrType(elem) => {
            expect(context, term, &type_type(), expected)?;
            erase(context, elem, &type_type())?;
            Ok(ersd::Term::Erased)
        }
        Prim::Arr(elems) => {
            let elem_type = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
                Subterm::Prim(Prim::ArrType(elem_type)) => elem_type,
                other => return Err(Error::type_mismatch(term.clone(), other, expected.clone())),
            };
            let erased_elems = elems
                .iter()
                .map(|e| erase(context, e, &elem_type).map(|t| t.into()))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(ersd::Prim::Arr(erased_elems).into())
        }
        Prim::ArrLen(type_, list) => {
            expect(context, term, &nat_type(), expected)?;
            erase(context, type_, &type_type())?;
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            let list_erased = erase(context, list, &expected_list_type)?;
            Ok(ersd::Prim::ArrLen(list_erased.into()).into())
        }
        Prim::ArrGet(type_, list, index) => {
            expect(context, term, type_, expected)?;
            erase(context, type_, &type_type())?;
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            let list_erased = erase(context, list, &expected_list_type)?;
            let index_erased = erase(context, index, &nat_type())?;
            Ok(ersd::Prim::ArrGet(list_erased.into(), index_erased.into()).into())
        }
        Prim::ArrSlice(type_, list, start, end) => {
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            expect(context, term, &expected_list_type, expected)?;
            erase(context, type_, &type_type())?;
            let list_erased = erase(context, list, &expected_list_type)?;
            let start_erased = erase(context, start, &nat_type())?;
            let end_erased = erase(context, end, &nat_type())?;
            Ok(
                ersd::Prim::ArrSlice(list_erased.into(), start_erased.into(), end_erased.into())
                    .into(),
            )
        }
        Prim::ArrAppend(type_, list, elem) => {
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            expect(context, term, &expected_list_type, expected)?;
            erase(context, type_, &type_type())?;
            let list_erased = erase(context, list, &expected_list_type)?;
            let elem_erased = erase(context, elem, type_)?;
            Ok(ersd::Prim::ArrAppend(list_erased.into(), elem_erased.into()).into())
        }
        Prim::ArrConcat(type_, operands) => {
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            expect(context, term, &expected_list_type, expected)?;
            erase(context, type_, &type_type())?;
            let erased = operands
                .iter()
                .map(|e| erase(context, e, &expected_list_type).map(|t| t.into()))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(ersd::Prim::ArrConcat(erased).into())
        }
        Prim::IoPrint(inner) => {
            expect(context, term, &unit_type(), expected)?;
            Ok(ersd::Prim::IoPrint(erase(context, inner, &bin_type())?.into()).into())
        }
        Prim::IoRead => {
            expect(context, term, &bin_type(), expected)?;
            Ok(ersd::Prim::IoRead.into())
        }
    }
}
