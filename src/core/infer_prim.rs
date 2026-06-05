use super::{Context, Error, Prim, Subterm, Term, erase, infer, reduce_with};

pub fn infer_prim(context: &mut Context, prim: &Prim) -> Result<Term, Error> {
    match prim {
        Prim::BlnType => Ok(Term::type_()),
        Prim::Bln(_) => Ok(Subterm::Prim(Prim::BlnType).into()),
        Prim::NatType => Ok(Term::type_()),
        Prim::Nat(_) => Ok(Subterm::Prim(Prim::NatType).into()),
        Prim::NatEql(left, right)
        | Prim::NatNeq(left, right)
        | Prim::NatLt(left, right)
        | Prim::NatGt(left, right)
        | Prim::NatLte(left, right)
        | Prim::NatGte(left, right) => {
            erase(context, left, &Subterm::Prim(Prim::NatType).into())?;
            erase(context, right, &Subterm::Prim(Prim::NatType).into())?;

            Ok(Subterm::Prim(Prim::BlnType).into())
        }
        Prim::NatAdd(left, right)
        | Prim::NatSub(left, right)
        | Prim::NatMul(left, right)
        | Prim::NatDiv(left, right)
        | Prim::NatRem(left, right) => {
            erase(context, left, &Subterm::Prim(Prim::NatType).into())?;
            erase(context, right, &Subterm::Prim(Prim::NatType).into())?;

            Ok(Subterm::Prim(Prim::NatType).into())
        }
        Prim::IntType => Ok(Term::type_()),
        Prim::Int(_) => Ok(Subterm::Prim(Prim::IntType).into()),
        Prim::IntEql(left, right)
        | Prim::IntNeq(left, right)
        | Prim::IntLt(left, right)
        | Prim::IntGt(left, right)
        | Prim::IntLte(left, right)
        | Prim::IntGte(left, right) => {
            erase(context, left, &Subterm::Prim(Prim::IntType).into())?;
            erase(context, right, &Subterm::Prim(Prim::IntType).into())?;

            Ok(Subterm::Prim(Prim::BlnType).into())
        }
        Prim::IntAdd(left, right)
        | Prim::IntSub(left, right)
        | Prim::IntMul(left, right)
        | Prim::IntDiv(left, right)
        | Prim::IntRem(left, right) => {
            erase(context, left, &Subterm::Prim(Prim::IntType).into())?;
            erase(context, right, &Subterm::Prim(Prim::IntType).into())?;

            Ok(Subterm::Prim(Prim::IntType).into())
        }
        Prim::FltType => Ok(Term::type_()),
        Prim::Flt(_) => Ok(Subterm::Prim(Prim::FltType).into()),
        Prim::FltAdd(left, right)
        | Prim::FltSub(left, right)
        | Prim::FltMul(left, right)
        | Prim::FltDiv(left, right)
        | Prim::FltMin(left, right)
        | Prim::FltMax(left, right) => {
            erase(context, left, &Subterm::Prim(Prim::FltType).into())?;
            erase(context, right, &Subterm::Prim(Prim::FltType).into())?;

            Ok(Subterm::Prim(Prim::FltType).into())
        }
        Prim::FltNeg(inner)
        | Prim::FltAbs(inner)
        | Prim::FltSqrt(inner)
        | Prim::FltFloor(inner)
        | Prim::FltCeil(inner)
        | Prim::FltTrunc(inner)
        | Prim::FltNearest(inner) => {
            erase(context, inner, &Subterm::Prim(Prim::FltType).into())?;

            Ok(Subterm::Prim(Prim::FltType).into())
        }
        Prim::FltEql(left, right)
        | Prim::FltNeq(left, right)
        | Prim::FltLt(left, right)
        | Prim::FltGt(left, right)
        | Prim::FltLte(left, right)
        | Prim::FltGte(left, right) => {
            erase(context, left, &Subterm::Prim(Prim::FltType).into())?;
            erase(context, right, &Subterm::Prim(Prim::FltType).into())?;

            Ok(Subterm::Prim(Prim::BlnType).into())
        }
        Prim::NatToStr(inner) => {
            erase(context, inner, &Subterm::Prim(Prim::NatType).into())?;

            Ok(Subterm::Prim(Prim::BinType).into())
        }
        Prim::IntToStr(inner) => {
            erase(context, inner, &Subterm::Prim(Prim::IntType).into())?;

            Ok(Subterm::Prim(Prim::BinType).into())
        }
        Prim::FltToStr(inner) => {
            erase(context, inner, &Subterm::Prim(Prim::FltType).into())?;

            Ok(Subterm::Prim(Prim::BinType).into())
        }
        Prim::NatToInt(inner) => {
            erase(context, inner, &Subterm::Prim(Prim::NatType).into())?;

            Ok(Subterm::Prim(Prim::IntType).into())
        }
        Prim::NatToFlt(inner) => {
            erase(context, inner, &Subterm::Prim(Prim::NatType).into())?;

            Ok(Subterm::Prim(Prim::FltType).into())
        }
        Prim::IntToNat(inner) => {
            erase(context, inner, &Subterm::Prim(Prim::IntType).into())?;

            Ok(Subterm::Prim(Prim::NatType).into())
        }
        Prim::IntToFlt(inner) => {
            erase(context, inner, &Subterm::Prim(Prim::IntType).into())?;

            Ok(Subterm::Prim(Prim::FltType).into())
        }
        Prim::FltToNat(inner) => {
            erase(context, inner, &Subterm::Prim(Prim::FltType).into())?;

            Ok(Subterm::Prim(Prim::NatType).into())
        }
        Prim::FltToInt(inner) => {
            erase(context, inner, &Subterm::Prim(Prim::FltType).into())?;

            Ok(Subterm::Prim(Prim::IntType).into())
        }
        Prim::BinType => Ok(Term::type_()),
        Prim::Bin(_) => Ok(Subterm::Prim(Prim::BinType).into()),
        Prim::BinLen(bin) => {
            let bin_type = infer(context, bin)?;
            let bin_type = reduce_with(context, &bin_type)?;
            match &*bin_type {
                Subterm::Prim(Prim::BinType) => Ok(Subterm::Prim(Prim::NatType).into()),
                other => Err(Error::type_mismatch(
                    Subterm::Prim(prim.clone()),
                    other.clone(),
                    Subterm::Prim(Prim::BinType),
                )),
            }
        }
        Prim::BinEql(left, right) => {
            erase(context, left, &Subterm::Prim(Prim::BinType).into())?;
            erase(context, right, &Subterm::Prim(Prim::BinType).into())?;

            Ok(Subterm::Prim(Prim::BlnType).into())
        }
        Prim::BinGet(bin, index) => {
            let bin_type = infer(context, bin)?;
            let bin_type = reduce_with(context, &bin_type)?;
            match &*bin_type {
                Subterm::Prim(Prim::BinType) => {
                    erase(context, index, &Subterm::Prim(Prim::NatType).into())?;
                    Ok(Subterm::Prim(Prim::NatType).into())
                }
                other => Err(Error::type_mismatch(
                    Subterm::Prim(prim.clone()),
                    other.clone(),
                    Subterm::Prim(Prim::BinType),
                )),
            }
        }
        Prim::BinSlice(bin, start, end) => {
            let bin_type = infer(context, bin)?;
            let bin_type = reduce_with(context, &bin_type)?;
            match &*bin_type {
                Subterm::Prim(Prim::BinType) => {
                    erase(context, start, &Subterm::Prim(Prim::NatType).into())?;
                    erase(context, end, &Subterm::Prim(Prim::NatType).into())?;
                    Ok(Subterm::Prim(Prim::BinType).into())
                }
                other => Err(Error::type_mismatch(
                    Subterm::Prim(prim.clone()),
                    other.clone(),
                    Subterm::Prim(Prim::BinType),
                )),
            }
        }
        Prim::BinAppend(bin, byte) => {
            let bin_type = infer(context, bin)?;
            let bin_type = reduce_with(context, &bin_type)?;
            match &*bin_type {
                Subterm::Prim(Prim::BinType) => {
                    erase(context, byte, &Subterm::Prim(Prim::NatType).into())?;
                    Ok(Subterm::Prim(Prim::BinType).into())
                }
                other => Err(Error::type_mismatch(
                    Subterm::Prim(prim.clone()),
                    other.clone(),
                    Subterm::Prim(Prim::BinType),
                )),
            }
        }
        Prim::BinConcat(operands) => {
            for operand in operands {
                erase(context, operand, &Subterm::Prim(Prim::BinType).into())?;
            }
            Ok(Subterm::Prim(Prim::BinType).into())
        }
        Prim::ArrType(elem) => {
            erase(context, elem, &Term::type_())?;
            Ok(Term::type_())
        }
        Prim::Arr(_) => Err(Error::cannot_infer_literal(Subterm::Prim(prim.clone()))),
        Prim::ArrLen(type_, list) => {
            erase(context, type_, &Term::type_())?;
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            erase(context, list, &expected_list_type)?;
            Ok(Subterm::Prim(Prim::NatType).into())
        }
        Prim::ArrGet(type_, list, index) => {
            erase(context, type_, &Term::type_())?;
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            erase(context, list, &expected_list_type)?;
            erase(context, index, &Subterm::Prim(Prim::NatType).into())?;
            Ok(type_.clone())
        }
        Prim::ArrSlice(type_, list, start, end) => {
            erase(context, type_, &Term::type_())?;
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            erase(context, list, &expected_list_type)?;
            erase(context, start, &Subterm::Prim(Prim::NatType).into())?;
            erase(context, end, &Subterm::Prim(Prim::NatType).into())?;
            Ok(expected_list_type)
        }
        Prim::ArrAppend(type_, list, elem) => {
            erase(context, type_, &Term::type_())?;
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            erase(context, list, &expected_list_type)?;
            erase(context, elem, type_)?;
            Ok(expected_list_type)
        }
        Prim::ArrConcat(type_, operands) => {
            erase(context, type_, &Term::type_())?;
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            for operand in operands {
                erase(context, operand, &expected_list_type)?;
            }
            Ok(expected_list_type)
        }
        Prim::IoPrint(inner) => {
            erase(context, inner, &Subterm::Prim(Prim::BinType).into())?;
            Ok(Term::tuple_type_unit())
        }
        Prim::IoRead => Ok(Subterm::Prim(Prim::BinType).into()),
    }
}
