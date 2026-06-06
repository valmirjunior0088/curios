use super::{Context, Error, Mode, Prim, Subterm, Term, elaborate, expect, infer, reduce_with};

/// Synthesize a primitive's type, checking its operands. Mirrors the old
/// `infer_prim`, but every operand obligation goes through `elaborate(Check)`
/// rather than `erase` — erase is now downstream lowering (§6).
fn synth_prim(context: &mut Context, prim: &Prim) -> Result<Term, Error> {
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
            elaborate(context, left, Mode::Check(Subterm::Prim(Prim::NatType).into()))?;
            elaborate(context, right, Mode::Check(Subterm::Prim(Prim::NatType).into()))?;
            Ok(Subterm::Prim(Prim::BlnType).into())
        }
        Prim::NatAdd(left, right)
        | Prim::NatSub(left, right)
        | Prim::NatMul(left, right)
        | Prim::NatDiv(left, right)
        | Prim::NatRem(left, right) => {
            elaborate(context, left, Mode::Check(Subterm::Prim(Prim::NatType).into()))?;
            elaborate(context, right, Mode::Check(Subterm::Prim(Prim::NatType).into()))?;
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
            elaborate(context, left, Mode::Check(Subterm::Prim(Prim::IntType).into()))?;
            elaborate(context, right, Mode::Check(Subterm::Prim(Prim::IntType).into()))?;
            Ok(Subterm::Prim(Prim::BlnType).into())
        }
        Prim::IntAdd(left, right)
        | Prim::IntSub(left, right)
        | Prim::IntMul(left, right)
        | Prim::IntDiv(left, right)
        | Prim::IntRem(left, right) => {
            elaborate(context, left, Mode::Check(Subterm::Prim(Prim::IntType).into()))?;
            elaborate(context, right, Mode::Check(Subterm::Prim(Prim::IntType).into()))?;
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
            elaborate(context, left, Mode::Check(Subterm::Prim(Prim::FltType).into()))?;
            elaborate(context, right, Mode::Check(Subterm::Prim(Prim::FltType).into()))?;
            Ok(Subterm::Prim(Prim::FltType).into())
        }
        Prim::FltNeg(inner)
        | Prim::FltAbs(inner)
        | Prim::FltSqrt(inner)
        | Prim::FltFloor(inner)
        | Prim::FltCeil(inner)
        | Prim::FltTrunc(inner)
        | Prim::FltNearest(inner) => {
            elaborate(context, inner, Mode::Check(Subterm::Prim(Prim::FltType).into()))?;
            Ok(Subterm::Prim(Prim::FltType).into())
        }
        Prim::FltEql(left, right)
        | Prim::FltNeq(left, right)
        | Prim::FltLt(left, right)
        | Prim::FltGt(left, right)
        | Prim::FltLte(left, right)
        | Prim::FltGte(left, right) => {
            elaborate(context, left, Mode::Check(Subterm::Prim(Prim::FltType).into()))?;
            elaborate(context, right, Mode::Check(Subterm::Prim(Prim::FltType).into()))?;
            Ok(Subterm::Prim(Prim::BlnType).into())
        }
        Prim::NatToStr(inner) => {
            elaborate(context, inner, Mode::Check(Subterm::Prim(Prim::NatType).into()))?;
            Ok(Subterm::Prim(Prim::BinType).into())
        }
        Prim::IntToStr(inner) => {
            elaborate(context, inner, Mode::Check(Subterm::Prim(Prim::IntType).into()))?;
            Ok(Subterm::Prim(Prim::BinType).into())
        }
        Prim::FltToStr(inner) => {
            elaborate(context, inner, Mode::Check(Subterm::Prim(Prim::FltType).into()))?;
            Ok(Subterm::Prim(Prim::BinType).into())
        }
        Prim::NatToInt(inner) => {
            elaborate(context, inner, Mode::Check(Subterm::Prim(Prim::NatType).into()))?;
            Ok(Subterm::Prim(Prim::IntType).into())
        }
        Prim::NatToFlt(inner) => {
            elaborate(context, inner, Mode::Check(Subterm::Prim(Prim::NatType).into()))?;
            Ok(Subterm::Prim(Prim::FltType).into())
        }
        Prim::IntToNat(inner) => {
            elaborate(context, inner, Mode::Check(Subterm::Prim(Prim::IntType).into()))?;
            Ok(Subterm::Prim(Prim::NatType).into())
        }
        Prim::IntToFlt(inner) => {
            elaborate(context, inner, Mode::Check(Subterm::Prim(Prim::IntType).into()))?;
            Ok(Subterm::Prim(Prim::FltType).into())
        }
        Prim::FltToNat(inner) => {
            elaborate(context, inner, Mode::Check(Subterm::Prim(Prim::FltType).into()))?;
            Ok(Subterm::Prim(Prim::NatType).into())
        }
        Prim::FltToInt(inner) => {
            elaborate(context, inner, Mode::Check(Subterm::Prim(Prim::FltType).into()))?;
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
            elaborate(context, left, Mode::Check(Subterm::Prim(Prim::BinType).into()))?;
            elaborate(context, right, Mode::Check(Subterm::Prim(Prim::BinType).into()))?;
            Ok(Subterm::Prim(Prim::BlnType).into())
        }
        Prim::BinGet(bin, index) => {
            let bin_type = infer(context, bin)?;
            let bin_type = reduce_with(context, &bin_type)?;
            match &*bin_type {
                Subterm::Prim(Prim::BinType) => {
                    elaborate(context, index, Mode::Check(Subterm::Prim(Prim::NatType).into()))?;
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
                    elaborate(context, start, Mode::Check(Subterm::Prim(Prim::NatType).into()))?;
                    elaborate(context, end, Mode::Check(Subterm::Prim(Prim::NatType).into()))?;
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
                    elaborate(context, byte, Mode::Check(Subterm::Prim(Prim::NatType).into()))?;
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
                elaborate(context, operand, Mode::Check(Subterm::Prim(Prim::BinType).into()))?;
            }
            Ok(Subterm::Prim(Prim::BinType).into())
        }
        Prim::ArrType(elem) => {
            elaborate(context, elem, Mode::Check(Term::type_()))?;
            Ok(Term::type_())
        }
        Prim::Arr(_) => Err(Error::cannot_infer_literal(Subterm::Prim(prim.clone()))),
        Prim::ArrLen(type_, list) => {
            elaborate(context, type_, Mode::Check(Term::type_()))?;
            let list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            elaborate(context, list, Mode::Check(list_type))?;
            Ok(Subterm::Prim(Prim::NatType).into())
        }
        Prim::ArrGet(type_, list, index) => {
            elaborate(context, type_, Mode::Check(Term::type_()))?;
            let list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            elaborate(context, list, Mode::Check(list_type))?;
            elaborate(context, index, Mode::Check(Subterm::Prim(Prim::NatType).into()))?;
            Ok(type_.clone())
        }
        Prim::ArrSlice(type_, list, start, end) => {
            elaborate(context, type_, Mode::Check(Term::type_()))?;
            let list_type: Term = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            elaborate(context, list, Mode::Check(list_type.clone()))?;
            elaborate(context, start, Mode::Check(Subterm::Prim(Prim::NatType).into()))?;
            elaborate(context, end, Mode::Check(Subterm::Prim(Prim::NatType).into()))?;
            Ok(list_type)
        }
        Prim::ArrAppend(type_, list, elem) => {
            elaborate(context, type_, Mode::Check(Term::type_()))?;
            let list_type: Term = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            elaborate(context, list, Mode::Check(list_type.clone()))?;
            elaborate(context, elem, Mode::Check(type_.clone()))?;
            Ok(list_type)
        }
        Prim::ArrConcat(type_, operands) => {
            elaborate(context, type_, Mode::Check(Term::type_()))?;
            let list_type: Term = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            for operand in operands {
                elaborate(context, operand, Mode::Check(list_type.clone()))?;
            }
            Ok(list_type)
        }
        Prim::IoPrint(inner) => {
            elaborate(context, inner, Mode::Check(Subterm::Prim(Prim::BinType).into()))?;
            Ok(Term::tuple_type_unit())
        }
        Prim::IoRead => Ok(Subterm::Prim(Prim::BinType).into()),
    }
}

pub fn elaborate_prim(
    context: &mut Context,
    term: &Term,
    prim: &Prim,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // `Arr` is the one naturally-checked primitive: it borrows its element type
    // from `expected` and cannot synthesize.
    if let Prim::Arr(elems) = prim {
        let Mode::Check(expected) = &mode else {
            return Err(Error::cannot_infer_literal(Subterm::Prim(prim.clone())));
        };

        let elem_type = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
            Subterm::Prim(Prim::ArrType(elem_type)) => elem_type,
            other => return Err(Error::type_mismatch(term.clone(), other, expected.clone())),
        };

        for elem in elems {
            elaborate(context, elem, Mode::Check(elem_type.clone()))?;
        }

        return Ok((term.clone(), expected.clone()));
    }

    let type_ = synth_prim(context, prim)?;

    if let Mode::Check(expected) = &mode {
        expect(context, term, &type_, expected)?;
    }

    Ok((term.clone(), type_))
}
