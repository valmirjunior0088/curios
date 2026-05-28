use {
    super::{
        Apply, Atom, AtomType, BlnMatch, Context, Error, Func, Let, Match, Nat, NatMatch, One,
        Prim, Proj, Rec, Scope, Subterm, Telescope, Term, Tuple, TupleType, Two, Type, Var, expect,
        infer, reduce_with, refine_head,
    },
    crate::ersd,
    std::collections::BTreeMap,
};

fn erase_prim(
    context: &mut Context,
    term: &Term,
    prim: &Prim,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    match prim {
        Prim::BlnType => {
            expect(context, term, &Type.into(), expected)?;

            Ok(ersd::Term::Erased)
        }
        &Prim::Bln(value) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BlnType).into(),
                expected,
            )?;

            Ok(ersd::Prim::Nat(if value { 1 } else { 0 }).into())
        }
        Prim::NatType => {
            expect(context, term, &Type.into(), expected)?;

            Ok(ersd::Term::Erased)
        }
        Prim::Nat(Nat::Zero) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::NatType).into(),
                expected,
            )?;

            Ok(ersd::Prim::Nat(0).into())
        }
        Prim::Nat(Nat::Succ(spine, inner)) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::NatType).into(),
                expected,
            )?;

            if matches!(inner.as_ref(), Subterm::Prim(Prim::Nat(Nat::Zero))) {
                Ok(ersd::Prim::Nat(*spine).into())
            } else {
                let inner_ersd = erase(context, inner, &Subterm::Prim(Prim::NatType).into())?;
                let spine_term: ersd::Term = ersd::Prim::Nat(*spine).into();
                Ok(ersd::Prim::NatAdd(spine_term.into(), inner_ersd.into()).into())
            }
        }
        Prim::NatEql(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BlnType).into(),
                expected,
            )?;

            Ok(ersd::Prim::NatEql(
                erase(context, left, &Subterm::Prim(Prim::NatType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::NatType).into())?.into(),
            )
            .into())
        }
        Prim::NatAdd(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::NatType).into(),
                expected,
            )?;

            Ok(ersd::Prim::NatAdd(
                erase(context, left, &Subterm::Prim(Prim::NatType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::NatType).into())?.into(),
            )
            .into())
        }
        Prim::NatSub(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::NatType).into(),
                expected,
            )?;

            Ok(ersd::Prim::NatSub(
                erase(context, left, &Subterm::Prim(Prim::NatType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::NatType).into())?.into(),
            )
            .into())
        }
        Prim::NatMul(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::NatType).into(),
                expected,
            )?;

            Ok(ersd::Prim::NatMul(
                erase(context, left, &Subterm::Prim(Prim::NatType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::NatType).into())?.into(),
            )
            .into())
        }
        Prim::NatNeq(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BlnType).into(),
                expected,
            )?;

            Ok(ersd::Prim::NatNeq(
                erase(context, left, &Subterm::Prim(Prim::NatType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::NatType).into())?.into(),
            )
            .into())
        }
        Prim::NatDiv(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::NatType).into(),
                expected,
            )?;

            Ok(ersd::Prim::NatDiv(
                erase(context, left, &Subterm::Prim(Prim::NatType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::NatType).into())?.into(),
            )
            .into())
        }
        Prim::NatRem(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::NatType).into(),
                expected,
            )?;

            Ok(ersd::Prim::NatRem(
                erase(context, left, &Subterm::Prim(Prim::NatType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::NatType).into())?.into(),
            )
            .into())
        }
        Prim::NatLt(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BlnType).into(),
                expected,
            )?;

            Ok(ersd::Prim::NatLt(
                erase(context, left, &Subterm::Prim(Prim::NatType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::NatType).into())?.into(),
            )
            .into())
        }
        Prim::NatGt(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BlnType).into(),
                expected,
            )?;

            Ok(ersd::Prim::NatGt(
                erase(context, left, &Subterm::Prim(Prim::NatType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::NatType).into())?.into(),
            )
            .into())
        }
        Prim::NatLte(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BlnType).into(),
                expected,
            )?;

            Ok(ersd::Prim::NatLte(
                erase(context, left, &Subterm::Prim(Prim::NatType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::NatType).into())?.into(),
            )
            .into())
        }
        Prim::NatGte(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BlnType).into(),
                expected,
            )?;

            Ok(ersd::Prim::NatGte(
                erase(context, left, &Subterm::Prim(Prim::NatType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::NatType).into())?.into(),
            )
            .into())
        }
        Prim::IntType => {
            expect(context, term, &Type.into(), expected)?;

            Ok(ersd::Term::Erased)
        }
        &Prim::Int(value) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::IntType).into(),
                expected,
            )?;

            Ok(ersd::Prim::Int(value).into())
        }
        Prim::IntEql(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BlnType).into(),
                expected,
            )?;

            Ok(ersd::Prim::IntEql(
                erase(context, left, &Subterm::Prim(Prim::IntType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::IntType).into())?.into(),
            )
            .into())
        }
        Prim::IntNeq(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BlnType).into(),
                expected,
            )?;

            Ok(ersd::Prim::IntNeq(
                erase(context, left, &Subterm::Prim(Prim::IntType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::IntType).into())?.into(),
            )
            .into())
        }
        Prim::IntAdd(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::IntType).into(),
                expected,
            )?;

            Ok(ersd::Prim::IntAdd(
                erase(context, left, &Subterm::Prim(Prim::IntType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::IntType).into())?.into(),
            )
            .into())
        }
        Prim::IntSub(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::IntType).into(),
                expected,
            )?;

            Ok(ersd::Prim::IntSub(
                erase(context, left, &Subterm::Prim(Prim::IntType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::IntType).into())?.into(),
            )
            .into())
        }
        Prim::IntMul(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::IntType).into(),
                expected,
            )?;

            Ok(ersd::Prim::IntMul(
                erase(context, left, &Subterm::Prim(Prim::IntType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::IntType).into())?.into(),
            )
            .into())
        }
        Prim::IntDiv(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::IntType).into(),
                expected,
            )?;

            Ok(ersd::Prim::IntDiv(
                erase(context, left, &Subterm::Prim(Prim::IntType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::IntType).into())?.into(),
            )
            .into())
        }
        Prim::IntRem(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::IntType).into(),
                expected,
            )?;

            Ok(ersd::Prim::IntRem(
                erase(context, left, &Subterm::Prim(Prim::IntType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::IntType).into())?.into(),
            )
            .into())
        }
        Prim::IntLt(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BlnType).into(),
                expected,
            )?;

            Ok(ersd::Prim::IntLt(
                erase(context, left, &Subterm::Prim(Prim::IntType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::IntType).into())?.into(),
            )
            .into())
        }
        Prim::IntGt(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BlnType).into(),
                expected,
            )?;

            Ok(ersd::Prim::IntGt(
                erase(context, left, &Subterm::Prim(Prim::IntType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::IntType).into())?.into(),
            )
            .into())
        }
        Prim::IntLte(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BlnType).into(),
                expected,
            )?;

            Ok(ersd::Prim::IntLte(
                erase(context, left, &Subterm::Prim(Prim::IntType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::IntType).into())?.into(),
            )
            .into())
        }
        Prim::IntGte(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BlnType).into(),
                expected,
            )?;

            Ok(ersd::Prim::IntGte(
                erase(context, left, &Subterm::Prim(Prim::IntType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::IntType).into())?.into(),
            )
            .into())
        }
        Prim::FltType => {
            expect(context, term, &Type.into(), expected)?;

            Ok(ersd::Term::Erased)
        }
        &Prim::Flt(flt) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::FltType).into(),
                expected,
            )?;

            Ok(ersd::Prim::Flt(flt.to_f32()).into())
        }
        Prim::FltAdd(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::FltType).into(),
                expected,
            )?;

            Ok(ersd::Prim::FltAdd(
                erase(context, left, &Subterm::Prim(Prim::FltType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::FltType).into())?.into(),
            )
            .into())
        }
        Prim::FltSub(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::FltType).into(),
                expected,
            )?;

            Ok(ersd::Prim::FltSub(
                erase(context, left, &Subterm::Prim(Prim::FltType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::FltType).into())?.into(),
            )
            .into())
        }
        Prim::FltMul(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::FltType).into(),
                expected,
            )?;

            Ok(ersd::Prim::FltMul(
                erase(context, left, &Subterm::Prim(Prim::FltType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::FltType).into())?.into(),
            )
            .into())
        }
        Prim::FltNeg(inner) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::FltType).into(),
                expected,
            )?;

            Ok(ersd::Prim::FltNeg(
                erase(context, inner, &Subterm::Prim(Prim::FltType).into())?.into(),
            )
            .into())
        }
        Prim::FltAbs(inner) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::FltType).into(),
                expected,
            )?;

            Ok(ersd::Prim::FltAbs(
                erase(context, inner, &Subterm::Prim(Prim::FltType).into())?.into(),
            )
            .into())
        }
        Prim::FltSqrt(inner) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::FltType).into(),
                expected,
            )?;

            Ok(ersd::Prim::FltSqrt(
                erase(context, inner, &Subterm::Prim(Prim::FltType).into())?.into(),
            )
            .into())
        }
        Prim::FltFloor(inner) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::FltType).into(),
                expected,
            )?;

            Ok(ersd::Prim::FltFloor(
                erase(context, inner, &Subterm::Prim(Prim::FltType).into())?.into(),
            )
            .into())
        }
        Prim::FltCeil(inner) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::FltType).into(),
                expected,
            )?;

            Ok(ersd::Prim::FltCeil(
                erase(context, inner, &Subterm::Prim(Prim::FltType).into())?.into(),
            )
            .into())
        }
        Prim::FltTrunc(inner) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::FltType).into(),
                expected,
            )?;

            Ok(ersd::Prim::FltTrunc(
                erase(context, inner, &Subterm::Prim(Prim::FltType).into())?.into(),
            )
            .into())
        }
        Prim::FltNearest(inner) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::FltType).into(),
                expected,
            )?;

            Ok(ersd::Prim::FltNearest(
                erase(context, inner, &Subterm::Prim(Prim::FltType).into())?.into(),
            )
            .into())
        }
        Prim::FltDiv(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::FltType).into(),
                expected,
            )?;

            Ok(ersd::Prim::FltDiv(
                erase(context, left, &Subterm::Prim(Prim::FltType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::FltType).into())?.into(),
            )
            .into())
        }
        Prim::FltMin(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::FltType).into(),
                expected,
            )?;

            Ok(ersd::Prim::FltMin(
                erase(context, left, &Subterm::Prim(Prim::FltType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::FltType).into())?.into(),
            )
            .into())
        }
        Prim::FltMax(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::FltType).into(),
                expected,
            )?;

            Ok(ersd::Prim::FltMax(
                erase(context, left, &Subterm::Prim(Prim::FltType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::FltType).into())?.into(),
            )
            .into())
        }
        Prim::FltEql(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BlnType).into(),
                expected,
            )?;

            Ok(ersd::Prim::FltEql(
                erase(context, left, &Subterm::Prim(Prim::FltType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::FltType).into())?.into(),
            )
            .into())
        }
        Prim::FltNeq(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BlnType).into(),
                expected,
            )?;

            Ok(ersd::Prim::FltNeq(
                erase(context, left, &Subterm::Prim(Prim::FltType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::FltType).into())?.into(),
            )
            .into())
        }
        Prim::FltLt(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BlnType).into(),
                expected,
            )?;

            Ok(ersd::Prim::FltLt(
                erase(context, left, &Subterm::Prim(Prim::FltType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::FltType).into())?.into(),
            )
            .into())
        }
        Prim::FltGt(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BlnType).into(),
                expected,
            )?;

            Ok(ersd::Prim::FltGt(
                erase(context, left, &Subterm::Prim(Prim::FltType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::FltType).into())?.into(),
            )
            .into())
        }
        Prim::FltLte(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BlnType).into(),
                expected,
            )?;

            Ok(ersd::Prim::FltLte(
                erase(context, left, &Subterm::Prim(Prim::FltType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::FltType).into())?.into(),
            )
            .into())
        }
        Prim::FltGte(left, right) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BlnType).into(),
                expected,
            )?;

            Ok(ersd::Prim::FltGte(
                erase(context, left, &Subterm::Prim(Prim::FltType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::FltType).into())?.into(),
            )
            .into())
        }
        Prim::NatToStr(inner) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BinType).into(),
                expected,
            )?;

            Ok(ersd::Prim::NatToStr(
                erase(context, inner, &Subterm::Prim(Prim::NatType).into())?.into(),
            )
            .into())
        }
        Prim::IntToStr(inner) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BinType).into(),
                expected,
            )?;

            Ok(ersd::Prim::IntToStr(
                erase(context, inner, &Subterm::Prim(Prim::IntType).into())?.into(),
            )
            .into())
        }
        Prim::FltToStr(inner) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BinType).into(),
                expected,
            )?;

            Ok(ersd::Prim::FltToStr(
                erase(context, inner, &Subterm::Prim(Prim::FltType).into())?.into(),
            )
            .into())
        }
        Prim::NatToInt(inner) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::IntType).into(),
                expected,
            )?;

            Ok(ersd::Prim::NatToInt(
                erase(context, inner, &Subterm::Prim(Prim::NatType).into())?.into(),
            )
            .into())
        }
        Prim::IntToNat(inner) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::NatType).into(),
                expected,
            )?;

            Ok(ersd::Prim::IntToNat(
                erase(context, inner, &Subterm::Prim(Prim::IntType).into())?.into(),
            )
            .into())
        }
        Prim::IntToFlt(inner) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::FltType).into(),
                expected,
            )?;

            Ok(ersd::Prim::IntToFlt(
                erase(context, inner, &Subterm::Prim(Prim::IntType).into())?.into(),
            )
            .into())
        }
        Prim::NatToFlt(inner) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::FltType).into(),
                expected,
            )?;

            Ok(ersd::Prim::NatToFlt(
                erase(context, inner, &Subterm::Prim(Prim::NatType).into())?.into(),
            )
            .into())
        }
        Prim::FltToInt(inner) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::IntType).into(),
                expected,
            )?;

            Ok(ersd::Prim::FltToInt(
                erase(context, inner, &Subterm::Prim(Prim::FltType).into())?.into(),
            )
            .into())
        }
        Prim::FltToNat(inner) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::NatType).into(),
                expected,
            )?;

            Ok(ersd::Prim::FltToNat(
                erase(context, inner, &Subterm::Prim(Prim::FltType).into())?.into(),
            )
            .into())
        }
        Prim::BinType => {
            expect(context, term, &Type.into(), expected)?;
            Ok(ersd::Term::Erased)
        }
        Prim::Bin(bytes) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BinType).into(),
                expected,
            )?;
            Ok(ersd::Prim::Bin(bytes.clone()).into())
        }
        Prim::BinLen(bin) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::NatType).into(),
                expected,
            )?;
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
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BlnType).into(),
                expected,
            )?;

            Ok(ersd::Prim::BinEql(
                erase(context, left, &Subterm::Prim(Prim::BinType).into())?.into(),
                erase(context, right, &Subterm::Prim(Prim::BinType).into())?.into(),
            )
            .into())
        }
        Prim::BinGet(bin, index) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::NatType).into(),
                expected,
            )?;
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
                erase(context, index, &Subterm::Prim(Prim::NatType).into())?.into(),
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
                erase(context, start, &Subterm::Prim(Prim::NatType).into())?.into(),
                erase(context, end, &Subterm::Prim(Prim::NatType).into())?.into(),
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
                erase(context, byte, &Subterm::Prim(Prim::NatType).into())?.into(),
            )
            .into())
        }
        Prim::BinConcat(operands) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BinType).into(),
                expected,
            )?;
            let erased = operands
                .iter()
                .map(|e| erase(context, e, &Subterm::Prim(Prim::BinType).into()).map(|t| t.into()))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(ersd::Prim::BinConcat(erased).into())
        }
        Prim::ArrType(elem) => {
            expect(context, term, &Type.into(), expected)?;
            erase(context, elem, &Type.into())?;
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
        Prim::ArrLen(list) => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::NatType).into(),
                expected,
            )?;
            let list_type = infer(context, list)?;
            let list_type_reduced = reduce_with(context, &list_type)?;
            if !matches!(&*list_type_reduced, Subterm::Prim(Prim::ArrType(_))) {
                return Err(Error::not_arr_type(term.clone(), list_type_reduced));
            }
            Ok(ersd::Prim::ArrLen(erase(context, list, &list_type_reduced)?.into()).into())
        }
        Prim::ArrGet(list, index) => {
            let list_type = infer(context, list)?;
            let list_type_reduced = reduce_with(context, &list_type)?;
            let elem_type = match &*list_type_reduced {
                Subterm::Prim(Prim::ArrType(elem)) => elem.clone(),
                _ => return Err(Error::not_arr_type(term.clone(), list_type_reduced)),
            };
            expect(context, term, &elem_type, expected)?;
            Ok(ersd::Prim::ArrGet(
                erase(context, list, &list_type_reduced)?.into(),
                erase(context, index, &Subterm::Prim(Prim::NatType).into())?.into(),
            )
            .into())
        }
        Prim::ArrSlice(list, start, end) => {
            let list_type = infer(context, list)?;
            let list_type_reduced = reduce_with(context, &list_type)?;
            if !matches!(&*list_type_reduced, Subterm::Prim(Prim::ArrType(_))) {
                return Err(Error::not_arr_type(term.clone(), list_type_reduced));
            }
            expect(context, term, &list_type_reduced, expected)?;
            Ok(ersd::Prim::ArrSlice(
                erase(context, list, &list_type_reduced)?.into(),
                erase(context, start, &Subterm::Prim(Prim::NatType).into())?.into(),
                erase(context, end, &Subterm::Prim(Prim::NatType).into())?.into(),
            )
            .into())
        }
        Prim::ArrAppend(list, elem) => {
            let list_type = infer(context, list)?;
            let list_type_reduced = reduce_with(context, &list_type)?;
            let elem_type = match &*list_type_reduced {
                Subterm::Prim(Prim::ArrType(e)) => e.clone(),
                _ => return Err(Error::not_arr_type(term.clone(), list_type_reduced)),
            };
            expect(
                context,
                term,
                &Subterm::Prim(Prim::arr_type(elem_type.clone())).into(),
                expected,
            )?;
            Ok(ersd::Prim::ArrAppend(
                erase(context, list, &list_type_reduced)?.into(),
                erase(context, elem, &elem_type)?.into(),
            )
            .into())
        }
        Prim::ArrConcat(operands) => {
            let expected_reduced = reduce_with(context, expected)?;
            if !matches!(&*expected_reduced, Subterm::Prim(Prim::ArrType(_))) {
                return Err(Error::not_an_array_type(term.clone(), expected.clone()));
            }
            let erased = operands
                .iter()
                .map(|e| erase(context, e, &expected_reduced).map(|t| t.into()))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(ersd::Prim::ArrConcat(erased).into())
        }
        Prim::IoPrint(inner) => {
            expect(
                context,
                term,
                &Subterm::TupleType(TupleType::unit()).into(),
                expected,
            )?;
            Ok(ersd::Prim::IoPrint(
                erase(context, inner, &Subterm::Prim(Prim::BinType).into())?.into(),
            )
            .into())
        }
        Prim::IoRead => {
            expect(
                context,
                term,
                &Subterm::Prim(Prim::BinType).into(),
                expected,
            )?;
            Ok(ersd::Prim::IoRead.into())
        }
    }
}

fn erase_func(
    context: &mut Context,
    func: &Func,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let Func { body } = func;

    let ft = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
        Subterm::FuncType(ft) => ft,
        _ => return Err(Error::not_a_function_type(term.clone(), expected.clone())),
    };

    let n = ft.telescope.len();
    let captures = body.free_vars().into_iter().collect::<Vec<_>>();

    let param_names = (0..n)
        .map(|i| context.fresh(body.label_iter().nth(i).flatten()))
        .collect::<Vec<_>>();
    let param_terms = param_names
        .iter()
        .map(|p| Term::from(Var::free(p)))
        .collect::<Vec<_>>();
    let param_refs = param_terms.iter().collect::<Vec<_>>();
    let body_opened = body.open(&param_refs);

    fn output_type(
        context: &mut Context,
        tele: Telescope<Term>,
        names: &[String],
        terms: &[Term],
    ) -> Term {
        match tele {
            Telescope::Done(body) => *body,
            Telescope::Cons(ty, rest) => {
                context.assume(&names[0], &ty);
                output_type(context, rest.open(&[&terms[0]]), &names[1..], &terms[1..])
            }
        }
    }

    let erased_body = context.with_frame(|context| {
        let output_type = output_type(context, ft.telescope, &param_names, &param_terms);
        erase(context, &body_opened, &output_type)
    })?;

    Ok(ersd::Func {
        captures,
        params: param_names,
        body: erased_body.into(),
    }
    .into())
}

fn erase_apply(
    context: &mut Context,
    apply: &Apply,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let Apply { head, params } = apply;

    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    let ft = match &*head_type {
        Subterm::FuncType(ft) => ft,
        _ => return Err(Error::not_a_function(term.clone(), head_type)),
    };

    if params.len() != ft.telescope.len() {
        return Err(Error::wrong_number_of_arguments(
            term.clone(),
            ft.telescope.len(),
            params.len(),
        ));
    }

    fn walk(
        context: &mut Context,
        tele: Telescope<Term>,
        params: &[Term],
        erased: &mut Vec<ersd::Term>,
    ) -> Result<Term, Error> {
        match tele {
            Telescope::Done(body) => Ok(*body),
            Telescope::Cons(ty, rest) => {
                let head = &params[0];
                erased.push(erase(context, head, &ty)?);
                walk(context, rest.open(&[head]), &params[1..], erased)
            }
        }
    }

    let mut erased_params = Vec::with_capacity(params.len());
    let result_type = walk(context, ft.telescope.clone(), params, &mut erased_params)?;
    let erased_head = erase(context, head, &head_type)?;

    expect(context, term, &result_type, expected)?;

    Ok(ersd::Apply {
        head: erased_head.into(),
        params: erased_params.into_iter().map(|p| p.into()).collect(),
    }
    .into())
}

fn erase_tuple(context: &mut Context, tuple: &Tuple, expected: &Term) -> Result<ersd::Term, Error> {
    let Tuple { fields } = tuple;

    let type_telescope = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
        Subterm::TupleType(TupleType { telescope }) => telescope,
        _ => return Err(Error::not_a_tuple_type(tuple.clone(), expected.clone())),
    };

    if fields.len() != type_telescope.len() {
        return Err(Error::tuple_arity_mismatch(
            tuple.clone(),
            type_telescope.len(),
            fields.len(),
        ));
    }

    fn walk(
        context: &mut Context,
        tele: Telescope<()>,
        fields: &[Term],
        erased: &mut Vec<ersd::Subterm>,
    ) -> Result<(), Error> {
        match tele {
            Telescope::Done(_) => Ok(()),
            Telescope::Cons(ty, rest) => {
                let head = &fields[0];
                erased.push(erase(context, head, &ty)?.into());
                walk(context, rest.open(&[head]), &fields[1..], erased)
            }
        }
    }

    let mut erased_fields = Vec::<ersd::Subterm>::new();
    walk(context, type_telescope, fields, &mut erased_fields)?;

    Ok(ersd::Tuple {
        fields: erased_fields,
    }
    .into())
}

fn erase_nat_induction(
    context: &mut Context,
    head: &Term,
    motive: &Scope<One>,
    zero_case: &Term,
    succ_case: &Scope<Two>,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    if !matches!(&*head_type, Subterm::Prim(Prim::NatType)) {
        return Err(Error::not_nat_type(term.clone(), head_type));
    }

    let head_label = context.fresh(motive.first_label());

    context.with_frame(|context| {
        context.assume(&head_label, &Subterm::Prim(Prim::NatType).into());

        erase(
            context,
            &motive.open(&[&Var::free(head_label).into()]),
            &Type.into(),
        )
    })?;

    let erased_zero_case = erase(
        context,
        zero_case,
        &motive.open(&[&Subterm::Prim(Prim::Nat(Nat::new(0))).into()]),
    )?;

    let pred_label = context.fresh(succ_case.first_label());
    let ih_label = context.fresh(succ_case.second_label());

    let erased_succ_case = context.with_frame(|context| {
        context.assume(&pred_label, &Subterm::Prim(Prim::NatType).into());
        context.assume(&ih_label, &motive.open(&[&Var::free(&pred_label).into()]));

        erase(
            context,
            &succ_case.open(&[&Var::free(&pred_label).into(), &Var::free(&ih_label).into()]),
            &motive.open(&[&Subterm::Prim(Prim::nat_add(
                Var::free(&pred_label),
                Subterm::Prim(Prim::Nat(Nat::new(1))),
            ))
            .into()]),
        )
    })?;

    let erased_head = erase(context, head, &head_type)?;

    expect(context, term, &motive.open(&[head]), expected)?;

    Ok(ersd::NatMatch::Induction {
        head: erased_head.into(),
        zero_case: erased_zero_case.into(),
        pred: pred_label,
        ih: ih_label,
        succ_case: erased_succ_case.into(),
    }
    .into())
}

fn erase_nat_dispatch(
    context: &mut Context,
    head: &Term,
    motive: &Scope<One>,
    cases: &BTreeMap<u32, Term>,
    default: &Term,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    if !matches!(&*head_type, Subterm::Prim(Prim::NatType)) {
        return Err(Error::not_nat_type(term.clone(), head_type));
    }

    let head_label = context.fresh(motive.first_label());

    context.with_frame(|context| {
        context.assume(&head_label, &Subterm::Prim(Prim::NatType).into());
        erase(
            context,
            &motive.open(&[&Var::free(head_label).into()]),
            &Type.into(),
        )
    })?;

    let erased_cases = cases
        .iter()
        .map(|(n, body)| {
            let case_expected = motive.open(&[&Subterm::Prim(Prim::Nat(Nat::new(*n))).into()]);
            context.with_frame(|context| {
                refine_head(
                    context,
                    head,
                    &Subterm::Prim(Prim::Nat(Nat::new(*n))).into(),
                )?;
                erase(context, body, &case_expected).map(|e| (*n, e.into()))
            })
        })
        .collect::<Result<Vec<_>, Error>>()?;

    let erased_default = erase(context, default, &motive.open(&[head]))?;

    let erased_head = erase(context, head, &head_type)?;

    expect(context, term, &motive.open(&[head]), expected)?;

    Ok(ersd::NatMatch::Dispatch {
        head: erased_head.into(),
        cases: erased_cases,
        default: erased_default.into(),
    }
    .into())
}

fn erase_nat_match(
    context: &mut Context,
    nm: &NatMatch,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    match nm {
        NatMatch::Induction {
            head,
            motive,
            zero_case,
            succ_case,
        } => erase_nat_induction(context, head, motive, zero_case, succ_case, term, expected),
        NatMatch::Dispatch {
            head,
            motive,
            cases,
            default,
        } => erase_nat_dispatch(context, head, motive, cases, default, term, expected),
    }
}

fn erase_bln_match(
    context: &mut Context,
    bm: &BlnMatch,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let BlnMatch {
        head,
        motive,
        false_case,
        true_case,
    } = bm;

    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    if !matches!(&*head_type, Subterm::Prim(Prim::BlnType)) {
        return Err(Error::not_bln_type(term.clone(), head_type));
    }

    let head_label = context.fresh(motive.first_label());

    context.with_frame(|context| {
        context.assume(&head_label, &Subterm::Prim(Prim::BlnType).into());
        erase(
            context,
            &motive.open(&[&Var::free(head_label).into()]),
            &Type.into(),
        )
    })?;

    let erased_false = context.with_frame(|context| {
        refine_head(context, head, &Subterm::Prim(Prim::Bln(false)).into())?;
        erase(
            context,
            false_case,
            &motive.open(&[&Subterm::Prim(Prim::Bln(false)).into()]),
        )
    })?;

    let erased_true = context.with_frame(|context| {
        refine_head(context, head, &Subterm::Prim(Prim::Bln(true)).into())?;
        erase(
            context,
            true_case,
            &motive.open(&[&Subterm::Prim(Prim::Bln(true)).into()]),
        )
    })?;

    let erased_head = erase(context, head, &head_type)?;

    expect(context, term, &motive.open(&[head]), expected)?;

    Ok(ersd::NatMatch::Dispatch {
        head: erased_head.into(),
        cases: vec![(0, erased_false.into())],
        default: erased_true.into(),
    }
    .into())
}

fn erase_proj(
    context: &mut Context,
    proj: &Proj,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let Proj { head, index } = proj;

    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    let TupleType { telescope } = match Term::unwrap_or_clone(head_type.clone()) {
        Subterm::TupleType(tt) => tt,
        other => return Err(Error::not_a_tuple(term.clone(), other)),
    };

    if *index >= telescope.len() {
        return Err(Error::tuple_index_out_of_bounds(
            term.clone(),
            *index,
            telescope.len(),
        ));
    }

    let field_type = telescope
        .nth(*index, |j| Proj::new((**head).clone(), j).into())
        .expect("index in range");

    expect(context, term, &field_type, expected)?;

    Ok(ersd::Proj {
        head: erase(context, head, &head_type)?.into(),
        index: *index,
    }
    .into())
}

fn erase_atom(
    context: &mut Context,
    atom: &Atom,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let atoms = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
        Subterm::AtomType(AtomType { atoms }) => atoms,
        _ => {
            return Err(Error::type_mismatch(
                term.clone(),
                AtomType::new([atom.clone()]),
                expected.clone(),
            ));
        }
    };

    let index = atoms
        .iter()
        .position(|candidate| candidate == atom)
        .ok_or_else(|| {
            Error::type_mismatch(
                term.clone(),
                AtomType::new([atom.clone()]),
                expected.clone(),
            )
        })?;

    Ok(ersd::Atom { index }.into())
}

fn erase_match(
    context: &mut Context,
    m: &Match,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let Match {
        head,
        motive,
        cases,
    } = m;

    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    let atoms = match Term::unwrap_or_clone(head_type.clone()) {
        Subterm::AtomType(AtomType { atoms }) => atoms,
        other => return Err(Error::not_an_atom_type(term.clone(), other)),
    };

    let head_label = context.fresh(motive.first_label());

    context.with_frame(|context| {
        context.assume(&head_label, &AtomType::new(atoms.iter().cloned()).into());

        erase(
            context,
            &motive.open(&[&Var::free(head_label).into()]),
            &Type.into(),
        )
    })?;

    if cases.len() != atoms.len() {
        return Err(Error::match_arity_mismatch(
            term.clone(),
            atoms.len(),
            cases.len(),
        ));
    }

    let cases = atoms
        .iter()
        .map(|atom| {
            let body = if let Some(body) = cases.get(atom) {
                body
            } else {
                return Err(Error::match_case_missing(term.clone(), atom.clone()));
            };

            let expected = motive.open(&[&atom.clone().into()]);

            context.with_frame(|context| {
                refine_head(context, head, &atom.clone().into())?;
                erase(context, body, &expected).map(Into::into)
            })
        })
        .collect::<Result<Vec<_>, Error>>()?;

    expect(context, term, &motive.open(&[head]), expected)?;

    Ok(ersd::Match {
        head: erase(context, head, &head_type)?.into(),
        cases,
    }
    .into())
}

fn erase_let(context: &mut Context, let_: &Let, expected: &Term) -> Result<ersd::Term, Error> {
    let Let {
        type_: body_type,
        body,
        tail,
    } = let_;

    erase(context, body_type, &Type.into())?;

    let name = context.fresh(tail.first_label());
    let erased_body = erase(context, body, body_type)?;
    let var_term = Var::free(&name).into();
    let tail = tail.open(&[&var_term]);

    let tail = context.with_frame(|context| {
        context.define_assuming(&name, body_type, body);

        erase(context, &tail, expected)
    })?;

    Ok(ersd::Let {
        name,
        body: erased_body.into(),
        tail: tail.into(),
    }
    .into())
}

fn erase_rec(context: &mut Context, rec: &Rec, expected: &Term) -> Result<ersd::Term, Error> {
    let Rec { items, tail } = rec;

    let names = tail
        .label_iter()
        .map(|l| context.fresh(l))
        .collect::<Vec<_>>();

    let label_terms = names
        .iter()
        .map(Var::free)
        .map(Into::into)
        .collect::<Vec<_>>();

    let label_terms = label_terms.iter().collect::<Vec<_>>();

    let items = items
        .iter()
        .map(|(type_, body)| (type_.open(&label_terms), body.open(&label_terms)))
        .collect::<Vec<_>>();

    let tail = tail.open(&label_terms);

    let erased = context.with_frame(|context| {
        for (name, (type_, _)) in names.iter().zip(items.iter()) {
            context.assume(name, type_);
        }

        for (type_, _) in &items {
            erase(context, type_, &Type.into())?;
        }

        for (name, (_, body)) in names.iter().zip(items.iter()) {
            context.define(name, body);
        }

        let erased_items = items
            .iter()
            .map(|(type_, body)| erase(context, body, type_).map(Into::into))
            .collect::<Result<Vec<_>, Error>>()?;

        Ok(ersd::Rec {
            names,
            items: erased_items,
            tail: erase(context, &tail, expected)?.into(),
        })
    })?;

    Ok(erased.into())
}

pub fn erase(context: &mut Context, term: &Term, expected: &Term) -> Result<ersd::Term, Error> {
    match &**term {
        Subterm::Prim(prim) => erase_prim(context, term, prim, expected),
        Subterm::BlnMatch(bm) => erase_bln_match(context, bm, term, expected),
        Subterm::NatMatch(nm) => erase_nat_match(context, nm, term, expected),
        Subterm::Type => {
            expect(context, term, &Type.into(), expected)?;
            Ok(ersd::Term::Erased)
        }
        Subterm::FuncType(_) => {
            let t = infer(context, term)?;
            expect(context, term, &t, expected)?;
            Ok(ersd::Term::Erased)
        }
        Subterm::Func(func) => erase_func(context, func, term, expected),
        Subterm::Apply(apply) => erase_apply(context, apply, term, expected),
        Subterm::TupleType(_) => {
            let t = infer(context, term)?;
            expect(context, term, &t, expected)?;
            Ok(ersd::Term::Erased)
        }
        Subterm::Tuple(tuple) => erase_tuple(context, tuple, expected),
        Subterm::Proj(proj) => erase_proj(context, proj, term, expected),
        Subterm::AtomType(_) => {
            let t = infer(context, term)?;
            expect(context, term, &t, expected)?;
            Ok(ersd::Term::Erased)
        }
        Subterm::Atom(atom) => erase_atom(context, atom, term, expected),
        Subterm::Match(m) => erase_match(context, m, term, expected),
        Subterm::Let(let_) => erase_let(context, let_, expected),
        Subterm::Rec(lr) => erase_rec(context, lr, expected),
        Subterm::Var(var) => {
            let t = infer(context, term)?;
            expect(context, term, &t, expected)?;
            Ok(ersd::Name::from(var.unwrap()).into())
        }
        Subterm::Spanned(span, inner) => {
            erase(context, inner, expected).map_err(|error| error.at(*span))
        }
    }
}

#[cfg(test)]
mod tests {
    use {
        super::*,
        crate::{
            core::{
                Atom, AtomType, Flt, Func, FuncType, Match, Nat, NatMatch, Prim, Rec, Term, Tuple,
                TupleType, Type, Var,
            },
            ersd, text,
        },
        std::time::Duration,
    };

    fn context() -> Context {
        Context::new(Duration::from_secs(1))
    }

    #[test]
    fn erase_dependent_tuple_type_over_atom_match_and_tuple_value() {
        let mut context = context();

        let tuple_type = Term::from(TupleType::new([
            ("x", Term::from(AtomType::new(["left", "right"]))),
            (
                "y",
                Term::from(Match::new(
                    Var::free("x"),
                    Some("m"),
                    Type,
                    vec![
                        ("left", AtomType::new(["hot"])),
                        ("right", AtomType::new(["cold"])),
                    ],
                )),
            ),
        ]));

        erase(&mut context, &tuple_type, &Type.into()).unwrap();

        let tuple = Term::from(Tuple::new([Atom::from("left"), Atom::from("hot")]));

        erase(&mut context, &tuple, &tuple_type).unwrap();

        let tuple = Term::from(Tuple::new([Atom::from("right"), Atom::from("cold")]));

        erase(&mut context, &tuple, &tuple_type).unwrap();
    }

    #[test]
    fn erase_dependent_tuple_type_rejects_wrong_branch_atom() {
        let mut context = context();

        let tuple_type = Term::from(TupleType::new([
            ("x", Term::from(AtomType::new(["left", "right"]))),
            (
                "y",
                Term::from(Match::new(
                    Var::free("x"),
                    Some("m"),
                    Type,
                    vec![
                        ("left", AtomType::new(["hot"])),
                        ("right", AtomType::new(["cold"])),
                    ],
                )),
            ),
        ]));

        let tuple = Term::from(Tuple::new([Atom::from("left"), Atom::from("cold")]));

        assert!(matches!(
            erase(&mut context, &tuple, &tuple_type),
            Err(Error::TypeMismatch { .. })
        ));
    }

    #[test]
    fn erase_match_singleton_lowers_to_match() {
        let type_ = text::to_core(&"'[yes, no]".parse().unwrap(), &text::PanicLoader).unwrap();

        let term = text::to_core(
            &r#"
                let x : '[unit] = 'unit;
                match x : _ => '[yes, no]
                | 'unit => 'yes
                end
            "#
            .parse()
            .unwrap(),
            &text::PanicLoader,
        )
        .unwrap();

        let erased = erase(&mut Context::new(Duration::from_secs(1)), &term, &type_).unwrap();

        let ersd::Term::Let(ersd::Let { body, tail, .. }) = erased else {
            panic!("expected let");
        };

        assert!(matches!(*body, ersd::Term::Atom(ersd::Atom { index: 0 })));
        assert!(matches!(*tail, ersd::Term::Match(_)));
    }

    #[test]
    fn erase_rec_single_identity_function() {
        let mut context = context();

        let func_type = Term::from(FuncType::new(
            [("x", AtomType::new(["a"]))],
            AtomType::new(["a"]),
        ));

        let term = Term::from(Rec::new(
            vec![("f", func_type.clone(), Func::new(["x"], Var::free("x")))],
            Var::free("f"),
        ));

        erase(&mut context, &term, &func_type).unwrap();
    }

    #[test]
    fn erase_preempts_on_cyclic_expected_type() {
        let mut context = context();

        context.define("loop", &Var::free("loop").into());

        assert!(matches!(
            erase(&mut context, &Type.into(), &Var::free("loop").into()),
            Err(Error::ConvertPreempted { .. })
        ));
    }

    #[test]
    fn erase_accepts_term_level_loop_with_stable_type() {
        let mut context = context();

        let type_ = Term::from(AtomType::new(["a"]));

        let term = Term::from(Rec::new(
            vec![("loop", type_.clone(), Var::free("loop"))],
            Var::free("loop"),
        ));

        erase(&mut context, &term, &type_).unwrap();
    }

    #[test]
    fn erase_prim_ops_typecheck() {
        let mut context = context();

        erase(
            &mut context,
            &Subterm::Prim(Prim::int_eql(
                Subterm::Prim(Prim::Int(1)),
                Subterm::Prim(Prim::Int(1)),
            ))
            .into(),
            &Subterm::Prim(Prim::BlnType).into(),
        )
        .unwrap();

        erase(
            &mut context,
            &Subterm::Prim(Prim::flt_add(
                Subterm::Prim(Prim::Flt(Flt::from_f32(1.5))),
                Subterm::Prim(Prim::Flt(Flt::from_f32(2.0))),
            ))
            .into(),
            &Subterm::Prim(Prim::FltType).into(),
        )
        .unwrap();
    }

    #[test]
    fn erase_func_captures_free_variables_before_opening_body() {
        let atom_type = Term::from(AtomType::new(["a"]));
        let tuple_type = Term::from(TupleType::new([
            ("z", atom_type.clone()),
            ("w", atom_type.clone()),
        ]));
        let type_ = Term::from(FuncType::new([("x", atom_type.clone())], tuple_type));
        let term = Term::from(Func::new(
            ["x"],
            Tuple::new([Term::from(Var::free("x")), Term::from(Var::free("y"))]),
        ));

        let mut context = Context::new(Duration::from_secs(1));
        context.assume("y", &atom_type);

        let erased = erase(&mut context, &term, &type_).unwrap();

        let ersd::Term::Func(ersd::Func { captures, .. }) = erased else {
            panic!("expected erased func");
        };

        assert_eq!(captures.len(), 1);
        assert!(captures.contains(&"y".to_string()));
    }

    #[test]
    fn erase_rejects_wrong_prim_operand_types() {
        assert!(matches!(
            erase(
                &mut Context::new(Duration::from_secs(1)),
                &Subterm::Prim(Prim::int_add(
                    Subterm::Prim(Prim::Int(1)),
                    Subterm::Prim(Prim::Flt(Flt::from_f32(2.0)))
                ))
                .into(),
                &Subterm::Prim(Prim::IntType).into(),
            ),
            Err(Error::TypeMismatch { .. })
        ));
    }

    #[test]
    fn erase_match_and_atom_stress_test() {
        let type_ =
            text::to_core(&"'[zeta, alpha, mu]".parse().unwrap(), &text::PanicLoader).unwrap();

        let term = text::to_core(
            &r#"
                let outer : '[zeta, alpha, mu] = 'mu;
                let alpha_case : '[zeta, alpha, mu] = 'alpha;
                let mu_case : '[zeta, alpha, mu] = 'mu;
                let zeta_case : '[zeta, alpha, mu] = 'zeta;
                match outer : subject => '[zeta, alpha, mu]
                | 'zeta =>
                    match alpha_case : nested => '[zeta, alpha, mu]
                    | 'zeta => 'alpha
                    | 'alpha => 'mu
                    | 'mu => 'zeta
                    end
                | 'alpha =>
                    match zeta_case : nested => '[zeta, alpha, mu]
                    | 'zeta => 'mu
                    | 'alpha => 'zeta
                    | 'mu => 'alpha
                    end
                | 'mu =>
                    match mu_case : nested => '[zeta, alpha, mu]
                    | 'zeta => 'zeta
                    | 'alpha => 'alpha
                    | 'mu => 'mu
                    end
                end
            "#
            .parse()
            .unwrap(),
            &text::PanicLoader,
        )
        .unwrap();

        let erased = erase(&mut Context::new(Duration::from_secs(1)), &term, &type_).unwrap();

        let ersd::Term::Let(ersd::Let {
            name: outer_name,
            body: outer_body,
            tail,
        }) = erased
        else {
            panic!("expected outer let");
        };

        assert_eq!(outer_name, "outer#0");
        assert!(matches!(
            *outer_body,
            ersd::Term::Atom(ersd::Atom { index: 1 })
        ));

        let ersd::Term::Let(ersd::Let {
            name: alpha_name,
            body: alpha_body,
            tail,
        }) = *tail
        else {
            panic!("expected alpha_case let");
        };

        assert_eq!(alpha_name, "alpha_case#1");
        assert!(matches!(
            *alpha_body,
            ersd::Term::Atom(ersd::Atom { index: 0 })
        ));

        let ersd::Term::Let(ersd::Let {
            name: mu_name,
            body: mu_body,
            tail,
        }) = *tail
        else {
            panic!("expected mu_case let");
        };

        assert_eq!(mu_name, "mu_case#2");
        assert!(matches!(
            *mu_body,
            ersd::Term::Atom(ersd::Atom { index: 1 })
        ));

        let ersd::Term::Let(ersd::Let {
            name: zeta_name,
            body: zeta_body,
            tail,
        }) = *tail
        else {
            panic!("expected zeta_case let");
        };

        assert_eq!(zeta_name, "zeta_case#3");
        assert!(matches!(
            *zeta_body,
            ersd::Term::Atom(ersd::Atom { index: 2 })
        ));

        let ersd::Term::Match(ersd::Match { head, cases }) = *tail else {
            panic!("expected outer erased case");
        };

        assert!(matches!(
            *head,
            ersd::Term::Name(name) if name.as_str() == "outer#0"
        ));

        assert_eq!(cases.len(), 3);

        let ersd::Term::Match(ersd::Match {
            head: alpha_head,
            cases: alpha_cases,
        }) = &*cases[0]
        else {
            panic!("expected nested case for 'alpha case");
        };

        assert!(matches!(
            &**alpha_head,
            ersd::Term::Name(name) if name.as_str() == "zeta_case#3"
        ));

        assert_eq!(alpha_cases.len(), 3);
        assert!(matches!(
            *alpha_cases[0],
            ersd::Term::Atom(ersd::Atom { index: 2 })
        ));
        assert!(matches!(
            *alpha_cases[1],
            ersd::Term::Atom(ersd::Atom { index: 0 })
        ));
        assert!(matches!(
            *alpha_cases[2],
            ersd::Term::Atom(ersd::Atom { index: 1 })
        ));

        let ersd::Term::Match(ersd::Match {
            head: mu_head,
            cases: mu_cases,
        }) = &*cases[1]
        else {
            panic!("expected nested case for 'mu case");
        };

        assert!(matches!(
            &**mu_head,
            ersd::Term::Name(name) if name.as_str() == "mu_case#2"
        ));

        assert_eq!(mu_cases.len(), 3);

        assert!(matches!(
            *mu_cases[0],
            ersd::Term::Atom(ersd::Atom { index: 0 })
        ));

        assert!(matches!(
            *mu_cases[1],
            ersd::Term::Atom(ersd::Atom { index: 1 })
        ));

        assert!(matches!(
            *mu_cases[2],
            ersd::Term::Atom(ersd::Atom { index: 2 })
        ));

        let ersd::Term::Match(ersd::Match {
            head: zeta_head,
            cases: zeta_cases,
        }) = &*cases[2]
        else {
            panic!("expected nested case for 'zeta case");
        };

        assert!(matches!(
            &**zeta_head,
            ersd::Term::Name(name) if name.as_str() == "alpha_case#1"
        ));

        assert_eq!(zeta_cases.len(), 3);

        assert!(matches!(
            *zeta_cases[0],
            ersd::Term::Atom(ersd::Atom { index: 1 })
        ));

        assert!(matches!(
            *zeta_cases[1],
            ersd::Term::Atom(ersd::Atom { index: 2 })
        ));

        assert!(matches!(
            *zeta_cases[2],
            ersd::Term::Atom(ersd::Atom { index: 0 })
        ));
    }

    #[test]
    fn erase_arr_nat_type_literal_len_and_get() {
        let mut context = context();

        let arr_nat = Subterm::Prim(Prim::arr_type(Subterm::Prim(Prim::NatType))).into();
        erase(&mut context, &arr_nat, &Type.into()).unwrap();

        let literal = Subterm::Prim(Prim::from(vec![
            Subterm::Prim(Prim::Nat(Nat::new(1))),
            Subterm::Prim(Prim::Nat(Nat::new(2))),
        ]))
        .into();
        erase(&mut context, &literal, &arr_nat).unwrap();

        context.assume("xs", &arr_nat);
        let len = Subterm::Prim(Prim::arr_len(Var::free("xs"))).into();
        assert_eq!(
            infer(&mut context, &len).unwrap(),
            Subterm::Prim(Prim::NatType).into()
        );

        let get = Subterm::Prim(Prim::arr_get(
            Var::free("xs"),
            Subterm::Prim(Prim::Nat(Nat::new(0))),
        ))
        .into();
        assert_eq!(
            infer(&mut context, &get).unwrap(),
            Subterm::Prim(Prim::NatType).into()
        );
    }

    #[test]
    fn erase_bin_type_literal_len_and_get() {
        let mut context = context();

        let bin_type = Subterm::Prim(Prim::BinType).into();
        erase(&mut context, &bin_type, &Type.into()).unwrap();

        let literal = Subterm::Prim(Prim::Bin(vec![1, 2, 3])).into();
        assert_eq!(infer(&mut context, &literal).unwrap(), bin_type);
        erase(&mut context, &literal, &bin_type).unwrap();

        context.assume("b", &bin_type);
        let len = Subterm::Prim(Prim::bin_len(Var::free("b"))).into();
        assert_eq!(
            infer(&mut context, &len).unwrap(),
            Subterm::Prim(Prim::NatType).into()
        );

        let get = Subterm::Prim(Prim::bin_get(
            Var::free("b"),
            Subterm::Prim(Prim::Nat(Nat::new(0))),
        ))
        .into();
        assert_eq!(
            infer(&mut context, &get).unwrap(),
            Subterm::Prim(Prim::NatType).into()
        );
    }

    #[test]
    fn erase_bin_append() {
        let mut context = context();

        let bin_type = Subterm::Prim(Prim::BinType).into();
        context.assume("b", &bin_type);
        context.assume("n", &Subterm::Prim(Prim::NatType).into());

        let append = Subterm::Prim(Prim::bin_append(Var::free("b"), Var::free("n"))).into();
        assert_eq!(infer(&mut context, &append).unwrap(), bin_type);
        erase(&mut context, &append, &bin_type).unwrap();
    }

    #[test]
    fn erase_bin_eql() {
        let mut context = context();

        let bin_type = Subterm::Prim(Prim::BinType).into();
        let bool_type = Subterm::Prim(Prim::BlnType).into();
        context.assume("a", &bin_type);
        context.assume("b", &bin_type);

        let eql = Subterm::Prim(Prim::bin_eql(Var::free("a"), Var::free("b"))).into();
        assert_eq!(infer(&mut context, &eql).unwrap(), bool_type);
        erase(&mut context, &eql, &bool_type).unwrap();
    }

    #[test]
    fn erase_nat_eql_returns_bool_atom() {
        let mut context = context();

        let bool_type = Subterm::Prim(Prim::BlnType).into();

        let eql = Subterm::Prim(Prim::nat_eql(
            Subterm::Prim(Prim::Nat(Nat::new(0))),
            Subterm::Prim(Prim::Nat(Nat::new(0))),
        ))
        .into();

        assert_eq!(infer(&mut context, &eql).unwrap(), bool_type);
        erase(&mut context, &eql, &bool_type).unwrap();
    }

    #[test]
    fn erase_nat_fold_rejects_non_nat_head() {
        let mut context = context();

        let bool_type = Term::from(AtomType::new(["false", "true"]));

        let nat_fold = Term::from(NatMatch::induction(
            Prim::Int(1),
            Some("m"),
            AtomType::new(["false", "true"]),
            Atom::from("false"),
            "pred",
            "ih",
            Atom::from("true"),
        ));

        assert!(matches!(
            erase(&mut context, &nat_fold, &bool_type),
            Err(Error::NotNatType { .. })
        ));
    }

    #[test]
    fn erase_nat_match_dispatches_to_named_case() {
        let mut context = context();

        let bool_type = Term::from(AtomType::new(["false", "true"]));

        let nat_match = Term::from(NatMatch::dispatch(
            Prim::Nat(Nat::new(5)),
            Some("m"),
            AtomType::new(["false", "true"]),
            [(5u32, Term::from(Atom::from("true")))],
            Atom::from("false"),
        ));

        erase(&mut context, &nat_match, &bool_type).unwrap();
    }

    #[test]
    fn erase_nat_match_rejects_non_nat_head() {
        let mut context = context();

        let bool_type = Term::from(AtomType::new(["false", "true"]));

        let nat_match = Term::from(NatMatch::dispatch(
            Prim::Int(0),
            Some("m"),
            AtomType::new(["false", "true"]),
            [(0u32, Term::from(Atom::from("true")))],
            Atom::from("false"),
        ));

        assert!(matches!(
            erase(&mut context, &nat_match, &bool_type),
            Err(Error::NotNatType { .. })
        ));
    }

    #[test]
    fn erase_lst_append() {
        let mut context = context();

        let arr_nat = Subterm::Prim(Prim::arr_type(Subterm::Prim(Prim::NatType))).into();
        context.assume("xs", &arr_nat);
        context.assume("n", &Subterm::Prim(Prim::NatType).into());

        let append = Subterm::Prim(Prim::arr_append(Var::free("xs"), Var::free("n"))).into();
        assert_eq!(infer(&mut context, &append).unwrap(), arr_nat);
        erase(&mut context, &append, &arr_nat).unwrap();
    }

    #[test]
    fn erase_three_field_tuple_type_and_value() {
        let mut context = context();

        let tuple_type = Term::from(TupleType::new([
            ("x", Term::from(AtomType::new(["a"]))),
            ("y", Term::from(AtomType::new(["b"]))),
            ("z", Term::from(AtomType::new(["c"]))),
        ]));

        erase(&mut context, &tuple_type, &Type.into()).unwrap();

        let tuple = Term::from(Tuple::new([
            Term::from(Atom::from("a")),
            Term::from(Atom::from("b")),
            Term::from(Atom::from("c")),
        ]));

        erase(&mut context, &tuple, &tuple_type).unwrap();
    }

    #[test]
    fn erase_bin_concat() {
        let mut context = context();

        let bin_type = Subterm::Prim(Prim::BinType).into();
        let concat = Subterm::Prim(Prim::bin_concat([
            Subterm::Prim(Prim::Bin(vec![1, 2])),
            Subterm::Prim(Prim::Bin(vec![3, 4])),
        ]))
        .into();

        erase(&mut context, &concat, &bin_type).unwrap();
    }

    #[test]
    fn erase_bin_concat_rejects_wrong_expected_type() {
        let mut context = context();

        let concat = Subterm::Prim(Prim::bin_concat([
            Subterm::Prim(Prim::Bin(vec![1])),
            Subterm::Prim(Prim::Bin(vec![2])),
        ]))
        .into();

        assert!(matches!(
            erase(&mut context, &concat, &Subterm::Prim(Prim::NatType).into()),
            Err(Error::TypeMismatch { .. })
        ));
    }

    #[test]
    fn erase_arr_concat() {
        let mut context = context();

        let arr_nat = Subterm::Prim(Prim::arr_type(Subterm::Prim(Prim::NatType))).into();
        context.assume("xs", &arr_nat);
        context.assume("ys", &arr_nat);

        let concat = Subterm::Prim(Prim::arr_concat([Var::free("xs"), Var::free("ys")])).into();

        erase(&mut context, &concat, &arr_nat).unwrap();
    }

    #[test]
    fn erase_arr_concat_rejects_wrong_expected_type() {
        let mut context = context();

        let arr_nat = Subterm::Prim(Prim::arr_type(Subterm::Prim(Prim::NatType))).into();
        context.assume("xs", &arr_nat);
        context.assume("ys", &arr_nat);

        let concat = Subterm::Prim(Prim::arr_concat([Var::free("xs"), Var::free("ys")])).into();

        assert!(matches!(
            erase(&mut context, &concat, &Subterm::Prim(Prim::NatType).into()),
            Err(Error::NotAnArrayType { .. })
        ));
    }
}
