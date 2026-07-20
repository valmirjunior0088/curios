use super::*;

pub(super) fn evaluate(op: CpsPrimOp, args: &[CpsAtom]) -> Option<CpsLiteral> {
    let literals = args
        .iter()
        .map(|atom| match atom {
            CpsAtom::Literal(literal) => Some(literal),
            _ => None,
        })
        .collect::<Option<Vec<_>>>()?;
    let nat = |index: usize| match literals[index] {
        CpsLiteral::Nat(value) => Some(*value),
        _ => None,
    };
    let int = |index: usize| match literals[index] {
        CpsLiteral::Int(value) => Some(*value),
        _ => None,
    };
    let flt = |index: usize| match literals[index] {
        CpsLiteral::Flt(value) => Some(*value),
        _ => None,
    };
    let bln = |value: bool| Some(CpsLiteral::Nat(value as u32));

    match op {
        CpsPrimOp::NatEql => bln(nat(0)? == nat(1)?),
        CpsPrimOp::NatNeq => bln(nat(0)? != nat(1)?),
        CpsPrimOp::NatAdd => Some(CpsLiteral::Nat(curios_base::nat_add(nat(0)?, nat(1)?))),
        CpsPrimOp::NatSub => Some(CpsLiteral::Nat(curios_base::nat_sub(nat(0)?, nat(1)?))),
        CpsPrimOp::NatMul => Some(CpsLiteral::Nat(curios_base::nat_mul(nat(0)?, nat(1)?))),
        CpsPrimOp::NatLt => bln(nat(0)? < nat(1)?),
        CpsPrimOp::NatDiv => Some(CpsLiteral::Nat(
            curios_base::nat_div(nat(0)?, nat(1)?).ok()?,
        )),
        CpsPrimOp::NatRem => Some(CpsLiteral::Nat(
            curios_base::nat_rem(nat(0)?, nat(1)?).ok()?,
        )),
        CpsPrimOp::NatGt => bln(nat(0)? > nat(1)?),
        CpsPrimOp::NatLte => bln(nat(0)? <= nat(1)?),
        CpsPrimOp::NatGte => bln(nat(0)? >= nat(1)?),
        CpsPrimOp::NatAnd => Some(CpsLiteral::Nat(nat(0)? & nat(1)?)),
        CpsPrimOp::NatOr => Some(CpsLiteral::Nat(nat(0)? | nat(1)?)),
        CpsPrimOp::NatXor => Some(CpsLiteral::Nat(nat(0)? ^ nat(1)?)),
        CpsPrimOp::NatShl => Some(CpsLiteral::Nat(curios_base::nat_shl(nat(0)?, nat(1)?))),
        CpsPrimOp::NatShr => Some(CpsLiteral::Nat(curios_base::nat_shr(nat(0)?, nat(1)?))),
        CpsPrimOp::NatRotl => Some(CpsLiteral::Nat(curios_base::nat_rotl(nat(0)?, nat(1)?))),
        CpsPrimOp::NatRotr => Some(CpsLiteral::Nat(curios_base::nat_rotr(nat(0)?, nat(1)?))),
        CpsPrimOp::NatClz => Some(CpsLiteral::Nat(nat(0)?.leading_zeros())),
        CpsPrimOp::NatCtz => Some(CpsLiteral::Nat(nat(0)?.trailing_zeros())),
        CpsPrimOp::NatPopcnt => Some(CpsLiteral::Nat(nat(0)?.count_ones())),
        CpsPrimOp::NatEqz => bln(nat(0)? == 0),
        CpsPrimOp::NatToInt => Some(CpsLiteral::Int(curios_base::nat_to_int(nat(0)?))),
        CpsPrimOp::NatToFlt => Some(CpsLiteral::Flt(nat(0)? as f32)),
        CpsPrimOp::IntEql => bln(int(0)? == int(1)?),
        CpsPrimOp::IntNeq => bln(int(0)? != int(1)?),
        CpsPrimOp::IntAdd => Some(CpsLiteral::Int(curios_base::int_add(int(0)?, int(1)?))),
        CpsPrimOp::IntSub => Some(CpsLiteral::Int(curios_base::int_sub(int(0)?, int(1)?))),
        CpsPrimOp::IntMul => Some(CpsLiteral::Int(curios_base::int_mul(int(0)?, int(1)?))),
        CpsPrimOp::IntDiv => Some(CpsLiteral::Int(
            curios_base::int_div(int(0)?, int(1)?).ok()?,
        )),
        CpsPrimOp::IntRem => Some(CpsLiteral::Int(
            curios_base::int_rem(int(0)?, int(1)?).ok()?,
        )),
        CpsPrimOp::IntLt => bln(int(0)? < int(1)?),
        CpsPrimOp::IntGt => bln(int(0)? > int(1)?),
        CpsPrimOp::IntLte => bln(int(0)? <= int(1)?),
        CpsPrimOp::IntGte => bln(int(0)? >= int(1)?),
        CpsPrimOp::IntAnd => Some(CpsLiteral::Int(int(0)? & int(1)?)),
        CpsPrimOp::IntOr => Some(CpsLiteral::Int(int(0)? | int(1)?)),
        CpsPrimOp::IntXor => Some(CpsLiteral::Int(int(0)? ^ int(1)?)),
        CpsPrimOp::IntShl => Some(CpsLiteral::Int(curios_base::int_shl(int(0)?, int(1)?))),
        CpsPrimOp::IntShr => Some(CpsLiteral::Int(curios_base::int_shr(int(0)?, int(1)?))),
        CpsPrimOp::IntRotl => Some(CpsLiteral::Int(curios_base::int_rotl(int(0)?, int(1)?))),
        CpsPrimOp::IntRotr => Some(CpsLiteral::Int(curios_base::int_rotr(int(0)?, int(1)?))),
        CpsPrimOp::IntClz => Some(CpsLiteral::Int((int(0)? as u32).leading_zeros() as i32)),
        CpsPrimOp::IntCtz => Some(CpsLiteral::Int((int(0)? as u32).trailing_zeros() as i32)),
        CpsPrimOp::IntPopcnt => Some(CpsLiteral::Int((int(0)? as u32).count_ones() as i32)),
        CpsPrimOp::IntEqz => bln(int(0)? == 0),
        CpsPrimOp::IntToNat => Some(CpsLiteral::Nat(curios_base::int_to_nat(int(0)?))),
        CpsPrimOp::IntToFlt => Some(CpsLiteral::Flt(int(0)? as f32)),
        CpsPrimOp::FltAdd => Some(CpsLiteral::Flt(flt(0)? + flt(1)?)),
        CpsPrimOp::FltSub => Some(CpsLiteral::Flt(flt(0)? - flt(1)?)),
        CpsPrimOp::FltMul => Some(CpsLiteral::Flt(flt(0)? * flt(1)?)),
        CpsPrimOp::FltDiv => Some(CpsLiteral::Flt(flt(0)? / flt(1)?)),
        CpsPrimOp::FltRem => Some(CpsLiteral::Flt(flt(0)? % flt(1)?)),
        CpsPrimOp::FltEql => bln(flt(0)? == flt(1)?),
        CpsPrimOp::FltNeq => bln(flt(0)? != flt(1)?),
        CpsPrimOp::FltLt => bln(flt(0)? < flt(1)?),
        CpsPrimOp::FltGt => bln(flt(0)? > flt(1)?),
        CpsPrimOp::FltLte => bln(flt(0)? <= flt(1)?),
        CpsPrimOp::FltGte => bln(flt(0)? >= flt(1)?),
        CpsPrimOp::FltMin => Some(CpsLiteral::Flt(curios_base::flt_min(flt(0)?, flt(1)?)?)),
        CpsPrimOp::FltMax => Some(CpsLiteral::Flt(curios_base::flt_max(flt(0)?, flt(1)?)?)),
        CpsPrimOp::FltNeg => Some(CpsLiteral::Flt(-flt(0)?)),
        CpsPrimOp::FltAbs => Some(CpsLiteral::Flt(flt(0)?.abs())),
        CpsPrimOp::FltSqrt => Some(CpsLiteral::Flt(flt(0)?.sqrt())),
        CpsPrimOp::FltFloor => Some(CpsLiteral::Flt(flt(0)?.floor())),
        CpsPrimOp::FltCeil => Some(CpsLiteral::Flt(flt(0)?.ceil())),
        CpsPrimOp::FltTrunc => Some(CpsLiteral::Flt(flt(0)?.trunc())),
        CpsPrimOp::FltNearest => Some(CpsLiteral::Flt(flt(0)?.round_ties_even())),
        CpsPrimOp::FltCopysign => Some(CpsLiteral::Flt(flt(0)?.copysign(flt(1)?))),
        CpsPrimOp::FltToNat => Some(CpsLiteral::Nat(curios_base::flt_to_nat(flt(0)?)?)),
        CpsPrimOp::FltToInt => Some(CpsLiteral::Int(curios_base::flt_to_int(flt(0)?)?)),
        _ => None,
    }
}
