use {
    super::*,
    curios_base::{
        flt_max, flt_min, flt_to_int, flt_to_nat, int_add, int_div, int_mul, int_rem, int_rotl,
        int_rotr, int_shl, int_shr, int_sub, int_to_nat, nat_add, nat_div, nat_mul, nat_rem,
        nat_rotl, nat_rotr, nat_shl, nat_shr, nat_sub, nat_to_int,
    },
};

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
    let bool_ = |value: bool| Some(CpsLiteral::Nat(value as u32));

    match op {
        CpsPrimOp::NatEql => bool_(nat(0)? == nat(1)?),
        CpsPrimOp::NatNeq => bool_(nat(0)? != nat(1)?),
        CpsPrimOp::NatAdd => Some(CpsLiteral::Nat(nat_add(nat(0)?, nat(1)?))),
        CpsPrimOp::NatSub => Some(CpsLiteral::Nat(nat_sub(nat(0)?, nat(1)?))),
        CpsPrimOp::NatMul => Some(CpsLiteral::Nat(nat_mul(nat(0)?, nat(1)?))),
        CpsPrimOp::NatLt => bool_(nat(0)? < nat(1)?),
        CpsPrimOp::NatDiv => Some(CpsLiteral::Nat(nat_div(nat(0)?, nat(1)?).ok()?)),
        CpsPrimOp::NatRem => Some(CpsLiteral::Nat(nat_rem(nat(0)?, nat(1)?).ok()?)),
        CpsPrimOp::NatGt => bool_(nat(0)? > nat(1)?),
        CpsPrimOp::NatLte => bool_(nat(0)? <= nat(1)?),
        CpsPrimOp::NatGte => bool_(nat(0)? >= nat(1)?),
        CpsPrimOp::NatAnd => Some(CpsLiteral::Nat(nat(0)? & nat(1)?)),
        CpsPrimOp::NatOr => Some(CpsLiteral::Nat(nat(0)? | nat(1)?)),
        CpsPrimOp::NatXor => Some(CpsLiteral::Nat(nat(0)? ^ nat(1)?)),
        CpsPrimOp::NatShl => Some(CpsLiteral::Nat(nat_shl(nat(0)?, nat(1)?))),
        CpsPrimOp::NatShr => Some(CpsLiteral::Nat(nat_shr(nat(0)?, nat(1)?))),
        CpsPrimOp::NatRotl => Some(CpsLiteral::Nat(nat_rotl(nat(0)?, nat(1)?))),
        CpsPrimOp::NatRotr => Some(CpsLiteral::Nat(nat_rotr(nat(0)?, nat(1)?))),
        CpsPrimOp::NatClz => Some(CpsLiteral::Nat(nat(0)?.leading_zeros())),
        CpsPrimOp::NatCtz => Some(CpsLiteral::Nat(nat(0)?.trailing_zeros())),
        CpsPrimOp::NatPopcnt => Some(CpsLiteral::Nat(nat(0)?.count_ones())),
        CpsPrimOp::NatEqz => bool_(nat(0)? == 0),
        CpsPrimOp::NatToInt => Some(CpsLiteral::Int(nat_to_int(nat(0)?))),
        CpsPrimOp::NatToFlt => Some(CpsLiteral::Flt(nat(0)? as f32)),
        CpsPrimOp::IntEql => bool_(int(0)? == int(1)?),
        CpsPrimOp::IntNeq => bool_(int(0)? != int(1)?),
        CpsPrimOp::IntAdd => Some(CpsLiteral::Int(int_add(int(0)?, int(1)?))),
        CpsPrimOp::IntSub => Some(CpsLiteral::Int(int_sub(int(0)?, int(1)?))),
        CpsPrimOp::IntMul => Some(CpsLiteral::Int(int_mul(int(0)?, int(1)?))),
        CpsPrimOp::IntDiv => Some(CpsLiteral::Int(int_div(int(0)?, int(1)?).ok()?)),
        CpsPrimOp::IntRem => Some(CpsLiteral::Int(int_rem(int(0)?, int(1)?).ok()?)),
        CpsPrimOp::IntLt => bool_(int(0)? < int(1)?),
        CpsPrimOp::IntGt => bool_(int(0)? > int(1)?),
        CpsPrimOp::IntLte => bool_(int(0)? <= int(1)?),
        CpsPrimOp::IntGte => bool_(int(0)? >= int(1)?),
        CpsPrimOp::IntAnd => Some(CpsLiteral::Int(int(0)? & int(1)?)),
        CpsPrimOp::IntOr => Some(CpsLiteral::Int(int(0)? | int(1)?)),
        CpsPrimOp::IntXor => Some(CpsLiteral::Int(int(0)? ^ int(1)?)),
        CpsPrimOp::IntShl => Some(CpsLiteral::Int(int_shl(int(0)?, int(1)?))),
        CpsPrimOp::IntShr => Some(CpsLiteral::Int(int_shr(int(0)?, int(1)?))),
        CpsPrimOp::IntRotl => Some(CpsLiteral::Int(int_rotl(int(0)?, int(1)?))),
        CpsPrimOp::IntRotr => Some(CpsLiteral::Int(int_rotr(int(0)?, int(1)?))),
        CpsPrimOp::IntClz => Some(CpsLiteral::Int((int(0)? as u32).leading_zeros() as i32)),
        CpsPrimOp::IntCtz => Some(CpsLiteral::Int((int(0)? as u32).trailing_zeros() as i32)),
        CpsPrimOp::IntPopcnt => Some(CpsLiteral::Int((int(0)? as u32).count_ones() as i32)),
        CpsPrimOp::IntEqz => bool_(int(0)? == 0),
        CpsPrimOp::IntToNat => Some(CpsLiteral::Nat(int_to_nat(int(0)?))),
        CpsPrimOp::IntToFlt => Some(CpsLiteral::Flt(int(0)? as f32)),
        CpsPrimOp::FltAdd => Some(CpsLiteral::Flt(flt(0)? + flt(1)?)),
        CpsPrimOp::FltSub => Some(CpsLiteral::Flt(flt(0)? - flt(1)?)),
        CpsPrimOp::FltMul => Some(CpsLiteral::Flt(flt(0)? * flt(1)?)),
        CpsPrimOp::FltDiv => Some(CpsLiteral::Flt(flt(0)? / flt(1)?)),
        CpsPrimOp::FltRem => Some(CpsLiteral::Flt(flt(0)? % flt(1)?)),
        CpsPrimOp::FltEql => bool_(flt(0)? == flt(1)?),
        CpsPrimOp::FltNeq => bool_(flt(0)? != flt(1)?),
        CpsPrimOp::FltLt => bool_(flt(0)? < flt(1)?),
        CpsPrimOp::FltGt => bool_(flt(0)? > flt(1)?),
        CpsPrimOp::FltLte => bool_(flt(0)? <= flt(1)?),
        CpsPrimOp::FltGte => bool_(flt(0)? >= flt(1)?),
        CpsPrimOp::FltMin => Some(CpsLiteral::Flt(flt_min(flt(0)?, flt(1)?)?)),
        CpsPrimOp::FltMax => Some(CpsLiteral::Flt(flt_max(flt(0)?, flt(1)?)?)),
        CpsPrimOp::FltNeg => Some(CpsLiteral::Flt(-flt(0)?)),
        CpsPrimOp::FltAbs => Some(CpsLiteral::Flt(flt(0)?.abs())),
        CpsPrimOp::FltSqrt => Some(CpsLiteral::Flt(flt(0)?.sqrt())),
        CpsPrimOp::FltFloor => Some(CpsLiteral::Flt(flt(0)?.floor())),
        CpsPrimOp::FltCeil => Some(CpsLiteral::Flt(flt(0)?.ceil())),
        CpsPrimOp::FltTrunc => Some(CpsLiteral::Flt(flt(0)?.trunc())),
        CpsPrimOp::FltNearest => Some(CpsLiteral::Flt(flt(0)?.round_ties_even())),
        CpsPrimOp::FltCopysign => Some(CpsLiteral::Flt(flt(0)?.copysign(flt(1)?))),
        CpsPrimOp::FltToNat => Some(CpsLiteral::Nat(flt_to_nat(flt(0)?)?)),
        CpsPrimOp::FltToInt => Some(CpsLiteral::Int(flt_to_int(flt(0)?)?)),
        _ => None,
    }
}
