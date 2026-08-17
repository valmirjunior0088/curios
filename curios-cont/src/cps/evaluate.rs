use {
    super::*,
    curios_num::{
        flt_max, flt_min, flt_to_int, flt_to_nat, int_add, int_div, int_mul, int_rem, int_rotl,
        int_rotr, int_shl, int_shr, int_sub, int_to_nat, nat_add, nat_div, nat_mul, nat_rem,
        nat_rotl, nat_rotr, nat_shl, nat_shr, nat_sub, nat_to_int,
    },
};

pub(super) fn evaluate(op: CpsIntrinsicOp, args: &[CpsAtom]) -> Option<CpsLiteral> {
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
        CpsIntrinsicOp::NatEql => bool_(nat(0)? == nat(1)?),
        // Fold only when the bounds hold; a constant that would trap stays an instruction so the trap keeps its place.
        CpsIntrinsicOp::WindowExtent => {
            let (s, e, len) = (nat(0)?, nat(1)?, nat(2)?);
            (s <= e && e <= len).then(|| CpsLiteral::Nat(e - s))
        }
        CpsIntrinsicOp::NatNeq => bool_(nat(0)? != nat(1)?),
        CpsIntrinsicOp::NatAdd => Some(CpsLiteral::Nat(nat_add(nat(0)?, nat(1)?))),
        CpsIntrinsicOp::NatSub => Some(CpsLiteral::Nat(nat_sub(nat(0)?, nat(1)?))),
        CpsIntrinsicOp::NatMul => Some(CpsLiteral::Nat(nat_mul(nat(0)?, nat(1)?))),
        CpsIntrinsicOp::NatLt => bool_(nat(0)? < nat(1)?),
        CpsIntrinsicOp::NatDiv => Some(CpsLiteral::Nat(nat_div(nat(0)?, nat(1)?).ok()?)),
        CpsIntrinsicOp::NatRem => Some(CpsLiteral::Nat(nat_rem(nat(0)?, nat(1)?).ok()?)),
        CpsIntrinsicOp::NatGt => bool_(nat(0)? > nat(1)?),
        CpsIntrinsicOp::NatLte => bool_(nat(0)? <= nat(1)?),
        CpsIntrinsicOp::NatGte => bool_(nat(0)? >= nat(1)?),
        CpsIntrinsicOp::NatAnd => Some(CpsLiteral::Nat(nat(0)? & nat(1)?)),
        CpsIntrinsicOp::NatOr => Some(CpsLiteral::Nat(nat(0)? | nat(1)?)),
        CpsIntrinsicOp::NatXor => Some(CpsLiteral::Nat(nat(0)? ^ nat(1)?)),
        CpsIntrinsicOp::NatShl => Some(CpsLiteral::Nat(nat_shl(nat(0)?, nat(1)?))),
        CpsIntrinsicOp::NatShr => Some(CpsLiteral::Nat(nat_shr(nat(0)?, nat(1)?))),
        CpsIntrinsicOp::NatRotl => Some(CpsLiteral::Nat(nat_rotl(nat(0)?, nat(1)?))),
        CpsIntrinsicOp::NatRotr => Some(CpsLiteral::Nat(nat_rotr(nat(0)?, nat(1)?))),
        CpsIntrinsicOp::NatClz => Some(CpsLiteral::Nat(nat(0)?.leading_zeros())),
        CpsIntrinsicOp::NatCtz => Some(CpsLiteral::Nat(nat(0)?.trailing_zeros())),
        CpsIntrinsicOp::NatPopcnt => Some(CpsLiteral::Nat(nat(0)?.count_ones())),
        CpsIntrinsicOp::NatEqz => bool_(nat(0)? == 0),
        CpsIntrinsicOp::NatToInt => Some(CpsLiteral::Int(nat_to_int(nat(0)?)?)),
        CpsIntrinsicOp::NatToFlt => Some(CpsLiteral::Flt(nat(0)? as f32)),
        CpsIntrinsicOp::IntEql => bool_(int(0)? == int(1)?),
        CpsIntrinsicOp::IntNeq => bool_(int(0)? != int(1)?),
        CpsIntrinsicOp::IntAdd => Some(CpsLiteral::Int(int_add(int(0)?, int(1)?))),
        CpsIntrinsicOp::IntSub => Some(CpsLiteral::Int(int_sub(int(0)?, int(1)?))),
        CpsIntrinsicOp::IntMul => Some(CpsLiteral::Int(int_mul(int(0)?, int(1)?))),
        CpsIntrinsicOp::IntDiv => Some(CpsLiteral::Int(int_div(int(0)?, int(1)?).ok()?)),
        CpsIntrinsicOp::IntRem => Some(CpsLiteral::Int(int_rem(int(0)?, int(1)?).ok()?)),
        CpsIntrinsicOp::IntLt => bool_(int(0)? < int(1)?),
        CpsIntrinsicOp::IntGt => bool_(int(0)? > int(1)?),
        CpsIntrinsicOp::IntLte => bool_(int(0)? <= int(1)?),
        CpsIntrinsicOp::IntGte => bool_(int(0)? >= int(1)?),
        CpsIntrinsicOp::IntAnd => Some(CpsLiteral::Int(int(0)? & int(1)?)),
        CpsIntrinsicOp::IntOr => Some(CpsLiteral::Int(int(0)? | int(1)?)),
        CpsIntrinsicOp::IntXor => Some(CpsLiteral::Int(int(0)? ^ int(1)?)),
        CpsIntrinsicOp::IntShl => Some(CpsLiteral::Int(int_shl(int(0)?, int(1)?))),
        CpsIntrinsicOp::IntShr => Some(CpsLiteral::Int(int_shr(int(0)?, int(1)?))),
        CpsIntrinsicOp::IntRotl => Some(CpsLiteral::Int(int_rotl(int(0)?, int(1)?))),
        CpsIntrinsicOp::IntRotr => Some(CpsLiteral::Int(int_rotr(int(0)?, int(1)?))),
        CpsIntrinsicOp::IntClz => Some(CpsLiteral::Int((int(0)? as u32).leading_zeros() as i32)),
        CpsIntrinsicOp::IntCtz => Some(CpsLiteral::Int((int(0)? as u32).trailing_zeros() as i32)),
        CpsIntrinsicOp::IntPopcnt => Some(CpsLiteral::Int((int(0)? as u32).count_ones() as i32)),
        CpsIntrinsicOp::IntEqz => bool_(int(0)? == 0),
        CpsIntrinsicOp::IntToNat => Some(CpsLiteral::Nat(int_to_nat(int(0)?)?)),
        CpsIntrinsicOp::IntToFlt => Some(CpsLiteral::Flt(int(0)? as f32)),
        CpsIntrinsicOp::FltAdd => Some(CpsLiteral::Flt(flt(0)? + flt(1)?)),
        CpsIntrinsicOp::FltSub => Some(CpsLiteral::Flt(flt(0)? - flt(1)?)),
        CpsIntrinsicOp::FltMul => Some(CpsLiteral::Flt(flt(0)? * flt(1)?)),
        CpsIntrinsicOp::FltDiv => Some(CpsLiteral::Flt(flt(0)? / flt(1)?)),
        CpsIntrinsicOp::FltRem => Some(CpsLiteral::Flt(flt(0)? % flt(1)?)),
        CpsIntrinsicOp::FltEql => bool_(flt(0)? == flt(1)?),
        CpsIntrinsicOp::FltNeq => bool_(flt(0)? != flt(1)?),
        CpsIntrinsicOp::FltLt => bool_(flt(0)? < flt(1)?),
        CpsIntrinsicOp::FltGt => bool_(flt(0)? > flt(1)?),
        CpsIntrinsicOp::FltLte => bool_(flt(0)? <= flt(1)?),
        CpsIntrinsicOp::FltGte => bool_(flt(0)? >= flt(1)?),
        CpsIntrinsicOp::FltMin => Some(CpsLiteral::Flt(flt_min(flt(0)?, flt(1)?)?)),
        CpsIntrinsicOp::FltMax => Some(CpsLiteral::Flt(flt_max(flt(0)?, flt(1)?)?)),
        CpsIntrinsicOp::FltNeg => Some(CpsLiteral::Flt(-flt(0)?)),
        CpsIntrinsicOp::FltAbs => Some(CpsLiteral::Flt(flt(0)?.abs())),
        CpsIntrinsicOp::FltSqrt => Some(CpsLiteral::Flt(flt(0)?.sqrt())),
        CpsIntrinsicOp::FltFloor => Some(CpsLiteral::Flt(flt(0)?.floor())),
        CpsIntrinsicOp::FltCeil => Some(CpsLiteral::Flt(flt(0)?.ceil())),
        CpsIntrinsicOp::FltTrunc => Some(CpsLiteral::Flt(flt(0)?.trunc())),
        CpsIntrinsicOp::FltNearest => Some(CpsLiteral::Flt(flt(0)?.round_ties_even())),
        CpsIntrinsicOp::FltCopysign => Some(CpsLiteral::Flt(flt(0)?.copysign(flt(1)?))),
        CpsIntrinsicOp::FltToNat => Some(CpsLiteral::Nat(flt_to_nat(flt(0)?)?)),
        CpsIntrinsicOp::FltToInt => Some(CpsLiteral::Int(flt_to_int(flt(0)?)?)),
        // Folds over the *runtime* representation, not the literal's kind: `Nat` and `Int` ride i31, while an `Flt` is a boxed struct and a `Bin` a rope reference, so both answer 0.
        CpsIntrinsicOp::IsImmediate => Some(CpsLiteral::Nat(match literals[0] {
            CpsLiteral::Nat(_) | CpsLiteral::Int(_) => 1,
            CpsLiteral::Flt(_) | CpsLiteral::Bin(_, _) => 0,
        })),
        _ => None,
    }
}
