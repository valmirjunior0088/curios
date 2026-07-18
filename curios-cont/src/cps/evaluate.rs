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
    let nat31 = |value: u64| (value < (1 << 31)).then_some(CpsLiteral::Nat(value as u32));
    let int31 = |value: i64| {
        ((-(1 << 30))..(1 << 30))
            .contains(&value)
            .then_some(CpsLiteral::Int(value as i32))
    };
    let wrap31s = |value: i32| value.wrapping_shl(1) >> 1;

    match op {
        CpsPrimOp::NatEql => bln(nat(0)? == nat(1)?),
        CpsPrimOp::NatNeq => bln(nat(0)? != nat(1)?),
        CpsPrimOp::NatAdd => nat31(nat(0)? as u64 + nat(1)? as u64),
        CpsPrimOp::NatSub => Some(CpsLiteral::Nat(nat(0)?.saturating_sub(nat(1)?))),
        CpsPrimOp::NatMul => nat31(nat(0)? as u64 * nat(1)? as u64),
        CpsPrimOp::NatLt => bln(nat(0)? < nat(1)?),
        CpsPrimOp::NatDiv => Some(CpsLiteral::Nat(
            nat(0)? / (nat(1)? != 0).then_some(nat(1)?)?,
        )),
        CpsPrimOp::NatRem => Some(CpsLiteral::Nat(
            nat(0)? % (nat(1)? != 0).then_some(nat(1)?)?,
        )),
        CpsPrimOp::NatGt => bln(nat(0)? > nat(1)?),
        CpsPrimOp::NatLte => bln(nat(0)? <= nat(1)?),
        CpsPrimOp::NatGte => bln(nat(0)? >= nat(1)?),
        CpsPrimOp::NatAnd => Some(CpsLiteral::Nat(nat(0)? & nat(1)?)),
        CpsPrimOp::NatOr => Some(CpsLiteral::Nat(nat(0)? | nat(1)?)),
        CpsPrimOp::NatXor => Some(CpsLiteral::Nat(nat(0)? ^ nat(1)?)),
        CpsPrimOp::NatShl => Some(CpsLiteral::Nat(nat(0)?.wrapping_shl(nat(1)?) & 0x7fff_ffff)),
        CpsPrimOp::NatShr => Some(CpsLiteral::Nat(nat(0)?.wrapping_shr(nat(1)?))),
        CpsPrimOp::NatRotl => nat31(nat(0)?.rotate_left(nat(1)?) as u64),
        CpsPrimOp::NatRotr => nat31(nat(0)?.rotate_right(nat(1)?) as u64),
        CpsPrimOp::NatClz => Some(CpsLiteral::Nat(nat(0)?.leading_zeros())),
        CpsPrimOp::NatCtz => Some(CpsLiteral::Nat(nat(0)?.trailing_zeros())),
        CpsPrimOp::NatPopcnt => Some(CpsLiteral::Nat(nat(0)?.count_ones())),
        CpsPrimOp::NatEqz => bln(nat(0)? == 0),
        CpsPrimOp::NatToInt => Some(CpsLiteral::Int(wrap31s(nat(0)? as i32))),
        CpsPrimOp::NatToFlt => Some(CpsLiteral::Flt(nat(0)? as f32)),
        CpsPrimOp::IntEql => bln(int(0)? == int(1)?),
        CpsPrimOp::IntNeq => bln(int(0)? != int(1)?),
        CpsPrimOp::IntAdd => int31(int(0)? as i64 + int(1)? as i64),
        CpsPrimOp::IntSub => int31(int(0)? as i64 - int(1)? as i64),
        CpsPrimOp::IntMul => int31(int(0)? as i64 * int(1)? as i64),
        CpsPrimOp::IntDiv => int31(int(0)? as i64 / (int(1)? != 0).then_some(int(1)?)? as i64),
        CpsPrimOp::IntRem => Some(CpsLiteral::Int(
            int(0)? % (int(1)? != 0).then_some(int(1)?)?,
        )),
        CpsPrimOp::IntLt => bln(int(0)? < int(1)?),
        CpsPrimOp::IntGt => bln(int(0)? > int(1)?),
        CpsPrimOp::IntLte => bln(int(0)? <= int(1)?),
        CpsPrimOp::IntGte => bln(int(0)? >= int(1)?),
        CpsPrimOp::IntAnd => Some(CpsLiteral::Int(wrap31s(int(0)? & int(1)?))),
        CpsPrimOp::IntOr => Some(CpsLiteral::Int(wrap31s(int(0)? | int(1)?))),
        CpsPrimOp::IntXor => Some(CpsLiteral::Int(wrap31s(int(0)? ^ int(1)?))),
        CpsPrimOp::IntShl => Some(CpsLiteral::Int(wrap31s(
            int(0)?.wrapping_shl(int(1)? as u32),
        ))),
        CpsPrimOp::IntShr => Some(CpsLiteral::Int(wrap31s(
            int(0)?.wrapping_shr(int(1)? as u32),
        ))),
        CpsPrimOp::IntRotl => int31((int(0)? as u32).rotate_left(int(1)? as u32) as i32 as i64),
        CpsPrimOp::IntRotr => int31((int(0)? as u32).rotate_right(int(1)? as u32) as i32 as i64),
        CpsPrimOp::IntClz => Some(CpsLiteral::Int((int(0)? as u32).leading_zeros() as i32)),
        CpsPrimOp::IntCtz => Some(CpsLiteral::Int((int(0)? as u32).trailing_zeros() as i32)),
        CpsPrimOp::IntPopcnt => Some(CpsLiteral::Int((int(0)? as u32).count_ones() as i32)),
        CpsPrimOp::IntEqz => bln(int(0)? == 0),
        CpsPrimOp::IntToNat => Some(CpsLiteral::Nat(int(0)? as u32 & 0x7fff_ffff)),
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
        CpsPrimOp::FltMin if !flt(0)?.is_nan() && !flt(1)?.is_nan() => {
            Some(CpsLiteral::Flt(flt(0)?.min(flt(1)?)))
        }
        CpsPrimOp::FltMax if !flt(0)?.is_nan() && !flt(1)?.is_nan() => {
            Some(CpsLiteral::Flt(flt(0)?.max(flt(1)?)))
        }
        CpsPrimOp::FltNeg => Some(CpsLiteral::Flt(-flt(0)?)),
        CpsPrimOp::FltAbs => Some(CpsLiteral::Flt(flt(0)?.abs())),
        CpsPrimOp::FltSqrt => Some(CpsLiteral::Flt(flt(0)?.sqrt())),
        CpsPrimOp::FltFloor => Some(CpsLiteral::Flt(flt(0)?.floor())),
        CpsPrimOp::FltCeil => Some(CpsLiteral::Flt(flt(0)?.ceil())),
        CpsPrimOp::FltTrunc => Some(CpsLiteral::Flt(flt(0)?.trunc())),
        CpsPrimOp::FltNearest => Some(CpsLiteral::Flt(flt(0)?.round_ties_even())),
        CpsPrimOp::FltCopysign => Some(CpsLiteral::Flt(flt(0)?.copysign(flt(1)?))),
        CpsPrimOp::FltToNat => {
            let value = flt(0)?;
            let truncated = value.trunc();
            (value.is_finite() && truncated > -1.0 && truncated < 2_147_483_648.0)
                .then_some(CpsLiteral::Nat(truncated as u32))
        }
        CpsPrimOp::FltToInt => {
            let value = flt(0)?;
            let truncated = value.trunc();
            (value.is_finite() && (-1_073_741_824.0..1_073_741_824.0).contains(&truncated))
                .then_some(CpsLiteral::Int(truncated as i32))
        }
        _ => None,
    }
}
