use {
    super::*,
    curios_num::{
        Floating, Integer, Natural, flt_to_int, flt_to_nat, int_div, int_mul, int_rem, int_shl,
        int_shr, int_to_nat, nat_div, nat_mul, nat_rem, nat_shl, nat_shr, nat_sub,
    },
};

/// The allowance the growing folds take here: a result wider than the envelope cannot be materialized whatever it is, so building one buys nothing and folding it would let a literal count ask this pass for a numeral no machine holds.
const FOLD_ALLOWANCE_BITS: u64 = ENVELOPE_BITS as u64;

/// Fold one intrinsic over literal operands, or decline.
///
/// **A proven trap declines here, where `curios-ersd`'s fold records it.** This answer is two-way — a literal or nothing — so an operation whose value leaves its carrier is left standing, and the guard `into_wasm` emits for it traps at the execution point instead. Recording the trap would be more precise and is what the erased fold does with its three-way outcome; declining is correct either way, and it is the convention `NatDiv` already established for a zero divisor.
///
/// A growth decline uses that same answer. The carriers are unbounded from `curios-ersd` down, so multiplication and the left shifts are the two operations that can be asked to build without bound; past [`FOLD_ALLOWANCE_BITS`] they leave the operation standing exactly as a trap does.
pub(super) fn evaluate(op: CpsIntrinsic, args: &[CpsAtom]) -> Option<CpsLiteral> {
    let literals = args
        .iter()
        .map(|atom| match atom {
            CpsAtom::Literal(literal) => Some(literal),
            _ => None,
        })
        .collect::<Option<Vec<_>>>()?;
    let nat = |index: usize| match literals[index] {
        CpsLiteral::Nat(value) => Some(value),
        _ => None,
    };
    let int = |index: usize| match literals[index] {
        CpsLiteral::Int(value) => Some(value),
        _ => None,
    };
    // The model, never the host: every arm below computes what `curios-core` folded and what the emitted Wasm executes, which is one function of the operands rather than three.
    let flt = |index: usize| match literals[index] {
        CpsLiteral::Flt(value) => Some(*value),
        _ => None,
    };
    let bool_ = |value: bool| Some(CpsLiteral::Nat(Natural::from(value as u32)));
    let flt_ = |value: Floating| Some(CpsLiteral::Flt(value));

    match op {
        CpsIntrinsic::NatEql => bool_(nat(0)? == nat(1)?),
        // Fold only when the bounds hold; a constant that would trap stays an instruction so the trap keeps its place.
        CpsIntrinsic::WindowExtent => {
            let (start, count, len) = (nat(0)?, nat(1)?, nat(2)?);

            (&(start + count) <= len).then(|| CpsLiteral::Nat(count.clone()))
        }
        CpsIntrinsic::NatNeq => bool_(nat(0)? != nat(1)?),
        CpsIntrinsic::NatAdd => Some(CpsLiteral::Nat(nat(0)? + nat(1)?)),
        CpsIntrinsic::NatSub => Some(CpsLiteral::Nat(nat_sub(nat(0)?, nat(1)?))),
        CpsIntrinsic::NatMul => Some(CpsLiteral::Nat(nat_mul(
            nat(0)?,
            nat(1)?,
            FOLD_ALLOWANCE_BITS,
        )?)),
        CpsIntrinsic::NatLt => bool_(nat(0)? < nat(1)?),
        CpsIntrinsic::NatDiv => Some(CpsLiteral::Nat(nat_div(nat(0)?, nat(1)?).ok()?)),
        CpsIntrinsic::NatRem => Some(CpsLiteral::Nat(nat_rem(nat(0)?, nat(1)?).ok()?)),
        CpsIntrinsic::NatLe => bool_(nat(0)? <= nat(1)?),
        CpsIntrinsic::NatAnd => Some(CpsLiteral::Nat(nat(0)? & nat(1)?)),
        CpsIntrinsic::NatOr => Some(CpsLiteral::Nat(nat(0)? | nat(1)?)),
        CpsIntrinsic::NatXor => Some(CpsLiteral::Nat(nat(0)? ^ nat(1)?)),
        CpsIntrinsic::NatShl => Some(CpsLiteral::Nat(nat_shl(
            nat(0)?,
            nat(1)?,
            FOLD_ALLOWANCE_BITS,
        )?)),
        CpsIntrinsic::NatShr => Some(CpsLiteral::Nat(nat_shr(nat(0)?, nat(1)?))),
        CpsIntrinsic::NatEqz => bool_(nat(0)?.is_zero()),
        CpsIntrinsic::NatToInt => Some(CpsLiteral::Int(Integer::from(nat(0)?.clone()))),
        CpsIntrinsic::NatToFlt => flt_(Floating::of_natural(nat(0)?)),
        CpsIntrinsic::IntEql => bool_(int(0)? == int(1)?),
        CpsIntrinsic::IntNeq => bool_(int(0)? != int(1)?),
        CpsIntrinsic::IntAdd => Some(CpsLiteral::Int(int(0)?.clone() + int(1)?.clone())),
        CpsIntrinsic::IntSub => Some(CpsLiteral::Int(int(0)?.clone() - int(1)?.clone())),
        CpsIntrinsic::IntMul => Some(CpsLiteral::Int(int_mul(
            int(0)?,
            int(1)?,
            FOLD_ALLOWANCE_BITS,
        )?)),
        CpsIntrinsic::IntDiv => Some(CpsLiteral::Int(int_div(int(0)?, int(1)?).ok()?)),
        CpsIntrinsic::IntRem => Some(CpsLiteral::Int(int_rem(int(0)?, int(1)?).ok()?)),
        CpsIntrinsic::IntLt => bool_(int(0)? < int(1)?),
        CpsIntrinsic::IntLe => bool_(int(0)? <= int(1)?),
        CpsIntrinsic::IntAnd => Some(CpsLiteral::Int(int(0)?.clone() & int(1)?.clone())),
        CpsIntrinsic::IntOr => Some(CpsLiteral::Int(int(0)?.clone() | int(1)?.clone())),
        CpsIntrinsic::IntXor => Some(CpsLiteral::Int(int(0)?.clone() ^ int(1)?.clone())),
        CpsIntrinsic::IntShl => Some(CpsLiteral::Int(int_shl(
            int(0)?,
            nat(1)?,
            FOLD_ALLOWANCE_BITS,
        )?)),
        CpsIntrinsic::IntShr => Some(CpsLiteral::Int(int_shr(int(0)?, nat(1)?))),
        CpsIntrinsic::IntEqz => bool_(int(0)?.is_zero()),
        CpsIntrinsic::IntToNat => Some(CpsLiteral::Nat(int_to_nat(int(0)?).ok()?)),
        CpsIntrinsic::IntToFlt => flt_(Floating::of_integer(int(0)?)),
        CpsIntrinsic::FltAdd => flt_(flt(0)? + flt(1)?),
        CpsIntrinsic::FltSub => flt_(flt(0)? - flt(1)?),
        CpsIntrinsic::FltMul => flt_(flt(0)? * flt(1)?),
        CpsIntrinsic::FltDiv => flt_(flt(0)? / flt(1)?),
        CpsIntrinsic::FltRem => flt_(flt(0)? % flt(1)?),
        CpsIntrinsic::FltEql => bool_(flt(0)?.eql(flt(1)?)),
        CpsIntrinsic::FltNeq => bool_(flt(0)?.neq(flt(1)?)),
        CpsIntrinsic::FltLt => bool_(flt(0)?.lt(flt(1)?)),
        CpsIntrinsic::FltLe => bool_(flt(0)?.le(flt(1)?)),
        CpsIntrinsic::FltMin => flt_(flt(0)?.min(flt(1)?)),
        CpsIntrinsic::FltMax => flt_(flt(0)?.max(flt(1)?)),
        CpsIntrinsic::FltNeg => flt_(-flt(0)?),
        CpsIntrinsic::FltAbs => flt_(flt(0)?.abs()),
        CpsIntrinsic::FltSqrt => flt_(flt(0)?.sqrt()),
        CpsIntrinsic::FltFloor => flt_(flt(0)?.floor()),
        CpsIntrinsic::FltCeil => flt_(flt(0)?.ceil()),
        CpsIntrinsic::FltTrunc => flt_(flt(0)?.trunc()),
        CpsIntrinsic::FltNearest => flt_(flt(0)?.nearest()),
        CpsIntrinsic::FltCopysign => flt_(flt(0)?.copysign(flt(1)?)),
        CpsIntrinsic::FltToNat => Some(CpsLiteral::Nat(flt_to_nat(flt(0)?).ok()?)),
        CpsIntrinsic::FltToInt => Some(CpsLiteral::Int(flt_to_int(flt(0)?).ok()?)),
        // Folds over the *runtime* representation, not the literal's kind: `Nat` and `Int` ride i31, while an `Flt` is a boxed struct and a `Bin` a rope reference, so both answer 0.
        CpsIntrinsic::IsImmediate => Some(CpsLiteral::Nat(Natural::from(match literals[0] {
            CpsLiteral::Nat(_) | CpsLiteral::Int(_) => 1u32,
            CpsLiteral::Flt(_) | CpsLiteral::Bin(_, _) => 0,
        }))),
        // The identity on the value, so a literal operand is the answer. Reached only when the dispatch above already folded to the immediate side.
        CpsIntrinsic::ImmediateGet => Some(literals[0].clone()),
        _ => None,
    }
}
