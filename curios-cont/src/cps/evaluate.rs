use {
    super::*,
    curios_num::{Floating, Integer, Natural},
};

/// The allowance the growing folds take here: `curios-ersd`'s own bound on a folded scalar, since a folded `Nat` or `Int` past the i31 materializes as a boxed constant and one wider than that could not have come down from the stage above. A literal count may still ask for a numeral no machine holds, and past this the fold declines.
const FOLD_ALLOWANCE_BITS: u64 = 65_536 * 8;

/// Fold one intrinsic over literal operands, or decline.
///
/// **A proven trap declines here, where `curios-ersd`'s fold records it.** This answer is two-way — a literal or nothing — so an operation whose value leaves its carrier is left standing, and the guard `into_wasm` emits for it traps at the execution point instead. Recording the trap would be more precise and is what the erased fold does with its three-way outcome; declining is correct either way, and it is the convention `NatDiv` already established for a zero divisor.
///
/// A growth decline uses that same answer. The carriers are unbounded from `curios-ersd` down, so multiplication and the left shifts are the two operations that can be asked to build without bound; past [`FOLD_ALLOWANCE_BITS`] they leave the operation standing exactly as a trap does.
pub(super) fn evaluate(op: Intrinsic, args: &[Atom]) -> Option<Literal> {
    let literals = args
        .iter()
        .map(|atom| match atom {
            Atom::Literal(literal) => Some(literal),
            _ => None,
        })
        .collect::<Option<Vec<_>>>()?;
    let nat = |index: usize| match literals[index] {
        Literal::Nat(value) => Some(value),
        _ => None,
    };
    let int = |index: usize| match literals[index] {
        Literal::Int(value) => Some(value),
        _ => None,
    };
    // The model, never the host: every arm below computes what `curios-core` folded and what the emitted Wasm executes, which is one function of the operands rather than three.
    let flt = |index: usize| match literals[index] {
        Literal::Flt(value) => Some(*value),
        _ => None,
    };
    let bool_ = |value: bool| Some(Literal::Nat(Natural::from(value as u32)));
    let flt_ = |value: Floating| Some(Literal::Flt(value));

    match op {
        Intrinsic::NatEql => bool_(nat(0)? == nat(1)?),
        // Fold only when the bounds hold; a constant that would trap stays an instruction so the trap keeps its place.
        Intrinsic::WindowExtent => {
            let (start, count, len) = (nat(0)?, nat(1)?, nat(2)?);

            (&(start + count) <= len).then(|| Literal::Nat(count.clone()))
        }
        Intrinsic::NatNeq => bool_(nat(0)? != nat(1)?),
        Intrinsic::NatAdd => Some(Literal::Nat(nat(0)? + nat(1)?)),
        Intrinsic::NatSub => Some(Literal::Nat(nat(0)?.monus(nat(1)?))),
        Intrinsic::NatMul => Some(Literal::Nat(
            nat(0)?.mul_within(nat(1)?, FOLD_ALLOWANCE_BITS)?,
        )),
        Intrinsic::NatLt => bool_(nat(0)? < nat(1)?),
        Intrinsic::NatDiv => Some(Literal::Nat(nat(0)?.div(nat(1)?).ok()?)),
        Intrinsic::NatRem => Some(Literal::Nat(nat(0)?.rem(nat(1)?).ok()?)),
        Intrinsic::NatLe => bool_(nat(0)? <= nat(1)?),
        Intrinsic::NatAnd => Some(Literal::Nat(nat(0)? & nat(1)?)),
        Intrinsic::NatOr => Some(Literal::Nat(nat(0)? | nat(1)?)),
        Intrinsic::NatXor => Some(Literal::Nat(nat(0)? ^ nat(1)?)),
        Intrinsic::NatShl => Some(Literal::Nat(
            nat(0)?.shl_within(nat(1)?, FOLD_ALLOWANCE_BITS)?,
        )),
        Intrinsic::NatShr => Some(Literal::Nat(nat(0)? >> nat(1)?)),
        Intrinsic::NatEqz => bool_(nat(0)?.is_zero()),
        Intrinsic::NatToInt => Some(Literal::Int(Integer::from(nat(0)?.clone()))),
        Intrinsic::NatToFlt(rounding) => flt_(Floating::of_natural(nat(0)?, rounding)),
        Intrinsic::IntEql => bool_(int(0)? == int(1)?),
        Intrinsic::IntNeq => bool_(int(0)? != int(1)?),
        Intrinsic::IntAdd => Some(Literal::Int(int(0)?.clone() + int(1)?.clone())),
        Intrinsic::IntSub => Some(Literal::Int(int(0)?.clone() - int(1)?.clone())),
        Intrinsic::IntMul => Some(Literal::Int(
            int(0)?.mul_within(int(1)?, FOLD_ALLOWANCE_BITS)?,
        )),
        Intrinsic::IntDiv => Some(Literal::Int(int(0)?.div(int(1)?).ok()?)),
        Intrinsic::IntRem => Some(Literal::Int(int(0)?.rem(int(1)?).ok()?)),
        Intrinsic::IntLt => bool_(int(0)? < int(1)?),
        Intrinsic::IntLe => bool_(int(0)? <= int(1)?),
        Intrinsic::IntAnd => Some(Literal::Int(int(0)?.clone() & int(1)?.clone())),
        Intrinsic::IntOr => Some(Literal::Int(int(0)?.clone() | int(1)?.clone())),
        Intrinsic::IntXor => Some(Literal::Int(int(0)?.clone() ^ int(1)?.clone())),
        Intrinsic::IntShl => Some(Literal::Int(
            int(0)?.shl_within(nat(1)?, FOLD_ALLOWANCE_BITS)?,
        )),
        Intrinsic::IntShr => Some(Literal::Int(int(0)? >> nat(1)?)),
        Intrinsic::IntEqz => bool_(int(0)?.is_zero()),
        Intrinsic::IntToNat => Some(Literal::Nat(Natural::try_from(int(0)?).ok()?)),
        Intrinsic::IntToFlt(rounding) => flt_(Floating::of_integer(int(0)?, rounding)),
        Intrinsic::FltAdd(rounding) => flt_(flt(0)?.sum(flt(1)?, rounding)),
        Intrinsic::FltSub(rounding) => flt_(flt(0)?.difference(flt(1)?, rounding)),
        Intrinsic::FltMul(rounding) => flt_(flt(0)?.product(flt(1)?, rounding)),
        Intrinsic::FltDiv(rounding) => flt_(flt(0)?.quotient(flt(1)?, rounding)),
        Intrinsic::FltFma(rounding) => flt_(flt(0)?.fma(flt(1)?, flt(2)?, rounding)),
        Intrinsic::FltRem => flt_(flt(0)? % flt(1)?),
        Intrinsic::FltEql => bool_(flt(0)?.eql(flt(1)?)),
        Intrinsic::FltNeq => bool_(flt(0)?.neq(flt(1)?)),
        Intrinsic::FltLt => bool_(flt(0)?.lt(flt(1)?)),
        Intrinsic::FltLe => bool_(flt(0)?.le(flt(1)?)),
        Intrinsic::FltMin => flt_(flt(0)?.min(flt(1)?)),
        Intrinsic::FltMax => flt_(flt(0)?.max(flt(1)?)),
        Intrinsic::FltNeg => flt_(-flt(0)?),
        Intrinsic::FltAbs => flt_(flt(0)?.abs()),
        Intrinsic::FltSqrt(rounding) => flt_(flt(0)?.sqrt(rounding)),
        Intrinsic::FltRoundIntegral(rounding) => flt_(flt(0)?.round_integral(rounding)),
        Intrinsic::FltCopysign => flt_(flt(0)?.copysign(flt(1)?)),
        Intrinsic::FltToNat => Some(Literal::Nat(flt(0)?.to_natural().ok()?)),
        Intrinsic::FltToInt => Some(Literal::Int(flt(0)?.to_integer().ok()?)),
        // Folds over the *runtime* representation, not the literal's kind: a `Nat` or `Int` is an i31 or a boxed magnitude, both of which the test admits, while an `Flt` is a boxed struct and a `Bin` a rope reference, so those answer 0.
        Intrinsic::IsImmediate => Some(Literal::Nat(Natural::from(match literals[0] {
            Literal::Nat(_) | Literal::Int(_) => 1u32,
            Literal::Flt(_) | Literal::Bin(_, _) => 0,
        }))),
        // The identity on the value, so a literal operand is the answer. Reached only when the dispatch above already folded to the immediate side.
        Intrinsic::ImmediateGet => Some(literals[0].clone()),
        _ => None,
    }
}
