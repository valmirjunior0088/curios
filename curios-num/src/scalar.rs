//! Exact scalar semantics of the erased numeric carriers — `Nat` as `u32`, `Int` as `i32`, `Flt` as binary32 — shared by every stage's constant folder so their arithmetic cannot drift. The runtime's i31 envelope appears nowhere in this module: a value the backend cannot box traps at the Wasm boundary instead of changing.
//!
//! Only operations with semantic freedom live here — saturating-versus-refusing choices, trap conditions, fold-decline conditions. Comparisons and plain bitwise operations have exactly one meaning and stay as native operators at their use sites.
//!
//! **What a signature here says.** `Result<_, ScalarTrap>` is an operation the program can trap on, and `Err` means it does at this argument: an answer the theory has that the carrier cannot hold, or an operand the operation's proof precondition excludes — which only an unsound proof delivers, and the runtime refuses. A folder must record that rather than decline. A bare return is total. Nothing here is undefined at a well-typed argument: a shift count is a `Nat` on both carriers, so the negative count the theory would have had to leave silent cannot be written. `curios-core` folds the same operations over unbounded `Natural`/`Integer` and over `Floating`'s binary32 model, and is the oracle for every one of them: what is stated here must be Core's answer or a refusal, never a third value. That is why the `Flt` narrowings below compute through the model and add only the carrier's own width — the semantics is not consulted about `u32`, and the carrier is not consulted about what a float means.

#[cfg(test)]
mod tests;

use crate::Floating;

/// Why an operation traps in its carrier: an answer the theory has that the carrier cannot hold, or an operand the operation's precondition excludes and the runtime refuses.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalarTrap {
    DivisionByZero,
    /// A value outside the carrier: a sum, product or shift past its width, or `i32::MIN / -1`.
    Overflow,
    /// A conversion with nothing to answer: a `Nat` or `Int` the other carrier does not hold, or a float outside the domain its precondition states — a NaN, an infinity, a negative where a natural is asked for — or whose integer part is past the carrier's width.
    ConversionRange,
}

/// `Nat` addition, refusing a sum past the carrier.
pub fn nat_add(left: u32, right: u32) -> Result<u32, ScalarTrap> {
    left.checked_add(right).ok_or(ScalarTrap::Overflow)
}

/// `Nat` subtraction is monus: truncated at zero, never negative.
pub fn nat_sub(left: u32, right: u32) -> u32 {
    left.saturating_sub(right)
}

/// `Nat` multiplication, refusing a product past the carrier.
pub fn nat_mul(left: u32, right: u32) -> Result<u32, ScalarTrap> {
    left.checked_mul(right).ok_or(ScalarTrap::Overflow)
}

/// `Nat` left shift — `value · 2^shift` — refusing a result past the carrier.
///
/// Widened rather than checked in place, because neither of the obvious tests is the condition. `u32::checked_shl` refuses only a *count* of 32 or more, so it answers `Some(0)` for `2^30 << 15`, where fifteen bits left the top; and testing the result's high bit afterwards cannot see bits that are already gone. The count is not reduced modulo anything: Wasm's mask is a property of `i32.shl`, not of `value · 2^shift`, and `curios-core` computes the latter over an unbounded `Natural`.
///
/// **The widening is not the condition on its own, which is the same defect one width up.** A large enough count pushes every bit past the intermediate's top as well — `2^30 << 40` is `2^70`, whose low sixty-four bits are zero — and the truncated `u64` reads back as a perfectly representable result. Clamping the count at the carrier's width closes it: a nonzero value shifted [`u32::BITS`] places has already left `u32`, so that count reaches the same verdict as every larger one, and below it the widened shift loses nothing. This is the argument `curios-cont`'s `emit_clamped_shift` makes at the i31 envelope, one layer down and at its own width.
///
/// Zero is answered before the count is looked at, since `0 · 2^k` is zero at every count and refusing it would refuse a value the carrier holds.
pub fn nat_shl(value: u32, shift: u32) -> Result<u32, ScalarTrap> {
    match value {
        0 => Ok(0),
        value => u64::from(value)
            .checked_shl(shift.min(u32::BITS))
            .and_then(|widened| u32::try_from(widened).ok())
            .ok_or(ScalarTrap::Overflow),
    }
}

/// `Nat` right shift — `⌊value / 2^shift⌋` — total, and never a trap: a quotient of a representable value is representable.
///
/// A count of 32 or more answers zero rather than reducing modulo 32. That is what `Natural`'s bignum shift answers in `curios-core`, and it is the arithmetic fact: shifting a 32-bit value right by 40 leaves nothing.
pub fn nat_shr(value: u32, shift: u32) -> u32 {
    value.checked_shr(shift).unwrap_or(0)
}

/// `Nat` division; traps on a zero divisor.
pub fn nat_div(left: u32, right: u32) -> Result<u32, ScalarTrap> {
    match right {
        0 => Err(ScalarTrap::DivisionByZero),
        right => Ok(left / right),
    }
}

/// `Nat` remainder; traps on a zero divisor.
pub fn nat_rem(left: u32, right: u32) -> Result<u32, ScalarTrap> {
    match right {
        0 => Err(ScalarTrap::DivisionByZero),
        right => Ok(left % right),
    }
}

/// `Int` addition, refusing a result past the carrier.
pub fn int_add(left: i32, right: i32) -> Result<i32, ScalarTrap> {
    left.checked_add(right).ok_or(ScalarTrap::Overflow)
}

/// `Int` subtraction, refusing a result past the carrier.
pub fn int_sub(left: i32, right: i32) -> Result<i32, ScalarTrap> {
    left.checked_sub(right).ok_or(ScalarTrap::Overflow)
}

/// `Int` multiplication, refusing a result past the carrier.
pub fn int_mul(left: i32, right: i32) -> Result<i32, ScalarTrap> {
    left.checked_mul(right).ok_or(ScalarTrap::Overflow)
}

/// `Int` left shift — `value · 2^shift` — refusing a result past the carrier. The count is a `Nat`, as on the unsigned twin.
///
/// Widened *and clamped* for the reasons [`nat_shl`] gives, and signed, so `-1 · 2^31` is `i32::MIN` and stays representable where the unsigned twin would refuse.
pub fn int_shl(value: i32, shift: u32) -> Result<i32, ScalarTrap> {
    match value {
        0 => Ok(0),
        value => i64::from(value)
            .checked_shl(shift.min(i32::BITS))
            .and_then(|widened| i32::try_from(widened).ok())
            .ok_or(ScalarTrap::Overflow),
    }
}

/// `Int` arithmetic right shift — `⌊value / 2^shift⌋` — total, and never a trap, for the reason [`nat_shr`] gives.
///
/// A count of 32 or more answers the sign rather than reducing modulo 32: shifting an `i32` right by 40 leaves `0` above zero and `-1` below it, which is what the bignum shift in `curios-core` answers.
pub fn int_shr(value: i32, shift: u32) -> i32 {
    value.checked_shr(shift).unwrap_or(value >> 31)
}

/// `Int` division; traps on a zero divisor and on `i32::MIN / -1`.
pub fn int_div(left: i32, right: i32) -> Result<i32, ScalarTrap> {
    if right == 0 {
        return Err(ScalarTrap::DivisionByZero);
    }
    // `checked_div` is `None` exactly on the `i32::MIN / -1` overflow here.
    left.checked_div(right).ok_or(ScalarTrap::Overflow)
}

/// `Int` remainder; traps on a zero divisor. `i32::MIN % -1` is `0` and does not trap.
pub fn int_rem(left: i32, right: i32) -> Result<i32, ScalarTrap> {
    match right {
        0 => Err(ScalarTrap::DivisionByZero),
        right => Ok(left.wrapping_rem(right)),
    }
}

/// `Nat` to `Int` preserving the number, refusing a value above `i32::MAX`, where no `i32` holds the same one. The conversions carry values, never bit views — reinterpretation belongs to explicit `Bin` casts.
pub fn nat_to_int(value: u32) -> Result<i32, ScalarTrap> {
    i32::try_from(value).map_err(|_| ScalarTrap::ConversionRange)
}

/// `Int` to `Nat` preserving the number, refusing a negative, which no natural equals.
pub fn int_to_nat(value: i32) -> Result<u32, ScalarTrap> {
    u32::try_from(value).map_err(|_| ScalarTrap::ConversionRange)
}

/// Truncate a binary32 to `u32`, refusing outside the domain [`Floating::to_natural`] states and past the carrier.
///
/// Two refusals, and only the second belongs here. The model decides what the truncation *is* — undefined on a NaN, an infinity or a negative — and this adds the erased carrier's own width on top of it. The semantics is not consulted about `u32`, and the carrier is not consulted about what a float means.
pub fn flt_to_nat(value: Floating) -> Result<u32, ScalarTrap> {
    value
        .to_natural()
        .and_then(|value| value.to_u32())
        .ok_or(ScalarTrap::ConversionRange)
}

/// Truncate a binary32 to `i32`, the twin of [`flt_to_nat`] over [`Floating::to_integer`]'s domain.
pub fn flt_to_int(value: Floating) -> Result<i32, ScalarTrap> {
    value
        .to_integer()
        .and_then(|value| value.to_i32())
        .ok_or(ScalarTrap::ConversionRange)
}
