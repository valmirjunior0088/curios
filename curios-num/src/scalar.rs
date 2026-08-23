//! Exact scalar semantics of the erased numeric carriers — `Nat` as `u32`, `Int` as `i32`, `Flt` as binary32 — shared by every stage's constant folder so their arithmetic cannot drift. The runtime's i31 envelope appears nowhere in this module: a value the backend cannot box traps at the Wasm boundary instead of changing.
//!
//! Only operations with semantic freedom live here — saturating-versus-refusing choices, trap conditions, fold-decline conditions. Comparisons and plain bitwise operations have exactly one meaning and stay as native operators at their use sites.
//!
//! **What a signature here says.** `Result<_, ScalarTrap>` is an operation that always has an answer in the theory and sometimes cannot represent it: `Err` means the program traps, and a folder must record that rather than decline. `Option` is an operation the theory itself leaves undefined at that argument — a negative shift count, which `Integer::checked_shl` also declines — so a folder must stay silent and leave the term alone. A bare return is total. `curios-core` folds the same operations over unbounded `Natural`/`Integer` and is the oracle for every one of them: what is stated here must be Core's answer or a refusal, never a third value.

#[cfg(test)]
mod tests;

/// Why an operation with an answer in the theory cannot produce one in its carrier.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalarTrap {
    DivisionByZero,
    /// A value outside the carrier: a sum, product or shift past its width, or `i32::MIN / -1`.
    Overflow,
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

/// `Nat` 32-bit left rotation; the count is taken modulo 32.
pub fn nat_rotl(value: u32, shift: u32) -> u32 {
    value.rotate_left(shift)
}

/// `Nat` 32-bit right rotation; the count is taken modulo 32.
pub fn nat_rotr(value: u32, shift: u32) -> u32 {
    value.rotate_right(shift)
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

/// `Int` left shift — `value · 2^shift` — refusing a result past the carrier, and declining a negative count.
///
/// The outer `None` is the theory's silence rather than a trap: `Integer::checked_shl` converts its amount through `to_usize`, which fails on a negative, so `curios-core` leaves such a term unfolded and this must too. Widened *and clamped* for the reasons [`nat_shl`] gives, and signed, so `-1 · 2^31` is `i32::MIN` and stays representable where the unsigned twin would refuse.
pub fn int_shl(value: i32, shift: i32) -> Option<Result<i32, ScalarTrap>> {
    let shift = u32::try_from(shift).ok()?;

    Some(match value {
        0 => Ok(0),
        value => i64::from(value)
            .checked_shl(shift.min(i32::BITS))
            .and_then(|widened| i32::try_from(widened).ok())
            .ok_or(ScalarTrap::Overflow),
    })
}

/// `Int` arithmetic right shift — `⌊value / 2^shift⌋` — never a trap, and declining a negative count for the reason [`int_shl`] gives.
///
/// A count of 32 or more answers the sign rather than reducing modulo 32: shifting an `i32` right by 40 leaves `0` above zero and `-1` below it, which is what the bignum shift in `curios-core` answers.
pub fn int_shr(value: i32, shift: i32) -> Option<i32> {
    let shift = u32::try_from(shift).ok()?;

    Some(value.checked_shr(shift).unwrap_or(value >> 31))
}

/// `Int` 32-bit left rotation of the carrier bits; the count is taken modulo 32.
pub fn int_rotl(value: i32, shift: i32) -> i32 {
    (value as u32).rotate_left(shift as u32) as i32
}

/// `Int` 32-bit right rotation of the carrier bits; the count is taken modulo 32.
pub fn int_rotr(value: i32, shift: i32) -> i32 {
    (value as u32).rotate_right(shift as u32) as i32
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

/// `Nat` to `Int` preserving the number; `None` traps above `i32::MAX`, where no `i32` holds the same value. The conversions carry values, never bit views — reinterpretation belongs to explicit `Bin` casts.
pub fn nat_to_int(value: u32) -> Option<i32> {
    i32::try_from(value).ok()
}

/// `Int` to `Nat` preserving the number; `None` traps on a negative, which no natural equals.
pub fn int_to_nat(value: i32) -> Option<u32> {
    u32::try_from(value).ok()
}

/// Truncate to `u32`; `None` traps outside `(-1, 2^32)` or on a non-finite input.
pub fn flt_to_nat(value: f32) -> Option<u32> {
    let truncated = value.trunc();
    (value.is_finite() && truncated > -1.0 && truncated < 4_294_967_296.0)
        .then_some(truncated as u32)
}

/// Truncate to `i32`; `None` traps outside `[-2^31, 2^31)` or on a non-finite input.
pub fn flt_to_int(value: f32) -> Option<i32> {
    let truncated = value.trunc();
    (value.is_finite() && (-2_147_483_648.0..2_147_483_648.0).contains(&truncated))
        .then_some(truncated as i32)
}

/// Binary32 minimum as `f32.min` computes it. `None` declines the fold on a NaN operand (Rust and Wasm disagree on NaN propagation), which is not a trap — the runtime operation proceeds. Operands that compare equal are the two zeros, or one value twice, and Wasm names the answer: the negative-signed one. It is spelled here rather than left to `f32::min`, whose LLVM lowering leaves the sign of an equal pair open.
pub fn flt_min(left: f32, right: f32) -> Option<f32> {
    if left.is_nan() || right.is_nan() {
        return None;
    }
    Some(match left == right {
        true if left.is_sign_negative() => left,
        true => right,
        false => left.min(right),
    })
}

/// Binary32 maximum as `f32.max` computes it; the twin of [`flt_min`], with an equal pair answering the positive-signed one.
pub fn flt_max(left: f32, right: f32) -> Option<f32> {
    if left.is_nan() || right.is_nan() {
        return None;
    }
    Some(match left == right {
        true if left.is_sign_negative() => right,
        true => left,
        false => left.max(right),
    })
}
