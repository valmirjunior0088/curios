//! Exact scalar semantics of the erased numeric carriers — `Nat` as `u32`, `Int` as `i32`, `Flt` as binary32 — shared by every stage's constant folder so their arithmetic cannot drift. The runtime's i31 envelope appears nowhere in this module: a value the backend cannot box traps at the Wasm boundary instead of changing.
//!
//! Only operations with semantic freedom live here — wrapping-versus-saturating choices, trap conditions, fold-decline conditions. Comparisons and plain bitwise operations have exactly one meaning and stay as native operators at their use sites.

/// Why an integer division cannot produce a value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DivTrap {
    DivisionByZero,
    /// `i32::MIN / -1`, the one signed quotient outside the carrier.
    Overflow,
}

/// `Nat` addition wraps the full 32-bit carrier.
pub fn nat_add(left: u32, right: u32) -> u32 {
    left.wrapping_add(right)
}

/// `Nat` subtraction is monus: truncated at zero, never negative.
pub fn nat_sub(left: u32, right: u32) -> u32 {
    left.saturating_sub(right)
}

/// `Nat` multiplication wraps the full 32-bit carrier.
pub fn nat_mul(left: u32, right: u32) -> u32 {
    left.wrapping_mul(right)
}

/// `Nat` left shift; the shift count is taken modulo 32, as in Wasm.
pub fn nat_shl(value: u32, shift: u32) -> u32 {
    value.wrapping_shl(shift)
}

/// `Nat` logical right shift; the shift count is taken modulo 32.
pub fn nat_shr(value: u32, shift: u32) -> u32 {
    value.wrapping_shr(shift)
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
pub fn nat_div(left: u32, right: u32) -> Result<u32, DivTrap> {
    match right {
        0 => Err(DivTrap::DivisionByZero),
        right => Ok(left / right),
    }
}

/// `Nat` remainder; traps on a zero divisor.
pub fn nat_rem(left: u32, right: u32) -> Result<u32, DivTrap> {
    match right {
        0 => Err(DivTrap::DivisionByZero),
        right => Ok(left % right),
    }
}

/// `Int` addition wraps the full 32-bit carrier.
pub fn int_add(left: i32, right: i32) -> i32 {
    left.wrapping_add(right)
}

/// `Int` subtraction wraps the full 32-bit carrier.
pub fn int_sub(left: i32, right: i32) -> i32 {
    left.wrapping_sub(right)
}

/// `Int` multiplication wraps the full 32-bit carrier.
pub fn int_mul(left: i32, right: i32) -> i32 {
    left.wrapping_mul(right)
}

/// `Int` left shift; the shift count is taken modulo 32.
pub fn int_shl(value: i32, shift: i32) -> i32 {
    value.wrapping_shl(shift as u32)
}

/// `Int` arithmetic right shift; the shift count is taken modulo 32.
pub fn int_shr(value: i32, shift: i32) -> i32 {
    value.wrapping_shr(shift as u32)
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
pub fn int_div(left: i32, right: i32) -> Result<i32, DivTrap> {
    if right == 0 {
        return Err(DivTrap::DivisionByZero);
    }
    // `checked_div` is `None` exactly on the `i32::MIN / -1` overflow here.
    left.checked_div(right).ok_or(DivTrap::Overflow)
}

/// `Int` remainder; traps on a zero divisor. `i32::MIN % -1` is `0` and does not trap.
pub fn int_rem(left: i32, right: i32) -> Result<i32, DivTrap> {
    match right {
        0 => Err(DivTrap::DivisionByZero),
        right => Ok(left.wrapping_rem(right)),
    }
}

/// `Nat` to `Int` is a carrier-bit reinterpretation.
pub fn nat_to_int(value: u32) -> i32 {
    value as i32
}

/// `Int` to `Nat` is a carrier-bit reinterpretation.
pub fn int_to_nat(value: i32) -> u32 {
    value as u32
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

/// Binary32 minimum; `None` declines the fold on a NaN operand (Rust and Wasm disagree on NaN propagation), which is not a trap — the runtime operation proceeds.
pub fn flt_min(left: f32, right: f32) -> Option<f32> {
    (!left.is_nan() && !right.is_nan()).then(|| left.min(right))
}

/// Binary32 maximum; `None` declines the fold on a NaN operand, which is not a trap — the runtime operation proceeds.
pub fn flt_max(left: f32, right: f32) -> Option<f32> {
    (!left.is_nan() && !right.is_nan()).then(|| left.max(right))
}
