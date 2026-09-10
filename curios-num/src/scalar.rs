//! Exact scalar semantics of the erased numeric carriers — `Nat` as [`Natural`], `Int` as [`Integer`], `Flt` as binary64 — shared by every stage's constant folder so their arithmetic cannot drift. The runtime's i31 envelope appears nowhere in this module: a value the backend cannot box traps at the Wasm boundary instead of changing.
//!
//! **The erased carriers are unbounded, and that is what makes a folder agree with Core by construction rather than by differential.** They were `u32` and `i32`, a width that named no target: the runtime's envelope is 31 bits, so a value in the `2³¹ .. 2³²−1` band folded to a number the same expression would have trapped on had an operand been live. Two widths remain — the unbounded one every layer above the emitter computes in, and the envelope `curios-cont` materializes into — and only the second refuses.
//!
//! Only operations with semantic freedom live here: the monus, the trap conditions, the shifts whose answer past a width is a fact rather than a wrap, and the conversions whose domain excludes an operand. Addition, subtraction on `Int` and the bitwise operations are total over an unbounded carrier and stay as ordinary arithmetic at their use sites, as the comparisons already did.
//!
//! **A growing operation takes its allowance from its caller.** Multiplication doubles a magnitude and a left shift grows it without bound, so a folder that ran them eagerly could be asked for a numeral no machine holds — `curios-core` is protected from that by charging every reduction step against a budget, and an erased-stage folder is not. The allowance is a parameter rather than a constant here because it is a fact about the caller's resources, not about what the operation means: `curios-ersd` bounds by the growth pool its evaluator already keeps, and `curios-cont` by the envelope it can materialize into. Past it the fold *declines*, which is invisible — a program means the same thing whether or not a fold fires — where a refusal would be observable.
//!
//! **What a signature here says.** `Result<_, ScalarTrap>` is an operation the program can trap on, and `Err` means it does at this argument: an operand the operation's proof precondition excludes, which only an unsound proof delivers. `Option` is an operation that may decline to fold, and `None` is that decline. A bare return is total. Nothing here is undefined at a well-typed argument: a shift count is a `Nat` on both carriers, so the negative count the theory would have had to leave silent cannot be written. `curios-core` folds the same operations over the same [`Natural`] and [`Integer`], and over `Floating`'s binary64 model, and is the oracle for every one of them: what is stated here must be Core's answer or a refusal, never a third value.

#[cfg(test)]
mod tests;

use crate::{Floating, Integer, Natural};

/// Why an operation traps in its carrier: an operand the operation's precondition excludes and the runtime refuses.
///
/// There is no overflow variant. An erased carrier holds whatever the theory computes, and the one width that refuses a magnitude is the emitter's envelope, which `curios-cont` raises as its own panic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalarTrap {
    DivisionByZero,
    /// A conversion with nothing to answer: an `Int` no natural equals, or a float outside the domain its precondition states — a NaN, an infinity, or a negative where a natural is asked for.
    ConversionRange,
}

/// Whether a result of `bits` fits the caller's `allowance`, in bits.
fn within(bits: Option<u64>, allowance: u64) -> bool {
    bits.is_some_and(|bits| bits <= allowance)
}

/// `Nat` multiplication, declining past `allowance` bits.
///
/// Growing, and the reason is not the obvious one: a single product is at most the sum of its operands' widths, but a *chain* of them squares, so thirty nested multiplications take a machine word past any memory there is while costing thirty steps. A step budget cannot see that; a width can.
pub fn nat_mul(left: &Natural, right: &Natural, allowance: u64) -> Option<Natural> {
    match within(left.bits().checked_add(right.bits()), allowance) {
        true => Some(left * right),
        false => None,
    }
}

/// `Int` multiplication, the signed twin of [`nat_mul`] under the same allowance.
pub fn int_mul(left: &Integer, right: &Integer, allowance: u64) -> Option<Integer> {
    match within(left.bits().checked_add(right.bits()), allowance) {
        true => Some(left.clone() * right.clone()),
        false => None,
    }
}

/// `Nat` left shift — `value · 2^shift` — declining past `allowance` bits.
///
/// Zero is answered before the count is looked at, since `0 · 2^k` is zero at every count and a decline there would leave a fold undone for nothing.
pub fn nat_shl(value: &Natural, shift: &Natural, allowance: u64) -> Option<Natural> {
    if value.is_zero() {
        return Some(Natural::zero());
    }

    let amount = shift.to_u64()?;

    match within(value.bits().checked_add(amount), allowance) {
        true => value.clone().checked_shl(shift.clone()),
        false => None,
    }
}

/// `Int` left shift, the signed twin of [`nat_shl`] under the same allowance.
pub fn int_shl(value: &Integer, shift: &Natural, allowance: u64) -> Option<Integer> {
    if value.is_zero() {
        return Some(Integer::from(0u32));
    }

    let amount = shift.to_u64()?;

    match within(value.bits().checked_add(amount), allowance) {
        true => value.clone().checked_shl(shift.clone()),
        false => None,
    }
}

/// `Nat` right shift — `⌊value / 2^shift⌋` — total, and never a trap: a quotient of a representable value is representable.
///
/// A count past the value's own width answers zero rather than reducing modulo anything. That is what the bignum shift in `curios-core` answers, and it is the arithmetic fact: shifting a value right past its top leaves nothing.
pub fn nat_shr(value: &Natural, shift: &Natural) -> Natural {
    value
        .clone()
        .checked_shr(shift.clone())
        .unwrap_or_else(Natural::zero)
}

/// `Int` arithmetic right shift — `⌊value / 2^shift⌋` — total, for the reason [`nat_shr`] gives.
///
/// A count past the value's width answers the sign: zero above it and `-1` below, which is what the bignum shift answers.
pub fn int_shr(value: &Integer, shift: &Natural) -> Integer {
    value.clone().checked_shr(shift.clone()).unwrap_or_else(|| {
        Integer::from(match value.to_natural().is_none() {
            true => -1,
            false => 0,
        })
    })
}

/// `Nat` subtraction is monus: truncated at zero, never negative.
///
/// Spelled here rather than as `-`, whose `num-bigint` meaning panics on underflow.
pub fn nat_sub(left: &Natural, right: &Natural) -> Natural {
    match left >= right {
        true => left - right,
        false => Natural::zero(),
    }
}

/// `Nat` division; traps on a zero divisor.
pub fn nat_div(left: &Natural, right: &Natural) -> Result<Natural, ScalarTrap> {
    left.clone()
        .checked_div(right.clone())
        .ok_or(ScalarTrap::DivisionByZero)
}

/// `Nat` remainder; traps on a zero divisor.
pub fn nat_rem(left: &Natural, right: &Natural) -> Result<Natural, ScalarTrap> {
    left.clone()
        .checked_rem(right.clone())
        .ok_or(ScalarTrap::DivisionByZero)
}

/// `Int` division; traps on a zero divisor.
///
/// The `i32::MIN / -1` overflow the bounded carrier had went with the bound: the quotient of two integers is an integer.
pub fn int_div(left: &Integer, right: &Integer) -> Result<Integer, ScalarTrap> {
    left.clone()
        .checked_div(right.clone())
        .ok_or(ScalarTrap::DivisionByZero)
}

/// `Int` remainder; traps on a zero divisor.
pub fn int_rem(left: &Integer, right: &Integer) -> Result<Integer, ScalarTrap> {
    left.clone()
        .checked_rem(right.clone())
        .ok_or(ScalarTrap::DivisionByZero)
}

/// `Int` to `Nat` preserving the number, refusing a negative, which no natural equals. The conversions carry values, never bit views — reinterpretation belongs to explicit `Bin` casts.
pub fn int_to_nat(value: &Integer) -> Result<Natural, ScalarTrap> {
    value.to_natural().ok_or(ScalarTrap::ConversionRange)
}

/// Truncate a binary64 to `Nat`, refusing outside the domain [`Floating::to_natural`] states.
///
/// One refusal where there were two: the model decides what the truncation *is* — undefined on a NaN, an infinity or a negative — and the erased carrier no longer adds a width on top of it.
pub fn flt_to_nat(value: Floating) -> Result<Natural, ScalarTrap> {
    value.to_natural().ok_or(ScalarTrap::ConversionRange)
}

/// Truncate a binary64 to `Int`, the twin of [`flt_to_nat`] over [`Floating::to_integer`]'s domain.
pub fn flt_to_int(value: Floating) -> Result<Integer, ScalarTrap> {
    value.to_integer().ok_or(ScalarTrap::ConversionRange)
}
