//! The contract the carriers' arithmetic keeps — `Nat` as [`Natural`], `Int` as [`Integer`], `Flt` as binary64 [`Floating`] — so that every stage's constant folder, calling the same methods, computes the same thing and their arithmetic cannot drift. The runtime's representation appears nowhere in this module: the running program computes the same unbounded values, an i31 while small and a boxed magnitude past it, and `curios`'s numeric tests hold it to this module by a differential grid.
//!
//! **The erased carriers are unbounded, and that is what makes a folder agree with Core by construction rather than by differential.** They were `u32` and `i32`, a width that named no target, so a value in a band past the runtime's width folded to a number the same expression would have trapped on had an operand been live. No width is left anywhere a value is computed: the only narrowing is the host wire's, which is not a fold.
//!
//! The operations with semantic freedom are methods of their carriers: the monus ([`Natural::monus`]), the trap conditions ([`Natural::div`], [`Integer::rem`] and their siblings), the left shifts and products whose growth needs an allowance ([`Natural::shl_within`], [`Integer::mul_within`]), and the conversions whose domain excludes an operand ([`Integer::to_natural`], [`Floating::to_natural`]). Addition, subtraction on `Int`, the right shifts and the bitwise operations are total over an unbounded carrier and stay as ordinary arithmetic at their use sites, as the comparisons already did.
//!
//! **A growing operation takes its allowance from its caller.** Multiplication doubles a magnitude and a left shift grows it without bound, so a folder that ran them eagerly could be asked for a numeral no machine holds — `curios-core` is protected from that by charging every reduction step against a budget, and an erased-stage folder is not. The allowance is a parameter rather than a constant here because it is a fact about the caller's resources, not about what the operation means: `curios-ersd` bounds by the growth pool its evaluator already keeps, and `curios-cont` by the same allowance, since a wider constant could not have come down from the stage above. Past it the fold *declines*, which is invisible — a program means the same thing whether or not a fold fires — where a refusal would be observable.
//!
//! **What a signature says.** `Result<_, ScalarTrap>` is an operation the program can trap on, and `Err` means it does at this argument: an operand the operation's proof precondition excludes, which only an unsound proof delivers. `Option` is an operation that may decline to fold, and `None` is that decline. A bare return is total. Nothing here is undefined at a well-typed argument: a shift count is a `Nat` on both carriers, so the negative count the theory would have had to leave silent cannot be written. `curios-core` folds the same operations over the same [`Natural`] and [`Integer`], and over `Floating`'s binary64 model, and is the oracle for every one of them: what is stated here must be Core's answer or a refusal, never a third value.

#[cfg(test)]
mod tests;

#[cfg(doc)]
use crate::{Floating, Integer, Natural};

/// Why an operation traps in its carrier: an operand the operation's precondition excludes and the runtime refuses.
///
/// There is no overflow variant. An erased carrier holds whatever the theory computes, and so does the running program.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalarTrap {
    DivisionByZero,
    /// A conversion with nothing to answer: an `Int` no natural equals, or a float outside the domain its precondition states — a NaN, an infinity, or a negative where a natural is asked for.
    ConversionRange,
    /// A decode of a binary that is not the encoding's width: [`Floating::of_le_bytes`] of anything but eight whole bytes.
    Malformed,
}

/// Whether a result of `bits` fits the caller's `allowance`, in bits.
pub(crate) fn within(bits: Option<u64>, allowance: u64) -> bool {
    bits.is_some_and(|bits| bits <= allowance)
}
