//! Deciding an `Int` comparison, including under symbols — [`compare_nat`]'s signed twin over the group normal form.
//!
//! ℤ under `+` is a group, so every relation reads through the difference: `a ⋈ b` iff `a - b ⋈ 0`. [`int_cancel_common`] moves that difference to the two sides by sign, and a pair whose residuals are both constants is decided by comparing them; anything else is undecided and rebuilt from the residuals, so `i + a < i + b` and `a < b` reduce to one term, which conversion needs.

use {
    super::*,
    crate::{ReduceError, Reducer, Term, int_cancel_common, int_has_stuck_product, int_normalize},
};

/// The structural outcome of comparing two `Int`s, with the operands their shared part cancelled off. A stuck product on either side is distributed first, by name, as `compare_nat` distributes.
pub(super) fn compare_int(
    reducer: &mut impl Reducer,
    left: Term,
    right: Term,
) -> Result<(Comparison, Term, Term), ReduceError> {
    let left = match int_has_stuck_product(&left) {
        true => int_normalize(reducer, left)?,
        false => left,
    };
    let right = match int_has_stuck_product(&right) {
        true => int_normalize(reducer, right)?,
        false => right,
    };
    let (left, right) = int_cancel_common(&left, &right);

    let outcome = match (left.as_int(), right.as_int()) {
        (Some(l), Some(r)) => from_ordering(l.cmp(&r)),
        _ => Comparison::Stuck,
    };

    Ok((outcome, left, right))
}

/// Reduce an `Int` comparison through [`compare_int`]: `read` projects the outcome to this op's boolean, or `None` when the operands do not decide it, in which case the neutral term is rebuilt from the cancelled operands.
pub(super) fn reduce_int_compare(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    read: impl FnOnce(Comparison) -> Option<bool>,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;
    let (outcome, left, right) = compare_int(reducer, left, right)?;

    Ok(match read(outcome) {
        Some(value) => Subterm::Intrinsic(Intrinsic::Bool(value)),
        None => Subterm::Intrinsic(rebuild(left, right)),
    })
}
