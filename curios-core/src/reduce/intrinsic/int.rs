//! Deciding an `Int` comparison, including under symbols — [`compare_nat`]'s signed twin over the group normal form.
//!
//! ℤ under `+` is a group, so every relation reads through the difference: `a ⋈ b` iff `a - b ⋈ 0`. [`int_cancel_common`] moves that difference to the two sides by sign, and a pair whose residuals are both constants is decided by comparing them; anything else is undecided and rebuilt from the residuals, so `i + a < i + b` and `a < b` reduce to one term, which conversion needs.

use {
    super::*,
    crate::{
        ReduceError, Reducer, Term, int_cancel_common, int_has_stuck_product, int_monomial,
        int_normalize, int_preimage, int_split_by_sign, int_terms,
    },
    curios_num::Natural,
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
        // Two sound verdicts, intersected: the preimages' order when both sides are widened naturals, and divisibility as `compare_nat` reads it — the argument needs every monomial to be an integer and nothing more, so it holds below zero, and the two constants are apart modulo the gcd exactly when their difference is.
        _ => {
            let (constant_left, summands_left) = int_terms(&left);
            let (constant_right, summands_right) = int_terms(&right);
            let coefficients = summands_left
                .iter()
                .chain(&summands_right)
                .map(|summand| int_monomial(summand).0.magnitude());
            let difference = (constant_left - constant_right).magnitude();
            let pulled_back = compare_preimages(reducer, &left, &right)?;
            match apart_modulo((&difference, &Natural::zero()), coefficients) {
                true => pulled_back.unequal(),
                false => pulled_back,
            }
        }
    };

    Ok((outcome, left, right))
}

/// The verdict on two `Int`s that are, once their difference is split by sign, both non-negative combinations of widened naturals: the `Nat` comparison of their preimages, which is sound because ℕ → ℤ preserves and reflects order. `Stuck` for any other pair. What makes `0 <= Nat/to_int(n)` true — the floor a widened natural carries is `Nat`'s own, read through the embedding rather than restated.
///
/// It only ever decides: the neutral term the caller rebuilds on `Stuck` is its own residual pair, so a stuck comparison keeps the spelling its guard recorded.
fn compare_preimages(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
) -> Result<Comparison, ReduceError> {
    let (left, right) = int_split_by_sign(left, right);
    match (int_preimage(&left), int_preimage(&right)) {
        (Some(left), Some(right)) => Ok(compare_nat(reducer, left, right)?.0),
        _ => Ok(Comparison::Stuck),
    }
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
