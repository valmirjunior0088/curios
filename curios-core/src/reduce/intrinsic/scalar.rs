//! The per-carrier operand folds: reduce both sides, and answer from the values when both are literals.
//!
//! One function per carrier and arity, each doing the same three things — reduce, match a literal pair, rebuild the redex when the match fails. What a failed match falls through to is the caller's business: the laws in [`laws`], or nothing.

use {
    super::*,
    crate::{Intrinsic, ReduceError, Reducer, Subterm, Term},
    curios_num::{Floating, Integer},
};

/// Read an already-reduced `Nat` term as a concrete `usize` index — `None` when it is still symbolic or too large to fit. The shared decode behind the `Bin`/`List` `get`/`slice` bounds.
pub(super) fn as_index(term: &Term) -> Option<usize> {
    term.as_nat().and_then(|n| n.to_natural()?.to_usize())
}

/// Reduce the operands of a `Bool` binary intrinsic as far as a fold could use them, then either `fold` the two literals or `rebuild` the neutral term. `Bool` has no numeric carrier at the type level, so the fold reads the `true`/`false` constructors directly.
///
/// **The right operand is reduced only once the left is a literal.** A fold needs both, so a stuck left settles the verdict whatever the right holds, and reducing the right then is work the answer cannot use. It was reduced regardless, and that made weak-head reduction of a `&&`/`||` tree its *full* normalization: a web of predicate definitions each naming the one before it twice unfolded `2^n` times under any demand on its top, since a local-bearing term is remembered by nothing — the cliff `curios`' `scrutinee_refinement_measurements` records under `proved`. Stopping at the left leaves the right as written, which conversion compares lazily through its own reduction, so no equality decision moves. The `Nat` folds below keep both operands eager because their identity laws (`x + 0`) read the right.
pub(super) fn reduce_bool_binary(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(bool, bool) -> bool,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let Some(l) = left.as_bool() else {
        return Ok(Subterm::Intrinsic(rebuild(left, right.clone())));
    };

    let right = reducer.reduce_forced(right.clone())?;
    Ok(Subterm::Intrinsic(match right.as_bool() {
        Some(r) => Intrinsic::Bool(fold(l, r)),
        None => rebuild(left, right),
    }))
}

pub(super) fn reduce_byte_binary(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(u8, u8) -> bool,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    Ok(Subterm::Intrinsic(match (&*left, &*right) {
        (Subterm::Intrinsic(Intrinsic::Byte(left)), Subterm::Intrinsic(Intrinsic::Byte(right))) => {
            Intrinsic::Bool(fold(*left, *right))
        }
        _ => rebuild(left, right),
    }))
}

/// `Int/shl`, the signed twin of [`reduce_nat_shl`].
pub(super) fn reduce_int_shl(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    let folded = match (left.as_int(), right.as_int()) {
        (Some(value), Some(amount)) => {
            reducer.spend(shift_bound(
                value.bits(),
                amount.to_natural().and_then(|amount| amount.to_u64()),
            ))?;

            value.checked_shl(amount).map(Intrinsic::Int)
        }
        _ => None,
    };

    Ok(Subterm::Intrinsic(match folded {
        Some(intrinsic) => intrinsic,
        None => Intrinsic::IntShl(left, right),
    }))
}

/// `Int` counterpart of [`reduce_nat_binary`]: fold both literal operands or rebuild the neutral term. The fold is partial for the same reason — the shifts decline a negative or oversized literal shift count (`None`); the total ops just wrap their result in `Some`.
pub(super) fn reduce_int_binary(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(Integer, Integer) -> Option<Intrinsic>,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    let folded = match (left.as_int(), right.as_int()) {
        (Some(l), Some(r)) => {
            reducer.spend(operand_bound(l.bits(), r.bits()))?;

            fold(l, r)
        }
        _ => None,
    };

    Ok(Subterm::Intrinsic(match folded {
        Some(intrinsic) => intrinsic,
        None => rebuild(left, right),
    }))
}

/// `Int/div`/`Int/rem`: like [`reduce_int_binary`], but a divisor that reduces to literal zero is a reported error — mathematically undefined, following `BinGet`'s pattern. The fold itself is exact and total past that: the type level pretends ℤ (see `Int`).
pub(super) fn reduce_int_division(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    kind: &'static str,
    fold: impl FnOnce(Integer, Integer) -> Option<Integer>,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let span = right.span().or_else(|| left.span());
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    if right.as_int().is_some_and(|divisor| divisor.is_zero()) {
        return Err(ReduceError::DivisionByZero { kind, span });
    }

    let folded = match (left.as_int(), right.as_int()) {
        (Some(l), Some(r)) => fold(l, r).map(Intrinsic::Int),
        _ => None,
    };

    Ok(Subterm::Intrinsic(match folded {
        Some(intrinsic) => intrinsic,
        None => rebuild(left, right),
    }))
}

/// `Flt` operations fold on literal operands by calling the model, `curios_num::Floating` — binary32 computed exactly over unbounded integers and rounded once, rather than whatever the compiler's host computes. There is no decline gate: with exactly one NaN and a runtime held to the same clauses, the model leaves nothing undetermined, so `1.0 + 1.0` is `2.0`, `1.0 / 0.0` is `+inf`, `0.0 / 0.0` is the NaN, and each is true of the running program. A symbolic operand rebuilds the neutral term.
///
/// **Why folding here is not the hazard the opacity this replaced was afraid of.** IEEE equality identifies `0.0` with `-0.0`, which `FltToLeBytes` tells apart — the singleton-forgery shape — but folding `FltEql(0.0, -0.0)` to the `Bool` `true` creates no convertibility: `Eq` still needs `refl`, conversion on literals is bitwise, and scrutinee refinement rewrites the scrutinee term rather than an operand. What *would* be a hazard is a fold the running program can disagree with, and the only thing IEEE and Wasm leave to the implementation is a computed NaN's sign and payload — which the one canonical NaN removes, and which `into_wasm` closes at the two operations that could read those bits.
///
/// The rule the opacity established survives verbatim: an intrinsic needs a fold here only if a type or a proof can depend on its value. `Flt` has moved to the other side of it, because [`/syn/Flt/Finite` and `/syn/Flt/NonNeg`](Intrinsic::signature) are bounds decided by a comparison.
///
/// One fact predates all of it, and `free_monoid::bin_measure` is where: `Bin/len(Flt/to_le_bytes(x))` is `4` for every `x`, symbolic `x` included. That is the arity of the operation's result rather than anything about the float, and it is what makes `Flt/of_le_bytes`'s length precondition dischargeable over the operation it inverts.
pub(super) fn reduce_flt_binary(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(Floating, Floating) -> Intrinsic,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    let folded = match (left.as_flt(), right.as_flt()) {
        (Some(l), Some(r)) => Some(fold(l, r)),
        _ => None,
    };

    Ok(Subterm::Intrinsic(match folded {
        Some(intrinsic) => intrinsic,
        None => rebuild(left, right),
    }))
}

/// `Int` counterpart of [`reduce_nat_unary`]. The fold's `None` rebuilds the neutral term: with `Int` unbounded at the type level, a conversion of a value the target cannot represent simply stays stuck.
pub(super) fn reduce_int_unary(
    reducer: &mut impl Reducer,
    inner: &Term,
    fold: impl FnOnce(Integer) -> Option<Intrinsic>,
    rebuild: impl FnOnce(Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let inner = reducer.reduce_forced(inner.clone())?;

    Ok(Subterm::Intrinsic(match inner.as_int().and_then(fold) {
        Some(intrinsic) => intrinsic,
        None => rebuild(inner),
    }))
}

/// [`reduce_flt_binary`]'s unary counterpart. The fold's `None` rebuilds the neutral term, which is how the two narrowings answer an operand outside the domain their bound states: a well-typed call carries a proof that excludes it, and a term that reaches here without one stays stuck rather than being given a value the model does not define.
pub(super) fn reduce_flt_unary(
    reducer: &mut impl Reducer,
    inner: &Term,
    fold: impl FnOnce(Floating) -> Option<Intrinsic>,
    rebuild: impl FnOnce(Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let inner = reducer.reduce_forced(inner.clone())?;

    Ok(Subterm::Intrinsic(match inner.as_flt().and_then(fold) {
        Some(intrinsic) => intrinsic,
        None => rebuild(inner),
    }))
}
