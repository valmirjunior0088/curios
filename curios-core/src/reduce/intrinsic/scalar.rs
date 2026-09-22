//! The per-carrier operand folds: reduce both sides, and answer from the values when both are literals.
//!
//! One function per carrier and arity, each doing the same three things — reduce, match a literal pair, rebuild the redex when the match fails. What a failed match falls through to is the caller's business: the laws in [`laws`], or nothing.

use {
    super::*,
    crate::{Cost, Intrinsic, ReduceError, Reducer, Subterm, Term},
    curios_num::{Floating, Integer, Natural},
};

/// Read an already-reduced `Nat` term as a concrete `usize` index — `None` when it is still symbolic or too large to fit. The shared decode behind the `Bin`/`List` `get`/`slice` bounds.
pub(super) fn as_index(term: &Term) -> Option<usize> {
    term.as_nat()
        .and_then(|n| usize::try_from(&n.to_natural()?).ok())
}

/// Whether a `Bool` binary fold reads its right operand under a stuck left. `&&` and `||` leave it as written; `==`, `!=` and `xor` reduce it; see [`reduce_bool_binary`] for why each side of that line is where it is.
pub(super) enum Right {
    AsWritten,
    Reduced,
}

/// Reduce the operands of a `Bool` binary intrinsic as far as a fold could use them, then either `fold` the two literals or `rebuild` the neutral term. `Bool` has no numeric carrier at the type level, so the fold reads the `true`/`false` constructors directly.
///
/// **A connective's right operand is reduced only once the left is a literal.** A fold needs both, so a stuck left settles the verdict whatever the right holds, and reducing the right then is work the answer cannot use. It was reduced regardless, and that made weak-head reduction of a `&&`/`||` tree its *full* normalization: a web of predicate definitions each naming the one before it twice unfolded `2^n` times under any demand on its top, since a local-bearing term is remembered by nothing — the cliff `curios`' `scrutinee_refinement_measurements` records under `proved`. Stopping at the left leaves the right as written, which conversion compares lazily through its own reduction, so no equality decision moves. `==`, `!=` and `xor` read both operands, as the `Nat` folds below do, because their laws read the right — `b == true` is `b`, `xor(b, xor(b, c))` is `c`, and `b == not b` is `false` only once `not b` is the `xor` it unfolds to — and no predicate web is built out of equalities the way one is out of conjunctions: `not` is `xor(_, true)`, whose right is a literal.
pub(super) fn reduce_bool_binary(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    reads: Right,
    fold: impl FnOnce(bool, bool) -> bool,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let Some(l) = left.as_bool() else {
        let right = match reads {
            Right::AsWritten => right.clone(),
            Right::Reduced => reducer.reduce_forced(right.clone())?,
        };
        return Ok(Subterm::Intrinsic(rebuild(left, right)));
    };

    let right = reducer.reduce_forced(right.clone())?;
    Ok(Subterm::Intrinsic(match right.as_bool() {
        Some(r) => Intrinsic::Bool(fold(l, r)),
        None => rebuild(left, right),
    }))
}

/// `Int/shl` and `Int/shr`: a signed value over a `Nat` count, the signed twins of [`reduce_nat_shl`] and its right shift. `cost` is what the fold may construct from the value's width and the count — [`shift_bound`] for a left shift, whose result grows by the count, and [`operand_bound`] for a right shift, whose result never does — and `fold` declines only a left shift by a count too large to be a shift count at all; the right shift is total, a count past the width answering the sign.
pub(super) fn reduce_int_shift(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    cost: impl FnOnce(u64, Option<u64>) -> Cost,
    fold: impl FnOnce(Integer, Natural) -> Option<Integer>,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    let folded = match (left.as_int(), right.as_nat().and_then(|n| n.to_natural())) {
        (Some(value), Some(amount)) => {
            reducer.spend(cost(value.bits(), u64::try_from(&amount).ok()))?;

            fold(value, amount).map(Intrinsic::Int)
        }
        _ => None,
    };

    Ok(Subterm::Intrinsic(match folded {
        Some(intrinsic) => intrinsic,
        None => rebuild(left, right),
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

/// `Flt` operations fold on literal operands by calling the model, `curios_num::Floating` — binary64 computed exactly over unbounded integers and rounded once, rather than whatever the compiler's host computes. There is no decline gate: the model pins every choice IEEE leaves open, which NaN an operation answers included, and the runtime is held to the same clauses, so `1.0 + 1.0` is `2.0`, `1.0 / 0.0` is `+inf`, `0.0 / 0.0` is the default NaN, and each is true of the running program. A symbolic operand rebuilds the neutral term.
///
/// **Why folding here is not the hazard the opacity this replaced was afraid of.** IEEE equality identifies `0.0` with `-0.0`, which `FltToLeBytes` tells apart — the singleton-forgery shape — but folding `FltEql(0.0, -0.0)` to the `Bool` `true` creates no convertibility: `Eq` still needs `refl`, conversion on literals is bitwise, and scrutinee refinement rewrites the scrutinee term rather than an operand. What *would* be a hazard is a fold the running program can disagree with, and the only thing IEEE and Wasm leave to the implementation is a computed NaN's sign and payload — which the model's NaN rule pins, and which `into_wasm` holds the running program to.
///
/// The rule the opacity established survives verbatim: an intrinsic needs a fold here only if a type or a proof can depend on its value. `Flt` has moved to the other side of it, because [`/sys/Bound/Finite` and `/sys/Bound/NonNeg`](Intrinsic::signature) are bounds decided by a comparison.
///
/// One fact predates all of it, and `free_monoid::bin_measure` is where: `Bin/len(Flt/to_le_bytes(x))` is `8` for every `x`, symbolic `x` included. That is the arity of the operation's result rather than anything about the float, and it is what makes `Flt/of_le_bytes`'s length precondition dischargeable over the operation it inverts.
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

/// [`reduce_flt_binary`]'s three-operand counterpart, for the fused multiply-add: every operand a literal folds, and anything symbolic rebuilds the neutral term.
pub(super) fn reduce_flt_ternary(
    reducer: &mut impl Reducer,
    (a, b, c): (&Term, &Term, &Term),
    fold: impl FnOnce(Floating, Floating, Floating) -> Intrinsic,
    rebuild: impl FnOnce(Term, Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let a = reducer.reduce_forced(a.clone())?;
    let b = reducer.reduce_forced(b.clone())?;
    let c = reducer.reduce_forced(c.clone())?;

    let folded = match (a.as_flt(), b.as_flt(), c.as_flt()) {
        (Some(x), Some(y), Some(z)) => Some(fold(x, y, z)),
        _ => None,
    };

    Ok(Subterm::Intrinsic(match folded {
        Some(intrinsic) => intrinsic,
        None => rebuild(a, b, c),
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
