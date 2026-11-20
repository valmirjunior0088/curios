//! The per-carrier operand folds: reduce both sides, and answer from the values when both are literals.
//!
//! One function per carrier and arity, each doing the same three things — reduce, match a literal pair, rebuild the redex when the match fails. What a failed match falls through to is the caller's business: the laws in [`laws`](super::laws), or nothing.

use {
    super::{nat_bound, operand_bound},
    crate::{
        Cost, Intrinsic, Nat, ReduceError, Reducer, Subterm, Term, int_exact_quotient,
        int_from_linear, int_is_zero, int_linear, int_sum, int_terms, same_atom,
    },
    curios_algebra::{
        Carrier, Divided, Half, signed_cofactor, signed_euclid_split, signed_floor_law,
    },
    curios_num::{Floating, Integer, Natural},
};

/// Read an already-reduced `Nat` term as a concrete `usize` index — `None` when it is still symbolic or too large to fit. The shared decode behind the `Bytes`/`List` `get`/`slice` bounds.
pub fn as_index(term: &Term) -> Option<usize> {
    term.as_nat()
        .and_then(|n| usize::try_from(&n.to_natural()?).ok())
}

/// Whether a `Bool` binary fold reads its right operand under a stuck left. `&&` and `||` leave it as written; `==`, `!=` and `xor` reduce it; see [`reduce_bool_binary`] for why each side of that line is where it is.
pub enum Right {
    AsWritten,
    Reduced,
}

/// Reduce the operands of a `Bool` binary intrinsic as far as a fold could use them, then either `fold` the two literals or `rebuild` the neutral term. `Bool` has no numeric carrier at the type level, so the fold reads the `true`/`false` constructors directly.
///
/// **A connective's right operand is reduced only once the left is a literal.** A fold needs both, so a stuck left settles the verdict whatever the right holds, and reducing the right then is work the answer cannot use. It was reduced regardless, and that made weak-head reduction of a `&&`/`||` tree its *full* normalization: a web of predicate definitions each naming the one before it twice unfolded `2^n` times under any demand on its top, since a local-bearing term is remembered by nothing — the cliff `curios`' `scrutinee_refinement_measurements` records under `proved`. Stopping at the left leaves the right as written, which conversion compares lazily through its own reduction, so no equality decision moves, and an elimination reads the stuck connective by the decision diagram, which reads its graph once. `==`, `!=` and `xor` read both operands, as the `Nat` folds below do, because their laws read the right — `b == true` is `b`, `xor(b, xor(b, c))` is `c`, and `b == not b` is `false` only once `not b` is the `xor` it unfolds to — and no predicate web is built out of equalities the way one is out of conjunctions: `not` is `xor(_, true)`, whose right is a literal.
pub fn reduce_bool_binary(
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

/// `Int/shl` and `Int/shr`: a signed value over a `Nat` count, the signed twins of [`reduce_nat_shl`](super::reduce_nat_shl) and its right shift. `cost` is what the fold may construct from the value's width and the count — [`shift_bound`](super::shift_bound) for a left shift, whose result grows by the count, and [`operand_bound`] for a right shift, whose result never does — and `fold` declines only a left shift by a count too large to be a shift count at all; the right shift is total, a count past the width answering the sign.
pub fn reduce_int_shift(
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

/// `Int` counterpart of [`reduce_nat_binary`](super::reduce_nat_binary): fold both literal operands or rebuild the neutral term. The fold is partial for the same reason — the shifts decline a negative or oversized literal shift count (`None`); the total ops just wrap their result in `Some`.
pub fn reduce_int_binary(
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

/// `Int/div`/`Int/rem`: like [`reduce_int_binary`], but a divisor that reduces to literal zero leaves the division stuck — mathematically undefined, and past the domain its bound states, following `BytesGet`'s pattern. The fold itself is exact and total past that: the type level pretends ℤ (see `Int`).
///
/// Beside the two-literal fold stand ℕ's laws, which Euclid's division keeps at ℤ: those that read no sign, an exact multiple, and at a literal divisor the Euclidean split ([`int_euclid_split`]) and then the floor law (`curios-algebra`'s `signed_floor_law`), the remainder a natural throughout.
pub fn reduce_int_division(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    half: Half,
    rebuild: impl Fn(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    // The remainder lies in `[0, |d|)`, a natural, and the quotient is an integer.
    let fold: fn(&Integer, &Integer) -> Option<Intrinsic> = match half {
        Half::Quotient => |dividend, divisor| dividend.div(divisor).ok().map(Intrinsic::Int),
        Half::Remainder => |dividend, divisor| {
            let remainder = dividend.rem(divisor).ok()?;
            Some(Intrinsic::Nat(Nat::new(remainder.magnitude())))
        },
    };
    let zero = || match half {
        Half::Quotient => Subterm::Intrinsic(Intrinsic::Int(Integer::from(0i32))),
        Half::Remainder => Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)),
    };
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    // A literal zero divisor is past the domain the division's bound states, which only a hypothesis stating the impossible reaches, and no integer answers it: the division stays stuck, and none of the laws below is read over it.
    if right.as_int().is_some_and(|divisor| divisor.is_zero()) {
        return Ok(Subterm::Intrinsic(rebuild(left, right)));
    }

    if let (Some(l), Some(r)) = (left.as_int(), right.as_int())
        && let Some(folded) = fold(&l, &r)
    {
        return Ok(Subterm::Intrinsic(folded));
    }

    // The laws that read no sign, the ones ℤ's division shares with ℕ's: a zero dividend divides to `0` with remainder `0` by any divisor, a dividend divides by `1` to itself with remainder `0`, and a dividend that is its divisor to `1` with remainder `0`, on the division's own precondition that the divisor is nonzero.
    let divided = match () {
        _ if int_is_zero(&left) => Some(half.of_zero()),
        _ if right.as_int() == Some(Integer::from(1i32)) => Some(half.by_one()),
        _ if same_atom(Carrier::Integer, &left, &right) => Some(half.by_itself()),
        _ => None,
    };
    if let Some(divided) = divided {
        return Ok(match divided {
            Divided::Zero => zero(),
            Divided::Dividend => Term::unwrap_or_clone(left),
            Divided::One => Subterm::Intrinsic(Intrinsic::Int(Integer::from(1i32))),
        });
    }

    // A literal divisor that divides the dividend's constant and every coefficient divides each in place and leaves no remainder, whatever the signs: `i / +1` is `i`, `i / -1` is `-i`, `(i * -6) / 2` is `i * -3`. The quotient has as many monomials as the dividend, charged before it is built.
    if let Some(divisor) = right.as_int()
        && let Some(divided) =
            int_literal_division(reducer, &left, &divisor, half, false, |rest| {
                rebuild(rest, right.clone())
            })?
    {
        return Ok(Term::unwrap_or_clone(divided));
    }

    Ok(Subterm::Intrinsic(rebuild(left, right)))
}

/// A half of the division of a symbolic `dividend` by the nonzero literal `divisor`, at `Int`, by the laws a literal divisor admits there: an exact multiple, the Euclidean split ([`int_euclid_split`]), then the floor law alone (`curios-algebra`'s `signed_floor_law`), which peels the whole divisors the constant certainly carries and leaves the rest divided by `divided`, in the spelling the division was read in. The remainder is a natural, answered `widened` where the spelling's remainder is an integer, as a mask's is. `None` where no law applies.
///
/// Every spelling of an `Int` division by a literal takes its laws here — `/` and `Int/rem` beside their proof, and a right shift or a mask by a power of two (`then_power_division`) — as `Nat`'s take theirs in one place.
pub fn int_literal_division(
    reducer: &mut impl Reducer,
    dividend: &Term,
    divisor: &Integer,
    half: Half,
    widened: bool,
    divided: impl FnOnce(Term) -> Intrinsic,
) -> Result<Option<Term>, ReduceError> {
    reducer.spend(Cost::collection(int_terms(dividend).1.len() as u64 + 1))?;
    // A widened remainder goes back through the reducer, which writes the widening into `Int`'s normal form.
    let mut remainder = |natural: Term| match widened {
        true => reducer.reduce_forced(Term::intrinsic(Intrinsic::NatToInt(natural))),
        false => Ok(natural),
    };
    let (constant, summands) = int_terms(dividend);
    if let Some(quotient) = int_exact_quotient(dividend, divisor) {
        return Ok(Some(match half {
            Half::Quotient => quotient,
            Half::Remainder => remainder(Term::intrinsic(Intrinsic::Nat(Nat::Zero)))?,
        }));
    }
    if let Some((quotient, natural)) = int_euclid_split(dividend, divisor) {
        return Ok(Some(match half {
            Half::Quotient => quotient,
            Half::Remainder => remainder(natural)?,
        }));
    }

    // The floor law alone, for a dividend the split could not close: peel the whole divisors the constant certainly carries and leave the rest divided.
    let Some((whole, rest)) = signed_floor_law(&constant, divisor) else {
        return Ok(None);
    };
    let peeled = Term::intrinsic(divided(int_from_linear(rest, int_linear(summands))));
    Ok(Some(match half {
        Half::Quotient => int_sum(&peeled, &Term::intrinsic(Intrinsic::Int(whole))),
        Half::Remainder => peeled,
    }))
}

/// A reduced dividend split against a nonzero literal divisor by Euclid's division into `(quotient, remainder)`, an integer and a natural, or `None` where the division is not forced: every monomial either a multiple of the divisor, which contributes its cofactor to the quotient, or a widened natural under a positive coefficient, whose bound ([`nat_bound`]) it adds to what the remainder may reach (`curios-algebra`'s `signed_euclid_split`). That is what makes `(2 · i + 1) / 2` reduce to `i` and its remainder to `1`.
fn int_euclid_split(dividend: &Term, divisor: &Integer) -> Option<(Term, Term)> {
    let (constant, summands) = int_terms(dividend);
    let mut quotient = Vec::new();
    let mut residual = Vec::new();
    let mut bounds = Vec::new();
    for (coefficient, factors) in int_linear(summands) {
        if let Some(cofactor) = signed_cofactor(&coefficient, divisor) {
            quotient.push((cofactor, factors));
            continue;
        }
        let [factor] = factors.as_slice() else {
            return None;
        };
        let Subterm::Intrinsic(Intrinsic::NatToInt(natural)) = &**factor else {
            return None;
        };
        if coefficient.sign() != Integer::from(1i32) {
            return None;
        }
        let scale = coefficient.magnitude();
        bounds.push(scale.clone() * nat_bound(natural)?);
        residual.push(Nat::scaled(scale, natural.clone()));
    }

    let (whole, rest) = signed_euclid_split(&constant, divisor, bounds)?;
    Some((
        int_from_linear(whole, quotient),
        Nat::sum_over_floor(residual, rest),
    ))
}

/// `Flt` operations fold on literal operands by calling the model, `curios_num::Floating` — binary64 computed exactly over unbounded integers and rounded once, rather than whatever the compiler's host computes. There is no decline gate: the model pins every choice IEEE leaves open, which NaN an operation answers included, and the runtime is held to the same clauses, so `1.0 + 1.0` is `2.0`, `1.0 / 0.0` is `+inf`, `0.0 / 0.0` is the default NaN, and each is true of the running program. A symbolic operand rebuilds the neutral term.
///
/// **Why folding here is no hazard.** IEEE equality identifies `0.0` with `-0.0`, which `FltToLeBytes` tells apart — the singleton-forgery shape — but folding `FltEql(0.0, -0.0)` to the `Bool` `true` creates no convertibility: `Eq` still needs `refl`, conversion on literals is bitwise, and scrutinee refinement rewrites the scrutinee term rather than an operand. What *would* be a hazard is a fold the running program can disagree with, and the only thing IEEE and Wasm leave to the implementation is a computed NaN's sign and payload — which the model's NaN rule pins, and which `into_wasm` holds the running program to.
///
/// The rule the opacity established survives verbatim: an intrinsic needs a fold here only if a type or a proof can depend on its value. `Flt` has moved to the other side of it, because the domains [`Intrinsic::signature`] states for `Flt/to_int` and `Flt/to_nat` are bounds decided by a comparison.
///
/// One fact predates all of it, and `free_monoid::bytes_measure` is where: `Bytes/len(Flt/to_le_bytes(x))` is `8` for every `x`, symbolic `x` included. That is the arity of the operation's result rather than anything about the float, and it is what makes `Flt/of_le_bytes`'s length precondition dischargeable over the operation it inverts.
pub fn reduce_flt_binary(
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
pub fn reduce_flt_ternary(
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

/// `Int` counterpart of [`reduce_nat_unary`](super::reduce_nat_unary). The fold's `None` rebuilds the neutral term: with `Int` unbounded at the type level, a conversion of a value the target cannot represent simply stays stuck.
pub fn reduce_int_unary(
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
pub fn reduce_flt_unary(
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
