//! The `Nat` folds that need more than an operand pair: division, and the bound and split that make it decide under symbols.
//!
//! `Nat` division is the one arithmetic family whose fold reaches past literals: [`nat_bound`] states how large a shape can be, and [`nat_euclid_split`] uses that to peel a quotient off a sum whose remainder cannot reach the divisor. The bound must never under-report — the split turns it into a definitional equation.

use {
    super::*,
    crate::{Intrinsic, Nat, ReduceError, Reducer, Subterm, Term},
};

/// Which half of a Euclidean division a fold computes. One enum rather than the pair of closures the other families take: the symbolic laws below build the quotient and the remainder out of the *same* split, so the two halves cannot be parameterized independently.
#[derive(Clone, Copy)]
pub(super) enum Euclid {
    Quotient,
    Remainder,
}

impl Euclid {
    pub(super) fn kind(self) -> &'static str {
        match self {
            Euclid::Quotient => "Nat/div",
            Euclid::Remainder => "Nat/rem",
        }
    }

    pub(super) fn fold(self, left: Nat, right: Nat) -> Option<Nat> {
        match self {
            Euclid::Quotient => left.checked_div(right),
            Euclid::Remainder => left.checked_rem(right),
        }
    }

    /// The neutral rebuild carries the *original* proof through unreduced. Its proposition is stated over the operands, which have only been reduced, so the two are convertible and the same proof still inhabits the rebuilt bound — reduction never has to derive one. Leaving it unreduced is deliberate besides: a bound's normal form is unobservable under proof irrelevance, and reducing into it would unfold whatever the caller proved it with, at every division this passes.
    pub(super) fn rebuild(self, left: Term, right: Term, non_zero: Term) -> Intrinsic {
        match self {
            Euclid::Quotient => Intrinsic::NatDiv {
                dividend: left,
                divisor: right,
                non_zero,
            },
            Euclid::Remainder => Intrinsic::NatRem {
                dividend: left,
                divisor: right,
                non_zero,
            },
        }
    }
}

/// A statically known upper bound on every value a reduced term can take, or `None` where it has none.
///
/// Every arm is unconditional, which is what lets the callers below turn a bound into a definitional equation. A `Byte` is `0..=255` by its carrier — `Nat/to_byte` wraps and `Byte` is not a wire type, so no embedder can supply one outside the range — and `x % n < n` holds by definition, a zero divisor having already been reported. The remaining arms are monotone in operands whose own bounds this establishes.
///
/// A wrong bound here is a false definitional equation, not a wrong value: see `documentation/soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md`.
pub(super) fn nat_bound(term: &Term) -> Option<Natural> {
    let Subterm::Intrinsic(intrinsic) = &**term else {
        return None;
    };

    match intrinsic {
        Intrinsic::Nat(Nat::Zero) => Some(Natural::zero()),
        Intrinsic::Nat(Nat::Succ(floor, inner)) => Some(floor + nat_bound(inner)?),
        Intrinsic::ByteToNat(_) => Some(Natural::from(u8::MAX)),
        Intrinsic::NatRem { divisor, .. } => {
            let divisor = divisor.as_nat()?.to_natural()?;
            (!divisor.is_zero()).then(|| divisor - Natural::one())
        }
        // Either bound alone is an upper bound, so one suffices; with both, the smaller wins.
        Intrinsic::NatAnd(left, right) => match (nat_bound(left), nat_bound(right)) {
            (Some(left), Some(right)) => Some(left.min(right)),
            (Some(bound), None) | (None, Some(bound)) => Some(bound),
            (None, None) => None,
        },
        Intrinsic::NatAdd(left, right) => Some(nat_bound(left)? + nat_bound(right)?),
        Intrinsic::NatMul(left, right) => Some(nat_bound(left)? * nat_bound(right)?),
        _ => None,
    }
}

/// A reduced summand read as `coefficient · factor` with a *literal* coefficient, or `None` for a summand that is not such a product — the reading [`Nat::literal_factor`] takes, minus its unit default, for the callers that need to know whether a literal was there.
fn nat_literal_factor(summand: &Term) -> Option<(Natural, Term)> {
    matches!(&**summand, Subterm::Intrinsic(Intrinsic::NatMul(..)))
        .then(|| Nat::literal_factor(summand))
        .filter(|(_, factor)| factor != summand)
}

/// Split a reduced dividend against a literal divisor into `(quotient, remainder)`, or `None` where the division is not forced.
///
/// Every summand must be either a literal multiple of `n` — contributing its cofactor to the quotient — or statically bounded. When the bounded summands together with the residual floor stay below `n`, none of them can carry into the next multiple, so the split is exact for every value the symbolic parts take. That is what makes `(256·x + Byte/to_nat(b)) / 256` reduce to `x`.
pub(super) fn nat_euclid_split(dividend: &Term, divisor: &Natural) -> Option<(Term, Term)> {
    let (floor, inner) = Nat::decompose(dividend);
    let mut quotient = Vec::new();
    let mut residual = Vec::new();
    let mut ceiling = &floor % divisor;

    for summand in Nat::summands(&inner) {
        match nat_literal_factor(&summand) {
            Some((coefficient, factor)) if (&coefficient % divisor).is_zero() => {
                quotient.push(Nat::scaled(coefficient / divisor, factor));
            }
            _ => {
                ceiling += nat_bound(&summand)?;
                residual.push(summand);
            }
        }
    }

    match ceiling < *divisor {
        true => Some((
            Nat::sum_over_floor(quotient, &floor / divisor),
            Nat::sum_over_floor(residual, &floor % divisor),
        )),
        false => None,
    }
}

/// `Nat/div`/`Nat/rem`: partial, like [`reduce_nat_binary`] is not — a divisor that reduces to literal zero is a reported error (the type-level mirror of the runtime trap, following `BinGet`'s pattern), never a Rust panic.
///
/// Past the closed fold, two unconditional laws let a literal divisor see through a symbolic dividend. Writing the dividend as `inner + floor` and the divisor as `n`:
///
/// The *floor law* is the division twin of `NatAdd`'s: `(i + f) / n = f/n + (i + f%n) / n`, and `(i + f) % n = (i + f%n) % n`. Both hold for every `i`, because `f = (f/n)·n + f%n` contributes exactly `f/n` whole divisors whatever `i` is. As with addition the floor only moves outward, and the residual floor `f%n < n` cannot fire the rule a second time.
///
/// The *split* additionally reads the summands, and is the rule that makes a base-256 encoding provably injective; [`nat_euclid_split`] states it and [`nat_bound`] states why the bounds it rests on are unconditional.
///
/// Nothing conditional may be added here. `(a + b)/n = a/n + b/n` is false — `1/2 + 1/2 = 0 ≠ 1` — so a law holding only for some values of a symbolic part would be a false definitional equation, and congruence carries one of those to `False`.
pub(super) fn reduce_nat_division(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    non_zero: &Term,
    euclid: Euclid,
) -> Result<Subterm, ReduceError> {
    let span = right.span().or_else(|| left.span());
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    let divisor = right.as_nat().and_then(|divisor| divisor.to_natural());
    if divisor.as_ref().is_some_and(Natural::is_zero) {
        return Err(ReduceError::DivisionByZero {
            kind: euclid.kind(),
            span,
        });
    }

    if let (Some(dividend), Some(by)) = (left.as_nat(), right.as_nat())
        && let Some(folded) = euclid.fold(dividend, by)
    {
        return Ok(Subterm::Intrinsic(Intrinsic::Nat(folded)));
    }

    // The unconditional laws a symbolic part cannot falsify: a zero dividend divides to `0` with remainder `0` by any divisor, a dividend divides by `1` to itself with remainder `0`, and a dividend divides by itself to `1` with remainder `0` — the last on the operation's own precondition that the divisor is nonzero, which its proof operand states for every value.
    let zero = || Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero));
    if Nat::is_zero(&left) {
        return Ok(zero());
    }
    if divisor.as_ref().is_some_and(Natural::is_one) {
        return Ok(match euclid {
            Euclid::Quotient => Term::unwrap_or_clone(left),
            Euclid::Remainder => zero(),
        });
    }
    if left == right {
        return Ok(match euclid {
            Euclid::Quotient => Subterm::Intrinsic(Intrinsic::Nat(Nat::new(Natural::one()))),
            Euclid::Remainder => zero(),
        });
    }

    if let Some(divisor) = &divisor {
        if let Some((quotient, remainder)) = nat_euclid_split(&left, divisor) {
            return Ok(Term::unwrap_or_clone(match euclid {
                Euclid::Quotient => quotient,
                Euclid::Remainder => remainder,
            }));
        }

        // The floor law alone, for a dividend the split could not close: peel the whole divisors the floor certainly carries and leave the rest neutral.
        let (floor, inner) = Nat::decompose(&left);
        if floor >= *divisor {
            let peeled = Term::intrinsic(euclid.rebuild(
                Nat::rebuild(&floor % divisor, inner),
                right.clone(),
                non_zero.clone(),
            ));

            return Ok(Term::unwrap_or_clone(match euclid {
                Euclid::Quotient => Nat::rebuild(&floor / divisor, peeled),
                Euclid::Remainder => peeled,
            }));
        }
    }

    Ok(Subterm::Intrinsic(euclid.rebuild(
        left,
        right,
        non_zero.clone(),
    )))
}

/// Reduce both operands of a `Nat` binary intrinsic, then either `fold` the two literals or `rebuild` the neutral term from the reduced operands.
///
/// The fold is charged [`operand_bound`] before it runs, so every operation reaching here must have a result bounded by its operands' widths. `Nat/shl` does not and is folded by [`reduce_nat_shl`] instead.
pub(super) fn reduce_nat_binary(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(Nat, Nat) -> Option<Intrinsic>,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    let folded = match (left.as_nat(), right.as_nat()) {
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

/// `Nat/shl`, folded under [`shift_bound`] rather than [`operand_bound`].
pub(super) fn reduce_nat_shl(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    let folded = match (left.as_nat(), right.as_nat()) {
        (Some(value), Some(amount)) => {
            reducer.spend(shift_bound(value.bits(), amount.to_u64()))?;

            value.checked_shl(amount).map(Intrinsic::Nat)
        }
        _ => None,
    };

    Ok(Subterm::Intrinsic(match folded {
        Some(intrinsic) => intrinsic,
        None => Intrinsic::NatShl(left, right),
    }))
}

/// Reduce the operand of a `Nat` unary intrinsic, then either `fold` the literal or `rebuild` the neutral term from the reduced operand.
pub(super) fn reduce_nat_unary(
    reducer: &mut impl Reducer,
    inner: &Term,
    fold: impl FnOnce(Nat) -> Option<Intrinsic>,
    rebuild: impl FnOnce(Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let inner = reducer.reduce_forced(inner.clone())?;

    Ok(Subterm::Intrinsic(match inner.as_nat().and_then(fold) {
        Some(intrinsic) => intrinsic,
        None => rebuild(inner),
    }))
}
