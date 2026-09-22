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
/// Every arm is unconditional, which is what lets the callers below turn a bound into a definitional equation.
///
/// **An arm exists where the result is bounded in every operand the value is not *antitone* in.** That is the criterion, and stating it is the point: what a bound was computed for used to be whatever somebody needed, which is not a property anything could check. It is strictly narrower than monotonicity — a product is monotone in each factor separately and still needs *both* bounded, since either one left free makes it unbounded — and it is what licenses the one-sided arms: a value that only shrinks as an operand grows needs no bound on that operand at all, which is why a subtrahend, a divisor and a shift amount are all read past.
///
/// A `Byte` is `0..=255` **by its carrier**, a fact about the type rather than about how the value was produced. Every producer establishes it, and no case analysis over them is performed or wanted: the operand under a stuck `ByteToNat` is normally a bare binder or a projection, which is exactly the seam this bound exists to serve. `Byte` is not a wire type either, so no embedder can supply one outside the range. `x % n < n` holds by definition, a zero divisor having already been reported.
///
/// **`NatShl` is deliberately absent, and the reason is resources rather than arithmetic.** A left shift is the one fold whose result size is not bounded by its operands' — `Nat/shl(1, 400000000)` is fifty megabytes of magnitude out of three lines of surface Curios — and this function takes no [`crate::Reducer`], so it cannot `spend` against the budget that exists to price exactly that. An arm would perform, uncharged, the allocation the reduction arm is careful to charge for. What reaches here as a `NatShl` is a shift by a *symbolic* count, which no operand bounds: a literal count is [`then_coefficient`]'s, so it arrives as a `NatMul` with its coefficient already charged and takes that arm.
///
/// An over-report only withholds the rule; an *under*-report is a false definitional equation, which is the direction `bound_upper_bounds_every_closed_instantiation` asserts. That gate is a hand-written block per shape rather than an enumeration, so an arm added here owes it one or it passes while checking nothing. A wrong bound is a false equation and not a wrong value: see `documentation/soundness/per-term-rules/the-bounds-oracle-and-the-division-family.md`.
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
        // Truncation only shrinks, so the left bound carries alone: the difference is antitone in the subtrahend, whatever it is.
        Intrinsic::NatSub(left, _) => nat_bound(left),
        // Antitone in the divisor, which `non_zero` holds at one or more — so the quotient is at most the dividend however the divisor is spelled, and a literal one tightens that to the quotient of the bounds.
        Intrinsic::NatDiv {
            dividend, divisor, ..
        } => {
            let bound = nat_bound(dividend)?;
            match divisor.as_nat().and_then(|divisor| divisor.to_natural()) {
                Some(divisor) => bound.div(&divisor).ok(),
                None => Some(bound),
            }
        }
        // The division arm's shift twin, antitone for the same reason: a right shift discards bits and adds none.
        Intrinsic::NatShr(operand, amount) => {
            let bound = nat_bound(operand)?;
            match amount.as_nat().and_then(|amount| amount.to_natural()) {
                Some(amount) => Some(&bound >> &amount),
                None => Some(bound),
            }
        }
        // Either bound alone is an upper bound, so one suffices; with both, the smaller wins.
        Intrinsic::NatAnd(left, right) => match (nat_bound(left), nat_bound(right)) {
            (Some(left), Some(right)) => Some(left.min(right)),
            (Some(bound), None) | (None, Some(bound)) => Some(bound),
            (None, None) => None,
        },
        // A join or a difference of bits reaches no bit neither side can, so the bound is the widest value of that many bits — taken over the *bounds*' bit lengths, the operands themselves being symbolic. Both operands are needed, neither being antitone. This is at most twice the larger bound, so unlike a left shift it cannot outgrow what it was handed.
        Intrinsic::NatOr(left, right) | Intrinsic::NatXor(left, right) => {
            let width = nat_bound(left)?.bits().max(nat_bound(right)?.bits());
            let width = u32::try_from(width).ok()?;
            Some(Natural::from(2u32).pow(width) - Natural::one())
        }
        Intrinsic::NatAdd(left, right) => Some(nat_bound(left)? + nat_bound(right)?),
        Intrinsic::NatMul(left, right) => Some(nat_bound(left)? * nat_bound(right)?),
        _ => None,
    }
}

/// The operands a reduced term never exceeds, as terms, each with whether the bound is strict — [`nat_bound`]'s criterion read at the operands instead of at a literal: a result antitone in one operand is at most its other operand, so the minuend, the dividend, either operand of `and` and the shifted value each bound their result, and a remainder is below its divisor outright, `non_zero` having placed the divisor at one or more. Only a shape whose operand *is* the bound is listed: a sum or a product of bounded parts stays under no operand of its own, and `nat_bound` is where those go.
///
/// Every pair is unconditional, as the literal oracle's arms are, and for the same reason it may be turned into a verdict: an under-report here is a false definitional equation. `dominators_upper_bound_every_closed_instantiation` holds each listed pair over values, block per shape.
pub(super) fn nat_dominators(term: &Term) -> Vec<(Term, bool)> {
    let Subterm::Intrinsic(intrinsic) = &**term else {
        return Vec::new();
    };

    match intrinsic {
        Intrinsic::NatSub(left, _) => vec![(left.clone(), false)],
        Intrinsic::NatDiv { dividend, .. } => vec![(dividend.clone(), false)],
        Intrinsic::NatRem {
            dividend, divisor, ..
        } => vec![(dividend.clone(), false), (divisor.clone(), true)],
        Intrinsic::NatAnd(left, right) => vec![(left.clone(), false), (right.clone(), false)],
        Intrinsic::NatShr(operand, _) => vec![(operand.clone(), false)],
        _ => Vec::new(),
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

/// A left shift its fold and its zero laws left neutral, read as the product it is when the count is a literal: `shl(x, k) = 2ᵏ · x`, on `Nat` and on `Int` alike, where it holds below zero too.
///
/// The equation holds for every value on the unbounded carriers the type level folds, and the run time refuses a shift that leaves its carrier rather than truncating it, so no layer below can tell the two spellings apart by a value. `product` spells the carrier's own multiplication and the result goes back through the reducer, so the shift enters the sum normal form through that fold — distribution over a sum and the floor law included — and restates none of it.
///
/// **The coefficient is what this rule builds, and it is charged before it exists**, under the same [`shift_bound`] the closed fold pays: a count is a *value*, so `shl(x, 400000000)` is refused rather than allocated. A symbolic count declines, since `2ʸ` is no literal, and so does a zero one, which the zero law already answered.
pub(super) fn then_coefficient(
    reducer: &mut impl Reducer,
    result: Subterm,
    product: impl FnOnce(Natural, Term) -> Term,
) -> Result<Subterm, ReduceError> {
    let (Subterm::Intrinsic(Intrinsic::NatShl(value, count))
    | Subterm::Intrinsic(Intrinsic::IntShl(value, count))) = &result
    else {
        return Ok(result);
    };
    let Some(count) = count.as_nat() else {
        return Ok(result);
    };
    let Some(exponent) = count.as_literal() else {
        return Ok(result);
    };
    let Ok(amount) = u64::try_from(exponent) else {
        return Ok(result);
    };
    reducer.spend(shift_bound(1, Some(amount)))?;

    match Natural::one().shl_within(exponent, u64::MAX) {
        Some(coefficient) => Ok(Term::unwrap_or_clone(
            reducer.reduce_forced(product(coefficient, value.clone()))?,
        )),
        None => Ok(result),
    }
}

/// A left shift by a *symbolic* count, read as the product it is where [`then_coefficient`] reads a literal one: `shl(v, k) = v · 2ᵏ`, the power spelled as the atom `shl(1, k₀)` over the count's symbolic part and the count's literal floor peeled into the coefficient — `shl(v, k₀ + f) = 2ᶠ · v · shl(1, k₀)` — on `Nat` and on `Int`, where it holds below zero too.
///
/// **The floor is what makes a count inductive.** `shl(v, k + 1) = 2 · shl(v, k)` is this rule at `f = 1`, so a proof about a shift recurses on its count's successor as a proof about any `Nat` does, and the fast shift is the one reasoned about rather than a library power standing in for it. The value leaves the shift with its literal factors, so `shl(2, x)` and `shl(1, x + 1)` are one monomial. The coefficient is charged before it is built, under the same [`shift_bound`]; the atom itself — `shl(1, k₀)` over a floorless count — is its own normal form, which is what keeps the rule from re-entering.
pub(super) fn then_power(
    reducer: &mut impl Reducer,
    result: Subterm,
    one: Term,
    rebuild: fn(Term, Term) -> Intrinsic,
    product: impl FnOnce(Natural, Term, Term) -> Term,
) -> Result<Subterm, ReduceError> {
    let (Subterm::Intrinsic(Intrinsic::NatShl(value, count))
    | Subterm::Intrinsic(Intrinsic::IntShl(value, count))) = &result
    else {
        return Ok(result);
    };
    let (floor, inner) = Nat::decompose(count);
    if Nat::is_zero(&inner) || (floor.is_zero() && *value == one) {
        return Ok(result);
    }
    let Some(amount) = u64::try_from(&floor).ok() else {
        return Ok(result);
    };
    reducer.spend(shift_bound(1, Some(amount)))?;

    match Natural::one().shl_within(&floor, u64::MAX) {
        Some(coefficient) => {
            let power = Term::intrinsic(rebuild(one, inner));
            Ok(Term::unwrap_or_clone(reducer.reduce_forced(product(
                coefficient,
                value.clone(),
                power,
            ))?))
        }
        None => Ok(result),
    }
}

/// A right shift whose count carries a literal floor over a symbolic part, taken in two steps — `shr(v, k₀ + f) = shr(shr(v, k₀), f)` — on `Nat` and on `Int`, since a floored quotient by `2ᵏ⁰` and then by `2ᶠ` is the floored quotient by their product, below zero as above it. The twin of [`then_power`] for the one direction a coefficient cannot carry: `shr(v, k₀ + 1)` is `shr(v, k₀) / 2`, and spelling it so would build a division node, which carries a proof that its divisor is nonzero that a reducer may not invent.
pub(super) fn then_split_shift(
    reducer: &mut impl Reducer,
    result: Subterm,
    rebuild: fn(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let (Subterm::Intrinsic(Intrinsic::NatShr(value, count))
    | Subterm::Intrinsic(Intrinsic::IntShr(value, count))) = &result
    else {
        return Ok(result);
    };
    let (floor, inner) = Nat::decompose(count);
    if floor.is_zero() || Nat::is_zero(&inner) {
        return Ok(result);
    }
    let peeled = rebuild(
        Term::intrinsic(rebuild(value.clone(), inner)),
        Term::intrinsic(Intrinsic::Nat(Nat::new(floor))),
    );
    Ok(Term::unwrap_or_clone(
        reducer.reduce_forced(Term::intrinsic(peeled))?,
    ))
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
