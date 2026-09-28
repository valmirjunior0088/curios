//! The `Nat` folds that need more than an operand pair: division, and the bound and split that make it decide under symbols.
//!
//! `Nat` division is the one arithmetic family whose fold reaches past literals: [`nat_bound`] states how large a shape can be, and [`nat_euclid_split`] uses that to peel a quotient off a sum whose remainder cannot reach the divisor. The bound must never under-report — the split turns it into a definitional equation.

use {
    super::*,
    crate::{Declaration, Intrinsic, Nat, ReduceError, Reducer, Subterm, Term},
    curios_algebra::{
        Carrier, Divided, FloorSplit, Half, Observed, Operation, cofactor, euclid_split, floor_law,
        power_of_two,
    },
};

/// Which half of a Euclidean division a fold computes. One enum rather than the pair of closures the other families take: the symbolic laws below build the quotient and the remainder out of the *same* split, so the two halves cannot be parameterized independently.
#[derive(Clone, Copy)]
pub(super) enum Euclid {
    Quotient,
    Remainder,
}

impl Euclid {
    /// The half of Euclid's division this is, as `curios-algebra` names it.
    pub(super) fn half(self) -> Half {
        match self {
            Euclid::Quotient => Half::Quotient,
            Euclid::Remainder => Half::Remainder,
        }
    }

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

/// A statically known upper bound on every value a reduced term can take, or `None` where it has none: a floor over a bounded part, or an operation whose declared meaning bounds it from what its operands are observed to be.
///
/// What bounds what is `curios-algebra`'s `Operation::upper_bound`, stated once per operation with the criterion that admits it; what is decided here is which intrinsic is which operation (`Intrinsic::algebra`), and the observations the bound reads — an operand's own bound, or its literal value — each taken only where the operation reads it. Every arm is unconditional, which is what lets the callers below turn a bound into a definitional equation.
///
/// **`NatShl` has no bound**, for the reason `Operation::upper_bound` records: what reaches here as a `NatShl` is a shift by a *symbolic* count, which no operand bounds, since a literal count is [`then_coefficient`]'s and arrives as a `NatMul` with its coefficient already charged.
///
/// An over-report only withholds the rule; an *under*-report is a false definitional equation, which is the direction `bound_upper_bounds_every_closed_instantiation` asserts. That gate is a hand-written block per shape rather than an enumeration, so a bound added to the algebra owes it one or it passes while checking nothing. A wrong bound is a false equation and not a wrong value: see `documentation/soundness/per-term-rules/the-bounds-oracle-and-the-division-family.md`.
pub(super) fn nat_bound(term: &Term) -> Option<Natural> {
    let Subterm::Intrinsic(intrinsic) = &**term else {
        return None;
    };
    match intrinsic {
        Intrinsic::Nat(Nat::Zero) => Some(Natural::zero()),
        Intrinsic::Nat(Nat::Succ(floor, inner)) => Operation::Sum.upper_bound(&[
            Observed {
                bound: Some(floor.clone()),
                literal: None,
            },
            Observed {
                bound: nat_bound(inner),
                literal: None,
            },
        ]),
        _ => match intrinsic.algebra() {
            Declaration::Operation {
                carrier: Carrier::Natural,
                operation,
                operands,
            } => {
                let (bounds, literals) = operation.bound_reads();
                let observed = operands
                    .as_slice()
                    .iter()
                    .enumerate()
                    .map(|(at, operand)| Observed {
                        bound: bounds.contains(&at).then(|| nat_bound(operand)).flatten(),
                        literal: literals
                            .contains(&at)
                            .then(|| operand.as_nat().and_then(|value| value.to_natural()))
                            .flatten(),
                    })
                    .collect::<Vec<_>>();
                operation.upper_bound(&observed)
            }
            _ => None,
        },
    }
}

/// The operands a reduced term never exceeds, as terms, each with whether the bound is strict — [`nat_bound`]'s criterion read at the operands instead of at a literal, as `curios-algebra`'s `Operation::dominators` states it per operation.
///
/// Every pair is unconditional, as the literal oracle's arms are, and for the same reason it may be turned into a verdict: an under-report here is a false definitional equation. `dominators_upper_bound_every_closed_instantiation` holds each listed pair over values, block per shape.
pub(super) fn nat_dominators(term: &Term) -> Vec<(Term, bool)> {
    match &**term {
        Subterm::Intrinsic(intrinsic) => match intrinsic.algebra() {
            Declaration::Operation {
                carrier: Carrier::Natural,
                operation,
                operands,
            } => operation
                .dominators()
                .iter()
                .map(|&(at, strict)| (operands.as_slice()[at].clone(), strict))
                .collect(),
            _ => Vec::new(),
        },
        _ => Vec::new(),
    }
}

/// A reduced summand read as `coefficient · factor` with a *literal* coefficient, or `None` for a summand that is not such a product — the reading [`Nat::literal_factor`] takes, minus its unit default, for the callers that need to know whether a literal was there.
fn nat_literal_factor(summand: &Term) -> Option<(Natural, Term)> {
    matches!(&**summand, Subterm::Intrinsic(Intrinsic::NatMul(..)))
        .then(|| Nat::literal_factor(summand))
        .filter(|(_, factor)| factor != summand)
}

/// Split a reduced dividend against a literal divisor into `(quotient, remainder)`, or `None` where the division is not forced: `curios-algebra`'s `euclid_split`, over each summand's literal coefficient and, where the divisor does not divide it, its bound. That is what makes `(256·x + Byte/to_nat(b)) / 256` reduce to `x`.
pub(super) fn nat_euclid_split(dividend: &Term, divisor: &Natural) -> Option<(Term, Term)> {
    let (floor, inner) = Nat::decompose(dividend);
    let mut quotient = Vec::new();
    let mut residual = Vec::new();
    let mut bounds = Vec::new();
    for summand in Nat::summands(&inner) {
        let multiple = nat_literal_factor(&summand).and_then(|(coefficient, factor)| {
            cofactor(&coefficient, divisor).map(|cofactor| (cofactor, factor))
        });
        match multiple {
            Some((cofactor, factor)) => quotient.push(Nat::scaled(cofactor, factor)),
            None => {
                bounds.push(nat_bound(&summand)?);
                residual.push(summand);
            }
        }
    }

    let FloorSplit { whole, rest } = euclid_split(&floor, divisor, bounds)?;
    Some((
        Nat::sum_over_floor(quotient, whole),
        Nat::sum_over_floor(residual, rest),
    ))
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
    let half = euclid.half();
    let divided = match () {
        _ if Nat::is_zero(&left) => Some(half.of_zero()),
        _ if divisor.as_ref().is_some_and(Natural::is_one) => Some(half.by_one()),
        _ if left == right => Some(half.by_itself()),
        _ => None,
    };
    if let Some(divided) = divided {
        return Ok(match divided {
            Divided::Zero => Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)),
            Divided::Dividend => Term::unwrap_or_clone(left),
            Divided::One => Subterm::Intrinsic(Intrinsic::Nat(Nat::new(Natural::one()))),
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
        if let Some(FloorSplit { whole, rest }) = floor_law(&floor, divisor) {
            let peeled = Term::intrinsic(euclid.rebuild(
                Nat::rebuild(rest, inner),
                right.clone(),
                non_zero.clone(),
            ));

            return Ok(Term::unwrap_or_clone(match half {
                Half::Quotient => Nat::rebuild(whole, peeled),
                Half::Remainder => peeled,
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

    match power_of_two(exponent) {
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

    match power_of_two(&floor) {
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
