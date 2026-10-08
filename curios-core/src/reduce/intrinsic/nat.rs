//! The `Nat` folds that need more than an operand pair: division, and the bound and split that make it decide under symbols.
//!
//! `Nat` division is the one arithmetic family whose fold reaches past literals: [`nat_bound`] states how large a shape can be, and [`nat_euclid_split`] uses that to peel a quotient off a sum whose remainder cannot reach the divisor. The bound must never under-report — the split turns it into a definitional equation.

use {
    super::{operand_bound, shift_bound},
    crate::{Cost, Declaration, Intrinsic, Nat, ReduceError, Reducer, Subterm, Term},
    curios_algebra::{
        Carrier, Divided, FloorSplit, Half, Observed, Operation, cofactor, euclid_split, floor_law,
        power_of_two,
    },
    curios_num::Natural,
    curios_utilities::recurse,
    std::iter,
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
/// An over-report only withholds the rule; an *under*-report is a false definitional equation, which is the direction `bound_upper_bounds_every_closed_instantiation` asserts. That gate is a hand-written block per shape rather than an enumeration, so a bound added to the algebra owes it one or it passes while checking nothing. A wrong bound is a false equation and not a wrong value.
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

/// A statically known lower bound on every value a reduced term takes: its floor over what its symbolic part is at least, and an operation's from what its operands are at least (`curios-algebra`'s `Operation::lower_bound`). Zero where nothing is known, which is no claim.
///
/// The mirror of [`nat_bound`], and held the mirrored way: an *over*-report is a false definitional equation, the direction `least_lower_bounds_every_closed_instantiation` asserts. What it buys is positivity: a power `pow(2, k)` is at least one, so a sum or a product over one is, and `0 < shl(x + 1, k)` reduces.
pub(super) fn nat_least(term: &Term) -> Natural {
    let Subterm::Intrinsic(intrinsic) = &**term else {
        return Natural::zero();
    };
    match intrinsic {
        Intrinsic::Nat(Nat::Zero) => Natural::zero(),
        Intrinsic::Nat(Nat::Succ(floor, inner)) => floor.clone() + &recurse(|| nat_least(inner)),
        _ => match intrinsic.algebra() {
            Declaration::Operation {
                carrier: Carrier::Natural,
                operation,
                operands,
            } => {
                let least = operands
                    .as_slice()
                    .iter()
                    .map(|operand| recurse(|| nat_least(operand)))
                    .collect::<Vec<_>>();
                operation.lower_bound(&least)
            }
            _ => Natural::zero(),
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
    let extent = right.extent().or_else(|| left.extent());
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    let divisor = right.as_nat().and_then(|divisor| divisor.to_natural());
    if divisor.as_ref().is_some_and(Natural::is_zero) {
        return Err(ReduceError::DivisionByZero {
            kind: euclid.kind(),
            extent,
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

/// A symbolic exponent as the monomials it is a sum of, each with its literal coefficient: `a + 2 · b` as `a` once and `b` twice. The exponent is normalized first, a product of sums in it distributed by name, so two spellings of one exponent split alike. Empty where the exponent has no symbolic part.
fn exponent_monomials(
    reducer: &mut impl Reducer,
    inner: &Term,
) -> Result<Vec<(Natural, Term)>, ReduceError> {
    if Nat::is_zero(inner) {
        return Ok(Vec::new());
    }
    let normalized = Nat::normalize(reducer, inner.clone())?;
    Ok(Nat::linear(Nat::summands(&normalized)))
}

/// One `power` to each monomial of an exponent, repeated as many times as the monomial's literal coefficient, every factor charged before it is built. `None` where a coefficient is past what a machine counts.
fn repeated_powers(
    reducer: &mut impl Reducer,
    monomials: Vec<(Natural, Term)>,
    power: impl Fn(Term) -> Term,
) -> Result<Option<Vec<Term>>, ReduceError> {
    let factors = monomials
        .iter()
        .fold(Natural::zero(), |factors, (repeated, _)| factors + repeated);
    reducer.spend(Cost::collection(
        u64::try_from(&factors).unwrap_or(u64::MAX),
    ))?;
    let mut powers = Vec::new();
    for (repeated, monomial) in monomials {
        let Ok(repeated) = usize::try_from(&repeated) else {
            return Ok(None);
        };
        powers.extend(iter::repeat_n(power(monomial), repeated));
    }
    Ok(Some(powers))
}

/// `Nat/pow`, its exponent a morphism from `(ℕ, +)` into `(ℕ, ·)` at a fixed base: `pow(x, a + 2 · b + f) = xᶠ · pow(x, a) · pow(x, b) · pow(x, b)`. The exponent's literal floor is so many factors of the base — one literal, charged by the bits it holds before it is built, where the base is one — and each monomial of its symbolic part is a power of its own, the atom `pow(x, m)`, repeated as many times as the monomial's literal coefficient. So `pow(x, 0)` is `1`, `pow(x, 1)` is `x`, a closed power folds, and a proof about a power recurses on its exponent's successor as a proof about any `Nat` does.
///
/// **A base of one answers one whatever the exponent, and a base of zero answers zero over a floor above zero**, neither reading how large the exponent is, so the answer is no host's: `pow(0, a + 1)` is zero, and `pow(0, a)` is the atom it is, one at `a = 0`.
///
/// **Nothing is read of the base but a literal.** A power of a power and a power of a product stay atoms, their laws lemmas: beside `pow(x, 2) = x · x` the first gives the second, which at two literals factors a base, and no finite set of identities is complete for exponentiation over the naturals. A product in the exponent is one monomial and one atom, never a product of powers.
///
/// **The exponent is normalized first** ([`exponent_monomials`]), and the atom — a base that is not one over one bare monomial — is its own normal form, which is what keeps the rule from re-entering. An exponent past what a machine counts leaves the node standing.
pub(super) fn reduce_nat_pow(
    reducer: &mut impl Reducer,
    base: &Term,
    exponent: &Term,
) -> Result<Subterm, ReduceError> {
    let base = reducer.reduce_forced(base.clone())?;
    let exponent = reducer.reduce_forced(exponent.clone())?;
    let literal = base.as_nat().and_then(|base| base.to_natural());
    if literal.as_ref().is_some_and(Natural::is_one) {
        return Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))));
    }

    let (floor, inner) = Nat::decompose(&exponent);
    // Zero to a floor above zero is zero, at a floor past what a machine counts as at one.
    if literal.as_ref().is_some_and(Natural::is_zero) && !floor.is_zero() {
        return Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)));
    }
    let monomials = exponent_monomials(reducer, &inner)?;
    let atom = matches!(monomials.as_slice(), [(repeated, _)] if repeated.is_one());
    let stuck = |base: Term, exponent: Term| Subterm::Intrinsic(Intrinsic::NatPow(base, exponent));
    if atom && floor.is_zero() {
        return Ok(stuck(base, exponent));
    }
    let Ok(amount) = u64::try_from(&floor) else {
        return Ok(stuck(base, exponent));
    };

    // The base to the floor: a literal's power, or so many factors of a symbolic one.
    let (coefficient, factors) = match &literal {
        Some(value) => {
            reducer.spend(Cost::big_number(value.bits().saturating_mul(amount)))?;
            match value.pow_within(&floor, u64::MAX) {
                Some(power) => (power, 0),
                None => return Ok(stuck(base, exponent)),
            }
        }
        None => {
            reducer.spend(Cost::collection(amount))?;
            match usize::try_from(amount) {
                Ok(factors) => (Natural::one(), factors),
                Err(_) => return Ok(stuck(base, exponent)),
            }
        }
    };
    let power = |monomial| Term::intrinsic(Intrinsic::NatPow(base.clone(), monomial));
    let Some(powers) = repeated_powers(reducer, monomials, power)? else {
        return Ok(stuck(base, exponent));
    };

    let product = iter::repeat_n(base.clone(), factors).chain(powers).fold(
        Term::intrinsic(Intrinsic::Nat(Nat::new(coefficient))),
        |product, factor| Term::intrinsic(Intrinsic::nat_mul(product, factor)),
    );
    Ok(Term::unwrap_or_clone(reducer.reduce_forced(product)?))
}

/// A left shift by a *symbolic* count, read as the product it is where [`then_coefficient`] reads a literal one: `shl(v, k) = v · 2ᵏ`, the power [`reduce_nat_pow`] folds. The count's literal floor is peeled into the coefficient and each monomial of its symbolic part is a power of two of its own, the atom `pow(2, m)`, repeated as many times as the monomial's literal coefficient: `shl(v, a + 2 · b + f) = 2ᶠ · v · pow(2, a) · pow(2, b) · pow(2, b)`, on `Nat` and on `Int`, where it holds below zero too.
///
/// **The floor is what makes a count inductive, and the split what makes it additive.** `shl(v, k + 1) = 2 · shl(v, k)` is this rule at `f = 1`, so a proof about a shift recurses on its count's successor as a proof about any `Nat` does; and `shl(shl(v, a), b) = shl(v, a + b)` holds at symbolic counts because both are `v` times the same two atoms. The value leaves the shift with its literal factors, so `shl(2, x)` and `shl(1, x + 1)` are one monomial, and `shl(1, x)` is `pow(2, x)`, one atom for the two spellings. A product in the count is one monomial and one atom, never a product of powers: `shl(1, x · y)` is not `shl(1, x) · shl(1, y)`.
///
/// **The count is normalized first** ([`exponent_monomials`]). `power` spells the atom over one monomial at the carrier the shift is at — at `Int` the widened `Nat` atom, which is what lets a comparison over it pull back to `Nat` and read it positive. The coefficient is charged before it is built, under the same [`shift_bound`], and every factor before the product is, so a literal coefficient no budget covers is refused. What the rule builds holds no shift, so it does not re-enter.
pub(super) fn then_power(
    reducer: &mut impl Reducer,
    result: Subterm,
    power: impl Fn(Term) -> Term,
    product: impl FnOnce(Natural, Term, Vec<Term>) -> Term,
) -> Result<Subterm, ReduceError> {
    let (Subterm::Intrinsic(Intrinsic::NatShl(value, count))
    | Subterm::Intrinsic(Intrinsic::IntShl(value, count))) = &result
    else {
        return Ok(result);
    };
    let (floor, inner) = Nat::decompose(count);
    if Nat::is_zero(&inner) {
        return Ok(result);
    }
    let exponents = exponent_monomials(reducer, &inner)?;
    let Some(amount) = u64::try_from(&floor).ok() else {
        return Ok(result);
    };
    reducer.spend(shift_bound(1, Some(amount)))?;
    let Some(coefficient) = power_of_two(&floor) else {
        return Ok(result);
    };
    let Some(powers) = repeated_powers(reducer, exponents, power)? else {
        return Ok(result);
    };
    Ok(Term::unwrap_or_clone(reducer.reduce_forced(product(
        coefficient,
        value.clone(),
        powers,
    ))?))
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
