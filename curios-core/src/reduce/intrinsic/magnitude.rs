//! The folds of `Int`'s `abs` and `sign`, and what leaves each.
//!
//! **An operand is read through what no substitution undoes.** A literal factor of every coefficient and the sign of the one that leads (`curios-algebra`'s `Scale`), and a power of a literal base at least two that every monomial holds, each leave an absolute value as a factor of it and a sign unchanged or flipped: `abs(8 * n)` is `8 * abs(n)`, `abs(+0 - n)` is `abs(n)`, `sign(shl(n, k))` is `sign(n)`. What is left shares no literal factor, leads with a coefficient above zero and holds no power in every monomial, so it is read once and reading it again changes nothing.
//!
//! **A combination is read from one end.** Its monomials are put in one order, by their factors' ranks as a product orders its factors, and the first of them leads: so `i - j` and `j - i` leave one operand under opposite signs, `abs` of either is one term, and `sign(+0 - n)` is `+0 - sign(n)` at every `n`, a difference among them.
//!
//! **What is left is a natural outright, or is compared, or stands.** `abs` of a non-negative combination of widened naturals is its preimage; `sign` of what the plain comparison holds above, at or below zero is `+1`, `+0` or `-1`; and anything else is the stuck node over what is left. Both compute from the operand as a term and read no arm, so `abs(i)` under a guard on `+0 <= i` is the node it was and a `/std` lemma converts it.
//!
//! The operand is normalized before it is read, a product of sums distributed by name, so one value has one reading.

use {
    super::{compare_int, image_bound},
    crate::{
        Cost, Intrinsic, Nat, ReduceError, Reducer, Subterm, Term, int_footprint, int_from_linear,
        int_has_stuck_product, int_linear, int_negate, int_normalize, int_preimage, int_terms,
        project_erased_universes,
    },
    curios_algebra::{Comparison, Scale, signed_cofactor},
    curios_num::Integer,
};

/// A reduced `Int` as `abs` and `sign` read it.
struct Reading {
    /// The literal factor every coefficient shares, and whether the leading one is below zero.
    scale: Scale,
    /// The powers every monomial holds, each as the `Nat` it widens: a power is at least one, so it is a factor of the absolute value and leaves the sign.
    powers: Vec<Term>,
    /// The operand over its scale and its powers.
    rest: Term,
}

impl Reading {
    /// Whether the operand is read as it stands.
    fn is_plain(&self) -> bool {
        self.scale.is_unit() && self.powers.is_empty()
    }
}

/// The `Nat` a factor widens, where the factor is a widened power of a literal base at least two: at least one whatever its exponent, and a product of such powers and a literal whatever is put for its exponent, which a power of a symbolic base is not.
fn power(factor: &Term) -> Option<&Term> {
    let Subterm::Intrinsic(Intrinsic::NatToInt(natural)) = &**factor else {
        return None;
    };
    let Subterm::Intrinsic(Intrinsic::NatPow(base, _)) = &**natural else {
        return None;
    };
    (base.as_nat().and_then(|value| value.to_natural()))
        .is_some_and(|value| !value.is_zero() && !value.is_one())
        .then_some(natural)
}

/// `operand` read: `None` where it is zero, which has no scale. The walk over its monomials is charged before it is made.
fn read(reducer: &mut impl Reducer, operand: &Term) -> Result<Option<Reading>, ReduceError> {
    let (constant, summands) = int_terms(operand);
    reducer.spend(Cost::collection(summands.len() as u64 + 1))?;
    // One order for a combination's monomials, whichever end it was written from: by each factor's rank, as a product orders its factors, two monomials of one rank keeping the order they came in. The first leads, and the constant leads a combination that has none.
    let mut monomials = int_linear(summands);
    monomials.sort_by_cached_key(|(_, factors)| {
        factors
            .iter()
            .map(|factor| project_erased_universes(factor).structural_hash())
            .collect::<Vec<_>>()
    });

    let coefficients = monomials
        .iter()
        .map(|(coefficient, _)| coefficient.clone())
        .chain((!constant.is_zero()).then(|| constant.clone()))
        .collect::<Vec<_>>();
    let Some(scale) = Scale::of(&coefficients) else {
        return Ok(None);
    };

    // A power leaves only where every monomial holds it and no constant stands beside them: the powers the first monomial holds, less each one another does not.
    let mut powers: Vec<Term> = match (constant.is_zero(), monomials.first()) {
        (true, Some((_, factors))) => factors.iter().filter_map(power).cloned().collect(),
        _ => Vec::new(),
    };
    for (_, factors) in monomials.iter().skip(1) {
        let mut held: Vec<&Term> = factors.iter().filter_map(power).collect();
        powers.retain(
            |shared| match held.iter().position(|other| *other == shared) {
                Some(at) => {
                    held.swap_remove(at);
                    true
                }
                None => false,
            },
        );
    }

    let divisor = scale.divisor();
    let divided = |coefficient: &Integer| {
        signed_cofactor(coefficient, &divisor).expect("the scale divides every coefficient")
    };
    let rest = monomials
        .into_iter()
        .map(|(coefficient, mut factors)| {
            for shared in &powers {
                let at = factors
                    .iter()
                    .position(|factor| power(factor) == Some(shared))
                    .expect("every monomial holds a shared power");
                factors.remove(at);
            }
            (divided(&coefficient), factors)
        })
        .collect();
    Ok(Some(Reading {
        scale,
        powers,
        rest: int_from_linear(divided(&constant), rest),
    }))
}

/// `operand` reduced and normalized, as both folds read it.
fn operand_of(reducer: &mut impl Reducer, operand: &Term) -> Result<Term, ReduceError> {
    let operand = reducer.reduce_forced(operand.clone())?;
    match int_has_stuck_product(&operand) {
        true => int_normalize(reducer, operand),
        false => Ok(operand),
    }
}

/// Reduce `Int/abs`: a literal's magnitude; otherwise the operand's literal factor times its powers times the magnitude of what is left, which is its preimage where it is a non-negative combination of widened naturals and the stuck node over it otherwise.
pub(super) fn reduce_int_abs(
    reducer: &mut impl Reducer,
    operand: &Term,
) -> Result<Subterm, ReduceError> {
    let operand = operand_of(reducer, operand)?;
    if let Some(value) = operand.as_int() {
        return Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::new(
            value.magnitude(),
        ))));
    }
    let Some(reading) = read(reducer, &operand)? else {
        return Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)));
    };

    let plain = reading.is_plain();
    // What is left is written back as its preimage where it has one, a monomial for each of its own.
    reducer.spend(image_bound(&int_footprint(&reading.rest)))?;
    let left = match int_preimage(&reading.rest) {
        Some(natural) => natural,
        None if plain => return Ok(Subterm::Intrinsic(Intrinsic::IntAbs(reading.rest))),
        None => Term::intrinsic(Intrinsic::IntAbs(reading.rest)),
    };
    if plain {
        return Ok(Term::unwrap_or_clone(left));
    }

    // The factor, what is left and each power, as one `Nat` product in its normal form: charged as the nodes it is, and reduced, which folds a literal and merges like powers.
    reducer.spend(Cost::term(2).saturating_mul(reading.powers.len() as u64 + 1))?;
    let scaled = Term::intrinsic(Intrinsic::nat_mul(
        Term::intrinsic(Intrinsic::Nat(Nat::new(reading.scale.factor))),
        left,
    ));
    let product = reading.powers.into_iter().fold(scaled, |product, power| {
        Term::intrinsic(Intrinsic::nat_mul(product, power))
    });
    Ok(Term::unwrap_or_clone(reducer.reduce_forced(product)?))
}

/// Reduce `Int/sign`: a literal's sign; otherwise the sign of what is left of the operand, flipped where its leading coefficient was below zero —`+1`, `+0` or `-1` where the plain comparison holds what is left above, at or below zero, and the stuck node over it otherwise.
pub(super) fn reduce_int_sign(
    reducer: &mut impl Reducer,
    operand: &Term,
) -> Result<Subterm, ReduceError> {
    let operand = operand_of(reducer, operand)?;
    if let Some(value) = operand.as_int() {
        return Ok(Subterm::Intrinsic(Intrinsic::Int(value.sign())));
    }
    let Some(reading) = read(reducer, &operand)? else {
        return Ok(Subterm::Intrinsic(Intrinsic::Int(Integer::from(0i32))));
    };

    let zero = Term::intrinsic(Intrinsic::Int(Integer::from(0i32)));
    let (verdict, _, _) = compare_int(reducer, reading.rest.clone(), zero)?;
    let decided = match verdict {
        Comparison::Gt => Some(1i32),
        Comparison::Eq => Some(0),
        Comparison::Lt => Some(-1),
        _ => None,
    };
    let flipped = reading.scale.negated;
    Ok(match decided {
        Some(sign) => Subterm::Intrinsic(Intrinsic::Int(Integer::from(match flipped {
            true => -sign,
            false => sign,
        }))),
        None if reading.is_plain() => Subterm::Intrinsic(Intrinsic::IntSign(reading.rest)),
        None => {
            reducer.spend(Cost::term(2))?;
            let left = Term::intrinsic(Intrinsic::IntSign(reading.rest));
            match flipped {
                true => Term::unwrap_or_clone(int_negate(&left)),
                false => Term::unwrap_or_clone(left),
            }
        }
    })
}

/// A stuck magnitude widened back, where the plain comparison decides its operand's sign: `to_int(abs(x))` is `x` where `x` is held at least zero and `-x` where at most. `None` for any other operand of the widening, and where the sign is not decided.
pub(super) fn widened_magnitude(
    reducer: &mut impl Reducer,
    natural: &Term,
) -> Result<Option<Term>, ReduceError> {
    let Subterm::Intrinsic(Intrinsic::IntAbs(operand)) = &**natural else {
        return Ok(None);
    };
    let zero = Term::intrinsic(Intrinsic::Int(Integer::from(0i32)));
    let (verdict, _, _) = compare_int(reducer, operand.clone(), zero)?;
    Ok(match verdict {
        Comparison::Gt | Comparison::Ge | Comparison::Eq => Some(operand.clone()),
        Comparison::Lt | Comparison::Le => Some(int_negate(operand)),
        _ => None,
    })
}
