//! A stuck `Flt` sign operation, written the one way its value is.
//!
//! `neg`, `abs` and `copysign` each move a value's sign bit and read no other, so a composition of them is a magnitude under a sign ([`SignMagnitude`]) and is written in that reading's one form: `neg(neg(x))` is `x`, `abs(neg(x))` is `abs(x)`, `neg(copysign(x, y))` is `copysign(x, neg(y))`. A literal is its absolute value under the bit it has, so `copysign(x, 1.0)` is `abs(x)` and `copysign(-2.0, y)` is `copysign(2.0, y)`.
//!
//! **The values are compared by identity**, as every fold compares its atoms, and which two are one is the fold's checker's to say: the three operations are among those whose atoms a checker classes before the fold is taken again (`folds_over_atoms`).

use {
    super::super::{ReduceError, Reducer},
    crate::{Cost, Intrinsic, Subterm, Term},
    curios_algebra::{SignBit, SignForm, SignMagnitude},
    curios_utilities::recurse,
};

/// `folded` in its sign reading's one form where it is a stuck sign operation, and `folded` otherwise. Each node of the form is charged before it is built.
pub(super) fn then_flt_sign_form(
    reducer: &mut impl Reducer,
    folded: Subterm,
) -> Result<Subterm, ReduceError> {
    let Subterm::Intrinsic(intrinsic) = &folded else {
        return Ok(folded);
    };
    let Some(reading) = flt_sign_applied(intrinsic) else {
        return Ok(folded);
    };
    let written = match reading.form() {
        SignForm::Bare(value) => value,
        SignForm::Negated(value) => flt_negated(reducer, value)?,
        SignForm::Absolute(value) => flt_absolute(reducer, value)?,
        SignForm::NegatedAbsolute(value) => {
            let absolute = flt_absolute(reducer, value)?;
            flt_negated(reducer, absolute)?
        }
        SignForm::Copied { magnitude, sign } => flt_copied(reducer, magnitude, sign)?,
        SignForm::CopiedNegated { magnitude, sign } => {
            let negated = flt_negated(reducer, sign)?;
            flt_copied(reducer, magnitude, negated)?
        }
    };
    Ok(Term::unwrap_or_clone(written))
}

/// What a sign operation makes of its operands' readings, and `None` for any other intrinsic.
fn flt_sign_applied(intrinsic: &Intrinsic) -> Option<SignMagnitude<Term>> {
    Some(match intrinsic {
        Intrinsic::FltNeg(inner) => flt_sign_magnitude(inner).negated(),
        Intrinsic::FltAbs(inner) => flt_sign_magnitude(inner).absolute(),
        Intrinsic::FltCopysign(magnitude, sign) => {
            flt_sign_magnitude(magnitude).signed_as(flt_sign_magnitude(sign))
        }
        _ => return None,
    })
}

/// A reduced `Flt` term as a magnitude under a sign: a literal is its absolute value under the bit it has, a sign operation is what it makes of its operands, and any other term is a value under its own sign.
fn flt_sign_magnitude(term: &Term) -> SignMagnitude<Term> {
    match &**term {
        Subterm::Intrinsic(Intrinsic::Flt(value)) => {
            let magnitude = value.abs();
            SignMagnitude {
                sign: match magnitude == *value {
                    true => SignBit::Clear,
                    false => SignBit::Set,
                },
                magnitude: Term::intrinsic(Intrinsic::Flt(magnitude)),
            }
        }
        Subterm::Intrinsic(intrinsic) => recurse(|| flt_sign_applied(intrinsic))
            .unwrap_or_else(|| SignMagnitude::of(term.clone())),
        _ => SignMagnitude::of(term.clone()),
    }
}

/// `neg(operand)`, computed where the operand is a literal.
fn flt_negated(reducer: &mut impl Reducer, operand: Term) -> Result<Term, ReduceError> {
    reducer.spend(Cost::term(1))?;
    Ok(Term::intrinsic(match operand.as_flt() {
        Some(value) => Intrinsic::Flt(-value),
        None => Intrinsic::FltNeg(operand),
    }))
}

/// `abs(operand)`, computed where the operand is a literal.
fn flt_absolute(reducer: &mut impl Reducer, operand: Term) -> Result<Term, ReduceError> {
    reducer.spend(Cost::term(1))?;
    Ok(Term::intrinsic(match operand.as_flt() {
        Some(value) => Intrinsic::Flt(value.abs()),
        None => Intrinsic::FltAbs(operand),
    }))
}

/// `copysign(magnitude, sign)` over a sign that is no literal, which a reading never leaves one.
fn flt_copied(
    reducer: &mut impl Reducer,
    magnitude: Term,
    sign: Term,
) -> Result<Term, ReduceError> {
    reducer.spend(Cost::term(2))?;
    Ok(Term::intrinsic(Intrinsic::FltCopysign(magnitude, sign)))
}
