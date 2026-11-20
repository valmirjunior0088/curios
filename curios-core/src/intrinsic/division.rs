//! A `Nat` division read through every spelling it has: a quotient or a remainder beside the proof that its divisor is nonzero, a right shift by `k`, the quotient by `2ᵏ`, and a mask of the `k` low bits, `and(x, 2ᵏ - 1)`, the remainder by `2ᵏ` — whose divisor is positive at every count and needs no proof.
//!
//! Every reader of a division — the folds, Euclid's recombination, the elaborator's procedure — takes a spelling through this view, so a law of the quotient is a law of the shift, and one of the remainder a law of the mask, by being read here and through no rule of their own.

use {
    super::Intrinsic,
    crate::{Nat, Term},
    curios_algebra::Half,
    curios_num::Natural,
};

/// One half of a `Nat` division, in the spelling an intrinsic writes it in.
#[derive(Clone)]
pub enum Division {
    /// `x / d` or `x % d`, beside the proof that `d` is nonzero.
    Proved {
        dividend: Term,
        divisor: Term,
        non_zero: Term,
        half: Half,
    },
    /// `shr(x, k)`, the quotient of `x` by `2ᵏ`.
    Shifted { value: Term, count: Term },
    /// `and(x, 2ᵏ - 1)`, the remainder of `x` by `2ᵏ`, `k` at least one, read off the literal mask: `width` is `k`, `divisor` is `2ᵏ`, and `mask_first` is whether the mask is the left operand.
    Masked {
        value: Term,
        width: u64,
        divisor: Natural,
        mask_first: bool,
    },
}

impl Division {
    pub fn dividend(&self) -> &Term {
        match self {
            Division::Proved { dividend, .. } => dividend,
            Division::Shifted { value, .. } | Division::Masked { value, .. } => value,
        }
    }

    pub fn half(&self) -> Half {
        match self {
            Division::Proved { half, .. } => *half,
            Division::Shifted { .. } => Half::Quotient,
            Division::Masked { .. } => Half::Remainder,
        }
    }

    /// The count the divisor is two to, where it is written as one: a shift's as written, a mask's its literal width.
    pub fn count(&self) -> Option<Term> {
        match self {
            Division::Proved { .. } => None,
            Division::Shifted { count, .. } => Some(count.clone()),
            Division::Masked { width, .. } => Some(literal(Natural::from(*width))),
        }
    }

    /// The divisor, as a term: a proved one as written, a shift's as `pow(2, k)`, and a mask's as the literal `2ᵏ`.
    pub fn divisor(&self) -> Term {
        match self {
            Division::Proved { divisor, .. } => divisor.clone(),
            Division::Shifted { count, .. } => Term::intrinsic(Intrinsic::NatPow(
                literal(Natural::from(2u32)),
                count.clone(),
            )),
            Division::Masked { divisor, .. } => literal(divisor.clone()),
        }
    }

    /// The quotient of this division, in the spelling it pairs with: the term `/` writes beside `%`, and the shift by `k` beside a mask of `k` bits; a quotient is its own.
    pub fn quotient(&self) -> Term {
        match self {
            Division::Proved {
                dividend,
                divisor,
                non_zero,
                ..
            } => Term::intrinsic(Intrinsic::NatDiv {
                dividend: dividend.clone(),
                divisor: divisor.clone(),
                non_zero: non_zero.clone(),
            }),
            Division::Shifted { value, count } => {
                Term::intrinsic(Intrinsic::NatShr(value.clone(), count.clone()))
            }
            Division::Masked { value, width, .. } => Term::intrinsic(Intrinsic::NatShr(
                value.clone(),
                literal(Natural::from(*width)),
            )),
        }
    }

    /// The same half of the same division of `dividend`, spelled as this one is.
    pub fn of(&self, dividend: Term) -> Intrinsic {
        match self {
            Division::Proved {
                divisor,
                non_zero,
                half: Half::Quotient,
                ..
            } => Intrinsic::NatDiv {
                dividend,
                divisor: divisor.clone(),
                non_zero: non_zero.clone(),
            },
            Division::Proved {
                divisor,
                non_zero,
                half: Half::Remainder,
                ..
            } => Intrinsic::NatRem {
                dividend,
                divisor: divisor.clone(),
                non_zero: non_zero.clone(),
            },
            Division::Shifted { count, .. } => Intrinsic::NatShr(dividend, count.clone()),
            Division::Masked {
                divisor,
                mask_first,
                ..
            } => {
                let mask = literal(divisor.clone() - Natural::one());
                match mask_first {
                    true => Intrinsic::NatAnd(mask, dividend),
                    false => Intrinsic::NatAnd(dividend, mask),
                }
            }
        }
    }
}

impl Intrinsic {
    /// The half of a `Nat` division this intrinsic is, in whichever spelling it is written; `None` for any other intrinsic.
    pub fn nat_division(&self) -> Option<Division> {
        let proved = |dividend: &Term, divisor: &Term, non_zero: &Term, half| Division::Proved {
            dividend: dividend.clone(),
            divisor: divisor.clone(),
            non_zero: non_zero.clone(),
            half,
        };
        match self {
            Intrinsic::NatDiv {
                dividend,
                divisor,
                non_zero,
            } => Some(proved(dividend, divisor, non_zero, Half::Quotient)),
            Intrinsic::NatRem {
                dividend,
                divisor,
                non_zero,
            } => Some(proved(dividend, divisor, non_zero, Half::Remainder)),
            Intrinsic::NatShr(value, count) => Some(Division::Shifted {
                value: value.clone(),
                count: count.clone(),
            }),
            Intrinsic::NatAnd(left, right) => {
                let (value, (width, divisor), mask_first) =
                    match (mask_width(right), mask_width(left)) {
                        (Some(read), _) => (left, read, false),
                        (None, Some(read)) => (right, read, true),
                        (None, None) => return None,
                    };
                Some(Division::Masked {
                    value: value.clone(),
                    width,
                    divisor,
                    mask_first,
                })
            }
            _ => None,
        }
    }
}

fn literal(value: Natural) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(value)))
}

/// The `k` a literal mask `2ᵏ - 1` keeps the low bits of, `k` at least one, with `2ᵏ`: a mask of none is zero, which `and`'s own identities answer.
fn mask_width(mask: &Term) -> Option<(u64, Natural)> {
    let divisor = mask.as_nat()?.to_natural()? + Natural::one();
    let width = divisor
        .power_of_two_exponent()
        .filter(|width| *width >= 1)?;
    Some((width, divisor))
}
