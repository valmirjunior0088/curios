//! A `Nat` division read through every spelling it has: a quotient or a remainder beside the proof that its divisor is nonzero, and a right shift by `k`, the quotient by `2ᵏ`, whose divisor is positive at every count and needs no proof.
//!
//! Every reader of a division — the folds, Euclid's recombination, the elaborator's procedure — takes a spelling through this view, so a law of the quotient is a law of the shift by being read here and through no rule of the shift's own.

use {super::Intrinsic, crate::Term, curios_algebra::Half};

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
}

impl Division {
    pub fn dividend(&self) -> &Term {
        match self {
            Division::Proved { dividend, .. } => dividend,
            Division::Shifted { value, .. } => value,
        }
    }

    pub fn half(&self) -> Half {
        match self {
            Division::Proved { half, .. } => *half,
            Division::Shifted { .. } => Half::Quotient,
        }
    }

    /// The count the divisor is two to, where it is written as one.
    pub fn count(&self) -> Option<&Term> {
        match self {
            Division::Proved { .. } => None,
            Division::Shifted { count, .. } => Some(count),
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
            _ => None,
        }
    }
}
