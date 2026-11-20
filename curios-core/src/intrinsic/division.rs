//! A division read through every spelling it has, at `Nat` and at `Int`: a quotient or a remainder beside the proof that its divisor is nonzero, a right shift by `k`, the quotient by `2ᵏ`, and a mask of the `k` low bits, `and(x, 2ᵏ - 1)`, the remainder by `2ᵏ` — whose divisor is positive at every count and needs no proof.
//!
//! Every reader of a division — the folds, Euclid's recombination, the elaborator's procedure — takes a spelling through this view, so a law of the quotient is a law of the shift, and one of the remainder a law of the mask, by being read here and through no rule of their own. Both carriers divide as Euclid does, so a shift is a quotient and a mask a remainder at `Int` as at `Nat`, below zero too: an arithmetic shift floors, and a mask of a two's-complement value is its residue.

use {
    super::Intrinsic,
    crate::{Nat, Term},
    curios_algebra::{Carrier, Half},
    curios_num::{Integer, Natural},
};

/// One half of a division, in the spelling an intrinsic writes it in, at the carrier of its dividend.
#[derive(Clone)]
pub enum Division {
    /// `x / d` or `x % d`, beside the proof that `d` is nonzero.
    Proved {
        carrier: Carrier,
        dividend: Term,
        divisor: Term,
        non_zero: Term,
        half: Half,
    },
    /// `shr(x, k)`, the quotient of `x` by `2ᵏ`.
    Shifted {
        carrier: Carrier,
        value: Term,
        count: Term,
    },
    /// `and(x, 2ᵏ - 1)`, the remainder of `x` by `2ᵏ`, `k` at least one, read off the literal mask: `width` is `k`, `divisor` is `2ᵏ`, and `mask_first` is whether the mask is the left operand.
    Masked {
        carrier: Carrier,
        value: Term,
        width: u64,
        divisor: Natural,
        mask_first: bool,
    },
}

impl Division {
    pub fn carrier(&self) -> Carrier {
        match self {
            Division::Proved { carrier, .. }
            | Division::Shifted { carrier, .. }
            | Division::Masked { carrier, .. } => *carrier,
        }
    }

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

    /// Whether the remainder this spelling answers is an integer, a natural widened: a mask at `Int`, where `Int/rem` answers the natural itself.
    pub fn widened(&self) -> bool {
        matches!(
            self,
            Division::Masked {
                carrier: Carrier::Integer,
                ..
            }
        )
    }

    /// The count the divisor is two to, where it is written as one: a shift's as written, a mask's its literal width.
    pub fn count(&self) -> Option<Term> {
        match self {
            Division::Proved { .. } => None,
            Division::Shifted { count, .. } => Some(count.clone()),
            Division::Masked { width, .. } => Some(nat_literal(Natural::from(*width))),
        }
    }

    /// The divisor, as a term of the dividend's carrier: a proved one as written, a shift's as `pow(2, k)`, widened at `Int`, and a mask's as the literal `2ᵏ`.
    pub fn divisor(&self) -> Term {
        match self {
            Division::Proved { divisor, .. } => divisor.clone(),
            Division::Shifted { carrier, count, .. } => {
                let power = Term::intrinsic(Intrinsic::NatPow(
                    nat_literal(Natural::from(2u32)),
                    count.clone(),
                ));
                match carrier {
                    Carrier::Integer => Term::intrinsic(Intrinsic::NatToInt(power)),
                    _ => power,
                }
            }
            Division::Masked {
                carrier, divisor, ..
            } => literal(*carrier, divisor.clone()),
        }
    }

    /// The quotient of this division, in the spelling it pairs with: the term `/` writes beside `%`, and the shift by `k` beside a mask of `k` bits; a quotient is its own.
    pub fn quotient(&self) -> Term {
        match self {
            Division::Proved {
                carrier,
                dividend,
                divisor,
                non_zero,
                ..
            } => Term::intrinsic(divided(
                *carrier,
                Half::Quotient,
                dividend.clone(),
                divisor.clone(),
                non_zero.clone(),
            )),
            Division::Shifted {
                carrier,
                value,
                count,
            } => Term::intrinsic(shifted(*carrier, value.clone(), count.clone())),
            Division::Masked {
                carrier,
                value,
                width,
                ..
            } => Term::intrinsic(shifted(
                *carrier,
                value.clone(),
                nat_literal(Natural::from(*width)),
            )),
        }
    }

    /// The same half of the same division of `dividend`, spelled as this one is.
    pub fn of(&self, dividend: Term) -> Intrinsic {
        match self {
            Division::Proved {
                carrier,
                divisor,
                non_zero,
                half,
                ..
            } => divided(*carrier, *half, dividend, divisor.clone(), non_zero.clone()),
            Division::Shifted { carrier, count, .. } => shifted(*carrier, dividend, count.clone()),
            Division::Masked {
                carrier,
                divisor,
                mask_first,
                ..
            } => {
                let mask = literal(*carrier, divisor.clone() - Natural::one());
                let (left, right) = match mask_first {
                    true => (mask, dividend),
                    false => (dividend, mask),
                };
                match carrier {
                    Carrier::Integer => Intrinsic::IntAnd(left, right),
                    _ => Intrinsic::NatAnd(left, right),
                }
            }
        }
    }
}

impl Intrinsic {
    /// The half of a division this intrinsic is, at `Nat` or at `Int`, in whichever spelling it is written; `None` for any other intrinsic.
    pub fn division(&self) -> Option<Division> {
        let proved =
            |carrier, dividend: &Term, divisor: &Term, non_zero: &Term, half| Division::Proved {
                carrier,
                dividend: dividend.clone(),
                divisor: divisor.clone(),
                non_zero: non_zero.clone(),
                half,
            };
        let masked = |carrier, left: &Term, right: &Term| {
            let (value, (width, divisor), mask_first) = match (mask_width(right), mask_width(left))
            {
                (Some(read), _) => (left, read, false),
                (None, Some(read)) => (right, read, true),
                (None, None) => return None,
            };
            Some(Division::Masked {
                carrier,
                value: value.clone(),
                width,
                divisor,
                mask_first,
            })
        };
        match self {
            Intrinsic::NatDiv {
                dividend,
                divisor,
                non_zero,
            } => Some(proved(
                Carrier::Natural,
                dividend,
                divisor,
                non_zero,
                Half::Quotient,
            )),
            Intrinsic::NatRem {
                dividend,
                divisor,
                non_zero,
            } => Some(proved(
                Carrier::Natural,
                dividend,
                divisor,
                non_zero,
                Half::Remainder,
            )),
            Intrinsic::IntDiv {
                dividend,
                divisor,
                non_zero,
            } => Some(proved(
                Carrier::Integer,
                dividend,
                divisor,
                non_zero,
                Half::Quotient,
            )),
            Intrinsic::IntRem {
                dividend,
                divisor,
                non_zero,
            } => Some(proved(
                Carrier::Integer,
                dividend,
                divisor,
                non_zero,
                Half::Remainder,
            )),
            Intrinsic::NatShr(value, count) => Some(Division::Shifted {
                carrier: Carrier::Natural,
                value: value.clone(),
                count: count.clone(),
            }),
            Intrinsic::IntShr(value, count) => Some(Division::Shifted {
                carrier: Carrier::Integer,
                value: value.clone(),
                count: count.clone(),
            }),
            Intrinsic::NatAnd(left, right) => masked(Carrier::Natural, left, right),
            Intrinsic::IntAnd(left, right) => masked(Carrier::Integer, left, right),
            _ => None,
        }
    }

    /// [`Intrinsic::division`] at `Nat`.
    pub fn nat_division(&self) -> Option<Division> {
        self.division()
            .filter(|division| division.carrier() == Carrier::Natural)
    }

    /// [`Intrinsic::division`] at `Int`.
    pub fn int_division(&self) -> Option<Division> {
        self.division()
            .filter(|division| division.carrier() == Carrier::Integer)
    }
}

/// A half of a division with its proof, at `carrier`.
fn divided(
    carrier: Carrier,
    half: Half,
    dividend: Term,
    divisor: Term,
    non_zero: Term,
) -> Intrinsic {
    match (carrier, half) {
        (Carrier::Integer, Half::Quotient) => Intrinsic::IntDiv {
            dividend,
            divisor,
            non_zero,
        },
        (Carrier::Integer, Half::Remainder) => Intrinsic::IntRem {
            dividend,
            divisor,
            non_zero,
        },
        (_, Half::Quotient) => Intrinsic::NatDiv {
            dividend,
            divisor,
            non_zero,
        },
        (_, Half::Remainder) => Intrinsic::NatRem {
            dividend,
            divisor,
            non_zero,
        },
    }
}

/// A right shift at `carrier`.
fn shifted(carrier: Carrier, value: Term, count: Term) -> Intrinsic {
    match carrier {
        Carrier::Integer => Intrinsic::IntShr(value, count),
        _ => Intrinsic::NatShr(value, count),
    }
}

fn nat_literal(value: Natural) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(value)))
}

/// A non-negative literal at `carrier`.
fn literal(carrier: Carrier, value: Natural) -> Term {
    match carrier {
        Carrier::Integer => Term::intrinsic(Intrinsic::Int(Integer::from(value))),
        _ => nat_literal(value),
    }
}

/// The `k` a literal mask `2ᵏ - 1` keeps the low bits of, `k` at least one, with `2ᵏ`: a mask of none is zero, which `and`'s own identities answer. A mask is a natural at `Nat` and a non-negative integer at `Int`, where `-1`, every bit set, is `and`'s unit and no mask.
fn mask_width(mask: &Term) -> Option<(u64, Natural)> {
    let mask = match mask.as_nat() {
        Some(mask) => mask.to_natural()?,
        None => {
            let mask = mask.as_int()?;
            (mask.sign() != Integer::from(-1i32)).then(|| mask.magnitude())?
        }
    };
    let divisor = mask + Natural::one();
    let width = divisor
        .power_of_two_exponent()
        .filter(|width| *width >= 1)?;
    Some((width, divisor))
}
