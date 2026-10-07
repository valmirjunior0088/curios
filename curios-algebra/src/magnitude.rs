//! An integer read through what leaves an absolute value and what flips a sign: a literal factor every coefficient shares, and a negation.
//!
//! `abs(g · r) = g · abs(r)` and `sign(g · r) = sign(r)` for a literal `g` above zero, and `abs(-r) = abs(r)` with `sign(-r) = -sign(r)`. So a combination is a literal factor and a sign over what is left ([`Scale::of`]), and `abs` and `sign` are read of what is left.
//!
//! **A combination and its negation are one reading under opposite signs.** The sign is the leading coefficient's, the caller saying which coefficient leads by an order of its own that a negation does not move, so `i - j` and `j - i` leave the same thing under opposite signs and `sign(-r) = -sign(r)` holds of every `r`, a sum of both signs among them.
//!
//! **Only what no substitution undoes is read.** A literal factor of every coefficient stays one whatever is put for an atom. A symbolic factor does not: a product by a sum is distributed, `(l + 1) · j` reaching here as `l · j + j`, so reading one would hold of a term and fail of an instance of it. `abs(a · b) = abs(a) · abs(b)` is therefore a lemma.

use curios_num::{Integer, Natural};

/// The literal an integer combination is a multiple of, and whether it is read negated.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scale {
    /// The greatest literal dividing every coefficient, at least one.
    pub factor: Natural,
    /// The leading coefficient is below zero, so the combination is read as the negation of one whose leading coefficient is above it.
    pub negated: bool,
}

impl Scale {
    /// The scale of a combination with these coefficients — its constant, where it has one, and each monomial's, none of them zero — the first of them the one that leads. `None` for the combination with none, which is zero.
    pub fn of(coefficients: &[Integer]) -> Option<Scale> {
        let factor = coefficients
            .iter()
            .fold(Natural::zero(), |factor, coefficient| {
                factor.gcd(&coefficient.magnitude())
            });
        let zero = Integer::from(Natural::zero());
        let leading = coefficients.first()?;
        Some(Scale {
            factor,
            negated: *leading < zero,
        })
    }

    /// Whether the combination is read as it stands: no factor past one and no negation.
    pub fn is_unit(&self) -> bool {
        self.factor.is_one() && !self.negated
    }

    /// What every coefficient is divided by to leave what `abs` and `sign` are read of: the factor, below zero where the combination is negated.
    pub fn divisor(&self) -> Integer {
        let factor = Integer::from(self.factor.clone());
        match self.negated {
            true => -factor,
            false => factor,
        }
    }
}

#[cfg(test)]
mod tests;
