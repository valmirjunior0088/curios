//! What a natural number lies below, as a power of two: the lattice over which the representation analysis decides which values a machine word holds.

use {
    super::{ENVELOPE_BITS, Lattice},
    curios_num::Natural,
};

/// Every value a site may hold lies below `2^bits`, or nothing bounds it.
///
/// A bound past `2³²` bounds nothing anything reads it for, since no register a natural rides is wider, so it is no bound; that also caps the lattice's height, so a site's bound rises at most a bit a move and a counter growing round a loop settles unbounded within 33 moves, with no widening of its own. The lower end is always zero, which is what lets a monus or a quotient keep its first operand's bound.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Bound(Option<u32>);

impl Bound {
    /// Bounded by nothing.
    pub(crate) const UNBOUNDED: Self = Self(None);
    /// Below `2³⁰`: the non-negative half of the i31, under which a machine word holds a natural.
    pub(crate) const WORD: Self = Self(Some(ENVELOPE_BITS.unsigned_abs() - 1));
    /// Below 2: a predicate's answer.
    pub(crate) const BIT: Self = Self(Some(1));
    /// Below `2⁸`: a byte.
    pub(crate) const BYTE: Self = Self(Some(8));
    /// Below `2³²`: a sequence's length, which every construction holds to `2³² - 1`.
    pub(crate) const LENGTH: Self = Self(Some(32));

    /// Below `2^bits`, which past `2³²` is no bound.
    fn below(bits: u64) -> Self {
        u32::try_from(bits)
            .ok()
            .filter(|bits| *bits <= 32)
            .map_or(Self::UNBOUNDED, |bits| Self(Some(bits)))
    }

    /// The least bound `value` lies below.
    pub(crate) fn of(value: &Natural) -> Self {
        Self::below(value.bits())
    }

    /// Whether a machine word holds every value this admits.
    pub(crate) fn is_word(self) -> bool {
        self.wider(Self::WORD) == Self::WORD
    }

    /// Whether a word read unsigned holds every value this admits.
    pub(crate) fn is_length(self) -> bool {
        self.wider(Self::LENGTH) == Self::LENGTH
    }

    /// The narrower of two bounds, which a value both hold of lies below.
    pub(crate) fn narrower(self, other: Self) -> Self {
        match (self.0, other.0) {
            (Some(left), Some(right)) => Self(Some(left.min(right))),
            (Some(bits), None) | (None, Some(bits)) => Self(Some(bits)),
            (None, None) => Self::UNBOUNDED,
        }
    }

    /// The wider of two bounds, which either's value lies below.
    pub(crate) fn wider(self, other: Self) -> Self {
        let mut wider = self;
        wider.join(other);
        wider
    }

    /// `bits` more: what a value below this, scaled by `2^bits`, lies below.
    pub(crate) fn raised(self, bits: u64) -> Self {
        self.0
            .map_or(Self::UNBOUNDED, |own| Self::below(u64::from(own) + bits))
    }

    /// `bits` fewer: what a value below this, divided by `2^bits`, lies below.
    pub(crate) fn lowered(self, bits: u64) -> Self {
        self.0.map_or(Self::UNBOUNDED, |own| {
            Self(Some(
                u32::try_from(u64::from(own).saturating_sub(bits)).unwrap_or(0),
            ))
        })
    }

    /// What a product of a value below this and one below `other` lies below.
    pub(crate) fn times(self, other: Self) -> Self {
        match other.0 {
            Some(bits) => self.raised(bits.into()),
            None => Self::UNBOUNDED,
        }
    }
}

impl Lattice for Bound {
    /// No value has arrived: the bound zero alone lies below, which every other bound admits.
    fn bottom() -> Self {
        Self(Some(0))
    }

    fn join(&mut self, incoming: Self) {
        *self = match (self.0, incoming.0) {
            (Some(own), Some(other)) => Self(Some(own.max(other))),
            _ => Self::UNBOUNDED,
        };
    }
}

#[cfg(test)]
mod tests;
