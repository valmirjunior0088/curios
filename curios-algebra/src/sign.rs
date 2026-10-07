//! A binary64 value read through the three operations that touch its sign bit and nothing else.
//!
//! `neg` flips the bit, `abs` clears it and `copysign` replaces it with another value's, each on every one of the 2⁶⁴ patterns, a NaN's included. So any composition of the three over opaque values is one value's bits below the sign, under a sign bit that is clear, set, some value's, or some value's flipped — a [`SignMagnitude`] — and two compositions are one value at every pattern exactly when they read as the same one. [`SignMagnitude::form`] is the one way each is written.
//!
//! **A value is its caller's handle**, compared by the caller's identity: this module asks only whether two handles are equal, which tells a value under its own sign from a value under another's.

/// A sign bit, as the sign operations leave one.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum SignBit<V> {
    Clear,
    Set,
    /// A value's sign bit.
    Of(V),
    /// A value's sign bit, flipped.
    Against(V),
}

impl<V> SignBit<V> {
    /// This bit flipped.
    pub fn flipped(self) -> Self {
        match self {
            SignBit::Clear => SignBit::Set,
            SignBit::Set => SignBit::Clear,
            SignBit::Of(value) => SignBit::Against(value),
            SignBit::Against(value) => SignBit::Of(value),
        }
    }
}

/// The bits of `magnitude` below its sign, under `sign`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SignMagnitude<V> {
    pub magnitude: V,
    pub sign: SignBit<V>,
}

impl<V: Clone + PartialEq> SignMagnitude<V> {
    /// A value none of the three operations was applied to: its own bits under its own sign.
    pub fn of(value: V) -> Self {
        SignMagnitude {
            sign: SignBit::Of(value.clone()),
            magnitude: value,
        }
    }

    /// `neg` of this value: its sign flipped.
    pub fn negated(self) -> Self {
        SignMagnitude {
            magnitude: self.magnitude,
            sign: self.sign.flipped(),
        }
    }

    /// `abs` of this value: its sign cleared.
    pub fn absolute(self) -> Self {
        SignMagnitude {
            magnitude: self.magnitude,
            sign: SignBit::Clear,
        }
    }

    /// `copysign` of this value and `other`: this magnitude under the other's sign. This value's sign and the other's magnitude are not read.
    pub fn signed_as(self, other: Self) -> Self {
        SignMagnitude {
            magnitude: self.magnitude,
            sign: other.sign,
        }
    }

    /// The one way this value is written: the value itself where it stands under its own sign, and otherwise the fewest operations that say its sign.
    pub fn form(self) -> SignForm<V> {
        let SignMagnitude { magnitude, sign } = self;
        match sign {
            SignBit::Clear => SignForm::Absolute(magnitude),
            SignBit::Set => SignForm::NegatedAbsolute(magnitude),
            SignBit::Of(sign) if sign == magnitude => SignForm::Bare(magnitude),
            SignBit::Against(sign) if sign == magnitude => SignForm::Negated(magnitude),
            SignBit::Of(sign) => SignForm::Copied { magnitude, sign },
            SignBit::Against(sign) => SignForm::CopiedNegated { magnitude, sign },
        }
    }
}

/// How a [`SignMagnitude`] is written, one form to a value.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum SignForm<V> {
    /// The value itself.
    Bare(V),
    /// `neg(v)`.
    Negated(V),
    /// `abs(v)`.
    Absolute(V),
    /// `neg(abs(v))`.
    NegatedAbsolute(V),
    /// `copysign(magnitude, sign)`, the two being two values.
    Copied { magnitude: V, sign: V },
    /// `copysign(magnitude, neg(sign))`, the two being two values.
    CopiedNegated { magnitude: V, sign: V },
}

#[cfg(test)]
mod tests;
