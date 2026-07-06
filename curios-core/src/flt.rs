use std::{
    fmt,
    ops::{Add, Div, Mul, Neg, Rem, Sub},
};

/// A type-level float literal, stored as the raw bits of an `f32` so `Eq` and `Hash` are derivable — terms must be hashable and decidably equal, which IEEE `f32` is not. Identity is therefore bitwise (`NaN == NaN` as terms, `0.0 != -0.0`), while the arithmetic and comparison ops below unwrap to `f32` and follow IEEE semantics, matching what the runtime's wasm `f32` ops will compute.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Flt {
    bits: u32,
}

impl Flt {
    /// Capture `v`'s exact bit pattern. Together with `to_f32` this is a lossless round trip (NaN payloads and signed zeros included) — construction never canonicalises a float it merely stores.
    pub fn from_f32(v: f32) -> Self {
        Self { bits: v.to_bits() }
    }

    pub(crate) fn to_f32(self) -> f32 {
        f32::from_bits(self.bits)
    }

    pub(crate) fn abs(self) -> Self {
        Self::from_f32(self.to_f32().abs())
    }

    pub(crate) fn sqrt(self) -> Self {
        Self::from_f32(self.to_f32().sqrt())
    }

    pub(crate) fn floor(self) -> Self {
        Self::from_f32(self.to_f32().floor())
    }

    pub(crate) fn ceil(self) -> Self {
        Self::from_f32(self.to_f32().ceil())
    }

    pub(crate) fn trunc(self) -> Self {
        Self::from_f32(self.to_f32().trunc())
    }

    pub(crate) fn nearest(self) -> Self {
        Self::from_f32(self.to_f32().round_ties_even())
    }

    pub(crate) fn min(self, other: Self) -> Self {
        Self::from_f32(self.to_f32().min(other.to_f32()))
    }

    pub(crate) fn max(self, other: Self) -> Self {
        Self::from_f32(self.to_f32().max(other.to_f32()))
    }

    pub(crate) fn eql(self, other: Self) -> bool {
        self.to_f32() == other.to_f32()
    }

    pub(crate) fn neq(self, other: Self) -> bool {
        self.to_f32() != other.to_f32()
    }

    pub(crate) fn lt(self, other: Self) -> bool {
        self.to_f32() < other.to_f32()
    }

    pub(crate) fn gt(self, other: Self) -> bool {
        self.to_f32() > other.to_f32()
    }

    pub(crate) fn lte(self, other: Self) -> bool {
        self.to_f32() <= other.to_f32()
    }

    pub(crate) fn gte(self, other: Self) -> bool {
        self.to_f32() >= other.to_f32()
    }
}

impl Add for Flt {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self::from_f32(self.to_f32() + other.to_f32())
    }
}

impl Sub for Flt {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self::from_f32(self.to_f32() - other.to_f32())
    }
}

impl Mul for Flt {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        Self::from_f32(self.to_f32() * other.to_f32())
    }
}

impl Div for Flt {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        Self::from_f32(self.to_f32() / other.to_f32())
    }
}

// C `fmod`: `x - trunc(x / y) * y` (the sign of the dividend), matching `f32`'s
// `%` and the `cont -> wasm` expansion of `Flt.rem`.
impl Rem for Flt {
    type Output = Self;

    fn rem(self, other: Self) -> Self {
        Self::from_f32(self.to_f32() % other.to_f32())
    }
}

impl Neg for Flt {
    type Output = Self;

    fn neg(self) -> Self {
        Self::from_f32(-self.to_f32())
    }
}

impl fmt::Display for Flt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_f32())
    }
}
