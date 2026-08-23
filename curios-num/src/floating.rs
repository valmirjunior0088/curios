use std::{
    fmt,
    ops::{Add, Div, Mul, Neg, Rem, Sub},
};

/// A type-level float literal, stored as the raw bits of an `f32` so `Eq` and `Hash` are derivable — terms must be hashable and decidably equal, which IEEE `f32` is not. Identity is therefore bitwise (`NaN == NaN` as terms, `0.0 != -0.0`), while the arithmetic and comparison ops below unwrap to `f32` and follow IEEE semantics, matching what the runtime's wasm `f32` ops will compute.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Floating {
    bits: u32,
}

impl Floating {
    /// Capture `v`'s exact bit pattern. Together with `to_f32` this is a lossless round trip (NaN payloads and signed zeros included) — construction never canonicalises a float it merely stores.
    pub fn from_f32(v: f32) -> Self {
        Self { bits: v.to_bits() }
    }

    pub fn to_f32(self) -> f32 {
        f32::from_bits(self.bits)
    }

    /// The stored bit pattern — the identity `Eq` and `Hash` are derived over, for a caller keying on it.
    pub fn to_bits(self) -> u32 {
        self.bits
    }

    pub fn abs(self) -> Self {
        Self::from_f32(self.to_f32().abs())
    }

    pub fn sqrt(self) -> Self {
        Self::from_f32(self.to_f32().sqrt())
    }

    pub fn floor(self) -> Self {
        Self::from_f32(self.to_f32().floor())
    }

    pub fn ceil(self) -> Self {
        Self::from_f32(self.to_f32().ceil())
    }

    pub fn trunc(self) -> Self {
        Self::from_f32(self.to_f32().trunc())
    }

    pub fn copysign(self, other: Self) -> Self {
        Self::from_f32(self.to_f32().copysign(other.to_f32()))
    }

    pub fn nearest(self) -> Self {
        Self::from_f32(self.to_f32().round_ties_even())
    }

    pub fn min(self, other: Self) -> Self {
        Self::from_f32(self.to_f32().min(other.to_f32()))
    }

    pub fn max(self, other: Self) -> Self {
        Self::from_f32(self.to_f32().max(other.to_f32()))
    }

    pub fn eql(self, other: Self) -> bool {
        self.to_f32() == other.to_f32()
    }

    pub fn neq(self, other: Self) -> bool {
        self.to_f32() != other.to_f32()
    }

    pub fn lt(self, other: Self) -> bool {
        self.to_f32() < other.to_f32()
    }

    pub fn gt(self, other: Self) -> bool {
        self.to_f32() > other.to_f32()
    }

    pub fn le(self, other: Self) -> bool {
        self.to_f32() <= other.to_f32()
    }

    pub fn ge(self, other: Self) -> bool {
        self.to_f32() >= other.to_f32()
    }
}

impl Add for Floating {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self::from_f32(self.to_f32() + other.to_f32())
    }
}

impl Sub for Floating {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self::from_f32(self.to_f32() - other.to_f32())
    }
}

impl Mul for Floating {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        Self::from_f32(self.to_f32() * other.to_f32())
    }
}

impl Div for Floating {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        Self::from_f32(self.to_f32() / other.to_f32())
    }
}

// C `fmod`: `x - trunc(x / y) * y` (the sign of the dividend), matching `f32`'s `%` and the `cont -> wasm` expansion of `Flt.rem`.
impl Rem for Floating {
    type Output = Self;

    fn rem(self, other: Self) -> Self {
        Self::from_f32(self.to_f32() % other.to_f32())
    }
}

impl Neg for Floating {
    type Output = Self;

    fn neg(self) -> Self {
        Self::from_f32(-self.to_f32())
    }
}

impl fmt::Display for Floating {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_f32())
    }
}
