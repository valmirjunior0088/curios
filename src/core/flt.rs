use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Flt {
    bits: u32,
}

impl Flt {
    pub fn from_f32(v: f32) -> Self {
        Self { bits: v.to_bits() }
    }

    pub fn to_f32(self) -> f32 {
        f32::from_bits(self.bits)
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

    pub fn lte(self, other: Self) -> bool {
        self.to_f32() <= other.to_f32()
    }

    pub fn gte(self, other: Self) -> bool {
        self.to_f32() >= other.to_f32()
    }

    pub fn add(self, other: Self) -> Self {
        Self::from_f32(self.to_f32() + other.to_f32())
    }

    pub fn sub(self, other: Self) -> Self {
        Self::from_f32(self.to_f32() - other.to_f32())
    }

    pub fn mul(self, other: Self) -> Self {
        Self::from_f32(self.to_f32() * other.to_f32())
    }

    pub fn div(self, other: Self) -> Self {
        Self::from_f32(self.to_f32() / other.to_f32())
    }

    pub fn neg(self) -> Self {
        Self::from_f32(-self.to_f32())
    }
}

impl fmt::Display for Flt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_f32())
    }
}
