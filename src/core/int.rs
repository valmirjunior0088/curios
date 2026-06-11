use {
    num_bigint::BigInt,
    num_traits::{FromPrimitive, ToPrimitive, Zero},
    std::{
        fmt,
        ops::{Add, Mul, Sub},
    },
};

/// A type-level integer. Unbounded — the type level pretends ℤ, the way
/// `Nat`'s `BigUint` pretends ℕ; the runtime's 31-bit range is enforced only
/// where a literal must materialize (`erase`'s narrowing) and by the
/// runtime's own overflow traps.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Int {
    value: BigInt,
}

impl Int {
    pub fn new(value: impl Into<BigInt>) -> Self {
        Self {
            value: value.into(),
        }
    }

    pub fn to_i32(&self) -> Option<i32> {
        self.value.to_i32()
    }

    pub fn is_zero(&self) -> bool {
        self.value.is_zero()
    }

    /// The integer part of `value`, exactly — `None` when there is none (NaN,
    /// ±inf). No finite float is out of range at the type level.
    pub fn from_f32_trunc(value: f32) -> Option<Self> {
        BigInt::from_f64(value.trunc() as f64).map(|value| Self { value })
    }

    /// `None` on a zero divisor — the reducer reports that case before
    /// folding. Truncates toward zero, like the runtime's `i32.div_s`.
    pub fn checked_div(self, other: Self) -> Option<Self> {
        (!other.value.is_zero()).then(|| Self {
            value: self.value / other.value,
        })
    }

    /// `None` on a zero divisor, like [`Int::checked_div`]. The remainder
    /// takes the dividend's sign, like the runtime's `i32.rem_s`.
    pub fn checked_rem(self, other: Self) -> Option<Self> {
        (!other.value.is_zero()).then(|| Self {
            value: self.value % other.value,
        })
    }
}

impl Add for Int {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            value: self.value + other.value,
        }
    }
}

impl Sub for Int {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
            value: self.value - other.value,
        }
    }
}

impl Mul for Int {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        Self {
            value: self.value * other.value,
        }
    }
}

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Delegate so format flags pass through — the printer relies on
        // `{:+}` for the surface `+`/`-` literal prefix.
        self.value.fmt(f)
    }
}
