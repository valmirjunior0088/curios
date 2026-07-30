use {
    num_bigint::BigInt,
    num_traits::{FromPrimitive, ToPrimitive, Zero},
    std::{
        fmt,
        ops::{Add, BitAnd, BitOr, BitXor, Mul, Sub},
    },
};

/// A type-level integer. Unbounded — the type level pretends ℤ, the way `Nat`'s `BigUint` pretends ℕ; the runtime's 31-bit range is enforced only where a literal must materialize (`erase`'s narrowing) and by the runtime's own overflow traps.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Int {
    #[cfg_attr(feature = "archive", rkyv(with = crate::BigIntBytes))]
    value: BigInt,
}

impl Int {
    /// An `Int` from anything `BigInt`-convertible — the entry point for integer literals, e.g. curios-text's lowering of a surface `Int` token.
    pub fn new(value: impl Into<BigInt>) -> Self {
        Self {
            value: value.into(),
        }
    }

    pub fn to_i32(&self) -> Option<i32> {
        self.value.to_i32()
    }

    /// `self << amount` as `self * 2^amount`, and `self >> amount` as the arithmetic (floor) shift `num-bigint` provides — both unbounded. `None` when `amount` is negative or too large to be a shift count, leaving the op a neutral term rather than fabricating a value.
    pub fn checked_shl(self, amount: Self) -> Option<Self> {
        Some(Self {
            value: self.value << amount.value.to_usize()?,
        })
    }

    pub fn checked_shr(self, amount: Self) -> Option<Self> {
        Some(Self {
            value: self.value >> amount.value.to_usize()?,
        })
    }

    pub fn is_zero(&self) -> bool {
        self.value.is_zero()
    }

    /// The integer part of `value`, exactly — `None` when there is none (NaN, ±inf). No finite float is out of range at the type level.
    pub fn from_f32_trunc(value: f32) -> Option<Self> {
        BigInt::from_f64(value.trunc() as f64).map(|value| Self { value })
    }

    /// `None` on a zero divisor — the reducer reports that case before folding. Truncates toward zero, like the runtime's `i32.div_s`.
    pub fn checked_div(self, other: Self) -> Option<Self> {
        (!other.value.is_zero()).then(|| Self {
            value: self.value / other.value,
        })
    }

    /// `None` on a zero divisor, like [`Int::checked_div`]. The remainder takes the dividend's sign, like the runtime's `i32.rem_s`.
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

/// Unbounded bitwise `and`/`or`/`xor`, on the infinite two's-complement representation `num-bigint` models. The type level pretends ℤ, so these impose no 31-bit limit; the runtime's i31 carrier is enforced only in the backend (see `scalar_eval`/`code_emitter`).
impl BitAnd for Int {
    type Output = Self;

    fn bitand(self, other: Self) -> Self {
        Self {
            value: self.value & other.value,
        }
    }
}

impl BitOr for Int {
    type Output = Self;

    fn bitor(self, other: Self) -> Self {
        Self {
            value: self.value | other.value,
        }
    }
}

impl BitXor for Int {
    type Output = Self;

    fn bitxor(self, other: Self) -> Self {
        Self {
            value: self.value ^ other.value,
        }
    }
}

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Delegate so format flags pass through — the printer relies on `{:+}` for the surface `+`/`-` literal prefix.
        self.value.fmt(f)
    }
}
