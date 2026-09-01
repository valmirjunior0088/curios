use {
    crate::Natural,
    num_bigint::BigInt,
    num_traits::{ToPrimitive, Zero},
    std::{
        fmt,
        ops::{Add, BitAnd, BitOr, BitXor, Mul, Neg, Sub},
    },
};

/// A type-level integer. Unbounded — the type level pretends ℤ, the way [`Natural`] pretends ℕ; the runtime's 31-bit range is enforced only where a literal must materialize (`erase`'s narrowing) and by the runtime's own overflow traps.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived]
pub struct Integer {
    #[archived_with(crate::BigIntBytes)]
    value: BigInt,
}

impl Integer {
    pub fn to_i32(&self) -> Option<i32> {
        self.value.to_i32()
    }

    /// How many bits the magnitude occupies, ignoring the sign — [`Natural::bits`], for the signed carrier.
    pub fn bits(&self) -> u64 {
        self.value.bits()
    }

    /// The same number as a [`Natural`]; `None` on a negative, which no natural equals. [`From<Natural>`](Integer::from) is the total inverse.
    pub fn to_natural(&self) -> Option<Natural> {
        self.value.to_biguint().map(Natural::new)
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

    /// `None` on a zero divisor — the reducer reports that case before folding. Truncates toward zero, like the runtime's `i32.div_s`.
    pub fn checked_div(self, other: Self) -> Option<Self> {
        (!other.value.is_zero()).then(|| Self {
            value: self.value / other.value,
        })
    }

    /// `None` on a zero divisor, like [`Integer::checked_div`]. The remainder takes the dividend's sign, like the runtime's `i32.rem_s`.
    pub fn checked_rem(self, other: Self) -> Option<Self> {
        (!other.value.is_zero()).then(|| Self {
            value: self.value % other.value,
        })
    }
}

impl Add for Integer {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            value: self.value + other.value,
        }
    }
}

impl Sub for Integer {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
            value: self.value - other.value,
        }
    }
}

impl Mul for Integer {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        Self {
            value: self.value * other.value,
        }
    }
}

/// Unbounded bitwise `and`/`or`/`xor`, on the infinite two's-complement representation `num-bigint` models. The type level pretends ℤ, so these impose no 31-bit limit; the runtime's i31 carrier is enforced only in the backend.
impl BitAnd for Integer {
    type Output = Self;

    fn bitand(self, other: Self) -> Self {
        Self {
            value: self.value & other.value,
        }
    }
}

impl BitOr for Integer {
    type Output = Self;

    fn bitor(self, other: Self) -> Self {
        Self {
            value: self.value | other.value,
        }
    }
}

impl BitXor for Integer {
    type Output = Self;

    fn bitxor(self, other: Self) -> Self {
        Self {
            value: self.value ^ other.value,
        }
    }
}

/// Negation is total and exact at the type level: ℤ is closed under it, so unlike the erased `i32` carrier there is no `i32::MIN` to trap on.
impl Neg for Integer {
    type Output = Self;

    fn neg(self) -> Self {
        Self { value: -self.value }
    }
}

/// Every primitive integer converts exactly, and this is the only way in: there is deliberately no `new` taking `impl Into<BigInt>`, because rustdoc would print that bound on a type whose whole purpose is that nothing above this crate names a bignum.
macro_rules! from_primitive {
    ($($primitive:ty),+ $(,)?) => {
        $(
            impl From<$primitive> for Integer {
                fn from(value: $primitive) -> Self {
                    Self { value: BigInt::from(value) }
                }
            }
        )+
    };
}

from_primitive!(
    i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize
);

/// Widening a natural is total and exact — ℕ ⊂ ℤ — and is the inverse of [`Integer::to_natural`] on every value that one accepts.
impl From<Natural> for Integer {
    fn from(value: Natural) -> Self {
        Self {
            value: BigInt::from(value.as_big_uint().clone()),
        }
    }
}

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Delegate so format flags pass through — the printer relies on `{:+}` for the surface `+`/`-` literal prefix.
        self.value.fmt(f)
    }
}
