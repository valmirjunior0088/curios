use {
    crate::{Natural, OutOfRange, ScalarTrap, within},
    num_bigint::{BigInt, Sign},
    num_traits::{ToPrimitive, Zero},
    std::{
        fmt,
        ops::{Add, BitAnd, BitOr, BitXor, Mul, Neg, Shr, Sub},
    },
};

/// A type-level integer. Unbounded — the type level pretends ℤ, the way [`Natural`] pretends ℕ; the running program is unbounded too, an i31 while a value is small and a boxed magnitude past it.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived]
pub struct Integer {
    #[archived_with(crate::BigIntBytes)]
    value: BigInt,
}

impl Integer {
    /// How many bits the magnitude occupies, ignoring the sign — [`Natural::bits`], for the signed carrier.
    pub fn bits(&self) -> u64 {
        self.value.bits()
    }

    /// The absolute value as a [`Natural`] — total where the narrowing to a [`Natural`] is not, for a reader that asks what divides a number and not which side of zero it is on.
    pub fn magnitude(&self) -> Natural {
        Natural::new(self.value.magnitude().clone())
    }

    /// Multiplication, the signed twin of [`Natural::mul_within`] under the same allowance.
    pub fn mul_within(&self, other: &Self, allowance: u64) -> Option<Self> {
        within(self.bits().checked_add(other.bits()), allowance).then(|| Self {
            value: &self.value * &other.value,
        })
    }

    /// `self · 2^shift`, the signed twin of [`Natural::shl_within`]. The count is a [`Natural`], as `/sys`'s `Int/shl` declares it, so there is no negative count to decline. The right shift has no such case: it is the total `>>` below.
    pub fn shl_within(&self, shift: &Natural, allowance: u64) -> Option<Self> {
        if self.is_zero() {
            return Some(Self::from(0u32));
        }

        let amount = u64::try_from(shift).ok()?;

        match within(self.bits().checked_add(amount), allowance) {
            true => Some(Self {
                value: &self.value << usize::try_from(amount).ok()?,
            }),
            false => None,
        }
    }

    pub fn is_zero(&self) -> bool {
        self.value.is_zero()
    }

    /// Division truncating toward zero, trapping on a zero divisor.
    pub fn div(&self, other: &Self) -> Result<Self, ScalarTrap> {
        match other.is_zero() {
            true => Err(ScalarTrap::DivisionByZero),
            false => Ok(Self {
                value: &self.value / &other.value,
            }),
        }
    }

    /// The remainder, taking the dividend's sign, and trapping on a zero divisor like [`Integer::div`].
    pub fn rem(&self, other: &Self) -> Result<Self, ScalarTrap> {
        match other.is_zero() {
            true => Err(ScalarTrap::DivisionByZero),
            false => Ok(Self {
                value: &self.value % &other.value,
            }),
        }
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

/// Unbounded bitwise `and`/`or`/`xor`, on the infinite two's-complement representation `num-bigint` models. The type level pretends ℤ, and the running program computes the same unbounded operations.
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

/// The arithmetic (floor) `⌊self / 2^amount⌋`, total for [`Natural`]'s reason: a count at or past the magnitude's width answers the sign — zero above it and `-1` below — without being narrowed to a host word.
impl Shr<&Natural> for &Integer {
    type Output = Integer;

    fn shr(self, amount: &Natural) -> Integer {
        match u64::try_from(amount).ok() {
            Some(amount) if amount < self.bits() => Integer {
                value: &self.value >> amount,
            },
            _ => Integer::from(match self.value.sign() == Sign::Minus {
                true => -1,
                false => 0,
            }),
        }
    }
}

/// Negation is total and exact: ℤ is closed under it, and the erased carrier is this same unbounded type, so there is no `i32::MIN` anywhere to trap on.
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

/// The machine narrowing: the value as an `i32`, when it fits.
impl TryFrom<&Integer> for i32 {
    type Error = OutOfRange;

    fn try_from(value: &Integer) -> Result<Self, OutOfRange> {
        value.value.to_i32().ok_or(OutOfRange)
    }
}

/// `Int/to_nat`: the same number as a [`Natural`], refusing a negative, which no natural equals — a language conversion with a trap, so its error is [`ScalarTrap::ConversionRange`] rather than [`OutOfRange`]. [`From<Natural>`](Integer::from) is the total inverse. The conversions carry values, never bit views — reinterpretation belongs to explicit `Bin` casts.
impl TryFrom<&Integer> for Natural {
    type Error = ScalarTrap;

    fn try_from(value: &Integer) -> Result<Self, ScalarTrap> {
        value
            .value
            .to_biguint()
            .map(Natural::new)
            .ok_or(ScalarTrap::ConversionRange)
    }
}

/// Widening a natural is total and exact — ℕ ⊂ ℤ — and is the inverse of [`Natural`]'s `TryFrom<&Integer>` on every value that one accepts.
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

/// The number, not the wrapper around the bignum holding it — [`Natural`]'s reason, at the signed carrier.
impl fmt::Debug for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}
