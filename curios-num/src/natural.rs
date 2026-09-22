#[cfg(test)]
mod tests;

use {
    crate::{ScalarTrap, within},
    num_bigint::BigUint,
    num_traits::{One, ToPrimitive, Zero},
    std::{
        fmt,
        ops::{Add, AddAssign, BitAnd, BitOr, BitXor, Div, Mul, Rem, Shr, Sub},
    },
};

/// A type-level natural. Unbounded — the type level pretends ℕ, the way [`Integer`](crate::Integer) pretends ℤ; the running program is unbounded too, an i31 while a value is small and a boxed magnitude past it.
///
/// The wrapped magnitude is private, which is the point: this crate is the only one that names `num-bigint`, so a consumer reaches ℕ through the operations below rather than through a bignum type it would have to depend on. The operations the *erased* stages fold with — [`Natural::mul_within`] and its siblings — impose no width either: a growing operation takes an allowance from its caller and declines past it, because how large a numeral is worth building is a fact about a stage's resources rather than about ℕ.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived]
pub struct Natural {
    #[archived_with(crate::BigUintBytes)]
    value: BigUint,
}

impl Natural {
    /// Crate-internal, because its bound names `BigUint` and rustdoc would print that on a type whose whole purpose is that no one above this crate says it. Consumers build a `Natural` from a primitive through [`From`], or from digits through [`Natural::parse_bytes`].
    pub(crate) fn new(value: impl Into<BigUint>) -> Self {
        Self {
            value: value.into(),
        }
    }

    pub fn zero() -> Self {
        Self {
            value: BigUint::zero(),
        }
    }

    pub fn one() -> Self {
        Self {
            value: BigUint::one(),
        }
    }

    /// Taken by reference so it can be passed as a function item — `option.is_some_and(Natural::is_zero)` is how the reducer tests a divisor without moving it.
    pub fn is_zero(&self) -> bool {
        self.value.is_zero()
    }

    pub fn is_one(&self) -> bool {
        self.value.is_one()
    }

    /// How many bits the magnitude occupies — zero for zero, `floor(log2(n)) + 1` otherwise.
    ///
    /// The size a reduction charges for a result *before* building it. Bits rather than limbs because a limb is a property of this target's `num-bigint` build and a bit is a property of the number, and the budget has to price a program the same on wasm32 as it does natively.
    pub fn bits(&self) -> u64 {
        self.value.bits()
    }

    /// The canonical little-endian byte encoding. Zero mints one zero byte, `[0]`, never the empty string. The host/guest handle token is encoded through exactly this, by `curios-abi`'s `Handle::encode`, and is never decoded back into a number: the host lifts a token by comparing its bytes, so there is no inverse here.
    pub fn to_bytes_le(&self) -> Vec<u8> {
        self.value.to_bytes_le()
    }

    /// Parse `digits` in `radix` — the surface lexer's one entry point for every numeral it reads, decimal included, so `0x`/`0b`/plain all decode the same way. `None` on an empty or ill-formed numeral.
    ///
    /// Deliberately `Option` rather than `Result`: `num-bigint`'s parse error type would otherwise appear in this crate's public API, which is exactly the leak the newtype exists to prevent. No caller inspects the reason — each reports its own diagnostic against the span it holds.
    pub fn parse_bytes(digits: &[u8], radix: u32) -> Option<Self> {
        BigUint::parse_bytes(digits, radix).map(Self::new)
    }

    /// `self` raised to `exponent`, unbounded — no carrier width bounds the result, which is what lets a test build a magnitude deliberately too large for any erased carrier to hold.
    pub fn pow(&self, exponent: u32) -> Self {
        Self {
            value: self.value.pow(exponent),
        }
    }

    /// `⌊√self⌋`, exact and total — zero for zero.
    ///
    /// Here rather than in [`Floating`](crate::Floating) because it is a fact about ℕ, and it is the one operation binary64's square root needs that the ring operations do not supply: a root is exact when `isqrt(n)² = n` and rounds off the remainder otherwise, which is the sticky bit its caller wants.
    pub fn isqrt(&self) -> Self {
        Self {
            value: self.value.sqrt(),
        }
    }

    /// The greatest common divisor, by Euclid's remainders — `gcd(n, 0)` is `n`, so the gcd of no numbers at all is the zero a fold starts from. Here because divisibility is a fact about ℕ: what a comparison reads off it is that two sums whose floors differ modulo the gcd of their coefficients are equal at no value.
    pub fn gcd(&self, other: &Self) -> Self {
        let (mut larger, mut smaller) = (self.value.clone(), other.value.clone());
        while !smaller.is_zero() {
            (larger, smaller) = (smaller.clone(), larger % smaller);
        }

        Self { value: larger }
    }

    /// `Nat` subtraction is monus: truncated at zero, never negative. A method rather than `-`, whose `num-bigint` meaning panics on underflow.
    pub fn monus(&self, other: &Self) -> Self {
        match self.value >= other.value {
            true => Self {
                value: &self.value - &other.value,
            },
            false => Self::zero(),
        }
    }

    /// Multiplication, declining past `allowance` bits.
    ///
    /// Growing, and the reason is not the obvious one: a single product is at most the sum of its operands' widths, but a *chain* of them squares, so thirty nested multiplications take a machine word past any memory there is while costing thirty steps. A step budget cannot see that; a width can.
    pub fn mul_within(&self, other: &Self, allowance: u64) -> Option<Self> {
        within(self.bits().checked_add(other.bits()), allowance).then(|| Self {
            value: &self.value * &other.value,
        })
    }

    /// `self · 2^shift`, declining past `allowance` bits. Zero is answered before the count is looked at, since `0 · 2^k` is zero at every count and a decline there would leave a fold undone for nothing. The right shift has no such case: it is the total `>>` below.
    pub fn shl_within(&self, shift: &Self, allowance: u64) -> Option<Self> {
        if self.is_zero() {
            return Some(Self::zero());
        }

        let amount = u64::try_from(shift).ok()?;

        match within(self.bits().checked_add(amount), allowance) {
            true => Some(Self {
                value: &self.value << usize::try_from(amount).ok()?,
            }),
            false => None,
        }
    }

    /// Division, trapping on a zero divisor.
    pub fn div(&self, other: &Self) -> Result<Self, ScalarTrap> {
        match other.is_zero() {
            true => Err(ScalarTrap::DivisionByZero),
            false => Ok(Self {
                value: &self.value / &other.value,
            }),
        }
    }

    /// The remainder, trapping on a zero divisor like [`Natural::div`].
    pub fn rem(&self, other: &Self) -> Result<Self, ScalarTrap> {
        match other.is_zero() {
            true => Err(ScalarTrap::DivisionByZero),
            false => Ok(Self {
                value: &self.value % &other.value,
            }),
        }
    }

    /// The raw magnitude, for this crate's own conversions only — [`Integer`](crate::Integer)'s widening is the one caller. Crate-internal because the whole point of the newtype is that nothing above `curios-num` names a `BigUint`.
    pub(crate) fn as_big_uint(&self) -> &BigUint {
        &self.value
    }
}

/// A number a narrower type cannot hold: what a narrowing out of [`Natural`] or [`Integer`](crate::Integer) answers when the value does not fit, as `TryFrom` between primitives answers `TryFromIntError`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OutOfRange;

/// The machine narrowings: the magnitude as a primitive, when it fits.
///
/// `u64` stands beside `usize` rather than being replaced by it, and the difference is not cosmetic: `usize` is 32 bits on wasm32 and 64 natively, so a question answered through it can be answered differently on the two targets. A reduction *charge* may not be, so a charge computed from a magnitude reads it through `u64`.
macro_rules! narrow_natural {
    ($($target:ty => $read:ident),+ $(,)?) => {
        $(
            impl TryFrom<&Natural> for $target {
                type Error = OutOfRange;

                fn try_from(value: &Natural) -> Result<Self, OutOfRange> {
                    value.value.$read().ok_or(OutOfRange)
                }
            }
        )+
    };
}

narrow_natural!(u8 => to_u8, u32 => to_u32, u64 => to_u64, usize => to_usize);

impl AddAssign for Natural {
    fn add_assign(&mut self, other: Self) {
        self.value += other.value;
    }
}

impl AddAssign<&Natural> for Natural {
    fn add_assign(&mut self, other: &Natural) {
        self.value += &other.value;
    }
}

/// Declares one binary operator over all four owned/borrowed operand pairings, forwarding to the pairing `num-bigint` already provides. Written once because a call site that holds a borrow — the reducer's `floor + nat_bound(inner)?`, the canceller's `floor_left - &shared` — must not have to clone merely to satisfy a signature.
macro_rules! binary_op {
    ($trait:ident, $method:ident) => {
        impl $trait for Natural {
            type Output = Self;

            fn $method(self, other: Self) -> Self {
                Natural {
                    value: $trait::$method(self.value, other.value),
                }
            }
        }

        impl $trait<&Natural> for Natural {
            type Output = Natural;

            fn $method(self, other: &Natural) -> Natural {
                Natural {
                    value: $trait::$method(self.value, &other.value),
                }
            }
        }

        impl $trait<Natural> for &Natural {
            type Output = Natural;

            fn $method(self, other: Natural) -> Natural {
                Natural {
                    value: $trait::$method(&self.value, other.value),
                }
            }
        }

        impl $trait<&Natural> for &Natural {
            type Output = Natural;

            fn $method(self, other: &Natural) -> Natural {
                Natural {
                    value: $trait::$method(&self.value, &other.value),
                }
            }
        }
    };
}

binary_op!(Add, add);
binary_op!(Mul, mul);

// Subtraction is `num-bigint`'s: it **panics** on underflow rather than truncating, so every call site must already know the difference is a natural. The language-level monus that saturates at zero is `nat_sub` — a different operation at a different layer, and deliberately not spelled `-`.
binary_op!(Sub, sub);

// `/` and `%` **panic** on a zero divisor, like `num-bigint`'s, and are for call sites that have already established the divisor is nonzero — the euclidean split, where the divisor came from a checked fold. A fold that must not trust its operands uses [`Natural::div`]/[`Natural::rem`] instead, which answer a zero divisor with its trap, which is why both spellings exist.
binary_op!(Div, div);
binary_op!(Rem, rem);

// Unbounded bitwise `and`/`or`/`xor`, on the infinite binary expansion `num-bigint` models. The type level pretends ℕ, and the running program computes the same unbounded operations.
binary_op!(BitAnd, bitand);
binary_op!(BitOr, bitor);
binary_op!(BitXor, bitxor);

/// `⌊self / 2^amount⌋`, total: a count at or past the magnitude's own width answers zero, which is the arithmetic rather than a decline. The count is consulted before it is narrowed — a count no word holds is past every width there is — which is what keeps the answer the theory's and not the host's: read through `usize` first, `2³²` folded natively and stayed a neutral term on wasm32, a definitional equation that depended on the target.
impl Shr<&Natural> for &Natural {
    type Output = Natural;

    fn shr(self, amount: &Natural) -> Natural {
        match u64::try_from(amount).ok() {
            Some(amount) if amount < self.bits() => Natural {
                value: &self.value >> amount,
            },
            _ => Natural::zero(),
        }
    }
}

macro_rules! from_primitive {
    ($($primitive:ty),+ $(,)?) => {
        $(
            impl From<$primitive> for Natural {
                fn from(value: $primitive) -> Self {
                    Self { value: BigUint::from(value) }
                }
            }
        )+
    };
}

from_primitive!(u8, u16, u32, u64, u128, usize);

/// Decimal, hex and binary rendering, so a printer can round-trip a literal in the radix it was written in: curios-text's `NatLiteral` carries the written radix and picks the matching format specifier.
macro_rules! radix_format {
    ($($trait:ident),+ $(,)?) => {
        $(
            impl fmt::$trait for Natural {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    // Delegate so format flags pass through, as `Integer`'s `Display` does for the surface `+`/`-` literal prefix.
                    fmt::$trait::fmt(&self.value, f)
                }
            }
        )+
    };
}

radix_format!(Display, Binary, UpperHex);

/// The number, not the wrapper around the bignum holding it. Written rather than derived because the erased stages render their IR with `{:?}` — `curios-cont` prints its `Literal` that way, and `wonder stage cont` is read by people — so a derived `Natural { value: 4 }` would put the representation in every dump where the carrier used to print `4`.
impl fmt::Debug for Natural {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}
