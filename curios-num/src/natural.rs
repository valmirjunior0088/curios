use {
    num_bigint::BigUint,
    num_traits::{One, ToPrimitive, Zero},
    std::{
        fmt,
        ops::{Add, AddAssign, BitAnd, BitOr, BitXor, Div, Mul, Rem, Sub},
    },
};

/// A type-level natural. Unbounded — the type level pretends ℕ, the way [`Integer`](crate::Integer) pretends ℤ; the runtime's 31-bit range is enforced only where a literal must materialize (`erase`'s narrowing) and by the runtime's own overflow traps.
///
/// The wrapped magnitude is private, which is the point: this crate is the only one that names `num-bigint`, so a consumer reaches ℕ through the operations below rather than through a bignum type it would have to depend on. The scalar semantics of the *erased* carrier — where `Nat` is a `u32` that refuses past its width — live in [`nat_add`](crate::nat_add) and its siblings instead; nothing here imposes a width.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

    pub fn to_u8(&self) -> Option<u8> {
        self.value.to_u8()
    }

    pub fn to_u32(&self) -> Option<u32> {
        self.value.to_u32()
    }

    /// The nearest `f32`, saturating to infinity on a magnitude too large to represent — which is how the `Flt` literal path detects a numeral no float can spell, since no literal may name an infinity.
    pub fn to_f32(&self) -> Option<f32> {
        self.value.to_f32()
    }

    /// The magnitude as a `u64`, when it fits.
    ///
    /// Beside [`Natural::to_usize`] rather than instead of it, and the difference is not cosmetic: `usize` is 32 bits on wasm32 and 64 natively, so a question answered through it can be answered differently on the two targets. A reduction *charge* may not be, so a charge computed from a magnitude reads it through this.
    pub fn to_u64(&self) -> Option<u64> {
        self.value.to_u64()
    }

    pub fn to_usize(&self) -> Option<usize> {
        self.value.to_usize()
    }

    pub fn to_i32(&self) -> Option<i32> {
        self.value.to_i32()
    }

    /// How many bits the magnitude occupies — zero for zero, `floor(log2(n)) + 1` otherwise.
    ///
    /// The size a reduction charges for a result *before* building it. Bits rather than limbs because a limb is a property of this target's `num-bigint` build and a bit is a property of the number, and the budget has to price a program the same on wasm32 as it does natively.
    pub fn bits(&self) -> u64 {
        self.value.bits()
    }

    /// The canonical little-endian byte encoding, and [`from_bytes_le`](Natural::from_bytes_le) its inverse. Zero mints one zero byte, `[0]`, never the empty string. The host/guest handle token rides on exactly this pair (`curios-abi`'s `Handle`), so the two ends cannot drift.
    pub fn to_bytes_le(&self) -> Vec<u8> {
        self.value.to_bytes_le()
    }

    pub fn from_bytes_le(bytes: &[u8]) -> Self {
        Self {
            value: BigUint::from_bytes_le(bytes),
        }
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
    /// Here rather than in [`Floating`](crate::Floating) because it is a fact about ℕ, and it is the one operation binary32's square root needs that the ring operations do not supply: a root is exact when `isqrt(n)² = n` and rounds off the remainder otherwise, which is the sticky bit its caller wants.
    pub fn isqrt(&self) -> Self {
        Self {
            value: self.value.sqrt(),
        }
    }

    /// `None` on a zero divisor — the reducer reports that case before folding.
    pub fn checked_div(self, other: Self) -> Option<Self> {
        (!other.value.is_zero()).then(|| Self {
            value: self.value / other.value,
        })
    }

    /// `None` on a zero divisor, like [`Natural::checked_div`].
    pub fn checked_rem(self, other: Self) -> Option<Self> {
        (!other.value.is_zero()).then(|| Self {
            value: self.value % other.value,
        })
    }

    /// `self << amount` as `self · 2^amount`, and `self >> amount` as `⌊self / 2^amount⌋` — both unbounded. `None` when `amount` is too large to be a shift count, leaving the op a neutral term rather than fabricating a value.
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

    /// The raw magnitude, for this crate's own conversions only — [`Integer`](crate::Integer)'s widening is the one caller. Crate-internal because the whole point of the newtype is that nothing above `curios-num` names a `BigUint`.
    pub(crate) fn as_big_uint(&self) -> &BigUint {
        &self.value
    }
}

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

// `/` and `%` **panic** on a zero divisor, like `num-bigint`'s, and are for call sites that have already established the divisor is nonzero — the euclidean split, where the divisor came from a checked fold. A fold that must *decline* rather than trust its operands uses `checked_div`/`checked_rem` instead, which is why both spellings exist.
binary_op!(Div, div);
binary_op!(Rem, rem);

// Unbounded bitwise `and`/`or`/`xor`, on the infinite binary expansion `num-bigint` models. The type level pretends ℕ, so these impose no 31-bit limit; the runtime's i31 carrier is enforced only in the backend.
binary_op!(BitAnd, bitand);
binary_op!(BitOr, bitor);
binary_op!(BitXor, bitxor);

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
