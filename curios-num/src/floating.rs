#[cfg(test)]
mod tests;

mod rounding;
pub use rounding::*;

use {
    crate::{Binary, Integer, Natural, ScalarTrap},
    std::{
        cmp::Ordering,
        fmt,
        ops::{Add, Div, Mul, Neg, Rem, Sub},
    },
};

/// The sign bit, the only bit any of `neg`, `abs` and `copysign` touches.
const SIGN_MASK: u64 = 0x8000_0000_0000_0000;
const EXPONENT_MASK: u64 = 0x7ff0_0000_0000_0000;
const MANTISSA_MASK: u64 = 0x000f_ffff_ffff_ffff;
/// The default NaN: positive, quiet and without a payload, which is what an invalid operation with no NaN operand answers.
const NAN_BITS: u64 = 0x7ff8_0000_0000_0000;
/// The first bit of the trailing significand: set on a quiet NaN and clear on a signaling one (§6.2.1). Quieting sets it and leaves every other bit, the payload included.
const QUIET_BIT: u64 = 1 << 51;
/// The largest finite magnitude, which an overflow answers where its direction does not reach the infinity.
const MAX_FINITE_BITS: u64 = 0x7fef_ffff_ffff_ffff;
/// A normal magnitude occupies exactly this many bits — the hidden bit included.
const SIGNIFICAND_BITS: u32 = 53;
/// How wide the *stored* mantissa field is — the significand without its hidden bit, and the distance the exponent field sits above bit zero.
const MANTISSA_BITS: u32 = 52;
/// The hidden bit's weight, which is also the least normal magnitude.
const HIDDEN_BIT: u64 = 1 << MANTISSA_BITS;
/// The exponent every subnormal has, and the floor no result's exponent goes below.
const MIN_EXPONENT: i32 = -1074;
/// What a magnitude's exponent gains to reach its stored field.
const EXPONENT_BIAS: i32 = 1075;
/// The field an infinity and a NaN share.
const INFINITE_FIELD: i32 = 2047;

/// IEEE 754-2019 binary64, every one of its 2⁶⁴ bit patterns a distinct value, computed exactly over unbounded integers and rounded once.
///
/// The bit pattern is the representation and the identity: `0.0` and `-0.0` are distinct, and so is every NaN, by its sign, its kind and its payload, so the derived `Eq` and `Hash` are bitwise and decidable where IEEE's own equality is neither. Terms must be hashable and decidably equal, and a type can therefore tell every value it is handed apart.
///
/// **No operation below calls an `f64` operation.** Every one unpacks its operands to a signed zero, a signed infinity, a NaN, or a `(sign, magnitude, exponent)` triple with the magnitude under `2^53`; computes exactly over [`Natural`]; and packs the result through the single `round`, which owns the subnormal grid, the carry renormalization and the overflow, each under the [`Rounding`] direction every operation that rounds is handed. That is the whole of why a float means the same thing on every host the compiler runs on, and the reason the conversion to `f64` survives at all is rendering and the tests' oracle — never semantics.
///
/// **Which NaN an operation answers is pinned, because IEEE leaves it open and the running program has to agree** (§6.2.3). An invalid operation with no NaN operand answers the default NaN, positive and quiet with no payload. An operation with NaN operands answers one of them made quiet, and of several the one whose quieted pattern is the greatest — a choice that does not read the operands' order, so every commutative operation stays commutative on every pattern. `neg`, `abs` and `copysign` touch the sign bit alone and never quiet, since IEEE makes them bit operations (§5.5.1).
///
/// The other choices IEEE leaves open are pinned as well: `min`/`max` are 754-2019's `minimum`/`maximum`, a NaN operand propagating and `-0.0` ordered below `+0.0`, which is what Wasm mandates; `rem` is exact `fmod`, the dividend's sign and never a rounding.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Floating {
    bits: u64,
}

/// What a bit pattern denotes: the four cases binary64 has, with a finite value carried as `(-1)^negative · magnitude · 2^exponent`.
///
/// Written out rather than read off the fields at each use because every operation's special cases are stated over these four, and the arithmetic below is then one clause per pair rather than a mask per line. A NaN carries nothing here: which one an operation answers is read off the operands' bits by [`Floating::propagate`].
#[derive(Debug, Clone)]
enum Unpacked {
    Zero {
        negative: bool,
    },
    Infinite {
        negative: bool,
    },
    Nan,
    /// `magnitude` is nonzero and under `2^53`; `exponent` is at least [`MIN_EXPONENT`].
    Finite {
        negative: bool,
        magnitude: Natural,
        exponent: i32,
    },
}

/// `value · 2^amount`, exact. Shift counts here are bounded by the operands' own bit lengths, which the callers keep in the low thousands, so a count that does not fit is this module's bug rather than an input's.
fn shift_left(value: &Natural, amount: u32) -> Natural {
    value
        .shl_within(&Natural::from(amount), u64::MAX)
        .expect("a shift count that fits")
}

/// `⌊value / 2^amount⌋`, the counterpart of [`shift_left`].
fn shift_right(value: &Natural, amount: u32) -> Natural {
    value >> &Natural::from(amount)
}

fn is_odd(value: &Natural) -> bool {
    !(value & &Natural::one()).is_zero()
}

/// The binary64 that `(-1)^negative · (magnitude + ε) · 2^exponent` rounds to under `rounding`, where `ε` is a residue in `(0, 1)` reported by `sticky` and zero when it is not.
///
/// The one place the format's shape is written down. It owns three things nothing above it repeats: the subnormal grid, since a result below `2^-1022` is rounded on the `2^-1074` lattice directly from the same magnitude and so is never rounded twice; the carry out of the top significand bit, which renormalizes and which crosses a subnormal into the least normal with no special case, because that encoding already agrees; and the overflow past the largest finite value, which answers the infinity or that largest value of the sign as the direction says (§7.4).
///
/// A caller that can lose bits — the divisions and the square root — must hand in an `exponent` low enough that at least one bit is dropped here, which is what makes `sticky` meaningful. The exact operations pass `false` and are normalized instead.
fn round(
    negative: bool,
    magnitude: &Natural,
    exponent: i32,
    sticky: bool,
    rounding: Rounding,
) -> Floating {
    if magnitude.is_zero() && !sticky {
        return Floating::zero(negative);
    }

    let bits = i64::try_from(magnitude.bits()).expect("a magnitude of representable width");
    let to_grid = i64::from(MIN_EXPONENT) - i64::from(exponent);

    // The whole value lies under half the least subnormal — a residue alone included — so it rounds to the zero of its sign unless the direction points away from zero, and then to the least subnormal.
    if to_grid > bits {
        return match rounding.rounds_up(negative, Ordering::Less, false, true) {
            true => Floating::least(negative),
            false => Floating::zero(negative),
        };
    }

    let to_significand = bits - i64::from(SIGNIFICAND_BITS);
    let shift = to_significand.max(to_grid);

    if shift <= 0 {
        // Nothing is dropped, so nothing is rounded: normalize instead, left until the significand is full or the exponent reaches the grid.
        let room = u32::try_from(-shift).expect("a negative shift of representable width");
        let headroom = u32::try_from(exponent - MIN_EXPONENT).expect("an exponent above the grid");
        let left = room.min(headroom);

        debug_assert!(!sticky, "a residue below a magnitude that loses no bits");

        return Floating::pack(
            negative,
            &shift_left(magnitude, left),
            exponent - left as i32,
            rounding,
        );
    }

    let shift = u32::try_from(shift).expect("a positive shift of representable width");

    let kept = shift_right(magnitude, shift);
    let dropped = magnitude - &shift_left(&kept, shift);
    let half = shift_left(&Natural::one(), shift - 1);

    // A residue puts the exact value past the midpoint when `dropped` sits on it, and cannot lift it there from below, since `dropped` is a whole number of units.
    let position = match (dropped.cmp(&half), sticky) {
        (Ordering::Equal, true) => Ordering::Greater,
        (position, _) => position,
    };
    let inexact = sticky || !dropped.is_zero();

    let kept = match rounding.rounds_up(negative, position, is_odd(&kept), inexact) {
        true => kept + Natural::one(),
        false => kept,
    };
    let exponent = exponent + shift as i32;

    // Rounding up can carry out of the significand: `0x1f_ffff_ffff_ffff` becomes `0x20_0000_0000_0000`, one bit wider.
    match kept.bits() > u64::from(SIGNIFICAND_BITS) {
        true => Floating::pack(negative, &shift_right(&kept, 1), exponent + 1, rounding),
        false => Floating::pack(negative, &kept, exponent, rounding),
    }
}

/// Capture an `f64`'s bit pattern, every NaN as the pattern it is. Not a semantics: this is how a test's oracle and the two infinities `/sys` spells as range bounds hand a value in, and the conversion back is how a printer reads one out. A literal never enters here — the lexer builds it through [`Floating::of_decimal`], so what it means is the model's and not the compiling host's.
impl From<f64> for Floating {
    fn from(v: f64) -> Self {
        Self::from_bits(v.to_bits())
    }
}

/// The bit pattern as the host's `f64`.
impl From<Floating> for f64 {
    fn from(v: Floating) -> Self {
        f64::from_bits(v.bits)
    }
}

impl Floating {
    /// The value this bit pattern is. Every pattern is one, so nothing is refused and nothing is merged.
    pub fn from_bits(bits: u64) -> Self {
        Self { bits }
    }

    /// The stored bit pattern — the identity `Eq` and `Hash` are derived over, for a caller keying on it.
    pub fn to_bits(self) -> u64 {
        self.bits
    }

    /// `Flt/to_le_bytes`: the eight bytes of the bit pattern, least significant first — the byte order every folder must agree on, which is why it is spelled once here.
    pub fn to_le_bytes(self) -> Binary {
        Binary::from_bytes(self.bits.to_le_bytes().to_vec())
    }

    /// `Flt/of_le_bytes`: the float eight little-endian bytes spell, bit for bit, so it inverts [`Floating::to_le_bytes`] from both sides. Its precondition states the eight, so anything else is [`ScalarTrap::Malformed`].
    pub fn of_le_bytes(value: &Binary) -> Result<Self, ScalarTrap> {
        value
            .to_bytes()
            .and_then(|bytes| <[u8; 8]>::try_from(bytes).ok())
            .map(|bytes| Self::from_bits(u64::from_le_bytes(bytes)))
            .ok_or(ScalarTrap::Malformed)
    }

    /// The default NaN, which an invalid operation with no NaN operand answers.
    pub fn nan() -> Self {
        Self { bits: NAN_BITS }
    }

    pub fn infinite(negative: bool) -> Self {
        Self {
            bits: Self::sign_bit(negative) | EXPONENT_MASK,
        }
    }

    pub fn zero(negative: bool) -> Self {
        Self {
            bits: Self::sign_bit(negative),
        }
    }

    /// The least subnormal of the sign, the value nearest zero after zero itself.
    fn least(negative: bool) -> Self {
        Self {
            bits: Self::sign_bit(negative) | 1,
        }
    }

    /// Whether this is a NaN, of either sign and either kind.
    pub fn is_nan(self) -> bool {
        self.bits & EXPONENT_MASK == EXPONENT_MASK && self.bits & MANTISSA_MASK != 0
    }

    /// Whether this is a *number*: finite, so neither infinity and not a NaN. The same reading `/sys/Bound/Finite` states, and what the surface lexer refuses a literal for.
    pub fn is_finite(self) -> bool {
        self.bits & EXPONENT_MASK != EXPONENT_MASK
    }

    fn sign_bit(negative: bool) -> u64 {
        match negative {
            true => SIGN_MASK,
            false => 0,
        }
    }

    fn is_negative(self) -> bool {
        self.bits & SIGN_MASK != 0
    }

    /// The NaN an operation with these operands answers when at least one is a NaN: the greatest of the NaN operands' quieted patterns, whatever order they arrive in. The default NaN when none is, which no caller relies on — each asks only once it has seen one.
    fn propagate(operands: &[Self]) -> Self {
        operands
            .iter()
            .filter(|operand| operand.is_nan())
            .map(|operand| operand.bits | QUIET_BIT)
            .max()
            .map_or_else(Self::nan, Self::from_bits)
    }

    /// Pack a finite value whose `magnitude` is either full-width (a normal) or sits at [`MIN_EXPONENT`] (a subnormal). Past the largest finite value it answers what `rounding` sends an overflow to at this sign (§7.4): the infinity, or that largest value.
    fn pack(negative: bool, magnitude: &Natural, exponent: i32, rounding: Rounding) -> Self {
        let magnitude = u64::try_from(magnitude).expect("a significand of binary64 width");

        if magnitude == 0 {
            return Self::zero(negative);
        }

        if magnitude < HIDDEN_BIT {
            debug_assert_eq!(exponent, MIN_EXPONENT, "a subnormal off the grid");

            return Self {
                bits: Self::sign_bit(negative) | magnitude,
            };
        }

        let field = exponent + EXPONENT_BIAS;

        match (
            field >= INFINITE_FIELD,
            rounding.overflows_to_infinity(negative),
        ) {
            (true, true) => Self::infinite(negative),
            (true, false) => Self {
                bits: Self::sign_bit(negative) | MAX_FINITE_BITS,
            },
            (false, _) => Self {
                bits: Self::sign_bit(negative)
                    | ((field as u64) << MANTISSA_BITS)
                    | (magnitude - HIDDEN_BIT),
            },
        }
    }

    fn unpack(self) -> Unpacked {
        let negative = self.is_negative();
        let field = ((self.bits & EXPONENT_MASK) >> MANTISSA_BITS) as i32;
        let mantissa = self.bits & MANTISSA_MASK;

        match (field, mantissa) {
            (0, 0) => Unpacked::Zero { negative },
            (0, mantissa) => Unpacked::Finite {
                negative,
                magnitude: Natural::from(mantissa),
                exponent: MIN_EXPONENT,
            },
            (INFINITE_FIELD, 0) => Unpacked::Infinite { negative },
            (INFINITE_FIELD, _) => Unpacked::Nan,
            (field, mantissa) => Unpacked::Finite {
                negative,
                magnitude: Natural::from(mantissa + HIDDEN_BIT),
                exponent: field - EXPONENT_BIAS,
            },
        }
    }

    /// The two magnitudes at one exponent, reached by shifting the larger-exponent one *left*, so the alignment loses nothing and every sum, difference and comparison below is exact.
    fn align(left: (&Natural, i32), right: (&Natural, i32)) -> (Natural, Natural, i32) {
        let exponent = left.1.min(right.1);
        let raise = |(magnitude, own): (&Natural, i32)| {
            shift_left(
                magnitude,
                u32::try_from(own - exponent).expect("an exponent difference of binary64 width"),
            )
        };

        (raise(left), raise(right), exponent)
    }

    /// The exact sum of two nonzero finite values, `(negative, magnitude, exponent)` each, rounded once. An exact zero is `+0.0`, or `-0.0` rounding toward negative (§6.3).
    fn exact_sum(
        left: (bool, &Natural, i32),
        right: (bool, &Natural, i32),
        rounding: Rounding,
    ) -> Self {
        let (left_magnitude, right_magnitude, exponent) =
            Self::align((left.1, left.2), (right.1, right.2));

        match left.0 == right.0 {
            true => round(
                left.0,
                &(left_magnitude + right_magnitude),
                exponent,
                false,
                rounding,
            ),
            false => match left_magnitude.cmp(&right_magnitude) {
                Ordering::Equal => Self::zero(rounding == Rounding::TowardNegative),
                Ordering::Greater => round(
                    left.0,
                    &(left_magnitude - right_magnitude),
                    exponent,
                    false,
                    rounding,
                ),
                Ordering::Less => round(
                    right.0,
                    &(right_magnitude - left_magnitude),
                    exponent,
                    false,
                    rounding,
                ),
            },
        }
    }

    /// The correctly rounded `(-1)^negative · (numerator / denominator) · 2^exponent` under `rounding`, with a nonzero denominator.
    ///
    /// Scaling is chosen so the integer quotient carries more bits than the significand needs *and* lands at an exponent below the subnormal grid, which is what leaves [`round`] a bit to drop and so a meaning for the sticky flag. The remainder is that flag: it says the exact value sits above the quotient without saying by how much, which is all any direction can use.
    fn rounded_quotient(
        negative: bool,
        numerator: &Natural,
        denominator: &Natural,
        exponent: i32,
        rounding: Rounding,
    ) -> Self {
        if numerator.is_zero() {
            return Self::zero(negative);
        }

        let width = i64::try_from(numerator.bits()).expect("a numerator of representable width")
            - i64::try_from(denominator.bits()).expect("a denominator of representable width");
        let scale = (i64::from(SIGNIFICAND_BITS) + 3 - width)
            .max(i64::from(exponent) - i64::from(MIN_EXPONENT) + 1)
            .max(0);
        let scale = u32::try_from(scale).expect("a scale of representable width");

        let scaled = shift_left(numerator, scale);
        let quotient = &scaled / denominator;
        let remainder = scaled % denominator;

        round(
            negative,
            &quotient,
            exponent - scale as i32,
            !remainder.is_zero(),
            rounding,
        )
    }

    /// The magnitude of `self`: the sign bit cleared, and nothing else touched, a NaN included.
    pub fn abs(self) -> Self {
        Self {
            bits: self.bits & !SIGN_MASK,
        }
    }

    /// `self + other` under `rounding`: the exact sum, one rounding. An exact zero sum of opposite signs is `+0.0`, or `-0.0` rounding toward negative (§6.3); `∞ - ∞` is invalid.
    pub fn sum(self, other: Self, rounding: Rounding) -> Self {
        match (self.unpack(), other.unpack()) {
            (Unpacked::Nan, _) | (_, Unpacked::Nan) => Self::propagate(&[self, other]),

            (Unpacked::Infinite { negative: left }, Unpacked::Infinite { negative: right }) => {
                match left == right {
                    true => Self::infinite(left),
                    false => Self::nan(),
                }
            }
            (Unpacked::Infinite { negative }, _) | (_, Unpacked::Infinite { negative }) => {
                Self::infinite(negative)
            }

            (Unpacked::Zero { negative: left }, Unpacked::Zero { negative: right }) => {
                Self::zero(match left == right {
                    true => left,
                    false => rounding == Rounding::TowardNegative,
                })
            }
            (Unpacked::Zero { .. }, _) => other,
            (_, Unpacked::Zero { .. }) => self,

            (
                Unpacked::Finite {
                    negative: left_negative,
                    magnitude: left,
                    exponent: left_exponent,
                },
                Unpacked::Finite {
                    negative: right_negative,
                    magnitude: right,
                    exponent: right_exponent,
                },
            ) => Self::exact_sum(
                (left_negative, &left, left_exponent),
                (right_negative, &right, right_exponent),
                rounding,
            ),
        }
    }

    /// `self - other` under `rounding`, which is the sum with `other` negated at every pattern — a NaN's sign included, so the NaN a difference answers is the one the sum it is would.
    pub fn difference(self, other: Self, rounding: Rounding) -> Self {
        self.sum(-other, rounding)
    }

    /// `self · other` under `rounding`: the exact product, one rounding. `0 · ∞` is invalid.
    pub fn product(self, other: Self, rounding: Rounding) -> Self {
        let negative = self.is_negative() != other.is_negative();

        match (self.unpack(), other.unpack()) {
            (Unpacked::Nan, _) | (_, Unpacked::Nan) => Self::propagate(&[self, other]),

            (Unpacked::Infinite { .. }, Unpacked::Zero { .. })
            | (Unpacked::Zero { .. }, Unpacked::Infinite { .. }) => Self::nan(),
            (Unpacked::Infinite { .. }, _) | (_, Unpacked::Infinite { .. }) => {
                Self::infinite(negative)
            }

            (Unpacked::Zero { .. }, _) | (_, Unpacked::Zero { .. }) => Self::zero(negative),

            (
                Unpacked::Finite {
                    magnitude: left,
                    exponent: left_exponent,
                    ..
                },
                Unpacked::Finite {
                    magnitude: right,
                    exponent: right_exponent,
                    ..
                },
            ) => round(
                negative,
                &(left * right),
                left_exponent + right_exponent,
                false,
                rounding,
            ),
        }
    }

    /// `self / other` under `rounding`: the exact quotient, one rounding. Division is total: a nonzero value over zero is the infinity of the sign, which is what lets `/std/Flt` spell its infinities with it, and `0 / 0` and `∞ / ∞` are invalid.
    pub fn quotient(self, other: Self, rounding: Rounding) -> Self {
        let negative = self.is_negative() != other.is_negative();

        match (self.unpack(), other.unpack()) {
            (Unpacked::Nan, _) | (_, Unpacked::Nan) => Self::propagate(&[self, other]),

            (Unpacked::Infinite { .. }, Unpacked::Infinite { .. })
            | (Unpacked::Zero { .. }, Unpacked::Zero { .. }) => Self::nan(),

            (Unpacked::Infinite { .. }, _) | (_, Unpacked::Zero { .. }) => Self::infinite(negative),
            (Unpacked::Zero { .. }, _) | (_, Unpacked::Infinite { .. }) => Self::zero(negative),

            (
                Unpacked::Finite {
                    magnitude: left,
                    exponent: left_exponent,
                    ..
                },
                Unpacked::Finite {
                    magnitude: right,
                    exponent: right_exponent,
                    ..
                },
            ) => Self::rounded_quotient(
                negative,
                &left,
                &right,
                left_exponent - right_exponent,
                rounding,
            ),
        }
    }

    /// The square root under `rounding`. `sqrt(-0.0)` is `-0.0`; the root of any other negative is invalid.
    pub fn sqrt(self, rounding: Rounding) -> Self {
        match self.unpack() {
            Unpacked::Nan => Self::propagate(&[self]),
            Unpacked::Zero { negative } => Self::zero(negative),
            Unpacked::Infinite { negative: true } => Self::nan(),
            Unpacked::Infinite { negative: false } => self,
            Unpacked::Finite { negative: true, .. } => Self::nan(),
            Unpacked::Finite {
                magnitude,
                exponent,
                ..
            } => {
                // `m · 2^e = (m · 2^b) · 2^2f` with `e = 2f + b`, so the root is `√(m · 2^b) · 2^f` and only the even half leaves the radicand.
                let half = exponent.div_euclid(2);
                let odd = exponent.rem_euclid(2) as u32;

                let width = i64::try_from(magnitude.bits()).expect("a magnitude of binary64 width");
                let scale = ((2 * i64::from(SIGNIFICAND_BITS) + 6 - width - i64::from(odd) + 1)
                    / 2)
                .max(i64::from(half) - i64::from(MIN_EXPONENT) + 1)
                .max(0);
                let scale = u32::try_from(scale).expect("a scale of representable width");

                let radicand = shift_left(&magnitude, odd + 2 * scale);
                let root = radicand.isqrt();
                let sticky = &root * &root != radicand;

                round(false, &root, half - scale as i32, sticky, rounding)
            }
        }
    }

    /// `self · multiplicand + addend` under `rounding`, the exact result rounded once (§5.4.1 fusedMultiplyAdd). A NaN operand answers first; otherwise `0 · ∞` is invalid whatever the addend, and so is an infinite product meeting the infinity of the other sign.
    pub fn fma(self, multiplicand: Self, addend: Self, rounding: Rounding) -> Self {
        if addend.is_nan() {
            return Self::propagate(&[self, multiplicand, addend]);
        }

        let negative = self.is_negative() != multiplicand.is_negative();

        match (self.unpack(), multiplicand.unpack()) {
            (Unpacked::Nan, _) | (_, Unpacked::Nan) => {
                Self::propagate(&[self, multiplicand, addend])
            }

            (Unpacked::Infinite { .. }, Unpacked::Zero { .. })
            | (Unpacked::Zero { .. }, Unpacked::Infinite { .. }) => Self::nan(),
            (Unpacked::Infinite { .. }, _) | (_, Unpacked::Infinite { .. }) => {
                match addend.unpack() {
                    Unpacked::Infinite { negative: other } if other != negative => Self::nan(),
                    _ => Self::infinite(negative),
                }
            }

            // An exact zero product: the sum with the addend is itself exact unless both are zeros, whose sign `sum` settles.
            (Unpacked::Zero { .. }, _) | (_, Unpacked::Zero { .. }) => {
                Self::zero(negative).sum(addend, rounding)
            }

            (
                Unpacked::Finite {
                    magnitude: left,
                    exponent: left_exponent,
                    ..
                },
                Unpacked::Finite {
                    magnitude: right,
                    exponent: right_exponent,
                    ..
                },
            ) => {
                let product = left * right;
                let product_exponent = left_exponent + right_exponent;

                match addend.unpack() {
                    Unpacked::Infinite { negative } => Self::infinite(negative),
                    Unpacked::Zero { .. } | Unpacked::Nan => {
                        round(negative, &product, product_exponent, false, rounding)
                    }
                    Unpacked::Finite {
                        negative: addend_negative,
                        magnitude: addend_magnitude,
                        exponent: addend_exponent,
                    } => Self::exact_sum(
                        (negative, &product, product_exponent),
                        (addend_negative, &addend_magnitude, addend_exponent),
                        rounding,
                    ),
                }
            }
        }
    }

    /// The integral value `self` rounds to under `rounding` (§5.3.1 roundToIntegral): `floor` is `TowardNegative`, `ceil` `TowardPositive`, `trunc` `TowardZero`, `nearest` `TiesToEven` and `round` `TiesToAway`. A zero result keeps the operand's sign, which is why `ceil(-0.5)` is `-0.0`.
    pub fn round_integral(self, rounding: Rounding) -> Self {
        let (negative, magnitude, exponent) = match self.unpack() {
            Unpacked::Nan => return Self::propagate(&[self]),
            // A zero and an infinity are each their own integral part.
            Unpacked::Zero { .. } | Unpacked::Infinite { .. } => return self,
            Unpacked::Finite {
                negative,
                magnitude,
                exponent,
            } => (negative, magnitude, exponent),
        };

        if exponent >= 0 {
            // Already an integer: nothing sits below the point.
            return self;
        }

        let drop = u32::try_from(-exponent).expect("a fractional width of binary64 range");

        let integral = shift_right(&magnitude, drop);
        let fraction = magnitude - shift_left(&integral, drop);
        let half = shift_left(&Natural::one(), drop - 1);

        let integral = match rounding.rounds_up(
            negative,
            fraction.cmp(&half),
            is_odd(&integral),
            !fraction.is_zero(),
        ) {
            true => integral + Natural::one(),
            false => integral,
        };

        // Every integral value a binary64 holds is one binary64 holds exactly, so nothing is dropped a second time and the direction no longer matters.
        round(negative, &integral, 0, false, Rounding::TiesToEven)
    }

    /// The magnitude of `self` with the sign of `other`, bit for bit: a NaN on either side keeps its payload and kind, and a NaN's sign is read like any other (§5.5.1).
    pub fn copysign(self, other: Self) -> Self {
        Self {
            bits: (self.bits & !SIGN_MASK) | (other.bits & SIGN_MASK),
        }
    }

    /// IEEE 754-2019 `minimum`: a NaN operand propagates, and an equal pair answers the negative-signed one, which is what tells the two zeros apart.
    pub fn min(self, other: Self) -> Self {
        match self.compare(other) {
            None => Self::propagate(&[self, other]),
            Some(Ordering::Less) => self,
            Some(Ordering::Greater) => other,
            Some(Ordering::Equal) => match self.is_negative() {
                true => self,
                false => other,
            },
        }
    }

    /// The twin of [`Floating::min`], with an equal pair answering the positive-signed one.
    pub fn max(self, other: Self) -> Self {
        match self.compare(other) {
            None => Self::propagate(&[self, other]),
            Some(Ordering::Greater) => self,
            Some(Ordering::Less) => other,
            Some(Ordering::Equal) => match self.is_negative() {
                true => other,
                false => self,
            },
        }
    }

    /// Numeric comparison — the two zeros equal — or `None` when either operand is a NaN, which is what makes every comparison but `neq` false against one.
    fn compare(self, other: Self) -> Option<Ordering> {
        let signed = |negative: bool, ordering: Ordering| match negative {
            true => ordering.reverse(),
            false => ordering,
        };

        match (self.unpack(), other.unpack()) {
            (Unpacked::Nan, _) | (_, Unpacked::Nan) => None,

            (Unpacked::Zero { .. }, Unpacked::Zero { .. }) => Some(Ordering::Equal),

            (Unpacked::Infinite { negative: left }, Unpacked::Infinite { negative: right }) => {
                Some(right.cmp(&left))
            }
            (Unpacked::Infinite { negative }, _) => Some(signed(negative, Ordering::Greater)),
            (_, Unpacked::Infinite { negative }) => Some(signed(negative, Ordering::Less)),

            (Unpacked::Zero { .. }, Unpacked::Finite { negative, .. }) => {
                Some(signed(negative, Ordering::Less))
            }
            (Unpacked::Finite { negative, .. }, Unpacked::Zero { .. }) => {
                Some(signed(negative, Ordering::Greater))
            }

            (
                Unpacked::Finite {
                    negative: left_negative,
                    magnitude: left,
                    exponent: left_exponent,
                },
                Unpacked::Finite {
                    negative: right_negative,
                    magnitude: right,
                    exponent: right_exponent,
                },
            ) => match left_negative == right_negative {
                false => Some(signed(left_negative, Ordering::Greater)),
                true => {
                    let (left, right, _) =
                        Self::align((&left, left_exponent), (&right, right_exponent));

                    Some(signed(left_negative, left.cmp(&right)))
                }
            },
        }
    }

    pub fn eql(self, other: Self) -> bool {
        self.compare(other) == Some(Ordering::Equal)
    }

    pub fn neq(self, other: Self) -> bool {
        !self.eql(other)
    }

    pub fn lt(self, other: Self) -> bool {
        self.compare(other) == Some(Ordering::Less)
    }

    pub fn le(self, other: Self) -> bool {
        matches!(self.compare(other), Some(Ordering::Less | Ordering::Equal))
    }

    /// The binary64 `value` rounds to under `rounding`, total: rounding is the canonical extension of the embedding, and a magnitude past the largest finite value answers what the direction sends an overflow to.
    pub fn of_natural(value: &Natural, rounding: Rounding) -> Self {
        round(false, value, 0, false, rounding)
    }

    /// [`Floating::of_natural`]'s signed twin.
    pub fn of_integer(value: &Integer, rounding: Rounding) -> Self {
        match Natural::try_from(value) {
            Ok(magnitude) => round(false, &magnitude, 0, false, rounding),
            Err(_) => round(true, &value.magnitude(), 0, false, rounding),
        }
    }

    /// `magnitude · 2^exponent`, negated when `negative`, rounded once under `rounding`: the model's own rounding, over a value given exactly rather than as operands. `/std/Flt/rounded/of_dyadic` is its Curios twin. An exact zero keeps the sign it is given.
    pub fn of_dyadic(
        negative: bool,
        magnitude: &Natural,
        exponent: i32,
        rounding: Rounding,
    ) -> Self {
        round(negative, magnitude, exponent, false, rounding)
    }

    /// The exact natural this truncates toward zero to, refusing outside the domain `/sys/Bound/NonNeg` states — a NaN, an infinity, or a negative value other than `-0.0`. One refusal where there were two: the model decides what the truncation *is*, and no carrier adds a width on top of it.
    ///
    /// Exact and unbounded: `to_natural(3.0e9)` is the natural `3000000000`, which the running program holds as a boxed magnitude.
    pub fn to_natural(self) -> Result<Natural, ScalarTrap> {
        self.truncate_natural().ok_or(ScalarTrap::ConversionRange)
    }

    fn truncate_natural(self) -> Option<Natural> {
        match self.unpack() {
            Unpacked::Nan | Unpacked::Infinite { .. } => None,
            Unpacked::Zero { .. } => Some(Natural::zero()),
            Unpacked::Finite { negative: true, .. } => None,
            Unpacked::Finite {
                magnitude,
                exponent,
                ..
            } => Some(Self::integral_part(&magnitude, exponent)),
        }
    }

    /// The exact integer this truncates toward zero to, refusing a NaN or an infinity — outside the domain `/sys/Bound/Finite` states.
    pub fn to_integer(self) -> Result<Integer, ScalarTrap> {
        self.truncate_integer().ok_or(ScalarTrap::ConversionRange)
    }

    fn truncate_integer(self) -> Option<Integer> {
        match self.unpack() {
            Unpacked::Nan | Unpacked::Infinite { .. } => None,
            Unpacked::Zero { .. } => Some(Integer::from(0u32)),
            Unpacked::Finite {
                negative,
                magnitude,
                exponent,
            } => {
                let magnitude = Integer::from(Self::integral_part(&magnitude, exponent));

                Some(match negative {
                    true => -magnitude,
                    false => magnitude,
                })
            }
        }
    }

    fn integral_part(magnitude: &Natural, exponent: i32) -> Natural {
        match u32::try_from(-exponent) {
            Ok(drop) => shift_right(magnitude, drop),
            Err(_) => shift_left(
                magnitude,
                u32::try_from(exponent).expect("a non-negative exponent"),
            ),
        }
    }

    /// The binary64 nearest `(-1)^negative · digits · 10^exponent`, correctly rounded with ties to even, with the sign applied last so a zero keeps it. This is how the lexer reads a literal, which IEEE's default direction governs.
    ///
    /// The clamps are what keep the exact arithmetic affordable: `digits` spelled in `d` decimal places bounds the value between `10^(d - 1 + exponent)` and `10^(d + exponent)`, which decides underflow and overflow before any power of ten is built. Inside them the numerator and denominator are exact and the quotient settles the rounding — one rounding, at the end, which is the whole difference between this and a chain of float multiplications.
    pub fn of_decimal(negative: bool, digits: &Natural, exponent: i32) -> Self {
        if digits.is_zero() {
            return Self::zero(negative);
        }

        let places = i64::try_from(digits.to_string().len()).expect("a numeral of stated width");
        // Widened so the clamps cannot overflow: the lexer admits any exponent an `i32` holds, and a sum at its ceiling is exactly what the overflow clamp exists to answer, not to panic on.
        let magnitude = places + i64::from(exponent);

        // The value is under `10^magnitude` and at least `10^(magnitude - 1)`. `10^-324 < 2^-1075`, half the least subnormal; `10^309 > 2^1024`, past the rounding threshold above the largest finite value, so a magnitude of 310 is past it.
        if magnitude <= -324 {
            return Self::zero(negative);
        }
        if magnitude >= 310 {
            return Self::infinite(negative);
        }

        let ten = Natural::from(10u32);
        let scale = ten.pow(exponent.unsigned_abs());

        match exponent >= 0 {
            // A non-negative decimal exponent is an exact integer, so there is nothing to divide and one rounding is all of it.
            true => round(negative, &(digits * &scale), 0, false, Rounding::TiesToEven),
            false => Self::rounded_quotient(negative, digits, &scale, 0, Rounding::TiesToEven),
        }
    }

    /// Exact `fmod`: the dividend's sign, never a rounding. `x % inf` is `x`, and an infinite dividend or a zero divisor is invalid.
    fn remainder(self, other: Self) -> Self {
        match (self.unpack(), other.unpack()) {
            (Unpacked::Nan, _) | (_, Unpacked::Nan) => Self::propagate(&[self, other]),
            (Unpacked::Infinite { .. }, _) | (_, Unpacked::Zero { .. }) => Self::nan(),
            (Unpacked::Zero { .. }, _) | (_, Unpacked::Infinite { .. }) => self,

            (
                Unpacked::Finite {
                    negative,
                    magnitude: left,
                    exponent: left_exponent,
                },
                Unpacked::Finite {
                    magnitude: right,
                    exponent: right_exponent,
                    ..
                },
            ) => {
                let (left, right, exponent) =
                    Self::align((&left, left_exponent), (&right, right_exponent));

                // The remainder of two aligned integers is one of them scaled, so it is representable and `round` drops nothing.
                round(
                    negative,
                    &(left % right),
                    exponent,
                    false,
                    Rounding::TiesToEven,
                )
            }
        }
    }
}

/// The sum under the default direction, ties to even.
impl Add for Floating {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        self.sum(other, Rounding::TiesToEven)
    }
}

/// The difference under the default direction, ties to even.
impl Sub for Floating {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        self.difference(other, Rounding::TiesToEven)
    }
}

/// The product under the default direction, ties to even.
impl Mul for Floating {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        self.product(other, Rounding::TiesToEven)
    }
}

/// The quotient under the default direction, ties to even.
impl Div for Floating {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        self.quotient(other, Rounding::TiesToEven)
    }
}

impl Rem for Floating {
    type Output = Self;

    fn rem(self, other: Self) -> Self {
        self.remainder(other)
    }
}

/// The sign bit flipped, and nothing else touched, a NaN included.
impl Neg for Floating {
    type Output = Self;

    fn neg(self) -> Self {
        Self {
            bits: self.bits ^ SIGN_MASK,
        }
    }
}

/// The host's `Debug` float format rather than its `Display`: it keeps a large magnitude short (`1e300`) and a negative zero signed (`-0.0`). A NaN other than the default one prints its pattern, since the host would print every NaN alike. Not a semantics — the surface `Flt/to_str` is `/std`'s own renderer — but what a dump and a report show for a constant.
impl fmt::Display for Floating {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.is_nan() && self.bits != NAN_BITS {
            true => write!(f, "NaN({:#018x})", self.bits),
            false => write!(f, "{:?}", f64::from(*self)),
        }
    }
}

/// The number, not the wrapper around the bits holding it — [`Natural`]'s reason, at the floating carrier: the erased stages render their IR with `{:?}`, and `wonder stage cont` is read by people, so a derived `Floating { bits: 4615626668101337088 }` would put the representation in every dump where `3.75` belongs.
impl fmt::Debug for Floating {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}
