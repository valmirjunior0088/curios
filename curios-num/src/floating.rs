#[cfg(test)]
mod tests;

use {
    crate::{Integer, Natural},
    std::{
        cmp::Ordering,
        fmt,
        ops::{Add, Div, Mul, Neg, Rem, Sub},
    },
};

/// The sign bit, the only bit any of `neg`, `abs` and `copysign` touches.
const SIGN_MASK: u32 = 0x8000_0000;
const EXPONENT_MASK: u32 = 0x7f80_0000;
const MANTISSA_MASK: u32 = 0x007f_ffff;
/// The one NaN. Every constructor canonicalizes to it, which is what makes the derived `Eq` and `Hash` value identity rather than bit identity.
const NAN_BITS: u32 = 0x7fc0_0000;
/// A normal magnitude occupies exactly this many bits — the hidden bit included.
const SIGNIFICAND_BITS: u32 = 24;
/// The hidden bit's weight, which is also the least normal magnitude.
const HIDDEN_BIT: u32 = 1 << 23;
/// The exponent every subnormal has, and the floor no result's exponent goes below.
const MIN_EXPONENT: i32 = -149;
/// What a magnitude's exponent gains to reach its stored field.
const EXPONENT_BIAS: i32 = 150;
/// The field an infinity and a NaN share.
const INFINITE_FIELD: i32 = 255;

/// IEEE 754-2019 binary32 with exactly one NaN, computed exactly over unbounded integers and rounded once.
///
/// The bit pattern is the representation, and the invariant is that *a NaN is `NAN_BITS`* — enforced by [`Floating::from_bits`], which every other constructor routes through. With one NaN, bitwise identity is value identity: `0.0` and `-0.0` are distinct values and stay distinct, `nan` is one value, and the derived `Eq` and `Hash` say exactly that. Terms must be hashable and decidably equal, which IEEE `f32` is not.
///
/// **No operation below calls an `f32` operation.** Every one unpacks its operands to a signed zero, a signed infinity, the NaN, or a `(sign, magnitude, exponent)` triple with the magnitude under `2^24`; computes exactly over [`Natural`]; and packs the result through the single `round` that owns the subnormal grid, the carry renormalization and the overflow to infinity. That is the whole of why a float means the same thing on every host the compiler runs on, and the reason `to_f32` survives at all is rendering and the tests' oracle — never semantics.
///
/// The choices IEEE leaves open are pinned rather than inherited: `min`/`max` propagate a NaN and order `-0.0` below `+0.0`, which is 754-2019's `minimum`/`maximum` and what Wasm mandates; `nearest` is ties-to-even; `rem` is exact `fmod`; `copysign(x, nan)` is `abs(x)`, since the one NaN has no sign to read.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Floating {
    bits: u32,
}

/// What a bit pattern denotes: the four cases binary32 has, with a finite value carried as `(-1)^negative · magnitude · 2^exponent`.
///
/// Written out rather than read off the fields at each use because every operation's special cases are stated over these four, and the arithmetic below is then one clause per pair rather than a mask per line.
#[derive(Debug, Clone)]
enum Unpacked {
    Zero {
        negative: bool,
    },
    Infinite {
        negative: bool,
    },
    Nan,
    /// `magnitude` is nonzero and under `2^24`; `exponent` is at least [`MIN_EXPONENT`].
    Finite {
        negative: bool,
        magnitude: Natural,
        exponent: i32,
    },
}

/// `value · 2^amount`, exact. Shift counts here are bounded by the operands' own bit lengths, which the callers keep in the low hundreds, so a count that does not fit is this module's bug rather than an input's.
fn shift_left(value: &Natural, amount: u32) -> Natural {
    value
        .clone()
        .checked_shl(Natural::from(amount))
        .expect("a shift count that fits")
}

/// `⌊value / 2^amount⌋`, the counterpart of [`shift_left`].
fn shift_right(value: &Natural, amount: u32) -> Natural {
    value
        .clone()
        .checked_shr(Natural::from(amount))
        .expect("a shift count that fits")
}

fn is_odd(value: &Natural) -> bool {
    !(value & &Natural::one()).is_zero()
}

/// The binary32 nearest to `(-1)^negative · (magnitude + ε) · 2^exponent`, round to nearest with ties to even, where `ε` is a residue in `(0, 1)` reported by `sticky` and zero when it is not.
///
/// The one place the format's shape is written down. It owns three things nothing above it repeats: the subnormal grid, since a result below `2^-126` is rounded on the `2^-149` lattice directly from the same magnitude and so is never rounded twice; the carry out of the top significand bit, which renormalizes and which crosses a subnormal into the least normal with no special case, because that encoding already agrees; and the overflow past the largest finite value, which answers the infinity of the sign, as round-to-nearest requires.
///
/// A caller that can lose bits — the divisions and the square root — must hand in an `exponent` low enough that at least one bit is dropped here, which is what makes `sticky` meaningful. The exact operations pass `false` and are normalized instead.
fn round(negative: bool, magnitude: &Natural, exponent: i32, sticky: bool) -> Floating {
    if magnitude.is_zero() {
        // With `exponent` at or below the grid, a residue alone is under half the least subnormal and rounds away.
        return Floating::zero(negative);
    }

    let bits = i64::try_from(magnitude.bits()).expect("a magnitude of representable width");
    let to_significand = bits - i64::from(SIGNIFICAND_BITS);
    let to_grid = i64::from(MIN_EXPONENT) - i64::from(exponent);
    // Past the magnitude's own width every further bit of shift answers zero, so capping keeps the count representable without changing an answer.
    let shift = to_significand.max(to_grid).min(bits + 2);

    if shift <= 0 {
        // Nothing is dropped, so nothing is rounded: normalize instead, left until the significand is full or the exponent reaches the grid.
        let room = u32::try_from(-shift).expect("a negative shift of representable width");
        let headroom = u32::try_from(exponent - MIN_EXPONENT).expect("an exponent above the grid");
        let left = room.min(headroom);

        debug_assert!(!sticky, "a residue below a magnitude that loses no bits");

        return Floating::encode(
            negative,
            &shift_left(magnitude, left),
            exponent - left as i32,
        );
    }

    let shift = u32::try_from(shift).expect("a positive shift of representable width");

    let kept = shift_right(magnitude, shift);
    let dropped = magnitude - &shift_left(&kept, shift);
    let half = shift_left(&Natural::one(), shift - 1);

    let up = match dropped.cmp(&half) {
        Ordering::Greater => true,
        Ordering::Less => false,
        // A residue puts the exact value above the midpoint, so only a genuine tie is broken to even.
        Ordering::Equal => sticky || is_odd(&kept),
    };

    let kept = match up {
        true => kept + Natural::one(),
        false => kept,
    };
    let exponent = exponent + shift as i32;

    // Rounding up can carry out of the significand: `0xffffff` becomes `0x1000000`, one bit wider.
    match kept.bits() > u64::from(SIGNIFICAND_BITS) {
        true => Floating::encode(negative, &shift_right(&kept, 1), exponent + 1),
        false => Floating::encode(negative, &kept, exponent),
    }
}

impl Floating {
    /// Adopt `bits`, canonicalizing every NaN pattern to the one NaN — the invariant the whole type rests on, and what makes `of_le_bytes` of any NaN pattern the NaN.
    pub fn from_bits(bits: u32) -> Self {
        let exponent_field = (bits & EXPONENT_MASK) >> 23;
        let mantissa = bits & MANTISSA_MASK;

        match exponent_field == 0xff && mantissa != 0 {
            true => Self { bits: NAN_BITS },
            false => Self { bits },
        }
    }

    /// Capture `v`'s bit pattern, canonicalizing a NaN. Not a semantics: this is how a test's oracle and a literal's host parse hand a value in, and [`Floating::to_f32`] is how a printer reads one out.
    pub fn from_f32(v: f32) -> Self {
        Self::from_bits(v.to_bits())
    }

    pub fn to_f32(self) -> f32 {
        f32::from_bits(self.bits)
    }

    /// The stored bit pattern — the identity `Eq` and `Hash` are derived over, for a caller keying on it.
    pub fn to_bits(self) -> u32 {
        self.bits
    }

    /// The one NaN.
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

    pub fn is_nan(self) -> bool {
        self.bits == NAN_BITS
    }

    /// Whether this is a *number*: finite, so neither infinity and not the NaN. The same reading `/syn/Flt/Finite` states, and what the surface lexer refuses a literal for.
    pub fn is_finite(self) -> bool {
        self.bits & EXPONENT_MASK != EXPONENT_MASK
    }

    fn sign_bit(negative: bool) -> u32 {
        match negative {
            true => SIGN_MASK,
            false => 0,
        }
    }

    fn is_negative(self) -> bool {
        self.bits & SIGN_MASK != 0
    }

    /// Pack a finite value whose `magnitude` is either full-width (a normal) or sits at [`MIN_EXPONENT`] (a subnormal), answering the infinity of the sign past the largest finite value.
    fn encode(negative: bool, magnitude: &Natural, exponent: i32) -> Self {
        let Some(magnitude) = magnitude.to_u32() else {
            return Self::infinite(negative);
        };

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

        match field >= INFINITE_FIELD {
            true => Self::infinite(negative),
            false => Self {
                bits: Self::sign_bit(negative) | ((field as u32) << 23) | (magnitude - HIDDEN_BIT),
            },
        }
    }

    fn unpack(self) -> Unpacked {
        let negative = self.is_negative();
        let field = ((self.bits & EXPONENT_MASK) >> 23) as i32;
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
                u32::try_from(own - exponent).expect("an exponent difference of binary32 width"),
            )
        };

        (raise(left), raise(right), exponent)
    }

    /// The correctly rounded `(-1)^negative · (numerator / denominator) · 2^exponent`, with a nonzero denominator.
    ///
    /// Scaling is chosen so the integer quotient carries more bits than the significand needs *and* lands at an exponent below the subnormal grid, which is what leaves [`round`] a bit to drop and so a meaning for the sticky flag. The remainder is that flag: it says the exact value sits above the quotient without saying by how much, which is all rounding to nearest can use.
    fn rounded_quotient(
        negative: bool,
        numerator: &Natural,
        denominator: &Natural,
        exponent: i32,
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
        let quotient = scaled.clone() / denominator.clone();
        let remainder = scaled % denominator.clone();

        round(
            negative,
            &quotient,
            exponent - scale as i32,
            !remainder.is_zero(),
        )
    }

    pub fn abs(self) -> Self {
        match self.is_nan() {
            true => self,
            false => Self {
                bits: self.bits & !SIGN_MASK,
            },
        }
    }

    /// `sqrt(-0.0)` is `-0.0`; the root of any other negative is the NaN.
    pub fn sqrt(self) -> Self {
        match self.unpack() {
            Unpacked::Nan => Self::nan(),
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

                let width = i64::try_from(magnitude.bits()).expect("a magnitude of binary32 width");
                let scale = ((2 * i64::from(SIGNIFICAND_BITS) + 6 - width - i64::from(odd) + 1)
                    / 2)
                .max(i64::from(half) - i64::from(MIN_EXPONENT) + 1)
                .max(0);
                let scale = u32::try_from(scale).expect("a scale of representable width");

                let radicand = shift_left(&magnitude, odd + 2 * scale);
                let root = radicand.isqrt();
                let sticky = &root * &root != radicand;

                round(false, &root, half - scale as i32, sticky)
            }
        }
    }

    pub fn floor(self) -> Self {
        self.to_integral(|negative, fraction| negative && !fraction.is_zero())
    }

    pub fn ceil(self) -> Self {
        self.to_integral(|negative, fraction| !negative && !fraction.is_zero())
    }

    pub fn trunc(self) -> Self {
        self.to_integral(|_, _| false)
    }

    /// Ties to even, the rounding the format itself uses.
    pub fn nearest(self) -> Self {
        self.to_integral_with(|integral, fraction, half| match fraction.cmp(half) {
            Ordering::Greater => true,
            Ordering::Less => false,
            Ordering::Equal => is_odd(integral),
        })
    }

    /// Truncate toward zero, then let `away` decide whether the magnitude steps up by one — the shared body of the four integral roundings. A zero result keeps the operand's sign, which is why `ceil(-0.5)` is `-0.0`.
    fn to_integral(self, away: impl FnOnce(bool, &Natural) -> bool) -> Self {
        self.to_integral_with(|_, fraction, _| away(self.is_negative(), fraction))
    }

    fn to_integral_with(self, away: impl FnOnce(&Natural, &Natural, &Natural) -> bool) -> Self {
        let Unpacked::Finite {
            negative,
            magnitude,
            exponent,
        } = self.unpack()
        else {
            // A zero, an infinity and the NaN are each their own integral part.
            return self;
        };

        if exponent >= 0 {
            // Already an integer: nothing sits below the point.
            return self;
        }

        let drop = u32::try_from(-exponent).expect("a fractional width of binary32 range");

        let integral = shift_right(&magnitude, drop);
        let fraction = magnitude - shift_left(&integral, drop);
        let half = shift_left(&Natural::one(), drop - 1);

        let integral = match away(&integral, &fraction, &half) {
            true => integral + Natural::one(),
            false => integral,
        };

        // Every integral value a binary32 holds is one binary32 holds exactly, so nothing is dropped a second time.
        round(negative, &integral, 0, false)
    }

    /// The sign of `other`, on the magnitude of `self`. The one NaN has no sign to read, so a NaN sign operand leaves the magnitude alone.
    pub fn copysign(self, other: Self) -> Self {
        match (self.is_nan(), other.is_nan()) {
            (true, _) => Self::nan(),
            (false, true) => self.abs(),
            (false, false) => Self {
                bits: (self.bits & !SIGN_MASK) | (other.bits & SIGN_MASK),
            },
        }
    }

    /// IEEE 754-2019 `minimum`: a NaN operand propagates, and an equal pair answers the negative-signed one, which is what tells the two zeros apart.
    pub fn min(self, other: Self) -> Self {
        match self.compare(other) {
            None => Self::nan(),
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
            None => Self::nan(),
            Some(Ordering::Greater) => self,
            Some(Ordering::Less) => other,
            Some(Ordering::Equal) => match self.is_negative() {
                true => other,
                false => self,
            },
        }
    }

    /// Numeric comparison — the two zeros equal — or `None` when either operand is the NaN, which is what makes every comparison but `neq` false against one.
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

    pub fn gt(self, other: Self) -> bool {
        self.compare(other) == Some(Ordering::Greater)
    }

    pub fn le(self, other: Self) -> bool {
        matches!(self.compare(other), Some(Ordering::Less | Ordering::Equal))
    }

    pub fn ge(self, other: Self) -> bool {
        matches!(
            self.compare(other),
            Some(Ordering::Greater | Ordering::Equal)
        )
    }

    /// The correctly rounded binary32 nearest `value`, total: rounding is the canonical extension of the embedding, and a magnitude past the largest finite value answers `+inf`.
    pub fn of_natural(value: &Natural) -> Self {
        round(false, value, 0, false)
    }

    /// [`Floating::of_natural`]'s signed twin.
    pub fn of_integer(value: &Integer) -> Self {
        match value.to_natural() {
            Some(magnitude) => round(false, &magnitude, 0, false),
            None => {
                let magnitude = (-value.clone())
                    .to_natural()
                    .expect("the negation of a negative integer is a natural");

                round(true, &magnitude, 0, false)
            }
        }
    }

    /// The exact natural this truncates toward zero to, or `None` outside the domain `/syn/Flt/NonNeg` states — a NaN, an infinity, or a negative value other than `-0.0`.
    ///
    /// Exact and unbounded: `to_natural(3.0e9)` is the natural `3000000000`, which no runtime carrier holds and which is refused downstream exactly as an overflowing `Nat` is, rather than being bent to fit here.
    pub fn to_natural(self) -> Option<Natural> {
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

    /// The exact integer this truncates toward zero to, or `None` on a NaN or an infinity — the domain `/syn/Flt/Finite` states.
    pub fn to_integer(self) -> Option<Integer> {
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

    /// The binary32 nearest `(-1)^negative · digits · 10^exponent`, correctly rounded, with the sign applied last so a zero keeps it.
    ///
    /// The clamps are what keep the exact arithmetic affordable: `digits` spelled in `d` decimal places bounds the value between `10^(d - 1 + exponent)` and `10^(d + exponent)`, which decides underflow and overflow before any power of ten is built. Inside them the numerator and denominator are exact and the quotient settles the rounding — one rounding, at the end, which is the whole difference between this and a chain of float multiplications.
    pub fn of_decimal(negative: bool, digits: &Natural, exponent: i32) -> Self {
        if digits.is_zero() {
            return Self::zero(negative);
        }

        let places = i32::try_from(digits.to_string().len()).expect("a numeral of stated width");

        // `10^-46 < 2^-150`, half the least subnormal; `10^39 > 2^128`, past the rounding threshold above the largest finite value.
        if places + exponent <= -46 {
            return Self::zero(negative);
        }
        if places - 1 + exponent >= 39 {
            return Self::infinite(negative);
        }

        let ten = Natural::from(10u32);
        let scale = ten.pow(exponent.unsigned_abs());

        match exponent >= 0 {
            // A non-negative decimal exponent is an exact integer, so there is nothing to divide and one rounding is all of it.
            true => round(negative, &(digits * &scale), 0, false),
            false => Self::rounded_quotient(negative, digits, &scale, 0),
        }
    }

    /// The exact sum, one rounding. An exact zero sum takes `+0.0`, the IEEE rule under round to nearest.
    fn sum(self, other: Self) -> Self {
        match (self.unpack(), other.unpack()) {
            (Unpacked::Nan, _) | (_, Unpacked::Nan) => Self::nan(),

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
                // Two zeros keep a shared sign and are `+0.0` otherwise, which is `(+0) + (-0)`.
                Self::zero(left && right)
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
            ) => {
                let (left, right, exponent) =
                    Self::align((&left, left_exponent), (&right, right_exponent));

                match left_negative == right_negative {
                    true => round(left_negative, &(left + right), exponent, false),
                    false => match left.cmp(&right) {
                        Ordering::Equal => Self::zero(false),
                        Ordering::Greater => round(left_negative, &(left - right), exponent, false),
                        Ordering::Less => round(right_negative, &(right - left), exponent, false),
                    },
                }
            }
        }
    }

    /// The exact product, one rounding.
    fn product(self, other: Self) -> Self {
        let negative = self.is_negative() != other.is_negative();

        match (self.unpack(), other.unpack()) {
            (Unpacked::Nan, _) | (_, Unpacked::Nan) => Self::nan(),

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
            ),
        }
    }

    /// The exact quotient, one rounding. Division is total: an IEEE infinity is a value rather than a failure, which is what lets `/std/Flt` spell its infinities with it.
    fn quotient(self, other: Self) -> Self {
        let negative = self.is_negative() != other.is_negative();

        match (self.unpack(), other.unpack()) {
            (Unpacked::Nan, _) | (_, Unpacked::Nan) => Self::nan(),

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
            ) => Self::rounded_quotient(negative, &left, &right, left_exponent - right_exponent),
        }
    }

    /// Exact `fmod`: the dividend's sign, never a rounding. `x % inf` is `x`, and the NaN answers an infinite dividend or a zero divisor.
    fn remainder(self, other: Self) -> Self {
        match (self.unpack(), other.unpack()) {
            (Unpacked::Nan, _) | (_, Unpacked::Nan) => Self::nan(),
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
                round(negative, &(left % right), exponent, false)
            }
        }
    }
}

impl Add for Floating {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        self.sum(other)
    }
}

impl Sub for Floating {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        self.sum(-other)
    }
}

impl Mul for Floating {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        self.product(other)
    }
}

impl Div for Floating {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        self.quotient(other)
    }
}

impl Rem for Floating {
    type Output = Self;

    fn rem(self, other: Self) -> Self {
        self.remainder(other)
    }
}

impl Neg for Floating {
    type Output = Self;

    fn neg(self) -> Self {
        match self.is_nan() {
            true => self,
            false => Self {
                bits: self.bits ^ SIGN_MASK,
            },
        }
    }
}

impl fmt::Display for Floating {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_f32())
    }
}
