//! Every rounding direction, executed against the model: the emitted helpers that compute what Wasm has no instruction for, held to `curios_num::Floating` bit for bit.

use {
    crate::tests::run,
    curios_num::{Floating, Integer, Natural, Rounding},
};

/// The operand grid: both zeros and both infinities, the subnormal ends, the normal ends and the top power of two, odd and even significands and a value a step below one, halfway integers for the ties, a thirds and a tenth whose products and quotients are inexact, extremes whose products overflow and underflow, and two quiet NaNs with payloads of either sign.
const OPERANDS: [u64; 27] = [
    0x0000_0000_0000_0000,
    0x8000_0000_0000_0000,
    0x3ff0_0000_0000_0000,
    0xbff0_0000_0000_0000,
    0x3fb9_9999_9999_999a,
    0xbfb9_9999_9999_999a,
    0x4008_0000_0000_0000,
    0x3fd5_5555_5555_5555,
    0x0000_0000_0000_0001,
    0x8000_0000_0000_0001,
    0x0010_0000_0000_0000,
    0x000f_ffff_ffff_ffff,
    0x7fef_ffff_ffff_ffff,
    0xffef_ffff_ffff_ffff,
    0x7fe0_0000_0000_0000,
    0x4340_0000_0000_0000,
    0x3ff0_0000_0000_0001,
    0x3fef_ffff_ffff_ffff,
    0x3ff8_0000_0000_0000,
    0x4004_0000_0000_0000,
    0x401c_0000_0000_0000,
    0x7e37_e43c_8800_759c,
    0x01a5_6e1f_c2f8_f359,
    0x7ff0_0000_0000_0000,
    0xfff0_0000_0000_0000,
    0x7ff8_0000_0000_0001,
    0xfff8_0000_0000_0012,
];

/// Naturals whose conversion rounds: past `2^53` at a tie and either side of one, past 64 bits where the conversion runs through a boxed magnitude, and past the largest finite float.
const NATURALS: [&str; 7] = [
    "9007199254740993",
    "9007199254740995",
    "18446744073709551617",
    "36893488147419103231",
    "340282366920938463463374607431768211457",
    "12345678901234567890123456789",
    "179769313486231580793728971405303415079934132710037826936173778980444968292764750946649017977587207096330286416692887910946555547851940402630657488671505820681908902000708383676273854845817711531764475730270069855571366959622842914819860834936475292719074168444365510704342711559699508093042880177904174497792",
];

/// The addends a fused multiply-add is taken over at each pair: the two zeros, one, a tiny value, and the negated rounded product, whose sum with the exact product is its rounding error — the case a fused operation exists for.
fn addends(a: Floating, b: Floating) -> [Floating; 5] {
    [
        Floating::from(0.0),
        Floating::from(-0.0),
        Floating::from(1.0),
        Floating::from(1.0e-300),
        -(a * b),
    ]
}

fn expected() -> Vec<u8> {
    let operands = OPERANDS.map(Floating::from_bits);
    let mut out = Vec::new();
    let mut push = |f: Floating| out.extend_from_slice(&f.to_bits().to_le_bytes());

    for rounding in Rounding::ALL {
        for &a in &operands {
            for &b in &operands {
                push(a.sum(b, rounding));
                push(a.difference(b, rounding));
                push(a.product(b, rounding));
                push(a.quotient(b, rounding));
                for c in addends(a, b) {
                    push(a.fma(b, c, rounding));
                }
            }
        }
        for &a in &operands {
            push(a.sqrt(rounding));
            push(a.round_integral(rounding));
        }
        for n in NATURALS {
            let natural = Natural::parse_bytes(n.as_bytes(), 10).expect("a numeral");
            push(Floating::of_natural(&natural, rounding));
            push(Floating::of_integer(&-Integer::from(natural), rounding));
        }
    }

    out
}

/// Every rounded operation in every direction, over runtime operands, against the model. The operands are scaled by a runtime `1.0`, which is exact and keeps a quiet NaN, so the whole grid runs in the emitted helpers rather than folding; the conversions add a runtime zero for the same reason, one of them through the boxed magnitude.
#[test]
fn every_direction_executes_as_the_model_rounds() {
    let spell = |bits: u64| {
        let bytes = bits
            .to_le_bytes()
            .iter()
            .map(|byte| format!("0x{byte:02X}"))
            .collect::<Vec<_>>()
            .join(", ");
        format!("Flt/of_le_bytes(x[{bytes}])")
    };
    let operands = OPERANDS.map(spell).join(",\n            ");
    let naturals = NATURALS.join(",\n            ");
    let source = format!(
        r#"
        use /std/{{Nat, Int, Flt, Bytes, List, Io, Dyadic}};
        use /std/Flt/{{Rounding, rounded}};
        let one = Nat/to_flt(Bytes/len(/std/rand/bytes(3)!)) / +3.0;
        let zero = Bytes/len(/std/rand/bytes(0)!);
        let operands = List/map([
            {operands},
        ], (v) => v * one);
        let naturals: List(Nat) = List/map([
            {naturals},
        ], (n) => n + zero);
        let addends(a: Flt, b: Flt) -> List(Flt) = [+0.0, -0.0, +1.0, +1.0e-300, Flt/neg(a * b)];
        let direction(r: Rounding) -> List(Flt) =
            let pairs = List/concat_map(operands, (a) => List/concat_map(operands, (b) => [
                rounded/add(r, a, b),
                rounded/sub(r, a, b),
                rounded/mul(r, a, b),
                rounded/div(r, a, b),
                ..List/map(addends(a, b), (c) => rounded/fma(r, a, b, c)),
            ]));
            let unary = List/concat_map(operands, (a) => [rounded/sqrt(r, a), rounded/to_integral(r, a)]);
            let conversions = List/concat_map(naturals, (n) => [
                rounded/of_nat(r, n),
                rounded/of_int(r, Int/sub(+0, Nat/to_int(n))),
            ]);
            [..pairs, ..unary, ..conversions];
        let results = List/concat_map([
            Rounding/ties_to_even(),
            Rounding/ties_to_away(),
            Rounding/toward_zero(),
            Rounding/toward_positive(),
            Rounding/toward_negative(),
        ], direction);
        let _ = Io/write(Io/stdout, Bytes/join(x[], List/map(results, Flt/to_le_bytes)))!;
        Io/pure(())
        "#
    );

    let actual = run(&source);
    let expected = expected();
    let values = |bytes: &[u8]| {
        bytes
            .chunks(8)
            .map(|chunk| u64::from_le_bytes(chunk.try_into().expect("eight bytes")))
            .collect::<Vec<_>>()
    };
    let (actual, expected) = (values(&actual), values(&expected));

    assert_eq!(actual.len(), expected.len(), "one result per case");
    let mismatches = actual
        .iter()
        .zip(&expected)
        .enumerate()
        .filter(|(_, (a, e))| a != e)
        .map(|(index, (a, e))| format!("case {index}: executed {a:#018x}, model {e:#018x}"))
        .collect::<Vec<_>>();
    assert!(
        mismatches.is_empty(),
        "{} mismatches:\n{}",
        mismatches.len(),
        mismatches
            .iter()
            .take(40)
            .cloned()
            .collect::<Vec<_>>()
            .join("\n")
    );
}

/// Magnitudes for the exact rounding: small and odd, either side of the significand's width, a tie at it, and one past 64 bits.
const MAGNITUDES: [&str; 7] = [
    "1",
    "3",
    "9007199254740991",
    "9007199254740993",
    "18014398509481987",
    "36893488147419103233",
    "0",
];

/// Exponents from below the subnormal floor through the grid and the normal range to past the overflow threshold.
const EXPONENTS: [i32; 11] = [
    -1200, -1127, -1075, -1074, -1023, -60, 0, 60, 969, 971, 1000,
];

/// `Flt/rounded/of_dyadic` — the Curios rounding every `/std` operation in the exact layer goes through — executed against the model's single rounding, every direction, both signs, over the grid above. The magnitude gains a runtime zero, so the rounding runs in the emitted program rather than folding.
#[test]
fn the_exact_rounding_executes_as_the_model_rounds() {
    let magnitudes = MAGNITUDES.join(", ");
    let exponents = EXPONENTS.map(|e| format!("{e:+}")).join(", ");
    let source = format!(
        r#"
        use /std/{{Nat, Int, Flt, Bytes, List, Io, Dyadic}};
        use /std/Flt/{{Rounding, rounded}};
        let zero = Nat/to_int(Bytes/len(/std/rand/bytes(0)!));
        let magnitudes: List(Int) = List/map([{magnitudes}], (m) => Nat/to_int(m) + zero);
        let exponents: List(Int) = [{exponents}];
        let direction(r: Rounding) -> List(Flt) =
            List/concat_map(magnitudes, (m) => List/concat_map(exponents, (e) => [
                rounded/of_dyadic(r, Dyadic {{ mantissa = m, exponent = e }}),
                rounded/of_dyadic(r, Dyadic {{ mantissa = Int/sub(+0, m), exponent = e }}),
            ]));
        let results = List/concat_map([
            Rounding/ties_to_even(),
            Rounding/ties_to_away(),
            Rounding/toward_zero(),
            Rounding/toward_positive(),
            Rounding/toward_negative(),
        ], direction);
        let _ = Io/write(Io/stdout, Bytes/join(x[], List/map(results, Flt/to_le_bytes)))!;
        Io/pure(())
        "#
    );

    let mut expected = Vec::new();
    for rounding in Rounding::ALL {
        for m in MAGNITUDES {
            let magnitude = Natural::parse_bytes(m.as_bytes(), 10).expect("a numeral");
            for e in EXPONENTS {
                for negative in [false, true] {
                    // `Int/sub(+0, m)` is `+0` for a zero magnitude, which keeps no sign; the rounding of an exact zero is `+0.0`.
                    let negative = negative && !magnitude.is_zero();
                    let value = Floating::of_dyadic(negative, &magnitude, e, rounding);
                    expected.push(value.to_bits());
                }
            }
        }
    }

    let actual = run(&source)
        .chunks(8)
        .map(|chunk| u64::from_le_bytes(chunk.try_into().expect("eight bytes")))
        .collect::<Vec<_>>();

    assert_eq!(actual.len(), expected.len(), "one result per case");
    for (index, (a, e)) in actual.iter().zip(&expected).enumerate() {
        assert_eq!(a, e, "case {index}: executed {a:#018x}, model {e:#018x}");
    }
}

/// A number as an exact rational, `numerator / denominator` with a positive denominator: the flags oracle's value, computed with no rounding anywhere, so what it concludes reads nothing of the code under test.
#[derive(Clone)]
struct Exact {
    numerator: Integer,
    denominator: Integer,
}

impl Exact {
    fn integer(value: Integer) -> Self {
        Self {
            numerator: value,
            denominator: Integer::from(1u32),
        }
    }

    fn power_of_two(exponent: i32) -> Self {
        let power = Integer::from(
            Natural::one()
                .shl_within(&Natural::from(exponent.unsigned_abs()), u64::MAX)
                .expect("a shift that fits"),
        );
        match exponent >= 0 {
            true => Self::integer(power),
            false => Self {
                numerator: Integer::from(1u32),
                denominator: power,
            },
        }
    }

    /// A finite float's value, `None` for an infinity or a NaN.
    fn of(value: Floating) -> Option<Self> {
        let bits = value.to_bits();
        let field = ((bits >> 52) & 0x7ff) as i32;
        let fraction = bits & 0x000f_ffff_ffff_ffff;
        let (magnitude, exponent) = match field {
            0x7ff => return None,
            0 => (fraction, -1074),
            _ => (fraction | (1 << 52), field - 1075),
        };
        let magnitude = Self::integer(Integer::from(Natural::from(magnitude)));
        let magnitude = magnitude.mul(&Self::power_of_two(exponent));
        Some(match bits >> 63 == 1 {
            true => magnitude.negated(),
            false => magnitude,
        })
    }

    fn negated(&self) -> Self {
        Self {
            numerator: -self.numerator.clone(),
            denominator: self.denominator.clone(),
        }
    }

    fn abs(&self) -> Self {
        Self {
            numerator: Integer::from(self.numerator.magnitude()),
            denominator: self.denominator.clone(),
        }
    }

    fn add(&self, other: &Self) -> Self {
        Self {
            numerator: self.numerator.clone() * other.denominator.clone()
                + other.numerator.clone() * self.denominator.clone(),
            denominator: self.denominator.clone() * other.denominator.clone(),
        }
    }

    fn mul(&self, other: &Self) -> Self {
        Self {
            numerator: self.numerator.clone() * other.numerator.clone(),
            denominator: self.denominator.clone() * other.denominator.clone(),
        }
    }

    /// `self / other`, `other` nonzero.
    fn div(&self, other: &Self) -> Self {
        let sign = match other.numerator < Integer::from(0u32) {
            true => Integer::from(-1i32),
            false => Integer::from(1u32),
        };
        Self {
            numerator: self.numerator.clone() * other.denominator.clone() * sign,
            denominator: self.denominator.clone() * Integer::from(other.numerator.magnitude()),
        }
    }

    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.numerator.clone() * other.denominator.clone())
            .cmp(&(other.numerator.clone() * self.denominator.clone()))
    }

    fn is_zero(&self) -> bool {
        self.numerator.is_zero()
    }
}

const INVALID: u8 = 16;
const DIVISION_BY_ZERO: u8 = 8;
const OVERFLOW: u8 = 4;
const UNDERFLOW: u8 = 2;
const INEXACT: u8 = 1;

/// The exceptions rounding the exact value `exact` to `result` in `rounding` raises, by IEEE's definitions read directly. Overflow: the value rounded with an unbounded exponent exceeds the largest finite one — which to nearest is the value reaching the midpoint between it and `2^1024`, toward the infinity of its own sign is exceeding it at all, and otherwise is reaching `2^1024` (§7.4). Inexact: the result's value differs, or it overflowed. Underflow: a nonzero value below `2^-1022` that is also inexact (§7.5).
fn raised_by_rounding(rounding: Rounding, exact: &Exact, result: Floating) -> u8 {
    let magnitude = exact.abs();
    let largest = Exact::of(Floating::from(f64::MAX)).expect("finite");
    let ceiling = Exact::power_of_two(1024);
    let midpoint = ceiling.add(&Exact::power_of_two(970).negated());
    let negative = exact.numerator < Integer::from(0u32);
    let overflow = match rounding {
        Rounding::TiesToEven | Rounding::TiesToAway => magnitude.cmp(&midpoint).is_ge(),
        Rounding::TowardPositive if !negative => magnitude.cmp(&largest).is_gt(),
        Rounding::TowardNegative if negative => magnitude.cmp(&largest).is_gt(),
        _ => magnitude.cmp(&ceiling).is_ge(),
    };
    let inexact = overflow || Exact::of(result).is_none_or(|value| value.cmp(exact).is_ne());
    let tiny = !exact.is_zero() && magnitude.cmp(&Exact::power_of_two(-1022)).is_lt();

    u8::from(overflow) * OVERFLOW
        + u8::from(tiny && inexact) * UNDERFLOW
        + u8::from(inexact) * INEXACT
}

fn is_signaling(f: Floating) -> bool {
    f.is_nan() && f.to_bits() & (1 << 51) == 0
}

fn is_infinite(f: Floating) -> bool {
    !f.is_nan() && !f.is_finite()
}

fn is_zero(f: Floating) -> bool {
    f.to_bits() << 1 == 0
}

/// The scalings `scale` is taken over: far below the subnormal floor, a small step each way, and far past the overflow threshold.
const SCALINGS: [i32; 4] = [-1100, -60, 60, 1100];

/// Every exception `signals` computes, over the operand grid and a signaling NaN, in every direction, against the oracle above. The operands run through the emitted program as the other tables do; the signaling NaN is assembled from bytes with a runtime byte, since scaling one by `1.0` would quiet it.
#[test]
fn every_exception_is_raised_as_ieee_defines_it() {
    let spell = |bits: u64| {
        let bytes = bits
            .to_le_bytes()
            .iter()
            .map(|byte| format!("0x{byte:02X}"))
            .collect::<Vec<_>>()
            .join(", ");
        format!("Flt/of_le_bytes(x[{bytes}])")
    };
    let operands = OPERANDS.map(spell).join(",\n            ");
    let scalings = SCALINGS.map(|n| format!("{n:+}")).join(", ");
    let source = format!(
        r#"
        use /std/{{Nat, Int, Flt, Bytes, Byte, List, Io, Bool}};
        use /std/Flt/{{Rounding, Exceptions, signals}};
        let one = Nat/to_flt(Bytes/len(/std/rand/bytes(3)!)) / +3.0;
        let tick = Nat/to_byte(Bytes/len(/std/rand/bytes(1)!) % 256);
        let signaling = Flt/of_le_bytes(x[tick, 0x00, 0x00, 0x00, 0x00, 0x00, 0xF0, 0x7F]);
        let operands = [..List/map([
            {operands},
        ], (v) => v * one), signaling];
        let scalings: List(Int) = [{scalings}];
        let flag(set: Bool, weight: Nat) -> Nat = match set | true => weight | false => 0 end;
        let code(e: Exceptions) -> Byte =
            Nat/to_byte((flag(e.invalid, 16) + flag(e.division_by_zero, 8) + flag(e.overflow, 4)
                + flag(e.underflow, 2) + flag(e.inexact, 1)) % 256);
        let addends(a: Flt, b: Flt) -> List(Flt) = [+0.0, +1.0, +1.0e-300, Flt/neg(a * b)];
        let direction(r: Rounding) -> List(Exceptions) =
            let pairs = List/concat_map(operands, (a) => List/concat_map(operands, (b) => [
                signals/add(r, a, b),
                signals/sub(r, a, b),
                signals/mul(r, a, b),
                signals/div(r, a, b),
                ..List/map(addends(a, b), (c) => signals/fma(r, a, b, c)),
            ]));
            let unary = List/concat_map(operands, (a) => [
                signals/sqrt(r, a),
                signals/to_integral_exact(r, a),
                ..List/map(scalings, (n) => signals/scale(r, a, n)),
            ]);
            [..pairs, ..unary];
        let results = List/concat_map([
            Rounding/ties_to_even(),
            Rounding/ties_to_away(),
            Rounding/toward_zero(),
            Rounding/toward_positive(),
            Rounding/toward_negative(),
        ], direction);
        let _ = Io/write(Io/stdout, Bytes/flatten(List/map(results, (e) => x[code(e)])))!;
        Io/pure(())
        "#
    );

    let mut operands = OPERANDS.map(Floating::from_bits).to_vec();
    operands.push(Floating::from_bits(0x7ff0_0000_0000_0001));
    let numeric = |rounding: Rounding, exact: Option<Exact>, result: Floating| {
        exact.map_or(0, |exact| raised_by_rounding(rounding, &exact, result))
    };

    let mut expected = Vec::new();
    for rounding in Rounding::ALL {
        for &a in &operands {
            for &b in &operands {
                let (x, y) = (Exact::of(a), Exact::of(b));
                let nan = a.is_nan() || b.is_nan();
                let signaling = is_signaling(a) || is_signaling(b);

                // A sum and a difference.
                for b in [b, -b] {
                    expected.push(match () {
                        _ if signaling => INVALID,
                        _ if nan => 0,
                        _ if is_infinite(a) || is_infinite(b) => {
                            u8::from(a.sum(b, rounding).is_nan()) * INVALID
                        }
                        _ => numeric(
                            rounding,
                            Some(x.clone().unwrap().add(&Exact::of(b).unwrap())),
                            a.sum(b, rounding),
                        ),
                    });
                }
                expected.push(match () {
                    _ if signaling => INVALID,
                    _ if nan => 0,
                    _ if (is_infinite(a) && is_zero(b)) || (is_zero(a) && is_infinite(b)) => {
                        INVALID
                    }
                    _ if is_infinite(a) || is_infinite(b) => 0,
                    _ => numeric(
                        rounding,
                        Some(x.clone().unwrap().mul(y.as_ref().unwrap())),
                        a.product(b, rounding),
                    ),
                });
                expected.push(match () {
                    _ if signaling => INVALID,
                    _ if nan => 0,
                    _ if (is_zero(a) && is_zero(b)) || (is_infinite(a) && is_infinite(b)) => {
                        INVALID
                    }
                    // Only a finite dividend: `∞ / 0` is an exact infinity no finite operand divided into (§7.3).
                    _ if is_zero(b) && !is_infinite(a) => DIVISION_BY_ZERO,
                    _ if is_infinite(a) || is_infinite(b) => 0,
                    _ => numeric(
                        rounding,
                        Some(x.clone().unwrap().div(y.as_ref().unwrap())),
                        a.quotient(b, rounding),
                    ),
                });
                for c in [
                    Floating::from(0.0),
                    Floating::from(1.0),
                    Floating::from(1.0e-300),
                    -(a * b),
                ] {
                    let zero_times_infinity =
                        (is_infinite(a) && is_zero(b)) || (is_zero(a) && is_infinite(b));
                    expected.push(match () {
                        _ if signaling || is_signaling(c) => INVALID,
                        _ if zero_times_infinity && !c.is_nan() => INVALID,
                        _ if nan || c.is_nan() => 0,
                        _ if is_infinite(a) || is_infinite(b) || is_infinite(c) => {
                            u8::from(a.fma(b, c, rounding).is_nan()) * INVALID
                        }
                        _ => numeric(
                            rounding,
                            Some(
                                x.clone()
                                    .unwrap()
                                    .mul(y.as_ref().unwrap())
                                    .add(&Exact::of(c).unwrap()),
                            ),
                            a.fma(b, c, rounding),
                        ),
                    });
                }
            }
        }
        for &a in &operands {
            let x = Exact::of(a);
            // A square root: invalid below `-0`, and otherwise inexact exactly when the root does not square back; a root is never tiny, and never overflows.
            expected.push(match () {
                _ if is_signaling(a) => INVALID,
                _ if a.is_nan() => 0,
                _ if a.lt(Floating::from(0.0)) => INVALID,
                _ if is_zero(a) || is_infinite(a) => 0,
                _ => {
                    let root = Exact::of(a.sqrt(rounding)).unwrap();
                    u8::from(root.mul(&root).cmp(x.as_ref().unwrap()).is_ne()) * INEXACT
                }
            });
            expected.push(match () {
                _ if is_signaling(a) => INVALID,
                _ if a.is_nan() => 0,
                _ => u8::from(a.round_integral(rounding).neq(a)) * INEXACT,
            });
            for n in SCALINGS {
                expected.push(match () {
                    _ if is_signaling(a) => INVALID,
                    _ if a.is_nan() || is_infinite(a) || is_zero(a) => 0,
                    _ => {
                        let exact = x.clone().unwrap().mul(&Exact::power_of_two(n));
                        let bits = a.to_bits();
                        let field = ((bits >> 52) & 0x7ff) as i32;
                        let fraction = bits & 0x000f_ffff_ffff_ffff;
                        let (magnitude, exponent) = match field {
                            0 => (fraction, -1074),
                            _ => (fraction | (1 << 52), field - 1075),
                        };
                        let result = Floating::of_dyadic(
                            bits >> 63 == 1,
                            &Natural::from(magnitude),
                            exponent + n,
                            rounding,
                        );
                        raised_by_rounding(rounding, &exact, result)
                    }
                });
            }
        }
    }

    let actual = run(&source);
    assert_eq!(actual.len(), expected.len(), "one result per case");
    let mismatches = actual
        .iter()
        .zip(&expected)
        .enumerate()
        .filter(|(_, (a, e))| a != e)
        .map(|(index, (a, e))| format!("case {index}: raised {a:05b}, IEEE {e:05b}"))
        .collect::<Vec<_>>();
    assert!(
        mismatches.is_empty(),
        "{} mismatches:\n{}",
        mismatches.len(),
        mismatches
            .iter()
            .take(40)
            .cloned()
            .collect::<Vec<_>>()
            .join("\n")
    );
}
