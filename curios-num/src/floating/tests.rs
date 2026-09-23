//! The model against two oracles — never against itself.
//!
//! The first is the host's `f64`, which is right about binary64 under the default direction on the machines this runs on: every case computes one operation twice and demands the same bits from a number, and a NaN from a NaN. The host is no oracle for *which* NaN, since hardware answers its own, so the NaN rule is held to a table of its own instead. The ordinary suite runs the edge grid with all its pairs, an exponent-complete corner sweep, a cancellation sweep, and a seeded sample; [`an_exhaustive_low_mantissa_sweep_agrees_with_the_host`] is `#[ignore]`d and carries what it last printed.
//!
//! The second covers every direction, where the host has nothing to say. It computes the exact value an operation would answer with unbounded precision and range, as an [`Exact`] rational, and checks the result against the inequality that *defines* its direction — toward negative, `r ≤ e < next_up(r)`, and so on — which reads no line of `round`, so the two cannot share a mistake.

use {super::*, std::num::NonZero};

/// A seeded stream, so a failure names a case that can be re-run. `xorshift64*`, written out rather than depended on: what is under test is arithmetic, and a generator is not worth a crate.
struct Stream {
    state: u64,
}

impl Stream {
    fn new(seed: u64) -> Self {
        Self { state: seed }
    }

    fn next(&mut self) -> u64 {
        self.state ^= self.state >> 12;
        self.state ^= self.state << 25;
        self.state ^= self.state >> 27;

        self.state.wrapping_mul(0x2545_f491_4f6c_dd1d)
    }
}

/// The IEEE corners: both zeros, both infinities, a NaN, the subnormal ends, the normal ends, the values either side of where consecutive integers stop being representable, and decimal fractions no binary64 holds exactly.
fn edges() -> Vec<f64> {
    vec![
        0.0,
        -0.0,
        f64::INFINITY,
        f64::NEG_INFINITY,
        f64::NAN,
        f64::from_bits(1),
        f64::from_bits(2),
        f64::from_bits(0x000f_ffff_ffff_ffff),
        f64::MIN_POSITIVE,
        -f64::MIN_POSITIVE,
        f64::MAX,
        f64::MIN,
        1.0,
        -1.0,
        0.5,
        2.0,
        3.0,
        -3.0,
        0.1,
        0.2,
        0.3,
        1.5,
        2.5,
        -2.5,
        9_007_199_254_740_991.0,
        9_007_199_254_740_992.0,
        9_007_199_254_740_993.0,
        1.0e-30,
        1.0e30,
        123.456,
        -987.654_3,
    ]
}

/// Agreement is bit-for-bit on a non-NaN result and by NaN-ness on a NaN one: which NaN is the model's rule, not the host's, and [`a_nan_result_is_the_greatest_quieted_nan_operand_or_the_default`] holds it.
///
/// `case` is a thunk rather than a `String` because the low-mantissa sweep calls this hundreds of billions of times: rendering the operands eagerly costs an allocation per comparison and dominates the arithmetic under test. An `assert!` format argument is evaluated only on failure, so the thunk is called only where it is read.
fn agrees(label: &str, case: impl Fn() -> String, expected: f64, actual: Floating) {
    match expected.is_nan() {
        true => assert!(
            actual.is_nan(),
            "{label} on {}: host answered a NaN, model answered {:#018x}",
            case(),
            actual.to_bits(),
        ),
        false => assert_eq!(
            actual.to_bits(),
            expected.to_bits(),
            "{label} on {}: host answered {expected:e}, model answered {:#018x}",
            case(),
            actual.to_bits(),
        ),
    }
}

/// Agreement bit for bit, NaNs included: for the operations IEEE makes bit operations, which the host performs the same way.
fn agrees_bitwise(label: &str, case: impl Fn() -> String, expected: f64, actual: Floating) {
    assert_eq!(
        actual.to_bits(),
        expected.to_bits(),
        "{label} on {}: host answered {:#018x}, model answered {:#018x}",
        case(),
        expected.to_bits(),
        actual.to_bits(),
    );
}

fn check_unary(value: f64) {
    let case = || format!("{:#018x}", value.to_bits());
    let subject = Floating::from(value);

    agrees_bitwise("neg", case, -value, -subject);
    agrees_bitwise("abs", case, value.abs(), subject.abs());
    agrees(
        "sqrt",
        case,
        value.sqrt(),
        subject.sqrt(Rounding::TiesToEven),
    );
    agrees(
        "floor",
        case,
        value.floor(),
        subject.round_integral(Rounding::TowardNegative),
    );
    agrees(
        "ceil",
        case,
        value.ceil(),
        subject.round_integral(Rounding::TowardPositive),
    );
    agrees(
        "trunc",
        case,
        value.trunc(),
        subject.round_integral(Rounding::TowardZero),
    );
    agrees(
        "nearest",
        case,
        value.round_ties_even(),
        subject.round_integral(Rounding::TiesToEven),
    );
    agrees(
        "round",
        case,
        value.round(),
        subject.round_integral(Rounding::TiesToAway),
    );
}

fn check_binary(left: f64, right: f64) {
    let case = || format!("{:#018x}, {:#018x}", left.to_bits(), right.to_bits());
    let a = Floating::from(left);
    let b = Floating::from(right);

    agrees("add", case, left + right, a + b);
    agrees("sub", case, left - right, a - b);
    agrees("mul", case, left * right, a * b);
    agrees("div", case, left / right, a / b);
    agrees("rem", case, left % right, a % b);

    // `f64::min`/`f64::max` are not 754-2019's `minimum`/`maximum`: they answer the non-NaN operand, and leave an equal pair's sign to the lowering. The model and Wasm both answer a NaN and the signed one, so this oracle is spelled out rather than borrowed.
    let ordered = |negative_wins: bool| match left.is_nan() || right.is_nan() {
        true => f64::NAN,
        false if left == right => match left.is_sign_negative() == negative_wins {
            true => left,
            false => right,
        },
        false if (left < right) == negative_wins => left,
        false => right,
    };

    agrees("min", case, ordered(true), a.min(b));
    agrees("max", case, ordered(false), a.max(b));
    agrees_bitwise("copysign", case, left.copysign(right), a.copysign(b));

    assert_eq!(a.eql(b), left == right, "eql on {}", case());
    assert_eq!(a.neq(b), left != right, "neq on {}", case());
    assert_eq!(a.lt(b), left < right, "lt on {}", case());
    assert_eq!(a.le(b), left <= right, "le on {}", case());
    // The model has no `gt`/`ge`: `a > b` is spelled `b < a` from the `/sys` row on (`documentation/design/toolchain/a-comparison-is-spelled-one-way-when-it-is-stuck.md`), so what the grid closes is the mirror itself, at every instantiation the NaN rows included.
    assert_eq!(b.lt(a), left > right, "gt as the lt mirror on {}", case());
    assert_eq!(b.le(a), left >= right, "ge as the le mirror on {}", case());
}

#[test]
fn every_operation_agrees_with_the_host_on_the_edge_grid() {
    for left in edges() {
        check_unary(left);

        for right in edges() {
            check_binary(left, right);
        }
    }
}

/// Exponent differences from zero to past the significand's width, which is the range where a subtraction cancels and the sticky bit alone decides the answer.
#[test]
fn a_cancelling_pair_agrees_with_the_host() {
    let mut stream = Stream::new(0x5eed_1234_5678_9abc);

    for _ in 0..2_000 {
        let left = f64::from_bits(stream.next());

        if !left.is_finite() || left == 0.0 {
            continue;
        }

        for step in 0..56u32 {
            let scale = f64::from_bits(u64::from(1023 - step) << 52);
            let right = left * scale;

            if right != 0.0 {
                check_binary(left, right);
                check_binary(left, -right);
            }
        }
    }
}

#[test]
fn a_seeded_sample_agrees_with_the_host() {
    let mut stream = Stream::new(0x1234_5678_9abc_def0);

    for _ in 0..50_000 {
        let left = f64::from_bits(stream.next());
        let right = f64::from_bits(stream.next());

        check_unary(left);
        check_binary(left, right);
    }
}

/// `fma` against the host's `mul_add`, which rounds once as IEEE's `fusedMultiplyAdd` does: every triple of the edge grid, then a seeded sample, where the addend is drawn near the product so the cancellation that makes a fused operation differ from two roundings is exercised.
#[test]
fn a_fused_multiply_add_agrees_with_the_host() {
    for left in edges() {
        for right in edges() {
            for addend in edges() {
                let case = || {
                    format!(
                        "{:#018x}, {:#018x}, {:#018x}",
                        left.to_bits(),
                        right.to_bits(),
                        addend.to_bits()
                    )
                };

                agrees(
                    "fma",
                    case,
                    left.mul_add(right, addend),
                    Floating::from(left).fma(
                        Floating::from(right),
                        Floating::from(addend),
                        Rounding::TiesToEven,
                    ),
                );
            }
        }
    }

    let mut stream = Stream::new(0x0f0f_1e1e_2d2d_3c3c);

    for _ in 0..50_000 {
        let left = f64::from_bits(stream.next());
        let right = f64::from_bits(stream.next());
        let addend = -(left * right)
            * f64::from_bits(stream.next() & 0x3fff_ffff_ffff_ffff | 0x3ff0_0000_0000_0000)
            / 1.5;
        let case = || {
            format!(
                "{:#018x}, {:#018x}, {:#018x}",
                left.to_bits(),
                right.to_bits(),
                addend.to_bits()
            )
        };

        agrees(
            "fma",
            case,
            left.mul_add(right, addend),
            Floating::from(left).fma(
                Floating::from(right),
                Floating::from(addend),
                Rounding::TiesToEven,
            ),
        );
    }
}

#[test]
fn a_conversion_agrees_with_the_host() {
    let mut stream = Stream::new(0x0fed_cba9_8765_4321);

    // Every tie at a 53-bit boundary, from where consecutive integers stop being representable to the top of the range, approached from both sides.
    for power in 53..1024u32 {
        let base = Natural::from(1u32)
            .shl_within(&Natural::from(power), u64::MAX)
            .unwrap();

        for offset in [0u32, 1, 2, 3] {
            let value = &base + &Natural::from(offset);
            let expected = value
                .to_string()
                .parse::<f64>()
                .expect("a parsable numeral");

            agrees(
                "of_natural",
                || format!("2^{power} + {offset}"),
                expected,
                Floating::of_natural(&value, Rounding::TiesToEven),
            );
        }
    }

    for _ in 0..20_000 {
        let value = Natural::from(stream.next());
        let expected = value
            .to_string()
            .parse::<f64>()
            .expect("a parsable numeral");

        agrees(
            "of_natural",
            || value.to_string(),
            expected,
            Floating::of_natural(&value, Rounding::TiesToEven),
        );
    }

    // The narrowings answer the exact integer part on their domain and refuse outside it. `to_natural(3.0e9)` is exact and unbounded, as the running program holds it.
    assert_eq!(
        Floating::from(3.0e9)
            .to_natural()
            .map(|value| value.to_string()),
        Ok("3000000000".to_string()),
    );
    assert_eq!(Floating::from(-0.0).to_natural(), Ok(Natural::zero()));
    assert_eq!(
        Floating::from(-0.5).to_natural(),
        Err(ScalarTrap::ConversionRange)
    );
    assert_eq!(
        Floating::from(f64::NAN).to_natural(),
        Err(ScalarTrap::ConversionRange)
    );
    assert_eq!(
        Floating::from(f64::INFINITY).to_integer(),
        Err(ScalarTrap::ConversionRange)
    );
    assert_eq!(
        Floating::from(-2.5)
            .to_integer()
            .map(|value| value.to_string()),
        Ok("-2".to_string()),
    );

    // The decomposition is the encoding's own: `-2.5` is `-5 · 2^-1` held as `-(5 · 2^50) · 2^-51`, the least subnormal is `1 · 2^-1074`, a zero of either sign is `(0, 0)`, and only the non-numbers are refused.
    let decomposed = |value: f64| {
        Floating::from(value)
            .to_dyadic()
            .map(|(mantissa, exponent)| (mantissa.to_string(), exponent))
    };
    assert_eq!(decomposed(-2.5), Ok(((-(5i64 << 50)).to_string(), -51)));
    assert_eq!(decomposed(f64::from_bits(1)), Ok(("1".to_string(), -1074)));
    assert_eq!(decomposed(-0.0), Ok(("0".to_string(), 0)));
    assert_eq!(
        decomposed(f64::MAX),
        Ok((((1i64 << 53) - 1).to_string(), 971))
    );
    assert_eq!(
        decomposed(f64::NEG_INFINITY),
        Err(ScalarTrap::ConversionRange)
    );
    assert_eq!(decomposed(f64::NAN), Err(ScalarTrap::ConversionRange));
}

#[test]
fn a_literal_narrows_the_way_the_host_parses_it() {
    let cases = [
        ("0", 0i32),
        ("1", 0),
        ("5", -1),
        ("123456789", -3),
        // Above the largest finite value yet below the rounding threshold, so it is that value and not an infinity — and the numeral one step up, which is.
        ("17976931348623157", 292),
        ("17976931348623159", 292),
        // The subnormal floor: a representable subnormal, a numeral under half the least one so it rounds away, and one above half so it rounds up to it.
        ("1", -323),
        ("1", -324),
        ("5", -324),
        ("999999999999999999999", -20),
        ("1", 309),
        ("1", -309),
        ("31415926535897932", -16),
        // The exponent ceiling and floor the lexer admits: the overflow clamp decides both, and used to overflow itself at the ceiling.
        ("1", i32::MAX),
        ("123456789", i32::MAX - 7),
        ("1", i32::MIN),
    ];

    for (digits, exponent) in cases {
        let value = Natural::parse_bytes(digits.as_bytes(), 10).expect("a numeral");
        let spelled = format!("{digits}e{exponent}");
        let expected = spelled.parse::<f64>().expect("a parsable literal");

        agrees(
            "of_decimal",
            || spelled.clone(),
            expected,
            Floating::of_decimal(false, &value, exponent),
        );
        agrees(
            "of_decimal",
            || format!("-{spelled}"),
            -expected,
            Floating::of_decimal(true, &value, exponent),
        );
    }
}

/// The mantissa patterns the corner sweeps pair with every exponent: the ends, the low bits rounding reads, the carry boundary, and two alternating fills.
const MANTISSA_CORNERS: [u64; 14] = [
    0,
    1,
    2,
    3,
    0x5555_5555_5555 & MANTISSA_MASK,
    0xaaaa_aaaa_aaaa & MANTISSA_MASK,
    1 << 26,
    (1 << 26) - 1,
    1 << 51,
    (1 << 51) - 1,
    MANTISSA_MASK - 2,
    MANTISSA_MASK - 1,
    MANTISSA_MASK,
    0x000f_0f0f_0f0f_0f0f,
];

/// Every unary operation at **every one of the 2048 exponent fields**, over both signs and a fixed set of mantissa corners.
///
/// This is what replaces binary32's exhaustive sweep, and it is deliberately a weaker claim honestly stated. 2⁶⁴ inputs cannot be enumerated, so completeness moves to the axis that decides an answer's *shape*: the exponent field selects zero, subnormal, normal, infinity or NaN, and picks the subnormal grid a result is rounded on. Sweeping it whole covers every one of those cases at every scale, including all 2046 normal exponents and the two special fields, where a sample would visit a handful. The mantissa is then covered by corners rather than exhaustively — the ends, the carry boundary at `2^51`, and the low bits `round` actually reads — with [`an_exhaustive_low_mantissa_sweep_agrees_with_the_host`] taking the low sixteen bits whole.
///
/// Cheap enough for the ordinary suite: 2048 × 14 × 2 inputs.
#[test]
fn every_exponent_agrees_with_the_host_at_the_mantissa_corners() {
    for field in 0..=INFINITE_FIELD as u64 {
        for mantissa in MANTISSA_CORNERS {
            let bits = (field << MANTISSA_BITS) | mantissa;

            check_unary(f64::from_bits(bits));
            check_unary(f64::from_bits(bits | SIGN_MASK));
        }
    }
}

/// Every unary operation at every exponent field, over **all 2¹⁶ low mantissa bits** at each, with the high bits taken from the same corners.
///
/// The low bits are where rounding is decided — the guard, the sticky residue and the tie — so taking them whole at every exponent is the strongest completeness claim available once 2⁶⁴ is off the table. Ignored because it is minutes rather than seconds, and split across threads for the same reason binary32's sweep was.
///
/// Reproduce with
///
/// ```text
/// cargo test --release -p curios-num -- --ignored --nocapture an_exhaustive_low_mantissa_sweep_agrees_with_the_host
/// ```
///
/// Last run printed `checked 3758096384 inputs across 16 threads, 0 mismatches`, in 1145 s.
#[test]
#[ignore]
fn an_exhaustive_low_mantissa_sweep_agrees_with_the_host() {
    let threads = std::thread::available_parallelism().map_or(1, NonZero::get);
    let fields = INFINITE_FIELD as u64 + 1;
    let span = fields.div_ceil(threads as u64);

    std::thread::scope(|scope| {
        for thread in 0..threads as u64 {
            let start = thread * span;
            let end = (start + span).min(fields);

            scope.spawn(move || {
                for field in start..end {
                    for high in MANTISSA_CORNERS {
                        let high = high & !0xffff;

                        for low in 0..=0xffffu64 {
                            let bits = (field << MANTISSA_BITS) | high | low;

                            check_unary(f64::from_bits(bits));
                            check_unary(f64::from_bits(bits | SIGN_MASK));
                        }
                    }
                }
            });
        }
    });

    println!(
        "checked {} inputs across {threads} threads, 0 mismatches",
        fields * MANTISSA_CORNERS.len() as u64 * (1 << 16) * 2
    );
}

/// A float's bytes are the host's `to_le_bytes`, and the decode is the host's `from_le_bytes`, bit for bit over the corners and NaNs of either sign, kind and payload — every pattern is a value, so the bytes invert from both sides. A binary of any width but eight bytes has no host counterpart and is refused, the precondition's own statement, which only an unsound proof reaches.
#[test]
fn a_float_crosses_into_its_bytes_as_the_host_writes_them() {
    for value in [
        1.0,
        -0.0,
        f64::MIN_POSITIVE,
        f64::MAX,
        f64::INFINITY,
        f64::NAN,
        f64::from_bits(0xfff8_0000_0000_0000),
        f64::from_bits(0x7ff0_0000_0000_0001),
        f64::from_bits(0xfff4_0000_0000_1234),
    ] {
        let bytes = Floating::from(value).to_le_bytes();

        assert_eq!(
            bytes.to_bytes().as_deref(),
            Some(&value.to_le_bytes()[..]),
            "{value}"
        );
        assert_eq!(
            Floating::of_le_bytes(&bytes).map(Floating::to_bits),
            Ok(value.to_bits()),
            "{value}"
        );
    }

    assert_eq!(
        Floating::of_le_bytes(&Binary::from_bytes(vec![0; 7])),
        Err(ScalarTrap::Malformed)
    );
}

/// NaN patterns of both signs and both kinds, with and without a payload.
const NANS: [u64; 6] = [
    NAN_BITS,
    NAN_BITS | SIGN_MASK,
    0x7ff0_0000_0000_0001,
    0xfff4_0000_0000_1234,
    0x7fff_ffff_ffff_ffff,
    0x7ff8_0000_0000_0042,
];

/// The NaN rule, stated as a table because the host is no oracle for it: a NaN operand answers itself made quiet, several answer the greatest of their quieted patterns whatever their order, an invalid operation with none answers the default NaN, and the bit operations never quiet.
#[test]
fn a_nan_result_is_the_greatest_quieted_nan_operand_or_the_default() {
    let one = Floating::from(1.0);

    for rounding in Rounding::ALL {
        for left in NANS {
            let a = Floating::from_bits(left);

            for (label, result) in [
                ("sum", a.sum(one, rounding)),
                ("sum", one.sum(a, rounding)),
                ("product", one.product(a, rounding)),
                ("quotient", a.quotient(one, rounding)),
                ("sqrt", a.sqrt(rounding)),
                ("fma", one.fma(one, a, rounding)),
                ("round_integral", a.round_integral(rounding)),
                ("min", a.min(one)),
                ("max", one.max(a)),
                ("rem", a % one),
            ] {
                assert_eq!(
                    result.to_bits(),
                    left | QUIET_BIT,
                    "{label} of {left:#018x} under {rounding:?}"
                );
            }

            // A difference is the sum with the subtrahend negated, the NaN's sign included.
            assert_eq!(
                one.difference(a, rounding).to_bits(),
                (left ^ SIGN_MASK) | QUIET_BIT,
                "difference of {left:#018x} under {rounding:?}"
            );

            for right in NANS {
                let b = Floating::from_bits(right);
                let expected = (left | QUIET_BIT).max(right | QUIET_BIT);

                for (label, result) in [
                    ("sum", a.sum(b, rounding)),
                    ("sum swapped", b.sum(a, rounding)),
                    ("product", a.product(b, rounding)),
                    ("product swapped", b.product(a, rounding)),
                    ("quotient", a.quotient(b, rounding)),
                    ("fma", a.fma(b, one, rounding)),
                    ("fma swapped", b.fma(a, one, rounding)),
                    ("min", a.min(b)),
                    ("max", b.max(a)),
                    ("rem", a % b),
                ] {
                    assert_eq!(
                        result.to_bits(),
                        expected,
                        "{label} of {left:#018x} and {right:#018x} under {rounding:?}"
                    );
                }
            }
        }

        let zero = Floating::from(0.0);
        let infinity = Floating::from(f64::INFINITY);

        for (label, result) in [
            ("0 / 0", zero.quotient(zero, rounding)),
            ("inf / inf", infinity.quotient(infinity, rounding)),
            ("inf - inf", infinity.difference(infinity, rounding)),
            ("0 * inf", zero.product(infinity, rounding)),
            ("sqrt(-1)", Floating::from(-1.0).sqrt(rounding)),
            ("sqrt(-inf)", (-infinity).sqrt(rounding)),
            ("fma(0, inf, 1)", zero.fma(infinity, one, rounding)),
            ("fma(inf, 1, -inf)", infinity.fma(one, -infinity, rounding)),
            ("inf % 1", infinity % one),
            ("1 % 0", one % zero),
        ] {
            assert_eq!(result.to_bits(), NAN_BITS, "{label} under {rounding:?}");
        }

        // A NaN operand answers before the invalid product `0 · ∞` does.
        let signaling = Floating::from_bits(0x7ff0_0000_0000_0001);
        assert_eq!(
            zero.fma(infinity, signaling, rounding).to_bits(),
            0x7ff8_0000_0000_0001
        );
    }

    for pattern in NANS {
        let nan = Floating::from_bits(pattern);

        assert_eq!(Floating::from_bits(pattern).to_bits(), pattern);
        assert_eq!((-nan).to_bits(), pattern ^ SIGN_MASK);
        assert_eq!(nan.abs().to_bits(), pattern & !SIGN_MASK);
        assert_eq!(
            nan.copysign(Floating::from(-1.0)).to_bits(),
            pattern | SIGN_MASK
        );
        assert_eq!(
            Floating::from(1.0).copysign(nan).to_bits(),
            1.0f64.copysign(f64::from_bits(pattern)).to_bits()
        );
        assert!(nan.is_nan() && !nan.is_finite());
    }
}

/// The signs and values the directions give where no rounding happens and where it runs out: an exact zero sum is `+0.0` except toward negative (§6.3), an overflow stops at the largest finite value unless the direction reaches the infinity (§7.4), a value under half the least subnormal reaches it only when the direction points away from zero, and round-to-integer keeps the operand's sign on a zero.
#[test]
fn an_exact_zero_and_an_overflow_take_the_sign_and_value_the_direction_names() {
    let value = |v: f64| Floating::from(v);
    let bits = |v: Floating| v.to_bits();
    let largest = f64::MAX;
    let least = f64::from_bits(1);

    for rounding in Rounding::ALL {
        let toward_negative = rounding == Rounding::TowardNegative;
        let cancelled = f64::from(Floating::zero(toward_negative));

        for (label, result, expected) in [
            ("+0 + -0", value(0.0).sum(value(-0.0), rounding), cancelled),
            ("-0 + +0", value(-0.0).sum(value(0.0), rounding), cancelled),
            ("-0 + -0", value(-0.0).sum(value(-0.0), rounding), -0.0),
            ("+0 + +0", value(0.0).sum(value(0.0), rounding), 0.0),
            ("1 + -1", value(1.0).sum(value(-1.0), rounding), cancelled),
            (
                "2.5 - 2.5",
                value(2.5).difference(value(2.5), rounding),
                cancelled,
            ),
            (
                "fma(1, 1, -1)",
                value(1.0).fma(value(1.0), value(-1.0), rounding),
                cancelled,
            ),
            (
                "fma(-0, 1, +0)",
                value(-0.0).fma(value(1.0), value(0.0), rounding),
                cancelled,
            ),
            ("-0 * 5", value(-0.0).product(value(5.0), rounding), -0.0),
        ] {
            assert_eq!(
                bits(result),
                expected.to_bits(),
                "{label} under {rounding:?}"
            );
        }

        let reaches = |negative: bool| rounding.overflows_to_infinity(negative);
        let overflow = |negative: bool| match (reaches(negative), negative) {
            (true, false) => f64::INFINITY,
            (true, true) => f64::NEG_INFINITY,
            (false, false) => largest,
            (false, true) => -largest,
        };
        assert_eq!(
            bits(value(largest).product(value(2.0), rounding)),
            overflow(false).to_bits(),
            "overflow under {rounding:?}"
        );
        assert_eq!(
            bits(value(-largest).product(value(2.0), rounding)),
            overflow(true).to_bits(),
            "negative overflow under {rounding:?}"
        );
        assert_eq!(
            bits(value(largest).sum(value(largest), rounding)),
            overflow(false).to_bits(),
            "overflowing sum under {rounding:?}"
        );

        // Half the least subnormal is a tie: to even it is zero, away from zero it is the least subnormal.
        let half_least = |negative: bool| match (rounding, negative) {
            (Rounding::TiesToAway | Rounding::TowardPositive, false) => least,
            (Rounding::TiesToAway | Rounding::TowardNegative, true) => -least,
            (_, false) => 0.0,
            (_, true) => -0.0,
        };
        assert_eq!(
            bits(value(least).product(value(0.5), rounding)),
            half_least(false).to_bits(),
            "half the least subnormal under {rounding:?}"
        );
        assert_eq!(
            bits(value(-least).product(value(0.5), rounding)),
            half_least(true).to_bits(),
            "minus half the least subnormal under {rounding:?}"
        );
    }

    for (operand, rounding, expected) in [
        (-0.5, Rounding::TowardPositive, -0.0f64),
        (0.5, Rounding::TowardNegative, 0.0),
        (-0.5, Rounding::TowardZero, -0.0),
        (2.5, Rounding::TiesToAway, 3.0),
        (-2.5, Rounding::TiesToAway, -3.0),
        (2.5, Rounding::TiesToEven, 2.0),
        (0.5, Rounding::TiesToEven, 0.0),
        (-1.5, Rounding::TowardNegative, -2.0),
    ] {
        assert_eq!(
            bits(value(operand).round_integral(rounding)),
            expected.to_bits(),
            "round_integral({operand}) under {rounding:?}"
        );
    }
}

/// An exact rational: the value an operation would answer with unbounded precision and range, and the second oracle's number.
#[derive(Clone, Debug)]
struct Exact {
    numerator: Integer,
    denominator: Natural,
}

impl Exact {
    fn integer(value: Integer) -> Self {
        Self {
            numerator: value,
            denominator: Natural::one(),
        }
    }

    fn zero() -> Self {
        Self::integer(Integer::from(0u32))
    }

    /// `2^1024`, where the grid would put the value past the largest finite one if the exponent went on: the oracle's stand-in for an infinity as a neighbor, which makes an overflow a tie that ties-to-even breaks toward the infinity, as IEEE does.
    fn past_largest(negative: bool) -> Self {
        let magnitude = Integer::from(shift_left(&Natural::one(), 1024));

        Self::integer(match negative {
            true => -magnitude,
            false => magnitude,
        })
    }

    /// A finite float's exact value, or the stand-in for an infinity.
    fn of(value: Floating) -> Self {
        match value.unpack() {
            Unpacked::Zero { .. } => Self::zero(),
            Unpacked::Infinite { negative } => Self::past_largest(negative),
            Unpacked::Nan => panic!("a NaN has no value"),
            Unpacked::Finite {
                negative,
                magnitude,
                exponent,
            } => {
                let signed = |magnitude: Natural| {
                    let magnitude = Integer::from(magnitude);
                    match negative {
                        true => -magnitude,
                        false => magnitude,
                    }
                };

                match u32::try_from(exponent) {
                    Ok(exponent) => Self::integer(signed(shift_left(&magnitude, exponent))),
                    Err(_) => Self {
                        numerator: signed(magnitude),
                        denominator: shift_left(&Natural::one(), exponent.unsigned_abs()),
                    },
                }
            }
        }
    }

    fn cmp(&self, other: &Self) -> Ordering {
        let left = self.numerator.clone() * Integer::from(other.denominator.clone());
        let right = other.numerator.clone() * Integer::from(self.denominator.clone());

        left.cmp(&right)
    }

    fn add(&self, other: &Self) -> Self {
        Self {
            numerator: self.numerator.clone() * Integer::from(other.denominator.clone())
                + other.numerator.clone() * Integer::from(self.denominator.clone()),
            denominator: &self.denominator * &other.denominator,
        }
    }

    fn negate(&self) -> Self {
        Self {
            numerator: -self.numerator.clone(),
            denominator: self.denominator.clone(),
        }
    }

    fn mul(&self, other: &Self) -> Self {
        Self {
            numerator: self.numerator.clone() * other.numerator.clone(),
            denominator: &self.denominator * &other.denominator,
        }
    }

    /// `self / other`, `other` nonzero.
    fn div(&self, other: &Self) -> Self {
        let numerator = self.numerator.clone() * Integer::from(other.denominator.clone());
        let numerator = match other.numerator < Integer::from(0u32) {
            true => -numerator,
            false => numerator,
        };

        Self {
            numerator,
            denominator: &self.denominator * &other.numerator.magnitude(),
        }
    }

    fn half(&self) -> Self {
        Self {
            numerator: self.numerator.clone(),
            denominator: &self.denominator * &Natural::from(2u32),
        }
    }

    /// Whether this is a whole number, whatever the fraction's spelling: a float's exact value comes unreduced, `1.0` as `2^52 / 2^52`.
    fn is_integer(&self) -> bool {
        (self.numerator.magnitude() % &self.denominator).is_zero()
    }

    fn magnitude(&self) -> Self {
        Self {
            numerator: Integer::from(self.numerator.magnitude()),
            denominator: self.denominator.clone(),
        }
    }
}

/// The next float up from a finite or infinite one, `+inf` past the largest finite value: the integer successor of the pattern, read through the sign.
fn next_up(value: Floating) -> Floating {
    let bits = value.to_bits();

    match (value.is_negative(), bits & !SIGN_MASK == 0) {
        (_, true) => Floating::least(false),
        (false, false) => Floating::from_bits(bits + 1),
        (true, false) => Floating::from_bits(bits - 1),
    }
}

fn next_down(value: Floating) -> Floating {
    -next_up(-value)
}

/// Whether `result` is what `rounding` makes of the exact value `place` locates. `place(v)` says how `v` stands against that value — `Less` below it, `Equal` on it, `Greater` above — which is all the oracle asks, so an irrational square root is located by squaring rather than represented.
fn directs(
    label: &str,
    case: impl Fn() -> String,
    rounding: Rounding,
    place: impl Fn(&Exact) -> Ordering,
    result: Floating,
) {
    assert!(
        !result.is_nan(),
        "{label} on {} under {rounding:?}: a NaN from finite operands",
        case()
    );

    let r = Exact::of(result);
    let exact_sign = place(&Exact::zero()).reverse();

    let holds = match result.is_finite() {
        // An infinity from finite operands is an overflow: past the midpoint to `2^1024` when rounding to nearest, past the largest finite value when directed toward it, and never toward zero.
        false => {
            let negative = result.is_negative();
            let largest = Exact::of(Floating::from_bits(
                Floating::sign_bit(negative) | MAX_FINITE_BITS,
            ));
            let midpoint = largest.add(&Exact::past_largest(negative)).half();
            let beyond = |edge: &Exact, strict: bool| {
                matches!(
                    (negative, place(edge), strict),
                    (false, Ordering::Less, _)
                        | (true, Ordering::Greater, _)
                        | (_, Ordering::Equal, false)
                )
            };

            match rounding {
                Rounding::TiesToEven | Rounding::TiesToAway => beyond(&midpoint, false),
                Rounding::TowardPositive => !negative && beyond(&largest, true),
                Rounding::TowardNegative => negative && beyond(&largest, true),
                Rounding::TowardZero => false,
            }
        }
        true => {
            let up = Exact::of(next_up(result));
            let down = Exact::of(next_down(result));
            // `r ≤ e < up` and `down < e ≤ r`, the two directed readings. Past the largest finite value there is no `up` to stay under: a direction that stops an overflow there answers it for every value beyond, so the bound is dropped rather than read at the infinity's stand-in.
            let from_below = place(&r) != Ordering::Greater
                && (!next_up(result).is_finite() || place(&up) == Ordering::Greater);
            let from_above = (!next_down(result).is_finite() || place(&down) == Ordering::Less)
                && place(&r) != Ordering::Less;

            let nearest = || {
                let low = place(&down.add(&r).half());
                let high = place(&r.add(&up).half());
                let even = result.to_bits() & 1 == 0;
                let tie = |neighbor: &Exact| match rounding {
                    Rounding::TiesToEven => even,
                    _ => r.magnitude().cmp(&neighbor.magnitude()) == Ordering::Greater,
                };

                match (low, high) {
                    (Ordering::Greater, _) | (_, Ordering::Less) => false,
                    (Ordering::Equal, _) => tie(&down),
                    (_, Ordering::Equal) => tie(&up),
                    _ => true,
                }
            };

            let placed = match rounding {
                Rounding::TowardNegative => from_below,
                Rounding::TowardPositive => from_above,
                Rounding::TowardZero => match exact_sign {
                    Ordering::Less => from_above,
                    _ => from_below,
                },
                Rounding::TiesToEven | Rounding::TiesToAway => nearest(),
            };

            // A zero that stands for a nonzero value carries its sign.
            let signed = !(r.cmp(&Exact::zero()) == Ordering::Equal
                && exact_sign != Ordering::Equal)
                || result.is_negative() == (exact_sign == Ordering::Less);

            placed && signed
        }
    };

    assert!(
        holds,
        "{label} on {} under {rounding:?}: answered {:#018x}",
        case(),
        result.to_bits()
    );
}

/// Every rounded operation, under every direction, over finite operands, each held to the inequality its direction defines.
fn check_directions(left: f64, right: f64, addend: f64) {
    let (a, b, c) = (
        Floating::from(left),
        Floating::from(right),
        Floating::from(addend),
    );

    if !(a.is_finite() && b.is_finite() && c.is_finite()) {
        return;
    }

    let case = || {
        format!(
            "{:#018x}, {:#018x}, {:#018x}",
            left.to_bits(),
            right.to_bits(),
            addend.to_bits()
        )
    };
    let (x, y, z) = (Exact::of(a), Exact::of(b), Exact::of(c));
    let at = |exact: Exact| move |v: &Exact| v.cmp(&exact);

    for rounding in Rounding::ALL {
        directs("sum", case, rounding, at(x.add(&y)), a.sum(b, rounding));
        directs(
            "difference",
            case,
            rounding,
            at(x.add(&y.negate())),
            a.difference(b, rounding),
        );
        directs(
            "product",
            case,
            rounding,
            at(x.mul(&y)),
            a.product(b, rounding),
        );
        directs(
            "fma",
            case,
            rounding,
            at(x.mul(&y).add(&z)),
            a.fma(b, c, rounding),
        );

        if y.cmp(&Exact::zero()) != Ordering::Equal {
            directs(
                "quotient",
                case,
                rounding,
                at(x.div(&y)),
                a.quotient(b, rounding),
            );
        }

        if x.cmp(&Exact::zero()) == Ordering::Greater {
            let root = |v: &Exact| match v.cmp(&Exact::zero()) {
                Ordering::Less => Ordering::Less,
                _ => v.mul(v).cmp(&x),
            };

            directs("sqrt", case, rounding, root, a.sqrt(rounding));
        }

        check_integral(a, rounding);
    }
}

/// `round_integral` held to its own definition, since its answer is an integer rather than the nearest float: an integer, within one of the operand on the side its direction names, or at most half away when rounding to nearest, with a tie broken to the even integer or away from zero.
fn check_integral(value: Floating, rounding: Rounding) {
    let answer = value.round_integral(rounding);
    let exact = Exact::of(value);
    let result = Exact::of(answer);
    let one = Exact::integer(Integer::from(1u32));

    assert!(
        result.is_integer(),
        "round_integral({value}) under {rounding:?} is not an integer: {answer}"
    );

    let below =
        result.cmp(&exact) != Ordering::Greater && exact.cmp(&result.add(&one)) == Ordering::Less;
    let above = result.cmp(&exact) != Ordering::Less
        && exact.cmp(&result.add(&one.negate())) == Ordering::Greater;
    let holds = match rounding {
        Rounding::TowardNegative => below,
        Rounding::TowardPositive => above,
        Rounding::TowardZero => match exact.cmp(&Exact::zero()) {
            Ordering::Less => above,
            _ => below,
        },
        Rounding::TiesToEven | Rounding::TiesToAway => {
            match exact.add(&result.negate()).magnitude().cmp(&one.half()) {
                Ordering::Less => true,
                Ordering::Greater => false,
                Ordering::Equal => match rounding {
                    Rounding::TiesToEven => result.half().is_integer(),
                    _ => result.magnitude().cmp(&exact.magnitude()) == Ordering::Greater,
                },
            }
        }
    };

    // A zero answer keeps the operand's sign.
    let signed = !answer.eql(Floating::zero(false)) || answer.is_negative() == value.is_negative();

    assert!(
        holds && signed,
        "round_integral({value}) under {rounding:?}: answered {answer}"
    );
}

#[test]
fn every_direction_rounds_the_edge_grid_the_way_it_names() {
    let finite: Vec<f64> = edges().into_iter().filter(|v| v.is_finite()).collect();

    for &left in &finite {
        for &right in &finite {
            for addend in [0.0, -0.0, 1.0, -1.0, f64::from_bits(1), f64::MAX, 1.0e-30] {
                check_directions(left, right, addend);
            }
        }
    }
}

/// A seeded sample of finite triples, with a band of operands drawn near one another so sums cancel and quotients and roots carry long residues.
#[test]
fn every_direction_rounds_a_seeded_sample_the_way_it_names() {
    let mut stream = Stream::new(0xd1ec_7ed5_eed0_0001);

    for round in 0..4_000 {
        let left = f64::from_bits(stream.next());
        let right = match round % 2 {
            0 => f64::from_bits(stream.next()),
            _ => -left * f64::from_bits(0x3ff0_0000_0000_0000 | (stream.next() & 0xfff)),
        };
        let addend =
            -(left * right) * f64::from_bits(0x3ff0_0000_0000_0000 | (stream.next() & 0xf_ffff));

        check_directions(left, right, addend);
    }
}

/// Every direction on the conversions from an unbounded integer: the ties at each 53-bit boundary up the range, a value past the largest finite one, and a seeded sample of both signs.
#[test]
fn every_direction_converts_an_integer_the_way_it_names() {
    let mut stream = Stream::new(0x7e57_c0de_0000_0002);
    let mut values = Vec::new();

    for power in (53..1030u32).step_by(7) {
        let base = shift_left(&Natural::one(), power);

        for offset in [0u32, 1, 2, 3, 5] {
            values.push(&base + &Natural::from(offset));
        }
    }

    for _ in 0..2_000 {
        values.push(Natural::from(stream.next()) * Natural::from(stream.next()));
    }

    for value in values {
        for rounding in Rounding::ALL {
            let exact = Exact::integer(Integer::from(value.clone()));

            directs(
                "of_natural",
                || value.to_string(),
                rounding,
                |v: &Exact| v.cmp(&exact),
                Floating::of_natural(&value, rounding),
            );

            let negative = -Integer::from(value.clone());
            let exact = Exact::integer(negative.clone());

            directs(
                "of_integer",
                || format!("-{value}"),
                rounding,
                |v: &Exact| v.cmp(&exact),
                Floating::of_integer(&negative, rounding),
            );
        }
    }
}
