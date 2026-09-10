//! The host's `f64` as an oracle — never as the definition.
//!
//! Every case below computes one operation twice, once through the model and once through the host, and demands the same bits. The host is right about binary64 on the machines this runs on, which is exactly what makes it a test: a disagreement is the model's bug until it is one the model *states*, and there is one of those — `copysign(x, nan)`, where the host reads the NaN's sign bit and the model has no sign to read, having one NaN. That case is excluded here and closed at the emitter.
//!
//! The ordinary suite runs the edge grid with all its pairs, an exponent-complete corner sweep, a cancellation sweep, and a seeded sample; [`an_exhaustive_low_mantissa_sweep_agrees_with_the_host`] is `#[ignore]`d and carries what it last printed.

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

/// Agreement is bit-for-bit on a non-NaN result and by NaN-ness on a NaN one — the model has one NaN and the host has many, which is the whole of the difference.
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

fn check_unary(value: f64) {
    let case = || format!("{:#018x}", value.to_bits());
    let subject = Floating::from_f64(value);

    agrees("neg", case, -value, -subject);
    agrees("abs", case, value.abs(), subject.abs());
    agrees("sqrt", case, value.sqrt(), subject.sqrt());
    agrees("floor", case, value.floor(), subject.floor());
    agrees("ceil", case, value.ceil(), subject.ceil());
    agrees("trunc", case, value.trunc(), subject.trunc());
    agrees("nearest", case, value.round_ties_even(), subject.nearest());
}

fn check_binary(left: f64, right: f64) {
    let case = || format!("{:#018x}, {:#018x}", left.to_bits(), right.to_bits());
    let (a, b) = (Floating::from_f64(left), Floating::from_f64(right));

    agrees("add", case, left + right, a + b);
    agrees("sub", case, left - right, a - b);
    agrees("mul", case, left * right, a * b);
    agrees("div", case, left / right, a / b);
    agrees("rem", case, left % right, a % b);

    // `f64::min`/`f64::max` are not 754-2019's `minimum`/`maximum`: they answer the non-NaN operand, and leave an equal pair's sign to the lowering. The model and Wasm both answer the NaN and the signed one, so this oracle is spelled out rather than borrowed.
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

    // The stated divergence: on a NaN sign operand the host reads its sign bit, and the model answers the magnitude.
    if !right.is_nan() {
        agrees("copysign", case, left.copysign(right), a.copysign(b));
    }

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

#[test]
fn a_conversion_agrees_with_the_host() {
    let mut stream = Stream::new(0x0fed_cba9_8765_4321);

    // Every tie at a 53-bit boundary, from where consecutive integers stop being representable to the top of the range, approached from both sides.
    for power in 53..1024u32 {
        let base = Natural::from(1u32)
            .checked_shl(Natural::from(power))
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
                Floating::of_natural(&value),
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
            Floating::of_natural(&value),
        );
    }

    // The narrowings answer the exact integer part on their domain and decline outside it. `to_natural(3.0e9)` is a value no runtime carrier holds and is refused downstream, not bent to fit here.
    assert_eq!(
        Floating::from_f64(3.0e9)
            .to_natural()
            .map(|value| value.to_string()),
        Some("3000000000".to_string()),
    );
    assert_eq!(Floating::from_f64(-0.0).to_natural(), Some(Natural::zero()));
    assert_eq!(Floating::from_f64(-0.5).to_natural(), None);
    assert_eq!(Floating::from_f64(f64::NAN).to_natural(), None);
    assert_eq!(Floating::from_f64(f64::INFINITY).to_integer(), None);
    assert_eq!(
        Floating::from_f64(-2.5)
            .to_integer()
            .map(|value| value.to_string()),
        Some("-2".to_string()),
    );
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
