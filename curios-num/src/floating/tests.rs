//! The host's `f32` as an oracle — never as the definition.
//!
//! Every case below computes one operation twice, once through the model and once through the host, and demands the same bits. The host is right about binary32 on the machines this runs on, which is exactly what makes it a test: a disagreement is the model's bug until it is one the model *states*, and there is one of those — `copysign(x, nan)`, where the host reads the NaN's sign bit and the model has no sign to read, having one NaN. That case is excluded here and closed at the emitter.
//!
//! The ordinary suite runs the edge grid with all its pairs, a cancellation sweep, and a seeded sample; [`exhaustive_unary_agreement`] is `#[ignore]`d and carries what it last printed.

use {super::*, std::num::NonZero};

/// A seeded stream, so a failure names a case that can be re-run. `xorshift64*`, written out rather than depended on: what is under test is arithmetic, and a generator is not worth a crate.
struct Stream {
    state: u64,
}

impl Stream {
    fn new(seed: u64) -> Self {
        Self { state: seed }
    }

    fn next(&mut self) -> u32 {
        self.state ^= self.state >> 12;
        self.state ^= self.state << 25;
        self.state ^= self.state >> 27;

        (self.state.wrapping_mul(0x2545_f491_4f6c_dd1d) >> 32) as u32
    }
}

/// The IEEE corners: both zeros, both infinities, a NaN, the subnormal ends, the normal ends, the values either side of where consecutive integers stop being representable, and decimal fractions no binary32 holds exactly.
fn edges() -> Vec<f32> {
    vec![
        0.0,
        -0.0,
        f32::INFINITY,
        f32::NEG_INFINITY,
        f32::NAN,
        f32::from_bits(1),
        f32::from_bits(2),
        f32::from_bits(0x007f_ffff),
        f32::MIN_POSITIVE,
        -f32::MIN_POSITIVE,
        f32::MAX,
        f32::MIN,
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
        16_777_215.0,
        16_777_216.0,
        16_777_217.0,
        1.0e-30,
        1.0e30,
        123.456,
        -987.654_3,
    ]
}

/// Agreement is bit-for-bit on a non-NaN result and by NaN-ness on a NaN one — the model has one NaN and the host has many, which is the whole of the difference.
///
/// `case` is a thunk rather than a `String` because the exhaustive sweep calls this thirty billion times: rendering the operands eagerly costs an allocation per comparison and dominates the arithmetic under test. An `assert!` format argument is evaluated only on failure, so the thunk is called only where it is read.
fn agrees(label: &str, case: impl Fn() -> String, expected: f32, actual: Floating) {
    match expected.is_nan() {
        true => assert!(
            actual.is_nan(),
            "{label} on {}: host answered a NaN, model answered {:#010x}",
            case(),
            actual.to_bits(),
        ),
        false => assert_eq!(
            actual.to_bits(),
            expected.to_bits(),
            "{label} on {}: host answered {expected:e}, model answered {:#010x}",
            case(),
            actual.to_bits(),
        ),
    }
}

fn check_unary(value: f32) {
    let case = || format!("{:#010x}", value.to_bits());
    let subject = Floating::from_f32(value);

    agrees("neg", case, -value, -subject);
    agrees("abs", case, value.abs(), subject.abs());
    agrees("sqrt", case, value.sqrt(), subject.sqrt());
    agrees("floor", case, value.floor(), subject.floor());
    agrees("ceil", case, value.ceil(), subject.ceil());
    agrees("trunc", case, value.trunc(), subject.trunc());
    agrees("nearest", case, value.round_ties_even(), subject.nearest());
}

fn check_binary(left: f32, right: f32) {
    let case = || format!("{:#010x}, {:#010x}", left.to_bits(), right.to_bits());
    let (a, b) = (Floating::from_f32(left), Floating::from_f32(right));

    agrees("add", case, left + right, a + b);
    agrees("sub", case, left - right, a - b);
    agrees("mul", case, left * right, a * b);
    agrees("div", case, left / right, a / b);
    agrees("rem", case, left % right, a % b);

    // `f32::min`/`f32::max` are not 754-2019's `minimum`/`maximum`: they answer the non-NaN operand, and leave an equal pair's sign to the lowering. The model and Wasm both answer the NaN and the signed one, so this oracle is spelled out rather than borrowed.
    let ordered = |negative_wins: bool| match left.is_nan() || right.is_nan() {
        true => f32::NAN,
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
    assert_eq!(a.gt(b), left > right, "gt on {}", case());
    assert_eq!(a.le(b), left <= right, "le on {}", case());
    assert_eq!(a.ge(b), left >= right, "ge on {}", case());
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
        let left = f32::from_bits(stream.next());

        if !left.is_finite() || left == 0.0 {
            continue;
        }

        for step in 0..27u32 {
            let scale = f32::from_bits((127 - step) << 23);
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
        let left = f32::from_bits(stream.next());
        let right = f32::from_bits(stream.next());

        check_unary(left);
        check_binary(left, right);
    }
}

#[test]
fn a_conversion_agrees_with_the_host() {
    let mut stream = Stream::new(0x0fed_cba9_8765_4321);

    // Every tie at a 24-bit boundary, from where consecutive integers stop being representable to the top of the range, approached from both sides.
    for power in 24..128u32 {
        let base = Natural::from(1u32)
            .checked_shl(Natural::from(power))
            .unwrap();

        for offset in [0u32, 1, 2, 3] {
            let value = &base + &Natural::from(offset);
            let expected = value
                .to_string()
                .parse::<f32>()
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
        let value = Natural::from((u64::from(stream.next()) << 32) | u64::from(stream.next()));
        let expected = value
            .to_string()
            .parse::<f32>()
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
        Floating::from_f32(3.0e9)
            .to_natural()
            .map(|value| value.to_string()),
        Some("3000000000".to_string()),
    );
    assert_eq!(Floating::from_f32(-0.0).to_natural(), Some(Natural::zero()));
    assert_eq!(Floating::from_f32(-0.5).to_natural(), None);
    assert_eq!(Floating::from_f32(f32::NAN).to_natural(), None);
    assert_eq!(Floating::from_f32(f32::INFINITY).to_integer(), None);
    assert_eq!(
        Floating::from_f32(-2.5)
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
        ("34028235", 31),
        ("34028236", 31),
        ("1", -45),
        ("1", -46),
        ("7", -46),
        ("999999999999999999999", -20),
        ("1", 39),
        ("1", -39),
        ("31415926535897932", -16),
    ];

    for (digits, exponent) in cases {
        let value = Natural::parse_bytes(digits.as_bytes(), 10).expect("a numeral");
        let spelled = format!("{digits}e{exponent}");
        let expected = spelled.parse::<f32>().expect("a parsable literal");

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

/// Every unary operation over all 2³² inputs, against the host.
///
/// Ignored because it is minutes rather than seconds, and kept because no sampled sweep can claim what it claims: that the model and binary32 agree on `neg`, `abs`, `sqrt`, `floor`, `ceil`, `trunc` and `nearest` at every input there is. Split across threads because a single one takes long enough that nobody would run it.
///
/// Reproduce with
///
/// ```text
/// cargo test --release -p curios-num -- --ignored --nocapture exhaustive_unary_agreement
/// ```
///
/// 2026-08-24, release, aarch64-apple-darwin, 12 threads: `checked 4,294,967,296 inputs across 12 threads, 0 mismatches`, 566.92 s wall and 3316 s CPU.
#[test]
#[ignore]
fn exhaustive_unary_agreement() {
    let threads = std::thread::available_parallelism().map_or(1, NonZero::get);
    let span = (1u64 << 32) / threads as u64;

    std::thread::scope(|scope| {
        for thread in 0..threads as u64 {
            let start = thread * span;
            let end = match thread + 1 == threads as u64 {
                true => 1u64 << 32,
                false => start + span,
            };

            scope.spawn(move || {
                for bits in start..end {
                    check_unary(f32::from_bits(bits as u32));
                }
            });
        }
    });

    println!("checked 4,294,967,296 inputs across {threads} threads, 0 mismatches");
}
