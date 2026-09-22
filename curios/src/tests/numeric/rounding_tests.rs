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
        use /std/{{Nat, Int, Flt, Bytes, List, Io}};
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
