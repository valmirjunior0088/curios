//! The `Flt` decimal codec, whose renderer and parser are `BigNat`-backed and whose expectations come from Rust's own `{:+}` and `str::parse::<f32>`. That oracle is why these two stay here; the arithmetic under them is the corpus's `/data/big_num`.

use super::run;

// === `BigNat`, unsigned. =========================================================

#[test]
#[allow(clippy::approx_constant)] // "+3.14" is a parse-and-render test vector, not π
fn flt_to_str_matches_rust_shortest_format() {
    // Stage 2: `Flt/to_str` is a real Dragon4 shortest-float renderer (BigNat-backed), matching `format!("{:+}", f32)` byte-for-byte — no longer the `of_bin` shim. The result is assembled from `Str` literals + `Nat/to_str` digits via `Str/concat`, so it carries the UTF-8 proof through `concat_closed` (closing the Stage 3 gap too). Expectations come straight from Rust's own `{:+}` so the test cannot drift from the oracle the host renderer used to call.
    let cases: &[(&str, f32)] = &[
        ("+1.0", 1.0),
        ("Flt/neg(+1.0)", -1.0),
        ("+0.0", 0.0),
        ("Flt/neg(+0.0)", -0.0),
        ("+0.5", 0.5),
        ("+1.5", 1.5),
        ("+0.25", 0.25),
        ("+0.125", 0.125),
        ("+0.1", 0.1),
        ("+3.14", 3.14),
        ("+2.5", 2.5),
        ("+100.0", 100.0),
        ("+1234.5", 1234.5),
        ("+1000000.0", 1000000.0),
        ("+8388608.0", 8388608.0),
        ("+12345678.0", 12345678.0),
        ("+16777216.0", 16777216.0),
        ("+123456790000000.0", 123456790000000.0),
        ("Flt/div(+1.0, +1000000.0)", 1.0 / 1000000.0),
        ("Flt/div(+1.0, +8388608.0)", 1.0 / 8388608.0),
        ("Flt/div(+1.0, +0.0)", f32::INFINITY),
        ("Flt/div(Flt/neg(+1.0), +0.0)", f32::NEG_INFINITY),
        ("Flt/div(+0.0, +0.0)", f32::NAN),
    ];
    let array = cases
        .iter()
        .map(|(expr, _)| format!("Flt/to_str({expr})"))
        .collect::<Vec<_>>()
        .join(", ");
    let source = format!(
        r#"
        use /std/{{Str, Flt, List}};
        /std/print(Str/join("|", [{array}]))
        "#
    );
    let expected = cases
        .iter()
        .map(|(_, value)| format!("{value:+}"))
        .collect::<Vec<_>>()
        .join("|");
    assert_eq!(run(&source), expected.into_bytes());
}

#[test]
#[allow(clippy::approx_constant)] // "3.14" is a parse-and-render test vector, not π
fn flt_of_str_matches_rust_parse() {
    // `Flt/of_str` is exact: the digits go into a `BigNat` and `D · 10^E` is narrowed to binary32 once, ties to even. The oracle is Rust's `str::parse::<f32>`, which is correctly rounded, rendered through the same `{:+}` the printer test uses so both halves of the codec answer to the same spelling. The table walks every rounding the old arithmetic got wrong: a nine-digit mantissa (rounded by `Nat/to_flt` before scaling), a large exponent (`pow10` by repeated multiplication is inexact past `10^10`), every subnormal (`pow10(39)` overflowed, so their reciprocal scale was zero), the overflow boundary — `3.4028236e38` is above the rounding threshold `2^128 − 2^103`, so it is `inf` and not the largest finite value, which is what makes it the row that catches a clamp placed one representable step out — and leading zeros, which the underflow clamp must count and the overflow clamp must not.
    let cases = [
        "12.0",
        ".5",
        "1e3",
        "+0.1",
        "-3.14",
        "123456.79",
        "1.2345679e-5",
        "9.999999e9",
        "16777217.0",
        "0.30000001",
        "3.4028235e38",
        "3.4028236e38",
        "3.4028237e38",
        "1e39",
        "1.1754944e-38",
        "1.1754942e-38",
        "2.137381e-39",
        "7.0e-45",
        "1.0e-45",
        "1e-46",
        "1e-50",
        "-0.0",
        "0.0e5",
        "00000000000000000000000000000000000000000001.5e0",
        "0.000000000000000000000000000000000000000000001",
        "1.00000006",
        "8388609.0",
        "2.5e-2",
        "7.1551326e37",
    ];
    let array = cases
        .iter()
        .map(|text| format!("Flt/to_str(Option/unwrap_or(Flt/of_str(\"{text}\"), Flt/nan))"))
        .collect::<Vec<_>>()
        .join(", ");
    let source = format!(
        r#"
        use /std/{{Str, Flt, List, Option}};
        /std/print(Str/join("|", [{array}]))
        "#
    );
    let expected = cases
        .iter()
        .map(|text| {
            format!(
                "{:+}",
                text.parse::<f32>().expect("a float the oracle parses")
            )
        })
        .collect::<Vec<_>>()
        .join("|");
    assert_eq!(run(&source), expected.into_bytes());
}

// === `BigInt`, signed. ===========================================================
