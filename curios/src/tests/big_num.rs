//! The `Flt` decimal codec, whose renderer and parser are `BigNat`-backed and whose expectations come from Rust's own `{:+}` and `str::parse::<f64>`. That oracle is why these two stay here; the arithmetic under them is the corpus's `/data/big_num`.

use super::run;

// === `BigNat`, unsigned. =========================================================

#[test]
#[allow(clippy::approx_constant)] // "+3.14" is a parse-and-render test vector, not π
fn flt_to_str_matches_rust_shortest_format() {
    // Stage 2: `Flt/to_str` is a real Dragon4 shortest-float renderer (BigNat-backed), matching `format!("{:+}", f64)` byte-for-byte — no longer the `of_bin` shim. The result is assembled from `Str` literals + `Nat/to_str` digits via `Str/concat`, so it carries the UTF-8 proof through `concat_closed` (closing the Stage 3 gap too). Expectations come straight from Rust's own `{:+}` so the test cannot drift from the oracle the host renderer used to call.
    let cases: &[(&str, f64)] = &[
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
        ("+4503599627370496.0", 4503599627370496.0),
        ("+12345678.0", 12345678.0),
        ("+9007199254740992.0", 9007199254740992.0),
        ("+123456790000000.0", 123456790000000.0),
        ("Flt/div(+1.0, +1000000.0)", 1.0 / 1000000.0),
        (
            "Flt/div(+1.0, +4503599627370496.0)",
            1.0 / 4503599627370496.0,
        ),
        ("Flt/div(+1.0, +0.0)", f64::INFINITY),
        ("Flt/div(Flt/neg(+1.0), +0.0)", f64::NEG_INFINITY),
        ("Flt/div(+0.0, +0.0)", f64::NAN),
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
    // `Flt/of_str` is exact: the digits go into a `BigNat` and `D · 10^E` is narrowed to binary64 once, ties to even. The oracle is Rust's `str::parse::<f64>`, which is correctly rounded, rendered through the same `{:+}` the printer test uses so both halves of the codec answer to the same spelling. The table walks the roundings a narrowing gets wrong: a seventeen-digit mantissa, a large exponent, the normal and subnormal boundaries, the overflow boundary — `1.7976931348623159e308` is above the rounding threshold `2^1024 − 2^970`, so it is `inf` and not the largest finite value, which is what makes it the row that catches a clamp placed one representable step out — and leading zeros, which the underflow clamp must count and the overflow clamp must not.
    let cases = [
        "12.0",
        ".5",
        "1e3",
        "+0.1",
        "-3.14",
        "123456.789012345",
        "1.2345678901234567e-5",
        "9.999999999999998e9",
        "9007199254740993.0",
        "0.30000000000000004",
        "1.7976931348623157e308",
        "1.7976931348623159e308",
        "1.797693134862316e308",
        "1e309",
        "2.2250738585072014e-308",
        "2.225073858507201e-308",
        "1.0e-310",
        "7.0e-324",
        "5.0e-324",
        "1e-324",
        "1e-330",
        "-0.0",
        "0.0e5",
        "00000000000000000000000000000000000000000001.5e0",
        "0.000000000000000000000000000000000000000000001",
        "1.0000000000000002",
        "4503599627370497.0",
        "2.5e-2",
        "7.1551326123456785e37",
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
                text.parse::<f64>().expect("a float the oracle parses")
            )
        })
        .collect::<Vec<_>>()
        .join("|");
    assert_eq!(run(&source), expected.into_bytes());
}

// === `BigInt`, signed. ===========================================================
