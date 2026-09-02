//! `/std/BigNat` and `/std/BigInt`: arbitrary-precision arithmetic over a limb spine.

use super::run;

// === `BigNat`, unsigned. =========================================================

#[test]
fn big_nat_add_ripples_carry() {
    // `add` propagates carry along the binary numeral: 99_999_999 ends in eight set bits, so adding 1 ripples the carry through all of them (the `pos_add`/`pos_add_c` twin recursion) before it lands.
    let source = r#"
        use /std/{Handle, Str, BigNat};
        /std/print(BigNat/to_str(BigNat/add(BigNat/of_nat(99999999), BigNat/of_nat(1))))
        "#;
    assert_eq!(run(source), b"100000000");
}

#[test]
fn big_nat_sub_borrows() {
    // `sub` runs the mask-based borrow recursion: 100_000_000 ends in eight clear bits, so subtracting 1 borrows through all of them. The result is canonical by construction — no trailing-zero cleanup exists to get wrong.
    let source = r#"
        use /std/{Handle, Str, BigNat};
        /std/print(BigNat/to_str(BigNat/sub(BigNat/of_nat(100000000), BigNat/of_nat(1))))
        "#;
    assert_eq!(run(source), b"99999999");
}

#[test]
fn big_nat_mul_small_propagates_carry() {
    // `mul_small` is `mul` against a decoded native operand: 9999 * 99999 = 999_890_001 crosses well past both inputs' bit widths, so every carry of the shift-and-add recursion has to land.
    let source = r#"
        use /std/{Handle, Str, BigNat};
        /std/print(BigNat/to_str(BigNat/mul/small(BigNat/of_nat(9999), 99999)))
        "#;
    assert_eq!(run(source), b"999890001");
}

#[test]
fn big_nat_mul_crosses_word_width() {
    // Full big-by-big `mul`: 123_456_789 × 987_654_321 = 121_932_631_112_635_269 needs 57 bits, so a correct rendering proves the product lives in the numeral itself, never in a fixed-width intermediate.
    let source = r#"
        use /std/{Handle, Str, BigNat};
        /std/print(BigNat/to_str(BigNat/mul(BigNat/of_nat(123456789), BigNat/of_nat(987654321))))
        "#;
    assert_eq!(run(source), b"121932631112635269");
}

#[test]
fn big_nat_mul_pow2_builds_large_powers() {
    // `mul_pow2` doubles past every fixed-width integer: 2^40 = 1_099_511_627_776 far exceeds the 31-bit `Nat` carrier, so a correct result proves each doubling is a low-bit prepend on the numeral, not native arithmetic.
    let source = r#"
        use /std/{Handle, Str, BigNat};
        /std/print(BigNat/to_str(BigNat/mul/pow2(BigNat/of_nat(1), 40)))
        "#;
    assert_eq!(run(source), b"1099511627776");
}

#[test]
fn big_nat_div2_and_parity() {
    // `div2` drops the low bit in O(1) and `is_even` reads it: 101 is odd and floor-halves to 50, which is even.
    let source = r#"
        use /std/{Handle, Str, Bool, BigNat};
        let show(b : Bool) -> Str =
            match b : (_) => Str
            | true => "T"
            | false => "F"
            end;
        let n = BigNat/of_nat(101);
        /std/print(Str/concat(
            Str/concat(BigNat/to_str(BigNat/div2(n)), show(BigNat/is_even(n))),
            show(BigNat/is_even(BigNat/div2(n)))))
        "#;
    assert_eq!(run(source), b"50FT");
}

#[test]
fn big_nat_bit_len_counts_binary_digits() {
    // `bit_len` is the numeral's length: zero has no bits, 1 is a single bit, and the 255 → 256 step is where the count grows from 8 to 9.
    let source = r#"
        use /std/{Handle, Str, Nat, List, BigNat};
        /std/print(Str/join(",", [
            Nat/to_str(BigNat/bit_len(BigNat/zero)),
            Nat/to_str(BigNat/bit_len(BigNat/of_nat(1))),
            Nat/to_str(BigNat/bit_len(BigNat/of_nat(255))),
            Nat/to_str(BigNat/bit_len(BigNat/of_nat(256)))]))
        "#;
    assert_eq!(run(source), b"0,1,8,9");
}

#[test]
fn big_nat_cmp_orders_by_magnitude() {
    // `cmp` lets the high bits decide (the recursion on the numeral tails), breaking ties on the low bit only afterward, so two values differing only in the lowest bit still order correctly: 12345678 < 12345679, equal to itself, and the reverse is greater.
    let source = r#"
        use /std/{Handle, Str, BigNat, Ordering};
        let show(o : Ordering) -> Str =
            match o : (_) => Str
            | lt() => "lt"
            | eq() => "eq"
            | gt() => "gt"
            end;
        let a = BigNat/of_nat(12345678);
        let b = BigNat/of_nat(12345679);
        /std/print(Str/concat(Str/concat(show(BigNat/cmp(a, b)), show(BigNat/cmp(a, a))), show(BigNat/cmp(b, a))))
        "#;
    assert_eq!(run(source), b"lteqgt");
}

#[test]
fn big_nat_zero_renders_and_roundtrips() {
    // Zero is its own constructor, which `to_str` renders as "0" (not the empty string), and a value with clear low bits round-trips through the binary long division that produces the decimal digits.
    let source = r#"
        use /std/{Handle, Str, BigNat};
        /std/print(Str/concat(Str/concat(BigNat/to_str(BigNat/zero), "/"), BigNat/to_str(BigNat/of_nat(70000))))
        "#;
    assert_eq!(run(source), b"0/70000");
}

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
        use /std/{{Handle, Str, Flt, List}};
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
        use /std/{{Handle, Str, Flt, List, Option}};
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

#[test]
fn bigint_add_crosses_zero_in_both_directions() {
    // Mixed-sign `add` routes through `pos_sub`, which reads the sign straight off `cmp`: the same magnitudes flipped land exactly on the other side of zero, and equal magnitudes land on zero itself — which has its own constructor, so "-0" cannot even be produced.
    let source = r#"
        use /std/{Handle, Str, List, BigInt};
        /std/print(Str/join(",", [
            BigInt/to_str(BigInt/add(BigInt/of_int(-70000), BigInt/of_int(+99999))),
            BigInt/to_str(BigInt/add(BigInt/of_int(+70000), BigInt/of_int(-99999))),
            BigInt/to_str(BigInt/add(BigInt/of_int(-70000), BigInt/of_int(+70000)))]))
        "#;
    assert_eq!(run(source), b"+29999,-29999,+0");
}

#[test]
fn bigint_sub_is_total() {
    // Unlike `BigNat/sub`, the signed `sub` never truncates: subtracting a larger value produces the genuine negative difference.
    let source = r#"
        use /std/{Handle, Str, BigInt};
        /std/print(BigInt/to_str(BigInt/sub(BigInt/of_nat(1), BigInt/of_nat(100000000))))
        "#;
    assert_eq!(run(source), b"-99999999");
}

#[test]
fn bigint_mul_multiplies_signs() {
    // The sign of a product is the product of the signs, and the magnitude rides `BigNat`'s numeral past any fixed width: (-99999) * (-99999) and (+99999) * (-99999) differ only in sign.
    let source = r#"
        use /std/{Handle, Str, List, BigInt};
        /std/print(Str/join(",", [
            BigInt/to_str(BigInt/mul(BigInt/of_int(-99999), BigInt/of_int(-99999))),
            BigInt/to_str(BigInt/mul(BigInt/of_int(+99999), BigInt/of_int(-99999))),
            BigInt/to_str(BigInt/mul(BigInt/of_int(-99999), BigInt/zero))]))
        "#;
    assert_eq!(run(source), b"+9999800001,-9999800001,+0");
}

#[test]
fn bigint_cmp_orders_across_signs() {
    // `cmp` decides by sign first and only then by magnitude — where the negative stratum orders REVERSED: -2 < -1 even though 2 > 1.
    let source = r#"
        use /std/{Handle, Str, BigInt, Ordering};
        let show(o : Ordering) -> Str =
            match o : (_) => Str
            | lt() => "lt"
            | eq() => "eq"
            | gt() => "gt"
            end;
        let m2 = BigInt/of_int(-2);
        let m1 = BigInt/of_int(-1);
        let p1 = BigInt/of_int(+1);
        /std/print(Str/concat(
            Str/concat(show(BigInt/cmp(m2, m1)), show(BigInt/cmp(m1, p1))),
            Str/concat(show(BigInt/cmp(p1, m1)), show(BigInt/cmp(m1, m1)))))
        "#;
    assert_eq!(run(source), b"ltltgteq");
}

#[test]
fn bigint_of_int_crosses_the_i31_boundary() {
    // `of_int` decodes a native `Int` sign-and-magnitude: the largest magnitude whose negation also fits the i31 carrier round-trips through the numeral in both signs and renders Int-style. (The carrier minimum itself is out of reach: `Int/abs(-2^30)` has no i31 representation.)
    let source = r#"
        use /std/{Handle, Str, List, BigInt};
        /std/print(Str/join(",", [
            BigInt/to_str(BigInt/of_int(+1073741823)),
            BigInt/to_str(BigInt/of_int(-1073741823)),
            BigInt/to_str(BigInt/of_int(+0))]))
        "#;
    assert_eq!(run(source), b"+1073741823,-1073741823,+0");
}

#[test]
fn bigint_neg_abs_and_parity() {
    // `neg` is an involution that `abs` collapses, and `is_even`/`div2` read the magnitude: -101 is odd and halves toward zero to -50.
    let source = r#"
        use /std/{Handle, Str, Bool, List, BigInt};
        let show(b : Bool) -> Str =
            match b : (_) => Str
            | true => "T"
            | false => "F"
            end;
        let n = BigInt/of_int(-101);
        /std/print(Str/join(",", [
            BigInt/to_str(BigInt/neg(n)),
            BigInt/to_str(BigInt/abs(n)),
            BigInt/to_str(BigInt/div2(n)),
            show(BigInt/is_even(n)),
            show(BigInt/is_even(BigInt/div2(n)))]))
        "#;
    assert_eq!(run(source), b"+101,+101,-50,F,T");
}

#[test]
fn bigint_operators_dispatch_through_concepts() {
    // The `/std` facades carry `Add`/`Subtract`/`Multiply`/`Equal`/`Compare`/`Show` witnesses for `BigInt`, so the operator syntax and `show` resolve on it like on any native numeric type.
    let source = r#"
        use /std/{Handle, Str, Bool, List, BigInt, Show};
        let a = BigInt/of_int(-6);
        let b = BigInt/of_int(+2);
        let show_bool(v : Bool) -> Str =
            match v : (_) => Str
            | true => "T"
            | false => "F"
            end;
        /std/print(Str/join(",", [
            Show/show(a + b),
            Show/show(a - b),
            Show/show(a * b),
            show_bool(a == b),
            show_bool(a < b)]))
        "#;
    assert_eq!(run(source), b"-4,-8,-12,F,T");
}
