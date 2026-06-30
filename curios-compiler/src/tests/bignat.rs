use {super::run, curios_runtime::MockHost, std::time::Duration};

#[test]
fn bignat_add_carries_across_limbs() {
    // `add` propagates carry across base-10^4 limbs: 99_999_999 (= [9999, 9999])
    // plus 1 ripples a carry through both limbs and grows a new top limb,
    // yielding [0, 0, 1] = 100_000_000.
    let source = r#"
        use /std/{Io, Str, BigNat};
        Io/print(BigNat/to_str(BigNat/add(BigNat/of_nat(99999999), BigNat/of_nat(1))))
        "#;
    assert_eq!(run(source), b"100000000");
}

#[test]
fn bignat_sub_borrows_and_trims() {
    // `sub` borrows across limbs and then trims the high zero limbs the borrow
    // leaves behind: 100_000_000 (= [0, 0, 1]) minus 1 borrows twice to [9999,
    // 9999, 0], which must trim back to the canonical [9999, 9999] = 99_999_999.
    let source = r#"
        use /std/{Io, Str, BigNat};
        Io/print(BigNat/to_str(BigNat/sub(BigNat/of_nat(100000000), BigNat/of_nat(1))))
        "#;
    assert_eq!(run(source), b"99999999");
}

#[test]
fn bignat_mul_small_propagates_carry() {
    // `mul_small` carries a value that itself exceeds the base: 9999 * 99999 =
    // 999_890_001, whose final carry (99_989) must be split into multiple limbs
    // rather than dropped into one.
    let source = r#"
        use /std/{Io, Str, BigNat};
        Io/print(BigNat/to_str(BigNat/mul_small(BigNat/of_nat(9999), 99999)))
        "#;
    assert_eq!(run(source), b"999890001");
}

#[test]
fn bignat_mul_pow2_builds_large_powers() {
    // `mul_pow2` doubles past every fixed-width integer: 2^40 = 1_099_511_627_776
    // far exceeds the 31-bit `Nat` carrier, so a correct result proves the value
    // is held across limbs, not in a single trapping `Nat`.
    let source = r#"
        use /std/{Io, Str, BigNat};
        Io/print(BigNat/to_str(BigNat/mul_pow2(BigNat/of_nat(1), 40)))
        "#;
    assert_eq!(run(source), b"1099511627776");
}

#[test]
fn bignat_compare_orders_by_magnitude() {
    // `compare` decides by the most-significant limb first (recursing to the top
    // of the little-endian list), so two values differing only in the lowest limb
    // still order correctly: 12345678 < 12345679, equal to itself, and the
    // reverse is greater.
    let source = r#"
        use /std/{Io, Str, BigNat, Order};
        let show(o : Order) -> Str =
            match o : Str
            | lt() => "lt"
            | eq() => "eq"
            | gt() => "gt"
            end;
        let a = BigNat/of_nat(12345678);
        let b = BigNat/of_nat(12345679);
        Io/print(Str/concat(Str/concat(show(BigNat/compare(a, b)), show(BigNat/compare(a, a))), show(BigNat/compare(b, a))))
        "#;
    assert_eq!(run(source), b"lteqgt");
}

#[test]
fn bignat_zero_renders_and_roundtrips() {
    // The canonical zero is the empty limb list, which `to_str` renders as "0"
    // (not the empty string), and a value whose lowest limb is zero round-trips
    // through `of_nat`/`to_str` with the high limb un-padded and the rest padded.
    let source = r#"
        use /std/{Io, Str, BigNat};
        Io/print(Str/concat(Str/concat(BigNat/to_str(BigNat/zero), "/"), BigNat/to_str(BigNat/of_nat(70000))))
        "#;
    assert_eq!(run(source), b"0/70000");
}

#[test]
#[allow(clippy::approx_constant)] // "+3.14" is a parse-and-render test vector, not π
fn flt_to_str_matches_rust_shortest_format() {
    // Stage 2: `Flt/to_str` is a real Dragon4 shortest-float renderer (BigNat-backed),
    // matching `format!("{:+}", f32)` byte-for-byte — no longer the `of_bin` shim. The
    // result is assembled from `Str` literals + `Nat/to_str` digits via `Str/concat`, so
    // it carries the UTF-8 proof through `concat_closed` (closing the Stage 3 gap too).
    // Expectations come straight from Rust's own `{:+}` so the test cannot drift from the
    // oracle the host renderer used to call.
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
        use /std/{{Io, Str, Flt, Arr}};
        Io/print(Str/join("|", [{array}]))
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
fn flt_to_le_bin_prints_raw_bytes() {
    let source = r#"
        std/Io/write(std/Io/stdout, std/Flt/to_le_bin(+1.5))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), 1.5f32.to_le_bytes());
}
