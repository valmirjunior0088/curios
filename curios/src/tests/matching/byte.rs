//! A byte's literals: a `Byte` column is dispatched on its values, as the natural `Byte/to_nat` gives, a packed run's elements take them, and an arm knows the byte it took or the keys it was reached past.

use {crate::test_support::*, curios_common::fill};

/// A byte is dispatched on its numerals and characters, `0` among them, the rest taking every other value.
#[test]
fn a_byte_is_dispatched_on_its_literals() {
    let source = r#"
        use /std/{Byte, List, Nat, Str};
        let kind(b: Byte) -> Nat =
            match b
            | 0 => 0
            | 0x0A => 1
            | 'a' => 2
            | _ => 3
            end;
        /std/print(Str/join(" ", List/map([kind(0), kind(10), kind(97), kind(200)], Nat/to_str)))
        "#;

    assert_eq!(run(source), b"0 1 2 3");
}

/// A packed run's elements take a byte's literals, so a prefix of bytes is a pattern.
#[test]
fn a_packed_run_takes_a_bytes_literals() {
    let source = r#"
        use /std/{Bytes, Nat};
        let escapes(b: Bytes) -> Nat =
            match b
            | 0x[] => 0
            | 0x['%', _, _, ..rest] => 1 + escapes(rest)
            | 0x['%', .._] => 100
            | 0x[_, ..rest] => escapes(rest)
            end;
        /std/print(Nat/to_str(escapes(0x['%', 1, 2, 'z', '%'])))
        "#;

    assert_eq!(run(source), b"101");
}

/// What a byte's dispatch refuses: a literal past a byte, a character past one, a successor, and a dispatch no binder closes.
#[test]
fn a_bytes_dispatch_refuses_what_no_byte_is() {
    for (arms, refusal) in [
        ("| 256 => 1 | _ => 0", "`256` is no `Byte`"),
        ("| 'ā' => 1 | _ => 0", "`'ā'` is no `Byte`"),
        ("| 0 => 1 | p + 1 => 0", "no successor pattern"),
        ("| 0 => 1 | 1 => 2", "no arm matches `2`"),
    ] {
        let error = error(&fill!(
            r#"
            use /std/{Byte, Nat};
            let f(b: Byte) -> Nat = match b ${arms} end;
            /std/print(Nat/to_str(f(0)))
            "#,
            arms
        ));
        assert!(error.contains(refusal), "{arms}: unexpected error: {error}");
    }
}

/// A numeral without a sign over an integer is refused by what it takes apart, naming the signed key an integer is taken apart by.
#[test]
fn a_literal_over_an_integer_names_what_a_literal_takes_apart() {
    let source = r#"
        use /std/{Int, Nat};
        let f(i: Int) -> Nat = match i | 0 => 1 | _ => 2 end;
        /std/print(Nat/to_str(f(+0)))
        "#;

    let error = error(source);
    assert!(
        error.contains("no literal pattern")
            && error.contains("takes a `Nat`, a `Byte` or a `Bool` apart")
            && error.contains("write `+0`"),
        "unexpected error: {error}"
    );
}

/// A byte is dispatched on itself: each arm knows the byte it took, so `Eq/refl()` proves `Eq()(b, 0x41)` in `| 0x41 =>`, and the arm past the keys knows the byte is none of them, so `b != 0x41` holds there.
#[test]
fn a_bytes_arm_knows_the_byte_it_took() {
    let source = r#"
        use /std/{Bool, Byte, Eq, Nat, Str};
        use /std/Bool/{Holds};
        let at(b: Byte, @same: Eq()(b, 0x41)) -> Nat = 1;
        let known(b: Byte) -> Nat = match b | 0x41 => at(b, @Eq/refl()) | _ => 0 end;
        let need(b: Byte, @ok: Holds(b != 0x41)) -> Nat = 2;
        let past(b: Byte) -> Nat = match b | 0x41 => 0 | _ => need(b) end;
        /std/print(Str/join(" ", [Nat/to_str(known(0x41)), Nat/to_str(known(0x42)), Nat/to_str(past(0x42))]))
        "#;

    assert_eq!(run(source), b"1 0 2");
}

/// A dispatch on a byte reads what the arm around it knows of that byte: past the key `0` it takes its own arm past `0`, so `escaped` writing any byte but `0` as itself is `Eq/refl()` there, and under a guard that the byte is `0` it takes its arm at `0`.
#[test]
fn a_dispatch_on_a_byte_takes_the_arm_its_keys_tests_decide() {
    let source = r#"
        use /std/{Byte, Bytes, Eq, Nat};
        use /std/Bool/{Holds};
        let escaped(b: Bytes) -> Bytes =
            match b
            | 0x[] => 0x[]
            | 0x[0, ..t] => 0x[0x0, 0xFF, ..escaped(t)]
            | 0x[h, ..t] => 0x[h, ..escaped(t)]
            end;
        let _kept(h: Byte, t: Bytes, other: Holds(h != 0)) -> Eq()(escaped(0x[h, ..t]), 0x[h, ..escaped(t)]) =
            match h
            | 0 => match other end
            | _ => Eq/refl()
            end;
        let _doubled(h: Byte, t: Bytes, zero: Holds(h == 0)) -> Eq()(escaped(0x[h, ..t]), 0x[0x0, 0xFF, ..escaped(t)]) =
            match h == 0
            | true => Eq/refl()
            | false => match zero end
            end;
        /std/print(Nat/to_str(Bytes/length(escaped(0x[0x0, 0x41]))))
        "#;

    assert_eq!(run(source), b"3");
}
