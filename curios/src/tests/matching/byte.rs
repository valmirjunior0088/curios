//! A byte's literals: a `Byte` column is dispatched on its values, as the natural `Byte/to_nat` gives, and a packed run's elements take them.

use {crate::tests::test_support::*, curios_common::fill};

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
            | x[] => 0
            | x['%', _, _, ..rest] => 1 + escapes(rest)
            | x['%', .._] => 100
            | x[_, ..rest] => escapes(rest)
            end;
        /std/print(Nat/to_str(escapes(x['%', 1, 2, 'z', '%'])))
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

/// A literal over a type that takes none is refused by what a literal takes apart, not as a mismatch against a natural.
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
            && error.contains("takes a `Nat`, a `Byte` or a `Bool` apart"),
        "unexpected error: {error}"
    );
}
