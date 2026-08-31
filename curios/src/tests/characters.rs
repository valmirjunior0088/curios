//! Character literals as numerals: realization at each carrier with `Char` as the shape default, and dispatch as `Nat` pattern literals.

use super::{error, run};

#[test]
fn a_character_realizes_at_the_expected_carrier() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Int, Byte, Bool, Char, Str, Io, Show, print};
        let line(s: Str) -> Io({}) = print(Str/concat(s, "\n"));
        let n: Nat = 'a';
        let b: Byte = 'a';
        let i: Int = 'a';
        let c = 'a';
        let _ = line(Nat/to_str(n))!;
        let _ = line(Show/show(b))!;
        let _ = line(Int/to_str(i))!;
        let _ = line(Show/show(c))!;
        line(Nat/to_str('\n'))
        "#),
        b"97\n97\n+97\na\n10\n"
    );
}

#[test]
fn an_operator_pins_a_character_operand_from_context() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Io};
        let n: Nat = 'a' + 1;
        /std/print(Str/concat(Nat/to_str(n), "\n"))
        "#),
        b"98\n"
    );
}

#[test]
fn a_packed_literal_takes_character_atoms() {
    assert_eq!(
        run(r#"
        use /std/{Bytes, Str, Option, Io};
        /std/print(Option/unwrap_or(Str/of_bytes(x['H', 'i', '\n']), ""))
        "#),
        b"Hi\n"
    );
}

#[test]
fn a_character_does_not_realize_at_flt_or_bool() {
    let report = error(
        r#"
        use /std/{Flt, Io};
        let f: Flt = 'a';
        /std/print("")
        "#,
    );
    assert!(report.contains("mismatch"), "{report}");

    let report = error(
        r#"
        use /std/{Bool, Io};
        let z: Bool = 'a';
        /std/print("")
        "#,
    );
    assert!(report.contains("mismatch"), "{report}");
}

#[test]
fn a_character_past_the_byte_range_is_refused() {
    let report = error(
        r#"
        use /std/{Byte, Io};
        let b: Byte = '€';
        /std/print("")
        "#,
    );
    assert!(report.contains("Byte"), "{report}");
}

#[test]
fn a_character_dispatches_as_a_nat_literal() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Char, Str, Io};
        let name(c: Char) -> Str =
            match Char/to_nat(c)
            | '\n' => "newline"
            | '\\' => "backslash"
            | 'a' => "letter a"
            | 0 => "nul"
            | _ => "other"
            end;
        let _ = /std/print(Str/concat(name('\n'), "\n"))!;
        let _ = /std/print(Str/concat(name('\\'), "\n"))!;
        let _ = /std/print(Str/concat(name('a'), "\n"))!;
        let _ = /std/print(Str/concat(name('b'), "\n"))!;
        /std/Io/pure(())
        "#),
        b"newline\nbackslash\nletter a\nother\n"
    );
}

#[test]
fn a_character_pattern_nests_inside_a_constructor() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Option, Str, Io};
        let m: Option(Nat) = Option/some(120);
        match m
        | some('x') => /std/print("the letter x\n")
        | some('y') => /std/print("the letter y\n")
        | _ => /std/print("something else\n")
        end
        "#),
        b"the letter x\n"
    );
}

#[test]
fn a_character_case_cannot_join_successor_peeling() {
    let report = error(
        r#"
        use /std/{Nat, Io};
        let f(n: Nat) -> Nat =
            match n
            | 'a' => 0
            | p + 1; _ => p
            | _ => 1
            end;
        /std/print("")
        "#,
    );
    assert!(!report.is_empty(), "{report}");
}

#[test]
fn a_character_dispatch_requires_a_default() {
    let report = error(
        r#"
        use /std/{Nat, Io};
        let f(n: Nat) -> Nat = match n | 'a' => 0 end;
        /std/print("")
        "#,
    );
    assert!(!report.is_empty(), "{report}");
}
