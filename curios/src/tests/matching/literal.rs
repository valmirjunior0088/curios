//! A literal pattern is realized at its column's type, as a literal term is at its expected one, and a numeral's sign names an `Int`.

use {crate::test_support::*, curios_common::fill};

/// A `Bool` is taken apart by `0` and `1`, its `false` and `true`, beside the truth values themselves and in a row typed by its own walk, and each arm knows the value it took.
#[test]
fn a_bool_is_taken_apart_by_its_numerals() {
    let source = r#"
        use /std/{Bool, Eq, List, Nat, Str};
        let bit(b: Bool) -> Nat => match b | 0 => 0 | 1 => 1 end;
        let mixed(b: Bool) -> Nat => match b | 0 => 10 | true => 11 end;
        let both(b: Bool, c: Bool) -> Nat => match (b, c) | (0, 0) => 1 | (_, 1) => 2 | (1, 0) => 3 end;
        let T(c: Bool) -> Type => match c | true => Nat | false => Str end;
        let pick(b: Bool, c: Bool) -> T(c) => match (b, c) | (0, 0) => "zero" | (_, 1) => 7 | (1, 0) => "one" end;
        let known(b: Bool) -> Eq()(b, Bool/not(Bool/not(b))) => match b | 0 => Eq/refl() | 1 => Eq/refl() end;
        let () = /std/print(Str/join(" ", List/map([bit(false), bit(true), mixed(false), mixed(true), both(false, false), both(false, true), both(true, true), both(true, false)], Nat/to_str)))!;
        /std/print(Str/join(" ", ["", pick(false, false), Nat/to_str(pick(false, true)), pick(true, false)]))
        "#;

    assert_eq!(run(source), b"0 1 10 11 1 2 2 3 zero 7 one");
}

/// A numeral pattern is refused where its column's type has no value it spells: one written with a sign is an `Int`'s, a `Bool` is `0` or `1`, and a successor takes only a `Nat` apart.
#[test]
fn a_numeral_pattern_is_refused_where_its_column_has_no_value_it_spells() {
    for (declaration, refusal) in [
        (
            "let f(n: Nat) -> Nat => match n | +1 => 1 | _ => 0 end;",
            "`+1` takes an `Int` apart by its value",
        ),
        (
            "let f(b: Byte) -> Nat => match b | -1 => 1 | _ => 0 end;",
            "`-1` takes an `Int` apart by its value",
        ),
        (
            "let f(b: Bool) -> Nat => match b | +1 => 1 | _ => 0 end;",
            "`+1` takes an `Int` apart by its value",
        ),
        (
            "let f(b: Bool) -> Nat => match b | 0 => 0 | 2 => 1 | _ => 2 end;",
            "`2` is no `Bool`",
        ),
        (
            "let f(b: Bool) -> Nat => match b | 'a' => 1 | _ => 0 end;",
            "takes a `Nat`, a `Byte` or a `Char` apart",
        ),
        (
            "let f(b: Bool) -> Nat => match b | 0 => 0 | p + 1 => 1 end;",
            "`p + 1` takes a `Nat` apart by its predecessor",
        ),
    ] {
        let error = error(&fill!(
            r#"
            use /std/{Bool, Byte, Nat};
            ${declaration}
            /std/print("ok")
            "#,
            declaration
        ));
        assert!(
            error.contains(refusal),
            "{declaration}: unexpected error: {error}"
        );
    }
}

/// An `Int` is taken apart by its signed keys: each arm knows the value it took, so `Eq/refl()` proves `Eq()(i, -1)` in `| -1 =>`, and the arm past the keys knows its value is none of them, so `Int/NonZero(i)` holds past `| +0 =>`.
#[test]
fn an_int_is_taken_apart_by_its_signed_keys() {
    let source = r#"
        use /std/{Eq, Int, List, Nat, Str};
        let name(i: Int) -> Str => match i | -1 => "minus one" | +0 => "zero" | +2 => "two" | _ => "other" end;
        let at(i: Int, @same: Eq()(i, -1)) -> Nat => 1;
        let known(i: Int) -> Nat => match i | -1 => at(i, @Eq/refl()) | _ => 0 end;
        let need(i: Int, @ok: Int/NonZero(i)) -> Nat => Int/abs(i);
        let past(i: Int) -> Nat => match i | +0 => 0 | _ => need(i) end;
        let () = /std/print(Str/join(" ", List/map([-1, +0, +2, +7], name)))!;
        /std/print(Str/join(" ", ["", Nat/to_str(known(-1)), Nat/to_str(known(+3)), Nat/to_str(past(-5))]))
        "#;

    assert_eq!(run(source), b"minus one zero two other 1 0 5");
}

/// An `Int`'s key is written with its sign, so a numeral without one and a character are refused there, naming the key a reader means, and a successor takes only a `Nat` apart. A key written twice is an unreachable arm, and a dispatch with no arm past its keys names the key of least magnitude no row writes, the nonnegative first.
#[test]
fn an_int_column_refuses_what_no_signed_key_spells() {
    for (declaration, refusal) in [
        (
            "let f(i: Int) -> Nat => match i | 1 => 1 | _ => 0 end;",
            "write `+1`",
        ),
        (
            "let f(i: Int) -> Nat => match i | 'a' => 1 | _ => 0 end;",
            "write `+97`",
        ),
        (
            "let f(i: Int) -> Nat => match i | p + 1 => 1 | _ => 0 end;",
            "`p + 1` takes a `Nat` apart by its predecessor",
        ),
        (
            "let f(i: Int) -> Nat => match i | -1 => 1 | +3 => 2 | +3 => 3 | _ => 0 end;",
            "unreachable arm",
        ),
        (
            "let f(i: Int) -> Nat => match i | -1 => 1 | +1 => 2 end;",
            "no arm matches `+0`",
        ),
        (
            "let f(i: Int) -> Nat => match i | +0 => 1 | +1 => 2 end;",
            "no arm matches `-1`",
        ),
    ] {
        let error = error(&fill!(
            r#"
            use /std/{Int, Nat};
            ${declaration}
            /std/print("ok")
            "#,
            declaration
        ));
        assert!(
            error.contains(refusal),
            "{declaration}: unexpected error: {error}"
        );
    }
}
