//! A literal pattern is realized at its column's type, as a literal term is at its expected one, and a numeral's sign names an `Int`.

use {crate::tests::test_support::*, curios_common::fill};

/// A `Bool` is taken apart by `0` and `1`, its `false` and `true`, beside the truth values themselves and in a row typed by its own walk, and each arm knows the value it took.
#[test]
fn a_bool_is_taken_apart_by_its_numerals() {
    let source = r#"
        use /std/{Bool, Eq, List, Nat, Str};
        let bit(b: Bool) -> Nat = match b | 0 => 0 | 1 => 1 end;
        let mixed(b: Bool) -> Nat = match b | 0 => 10 | true => 11 end;
        let both(b: Bool, c: Bool) -> Nat = match (b, c) | (0, 0) => 1 | (_, 1) => 2 | (1, 0) => 3 end;
        let T(c: Bool) -> Type = match c | true => Nat | false => Str end;
        let pick(b: Bool, c: Bool) -> T(c) = match (b, c) | (0, 0) => "zero" | (_, 1) => 7 | (1, 0) => "one" end;
        let known(b: Bool) -> Eq()(b, Bool/not(Bool/not(b))) = match b | 0 => Eq/refl() | 1 => Eq/refl() end;
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
            "let f(n: Nat) -> Nat = match n | +1 => 1 | _ => 0 end;",
            "`+1` takes an `Int` apart by its value",
        ),
        (
            "let f(b: Byte) -> Nat = match b | -0 => 1 | _ => 0 end;",
            "`-0` takes an `Int` apart by its value",
        ),
        (
            "let f(b: Bool) -> Nat = match b | +1 => 1 | _ => 0 end;",
            "`+1` takes an `Int` apart by its value",
        ),
        (
            "let f(b: Bool) -> Nat = match b | 0 => 0 | 2 => 1 | _ => 2 end;",
            "`2` is no `Bool`",
        ),
        (
            "let f(b: Bool) -> Nat = match b | 'a' => 1 | _ => 0 end;",
            "takes a `Nat`, a `Byte` or a `Char` apart",
        ),
        (
            "let f(b: Bool) -> Nat = match b | 0 => 0 | p + 1 => 1 end;",
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
