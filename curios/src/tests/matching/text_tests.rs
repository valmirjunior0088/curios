//! A character's and a string's literals: a `Char` column is dispatched on its code point, and a `Str` column taken apart by its bytes, each read at the field its literal term fills.

use {crate::tests::test_support::*, curios_common::fill};

/// A character is dispatched on its characters, alternatives among them, the rest taking every other value.
#[test]
fn a_char_is_dispatched_on_its_characters() {
    let source = r#"
        use /std/{Char, List, Nat, Str};
        let spacing(c: Char) -> Nat =
            match c
            | ' ' | '\t' => 0
            | '\n' => 1
            | 'é' => 2
            | _ => 3
            end;
        /std/print(Str/join(" ", List/map([spacing(' '), spacing('\t'), spacing('\n'), spacing('é'), spacing('x')], Nat/to_str)))
        "#;

    assert_eq!(run(source), b"0 0 1 2 3");
}

/// A string is taken apart by its bytes: strings that share a prefix, the empty string, a character past ASCII and a block string are each a case, and `_` takes every other string.
#[test]
fn a_string_is_dispatched_on_its_strings() {
    let source = r#"
        use /std/{List, Nat, Str};
        let command(name: Str) -> Nat =
            match name
            | "" => 0
            | "a" => 1
            | "ab" => 2
            | "abc" => 3
            | "b" => 4
            | "é" => 5
            | """
              two
              lines
              """ => 6
            | _ => 7
            end;
        /std/print(Str/join(" ", List/map(["", "a", "ab", "abc", "b", "é", "two\nlines", "abcd", "c"], (s) => Nat/to_str(command(s)))))
        "#;

    assert_eq!(run(source), b"0 1 2 3 4 5 6 7 7");
}

/// A string pattern stands wherever a pattern does: in a constructor's payload, a list's run and a tuple's field, beside the other columns it shares a row with.
#[test]
fn a_string_pattern_nests_where_a_pattern_does() {
    let source = r#"
        use /std/{List, Nat, Option, Str};
        let flag(o: Option(Str)) -> Nat =
            match o
            | some("--help") => 1
            | some(_) => 2
            | none() => 0
            end;
        let verb(args: List(Str)) -> Nat =
            match args
            | ["run", ..rest] => 10 + List/len(rest)
            | ["test"] => 20
            | _ => 0
            end;
        let pair(p: {Str, Nat}) -> Nat =
            match p
            | ("x", 0) => 1
            | ("x", n) => 2 + n
            | (_, n) => n
            end;
        /std/print(Str/join(" ", List/map(
            [flag(Option/some("--help")), flag(Option/some("-h")), flag(Option/none()), verb(["run", "a", "b"]), verb(["test"]), verb(["test", "a"]), pair(("x", 0)), pair(("x", 5)), pair(("y", 9))],
            Nat/to_str)))
        "#;

    assert_eq!(run(source), b"1 2 0 12 20 0 1 7 9");
}

/// What a character's and a string's dispatch refuse: a literal of another kind, a pattern of another shape, a column of another type, a case written twice, and a dispatch no binder closes, whose missing case is the empty string where no arm writes it and any other value where one does.
#[test]
fn a_text_dispatch_refuses_what_its_type_does_not_take() {
    for (declaration, refusal) in [
        (
            "let f(c: Char) -> Nat = match c | 97 => 1 | _ => 0 end;",
            "takes a `Nat` or a `Byte` apart",
        ),
        (
            "let f(c: Char) -> Nat = match c | 'a' => 1 | p + 1 => 0 end;",
            "takes a `Nat` or a `Byte` apart",
        ),
        (
            "let f(i: Int) -> Nat = match i | 'a' => 1 | _ => 0 end;",
            "takes a `Nat`, a `Byte` or a `Char` apart",
        ),
        (
            r#"let f(n: Nat) -> Nat = match n | "a" => 1 | _ => 0 end;"#,
            "takes a `Str` apart by its bytes",
        ),
        (
            r#"let f(b: Bytes) -> Nat = match b | "a" => 1 | _ => 0 end;"#,
            "takes a `Str` apart by its bytes",
        ),
        (
            r#"let f(s: Str) -> Nat = match s | "a" => 1 | x[] => 0 | _ => 2 end;"#,
            "shape mismatch",
        ),
        (
            r#"let f(s: Str) -> Nat = match s | "a" => 1 | "a" => 2 | _ => 0 end;"#,
            "unreachable arm",
        ),
        (
            r#"let f(s: Str) -> Nat = match s | "a" => 1 | "b" => 2 end;"#,
            r#"no arm matches `""`"#,
        ),
        (
            r#"let f(s: Str) -> Nat = match s | "" => 0 | "a" => 1 end;"#,
            "no arm matches `_`",
        ),
        (
            "let f(c: Char) -> Nat = match c | 'a' => 1 end;",
            "no arm matches `_`",
        ),
    ] {
        let error = error(&fill!(
            r#"
            use /std/{Bytes, Char, Int, Nat, Str};
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
