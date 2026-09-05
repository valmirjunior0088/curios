//! Trailing commas across every comma list, and the comments a parse captures as a product.

use crate::*;

use super::test_support::*;

#[test]
fn a_comment_banner_parses_without_native_recursion() {
    // One native frame per `--` line once made banner height a stack bound; the whitespace loop absorbs any run.
    let source = "-- banner\n".repeat(50_000) + "0";
    assert!(source.parse::<Term>().is_ok());
}

#[test]
fn field_lists_admit_a_trailing_comma() {
    // Every brace/paren field list — Σ-types, struct declarations, tuple literals, struct literals, concepts, witnesses — admits (and drops) one trailing comma after its last field.
    for (with, without) in [
        ("{ x : Nat, y : Nat, }", "{ x : Nat, y : Nat }"),
        ("(a, b,)", "(a, b)"),
        ("Pair { fst = a, snd = b, }", "Pair { fst = a, snd = b }"),
        (
            "Ordered(Nat) { use w, cmp = f, }",
            "Ordered(Nat) { use w, cmp = f }",
        ),
    ] {
        assert_eq!(
            with.parse::<Term>().unwrap(),
            without.parse::<Term>().unwrap(),
            "trailing comma changed the parse of {with:?}"
        );
    }

    for (with, without) in [
        (
            "struct Foo : pub Type { x : Type, } u",
            "struct Foo : pub Type { x : Type } u",
        ),
        (
            "concept Show(A : Type) : Type { show(A) -> Str, } u",
            "concept Show(A : Type) : Type { show(A) -> Str } u",
        ),
        (
            "satisfy Show(Nat) { show = Nat/to_str, } u",
            "satisfy Show(Nat) { show = Nat/to_str } u",
        ),
    ] {
        assert_eq!(
            with.parse::<Entrypoint>().unwrap(),
            without.parse::<Entrypoint>().unwrap(),
            "trailing comma changed the parse of {with:?}"
        );
    }

    // A one-element positional tuple's comma stays significant, and a lone or doubled comma stays rejected.
    assert!(matches!(
        "(x,)".parse::<Term>().unwrap().as_subterm(),
        Subterm::Tuple(_)
    ));
    assert!("{ , }".parse::<Term>().is_err());
    assert!("(a,,)".parse::<Term>().is_err());
}

// Every comma-separated list admits one optional trailing comma: the trailed spelling parses to exactly the tree of the untrailed spelling. One pair per list-owning production.
#[test]
fn trailing_comma_accepted_in_every_comma_list() {
    for (trailed, plain) in [
        // Call arguments (whitespace-eating and glued suffix forms).
        ("f(x, y,)", "f(x, y)"),
        ("f(@Nat, x,)", "f(@Nat, x)"),
        // Lambda parameters.
        ("(x,) => x", "(x) => x"),
        ("(x : Nat, y,) => y", "(x : Nat, y) => y"),
        // Function-type parameters.
        ("(Nat,) -> Nat", "(Nat) -> Nat"),
        ("(x : Nat, y : Nat,) -> Nat", "(x : Nat, y : Nat) -> Nat"),
        // List literals, with and without spreads.
        ("[1, 2,]", "[1, 2]"),
        ("[head, ..tail,]", "[head, ..tail]"),
        // Tuple literals already admitted the trailing comma; pinned here for completeness alongside the newly-uniform lists.
        ("(1, true,)", "(1, true)"),
        // Tuple types (field lists were already trailing; pinned).
        ("{Nat, Bool,}", "{Nat, Bool}"),
        // Signature-sugar parameter lists inside a tuple-type field.
        ("{run(input : Nat,) -> Nat}", "{run(input : Nat) -> Nat}"),
        // Definition-sugar parameter lists inside a tuple-literal field.
        ("(base = 3, bump(x,) = x)", "(base = 3, bump(x) = x)"),
        // Struct-literal head arguments.
        (
            "Pair(Nat, Bool,) { fst = 1 }",
            "Pair(Nat, Bool) { fst = 1 }",
        ),
        // Constructor match-pattern payloads.
        (
            "match o | some(x,) => x | none() => y end",
            "match o | some(x) => x | none() => y end",
        ),
        // A motive binder's annotation.
        (
            "match p : (x, y, q : Eq(A, x, y,)) => T | refl(v) => e end",
            "match p : (x, y, q : Eq(A, x, y)) => T | refl(v) => e end",
        ),
        // Local function-definition sugar.
        (
            "let f(x : Nat,) -> Nat = x; f(1)",
            "let f(x : Nat) -> Nat = x; f(1)",
        ),
    ] {
        assert_eq!(
            trailed.parse::<Term>().unwrap(),
            plain.parse::<Term>().unwrap(),
            "trailing-comma mismatch for {trailed:?}"
        );
    }
}

#[test]
fn trailing_comma_accepted_in_top_level_comma_lists() {
    for (trailed, plain) in [
        // Foreign wire parameters.
        (
            "foreign f : (Nat, Bytes,) -> Nat;",
            "foreign f : (Nat, Bytes) -> Nat;",
        ),
        // Use groups.
        ("use /std/{Nat, Bool,};", "use /std/{Nat, Bool};"),
        // Inductive parameters, indices, payload fields, and case targets.
        (
            "induct Opt(A : Type,) : pub Type | some(A,) | none() end",
            "induct Opt(A : Type) : pub Type | some(A) | none() end",
        ),
        (
            "induct V(T : Type) : (n : Nat,) -> pub Type | nil() : (z,) end",
            "induct V(T : Type) : (n : Nat) -> pub Type | nil() : (z) end",
        ),
        // Struct parameters (field lists were already trailing).
        (
            "struct P(A : Type,) : pub Type { a : A, }",
            "struct P(A : Type) : pub Type { a : A }",
        ),
        // Concept parameters and concept-field signature sugar.
        (
            "concept S(A : Type,) : Type { show(A,) -> Nat, }",
            "concept S(A : Type) : Type { show(A) -> Nat }",
        ),
        // Witness telescope and concept-application arguments.
        (
            "satisfy (@A : Type,) => S(A,) { show(a,) = a }",
            "satisfy (@A : Type) => S(A) { show(a) = a }",
        ),
        // Top-level function-definition sugar.
        (
            "pub let f(x : Nat,) -> Nat = x;",
            "pub let f(x : Nat) -> Nat = x;",
        ),
    ] {
        assert_eq!(
            trailed.parse::<Module>().unwrap(),
            plain.parse::<Module>().unwrap(),
            "trailing-comma mismatch for {trailed:?}"
        );
    }
}

// A separator alone is not a list: the trailing comma is admitted only after at least one item, and a lone comma still fails.
#[test]
fn lone_comma_is_not_an_empty_list() {
    assert!("f(,)".parse::<Term>().is_err());
    assert!("[,]".parse::<Term>().is_err());
    assert!("(,) => x".parse::<Term>().is_err());
    assert!("use /std/{,};".parse::<Module>().is_err());
}

#[test]
fn comments_are_captured_as_a_parse_product() {
    // Leading, interior, and trailing comments all record, in offset order, each spanning `--` through end of line.
    let source = "-- leading\nlet x : Nat = ( -- interior\n    5\n); -- trailing\n";
    assert_eq!(
        comments_of(source),
        ["-- leading", "-- interior", "-- trailing"]
    );
}

#[test]
fn a_comment_free_parse_yields_no_comments() {
    assert_eq!(comments_of("let x : Nat = 5;"), Vec::<String>::new());
}

#[test]
fn a_literal_containing_dashes_records_nothing() {
    // `--` inside a string literal is content, not a comment: the whitespace parser never runs inside a literal's interior.
    assert_eq!(
        comments_of(r#"let s : Str = "a -- b";"#),
        Vec::<String>::new()
    );
}

#[test]
fn backtracked_positions_record_a_comment_once() {
    // A parenthesized position is probed by several alternatives (dependent function type, plain parens, lambda); offset keying keeps the comment recorded once however many probes consume it.
    assert_eq!(
        comments_of("let x : Nat = ( -- probed\n    5\n);"),
        ["-- probed"]
    );
}

#[test]
fn entrypoint_parses_capture_tail_comments() {
    let source = curios_utilities::Source::inline("let x : Nat = 5; -- item\nx -- tail\n");
    let (_, comments) = Entrypoint::parse_with_comments(&source).expect("fixture parses");
    let texts = comments
        .iter()
        .map(|span| span.source.text[span.start..span.end].to_string())
        .collect::<Vec<_>>();
    assert_eq!(texts, ["-- item", "-- tail"]);
}

/// A literal mismatch quotes the token found, not as many characters as the literal is long: `=` where `=>` belongs is `'='`, not `'= '`.
#[test]
fn a_literal_mismatch_quotes_the_token_alone() {
    let report = "match n | 0 = 1 end".parse::<Term>().unwrap_err().format();
    assert!(
        report.contains("Expected '=>', obtained '='"),
        "reported {report}"
    );
    assert!(!report.contains("'= '"), "reported {report}");
}

/// A plain comment opens with `-- `, or is a bare `--` ending its line; `--` glued to a word is refused rather than read as a comment.
#[test]
fn a_comment_opens_with_a_space_or_ends_its_line() {
    assert_eq!(comments_of("-- spaced\nlet x : Nat = 5;"), ["-- spaced"]);
    assert_eq!(comments_of("--\nlet x : Nat = 5;"), ["--"]);

    let error = "--glued\nlet x : Nat = 5;"
        .parse::<Module>()
        .unwrap_err()
        .format();
    assert!(error.contains("with the space"), "{error}");
}
