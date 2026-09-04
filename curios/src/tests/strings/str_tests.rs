//! What a `Str` literal costs to compile and how one reads in a report. The `Str` surface itself — indexing, slicing and trimming at codepoint boundaries — is the corpus's `/strings/str`.

use crate::tests::{run, typecheck};

#[test]
fn literal_prints_its_bytes() {
    let source = r#"
        use /std/{Str, Handle};
        let s : Str = "hello";
        /std/print(s)
        "#;

    assert_eq!(run(source), b"hello");
}

// A literal's proof is `of_scan_eq(b, refl_scan(b))` — constant size, discharged by running the `scan_from` fold — so the term is a packed `Bytes` and an O(1) proof, and what bounds a literal's length is the reduction budget. It was not always: the proof used to be a right-nested `Utf8` derivation, one `more(c, st, t, rest)` link per byte, and elaborating it overflowed a default 2MB test thread near ~50 bytes. This 500-byte literal is an order of magnitude past that old cliff and no longer reaches deeply into anything; it is kept as the regression against a literal's cost becoming linear in its length again.
#[test]
fn long_str_literal_compiles_on_the_default_test_stack() {
    let literal = "0123456789".repeat(50); // 500 bytes: an order of magnitude past the old cliff
    let source = format!(
        r#"
        use /std/{{Str, Handle}};
        let s : Str = "{literal}";
        /std/print(s)
        "#
    );
    assert_eq!(run(&source), literal.as_bytes());
}

// A literal in a report spells as the literal. A `Str` is its bytes beside the scan witness certifying them, and a report that spells one structurally — `Str { x[0x62, 0x6F, 0x64, 0x79], of_scan_eq(…) }` for `"body"` — buries the one thing the author wrote under the representation that certifies it, which is what makes a decided proposition keyed by a written name unreadable at the moment it refuses one. Axis (f) of `Spelling`, whose identity for `Str` comes from the syntax registry rather than from a name this side of the prelude may spell.
#[test]
fn a_string_literal_spells_as_itself_in_a_report() {
    let source = r#"
        use /std/{Str, True, False};

        let Named(name: Str) -> Prop =
            match Str/eql(name, "body") | true => True | false => False end;

        let evidence: Named("body") = ?;

        /std/print("unreachable\n")
        "#;

    let error = typecheck(source).expect_err("a program with a written goal never compiles");
    assert!(error.contains(r#"Named("body")"#), "{error}");
    assert!(!error.contains("of_scan_eq"), "{error}");
}
