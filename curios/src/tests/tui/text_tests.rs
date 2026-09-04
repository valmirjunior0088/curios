//! Cell width, and the truncation and wrapping written over it.

use crate::tests::run;

// The three widths the table exists to separate: a Latin scalar at one, a CJK ideograph at two, and a combining mark at zero — so `e` followed by U+0301 measures one column while `Str/len` counts it as the two scalars it is. Curios string literals have no `\u` escape, so the sequence is interpolated from Rust rather than written in the program.
#[test]
fn width_counts_columns_rather_than_scalars() {
    let combining = "e\u{301}";
    let source = format!(
        r#"
        use /std/{{Nat, Str, Handle, Tui}};
        use /std/Tui/{{text}};
        /std/print(Str/join(" ", [
            Nat/to_str(text/width("hello")),
            Nat/to_str(text/width("{combining}")),
            Nat/to_str(Str/len("{combining}")),
            Nat/to_str(text/width("日本")),
            Nat/to_str(text/width("")),
        ]))
        "#
    );

    assert_eq!(run(&source), b"5 1 2 4 0");
}

// A double-width scalar that would straddle the boundary is dropped whole: truncating `"日本"` to three columns keeps one ideograph and leaves the result two columns wide, never one and a half.
#[test]
fn truncate_never_splits_a_wide_scalar() {
    let source = r#"
        use /std/{Nat, Str, Handle, Tui};
        use /std/Tui/{text};
        /std/print(Str/join("|", [
            text/truncate("abcdef", 3),
            text/truncate("abc", 10),
            text/truncate("日本", 3),
            text/truncate("abc", 0),
        ]))
        "#;

    assert_eq!(run(source), "abc|abc|日|".as_bytes());
}

#[test]
fn wrap_packs_words_up_to_the_width() {
    let source = r#"
        use /std/{Str, Handle, Tui};
        use /std/Tui/{text};
        /std/print(Str/join("/", text/wrap("the quick brown fox", 9)))
        "#;

    assert_eq!(run(source), b"the quick/brown fox");
}

// A word wider than the pane becomes its own over-wide line rather than being split mid-word; `Tui/Image/fit` is what crops it at the pane, which keeps the one coercion in one place.
#[test]
fn wrap_leaves_an_over_wide_word_on_its_own_line() {
    let source = r#"
        use /std/{Str, Handle, Tui};
        use /std/Tui/{text};
        /std/print(Str/join("/", text/wrap("a stupendously long word", 6)))
        "#;

    assert_eq!(run(source), b"a/stupendously/long/word");
}
