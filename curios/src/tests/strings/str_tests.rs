//! The `Str` surface: indexing, slicing and trimming at codepoint boundaries.

use super::super::run;

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

// `Str/len` and `Str/get` count and index by codepoint, not byte. The string is `a€😀` — a 1-byte, a 3-byte, and a 4-byte scalar — so its length is 3 and the codepoints decode to U+0061 (97), U+20AC (8364), and U+1F600 (128512).
#[test]
fn get_indexes_codepoints_of_every_width() {
    let source = r#"
        use /std/{Str, Char, Nat, Handle, Option};
        match Str/of_bytes(x[0x61, 0xe2, 0x82, 0xac, 0xf0, 0x9f, 0x98, 0x80]) : (_) => /std/Io({})
        | some(s) =>
            /std/print(Str/flatten([
                Nat/to_str(Str/len(s)), ",",
                Nat/to_str(Char/to_nat(Option/unwrap_or(Str/try_get(s, 0), '?'))), ",",
                Nat/to_str(Char/to_nat(Option/unwrap_or(Str/try_get(s, 1), '?'))), ",",
                Nat/to_str(Char/to_nat(Option/unwrap_or(Str/try_get(s, 2), '?')))
            ]))
        | none() => /std/print("bad")
        end
        "#;

    assert_eq!(run(source), b"3,97,8364,128512");
}

// `Str/get` is the proof-carrying indexer: `ok : Lt(i, len s)` flows (erased) into the `Bytes/get` bound, so it reads each codepoint with no fallback. Indexing `a€😀` at 0, 1, 2 yields U+0061, U+20AC, U+1F600 — same widths as `get`, but total.
#[test]
fn str_at_reads_codepoints_with_the_proof() {
    let source = r#"
        use /std/{Str, Char, Nat, Handle, Option};
        match Str/of_bytes(x[0x61, 0xe2, 0x82, 0xac, 0xf0, 0x9f, 0x98, 0x80]) : (_) => /std/Io({})
        | some(s) =>
            let out =
                let r0 = Nat/Lt/try(0, Str/len(s));
                let r1 = Nat/Lt/try(1, Str/len(s));
                let r2 = Nat/Lt/try(2, Str/len(s));
                match r0 : (_) => Option(Str)
                | none() => Option/none()
                | some(p0) => match r1 : (_) => Option(Str)
                | none() => Option/none()
                | some(p1) => match r2 : (_) => Option(Str)
                | none() => Option/none()
                | some(p2) => Option/some(Str/flatten([
                    Nat/to_str(Char/to_nat(Str/get(s, 0, p0))), ",",
                    Nat/to_str(Char/to_nat(Str/get(s, 1, p1))), ",",
                    Nat/to_str(Char/to_nat(Str/get(s, 2, p2)))]))
                end end end;
            /std/print(Option/unwrap_or(out, "oob"))
        | none() => /std/print("bad")
        end
        "#;

    assert_eq!(run(source), b"97,8364,128512");
}

// `Str/slice` cuts at codepoint boundaries, so taking one scalar from index 1 of `a€😀` yields the whole 3-byte euro sign — never a split sequence.
#[test]
fn slice_cuts_on_codepoint_boundaries() {
    let source = r#"
        use /std/{Str, Handle};
        match Str/of_bytes(x[0x61, 0xe2, 0x82, 0xac, 0xf0, 0x9f, 0x98, 0x80]) : (_) => /std/Io({})
        | some(s) => /std/print(Str/slice(s, 1, 1))
        | none() => /std/print("bad")
        end
        "#;

    assert_eq!(run(source), [0xe2, 0x82, 0xac]);
}

// An interior `Str/slice` over a mixed-width string exercises the single-pass O(n) cut: `drop_n` skips the leading `a` (1 byte) and `take_n` keeps the next three scalars (`é€😀`, of widths 2, 3, 4) as one window — never splitting a sequence. Three scalars from index 1 of `aé€😀b` yields `é€😀`.
#[test]
fn slice_spans_every_codepoint_width() {
    let source = r#"
        use /std/{Str, Handle};
        match Str/of_bytes(x[0x61, 0xc3, 0xa9, 0xe2, 0x82, 0xac, 0xf0, 0x9f, 0x98, 0x80, 0x62]) : (_) => /std/Io({})
        | some(s) => /std/print(Str/slice(s, 1, 3))
        | none() => /std/print("bad")
        end
        "#;

    assert_eq!(
        run(source),
        [0xc3, 0xa9, 0xe2, 0x82, 0xac, 0xf0, 0x9f, 0x98, 0x80]
    );
}

// `Str/trim` is string-typed and strips only the leading/trailing ASCII whitespace, leaving the interior multibyte scalar (`café`, with a 2-byte `é`) intact.
#[test]
fn trim_keeps_interior_multibyte() {
    let source = r#"
        use /std/{Str, Handle};
        match Str/of_bytes(x[0x20, 0x20, 0x63, 0x61, 0x66, 0xc3, 0xa9, 0x20, 0x20]) : (_) => /std/Io({})
        | some(s) => /std/print(Str/trim(s))
        | none() => /std/print("bad")
        end
        "#;

    assert_eq!(run(source), [0x63, 0x61, 0x66, 0xc3, 0xa9]);
}

// An all-whitespace string trims to empty: `trim_start` overshoots `trim_end`, and the `Nat/min` guard collapses the slice to nothing rather than trapping.
#[test]
fn trim_all_whitespace_is_empty() {
    let source = r#"
        use /std/{Str, Handle};
        match Str/of_bytes(x[0x20, 0x09, 0x20]) : (_) => /std/Io({})
        | some(s) => /std/print(Str/concat(Str/trim(s), "!"))
        | none() => /std/print("bad")
        end
        "#;

    assert_eq!(run(source), b"!");
}

#[test]
fn logical_operations_use_certified_chars() {
    let source = r#"
        use /std/{Char, Str, Nat, Option, Show, Handle};
        let s = "a€😀";
        let rebuilt = Str/fold(s, "", (c, acc) => Str/concat(acc, Show/show(c)));
        let second = Show/show(Option/unwrap_or(Str/try_get(s, 1), '?'));
        let euro = Option/unwrap_or(Str/find(s, '€'), 99);
        let supplementary = Option/unwrap_or(Str/find_index(s, (c) => Char/to_nat(c) > 0xFFFF), 99);
        let shown = Show/show('😀');
        let folded = Str/eql_ascii_ci("AbÉ", "aBÉ");
        let not_unicode_folded = Str/eql_ascii_ci("É", "é");
        /std/print(Str/flatten([
            rebuilt, "|", second, "|", Nat/to_str(euro), "|",
            Nat/to_str(supplementary), "|", shown, "|",
            /std/Bool/to_str(folded), "|", /std/Bool/to_str(not_unicode_folded), "|",
            Nat/to_str(Str/len(s)), "|", Str/slice(s, 1, 1)
        ]))
        "#;

    assert_eq!(run(source), "a€😀|€|1|2|😀|true|false|3|€".as_bytes());
}
