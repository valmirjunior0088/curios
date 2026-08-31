//! JSON escapes and structural refusals, and the characters that do not coerce to a numeric domain.

use crate::tests::{error, run};

#[test]
fn unicode_escapes_require_well_formed_surrogate_pairs() {
    let source = r#"
        use /std/{Json, Parse, Result, Str, Handle};
        use /std/Json/{str};
        let decoded(input : Str) -> Str =
            match Parse/run(Json/decode, Str/to_bytes(input)) : (_) => Str
            | success(value) =>
                match value : (_) => Str
                | str(s) => s
                | _ => "wrong"
                end
            | failure(msg) => msg
            end;
        /std/print(Str/join("|", [
            decoded("\"\\uD83D\\uDE00\""),
            decoded("\"\\uD83D\""),
            decoded("\"\\uD83D\\u0041\""),
            decoded("\"\\uDE00\""),
            decoded("\"a\\qb\"")
        ]))
        "#;

    // Each rejection is pinned to its reason. The backslash commits the escape parser, so a malformed escape reports itself rather than ending the string segment and failing on the closing quote that is not there — `many0(or(escape, segment))` once reported every one of these as `unexpected byte`.
    assert_eq!(
        run(source),
        "😀|expected low surrogate|invalid low surrogate|invalid Unicode scalar|unknown escape"
            .as_bytes()
    );
}

#[test]
fn structural_refusals_name_what_was_expected() {
    let source = r#"
        use /std/{Json, Parse, Result, Str, Handle};
        let decoded(input : Str) -> Str =
            match Parse/run(Json/decode, Str/to_bytes(input)) : (_) => Str
            | success(value) => Json/encode(value)
            | failure(msg) => msg
            end;
        /std/print(Str/join("|", [
            decoded("[]"),
            decoded("{}"),
            decoded("[true, [null], {\"a\": []}]"),
            decoded("[1,]"),
            decoded("[x]"),
            decoded("[1 2]"),
            decoded("{\"a\" 1}"),
            decoded("{\"a\": 1,}"),
            decoded("{,}"),
            decoded("tru"),
            decoded("["),
            decoded("")
        ]))
        "#;

    // Each refusal is the reason at the point the input stopped being JSON, not the bracket an outer parser failed on afterwards. A comma commits to the element after it and a key to its colon, the byte after an opening bracket decides between the close and the elements, and the primitives name what they expected — `sep_by0`'s backtracking once reported every one of the malformed inputs as `unexpected byte`, and a bad literal as `unexpected literal`.
    assert_eq!(
        run(source),
        concat!(
            "[]|{}|[true,[null],{\"a\":[]}]|expected value|expected value|expected ',' or ']'|",
            "expected ':'|expected '\"'|expected '\"'|expected \"true\"|",
            "unexpected end of input|unexpected end of input"
        )
        .as_bytes()
    );
}

#[test]
fn character_literals_do_not_coerce_to_numeric_domains() {
    for source in [
        "use /std/{Nat}; let n : Nat = 'a'; n",
        "use /std/{Byte}; let b : Byte = 'a'; b",
        "use /std/{Char, Nat}; let c : Char = 'a'; c == 97",
        "use /std/{Char, Byte}; let c : Char = 'a'; c == (0x61 : Byte)",
    ] {
        error(source);
    }
}
