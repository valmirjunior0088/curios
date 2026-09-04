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

/// A document is the whole input: a value followed by anything is refused, not read as its prefix.
///
/// `decode` used to be the bare value parser, and `Parse/run` hands back no position — so `nulll` decoded as `null`, `truex` as `true`, and `"a""b"` as `"a"`, with the rest discarded and no way for a caller to learn it. `tru` refusing while `truex` succeeded is the shape of it: the incomplete keyword was structural and the keyword with junk glued on was not. `Toml/decode` reaches its own end the same way.
#[test]
fn a_document_is_refused_when_anything_follows_the_value() {
    let source = r#"
        use /std/{Json, Parse, Result, Str, Handle};
        let decoded(input : Str) -> Str =
            match Parse/run(Json/decode, Str/to_bytes(input)) : (_) => Str
            | success(value) => Json/encode(value)
            | failure(msg) => msg
            end;
        /std/print(Str/join("|", [
            decoded("nulll"),
            decoded("truex"),
            decoded("1 garbage"),
            decoded("[1,2] junk"),
            decoded("{} }}}"),
            decoded("\"a\"\"b\""),
            decoded("  { \"a\" : 1 }  \n"),
            decoded("[]")
        ]))
        "#;

    // The last two are the control: trivia on either side of a document is still trivia, since the value parsers consume only what precedes them.
    assert_eq!(
        run(source),
        concat!(
            "expected end of input|expected end of input|expected end of input|",
            "expected end of input|expected end of input|expected end of input|",
            "{\"a\":1}|[]"
        )
        .as_bytes()
    );
}

/// Every number the encoder writes is a JSON number, so a non-finite one becomes `null` rather than a literal the grammar has no room for.
///
/// `Flt/to_str` writes `+inf`, `-inf` and `NaN`, and `encode_num` passed all three straight through — so `Json/encode` produced documents this module's own `decode` refuses, from arithmetic nobody spelled: an overflowing multiply is the fourth row. `Flt`'s `Spell` witness detects the same three from the same rendering, for the same reason.
#[test]
fn a_non_finite_number_encodes_as_null() {
    let source = r#"
        use /std/{Json, Parse, Result, Str, Flt, Handle};
        let written(value : Json) -> Str =
            let text = Json/encode(value);
            match Parse/run(Json/decode, Str/to_bytes(text)) : (_) => Str
            | success(_) => text
            | failure(msg) => Str/concat("NOT JSON: ", msg)
            end;
        /std/print(Str/join("|", [
            written(Json/num(Flt/pos_inf)),
            written(Json/num(Flt/neg_inf)),
            written(Json/num(Flt/nan)),
            written(Json/num(Flt/mul(1.0e30, 1.0e30))),
            written(Json/arr([Json/num(1.0), Json/num(Flt/pos_inf)])),
            written(Json/num(1.5)),
            written(Json/num(-1.5)),
            written(Json/num(-0.0))
        ]))
        "#;

    // Each row is written *and* read back, so a rendering the grammar does not admit shows up as its refusal rather than as text. The finite tail is the control: the `+` a non-negative `to_str` writes is still stripped, and a negative sign still is not.
    assert_eq!(
        run(source),
        "null|null|null|null|[1,null]|1.5|-1.5|-0".as_bytes()
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
