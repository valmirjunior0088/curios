use {crate::*, curios_common::Source};

/// The refusal `text` earns as a manifest, reported where it stands: the tag and the facts, then the text it stands at.
fn reported(text: &str) -> (String, String) {
    let source = Source::held("app/curios.toml", text);
    let refused = text
        .parse::<Manifest>()
        .expect_err("a manifest this test states is refused");
    let said = refused.said.to_string();
    let report = refused.located(&source);
    let span = report.span().expect("the refusal names a place");

    (said, span.source.text()[span.start..span.end].to_string())
}

/// A field no table takes is refused at its key, with the fields its table takes and the one it was nearest to.
#[test]
fn a_field_no_table_takes_is_refused_at_its_key() {
    let (said, at) = reported("name = \"app\"\n\n[format]\nline-widht = 80\n");

    assert_eq!(
        said,
        "unknown field\n  `line-widht` is no field of `[format]`\n  it takes: `line-width`, `flat-match-width`, `flat-match-arms`\n  the nearest: `line-width`"
    );
    assert_eq!(at, "line-widht");
}

/// A row that leaves out a field it requires is refused at the row, by the table TOML heads it with.
#[test]
fn a_row_missing_a_field_it_requires_names_its_table() {
    let (said, at) = reported("name = \"app\"\n\n[dependencies]\nhttp = { path = \"../http\" }\n");

    assert_eq!(
        said,
        "missing field\n  `[dependencies.http]` requires `source`"
    );
    assert_eq!(at, "http");
}

/// A value of another kind than its key takes is refused at the value, as written, with what the key takes in TOML's words.
#[test]
fn a_value_of_another_kind_says_what_its_key_takes() {
    let (said, at) = reported("name = \"app\"\nmembers = [\"core\", 7]\n");

    assert_eq!(
        said,
        "invalid value\n  written: `7`\n  each entry of `members` is a string"
    );
    assert_eq!(at, "7");
}

/// A string none of the ones its key takes is refused with each it does take, as TOML writes them.
#[test]
fn a_string_none_its_key_takes_lists_those_it_does() {
    let (said, at) = reported("name = \"app\"\n\n[dependencies]\nhttp = { source = \"gti\" }\n");

    assert_eq!(
        said,
        "invalid value\n  written: `\"gti\"`\n  `source` is one of: `\"member\"`, `\"catalog\"`, `\"git\"`, `\"path\"`"
    );
    assert_eq!(at, "\"gti\"");
}
