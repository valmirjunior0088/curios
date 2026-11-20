use {super::*, crate::Manifest, std::path::PathBuf};

/// The report of what `text`, a manifest at `app/curios.toml`, is refused for.
fn reported(text: &str) -> Report {
    let source = Source::held("app/curios.toml", text);

    text.parse::<Manifest>()
        .expect_err("a manifest this test states is refused")
        .located(&source)
}

/// The text `report` is about, and the line and the column it begins at.
fn placed(report: &Report) -> (String, (usize, usize)) {
    let span = report.span().expect("the refusal names a place");

    (
        span.source.text()[span.start..span.end].to_string(),
        span.line_column(),
    )
}

/// A refusal of a value stands at the value, and one of an entry at its key, each by the keys that lead to it.
#[test]
fn a_refusal_stands_where_its_keys_lead() {
    let version = reported("name = \"app\"\ncompiler = \"^1.2\"\n");
    assert_eq!(placed(&version), ("\"^1.2\"".to_string(), (2, 12)));

    let surplus =
        reported("name = \"app\"\n\n[dependencies]\njson = { path = \"../json\", rev = \"1\" }\n");
    assert_eq!(placed(&surplus), ("rev".to_string(), (4, 28)));

    let missing = reported("name = \"app\"\n\n[dependencies]\njson = { git = \"https://e\" }\n");
    assert_eq!(placed(&missing), ("json".to_string(), (4, 1)));

    let sources = reported(
        "name = \"app\"\n\n[dependencies]\njson = { git = \"https://e\", path = \"../json\" }\n",
    );
    assert_eq!(sources.headline, "conflicting fields");
    assert_eq!(placed(&sources), ("path".to_string(), (4, 29)));

    let path = reported(
        "name = \"app\"\n\n[executables]\nserve = {}\nbench = { path = \"../bench.crs\" }\n",
    );
    assert_eq!(placed(&path), ("\"../bench.crs\"".to_string(), (5, 18)));

    let named = reported("name = \"app\"\n\n[executables]\nrun-me = {}\n");
    assert_eq!(named.headline, "invalid name");
    assert_eq!(placed(&named), ("run-me".to_string(), (4, 1)));

    let hash = reported(
        "name = \"app\"\n\n[foreign.digest.native]\npath = \"digest.wasm\"\nhash = \"sha1:0\"\n",
    );
    assert_eq!(hash.headline, "unknown hash scheme");
    assert_eq!(placed(&hash), ("\"sha1:0\"".to_string(), (5, 8)));

    let moduleless = reported("name = \"app\"\n\n[foreign.digest]\n");
    assert_eq!(moduleless.headline, "missing module");
    assert_eq!(placed(&moduleless), ("digest".to_string(), (3, 10)));
}

/// A key written twice is TOML's refusal, standing at the second; a table written as an array of rows is the reader's, standing at its header.
#[test]
fn a_key_written_twice_and_an_array_where_a_table_is_read_stand_where_they_are_written() {
    let twice =
        reported("name = \"app\"\n\n[executables]\nserve = {}\nserve = { path = \"other.crs\" }\n");
    assert_eq!(twice.headline, "syntax error");
    assert_eq!(placed(&twice), ("serve".to_string(), (5, 1)));

    let rows = reported("name = \"app\"\n\n[[executables]]\nname = \"serve\"\n");
    assert_eq!(rows.headline, "invalid value");
    assert_eq!(placed(&rows), ("[[executables]]".to_string(), (3, 1)));
}

/// A field no table takes is refused at its key, as text that is no TOML is where its reader stopped, and a manifest refused as a whole is reported about its file.
#[test]
fn the_readers_own_refusal_keeps_its_place_and_a_whole_manifest_its_file() {
    let key = reported("name = \"app\"\nnmae = \"app\"\n");
    assert_eq!(key.headline, "unknown field");
    assert_eq!(placed(&key).1, (2, 1));

    let empty = reported("");
    assert_eq!(empty.headline, "empty manifest");
    assert_eq!(
        empty.about,
        Some(About::Path(PathBuf::from("app/curios.toml")))
    );
}

/// A way out that is a command is carried to the report.
#[test]
fn a_command_that_is_a_way_out_reaches_the_report() {
    let unpinned = reported("name = \"app\"\n\n[foreign.digest.native]\npath = \"digest.wasm\"\n");

    assert_eq!(unpinned.headline, "missing hash");
    assert_eq!(
        unpinned.tries,
        ["curios pin foreign digest --target native --refresh"]
    );
    assert_eq!(
        unpinned.rendered(curios_common::Kind::Error, &curios_common::Style::plain()),
        concat!(
            "missing hash\n",
            "│  `digest` states no `hash` in `[foreign.digest.native]`\n",
            "│  a file is accepted against one however it arrives\n",
            "│\n",
            "│  3 │ [foreign.digest.native]\n",
            "│    │                 ^^^^^^\n",
            "│\n",
            "├─ try; curios pin foreign digest --target native --refresh\n",
            "└─ [error] app/curios.toml:3:17",
        )
    );
}
