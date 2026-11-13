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

    let surplus = reported(
        "name = \"app\"\n\n[dependencies]\njson = { source = \"path\", path = \"../json\", rev = \"1\" }\n",
    );
    assert_eq!(placed(&surplus), ("rev".to_string(), (4, 45)));

    let missing = reported("name = \"app\"\n\n[dependencies]\njson = { source = \"git\" }\n");
    assert_eq!(placed(&missing), ("json".to_string(), (4, 1)));
}

/// A row of an array is found by its position, and what it states by the keys beneath it.
#[test]
fn a_row_is_found_by_its_position() {
    let twice = reported(
        "name = \"app\"\n\n[[executables]]\nname = \"serve\"\n\n[[executables]]\nname = \"serve\"\npath = \"other.crs\"\n",
    );
    assert_eq!(twice.headline, "duplicate executable");
    assert_eq!(placed(&twice).1.0, 6);

    let path = reported(
        "name = \"app\"\n\n[[executables]]\nname = \"serve\"\n\n[[executables]]\nname = \"bench\"\npath = \"../bench.crs\"\n",
    );
    assert_eq!(placed(&path), ("\"../bench.crs\"".to_string(), (8, 8)));

    let hash = reported(
        "name = \"app\"\n\n[[foreign]]\nname = \"digest\"\n\n[foreign.native]\npath = \"digest.wasm\"\nhash = \"sha1:0\"\n",
    );
    assert_eq!(hash.headline, "unknown hash scheme");
    assert_eq!(placed(&hash), ("\"sha1:0\"".to_string(), (8, 8)));
}

/// Text that is no TOML, or holds a key no table takes, is refused where its reader stopped, and a manifest refused as a whole is reported about its file.
#[test]
fn the_readers_own_refusal_keeps_its_place_and_a_whole_manifest_its_file() {
    let key = reported("name = \"app\"\nnmae = \"app\"\n");
    assert_eq!(key.headline, "syntax error");
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
    let unpinned = reported(
        "name = \"app\"\n\n[[foreign]]\nname = \"digest\"\n\n[foreign.native]\npath = \"digest.wasm\"\n",
    );

    assert_eq!(unpinned.headline, "missing hash");
    assert_eq!(
        unpinned.tries,
        ["curios pin foreign digest --target native --refresh"]
    );
    assert_eq!(
        unpinned.rendered(curios_common::Kind::Error, &curios_common::Style::plain()),
        concat!(
            "missing hash\n",
            "│  `digest` states no `hash` in `[foreign.native]`\n",
            "│  a file is accepted against one however it arrives\n",
            "├─ try; curios pin foreign digest --target native --refresh\n",
            "│\n",
            "│  6 │ [foreign.native]\n",
            "│    │          ^^^^^^\n",
            "│\n",
            "╰─ [error] app/curios.toml:6:10",
        )
    );
}
