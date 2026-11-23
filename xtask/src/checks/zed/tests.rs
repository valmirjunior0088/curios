//! What the Zed check reads of the manifests it holds to each other, over manifests of the tests' own.

use super::*;

#[test]
fn each_grammar_the_extension_pins_is_read_with_its_rev_and_path_in_order() {
    let manifest = r#"
id = "curios"

[grammars.curios]
repository = "https://example.invalid/curios"
rev = "aaa"
path = "editors/tree-sitter/base"

[grammars.curios_core]
repository = "https://example.invalid/curios"
rev = "bbb"
path = "editors/tree-sitter/core"

[language_servers.curios]
name = "Curios"
"#;

    assert_eq!(
        pins(manifest).unwrap(),
        [
            Pin {
                rev: "aaa".to_string(),
                path: "editors/tree-sitter/base".to_string()
            },
            Pin {
                rev: "bbb".to_string(),
                path: "editors/tree-sitter/core".to_string()
            },
        ]
    );
}

#[test]
fn a_grammar_pinned_without_its_rev_is_refused_by_its_name() {
    let refusal = pins("[grammars.curios]\npath = \"editors/tree-sitter/base\"\n").unwrap_err();

    assert_eq!(refusal, "extension.toml's grammar `curios` states no `rev`");
}

/// The tables agree where each states the same lints at the same values, whatever order each writes them in; a value that differs, and a lint one states and the other does not, are each named.
#[test]
fn a_lint_the_copy_states_otherwise_is_named_whatever_order_either_writes_them_in() {
    let workspace = "[workspace.lints.rust]\nwarnings = \"deny\"\nunused = \"warn\"\n\n[workspace.lints.clippy]\nunused_self = \"warn\"\n";
    let own =
        |rust: &str| format!("[lints.rust]\n{rust}\n[lints.clippy]\nunused_self = \"warn\"\n");
    let held = lints(workspace, &["workspace", "lints"]).unwrap();
    let departed = |rust: &str| departures(&held, &lints(&own(rust), &["lints"]).unwrap());

    assert_eq!(
        departed("unused = \"warn\"\nwarnings = \"deny\"\n"),
        Vec::<String>::new()
    );
    assert_eq!(
        departed("unused = \"deny\"\nwarnings = \"deny\"\nmissing_docs = \"warn\"\n"),
        [
            "the lint `rust.missing_docs` is unstated in Cargo.toml and `\"warn\"` in editors/zed/Cargo.toml, a copy of the workspace's table",
            "the lint `rust.unused` is `\"warn\"` in Cargo.toml and `\"deny\"` in editors/zed/Cargo.toml, a copy of the workspace's table",
        ]
    );
}
