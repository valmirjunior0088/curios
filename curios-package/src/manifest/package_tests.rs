//! A manifest's mode, name and executables: a package and an umbrella, the manifests that are neither or both, and an executable or default no package can hold.

use {super::test_support::*, super::*, curios_common::fill};

/// The package mode, whole: one row of every source, two executables, and a default.
#[test]
fn a_package_parses() {
    let Manifest::Package(package) = parse(&fill!(
        r#"
            name = "json"
            default = "serve"

            [dependencies]
            base = { member = true }
            toml_parse = { catalog = true }
            http = { git = "https://example/http", rev = "abc123", hash = "c1:${digest}" }
            tools = { path = "../tools" }

            [executables]
            serve = {}
            bench = { path = "tools/bench.crs" }
        "#,
        digest = "a".repeat(64)
    )) else {
        panic!("a manifest declaring `name` is a package");
    };

    assert_eq!(package.name, "json");
    assert_eq!(package.default.as_deref(), Some("serve"));

    // A declared name locates its file; an explicit path overrides it.
    assert_eq!(
        package.executables,
        BTreeMap::from([
            (
                "bench".to_string(),
                Executable {
                    path: PathBuf::from("tools/bench.crs"),
                }
            ),
            (
                "serve".to_string(),
                Executable {
                    path: PathBuf::from("serve.crs"),
                }
            ),
        ])
    );

    assert_eq!(
        package.dependencies.keys().collect::<Vec<_>>(),
        vec!["base", "http", "toml_parse", "tools"]
    );
    assert_eq!(package.dependencies["base"], Dependency::Member);
    assert_eq!(package.dependencies["toml_parse"], Dependency::Catalog);
    assert_eq!(
        package.dependencies["tools"],
        Dependency::Path {
            path: PathBuf::from("../tools")
        }
    );
    assert!(matches!(
        &package.dependencies["http"],
        Dependency::Git { url, rev, .. } if url == "https://example/http" && rev == "abc123"
    ));
}

/// A package with nothing but a name: the library its name obligates is the whole of it.
#[test]
fn a_name_alone_is_a_package() {
    let Manifest::Package(package) = parse(r#"name = "json""#) else {
        panic!("a manifest declaring `name` is a package");
    };

    assert_eq!(package.name, "json");
    assert!(package.dependencies.is_empty());
    assert!(package.executables.is_empty());
}

#[test]
fn an_umbrella_parses() {
    let Manifest::Umbrella(umbrella) = parse(&fill!(
        concat!(
            r#"
            compiler = ""#,
            env!("CARGO_PKG_VERSION"),
            r#""
            members = ["json", "http_client", "tools/cli"]

            [catalog]
            toml_parse = { git = "https://example/toml", rev = "abc123", hash = "c1:${digest}" }
        "#
        ),
        digest = "b".repeat(64)
    )) else {
        panic!("a manifest declaring `members` is an umbrella");
    };

    assert_eq!(
        umbrella.members,
        vec![
            PathBuf::from("json"),
            PathBuf::from("http_client"),
            PathBuf::from("tools/cli"),
        ]
    );
    assert_eq!(umbrella.catalog.len(), 1);
}

/// The modes are mutually exclusive, and the refusal names a key from each side.
#[test]
fn a_dual_role_manifest_is_refused() {
    let refusal = refuse(
        r#"
            name = "json"
            members = ["parser"]
        "#,
    );

    assert!(refusal.contains("never both"), "{refusal}");
    assert!(refusal.contains("`name`"), "{refusal}");
    assert!(refusal.contains("`members`"), "{refusal}");
}

#[test]
fn a_manifest_in_neither_mode_is_refused() {
    let refusal = refuse("");

    assert!(refusal.contains("declares neither"), "{refusal}");
}

/// `name` is the declaration a library is obligated by, so a package cannot omit it and keep its dependencies.
#[test]
fn a_nameless_package_is_refused() {
    let refusal = refuse(
        r#"
            [dependencies]
            json = { member = true }
        "#,
    );

    assert!(refusal.contains("declares no `name`"), "{refusal}");
}

/// A dash is the ordinary way to spell a package name elsewhere, and the one this language cannot resolve. A `/` is the other: a name is one word, so the segments that would have made `myorg/json` a namespace are refused by the same identifier check rather than by a rule of their own.
#[test]
fn a_name_no_path_could_spell_is_refused() {
    for (written, reason) in [
        ("my-org", "no identifier"),
        ("myorg/json", "no identifier"),
        ("struct", "is a keyword"),
        ("", "it is empty"),
    ] {
        let refusal = refuse(&format!("name = {written:?}"));
        assert!(refusal.contains(reason), "{written:?}: {refusal}");
    }
}

/// An executable's name is what `curios run <name>` dispatches on, so it cannot be spelled like a file.
#[test]
fn an_executable_named_by_no_identifier_is_refused() {
    let refusal = refuse(
        r#"
            name = "json"

            [executables]
            run-me = {}
        "#,
    );

    assert!(refusal.contains("not named by one identifier"), "{refusal}");
}

/// A manifest still written as rows of `[[executables]]` meets the reader's own refusal: an array is written where a table is read.
#[test]
fn executables_written_as_rows_are_refused_as_an_array() {
    let refusal = refuse("name = \"json\"\n\n[[executables]]\nname = \"serve\"\n");

    assert_eq!(
        refusal,
        "invalid value\n  written: an array\n  `executables` is a table"
    );
}

/// The package root has one stem space, and the library header already holds a stem in it.
#[test]
fn an_executable_over_the_library_header_is_refused() {
    let refusal = refuse(
        r#"
            name = "json"

            [executables]
            lib = {}
        "#,
    );

    assert!(refusal.contains("library header"), "{refusal}");
}

/// The stem-space refusal compares an executable's path by spelling, so a spelling that names the same file another way — `./lib.crs`, `app/../lib.crs`, an absolute path — is refused where the entry is read rather than walked past to a parse error inside the header.
#[test]
fn an_executable_path_that_is_not_plain_is_refused() {
    for path in ["./lib.crs", "app/../lib.crs", "/tmp/app.crs", "./app.crs"] {
        let refusal = refuse(&fill!(
            r#"
                name = "json"

                [executables]
                app = { path = "${path}" }
            "#,
            path
        ));

        assert!(
            refusal.contains("no plain relative path"),
            "{path}: {refusal}"
        );
    }
}

#[test]
fn a_dangling_default_is_refused() {
    let refusal = refuse(
        r#"
            name = "json"
            default = "serve"
        "#,
    );

    assert!(
        refusal.contains("declares no executable of that name"),
        "{refusal}"
    );
}

/// A catalog row is what a marker resolves to, so it cannot be a marker itself.
#[test]
fn a_marker_in_the_catalog_is_refused() {
    let refusal = refuse(concat!(
        r#"
            compiler = ""#,
        env!("CARGO_PKG_VERSION"),
        r#""
            members = ["json"]

            [catalog]
            base = { member = true }
        "#
    ));

    assert!(refusal.contains("is a marker"), "{refusal}");
}

/// There is no privilege field, and a manifest cannot invent one.
#[test]
fn an_unknown_key_is_refused() {
    let refusal = refuse(
        r#"
            name = "json"
            privileged = true
        "#,
    );

    assert!(refusal.contains("unknown field"), "{refusal}");
}
