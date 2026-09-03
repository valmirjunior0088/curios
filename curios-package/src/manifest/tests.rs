use super::*;

/// The manifest `source` states, or the panic of it having been refused.
fn parse(source: &str) -> Manifest {
    source
        .parse()
        .expect("a manifest this test states is legal")
}

/// The refusal `source` earns, or the panic of it having been accepted.
fn refuse(source: &str) -> String {
    source
        .parse::<Manifest>()
        .expect_err("a manifest this test states is refused")
}

/// The package mode, whole: one row of every source, two executables, and a default.
#[test]
fn a_package_parses() {
    let Manifest::Package(package) = parse(&format!(
        r#"
            name = "json"
            default = "serve"

            [dependencies]
            base = {{ source = "member" }}
            toml_parse = {{ source = "catalog" }}
            http = {{ source = "git", url = "https://example/http", rev = "abc123", hash = "c1:{digest}" }}
            tools = {{ source = "path", path = "../tools" }}

            [[executables]]
            name = "serve"

            [[executables]]
            name = "bench"
            path = "tools/bench.crs"
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
        vec![
            Executable {
                name: "serve".to_string(),
                path: PathBuf::from("serve.crs"),
            },
            Executable {
                name: "bench".to_string(),
                path: PathBuf::from("tools/bench.crs"),
            },
        ]
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
    let Manifest::Umbrella(umbrella) = parse(&format!(
        r#"
            members = ["json", "http_client", "tools/cli"]

            [catalog]
            toml_parse = {{ source = "git", url = "https://example/toml", rev = "abc123", hash = "c1:{digest}" }}
        "#,
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
            json = { source = "member" }
        "#,
    );

    assert!(refusal.contains("declares none"), "{refusal}");
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

/// A dependency key is a canonical name too, and earns the same refusal.
#[test]
fn a_dependency_named_by_no_path_is_refused() {
    let refusal = refuse(
        r#"
            name = "json"

            [dependencies]
            "my-org" = { source = "member" }
        "#,
    );

    assert!(refusal.contains("no name a path could spell"), "{refusal}");
}

/// There is no string shorthand: a revision is an opaque string, so a bare one could not be told from a resolver.
#[test]
fn a_string_shorthand_row_is_refused() {
    let refusal = refuse(
        r#"
            name = "json"

            [dependencies]
            http = "1.0"
        "#,
    );

    assert!(refusal.contains("invalid type"), "{refusal}");
}

/// Live code has no pin, so a marker row carrying one is refused rather than ignored.
#[test]
fn a_pin_on_a_live_row_is_refused() {
    let refusal = refuse(
        r#"
            name = "json"

            [dependencies]
            base = { source = "member", rev = "abc123" }
        "#,
    );

    assert!(refusal.contains("takes no `rev`"), "{refusal}");
}

/// A fetchable row states both columns: the instruction that resolves it, and the criterion that accepts it.
#[test]
fn a_fetchable_row_missing_a_column_is_refused() {
    for (row, missing) in [
        (r#"{ source = "git", rev = "abc", hash = "c1:x" }"#, "`url`"),
        (
            r#"{ source = "git", url = "https://e", hash = "c1:x" }"#,
            "`rev`",
        ),
        (
            r#"{ source = "git", url = "https://e", rev = "abc" }"#,
            "`hash`",
        ),
        (r#"{ source = "path" }"#, "`path`"),
    ] {
        let refusal = refuse(&format!("name = \"json\"\n[dependencies]\nhttp = {row}"));
        assert!(refusal.contains(missing), "{row}: {refusal}");
    }
}

/// The hash's scheme is checked where the hash is written, not where it is verified.
#[test]
fn a_malformed_hash_is_refused() {
    let refusal = refuse(
        r#"
            name = "json"

            [dependencies]
            http = { source = "git", url = "https://e", rev = "abc", hash = "sha256:beef" }
        "#,
    );

    assert!(refusal.contains("names no hash scheme"), "{refusal}");
}

/// A `url` or a `rev` beginning with `-` is refused where the row is read, because `curate` hands both to `git` and git reads a leading `-` as an option.
///
/// **The hash answers for the delivery, not for the invocation that fetched it.** It is checked after `git remote add`, `git fetch` and `git checkout` have already run on these two values, and all three read a dash-leading positional as an option — so a row could reach git's option parser on a machine that had only vouched for a tree's bytes. `curate` walks each fetched dependency's own manifest for further rows, which is what puts the values under somebody else's hand.
#[test]
fn a_fetchable_column_that_would_read_as_an_option_is_refused() {
    for (field, row) in [
        (
            "url",
            r#"{ source = "git", url = "-some-option", rev = "abc", hash = "c1:x" }"#,
        ),
        (
            "rev",
            r#"{ source = "git", url = "https://e", rev = "-some-option", hash = "c1:x" }"#,
        ),
    ] {
        let refusal = refuse(&format!("name = \"json\"\n[dependencies]\nhttp = {row}"));
        assert!(
            refusal.contains(&format!("`{field}` beginning with `-`")),
            "{row}: {refusal}"
        );
        assert!(refusal.contains("would read it as an option"), "{refusal}");
    }
}

/// A dash inside a `url` or a `rev` is ordinary, and only the leading one is refused: the rule is about what git's option parser reads, not about the character.
#[test]
fn a_dash_inside_a_fetchable_column_is_ordinary() {
    let Manifest::Package(package) = parse(&format!(
        r#"
            name = "json"

            [dependencies]
            http = {{ source = "git", url = "https://example/a-b", rev = "release-1.2", hash = "c1:{digest}" }}
        "#,
        digest = "a".repeat(64)
    )) else {
        panic!("a manifest declaring `name` is a package");
    };

    assert!(package.dependencies.contains_key("http"));
}

/// An executable's name is what `curios run <name>` dispatches on, so it cannot be spelled like a file.
#[test]
fn an_executable_named_by_no_identifier_is_refused() {
    let refusal = refuse(
        r#"
            name = "json"

            [[executables]]
            name = "run-me"
        "#,
    );

    assert!(refusal.contains("single identifier"), "{refusal}");
}

#[test]
fn an_executable_declared_twice_is_refused() {
    let refusal = refuse(
        r#"
            name = "json"

            [[executables]]
            name = "serve"

            [[executables]]
            name = "serve"
            path = "other.crs"
        "#,
    );

    assert!(refusal.contains("declared twice"), "{refusal}");
}

/// The package root has one stem space, and the library header already holds a stem in it.
#[test]
fn an_executable_over_the_library_header_is_refused() {
    let refusal = refuse(
        r#"
            name = "json"

            [[executables]]
            name = "lib"
        "#,
    );

    assert!(refusal.contains("library header"), "{refusal}");
}

/// The stem-space refusals compare a row's path by spelling, so a spelling that names the same file another way — `./lib.crs`, `app/../lib.crs`, an absolute path — is refused where the row is read rather than walked past to a parse error inside the header.
#[test]
fn an_executable_path_that_is_not_plain_is_refused() {
    for path in ["./lib.crs", "app/../lib.crs", "/tmp/app.crs", "./app.crs"] {
        let refusal = refuse(&format!(
            r#"
                name = "json"

                [[executables]]
                name = "app"
                path = "{path}"
            "#,
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

    assert!(refusal.contains("does not declare"), "{refusal}");
}

/// A catalog row is what a marker resolves to, so it cannot be a marker itself.
#[test]
fn a_marker_in_the_catalog_is_refused() {
    let refusal = refuse(
        r#"
            members = ["json"]

            [catalog]
            base = { source = "member" }
        "#,
    );

    assert!(refusal.contains("names a marker source"), "{refusal}");
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
