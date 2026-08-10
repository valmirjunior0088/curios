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

/// The package mode, whole: a multi-segment name, one row of every source, two executables, and a default.
#[test]
fn a_package_parses() {
    let Manifest::Package(package) = parse(&format!(
        r#"
            name = "myorg/json"
            default = "serve"

            [dependencies]
            "myorg/base" = {{ source = "member" }}
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

    assert_eq!(package.name, Qualifier::from(["myorg", "json"]));
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
        vec![
            &Qualifier::from(["http"]),
            &Qualifier::from(["myorg", "base"]),
            &Qualifier::from(["toml_parse"]),
            &Qualifier::from(["tools"]),
        ]
    );
    assert_eq!(
        package.dependencies[&Qualifier::from(["myorg", "base"])],
        Dependency::Member
    );
    assert_eq!(
        package.dependencies[&Qualifier::from(["toml_parse"])],
        Dependency::Catalog
    );
    assert_eq!(
        package.dependencies[&Qualifier::from(["tools"])],
        Dependency::Path {
            path: PathBuf::from("../tools")
        }
    );
    assert!(matches!(
        &package.dependencies[&Qualifier::from(["http"])],
        Dependency::Git { url, rev, .. } if url == "https://example/http" && rev == "abc123"
    ));
}

/// A package with nothing but a name: the library its name obligates is the whole of it.
#[test]
fn a_name_alone_is_a_package() {
    let Manifest::Package(package) = parse(r#"name = "json""#) else {
        panic!("a manifest declaring `name` is a package");
    };

    assert_eq!(package.name, Qualifier::from(["json"]));
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

/// A dash is the ordinary way to spell a package name elsewhere, and the one this language cannot resolve.
#[test]
fn a_name_no_path_could_spell_is_refused() {
    for (written, reason) in [
        ("my-org/json", "no identifier"),
        ("myorg//json", "empty segment"),
        ("myorg/struct", "is a keyword"),
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
            "my-org/base" = { source = "member" }
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
