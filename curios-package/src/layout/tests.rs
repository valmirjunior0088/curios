use {
    super::*,
    curios_base::Qualifier,
    std::{
        fs,
        path::PathBuf,
        time::{SystemTime, UNIX_EPOCH},
    },
};

/// A fresh directory nothing else is using.
fn temp_dir(name: &str) -> PathBuf {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();

    std::env::temp_dir().join(format!("curios-{name}-{}-{millis}", std::process::id()))
}

/// A package directory holding `manifest` and the files `files` names, relative to it.
fn package(name: &str, manifest: &str, files: &[(&str, &str)]) -> PathBuf {
    let directory = temp_dir(name);
    fs::create_dir_all(&directory).unwrap();
    fs::write(directory.join(MANIFEST), manifest).unwrap();

    for (path, source) in files {
        let path = directory.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, source).unwrap();
    }

    directory
}

/// The library header anchors at the manifest's directory: `mod parse` is `parse.crs` beside it, and `parse`'s own children stem-nest below.
#[test]
fn a_package_mounts_its_declared_name_over_its_manifest_directory() {
    let directory = package(
        "layout-package",
        "name = \"json\"\n",
        &[
            ("lib.crs", "pub mod parse;"),
            ("parse.crs", "pub mod lexer;"),
            ("parse/lexer.crs", "pub let token : Type = Type;"),
        ],
    );

    let (package, source) = package_at(&directory).expect("a package with a library header");

    assert_eq!(package.name, "json");
    assert_eq!(
        source
            .expect("a package with a library header")
            .mounts()
            .first()
            .map(|mount| mount.prefix.clone()),
        Some(Qualifier::from(["json"]))
    );

    fs::remove_dir_all(directory).unwrap();
}

/// A package of nothing but programs has no body, and no vestigial file to write saying so.
#[test]
fn a_package_with_no_library_header_has_no_library() {
    let directory = package(
        "layout-headerless",
        "name = \"json\"\n\n[[executables]]\nname = \"serve\"\n",
        &[("serve.crs", "")],
    );

    let (package, source) = package_at(&directory).expect("a package of programs alone");

    assert!(source.is_none());
    assert_eq!(package.executables.len(), 1);

    fs::remove_dir_all(directory).unwrap();
}

/// Only the header's *absence* is an answer: one that fails to parse is still a refusal.
#[test]
fn an_unparsable_library_header_is_refused() {
    let directory = package(
        "layout-broken-header",
        "name = \"json\"\n",
        &[("lib.crs", "pub let x : = ;")],
    );

    let refusal = package_at(&directory)
        .map(|_| ())
        .expect_err("a header that does not parse");
    assert!(refusal.contains("lib.crs"), "{refusal}");

    fs::remove_dir_all(directory).unwrap();
}

/// One stem space: an executable beside a module of the same name is a refusal naming both.
#[test]
fn a_stem_claimed_twice_is_refused() {
    let directory = package(
        "layout-stem-clash",
        "name = \"json\"\n\n[[executables]]\nname = \"parse\"\n",
        &[("lib.crs", "pub mod parse;"), ("parse.crs", "")],
    );

    let refusal = package_at(&directory)
        .map(|_| ())
        .expect_err("a stem claimed twice");
    assert!(
        refusal.contains("claims the stem `parse` twice"),
        "{refusal}"
    );
    assert!(refusal.contains("mod parse"), "{refusal}");
    assert!(refusal.contains("the executable \"parse\""), "{refusal}");

    fs::remove_dir_all(directory).unwrap();
}

/// An executable whose path leaves the package root claims its stem somewhere else, so it does not collide here.
#[test]
fn an_executable_outside_the_root_claims_no_stem_in_it() {
    let directory = package(
        "layout-stem-elsewhere",
        "name = \"json\"\n\n[[executables]]\nname = \"parse\"\npath = \"tools/parse.crs\"\n",
        &[("lib.crs", "pub mod parse;"), ("parse.crs", "")],
    );

    package_at(&directory).expect("an executable outside the package root");

    fs::remove_dir_all(directory).unwrap();
}

/// An umbrella compiles nothing of its own, and saying so beats an absent-library refusal about a file it never wanted.
#[test]
fn an_umbrella_is_not_a_unit() {
    let directory = package("layout-umbrella", "members = [\"json\"]\n", &[]);

    let refusal = package_at(&directory)
        .map(|_| ())
        .expect_err("an umbrella is not a unit");
    assert!(refusal.contains("compiles nothing of its own"), "{refusal}");

    fs::remove_dir_all(directory).unwrap();
}
