use {
    super::*,
    curios_base::Qualifier,
    std::{
        fs,
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

/// A tree of `(relative path, contents)` pairs, rooted at a fresh directory.
fn tree(name: &str, files: &[(&str, &str)]) -> PathBuf {
    let root = temp_dir(name);

    for (path, source) in files {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, source).unwrap();
    }

    root
}

/// A package with no umbrella over it governs itself, and is its own governing root.
#[test]
fn a_lone_package_governs_itself() {
    let root = tree(
        "govern-lone",
        &[("curios.toml", "name = \"json\"\n"), ("lib.crs", "")],
    );

    let governing = Governing::of(&root).expect("a package governs the directory it is in");

    assert_eq!(governing.package.name, Qualifier::from(["json"]));
    assert!(governing.umbrella.is_none());
    assert!(same_directory(&governing.root, &root));

    fs::remove_dir_all(root).unwrap();
}

/// The walk starts at the working directory and goes up, so a subdirectory of a package is inside it.
#[test]
fn a_subdirectory_is_governed_by_the_package_above_it() {
    let root = tree(
        "govern-subdirectory",
        &[
            ("curios.toml", "name = \"json\"\n"),
            ("lib.crs", "pub mod parse;"),
            ("parse/lexer.crs", ""),
        ],
    );

    let governing =
        Governing::of(&root.join("parse")).expect("the package above the working directory");

    assert_eq!(governing.package.name, Qualifier::from(["json"]));

    fs::remove_dir_all(root).unwrap();
}

/// An umbrella governs what it enumerates, and its directory is where the store goes.
#[test]
fn an_umbrella_governs_a_member_it_enumerates() {
    let root = tree(
        "govern-member",
        &[
            ("curios.toml", "members = [\"json\", \"tools/cli\"]\n"),
            ("json/curios.toml", "name = \"json\"\n"),
            ("json/lib.crs", ""),
            ("tools/cli/curios.toml", "name = \"cli\"\n"),
            ("tools/cli/lib.crs", ""),
        ],
    );

    // Enumeration may point deep, which is what gives deep organization with a flat manifest.
    for (member, name) in [("json", "json"), ("tools/cli", "cli")] {
        let governing = Governing::of(&root.join(member)).expect("an enumerated member");

        assert_eq!(governing.package.name, Qualifier::from([name]));
        assert!(governing.umbrella.is_some(), "{member} is enumerated");
        assert!(same_directory(&governing.root, &root), "{member}");
    }

    fs::remove_dir_all(root).unwrap();
}

/// Enumeration bounds the walk: a package sitting inside an umbrella's tree that the umbrella does not list is governed by nothing above it.
#[test]
fn an_umbrella_governs_nothing_it_does_not_enumerate() {
    let root = tree(
        "govern-unenumerated",
        &[
            ("curios.toml", "members = [\"json\"]\n"),
            ("json/curios.toml", "name = \"json\"\n"),
            ("json/lib.crs", ""),
            ("scratch/curios.toml", "name = \"scratch\"\n"),
            ("scratch/lib.crs", ""),
        ],
    );

    let governing = Governing::of(&root.join("scratch")).expect("an unenumerated package");

    assert!(governing.umbrella.is_none());
    assert!(same_directory(&governing.root, &root.join("scratch")));

    fs::remove_dir_all(root).unwrap();
}

/// An umbrella is not a package, so standing in its root there is nothing for `run` to compile.
#[test]
fn an_umbrella_root_is_governed_by_no_package() {
    let root = tree(
        "govern-umbrella-root",
        &[
            ("curios.toml", "members = [\"json\"]\n"),
            ("json/curios.toml", "name = \"json\"\n"),
            ("json/lib.crs", ""),
        ],
    );

    let refusal = Governing::of(&root)
        .map(|_| ())
        .expect_err("an umbrella root declares no package");
    assert!(
        refusal.contains("no `curios.toml` declaring a package"),
        "{refusal}"
    );

    fs::remove_dir_all(root).unwrap();
}
