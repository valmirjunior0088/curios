use {
    super::*,
    crate::{Manifest, package_at},
    std::{
        path::PathBuf,
        time::{SystemTime, UNIX_EPOCH},
    },
};

/// A path nothing has created yet, spelled so its last segment is a legal package name.
///
/// Underscores rather than the dashes the other suites use: a scaffolded package is named after its directory, so a directory nobody could name a package after is not a fixture for this.
fn temp_dir(name: &str) -> PathBuf {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();

    std::env::temp_dir().join(format!("curios_{name}_{}_{millis}", std::process::id()))
}

/// What `new` writes, the rest of this crate accepts — which is the only claim scaffolding can honestly make.
///
/// Both halves, in one package: the library header resolves and the declared executable exists, so neither part of a package is something a beginner has to discover a flag to get.
#[test]
fn a_scaffolded_package_is_one_this_crate_reads() {
    let root = temp_dir("scaffold");
    scaffold(&root).expect("a fresh directory");

    let (package, source) = package_at(&root).expect("what was written parses and resolves");

    assert_eq!(package.executables.len(), 1);
    assert_eq!(
        package.executables[0].name,
        root.file_name().unwrap().to_string_lossy()
    );
    assert!(root.join(package.executables[0].path.clone()).is_file());

    assert!(source.is_some(), "the library header resolves");
    assert!(root.join(LIBRARY).is_file());

    fs::remove_dir_all(root).unwrap();
}

/// The name is checked before anything is written, so a refusal leaves nothing behind to clean up.
#[test]
fn a_name_no_path_could_spell_is_refused_before_writing() {
    let root = temp_dir("scaffold_bad_name").join("not-a-name");

    let refusal = scaffold(&root).expect_err("a dash is no identifier");
    assert!(refusal.contains("no name a path could spell"), "{refusal}");
    assert!(!root.exists(), "nothing is left behind");
}

/// `new` starts a package rather than adopting one, so it will not write over a manifest that is already there.
#[test]
fn an_existing_package_is_not_overwritten() {
    let root = temp_dir("scaffold_occupied");
    scaffold(&root).expect("a fresh directory");
    let written = fs::read_to_string(root.join(MANIFEST)).unwrap();

    let refusal = scaffold(&root).expect_err("a directory that already holds a package");
    assert!(refusal.contains("already holds a package"), "{refusal}");
    assert_eq!(
        fs::read_to_string(root.join(MANIFEST)).unwrap(),
        written,
        "and the one that was there is untouched"
    );

    fs::remove_dir_all(root).unwrap();
}

/// The manifest it writes is in package mode, which is the mode every other rule in this crate is about.
#[test]
fn a_scaffolded_manifest_declares_a_package() {
    let root = temp_dir("scaffold_mode");
    scaffold(&root).expect("a fresh directory");

    let manifest = fs::read_to_string(root.join(MANIFEST)).unwrap();
    assert!(
        matches!(manifest.parse::<Manifest>(), Ok(Manifest::Package(_))),
        "{manifest}"
    );

    fs::remove_dir_all(root).unwrap();
}

/// The store is the one directory the toolchain generates, and a fresh package starts out ignoring it.
#[test]
fn a_scaffolded_package_ignores_its_store() {
    let root = temp_dir("scaffold_ignore");
    let written = scaffold(&root).expect("a fresh directory");

    let ignore = root.join(IGNORE);
    assert!(written.contains(&ignore), "and it is reported as written");
    assert_eq!(
        fs::read_to_string(&ignore).unwrap(),
        format!("/{STORE}/\n"),
        "the store and nothing else"
    );

    fs::remove_dir_all(root).unwrap();
}

/// A file that is already there is the user's, so `new` refuses before writing anything rather than replacing it — for every file it would write, not only the manifest.
#[test]
fn an_existing_file_is_not_overwritten() {
    for name in [LIBRARY, EXECUTABLE, IGNORE] {
        let root = temp_dir("scaffold_occupied_file");
        fs::create_dir_all(&root).unwrap();
        fs::write(root.join(name), "theirs\n").unwrap();

        let refusal = scaffold(&root).expect_err("a directory that already holds the file");
        assert!(
            refusal.contains(&format!("already holds a {name}")),
            "{refusal}"
        );
        assert_eq!(fs::read_to_string(root.join(name)).unwrap(), "theirs\n");
        assert!(!root.join(MANIFEST).exists(), "nothing else was written");

        fs::remove_dir_all(root).unwrap();
    }
}
