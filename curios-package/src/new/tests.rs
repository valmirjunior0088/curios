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
#[test]
fn a_scaffolded_program_is_a_package_this_crate_reads() {
    let root = temp_dir("scaffold_program");
    scaffold(&root, false).expect("a fresh directory");

    let (package, source) = package_at(&root).expect("what was written parses and resolves");

    assert_eq!(package.executables.len(), 1);
    assert_eq!(
        package.executables[0].name,
        root.file_name().unwrap().to_string_lossy()
    );
    assert!(root.join(package.executables[0].path.clone()).is_file());

    // A package of nothing but a program has no library, and no empty file pretending otherwise.
    assert!(source.is_none());
    assert!(!root.join(LIBRARY).exists());

    fs::remove_dir_all(root).unwrap();
}

/// And the other direction: a library package gets a header and no program.
#[test]
fn a_scaffolded_library_is_a_package_this_crate_reads() {
    let root = temp_dir("scaffold_library");
    scaffold(&root, true).expect("a fresh directory");

    let (package, source) = package_at(&root).expect("what was written parses and resolves");

    assert!(package.executables.is_empty());
    assert!(source.is_some());
    assert!(root.join(LIBRARY).is_file());

    fs::remove_dir_all(root).unwrap();
}

/// The name is checked before anything is written, so a refusal leaves nothing behind to clean up.
#[test]
fn a_name_no_path_could_spell_is_refused_before_writing() {
    let root = temp_dir("scaffold_bad_name").join("not-a-name");

    let refusal = scaffold(&root, false).expect_err("a dash is no identifier");
    assert!(refusal.contains("no name a path could spell"), "{refusal}");
    assert!(!root.exists(), "nothing is left behind");
}

/// `new` starts a package rather than adopting one, so it will not write over a manifest that is already there.
#[test]
fn an_existing_package_is_not_overwritten() {
    let root = temp_dir("scaffold_occupied");
    scaffold(&root, false).expect("a fresh directory");
    let written = fs::read_to_string(root.join(MANIFEST)).unwrap();

    let refusal = scaffold(&root, true).expect_err("a directory that already holds a package");
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
    scaffold(&root, false).expect("a fresh directory");

    let manifest = fs::read_to_string(root.join(MANIFEST)).unwrap();
    assert!(
        matches!(manifest.parse::<Manifest>(), Ok(Manifest::Package(_))),
        "{manifest}"
    );

    fs::remove_dir_all(root).unwrap();
}
