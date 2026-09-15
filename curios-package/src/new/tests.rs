use {
    super::*,
    crate::{Manifest, package_at},
    curios_utilities::test_support::Temporary,
};

/// A path nothing has created yet, inside a directory of its own that goes away with the test, whose last segment is `name`.
///
/// The segment is `name` rather than the guard's own directory, because a scaffolded package is named after its directory and the guard spells its own with dashes no package name may hold — so `name` has to be a legal one, underscores and all.
fn fresh(name: &str) -> (Temporary, PathBuf) {
    let temporary = Temporary::new("new", name);
    let root = temporary.join(name);

    (temporary, root)
}

/// What `new` writes, the rest of this crate accepts — which is the only claim scaffolding can honestly make.
///
/// Both halves, in one package: the library header resolves and the declared executable exists, so neither part of a package is something a beginner has to discover a flag to get.
#[test]
fn a_scaffolded_package_is_one_this_crate_reads() {
    let (_temporary, root) = fresh("scaffold");
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
}

/// The name is checked before anything is written, so a refusal leaves nothing behind to clean up.
#[test]
fn a_name_no_path_could_spell_is_refused_before_writing() {
    let (_temporary, parent) = fresh("scaffold_bad_name");
    let root = parent.join("not-a-name");

    let refusal = scaffold(&root).expect_err("a dash is no identifier");
    assert!(refusal.contains("no name a path could spell"), "{refusal}");
    assert!(!root.exists(), "nothing is left behind");
}

/// `new` starts a package rather than adopting one, so it will not write over a manifest that is already there.
#[test]
fn an_existing_package_is_not_overwritten() {
    let (_temporary, root) = fresh("scaffold_occupied");
    scaffold(&root).expect("a fresh directory");
    let written = fs::read_to_string(root.join(MANIFEST)).unwrap();

    let refusal = scaffold(&root).expect_err("a directory that already holds a package");
    assert!(refusal.contains("already holds a package"), "{refusal}");
    assert_eq!(
        fs::read_to_string(root.join(MANIFEST)).unwrap(),
        written,
        "and the one that was there is untouched"
    );
}

/// The manifest it writes is in package mode, which is the mode every other rule in this crate is about.
#[test]
fn a_scaffolded_manifest_declares_a_package() {
    let (_temporary, root) = fresh("scaffold_mode");
    scaffold(&root).expect("a fresh directory");

    let manifest = fs::read_to_string(root.join(MANIFEST)).unwrap();
    assert!(
        matches!(manifest.parse::<Manifest>(), Ok(Manifest::Package(_))),
        "{manifest}"
    );
}

/// The store is the one directory the toolchain generates, and a fresh package starts out ignoring it.
#[test]
fn a_scaffolded_package_ignores_its_store() {
    let (_temporary, root) = fresh("scaffold_ignore");
    let written = scaffold(&root).expect("a fresh directory");

    let ignore = root.join(IGNORE);
    assert!(written.contains(&ignore), "and it is reported as written");
    assert_eq!(
        fs::read_to_string(&ignore).unwrap(),
        format!("/{STORE}/\n"),
        "the store and nothing else"
    );
}

/// A file that is already there is the user's, so `new` refuses before writing anything rather than replacing it — for every file it would write, not only the manifest.
#[test]
fn an_existing_file_is_not_overwritten() {
    for name in [LIBRARY, EXECUTABLE, IGNORE] {
        let (_temporary, root) = fresh("scaffold_occupied_file");
        fs::create_dir_all(&root).unwrap();
        fs::write(root.join(name), "theirs\n").unwrap();

        let refusal = scaffold(&root).expect_err("a directory that already holds the file");
        assert!(
            refusal.contains(&format!("already holds a {name}")),
            "{refusal}"
        );
        assert_eq!(fs::read_to_string(root.join(name)).unwrap(), "theirs\n");
        assert!(!root.join(MANIFEST).exists(), "nothing else was written");
    }
}
