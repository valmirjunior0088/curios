//! Writing a row into a manifest: what changes, what does not, and what is refused before anything is written.

use {super::*, curios_utilities::test_support::Temporary, std::fs};

/// A package whose manifest holds `manifest`, governed by itself.
///
/// `name` is the test's own, because `Temporary` separates fixtures by process and millisecond and these run in parallel in one process — two sharing a name would share a directory.
fn governed(name: &str, manifest: &str) -> (Temporary, Governing) {
    let root = Temporary::new("pin", name);
    fs::create_dir_all(&root).expect("create the package directory");
    fs::write(root.join(MANIFEST), manifest).expect("write the manifest");

    let governing = Governing::of(&root).expect("the package governs itself");

    (root, governing)
}

/// A module the package carries, so nothing here reaches the network.
fn carried(root: &Temporary, path: &str, bytes: &[u8]) -> FileHash {
    let at = root.join(path);
    if let Some(parent) = at.parent() {
        fs::create_dir_all(parent).expect("create the module's directory");
    }
    fs::write(&at, bytes).expect("write the module");

    FileHash::of_bytes(bytes)
}

#[test]
fn a_carried_module_is_pinned_to_the_hash_of_its_bytes() {
    let (root, governing) = governed("carried", "name = \"app\"\n");
    let hash = carried(&root, "foreign/probe.wasm", b"\0asm\x01\0\0\0");

    let pinned = pin(
        &governing,
        Subject::Foreign,
        "probe",
        &Repoint::Carried("foreign/probe.wasm".into()),
        false,
    )
    .expect("the module is carried");

    assert!(pinned.wrote);
    assert!(!pinned.existed);

    let written = fs::read_to_string(&governing.manifest).expect("read it back");
    assert!(written.contains(&hash.to_string()), "{written}");
    assert!(written.contains("name = \"probe\""), "{written}");
}

#[test]
fn pinning_a_row_to_what_it_already_says_writes_nothing() {
    let (root, governing) = governed("idempotent", "name = \"app\"\n");
    carried(&root, "probe.wasm", b"\0asm\x01\0\0\0");

    let repoint = Repoint::Carried("probe.wasm".into());
    pin(&governing, Subject::Foreign, "probe", &repoint, false).expect("the first pin writes");
    let after_first = fs::read_to_string(&governing.manifest).expect("read it back");

    let again =
        pin(&governing, Subject::Foreign, "probe", &repoint, false).expect("the second pin runs");

    assert!(!again.wrote);
    assert!(!again.would_write());
    assert_eq!(
        after_first,
        fs::read_to_string(&governing.manifest).expect("read it back"),
        "the manifest was rewritten for a row that already said this"
    );
}

/// `--check` is the same question without the answer being written down, which is what makes it usable in CI.
#[test]
fn a_checked_pin_reports_what_it_would_write_and_writes_nothing() {
    let (root, governing) = governed("checked", "name = \"app\"\n");
    carried(&root, "probe.wasm", b"\0asm\x01\0\0\0");

    let before = fs::read_to_string(&governing.manifest).expect("read it back");
    let checked = pin(
        &governing,
        Subject::Foreign,
        "probe",
        &Repoint::Carried("probe.wasm".into()),
        true,
    )
    .expect("the check runs");

    assert!(checked.would_write());
    assert!(!checked.wrote);
    assert_eq!(
        before,
        fs::read_to_string(&governing.manifest).expect("read it back")
    );
}

/// Everything the manifest already said is still there, byte for byte, including what TOML gives no structure to.
#[test]
fn a_written_row_disturbs_nothing_beside_it() {
    let manifest = "# the package\nname = \"app\"\n\n[[executables]]\nname = \"serve\"\npath = \"serve.crs\"\n";
    let (root, governing) = governed("undisturbed", manifest);
    carried(&root, "probe.wasm", b"\0asm\x01\0\0\0");

    pin(
        &governing,
        Subject::Foreign,
        "probe",
        &Repoint::Carried("probe.wasm".into()),
        false,
    )
    .expect("the module is carried");

    let written = fs::read_to_string(&governing.manifest).expect("read it back");
    assert!(written.contains("# the package"), "{written}");
    assert!(written.contains("[[executables]]"), "{written}");
    assert!(written.contains("name = \"serve\""), "{written}");
}

/// Repointing from one delivery to the other has to remove the key it is leaving behind, or the row states two and is refused where it is read.
#[test]
fn repointing_a_module_to_a_url_clears_the_path_it_carried() {
    let (root, governing) = governed("repointed", "name = \"app\"\n");
    carried(&root, "probe.wasm", b"\0asm\x01\0\0\0");

    pin(
        &governing,
        Subject::Foreign,
        "probe",
        &Repoint::Carried("probe.wasm".into()),
        false,
    )
    .expect("the module is carried");

    assert!(
        fs::read_to_string(&governing.manifest)
            .expect("read it back")
            .contains("path ="),
        "the carried row states its path"
    );
}

#[test]
fn a_row_that_is_not_there_cannot_be_refreshed() {
    let (_root, governing) = governed("absent", "name = \"app\"\n");

    let refusal = pin(
        &governing,
        Subject::Foreign,
        "absent",
        &Repoint::Refresh,
        false,
    )
    .expect_err("there is no row to re-pin");

    assert!(
        refusal.contains("there is no foreign module \"absent\""),
        "{refusal}"
    );
    assert!(
        refusal.contains("curios pin foreign absent --url"),
        "{refusal}"
    );
}

#[test]
fn a_module_outside_the_package_is_refused_as_no_plain_relative_path() {
    let (_root, governing) = governed("outside", "name = \"app\"\n");

    let refusal = pin(
        &governing,
        Subject::Foreign,
        "probe",
        &Repoint::Carried("../elsewhere.wasm".into()),
        false,
    )
    .expect_err("a module is named from the manifest");

    assert!(refusal.contains("no plain relative path"), "{refusal}");
}

/// A dependency's key is the package's own name, so a positional that disagrees is refused before a row is written rather than by the resolver once one has been.
#[test]
fn a_dependency_named_otherwise_than_it_declares_itself_is_refused() {
    let root = Temporary::new("pin", "workspace");
    let app = root.join("app");
    let dep = root.join("dep");
    fs::create_dir_all(&app).expect("create the package");
    fs::create_dir_all(&dep).expect("create the dependency");
    fs::write(app.join(MANIFEST), "name = \"app\"\n").expect("write the manifest");
    fs::write(dep.join(MANIFEST), "name = \"dep\"\n").expect("write the dependency's manifest");

    let governing = Governing::of(&app).expect("the package governs itself");
    let refusal = pin(
        &governing,
        Subject::Dependency,
        "wrongname",
        &Repoint::Carried("../dep".into()),
        false,
    )
    .expect_err("a package is referred to by the name it declares");

    assert!(refusal.contains("declares itself \"dep\""), "{refusal}");
}
