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
    let (root, governing) = governed("carried", "compiler = \"0.0.0\"\nname = \"app\"\n");
    let hash = carried(&root, "foreign/probe.wasm", b"\0asm\x01\0\0\0");

    let pinned = pin(
        &governing,
        Subject::Foreign(Target::Native),
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
    assert!(written.contains("[foreign.native]"), "{written}");

    let crate::Manifest::Package(package) = written.parse().expect("what was written reads back")
    else {
        panic!("a package");
    };
    assert_eq!(
        package.foreign[0]
            .native
            .as_ref()
            .map(|native| &native.module),
        Some(&crate::Module::Filed("foreign/probe.wasm".into()))
    );
}

/// A gateway is pinned into the row's `[foreign.browser]` table, beside whatever its native table says, and described as the JavaScript it is rather than as a module to compile.
#[test]
fn a_gateway_is_pinned_into_the_browser_table() {
    let (root, governing) = governed(
        "gateway",
        "compiler = \"0.0.0\"\nname = \"app\"\n\n[[foreign]]\nname = \"dom\"\nnative = { path = \"dom.wasm\", hash = \"f1:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\" }\n",
    );
    let hash = carried(&root, "web/dom.js", b"export const setText = () => {};\n");

    let pinned = pin(
        &governing,
        Subject::Foreign(Target::Browser),
        "dom",
        &Repoint::Carried("web/dom.js".into()),
        false,
    )
    .expect("the gateway is carried");

    let probe = pinned.probe.expect("a delivery is described");
    assert!(probe.kind.contains("JavaScript gateway"), "{}", probe.kind);
    assert!(
        !probe.magic,
        "a gateway's exports are read where the page loads it"
    );

    let written = fs::read_to_string(&governing.manifest).expect("read it back");
    assert!(written.contains("[foreign.browser]"), "{written}");
    assert!(
        written.contains("native = { path = \"dom.wasm\""),
        "{written}"
    );

    let crate::Manifest::Package(package) = written.parse().expect("what was written reads back")
    else {
        panic!("a package");
    };
    let gateway = package.foreign[0]
        .browser
        .as_ref()
        .expect("a browser delivery");
    assert_eq!(
        gateway.delivery.module,
        crate::Module::Filed("web/dom.js".into())
    );
    assert_eq!(gateway.delivery.hash, hash);
    assert!(
        package.foreign[0].native.is_some(),
        "the native table is left as it was"
    );
}

/// A delivery its author wrote inline, `native = { … }`, is re-pinned inline, as a dependency row is, rather than gaining a table beside it.
#[test]
fn an_inline_delivery_is_re_pinned_inline() {
    let (root, governing) = governed(
        "inline",
        "compiler = \"0.0.0\"\nname = \"app\"\n\n[[foreign]]\nname = \"probe\"\nnative = { path = \"old.wasm\", hash = \"f1:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\" }\n",
    );
    let hash = carried(&root, "probe.wasm", b"\0asm\x01\0\0\0");

    pin(
        &governing,
        Subject::Foreign(Target::Native),
        "probe",
        &Repoint::Carried("probe.wasm".into()),
        false,
    )
    .expect("the row is re-pinned");

    let written = fs::read_to_string(&governing.manifest).expect("read it back");
    assert!(
        written.contains(&format!(
            "native = {{ path = \"probe.wasm\", hash = \"{hash}\" }}"
        )),
        "{written}"
    );
    assert!(!written.contains("[foreign.native]"), "{written}");
}

#[test]
fn pinning_a_row_to_what_it_already_says_writes_nothing() {
    let (root, governing) = governed("idempotent", "compiler = \"0.0.0\"\nname = \"app\"\n");
    carried(&root, "probe.wasm", b"\0asm\x01\0\0\0");

    let repoint = Repoint::Carried("probe.wasm".into());
    pin(
        &governing,
        Subject::Foreign(Target::Native),
        "probe",
        &repoint,
        false,
    )
    .expect("the first pin writes");
    let after_first = fs::read_to_string(&governing.manifest).expect("read it back");

    let again = pin(
        &governing,
        Subject::Foreign(Target::Native),
        "probe",
        &repoint,
        false,
    )
    .expect("the second pin runs");

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
    let (root, governing) = governed("checked", "compiler = \"0.0.0\"\nname = \"app\"\n");
    carried(&root, "probe.wasm", b"\0asm\x01\0\0\0");

    let before = fs::read_to_string(&governing.manifest).expect("read it back");
    let checked = pin(
        &governing,
        Subject::Foreign(Target::Native),
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
    let manifest = "# the package\ncompiler = \"0.0.0\"\nname = \"app\"\n\n[[executables]]\nname = \"serve\"\npath = \"serve.crs\"\n";
    let (root, governing) = governed("undisturbed", manifest);
    carried(&root, "probe.wasm", b"\0asm\x01\0\0\0");

    pin(
        &governing,
        Subject::Foreign(Target::Native),
        "probe",
        &Repoint::Carried("probe.wasm".into()),
        false,
    )
    .expect("the module is carried");

    let written = fs::read_to_string(&governing.manifest).expect("read it back");
    assert!(written.contains("# the package"), "{written}");
    assert!(written.contains("[[executables]]"), "{written}");
    assert!(written.contains("name = \"serve\""), "{written}");
    assert!(written.contains("compiler = \"0.0.0\""), "{written}");
}

/// Repointing from one delivery to the other has to remove the key it is leaving behind, or the row states two and is refused where it is read.
#[test]
fn repointing_a_module_to_a_url_clears_the_path_it_carried() {
    let (root, governing) = governed("repointed", "compiler = \"0.0.0\"\nname = \"app\"\n");
    carried(&root, "probe.wasm", b"\0asm\x01\0\0\0");

    pin(
        &governing,
        Subject::Foreign(Target::Native),
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
    let (_root, governing) = governed("absent", "compiler = \"0.0.0\"\nname = \"app\"\n");

    let refusal = pin(
        &governing,
        Subject::Foreign(Target::Native),
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
    let (_root, governing) = governed("outside", "compiler = \"0.0.0\"\nname = \"app\"\n");

    let refusal = pin(
        &governing,
        Subject::Foreign(Target::Native),
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
    fs::write(app.join(MANIFEST), "compiler = \"0.0.0\"\nname = \"app\"\n")
        .expect("write the manifest");
    fs::write(dep.join(MANIFEST), "compiler = \"0.0.0\"\nname = \"dep\"\n")
        .expect("write the dependency's manifest");

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
