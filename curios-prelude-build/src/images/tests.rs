//! The key, over a workspace of four crates the fixtures write, and the images, over ones they make up.

use {super::*, curios_common::test_support::Temporary, std::fs};

const ROOT: &str = r#"[workspace]
members = ["p", "a", "b", "c"]

[workspace.dependencies]
a = { path = "a" }
"#;

const P: &str = r#"[package]
name = "p"

[build-dependencies]
a = { workspace = true }
"#;

const A: &str = r#"[package]
name = "a"

[dependencies]
b = { path = "../b" }
"#;

/// A workspace whose prelude, `p`, build-depends on `a`, which depends on `b`, while `c` stands beside them, linked by none.
fn workspace(name: &str) -> Temporary {
    let root = Temporary::new("prelude-images", name);
    written(&root, "Cargo.toml", ROOT);
    written(&root, "Cargo.lock", "");
    written(&root, "rust-toolchain.toml", "");
    written(&root, "p/Cargo.toml", P);
    written(&root, "p/build.rs", "fn main() {}\n");
    written(&root, "p/std/lib.crs", "");
    written(&root, "a/Cargo.toml", A);
    written(&root, "a/src/lib.rs", "");
    for name in ["b", "c"] {
        written(
            &root,
            &format!("{name}/Cargo.toml"),
            &format!("[package]\nname = \"{name}\"\n"),
        );
        written(&root, &format!("{name}/src/lib.rs"), "");
    }

    root
}

/// Write `text` at `path` under `root`, its directories made.
fn written(root: &Path, path: &str, text: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().expect("a fixture's file sits in a directory")).unwrap();
    fs::write(path, text).unwrap();
}

/// The key of the workspace at `root`, whose prelude is `p`.
fn key_of(root: &Path) -> String {
    key(root, &root.join("p"), "rustc 1.99.1")
}

#[test]
fn an_edit_to_a_crate_the_script_links_moves_the_key() {
    let root = workspace("linked");
    let before = key_of(&root);
    written(&root, "b/src/lib.rs", "pub fn edited() {}\n");

    assert_ne!(key_of(&root), before);
}

#[test]
fn an_edit_to_a_crate_the_script_does_not_link_leaves_the_key() {
    let root = workspace("unlinked");
    let before = key_of(&root);
    written(&root, "c/src/lib.rs", "pub fn edited() {}\n");

    assert_eq!(key_of(&root), before);
}

#[test]
fn a_file_under_an_entry_whose_name_begins_with_a_dot_leaves_the_key() {
    let root = workspace("hidden");
    let before = key_of(&root);
    written(&root, "a/.artifacts/tables.rs", "");
    written(&root, "p/.artifacts/images/marker", "");
    written(&root, "p/std/.curios/memo", "");

    assert_eq!(key_of(&root), before);
}

/// A file no build the script links compiles leaves the key: a Rust file at each of a test's places, and a crate's `README.md`.
#[test]
fn a_rust_file_at_a_tests_place_or_a_readme_leaves_the_key() {
    let root = workspace("tests-place");
    let before = key_of(&root);
    for path in [
        "b/src/tests.rs",
        "b/src/walk_tests.rs",
        "b/src/test_support.rs",
        "b/src/walk/tests/case.rs",
        "b/src/test_support/fixture.rs",
        "b/tests/end_to_end.rs",
        "b/README.md",
    ] {
        written(&root, path, "// edited\n");
    }

    assert_eq!(key_of(&root), before);
}

/// Beside them, a production module and a `.crs` under a `tests` directory, which is `/std`'s source, each move it.
#[test]
fn a_production_module_or_a_library_test_module_moves_the_key() {
    for (at, path) in ["b/src/walk.rs", "p/std/Walk/tests/case.crs"]
        .into_iter()
        .enumerate()
    {
        let root = workspace(&format!("moved-{at}"));
        let before = key_of(&root);
        written(&root, path, "// edited\n");

        assert_ne!(key_of(&root), before, "{path}");
    }
}

/// What the place leaves out holds what no link compiles: every Rust file at a test's place under the `src` of a crate the prelude's script links is declared beneath a `#[cfg(test)]` or `test-support` gate, or beneath a module that is itself at a test's place, so it reaches no build of the script.
#[test]
fn every_rust_file_at_a_tests_place_is_reached_only_through_a_test_gate() {
    let workspace = canonical(
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("the crate sits in the workspace"),
    );
    let prelude = workspace.join("curios-prelude");
    let mut ungated = Vec::new();
    for crate_root in linked(&workspace, &prelude) {
        let source = crate_root.join("src");
        for file in rust_files(&source) {
            if tests_place(&crate_root, &file) && !gated(&crate_root, &source, &file) {
                ungated.push(file);
            }
        }
    }

    assert!(
        ungated.is_empty(),
        "at a test's place and declared under no test gate: {ungated:?}"
    );
}

/// Every Rust file under `directory`, its own subdirectories' among them.
fn rust_files(directory: &Path) -> Vec<PathBuf> {
    let Ok(entries) = fs::read_dir(directory) else {
        return Vec::new();
    };
    let mut found = Vec::new();
    for entry in entries {
        let path = entry.unwrap().path();
        if path.is_dir() {
            found.extend(rust_files(&path));
        } else if path.extension().is_some_and(|extension| extension == "rs") {
            found.push(path);
        }
    }

    found
}

/// Whether `file`, under the crate's `source`, is declared beneath a test gate in the module above it, or that module is itself at a test's place; a file no module declares is compiled by nothing.
fn gated(crate_root: &Path, source: &Path, file: &Path) -> bool {
    let directory = file.parent().expect("a file sits in a directory");
    let parent = match directory == source {
        true => ["lib.rs", "main.rs"]
            .iter()
            .map(|root| source.join(root))
            .find(|root| root.exists())
            .expect("a crate holds a root module"),
        false => directory.with_extension("rs"),
    };
    if tests_place(crate_root, &parent) {
        return true;
    }
    let name = file.file_stem().and_then(|stem| stem.to_str()).unwrap();
    let text = fs::read_to_string(&parent).unwrap_or_default();
    let lines = text.lines().collect::<Vec<_>>();
    let Some(at) = lines.iter().position(|line| {
        let line = line.trim();
        line == format!("mod {name};") || line == format!("pub mod {name};")
    }) else {
        return true;
    };

    lines[..at]
        .iter()
        .rev()
        .take_while(|line| line.trim_start().starts_with("#["))
        .any(|line| line.contains("cfg(test)") || line.contains("test-support"))
}

#[test]
fn a_build_dependency_a_target_table_adds_takes_its_crate_into_the_key() {
    let root = workspace("target");
    let edge = "\n[target.'cfg(unix)'.build-dependencies]\nc = { path = \"../c\" }\n";
    written(&root, "a/Cargo.toml", &[A, edge].concat());
    let before = key_of(&root);
    written(&root, "c/src/lib.rs", "pub fn edited() {}\n");

    assert_ne!(key_of(&root), before);
}

#[test]
fn another_compiler_moves_the_key() {
    let root = workspace("compiler");

    assert_ne!(
        key(&root, &root.join("p"), "rustc 1.99.1"),
        key(&root, &root.join("p"), "rustc 1.99.2")
    );
}

#[test]
fn the_key_is_the_same_wherever_the_workspace_stands() {
    assert_eq!(key_of(&workspace("here")), key_of(&workspace("there")));
}

/// Images under `root` filed for the key `state`, two made up, with the directory they were made in.
fn filled(root: &Path) -> (Images, PathBuf) {
    written(root, "out/sys.rkyv", "sys");
    written(root, "out/std.rkyv", "std");
    let out = root.join("out");
    let images = Images::open(root.join("images"));
    images.fill("state", &out, &[PathBuf::from("lib.crs")]);

    (images, out)
}

/// A directory under `root` for a run to copy the images into.
fn taken(root: &Path) -> PathBuf {
    let taken = root.join("taken");
    fs::create_dir_all(&taken).unwrap();

    taken
}

#[test]
fn an_entry_is_reused_for_its_key_alone() {
    let root = Temporary::new("prelude-images", "reused");
    let (images, _) = filled(&root);
    let taken = taken(&root);

    assert_eq!(images.reuse("another", &taken), None);
    assert_eq!(
        images.reuse("state", &taken),
        Some(vec![PathBuf::from("lib.crs")])
    );
    assert_eq!(fs::read(taken.join("std.rkyv")).unwrap(), b"std");
}

#[test]
fn an_image_other_than_the_one_its_marker_names_is_not_reused() {
    let root = Temporary::new("prelude-images", "altered");
    let (images, _) = filled(&root);
    written(&root, "images/std.rkyv", "altered");

    assert_eq!(images.reuse("state", &taken(&root)), None);
}

#[test]
fn made_images_are_compared_with_the_entry_of_their_key_alone() {
    let root = Temporary::new("prelude-images", "compared");
    let (images, out) = filled(&root);

    assert_eq!(images.compare("state", &out), Ok(true));
    assert_eq!(images.compare("another", &out), Ok(false));
}

#[test]
fn an_image_made_otherwise_than_the_entry_of_its_key_holds_is_refused() {
    let root = Temporary::new("prelude-images", "refused");
    let (images, out) = filled(&root);
    written(&root, "out/std.rkyv", "otherwise");

    assert!(images.compare("state", &out).is_err());
}

#[test]
fn images_made_while_a_source_moved_are_neither_compared_nor_filed() {
    let root = Temporary::new("prelude-images", "moved");
    let (images, out) = filled(&root);
    written(&root, "out/std.rkyv", "otherwise");

    assert!(images.settle("state", "state", &out, &[], true).is_err());
    assert_eq!(
        images.settle("state", "moved", &out, &[], true),
        Ok(Settled::Moved)
    );
    let taken = taken(&root);
    images.reuse("state", &taken);
    assert_eq!(fs::read(taken.join("std.rkyv")).unwrap(), b"std");
}
