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
