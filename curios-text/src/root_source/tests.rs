//! The one spelling two paths to a file share, and whether a `mod` chain reaches a module.

use {
    super::{Overlay, RootSource, identity},
    curios_utilities::{Qualifier, RootKind},
    std::{
        fs,
        path::{Path, PathBuf},
        time::{SystemTime, UNIX_EPOCH},
    },
};

/// A file that does not exist is spelled by its canonical parent and its name, and a bare name's parent is the current directory — not the empty path, which canonicalizes to nothing and left the relative name as given, so every caller read it as a path with no directory at all.
#[test]
fn a_bare_name_the_disk_does_not_hold_is_spelled_under_the_current_directory() {
    let here = std::env::current_dir().unwrap().canonicalize().unwrap();

    assert_eq!(
        identity(Path::new("curios-does-not-exist.crs")),
        here.join("curios-does-not-exist.crs")
    );
    assert_eq!(
        identity(Path::new("src/curios-does-not-exist.crs")),
        here.join("src").join("curios-does-not-exist.crs")
    );
}

/// A tree of `(relative path, contents)` pairs, rooted at a fresh directory nothing else is using.
fn tree(name: &str, files: &[(&str, &str)]) -> PathBuf {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();
    let root = std::env::temp_dir().join(format!("curios-{name}-{}-{millis}", std::process::id()));

    for (path, source) in files {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, source).unwrap();
    }

    root
}

/// A module is declared when a `mod` chain from the header reaches it — through a file module, through an inline body, or as the header itself — and a file the chain never names is not, wherever it sits.
#[test]
fn a_module_is_declared_when_a_mod_chain_from_the_header_reaches_it() {
    let root = tree(
        "declares-module",
        &[
            (
                "lib.crs",
                "pub mod parse;\n\npub mod inline\n    pub mod deep;\nend\n",
            ),
            ("parse.crs", "pub mod lexer;\n"),
            ("parse/lexer.crs", ""),
            ("inline/deep.crs", ""),
            ("stray.crs", ""),
            ("parse/stray.crs", ""),
        ],
    );
    let source = RootSource::mounted("json", RootKind::Ordinary, root.join("lib.crs"), &root);

    for (module, declared) in [
        ("/json", true),
        ("/json/parse", true),
        ("/json/parse/lexer", true),
        ("/json/inline/deep", true),
        ("/json/stray", false),
        ("/json/parse/stray", false),
        ("/other", false),
    ] {
        let qualifier = Qualifier::from(module.split('/').filter(|s| !s.is_empty()));
        assert_eq!(
            source.declares_module(&qualifier).unwrap(),
            declared,
            "{module}"
        );
    }

    fs::remove_dir_all(root).unwrap();
}

/// The walk reads through the overlay as discovery does, so a `mod` an editor has written and not saved already declares its module.
#[test]
fn an_unsaved_mod_declares_its_module_through_the_overlay() {
    let root = tree("declares-overlay", &[("lib.crs", ""), ("fresh.crs", "")]);
    let overlay = Overlay::of([(root.join("lib.crs"), "pub mod fresh;\n".to_string())]);
    let source = RootSource::mounted("json", RootKind::Ordinary, root.join("lib.crs"), &root)
        .with_overlay(overlay);

    assert!(
        source
            .declares_module(&Qualifier::from(["json", "fresh"]))
            .unwrap()
    );

    fs::remove_dir_all(root).unwrap();
}
