//! The one spelling two paths to a file share, whether a `mod` chain reaches a module, and when a file is parsed again.

use {
    super::{Error, Overlay, RootSource, identity},
    crate::Entrypoint,
    curios_utilities::{Qualifier, RootKind, Source, test_support::Temporary},
    std::{fs, path::Path, rc::Rc},
};

/// An entry's header is the one file a loader never reads, so its `mod` chain is walked from the items the entry was parsed into: a module reached through a file module below it counts, and one nothing declares does not.
#[test]
fn an_entry_declares_what_its_mod_chain_reaches() {
    let root = tree(
        "entry-declares",
        &[
            ("serve.crs", "mod helper;\n/std/print(\"\")\n"),
            ("serve/helper.crs", "mod deep;\n"),
            ("serve/helper/deep.crs", ""),
            ("serve/stray.crs", ""),
        ],
    );
    let (entrypoint, loader, _) =
        Entrypoint::opened(&root.join("serve.crs")).expect("the entry parses");
    let declares = |qualifier: Qualifier| {
        loader
            .entry_declares(&entrypoint.module.items, &qualifier)
            .expect("every header on the chain reads")
    };

    assert!(declares(Qualifier::from(["helper"])));
    assert!(declares(Qualifier::from(["helper", "deep"])));
    assert!(!declares(Qualifier::from(["stray"])));
}

/// The files a unit is written in are its header and every file module a `mod` chain reaches from it, in declaration order — a file beside them that nothing declares is none of them — and an entry's are the modules its own `mod` chain reaches.
#[test]
fn the_declared_files_are_what_the_mod_chains_reach() {
    let root = tree(
        "declared-files",
        &[
            ("lib.crs", "pub mod parse;\n"),
            ("parse.crs", "pub mod lexer;\n"),
            ("parse/lexer.crs", ""),
            ("stray.crs", ""),
            ("serve.crs", "mod helper;\n/std/print(\"\")\n"),
            ("serve/helper.crs", ""),
        ],
    );

    let library = RootSource::mounted(
        "json",
        RootKind::Ordinary,
        root.join("lib.crs"),
        root.to_path_buf(),
    );
    assert_eq!(
        library.declared_files().expect("every header reads"),
        vec![
            root.join("lib.crs"),
            root.join("parse.crs"),
            root.join("parse/lexer.crs"),
        ]
    );

    let (entrypoint, loader, _) =
        Entrypoint::opened(&root.join("serve.crs")).expect("the entry parses");
    assert_eq!(
        loader
            .entry_declared_files(&entrypoint.module.items)
            .expect("every header reads"),
        vec![root.join("serve/helper.crs")]
    );
}

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

/// Text written as a module is supplied as a unit of its own at the prefix given, its header held rather than read; text written as a program is no module.
#[test]
fn text_written_as_a_module_is_supplied_at_the_prefix_given() {
    let unit = RootSource::labelled_module("stdin", "<stdin>", "pub let one : /std/Nat = 1;\n")
        .expect("items alone are a module");

    assert_eq!(
        unit.mounts()
            .into_iter()
            .map(|mount| mount.prefix)
            .collect::<Vec<_>>(),
        vec![Qualifier::from(["stdin"])]
    );
    assert!(matches!(
        unit.load(&Qualifier::from(["stdin"])),
        Ok(module) if module.items.len() == 1
    ));

    assert!(RootSource::labelled_module("stdin", "<stdin>", "/std/print(\"\")\n").is_none());
}

/// A tree of `(relative path, contents)` pairs, in a directory of its own that goes away with the test.
fn tree(name: &str, files: &[(&str, &str)]) -> Temporary {
    let root = Temporary::new("root-source", name);

    for (path, source) in files {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, source).unwrap();
    }

    fs::create_dir_all(&root).unwrap();

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
    let source = RootSource::mounted(
        "json",
        RootKind::Ordinary,
        root.join("lib.crs"),
        root.to_path_buf(),
    );

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
}

/// The walk reads through the overlay as discovery does, so a `mod` an editor has written and not saved already declares its module.
#[test]
fn an_unsaved_mod_declares_its_module_through_the_overlay() {
    let root = tree("declares-overlay", &[("lib.crs", ""), ("fresh.crs", "")]);
    let overlay = Overlay::of([(root.join("lib.crs"), "pub mod fresh;\n".to_string())]);
    let source = RootSource::mounted(
        "json",
        RootKind::Ordinary,
        root.join("lib.crs"),
        root.to_path_buf(),
    )
    .with_overlay(overlay);

    assert!(
        source
            .declares_module(&Qualifier::from(["json", "fresh"]))
            .unwrap()
    );
}

/// A source over the tree at `root`, mounted as `/json` with `lib.crs` as its header.
fn json(root: &Path) -> RootSource {
    RootSource::mounted("json", RootKind::Ordinary, root.join("lib.crs"), root)
}

/// The one source `source` has read, after loading `/json`'s header alone.
fn header_read(source: &RootSource) -> Rc<Source> {
    let mut reads = source.reads();
    assert_eq!(reads.len(), 1, "the header alone was read");

    reads.pop().expect("one read").1
}

/// The memo is keyed by the file's one spelling and validated by its text, so a second source over an unchanged file is handed the parse the first one took — the same `Rc<Source>`, which nothing but the memo could produce twice.
#[test]
fn an_unchanged_file_is_parsed_once_per_thread() {
    let root = tree(
        "memo-unchanged",
        &[("lib.crs", "pub mod parse;\n"), ("parse.crs", "")],
    );
    let qualifier = Qualifier::from(["json"]);

    let first = json(&root);
    first.load(&qualifier).unwrap();
    let second = json(&root);
    second.load(&qualifier).unwrap();

    assert!(
        Rc::ptr_eq(&header_read(&first), &header_read(&second)),
        "the second load was handed the first's parse"
    );
}

#[test]
fn a_changed_file_is_parsed_again() {
    let root = tree(
        "memo-changed",
        &[("lib.crs", "pub mod parse;\n"), ("parse.crs", "")],
    );
    let qualifier = Qualifier::from(["json"]);

    let before = json(&root);
    before.load(&qualifier).unwrap();

    fs::write(root.join("lib.crs"), "pub mod parse;\npub mod more;\n").unwrap();
    let after = json(&root);
    let module = after.load(&qualifier).unwrap();

    assert!(!Rc::ptr_eq(&header_read(&before), &header_read(&after)));
    assert_eq!(header_read(&after).text, "pub mod parse;\npub mod more;\n");
    assert_eq!(module.items.len(), 2, "and the new text is what was parsed");
}

/// An overlay holding exactly the disk's text is the same text at the same path, so it is the same parse.
#[test]
fn an_overlay_holding_the_disks_text_shares_the_disks_parse() {
    let root = tree(
        "memo-overlay",
        &[("lib.crs", "pub mod parse;\n"), ("parse.crs", "")],
    );
    let qualifier = Qualifier::from(["json"]);

    let disk = json(&root);
    disk.load(&qualifier).unwrap();

    let overlay = Overlay::of([(root.join("lib.crs"), "pub mod parse;\n".to_string())]);
    let held = json(&root).with_overlay(overlay);
    held.load(&qualifier).unwrap();

    assert!(Rc::ptr_eq(&header_read(&disk), &header_read(&held)));
}

/// A parse failure is reported and records nothing, and it evicts nothing: the file's last good parse answers the next read of that text.
#[test]
fn a_parse_failure_is_reported_and_leaves_the_last_good_parse_in_place() {
    let root = tree(
        "memo-failure",
        &[("lib.crs", "pub mod parse;\n"), ("parse.crs", "")],
    );
    let qualifier = Qualifier::from(["json"]);

    let good = json(&root);
    good.load(&qualifier).unwrap();

    fs::write(root.join("lib.crs"), "pub mod parse;\n%%%\n").unwrap();
    let broken = json(&root);
    assert!(matches!(
        broken.load(&qualifier),
        Err(Error::ModuleLoadFailed { .. })
    ));
    assert!(
        broken.reads().is_empty(),
        "nothing is recorded for a read that did not parse"
    );

    fs::write(root.join("lib.crs"), "pub mod parse;\n").unwrap();
    let restored = json(&root);
    restored.load(&qualifier).unwrap();

    assert!(
        Rc::ptr_eq(&header_read(&good), &header_read(&restored)),
        "the failure evicted nothing"
    );
}
