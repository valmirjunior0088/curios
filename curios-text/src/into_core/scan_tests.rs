//! What a unit keeps of each of its files: a later lowering that reads a file as the text a baseline read takes what the baseline read of it, and parses the file only where a declaration of it is lowered again.

use {
    super::{Headed, Loaded, Scan, UnitSource, into_core_unit, test_support::syntax},
    crate::{RootSource, tree_of},
    curios_utilities::{Qualifier, RootKind, Source, test_support::Temporary},
    std::{collections::BTreeMap, fs, path::Path, rc::Rc, sync::Arc},
};

/// `files` written under a directory of their own, `lib.crs` the header of the unit `/lib`.
fn written(name: &str, files: &[(&str, &str)]) -> Temporary {
    let root = Temporary::new("scan", name);
    fs::create_dir_all(&*root).unwrap();
    for (path, text) in files {
        fs::write(root.join(path), text).unwrap();
    }

    root
}

/// The unit `/lib` as the files under `root` hold it now.
fn library(root: &Path) -> RootSource {
    RootSource::mounted("lib", RootKind::Ordinary, root.join("lib.crs"), root)
}

/// One item of every kind a module holds, and one the parser cannot read.
const EVERY_KIND: &str = "use /lib/b/{z};

pub mod inner
    pub let w: Type = Type;
end

pub let x: Type = Type;

pub induct Colour : pub Type | red() | green() end

pub struct Box: pub Type {
    held: Type,
}

pub concept Shown(A: Type): pub Type {
    shown(A) -> Type,
}

test nothing = x;

pub let unread: Type = ;
";

#[test]
fn a_file_read_as_the_text_a_baseline_read_is_not_parsed() {
    let root = written(
        "standing",
        &[
            ("lib.crs", "pub mod a;\n"),
            ("a.crs", "pub let x: Type = Type;\n"),
        ],
    );
    let module = Qualifier::from(["lib", "a"]);
    let baseline = into_core_unit(&UnitSource::mounted(&library(&root)), &[], syntax()).unwrap();

    let source = library(&root);
    let loaded = Loaded::of(
        &UnitSource::mounted(&source),
        Some(baseline.scans()),
        &module,
    )
    .unwrap();

    assert!(
        loaded.tree.get().is_none(),
        "a file whose text stands was parsed"
    );
    assert!(
        Rc::ptr_eq(&loaded.heads, &baseline.scans()[&module].heads),
        "a text this thread already holds is the source its scan lies in, and nothing is placed again"
    );
    assert_eq!(
        source.reads().len(),
        1,
        "a file read for its text is a file the unit read"
    );
}

#[test]
fn a_file_whose_text_changed_is_parsed() {
    let root = written(
        "changed",
        &[
            ("lib.crs", "pub mod a;\n"),
            ("a.crs", "pub let x: Type = Type;\n"),
        ],
    );
    let module = Qualifier::from(["lib", "a"]);
    let baseline = into_core_unit(&UnitSource::mounted(&library(&root)), &[], syntax()).unwrap();

    fs::write(
        root.join("a.crs"),
        "pub let x: Type = Type;\n\npub let y: Type = x;\n",
    )
    .unwrap();
    let source = library(&root);
    let loaded = Loaded::of(
        &UnitSource::mounted(&source),
        Some(baseline.scans()),
        &module,
    )
    .unwrap();

    assert!(loaded.tree.get().is_some());
    assert_eq!(
        loaded.heads.len(),
        2,
        "what a changed file declares is read off its own text"
    );
}

/// A unit restored from a store holds each scan in a source of its own, and one built elsewhere under another path: what the scan kept is the heads a parse of the file as read gives, every position in the file that was read.
#[test]
fn what_a_scan_kept_is_placed_in_the_file_as_it_is_read() {
    let root = written(
        "placed",
        &[("lib.crs", "pub mod a;\n"), ("a.crs", EVERY_KIND)],
    );
    let module = Qualifier::from(["lib", "a"]);
    let elsewhere = Source::held("elsewhere/a.crs", EVERY_KIND);
    let scans = BTreeMap::from([(
        module,
        Scan {
            source: Arc::clone(&elsewhere),
            heads: Rc::new(Headed::of(&tree_of(&elsewhere).unwrap())),
        },
    )]);

    let source = library(&root);
    let loaded = Loaded::of(&UnitSource::mounted(&source), Some(&scans), &module).unwrap();

    assert!(
        loaded.tree.get().is_none(),
        "a file whose text stands was parsed"
    );
    let (_, read) = source.reads().pop().expect("the file was read");
    assert_eq!(
        format!("{:?}", loaded.heads),
        format!("{:?}", Headed::of(&tree_of(&read).unwrap())),
    );
}

/// A declaration of a standing file is lowered from its tree where a name it wrote resolves elsewhere, which is what parses the file: the unit is the one lowering the whole of it gives.
#[test]
fn a_standing_file_is_parsed_where_a_declaration_of_it_is_lowered_again() {
    let files = [
        (
            "lib.crs",
            "pub mod a;\npub mod b;\npub mod c;\npub mod d;\n",
        ),
        (
            "a.crs",
            "pub let y: Type = /lib/b/x;\n\npub let z: Type = Type;\n",
        ),
        ("b.crs", "pub use /lib/c/{x};\n"),
        ("c.crs", "pub let x: Type = Type;\n"),
        ("d.crs", "pub let x: Type = Type;\n"),
    ];
    let root = written("lowered-again", &files);
    let baseline = into_core_unit(&UnitSource::mounted(&library(&root)), &[], syntax()).unwrap();

    fs::write(root.join("b.crs"), "pub use /lib/d/{x};\n").unwrap();
    let over = super::into_core_unit_over(
        &UnitSource::mounted(&library(&root)),
        &[],
        syntax(),
        &baseline,
    )
    .unwrap();
    let whole = into_core_unit(&UnitSource::mounted(&library(&root)), &[], syntax()).unwrap();

    assert_eq!(over.core(), whole.core());
    assert_eq!(
        over.kept_from(&baseline)
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>(),
        ["/lib/a/z", "/lib/c/x", "/lib/d/x"],
        "every declaration but the one whose name moved is the baseline's"
    );
}
