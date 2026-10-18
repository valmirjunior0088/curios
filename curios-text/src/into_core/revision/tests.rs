//! A unit lowered over a baseline as a version of it: the unit a whole lowering of the same text gives, with its items where the baseline holds them.

use {
    super::super::{
        PreparedText, UnitSource, into_core_unit, into_core_unit_over, test_support::syntax,
    },
    crate::RootSource,
    curios_utilities::{RootKind, test_support::Temporary},
    std::{fs, path::Path},
};

/// `a.crs`, holding `text`, as the one module of the unit `/lib`, under a directory of its own.
fn written(name: &str, text: &str) -> Temporary {
    let root = Temporary::new("revision", name);
    fs::create_dir_all(&*root).unwrap();
    fs::write(root.join("lib.crs"), "pub mod a;\n").unwrap();
    fs::write(root.join("a.crs"), text).unwrap();

    root
}

/// The unit `/lib` as the files under `root` hold it now.
fn library(root: &Path) -> RootSource {
    RootSource::mounted("lib", RootKind::Ordinary, root.join("lib.crs"), root)
}

/// `a.crs` written as `text`, the unit lowered over `baseline`, and lowered whole.
fn edited(root: &Path, text: &str, baseline: &PreparedText) -> (PreparedText, PreparedText) {
    fs::write(root.join("a.crs"), text).unwrap();
    let source = library(root);
    let over =
        into_core_unit_over(&UnitSource::mounted(&source), &[], syntax(), baseline, true).unwrap();
    let whole = into_core_unit(&UnitSource::mounted(&source), &[], syntax()).unwrap();

    (over, whole)
}

/// The names a unit lowered over a baseline lowered anew, spelled.
fn anew(unit: &PreparedText) -> Vec<String> {
    unit.lowered_anew()
        .expect("lowered over a baseline")
        .iter()
        .map(|name| name.faithful().to_string())
        .collect()
}

/// A declaration lowered again at every edit is the one a whole lowering gives, and stands between the same two items each time, so a version keeps finding room for it.
#[test]
fn a_declaration_lowered_again_stands_where_it_stood() {
    let root = written(
        "again",
        "pub let x: Type = Type;\n\npub let y: Type = Type;\n\npub let z: Type = Type;\n",
    );
    let baseline = into_core_unit(&UnitSource::mounted(&library(&root)), &[], syntax()).unwrap();

    let (first, whole) = edited(
        &root,
        "pub let x: Type = Type;\n\npub let y: Type = x;\n\npub let z: Type = Type;\n",
        &baseline,
    );
    assert_eq!(first.core(), whole.core());
    assert_eq!(anew(&first), ["/lib/a/y"]);

    let (second, whole) = edited(
        &root,
        "pub let x: Type = Type;\n\npub let y: Type = z;\n\npub let z: Type = Type;\n",
        &first,
    );
    assert_eq!(second.core(), whole.core());
    assert_eq!(anew(&second), ["/lib/a/y"]);
    assert!(
        second.core().items.places().eq(first.core().items.places()),
        "the declaration lowered again stands at the place it stood"
    );
}

/// A declaration whose text moved ahead of another's is kept, and moved where a whole lowering writes it.
#[test]
fn a_declaration_moved_ahead_of_another_is_kept_where_it_is_written() {
    let root = written(
        "moved",
        "pub let x: Type = Type;\n\npub let y: Type = Type;\n\npub let z: Type = Type;\n",
    );
    let baseline = into_core_unit(&UnitSource::mounted(&library(&root)), &[], syntax()).unwrap();

    let (over, whole) = edited(
        &root,
        "pub let z: Type = Type;\n\npub let x: Type = Type;\n\npub let y: Type = Type;\n",
        &baseline,
    );

    assert_eq!(over.core(), whole.core());
    assert!(
        anew(&over).is_empty(),
        "every declaration is the baseline's"
    );
}

/// Declarations added and taken away at once leave the unit a whole lowering gives, the ones neither touches kept.
#[test]
fn declarations_added_and_taken_away_leave_the_unit_a_whole_lowering_gives() {
    let root = written(
        "added",
        "pub let x: Type = Type;\n\npub let y: Type = Type;\n\npub let z: Type = Type;\n",
    );
    let baseline = into_core_unit(&UnitSource::mounted(&library(&root)), &[], syntax()).unwrap();

    let (over, whole) = edited(
        &root,
        "pub let w: Type = Type;\n\npub let x: Type = Type;\n\npub let v: Type = x;\n\npub let u: Type = v;\n\npub let z: Type = Type;\n",
        &baseline,
    );

    assert_eq!(over.core(), whole.core());
    assert_eq!(anew(&over), ["/lib/a/u", "/lib/a/v", "/lib/a/w"]);
    assert_eq!(
        over.dropped()
            .expect("lowered over a baseline")
            .iter()
            .map(|name| name.faithful().to_string())
            .collect::<Vec<_>>(),
        ["/lib/a/y"],
    );
}

/// `files` written under a directory of their own, `lib.crs` the header of the unit `/lib`.
fn files(name: &str, files: &[(&str, &str)]) -> Temporary {
    let root = Temporary::new("revision", name);
    fs::create_dir_all(&*root).unwrap();
    for (path, text) in files {
        fs::write(root.join(path), text).unwrap();
    }

    root
}

/// The file `path` under `root` written as `text`, the unit lowered over `baseline` and lowered whole, each with the tables a lowering over it reads.
fn rewritten(
    root: &Path,
    path: &str,
    text: &str,
    baseline: &PreparedText,
) -> (PreparedText, PreparedText) {
    fs::write(root.join(path), text).unwrap();
    let source = library(root);
    let over =
        into_core_unit_over(&UnitSource::mounted(&source), &[], syntax(), baseline, true).unwrap();
    let whole = into_core_unit(&UnitSource::mounted(&source), &[], syntax()).unwrap();

    (over, whole)
}

/// The direct interfaces, the export views and the items of a unit lowered over a baseline are the ones a whole lowering gives.
fn same_unit(over: &PreparedText, whole: &PreparedText) {
    assert!(over.table == whole.table, "the direct interfaces differ");
    assert!(over.public == whole.public, "the export views differ");
    assert_eq!(over.core(), whole.core());
}

/// A binding added to a module another re-exports by a glob is exported by both, the module that re-exports it resolved again for what its `pub use` read.
#[test]
fn a_binding_added_reaches_the_modules_that_re_export_it() {
    let root = files(
        "glob",
        &[
            ("lib.crs", "pub mod a;\npub mod b;\n"),
            ("a.crs", "pub let x: Type = Type;\n"),
            ("b.crs", "pub use /lib/a/*;\n"),
        ],
    );
    let baseline = into_core_unit(&UnitSource::mounted(&library(&root)), &[], syntax()).unwrap();

    let (over, whole) = rewritten(
        &root,
        "a.crs",
        "pub let x: Type = Type;\n\npub let y: Type = Type;\n",
        &baseline,
    );

    same_unit(&over, &whole);
}

/// A module the unit no longer declares leaves its tables, with every module under it.
#[test]
fn a_module_taken_away_leaves_the_tables() {
    let root = files(
        "taken",
        &[
            ("lib.crs", "pub mod a;\npub mod b;\n"),
            ("a.crs", "pub let x: Type = Type;\n"),
            ("b.crs", "pub mod inner\n    pub let w: Type = Type;\nend\n"),
        ],
    );
    let baseline = into_core_unit(&UnitSource::mounted(&library(&root)), &[], syntax()).unwrap();

    let (over, whole) = rewritten(&root, "lib.crs", "pub mod a;\n", &baseline);

    same_unit(&over, &whole);
}

/// The namespace an inductive makes leaves the tables with the inductive.
#[test]
fn a_family_taken_away_takes_its_namespace() {
    let root = files(
        "family",
        &[
            ("lib.crs", "pub mod a;\n"),
            (
                "a.crs",
                "pub induct Colour : pub Type | red() | green() end\n\npub let x: Type = Type;\n",
            ),
        ],
    );
    let baseline = into_core_unit(&UnitSource::mounted(&library(&root)), &[], syntax()).unwrap();

    let (over, whole) = rewritten(&root, "a.crs", "pub let x: Type = Type;\n", &baseline);

    same_unit(&over, &whole);
}
