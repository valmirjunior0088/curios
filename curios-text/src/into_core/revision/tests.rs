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

/// `a.crs` written as `text`, the unit lowered over `baseline`, and lowered whole. The baseline's paths are made first, as a report over it makes them, so the unit lowered over it revises them.
fn edited(root: &Path, text: &str, baseline: &PreparedText) -> (PreparedText, PreparedText) {
    fs::write(root.join("a.crs"), text).unwrap();
    let source = library(root);
    baseline.paths();
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

/// The file `path` under `root` written as `text`, the unit lowered over `baseline` and lowered whole, each with the tables a lowering over it reads. The baseline's paths are made first, as [`edited`] makes them.
fn rewritten(
    root: &Path,
    path: &str,
    text: &str,
    baseline: &PreparedText,
) -> (PreparedText, PreparedText) {
    fs::write(root.join(path), text).unwrap();
    let source = library(root);
    baseline.paths();
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
    assert_eq!(
        over.paths(),
        whole.paths(),
        "the paths each name is written at, and who may write each, differ"
    );
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

/// A binding made private is seen by its module alone, and a binding added is seen as a whole lowering decides.
#[test]
fn a_binding_made_private_is_seen_by_its_module_alone() {
    let root = files(
        "private",
        &[
            ("lib.crs", "pub mod a;\npub mod b;\n"),
            ("a.crs", "pub let x: Type = Type;\n\npub let y: Type = x;\n"),
            ("b.crs", "pub use /lib/a/{y};\n"),
        ],
    );
    let baseline = into_core_unit(&UnitSource::mounted(&library(&root)), &[], syntax()).unwrap();

    let (over, whole) = rewritten(
        &root,
        "a.crs",
        "let x: Type = Type;\n\npub let y: Type = Type;\n\npub let z: Type = y;\n",
        &baseline,
    );

    same_unit(&over, &whole);
}

/// The refusal of the file `path` under `root` written as `text`, lowered over `baseline` and lowered whole, each as a report prints it.
fn refusals(root: &Path, path: &str, text: &str, baseline: &PreparedText) -> (String, String) {
    fs::write(root.join(path), text).unwrap();
    let source = library(root);
    let over = into_core_unit_over(&UnitSource::mounted(&source), &[], syntax(), baseline, true)
        .err()
        .expect("the edit is refused over the baseline");
    let whole = into_core_unit(&UnitSource::mounted(&source), &[], syntax())
        .err()
        .expect("the edit is refused whole");

    (over.to_string(), whole.to_string())
}

/// A declaration lowered now whose signature names what its readers cannot see is refused as a whole lowering refuses it.
#[test]
fn a_declaration_lowered_now_exposing_a_private_name_is_refused() {
    let root = written(
        "exposing",
        "let hidden: Type = Type;\n\npub let shown: Type = Type;\n",
    );
    let baseline = into_core_unit(&UnitSource::mounted(&library(&root)), &[], syntax()).unwrap();

    let (over, whole) = refusals(
        &root,
        "a.crs",
        "let hidden: Type = Type;\n\npub let shown(x: hidden) -> Type = Type;\n",
        &baseline,
    );

    assert_eq!(over, whole);
}

/// A name made private is refused where a kept declaration exposes it, as a whole lowering refuses it: what a name is seen by moved, so every signature is audited again.
#[test]
fn a_name_made_private_refuses_what_exposes_it() {
    let root = written(
        "hidden",
        "pub let hidden: Type = Type;\n\npub let shown(x: hidden) -> Type = Type;\n",
    );
    let baseline = into_core_unit(&UnitSource::mounted(&library(&root)), &[], syntax()).unwrap();

    let (over, whole) = refusals(
        &root,
        "a.crs",
        "let hidden: Type = Type;\n\npub let shown(x: hidden) -> Type = Type;\n",
        &baseline,
    );

    assert_eq!(over, whole);
}

/// A concept, two records and `rest` after them.
fn witnessed(rest: &str) -> String {
    [
        "pub concept Show(A: Type): pub Type { show: A }\n\n",
        "pub struct Foo: pub Type { }\n\n",
        "pub struct Bar: pub Type { }\n\n",
        rest,
    ]
    .concat()
}

/// The records of what each witness's key read, of a unit lowered over a baseline and of a whole lowering, are one.
fn same_keys(over: &PreparedText, whole: &PreparedText) {
    assert!(
        over.half().keyed == whole.half().keyed,
        "what the witnesses' keys read differs"
    );
}

/// A witness an edit elsewhere does not reach keeps its name, and nothing of it is lowered again.
#[test]
fn a_witness_no_edit_reaches_keeps_its_name() {
    let root = written(
        "witness-kept",
        &witnessed("satisfy Show(Foo) { show = Foo { } }\n\npub let x: Type = Type;\n"),
    );
    let baseline = into_core_unit(&UnitSource::mounted(&library(&root)), &[], syntax()).unwrap();

    let (over, whole) = edited(
        &root,
        &witnessed("satisfy Show(Foo) { show = Foo { } }\n\npub let x: Type = Foo;\n"),
        &baseline,
    );

    same_unit(&over, &whole);
    same_keys(&over, &whole);
    assert_eq!(anew(&over), ["/lib/a/x"]);
}

/// A witness whose key reads a declaration lowered again is named by what its key reads now.
#[test]
fn a_witness_whose_key_read_moved_is_named_again() {
    let root = written(
        "witness-rekeyed",
        &witnessed("pub let Shown: Type = Foo;\n\nsatisfy Show(Shown) { show = Foo { } }\n"),
    );
    let baseline = into_core_unit(&UnitSource::mounted(&library(&root)), &[], syntax()).unwrap();

    let (over, whole) = edited(
        &root,
        &witnessed("pub let Shown: Type = Bar;\n\nsatisfy Show(Shown) { show = Foo { } }\n"),
        &baseline,
    );

    same_unit(&over, &whole);
    same_keys(&over, &whole);
}

/// A witness written ahead of a kept one at the same concept and key takes the kept one's place among its module's, and the kept one the next.
#[test]
fn a_witness_written_ahead_of_one_alike_takes_its_place() {
    let root = written(
        "witness-alike",
        &witnessed("satisfy Show(Foo) { show = Foo { } }\n"),
    );
    let baseline = into_core_unit(&UnitSource::mounted(&library(&root)), &[], syntax()).unwrap();

    let (over, whole) = edited(
        &root,
        &witnessed(
            "satisfy Show(Foo) { show = Foo { } }\n\nsatisfy Show(Foo) { show = Foo { } }\n",
        ),
        &baseline,
    );

    same_unit(&over, &whole);
    same_keys(&over, &whole);
}

/// The lints of a unit lowered over a baseline and of a whole lowering, each as a report prints it, in order.
fn lints_of(unit: &PreparedText) -> Vec<String> {
    unit.lints()
        .iter()
        .map(|lint| format!("{:?}", lint.report))
        .collect()
}

/// `a.crs` lowered whole, then written as each of `edits` in turn over the last lowering: at each, the lints of the unit lowered over the last and of a whole lowering are one.
fn same_lints(name: &str, first: &str, edits: &[&str]) {
    let root = written(name, first);
    let mut baseline =
        into_core_unit(&UnitSource::mounted(&library(&root)), &[], syntax()).unwrap();
    for edit in edits {
        let (over, whole) = edited(&root, edit, &baseline);
        assert_eq!(lints_of(&over), lints_of(&whole), "after writing {edit:?}");
        baseline = over;
    }
}

/// A private declaration whose one reader stops reading it becomes unused, and one a reader starts to read becomes used again.
#[test]
fn a_declaration_its_reader_drops_becomes_unused() {
    same_lints(
        "lint-dropped",
        "let helper: Type = Type;\n\npub let x: Type = helper;\n",
        &[
            "let helper: Type = Type;\n\npub let x: Type = Type;\n",
            "let helper: Type = Type;\n\npub let x: Type = helper;\n",
        ],
    );
}

/// Private declarations that read one another and nothing reaches are unused, a cycle among them included, and a root that reaches one of them reaches the rest.
#[test]
fn a_cycle_nothing_reaches_is_unused() {
    same_lints(
        "lint-cycle",
        "let even(n: Type) -> Type = odd(n);\n\nlet odd(n: Type) -> Type = even(n);\n\npub let x: Type = even(Type);\n",
        &[
            "let even(n: Type) -> Type = odd(n);\n\nlet odd(n: Type) -> Type = even(n);\n\npub let x: Type = Type;\n",
            "let even(n: Type) -> Type = odd(n);\n\nlet odd(n: Type) -> Type = even(n);\n\npub let x: Type = odd(Type);\n",
        ],
    );
}

/// A private module every declaration of which goes unused is reported once, at the module, and a declaration added that reads one of them lifts the fold.
#[test]
fn a_private_module_whose_declarations_go_unused_is_folded() {
    same_lints(
        "lint-module",
        "mod inner\n    pub let w: Type = Type;\nend\n\npub let x: Type = inner/w;\n",
        &[
            "mod inner\n    pub let w: Type = Type;\nend\n\npub let x: Type = Type;\n",
            "mod inner\n    pub let w: Type = Type;\nend\n\npub let x: Type = Type;\n\npub let y: Type = inner/w;\n",
        ],
    );
}

/// A declaration made private is reported where nothing reads it, and made public again is not.
#[test]
fn a_declaration_made_private_is_reported_where_nothing_reads_it() {
    same_lints(
        "lint-private",
        "pub let w: Type = Type;\n\npub let x: Type = Type;\n",
        &[
            "let w: Type = Type;\n\npub let x: Type = Type;\n",
            "pub let w: Type = Type;\n\npub let x: Type = Type;\n",
        ],
    );
}

/// The outcome of the file `path` under `root` written as `text`, lowered over `baseline` and lowered whole: the refusal each prints, or that each lowered.
fn outcomes(root: &Path, path: &str, text: &str, baseline: &PreparedText) -> (String, String) {
    fs::write(root.join(path), text).unwrap();
    let source = library(root);
    let outcome = |lowered: Result<PreparedText, crate::Error>| match lowered {
        Ok(_) => "lowered".to_string(),
        Err(error) => error.to_string(),
    };

    (
        outcome(into_core_unit_over(
            &UnitSource::mounted(&source),
            &[],
            syntax(),
            baseline,
            true,
        )),
        outcome(into_core_unit(&UnitSource::mounted(&source), &[], syntax())),
    )
}

/// An alias lowered again has the unit audited whole, every definition and re-export of the unit made read where the walk did not lower it, and the unit lowers or is refused as a whole lowering is.
#[test]
fn an_alias_lowered_again_is_audited_with_the_whole_unit() {
    let root = written(
        "alias",
        "struct Hidden: Type { }\n\npub struct Shown: pub Type { }\n\npub let Alias: Type = Shown;\n\npub let used(x: Alias) -> Type = Type;\n",
    );
    let baseline = into_core_unit(&UnitSource::mounted(&library(&root)), &[], syntax()).unwrap();

    for text in [
        "struct Hidden: Type { }\n\npub struct Shown: pub Type { }\n\npub let Alias: Type = Hidden;\n\npub let used(x: Alias) -> Type = Type;\n",
        "struct Hidden: Type { }\n\npub struct Shown: pub Type { }\n\npub let Alias: Type = Shown;\n\npub let used(x: Alias) -> Type = Type;\n\npub let more(x: Alias) -> Type = Type;\n",
    ] {
        let (over, whole) = outcomes(&root, "a.crs", text, &baseline);
        assert_eq!(over, whole, "after writing {text:?}");
    }
}

/// A witness of a file no edit touched whose key read a declaration of another file lowered again is named by what its key reads now: its module is entered though its text and scope stand.
#[test]
fn a_witness_of_a_standing_file_whose_key_read_moved_is_named_again() {
    let declared = |shown: &str| {
        [
            "pub concept Show(A: Type): pub Type { show: A }\n\n",
            "pub struct Foo: pub Type { }\n\n",
            "pub struct Bar: pub Type { }\n\n",
            "pub let Shown: Type = ",
            shown,
            ";\n",
        ]
        .concat()
    };
    let root = files(
        "witness-file",
        &[
            ("lib.crs", "pub mod a;\npub mod b;\n"),
            ("a.crs", &declared("Foo")),
            (
                "b.crs",
                "satisfy /lib/a/Show(/lib/a/Shown) { show = /lib/a/Foo { } }\n",
            ),
        ],
    );
    let baseline = into_core_unit(&UnitSource::mounted(&library(&root)), &[], syntax()).unwrap();

    let (over, whole) = rewritten(&root, "a.crs", &declared("Bar"), &baseline);

    same_unit(&over, &whole);
    same_keys(&over, &whole);
}

/// The `foreign` rows of modules no edit reaches stand among the unit's rows where their declarations stand, with a row lowered again between them, the modules passed unentered.
#[test]
fn a_foreign_row_no_edit_reaches_stands_where_its_declaration_does() {
    let root = files(
        "foreign",
        &[
            ("lib.crs", "pub mod a;\npub mod b;\npub mod c;\n"),
            ("a.crs", "foreign first_row(Nat) -> Nat;\n"),
            ("b.crs", "foreign middle_row(Nat) -> Nat;\n"),
            ("c.crs", "foreign last_row(Nat) -> Nat;\n"),
        ],
    );
    let baseline = into_core_unit(&UnitSource::mounted(&library(&root)), &[], syntax()).unwrap();

    let (over, whole) = rewritten(
        &root,
        "b.crs",
        "foreign middle_row(Bytes) -> Nat;\n",
        &baseline,
    );

    same_unit(&over, &whole);
    let rows = |unit: &PreparedText| format!("{:?}", unit.foreigns().iter().collect::<Vec<_>>());
    assert_eq!(rows(&over), rows(&whole));
    assert_eq!(anew(&over), ["/lib/b/middle_row"]);
}

/// A binding made private is spelled by its own module alone, and a binding a module starts to re-export is spelled at the new path too: the baseline's paths revised where the audiences were read again are the ones a whole lowering makes.
#[test]
fn the_paths_a_report_spells_by_are_revised_where_audiences_moved() {
    let root = files(
        "paths",
        &[
            ("lib.crs", "pub mod a;\npub mod b;\n"),
            ("a.crs", "pub let x: Type = Type;\n\npub let y: Type = x;\n"),
            ("b.crs", "pub let z: Type = Type;\n"),
        ],
    );
    let baseline = into_core_unit(&UnitSource::mounted(&library(&root)), &[], syntax()).unwrap();

    let (private, whole) = rewritten(
        &root,
        "a.crs",
        "let x: Type = Type;\n\npub let y: Type = x;\n",
        &baseline,
    );
    same_unit(&private, &whole);
    assert_ne!(private.paths(), baseline.paths());

    let (reexported, whole) = rewritten(
        &root,
        "b.crs",
        "pub use /lib/a/{y};\n\npub let z: Type = Type;\n",
        &private,
    );
    same_unit(&reexported, &whole);
    assert_ne!(reexported.paths(), private.paths());
}
