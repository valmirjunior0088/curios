//! The census of a unit's lowering, put to a library whose every record holds and to records altered so that one does not.

use {
    super::*,
    crate::{into_core::test_support::*, *},
    curios_common::{RootKind, test_support::*},
    std::fs,
};

/// A library under a directory of its own: a concept and its witness, readers of its method through a `use`, through a module re-exporting it by a glob and by its absolute path, and a bare name of a reader's own module.
fn library(name: &str) -> (Temporary, PreparedText) {
    let root = Temporary::new("census", name);
    fs::create_dir_all(&*root).unwrap();
    for (path, text) in [
        (
            "lib.crs",
            "pub mod m;\npub mod n;\npub mod p;\npub mod q;\npub mod r;\n",
        ),
        (
            "m.crs",
            "pub concept C(A: Type): pub Type {\n    meth: A,\n}\n\nsatisfy C(Type) {\n    meth = Type,\n}\n",
        ),
        (
            "n.crs",
            "use /lib/m/{C};\n\npub let through_use : Type = C/meth(@Type);\n",
        ),
        ("p.crs", "pub use /lib/m/*;\n"),
        (
            "q.crs",
            "use /lib/p/{C};\n\npub let through_reexport : Type = C/meth(@Type);\n",
        ),
        (
            "r.crs",
            "pub let absolute : Type = /lib/m/C/meth(@Type);\n\npub let again : Type = absolute;\n",
        ),
    ] {
        fs::write(root.join(path), text).unwrap();
    }
    let source = RootSource::mounted("lib", RootKind::Ordinary, root.join("lib.crs"), &*root);
    let unit = into_core_unit(&UnitSource::mounted(&source), &[], syntax()).unwrap();

    (root, unit)
}

/// The record of the declaration `unit` lowered `path` from, with the place its first item stands at.
fn record_of(unit: &PreparedText, path: &str) -> (i64, LoweredDeclaration) {
    let name = global_name(path);
    unit.lowerings()
        .iter()
        .find(|(first, _)| {
            usize::try_from(*first)
                .is_ok_and(|first| unit.core().items.names(first).contains(&name))
        })
        .map(|(first, record)| (first, LoweredDeclaration::clone(record)))
        .unwrap_or_else(|| panic!("{path} is declared"))
}

#[test]
fn a_library_lowered_whole_holds_every_declaration_to_its_record_and_its_answers() {
    let (_root, unit) = library("whole");

    assert_eq!(unit.census(&[], syntax()), Vec::<String>::new());
}

#[test]
fn a_record_missing_a_binding_its_doors_read_answers_otherwise() {
    let (_root, unit) = library("unrecorded");
    let (first, mut record) = record_of(&unit, "/lib/n/through_use");
    record
        .consulted
        .retain(|consulted| !matches!(consulted, Consulted::Scope { .. }));

    let departures = unit.with_census(&[], syntax(), |census| census.departures(first, &record));

    assert!(
        departures
            .iter()
            .any(|departure| departure.contains("`C/meth` answers otherwise")),
        "{departures:?}"
    );
}

#[test]
fn a_record_whose_answer_is_not_what_was_lowered_lowers_otherwise() {
    let (_root, unit) = library("unanswered");
    let (first, mut record) = record_of(&unit, "/lib/n/through_use");
    let mut trace = curios_core::Trace::default();
    for (lookup, found) in record.trace.asked() {
        trace.record(*lookup, Found::Declaration(found.target()));
    }
    record.trace = trace;

    let departures = unit.with_census(&[], syntax(), |census| census.departures(first, &record));

    assert!(
        departures
            .iter()
            .any(|departure| departure.contains("lowers otherwise where only its answers stand")),
        "{departures:?}"
    );
}
