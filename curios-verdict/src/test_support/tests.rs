//! What a history's builds file and mount.

use {super::*, curios_common::Qualifier, curios_core::Totality};

/// The name `/lib/<label>`.
fn lib(label: &str) -> Global {
    Global::Authored(Qualifier::from(["lib", label]))
}

/// What the last unit a build mounts states of `name`, as its record classifies it.
fn mounted(history: &History, name: &Global) -> Option<Totality> {
    history
        .built(|scope| {
            let unit = scope.units().last().expect("a unit is mounted");
            unit.certification().totality(name)
        })
        .expect("the history builds")
}

#[test]
fn a_step_files_its_unit_and_a_later_build_mounts_it_from_the_store() {
    let mut history = History::new();
    history
        .step("lib", Written::Program, "pub let one: /std/Nat = 1;\n")
        .expect("the step builds");

    let (mounts, lowered) = history
        .built(|scope| {
            let unit = scope.units().last().expect("a unit is mounted");
            (
                unit.mounts()
                    .iter()
                    .map(|mount| mount.prefix)
                    .collect::<Vec<_>>(),
                unit.text().is_some(),
            )
        })
        .expect("the history builds");

    assert_eq!(mounts, [Qualifier::from(["lib"])]);
    assert!(lowered, "a program's unit holds its lowering");
    assert!(mounted(&history, &lib("one")).is_some());
}

#[test]
fn a_core_text_unit_stepped_twice_is_the_unit_its_last_text_is() {
    let first = "let /lib/one : #NatType = 1;\n";
    let last = "let /lib/one : #NatType = 2;\n";
    let core = |history: &History| {
        history
            .built(|scope| {
                let unit = scope.units().last().expect("a unit is mounted");
                assert!(
                    unit.text().is_none(),
                    "a unit read from Core text holds no lowering"
                );
                curios_archive::to_bytes(unit.core())
                    .expect("a module serializes")
                    .to_vec()
            })
            .expect("the history builds")
    };

    let mut stepped = History::new();
    stepped
        .step("lib", Written::CoreText, first)
        .expect("the first step builds");
    stepped
        .step("lib", Written::CoreText, last)
        .expect("the last step builds over the first");
    let mut whole = History::new();
    whole
        .step("lib", Written::CoreText, last)
        .expect("the step builds");

    assert_eq!(core(&stepped), core(&whole));
}

/// A step that rewrites a program's unit as Core text is read from its Core text: the record the program's step filed names the program's file, whose text still stands, and not the one the unit is now written in.
///
/// Mutation-checked at `Verdicts`' `agrees`: with the record held only to the files it names, the program's unit is mounted and its lowering with it.
#[test]
fn a_unit_stepped_from_a_program_to_core_text_is_read_from_its_core_text() {
    let mut history = History::new();
    history
        .step("lib", Written::Program, "pub let one: /std/Nat = 1;\n")
        .expect("the program step builds");
    history
        .step("lib", Written::CoreText, "let /lib/one : #NatType = 2;\n")
        .expect("the Core text step builds");

    let lowered = history
        .built(|scope| {
            scope
                .units()
                .last()
                .expect("a unit is mounted")
                .text()
                .is_some()
        })
        .expect("the history builds");

    assert!(
        !lowered,
        "the program's unit was mounted for the Core text step"
    );
}

#[test]
fn a_withheld_record_is_the_one_a_later_build_mounts() {
    let mut history = History::new();
    history
        .step(
            "lib",
            Written::Program,
            "pub let one: /std/Nat = 1;\npub let two: /std/Nat = 2;\n",
        )
        .expect("the step builds");

    history
        .withhold("lib", Some(&[lib("one")]))
        .expect("the record is withheld in part");
    assert!(mounted(&history, &lib("one")).is_none());
    assert!(mounted(&history, &lib("two")).is_some());

    history
        .withhold("lib", None)
        .expect("the record is withheld whole");
    assert!(mounted(&history, &lib("two")).is_none());
}

#[test]
fn a_record_no_step_filed_is_not_withheld() {
    let mut history = History::new();

    assert!(matches!(
        history.withhold("lib", None),
        Err(Stopped {
            why: Unbuilt::Stored(_),
            ..
        })
    ));
}
