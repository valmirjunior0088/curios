//! A name already declared: a module is refused for it before anything is judged, whether the name is its environment's or one of its own.

use {
    super::test_support::*,
    crate::{Error, Globals, Verdict},
    curios_analysis::test_support::SYNTAX,
    curios_core::{Certification, Free, Global, Level, Places, Term},
};

/// Whether `verdicts` is the one refusal a module redeclaring `name` meets: a redeclaration is refused before anything is judged, so no other verdict stands beside it.
fn redeclares(verdicts: &[Verdict], name: &Global) -> bool {
    matches!(
        verdicts,
        [Verdict { name: Some(owner), error: Error::Redeclared(redeclared) }]
            if owner == name && *redeclared == Free::from(name)
    )
}

/// A definition under a name the environment holds, differing from the environment's, is refused for the name.
///
/// `shadowed` is `Nat` in the environment and `Bool` in the module, and `reader : Nat = shadowed` reads it. Standing alone the module is refused, `reader` inferring `Bool` where `Nat` was declared. Passed over by name it would be certified, `reader` typed against the environment's `shadowed` while every stage below compiles the body the module holds; so the module is refused for `shadowed`, and `reader` is not judged at all.
///
/// Mutation-checked against the environment's question: with an item's held name never asked after, the module is judged, and `shadowed` is refused where the walk writes it and no sooner.
#[test]
fn a_definition_differing_from_the_environments_is_refused() {
    let module = shadowing_items();

    let alone = fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX);
    assert!(
        alone
            .iter()
            .any(|verdict| verdict.name == Some(reader_name())
                && matches!(verdict.error, Error::Mismatch { .. })),
        "the control stopped refusing: `reader : Nat` must not accept a `Bool`-typed `shadowed`: {alone:?}",
    );

    let verdicts = fixture_verdicts(
        &module,
        1_000_000,
        &already_judged(&judged_environment()).globals(),
        SYNTAX,
    );
    assert!(
        redeclares(&verdicts, &shadowed_name()),
        "a definition differing from the environment's under its name was not refused for the name: {verdicts:?}",
    );
}

/// A registry entry under a name the environment holds, differing from the environment's, is refused for the name.
///
/// Declared over the environment's it would be the entry every rule reads, with the clauses that check a declaration passed over: a payload at `Type 5` under a family declared at `Type 0` is refused as `Oversized` standing alone, and by name alone it would be certified against an environment holding a benign `Shadowed`.
///
/// Mutation-checked against the environment's question: with a held registry key never asked after, the entry is refused where the walk writes it and judged all the same, `Oversized` beside the name's refusal.
#[test]
fn a_registry_entry_differing_from_the_environments_is_refused() {
    let oversized = shadowing_registry(Term::type_at(Level::constant(5)));

    let alone = fixture_verdicts(&oversized, 1_000_000, &Globals::default(), SYNTAX);
    assert!(
        alone
            .iter()
            .any(|verdict| matches!(verdict.error, Error::Oversized { .. })),
        "the control stopped refusing: a `Type 5` payload exceeds a family declared at `Type 0`: {alone:?}",
    );

    let verdicts = fixture_verdicts(
        &oversized,
        1_000_000,
        &already_judged(&judged_environment()).globals(),
        SYNTAX,
    );
    assert!(
        redeclares(&verdicts, &shadowed_family()),
        "a registry entry differing from the environment's under its name was not refused for the name: {verdicts:?}",
    );
}

/// The polarity vector a walk composes through is the one the certifier filed for the family, never the one its entry carries.
///
/// `Wrapper(A)` holds a function out of `A`, and its entry is stamped `Strict` in `A`: elaboration's vector, and here a lie. `Outer` stores a `Wrapper(Outer)`, so it reaches itself to the left of an arrow and is refused. The walk over `Outer` is handed `Wrapper`'s unit twice: under the record the walk over that unit left, which files the vector that walk computed, and under no record, where the walk computes the vector for itself before it judges anything. Believing the stamp on either path accepts `Outer`, a family negative in itself.
///
/// The control is the same pair with `Wrapper` holding an `A`: the stamp is then the truth and `Outer` is accepted on both paths, so the refusal is the vector's and not that of every family storing itself in a mounted one.
///
/// Mutation-checked against the environment's construction: keeping the vector each entry carries accepts `Outer`.
#[test]
fn a_mounted_familys_vector_is_the_certifiers_and_not_its_entrys() {
    for negatively in [true, false] {
        let unit = wrapper_of_its_parameter(negatively);
        let certified = fixture_certified(&unit, 1_000_000, &Globals::default(), SYNTAX);
        assert_eq!(
            certified.verdicts,
            Vec::new(),
            "`Wrapper` is well formed on its own, whichever way it holds `A`",
        );

        let environments = [
            (
                "the record its walk left",
                AlreadyJudged::with(&unit, &certified.certification),
            ),
            ("no record", already_judged(&unit)),
        ];
        for (under, environment) in environments {
            let verdicts = fixture_verdicts(
                &stored_in_the_wrapper(),
                1_000_000,
                &environment.globals(),
                SYNTAX,
            );
            let refused = verdicts.iter().any(|verdict| {
                verdict.name == Some(stored_family())
                    && matches!(verdict.error, Error::NotPositive { .. })
            });

            match negatively {
                true => assert!(
                    refused,
                    "`Outer` was accepted on a `Strict` its entry's stamp claims, under {under}: {verdicts:?}",
                ),
                false => assert_eq!(
                    verdicts,
                    Vec::new(),
                    "the control stopped holding: `Outer` stores itself strictly, under {under}",
                ),
            }
        }
    }
}

/// A declaration under a name the environment holds is refused for the name though it is the environment's, term for term: what an environment holds is read where it stands, and no comparison decides that two declarations under one name are one.
///
/// The module carries the environment's `shadowed` and `Shadowed`, rebuilt so that neither is the environment's allocation, beside `reader : Nat = shadowed`. Each held name is refused once and nothing is judged, `reader` with the rest: the walk's record is empty.
///
/// Mutation-checked against each of the environment's two questions, an item's names and a registry's keys: with either never asked, the module is refused for the other name alone.
#[test]
fn a_declaration_equal_to_the_environments_is_refused_for_its_name() {
    let mut module = judged_environment();
    module.items.push(reader());

    let rechecked = fixture_certified(
        &module,
        1_000_000,
        &already_judged(&judged_environment()).globals(),
        SYNTAX,
    );

    let refused = rechecked
        .verdicts
        .iter()
        .map(|verdict| match &verdict.error {
            Error::Redeclared(redeclared) => (verdict.name, Some(*redeclared)),
            _ => (verdict.name, None),
        })
        .collect::<Vec<_>>();
    assert_eq!(
        refused,
        [shadowed_name(), shadowed_family()].map(|name| (Some(name), Some(Free::from(&name)))),
        "a module carrying its environment's own declarations was not refused once for each name",
    );
    assert_eq!(
        rechecked.certification.totality(&reader_name()),
        None,
        "a module refused for a name was judged all the same",
    );
}

/// A name is one declaration across the namespaces: a definition under a name the environment holds as a registry key and under no definition is refused for it.
///
/// The definition is well typed, and standing alone it is certified, so the refusal is the name's.
///
/// Mutation-checked against the cross-namespace question: with it removed the definition is judged and the module certified.
#[test]
fn a_definition_under_a_name_the_environment_holds_as_a_registry_key_is_refused() {
    let module = definition_under_the_family();

    assert_eq!(
        fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX),
        Vec::new(),
        "the control stopped certifying: the definition is well typed standing alone",
    );

    let verdicts = fixture_verdicts(
        &module,
        1_000_000,
        &already_judged(&judged_environment()).globals(),
        SYNTAX,
    );
    assert!(
        redeclares(&verdicts, &shadowed_family()),
        "a definition under a name held only as a registry key was not refused for the name: {verdicts:?}",
    );
}

/// A group one of whose names the environment holds is refused, named by its first member as a group is.
///
/// Mutation-checked against the environment's question: with an item's held name never asked after, the group is judged and refused where the walk writes its members, and no sooner.
#[test]
fn a_group_the_environment_holds_in_part_is_refused() {
    let module = group_held_in_part();

    assert_eq!(
        fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX),
        Vec::new(),
        "the control stopped certifying: the group is well formed standing alone",
    );

    let verdicts = fixture_verdicts(
        &module,
        1_000_000,
        &already_judged(&judged_environment()).globals(),
        SYNTAX,
    );
    assert!(
        redeclares(&verdicts, &shadowed_name()),
        "a group held in part was not refused for the name the environment holds: {verdicts:?}",
    );
}

/// A name is declared by one item of a module: a second item under it is refused, and so is a group naming one member twice.
///
/// The walk orders items by what each names, and a name declared twice has two owners: `thm : p` is typed while `p` is `Holds`, the other `p` names `thm` and so is judged after it, and its definition would stand where the first did — the closing `thm` then read at a `p` that reduces to `Absurd`. With `p` declared once the closing term is the `Mismatch` it should be.
///
/// Mutation-checked against the set of names already declared: with it never consulted the program is certified at `Absurd` with no refusal.
#[test]
fn a_name_declared_by_two_items_is_refused() {
    let once = fixture_verdicts(
        &statement_declared(false),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );
    assert!(
        once.iter().any(
            |verdict| verdict.name.is_none() && matches!(verdict.error, Error::Mismatch { .. })
        ),
        "the control stopped refusing: `thm : Holds` does not close a program at `Absurd`: {once:?}",
    );

    let twice = fixture_verdicts(
        &statement_declared(true),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );
    assert!(
        redeclares(&twice, &statement_name()),
        "a name declared by two items was not refused: {twice:?}",
    );

    let repeated = fixture_verdicts(
        &group_naming_a_member_twice(),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );
    assert!(
        redeclares(&repeated, &shadowed_name()),
        "a group naming one member twice was not refused: {repeated:?}",
    );
}

/// A name is held by one registry of a module: a `struct` and an `induct` under one name are refused.
///
/// Every analysis over the declaration set files a declaration under its name, so of two under one name one is never analysed: `struct Bad { f : (Bad) -> Absurd }` beside an empty `induct Bad` is read as the family twice, and the record's negative occurrence closes Curry's paradox. With the record alone it is the `NotPositive` it should be.
///
/// Mutation-checked against the question of the other registry: with it removed the program is certified at `Absurd` with no refusal.
#[test]
fn a_name_in_both_registries_is_refused() {
    let alone = fixture_verdicts(
        &record_negative_in_itself(false),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );
    assert!(
        alone
            .iter()
            .any(|verdict| matches!(verdict.error, Error::NotPositive { .. })),
        "the control stopped refusing: the record names itself to the left of an arrow: {alone:?}",
    );

    let both = fixture_verdicts(
        &record_negative_in_itself(true),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );
    assert!(
        redeclares(&both, &negative_record_name()),
        "a name in both registries was not refused: {both:?}",
    );
}

/// A driver mounts a unit once. Mounting one twice is a construction bug in every namespace, a registry entry as much as a definition.
///
/// Mutation-checked against the registry's assertion: extended over the entry already there, the second mount returns.
#[test]
#[should_panic(expected = "a mounted unit redeclared a name already in scope")]
fn a_registry_entry_mounted_twice_is_a_construction_bug() {
    let unit = shadowing_registry(Term::type_ground());
    let judged = already_judged(&unit);
    let places = Places::of(&unit);
    let certification = Certification::default();
    let mut globals = judged.globals();

    globals.mount(&unit, &places, &certification);
}
