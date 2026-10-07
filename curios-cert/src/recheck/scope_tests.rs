//! A name already declared: the environment's own declaration passed over, any other refused before anything is judged.

use {
    super::test_support::*,
    crate::{Error, Globals, Verdict},
    curios_analysis::test_support::SYNTAX,
    curios_core::{Certification, Free, Global, Level, Term, Totality},
};

/// Whether `verdicts` is the one refusal a module redeclaring `name` meets: standing is decided before anything is judged, so no other verdict stands beside it.
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
/// Mutation-checked against the comparison: answering that the item is the environment's own certifies the module with no refusal.
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
        &already_judged(&judged_environment()),
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
/// Mutation-checked against the comparison: answering that the entry is the environment's own certifies the module with no refusal.
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
        &already_judged(&judged_environment()),
        SYNTAX,
    );
    assert!(
        redeclares(&verdicts, &shadowed_family()),
        "a registry entry differing from the environment's under its name was not refused for the name: {verdicts:?}",
    );
}

/// A registry entry is the environment's own by what the certifier reads of it: its terms, and the variance vector a walk answers from. Elaboration's stamps take no part — the polarity vector, which no judgment here reads and which a recompile writes again over the whole module, the owning module and the representation's privacy — so an entry differing from the environment's in those alone is passed over.
///
/// Mutation-checked against the comparison: taken over the whole entry, the family and the record are each refused for their name.
#[test]
fn a_registry_entry_differing_in_elaborations_stamps_alone_is_passed_over() {
    assert_eq!(
        fixture_verdicts(
            &two_registry_entries(Varied::Stamps),
            1_000_000,
            &already_judged(&two_registry_entries(Varied::Nothing)),
            SYNTAX,
        ),
        Vec::new(),
        "an entry differing from the environment's in elaboration's stamps alone was not passed over",
    );
}

/// What the certifier reads of a registry entry is compared in full: a family differing in its variance vector, which a later walk would answer from, and a record differing in a field's type are each refused for the name.
///
/// Mutation-checked against each comparison: with the variance vector left out the family is passed over and its carried vector denied instead, and with the arity left out the record is certified.
#[test]
fn a_registry_entry_differing_in_what_the_certifier_reads_is_refused() {
    let environment = already_judged(&two_registry_entries(Varied::Nothing));

    let verdicts = fixture_verdicts(
        &two_registry_entries(Varied::Variance),
        1_000_000,
        &environment,
        SYNTAX,
    );
    assert!(
        redeclares(&verdicts, &shadowed_family()),
        "a family differing from the environment's in its variance vector was not refused for the name: {verdicts:?}",
    );

    let verdicts = fixture_verdicts(
        &two_registry_entries(Varied::Field),
        1_000_000,
        &environment,
        SYNTAX,
    );
    assert!(
        redeclares(&verdicts, &kept_record()),
        "a record differing from the environment's in a field's type was not refused for the name: {verdicts:?}",
    );
}

/// A declaration equal to the one the environment holds under its name is the environment's own: it is passed over, and the module certified.
///
/// The module carries the environment's `shadowed` and `Shadowed`, rebuilt so that neither is the environment's allocation, beside `reader : Nat = shadowed`. The walk's record names `reader` and not `shadowed`, which is what being passed over leaves: the walk that built the environment judged it.
///
/// Mutation-checked against the comparison: answering that an equal declaration is redeclared refuses the module.
#[test]
fn a_declaration_equal_to_the_environments_is_passed_over() {
    let mut module = judged_environment();
    module.items.push(reader());

    let rechecked = fixture_certified(
        &module,
        1_000_000,
        &already_judged(&judged_environment()),
        SYNTAX,
    );

    assert_eq!(
        rechecked.verdicts,
        Vec::new(),
        "a module carrying the environment's own declarations beside a new one was refused",
    );
    assert_eq!(
        rechecked.certification.totality(&reader_name()),
        Some(Totality::Total),
        "the new item was not judged",
    );
    assert_eq!(
        rechecked.certification.totality(&shadowed_name()),
        None,
        "the environment's own item was judged again rather than passed over",
    );
}

/// A namespace answers for itself: a definition whose name the environment holds only as a registry key is refused, there being no entry it could be the environment's own by.
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
        &already_judged(&judged_environment()),
        SYNTAX,
    );
    assert!(
        redeclares(&verdicts, &shadowed_family()),
        "a definition under a name held only as a registry key was not refused for the name: {verdicts:?}",
    );
}

/// A group is the environment's whole or new whole: one some of whose names the environment holds is refused, named by its first member as a group is.
///
/// Mutation-checked against the comparison: answering that the group is the environment's own certifies the module with no refusal.
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
        &already_judged(&judged_environment()),
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
    let mut globals = already_judged(&unit);

    globals.mount(&unit, &Certification::default());
}

/// An environment is built from a unit that declares each name once: one declaring a name twice is its builder's fault, and no environment is made of it.
///
/// Mutation-checked against the assertion: with the entries collected, the later one standing, an environment is built.
#[test]
#[should_panic(expected = "a unit put in scope declares one name twice")]
fn an_environment_is_not_built_from_a_unit_declaring_a_name_twice() {
    already_judged(&group_naming_a_member_twice());
}
