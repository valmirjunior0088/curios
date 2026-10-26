//! Variance of a family's universe levels: which instances the walk compares at a level and which it treats as one, what the walk files of each level, and a carried vector held to what the walk would file.

use {
    super::test_support::*,
    crate::{Error, Globals, Verdict},
    curios_analysis::test_support::SYNTAX,
    curios_core::{Global, Intrinsic, Level, Module, Term, UniverseParam, Variance},
    curios_utilities::Qualifier,
};

fn level_one() -> Level {
    Level::zero().succ().expect("level zero has a successor")
}

fn refusals(module: &Module) -> Vec<Verdict> {
    fixture_verdicts(module, 1_000_000, &Globals::default(), SYNTAX)
}

/// Whether the identity put at two instances was itself refused: the walk's own verdict on the pair, apart from anything the reconciliation says of the declaration.
fn coercion_refused(verdicts: &[Verdict]) -> bool {
    verdicts.iter().any(|verdict| {
        verdict.name == Some(coercion_name()) && matches!(verdict.error, Error::Mismatch { .. })
    })
}

fn denied(verdicts: &[Verdict], family: Global, level: usize) -> bool {
    verdicts.iter().any(|verdict| {
        verdict.error
            == Error::VarianceDenied {
                name: family,
                level,
            }
    })
}

// The level types `Wrap`'s parameter and nothing it holds, so the two instances are one type.
#[test]
fn two_instances_apart_in_an_irrelevant_level_convert() {
    let wrap = Global::Authored(Qualifier::from(["Wrap"]));
    let module = coercion_module(
        wrap,
        wrap_declaration(vec![Variance::Irrelevant]),
        vec![Term::intrinsic(Intrinsic::NatType)],
        Level::zero(),
        level_one(),
    );

    assert_eq!(refusals(&module), Vec::new());
}

// `Box` holds a `Type u`, carried honestly and carried with no vector at all.
#[test]
fn an_invariant_level_is_compared() {
    let boxed = Global::Authored(Qualifier::from(["Box"]));

    for variances in [vec![Variance::Invariant], Vec::new()] {
        let module = coercion_module(
            boxed,
            box_declaration(variances),
            Vec::new(),
            level_one(),
            Level::zero(),
        );

        assert!(coercion_refused(&refusals(&module)));
    }
}

// The lie that would read `box(Type 0) : Box.{1}` back at `Box.{0}`: the walk compares two instances by the vector it computed, so the coercion is refused, and the entry carrying the lie is denied where it is declared, since the elaborator of a later unit reads what an entry carries. An entry past the declaration's count is denied the same way.
#[test]
fn a_carried_irrelevance_the_walk_denies_is_refused_and_read_by_no_comparison() {
    let boxed = Global::Authored(Qualifier::from(["Box"]));

    let believed = refusals(&coercion_module(
        boxed,
        box_declaration(vec![Variance::Irrelevant]),
        Vec::new(),
        level_one(),
        Level::zero(),
    ));
    assert!(coercion_refused(&believed));
    assert!(denied(&believed, boxed, 0));

    let overlong = refusals(&coercion_module(
        boxed,
        box_declaration(vec![Variance::Invariant, Variance::Irrelevant]),
        Vec::new(),
        level_one(),
        level_one(),
    ));
    assert!(denied(&overlong, boxed, 1));
}

// `Wrap` carried as invariant in a level its declaration never mentions, which is what a family read before its payload was known carries: the walk compares two instances by the vector it computed, in which the level is irrelevant, so the coercion between them stands, and a carried vector that claims less is denied nothing.
#[test]
fn a_carried_invariance_the_walk_would_grant_is_read_by_no_comparison() {
    let wrap = Global::Authored(Qualifier::from(["Wrap"]));
    let carried = |to: Level| {
        refusals(&coercion_module(
            wrap,
            wrap_declaration(vec![Variance::Invariant]),
            vec![Term::intrinsic(Intrinsic::NatType)],
            Level::zero(),
            to,
        ))
    };

    assert_eq!(carried(Level::zero()), Vec::new());

    assert_eq!(carried(level_one()), Vec::new());
}

// `Fam`'s first level reaches no payload and no index type, and its constructor's target puts it at `Box`'s invariant position: two instances apart in it have different inhabitants at one index. Carried honestly the coercion is refused; carried as irrelevant the declaration is.
#[test]
fn a_level_only_an_index_target_mentions_is_invariant() {
    let (_, honest) = index_target_module(vec![Variance::Invariant, Variance::Invariant]);
    assert!(coercion_refused(&refusals(&honest)));

    let (family, lying) = index_target_module(vec![Variance::Irrelevant, Variance::Invariant]);
    assert!(denied(&refusals(&lying), family, 0));
}

// A concept-shaped struct: the level its method binds at is held by a field, and the level of its carrier's codomain by the parameter's type alone.
#[test]
fn a_concepts_method_level_is_compared() {
    let honest = || vec![Variance::Invariant, Variance::Irrelevant];

    let (_, codomain_apart) = method_level_module(honest(), [0, 0], [0, 1]);
    assert_eq!(refusals(&codomain_apart), Vec::new());

    let (_, method_apart) = method_level_module(honest(), [0, 1], [1, 1]);
    assert!(coercion_refused(&refusals(&method_apart)));

    let (name, lying) = method_level_module(vec![Variance::Irrelevant; 2], [0, 1], [1, 1]);
    assert!(denied(&refusals(&lying), name, 0));
}

// `Held` reaches a family a unit in scope declares, which answers from the vector the certifier filed for it: its unit's record's, or the one this walk computes before it judges anything where the record files none, as here. So an irrelevance `Held` carries is granted over a scope's irrelevant `Wrap` and denied over its invariant `Box`, and over a scope whose `Box` carries a lie it is denied as well, what an entry carries being read by no judgment.
#[test]
fn a_family_of_a_unit_in_scope_answers_from_what_the_certifier_filed() {
    let level = Level::param(UniverseParam(0));
    let wrap = Global::Authored(Qualifier::from(["Wrap"]));
    let boxed = Global::Authored(Qualifier::from(["Box"]));
    let over_wrap = || {
        held_module(
            Term::induct_type_at(
                wrap,
                [level.clone()],
                [Term::intrinsic(Intrinsic::NatType)],
                Vec::<Term>::new(),
            ),
            Term::type_at(level.clone()),
            vec![Variance::Irrelevant],
        )
    };
    let over_box = || {
        held_module(
            Term::induct_type_at(
                boxed,
                [level.clone()],
                Vec::<Term>::new(),
                Vec::<Term>::new(),
            ),
            Term::type_at(level.succ().expect("a parameter has a successor")),
            vec![Variance::Irrelevant],
        )
    };
    let judged = |module: &Module, scope: AlreadyJudged| {
        fixture_verdicts(module, 1_000_000, &scope.globals(), SYNTAX)
    };

    let (_, module) = over_wrap();
    assert_eq!(
        judged(
            &module,
            scope_of(wrap, wrap_declaration(vec![Variance::Irrelevant]))
        ),
        Vec::new()
    );

    let (held, module) = over_box();
    assert!(denied(
        &judged(
            &module,
            scope_of(boxed, box_declaration(vec![Variance::Invariant]))
        ),
        held,
        0
    ));
    assert!(denied(
        &judged(
            &module,
            scope_of(boxed, box_declaration(vec![Variance::Irrelevant]))
        ),
        held,
        0
    ));
}

// `Uses` mentions its level only on `Alias.{u}(A)`, which unfolds to `A`: the recomputation reads the reduct and grants the irrelevance the entry carries.
#[test]
fn a_level_a_reduction_removes_is_carried_as_irrelevant() {
    let (_, module) = alias_module(vec![Variance::Irrelevant]);

    assert_eq!(refusals(&module), Vec::new());
}

/// Whether every refusal is the coercion's: the declarations the fixture forges were accepted, so a refused coercion is the walk's verdict on the instances and not the echo of a family it refused.
fn only_the_coercion_refused(verdicts: &[Verdict]) -> bool {
    coercion_refused(verdicts)
        && verdicts
            .iter()
            .all(|verdict| verdict.name == Some(coercion_name()))
}

// `Arm`'s first level is stated by one arm of a match stuck on its parameter and by no other part the walk reads: a stuck form is read through its children, so the arm's `Type u` holds the level, and two instances apart in it are two types. Granted irrelevant, `c(Type 0): Arm.{1, 2}(true)` read at `Arm.{0, 2}(true)` would hold `Type 0` at `Type 0`. Carried honestly the coercion is refused, and nothing else is; carried as irrelevant the declaration is denied.
#[test]
fn a_level_one_arm_of_a_stuck_match_states_is_invariant() {
    let (_, honest) = stuck_arm_module(vec![Variance::Invariant, Variance::Invariant]);
    assert!(only_the_coercion_refused(&refusals(&honest)));

    let (family, lying) = stuck_arm_module(vec![Variance::Irrelevant, Variance::Invariant]);
    assert!(denied(&refusals(&lying), family, 0));
}

// `Held` reaches `Box` only through `Pick`, a definition whose value is `Box.{l}` and names its second level nowhere: the walk reads the payload's reduct, so the first level stands at `Box`'s invariant position and the unfolding drops the second. Two instances apart in the first are refused at the coercion and nowhere else, two apart in the second convert, and carried as irrelevant in the first the declaration is denied.
#[test]
fn a_level_reached_through_a_definition_the_walk_unfolds_is_invariant() {
    let honest = || vec![Variance::Invariant, Variance::Irrelevant];

    let (_, kept_apart) = picked_module(honest(), [0, 0], [1, 0]);
    assert!(only_the_coercion_refused(&refusals(&kept_apart)));

    let (_, dropped_apart) = picked_module(honest(), [0, 0], [0, 1]);
    assert_eq!(refusals(&dropped_apart), Vec::new());

    let (family, lying) = picked_module(vec![Variance::Irrelevant; 2], [0, 0], [1, 0]);
    assert!(denied(&refusals(&lying), family, 0));
}
