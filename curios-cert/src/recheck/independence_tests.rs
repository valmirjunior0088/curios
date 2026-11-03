//! A verdict filed for one declaration leaves every other declaration's verdict as a walk that refused nothing would have left it.

use {
    super::test_support::*,
    crate::*,
    curios_analysis::test_support::*,
    curios_common::{Plicity, Qualifier},
    curios_core::{
        Atom, Definitions, Entrypoint, Free, Global, InductDecl, InductParam, Program, Telescope,
        Term, UniverseContext, Universes, test_support::*,
    },
    std::collections::BTreeMap,
};

/// A declaration admitted on a refused declaration's stated definition leaves the module refused.
///
/// `check_definition` returns before its define step and the walk defines the item anyway, at its declared type with its real body, so every later declaration is judged against exactly what a fully passing walk would have shown it. `lie : Absurd = ()` is refused and still stands at `Absurd`, so `truth : Absurd = lie` is admitted and the program closes with a closed inhabitant of an empty proposition whose verdict rests on what the refused declaration's judgment put in the environment. What stands between that and a proof of `False` is the walk's own list, which `curios-pipeline` fails the build on at its first entry: the walk reports the lying declaration and nothing else, so a declaration admitted on a refused one's definition admits no module.
///
/// The list is asserted whole, so the test says both halves at once: the lie is reported, and nothing that rests on it is — a walk that dropped the lie's verdict would admit this module at an empty proposition.
#[test]
fn a_declaration_admitted_on_a_refused_declarations_definition_leaves_the_module_refused() {
    let family = Global::Authored(Qualifier::from(["Absurd"]));
    let absurd = || Term::induct_type(family, Vec::<Term>::new(), Vec::<Term>::new());
    let lie = Global::Authored(Qualifier::from(["lie"]));
    let truth = Global::Authored(Qualifier::from(["truth"]));

    let definitions = declaring(
        Definitions {
            items: vec![
                authored(&lie, absurd(), Term::tuple(Vec::<Term>::new())),
                authored(&truth, absurd(), Term::instance(lie, Universes::default())),
            ]
            .into(),
            ..Definitions::default()
        },
        BTreeMap::from([(family, proposition(Vec::new()))]),
        [],
        [],
    );
    let program = Program {
        definitions,
        entrypoint: Entrypoint {
            body: Term::instance(truth, Universes::default()),
            type_: Some(absurd()),
        },
    };

    let verdicts = fixture_verdicts(&program, 1_000_000, &Globals::default(), SYNTAX);
    assert_eq!(
        verdicts
            .iter()
            .map(|verdict| verdict.name)
            .collect::<Vec<_>>(),
        vec![Some(lie)],
        "the walk must refuse the lying declaration, and nothing of what rests on it: {verdicts:?}",
    );
}

/// A family admitted by composing through the polarity vector a refused declaration's judgment filed leaves the module refused.
///
/// `accept` files each entry's polarity vector the moment `positivity_vectors` hands one back, and `check_induct_decl` runs after that, so a vector stands in the environment for an entry the walk goes on to refuse — `Shape(A : Type)` below declares `mk` twice, which `check_distinct_tags` refuses as `RepeatedTag`, while its parameter is strictly positive in both of them and the vector filed for it says so. `Knot`, declared after it, puts itself inside that parameter, and its own strict positivity composes through the filed vector and accepts; with no vector on record the parameter would answer `Mixed` and `Knot` would be refused. So the vector a refused declaration's judgment filed is what a later declaration's verdict rests on, and what stands between that and a family nothing established the positivity of is the walk's own list: the walk reports the family with the repeated tag and nothing else.
///
/// Mutation-checked against `Knot`'s acceptance resting on anything else: with `Shape`'s payload at `(A) -> Nat`, so that the filed vector reads the parameter `Neg`, the same module is refused twice over, `Knot` by `NotPositive { polarity: Neg }` at its `wrap` payload.
#[test]
fn a_family_admitted_through_a_refused_declarations_vector_leaves_the_module_refused() {
    let shape = Global::Authored(Qualifier::from(["Shape"]));
    let knot = Global::Authored(Qualifier::from(["Knot"]));
    let param = Free::local(710, Some("A"));
    let payload = Free::local(711, Some("a"));
    let held = Free::local(712, Some("x"));

    // induct Shape(A : Type) : Type | mk(a : A) | mk(a : A) end — one tag declared twice.
    let carried = || {
        (
            Atom::from("mk"),
            InductParam::new(
                Telescope::build(
                    [
                        (param, Term::type_ground()),
                        (payload, Term::free_var(&param)),
                    ],
                    Vec::new(),
                ),
                vec![Plicity::Implicit, Plicity::Explicit],
            ),
        )
    };
    let shape_declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::build([(param, Term::type_ground())], Telescope::done(())),
        constructors: vec![carried(), carried()].into(),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    // induct Knot : Type | wrap(x : Shape(Knot)) end
    let knot_declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("wrap"),
            InductParam::new(
                Telescope::build(
                    [(
                        held,
                        Term::induct_type(
                            shape,
                            [Term::induct_type(
                                knot,
                                Vec::<Term>::new(),
                                Vec::<Term>::new(),
                            )],
                            Vec::<Term>::new(),
                        ),
                    )],
                    Vec::new(),
                ),
                vec![Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let definitions = declaring(
        Definitions::default(),
        BTreeMap::from([(shape, shape_declaration), (knot, knot_declaration)]),
        [],
        [],
    );

    let verdicts = fixture_verdicts(&definitions, 1_000_000, &Globals::default(), SYNTAX);
    assert_eq!(
        verdicts
            .iter()
            .map(|verdict| (verdict.name, matches!(verdict.error, Error::RepeatedTag(_))))
            .collect::<Vec<_>>(),
        vec![(Some(shape), true)],
        "the walk must refuse the family with the repeated tag, and nothing that composed through its vector: {verdicts:?}",
    );
}
