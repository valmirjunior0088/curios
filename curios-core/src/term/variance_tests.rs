//! The level alignment under a reading of the nominal families and of the definitions: which pairs it leaves out, which it leaves to a curried head's type, and which spellings of a name it recognizes.

use {super::test_support::*, crate::*, std::collections::BTreeMap};

const STAR: Variance = Variance::Irrelevant;
const EQUAL: Variance = Variance::Invariant;

fn registry(families: &[(&str, InductDecl)]) -> Registry {
    Registry {
        inducts: families
            .iter()
            .map(|(path, declaration)| (nominal(path), declaration.clone()))
            .collect(),
        definitions: BTreeMap::new(),
    }
}

/// A registry of definitions alone, each with its vector and the arity one spine applies it in full by.
fn defining(definitions: &[(&str, &[Variance], usize)]) -> Registry {
    Registry {
        inducts: BTreeMap::new(),
        definitions: definitions
            .iter()
            .map(|(path, variances, arity)| (nominal(path), (variances.to_vec(), *arity)))
            .collect(),
    }
}

fn at(level: u32) -> Level {
    Level::constant(level)
}

fn nat() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

fn number(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}

/// The family `path`'s name at `levels`: the head an application of it stands on.
fn named(path: &str, levels: &[u32]) -> Term {
    Term::instance_of(
        &Free::from(&nominal(path)),
        levels.iter().copied().map(at).collect::<Universes>(),
    )
}

fn pair(this: u32, that: u32) -> (usize, Level, Level) {
    (0, at(this), at(that))
}

/// What the walk answers for two terms that differ in levels alone and hold no curried head: the pairs it compares, and the ones it sets apart as irrelevant.
fn found(
    compared: Vec<(usize, Level, Level)>,
    irrelevant: Vec<(usize, Level, Level)>,
) -> Option<LevelDifferences> {
    Some(LevelDifferences {
        compared,
        irrelevant,
        curried: Vec::new(),
    })
}

/// What the walk answers for two terms whose every differing pair sits at a curried head.
fn left_to_the_type(curried: Vec<(usize, Level, Level)>) -> Option<LevelDifferences> {
    Some(LevelDifferences {
        curried,
        ..LevelDifferences::default()
    })
}

// A node's own levels are its family's universe parameters in order, so the pair at an irrelevant position is set apart and the one at an invariant position is compared.
#[test]
fn an_irrelevant_position_of_a_node_is_no_difference() {
    let families = registry(&[("Two", shaped(1, 0, &[EQUAL, STAR]))]);
    let node = |levels: [u32; 2]| {
        Term::induct_type_at(nominal("Two"), levels.map(at), [nat()], Vec::<Term>::new())
    };

    assert_eq!(
        node([0, 0]).level_differences(&node([0, 1]), |_| false, &families),
        found(Vec::new(), vec![pair(0, 1)])
    );
    assert_eq!(
        node([0, 0]).level_differences(&node([1, 1]), |_| false, &families),
        found(vec![pair(0, 1)], vec![pair(0, 1)])
    );
    assert_eq!(
        node([0, 0]).level_differences(&node([0, 1]), |_| false, &()),
        found(vec![pair(0, 1)], Vec::new())
    );
}

// Only the node's own level is set apart: a level inside an argument is compared as any other.
#[test]
fn the_arguments_of_a_node_are_still_aligned() {
    let families = registry(&[("Wrap", shaped(1, 0, &[STAR]))]);
    let node = |level: u32, inner: u32| {
        Term::induct_type_at(
            nominal("Wrap"),
            [at(level)],
            [Term::type_at(at(inner))],
            Vec::<Term>::new(),
        )
    };

    assert_eq!(
        node(0, 3).level_differences(&node(1, 4), |_| false, &families),
        found(vec![pair(3, 4)], vec![pair(0, 1)])
    );
}

// A family's name applied in full is the node it builds, whichever spine carries its arguments: parameters alone, indices alone, or the two curried.
#[test]
fn a_familys_name_applied_in_full_is_read_as_its_node() {
    let families = registry(&[
        ("Wrap", shaped(1, 0, &[STAR])),
        ("Sized", shaped(0, 1, &[STAR])),
        ("Both", shaped(1, 1, &[STAR])),
    ]);
    let wrap = |level| Term::apply(named("Wrap", &[level]), [nat()]);
    let sized = |level| Term::apply(named("Sized", &[level]), [number(3)]);
    let both = |level| Term::apply(Term::apply(named("Both", &[level]), [nat()]), [number(3)]);

    for spelled in [&wrap as &dyn Fn(u32) -> Term, &sized, &both] {
        assert_eq!(
            spelled(0).level_differences(&spelled(1), |_| false, &families),
            found(Vec::new(), vec![pair(0, 1)])
        );
        assert_eq!(
            spelled(0).level_differences(&spelled(1), |_| false, &()),
            found(vec![pair(0, 1)], Vec::new())
        );
    }
}

// A family's name short of its full application, passed as a family or applied to its parameters and waiting for an index, is a function. Apart in a level the family is irrelevant in, two of them are one where eta applies both and two where every level is compared, so the pair is neither a difference nor none: it is left to the type. A level the family is invariant in is compared either way, and under `()` every level is.
#[test]
fn a_bare_or_partly_applied_family_name_is_left_to_its_type() {
    let families = registry(&[
        ("Wrap", shaped(1, 0, &[STAR])),
        ("Both", shaped(1, 1, &[STAR])),
        ("Held", shaped(1, 0, &[EQUAL])),
    ]);
    let partly = |level| Term::apply(named("Both", &[level]), [nat()]);

    assert_eq!(
        named("Wrap", &[0]).level_differences(&named("Wrap", &[1]), |_| false, &families),
        left_to_the_type(vec![pair(0, 1)])
    );
    assert_eq!(
        partly(0).level_differences(&partly(1), |_| false, &families),
        left_to_the_type(vec![pair(0, 1)])
    );
    assert_eq!(
        named("Held", &[0]).level_differences(&named("Held", &[1]), |_| false, &families),
        found(vec![pair(0, 1)], Vec::new())
    );
    assert_eq!(
        named("Wrap", &[0]).level_differences(&named("Wrap", &[1]), |_| false, &()),
        found(vec![pair(0, 1)], Vec::new())
    );
}

// A definition applied by the one spine its type asks for is its value at those arguments, so its instance's levels pair by its vector as a family's do, and its arguments are aligned after them. One that asks for no argument is in full as its bare name.
#[test]
fn a_definition_applied_in_full_is_read_by_its_vector() {
    let definitions = defining(&[("pick", &[STAR, EQUAL], 1), ("unit", &[STAR], 0)]);
    let pick = |levels: [u32; 2], inner: u32| {
        Term::apply(named("pick", &levels), [Term::type_at(at(inner))])
    };

    assert_eq!(
        pick([0, 0], 3).level_differences(&pick([1, 2], 4), |_| false, &definitions),
        found(vec![pair(0, 2), pair(3, 4)], vec![pair(0, 1)])
    );
    assert_eq!(
        named("unit", &[0]).level_differences(&named("unit", &[1]), |_| false, &definitions),
        found(Vec::new(), vec![pair(0, 1)])
    );
    assert_eq!(
        pick([0, 0], 3).level_differences(&pick([1, 0], 3), |_| false, &()),
        found(vec![pair(0, 1)], Vec::new())
    );
}

// A definition short of its one spine is a function, as a family's bare name is: the level its value does not hold is left to the type, and the one it holds is compared.
#[test]
fn a_definition_passed_bare_is_left_to_its_type() {
    let definitions = defining(&[("pick", &[STAR, EQUAL], 1)]);

    assert_eq!(
        named("pick", &[0, 0]).level_differences(&named("pick", &[1, 0]), |_| false, &definitions),
        left_to_the_type(vec![pair(0, 1)])
    );
    assert_eq!(
        named("pick", &[0, 0]).level_differences(&named("pick", &[1, 2]), |_| false, &definitions),
        Some(LevelDifferences {
            compared: vec![pair(0, 2)],
            irrelevant: Vec::new(),
            curried: vec![pair(0, 1)],
        })
    );
}

// A lambda's binder is annotated at a level eta never reads: at a function type both sides are applied to one binder first. So the pair in an annotation is left to the type, where the body's levels are compared as anywhere. Under `()` the annotation is compared with the rest, in reading order.
#[test]
fn a_lambdas_binder_annotation_is_left_to_its_type() {
    let families = registry(&[]);
    let carrier = Free::local(980, Some("A"));
    let lambda = |binder: u32, body: u32| {
        Term::func(
            [(carrier, Term::type_at(at(binder)))],
            Term::type_at(at(body)),
        )
    };

    assert_eq!(
        lambda(0, 3).level_differences(&lambda(1, 3), |_| false, &families),
        left_to_the_type(vec![pair(0, 1)])
    );
    assert_eq!(
        lambda(0, 3).level_differences(&lambda(1, 4), |_| false, &families),
        Some(LevelDifferences {
            compared: vec![pair(3, 4)],
            irrelevant: Vec::new(),
            curried: vec![pair(0, 1)],
        })
    );
    assert_eq!(
        lambda(0, 3).level_differences(&lambda(1, 4), |_| false, &()),
        found(vec![pair(0, 1), pair(3, 4)], Vec::new())
    );
}

// Two bare projections of one former differ in the level of the node the member builds and in its binders' annotations, which eta never compares: every pair is left to the type, whatever the family's vector says of the node. Under `()` the two groups are aligned level by level.
#[test]
fn two_bare_projections_of_a_former_are_left_to_their_type_whole() {
    for variance in [STAR, EQUAL] {
        let families = registry(&[("Wrap", shaped(1, 0, &[variance]))]);
        let differences = former("Wrap", 0)
            .level_differences(&former("Wrap", 1), |_| false, &families)
            .unwrap();

        assert!(differences.compared.is_empty() && differences.irrelevant.is_empty());
        assert!(
            !differences.curried.is_empty()
                && differences.curried.iter().all(|found| *found == pair(0, 1))
        );
    }

    assert!(
        former("Wrap", 0)
            .level_differences(&former("Wrap", 1), |_| false, &())
            .is_some_and(|found| !found.compared.is_empty() && found.curried.is_empty())
    );
}

// An `induct`'s former is a projection of its group, and reduction hands it over with the levels substituted into the whole group, binder annotations included. Applied in full it is read as the node its member builds; under `()` the two groups are aligned level by level.
#[test]
fn an_applied_former_is_read_as_the_node_it_builds() {
    let families = registry(&[("Wrap", shaped(1, 0, &[STAR]))]);
    let former = |level| former_applied("Wrap", level);

    assert_eq!(
        former(0).level_differences(&former(1), |_| false, &families),
        found(Vec::new(), vec![pair(0, 1)])
    );
    assert!(
        former(0)
            .level_differences(&former(1), |_| false, &())
            .is_some_and(|found| !found.compared.is_empty() && found.irrelevant.is_empty())
    );
}

// Two different families applied in full differ in more than levels.
#[test]
fn two_families_are_not_one_term_up_to_levels() {
    let families = registry(&[
        ("Wrap", shaped(1, 0, &[STAR])),
        ("Other", shaped(1, 0, &[STAR])),
    ]);
    let applied = |path: &str| Term::apply(named(path, &[0]), [nat()]);

    assert_eq!(
        applied("Wrap").level_differences(&applied("Other"), |_| false, &families),
        None
    );
}
