//! The level alignment under a reading of the nominal families: which pairs it leaves out, and which spellings of a family it recognizes.

use {super::test_support::*, crate::*};

const STAR: Variance = Variance::Irrelevant;
const EQUAL: Variance = Variance::Invariant;

fn registry(families: &[(&str, InductDecl)]) -> Registry {
    Registry {
        inducts: families
            .iter()
            .map(|(path, declaration)| (nominal(path), declaration.clone()))
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
        levels.iter().copied().map(at).collect(),
    )
}

fn pair(this: u32, that: u32) -> (usize, Level, Level) {
    (0, at(this), at(that))
}

/// What the walk answers for two terms that differ in levels alone: the pairs it compares, and the ones it sets apart as irrelevant.
fn found(
    compared: Vec<(usize, Level, Level)>,
    irrelevant: Vec<(usize, Level, Level)>,
) -> Option<LevelDifferences> {
    Some(LevelDifferences {
        compared,
        irrelevant,
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

// Variance applies to a family applied in full and to nothing short of it: a former passed as a family, or applied to its parameters and waiting for an index, keeps every pair.
#[test]
fn a_bare_or_partly_applied_former_keeps_every_pair() {
    let families = registry(&[
        ("Wrap", shaped(1, 0, &[STAR])),
        ("Both", shaped(1, 1, &[STAR])),
    ]);
    let partly = |level| Term::apply(named("Both", &[level]), [nat()]);

    assert_eq!(
        named("Wrap", &[0]).level_differences(&named("Wrap", &[1]), |_| false, &families),
        found(vec![pair(0, 1)], Vec::new())
    );
    assert_eq!(
        partly(0).level_differences(&partly(1), |_| false, &families),
        found(vec![pair(0, 1)], Vec::new())
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
