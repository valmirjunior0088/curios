//! A match on a proof, put to the kernel where the elaborator refuses first: the sort its motive is held to, a universe among the relevant ones, and the instance a closed one states.

use {
    super::test_support::*,
    crate::{Error, infer},
    curios_core::{Intrinsic, Level, Many, Reducer, Scope, Term},
    curios_utilities::Plicity,
};

/// A motive whose sort its index binder decides is no motive. `Side : (Bool) -> Prop | left : (true) | right : (false)` is eliminated at `(i, s) => match i : (c) => (match c : (_) => Type 1 | false => Type | true => Prop end) | false => Nat | true => Unit end`: at `left`'s case the result is the proposition `Unit`, and at `right`'s it is `Nat`, the data a proposition of two constructors is not taken into. Typed under its binder `i` it lands in no sort, so the match is refused before the guard or either arm is asked. `curios`'s `tests::board::large_elimination_tests` puts the same motive to the elaborator under this name.
#[test]
fn a_motive_whose_sort_its_index_decides_is_no_motive() {
    let mut kernel = kernel();
    let literal = |value: bool| Term::intrinsic(Intrinsic::Bool(value));

    let unit = declare_at(
        &mut kernel,
        "Unit",
        Term::prop(),
        Vec::new(),
        vec![nullary_at("u", Vec::new())],
    );
    let family = declare_at(
        &mut kernel,
        "Side",
        Term::prop(),
        vec![Term::intrinsic(Intrinsic::BoolType)],
        vec![
            nullary("left", literal(true)),
            nullary("right", literal(false)),
        ],
    );

    // The motive's index binder, as `eliminate` names it, and the binder of the sort it is read at.
    let (index, case) = (binder(51, "i"), binder(70, "c"));
    let sort_at = Term::bool_match(
        Term::free_var(&case),
        None,
        Term::type_at(Level::constant(1)),
        Term::type_ground(),
        Term::prop(),
    );
    let motive = Term::bool_match(
        Term::free_var(&index),
        Some(&case),
        sort_at,
        nat_type(),
        Term::induct_type(unit, Vec::<Term>::new(), Vec::<Term>::new()),
    );

    let term = eliminate(
        &mut kernel,
        &family,
        literal(false),
        motive,
        vec![
            (
                "left",
                Vec::new(),
                Term::variant(unit, Vec::<Term>::new(), "u", Vec::<Term>::new()),
            ),
            ("right", Vec::new(), nat(7)),
        ],
    );

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(Error::NotAMotive { refusal, .. }) if matches!(*refusal, Error::NotASort(_))
    ));
}

/// Berardi's route, where proof irrelevance meets a proposition that decides something: `Two : (Nat) -> Prop | here : (0) | there : (0)` eliminated at `(i, s) => Prop`, answering the inhabited `Target(0)` at `here` and the empty `Absurd(0)` at `there`. Irrelevance makes `here` and `there` one value, so the two answers would be one proposition and a proof of the first a proof of the second. A universe is relevant, so the guard refuses the match into `Prop` as it refuses one into `Nat` (`tests::a_proposition_with_two_constructors_does_not_eliminate_into_a_type`). `curios`'s `tests::board::large_elimination_tests` puts the same elimination to the elaborator under this name.
#[test]
fn a_proposition_with_two_constructors_does_not_eliminate_into_a_universe() {
    let mut kernel = kernel();

    let target = declare(
        &mut kernel,
        "Target",
        Term::prop(),
        vec![nullary("only", nat(0))],
    );
    let absurd = declare(&mut kernel, "Absurd", Term::prop(), Vec::new());
    let family = declare(
        &mut kernel,
        "Two",
        Term::prop(),
        vec![nullary("here", nat(0)), nullary("there", nat(0))],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        nat(0),
        Term::prop(),
        vec![
            (
                "here",
                Vec::new(),
                Term::induct_type(target, Vec::<Term>::new(), [nat(0)]),
            ),
            (
                "there",
                Vec::new(),
                Term::induct_type(absurd, Vec::<Term>::new(), [nat(0)]),
            ),
        ],
    );

    assert_eq!(
        infer(&mut kernel, &term),
        Err(Error::LargeElimination(family)),
    );
}

/// The control: a proposition whose one constructor holds nothing is taken into `Prop` and typed there, so the refusal above is the second constructor's.
#[test]
fn a_proposition_with_one_bare_constructor_eliminates_into_a_universe() {
    let mut kernel = kernel();

    let target = declare(
        &mut kernel,
        "Target",
        Term::prop(),
        vec![nullary("only", nat(0))],
    );
    let family = declare(
        &mut kernel,
        "One",
        Term::prop(),
        vec![nullary("it", nat(0))],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        nat(0),
        Term::prop(),
        vec![(
            "it",
            Vec::new(),
            Term::induct_type(target, Vec::<Term>::new(), [nat(0)]),
        )],
    );

    assert_eq!(infer(&mut kernel, &term), Ok(Term::prop()));
}

/// A closed match on a proof is its arm at the payload the instance it states says, asked nothing of the indices, since a closed proof of a family with one constructor is that constructor: over `same(0)`, a match stating `Same(1, 1)` reduces to `1`. The reading is true only of an instance typing held to the scrutinee's own type, and this one is refused where it is typed, as the open match stating another instance is (`tests::a_stated_instance_is_held_to_the_scrutinees_type`).
#[test]
fn a_closed_match_stating_an_instance_its_scrutinee_is_not_at_is_refused() {
    let mut kernel = kernel();
    let z = binder(0, "z");
    let family = declare_at(
        &mut kernel,
        "Same",
        Term::prop(),
        vec![nat_type(), nat_type()],
        vec![carrying_at(
            "same",
            z,
            nat_type(),
            vec![Term::free_var(&z), Term::free_var(&z)],
        )],
    );

    let (left, right, subject, arm_binder) = (
        binder(51, "i"),
        binder(52, "j"),
        binder(53, "s"),
        binder(10, "z"),
    );
    let term = Term::induct_match_scoped_marked(
        Term::variant(family, Vec::<Term>::new(), "same", [nat(0)]),
        Scope::close(Many(3), &[&left, &right, &subject], nat_type()),
        [(
            "same",
            vec![(Plicity::Explicit, arm_binder)],
            Term::free_var(&arm_binder),
        )],
        None,
    )
    .stating(Term::induct_type(
        family,
        Vec::<Term>::new(),
        [nat(1), nat(1)],
    ));

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(Error::Mismatch { .. })
    ));
    assert_eq!(kernel.reduce(term), Ok(nat(1)));
}
