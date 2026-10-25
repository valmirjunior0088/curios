//! A match on a proof, put to the kernel where the elaborator refuses first: the sort its motive is held to, and the instance a closed one states.

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
