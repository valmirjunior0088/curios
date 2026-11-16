//! A match on a proof, put to the certifier where the elaborator refuses first: the sort its goal is held to, a universe among the relevant ones, and the instance a closed one states.

use {
    super::test_support::*,
    crate::{test_support::*, *},
    curios_common::fill,
    curios_core::Reducer,
};

/// A goal whose sort its scrutinee's index decides is no goal. `Side : (Bool) -> Prop | left : (true) | right : (false)` is eliminated at `subject : Side(j)` into `match j | false => Nat | true => Unit end`: at `left`'s case the result is the proposition `Unit`, and at `right`'s it is `Nat`, the data a proposition of two constructors is not taken into. Typed at the open `j` its type is `match j | false => Type | true => Prop end`, which lands in no sort, so the match is refused before the guard or either arm is asked. `curios`'s `tests::rules::large_elimination` puts the same goal to the elaborator under this name.
#[test]
fn a_goal_whose_sort_its_index_decides_is_no_goal() {
    let mut certifier = certifier();
    declare(&mut certifier, "Unit", "", "Prop", &["u()"]);
    declare(
        &mut certifier,
        "Side",
        "i: #BoolType",
        "Prop",
        &["left(): (true)", "right(): (false)"],
    );
    let goal = fill!(
        "#{goal = #{goal = #{level = 1} Type} match j | false => Type | true => Prop end} match j | false => #NatType | true => ${unit} end",
        unit = family_at("Unit", "")
    );
    let [matched] = under(
        &mut certifier,
        &fill!(
            "j: #BoolType, subject: ${side}",
            side = family_at("Side", "j")
        ),
        &fill!(
            "${matched},",
            matched = eliminating(
                &goal,
                &["left() => #{node = Variant} /Unit/u()", "right() => 7"]
            )
        ),
    );

    assert!(matches!(
        infer(&mut certifier, &matched),
        Err(Error::NotAGoal { refusal, .. }) if matches!(*refusal, Error::NotASort(_))
    ));
}

/// Berardi's route, where proof irrelevance meets a proposition that decides something: `Two : (Nat) -> Prop | here : (0) | there : (0)` eliminated at `(i, s) => Prop`, answering the inhabited `Target(0)` at `here` and the empty `Absurd(0)` at `there`. Irrelevance makes `here` and `there` one value, so the two answers would be one proposition and a proof of the first a proof of the second. A universe is relevant, so the guard refuses the match into `Prop` as it refuses one into `Nat` (`tests::a_proposition_with_two_constructors_does_not_eliminate_into_a_type`). `curios`'s `tests::rules::large_elimination` puts the same elimination to the elaborator under this name.
#[test]
fn a_proposition_with_two_constructors_does_not_eliminate_into_a_universe() {
    let mut certifier = certifier();
    declare(
        &mut certifier,
        "Target",
        "i: #NatType",
        "Prop",
        &["only(): (0)"],
    );
    declare(&mut certifier, "Absurd", "i: #NatType", "Prop", &[]);
    declare(
        &mut certifier,
        "Two",
        "i: #NatType",
        "Prop",
        &["here(): (0)", "there(): (0)"],
    );
    let [matched] = under(
        &mut certifier,
        &fill!("subject: ${two}", two = family_at("Two", "0")),
        &fill!(
            "${matched},",
            matched = eliminating(
                "Prop",
                &[
                    &fill!("here() => ${target}", target = family_at("Target", "0")),
                    &fill!("there() => ${absurd}", absurd = family_at("Absurd", "0")),
                ]
            )
        ),
    );

    assert_eq!(
        infer(&mut certifier, &matched),
        Err(Error::LargeElimination(named("Two"))),
    );
}

/// The control: a proposition whose one constructor holds nothing is taken into `Prop` and typed there, so the refusal above is the second constructor's.
#[test]
fn a_proposition_with_one_bare_constructor_eliminates_into_a_universe() {
    let mut certifier = certifier();
    declare(
        &mut certifier,
        "Target",
        "i: #NatType",
        "Prop",
        &["only(): (0)"],
    );
    declare(&mut certifier, "One", "i: #NatType", "Prop", &["it(): (0)"]);
    let [matched] = under(
        &mut certifier,
        &fill!("subject: ${one}", one = family_at("One", "0")),
        &fill!(
            "${matched},",
            matched = eliminating(
                "Prop",
                &[&fill!(
                    "it() => ${target}",
                    target = family_at("Target", "0")
                )]
            )
        ),
    );

    assert_eq!(infer(&mut certifier, &matched), Ok(term("Prop")));
}

/// A closed match on a proof is its arm at the payload the instance it states says, asked nothing of the indices, since a closed proof of a family with one constructor is that constructor: over `same(0)`, a match stating `Same(1, 1)` reduces to `1`. The reading is true only of an instance typing held to the scrutinee's own type, and this one is refused where it is typed, as the open match stating another instance is (`tests::a_stated_instance_is_held_to_the_scrutinees_type`).
#[test]
fn a_closed_match_stating_an_instance_its_scrutinee_is_not_at_is_refused() {
    let mut certifier = certifier();
    declare(
        &mut certifier,
        "Same",
        "i: #NatType, j: #NatType",
        "Prop",
        &["same(z: #NatType): (z, z)"],
    );
    let matched = term(&fill!(
        "#{goal = #NatType, cases = {stated = ${stated}}} match #{node = Variant} /Same/same(0) | same(z) => z end",
        stated = family_at("Same", "1, 1")
    ));

    assert!(matches!(
        infer(&mut certifier, &matched),
        Err(Error::Mismatch { .. })
    ));
    assert_eq!(certifier.reduce(matched), Ok(term("1")));
}
