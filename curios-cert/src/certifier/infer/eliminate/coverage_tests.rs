//! Coverage: an absent arm excused by a clash or refused undecided, the catch-all and what it sees, and an arm of the wrong type or arity.

use {
    super::test_support::*,
    crate::{test_support::*, *},
    curios_common::fill,
    curios_core::Term,
};

/// The elimination over `Ix` at `index` whose goal is `Nat` and whose arms are `arms`, opened in `certifier` with `Ix` declared beside the hypotheses `binders`.
fn over_ix(certifier: &mut Certifier, binders: &str, index: &str, arms: &[&str]) -> Term {
    declare(certifier, "Ix", "i: #NatType", "Type", &[ZERO, SUCCESSOR]);
    let [matched] = under(
        certifier,
        &fill!(
            "${binders}subject: ${ix}",
            binders,
            ix = family_at("Ix", index)
        ),
        &fill!("${matched},", matched = eliminating("#NatType", arms)),
    );

    matched
}

/// An absent arm whose targets clash with the scrutinee's indices is legitimately absent: no value of that constructor can carry them. `s(p)` aims at `p + 1`, the scrutinee sits at `0`, and the `s` arm may be omitted.
#[test]
fn a_clashing_absent_arm_is_legitimately_absent() {
    let mut certifier = certifier();
    let matched = over_ix(&mut certifier, "", "0", &["z() => 1"]);

    assert_eq!(infer(&mut certifier, &matched), Ok(term("#NatType")));
}

/// A written arm whose targets clash with the scrutinee's indices is not typed, by the answer that excuses its absence: no value of that constructor carries them, so no reduction takes the arm. `s(p)` aims at `p + 1`, the scrutinee sits at `0`, and the `s` arm holds a `Bool` where the result is `Nat`. At a variable index nothing clashes, and the same arm is refused.
///
/// Mutation-checked: with a clashing arm typed as written, the first elimination is refused.
#[test]
fn a_clashing_written_arm_is_not_typed() {
    let over = |binders: &str, index: &str| {
        let mut certifier = certifier();
        let matched = over_ix(
            &mut certifier,
            binders,
            index,
            &["z() => 1", "s(p) => true"],
        );

        infer(&mut certifier, &matched)
    };

    assert_eq!(over("", "0"), Ok(term("#NatType")));
    assert!(over("b: #NatType, ", "b").is_err());
}

/// An absent arm the unifier cannot rule out is a refusal: at a *variable* index nothing clashes, so every constructor must have an arm or a catch-all. Undecided is not absent.
#[test]
fn an_undecided_absent_arm_is_refused() {
    let mut certifier = certifier();
    let matched = over_ix(&mut certifier, "b: #NatType, ", "b", &["s(p) => 0"]);

    assert!(matches!(
        infer(&mut certifier, &matched),
        Err(Error::MissingArm { .. }),
    ));
}

/// A catch-all stands for every constructor not enumerated, so nothing further is owed for the absent arms.
#[test]
fn a_catch_all_covers_absent_arms() {
    let mut certifier = certifier();
    let matched = over_ix(&mut certifier, "b: #NatType, ", "b", &["_ => 7"]);

    assert_eq!(infer(&mut certifier, &matched), Ok(term("#NatType")));
}

/// The terms `terms`, over `b : Nat`, `subject : Ix(b)` and the hypotheses `binders`, opened in `certifier` with `Ix` and `P : (n : Nat, x : Ix(n)) -> Type` declared.
fn over_a_scrutinee<const N: usize>(
    certifier: &mut Certifier,
    binders: &str,
    terms: &str,
) -> [Term; N] {
    declare(certifier, "Ix", "i: #NatType", "Type", &[ZERO, SUCCESSOR]);
    scrutinee_family(certifier, "Ix");

    under(
        certifier,
        &fill!(
            "b: #NatType, subject: ${ix}, ${binders}",
            ix = family_at("Ix", "b"),
            binders
        ),
        terms,
    )
}

/// A catch-all is checked at the goal's *scrutinee* instance — the instance the elimination is then given.
///
/// Every other arm has a case value of its own to be checked at, and a catch-all is the one arm that does not: it binds nothing and refines no index, so the only value it can stand for is the scrutinee. `infer` reads the elimination's type off its goal, so an arm checked at any other instance proves something other than what the elimination hands its caller.
///
/// Opening the scrutinee binder at the family *type* `Ix(b)` instead — a type substituted where a value of that type belongs — would refuse this hypothesis at `P(b, subject)` against an expectation of `P(b, Ix(b))`. That direction fails closed, because nothing well-typed inhabits the expectation it manufactures; the direction that does not is the certifier's, which would establish nothing whatever about the scrutinee for any catch-all it accepted. The source spelling is `match t : (q) => Eq()(q, q) | a() => Eq/refl() | _ => Eq/refl() end`. The fixed prelude does not ask the question — every catch-all there has a goal that ignores its scrutinee — so only a fixture does.
///
/// [`a_catch_all_at_another_value_of_the_family_is_refused`] is the paired control: closing this by not checking the catch-all at all would satisfy the test above and nothing else.
#[test]
fn a_catch_all_sees_its_own_scrutinee() {
    let mut certifier = certifier();
    let [matched, at_the_scrutinee] = over_a_scrutinee(
        &mut certifier,
        "w: /P(b, subject)",
        &fill!(
            "${matched}, /P(b, subject)",
            matched = eliminating("/P(b, subject)", &["_ => w"])
        ),
    );

    assert_eq!(infer(&mut certifier, &matched), Ok(at_the_scrutinee));
}

/// The control for [`a_catch_all_sees_its_own_scrutinee`]: a catch-all body at *another* inhabitant of the family is refused, and refused against the scrutinee's instance.
///
/// `other` sits at the same type as `subject` and is a different variable, so `P(b, other)` and `P(b, subject)` are distinct types with nothing to convert them. Asserting the expectation rather than merely the refusal is what makes this a control: a certifier that stopped checking the catch-all would accept this, and one that checked it at some other instance would name that instance here.
#[test]
fn a_catch_all_at_another_value_of_the_family_is_refused() {
    let mut certifier = certifier();
    let [matched, at_the_scrutinee] = over_a_scrutinee(
        &mut certifier,
        &fill!("other: ${ix}, w: /P(b, other)", ix = family_at("Ix", "b")),
        &fill!(
            "${matched}, /P(b, subject)",
            matched = eliminating("/P(b, subject)", &["_ => w"])
        ),
    );

    let Err(Error::Mismatch { expected, .. }) = infer(&mut certifier, &matched) else {
        panic!("expected the catch-all to be refused at the scrutinee's instance");
    };
    assert_eq!(*expected, at_the_scrutinee);
}

/// An arm is checked against the goal at *its own* constructor's index target, so a body of the wrong type is refused even where the elimination's overall type is fine.
#[test]
fn an_arm_body_of_the_wrong_type_is_refused() {
    let mut certifier = certifier();
    declare(
        &mut certifier,
        "Flag",
        "i: #NatType",
        "Type",
        &["on(): (0)", "off(): (0)"],
    );
    let [matched] = under(
        &mut certifier,
        &fill!("subject: ${flag}", flag = family_at("Flag", "0")),
        &fill!(
            "${matched},",
            matched = eliminating("#NatType", &["on() => 1", "off() => true"])
        ),
    );

    assert!(matches!(
        infer(&mut certifier, &matched),
        Err(Error::Mismatch { .. }),
    ));
}

/// An arm binding the wrong number of payload components is refused: the count is the constructor's, not the arm's to choose.
#[test]
fn an_arm_of_the_wrong_payload_arity_is_refused() {
    let mut certifier = certifier();
    declare(
        &mut certifier,
        "Wrapped",
        "i: #NatType",
        "Type",
        &["mk(value: #NatType): (0)"],
    );
    let [matched] = under(
        &mut certifier,
        &fill!("subject: ${wrapped}", wrapped = family_at("Wrapped", "0")),
        &fill!(
            "${matched},",
            matched = eliminating("#NatType", &["mk() => 1"])
        ),
    );

    assert!(matches!(
        infer(&mut certifier, &matched),
        Err(Error::Arity { .. }),
    ));
}
