//! Coverage: an absent arm excused by a clash or refused undecided, the catch-all and what it sees, and an arm of the wrong type or arity.

use {
    super::test_support::*,
    crate::{Error, Kernel, infer},
    curios_core::{Free, Intrinsic, Many, Scope, Term},
    curios_utilities::Plicity,
};

/// An absent arm whose targets clash with the scrutinee's indices is legitimately absent: no value of that constructor can carry them. `s(p)` aims at `p + 1`, the scrutinee sits at `0`, and the `s` arm may be omitted.
#[test]
fn a_clashing_absent_arm_is_legitimately_absent() {
    let mut kernel = kernel();
    let z_payload = binder(1, "p");

    let family = declare(
        &mut kernel,
        "Ix",
        Term::type_ground(),
        vec![
            nullary("z", nat(0)),
            carrying("s", z_payload, nat_type(), succ(Term::free_var(&z_payload))),
        ],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        nat(0),
        nat_type(),
        vec![("z", Vec::new(), nat(1))],
    );

    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
}

/// A written arm whose targets clash with the scrutinee's indices is not typed, by the answer that excuses its absence: no value of that constructor carries them, so no reduction takes the arm. `s(p)` aims at `p + 1`, the scrutinee sits at `0`, and the `s` arm holds a `Bool` where the result is `Nat`. At a variable index nothing clashes, and the same arm is refused.
///
/// Mutation-checked: with a clashing arm typed as written, the first elimination is refused.
#[test]
fn a_clashing_written_arm_is_not_typed() {
    let over = |index: fn(&mut Kernel) -> Term| {
        let mut kernel = kernel();
        let payload = binder(1, "p");
        let arm_binder = binder(10, "p");
        let family = declare(
            &mut kernel,
            "Ix",
            Term::type_ground(),
            vec![
                nullary("z", nat(0)),
                carrying("s", payload, nat_type(), succ(Term::free_var(&payload))),
            ],
        );
        let index = index(&mut kernel);
        let term = eliminate(
            &mut kernel,
            &family,
            index,
            nat_type(),
            vec![
                ("z", Vec::new(), nat(1)),
                (
                    "s",
                    vec![arm_binder],
                    Term::intrinsic(Intrinsic::Bool(true)),
                ),
            ],
        );

        infer(&mut kernel, &term)
    };

    assert_eq!(over(|_| nat(0)), Ok(nat_type()));
    assert!(
        over(|kernel| {
            let open = binder(0, "b");
            kernel.assume(&open, &nat_type());
            Term::free_var(&open)
        })
        .is_err()
    );
}

/// An absent arm the unifier cannot rule out is a refusal: at a *variable* index nothing clashes, so every constructor must have an arm or a catch-all. Undecided is not absent.
#[test]
fn an_undecided_absent_arm_is_refused() {
    let mut kernel = kernel();
    let b = binder(0, "b");
    let payload = binder(1, "p");
    let arm_binder = binder(10, "p");

    kernel.assume(&b, &nat_type());

    let family = declare(
        &mut kernel,
        "Ix",
        Term::type_ground(),
        vec![
            nullary("z", nat(0)),
            carrying("s", payload, nat_type(), succ(Term::free_var(&payload))),
        ],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        Term::free_var(&b),
        nat_type(),
        vec![("s", vec![arm_binder], nat(0))],
    );

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(Error::MissingArm { .. }),
    ));
}

/// A catch-all stands for every constructor not enumerated, so nothing further is owed for the absent arms.
#[test]
fn a_catch_all_covers_absent_arms() {
    let mut kernel = kernel();
    let b = binder(0, "b");
    let payload = binder(1, "p");
    let subject = binder(50, "subject");

    kernel.assume(&b, &nat_type());

    let family = declare(
        &mut kernel,
        "Ix",
        Term::type_ground(),
        vec![
            nullary("z", nat(0)),
            carrying("s", payload, nat_type(), succ(Term::free_var(&payload))),
        ],
    );

    kernel.assume(
        &subject,
        &Term::induct_type(family, Vec::<Term>::new(), [Term::free_var(&b)]),
    );
    let motive = Scope::close(Many(2), &[&binder(51, "i"), &binder(52, "s")], nat_type());
    let term = Term::induct_match_scoped_marked(
        Term::free_var(&subject),
        motive,
        Vec::<(&str, Vec<(Plicity, Free)>, Term)>::new(),
        Some(nat(7)),
    );

    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
}

/// A catch-all is checked at the motive's *scrutinee* instance — the instance the elimination is then given.
///
/// Every other arm has a case value of its own to be checked at, and a catch-all is the one arm that does not: it binds nothing and refines no index, so the only value it can stand for is the scrutinee. `infer` reads the elimination's type off `motive.open([indices, head])`, so an arm checked at any other instance proves something other than what the elimination hands its caller.
///
/// Opening the scrutinee binder at the family *type* `Ix(b)` instead — a type substituted where a value of that type belongs — would refuse this hypothesis at `P(b, subject)` against an expectation of `P(b, Ix(b))`. That direction fails closed, because nothing well-typed inhabits the expectation it manufactures; the direction that does not is the certifier's, which would establish nothing whatever about the scrutinee for any catch-all it accepted. The source spelling is `match t : (q) => Eq()(q, q) | a() => Eq/refl() | _ => Eq/refl() end`. The fixed prelude does not ask the question — every catch-all there has a motive that ignores its scrutinee — so only a fixture does.
///
/// [`a_catch_all_at_another_value_of_the_family_is_refused`] is the paired control: closing this by not checking the catch-all at all would satisfy the test above and nothing else.
#[test]
fn a_catch_all_sees_its_own_scrutinee() {
    let mut kernel = kernel();
    let b = binder(0, "b");
    let payload = binder(1, "p");
    let subject = binder(50, "subject");
    let hypothesis = binder(3, "w");

    kernel.assume(&b, &nat_type());

    let family = declare(
        &mut kernel,
        "Ix",
        Term::type_ground(),
        vec![
            nullary("z", nat(0)),
            carrying("s", payload, nat_type(), succ(Term::free_var(&payload))),
        ],
    );
    let p = scrutinee_family(&mut kernel, binder(2, "P"), &family);

    kernel.assume(
        &subject,
        &Term::induct_type(family, Vec::<Term>::new(), [Term::free_var(&b)]),
    );
    let at_the_scrutinee = Term::apply(p.clone(), [Term::free_var(&b), Term::free_var(&subject)]);
    kernel.assume(&hypothesis, &at_the_scrutinee);

    let term = Term::induct_match_scoped_marked(
        Term::free_var(&subject),
        Scope::close(
            Many(2),
            &[&binder(51, "i"), &binder(52, "s")],
            Term::apply(
                p,
                [
                    Term::free_var(&binder(51, "i")),
                    Term::free_var(&binder(52, "s")),
                ],
            ),
        ),
        Vec::<(&str, Vec<(Plicity, Free)>, Term)>::new(),
        Some(Term::free_var(&hypothesis)),
    );

    assert_eq!(infer(&mut kernel, &term), Ok(at_the_scrutinee));
}

/// The control for [`a_catch_all_sees_its_own_scrutinee`]: a catch-all body at *another* inhabitant of the family is refused, and refused against the scrutinee's instance.
///
/// `other` sits at the same type as `subject` and is a different variable, so `P(b, other)` and `P(b, subject)` are distinct types with nothing to convert them. Asserting the expectation rather than merely the refusal is what makes this a control: a kernel that stopped checking the catch-all would accept this, and one that checked it at some other instance would name that instance here.
#[test]
fn a_catch_all_at_another_value_of_the_family_is_refused() {
    let mut kernel = kernel();
    let b = binder(0, "b");
    let payload = binder(1, "p");
    let subject = binder(50, "subject");
    let other = binder(53, "other");
    let hypothesis = binder(3, "w");

    kernel.assume(&b, &nat_type());

    let family = declare(
        &mut kernel,
        "Ix",
        Term::type_ground(),
        vec![
            nullary("z", nat(0)),
            carrying("s", payload, nat_type(), succ(Term::free_var(&payload))),
        ],
    );
    let p = scrutinee_family(&mut kernel, binder(2, "P"), &family);

    let family_at_b = Term::induct_type(family, Vec::<Term>::new(), [Term::free_var(&b)]);
    kernel.assume(&subject, &family_at_b);
    kernel.assume(&other, &family_at_b);
    kernel.assume(
        &hypothesis,
        &Term::apply(p.clone(), [Term::free_var(&b), Term::free_var(&other)]),
    );

    let term = Term::induct_match_scoped_marked(
        Term::free_var(&subject),
        Scope::close(
            Many(2),
            &[&binder(51, "i"), &binder(52, "s")],
            Term::apply(
                p.clone(),
                [
                    Term::free_var(&binder(51, "i")),
                    Term::free_var(&binder(52, "s")),
                ],
            ),
        ),
        Vec::<(&str, Vec<(Plicity, Free)>, Term)>::new(),
        Some(Term::free_var(&hypothesis)),
    );

    let Err(Error::Mismatch { expected, .. }) = infer(&mut kernel, &term) else {
        panic!("expected the catch-all to be refused at the scrutinee's instance");
    };
    assert_eq!(
        *expected,
        Term::apply(p, [Term::free_var(&b), Term::free_var(&subject)]),
    );
}

/// An arm is checked against the motive at *its own* constructor's index target, so a body of the wrong type is refused even where the elimination's overall type is fine.
#[test]
fn an_arm_body_of_the_wrong_type_is_refused() {
    let mut kernel = kernel();

    let family = declare(
        &mut kernel,
        "Flag",
        Term::type_ground(),
        vec![nullary("on", nat(0)), nullary("off", nat(0))],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        nat(0),
        nat_type(),
        vec![
            ("on", Vec::new(), nat(1)),
            ("off", Vec::new(), Term::intrinsic(Intrinsic::Bool(true))),
        ],
    );

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(Error::Mismatch { .. }),
    ));
}

/// An arm binding the wrong number of payload components is refused: the count is the constructor's, not the arm's to choose.
#[test]
fn an_arm_of_the_wrong_payload_arity_is_refused() {
    let mut kernel = kernel();
    let value = binder(0, "value");

    let family = declare(
        &mut kernel,
        "Wrapped",
        Term::type_ground(),
        vec![carrying("mk", value, nat_type(), nat(0))],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        nat(0),
        nat_type(),
        vec![("mk", Vec::new(), nat(1))],
    );

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(Error::Arity { .. }),
    ));
}
