//! Context specialization under an arm: the index equations it forces, the clashes that excuse it, and a match over indices or instances that stands.

use {
    super::test_support::*,
    crate::{Error, Kernel, infer},
    curios_core::{Free, Global, Intrinsic, Reducer, Term, UniverseContext},
};

/// The arm rule specializes the *context*, not just the motive. Matching `s(p) : Ix(p + 1)` against a scrutinee typed `Ix(b)` forces `b ≡ p + 1`, so a hypothesis at `P(b)` inhabits the arm's expectation `P(p + 1)`.
///
/// This is the shape transitivity over an inductive order needs, where matching the first proof pins the index the second proof's type names: the equation refines an *outer* binder, which no motive opening reaches and no arm binder owns.
#[test]
fn a_forced_index_equation_refines_an_outer_hypothesis() {
    let mut kernel = kernel();
    let b = binder(0, "b");
    let payload = binder(1, "p");
    let w = binder(3, "w");
    let arm_binder = binder(10, "p");

    let p = opaque_family(&mut kernel, binder(2, "P"));
    kernel.assume(&b, &nat_type());
    kernel.assume(&w, &Term::apply(p.clone(), [Term::free_var(&b)]));

    let family = declare(
        &mut kernel,
        "Ix",
        Term::type_ground(),
        vec![carrying(
            "s",
            payload,
            nat_type(),
            succ(Term::free_var(&payload)),
        )],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        Term::free_var(&b),
        Term::apply(p.clone(), [Term::free_var(&binder(51, "i"))]),
        vec![("s", vec![arm_binder], Term::free_var(&w))],
    );

    assert_eq!(
        infer(&mut kernel, &term),
        Ok(Term::apply(p, [Term::free_var(&b)])),
    );
}

/// A forced equation whose solution mentions a refinable variable is refused rather than substituted: `Cyc(b)` matched against a case aimed at `b + 1` would solve `b := b + 1`, and the occurs check must leave the arm unspecialized instead. The hypothesis therefore stays at `P(b)` and fails the expectation `P(b + 1)` — a refusal, never a silent cycle.
#[test]
fn a_cyclic_index_equation_refines_nothing() {
    let mut kernel = kernel();
    let b = binder(0, "b");
    let w = binder(3, "w");

    let p = opaque_family(&mut kernel, binder(2, "P"));
    kernel.assume(&b, &nat_type());
    kernel.assume(&w, &Term::apply(p.clone(), [Term::free_var(&b)]));

    let family = declare(
        &mut kernel,
        "Cyc",
        Term::type_ground(),
        vec![nullary("mk", succ(Term::free_var(&b)))],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        Term::free_var(&b),
        Term::apply(p, [Term::free_var(&binder(51, "i"))]),
        vec![("mk", Vec::new(), Term::free_var(&w))],
    );

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(Error::Mismatch { .. }),
    ));
}

/// A variable `Bool` scrutinee stands refined to the literal in each arm — the zero-index instance of the same specialization.
#[test]
fn a_bool_arm_sees_its_scrutinee_at_the_literal() {
    let mut kernel = kernel();
    let h = binder(0, "h");
    let w = binder(3, "w");
    let motive_binder = binder(51, "x");

    let p = binder(2, "P");
    kernel
        .declare(
            &p,
            &Term::func_type(
                [(binder(90, "x"), Term::intrinsic(Intrinsic::BoolType))],
                Term::type_ground(),
            ),
            &UniverseContext::default(),
        )
        .expect("a name is declared once");
    let p = Term::free_var(&p);

    kernel.assume(&h, &Term::intrinsic(Intrinsic::BoolType));
    kernel.assume(&w, &Term::apply(p.clone(), [Term::free_var(&h)]));

    let term = Term::bool_match(
        Term::free_var(&h),
        Some(&motive_binder),
        Term::apply(p.clone(), [Term::free_var(&motive_binder)]),
        Term::free_var(&w),
        Term::free_var(&w),
    );

    assert_eq!(
        infer(&mut kernel, &term),
        Ok(Term::apply(p, [Term::free_var(&h)])),
    );
}

/// Two forcings of one binder that clash make the case impossible, so the arm may be omitted — and at a proposition they do not, so it may not.
///
/// This is the clash rule reaching the kernel through the walk that certifies a module, rather than through the shared unifier alone. The target is *non-linear*: `same(@z) : (z, z)` is `Eq`'s shape, so neither index position clashes on its own — each solves `z`, here to `a()` and to `b()` — and what decides the case is whether those two forcings can be one value.
///
/// The pair differs in nothing but the sort of the *carrier the forcings inhabit* — not of the family being eliminated, which stays a proposition as `Eq` is. That is the distinction the rule turns on and the one a fixture most easily gets wrong: what licenses telling two constructors apart is their own family's sort. At a relevant carrier they are values a program distinguishes, so `Same(a(), b())` is empty and the omission stands. At a `Prop` carrier irrelevance identifies them, the deletion rule reconciles the pair before a clash is ever asked for, and the omitted arm is one the elimination cannot prove absent — the vacuous-elimination route, which is why that half is the half worth holding.
#[test]
fn a_clash_between_two_forcings_of_one_binder_excuses_the_arm() {
    for (label, carrier_sort, omission_stands) in [
        (
            "a relevant carrier, whose two constructors a program tells apart",
            Term::type_ground(),
            true,
        ),
        (
            "a proposition, whose two inhabitants irrelevance identifies",
            Term::prop(),
            false,
        ),
    ] {
        let mut kernel = kernel();
        let carrier = declare_at(
            &mut kernel,
            "Bit",
            carrier_sort,
            Vec::new(),
            vec![nullary_at("a", Vec::new()), nullary_at("b", Vec::new())],
        );
        let bit = Term::induct_type(carrier, Vec::<Term>::new(), Vec::<Term>::new());
        let inhabitant =
            |tag: &str| Term::variant(carrier, Vec::<Term>::new(), tag, Vec::<Term>::new());

        let value = binder(60, "z");
        let family = declare_at(
            &mut kernel,
            "Same",
            Term::prop(),
            vec![bit.clone(), bit.clone()],
            vec![carrying_at(
                "same",
                value,
                bit.clone(),
                vec![Term::free_var(&value), Term::free_var(&value)],
            )],
        );

        // No arm for `same`, at two indices it cannot both equal.
        let term = eliminate_at(
            &mut kernel,
            &family,
            vec![inhabitant("a"), inhabitant("b")],
            nat_type(),
            Vec::new(),
        );

        let verdict = infer(&mut kernel, &term);
        match omission_stands {
            true => assert!(verdict.is_ok(), "{label}: the omitted arm was demanded"),
            false => assert!(
                matches!(verdict, Err(Error::MissingArm { .. })),
                "{label}: the omitted arm was excused"
            ),
        }
    }
}

/// One variable of the scrutinee's indices forced to two values that clash makes the case impossible as two forcings of one binder do, so the arm may be omitted — and at a proposition they do not, so it may not.
///
/// The mirror of the fixture above. The targets are closed, `apart() : (a(), b())`, and it is the scrutinee's indices that name one variable twice, `(x, x)`. The unifier's first direction decides nothing of it, a variable being what may yet be either target; the second solves `x` to each target and asks whether the two can be one value. At a relevant carrier they cannot, so no value of `Apart(x, x)` is `apart()` and the omission stands. At a `Prop` carrier they convert, `apart()` inhabits `Apart(x, x)` for every `x`, and the omitted arm is one the elimination cannot prove absent.
///
/// Mutation-checked: with absence asked of the first direction alone, the relevant half demands the arm.
#[test]
fn an_outer_variable_forced_two_ways_excuses_the_arm() {
    for (label, carrier_sort, omission_stands) in [
        (
            "a relevant carrier, whose two constructors a program tells apart",
            Term::type_ground(),
            true,
        ),
        (
            "a proposition, whose two inhabitants irrelevance identifies",
            Term::prop(),
            false,
        ),
    ] {
        let mut kernel = kernel();
        let carrier = declare_at(
            &mut kernel,
            "Bit",
            carrier_sort,
            Vec::new(),
            vec![nullary_at("a", Vec::new()), nullary_at("b", Vec::new())],
        );
        let bit = Term::induct_type(carrier, Vec::<Term>::new(), Vec::<Term>::new());
        let inhabitant =
            |tag: &str| Term::variant(carrier, Vec::<Term>::new(), tag, Vec::<Term>::new());

        let family = declare_at(
            &mut kernel,
            "Apart",
            Term::type_ground(),
            vec![bit.clone(), bit.clone()],
            vec![nullary_at("apart", vec![inhabitant("a"), inhabitant("b")])],
        );

        // No arm for `apart`, at two indices that are one variable.
        let x = binder(61, "x");
        kernel.assume(&x, &bit);
        let term = eliminate_at(
            &mut kernel,
            &family,
            vec![Term::free_var(&x), Term::free_var(&x)],
            nat_type(),
            Vec::new(),
        );

        let verdict = infer(&mut kernel, &term);
        match omission_stands {
            true => assert!(verdict.is_ok(), "{label}: the omitted arm was demanded"),
            false => assert!(
                matches!(verdict, Err(Error::MissingArm { .. })),
                "{label}: the omitted arm was excused"
            ),
        }
    }
}

/// `Same : (Nat, Nat) -> Prop | same(z : Nat) : (z, z)`, the shape of an equality: one constructor, its payload both of its indices.
fn same(kernel: &mut Kernel) -> Global {
    let z = binder(0, "z");

    declare_at(
        kernel,
        "Same",
        Term::prop(),
        vec![nat_type(), nat_type()],
        vec![carrying_at(
            "same",
            z,
            nat_type(),
            vec![Term::free_var(&z), Term::free_var(&z)],
        )],
    )
}

/// `Same(left, right)`, the instance a match over it states.
fn same_at(family: &Global, left: &Free, right: &Free) -> Term {
    Term::induct_type(
        *family,
        Vec::<Term>::new(),
        vec![Term::free_var(left), Term::free_var(right)],
    )
}

/// A match on a proposition whose indices pin its payload fires with its scrutinee unreduced, binding the payload to the index that states it: the scrutinee is a variable, which no reduction takes to a constructor, and its type's two indices are one, which is everything `same` would say.
#[test]
fn an_arm_that_names_a_pinned_payload_is_bound_to_the_index() {
    let mut kernel = kernel();
    let family = same(&mut kernel);
    let n = binder(60, "n");
    let arm_binder = binder(10, "z");
    kernel.assume(&n, &nat_type());

    let term = eliminate_at(
        &mut kernel,
        &family,
        vec![Term::free_var(&n), Term::free_var(&n)],
        nat_type(),
        vec![("same", vec![arm_binder], Term::free_var(&arm_binder))],
    )
    .stating(same_at(&family, &n, &n));

    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
    assert_eq!(kernel.reduce(term), Ok(Term::free_var(&n)));
}

/// An arm that names no payload fires the same way, with nothing to bind.
#[test]
fn an_arm_written_over_the_index_fires_with_its_proof_unreduced() {
    let mut kernel = kernel();
    let family = same(&mut kernel);
    let n = binder(60, "n");
    kernel.assume(&n, &nat_type());

    let term = eliminate_at(
        &mut kernel,
        &family,
        vec![Term::free_var(&n), Term::free_var(&n)],
        nat_type(),
        vec![("same", vec![binder(10, "z")], Term::free_var(&n))],
    )
    .stating(same_at(&family, &n, &n));

    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
    assert_eq!(kernel.reduce(term), Ok(Term::free_var(&n)));
}

/// The control: at two indices nothing holds one, the match stands. The scrutinee is the same variable, so a rule that fired on the shape of the match alone would answer here too.
#[test]
fn a_match_over_indices_that_differ_stands() {
    let mut kernel = kernel();
    let family = same(&mut kernel);
    let (n, m) = (binder(60, "n"), binder(61, "m"));
    kernel.assume(&n, &nat_type());
    kernel.assume(&m, &nat_type());

    let term = eliminate_at(
        &mut kernel,
        &family,
        vec![Term::free_var(&n), Term::free_var(&m)],
        nat_type(),
        vec![("same", vec![binder(10, "z")], Term::free_var(&n))],
    )
    .stating(same_at(&family, &n, &m));

    assert_eq!(kernel.reduce(term.clone()), Ok(term));
}

/// What a match fires on is the instance it states, and a match that states none stands: the scrutinee is assumed at `Same(n, n)`, whose indices pin the payload, and no rule looks that up.
#[test]
fn a_match_that_states_no_instance_stands() {
    let mut kernel = kernel();
    let family = same(&mut kernel);
    let n = binder(60, "n");
    let arm_binder = binder(10, "z");
    kernel.assume(&n, &nat_type());

    let term = eliminate_at(
        &mut kernel,
        &family,
        vec![Term::free_var(&n), Term::free_var(&n)],
        nat_type(),
        vec![("same", vec![arm_binder], Term::free_var(&arm_binder))],
    );

    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
    assert_eq!(kernel.reduce(term.clone()), Ok(term));
}

/// The instance a match states is held to the type its scrutinee is inferred at: a match over a scrutinee at `Same(n, m)` that states `Same(n, n)` is refused where it is typed. Reduced where nothing typed it, it is its arm at the payload it states, the step every reader of the term takes alike, with the scrutinee's own type read by none.
///
/// Mutation-checked: with the stated instance left unheld, the match is typed at `Nat`.
#[test]
fn a_stated_instance_is_held_to_the_scrutinees_type() {
    let mut kernel = kernel();
    let family = same(&mut kernel);
    let (n, m) = (binder(60, "n"), binder(61, "m"));
    let arm_binder = binder(10, "z");
    kernel.assume(&n, &nat_type());
    kernel.assume(&m, &nat_type());

    let term = eliminate_at(
        &mut kernel,
        &family,
        vec![Term::free_var(&n), Term::free_var(&m)],
        nat_type(),
        vec![("same", vec![arm_binder], Term::free_var(&arm_binder))],
    )
    .stating(same_at(&family, &n, &n));

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(Error::Mismatch { .. })
    ));
    assert_eq!(kernel.reduce(term), Ok(Term::free_var(&n)));
}
