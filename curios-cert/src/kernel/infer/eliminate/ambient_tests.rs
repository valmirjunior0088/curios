//! The arm rule at an ambient goal: the form's reason to exist, a lying goal, and its two preconditions.

use {
    super::test_support::*,
    crate::{Kernel, KernelError, infer},
    curios_analysis::fixture::SYNTAX,
    curios_core::{
        Atom, Carrier, Cases, Free, Global, InductArm, Intrinsic, Many, ReduceError, Scope, Term,
        Two, UniverseContext,
    },
    curios_utilities::Plicity,
};

/// `F : (Nat) -> Type` with `a : F(0)` and `b(m : Nat) : F(m + 1)`.
fn family(kernel: &mut Kernel) -> Global {
    let m = binder(70, "m");
    declare(
        kernel,
        "F",
        Term::type_ground(),
        vec![
            nullary("a", nat(0)),
            carrying("b", m.clone(), nat_type(), succ(Term::free_var(&m))),
        ],
    )
}

/// An opaque `Q : (n : Nat, x : F(n), p : P(n, x)) -> Type`: a type former whose last parameter is typed by the scrutinee and its index, so a goal stating it about a hypothesis can be written where that hypothesis stands and nowhere else.
fn dependent_family(kernel: &mut Kernel, name: Free, family: &Global, p: &Term) -> Term {
    let n = binder(94, "n");
    let x = binder(95, "x");
    let at = Term::induct_type(family.clone(), Vec::<Term>::new(), [Term::free_var(&n)]);
    kernel.declare(
        &name,
        &Term::func_type(
            [
                (n.clone(), nat_type()),
                (x.clone(), at),
                (
                    binder(96, "p"),
                    Term::apply(p.clone(), [Term::free_var(&n), Term::free_var(&x)]),
                ),
            ],
            Term::type_ground(),
        ),
        &UniverseContext::default(),
    );

    Term::free_var(&name)
}

/// The two arms of an elimination of `F`, each `body`, as `Cases::Induct` in declaration order.
fn arms(body: &Term) -> Cases {
    let m = binder(71, "m");
    Cases::Induct {
        cases: vec![
            (
                Atom::from("a"),
                InductArm::new(Scope::close(Many(0), &[], body.clone()), Vec::new()),
            ),
            (
                Atom::from("b"),
                InductArm::new(
                    Scope::close(Many(1), &[&m], body.clone()),
                    vec![Plicity::Explicit],
                ),
            ),
        ],
        default: None,
    }
}

/// The reason the form exists. `w : Q(n, x, h)` with `h : P(n, x)`: stated at the ambient goal, each arm inhabits `Q(n, x, h)` specialized — `Q(0, a(), h)` with `h` shadowed at `P(0, a())` — and `w` does so once shadowed itself. The family stating the same result, `(i, s) => Q(i, s, h)`, cannot be typed under its own binders, since `h : P(n, x)` is not a `P(i, s)`, which is what used to force a convoy.
#[test]
fn a_goal_over_a_hypothesis_typed_by_the_scrutinee_needs_no_family() {
    let mut kernel = kernel();
    let family = family(&mut kernel);
    let p = scrutinee_family(&mut kernel, binder(60, "P"), &family);
    let q = dependent_family(&mut kernel, binder(61, "Q"), &family, &p);

    let n = binder(62, "n");
    let x = binder(63, "x");
    let h = binder(64, "h");
    let w = binder(65, "w");
    kernel.assume(&n, &nat_type());
    kernel.assume(
        &x,
        &Term::induct_type(family.clone(), Vec::<Term>::new(), [Term::free_var(&n)]),
    );
    kernel.assume(
        &h,
        &Term::apply(p.clone(), [Term::free_var(&n), Term::free_var(&x)]),
    );
    let goal = Term::apply(
        q.clone(),
        [Term::free_var(&n), Term::free_var(&x), Term::free_var(&h)],
    );
    kernel.assume(&w, &goal);

    let ambient = Term::match_ambient(Term::free_var(&x), goal.clone(), arms(&Term::free_var(&w)));
    assert_eq!(
        infer(&mut kernel, &ambient).expect("the ambient goal is inhabited by each arm"),
        goal,
    );

    let i = binder(66, "i");
    let s = binder(67, "s");
    let family_form = Term::induct_match_scoped_marked(
        Term::free_var(&x),
        Scope::close(
            Many(2),
            &[&i, &s],
            Term::apply(
                q,
                [Term::free_var(&i), Term::free_var(&s), Term::free_var(&h)],
            ),
        ),
        [
            ("a", Vec::new(), Term::free_var(&w)),
            (
                "b",
                vec![(Plicity::Explicit, binder(71, "m"))],
                Term::free_var(&w),
            ),
        ],
        None,
    );
    assert!(
        matches!(
            infer(&mut kernel, &family_form),
            Err(KernelError::NotAMotive(_))
        ),
        "the family stating the same goal typed under its own binders",
    );
}

/// An ambient goal is a claim like any other: an arm that does not inhabit it is the mismatch it is.
#[test]
fn a_lying_ambient_goal_is_refused() {
    let mut kernel = kernel();
    let family = family(&mut kernel);
    let p = scrutinee_family(&mut kernel, binder(60, "P"), &family);

    let n = binder(62, "n");
    let x = binder(63, "x");
    kernel.assume(&n, &nat_type());
    kernel.assume(
        &x,
        &Term::induct_type(family.clone(), Vec::<Term>::new(), [Term::free_var(&n)]),
    );
    let goal = Term::apply(p, [Term::free_var(&n), Term::free_var(&x)]);

    let lying = Term::match_ambient(Term::free_var(&x), goal, arms(&nat(0)));
    assert!(matches!(
        infer(&mut kernel, &lying),
        Err(KernelError::Mismatch { .. })
    ));
}

/// An ambient result over an expression scrutinee — what substituting a `let` value or an argument into the variable form leaves behind — reads the goal's occurrences of that expression as the case: `P(0, f(n))` over `match f(n)` is `P(0, a())` in the `a` arm, which `h : P(0, f(n))` inhabits there through the case equation the arm records for `f(n)`, exactly as it would under a family. A goal that never names the expression is the same goal in every arm.
#[test]
fn an_ambient_result_over_an_expression_replaces_its_occurrences() {
    let mut kernel = kernel();
    let family = family(&mut kernel);
    let p = scrutinee_family(&mut kernel, binder(60, "P"), &family);

    // `f : (Nat) -> F(0)` applied to a parameter is an expression scrutinee typed at index `0`, so only the `a` arm is reachable, and `b`'s targets clash with the actual index.
    let f = binder(62, "f");
    let n = binder(63, "n");
    let at_zero = Term::induct_type(family.clone(), Vec::<Term>::new(), [nat(0)]);
    kernel.declare(
        &f,
        &Term::func_type([(binder(64, "k"), nat_type())], at_zero.clone()),
        &UniverseContext::default(),
    );
    kernel.assume(&n, &nat_type());
    let expression = Term::apply(Term::free_var(&f), [Term::free_var(&n)]);
    let h = binder(65, "h");
    let goal = Term::apply(p, [nat(0), expression.clone()]);
    kernel.assume(&h, &goal);

    // The goal names the expression, so the `a` arm is checked at `P(0, a())`, and the elimination's type is the goal as written.
    let naming = Term::match_ambient(expression.clone(), goal.clone(), arms(&Term::free_var(&h)));
    assert_eq!(
        infer(&mut kernel, &naming).expect("the occurrence stands for the case"),
        goal
    );

    // A goal that never names the expression is the same goal in every arm, and the elimination's type is that goal.
    let constant = Term::match_ambient(expression, nat_type(), arms(&nat(0)));
    assert_eq!(
        infer(&mut kernel, &constant).expect("a constant goal is inhabited by each arm"),
        nat_type()
    );
}

/// A fold's induction hypothesis is the fold at the tail, typed at the result at the tail, and an ambient goal has no tail to be taken at once the head is substituted away: refused, whatever the arms.
#[test]
fn an_ambient_fold_is_refused() {
    let mut kernel = kernel();
    let k = binder(80, "k");
    kernel.assume(&k, &nat_type());

    let fold = Term::match_ambient(
        Term::free_var(&k),
        nat_type(),
        Cases::FreeMonoid {
            carrier: Carrier::Nat {
                empty_case: nat(0),
                cons_case: Scope::close(Two, &[&binder(81, "pred"), &binder(82, "ih")], nat(0)),
            },
        },
    );
    assert!(matches!(
        infer(&mut kernel, &fold),
        Err(KernelError::AmbientFold(_))
    ));
}

/// A budget failure is not a malformed motive, on the way from a motive's type to its sort as on the way to the type. `T : U`, where `U` is a `Nat` eliminator counting a literal down to `Type`, so the type of what the result states is in hand at once and reading *it* as a sort is what the budget cannot afford. That read reported `NotAMotive` whatever it was refused for, which sent a reader to a motive with nothing wrong with it; an ambient goal and a family's motive go through the one read, so both are put to it. The control is the same elimination under a budget that affords the count, which is what says the refusal was the budget's.
#[test]
fn a_result_whose_sort_the_budget_cannot_reach_is_refused_for_the_budget() {
    let attempt = |budget: u64, ambient: bool| {
        let mut kernel = Kernel::new(budget, SYNTAX);
        kernel.set_local_floor(1_000);

        let (b, t, x) = (binder(80, "b"), binder(81, "T"), binder(82, "x"));
        let universe = Term::nat_match(
            nat(50_000),
            None,
            Term::type_ground(),
            Term::type_ground(),
            &binder(83, "pred"),
            &binder(84, "ih"),
            Term::free_var(&binder(84, "ih")),
        );
        kernel.assume(&b, &Term::intrinsic(Intrinsic::BoolType));
        kernel.assume(&t, &universe);
        kernel.assume(&x, &Term::free_var(&t));

        let elimination = match ambient {
            true => Term::match_ambient(
                Term::free_var(&b),
                Term::free_var(&t),
                Cases::Bool {
                    false_case: Term::free_var(&x),
                    true_case: Term::free_var(&x),
                },
            ),
            false => Term::bool_match_scoped(
                Term::free_var(&b),
                Scope::close(Many(1), &[&binder(85, "s")], Term::free_var(&t)),
                Term::free_var(&x),
                Term::free_var(&x),
            ),
        };

        infer(&mut kernel, &elimination)
    };

    for ambient in [false, true] {
        assert!(
            matches!(
                attempt(20_000, ambient),
                Err(KernelError::Reduce(ReduceError::Exhausted { .. }))
            ),
            "ambient = {ambient}: the sort read ran out of budget and was reported as something else",
        );
        attempt(1_000_000_000, ambient).unwrap_or_else(|error| {
            panic!("ambient = {ambient}: a budget that affords the count refused: {error:?}")
        });
    }
}
