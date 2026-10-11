//! The arm rule at an ambient goal: the form's reason to exist, a lying goal, and its two preconditions.

use {
    super::test_support::*,
    crate::{Error, Kernel, infer},
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Atom, Carrier, Cases, Free, Global, InductArm, Intrinsic, Many, ReduceError, Scope, Term,
        Two, UniverseContext, test_support::match_ambient,
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
            carrying("b", m, nat_type(), succ(Term::free_var(&m))),
        ],
    )
}

/// An opaque `Q : (n : Nat, x : F(n), p : P(n, x)) -> Type`: a type former whose last parameter is typed by the scrutinee and its index, so a goal stating it about a hypothesis can be written where that hypothesis stands and nowhere else.
fn dependent_family(kernel: &mut Kernel, name: Free, family: &Global, p: &Term) -> Term {
    let n = binder(94, "n");
    let x = binder(95, "x");
    let at = Term::induct_type(*family, Vec::<Term>::new(), [Term::free_var(&n)]);
    kernel
        .declare(
            &name,
            &Term::func_type(
                [
                    (n, nat_type()),
                    (x, at),
                    (
                        binder(96, "p"),
                        Term::apply(p.clone(), [Term::free_var(&n), Term::free_var(&x)]),
                    ),
                ],
                Term::type_ground(),
            ),
            &UniverseContext::default(),
        )
        .expect("a name is declared once");

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
        stated: None,
    }
}

/// The reason the form exists. `w : Q(n, x, h)` with `h : P(n, x)`: stated at the ambient goal, each arm inhabits `Q(n, x, h)` specialized — `Q(0, a(), h)` with `h` shadowed at `P(0, a())` — and `w` does so once shadowed itself. The family stating the same result, `(i, s) => Q(i, s, h)`, cannot be typed under its own binders, since `h : P(n, x)` is not a `P(i, s)`, which is what would otherwise force a convoy.
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
        &Term::induct_type(family, Vec::<Term>::new(), [Term::free_var(&n)]),
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

    let ambient = match_ambient(Term::free_var(&x), goal.clone(), arms(&Term::free_var(&w)));
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
            Err(Error::NotAMotive { .. })
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
        &Term::induct_type(family, Vec::<Term>::new(), [Term::free_var(&n)]),
    );
    let goal = Term::apply(p, [Term::free_var(&n), Term::free_var(&x)]);

    let lying = match_ambient(Term::free_var(&x), goal, arms(&nat(0)));
    assert!(matches!(
        infer(&mut kernel, &lying),
        Err(Error::Mismatch { .. })
    ));
}

/// An ambient result over an expression scrutinee — what a `let`-bound scrutinee read by value, or an argument substituted into the variable form, leaves behind — reads the goal's occurrences of that expression as the case: `P(0, f(n))` over `match f(n)` is `P(0, a())` in the `a` arm, which `h : P(0, f(n))` inhabits there through the case equation the arm records for `f(n)`, exactly as it would under a family. A goal that never names the expression is the same goal in every arm.
#[test]
fn an_ambient_result_over_an_expression_replaces_its_occurrences() {
    let mut kernel = kernel();
    let family = family(&mut kernel);
    let p = scrutinee_family(&mut kernel, binder(60, "P"), &family);

    // `f : (Nat) -> F(0)` applied to a parameter is an expression scrutinee typed at index `0`, so only the `a` arm is reachable, and `b`'s targets clash with the actual index.
    let f = binder(62, "f");
    let n = binder(63, "n");
    let at_zero = Term::induct_type(family, Vec::<Term>::new(), [nat(0)]);
    kernel
        .declare(
            &f,
            &Term::func_type([(binder(64, "k"), nat_type())], at_zero.clone()),
            &UniverseContext::default(),
        )
        .expect("a name is declared once");
    kernel.assume(&n, &nat_type());
    let expression = Term::apply(Term::free_var(&f), [Term::free_var(&n)]);
    let h = binder(65, "h");
    let goal = Term::apply(p, [nat(0), expression.clone()]);
    kernel.assume(&h, &goal);

    // The goal names the expression, so the `a` arm is checked at `P(0, a())`, and the elimination's type is the goal as written.
    let naming = match_ambient(expression.clone(), goal.clone(), arms(&Term::free_var(&h)));
    assert_eq!(
        infer(&mut kernel, &naming).expect("the occurrence stands for the case"),
        goal
    );

    // A goal that never names the expression is the same goal in every arm, and the elimination's type is that goal.
    let constant = match_ambient(expression, nat_type(), arms(&nat(0)));
    assert_eq!(
        infer(&mut kernel, &constant).expect("a constant goal is inhabited by each arm"),
        nat_type()
    );
}

/// A fold's induction hypothesis is the fold at the tail, typed at the result at the tail, and an ambient goal has no tail to be taken at once the head is substituted away: a fold whose arm reads its hypothesis is refused at one.
#[test]
fn an_ambient_fold_that_reads_its_hypothesis_is_refused() {
    let mut kernel = kernel();
    let k = binder(80, "k");
    kernel.assume(&k, &nat_type());

    let ih = binder(82, "ih");
    let fold = match_ambient(
        Term::free_var(&k),
        nat_type(),
        Cases::FreeMonoid {
            carrier: Carrier::Nat {
                empty_case: nat(0),
                cons_case: Scope::close(Two, &[&binder(81, "pred"), &ih], Term::free_var(&ih)),
            },
        },
    );
    assert!(matches!(
        infer(&mut kernel, &fold),
        Err(Error::AmbientFold(_))
    ));
}

/// A case split reads no hypothesis, so nothing needs the type an ambient goal cannot give one: its reduct never contains the fold at the tail, and each arm is checked at its case value as a `Bool` arm is. The same match, its arms each inhabiting the goal, is inferred at the goal.
#[test]
fn an_ambient_case_split_is_checked_at_each_case() {
    let mut kernel = kernel();
    let k = binder(80, "k");
    kernel.assume(&k, &nat_type());

    let split = match_ambient(
        Term::free_var(&k),
        nat_type(),
        Cases::FreeMonoid {
            carrier: Carrier::Nat {
                empty_case: nat(0),
                cons_case: Scope::close(
                    Two,
                    &[&binder(81, "pred"), &binder(82, "ih")],
                    Term::free_var(&binder(81, "pred")),
                ),
            },
        },
    );
    assert_eq!(
        infer(&mut kernel, &split).expect("each arm inhabits the goal at its case"),
        nat_type()
    );
}

/// The split's arms are still held to the goal: an identity arm that does not inhabit it is refused as the mismatch it is.
#[test]
fn an_ambient_case_split_whose_arm_misses_the_goal_is_refused() {
    let mut kernel = kernel();
    let k = binder(80, "k");
    kernel.assume(&k, &nat_type());

    let split = match_ambient(
        Term::free_var(&k),
        Term::intrinsic(Intrinsic::BoolType),
        Cases::FreeMonoid {
            carrier: Carrier::Nat {
                empty_case: nat(0),
                cons_case: Scope::close(
                    Two,
                    &[&binder(81, "pred"), &binder(82, "ih")],
                    Term::intrinsic(Intrinsic::Bool(true)),
                ),
            },
        },
    );
    assert!(matches!(
        infer(&mut kernel, &split),
        Err(Error::Mismatch { .. })
    ));
}

/// A budget failure is not a malformed motive, on the way from a motive's type to its sort as on the way to the type. `T : U`, where `U` is a `Nat` eliminator counting a literal down to `Type`, so the type of what the result states is in hand at once and reading *it* as a sort is what the budget cannot afford. Reporting `NotAMotive` whatever that read was refused for would send a reader to a motive with nothing wrong with it; an ambient goal and a family's motive go through the one read, so both are put to it. The control is the same elimination under a budget that affords the count, which is what says the refusal was the budget's.
#[test]
fn a_result_whose_sort_the_budget_cannot_reach_is_refused_for_the_budget() {
    let attempt = |budget: u64, ambient: bool| {
        let mut kernel = Kernel::new(budget, SYNTAX);

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
            true => match_ambient(
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
                Err(Error::Reduce(ReduceError::Exhausted { .. }))
            ),
            "ambient = {ambient}: the sort read ran out of budget and was reported as something else",
        );
        attempt(1_000_000_000, ambient).unwrap_or_else(|error| {
            panic!("ambient = {ambient}: a budget that affords the count refused: {error:?}")
        });
    }
}

/// An arm reduction never takes is not typed: over the literal `false` an elimination is its `false` arm wherever it stands, and nothing reads the other. Here the other holds a `Bool` where the result is `Nat`, as the result an arm is checked at leaves a proof standing under a guard that is no longer one.
///
/// Mutation-checked: with every arm typed, the elimination is refused.
#[test]
fn an_arm_under_a_scrutinee_that_is_another_case_is_not_typed() {
    let mut kernel = kernel();
    let never = Term::intrinsic(Intrinsic::Bool(true));
    let term = Term::bool_match(
        Term::intrinsic(Intrinsic::Bool(false)),
        None,
        nat_type(),
        nat(0),
        never,
    );

    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
}

/// The arm a literal scrutinee does take is typed as every arm is: over `true`, a `Bool` in the `true` arm of a `Nat` result is refused.
#[test]
fn the_arm_a_literal_scrutinee_takes_is_typed() {
    let mut kernel = kernel();
    let taken = Term::intrinsic(Intrinsic::Bool(true));
    let term = Term::bool_match(
        Term::intrinsic(Intrinsic::Bool(true)),
        None,
        nat_type(),
        nat(0),
        taken,
    );

    assert!(infer(&mut kernel, &term).is_err());
}

/// `not : (Bool) -> Bool`, as the function it is.
fn not() -> Term {
    let c = binder(86, "c");
    let bool_type = Term::intrinsic(Intrinsic::BoolType);

    Term::func(
        [(c, bool_type.clone())],
        Term::bool_match(
            Term::free_var(&c),
            None,
            bool_type,
            Term::intrinsic(Intrinsic::Bool(true)),
            Term::intrinsic(Intrinsic::Bool(false)),
        ),
    )
}

/// The `Nat` elimination of `scrutinee` whose `false` arm is `0` and whose `true` arm is `in_true`.
fn nat_by(scrutinee: Term, in_true: Term) -> Term {
    Term::bool_match(scrutinee, None, nat_type(), nat(0), in_true)
}

/// A `Bool` where a `Nat` is wanted: what an arm holds that no rule types as it is written.
fn wrong() -> Term {
    Term::intrinsic(Intrinsic::Bool(true))
}

/// A scrutinee that is no case as it stands has every arm typed, and a refused one is no arm of the elimination only where the scrutinee computes to another case. Over a variable the `Bool` in the `true` arm of a `Nat` result is refused. Over `not(true)`, which computes to `false`, that arm is one no reduction takes. Over `not(false)` it is the arm taken, and its refusal stands.
///
/// Mutation-checked: with no refused arm withdrawn, the second elimination is refused; with one withdrawn whatever its scrutinee computes to, the third is accepted.
#[test]
fn an_arm_refused_under_a_scrutinee_that_computes_to_another_case_is_not_typed() {
    let mut kernel = kernel();
    let c = binder(80, "c");
    let applied = |argument: bool| Term::apply(not(), [Term::intrinsic(Intrinsic::Bool(argument))]);

    let stuck = kernel.scoped(|kernel| {
        kernel.assume(&c, &Term::intrinsic(Intrinsic::BoolType));
        infer(kernel, &nat_by(Term::free_var(&c), wrong()))
    });
    assert!(stuck.is_err());

    assert_eq!(
        infer(&mut kernel, &nat_by(applied(true), wrong())),
        Ok(nat_type())
    );
    assert!(infer(&mut kernel, &nat_by(applied(false), wrong())).is_err());
}

/// An arm is typed under the equations around it as its own case restates them, and one of those may be what computes to another case. The outer arm assumes `not(c)` is `true`; inside it the `true` arm of a split on `c` restates that as `not(true)`, which names no local and computes to `false`, so the inner arm is one no reduction takes and the `Bool` it holds is not typed. The inner `false` arm restates it as `not(false)`, which holds, and the same `Bool` there is refused.
///
/// Mutation-checked: with the restated equations left out of what an arm assumed, the first elimination is refused.
#[test]
fn an_arm_refused_under_an_equation_its_case_closed_is_not_typed() {
    let nested = |in_false: Term, in_true: Term| {
        let mut kernel = kernel();
        let c = binder(80, "c");
        kernel.assume(&c, &Term::intrinsic(Intrinsic::BoolType));
        let inner = Term::bool_match(Term::free_var(&c), None, nat_type(), in_false, in_true);

        infer(
            &mut kernel,
            &nat_by(Term::apply(not(), [Term::free_var(&c)]), inner),
        )
    };

    assert_eq!(nested(nat(0), wrong()), Ok(nat_type()));
    assert!(nested(wrong(), nat(0)).is_err());
}

/// A scrutinee that names a local and computes to another case all the same leaves its arm one no reduction takes: `and(not(true), d)` is `false` whatever `d` is, and the `Bool` in its `true` arm is not typed. Its equation is recorded, naming `d`, and answers nothing the arm holds.
#[test]
fn an_arm_refused_under_a_scrutinee_that_names_a_local_and_computes_is_not_typed() {
    let mut kernel = kernel();
    let (a, b, d) = (binder(87, "a"), binder(88, "b"), binder(80, "d"));
    let bool_type = Term::intrinsic(Intrinsic::BoolType);
    kernel.assume(&d, &bool_type);
    let and = Term::func(
        [(a, bool_type.clone()), (b, bool_type.clone())],
        Term::bool_match(
            Term::free_var(&a),
            None,
            bool_type,
            Term::intrinsic(Intrinsic::Bool(false)),
            Term::free_var(&b),
        ),
    );
    let scrutinee = Term::apply(
        and,
        [
            Term::apply(not(), [Term::intrinsic(Intrinsic::Bool(true))]),
            Term::free_var(&d),
        ],
    );

    assert_eq!(
        infer(&mut kernel, &nat_by(scrutinee, wrong())),
        Ok(nat_type())
    );
}

/// A budget that cannot afford to compute the scrutinee withdraws no refusal: the scrutinee counts a literal down to `false`, its `true` arm holds a `Bool` where the result is `Nat`, and the elimination is refused for the budget where the count is past it and accepted where it is not.
#[test]
fn a_budget_that_cannot_compute_the_scrutinee_withdraws_no_refusal() {
    let attempt = |budget: u64| {
        let mut kernel = Kernel::new(budget, SYNTAX);
        let ih = binder(84, "ih");
        let counted = Term::nat_match(
            nat(50_000),
            None,
            Term::intrinsic(Intrinsic::BoolType),
            Term::intrinsic(Intrinsic::Bool(false)),
            &binder(83, "pred"),
            &ih,
            Term::free_var(&ih),
        );

        infer(&mut kernel, &nat_by(counted, wrong()))
    };

    assert!(matches!(
        attempt(20_000),
        Err(Error::Reduce(ReduceError::Exhausted { .. }))
    ));
    assert_eq!(attempt(1_000_000_000), Ok(nat_type()));
}

/// A fold is no case split: its reduct holds the fold at the tail, so over the literal `3` it takes its identity arm in the end, and that arm is typed though `3` is another case. Here it holds a `Bool` where the motive is `Nat`.
///
/// Mutation-checked: with a fold's arms asked as a case split's are, the fold is accepted.
#[test]
fn every_arm_of_a_fold_over_a_literal_is_typed() {
    let mut kernel = kernel();
    let ih = binder(82, "ih");
    let fold = Term::nat_match(
        nat(3),
        None,
        nat_type(),
        Term::intrinsic(Intrinsic::Bool(true)),
        &binder(81, "pred"),
        &ih,
        Term::free_var(&ih),
    );

    assert!(infer(&mut kernel, &fold).is_err());
}

/// Nor is a fold's refused arm withdrawn where its scrutinee computes: a fold takes its identity arm in the end whatever its scrutinee is, so that arm is typed. The scrutinee here is a product that is no literal as it stands and computes to `3`.
#[test]
fn every_arm_of_a_fold_over_a_scrutinee_that_computes_is_typed() {
    let mut kernel = kernel();
    let ih = binder(82, "ih");
    let fold = Term::nat_match(
        Term::intrinsic(Intrinsic::NatMul(nat(3), nat(1))),
        None,
        nat_type(),
        wrong(),
        &binder(81, "pred"),
        &ih,
        Term::free_var(&ih),
    );

    assert!(infer(&mut kernel, &fold).is_err());
}
