//! Recursive heads: alpha-variant groups, two groups compared as binders, a folded call compared against its unfolding, and the pair unfolded against itself once.

use {
    super::{Pairs, convert_intrinsic, test_support::*},
    crate::{Kernel, convert},
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Free, Intrinsic, Level, Term, UniverseConstraint, UniverseConstraintKind,
        UniverseConstraintOrigin, UniverseContext, UniverseParam,
    },
};

/// `lower ≤ upper` between two of the item's own universe parameters, as `assume_universes` takes it.
fn between(lower: usize, upper: usize) -> UniverseConstraint {
    UniverseConstraint {
        lower: Level::param(UniverseParam(lower)),
        upper: Level::param(UniverseParam(upper)),
        origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
    }
}

/// The fold at `Type⟨parameter⟩` applied to `Nat`, a discarded argument, and a symbolic count.
fn call(parameter: usize, discarded: &Free, count: &Free) -> Term {
    Term::apply(
        polymorphic_fold(Level::param(UniverseParam(parameter))),
        [nat_type(), Term::free_var(discarded), Term::free_var(count)],
    )
}

/// Binder *identity* must not leak into conversion. Two `rec` groups written with different minted names are the same group: binder names are display hints, and the bodies are de Bruijn-indexed under their scopes.
///
/// This is the property that lets a folded recursive call be compared structurally at all — see the projection arm, which requires the groups to be equal.
#[test]
fn two_alpha_variant_recursive_groups_convert() {
    let mut kernel = kernel();
    let x = binder(90, "x");

    let countdown = |group_binder: Free, param: Free, motive: Free, pred: Free, ih: Free| {
        let body = Term::func(
            [(param, nat_type())],
            Term::nat_match(
                Term::free_var(&param),
                Some(&motive),
                nat_type(),
                nat(0),
                &pred,
                &ih,
                Term::apply(Term::free_var(&group_binder), [Term::free_var(&pred)]),
            ),
        );

        Term::rec(
            [(
                group_binder,
                Term::func_type([(param, nat_type())], nat_type()),
                body,
            )],
            Term::apply(Term::free_var(&group_binder), [Term::free_var(&x)]),
        )
    };

    let left = countdown(
        binder(0, "countdown"),
        binder(1, "n"),
        binder(2, "m"),
        binder(3, "pred"),
        binder(4, "ih"),
    );
    let right = countdown(
        binder(10, "loop"),
        binder(11, "k"),
        binder(12, "motive"),
        binder(13, "p"),
        binder(14, "rest"),
    );

    assert_ne!(
        format!("{left}"),
        String::new(),
        "the fixture should render, so a failure names real terms",
    );
    assert_eq!(convert(&mut kernel, &nat_type(), &left, &right), Ok(true));
}

/// A recursive call applied to a symbolic argument stays folded, and comparing it with itself terminates rather than unfolding in lockstep forever.
#[test]
fn a_folded_recursive_call_converts_without_unfolding_forever() {
    let mut kernel = Kernel::new(10_000, SYNTAX);

    let n = binder(0, "n");
    let motive = binder(1, "m");
    let pred = binder(2, "pred");
    let hypothesis = binder(3, "ih");
    let countdown = binder(4, "countdown");
    let x = binder(5, "x");

    let body = Term::func(
        [(n, nat_type())],
        Term::nat_match(
            Term::free_var(&n),
            Some(&motive),
            nat_type(),
            nat(0),
            &pred,
            &hypothesis,
            Term::apply(Term::free_var(&countdown), [Term::free_var(&pred)]),
        ),
    );

    let group = [(
        countdown,
        Term::func_type([(n, nat_type())], nat_type()),
        body,
    )];

    let folded = Term::rec(
        group.clone(),
        Term::apply(Term::free_var(&countdown), [Term::free_var(&x)]),
    );
    let same_shape_other_argument = Term::rec(
        group,
        Term::apply(
            Term::free_var(&countdown),
            [Term::free_var(&binder(6, "y"))],
        ),
    );

    assert_eq!(
        convert(&mut kernel, &nat_type(), &folded, &folded.clone()),
        Ok(true),
    );
    assert_eq!(
        convert(
            &mut kernel,
            &nat_type(),
            &folded,
            &same_shape_other_argument,
        ),
        Ok(false),
    );
}

/// `rec f : (n: Nat) -> Nat = (n) => match n | 0 => base | pred + 1 => f(pred) + by; f`, its step's sum written `f(pred) + by` or, commuted, `by + f(pred)`: one recursion in two spellings no rename relates.
fn stepping(base: usize, by: usize, commuted: bool) -> Term {
    let member = binder(60, "f");
    let (n, motive, pred, ih) = (
        binder(61, "n"),
        binder(62, "m"),
        binder(63, "pred"),
        binder(64, "ih"),
    );
    let call = Term::apply(Term::free_var(&member), [Term::free_var(&pred)]);
    let step = match commuted {
        false => Intrinsic::nat_add(call, nat(by)),
        true => Intrinsic::nat_add(nat(by), call),
    };

    Term::rec(
        [(
            member,
            Term::func_type([(n, nat_type())], nat_type()),
            Term::func(
                [(n, nat_type())],
                Term::nat_match(
                    Term::free_var(&n),
                    Some(&motive),
                    nat_type(),
                    nat(base),
                    &pred,
                    &ih,
                    Term::intrinsic(step),
                ),
            ),
        )],
        Term::free_var(&member),
    )
}

/// Two recursive groups are compared as the binders they are: the same member of two groups is one where, member for member, their types and their bodies convert under one shared set of binders. The two here are one recursion whose step is `f(pred) + 2` in one and `2 + f(pred)` in the other, so no spelling relates them, and an unfolding of both poses the same two calls under fresh binders without end. They are compared bare, at their function type, and as two calls, whose heads are compared before their spines. The elaborator's twin shares the name.
///
/// Mutation-checked: with the comparison removed the budget, small here, runs out on both; with it holding any two groups of one shape, `two_groups_whose_bodies_part_are_apart` fails.
#[test]
fn two_groups_whose_bodies_convert_are_one() {
    let mut kernel = Kernel::new(10_000, SYNTAX);
    let x = binder(90, "x");
    kernel.assume(&x, &nat_type());
    let (left, right) = (stepping(0, 2, false), stepping(0, 2, true));
    let function = Term::func_type([(binder(91, "n"), nat_type())], nat_type());

    assert_eq!(convert(&mut kernel, &function, &left, &right), Ok(true));
    assert_eq!(
        convert(
            &mut kernel,
            &nat_type(),
            &Term::apply(left, [Term::free_var(&x)]),
            &Term::apply(right, [Term::free_var(&x)]),
        ),
        Ok(true),
    );
}

/// The control: two groups of one shape whose base cases part are two functions, bare and at the head of two calls. The comparison refuses them at the base case, and so does the unfolding it leaves them to, a fold's base case being compared before its step.
#[test]
fn two_groups_whose_bodies_part_are_apart() {
    let mut kernel = Kernel::new(10_000, SYNTAX);
    let x = binder(90, "x");
    kernel.assume(&x, &nat_type());
    let (left, right) = (stepping(0, 2, false), stepping(1, 2, true));
    let function = Term::func_type([(binder(91, "n"), nat_type())], nat_type());

    assert_eq!(convert(&mut kernel, &function, &left, &right), Ok(false));
    assert_eq!(
        convert(
            &mut kernel,
            &nat_type(),
            &Term::apply(left, [Term::free_var(&x)]),
            &Term::apply(right, [Term::free_var(&x)]),
        ),
        Ok(false),
    );
}

/// `rec f : (n: Nat, x: Nat) -> Nat = (n, x) => match n | 0 => x | pred + 1 => f(pred, x); f`, or with its two parameters in the other order where `flipped`: one recursion on `n` that hands `x` back, over two orders of its arguments.
fn handing_back(flipped: bool) -> Term {
    let member = binder(110, "f");
    let (n, x) = (binder(111, "n"), binder(112, "x"));
    let (motive, pred, ih) = (binder(113, "m"), binder(114, "pred"), binder(115, "ih"));
    let ordered = |n: Term, x: Term| match flipped {
        false => [n, x],
        true => [x, n],
    };
    let parameters = match flipped {
        false => [(n, nat_type()), (x, nat_type())],
        true => [(x, nat_type()), (n, nat_type())],
    };

    Term::rec(
        [(
            member,
            Term::func_type(parameters.clone(), nat_type()),
            Term::func(
                parameters,
                Term::nat_match(
                    Term::free_var(&n),
                    Some(&motive),
                    nat_type(),
                    Term::free_var(&x),
                    &pred,
                    &ih,
                    Term::apply(
                        Term::free_var(&member),
                        ordered(Term::free_var(&pred), Term::free_var(&x)),
                    ),
                ),
            ),
        )],
        Term::free_var(&member),
    )
}

/// `rec g : (x: Nat, y: Nat, z: Nat) -> Nat = (x, y, z) => match z | 0 => 0 | pred + 1 => g(y, 0, pred); g`: a recursion that discards `x` at its first step and `y` at its second, so two calls apart in `x` are one after one unfolding, and two apart in `y` after two.
fn shifting() -> Term {
    let member = binder(120, "g");
    let (x, y, z) = (binder(121, "x"), binder(122, "y"), binder(123, "z"));
    let (motive, pred, ih) = (binder(124, "m"), binder(125, "pred"), binder(126, "ih"));
    let parameters = [(x, nat_type()), (y, nat_type()), (z, nat_type())];

    Term::rec(
        [(
            member,
            Term::func_type(parameters.clone(), nat_type()),
            Term::func(
                parameters,
                Term::nat_match(
                    Term::free_var(&z),
                    Some(&motive),
                    nat_type(),
                    nat(0),
                    &pred,
                    &ih,
                    Term::apply(
                        Term::free_var(&member),
                        [Term::free_var(&y), nat(0), Term::free_var(&pred)],
                    ),
                ),
            ),
        )],
        Term::free_var(&member),
    )
}

/// A pair of calls is unfolded against itself once, and met again inside that unfolding it is apart. The first two are one recursion over two orders of its arguments, which no comparison as binders holds, one binder standing for both members where their calls take their arguments in two orders. Unfolded, they pose their two heads again one binder further in, and would again at every round, in a scope no round has met. The second two are two calls of one member that a second unfolding would reconcile: the first poses the same member against itself at two spines, and that is where it stops. What holds either pair is an induction, which a lemma states, and the calls one unfolding reconciles are still one. The elaborator's twin shares the name.
///
/// Mutation-checked: with a pair unfolded however often it is met, the budget, small here, runs out on the first two and the second two convert.
#[test]
fn a_pair_met_again_inside_its_own_unfolding_is_apart() {
    let mut kernel = Kernel::new(10_000, SYNTAX);
    let [a, b, c, d, n] = [
        binder(92, "a"),
        binder(93, "b"),
        binder(94, "c"),
        binder(95, "d"),
        binder(96, "n"),
    ]
    .map(|local| {
        kernel.assume(&local, &nat_type());
        Term::free_var(&local)
    });

    assert_eq!(
        convert(
            &mut kernel,
            &nat_type(),
            &Term::apply(handing_back(false), [n.clone(), a.clone()]),
            &Term::apply(handing_back(true), [a.clone(), n.clone()]),
        ),
        Ok(false),
    );

    let call = |x: &Term, y: &Term| Term::apply(shifting(), [x.clone(), y.clone(), n.clone()]);
    assert_eq!(
        convert(&mut kernel, &nat_type(), &call(&a, &c), &call(&b, &d)),
        Ok(false),
    );
    assert_eq!(
        convert(&mut kernel, &nat_type(), &call(&a, &c), &call(&b, &c)),
        Ok(true),
    );
}

/// A verdict that rested on a pair left apart is not remembered. Two calls are apart inside their own unfolding and nowhere else, so a refusal that met one is true of the path it was reached on, and it is filed nowhere — where a comparison that met none is filed, and forgotten with the budget.
///
/// Mutation-checked: with a verdict filed whatever was left apart while deciding it, the two calls are remembered apart; and with the declaration's verdicts kept across a restore, the control still answers after it.
#[test]
fn a_verdict_that_rested_on_a_pair_left_apart_is_not_remembered() {
    let mut kernel = Kernel::new(10_000, SYNTAX);
    let (n, a) = (binder(92, "n"), binder(93, "a"));
    kernel.assume(&n, &nat_type());
    kernel.assume(&a, &nat_type());
    let nat_ = nat_type();
    let left = Term::apply(
        handing_back(false),
        [Term::free_var(&n), Term::free_var(&a)],
    );
    let right = Term::apply(handing_back(true), [Term::free_var(&a), Term::free_var(&n)]);

    assert_eq!(convert(&mut kernel, &nat_, &left, &right), Ok(false));
    assert_eq!(kernel.convert_hit(&nat_, &left, &right), None);

    let ground = Term::type_ground();
    let x = binder(40, "x");
    let redex = Term::apply(
        Term::func([(x, Term::type_ground())], Term::free_var(&x)),
        [nat_type()],
    );
    assert_eq!(convert(&mut kernel, &ground, &redex, &nat_type()), Ok(true));
    assert_eq!(kernel.convert_hit(&ground, &redex, &nat_type()), Some(true));

    kernel.restore_budget();
    assert_eq!(kernel.convert_hit(&ground, &redex, &nat_type()), None);
}

/// Conversion keeps the constant function apart from the identity whatever index the local it mentions was minted at.
///
/// A positive control for capture-avoidance. Eta at a function type opens a binder that would alias a free local the kernel could mint again — `(x) => y` and `(x) => x` become the same term the moment the opened binder *is* `y` — so `y` is assumed at each of the first identities a fresh kernel hands out, one of which is the binder the comparison opens. `Kernel::assume` raises the counter past it, so that binder is another. Mutation-checked: an `assume` that leaves the counter alone makes the two convertible.
#[test]
fn conversion_separates_a_constant_from_the_identity_at_every_colliding_index() {
    let nat = Term::intrinsic(Intrinsic::NatType);
    let parameter = Free::local(9_000, Some("x"));
    let identity = Term::func([(parameter, nat.clone())], Term::free_var(&parameter));
    let function = Term::func_type([(parameter, nat.clone())], nat.clone());

    for index in 0..16 {
        let colliding = Free::local(index, Some("y"));
        let mut kernel = Kernel::new(100_000, SYNTAX);
        kernel.assume(&colliding, &nat);
        let constant = Term::func([(parameter, nat.clone())], Term::free_var(&colliding));

        assert!(
            !convert(&mut kernel, &function, &constant, &identity)
                .expect("the comparison completes"),
            "the constant function over local {index} and the identity are not convertible",
        );
    }
}

/// Two recursive types of two shapes that unfold alike are apart: no goal is held for having been met before. Both sides are the type `X = (X) -> Nat`, spelled as groups of two lengths — the right carries a second, unused member — so no position pairs them as binders, and forced against each other each is a function type whose domain poses the two again. Inside their own unfolding the pair is apart. What the two spellings share is a coinduction, which no rule of conversion checks. The elaborator's twin shares the name.
///
/// Mutation-checked: with a pair unfolded however often it is met, the budget, small here, runs out.
#[test]
fn two_groups_of_two_shapes_that_unfold_alike_are_apart() {
    let mut kernel = Kernel::new(10_000, SYNTAX);

    assert_eq!(
        convert(
            &mut kernel,
            &Term::type_ground(),
            &equirecursive(binder(20, "a"), binder(21, "x"), false),
            &equirecursive(binder(30, "b"), binder(31, "y"), true),
        ),
        Ok(false),
    );
}

/// Two instances of one recursive group are one term when the item's hypotheses force their levels equal: the group is decided by `rec_instances` — as an applied head, and bare at `Type` — with the equation every other level-bearing head already gets, rather than unfolded. The elaborator's twin of this proposition shares the name.
#[test]
fn two_instances_of_one_recursive_group_convert_when_their_levels_are_equal_under_the_hypotheses() {
    let mut kernel = kernel();
    kernel.assume_universes(&UniverseContext {
        parameter_count: 2,
        quality_count: 0,
        constraints: vec![between(0, 1), between(1, 0)],
    });
    let a = binder(80, "a");
    let n = binder(81, "n");

    assert_eq!(
        convert(&mut kernel, &nat_type(), &call(0, &a, &n), &call(1, &a, &n)),
        Ok(true),
        "the same fold at two levels the hypotheses equate did not converge",
    );
    assert_eq!(
        convert(
            &mut kernel,
            &Term::type_ground(),
            &polymorphic_fold(Level::param(UniverseParam(0))),
            &polymorphic_fold(Level::param(UniverseParam(1))),
        ),
        Ok(true),
        "the bare projections were not decided before reduction opened them",
    );
}

/// Two instances of one recursive group whose levels the hypotheses do not force equal are refused at the head, without an unfolding: each unfolding reproduces the same two instances on the recursive call, so a retry could only pose them again. The elaborator's twin shares the name.
#[test]
fn two_instances_of_one_recursive_group_at_unequal_levels_are_refused_without_unfolding() {
    let mut kernel = Kernel::new(10_000, SYNTAX);
    kernel.assume_universes(&UniverseContext {
        parameter_count: 2,
        quality_count: 0,
        constraints: vec![between(0, 1)],
    });
    let a = binder(80, "a");
    let n = binder(81, "n");

    assert_eq!(
        convert(&mut kernel, &nat_type(), &call(0, &a, &n), &call(1, &a, &n)),
        Ok(false),
        "two instances related only by cumulativity were not refused outright",
    );
}

/// The retry the head rule must not take away: two calls of one group at one level whose spines differ only in an argument the fold discards still converge, through the one unfolding that discards it.
#[test]
fn two_calls_of_one_recursive_group_with_different_spines_still_retry_through_an_unfolding() {
    let mut kernel = kernel();
    kernel.assume_universes(&UniverseContext {
        parameter_count: 1,
        quality_count: 0,
        constraints: Vec::new(),
    });
    let a = binder(80, "a");
    let b = binder(82, "b");
    let n = binder(81, "n");

    assert_eq!(
        convert(&mut kernel, &nat_type(), &call(0, &a, &n), &call(0, &b, &n)),
        Ok(true),
        "a spine difference the fold discards stopped being reconciled by unfolding",
    );
}

/// **Two calls of one recursive group differing only in a proof convert by their spines, the proof compared at the member's own type.** A `rec` projection carries its member type, so `spine_telescope` reads the telescope there as it does off a variable's declaration. `tests::board`'s `a_recursive_function_carrying_a_proof_converts_without_unfolding` is the program, reaching the group through a global it is the value of; `two_calls_of_one_recursive_group_with_different_spines_still_retry_through_an_unfolding`, whose spines differ in an argument the fold discards, is the control that a mismatch still unfolds.
///
/// Mutation-checked against the two halves of the rule, neither of which this goal holds alone: with no telescope read off a `rec` projection it still passes, the two proofs compared at `Type` and one by the proposition a lookup types both at (`by_their_own_type`), and removing the spine attempt before forcing leaves it passing — that half is `irrelevance_tests`'s `a_definition_applied_to_two_proofs_converts_before_unfolding`, which fails under it.
#[test]
fn two_calls_of_one_recursive_group_at_two_proofs_convert_without_unfolding() {
    let mut kernel = Kernel::new(10_000, SYNTAX);
    let proposition = declare(&mut kernel, "P", Term::prop());

    let countdown = binder(20, "countdown");
    let (n, p) = (binder(21, "n"), binder(22, "p"));
    let (motive, pred, ih) = (binder(23, "m"), binder(24, "pred"), binder(25, "ih"));
    let body = Term::func(
        [(n, nat_type()), (p, proposition.clone())],
        Term::nat_match(
            Term::free_var(&n),
            Some(&motive),
            nat_type(),
            nat(0),
            &pred,
            &ih,
            Term::apply(
                Term::free_var(&countdown),
                [Term::free_var(&pred), Term::free_var(&p)],
            ),
        ),
    );
    let group = Term::rec(
        [(
            countdown,
            Term::func_type([(n, nat_type()), (p, proposition.clone())], nat_type()),
            body,
        )],
        Term::free_var(&countdown),
    );

    let x = binder(26, "x");
    let (left, right) = (binder(27, "p"), binder(28, "q"));
    kernel.assume(&x, &nat_type());
    kernel.assume(&left, &proposition);
    kernel.assume(&right, &proposition);

    assert_eq!(
        convert(
            &mut kernel,
            &nat_type(),
            &Term::apply(group.clone(), [Term::free_var(&x), Term::free_var(&left)]),
            &Term::apply(group, [Term::free_var(&x), Term::free_var(&right)]),
        ),
        Ok(true),
    );
}

/// What the intrinsic rule hands on is compared inside the pairs of the goal that produced it, not from none. With `g` against itself already being unfolded, `x + g(a, c, n)` against `x + g(b, c, n)` comes down to the two calls, which one unfolding reconciles and which are apart inside the unfolding they stand in. Compared from no pair, the same two sums are one.
#[test]
fn what_the_intrinsic_rule_hands_on_is_compared_inside_the_pairs_of_its_goal() {
    let mut kernel = kernel();
    let [x, a, b, c, n] = [
        binder(40, "x"),
        binder(41, "a"),
        binder(42, "b"),
        binder(43, "c"),
        binder(44, "n"),
    ]
    .map(|local| {
        kernel.assume(&local, &nat_type());
        Term::free_var(&local)
    });
    let call = |first: &Term| Term::apply(shifting(), [first.clone(), c.clone(), n.clone()]);
    let sum = |summand: Term| Intrinsic::nat_add(x.clone(), summand);

    let mut pairs = Pairs::default();
    assert!(pairs.unfold(&shifting(), &shifting()));
    assert_eq!(
        convert_intrinsic(&mut kernel, &mut pairs, &sum(call(&a)), &sum(call(&b))),
        Ok(false),
        "the two calls were unfolded inside their own unfolding",
    );
    assert_eq!(
        convert_intrinsic(
            &mut kernel,
            &mut Pairs::default(),
            &sum(call(&a)),
            &sum(call(&b))
        ),
        Ok(true),
        "the control: from no pair, the two calls are one after one unfolding",
    );
}
