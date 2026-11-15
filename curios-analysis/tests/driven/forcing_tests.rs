//! Index forcings through a real checker: a binder forced twice, a binder or family out of reach, and the outer solution a pinned binder is rewritten through.

use {
    super::test_support::*,
    curios_analysis::{Invert, another_case, invert_indices, solve_indices},
    curios_common::Qualifier,
    curios_core::{Free, Global, Intrinsic, Level, Nat, Term},
    std::slice,
};

/// The bound `fits_its_variable` states, put to the function that states it: a variable assumed at a sort keeps a value only where the judge holds the value's own sort below that sort, an outer variable and a case's binder alike.
///
/// The value is the literal `Type 4`, whose sort every reader computes as `Type 5`. Assumed at `Type 5` the variable holds it; assumed at `Type 0` it does not, and the solution is the one the rule says is dropped.
#[test]
fn a_variable_at_a_sort_keeps_only_a_value_its_sort_holds() {
    let target = Term::type_at(Level::constant(4));

    for (label, assumed, held, binder) in [
        (
            "an outer variable, the value's sort below its own",
            5,
            true,
            false,
        ),
        ("an outer variable, above its own", 0, false, false),
        ("a binder, the value's sort below its own", 5, true, true),
        ("a binder, above its own", 0, false, true),
    ] {
        let mut certifier = certifier();
        let variable = Free::local(900, Some("X"));
        certifier.assume(&variable, &Term::type_at(Level::constant(assumed)));

        let (actual, case) = (Term::free_var(&variable), target.clone());
        let (actuals, targets, binders) = match binder {
            false => ([actual], [case], Vec::new()),
            true => ([case], [actual], vec![variable]),
        };
        let Invert::Solved(solutions) = solve_indices(&mut certifier, &actuals, &targets, &binders)
            .expect("the certifier answers")
        else {
            panic!("{label}: a reachable case was reported unreachable");
        };

        match held {
            true => assert_eq!(solutions, vec![(variable, target.clone())], "{label}"),
            false => assert!(solutions.is_empty(), "{label}: {solutions:?}"),
        }
    }
}

/// A binder forced in two index positions is one equation between its two forcings: the first solves it, and the second, read through that solution, is an equation between the two values, which deletion, conflict and injectivity decide as they decide any.
///
/// Every fixture forces one binder in the first index position and again in the second — the shape `refl(@z) : (z, z)` meets at `Eq()(a, b)` — and the first two differ in nothing but the family's sort, which is what makes them a control pair rather than two unrelated cases.
///
/// At a proposition the two forcings `a()` and `b()` are one value by irrelevance, so the equation between them is deleted and the first forcing's solution stands: sound because `Eq : Prop` makes the system definitionally K. At a relevant family the same two are constructors a program tells apart, and each forcing was reached by injective steps, so the case is reachable only if `a()` is `b()`: the verdict is `Impossible`, the conflict rule arriving through a non-linear target, and it is what lets `match h end` close a hypothesis `Eq()(false, true)`. The third fixture is the control on *that*: two forcings that neither are one value nor clash — one opaque function at two arguments, which may well agree — leave the equation between them undecided, and the first forcing's solution stands beside it.
///
/// `Impossible` excuses an arm from being checked at all, and an arm excused wrongly at a `Prop`-sorted family is the vacuous-elimination route to a closed inhabitant of `False`, routed there from index inversion. So the proposition's verdict is the one this holds hardest: the equation between the forcings is put to the walk a linear position is put to, whose tag test is licensed by the family's sort.
#[test]
fn a_binder_forced_twice_is_one_equation_between_its_forcings() {
    enum Verdict {
        Survives,
        Impossible,
    }

    for (label, sort, opaque, expected) in [
        (
            "a proposition, whose two inhabitants irrelevance identifies",
            Term::prop(),
            false,
            Verdict::Survives,
        ),
        (
            "a relevant family, whose two constructors a program tells apart",
            Term::type_ground(),
            false,
            Verdict::Impossible,
        ),
        (
            "a relevant family, at two values nothing tells apart or together",
            Term::type_ground(),
            true,
            Verdict::Survives,
        ),
    ] {
        let mut certifier = certifier();
        let family = declare(&mut certifier, "Forced", sort);
        let family_type = Term::induct_type(family, Vec::<Term>::new(), Vec::<Term>::new());
        let binder = Free::local(900, Some("p"));
        let opaque_function = Free::local(901, Some("g"));
        let argument = Free::local(902, Some("x"));

        certifier.assume(&binder, &family_type);
        certifier.assume(
            &opaque_function,
            &Term::func_type([(argument, family_type.clone())], family_type.clone()),
        );

        let inhabitant = |tag| {
            let constructed = Term::variant(family, Vec::<Term>::new(), tag, Vec::<Term>::new());
            match opaque {
                true => Term::apply(Term::free_var(&opaque_function), [constructed]),
                false => constructed,
            }
        };
        let target = Term::free_var(&binder);

        let outcome = invert_indices(
            &mut certifier,
            &[inhabitant("a"), inhabitant("b")],
            &[target.clone(), target],
            slice::from_ref(&binder),
        )
        .expect("inversion is a total function of finished terms");

        let solutions = match (outcome, expected) {
            (Invert::Impossible, Verdict::Impossible) => continue,
            (Invert::Impossible, _) => panic!(
                "{label}: a re-forced binder was reported unreachable, which excuses the arm from being checked"
            ),
            (Invert::Solved(_), Verdict::Impossible) => {
                panic!("{label}: two forcings that clash left the case reachable")
            }
            (Invert::Solved(solutions), Verdict::Survives) => solutions,
        };

        assert_eq!(
            solutions,
            vec![(binder, inhabitant("a"))],
            "{label}: the binder is solved to its first forcing and to nothing else",
        );
    }
}

/// The clash rule's license is the registry, and a family the registry cannot answer for must not clash.
///
/// Two different constructors of one family definitely clash only when the family is *relevant*: at a `Prop`-sorted family irrelevance makes every inhabitant the same value, so reading tag disjointness there contradicts conversion, and an arm excused as impossible on that reading is the vacuous-elimination route to a closed inhabitant of `False`. The sort is read out of `induct_decl`, so the whole of that protection rests on the lookup answering — and a lookup that answered nothing and fell through to the tag check would answer `Impossible`: the analysis deciding the strong verdict *because* it was blind, where every other blindness on this seam — an unknown positivity name, an undecodable size, an unassumed binder — turns into a refusal. The branch is unreachable from either checker as far as the walks are known, a well-typed variant's family being registered wherever the term came from, and it is pinned anyway: the conservative answer is not the obvious one to write, and nothing else in the workspace would notice the wrong one.
///
/// The control pair holds what the rule must not move: the same position with the declaration present still clashes at a relevant sort, and still yields nothing at `Prop`.
#[test]
fn a_family_the_registry_cannot_answer_for_does_not_clash() {
    let inhabitant = |family: &Global, tag: &str| {
        Term::variant(*family, Vec::<Term>::new(), tag, Vec::<Term>::new())
    };

    // The subject: the family was never declared, so the sort read is blind.
    let mut blind = certifier();
    let family = Global::Authored(Qualifier::from(["Blind"]));
    let outcome = invert_indices(
        &mut blind,
        &[inhabitant(&family, "a")],
        &[inhabitant(&family, "b")],
        &[],
    )
    .expect("inversion is a total function of finished terms");
    assert!(
        matches!(outcome, Invert::Solved(ref solutions) if solutions.is_empty()),
        "a family the registry cannot answer for was decided rather than refused",
    );

    for (label, sort, impossible) in [
        (
            "a relevant family's two constructors definitely clash",
            Term::type_ground(),
            true,
        ),
        (
            "a proposition's two constructors are one value and yield nothing",
            Term::prop(),
            false,
        ),
    ] {
        let mut sighted = certifier();
        let family = declare(&mut sighted, "Blind", sort);
        let outcome = invert_indices(
            &mut sighted,
            &[inhabitant(&family, "a")],
            &[inhabitant(&family, "b")],
            &[],
        )
        .expect("inversion is a total function of finished terms");
        assert_eq!(matches!(outcome, Invert::Impossible), impossible, "{label}");
    }
}

/// An outer variable inside an index is solved, and only a local.
///
/// A case whose target is `1`, met at an actual index `n + 1`, is reachable only when `n` is `0`: the peel reduces the pair to `0` against `n`, and the unifier solves the outer variable there. Both checkers get it from `solve_indices`. The control drops the one fact the rule reads from its driver: with `n` not a local the walk opened, it is a name no case can refine, and nothing is solved.
#[test]
fn an_outer_variable_inside_an_index_is_solved() {
    let n = Free::local(900, Some("n"));
    let nat = |k: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(k)));
    let actual = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat(1)));

    for (label, local) in [("a local", true), ("a name no case refines", false)] {
        let mut certifier = certifier();
        if local {
            certifier.assume(&n, &Term::intrinsic(Intrinsic::NatType));
        }

        let Invert::Solved(solutions) =
            solve_indices(&mut certifier, slice::from_ref(&actual), &[nat(1)], &[])
                .expect("the certifier answers")
        else {
            panic!("{label}: a reachable case was reported unreachable");
        };

        match local {
            true => assert_eq!(solutions, vec![(n, nat(0))], "{label}"),
            false => assert!(solutions.is_empty(), "{label}: {solutions:?}"),
        }
    }
}

/// A case is solved as one idempotent substitution: a binder solved to a value naming an outer variable is rewritten through what that variable is solved to.
///
/// Actuals `(n + 1, n + 1)` against targets `(k, 1)`: the first position solves `k := n + 1`, and the second learns `n := 0`. Applied one after the other the pair would leave `k` naming a variable the same substitution replaces; made idempotent, `k`'s value no longer mentions `n` at all.
#[test]
fn a_case_is_solved_as_one_idempotent_substitution() {
    let mut certifier = certifier();
    let n = Free::local(900, Some("n"));
    let k = Free::local(901, Some("k"));
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let nat = |value: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(value)));
    certifier.assume(&n, &nat_type);
    certifier.assume(&k, &nat_type);

    let actual = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat(1)));
    let Invert::Solved(solutions) = solve_indices(
        &mut certifier,
        &[actual.clone(), actual],
        &[Term::free_var(&k), nat(1)],
        slice::from_ref(&k),
    )
    .expect("the certifier answers") else {
        panic!("a reachable case was reported unreachable");
    };

    let value_of = |name: &Free| {
        solutions
            .iter()
            .find(|(solved, _)| solved == name)
            .map(|(_, value)| value.clone())
    };
    assert_eq!(
        value_of(&n),
        Some(nat(0)),
        "the outer variable: {solutions:?}"
    );
    let pinned = value_of(&k).expect("the binder is pinned");
    assert!(
        !pinned.mentions_free(&n),
        "the pinned value still names what the substitution replaces: {pinned:?}"
    );
}

/// An outer variable forced twice is one equation between its two values: a local forced to two values a program tells apart leaves the case with no value, and one forced to two inhabitants of a proposition is forced to one value twice.
///
/// Actuals `(x, x)` against targets `(a(), b())`, for a case with no binder. `x` is solved to `a()`, and the second position is then `a() ~ b()`. At a relevant family the two clash and the case is impossible, which is what leaves a written arm untyped by the certifier and may leave it out. At a proposition the same two are one value, so the case stays reachable: read as a clash there, an arm would go unchecked on the strength of two proofs being told apart.
#[test]
fn an_outer_variable_forced_twice_clashes_only_where_its_forcings_are_told_apart() {
    for (label, sort, impossible) in [
        (
            "a relevant family, whose two constructors a program tells apart",
            Term::type_ground(),
            true,
        ),
        (
            "a proposition, whose two inhabitants irrelevance identifies",
            Term::prop(),
            false,
        ),
    ] {
        let mut certifier = certifier();
        let family = declare(&mut certifier, "Forced", sort);
        let x = Free::local(900, Some("x"));
        certifier.assume(
            &x,
            &Term::induct_type(family, Vec::<Term>::new(), Vec::<Term>::new()),
        );

        let inhabitant = |tag| Term::variant(family, Vec::<Term>::new(), tag, Vec::<Term>::new());
        let actuals = [Term::free_var(&x), Term::free_var(&x)];
        let targets = [inhabitant("a"), inhabitant("b")];

        let outcome =
            solve_indices(&mut certifier, &actuals, &targets, &[]).expect("the certifier answers");
        assert_eq!(matches!(outcome, Invert::Impossible), impossible, "{label}");
    }
}

/// A value is another case than an arm's where it names a case as it stands and the unifier that excuses an absent arm finds the two in a definite clash. Both checkers know an arm no reduction takes by this: the certifier types none, and the elaborator words its refusal of one.
///
/// The controls are what the rule must not answer: the arm's own case, a term that names no case as it stands — a variable, a successor over one — and two constructors of a proposition, which conversion holds to be one value.
#[test]
fn a_value_is_another_case_only_where_it_names_one_that_clashes() {
    let mut certifier = certifier();
    let relevant = declare(&mut certifier, "Relevant", Term::type_ground());
    let proposition = declare(&mut certifier, "Proposition", Term::prop());
    let n = Free::local(900, Some("n"));
    let pred = Free::local(901, Some("pred"));
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    certifier.assume(&n, &nat_type);
    certifier.assume(&pred, &nat_type);

    let nat = |value: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(value)));
    let bool_ = |value: bool| Term::intrinsic(Intrinsic::Bool(value));
    let successor = |of: &Free| Term::intrinsic(Intrinsic::nat_add(Term::free_var(of), nat(1)));
    let constructed = |family: Global, tag: &str| {
        Term::variant(family, Vec::<Term>::new(), tag, Vec::<Term>::new())
    };

    for (label, scrutinee, case, binders, expected) in [
        (
            "a literal against the other literal",
            bool_(false),
            bool_(true),
            Vec::new(),
            true,
        ),
        (
            "a literal against itself",
            bool_(true),
            bool_(true),
            Vec::new(),
            false,
        ),
        (
            "zero against a successor",
            nat(0),
            successor(&pred),
            vec![pred],
            true,
        ),
        (
            "a positive literal against zero",
            nat(2),
            nat(0),
            Vec::new(),
            true,
        ),
        (
            "a positive literal against a successor",
            nat(2),
            successor(&pred),
            vec![pred],
            false,
        ),
        (
            "a successor over a variable against zero",
            successor(&n),
            nat(0),
            Vec::new(),
            false,
        ),
        (
            "a variable against a literal",
            Term::free_var(&n),
            nat(0),
            Vec::new(),
            false,
        ),
        (
            "two constructors of a relevant family",
            constructed(relevant, "a"),
            constructed(relevant, "b"),
            Vec::new(),
            true,
        ),
        (
            "one constructor of a relevant family",
            constructed(relevant, "a"),
            constructed(relevant, "a"),
            Vec::new(),
            false,
        ),
        (
            "two constructors of a proposition",
            constructed(proposition, "a"),
            constructed(proposition, "b"),
            Vec::new(),
            false,
        ),
    ] {
        assert_eq!(
            another_case(&mut certifier, &scrutinee, &case, &binders)
                .expect("the certifier answers"),
            expected,
            "{label}",
        );
    }
}

/// Depth is bought with stack: a pair of index terms nested five thousand layers deep is inverted on the default test-thread stack, where the walk descends a tuple's fields one level at a time and forces both sides at each.
///
/// What this guards is the shape `.claude/rules/rust.md` asks of every walk over data-shaped depth, which the two analyses beside this one were brought to first. The two towers differ only at the bottom, `0` against `1`, so no structural shortcut can answer and the descent is forced the whole way; the answer is a clash, which is what the intrinsic carriers' deduction says of two different literals.
///
/// Mutation-checked: with `Walk::unify_index`'s `recurse` bracket removed, the run prints `has overflowed its stack` and aborts, which is a death and not a refusal.
#[test]
fn a_deep_pair_of_index_terms_is_inverted_on_the_default_stack() {
    let mut certifier = certifier();
    let deep = |bottom: Term| {
        let mut term = bottom;
        for _ in 0..5_000 {
            term = Term::tuple([term]);
        }
        term
    };
    let actual = deep(Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))));
    let target = deep(Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))));

    assert!(matches!(
        solve_indices(
            &mut certifier,
            slice::from_ref(&actual),
            slice::from_ref(&target),
            &[]
        ),
        Ok(Invert::Impossible)
    ));
}

/// Two `List` literals over `Nat` apart in an element, or in their lengths, are no case a value meets: inverted pointwise as a tuple's fields are, and by length where the strip leaves runs alone, through the certifier that judges them. Mutation-checked: with `unify_node` reading two literals as any two words, the element pair is undecided, and with the strip reading no length, so is the length pair.
#[test]
fn two_list_literals_apart_in_an_element_or_in_length_are_impossible() {
    let list = |items: &[u64]| {
        Term::intrinsic(Intrinsic::List {
            element: Term::intrinsic(Intrinsic::NatType),
            items: items
                .iter()
                .map(|item| Term::intrinsic(Intrinsic::Nat(Nat::new(*item))))
                .collect(),
        })
    };

    for (label, actual, target) in [
        ("an element", list(&[0]), list(&[1])),
        ("a length", list(&[0]), list(&[1, 2])),
    ] {
        let verdict = solve_indices(&mut certifier(), &[actual], &[target], &[])
            .expect("the certifier answers");

        assert!(matches!(verdict, Invert::Impossible), "{label}");
    }
}
