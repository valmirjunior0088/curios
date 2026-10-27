//! Index forcings through a real checker: a binder forced twice, a binder or family out of reach, and the outer solution a pinned binder is rewritten through.

use {
    super::test_support::{declare, kernel},
    curios_analysis::{Invert, another_case, invert_indices, solve_indices},
    curios_core::{Free, Global, Intrinsic, Nat, Term},
    curios_utilities::Qualifier,
    std::slice,
};

/// The deletion rule (Goguen–McBride–McKinna) decides a binder forced in two index positions by *convertibility*, and this is the direction no program reaches.
///
/// Every re-forcing the corpus makes that the rule accepts has two *syntactically identical* forcings, so a plain equality test would decide every acceptance the prelude and the test programs contain. The semantic half of the rule — accepting two forcings that differ but convert — is exercised by no program, which is precisely the condition under which a rule's mistakes stay invisible.
///
/// So its answers are put to it directly. Every fixture forces one binder in the first index position and again in the second — the shape `refl(@z) : (z, z)` meets at `Eq()(a, b)` — and the first two differ in nothing but the family's sort, which is what makes them a control pair rather than two unrelated cases.
///
/// At a proposition the two forcings `a()` and `b()` convert by irrelevance, so the rule deletes the redundant constraint and one solution survives: sound because `Eq : Prop` makes the system definitionally K, and harmless because the surviving substitution is interchangeable with the one it replaced. At a relevant family the same two are constructors a program tells apart, and each forcing was reached by injective steps, so the case is reachable only if `a()` is `b()`: the verdict is `Impossible`, the conflict rule arriving through a non-linear target, and it is what lets `match h end` close a hypothesis `Eq()(false, true)`. The third fixture is the control on *that*: two forcings that do not convert and do not clash either — one opaque function at two arguments, which may well agree — are dropped, the arm still checked with the binder unsolved.
///
/// `Impossible` excuses an arm from being checked at all, and an arm excused wrongly at a `Prop`-sorted family is the vacuous-elimination route to a closed inhabitant of `False`, routed there from index inversion. So the proposition's verdict is the one this holds hardest: the clash is asked of the walk a linear position is put to, whose tag test is licensed by the family's sort, and only after conversion said no — and at a proposition conversion says yes.
#[test]
fn a_binder_forced_twice_survives_only_when_its_forcings_convert() {
    enum Verdict {
        Survives,
        Dropped,
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
            Verdict::Dropped,
        ),
    ] {
        let mut kernel = kernel();
        let family = declare(&mut kernel, "Forced", sort);
        let family_type = Term::induct_type(family, Vec::<Term>::new(), Vec::<Term>::new());
        let binder = Free::local(900, Some("p"));
        let opaque_function = Free::local(901, Some("g"));
        let argument = Free::local(902, Some("x"));

        kernel.assume(&binder, &family_type);
        kernel.assume(
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
            &mut kernel,
            &[inhabitant("a"), inhabitant("b")],
            &[target.clone(), target],
            slice::from_ref(&binder),
        )
        .expect("inversion is a total function of finished terms");

        let surviving = match (outcome, expected) {
            (Invert::Impossible, Verdict::Impossible) => continue,
            (Invert::Impossible, _) => panic!(
                "{label}: a re-forced binder was reported unreachable, which excuses the arm from being checked"
            ),
            (Invert::Solved(_), Verdict::Impossible) => {
                panic!("{label}: two forcings that clash left the case reachable")
            }
            (Invert::Solved(solutions), Verdict::Survives) => (solutions, 1),
            (Invert::Solved(solutions), Verdict::Dropped) => (solutions, 0),
        };
        let (solutions, count) = surviving;

        assert_eq!(
            solutions.len(),
            count,
            "{label}: the deletion rule kept {} of the two forcings",
            solutions.len(),
        );
        assert!(
            solutions.iter().all(|(bound, _)| *bound == binder),
            "{label}: a solution was recorded for a binder that was never forced",
        );
    }
}

/// The rule's third answer, which the pair above does not reach: a binder whose *type* is out of scope.
///
/// `consolidate` decides a re-forcing by asking the `Judge` for the binder's assumed type and comparing the two forcings at it. With no assumption there is nothing to compare at, so it drops the binder's solutions — the conservative direction, leaving the binder unsolved and the arm still checked. No program reaches the branch: both kernel callers reach `invert_indices` through `open_payload`, which assumes every binder it opens before the body runs, so from the kernel it is unreachable by construction.
///
/// It is pinned anyway, because the conservative answer is not the obvious one to write. `None => true` — no type, so nothing to disagree about, so keep the forcing — is the plausible slip, and it would accept a re-forcing that *no convertibility test ever decided*, which is the deletion rule discharging a constraint it never checked. Nothing else in the workspace would notice.
///
/// The discrimination is against the two constructor cases above rather than beside them: identical family, identical sort, identical forcings, and the binder assumed there and not here. At a proposition that one difference must turn one surviving solution into none. At a relevant family it must turn `Impossible` into none as well: the clash is asked only after conversion has said no, and with no type conversion was never asked, so the strong verdict — the one that excuses an arm from being checked — would be decided out of blindness, which is the direction no analysis on this seam takes.
#[test]
fn a_binder_whose_type_is_out_of_scope_drops_its_forcings() {
    for (label, sort) in [
        ("a proposition", Term::prop()),
        ("a relevant family", Term::type_ground()),
    ] {
        let mut kernel = kernel();
        let family = declare(&mut kernel, "Forced", sort);
        let binder = Free::local(900, Some("p"));

        // Deliberately not assumed: this is the whole of the fixture.

        let inhabitant = |tag| Term::variant(family, Vec::<Term>::new(), tag, Vec::<Term>::new());
        let target = Term::free_var(&binder);

        let outcome = invert_indices(
            &mut kernel,
            &[inhabitant("a"), inhabitant("b")],
            &[target.clone(), target],
            slice::from_ref(&binder),
        )
        .expect("inversion is a total function of finished terms");

        let Invert::Solved(solutions) = outcome else {
            panic!(
                "{label}: a binder with no assumption was reported unreachable, which excuses the arm from being checked"
            );
        };

        assert!(
            solutions.is_empty(),
            "{label}: a binder whose type is out of scope kept a forcing that no convertibility test decided",
        );
    }
}

/// The clash rule's license is the registry, and a family the registry cannot answer for must not clash.
///
/// Two different constructors of one family definitely clash only when the family is *relevant*: at a `Prop`-sorted family irrelevance makes every inhabitant the same value, so reading tag disjointness there contradicts conversion, and an arm excused as impossible on that reading is the vacuous-elimination route to a closed inhabitant of `False`. The sort is read out of `induct_decl`, so the whole of that protection rests on the lookup answering — and a lookup that answered nothing and fell through to the tag check would answer `Impossible`: the analysis deciding the strong verdict *because* it was blind, where every other blindness on this seam — an unknown positivity name, an undecodable size, an unassumed binder — turns into a refusal. The branch is unreachable from either checker as far as the walks are known, a well-typed variant's family being registered wherever the term came from, but the fixture above pins an equally unreachable branch for the same reason: the conservative answer is not the obvious one to write, and nothing else in the workspace would notice the wrong one.
///
/// The control pair holds what the rule must not move: the same position with the declaration present still clashes at a relevant sort, and still yields nothing at `Prop`.
#[test]
fn a_family_the_registry_cannot_answer_for_does_not_clash() {
    let inhabitant = |family: &Global, tag: &str| {
        Term::variant(*family, Vec::<Term>::new(), tag, Vec::<Term>::new())
    };

    // The subject: the family was never declared, so the sort read is blind.
    let mut blind = kernel();
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
        let mut sighted = kernel();
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

/// The outer direction reaches a variable inside an index, and only a local's.
///
/// A case whose target is `1`, met at an actual index `n + 1`, is reachable only when `n` is `0`: the peel reduces the pair to `0` against `n`, and the second direction solves the outer variable there. Both checkers get it from `solve_indices`. The control drops the one fact the rule reads from its driver: with `n` not a local the walk opened, it is a name no case can refine, and nothing is solved.
#[test]
fn the_outer_direction_solves_a_variable_inside_an_index() {
    let n = Free::local(900, Some("n"));
    let nat = |k: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(k)));
    let actual = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat(1)));

    for (label, local) in [("a local", true), ("a name no case refines", false)] {
        let mut kernel = kernel();
        if local {
            kernel.assume(&n, &Term::intrinsic(Intrinsic::NatType));
        }

        let Invert::Solved(solutions) =
            solve_indices(&mut kernel, slice::from_ref(&actual), &[nat(1)], &[])
                .expect("the kernel answers")
        else {
            panic!("{label}: a reachable case was reported unreachable");
        };

        match local {
            true => assert_eq!(solutions, vec![(n, nat(0))], "{label}"),
            false => assert!(solutions.is_empty(), "{label}: {solutions:?}"),
        }
    }
}

/// A binder the first direction pins is rewritten through what the second solves, so the arm's substitution is idempotent.
///
/// Actuals `(n + 1, n + 1)` against targets `(k, 1)`: the first direction pins `k := n + 1`, and the second learns `n := 0` from the other position. Applied one after the other the pair would leave `k` naming a variable the same substitution replaces; composed, `k`'s value no longer mentions `n` at all.
#[test]
fn a_pinned_binder_is_rewritten_through_the_outer_solution() {
    let mut kernel = kernel();
    let n = Free::local(900, Some("n"));
    let k = Free::local(901, Some("k"));
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let nat = |value: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(value)));
    kernel.assume(&n, &nat_type);
    kernel.assume(&k, &nat_type);

    let actual = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat(1)));
    let Invert::Solved(solutions) = solve_indices(
        &mut kernel,
        &[actual.clone(), actual],
        &[Term::free_var(&k), nat(1)],
        slice::from_ref(&k),
    )
    .expect("the kernel answers") else {
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
        "the pinned value still names what the substitution replaces: {pinned}"
    );
}

/// The outer direction's clash is held as the first direction's is: a local forced to two values a program tells apart leaves the case with no value, and one forced to two inhabitants of a proposition is forced to one value twice.
///
/// Actuals `(x, x)` against targets `(a(), b())`, for a case with no binder. The first direction reads a variable as what may yet be the target, and decides nothing. The second solves `x` to each target in turn. At a relevant family the two clash and the case is impossible, which is what leaves a written arm untyped by the kernel and may leave it out. At a proposition the same two convert, so the case stays reachable: read as a clash there, an arm would go unchecked on the strength of two proofs being told apart.
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
        let mut kernel = kernel();
        let family = declare(&mut kernel, "Forced", sort);
        let x = Free::local(900, Some("x"));
        kernel.assume(
            &x,
            &Term::induct_type(family, Vec::<Term>::new(), Vec::<Term>::new()),
        );

        let inhabitant = |tag| Term::variant(family, Vec::<Term>::new(), tag, Vec::<Term>::new());
        let actuals = [Term::free_var(&x), Term::free_var(&x)];
        let targets = [inhabitant("a"), inhabitant("b")];

        assert!(
            matches!(
                invert_indices(&mut kernel, &actuals, &targets, &[]),
                Ok(Invert::Solved(_))
            ),
            "{label}: the first direction decided what a variable may yet be",
        );

        let outcome =
            solve_indices(&mut kernel, &actuals, &targets, &[]).expect("the kernel answers");
        assert_eq!(matches!(outcome, Invert::Impossible), impossible, "{label}");
    }
}

/// A value is another case than an arm's where it names a case as it stands and the unifier that excuses an absent arm finds the two in a definite clash. Both checkers know an arm no reduction takes by this: the kernel types none, and the elaborator words its refusal of one.
///
/// The controls are what the rule must not answer: the arm's own case, a term that names no case as it stands — a variable, a successor over one — and two constructors of a proposition, which conversion holds to be one value.
#[test]
fn a_value_is_another_case_only_where_it_names_one_that_clashes() {
    let mut kernel = kernel();
    let relevant = declare(&mut kernel, "Relevant", Term::type_ground());
    let proposition = declare(&mut kernel, "Proposition", Term::prop());
    let n = Free::local(900, Some("n"));
    let pred = Free::local(901, Some("pred"));
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    kernel.assume(&n, &nat_type);
    kernel.assume(&pred, &nat_type);

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
            another_case(&mut kernel, &scrutinee, &case, &binders).expect("the kernel answers"),
            expected,
            "{label}",
        );
    }
}
