use {
    crate::{Invert, Kernel, invert_indices},
    curios_base::{Qualifier, RootId},
    curios_core::{Atom, Free, Global, InductDecl, InductParam, Telescope, Term, UniverseContext},
    std::slice,
};

fn kernel() -> Kernel {
    let mut kernel = Kernel::new(100_000);
    kernel.set_local_floor(1_000);
    kernel
}

/// A nullary family at `result_sort` with two nullary constructors, `a` and `b`.
///
/// Two constructors is the point: it gives the family two closed inhabitants that are syntactically distinct, so whether they are *interchangeable* is decided by the family's sort and by nothing else.
fn declare(kernel: &mut Kernel, path: &str, result_sort: Term) -> Global {
    let family = Global::Authored(Qualifier::from([path]));

    kernel.declare_induct(
        &family,
        &InductDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::done(())),
            constructors: ["a", "b"]
                .into_iter()
                .map(|tag| {
                    (
                        Atom::from(tag),
                        InductParam {
                            telescope: Telescope::done(Vec::new()),
                            plicities: Vec::new(),
                        },
                    )
                })
                .collect(),
            result_sort,
            module: Qualifier::from([path]),
            root: RootId::Entry,
            rep_public: true,
            polarities: Vec::new(),
        },
    );

    family
}

/// The deletion rule (Goguen–McBride–McKinna) decides a binder forced in two index positions by *convertibility*, and this is the direction no program reaches.
///
/// Instrumenting `consolidate` and running the whole corpus — the fixed prelude through the kernel's own walk, plus every test program through both checkers — counts 5883 inversions, 20 of which re-force a binder. Sixteen refuse, on genuinely inconvertible `Bits` spines. The four that accept are all `prior == value`: the two forcings are *syntactically identical*, so a plain equality test would have decided every acceptance the corpus contains. The semantic half of the rule — accepting two forcings that differ but convert — is exercised by nothing, which is precisely the condition under which a rule's mistakes stay invisible. See DESIGN.md's note that (V)'s argument rule sat inert at 6010 of 6041 sites while the corpus passed throughout.
///
/// So the two directions are put to it directly. Both fixtures force one binder to `a()` in the first index position and to `b()` in the second, differing in nothing but the family's sort — which is what makes them a control pair rather than two unrelated cases.
///
/// At a proposition the two forcings convert by irrelevance, so the rule deletes the redundant constraint and one solution survives: sound because `Eq : Prop` makes the system definitionally K, and harmless because the surviving substitution is interchangeable with the one it replaced. At a relevant family they do not convert, and the binder's solutions are dropped rather than reconciled — the arm is still checked, with the binder simply unsolved.
///
/// What both must never be is `Impossible`. That verdict excuses an arm from being checked at all, and an arm excused wrongly at a `Prop`-sorted family is the vacuous-elimination route to a closed inhabitant of `False` that DESIGN.md records under index inversion. `consolidate` returns no clash by construction; this is what holds that to account, since the refusing path removes solutions and a future rewrite could as easily report the position unreachable.
#[test]
fn a_binder_forced_twice_survives_only_when_its_forcings_convert() {
    for (label, sort, surviving) in [
        (
            "a proposition, whose two inhabitants irrelevance identifies",
            Term::prop(),
            1,
        ),
        (
            "a relevant family, whose two inhabitants a program tells apart",
            Term::type_ground(),
            0,
        ),
    ] {
        let mut kernel = kernel();
        let family = declare(&mut kernel, "Forced", sort);
        let binder = Free::local(900, Some("p"));

        kernel.assume(
            &binder,
            &Term::induct_type(family.clone(), Vec::<Term>::new(), Vec::<Term>::new()),
        );

        let inhabitant =
            |tag| Term::variant(family.clone(), Vec::<Term>::new(), tag, Vec::<Term>::new());
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
                "{label}: a re-forced binder was reported unreachable, which excuses the arm from being checked"
            );
        };

        assert_eq!(
            solutions.len(),
            surviving,
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
/// `consolidate` decides a re-forcing by asking the `Judge` for the binder's assumed type and comparing the two forcings at it. With no assumption there is nothing to compare at, so it drops the binder's solutions — the conservative direction, leaving the binder unsolved and the arm still checked. Instrumenting that branch and running the fixed prelude through the kernel's own walk, every program in `curios`'s corpus through both checkers, and this crate's own tests counts it firing **zero** times in all three: both kernel callers reach `invert_indices` through `open_payload`, which assumes every binder it opens before the body runs, so from this crate the branch is unreachable by construction.
///
/// It is pinned anyway, because the conservative answer is not the obvious one to write. `None => true` — no type, so nothing to disagree about, so keep the forcing — is the plausible slip, and it would accept a re-forcing that *no convertibility test ever decided*, which is the deletion rule discharging a constraint it never checked. Nothing else in the workspace would notice.
///
/// The discrimination is against the proposition case above rather than beside it: identical family, identical sort, identical forcings, and the binder assumed there and not here. That one difference must turn one surviving solution into none. And as there, the verdict must stay `Solved` — `Impossible` excuses the arm from being checked, and an arm excused wrongly at a `Prop`-sorted family is the vacuous-elimination route to `False`.
#[test]
fn a_binder_whose_type_is_out_of_scope_drops_its_forcings() {
    let mut kernel = kernel();
    let family = declare(&mut kernel, "Forced", Term::prop());
    let binder = Free::local(900, Some("p"));

    // Deliberately not assumed: this is the whole of the fixture.

    let inhabitant =
        |tag| Term::variant(family.clone(), Vec::<Term>::new(), tag, Vec::<Term>::new());
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
            "a binder with no assumption was reported unreachable, which excuses the arm from being checked"
        );
    };

    assert!(
        solutions.is_empty(),
        "a binder whose type is out of scope kept a forcing that no convertibility test decided",
    );
}
