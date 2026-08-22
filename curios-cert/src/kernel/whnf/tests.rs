use {
    super::unfold_rec,
    crate::{Kernel, whnf},
    curios_core::{
        Apply, Category, Cost, Free, Global, Intrinsic, Level, Many, Nat, ReduceError, Reducer,
        Scope, Subterm, Term, UniverseContext,
    },
    curios_utilities::{Grain, PackedBin, Qualifier},
};

/// The kernel every test starts from. The floor keeps the identities minted below out of the range the kernel mints from for eta-contraction, exactly as a real caller must seed it above the lowerer's and the elaborator's binders.
fn kernel() -> Kernel {
    let mut kernel = Kernel::new(1_000_000, crate::fixture::SYNTAX);
    kernel.set_local_floor(1_000);
    kernel
}

/// A test binder. Indices below the kernel's floor, so they cannot alias one it mints itself.
fn binder(index: u32, hint: &str) -> Free {
    Free::local(index, Some(hint))
}

fn nat(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}

fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

fn monomorphic() -> UniverseContext {
    UniverseContext::default()
}

fn polymorphic() -> UniverseContext {
    UniverseContext {
        parameter_count: 1,
        ..Default::default()
    }
}

#[test]
fn beta_opens_a_function_over_its_arguments() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let term = Term::apply(
        Term::func([(x.clone(), nat_type())], Term::free_var(&x)),
        [nat(7)],
    );

    assert_eq!(whnf(&mut kernel, term), Ok(nat(7)));
}

#[test]
fn delta_unfolds_a_monomorphic_definition() {
    let mut kernel = kernel();
    let f = binder(0, "f");
    kernel.define(&f, &nat_type(), &nat(3), &monomorphic());

    assert_eq!(whnf(&mut kernel, Term::free_var(&f)), Ok(nat(3)));
}

/// A definition generalized over universe parameters denotes no particular instance, so a bare occurrence of it is a normal form. Unfolding it here would silently pick an instance nobody stated.
#[test]
fn delta_withholds_a_universe_polymorphic_definition() {
    let mut kernel = kernel();
    let f = binder(0, "f");
    kernel.define(&f, &nat_type(), &nat(3), &polymorphic());

    let occurrence = Term::free_var(&f);
    assert_eq!(whnf(&mut kernel, occurrence.clone()), Ok(occurrence));
}

/// The same definition *does* unfold through a stated instance, which is the one position that names which one it is.
#[test]
fn a_universe_instance_unfolds_what_a_bare_occurrence_withholds() {
    let mut kernel = kernel();
    let f = binder(0, "f");
    kernel.define(&f, &nat_type(), &nat(3), &polymorphic());

    let instance = Term::universe_inst(Term::free_var(&f), vec![Level::zero()]);
    assert_eq!(whnf(&mut kernel, instance), Ok(nat(3)));
}

#[test]
fn an_undefined_variable_is_its_own_normal_form() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let occurrence = Term::free_var(&x);
    assert_eq!(whnf(&mut kernel, occurrence.clone()), Ok(occurrence));
}

/// Zeta. The second binding refers to the first, so this also pins the left-to-right order: `y` must see `x`'s value, not `x` itself.
#[test]
fn zeta_substitutes_let_bindings_left_to_right() {
    let mut kernel = kernel();
    let x = binder(0, "x");
    let y = binder(1, "y");

    let term = Term::let_(
        &x,
        nat_type(),
        nat(2),
        Term::let_(
            &y,
            nat_type(),
            Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), nat(3))),
            Term::free_var(&y),
        ),
    );

    assert_eq!(whnf(&mut kernel, term), Ok(nat(5)));
}

/// The intrinsic folds are shared with the elaborator through `Reducer`; this is the kernel reaching them with its own strategy underneath.
#[test]
fn intrinsics_fold_through_the_reducer_seam() {
    let mut kernel = kernel();
    let x = binder(0, "x");
    kernel.define(&x, &nat_type(), &nat(2), &monomorphic());

    let term = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), nat(2)));

    assert_eq!(whnf(&mut kernel, term), Ok(nat(4)));
}

#[test]
fn iota_selects_an_inductive_arm_and_binds_its_payload() {
    let mut kernel = kernel();
    let motive = binder(0, "m");
    let payload = binder(1, "a");

    let term = Term::induct_match(
        Term::variant(
            Global::Authored(Qualifier::from(["E"])),
            Vec::<Term>::new(),
            "some",
            [nat(42)],
        ),
        Some(&motive),
        nat_type(),
        [
            ("none", Vec::<Free>::new(), nat(0)),
            ("some", vec![payload.clone()], Term::free_var(&payload)),
        ],
    );

    assert_eq!(whnf(&mut kernel, term), Ok(nat(42)));
}

#[test]
fn iota_peels_one_successor_off_a_nat() {
    let mut kernel = kernel();
    let motive = binder(0, "m");
    let pred = binder(1, "pred");
    let hypothesis = binder(2, "ih");

    // `match 3 { 0 => 0 | succ(pred, _) => pred }` peels exactly one layer.
    let term = Term::nat_match(
        nat(3),
        Some(&motive),
        nat_type(),
        nat(0),
        &pred,
        &hypothesis,
        Term::free_var(&pred),
    );

    assert_eq!(whnf(&mut kernel, term), Ok(nat(2)));
}

#[test]
fn a_switch_takes_the_case_a_literal_names() {
    let mut kernel = kernel();
    let motive = binder(0, "m");

    let term = Term::switch(
        nat(2),
        Some(&motive),
        nat_type(),
        [(1u32, nat(10)), (2, nat(20))],
        nat(99),
    );

    assert_eq!(whnf(&mut kernel, term), Ok(nat(20)));
}

#[test]
fn a_switch_falls_through_to_its_default() {
    let mut kernel = kernel();
    let motive = binder(0, "m");

    let term = Term::switch(
        nat(7),
        Some(&motive),
        nat_type(),
        [(1u32, nat(10)), (2, nat(20))],
        nat(99),
    );

    assert_eq!(whnf(&mut kernel, term), Ok(nat(99)));
}

/// A symbolic scrutinee decides nothing, so the switch rebuilds as the neutral term it is rather than guessing an arm.
#[test]
fn a_switch_on_a_symbolic_scrutinee_stays_stuck() {
    let mut kernel = kernel();
    let motive = binder(0, "m");
    let n = binder(1, "n");

    let term = Term::switch(
        Term::free_var(&n),
        Some(&motive),
        nat_type(),
        [(1u32, nat(10))],
        nat(99),
    );

    let reduced = whnf(&mut kernel, term).expect("a stuck term still reduces");
    assert!(matches!(&*reduced, Subterm::Match(_)));
}

#[test]
fn a_bool_match_dispatches_on_a_literal() {
    let mut kernel = kernel();
    let motive = binder(0, "m");

    let term = Term::bool_match(
        Term::intrinsic(Intrinsic::Bool(true)),
        Some(&motive),
        nat_type(),
        nat(0),
        nat(1),
    );

    assert_eq!(whnf(&mut kernel, term), Ok(nat(1)));
}

#[test]
fn projection_selects_a_tuple_field() {
    let mut kernel = kernel();

    let term = Term::proj(Term::tuple([nat(10), nat(20), nat(30)]), 1);

    assert_eq!(whnf(&mut kernel, term), Ok(nat(20)));
}

/// A constructor is projected through the flat runtime view `(tag, payload...)`, so field 1 is payload component 0 — unlike a struct, which has no tag to skip.
#[test]
fn projection_skips_a_variants_tag_but_not_a_structs() {
    let mut kernel = kernel();
    let name = Global::Authored(Qualifier::from(["E"]));

    let variant = Term::proj(
        Term::variant(name.clone(), Vec::<Term>::new(), "some", [nat(42)]),
        1,
    );
    assert_eq!(whnf(&mut kernel, variant), Ok(nat(42)));

    let struct_ = Term::proj(Term::struct_(name, Vec::<Term>::new(), [nat(42)]), 0);
    assert_eq!(whnf(&mut kernel, struct_), Ok(nat(42)));
}

#[test]
fn eta_contracts_a_function_that_only_forwards() {
    let mut kernel = kernel();
    let x = binder(0, "x");
    let f = binder(1, "f");

    let term = Term::func(
        [(x.clone(), nat_type())],
        Term::apply(Term::free_var(&f), [Term::free_var(&x)]),
    );

    assert_eq!(whnf(&mut kernel, term), Ok(Term::free_var(&f)));
}

/// The side condition is load-bearing: contracting `(x) => x(x)` would move an occurrence of `x` out from under the binder that gives it meaning.
#[test]
fn eta_declines_when_the_head_mentions_the_binder() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let term = Term::func(
        [(x.clone(), nat_type())],
        Term::apply(Term::free_var(&x), [Term::free_var(&x)]),
    );

    assert_eq!(whnf(&mut kernel, term.clone()), Ok(term));
}

/// A recursive call keeps its folded spelling until an eliminator demands the value, which is what stops an occurrence from unfolding forever.
#[test]
fn a_recursive_application_stays_folded_until_forced() {
    let mut kernel = kernel();
    let n = binder(0, "n");
    let motive = binder(1, "m");
    let pred = binder(2, "pred");
    let hypothesis = binder(3, "ih");
    let countdown = binder(4, "countdown");
    let x = binder(5, "x");

    let body = Term::func(
        [(n.clone(), nat_type())],
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
        countdown.clone(),
        Term::func_type([(n.clone(), nat_type())], nat_type()),
        body,
    )];

    // Applied to a symbolic argument, it reduces to a folded recursive call.
    let symbolic = Term::rec(
        group.clone(),
        Term::apply(Term::free_var(&countdown), [Term::free_var(&x)]),
    );
    let Subterm::Rec(rec) = Term::unwrap_or_clone(symbolic) else {
        unreachable!("built as a rec")
    };
    let reduced = whnf(&mut kernel, unfold_rec(rec)).expect("ordinary reduction terminates");
    assert!(matches!(
        &*reduced,
        Subterm::Apply(Apply { head, .. }) if head.as_rec_proj().is_some()
    ));

    // Applied to a literal, forcing runs it to the end.
    let concrete = Term::rec(group, Term::apply(Term::free_var(&countdown), [nat(3)]));
    assert_eq!(kernel.reduce_forced(concrete), Ok(nat(0)));
}

/// The kernel is not strongly normalizing, and the budget is what makes every judgment terminate anyway. A group that consumes nothing spins until it runs out, which is an answer rather than a hang.
#[test]
fn a_non_productive_recursion_exhausts_the_budget() {
    let mut kernel = Kernel::new(1_000, crate::fixture::SYNTAX);
    kernel.set_local_floor(1_000);
    let loop_ = binder(0, "loop");
    let n = binder(1, "n");

    let body = Term::func(
        [(n.clone(), nat_type())],
        Term::apply(Term::free_var(&loop_), [Term::free_var(&n)]),
    );

    let term = Term::rec(
        [(
            loop_.clone(),
            Term::func_type([(n.clone(), nat_type())], nat_type()),
            body,
        )],
        Term::apply(Term::free_var(&loop_), [nat(1)]),
    );

    assert!(
        kernel
            .reduce_forced(term)
            .is_err_and(|spent| spent.is_exhausted())
    );
}

/// Each judgment gets the full budget back, so one expensive declaration cannot starve the next.
///
/// An undefined variable costs exactly one *step* — it is looked at once and is already normal — on top of the one guarded level the reduction enters. So the smallest budget that affords exactly one reduction is a frame plus a step, spelled from the constants rather than as a number, and what the second and third calls do is entirely about the refill.
///
/// The frame is charged per new *peak* depth, so a second reduction at the same depth would be free of it — which is why the refill matters here twice over: `restore_budget` resets the peak as well as the budget, so the second call pays for its level again exactly as the first did.
#[test]
fn restoring_the_budget_refills_it() {
    let mut kernel = Kernel::new(Cost::FRAME.get() + Cost::STEP.get(), crate::fixture::SYNTAX);
    kernel.set_local_floor(1_000);
    let x = binder(0, "x");
    let occurrence = Term::free_var(&x);

    assert_eq!(
        whnf(&mut kernel, occurrence.clone()),
        Ok(occurrence.clone())
    );
    assert!(whnf(&mut kernel, occurrence.clone()).is_err_and(|spent| spent.is_exhausted()));

    kernel.restore_budget();
    assert_eq!(whnf(&mut kernel, occurrence.clone()), Ok(occurrence));
}

/// A remembered reduct is the same answer the term would compute — including across a scope boundary, which a local-free key cannot observe.
#[test]
fn a_memoized_unfold_answers_the_same_across_scopes() {
    let mut kernel = kernel();
    let name = binder(0, "two");
    kernel.define(
        &name,
        &nat_type(),
        &Term::intrinsic(Intrinsic::nat_add(nat(1), nat(1))),
        &UniverseContext::default(),
    );

    let inside = kernel.scoped(|kernel| {
        let binder_ = binder(1, "x");
        kernel.assume(&binder_, &nat_type());

        kernel
            .reduce_forced(Term::free_var(&name))
            .expect("reduces")
    });

    let outside = kernel
        .reduce_forced(Term::free_var(&name))
        .expect("reduces");
    assert_eq!(inside, outside);
    assert_eq!(outside, nat(2));
}

/// Redefining a name clears every memo, so validity is by construction rather than by an append-only assumption.
#[test]
fn a_redefinition_clears_the_memos() {
    let mut kernel = kernel();
    let name = binder(0, "n");

    kernel.define(&name, &nat_type(), &nat(1), &UniverseContext::default());
    assert_eq!(
        kernel
            .reduce_forced(Term::free_var(&name))
            .expect("reduces"),
        nat(1),
    );

    kernel.define(&name, &nat_type(), &nat(2), &UniverseContext::default());
    assert_eq!(
        kernel
            .reduce_forced(Term::free_var(&name))
            .expect("reduces"),
        nat(2),
    );
}

/// A case equation lives exactly as long as its arm.
///
/// Every arm rule brackets its work in `mark`/`retract`, and the reducer consults these at stuck heads — so an equation outliving its bracket is a definitional equality between two terms that are not equal, applied to everything checked after it. The bracket is a truncation to a recorded length; this is what holds it to that.
#[test]
fn a_case_equation_does_not_outlive_its_scope() {
    let mut kernel = kernel();
    let scrutinee = binder(1, "n");
    kernel.assume(&scrutinee, &nat_type());
    // Local-free scrutinees are deliberately not recorded, so the key has to mention a local.
    let stuck = Term::free_var(&scrutinee);

    kernel.scoped(|kernel| {
        kernel.refine(stuck.clone(), nat(0));
        assert_eq!(kernel.refinement_of(&stuck), Some(nat(0)));
    });
    assert_eq!(kernel.refinement_of(&stuck), None);
}

/// What reducing `term` costs `kernel`, read off the remaining budget on either side.
fn spent(kernel: &mut Kernel, term: Term) -> u64 {
    let (before, _) = kernel.consumption();
    kernel.reduce_forced(term).expect("reduces");
    let (after, _) = kernel.consumption();

    before - after
}

/// A closed arithmetic tree `links` deep. Local-free, so it is a term the `whnf`/`forced` tables may key on — and one the closed machine takes, at machine depth.
fn chain(links: usize) -> Term {
    (0..links).fold(nat(0), |accumulator, _| {
        Term::intrinsic(Intrinsic::nat_add(accumulator, nat(1)))
    })
}

/// The same tree over an open tip, which the closed machine's gate declines — the term that still exercises the recursive strategy and the depth it prices.
fn open_chain(links: usize, tip: &Free) -> Term {
    (0..links).fold(Term::free_var(tip), |accumulator, _| {
        Term::intrinsic(Intrinsic::nat_add(accumulator, nat(1)))
    })
}

/// A term-keyed memo hit spends nothing, so the same closed term reduced twice within one declaration costs its full price once and O(1) after.
///
/// This is what a hit charging the recorded cost of the computation it replaces got wrong. That charge prices what a memo-free evaluator would have spent rather than what this kernel did, and recorded costs compound — a subterm hit twice per level makes the charge exponential in a structure the memos evaluate linearly — which is how a 262 144-step budget came to be declared exhausted after 6 547 actual reduction steps.
#[test]
fn a_repeated_reduction_within_one_declaration_is_free() {
    let mut kernel = kernel();
    let term = chain(64);

    let first = spent(&mut kernel, term.clone());
    let second = spent(&mut kernel, term);

    assert!(first > 1, "the first reduction is the one that pays");
    assert_eq!(second, 0);
}

/// A free hit is only deterministic because the tables it reads live exactly as long as the budget does. Restoring one discards the other, so what a declaration spends is decided by the declaration rather than by which declarations were walked before it.
#[test]
fn restoring_the_budget_forgets_the_term_keyed_memos() {
    let mut kernel = kernel();
    let term = chain(64);

    let first = spent(&mut kernel, term.clone());
    kernel.restore_budget();
    let after_boundary = spent(&mut kernel, term);

    assert_eq!(after_boundary, first);
}

/// The name-keyed table is the one that is *not* cleared at a declaration boundary, and it stays charged for exactly that reason: an entry outliving a declaration may not also be free, or which declarations came first would decide what this one can afford.
///
/// So a hit costs what computing the body cost, and the second occurrence spends what the first did less the peak-depth rule's discount — the first call's [`Cost::FRAME`] for a level that is no longer a new peak — and less the warmth of the reduct's own memo entry, which the first call stored and the second call hits. Before the closed machine the discount was exactly one frame; the machine's run replaces the reduct's re-derivation with a table hit too, so the equation is stated as the two bounds that survive either evaluator: the hit is charged the bulk of what it replaces, and the discount never exceeds a frame plus the follow-on warmth.
///
/// That near-equality is also why the table's *survival* cannot be asserted here: a charged hit and a recomputation are nearly the same number by construction, and only the wall clock separates them.
#[test]
fn an_unfold_hit_is_charged_what_it_replaces() {
    let mut kernel = kernel();
    let name = binder(0, "chain");
    kernel.define(&name, &nat_type(), &chain(64), &monomorphic());
    let occurrence = Term::free_var(&name);

    let first = spent(&mut kernel, occurrence.clone());
    let second = spent(&mut kernel, occurrence);

    assert!(first > 1, "computing the body is what the first call pays");
    assert!(
        second > first / 2,
        "the hit is charged what it replaces: {second} against {first}"
    );
    assert!(
        first - second <= Cost::FRAME.get() + 64,
        "the discount is the un-repeated peak frame plus follow-on warmth: {second} against {first}"
    );
}

/// Memoization may only *reduce* what a judgment spends. That is what makes free hits monotone against the kernel that shipped before them — no program that certified then can stop certifying now — and it is the half of the old bit-identical invariant this design keeps: a semantic refusal is budget-independent, so only an exhaustion point can move, and it can only move later.
///
/// The subject reduces the same closed term twice in *separate* calls, so the inequality is strict: the memoized kernel's second call is a table hit where the uncached kernel runs the machine again. Repetition inside one call would no longer separate them, because the machine's own run-scoped values are a memo both kernels get.
#[test]
fn cached_spend_never_exceeds_uncached() {
    let repeated = chain(32);

    let mut cached = kernel();
    let mut uncached = Kernel::uncached(1_000_000, crate::fixture::SYNTAX);
    uncached.set_local_floor(1_000);

    let with_memos = spent(&mut cached, repeated.clone()) + spent(&mut cached, repeated.clone());
    let without = spent(&mut uncached, repeated.clone()) + spent(&mut uncached, repeated);

    assert!(with_memos < without, "{with_memos} against {without}");
}

/// Reduce an open term and a closed one inside an arm refining `n` to `0`, then both again after the arm retracts: the two inside reducts followed by the two outside ones.
fn across_an_arm(kernel: &mut Kernel) -> [Term; 4] {
    let n = binder(1, "n");
    kernel.assume(&n, &nat_type());

    let open = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat(1)));
    let closed = chain(8);

    let (inside_open, inside_closed) = kernel.scoped(|kernel| {
        kernel.refine(Term::free_var(&n), nat(0));

        (
            kernel.reduce_forced(open.clone()).expect("reduces"),
            kernel.reduce_forced(closed.clone()).expect("reduces"),
        )
    });

    [
        inside_open,
        inside_closed,
        kernel.reduce_forced(open).expect("reduces"),
        kernel.reduce_forced(closed).expect("reduces"),
    ]
}

/// An arm's case equation reaches the reduct and not the table.
///
/// This is the load-bearing half of the memos' first invariant, and it is a claim held in one component about another: [`Memos::storable`](super::super::Memos) admits only a *local-free* term, while [`Scope::refine`](super::super::Scope) records only a *local-bearing* scrutinee, so the two sets are disjoint and no remembered reduct can rest on an equation later retracted. What stood behind that pair was `curios-prelude-archive`'s `kernel_memo_parity`, which averages the whole prelude rather than aiming at the interlock — coverage by corpus, the standard the perimeter declines to accept elsewhere.
///
/// Both terms are needed and they check different halves. The open one is the equation's subject: inside the arm it reduces to `1` where nothing outside makes it anything but stuck, so the retraction has something to fail to survive — without that inequality the assertion below would hold of a kernel that had never refined anything. The closed one crosses the *other* gate: `machine_admissible` declines the closed machine while any equation is live, so its inside reduct comes from the recursive strategy, and the outside call — where the machine would otherwise run — is served by the table entry that strategy stored. Both routes have to reach the same value as a kernel that never entered the arm at all, which is what `control` is.
///
/// The whole sequence then runs again with the memos off, which is the parity half: with nothing remembered, an equation that leaked into a table cannot leak, so the two kernels agreeing on all four reducts is the property `kernel_memo_parity` asserts over the prelude, asked here of terms chosen to reach the gate.
///
/// Mutation-checked: dropping the local-free test from `Memos::storable` remembers the arm's answer for the open term under the `forced` table, and the outside reduction hands back `1` where the stuck successor of `n` is what the term reduces to — failing at the retraction assertion below and leaving the closed half green. Two cost fixtures in this module move under the same mutation, `an_unfold_hit_is_charged_what_it_replaces` and `restoring_the_budget_refills_it`; both are resource assertions, and this is the only one that sees the false equation.
#[test]
fn a_case_equation_reaches_the_reduct_and_not_the_memos() {
    let mut cached = kernel();
    let [inside_open, inside_closed, outside_open, outside_closed] = across_an_arm(&mut cached);

    // The control: the same two terms under a kernel that never assumed an equation, which is what both outside reducts have to be.
    let mut control = kernel();
    let n = binder(1, "n");
    control.assume(&n, &nat_type());
    let untouched_open = control
        .reduce_forced(Term::intrinsic(Intrinsic::nat_add(
            Term::free_var(&n),
            nat(1),
        )))
        .expect("reduces");
    let untouched_closed = control.reduce_forced(chain(8)).expect("reduces");

    assert_eq!(inside_open, nat(1), "the equation answers the open term");
    assert_ne!(
        inside_open, untouched_open,
        "the equation has to change the open term, or the retraction below proves nothing"
    );
    assert_eq!(
        outside_open, untouched_open,
        "the arm's answer did not outlive the arm"
    );
    assert_eq!(
        inside_closed, untouched_closed,
        "the strategy and the machine agree on the closed term across the gate between them"
    );
    assert_eq!(outside_closed, untouched_closed);

    let mut uncached = Kernel::uncached(1_000_000, crate::fixture::SYNTAX);
    uncached.set_local_floor(1_000);

    assert_eq!(
        across_an_arm(&mut uncached),
        [inside_open, inside_closed, outside_open, outside_closed],
        "the memos changed no reduct on either side of the arm"
    );
}

/// The probe before decomposition is what makes an equation's answer independent of affording the reduction it spares.
///
/// The key is stored as written — the shape `assume_case_value`'s error path records, and the shape whose folding the early ask exists to spare — over an accumulation this budget cannot fold. The subject answers the case value in one step; the control differs from it only in never assuming the equation, and exhausts on the same term under the same budget, which is the demonstration that the subject's answer did not come from the reduction. Without the control, the assertion would hold of a budget that simply afforded the fold.
///
/// Mutation-checked against the probe points one at a time: with the ask before decomposition removed from `whnf_within`, the subject exhausted exactly as the control does, so this is the fixture that distinguishes that point; with the ask at the stuck reduct removed instead, it still passed, which is `the_two_consultation_points_answer_one_equation_alike`'s half to see.
#[test]
fn a_case_equation_answers_a_term_the_budget_cannot_reduce() {
    let budget = Cost::FRAME.get() * 4;
    let n = binder(1, "n");
    let key = Term::intrinsic(Intrinsic::nat_add(chain(100_000), Term::free_var(&n)));

    let mut control = Kernel::new(budget, crate::fixture::SYNTAX);
    control.set_local_floor(1_000);
    control.assume(&n, &nat_type());
    assert!(
        whnf(&mut control, key.clone()).is_err_and(|spent| spent.is_exhausted()),
        "the fold has to be unaffordable, or the subject's answer proves nothing"
    );

    let mut subject = Kernel::new(budget, crate::fixture::SYNTAX);
    subject.set_local_floor(1_000);
    subject.assume(&n, &nat_type());
    let answered = subject.scoped(|kernel| {
        kernel.refine(key.clone(), nat(0));
        whnf(kernel, key.clone())
    });

    assert_eq!(answered, Ok(nat(0)));
}

/// The two consultation points answer one equation alike, and the match between them is structural — universe instances included, since the key stopped being a universe-erased projection.
///
/// The key is taken as the kernel's own reduct of a written term whose inner operand folds, so it is a spelling that exists only as a reduct: the written form can reach it through reduction alone, which is what makes the ask at the stuck value the only probe that can see it. The control kernel — the same two reductions with no equation assumed — pins the two premises the subject rests on: the key differs from the written spelling, so the two probes below genuinely take different routes to it, and the key re-reduces to itself, which is the idempotence argument for merging the points in executable form.
///
/// Mutation-checked the other way around from `a_case_equation_answers_a_term_the_budget_cannot_reduce`: with the ask at the stuck reduct removed from `whnf_within`, the written probe handed back the unrefined key, so this is the fixture that distinguishes that point; with the ask before decomposition removed instead, both probes still answered, the key's own spelling being re-derived by decomposition and caught at the reduct. Neither pre-existing case-equation fixture moved under either mutation, which is the coverage gap this pair was written to close.
#[test]
fn the_two_consultation_points_answer_one_equation_alike() {
    let n = binder(1, "n");
    let written = Term::intrinsic(Intrinsic::nat_add(
        Term::free_var(&n),
        Term::intrinsic(Intrinsic::nat_add(nat(30), nat(34))),
    ));

    let mut control = kernel();
    control.assume(&n, &nat_type());
    let key = whnf(&mut control, written.clone()).expect("reduces");
    assert_ne!(
        key, written,
        "the inner operand has to fold, or the two probes below are one probe"
    );
    assert_eq!(
        whnf(&mut control, key.clone()),
        Ok(key.clone()),
        "a stored key is a normal form, so re-reducing it is identity"
    );

    let mut subject = kernel();
    subject.assume(&n, &nat_type());
    let (at_key, at_written) = subject.scoped(|kernel| {
        kernel.refine(key.clone(), nat(0));

        (whnf(kernel, key.clone()), whnf(kernel, written.clone()))
    });

    assert_eq!(at_key, Ok(nat(0)), "the term is the key: the early ask");
    assert_eq!(
        at_written,
        Ok(nat(0)),
        "reduction reaches the key: the ask at the stuck reduct"
    );
}

/// The escalation: an equation recorded as written still answers the spelling only reduction reaches.
///
/// This is the half of the two-tier key that keeping the *written* spelling alone would lose, and losing it is not hypothetical — keying the kernel on the written form and stopping there refused prelude items whose decided propositions no longer collapsed to `True`. Here the subject probes with the equation's reduct, which the written spelling cannot match; the answer comes from a reduced spelling `refined_reduct` settled on demand, at the stuck-reduct probe point, because that is where a spelling reduction produced arrives.
///
/// The control fixes the premise the fixture rests on: the written form has to reduce to something else, or the probe below would hit the written spelling and this would be testing the first tier over again.
///
/// Mutation-checked three ways, all of which return the unrefined `n + 64`. Removing the escalation from `refined_reduct` leaves only the written spelling, which does not match. Narrowing `Scope::hide_refinements_from` to withhold nothing — or to withhold the equations inside the settling one but not it — makes the settlement meet its own equation at the reducer's first probe and settle the reduced spelling to the case value it was assuming, which no reduct will ever equal. None of the three moved [`a_local_free_term_is_never_refined`], [`a_case_equation_reaches_the_reduct_and_not_the_memos`] or either pre-existing consultation-point fixture.
#[test]
fn a_case_equation_answers_a_spelling_only_reduction_reaches() {
    let n = binder(1, "n");
    let written = Term::intrinsic(Intrinsic::nat_add(
        Term::free_var(&n),
        Term::intrinsic(Intrinsic::nat_add(nat(30), nat(34))),
    ));

    let mut control = kernel();
    control.assume(&n, &nat_type());
    let reduct = whnf(&mut control, written.clone()).expect("reduces");
    assert_ne!(
        reduct, written,
        "the inner operand has to fold, or the probe below is the written spelling's"
    );

    let mut subject = kernel();
    subject.assume(&n, &nat_type());
    let answered = subject.scoped(|kernel| {
        kernel.refine(written.clone(), nat(0));

        whnf(kernel, reduct.clone())
    });

    assert_eq!(answered, Ok(nat(0)));
}

/// Settling an equation's reduced spelling withholds every equation assumed *inside* it, and not only the equation itself.
///
/// **This is what makes a deferred reduction mean what an eager one meant.** The reduction this replaced ran at registration, when the equations inside the arm did not exist yet and the stack below it was already frozen; running it later has to reconstruct that view, or the reduct rests on an equation that retracts before the entry holding it does — a remembered spelling outliving its own justification.
///
/// The subject nests an inner equation over `n + 1` inside an outer one, then probes with the outer equation's true reduct — what a kernel that never assumed the inner one computes, which is what `control` is for. Reaching it means the settlement did not consult the inner equation.
///
/// Mutation-checked with the mutation that moves this fixture and nothing else: leaving `Scope::unasked_refinement`'s reading of the limit alone — so the loop still terminates — while the two probes skip only the equation being settled. The inner equation then fires inside the outer's reduction, which answers `5 + 64` and settles the outer spelling to `69`, so the probe misses and the unrefined reduct comes back. The three coarser mutations recorded on [`a_case_equation_answers_a_spelling_only_reduction_reaches`] move this fixture too, and none of them separates the two halves of what withholding does; this one does. Relaxing *both* readings instead sends the settlement back into the entry it is already settling, which is `Scope::unasked_refinement`'s second job and not this fixture's.
#[test]
fn settling_a_reduced_spelling_withholds_the_equations_inside_it() {
    let n = binder(1, "n");
    let inner = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat(1)));
    let outer = Term::intrinsic(Intrinsic::nat_add(
        inner.clone(),
        Term::intrinsic(Intrinsic::nat_add(nat(30), nat(34))),
    ));

    let mut control = kernel();
    control.assume(&n, &nat_type());
    let reduct = whnf(&mut control, outer.clone()).expect("reduces");
    assert_ne!(
        reduct, outer,
        "the written spelling has to fold, or the probe below is the written spelling's"
    );

    let mut subject = kernel();
    subject.assume(&n, &nat_type());
    let answered = subject.scoped(|kernel| {
        kernel.refine(outer.clone(), nat(0));

        kernel.scoped(|kernel| {
            kernel.refine(inner.clone(), nat(5));

            whnf(kernel, reduct.clone())
        })
    });

    assert_eq!(answered, Ok(nat(0)));
}

/// A local-free term is never refined, however an equation's reduced spelling settles.
///
/// **The interlock's other half, and the one the written key does not cover.** `Scope::refine` admits only a local-bearing scrutinee, so nothing an equation is *recorded* under can collide with what the evaluation memos store; a reduced spelling is whatever reduction returned, and can perfectly well be local-free. What holds the line there is `refined_reduct`'s gate: a term with no local free is not probed at all, so it cannot be answered by an equation and its remembered reduct cannot outlive the arm.
///
/// Both sides are asserted for the reason `a_case_equation_reaches_the_reduct_and_not_the_memos` asserts both: the inside reduct is what a leak would corrupt, and the outside one is what a leaked memo entry would then hand back.
///
/// Mutation-checked: dropping the gate settles `konst(n)`'s reduced spelling to `7` and refines the local-free `konst(1)` to `0`, inside the arm and — through the memo entry that reduction stores — outside it as well. It is the only mutation in this module's set that moves this fixture, it moves no other new one, and it also moves [`a_case_equation_answers_a_term_the_budget_cannot_reduce`], whose deliberately tiny budget then goes on a settlement no probe asked for.
#[test]
fn a_local_free_term_is_never_refined() {
    let n = binder(1, "n");
    let konst = binder(2, "konst");
    let x = binder(3, "x");

    let mut kernel = kernel();
    kernel.define(
        &konst,
        &Term::func_type([(x.clone(), nat_type())], nat_type()),
        &Term::func([(x.clone(), nat_type())], nat(7)),
        &monomorphic(),
    );
    kernel.assume(&n, &nat_type());

    let open = Term::apply(Term::free_var(&konst), [Term::free_var(&n)]);
    let closed = Term::apply(Term::free_var(&konst), [nat(1)]);

    let inside = kernel.scoped(|kernel| {
        kernel.refine(open.clone(), nat(0));

        whnf(kernel, closed.clone())
    });
    let outside = whnf(&mut kernel, closed);

    assert_eq!(inside, Ok(nat(7)), "the equation is not this term's");
    assert_eq!(outside, Ok(nat(7)), "and nothing remembered says otherwise");
}

/// A reduct that *drops* a local is still reached, which is the direction `Scope::could_reduce_to` must not be strict in.
///
/// The filter deciding whether a settlement is worth performing tests that the candidate's locals are a *subset* of the key's, and subset rather than equality is the whole of what it can afford to claim: reduction may drop a local — the second projection of two, an argument a body ignores — while it can never introduce one, since it substitutes only closed definition bodies and subterms of the term it is reducing. Reading the test as equality, or as "the candidate mentions every local the key does", loses exactly this equation, and loses it silently: the arm simply stops refining.
///
/// `second(n, m)` mentions both binders and reduces to `m` alone, so the candidate's locals are a strict subset of the key's. That is a shape a filter tightened by one word would refuse.
///
/// Mutation-checked with the tightening itself — the subset read as set equality — which moves this fixture and no other. Relaxing the filter the other way, to admit everything, moves nothing at all: it is a cost filter, and what says it is doing its job is `curios`' `scrutinee_refinement_measurements` rather than any assertion here.
#[test]
fn a_reduct_that_drops_a_local_is_still_reached() {
    let n = binder(1, "n");
    let m = binder(2, "m");
    let second = binder(3, "second");
    let x = binder(4, "x");
    let y = binder(5, "y");

    let mut kernel = kernel();
    kernel.define(
        &second,
        &Term::func_type(
            [(x.clone(), nat_type()), (y.clone(), nat_type())],
            nat_type(),
        ),
        &Term::func(
            [(x.clone(), nat_type()), (y.clone(), nat_type())],
            Term::free_var(&y),
        ),
        &monomorphic(),
    );
    kernel.assume(&n, &nat_type());
    kernel.assume(&m, &nat_type());

    let written = Term::apply(
        Term::free_var(&second),
        [Term::free_var(&n), Term::free_var(&m)],
    );

    let answered = kernel.scoped(|kernel| {
        kernel.refine(written.clone(), nat(0));

        whnf(kernel, Term::free_var(&m))
    });

    assert_eq!(answered, Ok(nat(0)));
}

/// Depth is refused by the counter, and the refusal says so. Before the frame row, a reduction driven deep took real stack and the budget observed none of it — `recurse` grows rather than aborting, so what bounded depth was the host's memory rather than anything the program could be told about.
///
/// The subject is a chain of nested intrinsic operands over an *open* tip — a term the closed machine's gate declines, so the recursive strategy re-enters reduction once per link and the budget affords a handful of levels and no more. The closed twin of this chain no longer trips the row at all, which is the machine's whole yield and is asserted by its own tests.
#[test]
fn a_deep_reduction_is_refused_and_the_refusal_names_depth() {
    let mut kernel = Kernel::new(Cost::FRAME.get() * 4, crate::fixture::SYNTAX);
    kernel.set_local_floor(1_000);
    let tip = binder(0, "tip");

    let refusal = kernel
        .reduce_forced(open_chain(64, &tip))
        .expect_err("four frames do not afford sixty-four levels");

    assert!(
        matches!(
            refusal,
            ReduceError::Exhausted {
                category: Category::Depth,
                ..
            }
        ),
        "expected a depth refusal, got {refusal:?}"
    );
}

/// The retention quota degrades the cache rather than refusing the program: an allowance too small for any entry leaves every reduction correct and every one of them cold.
///
/// Correctness is the assertion, and the coldness is what makes it worth making — with retention exhausted the second reduction re-derives instead of hitting, so this is the same term reduced twice by two different routes to the same answer.
#[test]
fn an_exhausted_retention_quota_leaves_the_answer_alone() {
    let mut warm = kernel();
    let mut cold = Kernel::with_retention(1_000_000, 0, crate::fixture::SYNTAX);
    cold.set_local_floor(1_000);

    let term = chain(64);
    let expected = warm.reduce_forced(term.clone()).expect("reduces");

    assert_eq!(cold.reduce_forced(term.clone()), Ok(expected.clone()));
    assert_eq!(cold.reduce_forced(term), Ok(expected));
    assert_eq!(cold.retained(), 0, "nothing was admitted, so nothing spent");
}

/// A cold cache costs *work*, which is the warmth dependence the specification states rather than claims away: the second reduction of a term the memos declined to keep re-derives it, where a warm one hands it back for nothing.
///
/// What the cold second reduction does *not* re-pay is the frames, and that is the peak-depth rule showing through rather than an exception to it — the first reduction already reached that depth, so re-reaching it is free. The re-derivation it does pay is the transitions and the construction, which is what makes the two figures below differ by more than rounding.
#[test]
fn a_declined_insertion_costs_the_next_reduction_a_re_derivation() {
    let term = chain(64);

    let mut warm = kernel();
    spent(&mut warm, term.clone());
    let warm_again = spent(&mut warm, term.clone());

    let mut cold = Kernel::with_retention(1_000_000, 0, crate::fixture::SYNTAX);
    cold.set_local_floor(1_000);
    spent(&mut cold, term.clone());
    let cold_again = spent(&mut cold, term);

    assert_eq!(warm_again, 0, "a retained entry is hit for nothing");
    assert!(
        cold_again > 0,
        "a declined entry is not there to be hit, so the work happens again"
    );
}

/// A boolean operation reduces its right operand only once its left is a literal. The left here is a local, so the right — a fold that would answer `true` — is handed back as written; with the left `true`, the same right folds and so does the whole. This is the rule that keeps weak-head reduction of a `&&`/`||` tree from being its full normalization, which on a web of predicate definitions naming each other twice was `2^n` under every demand — see `reduce_bool_binary`.
#[test]
fn a_stuck_left_operand_leaves_the_right_as_written() {
    let mut kernel = kernel();
    let x = binder(0, "x");
    kernel.assume(&x, &Term::intrinsic(Intrinsic::BoolType));
    let literal = |value: bool| Term::intrinsic(Intrinsic::Bool(value));
    let folds = Term::intrinsic(Intrinsic::BoolAnd(literal(true), literal(true)));

    let stuck = Term::intrinsic(Intrinsic::BoolAnd(Term::free_var(&x), folds.clone()));
    assert_eq!(
        whnf(&mut kernel, stuck.clone()),
        Ok(stuck),
        "a stuck left settles the fold, and the right is not reduced for an answer it cannot change"
    );

    let open = Term::intrinsic(Intrinsic::BoolAnd(literal(true), folds));
    assert_eq!(
        whnf(&mut kernel, open),
        Ok(literal(true)),
        "a literal left reads the right and folds"
    );
}

/// A global name handed to a closed function stays a name in what the machine hands back, exactly as it does under the strategy: `twice(g)` at a plain demand is `(x) => g(g(x))` with `g` *named*, not `g`'s body substituted twice. The machine evaluated every beta argument and substituted its value, which on a function-valued global inlined the definition once per occurrence — and a web of definitions each naming the one before it twice came back as a graph whose tree was `2^n`, retained by the unfold memo and opened as a tree by the strategy's own beta. The strategy substitutes the argument as written, so the two reducts were never identical here, and this fixture is the one that sees it.
#[test]
fn the_closed_machine_keeps_a_global_argument_as_a_name() {
    let g = Free::global(Qualifier::from(["g"]));
    let twice = Free::global(Qualifier::from(["twice"]));
    let (x, f) = (binder(0, "x"), binder(1, "f"));
    let unary = Term::func_type([(x.clone(), nat_type())], nat_type());

    let define = |kernel: &mut Kernel| {
        kernel.define(
            &g,
            &unary,
            &Term::func(
                [(x.clone(), nat_type())],
                Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), nat(1))),
            ),
            &monomorphic(),
        );
        kernel.define(
            &twice,
            &Term::func_type([(f.clone(), unary.clone())], unary.clone()),
            &Term::func(
                [(f.clone(), unary.clone())],
                Term::func(
                    [(x.clone(), nat_type())],
                    Term::apply(
                        Term::free_var(&f),
                        [Term::apply(Term::free_var(&f), [Term::free_var(&x)])],
                    ),
                ),
            ),
            &monomorphic(),
        );
    };

    let term = Term::apply(Term::free_var(&twice), [Term::free_var(&g)]);

    let mut machined = kernel();
    define(&mut machined);
    let mut strategy = strategy_kernel();
    define(&mut strategy);

    let reduct = machined.reduce(term.clone()).expect("the machine reduces");
    assert_eq!(
        Some(reduct.clone()),
        strategy.reduce(term).ok(),
        "the machine and the strategy agree on the function handed back"
    );
    assert!(
        reduct.mentions_free(&g),
        "the argument survives as the name it was passed as:\n{reduct}"
    );
}

/// The strategy arm of the differential fixture below: the ordinary test kernel with its closed machine off, so every closed term is walked by the recursive strategy. Beside its one consumer on purpose — nothing else may evaluate with the machine disabled.
fn strategy_kernel() -> Kernel {
    let mut kernel = kernel();
    kernel.machine = false;
    kernel
}

/// **The differential fixture the machine's perimeter entry names.** The same closed terms are put to a kernel with the closed machine and to one without it — the recursive strategy — and the reducts must be identical, term for term, **at both demands**. The battery covers each rule the machine implements on its own: beta over eagerly-evaluated arguments, zeta's left-to-right release, all four match families, projection, recursive unfolding to a value, and the two fold recursion encodings over a packed carrier. Both evaluators determine these completely — a first-order value at the forced demand, and at the plain one either that or the folded spelling the demand stops at — so equality here is syntactic rather than up-to-anything.
///
/// It asked `reduce_forced` alone until the plain demand was found to be where the machine and the strategy could disagree, on `forced_then_plain` below. Every recursive term in the battery before it is a `rec` block that both evaluators leave unopened at a plain demand, so the comparison ran but reached nothing.
#[test]
fn the_closed_machine_agrees_with_the_strategy() {
    let bin_type = Term::intrinsic(Intrinsic::BinType(Grain::X));
    let bytes =
        |data: Vec<u8>| Term::intrinsic(Intrinsic::Bin(Grain::X, PackedBin::from_bytes(data)));
    let motive = || {
        let m = binder(100, "m");
        Scope::close(Many(1), &[&m], nat_type())
    };

    let ih_fold = {
        let (h, t, ih) = (binder(0, "h"), binder(1, "t"), binder(2, "ih"));
        Term::bin_match_scoped(
            Grain::X,
            bytes(vec![7; 40]),
            motive(),
            nat(0),
            &h,
            &t,
            &ih,
            Term::intrinsic(Intrinsic::nat_add(Term::free_var(&ih), nat(1))),
        )
    };

    let tail_fold = {
        let (go, acc, b) = (binder(0, "go"), binder(1, "acc"), binder(2, "b"));
        let (h, t, ih) = (binder(3, "h"), binder(4, "t"), binder(5, "ih"));
        let body = Term::func(
            [(acc.clone(), nat_type()), (b.clone(), bin_type.clone())],
            Term::bin_match_scoped(
                Grain::X,
                Term::free_var(&b),
                motive(),
                Term::free_var(&acc),
                &h,
                &t,
                &ih,
                Term::apply(
                    Term::free_var(&go),
                    [
                        Term::intrinsic(Intrinsic::nat_add(
                            Term::free_var(&acc),
                            Term::intrinsic(Intrinsic::ByteToNat(Term::free_var(&h))),
                        )),
                        Term::free_var(&t),
                    ],
                ),
            ),
        );
        Term::rec(
            [(
                go.clone(),
                Term::func_type(
                    [(acc.clone(), nat_type()), (b.clone(), bin_type.clone())],
                    nat_type(),
                ),
                body,
            )],
            Term::apply(Term::free_var(&go), [nat(0), bytes(vec![3; 40])]),
        )
    };

    let countdown = {
        let (n, motive_b) = (binder(0, "n"), binder(1, "m"));
        let (pred, hypothesis, member) = (binder(2, "pred"), binder(3, "ih"), binder(4, "member"));
        let body = Term::func(
            [(n.clone(), nat_type())],
            Term::nat_match(
                Term::free_var(&n),
                Some(&motive_b),
                nat_type(),
                nat(0),
                &pred,
                &hypothesis,
                Term::apply(Term::free_var(&member), [Term::free_var(&pred)]),
            ),
        );
        Term::rec(
            [(
                member.clone(),
                Term::func_type([(n.clone(), nat_type())], nat_type()),
                body,
            )],
            Term::apply(Term::free_var(&member), [nat(9)]),
        )
    };

    let beta_zeta = {
        let (x, y) = (binder(0, "x"), binder(1, "y"));
        Term::apply(
            Term::func(
                [(x.clone(), nat_type()), (y.clone(), nat_type())],
                Term::let_(
                    &y,
                    nat_type(),
                    Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), nat(3))),
                    Term::free_var(&y),
                ),
            ),
            [nat(2), nat(0)],
        )
    };

    let switch = {
        let m = binder(0, "m");
        Term::switch(
            nat(2),
            Some(&m),
            nat_type(),
            [(1u32, nat(10)), (2, nat(20))],
            nat(99),
        )
    };

    let projection = Term::proj(Term::tuple([nat(10), nat(20), nat(30)]), 2);

    let induct = {
        let (m, payload) = (binder(0, "m"), binder(1, "a"));
        Term::induct_match(
            Term::variant(
                Global::Authored(Qualifier::from(["E"])),
                Vec::<Term>::new(),
                "some",
                [Term::intrinsic(Intrinsic::nat_add(nat(40), nat(2)))],
            ),
            Some(&m),
            nat_type(),
            [
                ("none", Vec::<Free>::new(), nat(0)),
                ("some", vec![payload.clone()], Term::free_var(&payload)),
            ],
        )
    };

    // A run that forces a *bare* member selection and then asks a plain demand for a call on the same member. Both demands are exercised in one term because the machine's value memo is run-scoped: the `let` value is an intrinsic operand, which is forced, and its tail is an ordinary application, which must come back folded. The memo is keyed on the term alone, so a projection recorded at the forced demand was answered to the plain probe, and the machine ran the whole fold where the strategy stops at the folded spelling.
    let forced_then_plain = {
        let (go, b, x) = (binder(0, "go"), binder(1, "b"), binder(6, "x"));
        let (h, t, ih) = (binder(2, "h"), binder(3, "t"), binder(4, "ih"));
        let body = Term::func(
            [(b.clone(), bin_type.clone())],
            Term::bin_match_scoped(
                Grain::X,
                Term::free_var(&b),
                motive(),
                nat(0),
                &h,
                &t,
                &ih,
                Term::apply(Term::free_var(&go), [Term::free_var(&t)]),
            ),
        );
        let Subterm::Rec(rec) = Term::unwrap_or_clone(Term::rec(
            [(
                go.clone(),
                Term::func_type([(b.clone(), bin_type.clone())], nat_type()),
                body,
            )],
            Term::let_(
                &x,
                nat_type(),
                Term::intrinsic(Intrinsic::nat_add(Term::free_var(&go), nat(1))),
                Term::apply(Term::free_var(&go), [bytes(vec![1, 2, 3])]),
            ),
        )) else {
            unreachable!("built as a rec")
        };

        unfold_rec(rec)
    };

    for term in [
        chain(64),
        ih_fold,
        tail_fold,
        countdown,
        beta_zeta,
        switch,
        projection,
        induct,
        forced_then_plain,
    ] {
        let mut machined = kernel();
        let mut strategy = strategy_kernel();

        assert_eq!(
            machined.reduce_forced(term.clone()),
            strategy.reduce_forced(term.clone()),
            "the machine and the strategy disagreed on {term}",
        );

        // The plain demand is a separate contract, not a weaker reading of the one above: it is where a folded recursive spelling is the answer rather than a step on the way to one, so a machine that unfolds here computes a value the strategy never offers. Asking only the forced demand left that whole half of the machine uncompared.
        let mut machined = kernel();
        let mut strategy = strategy_kernel();

        assert_eq!(
            machined.reduce(term.clone()),
            strategy.reduce(term.clone()),
            "the machine and the strategy disagreed at a plain demand on {term}",
        );
    }
}
