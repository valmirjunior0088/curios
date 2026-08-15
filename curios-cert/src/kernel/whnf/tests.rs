use {
    super::unfold_rec,
    crate::{Kernel, whnf},
    curios_core::{
        Apply, Cost, Free, Global, Intrinsic, Level, Nat, Reducer, Subterm, Term, UniverseContext,
    },
    curios_utilities::Qualifier,
};

/// The kernel every test starts from. The floor keeps the identities minted below out of the range the kernel mints from for eta-contraction, exactly as a real caller must seed it above the lowerer's and the elaborator's binders.
fn kernel() -> Kernel {
    let mut kernel = Kernel::new(1_000_000);
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
    let mut kernel = Kernel::new(1_000);
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
    let mut kernel = Kernel::new(Cost::FRAME.get() + Cost::STEP.get());
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

/// A closed arithmetic tree `links` deep. Local-free, so it is a term the `whnf`/`forced` tables may key on, and its cost is proportional to its depth.
fn chain(links: usize) -> Term {
    (0..links).fold(nat(0), |accumulator, _| {
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
/// So a hit costs what computing the body cost, and the second occurrence spends what the first did — **less exactly one frame**, which is the peak-depth rule showing through rather than an exception to it. The first call enters a guarded level that is deeper than any this judgment had reached and pays [`Cost::FRAME`] for it; the second re-enters the same depth, which is no longer a new peak and is therefore free. Everything the replay itself charges is identical, frames the *body* paid included, since those are part of the recorded cost.
///
/// That near-equality is also why the table's *survival* cannot be asserted here: a charged hit and a recomputation are the same number by construction, and only the wall clock separates them.
#[test]
fn an_unfold_hit_is_charged_what_it_replaces() {
    let mut kernel = kernel();
    let name = binder(0, "chain");
    kernel.define(&name, &nat_type(), &chain(64), &monomorphic());
    let occurrence = Term::free_var(&name);

    let first = spent(&mut kernel, occurrence.clone());
    let second = spent(&mut kernel, occurrence);

    assert!(first > 1, "computing the body is what the first call pays");
    assert_eq!(second, first - Cost::FRAME.get());
}

/// Memoization may only *reduce* what a judgment spends. That is what makes free hits monotone against the kernel that shipped before them — no program that certified then can stop certifying now — and it is the half of the old bit-identical invariant this design keeps: a semantic refusal is budget-independent, so only an exhaustion point can move, and it can only move later.
///
/// The subject repeats a closed subterm, so the inequality is strict rather than merely satisfied.
#[test]
fn cached_spend_never_exceeds_uncached() {
    let repeated = Term::intrinsic(Intrinsic::nat_add(chain(32), chain(32)));

    let mut cached = kernel();
    let mut uncached = Kernel::uncached(1_000_000);
    uncached.set_local_floor(1_000);

    let with_memos = spent(&mut cached, repeated.clone());
    let without = spent(&mut uncached, repeated);

    assert!(with_memos < without, "{with_memos} against {without}");
}
