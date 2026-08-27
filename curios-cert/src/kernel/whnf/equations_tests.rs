//! Case equations inside an arm: what they answer, and how far out of their scope a remembered reduct may travel.

use {
    crate::{Kernel, whnf},
    curios_core::{Cost, Intrinsic, Reducer, Term},
};

use super::test_support::*;

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

/// An arm's case equation reaches the reduct and not the table.
///
/// This is the load-bearing half of the memos' first invariant, and it is a claim held in one component about another: the tables that outlive an arm hold only *local-free* terms, while [`Scope::refine`](super::super::Scope) records only a *local-bearing* scrutinee, so the two sets are disjoint and no remembered reduct that outlives an arm can rest on an equation it retracted — and the local-bearing tables, which may, are cleared with it. What stood behind that pair was `curios-prelude-archive`'s `kernel_memo_parity`, which averages the whole prelude rather than aiming at the interlock — coverage by corpus, the standard the perimeter declines to accept elsewhere.
///
/// Both terms are needed and they check different halves. The open one is the equation's subject: inside the arm it reduces to `1` where nothing outside makes it anything but stuck, so the retraction has something to fail to survive — without that inequality the assertion below would hold of a kernel that had never refined anything. The closed one crosses the *other* gate: `machine_admissible` declines the closed machine while any equation is live, so its inside reduct comes from the recursive strategy, and the outside call — where the machine would otherwise run — is served by the table entry that strategy stored. Both routes have to reach the same value as a kernel that never entered the arm at all, which is what `control` is.
///
/// The whole sequence then runs again with the memos off, which is the parity half: with nothing remembered, an equation that leaked into a table cannot leak, so the two kernels agreeing on all four reducts is the property `kernel_memo_parity` asserts over the prelude, asked here of terms chosen to reach the gate.
///
/// Mutation-checked: dropping the clear of the local-bearing tables at `Kernel::scoped`'s retract remembers the arm's answer for the open term under the `local_forced` table, and the outside reduction hands back `1` where the stuck successor of `n` is what the term reduces to — failing at the retraction assertion below and leaving the closed half green. The other direction, an entry from before the arm answering inside it, is `a_remembered_reduct_does_not_outlive_the_equations_it_was_taken_under`'s.
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

    let mut uncached = Kernel::uncached(1_000_000, crate::SYNTAX);
    uncached.set_local_floor(1_000);

    assert_eq!(
        across_an_arm(&mut uncached),
        [inside_open, inside_closed, outside_open, outside_closed],
        "the memos changed no reduct on either side of the arm"
    );
}

/// The same interlock from the other side, which the local-bearing memo made a question: a stuck reduct remembered *before* an arm must not answer inside it, where an equation has since made the term something else — and the arm's answer, remembered inside, must not answer after it.
///
/// This is the fixture for the rule that a local-bearing reduct lives exactly as long as the set of equations in force: `Memos::begin_equations` clears the local tables where an equation is assumed, where it is retracted, and around a settlement. The open term is reduced before the arm, inside it, and after it; the first and third are the stuck successor and the second is `1`, and the uncached kernel agrees on all three. Mutation-checked: dropping the clear at `Kernel::refine` answers the inside reduction from the entry the outside one stored, and the middle assertion is what sees it.
#[test]
fn a_remembered_reduct_does_not_outlive_the_equations_it_was_taken_under() {
    let sequence = |kernel: &mut Kernel| {
        let n = binder(1, "n");
        kernel.assume(&n, &nat_type());
        let open = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat(1)));

        let before = kernel.reduce_forced(open.clone()).expect("reduces");
        let inside = kernel.scoped(|kernel| {
            kernel.refine(Term::free_var(&n), nat(0));
            kernel.reduce_forced(open.clone()).expect("reduces")
        });
        let after = kernel.reduce_forced(open).expect("reduces");

        [before, inside, after]
    };

    let mut cached = kernel();
    let [before, inside, after] = sequence(&mut cached);

    assert_ne!(
        before, inside,
        "the equation has to change the open term, or the assertions below prove nothing"
    );
    assert_eq!(
        inside,
        nat(1),
        "the equation answers inside the arm, whatever was remembered before it"
    );
    assert_eq!(after, before, "and its answer does not outlive it");

    let mut uncached = Kernel::uncached(1_000_000, crate::SYNTAX);
    uncached.set_local_floor(1_000);
    assert_eq!(
        sequence(&mut uncached),
        [before, inside, after],
        "the memos changed no reduct on any side of the arm"
    );
}

/// A local-bearing term is remembered for as long as the equations in force stand: the second reduction within a declaration spends nothing, exactly as a closed term's does. This is what the web of definitions an index inversion forces — each naming the one before it twice, a local in every one — was re-derived `2^n` times for want of.
#[test]
fn a_local_bearing_reduct_is_a_free_hit_within_its_span() {
    let mut kernel = kernel();
    let n = binder(1, "n");
    kernel.assume(&n, &nat_type());
    let open = Term::intrinsic(Intrinsic::nat_add(chain(64), Term::free_var(&n)));

    let first = spent(&mut kernel, open.clone());
    let second = spent(&mut kernel, open);

    assert!(first > 1, "the first reduction does the work");
    assert_eq!(second, 0, "the second is remembered");
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

    let mut control = Kernel::new(budget, crate::SYNTAX);
    control.set_local_floor(1_000);
    control.assume(&n, &nat_type());
    assert!(
        whnf(&mut control, key.clone()).is_err_and(|spent| spent.is_exhausted()),
        "the fold has to be unaffordable, or the subject's answer proves nothing"
    );

    let mut subject = Kernel::new(budget, crate::SYNTAX);
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
