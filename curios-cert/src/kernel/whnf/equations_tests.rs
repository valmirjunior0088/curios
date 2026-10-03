//! Case equations inside an arm: what they answer, and how far out of their scope a remembered reduct may travel.

use {
    super::test_support::*,
    crate::{Kernel, whnf},
    curios_analysis::test_support::SYNTAX,
    curios_core::{Cost, Exhaustion, Free, Intrinsic, Reducer, Term},
    curios_utilities::Qualifier,
};

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
        kernel
            .refine(stuck.clone(), nat(0))
            .expect("the equation records");
        assert_eq!(kernel.refinement_of(&stuck), Some(nat(0)));
    });
    assert_eq!(kernel.refinement_of(&stuck), None);
}

/// An arm's case equation reaches the reduct and not the table.
///
/// This is the load-bearing half of the memos' first invariant, and it is a claim held in one component about another: the tables that outlive an arm hold only *local-free* terms, while [`Scope::refine`](super::super::Scope) records only a *local-bearing* scrutinee, so the two sets are disjoint and no remembered reduct that outlives an arm can rest on an equation it retracted — and the local-bearing tables, which may, are cleared with it. `curios-prelude-archive`'s `kernel_memo_parity` averages the whole prelude rather than aiming at the interlock — coverage by corpus, the standard the soundness board declines to accept elsewhere — so this aims at it.
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

    let mut uncached = Kernel::uncached(1_000_000, SYNTAX);

    assert_eq!(
        across_an_arm(&mut uncached),
        [inside_open, inside_closed, outside_open, outside_closed],
        "the memos changed no reduct on either side of the arm"
    );
}

/// The same interlock from the other side, which a local-bearing memo raises: a stuck reduct remembered *before* an arm must not answer inside it, where an equation has since made the term something else — and the arm's answer, remembered inside, must not answer after it.
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
            kernel
                .refine(Term::free_var(&n), nat(0))
                .expect("the equation records");
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

    let mut uncached = Kernel::uncached(1_000_000, SYNTAX);
    assert_eq!(
        sequence(&mut uncached),
        [before, inside, after],
        "the memos changed no reduct on any side of the arm"
    );
}

/// A local-bearing term is remembered for as long as the equations in force stand: the second reduction within a declaration spends nothing, exactly as a closed term's does. Without it, the web of definitions an index inversion forces — each naming the one before it twice, a local in every one — would be re-derived `2^n` times.
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

    let mut control = Kernel::new(budget, SYNTAX);
    control.assume(&n, &nat_type());
    assert!(
        whnf(&mut control, key.clone()).is_err_and(|spent| spent.is_exhausted()),
        "the fold has to be unaffordable, or the subject's answer proves nothing"
    );

    let mut subject = Kernel::new(budget, SYNTAX);
    subject.assume(&n, &nat_type());
    let answered = subject.scoped(|kernel| {
        kernel
            .refine(key.clone(), nat(0))
            .expect("the equation records");
        whnf(kernel, key.clone())
    });

    assert_eq!(answered, Ok(nat(0)));
}

/// The two consultation points answer one equation alike, and the match between them is structural — universe instances included, since the key is not a universe-erased projection.
///
/// The key is taken as the kernel's own reduct of a written term whose inner operand folds, so it is a spelling that exists only as a reduct: the written form can reach it through reduction alone, which is what makes the ask at the stuck value the only probe that can see it. The control kernel — the same two reductions with no equation assumed — pins the two premises the subject rests on: the key differs from the written spelling, so the two probes below genuinely take different routes to it, and the key re-reduces to itself, which is the idempotence argument for merging the points in executable form.
///
/// Mutation-checked the other way around from `a_case_equation_answers_a_term_the_budget_cannot_reduce`: with the ask at the stuck reduct removed from `whnf_within`, the written probe handed back the unrefined key, so this is the fixture that distinguishes that point; with the ask before decomposition removed instead, both probes still answered, the key's own spelling being re-derived by decomposition and caught at the reduct. Neither other case-equation fixture moved under either mutation, which is why this pair exists.
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
        kernel
            .refine(key.clone(), nat(0))
            .expect("the equation records");

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
/// This is the half of the two-tier key that keeping the *written* spelling alone would lose, and losing it is not hypothetical — keying on the written form alone refuses prelude items whose decided propositions then fail to collapse to `True`. Here the subject probes with the equation's reduct, which the written spelling cannot match; the answer comes from a reduced spelling `refined_reduct` settled on demand, at the stuck-reduct probe point, because that is where a spelling reduction produced arrives.
///
/// The control fixes the premise the fixture rests on: the written form has to reduce to something else, or the probe below would hit the written spelling and this would be testing the first tier over again.
///
/// Mutation-checked three ways, all of which return the unrefined `n + 64`. Removing the escalation from `refined_reduct` leaves only the written spelling, which does not match. Narrowing `Scope::hide_refinements_from` to withhold nothing — or to withhold the equations inside the settling one but not it — makes the settlement meet its own equation at the reducer's first probe and settle the reduced spelling to the case value it was assuming, which no reduct will ever equal. None of the three moved [`a_local_free_term_is_never_refined`], [`a_case_equation_reaches_the_reduct_and_not_the_memos`] or either consultation-point fixture.
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
        kernel
            .refine(written.clone(), nat(0))
            .expect("the equation records");

        whnf(kernel, reduct.clone())
    });

    assert_eq!(answered, Ok(nat(0)));
}

/// Settling an equation's reduced spelling withholds every equation assumed *inside* it, and not only the equation itself.
///
/// **This is what makes a deferred reduction mean what an eager one would.** A reduction at registration runs before the equations inside the arm exist, with the stack below it frozen; running it later has to reconstruct that view, or the reduct rests on an equation that retracts before the entry holding it does — a remembered spelling outliving its own justification.
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
        kernel
            .refine(outer.clone(), nat(0))
            .expect("the equation records");

        kernel.scoped(|kernel| {
            kernel
                .refine(inner.clone(), nat(5))
                .expect("the equation records");

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
        &Term::func_type([(x, nat_type())], nat_type()),
        &Term::func([(x, nat_type())], nat(7)),
        &monomorphic(),
    );
    kernel.assume(&n, &nat_type());

    let open = Term::apply(Term::free_var(&konst), [Term::free_var(&n)]);
    let closed = Term::apply(Term::free_var(&konst), [nat(1)]);

    let inside = kernel.scoped(|kernel| {
        kernel
            .refine(open.clone(), nat(0))
            .expect("the equation records");

        whnf(kernel, closed.clone())
    });
    let outside = whnf(&mut kernel, closed);

    assert_eq!(inside, Ok(nat(7)), "the equation is not this term's");
    assert_eq!(outside, Ok(nat(7)), "and nothing remembered says otherwise");
}

/// A closed term remembered before an arm answers inside it as a kernel with no memo would, which is the precondition of `whnf_within` asking the memo before the equations.
///
/// The memo's declaration-lived tables hold only local-free terms, and no equation is recorded under a local-free spelling (`records_case_equation`), so an entry can never stand in for an answer an equation in force would give. The fixture asks for exactly that: `konst(1)` reduced outside, then an arm that would equate `konst(1)` with `0` were the equation recorded, and the same term asked inside it — of a caching kernel and an uncaching one, which must agree.
///
/// Mutation-checked: recording the local-free equation while keeping the order has the caching kernel answer `7` from the entry it stored outside, and the uncaching one `0` from the equation, and the assertion sees them part.
#[test]
fn a_remembered_closed_term_answers_inside_an_arm_as_an_uncached_kernel_does() {
    let sequence = |kernel: &mut Kernel| {
        // A global, as a top-level definition is: a name `binder` mints is a local, and a term mentioning one is local-bearing however closed it reads.
        let konst = Free::global(Qualifier::from(["konst"]));
        let x = binder(3, "x");
        kernel.define(
            &konst,
            &Term::func_type([(x, nat_type())], nat_type()),
            &Term::func([(x, nat_type())], nat(7)),
            &monomorphic(),
        );
        let closed = Term::apply(Term::free_var(&konst), [nat(1)]);

        let before = whnf(kernel, closed.clone());
        let inside = kernel.scoped(|kernel| {
            kernel
                .refine(closed.clone(), nat(0))
                .expect("asking to record a local-free equation is not an error");
            whnf(kernel, closed.clone())
        });

        [before, inside]
    };

    let cached = sequence(&mut kernel());
    let mut uncached = Kernel::uncached(1_000_000, SYNTAX);

    assert_eq!(cached, [Ok(nat(7)), Ok(nat(7))]);
    assert_eq!(
        sequence(&mut uncached),
        cached,
        "the memo changed an answer inside the arm"
    );
}

/// A reduct that *drops* a local is still reached, which is the direction `curios_analysis::could_reduce_to` must not be strict in.
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
        &Term::func_type([(x, nat_type()), (y, nat_type())], nat_type()),
        &Term::func([(x, nat_type()), (y, nat_type())], Term::free_var(&y)),
        &monomorphic(),
    );
    kernel.assume(&n, &nat_type());
    kernel.assume(&m, &nat_type());

    let written = Term::apply(
        Term::free_var(&second),
        [Term::free_var(&n), Term::free_var(&m)],
    );

    let answered = kernel.scoped(|kernel| {
        kernel
            .refine(written.clone(), nat(0))
            .expect("the equation records");

        whnf(kernel, Term::free_var(&m))
    });

    assert_eq!(answered, Ok(nat(0)));
}

/// A guard's equation is recorded on the guard's written spelling, and the false arm of `n < m` is the fact `m <= n` read the other way: the reducer asks the dual spelling with the literal negated, and the record stays as written.
#[test]
fn a_comparison_refined_answers_its_dual_negated() {
    let mut kernel = kernel();
    let n = binder(1, "n");
    let m = binder(2, "m");
    kernel.assume(&n, &nat_type());
    kernel.assume(&m, &nat_type());
    let guard = Term::intrinsic(Intrinsic::nat_lt(Term::free_var(&n), Term::free_var(&m)));
    let dual = Term::intrinsic(Intrinsic::NatLe(Term::free_var(&m), Term::free_var(&n)));

    kernel.scoped(|kernel| {
        kernel
            .refine(guard.clone(), Term::intrinsic(Intrinsic::Bool(false)))
            .expect("the equation records");
        let reduced = whnf(kernel, dual.clone()).expect("a dual probe reduces");
        assert_eq!(reduced.as_bool(), Some(true));
        assert_eq!(
            kernel.refinement_of(&dual),
            None,
            "the record stays as written"
        );
    });
}

/// A guard's equation answers the same bound spelled across the `<`/`<=` seam.
///
/// `List/slice`'s bound arrives as `i + 1 <= len` where the guard deciding it was written `i < len`, and the two are one proposition on `Nat`: `whnf` retries a miss on the successor spelling, carrying the literal across rather than negating it as the dual retry does. Without it a guard discharges a bound only when the author spelled the comparison the way the standard library's signature happens to.
///
/// **Both operands are spelled as a program spells them, which is what the retry has to survive.** The guard's own operand is the *unreduced* call — a real one records `List/len(l)`, the application the author wrote, and the bound arrives with that call already folded to its intrinsic — so the seam is asked of the key's settled reduct rather than of the record. Asking the written spelling alone answers this fixture while a surface program is still refused, the kernel refusing what the elaborator accepts. The stand-in here is a definition the key mentions and the probe does not, which is the same asymmetry with nothing else in it.
#[test]
fn a_guard_answers_a_bound_spelled_across_the_successor_seam() {
    let index = binder(1, "i");
    let length = binder(2, "len");

    // The operand as a program writes it: a term reduction folds, standing for the `List/len(l)` call a guard records and the bound meets already folded.
    let written_length = Term::intrinsic(Intrinsic::nat_add(
        Term::free_var(&length),
        Term::intrinsic(Intrinsic::nat_add(nat(30), nat(34))),
    ));

    let below = Term::intrinsic(Intrinsic::nat_lt(
        Term::free_var(&index),
        written_length.clone(),
    ));
    // The bound as a signature instantiates it: `i + 1 <= len`, the successor written as the sum it is before reduction folds it into a floor.
    let within = Term::intrinsic(Intrinsic::nat_lte(
        Term::intrinsic(Intrinsic::nat_add(Term::free_var(&index), nat(1))),
        written_length.clone(),
    ));

    let mut control = kernel();
    control.assume(&index, &nat_type());
    control.assume(&length, &nat_type());
    let folded = whnf(&mut control, written_length.clone()).expect("the operand reduces");
    assert_ne!(
        folded, written_length,
        "the operand has to fold, or the probe never leaves the written spelling",
    );

    let mut kernel = kernel();
    kernel.assume(&index, &nat_type());
    kernel.assume(&length, &nat_type());

    kernel.scoped(|kernel| {
        kernel
            .refine(below.clone(), Term::intrinsic(Intrinsic::Bool(true)))
            .expect("the equation records");

        let reduced = whnf(kernel, within.clone()).expect("the bound reduces");
        assert_eq!(
            reduced,
            Term::intrinsic(Intrinsic::Bool(true)),
            "the guard `i < len` did not answer the bound `i + 1 <= len`",
        );
    });

    // Outside the arm it is stuck again: the seam is a second spelling to look under, never a fact of its own.
    let reduced = whnf(&mut kernel, within).expect("the bound reduces");
    assert!(
        reduced.as_bool().is_none(),
        "the successor spelling answered outside the arm that recorded the guard: {reduced:?}",
    );
}

/// A guard written through a dispatch answers its dual at the first probe, in the arm the decision procedure proves dead, exactly as the elaborator answers it there.
///
/// **This is the divergence the resolved spelling closes.** A comparison through a concept elaborates to an application — `(?w).1(a, hi)` — whose intrinsic shape only a reduction of its head exposes. Met only as the settled reduct of the written key — at the second probe point, after the decision procedure has folded the probe — that shape would be answered from the procedure, where the elaborator, which registers the dispatch-resolved spelling beside the written one and asks it first, answers it from the equation: in an arm whose guard is decided against its case value, a program the elaborator accepts would be refused by the kernel. `/std/Str/Valid`'s three-byte decoding meets it: its false arm of `cp % 4096 / 64 <= 63` asks `63 < cp % 4096 / 64`.
///
/// The stand-in for the dispatch is a witness holding the method, projected and applied as elaboration spells a concept call — a head that carries no name, which is the shape both checkers resolve — and the subject `n % 64` is one whose bound the procedure reads. The control pins the premise: with no equation, the procedure folds the probe to `false`, so the arm is dead and the subject's `true` can have come only from the equation, read the other way.
///
/// Mutation-checked: recording no resolved spelling in `Kernel::refine` answers the probe `false` — the procedure's fold, with the equation never met — and moves no other fixture in this module.
#[test]
fn a_dispatched_guard_answers_its_dual_before_the_procedure_folds_it() {
    let n = binder(1, "n");
    let witness = binder(2, "witness");
    let method = binder(3, "le");
    let a = binder(4, "a");
    let b = binder(5, "b");
    let positive = binder(6, "positive");

    let comparison = [(a, nat_type()), (b, nat_type())];
    let method_type = Term::func_type(comparison.clone(), Term::intrinsic(Intrinsic::BoolType));
    let mut kernel = kernel();
    kernel.define(
        &witness,
        &Term::tuple_type([(method, method_type)]),
        &Term::tuple([Term::func(
            comparison,
            Term::intrinsic(Intrinsic::nat_lte(Term::free_var(&a), Term::free_var(&b))),
        )]),
        &monomorphic(),
    );
    kernel.assume(&n, &nat_type());

    let subject = Term::intrinsic(Intrinsic::nat_rem(
        Term::free_var(&n),
        nat(64),
        Term::free_var(&positive),
    ));
    let guard = Term::apply(
        Term::proj(Term::free_var(&witness), 0),
        [subject.clone(), nat(63)],
    );
    let dual = Term::intrinsic(Intrinsic::nat_lt(nat(63), subject));

    assert_eq!(
        whnf(&mut kernel, dual.clone()),
        Ok(Term::intrinsic(Intrinsic::Bool(false))),
        "the procedure has to decide the probe, or the arm is not dead"
    );

    let answered = kernel.scoped(|kernel| {
        kernel
            .refine(guard, Term::intrinsic(Intrinsic::Bool(false)))
            .expect("the equation records");

        whnf(kernel, dual)
    });

    assert_eq!(answered, Ok(Term::intrinsic(Intrinsic::Bool(true))));
}

/// A resolved spelling that drops the scrutinee's locals is not recorded, so no local-free term is refined through it.
///
/// **The written key's gate does not cover it, and the probe that asks it is the one every term reaches.** Resolving a dispatch substitutes its arguments into the method's body, and a method that ignores its local argument resolves a local-bearing guard to a local-free comparison — here `(w.0)(n, 5)` to `5 <= 63`. Recorded, it would answer that comparison, a term that has nothing to do with `n`, at the probe before decomposition: `false` inside an arm where it is `true`, and through the memo entry that reduction stores, outside the arm as well. That is the interlock [`a_local_free_term_is_never_refined`] holds for the reduced spelling, held here for the resolved one.
///
/// Both sides are asserted for the reason that fixture asserts both: the inside reduct is what a recorded local-free key corrupts, and the outside one is what the leaked memo entry then hands back.
///
/// Mutation-checked: recording the resolved spelling unfiltered in `Scope::refine` refines `5 <= 63` to `false` inside the arm and outside it, and moves no other fixture in this module.
#[test]
fn a_resolved_spelling_that_drops_its_locals_is_never_recorded() {
    let n = binder(1, "n");
    let witness = binder(2, "witness");
    let method = binder(3, "le");
    let a = binder(4, "a");
    let b = binder(5, "b");

    let comparison = [(a, nat_type()), (b, nat_type())];
    let method_type = Term::func_type(comparison.clone(), Term::intrinsic(Intrinsic::BoolType));
    let mut kernel = kernel();
    // The method reads only its second operand, so the local the guard passes as the first is gone once it is opened.
    kernel.define(
        &witness,
        &Term::tuple_type([(method, method_type)]),
        &Term::tuple([Term::func(
            comparison,
            Term::intrinsic(Intrinsic::nat_lte(Term::free_var(&b), nat(63))),
        )]),
        &monomorphic(),
    );
    kernel.assume(&n, &nat_type());

    let guard = Term::apply(
        Term::proj(Term::free_var(&witness), 0),
        [Term::free_var(&n), nat(5)],
    );
    let closed = Term::intrinsic(Intrinsic::nat_lte(nat(5), nat(63)));

    let inside = kernel.scoped(|kernel| {
        kernel
            .refine(guard, Term::intrinsic(Intrinsic::Bool(false)))
            .expect("the equation records");

        whnf(kernel, closed.clone())
    });
    let outside = whnf(&mut kernel, closed);

    let true_ = Term::intrinsic(Intrinsic::Bool(true));
    assert_eq!(inside, Ok(true_.clone()), "the equation is not this term's");
    assert_eq!(outside, Ok(true_), "and nothing remembered says otherwise");
}

/// Resolving a dispatched guard forces none of its operands, so entering the arm costs no evaluation of what the guard compares.
///
/// **This is what makes recording the resolved spelling at entry affordable.** A guard is how a program avoids evaluating its subject — `i < len(b)` over a `b` built by an accumulation — and a resolution that reduced its operands would evaluate that subject at every arm that tests it, before anything asked. Only heads are reduced, so the guard's operand here, an accumulation the budget cannot fold, is carried into the resolved spelling as written; and the dual probe meets it there, so the arm answers without folding it either.
///
/// The control fixes the premise: the probe alone exhausts the same budget, so neither the recording nor the answer can have come from folding it.
///
/// Mutation-checked: reducing each argument before `resolved_spelling` opens the method exhausts the budget at `Kernel::refine`, and moves no other fixture in this module.
#[test]
fn resolving_a_dispatched_guard_forces_none_of_its_operands() {
    let budget = Cost::FRAME.get() * 4;
    let n = binder(1, "n");
    let witness = binder(2, "witness");
    let method = binder(3, "le");
    let a = binder(4, "a");
    let b = binder(5, "b");

    let comparison = [(a, nat_type()), (b, nat_type())];
    let method_type = Term::func_type(comparison.clone(), Term::intrinsic(Intrinsic::BoolType));
    let kernel_over = |budget| {
        let mut kernel = Kernel::new(budget, SYNTAX);
        kernel.define(
            &witness,
            &Term::tuple_type([(method, method_type.clone())]),
            &Term::tuple([Term::func(
                comparison.clone(),
                Term::intrinsic(Intrinsic::nat_lte(Term::free_var(&a), Term::free_var(&b))),
            )]),
            &monomorphic(),
        );
        kernel.assume(&n, &nat_type());
        kernel
    };

    let expensive = chain(100_000);
    let guard = Term::apply(
        Term::proj(Term::free_var(&witness), 0),
        [Term::free_var(&n), expensive.clone()],
    );
    let dual = Term::intrinsic(Intrinsic::nat_lt(expensive, Term::free_var(&n)));

    let mut control = kernel_over(budget);
    assert!(
        whnf(&mut control, dual.clone()).is_err_and(|spent| spent.is_exhausted()),
        "the operand has to be unaffordable, or the subject's answer proves nothing"
    );

    let mut subject = kernel_over(budget);
    let answered = subject.scoped(|kernel| {
        kernel
            .refine(guard, Term::intrinsic(Intrinsic::Bool(false)))
            .expect("resolving reduces no operand");

        whnf(kernel, dual)
    });

    assert_eq!(answered, Ok(Term::intrinsic(Intrinsic::Bool(true))));
}

/// The reduced spelling a refinement probe compares in reads each operand as a probe, as the elaborator's twin does: an operand with no value at the type level is kept as written, so the probe misses rather than refusing the judgment it serves.
#[test]
fn an_operand_with_no_value_is_compared_as_written() {
    let mut kernel = kernel();
    let x = binder(1, "x");
    kernel.assume(&x, &nat_type());
    let undefined = Term::intrinsic(Intrinsic::NatDiv {
        dividend: nat(1),
        divisor: nat(0),
        non_zero: Term::free_var(&x),
    });
    assert!(
        whnf(&mut kernel, undefined.clone()).is_err_and(|error| !error.is_exhausted()),
        "the operand has a value, so the probe is not put to the question"
    );
    let probe = Term::intrinsic(Intrinsic::NatEql(undefined, Term::free_var(&x)));

    assert_eq!(super::canonical_operands(&mut kernel, &probe), Ok(probe));
}
