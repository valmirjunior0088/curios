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
/// This is the load-bearing half of the memos' first invariant, and it is a claim held in one component about another: the tables that outlive an arm hold only *local-free* terms, while [`Scope::refine`](super::super::Scope) records only a *local-bearing* scrutinee, so the two sets are disjoint and no remembered reduct that outlives an arm can rest on an equation it retracted — and the local-bearing tables, which may, are cleared with it. `curios-prelude`'s `kernel_memo_parity` averages the whole prelude rather than aiming at the interlock — coverage by corpus, the standard the soundness board declines to accept elsewhere — so this aims at it.
///
/// Both terms are needed and they check different halves. The open one is the equation's subject: inside the arm it reduces to `1` where nothing outside makes it anything but stuck, so the retraction has something to fail to survive — without that inequality the assertion below would hold of a kernel that had never refined anything. The closed one is the other half: no equation answers it, so the machine takes it inside the arm as it does outside, and the outside call is served by the table entry the inside run stored. Both have to reach the same value as a kernel that never entered the arm at all, which is what `control` is.
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

/// A quotient whose only local is the proof of its bound names a local, and its reduct lives as such a term's does: an equation answers it inside its arm alone. A fold reads no proof, so `6 / 2` under the proof `p` folds to `3` as it does under any proof, and only the free-variable walk, which hands the proof of a bound out beside the operands (`Intrinsic::for_each_operand`), says the term is not local-free. Counted local-free, it would take the closed machine, file its reduct in the tables that live the declaration and record no equation, so the arm's `0` would never be its answer.
///
/// The quotient is reduced before the arm, inside it and after it, as `a_remembered_reduct_does_not_outlive_the_equations_it_was_taken_under` reduces its open term, and the uncached kernel agrees on all three. Mutation-checked: with `Intrinsic::for_each_operand` handing a bound's proof to no reader, the quotient reads `3` inside the arm and the assertion on `inside` is what sees it; the same mutation fails six of `kernel::infer`'s intrinsic and signature tests, where `operands` comes up one short of its row.
#[test]
fn a_quotient_naming_a_local_only_in_its_bound_is_refined_within_its_arm_alone() {
    let sequence = |kernel: &mut Kernel| {
        let bound = binder(1, "bound");
        let p = binder(2, "p");
        kernel.assume(&bound, &Term::prop());
        kernel.assume(&p, &Term::free_var(&bound));
        let quotient = Term::intrinsic(Intrinsic::NatDiv {
            dividend: nat(6),
            divisor: nat(2),
            non_zero: Term::free_var(&p),
        });

        let before = kernel.reduce_forced(quotient.clone()).expect("reduces");
        let inside = kernel.scoped(|kernel| {
            kernel
                .refine(quotient.clone(), nat(0))
                .expect("the equation records");
            kernel.reduce_forced(quotient.clone()).expect("reduces")
        });
        let after = kernel.reduce_forced(quotient).expect("reduces");

        [before, inside, after]
    };

    let mut cached = kernel();
    let [before, inside, after] = sequence(&mut cached);

    assert_eq!(before, nat(3), "a fold reads no proof");
    assert_eq!(
        inside,
        nat(0),
        "the proof's local makes the quotient a term an equation answers"
    );
    assert_eq!(after, before, "and its answer does not outlive the arm");

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
    kernel
        .define(
            &konst,
            &Term::func_type([(x, nat_type())], nat_type()),
            &Term::func([(x, nat_type())], nat(7)),
            &monomorphic(),
        )
        .expect("a name is declared once");
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
        kernel
            .define(
                &konst,
                &Term::func_type([(x, nat_type())], nat_type()),
                &Term::func([(x, nat_type())], nat(7)),
                &monomorphic(),
            )
            .expect("a name is declared once");
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
    kernel
        .define(
            &second,
            &Term::func_type([(x, nat_type()), (y, nat_type())], nat_type()),
            &Term::func([(x, nat_type()), (y, nat_type())], Term::free_var(&y)),
            &monomorphic(),
        )
        .expect("a name is declared once");
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

/// An arm that records no equation changes nothing a remembered answer read. A scrutinee naming no local records none — and an arm on a variable substitutes its case through its body, so every match beneath it on that variable scrutinizes a literal — so what was reduced before such an arm answers inside it and after it, for nothing.
///
/// Mutation-checked: with the scoped tables cleared at every arm whether or not it recorded an equation, the reduction inside the arm is paid for again.
#[test]
fn an_arm_that_records_no_equation_leaves_what_was_remembered() {
    let mut kernel = kernel();
    let n = binder(1, "n");
    kernel.assume(&n, &nat_type());
    let open = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat(1)));

    let before = spent(&mut kernel, open.clone());
    let inside = kernel.scoped(|kernel| {
        kernel
            .refine(nat(0), nat(0))
            .expect("a literal scrutinee resolves nothing");
        spent(kernel, open.clone())
    });
    let after = spent(&mut kernel, open);

    assert!(before > 0, "the first reduction is the one that pays");
    assert_eq!(inside, 0);
    assert_eq!(after, 0);
}

/// An equation in force follows the solution an arm is checked under.
///
/// The kernel reads an arm with its case's solution substituted, so under `match f(n) < 5` a second match on `n` reads its `0` arm with `f(0) < 5` where the program wrote `f(n) < 5`, and the guard's equation, keyed on the term that names `n`, would answer nothing the arm holds — the kernel refusing a fact the elaborator, which refines `n` in a store and keeps it spelled, accepts. Restated, the equation answers the instance for as long as the arm stands, and as it was recorded once the arm is gone.
///
/// Mutation-checked three ways. With `Scope::restate` assuming nothing the instance stays stuck inside the arm. With the recorded equation left answering beside its restatement, the recorded spelling still answers there. With `Scope::retract` reviving nothing, the recorded spelling answers nothing after the arm.
#[test]
fn a_case_equation_follows_the_solution_an_arm_is_checked_under() {
    let n = binder(1, "n");
    let f = binder(2, "f");
    let x = binder(3, "x");
    let guard = |argument: Term| {
        Term::intrinsic(Intrinsic::nat_lt(
            Term::apply(Term::free_var(&f), [argument]),
            nat(5),
        ))
    };
    let written = guard(Term::free_var(&n));
    let at_zero = guard(nat(0));

    let mut kernel = kernel();
    kernel.assume(&n, &nat_type());
    kernel.assume(&f, &Term::func_type([(x, nat_type())], nat_type()));

    kernel.scoped(|kernel| {
        kernel
            .refine(written.clone(), Term::intrinsic(Intrinsic::Bool(true)))
            .expect("the equation records");
        assert_eq!(
            whnf(kernel, at_zero.clone()).expect("reduces").as_bool(),
            None,
            "the instance is no term the guard was written as, or the arm below proves nothing"
        );

        kernel.scoped(|kernel| {
            kernel.restate_refinements(&[(n, nat(0))]);

            assert_eq!(
                whnf(kernel, at_zero.clone()).expect("reduces").as_bool(),
                Some(true),
                "the equation answers at the arm's solution"
            );
            assert_eq!(
                kernel.refinement_of(&written),
                None,
                "and the recorded spelling steps aside for it"
            );
        });

        assert_eq!(
            whnf(kernel, at_zero.clone()).expect("reduces").as_bool(),
            None,
            "the restatement does not outlive the arm"
        );
        assert_eq!(
            kernel
                .refinement_of(&written)
                .and_then(|value| value.as_bool()),
            Some(true),
            "and the recorded equation answers again"
        );
    });
}

/// A restated equation is recorded by the rule every equation is: one the solution leaves naming no local is not.
///
/// `g` is a name with no body and no local in it, so `g(n) < 5` at `n := 0` is local-free, and an equation about a local-free term would leave the evaluation memos an entry resting on an equation the arm's exit retracts (`curios_analysis::records_case_equation`). The instance therefore stays the stuck term it is.
#[test]
fn a_restated_equation_naming_no_local_is_not_recorded() {
    let n = binder(1, "n");
    let g = Free::global(Qualifier::from(["g"]));
    let x = binder(3, "x");
    let guard = |argument: Term| {
        Term::intrinsic(Intrinsic::nat_lt(
            Term::apply(Term::free_var(&g), [argument]),
            nat(5),
        ))
    };
    let written = guard(Term::free_var(&n));
    let at_zero = guard(nat(0));

    let mut kernel = kernel();
    kernel.assume(&n, &nat_type());
    kernel.assume(&g, &Term::func_type([(x, nat_type())], nat_type()));

    kernel.scoped(|kernel| {
        kernel
            .refine(written.clone(), Term::intrinsic(Intrinsic::Bool(true)))
            .expect("the equation records");

        kernel.scoped(|kernel| {
            kernel.restate_refinements(&[(n, nat(0))]);

            assert_eq!(
                whnf(kernel, at_zero.clone()).expect("reduces").as_bool(),
                None
            );
        });
    });
}
