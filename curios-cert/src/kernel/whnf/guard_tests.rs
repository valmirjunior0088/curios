//! A guard inside an arm: its dual and its spelling across the successor seam, a dispatched guard resolved, and an operand compared as written.

use {
    super::test_support::*,
    crate::{Kernel, resolved_spelling, whnf},
    curios_analysis::{RESOLVED_SPELLING_LAYERS, test_support::SYNTAX},
    curios_core::{Cost, Exhaustion, Free, Intrinsic, Term},
};

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
    kernel
        .define(
            &witness,
            &Term::tuple_type([(method, method_type)]),
            &Term::tuple([Term::func(
                comparison,
                Term::intrinsic(Intrinsic::nat_lte(Term::free_var(&a), Term::free_var(&b))),
            )]),
            &monomorphic(),
        )
        .expect("a name is declared once");
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
    kernel
        .define(
            &witness,
            &Term::tuple_type([(method, method_type)]),
            &Term::tuple([Term::func(
                comparison,
                Term::intrinsic(Intrinsic::nat_lte(Term::free_var(&b), nat(63))),
            )]),
            &monomorphic(),
        )
        .expect("a name is declared once");
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

/// A guard over `subject` dispatched through `layers` functions, two or more: the innermost compares its two operands, and each one over it applies the one before it to its first operand twice. No layer is an eta-redex of the next, so `resolved_spelling` opens them one at a time, down to `subject <= subject`.
fn dispatched_through(kernel: &mut Kernel, layers: usize, subject: &Free) -> Term {
    let (first, second) = (binder(100, "first"), binder(101, "second"));
    let signature = [(first, nat_type()), (second, nat_type())];
    let type_ = Term::func_type(signature.clone(), Term::intrinsic(Intrinsic::BoolType));
    let mut body = Term::intrinsic(Intrinsic::nat_lte(
        Term::free_var(&first),
        Term::free_var(&second),
    ));
    let mut head = None;

    for layer in 0..layers {
        let name = binder(102 + layer as u32, "layer");
        kernel
            .define(
                &name,
                &type_,
                &Term::func(signature.clone(), body),
                &monomorphic(),
            )
            .expect("a name is declared once");
        body = Term::apply(
            Term::free_var(&name),
            [Term::free_var(&first), Term::free_var(&first)],
        );
        head = Some(name);
    }

    Term::apply(
        Term::free_var(&head.expect("a dispatch has a layer")),
        [Term::free_var(subject), nat(63)],
    )
}

/// The layers a dispatch opens are bounded, and a spine that has not settled inside the bound resolves to nothing, never to however far it was opened: the arm then records its written spelling alone, which the equation still answers through the key's own reduct.
///
/// Mutation-checked against the bound: handing back what the last layer opened to once the layers run out resolves the deeper dispatch too.
#[test]
fn a_dispatch_deeper_than_the_layers_opened_resolves_to_nothing() {
    let n = binder(1, "n");
    let comparison = Term::intrinsic(Intrinsic::nat_lte(Term::free_var(&n), Term::free_var(&n)));
    let resolved_through = |layers: usize| {
        let mut kernel = kernel();
        kernel.assume(&n, &nat_type());
        let guard = dispatched_through(&mut kernel, layers, &n);

        resolved_spelling(&mut kernel, &guard)
    };

    // A function is one layer, and seeing that what the last one opened to is no application is one more.
    assert_eq!(
        resolved_through(RESOLVED_SPELLING_LAYERS - 1),
        Ok(Some(comparison)),
        "inside the bound the dispatch resolves to the comparison it ends in",
    );
    assert_eq!(
        resolved_through(RESOLVED_SPELLING_LAYERS),
        Ok(None),
        "past it the dispatch is not one",
    );
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
        kernel
            .define(
                &witness,
                &Term::tuple_type([(method, method_type.clone())]),
                &Term::tuple([Term::func(
                    comparison.clone(),
                    Term::intrinsic(Intrinsic::nat_lte(Term::free_var(&a), Term::free_var(&b))),
                )]),
                &monomorphic(),
            )
            .expect("a name is declared once");
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
