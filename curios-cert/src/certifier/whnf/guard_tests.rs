//! A guard inside an arm: its dual and its spelling across the successor seam, a dispatched guard resolved, and an operand compared as written.

use {
    super::test_support::*,
    crate::certifier::by_value::test_support::*,
    crate::{test_support::*, *},
    curios_analysis::{RESOLVED_SPELLING_LAYERS, test_support::*},
    curios_common::fill,
    curios_core::{Cost, Exhaustion, Reducer, Term},
};

/// `witness`, a structure of one method comparing two numbers by `${compared}`: the stand-in for a dispatch, projected and applied as elaboration spells a concept call.
const WITNESS: &str = "let /witness: {le: (a: #NatType, b: #NatType) -> #BoolType} = ((a: #NatType, b: #NatType) => ${compared},);";

/// A guard's equation is recorded on the guard's written spelling, and the false arm of `n < m` is the fact `m <= n` read the other way: the reducer asks the dual spelling with the literal negated, and the record stays as written.
#[test]
fn a_comparison_refined_answers_its_dual_negated() {
    let mut certifier = certifier();
    let [guard, dual] = under(
        &mut certifier,
        "n: #NatType, m: #NatType",
        "#NatLt(n, m), #NatLe(m, n)",
    );

    certifier.scoped(|certifier| {
        certifier
            .refine(guard.clone(), term("false"))
            .expect("the equation records");
        let reduced = certifier
            .reduce(dual.clone())
            .expect("a dual probe reduces");
        assert_eq!(reduced.as_bool(), Some(true));
        assert_eq!(
            certifier.refinement_of(&dual),
            None,
            "the record stays as written"
        );
    });
}

/// A guard's equation answers the same bound spelled across the `<`/`<=` seam.
///
/// `List/slice`'s bound arrives as `i + 1 <= len` where the guard deciding it was written `i < len`, and the two are one proposition on `Nat`: `whnf` retries a miss on the successor spelling, carrying the literal across rather than negating it as the dual retry does. Without it a guard discharges a bound only when the author spelled the comparison the way the standard library's signature happens to.
///
/// **Both operands are spelled as a program spells them, which is what the retry has to survive.** The guard's own operand is the *unreduced* call — a real one records `List/len(l)`, the application the author wrote, and the bound arrives with that call already folded to its intrinsic — so the seam is asked of the key's settled reduct rather than of the record. Asking the written spelling alone answers this fixture while a surface program is still refused, the certifier refusing what the elaborator accepts. The stand-in here is a definition the key mentions and the probe does not, which is the same asymmetry with nothing else in it.
#[test]
fn a_guard_answers_a_bound_spelled_across_the_successor_seam() {
    // The operand as a program writes it, `len + (30 + 34)`: a term reduction folds, standing for the `List/len(l)` call a guard records and the bound meets already folded. The bound as a signature instantiates it is `i + 1 <= len`, the successor written as the sum it is before reduction folds it into a floor.
    let hypotheses = "i: #NatType, len: #NatType";
    let terms = "#NatAdd(len, #NatAdd(30, 34)), #NatLt(i, #NatAdd(len, #NatAdd(30, 34))), #NatLe(#NatAdd(i, 1), #NatAdd(len, #NatAdd(30, 34)))";

    let mut control = certifier();
    let [written_length, _, _] = under(&mut control, hypotheses, terms);
    let folded = control
        .reduce(written_length.clone())
        .expect("the operand reduces");
    assert_ne!(
        folded, written_length,
        "the operand has to fold, or the probe never leaves the written spelling",
    );

    let mut certifier = certifier();
    let [_, below, within] = under(&mut certifier, hypotheses, terms);

    certifier.scoped(|certifier| {
        certifier
            .refine(below.clone(), term("true"))
            .expect("the equation records");

        let reduced = certifier.reduce(within.clone()).expect("the bound reduces");
        assert_eq!(
            reduced,
            term("true"),
            "the guard `i < len` did not answer the bound `i + 1 <= len`",
        );
    });

    // Outside the arm it is stuck again: the seam is a second spelling to look under, never a fact of its own.
    let reduced = certifier.reduce(within).expect("the bound reduces");
    assert!(
        reduced.as_bool().is_none(),
        "the successor spelling answered outside the arm that recorded the guard: {reduced:?}",
    );
}

/// A guard written through a dispatch answers its dual at the first probe, in the arm the decision procedure proves dead, exactly as the elaborator answers it there.
///
/// **This is the divergence the resolved spelling closes.** A comparison through a concept elaborates to an application — `(?w).1(a, hi)` — whose intrinsic shape only a reduction of its head exposes. Met only as the settled reduct of the written key — at the second probe point, after the decision procedure has folded the probe — that shape would be answered from the procedure, where the elaborator, which registers the dispatch-resolved spelling beside the written one and asks it first, answers it from the equation: in an arm whose guard is decided against its case value, a program the elaborator accepts would be refused by the certifier. `/std/Str/Valid`'s three-byte decoding meets it: its false arm of `cp % 4096 / 64 <= 63` asks `63 < cp % 4096 / 64`.
///
/// The stand-in for the dispatch is a witness holding the method, projected and applied as elaboration spells a concept call — a head that carries no name, which is the shape both checkers resolve — and the subject `n % 64` is one whose bound the procedure reads. The control pins the premise: with no equation, the procedure folds the probe to `false`, so the arm is dead and the subject's `true` can have come only from the equation, read the other way.
///
/// Mutation-checked: recording no resolved spelling in `Certifier::refine` answers the probe `false` — the procedure's fold, with the equation never met — and moves no other fixture in this module.
#[test]
fn a_dispatched_guard_answers_its_dual_before_the_procedure_folds_it() {
    let mut certifier = certifier();
    declared(&mut certifier, &fill!(WITNESS, compared = "#NatLe(a, b)"));
    let [guard, dual] = under(
        &mut certifier,
        "n: #NatType, positive: /std/Bool/Holds(#NatLt(0, 64))",
        "/witness.0(#NatRem(n, 64, positive), 63), #NatLt(63, #NatRem(n, 64, positive))",
    );

    assert_eq!(
        certifier.reduce(dual.clone()),
        Ok(term("false")),
        "the procedure has to decide the probe, or the arm is not dead"
    );

    let answered = certifier.scoped(|certifier| {
        certifier
            .refine(guard, term("false"))
            .expect("the equation records");

        certifier.reduce(dual)
    });

    assert_eq!(answered, Ok(term("true")));
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
    let mut certifier = certifier();
    // The method reads only its second operand, so the local the guard passes as the first is gone once it is opened.
    declared(&mut certifier, &fill!(WITNESS, compared = "#NatLe(b, 63)"));
    let [guard] = under(&mut certifier, "n: #NatType", "/witness.0(n, 5),");
    let closed = term("#NatLe(5, 63)");

    let inside = certifier.scoped(|certifier| {
        certifier
            .refine(guard, term("false"))
            .expect("the equation records");

        certifier.reduce(closed.clone())
    });
    let outside = certifier.reduce(closed);

    assert_eq!(inside, Ok(term("true")), "the equation is not this term's");
    assert_eq!(
        outside,
        Ok(term("true")),
        "and nothing remembered says otherwise"
    );
}

/// A guard over a `Nat` `n` dispatched through `layers` functions, two or more, declared in `certifier`: the innermost compares its two operands, and each one over it applies the one before it to its first operand twice. No layer is an eta-redex of the next, so `resolved_spelling` opens them one at a time, down to `n <= n`. The guard, with `n` opened, and `n <= n`.
fn dispatched_through(certifier: &mut Certifier, layers: usize) -> [Term; 2] {
    let mut body = "#NatLe(first, second)".to_string();
    for layer in 0..layers {
        declared(
            certifier,
            &fill!(
                "let /layer${layer}: (first: #NatType, second: #NatType) -> #BoolType = (first: #NatType, second: #NatType) => ${body};",
                layer,
                body
            ),
        );
        body = fill!("/layer${layer}(first, first)", layer);
    }
    let last = layers - 1;

    under(
        certifier,
        "n: #NatType",
        &fill!("/layer${last}(n, 63), #NatLe(n, n)", last),
    )
}

/// The layers a dispatch opens are bounded, and a spine that has not settled inside the bound resolves to nothing, never to however far it was opened: the arm then records its written spelling alone, which the equation still answers through the key's own reduct.
///
/// Mutation-checked against the bound: handing back what the last layer opened to once the layers run out resolves the deeper dispatch too.
#[test]
fn a_dispatch_deeper_than_the_layers_opened_resolves_to_nothing() {
    let resolved_through = |layers: usize| {
        let mut certifier = certifier();
        let [guard, comparison] = dispatched_through(&mut certifier, layers);

        (
            reading(&mut certifier, |certifier| {
                resolved_spelling(certifier, &guard)
            }),
            comparison,
        )
    };

    // A function is one layer, and seeing that what the last one opened to is no application is one more.
    let (resolved, comparison) = resolved_through(RESOLVED_SPELLING_LAYERS - 1);
    assert_eq!(
        resolved,
        Ok(Some(comparison)),
        "inside the bound the dispatch resolves to the comparison it ends in",
    );
    assert_eq!(
        resolved_through(RESOLVED_SPELLING_LAYERS).0,
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
/// Mutation-checked: reducing each argument before `resolved_spelling` opens the method exhausts the budget at `Certifier::refine`, and moves no other fixture in this module.
#[test]
fn resolving_a_dispatched_guard_forces_none_of_its_operands() {
    let budget = Cost::FRAME.get() * 4;
    let expensive = added("0", 100_000);
    let terms = fill!(
        "/witness.0(n, ${expensive}), #NatLt(${expensive}, n)",
        expensive
    );
    let certifier_over = |budget| {
        let mut certifier = Certifier::new(budget, SYNTAX);
        declared(&mut certifier, &fill!(WITNESS, compared = "#NatLe(a, b)"));
        let [guard, dual] = under(&mut certifier, "n: #NatType", &terms);

        (certifier, guard, dual)
    };

    let (mut control, _, dual) = certifier_over(budget);
    assert!(
        control
            .reduce(dual)
            .is_err_and(|spent| spent.is_exhausted()),
        "the operand has to be unaffordable, or the subject's answer proves nothing"
    );

    let (mut subject, guard, dual) = certifier_over(budget);
    let answered = subject.scoped(|certifier| {
        certifier
            .refine(guard, term("false"))
            .expect("resolving reduces no operand");

        certifier.reduce(dual)
    });

    assert_eq!(answered, Ok(term("true")));
}

/// The reduced spelling a refinement probe compares in reads each operand as a probe, as the elaborator's twin does: an operand with no value at the type level is kept as written, so the probe misses rather than refusing the judgment it serves. The operand here is an instance whose body, read at the level it states, asks for a successor no offset holds.
#[test]
fn an_operand_with_no_value_is_compared_as_written() {
    let mut certifier = certifier();
    declared(
        &mut certifier,
        "#{universe_context = {parameter_count = 1}}\nlet /lifted: #{level = u + 2} Type = #{level = u + 1} Type;",
    );
    let max = u32::MAX;
    let [undefined, probe] = under(
        &mut certifier,
        "x: #NatType",
        &fill!(
            "#{universes = {levels = [${max}]}} /lifted, #NatEql(#{universes = {levels = [${max}]}} /lifted, x)",
            max
        ),
    );
    assert!(
        certifier
            .reduce(undefined)
            .is_err_and(|error| !error.is_exhausted()),
        "the operand has a value, so the probe is not put to the question"
    );

    assert_eq!(
        reading(&mut certifier, |certifier| {
            super::canonical_operands(certifier, &probe)
        }),
        Ok(probe)
    );
}
