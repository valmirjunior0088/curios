//! An effect is not a value: what may be a scrutinee, an argument, or inhabit a pure arrow.

//! End-to-end coverage for the soundness perimeter entries that nothing else guards.
//!
//! The soundness perimeter is `documentation/soundness/`, one entry per rule, each graded *probed*, *argued*, or *auditable only* (see `documentation/design/language/the-soundness-perimeter.md`). "Probed" is a claim about executable evidence, so it needs a test that fails when the rule stops holding — otherwise the grade records what someone once tried by hand and decays the moment nobody remembers doing it.
//!
//! The entries with their own homes are not repeated here: strict positivity lives in `tests::positivity`, the two totality obligations in `tests::soundness`, and witness coherence in `tests::concepts`. What is left is the large-elimination guard, `Prop` non-informativeness, coverage, and the foreign wire contract — four rules the claim rests on that had no regression test at all.
//!
//! Each rejection asserts its *own* diagnostic, following `tests::soundness`. A perimeter test that accepts any error is worse than none: an invalid fixture passes it while the rule it names goes unchecked. That is not hypothetical — the first draft of these probes "passed" on `unbound variable`, having never reached the check at all.

use crate::tests::run;

use super::test_support::*;

// The arm rule at its one arm with no case value of its own. A `| _ =>` catch-all binds nothing and refines no index, so the only instance it can be checked at is the scrutinee's — which is the instance the elimination then hands its caller.
//
// This is the two-checker matrix's own quadrant rather than a route to `False`: the elaborator checks the catch-all at the actual scrutinee and the kernel opened the motive's scrutinee binder at the family *type* `/Three`, so this program elaborated and was then refused with `/Three` standing in both term positions of the `Eq`. That direction fails closed — nothing well-typed inhabits an expectation with a type substituted for a value — but the certifier's own judgment is the one that matters, and it established nothing about the scrutinee for any catch-all it accepted. No prelude catch-all has a scrutinee-dependent motive, so the disagreement count stayed at zero and the rule was never asked.
//
// The instance is pinned where the rule lives, in `curios_cert::kernel::infer::eliminate::tests`: `a_catch_all_sees_its_own_scrutinee` with its control `a_catch_all_at_another_value_of_the_family_is_refused`, which asserts the expectation itself so that not checking the catch-all cannot pass for closing the hole.
#[test]
fn a_catch_all_is_checked_at_its_scrutinee() {
    assert_eq!(run(A_CATCH_ALL_IS_CHECKED_AT_ITS_SCRUTINEE), b"1");
}

// The purity premise, attacked through the one store that rewrites a term before the guard can see it. `refine_head`'s fallback registers the whole canonical scrutinee against the arm's case value, and the reducer consults that store *ahead of* folding the intrinsic — so inside `| true =>` the effectful `Cell/get(c)` reduces to `true` and never reaches `reduce_intrinsic`. The annotation `Eq(Cell/get(c), true)` is admitted on those terms, and it is stored **as written**: `p`'s recorded type keeps the `Cell/get(c)`, and only the conversion that discharged `Eq/refl` ever saw it as `true`.
//
// Two occurrences of one syntactic term then denote two different values. After `Cell/set(c, false)` the inner `match Cell/get(c)` refines that same term to `false`, `p` re-reads at `Eq(false, true)`, and `/std/Bool/false_neq_true` turns it into `/std/False`. The arm deriving it is *reachable*: the first read is `true`, so the outer arm runs, and the second is `false`, so the inner one runs too.
//
// Verified while the hole was open, and the acceptance is the refinement's doing rather than a fixture that never reached the check: the identical program with the derivation moved to the inner `| true =>` arm — where the refinement is `true`, so `p` re-reads at `Eq(true, true)` — is refused by `curios-elab` with `type mismatch`, while this one passes elaboration entire. It never compiled, because `curios-cert` refuses it, but not by any rule of its own: `whnf` folds an `Intrinsic` at the top of its loop and only consults `refinement_of` on the value that comes back, so the same conversion dies on `reduction failed in the kernel`. The kernel's ordering is what stood between this and a closed inhabitant of `False`; this asserts the elaborator's half.
#[test]
fn an_effectful_scrutinee_is_not_a_value() {
    rejected_by(AN_EFFECTFUL_SCRUTINEE_IS_NOT_A_VALUE, "Io");
}

#[test]
fn a_match_on_a_forced_cell_read_still_compiles() {
    assert_eq!(run(A_MATCH_ON_A_FORCED_CELL_READ_STILL_COMPILES), b"t");
}

/// Asserted on the *argument*, which is where this shape now dies. The old rule withheld an equation and the program failed where it needed one, so the discriminator was `p`'s type still reading the unrefined `g(Cell/get(c))`; there is no equation to withhold any more. `Cell/get(@Bool, c)` is an `Io(Bool)` and `f : (Bool) -> Bool` does not take one, so the derivation is refused at the first of its four occurrences and never reaches a refinement at all.
#[test]
fn an_effect_behind_a_stuck_head_is_not_an_argument() {
    rejected_by(AN_EFFECT_BEHIND_A_STUCK_HEAD_IS_NOT_AN_ARGUMENT, "Io");
}

#[test]
fn a_stuck_application_scrutinee_still_refines() {
    assert_eq!(run(A_STUCK_APPLICATION_SCRUTINEE_STILL_REFINES), b"t");
}

/// Asserted on the offending *argument*: the refusal is that `(b) => Cell/get(c)` cannot be passed where a `(Bool) -> Bool` is wanted, so the description type has to appear in the diagnostic. A fixture refused anywhere else — at the cell, at `Eq/refl`, at an arm — would not produce that, and this file's rule is that a perimeter test asserts its own diagnostic.
#[test]
fn an_effect_cannot_inhabit_a_pure_arrow() {
    rejected_by(AN_EFFECT_CANNOT_INHABIT_A_PURE_ARROW, "Io");
}

#[test]
fn a_parameter_headed_scrutinee_refines_again() {
    assert_eq!(run(A_PARAMETER_HEADED_SCRUTINEE_REFINES_AGAIN), b"t");
}
