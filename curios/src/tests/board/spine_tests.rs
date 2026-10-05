//! A spine's arguments and the proofs they carry: where an argument is typed, and two calls of one definition compared before either unfolds.

use {super::test_support::*, crate::tests::run};

// **The position this row is about is typed.** `p` and `q` inhabit one proposition, so they are interchangeable at their own type — and as arguments of an opaque head they meet the head's telescope rather than `ground`, where the goal type is `Type`, irrelevance is never asked, and the kernel would refuse what the elaborator accepts.
//
// A variable head has a typed context after all: it was assumed or declared at a function type, and its telescope is what its arguments inhabit, exactly as a declaration's field telescope is what a field inhabits. Reading it is a lookup rather than an inference, so the spine costs nothing more and conversion consults nothing new. A universe instance of a variable and a `rec` member carry one the same way, and so does an application or a projection of such a head. What remains untyped is a head that names no type here — a stuck elimination — and a binder carrying `ground_scope`'s stand-in, which is not a function type and so hands back no telescope.
#[test]
fn a_spine_argument_compares_at_the_heads_domain() {
    assert_eq!(run(A_SPINE_ARGUMENT_COMPARES_AT_THE_HEADS_DOMAIN), b"1");
}

// **A spine's arguments are compared at the types its head assigns under every head a lookup types.** Past a curried head and past a projected one the kernel compared the arguments at `Type`, so two proofs of one proposition stayed apart there where the elaborator, which reads the head's type whatever the head is, had accepted them. The refusal beside it is a relevant argument in the same position.
#[test]
fn a_spines_arguments_are_typed_under_a_curried_and_a_projected_head() {
    assert_eq!(
        run(A_SPINES_ARGUMENTS_ARE_TYPED_UNDER_A_CURRIED_AND_A_PROJECTED_HEAD),
        b"1"
    );
}

#[test]
fn a_relevant_argument_past_a_curried_head_stays_apart() {
    rejected_by(
        A_RELEVANT_ARGUMENT_PAST_A_CURRIED_HEAD_STAYS_APART,
        "type mismatch",
    );
}

// **Two applications of one definition are decided by their spines before either is unfolded, in both checkers.** Forcing both sides first would land the proof they differ in in a stuck match's scrutinee, which is compared at `Type`, and refuse what comparing the spines of one global first accepts.
#[test]
fn a_definition_applied_to_two_proofs_converts_before_unfolding() {
    assert_eq!(
        run(A_DEFINITION_APPLIED_TO_TWO_PROOFS_CONVERTS_BEFORE_UNFOLDING),
        b"1"
    );
}

// **The spines are compared on the pair as posed, at whatever type, and whatever spells the two calls.** At a function or a record type the kernel fires eta by the type, and tried after it the spine rule never ran: eta hands on each call applied or projected, no call of a definition at its head, both calls unfolded, and the two proofs were a stuck match's scrutinees. The kernel alone refused the first two programs. The third is the definition's call applied once more, a curried spine, which the kernel's rule did not look through and the elaborator's did. The fourth was the elaborator's to refuse: `Eq/refl()`'s implicit is solved to the first call, the goal that follows spells that side as the metavariable, and its rule read the side as written.
//
// The refusal beside it is two calls that differ in an argument that matters, which no spine comparison identifies.
#[test]
fn two_calls_of_one_definition_convert_by_their_spines_at_any_type() {
    assert_eq!(
        run(TWO_CALLS_OF_ONE_DEFINITION_CONVERT_BY_THEIR_SPINES_AT_ANY_TYPE),
        b"1"
    );
}

#[test]
fn two_calls_that_differ_in_a_relevant_argument_stay_apart() {
    rejected_by(
        TWO_CALLS_THAT_DIFFER_IN_A_RELEVANT_ARGUMENT_STAY_APART,
        "type mismatch",
    );
}

// **Two calls of one definition converge wherever they sit, whichever rule reaches them.** The spine rules decide the pair only while it is still spelled as two calls. Under a lambda's body, a tuple's component, a `let`'s value, a list's element, a `match`'s scrutinee, an operation's operand and a projection's head, reduction reaches the call first and unfolds it, and the two proofs become a stuck elimination's scrutinees, which were compared at `Type` and held apart: by the kernel alone under the first two, by both checkers under the rest, and by the elaborator alone at a curried call read by reflexivity. A scrutinee is compared at the type a lookup gives it, where that is a proposition, so the unfolded forms converge and the verdict no longer follows which spelling survived. The last program is a proof past a stuck `match`'s head, an argument neither checker types, read the same way.
//
// The refusal beside it is two calls that differ in an argument that matters, under a `let`: the lookup types it at `Nat`, where nothing ends the comparison.
#[test]
fn two_calls_of_one_definition_convert_wherever_they_sit() {
    assert_eq!(
        run(TWO_CALLS_OF_ONE_DEFINITION_CONVERT_WHEREVER_THEY_SIT),
        b"1"
    );
}

#[test]
fn two_calls_that_differ_in_a_relevant_argument_stay_apart_wherever_they_sit() {
    rejected_by(
        TWO_CALLS_THAT_DIFFER_IN_A_RELEVANT_ARGUMENT_STAY_APART_WHEREVER_THEY_SIT,
        "type mismatch",
    );
}

// The same pair where the definition mentions `Eq`, which makes it universe-polymorphic: each occurrence is an instance at levels of its own. Its heads are compared after the spines, which is when their levels are identified; a spine rule matching a bare variable head only would refuse it.
#[test]
fn a_polymorphic_definition_applied_to_two_proofs_converts() {
    assert_eq!(
        run(A_POLYMORPHIC_DEFINITION_APPLIED_TO_TWO_PROOFS_CONVERTS),
        b"1"
    );
}

// An intrinsic carries its proofs as operands, compared at the proposition `Intrinsic::signature` declares for each, so two proofs of one bound meet irrelevance there. The two heads differ, so neither spine rule decides the pair and both sides unfold to the division itself.
#[test]
fn an_intrinsic_applied_to_two_proofs_converts_at_their_proposition() {
    assert_eq!(
        run(AN_INTRINSIC_APPLIED_TO_TWO_PROOFS_CONVERTS_AT_THEIR_PROPOSITION),
        b"1"
    );
}

// An ordinary program that forcing first cannot finish: a recursive function passing a proof along, unfolded under fresh binders at every round, so the conversion recurrence would never see its goal again and would spend the whole budget before refusing.
#[test]
fn a_recursive_function_carrying_a_proof_converts_without_unfolding() {
    assert_eq!(
        run(A_RECURSIVE_FUNCTION_CARRYING_A_PROOF_CONVERTS_WITHOUT_UNFOLDING),
        b"1"
    );
}

// The same function eliminating its proof in an arm, which unfolding first refuses outright: the two eliminations differ only in their scrutinees, and a scrutinee is compared at `Type`.
#[test]
fn a_recursive_function_matching_its_proof_converts() {
    assert_eq!(run(A_RECURSIVE_FUNCTION_MATCHING_ITS_PROOF_CONVERTS), b"1");
}

// Carneiro's first step, two accessibility proofs at one recursive call. Unfolded, the calls reduce through different eliminations of the proofs and nothing reconciles them; compared by their spines, the proofs are one value. Its continuation is *not* decided — `f(0, lt, a)` against the call one reduction step further on stays refused, the non-transitivity every checker of a theory with this elimination has.
#[test]
fn two_accessibility_proofs_at_one_recursive_call_convert() {
    assert_eq!(
        run(TWO_ACCESSIBILITY_PROOFS_AT_ONE_RECURSIVE_CALL_CONVERT),
        b"1"
    );
}

// **Irrelevance decides a goal before either side is reduced, in the elaborator as in the kernel, wherever nothing is flexible.** Inside the arm of `match h(Top, Top)` the case equation makes that proof `refl`, the cast along it reduces, and Abel and Coquand's `Omega(h)` unfolds forever — no recursion anywhere, so the totality obligations have nothing to refuse. Outside the arm the same comparison is accepted either way. Mutation-checked: without the check ahead of reduction the elaborator runs out of steps on exactly that goal.
#[test]
fn a_proof_is_not_reduced_to_compare_it_with_another() {
    assert_eq!(run(A_PROOF_IS_NOT_REDUCED_TO_COMPARE_IT_WITH_ANOTHER), b"1");
}
