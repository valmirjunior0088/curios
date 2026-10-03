//! Eta at a function and a record, and where irrelevance takes over from comparison.

use {super::test_support::*, crate::tests::run};

// Eta and untyped child positions. Conversion is type-directed, so eta is what converts `f` with `(x) => f(x)` and `p` with `(p.0, p.1)` without either side having to be written in that shape. Both rules are *accepting*, so each widens what counts as equal, and the two refusals beside them are what keep the acceptance from reading as "any two functions convert" and "any two records convert": drop the binder from the expansion and the equation dies, swap the components and it dies. Without them a `compare` that answered `true` at every Π and every Σ would satisfy the accepting rung and nothing here would notice.
#[test]
fn converts_a_function_and_a_record_with_their_expansions() {
    assert_eq!(
        run(ETA_CONVERTS_A_FUNCTION_AND_A_RECORD_WITH_THEIR_EXPANSIONS),
        b"1"
    );
}

#[test]
fn an_expansion_that_drops_its_binder_is_not_eta() {
    rejected_by(
        AN_EXPANSION_THAT_DROPS_ITS_BINDER_IS_NOT_ETA,
        "type mismatch",
    );
}

#[test]
fn an_expansion_that_swaps_its_components_is_not_eta() {
    rejected_by(
        AN_EXPANSION_THAT_SWAPS_ITS_COMPONENTS_IS_NOT_ETA,
        "type mismatch",
    );
}

// The composition the row named as unattacked — "eta at a function type whose codomain is a proposition, where the expansion's body lands at a `Prop`-sorted goal and irrelevance discharges it without comparing anything" — and at Π there is nothing to attack, because the shape cannot arise. `turn` tries irrelevance *before* it dispatches on the goal type's shape, and `func_sort` makes a Π into a proposition a proposition whatever it quantifies over, so the goal is discharged whole at the top and eta never opens a binder at all. Any two such functions are equal, which is this accepting rung.
//
// The relevant-codomain pair beside it is what says the discharge is the proposition's doing rather than conversion giving up on function types: the same two binders at `(Nat) -> Nat` are not identified.
#[test]
fn a_function_into_a_proposition_is_discharged_before_eta() {
    assert_eq!(
        run(A_FUNCTION_INTO_A_PROPOSITION_IS_DISCHARGED_BEFORE_ETA),
        b"1"
    );
}

#[test]
fn a_function_into_a_type_is_not_discharged_uncompared() {
    rejected_by(
        A_FUNCTION_INTO_A_TYPE_IS_NOT_DISCHARGED_UNCOMPARED,
        "type mismatch",
    );
}

// The same composition where it *is* reachable, which is Σ rather than Π. `tuple_sort` makes a record a proposition only when every component is one, so `{Nat, Eq()(0, 0)}` stays relevant, irrelevance does not preempt eta, and eta is what hands the second component to a `Prop`-sorted goal. Both rules fire in one equation here — eta at Π opens the binder, eta at Σ splits the record — and only the relevant component is ever compared.
//
// The refusal beside it pins that last clause: replace the relevant component with a literal and the equation dies although the proof component still matches, so the acceptance above is not eta declining to look.
#[test]
fn hands_a_records_proof_component_to_irrelevance() {
    assert_eq!(
        run(ETA_HANDS_A_RECORDS_PROOF_COMPONENT_TO_IRRELEVANCE),
        b"1"
    );
}

#[test]
fn still_compares_a_records_relevant_component() {
    rejected_by(
        ETA_STILL_COMPARES_A_RECORDS_RELEVANT_COMPONENT,
        "type mismatch",
    );
}

// **The position this row is about is typed.** `p` and `q` inhabit one proposition, so they are interchangeable at their own type — and as arguments of an opaque head they meet the head's telescope rather than `ground`, where the goal type is `Type`, irrelevance is never asked, and the kernel would refuse what the elaborator accepts.
//
// A variable head has a typed context after all: it was assumed or declared at a function type, and its telescope is what its arguments inhabit, exactly as a declaration's field telescope is what a field inhabits. Reading it is a lookup rather than an inference, so the spine costs nothing more and conversion consults nothing new. A universe instance of a variable and a `rec` member carry one the same way. What remains untyped is a head that names no type here — a record projection or a stuck elimination — and a binder carrying `ground_scope`'s stand-in, which is not a function type and so hands back no telescope.
#[test]
fn a_spine_argument_compares_at_the_heads_domain() {
    assert_eq!(run(A_SPINE_ARGUMENT_COMPARES_AT_THE_HEADS_DOMAIN), b"1");
}

// **Two applications of one definition are decided by their spines before either is unfolded, in both checkers.** Forcing both sides first would land the proof they differ in in a stuck match's scrutinee, which is compared at `Type`, and refuse what comparing the spines of one global first accepts.
#[test]
fn a_definition_applied_to_two_proofs_converts_before_unfolding() {
    assert_eq!(
        run(A_DEFINITION_APPLIED_TO_TWO_PROOFS_CONVERTS_BEFORE_UNFOLDING),
        b"1"
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

// **What this pins is the vacuous walk, and the invariant that licenses it.** Every field of `Sealed` is a proposition, so `struct_eta`'s walk compares *nothing at all* and answers `true` on the strength of the neutral restriction alone. That is sound because `other` inhabits `Sealed` — conversion is only ever asked about two terms of one type — and because eta for a single-constructor record equates any inhabitant with the literal of its projections.
//
// The restriction is a proxy for that invariant rather than a second guarantee: a `Var` is as arbitrary a term as any other, and the invariant is a property of the *callers* rather than of this function. `struct_eta` says so in its own documentation.
//
// Its comparison is `Sealed { one = p, two = q }` against `b` as arguments of `f`, and a spine's arguments take the telescope their head carries, so the position is typed: the fixture reaches `struct_eta` from a typed goal, and what it holds is the vacuous walk itself.
#[test]
fn a_nominal_structs_eta_is_not_forfeited_there() {
    assert_eq!(run(A_NOMINAL_STRUCTS_ETA_IS_NOT_FORFEITED_THERE), b"1");
}

// **Where the forfeiture ends.** A struct literal's fields and a constructor's payload have a typed context the opaque head's arguments lack: the declaration's own telescope, which `struct_eta` already reads on the neutral side. Both checkers compare fields and payloads at the telescope; compared at `Type`, every proof-carrying value built from a different proof of the same fact would be refused, and `Str/concat` would not associate. The four equations below are those shapes: a field, a payload, a standard-library constructor, and the string law.
#[test]
fn a_proof_field_does_not_distinguish_two_literals() {
    assert_eq!(run(A_PROOF_FIELD_DOES_NOT_DISTINGUISH_TWO_LITERALS), b"1");
}

// **`/std`'s own laws need the rest of it.** `State`'s left identity sets `State/bind`'s literal against the neutral `f(a)`, and two things make it converge. The field compares at the function type the declaration gives it, where eta opens both sides at a fresh state, rather than at `Type`, where a lambda never meets a projection. And a stuck *application* counts as the neutral inhabitant it is, where a `struct_eta` taking only a variable or a projection would refuse the law not on its content but on the shape of the side it is stated against.
//
// A refusal there falls through to `unfolded_retry` — an application may have an unfolding left where a variable and a projection have none, so the eta attempt is tried first and a failure hands the pair on rather than deciding it.
#[test]
fn a_structs_function_field_meets_a_neutral_application() {
    assert_eq!(
        run(A_STRUCTS_FUNCTION_FIELD_MEETS_A_NEUTRAL_APPLICATION),
        b"1"
    );
}

// The right identity is the same law with a variable on the neutral side, which `struct_eta` admits as a variable — so this one needs the typed field alone, and it is what separates the two halves.
#[test]
fn a_structs_function_field_meets_a_neutral_variable() {
    assert_eq!(run(A_STRUCTS_FUNCTION_FIELD_MEETS_A_NEUTRAL_VARIABLE), b"1");
}

// The control for the pair above. Associativity sets two literals against each other, so both checkers compare the field at the declaration's telescope and the law certifies: what the two refusals lack is a typed position, not a rule about `State`.
#[test]
fn two_struct_literals_compare_their_function_fields() {
    assert_eq!(run(TWO_STRUCT_LITERALS_COMPARE_THEIR_FUNCTION_FIELDS), b"1");
}

// Conversion's recurrence rule. Two recursions that fold to themselves arrive at the same goal when compared, and a history that read "already assumed" as "proved" would equate two definitions that differ.
#[test]
fn two_distinct_recursions_do_not_convert() {
    rejected_by(DISTINCT_RECURSIONS_ARE_NOT_EQUAL, "type mismatch");
}
