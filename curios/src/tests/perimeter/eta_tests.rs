//! Eta at a function and a record, and where irrelevance takes over from comparison.

//! End-to-end coverage for the soundness perimeter entries that nothing else guards.
//!
//! The soundness perimeter is `documentation/soundness/`, one entry per rule, each graded *probed*, *argued*, or *auditable only* (see `documentation/design/language/the-soundness-perimeter.md`). "Probed" is a claim about executable evidence, so it needs a test that fails when the rule stops holding — otherwise the grade records what someone once tried by hand and decays the moment nobody remembers doing it.
//!
//! The entries with their own homes are not repeated here: strict positivity lives in `tests::positivity`, the two totality obligations in `tests::soundness`, and witness coherence in `tests::concepts`. What is left is the large-elimination guard, `Prop` non-informativeness, coverage, and the foreign wire contract — four rules the claim rests on that had no regression test at all.
//!
//! Each rejection asserts its *own* diagnostic, following `tests::soundness`. A perimeter test that accepts any error is worse than none: an invalid fixture passes it while the rule it names goes unchecked. That is not hypothetical — the first draft of these probes "passed" on `unbound variable`, having never reached the check at all.

use crate::tests::run;

use super::test_support::*;

// Eta and untyped child positions, the row that carried an argument and no program at all. Conversion is type-directed, so eta is what converts `f` with `(x) => f(x)` and `p` with `(p.0, p.1)` without either side having to be written in that shape. Both rules are *accepting*, so each widens what counts as equal, and the two refusals beside them are what keep the acceptance from reading as "any two functions convert" and "any two records convert": drop the binder from the expansion and the equation dies, swap the components and it dies. Without them a `compare` that answered `true` at every Π and every Σ would satisfy the accepting rung and nothing here would notice.
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

// The same composition where it *is* reachable, which is Σ rather than Π. `tuple_sort` makes a record a proposition only when every component is one, so `{Nat, Eq(0, 0)}` stays relevant, irrelevance does not preempt eta, and eta is what hands the second component to a `Prop`-sorted goal. Both rules fire in one equation here — eta at Π opens the binder, eta at Σ splits the record — and only the relevant component is ever compared.
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

// The row's second clause in the direction it claims: comparing an untyped child at `Type` forfeits rules rather than adding them. `p` and `q` inhabit one proposition, so they are interchangeable at their own type — but as arguments of an opaque head they meet `ground`, where the goal type is `Type` and irrelevance is never asked. What the forfeiture costs is exactly what the row says it costs, a *refusal*, and a refusal is a disagreement, which is the signal the second checker exists to produce.
//
// This is the first row of the matrix above to sit in that quadrant. The elaborator accepts, comparing the arguments at the domain the head assigns them; the kernel refuses, and the fragment asserted is the two argument spellings themselves, so a fixture broken anywhere else could not pass it.
#[test]
fn a_grounded_argument_forfeits_irrelevance() {
    rejected_by(A_GROUNDED_ARGUMENT_FORFEITS_IRRELEVANCE, "f(p), f(q)");
}

// **The clause is inexact, and this is the correction.** What grounding forfeits is *type-directed* eta and *goal-level* irrelevance — the two rules `turn` reaches by asking what the goal type is. `struct_eta` is neither. It reads the literal's own declaration, so it fires from `structural`, which is precisely where a grounded comparison lands, and it skips a `Prop`-sorted field by asking `Sort::of` about the declaration's field type. Both rules the clause says are gone are therefore live in an untyped position, and this is the sharpest spelling of it: every field of `Sealed` is a proposition, so the field walk compares *nothing at all* and the literal is equated with the neutral on the strength of the neutral restriction alone.
//
// It is not unsound, and the reason is not the one `struct_eta`'s own comment gives. That comment says the neutral restriction is what stops the literal being equated with "an arbitrary term that merely appeared in the same untyped position" — but a `Var` is such a term, and the restriction excludes only non-neutrals. What actually establishes that the neutral inhabits `Sealed` is that every grounded pair is the corresponding children of two parents already shown convertible, so their types agree by typing. That is a property of the *callers* of `ground` rather than of `struct_eta`, and it is written in neither place.
#[test]
fn a_nominal_structs_eta_is_not_forfeited_there() {
    assert_eq!(run(A_NOMINAL_STRUCTS_ETA_IS_NOT_FORFEITED_THERE), b"1");
}

// **Where the forfeiture ends.** A struct literal's fields and a constructor's payload have a typed context the opaque head's arguments lack: the declaration's own telescope, which `struct_eta` already reads on the neutral side. The kernel used to compare both at `Type` anyway and refused every proof-carrying value built from a different proof of the same fact — `Str/concat` did not associate, and `Vec/eq_of_list` exists because `Eq/refl()` was refused there. Both checkers now compare fields and payloads at the telescope, and the four equations below are the shapes that were refused: a field, a payload, a standard-library constructor, and the string law that first exposed it.
#[test]
fn a_proof_field_does_not_distinguish_two_literals() {
    assert_eq!(run(A_PROOF_FIELD_DOES_NOT_DISTINGUISH_TWO_LITERALS), b"1");
}

// **The forfeiture reaches `/std`'s own laws.** `State`'s left identity sets a structure literal against the neutral `f(a)`: the elaborator compares the one field at its declared function type, where eta opens both sides at a fresh state, and the kernel's struct eta projects the neutral and compares at `Type`, where a lambda never meets a projection. So the law closes by `Eq/refl()` for one checker and is refused by the other, in the safe direction — found by stating the monad laws for the library's own witnesses rather than by a hunt.
#[test]
fn a_structs_function_field_forfeits_eta_against_a_neutral() {
    rejected_by(
        A_STRUCTS_FUNCTION_FIELD_FORFEITS_ETA_AGAINST_A_NEUTRAL,
        "State/bind",
    );
}

// The right identity is the same forfeiture with a variable on the neutral side, which is the shape `struct_eta`'s neutral restriction is written for.
#[test]
fn a_structs_function_field_forfeits_eta_against_a_variable() {
    rejected_by(
        A_STRUCTS_FUNCTION_FIELD_FORFEITS_ETA_AGAINST_A_VARIABLE,
        "State/bind",
    );
}

// The control for the pair above. Associativity sets two literals against each other, so both checkers compare the field at the declaration's telescope and the law certifies: what the two refusals lack is a typed position, not a rule about `State`.
#[test]
fn two_struct_literals_compare_their_function_fields() {
    assert_eq!(run(TWO_STRUCT_LITERALS_COMPARE_THEIR_FUNCTION_FIELDS), b"1");
}
