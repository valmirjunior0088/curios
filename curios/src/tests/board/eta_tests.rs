//! Eta at a function, a record and a unit, and where irrelevance takes over from comparison.

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

// **Eta by a literal needs no type, so it holds where none directs it.** The elaborator fires eta by a side's shape at every goal, and the kernel by the goal's type, which a child it compares at `Type` does not have: a stuck elimination's arm, a projection's head, a lambda's body inside one, and — with no `match` written — the arms a definition unfolds to once a record's eta has projected its two calls. An expansion equal to its neutral at the goal was refused there, by the kernel alone, so conversion was no congruence in the trusted checker and the refusal reached the author as the kernel's. A lambda and a tuple literal against a neutral fire the rule by their own shape (`function_eta`, `tuple_eta`), as a struct literal always has.
//
// The two refusals beside it are the near misses of the first fixtures, in the same position: the rule compares what the literal holds, and does not give up on functions and records there.
#[test]
fn a_literals_eta_holds_where_no_type_directs_it() {
    assert_eq!(run(A_LITERALS_ETA_HOLDS_WHERE_NO_TYPE_DIRECTS_IT), b"1");
}

#[test]
fn an_expansion_that_drops_its_binder_is_not_eta_in_an_arm() {
    rejected_by(
        AN_EXPANSION_THAT_DROPS_ITS_BINDER_IS_NOT_ETA_IN_AN_ARM,
        "type mismatch",
    );
}

#[test]
fn an_expansion_that_swaps_its_components_is_not_eta_in_an_arm() {
    rejected_by(
        AN_EXPANSION_THAT_SWAPS_ITS_COMPONENTS_IS_NOT_ETA_IN_AN_ARM,
        "type mismatch",
    );
}

// **Unit eta is the type's, in both checkers.** A type with no field — the empty Σ, a nominal struct that declares none — has one inhabitant, so any two terms convert at it, and each checker decides the goal by its type ahead of every structural rule. The elaborator reached the rule only between two sides no structural rule claimed and the kernel had it against a literal alone, so two variables elaborated and were refused by the kernel, and two applications or two stuck matches were refused by the elaborator, which held each equal to a variable. The first two programs are the ones that reached the kernel's refusal; the rest are the shapes, and the two eta rules that carry a goal to a unit.
//
// The refusal beside it is what keeps the acceptance from reading as "any two records convert": one relevant field keeps two neutrals apart.
#[test]
fn any_two_terms_convert_at_a_type_with_no_field() {
    assert_eq!(run(ANY_TWO_TERMS_CONVERT_AT_A_TYPE_WITH_NO_FIELD), b"1");
}

#[test]
fn two_neutrals_at_a_record_with_a_relevant_field_stay_apart() {
    rejected_by(
        TWO_NEUTRALS_AT_A_RECORD_WITH_A_RELEVANT_FIELD_STAY_APART,
        "type mismatch",
    );
}

// **A nominal struct has no eta by its type, and what eta would decide is read off the type.** Two neutrals at a struct have no literal to open, so neither checker expands them: any two terms convert at a struct every field of which has one inhabitant — a unit, a proof, a function into a unit, a record or a struct of such — and are left to their heads otherwise. The elaborator projected two neutrals at a struct and compared the projections with no type, which decided nothing, and the kernel compared their heads; once a lookup typed those projections the elaborator accepted two variables at a struct of a unit and the kernel refused them, which is the first program.
//
// The two refusals beside it are the rule's bounds: one relevant field keeps two neutrals apart, and a struct that reaches itself answers no where it is met again, which is what ends the walk.
#[test]
fn any_two_terms_convert_at_a_struct_with_one_inhabitant() {
    assert_eq!(
        run(ANY_TWO_TERMS_CONVERT_AT_A_STRUCT_WITH_ONE_INHABITANT),
        b"1"
    );
}

#[test]
fn two_neutrals_at_a_struct_with_a_relevant_field_stay_apart() {
    rejected_by(
        TWO_NEUTRALS_AT_A_STRUCT_WITH_A_RELEVANT_FIELD_STAY_APART,
        "type mismatch",
    );
}

#[test]
fn two_neutrals_at_a_struct_that_reaches_itself_stay_apart() {
    rejected_by(
        TWO_NEUTRALS_AT_A_STRUCT_THAT_REACHES_ITSELF_STAY_APART,
        "type mismatch",
    );
}

// **Where a child is compared with no type, what a type directs between two neutrals is read off the type a lookup gives both.** A tuple literal's component inside a stuck elimination's arm is such a child, and two variables there at a record of units, at a function into a unit or at a struct of a unit were refused by both checkers, which each equate them wherever a type reaches them. Neither side is expanded: eta between two neutrals decides nothing the type's shape does not, and the goal it would pose at a looked-up type is one the recurrence rule assumes. The last program is two proofs of a proposition whose implicit the elaborator solved, which it refused where the same proposition written out converged.
//
// The refusal beside it is the same pair at a record with a relevant field.
#[test]
fn two_neutrals_convert_where_a_lookup_gives_them_a_type_with_one_inhabitant() {
    assert_eq!(
        run(TWO_NEUTRALS_CONVERT_WHERE_A_LOOKUP_GIVES_THEM_A_TYPE_WITH_ONE_INHABITANT),
        b"1"
    );
}

#[test]
fn two_neutrals_stay_apart_where_a_lookup_gives_them_a_type_with_a_relevant_field() {
    rejected_by(
        TWO_NEUTRALS_STAY_APART_WHERE_A_LOOKUP_GIVES_THEM_A_TYPE_WITH_A_RELEVANT_FIELD,
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
