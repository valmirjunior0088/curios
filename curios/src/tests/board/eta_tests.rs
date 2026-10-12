//! Eta at a function and a record, what the unit type has in its place, and where irrelevance takes over from comparison.

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

// **A literal's eta is taken against every neutral, one set in both checkers.** The kernel took it against a variable, a projection and a stuck application, and the elaborator against any term, so a literal against a stuck elimination elaborated and was refused by the kernel alone: a tuple literal and a lambda in a stuck elimination's arm, where no type directs the rule, a tuple literal there against a definition that unfolds to one, and a struct literal at the struct's own type. A neutral is every term whose head does not show its type, which is what a shape can tell where the goal states no type.
//
// The two refusals beside it are the near misses: the rule compares what the literal holds.
#[test]
fn a_literals_eta_is_taken_against_a_stuck_elimination() {
    assert_eq!(
        run(A_LITERALS_ETA_IS_TAKEN_AGAINST_A_STUCK_ELIMINATION),
        b"1"
    );
}

#[test]
fn a_tuple_that_swaps_a_stuck_eliminations_components_is_not_its_eta() {
    rejected_by(
        A_TUPLE_THAT_SWAPS_A_STUCK_ELIMINATIONS_COMPONENTS_IS_NOT_ITS_ETA,
        "type mismatch",
    );
}

#[test]
fn a_struct_that_swaps_a_stuck_eliminations_fields_is_not_its_eta() {
    rejected_by(
        A_STRUCT_THAT_SWAPS_A_STUCK_ELIMINATIONS_FIELDS_IS_NOT_ITS_ETA,
        "type mismatch",
    );
}

// **The unit type has no eta, in either checker.** A type with no field — the empty Σ, a nominal struct that declares none — has one inhabitant by its shape, and no rule reads that: two neutrals at it are compared by their heads, and its literal's walk, which would compare nothing, is not taken against a neutral. Read off the type, the verdict held inside an arm that fixed a variable and was remembered past it, at a type that is the unit under one case and `Nat` under another, and proved `False` there. The shapes below are the ones a rule of that kind reached: two neutrals, the literal against one, a struct with no field, a function into a unit, and two stuck matches at a record of units, where eta projects both sides and each projection meets a unit.
//
// A record with a relevant field keeps two neutrals apart as it did.
#[test]
fn two_neutrals_at_the_unit_type_stay_apart() {
    rejected_by(TWO_NEUTRALS_AT_THE_UNIT_TYPE_STAY_APART, "type mismatch");
}

#[test]
fn a_neutral_at_the_unit_type_is_not_its_literal() {
    rejected_by(
        A_NEUTRAL_AT_THE_UNIT_TYPE_IS_NOT_ITS_LITERAL,
        "type mismatch",
    );
}

#[test]
fn two_neutrals_at_a_struct_with_no_field_stay_apart() {
    rejected_by(
        TWO_NEUTRALS_AT_A_STRUCT_WITH_NO_FIELD_STAY_APART,
        "type mismatch",
    );
}

#[test]
fn two_neutrals_at_a_function_into_a_unit_stay_apart() {
    rejected_by(
        TWO_NEUTRALS_AT_A_FUNCTION_INTO_A_UNIT_STAY_APART,
        "type mismatch",
    );
}

#[test]
fn two_stuck_matches_at_a_record_of_units_stay_apart() {
    rejected_by(
        TWO_STUCK_MATCHES_AT_A_RECORD_OF_UNITS_STAY_APART,
        "type mismatch",
    );
}

#[test]
fn two_neutrals_at_a_record_with_a_relevant_field_stay_apart() {
    rejected_by(
        TWO_NEUTRALS_AT_A_RECORD_WITH_A_RELEVANT_FIELD_STAY_APART,
        "type mismatch",
    );
}

// **A nominal struct has no eta by its type, and two neutrals at one are made one by nothing but a proposition.** Two neutrals at a struct have no literal to open, so neither checker expands them, and a struct every field of which has one inhabitant — a unit, a proof — has one by its shape, which no rule reads: its two neutrals stay apart. A struct meant to have one inhabitant is declared a proposition, and any two terms at it convert unread: two variables, two applications, two stuck matches, and a cast between two instances of a family over it.
//
// The refusals after it are structs over data, under whatever nesting and however a declaration names itself.
#[test]
fn two_neutrals_at_a_struct_of_a_unit_stay_apart() {
    rejected_by(
        TWO_NEUTRALS_AT_A_STRUCT_OF_A_UNIT_STAY_APART,
        "type mismatch",
    );
}

#[test]
fn two_neutrals_at_a_struct_of_a_proof_stay_apart() {
    rejected_by(
        TWO_NEUTRALS_AT_A_STRUCT_OF_A_PROOF_STAY_APART,
        "type mismatch",
    );
}

#[test]
fn any_two_terms_convert_at_a_struct_declared_a_proposition() {
    assert_eq!(
        run(ANY_TWO_TERMS_CONVERT_AT_A_STRUCT_DECLARED_A_PROPOSITION),
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

#[test]
fn two_neutrals_at_a_struct_nested_over_a_relevant_field_stay_apart() {
    rejected_by(
        TWO_NEUTRALS_AT_A_STRUCT_NESTED_OVER_A_RELEVANT_FIELD_STAY_APART,
        "type mismatch",
    );
}

#[test]
fn two_neutrals_at_structs_that_reach_each_other_stay_apart() {
    rejected_by(
        TWO_NEUTRALS_AT_STRUCTS_THAT_REACH_EACH_OTHER_STAY_APART,
        "type mismatch",
    );
}

#[test]
fn two_neutrals_at_a_struct_that_reaches_itself_stay_apart_inside_its_declaration() {
    rejected_by(
        TWO_NEUTRALS_AT_A_STRUCT_THAT_REACHES_ITSELF_STAY_APART_INSIDE_ITS_DECLARATION,
        "type mismatch",
    );
}

// **A goal's type is read forced in both checkers.** A struct declared with `and` is referred to through its group, which weak-head reduction leaves folded, and what a type directs is keyed on the type's own shape and sort. Read in weak-head form, two proofs of a proposition declared in a group are compared by their heads where the same struct declared alone converges, and where a lookup, which forces what it reads, converges at a child no type was handed for.
//
// The refusal beside it is a group's member that is data.
#[test]
fn a_goals_type_is_read_forced() {
    assert_eq!(run(A_GOALS_TYPE_IS_READ_FORCED), b"1");
}

#[test]
fn two_neutrals_at_a_groups_member_with_a_relevant_field_stay_apart() {
    rejected_by(
        TWO_NEUTRALS_AT_A_GROUPS_MEMBER_WITH_A_RELEVANT_FIELD_STAY_APART,
        "type mismatch",
    );
}

// **Where a child is compared with no type, two neutrals a lookup types at one proposition convert.** A tuple literal's component inside a stuck elimination's arm is such a child, and two proofs there were refused by both checkers, which each equate them wherever a type reaches them. Neither side is expanded: eta between two neutrals decides nothing irrelevance does not, and the goal it would pose at a looked-up type is the one in hand, posed again without end. The last program is two proofs of a proposition whose implicit the elaborator solved, which it refused where the same proposition written out converged.
//
// The refusals beside it are the same pair at a record of units, whose shape decides nothing, and at a record with a relevant field.
#[test]
fn two_neutrals_convert_where_a_lookup_gives_them_a_proposition() {
    assert_eq!(
        run(TWO_NEUTRALS_CONVERT_WHERE_A_LOOKUP_GIVES_THEM_A_PROPOSITION),
        b"1"
    );
}

#[test]
fn two_neutrals_stay_apart_where_a_lookup_gives_them_a_record_of_units() {
    rejected_by(
        TWO_NEUTRALS_STAY_APART_WHERE_A_LOOKUP_GIVES_THEM_A_RECORD_OF_UNITS,
        "type mismatch",
    );
}

#[test]
fn two_neutrals_stay_apart_where_a_lookup_gives_them_a_type_with_a_relevant_field() {
    rejected_by(
        TWO_NEUTRALS_STAY_APART_WHERE_A_LOOKUP_GIVES_THEM_A_TYPE_WITH_A_RELEVANT_FIELD,
        "type mismatch",
    );
}

// **A lookup types every side a position can hold without inferring, and a binder conversion opens carries its type.** Where a child is compared with no type, each checker read what a type directs off the type a lookup gave both sides, and three sides had none. A stuck elimination, so that one at a proposition met neither a variable at it nor an elimination of another scrutinee: its type is its result at its scrutinee. A constructor's value, so that a proof built by one did not meet a proof a variable names: its type is its declaration's. And a binder an arm opened, which the kernel assumed at a stand-in and the elaborator recorded no type for: it is opened at its constructor's field type, so two proofs an arm binds are one. Each program sets its pair as a tuple literal's component in a stuck elimination's arm, a child no type reaches.
//
// The refusal beside each is the same pair at a relevant type.
#[test]
fn a_stuck_elimination_is_typed_by_its_result_at_its_scrutinee() {
    assert_eq!(
        run(A_STUCK_ELIMINATION_IS_TYPED_BY_ITS_RESULT_AT_ITS_SCRUTINEE),
        b"1"
    );
}

#[test]
fn a_stuck_elimination_at_a_relevant_type_stays_apart_from_a_variable() {
    rejected_by(
        A_STUCK_ELIMINATION_AT_A_RELEVANT_TYPE_STAYS_APART_FROM_A_VARIABLE,
        "type mismatch",
    );
}

#[test]
fn a_constructors_value_is_typed_by_its_declaration() {
    assert_eq!(run(A_CONSTRUCTORS_VALUE_IS_TYPED_BY_ITS_DECLARATION), b"1");
}

#[test]
fn a_constructors_value_at_a_relevant_family_stays_apart_from_a_variable() {
    rejected_by(
        A_CONSTRUCTORS_VALUE_AT_A_RELEVANT_FAMILY_STAYS_APART_FROM_A_VARIABLE,
        "type mismatch",
    );
}

#[test]
fn an_arms_binders_are_opened_at_the_types_their_constructor_gives_them() {
    assert_eq!(
        run(AN_ARMS_BINDERS_ARE_OPENED_AT_THE_TYPES_THEIR_CONSTRUCTOR_GIVES_THEM),
        b"1"
    );
}

#[test]
fn two_binders_of_an_arm_at_a_relevant_type_stay_apart() {
    rejected_by(
        TWO_BINDERS_OF_AN_ARM_AT_A_RELEVANT_TYPE_STAY_APART,
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

// **A struct literal with no field of data has no eta.** Every field of `Sealed` is a proposition, so the literal's walk against a neutral compares nothing at all, and answering `true` there would make any neutral the literal by the shape of its type: two neutrals would be one through it where their heads keep them apart. Declared in `Type`, the literal and the neutral stay apart; declared a proposition, irrelevance decides the pair before any walk is taken.
//
// The comparison is `Sealed { one = p, two = q }` against `b` as arguments of `f`, and a spine's arguments take the telescope their head carries, so the position is typed.
#[test]
fn a_struct_literal_of_proofs_alone_has_no_eta() {
    rejected_by(A_STRUCT_LITERAL_OF_PROOFS_ALONE_HAS_NO_ETA, "type mismatch");
}

#[test]
fn a_struct_declared_a_proposition_meets_its_literal() {
    assert_eq!(run(A_STRUCT_DECLARED_A_PROPOSITION_MEETS_ITS_LITERAL), b"1");
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

// Two recursions of one shape that part in their base case. Unfolded once, each poses the same two calls again, and a conversion that took a pair it met again as held would equate two definitions that differ.
#[test]
fn two_distinct_recursions_do_not_convert() {
    rejected_by(DISTINCT_RECURSIONS_ARE_NOT_EQUAL, "type mismatch");
}
