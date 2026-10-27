//! Two terms at a type with no field or one inhabitant convert, and at a relevant field, a struct that reaches itself, or an elimination, arm binder or constructor at a relevant type, stay apart.

use {super::test_support::*, crate::tests::run};

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
// The refusals beside it are the rule's bounds: one relevant field keeps two neutrals apart, under whatever nesting, and a struct that reaches itself answers no where it is met again, which is what ends the walk — by its own fields or through another struct's, and asked inside its own declaration as after it.
//
// A struct nested in its own parameter is met again too, and is judged again: both checkers refused two variables at `Pair(Pair({}))` where they converged at `Pair(Other({}))`, a verdict that followed which name wrapped a unit.
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

// **A goal's type is read forced in both checkers.** A struct declared with `and` is referred to through its group, and a type a recursive definition computes is a call of it: weak-head reduction leaves both folded, and what a type directs is keyed on the type's own shape and sort. Read in weak-head form, two proofs of a proposition declared in a group and two variables at a struct of units a definition computes are compared by their heads where the same type written out converges, and where a lookup, which forces what it reads, converges at a child no type was handed for. A group is a cycle, and a struct that reaches itself has no one inhabitant by its shape, so the group's members here are propositions.
//
// The refusal beside it is a group's member with a relevant field.
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

// **Where a child is compared with no type, what a type directs between two neutrals is read off the type a lookup gives both.** A tuple literal's component inside a stuck elimination's arm is such a child, and two variables there at a record of units, at a function into a unit or at a struct of a unit were refused by both checkers, which each equate them wherever a type reaches them. Neither side is expanded: eta between two neutrals decides nothing the type's shape does not, and the goal it would pose at a looked-up type is the one in hand, posed again without end. The last program is two proofs of a proposition whose implicit the elaborator solved, which it refused where the same proposition written out converged.
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
