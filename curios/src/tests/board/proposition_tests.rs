//! What a proposition may carry and what is one: no informative field, method or superclass, no type field or payload over its own sort, and the records, functions and lists that are or are not propositions.

use {
    super::test_support::*,
    crate::tests::{error, run},
};

// `Prop` non-informativeness, which is what makes proof irrelevance safe: a proposition whose inhabitants differ observably is not a subsingleton, so identifying them would identify the data they carry. A hidden field is a field like any other, whatever position a literal gives it.
#[test]
fn a_proposition_may_not_carry_informative_fields() {
    for source in [
        A_PROPOSITION_MAY_NOT_CARRY_INFORMATIVE_FIELDS,
        A_PROPOSITION_MAY_NOT_CARRY_A_HIDDEN_INFORMATIVE_FIELD,
    ] {
        rejected_by(source, "is informative");
    }
}

// A concept is a structure, so the same rule must reach a `Prop`-sorted concept whose method returns data. Worth its own fixture: the concept path generates its record entry rather than declaring it, so it could regress independently.
#[test]
fn a_proposition_concept_may_not_carry_informative_methods() {
    rejected_by(
        A_PROPOSITION_CONCEPT_MAY_NOT_CARRY_INFORMATIVE_METHODS,
        "is informative",
    );
}

// The refusal names the declaration by the keyword it is written with and stands at the field's type, spelling the field as it is written: a superclass edge under `use`, a labeled field by its label.
#[test]
fn an_informative_field_is_reported_as_written_at_its_type() {
    for (source, words, place) in [
        (
            "use /std/{Show};\nconcept Carry(A: Type): Prop { use Show(A), }\n/std/print(\"no\")\n",
            "concept 'Carry' is declared at sort 'Prop' but its field 'use Show(A)' is informative\n  a 'Prop' concept's fields must all be propositions\n",
            "    2 | concept Carry(A: Type): Prop { use Show(A), }\n      |                                    ^^^^^^^",
        ),
        (
            "use /std/{Nat};\nuse /std/Bool/{True};\npub struct Bad: Prop { first: True, value: Nat, }\n/std/print(\"no\")\n",
            "struct 'Bad' is declared at sort 'Prop' but its field 'value: Nat' is informative\n  a 'Prop' struct's fields must all be propositions\n",
            "    3 | pub struct Bad: Prop { first: True, value: Nat, }\n      |                                            ^^^",
        ),
    ] {
        let report = error(source);
        assert!(report.contains(words), "expected the words:\n{report}");
        assert!(report.contains(place), "expected the place:\n{report}");
    }
}

// A superclass edge is a field of the generated record too, one no method wrapper names: two witnesses of a `Prop` concept over a data concept would be one value by irrelevance and hold two dictionaries a projection tells apart.
#[test]
fn a_proposition_concept_may_not_carry_an_informative_superclass() {
    rejected_by(
        A_PROPOSITION_CONCEPT_MAY_NOT_CARRY_AN_INFORMATIVE_SUPERCLASS,
        "is informative",
    );
}

// `Prop` non-informativeness at the same half, and the shorter route: a field whose type is a universe holds a type as data, and a projection reads it back out meeting no elimination guard at all, so two convertible propositions hand `.0` two different types.
//
// Both gates ask `carries_information`. Its control in `curios-cert`, `a_proposition_may_carry_a_proof`, keeps a genuine proof field legal, and types that field as a `Prop`-sorted family rather than as the universe `Prop`: typed at the universe, it would assert the admitted shape and pass for the wrong reason.
#[test]
fn a_proposition_may_not_carry_a_type_field() {
    rejected_by(A_PROPOSITION_MAY_NOT_CARRY_A_TYPE_FIELD, "is informative");
}

// Definitional proof irrelevance, at the premise the rule is stated over rather than at the rule: *which* types are propositions. `Prop` is the type of propositions, so anything admitted at `Prop` is one as far as every later rule is concerned, and irrelevance then identifies its inhabitants without looking at them.
//
// A list of proofs is not a proposition however little its elements carry — a list has a length, so two of them are distinguishable where their elements are not, and `Sort::of` says exactly that in both checkers. The *typing* rule for a parameterized intrinsic former follows that one rule rather than reporting the element's sort as the former's, which would infer such a list at `Prop` on both sides — and nothing here needs the former written in a `Prop` position: `@X : Prop` is solved by unification, and the solution is the reduced `ListType` node. The list is one of `Option(True)`, a type that holds a proof.
//
// From there every step is the ordinary machinery. `all_equal` is sound and stays accepted below — its conclusion is discharged because irrelevance identifies any two inhabitants of the proposition `X`. Instantiating `X` at the list's type would equate a one-element list with the empty one; congruence through `List/len` carries that to `1` and `0`, and a transport takes `()` into `False`.
//
// Both controls are load-bearing, because the two ways to "close" this without fixing it are to stop believing `Prop` and to stop believing `List`. Irrelevance must still identify two genuinely different inhabitants of a real proposition, and a list of evidence must still be an ordinary list with a length. Both are run: the first has both checkers accept the lemma and its instantiation, and its call, whose every argument erases, reaches the runtime as the program it is.
#[test]
fn a_list_of_proofs_is_not_a_proposition() {
    rejected_by(
        A_LIST_OF_PROOFS_IS_NOT_A_PROPOSITION,
        "a type was given where the implicit argument 'P' of 'Same' takes a proposition",
    );
}

#[test]
fn irrelevance_still_identifies_a_propositions_inhabitants() {
    assert_eq!(
        run(IRRELEVANCE_STILL_IDENTIFIES_A_PROPOSITIONS_INHABITANTS),
        b"1"
    );
}

#[test]
fn a_list_of_evidence_is_still_a_list() {
    assert_eq!(run(A_LIST_OF_EVIDENCE_IS_STILL_A_LIST), b"1");
}

// What a proposition may carry. Both of its rules are *accepting* — a Π into a proposition is a proposition whatever it quantifies over, and a record of propositions is one — so they widen what counts as a proof, and everything irrelevance and erasure do downstream turns on that verdict.
//
// The pair below is `tuple_sort`. A record is `Prop`-sorted only where every component pushed no level, which is what makes an anonymous Σ non-informative *by formation* where a declared `struct` needs `check_non_informative` to make it so — and the two predicates are the same one, so the declaration gate is what these fixtures read the formation verdict through. The empty record is the carve-out and has to fall the other way: `{}` is the unit type an effect returns, so calling it a proposition would erase a value the runtime needs.
#[test]
fn a_record_of_propositions_is_a_proposition() {
    assert_eq!(run(A_RECORD_OF_PROPOSITIONS_IS_A_PROPOSITION), b"1");
}

#[test]
fn the_empty_record_is_not_a_proposition() {
    rejected_by(THE_EMPTY_RECORD_IS_NOT_A_PROPOSITION, "is informative");
}

// `func_sort`: a function into a proposition is a proposition whatever it quantifies over, at the join of its domains' levels and its own, so quantifying over a universe yields a proposition a level above it, never one among those it ranges over. The rule's soundness is not its own all the same: a family that is its own double function space is kept out by positivity — see the positivity fixture below.
#[test]
fn a_function_into_a_proposition_is_a_proposition() {
    assert_eq!(run(A_FUNCTION_INTO_A_PROPOSITION_IS_A_PROPOSITION), b"1");
}

#[test]
fn a_function_into_a_type_is_not_a_proposition() {
    rejected_by(
        A_FUNCTION_INTO_A_TYPE_IS_NOT_A_PROPOSITION,
        "is informative",
    );
}

// A proposition is sized as a type is. Unsized, `Bad` held a payload quantifying over the sort it sat in, the payload applied at `Bad` handed back the value it was given, and the application read as a descent: the ticket in `xboard`'s totality part.
#[test]
fn a_payload_quantifying_over_its_own_propositions_sort_is_refused() {
    rejected_by(
        A_PAYLOAD_QUANTIFYING_OVER_ITS_OWN_PROPOSITIONS_SORT,
        "this Prop would need to be strictly below itself",
    );
}

// The neighbouring route in `Type`, which no exemption opens: a payload an index pins counts at its level like any other.
#[test]
fn a_large_payload_pinned_by_an_index_is_refused() {
    rejected_by(A_LARGE_PAYLOAD_PINNED_BY_AN_INDEX, "strictly below itself");
}

// The neighbouring route through a data family: its payload is a proof of a proposition one level up, which counts at that level, so the family cannot be squashed into the propositions the payload ranges over.
#[test]
fn a_proof_payload_of_a_data_family_counts_at_its_level() {
    rejected_by(
        A_PROOF_PAYLOAD_REACHED_THROUGH_A_SQUASH,
        "strictly below itself",
    );
}

// The control: the family is admitted at the level its payload fits under, and the payload is applied at a proposition below it.
#[test]
fn a_proposition_holding_a_polymorphic_proof_is_admitted_at_its_level() {
    assert_eq!(
        run(A_PROPOSITION_HOLDING_A_POLYMORPHIC_PROOF_IS_ADMITTED),
        b"1"
    );
}

// The same sizing through a record rather than an `induct`: a `Prop`-sorted `struct` holding `(A: Prop) -> (A) -> A` sits at the level of the propositions its field ranges over, so projecting the field and applying it at the struct itself asks that `Prop` to be strictly below itself. The `induct` neighbours above refuse the declaration where its constructor is sized; the struct is admitted and the paradox is stopped at the self-instantiation, through `tuple_sort`'s field sizing rather than constructor sizing. `a_struct_proposition_holding_a_polymorphic_proof_is_admitted` is the control, the same field applied at a proposition below it.
#[test]
fn a_struct_proposition_quantifying_over_its_own_sort_is_refused() {
    rejected_by(
        A_STRUCT_PROPOSITION_QUANTIFYING_OVER_ITS_OWN_SORT,
        "this Prop would need to be strictly below itself",
    );
}

#[test]
fn a_struct_proposition_holding_a_polymorphic_proof_is_admitted() {
    assert_eq!(
        run(A_STRUCT_PROPOSITION_HOLDING_A_POLYMORPHIC_PROOF_IS_ADMITTED),
        b"1"
    );
}
