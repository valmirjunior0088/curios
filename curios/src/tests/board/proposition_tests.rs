//! What a proposition may carry and what it may be eliminated into.

use {super::test_support::*, crate::tests::run};

// The large-elimination guard, in the direction that matters for soundness. Every `Box` is definitionally equal to every other by proof irrelevance, so reading a `Nat` back out of one would make 0 and 7 convertible.
#[test]
fn a_multi_constructor_proposition_cannot_be_eliminated_into_data() {
    rejected_by(
        A_MULTI_CONSTRUCTOR_PROPOSITION_CANNOT_BE_ELIMINATED_INTO_DATA,
        "cannot eliminate the proposition",
    );
}

// The same guard, in the direction that matters for the language staying usable. A guard that rejected these would be indistinguishable from one that rejected everything, and `ex falso` and transport are both load-bearing: `/std/Eq/subst` in the prelude is the singleton case.
#[test]
fn an_empty_proposition_still_eliminates_into_data() {
    assert_eq!(run(AN_EMPTY_PROPOSITION_STILL_ELIMINATES_INTO_DATA), b"0");
}

#[test]
fn a_proposition_still_eliminates_into_another_proposition() {
    assert_eq!(
        run(A_PROPOSITION_STILL_ELIMINATES_INTO_ANOTHER_PROPOSITION),
        b"1"
    );
}

// The guard's other refusal: one constructor, holding a proof no index pins. Nothing a program computes could tell which proof it was handed, but the arm is handed the proof the match was given, and a recursion along it has a type reduce the lemma that built it. `f` is that recursion on an accessibility proof, the elimination whose conversion Carneiro (*The Type Theory of Lean*, 2019, §3.1) shows no checker decides transitively: two proofs at one recursive call reduce through different eliminations, and nothing reconciles the calls one step further on.
#[test]
fn a_proposition_holding_a_proof_cannot_be_eliminated_into_data() {
    rejected_by(
        A_PROPOSITION_HOLDING_A_PROOF_CANNOT_BE_ELIMINATED_INTO_DATA,
        "cannot eliminate the proposition",
    );
}

// The control: the same proposition is taken apart into a proposition, which is how an accessibility proof is inverted.
#[test]
fn a_proposition_holding_a_proof_still_eliminates_into_a_proposition() {
    assert_eq!(
        run(A_PROPOSITION_HOLDING_A_PROOF_STILL_ELIMINATES_INTO_A_PROPOSITION),
        b"1"
    );
}

// The guard counts the constructors a family declares, in the elaborator as in the kernel. `Side(n, 0)` leaves `left` alone and its payload is the index, so a guard that counted the arms the index left would admit `which`, which the kernel refuses. The refusal is the elaborator's own.
#[test]
fn a_constructor_an_index_rules_out_still_counts() {
    rejected_by(
        A_CONSTRUCTOR_AN_INDEX_RULES_OUT_STILL_COUNTS,
        "cannot eliminate the proposition '/Side'",
    );
}

// A match that takes a proposition into data fires on its scrutinee's indices, with the proof unreduced. `p` is a hypothesis, so no reduction takes it to `refl`, and conversion already holds `a + b` and `b + a` one number: the cast along it is its argument, where it stood before on a proof nothing could reduce.
#[test]
fn a_cast_along_a_hypothesis_the_algebra_equates_is_its_argument() {
    assert_eq!(
        run(A_CAST_ALONG_A_HYPOTHESIS_THE_ALGEBRA_EQUATES_IS_ITS_ARGUMENT),
        b"1"
    );
}

// The scrutinee's type is looked up, so a proof written as a `let` or as a function applied where it stands is first stepped to the hypothesis it is: neither step unfolds a definition, and a cast along either is the cast along the hypothesis.
#[test]
fn a_cast_along_a_proof_written_as_a_redex_is_its_argument() {
    assert_eq!(
        run(A_CAST_ALONG_A_PROOF_WRITTEN_AS_A_REDEX_IS_ITS_ARGUMENT),
        b"1"
    );
}

// The control: where conversion does not hold the two ends one, the cast stands. Its statement is true, and proved by matching on `p`; a rule that fired on the shape of the match alone would accept it by reflexivity.
#[test]
fn a_cast_along_a_hypothesis_whose_ends_differ_stands() {
    rejected_by(
        A_CAST_ALONG_A_HYPOTHESIS_WHOSE_ENDS_DIFFER_STANDS,
        "type mismatch",
    );
}

// No checker reduces a proof to take it into data. `slow(n)` proves `n = n` by an induction that reads its hypothesis, so at a hundred billion its proof is a hundred billion steps from `refl`, and a cast that reduced it to fire would run out of steps: closed, the cast is its argument outright; open, `slow(n)` is stuck on `n` and the indices are one all the same; and where a closed match's arm names a payload its equations leave open, as `inner`'s does, the closed machine is handed the payload its scrutinee's type states and evaluates no more of the proof. The running program casts along it too, with the proof erased.
#[test]
fn a_cast_along_a_lemma_runs_none_of_the_lemma() {
    assert_eq!(run(A_CAST_ALONG_A_LEMMA_RUNS_NONE_OF_THE_LEMMA), b"7");
}

// An arm may name the payload its indices pin. Where the arm's equations hold the two one, as in `z_of`, the elaborator writes the name as the index; where they leave it open, as in `inner`, whose two indices `x + 1` and `y + 1` do not unify, the arm keeps the name and reduction binds it to the index. Either way the match fires with its proof unreduced: each scrutinee here is a hypothesis. `boxed` is `inner` over a constructor that takes a level: each `Option/some` written takes one of its own until its declaration closes, and the two instances the indices stand at are held to one where the match fires.
#[test]
fn an_arm_that_names_a_pinned_payload_reads_the_index() {
    assert_eq!(
        run(AN_ARM_THAT_NAMES_A_PINNED_PAYLOAD_READS_THE_INDEX),
        b"42"
    );
}

// `Prop` non-informativeness, which is what makes proof irrelevance safe: a proposition whose inhabitants differ observably is not a subsingleton, so identifying them would identify the data they carry.
#[test]
fn a_proposition_may_not_carry_informative_fields() {
    rejected_by(
        A_PROPOSITION_MAY_NOT_CARRY_INFORMATIVE_FIELDS,
        "is informative",
    );
}

// A concept is a structure, so the same rule must reach a `Prop`-sorted concept whose method returns data. Worth its own fixture: the concept path generates its record entry rather than declaring it, so it could regress independently.
#[test]
fn a_proposition_concept_may_not_carry_informative_methods() {
    rejected_by(
        A_PROPOSITION_CONCEPT_MAY_NOT_CARRY_INFORMATIVE_METHODS,
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
// A list of proofs is not a proposition however little its elements carry — a list has a length, so two of them are distinguishable where their elements are not, and `Sort::of` says exactly that in both checkers. The *typing* rule for a parameterized intrinsic former follows that one rule rather than reporting the element's sort as the former's, which would infer such a list at `Prop` on both sides — and nothing here needs the former written in a `Prop` position: `@X : Prop` is solved by unification, and the solution is the reduced `ListType` node. No proposition is an element type, so the list is one of `Evidence(True)`, the type made to hold a proof.
//
// From there every step is the ordinary machinery. `all_equal` is sound and stays accepted below — its conclusion is discharged because irrelevance identifies any two inhabitants of the proposition `X`. Instantiating `X` at the list's type would equate a one-element list with the empty one; congruence through `List/len` carries that to `1` and `0`, and a transport takes `()` into `False`.
//
// Both controls are load-bearing, because the two ways to "close" this without fixing it are to stop believing `Prop` and to stop believing `List`. Irrelevance must still identify two genuinely different inhabitants of a real proposition, and a list of evidence must still be an ordinary list with a length. Both are run: the first has both checkers accept the lemma and its instantiation, and its call, whose every argument erases, reaches the runtime as the program it is.
#[test]
fn a_list_of_proofs_is_not_a_proposition() {
    rejected_by(A_LIST_OF_PROOFS_IS_NOT_A_PROPOSITION, "type mismatch");
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

// `func_sort`, and the half that makes `Prop` impredicative: the domain's level is discarded when the codomain is a proposition, so quantifying over a universe still yields one. That is what puts Coquand–Paulin's construction in range, which is why this rule's soundness is not its own — see the positivity fixture below.
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

// The row's stated attack shape, answered: "a proposition arrived at by *formation* rather than by declaration, carrying something the guards above only ever check where a declaration is consulted". The large-elimination guard is one of those guards, and it does *not* only check where a declaration is consulted — it asks `Sort::of`, which computes formation. So eliminating a two-constructor proposition into an anonymous Σ of `Nat`s is refused exactly as into a bare `Nat`, and the control is the same elimination into an anonymous Σ of proofs, which must stay legal because a proposition eliminated into a proposition needs no condition at all.
#[test]
fn a_proposition_may_not_be_eliminated_into_a_formed_type() {
    rejected_by(
        A_PROPOSITION_MAY_NOT_BE_ELIMINATED_INTO_A_FORMED_TYPE,
        "cannot eliminate the proposition",
    );
}

#[test]
fn a_proposition_still_eliminates_into_a_formed_proposition() {
    assert_eq!(
        run(A_PROPOSITION_STILL_ELIMINATES_INTO_A_FORMED_PROPOSITION),
        b"1"
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
