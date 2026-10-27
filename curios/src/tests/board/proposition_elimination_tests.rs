//! What a proposition may be eliminated into: data where no constructor applies, another proposition always, and a cast along a proof that runs none of it.

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
        "cannot eliminate the proposition 'Side'",
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

// The match states the instance it eliminates and reads nothing of its scrutinee, so a proof written as a `let` or as a function applied where it stands is read no more than a hypothesis is: a cast along either is the cast along the hypothesis.
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

// No checker reduces a proof to take it into data. `slow(n)` proves `n = n` by an induction that reads its hypothesis, so at a hundred billion its proof is a hundred billion steps from `refl`, and a cast that reduced it to fire would run out of steps: closed, the cast is its argument outright; open, `slow(n)` is stuck on `n` and the indices are one all the same; and where a closed match's arm names a payload its equations leave open, as `inner`'s does, the closed machine is handed the payload the instance the match states says and evaluates no more of the proof. The running program casts along it too, with the proof erased.
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

// **A match that takes a proposition into data states the instance it eliminates, and fires on it.** `read` is `g(b)` wherever it stands, its proof unreduced, so a hypothesis about it is one about `g(b)`; and under a case of `b` it is its arm at that case, by the substitution into the term and by nothing read off what `p` is assumed at. The soundness board's elimination part holds the program that forged `False` where the match read that type instead.
#[test]
fn a_match_on_a_pinned_proof_is_its_arm_at_the_index_under_every_case() {
    assert_eq!(
        run(A_MATCH_ON_A_PINNED_PROOF_IS_ITS_ARM_AT_THE_INDEX_UNDER_EVERY_CASE),
        b"ok"
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
