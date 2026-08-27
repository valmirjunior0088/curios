//! What a proposition may carry and what it may be eliminated into.

//! End-to-end coverage for the soundness perimeter entries that nothing else guards.
//!
//! `design.md` states the consistency claim against an enumerated perimeter, and `soundness.md` is that perimeter: one entry per rule, each graded *probed*, *argued*, or *auditable only*. "Probed" is a claim about executable evidence, so it needs a test that fails when the rule stops holding — otherwise the grade records what someone once tried by hand and decays the moment nobody remembers doing it.
//!
//! The entries with their own homes are not repeated here: strict positivity lives in `tests::positivity`, the two totality obligations in `tests::soundness`, and witness coherence in `tests::concepts`. What is left is the large-elimination guard, `Prop` non-informativeness, coverage, and the foreign wire contract — four rules the claim rests on that had no regression test at all.
//!
//! Each rejection asserts its *own* diagnostic, following `tests::soundness`. A perimeter test that accepts any error is worse than none: an invalid fixture passes it while the rule it names goes unchecked. That is not hypothetical — the first draft of these probes "passed" on `unbound variable`, having never reached the check at all.

use super::super::run;

use super::test_support::*;

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
// Both gates asked `carries_information`, so `check_struct_decl` returned `Ok(())` while the hole was open. Its control in `curios-cert` — `a_proposition_may_carry_a_proof`, which keeps a genuine proof field legal — was itself mis-specified, typing its field as the universe `Prop` rather than as a `Prop`-sorted family, so it asserted the admitted shape and passed for the wrong reason. That is why the discrimination this row names went untested on the kernel side for as long as it did.
#[test]
fn a_proposition_may_not_carry_a_type_field() {
    rejected_by(A_PROPOSITION_MAY_NOT_CARRY_A_TYPE_FIELD, "is informative");
}

// Definitional proof irrelevance, at the premise the rule is stated over rather than at the rule: *which* types are propositions. `Prop` is the type of propositions, so anything admitted at `Prop` is one as far as every later rule is concerned, and irrelevance then identifies its inhabitants without looking at them.
//
// `List(P)` is not a proposition however propositional `P` is — a list has a length, so two of them are distinguishable where their elements are not, and `Sort::of` says exactly that in both checkers. The *typing* rule for a parameterized intrinsic former was a second implementation of that one rule and reported the element's sort as the former's, so `List(True)` inferred at `Prop` on both sides. Nothing here needed the former written in a `Prop` position: `@X : Prop` is solved by unification, and the solution is the reduced `ListType` node.
//
// From there every step is the ordinary machinery. `all_equal` is sound and stays accepted below — reflexivity discharges `Eq(@X, x, y)` because irrelevance identifies any two inhabitants of the proposition `X`. Instantiating `X` at `List(True)` yields `Eq(one, none)` for a one-element list against the empty one; `Eq/cong` through `List/len` carries that to `Eq(1, 0)`, and `Eq/subst` transports `()` into `False`.
//
// Verified against the compiler of the day, while the hole was open: this source elaborated and `recheck_module_suffix` on the compile path certified `let /bad : Eq(List True, /one, /none)` with zero refusals — the `wonder stage core-elab` rendering shows the solved `@X` as the `List` former applied to `True`. It never reached a runtime, and not because a checker stopped it: erasure refuses *any* call whose every argument erases, which is a defect of the erase boundary rather than of this rule, and which the control below trips identically at a genuine proposition.
//
// Both controls are load-bearing, because the two ways to "close" this without fixing it are to stop believing `Prop` and to stop believing `List`. Irrelevance must still identify two genuinely different inhabitants of a real proposition, and a list of proofs must still be an ordinary list with a length. The first is asserted through the two-checker matrix rather than by running, since it is the program the erase boundary cannot lower; what it has to establish is that both checkers still accept the lemma and its instantiation.
#[test]
fn a_list_of_proofs_is_not_a_proposition() {
    rejected_by(A_LIST_OF_PROOFS_IS_NOT_A_PROPOSITION, "type mismatch");
}

#[test]
fn a_list_of_proofs_is_still_a_list() {
    assert_eq!(run(A_LIST_OF_PROOFS_IS_STILL_A_LIST), b"1");
}

// Sort formation, the row that had an argument and no program. Both of its rules are *accepting* — a Π into a proposition is a proposition whatever it quantifies over, and a record of propositions is one — so they widen what counts as a proof, and everything irrelevance and erasure do downstream turns on that verdict.
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
