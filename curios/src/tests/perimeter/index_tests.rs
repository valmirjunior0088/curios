//! Index inversion and K: what forces a binder, and what may not excuse an omitted arm.

//! End-to-end coverage for the soundness perimeter entries that nothing else guards.
//!
//! `design.md` states the consistency claim against an enumerated perimeter, and `soundness.md` is that perimeter: one entry per rule, each graded *probed*, *argued*, or *auditable only*. "Probed" is a claim about executable evidence, so it needs a test that fails when the rule stops holding — otherwise the grade records what someone once tried by hand and decays the moment nobody remembers doing it.
//!
//! The entries with their own homes are not repeated here: strict positivity lives in `tests::positivity`, the two totality obligations in `tests::soundness`, and witness coherence in `tests::concepts`. What is left is the large-elimination guard, `Prop` non-informativeness, coverage, and the foreign wire contract — four rules the claim rests on that had no regression test at all.
//!
//! Each rejection asserts its *own* diagnostic, following `tests::soundness`. A perimeter test that accepts any error is worse than none: an invalid fixture passes it while the rule it names goes unchecked. That is not hypothetical — the first draft of these probes "passed" on `unbound variable`, having never reached the check at all.

use super::super::run;

use super::test_support::*;

// The large-elimination guard again, at its *singleton* rung. A one-constructor proposition may eliminate into data only when every payload binder is non-informative — a proposition itself, or *pinned* by the constructor's index targets, as `Eq`'s `refl(@z) : (z, z)` recovers `z`.
//
// Occurring in an index target is not the same as being determined by one. `blur` is constant, so `Loose(0)` is inhabited by `mk(0)` and by `mk(7)` alike, and no index tells them apart — proof irrelevance identifies the two inhabitants while `extract` would observe them apart, and the gap is a closed inhabitant of `False`. `singleton_eliminable` once read `a` as forced because it *occurs* in `blur(a)` — a syntactic occurrence test — and this program printed "FORGED". Both checkers now decide the condition by the shared `pinned_by_targets` walk: a binder counts only when matching a value against the target recovers it, which `blur(a)` never does.
//
// The two ends of the discrimination are covered alongside: the same declaration with target `(0)` is rejected below, and `(a)` is a genuinely forced binder that must stay accepted.
#[test]
fn a_non_injective_index_target_does_not_force_its_binder() {
    rejected_by(
        A_NON_INJECTIVE_INDEX_TARGET_DOES_NOT_FORCE_ITS_BINDER,
        "cannot eliminate the proposition",
    );
}

// Proof irrelevance and index inversion disagree about a `Prop`-valued index, and the disagreement is a closed inhabitant of `False`. Conversion identifies `Two/a()` with `Two/b()` — any two inhabitants of a proposition are equal — so `Ind(Two/a())` and `Ind(Two/b())` are the same type and `coerce` is well typed. Inversion decides a case is impossible by *syntactic* constructor clash (`invert_indices` decomposes constructor forms and clashes on distinct tags, with no sort condition), so it reads `only`'s target `Two/a()` against the actual index `Two/b()` as disjoint and accepts the arm-less elimination as vacuous — at a type conversion just proved inhabited.
//
// Verified against the compiler of the day, while the hole was open: this source compiled (`curios compile` exited 0, and `recheck_module_suffix` on the compile path certified `let /bad : False = boom(coerce(only()))`), and running it trapped at the `unreachable` the vacuous elimination emitted, which is the runtime witness that the impossibility claim was false. The rule that refuses it now exists: a clash may only be concluded at a position whose type distinguishes its inhabitants, and the shared walk both checkers reach decides that from the declaration's own `result_sort` (`curios-analysis/src/invert.rs`), deriving nothing at all — no clash, no equations — at a `Prop`-valued position.
//
// The refusal is the coverage rule's: with the clash retracted, `only` is an ordinary reachable constructor, and an elimination with no arm for it is missing one it cannot prove absent.
#[test]
fn a_proposition_valued_index_cannot_make_an_elimination_vacuous() {
    rejected_by(
        A_PROPOSITION_VALUED_INDEX_CANNOT_MAKE_AN_ELIMINATION_VACUOUS,
        "is not provably impossible at the scrutinee's indices",
    );
}

// The same disagreement through coverage rather than vacuity, which is why the fix belongs in the shared walk and not at one of its callers: here the elimination has an arm, and it is the *omitted* one that inversion wrongly excuses. `Ind/right()` inhabits `Ind(Two/a())` by the same conversion, so the match falls through every arm it enumerated.
#[test]
fn a_proposition_valued_index_cannot_excuse_an_omitted_arm() {
    rejected_by(
        A_PROPOSITION_VALUED_INDEX_CANNOT_EXCUSE_AN_OMITTED_ARM,
        "is not provably impossible at the scrutinee's indices",
    );
}

// The same rule one decomposition step down, which is where its implementation could most plausibly have parted from its statement. `invert_indices` decides the `Prop` condition from the *family of the values being compared* rather than from the declared type of the position, and it decides it wherever the recursion reaches rather than only at the top of one: here `Pair` is relevant and the `Two` it carries is not, so the condition has to fire inside a matching constructor's payload. Read off the index domain instead — `Pair : Type`, therefore relevant — or applied only at `top`, `Two/a()` against `Two/b()` would clash at depth exactly as it once did at the surface, and both routes below would be closed inhabitants of `False` again.
//
// Both were refused when run, and the part worth keeping is that `coerce` elaborated in each: conversion *did* identify the two `Ind` instances through the nested proof, so the premise each exploit needs was available and inversion's refusal to clash on it is the only thing that stood in the way. Null result; the probes are recorded so the rung is not re-attacked.
#[test]
fn a_nested_proposition_valued_index_cannot_make_an_elimination_vacuous() {
    rejected_by(
        A_NESTED_PROPOSITION_VALUED_INDEX_CANNOT_MAKE_AN_ELIMINATION_VACUOUS,
        "is not provably impossible at the scrutinee's indices",
    );
}

#[test]
fn a_nested_proposition_valued_index_cannot_excuse_an_omitted_arm() {
    rejected_by(
        A_NESTED_PROPOSITION_VALUED_INDEX_CANNOT_EXCUSE_AN_OMITTED_ARM,
        "is not provably impossible at the scrutinee's indices",
    );
}

// Coverage's *accepting* rung, which had no fixture of its own: an omitted arm excused because its index target genuinely clashes with the scrutinee's. Every fixture above asserts the refusing direction, so a change that made every absent arm mandatory would break the standard library and nothing in this file — and the standard library leans on this rung hard, at 14 excusals over `/syn/Str/Utf8`, `/std/Nat/Le/Ind`, and `/std/Vec` in one kernel walk of the prelude, with no omitted arm ever coming back mandatory. `AN_EMPTY_PROPOSITION_STILL_ELIMINATES_INTO_DATA` is not this control: a family with no constructors leaves the coverage loop with nothing to iterate, so it exercises the loop's absence rather than its verdict.
//
// It is deliberately the nested shape, which is what discriminates the two refusals above rather than merely sitting beside them: the clash is at the same depth and differs only in that `Pair/mk(0)` and `Pair/mk(1)` are values a program can tell apart.
#[test]
fn a_nested_relevant_clash_still_excuses_an_omitted_arm() {
    assert_eq!(
        run(A_NESTED_RELEVANT_CLASH_STILL_EXCUSES_AN_OMITTED_ARM),
        b"0"
    );
}

// The lower end of that discrimination: drop `a` from the index target and the guard fires. Without this, a fix could "close" the hole above by rejecting every indexed proposition and nothing here would notice.
#[test]
fn an_unmentioned_payload_binder_is_not_forced() {
    rejected_by(
        AN_UNMENTIONED_PAYLOAD_BINDER_IS_NOT_FORCED,
        "cannot eliminate the proposition",
    );
}

// The singleton rung's side condition at its other half: a payload that is *itself a type*. `mk(A : Type)` pins nothing, so eliminating a `Box` recovers the type it was built with while irrelevance says every `Box` is the same one — `Eq/cong` then equates any two types and `Eq/subst` transports `0` into `False`.
//
// The elaborator refuses at `unbox`, so the items after it never elaborate; they document the route rather than being checked. The certifier is where this rule needed backing up and where it was wrong: `carries_information` reported a component whose type is a universe as carrying nothing, on the reasoning that erasure deletes a type either way — which confuses what the runtime observes with what conversion observes. `recheck_module_verdicts` certified the hand-built equivalent of this program with zero refusals, memos on and off.
//
// No surface program reaches that gate, which is what the `NotAsked` in this row's kernel column means. The executable guarantee therefore lives beside the rule: `curios_cert::recheck::tests::a_derivation_through_a_type_carrying_proposition_is_refused` holds the whole derivation shut, and the two singleton fixtures in `curios_cert::kernel::infer::eliminate::tests` pin the predicate at both halves of the clause that admitted it.
#[test]
fn a_singleton_carrying_a_type_does_not_eliminate_into_a_type() {
    rejected_by(
        A_SINGLETON_CARRYING_A_TYPE_DOES_NOT_ELIMINATE,
        "cannot eliminate the proposition",
    );
}
