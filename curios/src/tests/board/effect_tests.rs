//! An effect is not a value: what may be a scrutinee, an argument, or inhabit a pure arrow, and what may take a description apart.

use {super::test_support::*, crate::tests::run};

// The arm rule at its one arm with no case value of its own. A `| _ =>` catch-all binds nothing and refines no index, so the only instance it can be checked at is the scrutinee's — which is the instance the elimination then hands its caller.
//
// This is a disagreement between the two checkers rather than a route to `False`: the elaborator checks the catch-all at the actual scrutinee, and a kernel opening the motive's scrutinee binder at the family *type* `/Three` would refuse this program with `/Three` standing in both term positions of the `Eq`. That direction fails closed — nothing well-typed inhabits an expectation with a type substituted for a value — but the certifier's own judgment is the one that matters, and it would establish nothing about the scrutinee for any catch-all it accepted. No prelude catch-all has a scrutinee-dependent motive, so this fixture is where the rule is asked.
//
// The instance is pinned where the rule lives, in `curios_cert::kernel::infer::eliminate::coverage_tests`: `a_catch_all_sees_its_own_scrutinee` with its control `a_catch_all_at_another_value_of_the_family_is_refused`, which asserts the expectation itself so that not checking the catch-all cannot pass for closing the hole.
#[test]
fn a_catch_all_is_checked_at_its_scrutinee() {
    assert_eq!(run(A_CATCH_ALL_IS_CHECKED_AT_ITS_SCRUTINEE), b"1");
}

// The purity premise every refinement rests on: an arm is reached only when its scrutinee *equals* the case's value, which a term of non-`Io` type does because it denotes one value. An effectful read does not — two occurrences of `Cell/fill(c, true)` around a `Cell/fill(c, false)` would denote two values, refining both would re-read `p : Eq()(Cell/fill(c, true), true)` at `Eq()(false, true)` in the inner arm, and `/std/Bool/false_neq_true` would turn that into `/std/Bool/False` in an arm the run reaches. Its type is `Io(Bool)`, opaque and with no cases, so nothing lets the program treat it as a `Bool`, and the refusal names `Io`.
#[test]
fn an_effectful_scrutinee_is_not_a_value() {
    rejected_by(AN_EFFECTFUL_SCRUTINEE_IS_NOT_A_VALUE, "Io");
}

#[test]
fn a_match_on_a_forced_cell_read_still_compiles() {
    assert_eq!(run(A_MATCH_ON_A_FORCED_CELL_READ_STILL_COMPILES), b"t");
}

/// Asserted on the *argument*: the fixture uses `Cell/fill(c, true) : Io(Bool)`, which `f : (Bool) -> Bool` does not take, so the derivation is refused at the first occurrence and never reaches a refinement at all.
#[test]
fn an_effect_behind_a_stuck_head_is_not_an_argument() {
    rejected_by(AN_EFFECT_BEHIND_A_STUCK_HEAD_IS_NOT_AN_ARGUMENT, "Io");
}

#[test]
fn a_stuck_application_scrutinee_still_refines() {
    assert_eq!(run(A_STUCK_APPLICATION_SCRUTINEE_STILL_REFINES), b"t");
}

/// Asserted on the offending *argument*: the refusal is that `(b) => Cell/fill(c, true)` cannot be passed where a `(Bool) -> Bool` is wanted, so the description type has to appear in the diagnostic. A fixture refused anywhere else — at the cell, at `Eq/refl`, at an arm — would not produce that, and this file's rule is that a board test asserts its own diagnostic.
#[test]
fn an_effect_cannot_inhabit_a_pure_arrow() {
    rejected_by(AN_EFFECT_CANNOT_INHABIT_A_PURE_ARROW, "Io");
}

#[test]
fn a_parameter_headed_scrutinee_refines_again() {
    assert_eq!(run(A_PARAMETER_HEADED_SCRUTINEE_REFINES_AGAIN), b"t");
}

// The control the four refusals below are read against, and what makes them the only thing in the way: a description of a computation yielding `False` is *constructible*. `exit` is typed `(@A : Type, Byte) -> Io(A)`, so no proposition may be its result — but a carrier with no constructors may, and `bind`'s continuation receives the `Empty` and eliminates it into `False` inside another description. The program compiles and runs, which is what the decision means by an inhabitant of `Io(False)` proving nothing.
#[test]
fn a_description_of_a_contradiction_still_compiles() {
    assert_eq!(run(A_DESCRIPTION_OF_A_CONTRADICTION_STILL_COMPILES), b"ok");
}

// No match case. The carrier has no constructors, which is exactly the shape the zero-arm match eliminates — so the one rule that takes a type apart without naming a case is the first thing to try on a description, and `Io(Empty)` would hand `False` out of the fixture above. The refusal names the head's type, so it is the carrier that is refused and not the arity.
#[test]
fn a_description_is_no_empty_type() {
    rejected_by(
        A_DESCRIPTION_IS_NO_EMPTY_TYPE,
        "a match with no arms eliminates only a type with no constructors",
    );
}

// No projection, which is the elimination that meets no guard at all: a field read answers whatever the record holds, so a description that read as a record of its payload would release it with nothing to check.
#[test]
fn a_description_is_no_record() {
    rejected_by(A_DESCRIPTION_IS_NO_RECORD, "projected from a non-tuple");
}

// An eliminator in any spelling, and the one spelling a program may declare for itself: `Monad/Lift(M, N)`'s `lift` carries an action from one monad into another, and `N` is the caller's to choose. Chosen as a nominal wrapper, the embedding is `(Io(A)) -> Box(A)`, whose only honest body needs the `A` — so the whole discipline reduces to there being no term of `(Io(A)) -> A`, and the refusal stands in the witness where the author would write one. A lambda-shaped identity monad cannot even be keyed, which is why this is stated over a struct.
#[test]
fn a_description_cannot_be_carried_out_of_its_monad() {
    rejected_by(
        A_DESCRIPTION_CANNOT_BE_CARRIED_OUT_OF_ITS_MONAD,
        "the witness of 'Monad/Lift' for key '(Io, Box)'",
    );
}

// The carrier is opaque to conversion as well, which is the eliminator a transport would need: held one with what it yields, `Eq/refl()` would inhabit the identification and `Eq/subst` would do the rest, with no elimination of a description written anywhere. It is asserted here rather than in the law grid because it is this rule that rests on it.
#[test]
fn a_description_is_not_what_it_yields() {
    rejected_by(
        A_DESCRIPTION_IS_NOT_WHAT_IT_YIELDS,
        "Eq(@Type)(Io(Empty), Empty)",
    );
}
