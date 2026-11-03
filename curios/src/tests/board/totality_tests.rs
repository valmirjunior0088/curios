//! A partial definition at an erased position, however it is reached, and what does not count as descent.

use super::test_support::*;

/// A partial definition behind a `Type`-sorted carrier, reached four ways, and a diverging proof at a position the judgment infers rather than checks: each is a proof resting on something that may not terminate, however it is reached.
#[test]
fn a_proof_resting_on_a_partial_definition_is_refused_however_it_is_reached() {
    for source in [
        PARTIAL_DIRECT,
        PARTIAL_THROUGH_WITNESS,
        PARTIAL_HIGHER_ORDER,
        PARTIAL_IN_FIELD,
        INFERRED_PROOF_POSITION,
    ] {
        rejected_by(source, "not known to terminate");
    }
}

#[test]
fn a_recursion_written_inline_under_a_carrier_is_still_partial() {
    rejected_by(INLINE_REC_UNDER_CARRIER, "not known to terminate");
}

#[test]
fn a_type_reaching_a_partial_definition_is_refused() {
    rejected_by(TYPE_REACHING_PARTIAL, "not known to terminate");
}

#[test]
fn saturating_subtraction_is_not_descent() {
    rejected_by(
        SATURATING_SUBTRACTION_IS_NOT_DESCENT,
        "not known to terminate",
    );
}

#[test]
fn permuting_arguments_is_not_descent() {
    rejected_by(PERMUTING_ARGUMENTS_IS_NOT_DESCENT, "not known to terminate");
}

// A structure's field is read by projection, which takes no case of the value apart, so a function field applied off one grades below nothing: `f` would recur for ever along `l`'s `next`, a structure that hands itself back, and is refused as no descent wherever it is reached.
#[test]
fn a_field_applied_off_a_structure_is_not_descent() {
    rejected_by(
        r#"
        use /std/Bool/{False};

        struct S: Type { next: () -> S }

        let l() -> S = S { next = l };

        let f(x: S) -> False = f(x.next());

        let bad: False = f(l());

        /std/print("FORGED")
        "#,
        "reaches 'f', which is not known to terminate",
    );
}

// An application of a payload is a child of its constructor, which is true of a value that is a finite tree (`curios-analysis`'s `totality`), the premise the paradox of trees breaks: `down` descends by it and is admitted, and `x`, whose payload hands back `x` itself, is a tree with no bottom for the descent to reach. A recursive binding is not known to terminate, so the proof that reaches it is refused at `x`, and the descent is never asked of it.
#[test]
fn a_value_that_is_its_own_child_is_not_known_to_terminate() {
    rejected_by(
        r#"
        use /std/Bool/{False};

        induct N: Type
        | s(p: () -> N)
        end

        let down(n: N) -> False = match n | s(p) => down(p()) end;

        let x: N = N/s(() => x);

        let bad: False = down(x);

        /std/print("FORGED")
        "#,
        "reaches 'x', which is not known to terminate",
    );
}

// The size order reads an arm's case for a `Prop`-sorted scrutinee as for any other (`refined_by_arm`), so a recursion descends on an inductive proposition's constructors, and a proof irrelevance holds equal to the scrutinee is then graded below it. What stands between that reading and a closed inhabitant of `False` is coverage, and this is the shape that asks for it: `Q`'s index is a proof, so irrelevance makes `Q(v)` and `Q(Wit/s(v))` one type and the recursive call on the payload is well typed, and `Q/nil()` inhabits `Q(Wit/s(Wit/triv()))` for the same reason — a proof of `Q` at the scrutinee's own type whose payload the descent reads as smaller.
//
// `drain` has no base case, so the forgery needs `nil`'s arm absent, and a proposition's two constructors never clash (`curios-analysis`'s `invert`, whose tag test fires only at a relevant family). So the arm is mandatory and has to inhabit `False`. The refusal is coverage's, not the size order's: a change that let a `Prop`-valued index excuse an omitted arm would hand this program the descent it already has.
#[test]
fn a_recursion_on_a_propositions_case_still_needs_the_arm_it_omits() {
    rejected_by(
        r#"
        use /std/Bool/{False};

        induct Wit: Prop
        | triv()
        | s(inner: Wit)
        end

        induct Q: (Wit) -> Prop
        | nil(): (Wit/triv())
        | cons(v: Wit, inner: Q(v)): (Wit/s(v))
        end

        let drain(v: Wit, q: Q(Wit/s(v))) -> False =
            match q: (_, _) => False
            | cons(w, inner) => drain(w, inner)
            end;

        let bad: False = drain(Wit/triv(), Q/nil());

        /std/print("FORGED")
        "#,
        "is not provably impossible at the scrutinee's indices",
    );
}

/// A proof standing on a partial term its elimination reads nothing of is refused too.
#[test]
fn a_partial_inhabitant_of_an_impossible_index_proves_nothing() {
    rejected_by(PARTIAL_UNDER_AN_EMPTY_ELIMINATION, "not known to terminate");
}
