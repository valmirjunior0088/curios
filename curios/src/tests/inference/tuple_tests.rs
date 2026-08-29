//! The tuple literal, which is checked-only and therefore lives or dies by its expectation.
//!
//! Two rules meet here. A literal whose expectation never gains structure synthesizes the non-dependent product, because by then nothing is left to send a dependent telescope. A literal whose expectation is merely *stuck* — a `rec` waiting on an index it has not been given — waits instead, since a solution is still coming. The refusals matter as much as the acceptances: a rigid non-tuple expectation and a rigidly stuck one both refuse at the literal.

use crate::tests::{error, run};

// A tuple argument that is the *only* thing determining a call's type variable, where the result type does not mention it — so the apply's turnaround pins nothing and the literal is left holding its own expectation. The force tier settles it there, inside the call, rather than leaving it for the item's drain.
#[test]
fn a_tuple_argument_no_caller_pins_settles_to_its_product() {
    let source = r#"
        use /std/{Nat, Handle};
        let swallow(@A : Type, a : A) -> Nat = 0;
        let n : Nat = swallow((1, 2));
        let _ = Handle/write(Handle/stdout, /std/Str/to_bytes(Nat/to_str(n)))!;
        /std/Io/pure(())
        "#;

    assert_eq!(run(source), b"0");
}

// A non-empty tuple literal parks against a bare expected metavariable, and rightly: a dependent telescope can only ever arrive from the expectation, so committing to the non-dependent product while one could still arrive would be a guess. When the drain has established that nothing is left to send one, the guess is no longer a guess — and `?` must answer, as it already does for `()` and for a list literal.
#[test]
fn a_tuple_literal_synthesizes_when_its_expected_type_never_gains_structure() {
    let source = r#"
        let y : ? = (1, true);
        /std/print("ok\n")
        "#;

    let report = error(source);
    assert!(
        report.contains("? = {Nat, Bool}"),
        "expected the goal to report the synthesized product:\n{report}"
    );
}

// The one-field literal takes the same route. Its trailing comma is all that separates it from a parenthesized term, so a reader has no other way to learn which one the elaborator saw.
#[test]
fn a_one_field_tuple_literal_synthesizes_against_a_written_goal() {
    let source = r#"
        let y : ? = (1,);
        /std/print("ok\n")
        "#;

    let report = error(source);
    assert!(
        report.contains("? = {Nat}"),
        "expected a one-field product:\n{report}"
    );
}

// Settling the literal wakes whatever was parked on the metavariable it solved, and a woken obligation reports for itself. Here that is the missing tuple witness — the answer the program deserves, where before the same program said only that some type never gained structure.
#[test]
fn a_settled_tuple_reports_the_obligation_it_unblocked() {
    let source = r#"
        use /std/{Bool, Show, Str};
        let s : Str = Show/show((true, false));
        /std/print("ok\n")
        "#;

    let report = error(source);
    assert!(
        report.contains("no witness of Show({Bool, Bool})"),
        "expected the witness goal the tuple unblocked:\n{report}"
    );
}

// An expectation that *does* arrive must still win: the product here is written, and the literal is checked against it rather than synthesized.
#[test]
fn a_written_tuple_type_still_checks_the_literal_against_itself() {
    let source = r#"
        use /std/{Nat, Bool, Handle};
        let id(@A : Type, a : A) -> A = a;
        let z : {Nat, Bool} = id((1, true));
        /std/print(Nat/to_str(z.0))
        "#;

    assert_eq!(run(source), b"1");
}

// A description-indexed constructor written the way a user writes one, with the index left implicit. The payload's type is `Count(?L)` — a `rec` that cannot choose an arm until `?L` is known, so reduction hands back the folded call itself. That is not a tuple type *yet*, and refusing there refuses one step before the turnaround that solves `?L` from the written result type.
#[test]
fn a_tuple_payload_waits_for_the_index_that_types_it() {
    let source = r#"
        use /std/{Nat, Str, Handle};
        induct Labels : pub Type | nil() | cons(Str, Labels) end
        rec Count(L : Labels) -> Type =
            match L : (_) => Type
            | nil() => {}
            | cons(l, rest) => {Nat, Count(rest)}
            end;
        induct Boxed(L : Labels) : pub Type | mk(Count(L)) end
        let b : Boxed(Labels/cons("a", Labels/nil())) = Boxed/mk((1, ()));
        match b | mk(_) => /std/print("unlocked") end
        "#;

    assert_eq!(run(source), b"unlocked");
}

// The blocker has to be a *metavariable*, not merely a stuck reduction. `Count(L)` over a bound parameter is stuck for good — no solution is coming — so it refuses at the literal with the expectation named, rather than parking into a report about structure that never arrived.
#[test]
fn a_payload_stuck_on_a_rigid_index_is_refused_at_the_literal() {
    let source = r#"
        use /std/{Nat, Str, Handle};
        induct Labels : pub Type | nil() | cons(Str, Labels) end
        rec Count(L : Labels) -> Type =
            match L : (_) => Type
            | nil() => {}
            | cons(l, rest) => {Nat, Count(rest)}
            end;
        let f(L : Labels) -> Nat =
            let x : Count(L) = (1, ());
            0;
        /std/print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("expected type is not a tuple type") && report.contains("Count(L)"),
        "expected the refusal to name the stuck expectation:\n{report}"
    );
}

// Waiting also sharpens the wrong program's report: once the index solves, the payload type reduces and the literal is measured against it, so a mismatched shape is named as the arity it is instead of as a type that never became a tuple.
#[test]
fn a_settled_index_measures_the_literal_against_the_type_it_computes() {
    let source = r#"
        use /std/{Nat, Str, Handle};
        induct Labels : pub Type | nil() | cons(Str, Labels) end
        rec Count(L : Labels) -> Type =
            match L : (_) => Type
            | nil() => {}
            | cons(l, rest) => {Nat, Count(rest)}
            end;
        induct Boxed(L : Labels) : pub Type | mk(Count(L)) end
        let b : Boxed(Labels/nil()) = Boxed/mk((1, ()));
        /std/print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("tuple has 2 field(s) but expected type has 0"),
        "expected the arity report the reduced payload type makes possible:\n{report}"
    );
}

// Labels are part of a tuple type's identity, so a literal synthesized with nothing expecting it must keep the labels it wrote: the product is what its projections are resolved against, and an unlabeled one has no field called `a`.
#[test]
fn a_labeled_literal_synthesizes_with_its_labels() {
    let source = r#"
        use /std/{Nat, Bool, Handle};
        let z = (a = 1, b = true);
        /std/print(Nat/to_str(z.a))
        "#;

    assert_eq!(run(source), b"1");
}

// The same product, read back through the oracle: the labels are in the type, not only in the literal.
#[test]
fn a_synthesized_labeled_product_reports_its_labels() {
    let source = r#"
        let y : ? = (a = 1, b = true);
        /std/print("ok\n")
        "#;

    let report = error(source);
    assert!(
        report.contains("? = {a: Nat, b: Bool}"),
        "expected the synthesized product to carry its labels:\n{report}"
    );
}
