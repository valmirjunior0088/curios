//! What an elimination tells the elaborator.
//!
//! Two things, and they are why the motive may be omitted at all: the result type inferred from the arms, and the definitional refinement each arm carries for its own body. A guard is the same mechanism wearing `choose`'s syntax.

use crate::tests::run;

#[test]
fn match_omitted_motive_infers() {
    // The same induction as `triangular_sum`, but with the motive omitted. It is non-dependent (every arm has type `std/Nat`), so the synthesized metavar motive is solved by the arms — no explicit `: std/Nat` needed.
    let source = r#"
        let result : std/Nat =
            match 5
            | 0 => 0
            | pred + 1; ih => std/Nat/add(ih, pred)
            end;
        let _ = std/Io/write(std/Io/stdout, /std/Str/to_bytes(std/Nat/to_str(result)))!;
        /std/Io/pure(())
        "#;

    assert_eq!(run(source), b"10");
}

#[test]
fn omitted_motive_infers_over_a_compound_scrutinee() {
    // The motive hole's scope is opened with the scrutinee — a non-pattern spine entry when the scrutinee is compound. Occurrence abstraction in `solve` rewrites the scrutinee's occurrences in the expected type to the motive binder, so the dependent motive infers where it previously had to be spelled.
    let source = r#"
        use /std/{Nat, Vec, Io};
        let build(n : Nat) -> Vec(Nat, n) =
            match n : (m) => Vec(Nat, m)
            | 0 => Vec/nil()
            | pred + 1; ih => Vec/cons(0, ih)
            end;
        let d(k : Nat) -> Vec(Nat, Nat/add(k, k)) =
            match Nat/add(k, k)
            | 0 => Vec/nil()
            | pred + 1; ih => build(Nat/succ(pred))
            end;
        /std/print(Nat/to_str(Vec/len(d(2))))
        "#;

    assert_eq!(run(source), b"4");
}

// Scrutinee refinement keys on the applied head's *label* (the reducer's Rung-B probe in `reduce`). A concept-dispatched comparison reduces past the `Compare` wrapper to an intrinsic normal form, which is not an application — so before `head_label` covered intrinsics, `match a <= hi` registered a refinement key the probe could never look up and the arm silently failed to refine, while the equivalent `Nat/le(a, hi)` spelling worked. Operators must be usable in a proof-carrying position, not just the intrinsic spelling.
#[test]
fn operator_scrutinee_refines_a_proof_carrying_arm() {
    let source = r#"
        use /std/{Nat, Option, True, False};
        let AtMost(a : Nat, hi : Nat) -> Prop =
            match a <= hi : (_) => Prop
            | false => False
            | true => True
            end;
        let certify(a : Nat, hi : Nat) -> Option(AtMost(a, hi)) =
            match a <= hi
            | false => Option/none()
            | true => Option/some(True/qed())
            end;
        match certify(3, 9)
        | some(_) => /std/print("refined")
        | none() => /std/print("no")
        end
        "#;

    assert_eq!(run(source), b"refined");
}

/// A guard's refinement discharges a window bound whose spelling sits one definitional step away: the slice obligation states its end as `0 + n`, the guard can only spell `n <= List/len(l)`, and the probe-time canonicalization brings the two together. The regression this pins: the refinement store records a universes-erased key, and erasure strips the `Instance` a polymorphic global (`List/len`) unfolds through — so canonicalizing the *erased* key stalled where the goal side reduced, and the bound reported as an uninferred implicit against a caller who had established it. The canonicalization now reduces the unerased original stored beside the key.
#[test]
fn a_guard_discharges_a_bound_spelled_one_reduction_away() {
    let source = r#"
        use /std/{Nat, List, Str, Io};
        let take(l: List(Nat), n: Nat) -> List(Nat) =
            match n <= List/len(l) | true => List/slice(l, 0, n) | false => [] end;
        /std/print(Str/concat(Nat/to_str(List/len(take([1, 2, 3], 2))), "\n"))
        "#;
    assert_eq!(run(source), b"2\n");
}
