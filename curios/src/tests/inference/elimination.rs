//! What an elimination tells the elaborator.
//!
//! Two things, and they are why a match writes no result: the result type inferred from the arms, and the definitional refinement each arm carries for its own body. A guard is the same mechanism written after an arm's pattern.

use crate::tests::test_support::*;

#[test]
fn a_recursion_matches_at_its_declared_result() {
    // The same recursion as `triangular_sum`: the match is checked at the declared result, `std/Nat`, and each arm at it.
    let source = r#"
        let triangle(n : std/Nat) -> std/Nat =
            match n
            | 0 => 0
            | pred + 1 => std/Nat/add(triangle(pred), pred)
            end;
        let result : std/Nat = triangle(5);
        let entry() -> /std/Io({}) =
            let _ = /std/print(std/Nat/to_str(result))!;
            /std/Io/pure(());
        entry()
        "#;

    assert_eq!(run(source), b"10");
}

#[test]
fn a_goal_over_a_compound_scrutinee_is_read_at_each_case() {
    // The goal `Vec(Nat, Nat/add(k, k))` writes the compound scrutinee, so each arm reads it with its case in the scrutinee's place: `Vec(Nat, 0)`, and `Vec(Nat, pred + 1)`.
    let source = r#"
        use /std/{Nat, Vec, Io};
        let build(n : Nat) -> Vec(Nat, n) =
            match n
            | 0 => Vec/nil()
            | pred + 1 => Vec/cons(0, build(pred))
            end;
        let d(k : Nat) -> Vec(Nat, Nat/add(k, k)) =
            match Nat/add(k, k)
            | 0 => Vec/nil()
            | pred + 1 => build(Nat/succ(pred))
            end;
        /std/print(Nat/to_str(Vec/length(d(2))))
        "#;

    assert_eq!(run(source), b"4");
}

// Scrutinee refinement keys on the applied head's tag, `Term::head_key`, which the reducer's refinement lookup gates on. A concept-dispatched comparison reduces past the `Ord` witness to an intrinsic normal form, which is not an application, so `head_key` tags the intrinsic: an untagged `match a <= hi` would register a refinement key the probe never looks up, and the arm would silently fail to refine where the equivalent `Nat/le(a, hi)` spelling refines. Operators must be usable in a proof-carrying position, not just the intrinsic spelling.
#[test]
fn operator_scrutinee_refines_a_proof_carrying_arm() {
    let source = r#"
        use /std/{Nat, Option};
        use /std/Bool/{True, False};
        let AtMost(a : Nat, hi : Nat) -> Prop =
            match a <= hi
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

/// A guard's refinement discharges a window bound whose spelling sits one definitional step away: the slice obligation states its end as `0 + n`, the guard can only spell `n <= List/length(l)`, and the guard's reduced spelling brings the two together. The refinement store records a universes-erased key, and erasure strips the `Instance` a polymorphic global (`List/length`) unfolds through, so a settlement reduces the unerased original stored beside the key: the *erased* key would stall where the goal side reduces, and the bound would be reported as an uninferred implicit against a caller who had established it.
#[test]
fn a_guard_discharges_a_bound_spelled_one_reduction_away() {
    let source = r#"
        use /std/{Nat, List, Str, Io};
        let take(l: List(Nat), n: Nat) -> List(Nat) =
            match n <= List/length(l) | true => List/slice(l, 0, n) | false => [] end;
        /std/print(Str/concat(Nat/to_str(List/length(take([1, 2, 3], 2))), "\n"))
        "#;
    assert_eq!(run(source), b"2\n");
}
