//! An arm is erased under the refinement it was elaborated under, however it is lowered: a case split as well as a fold, an expression scrutinee as well as a variable. Erasure re-types what it walks — the head of an application is inferred again — so an arm whose proof reads the scrutinee at its case is refused unless the case is in scope.

use crate::tests::run;

// The successor arm ignores its hypothesis, so it erases as a case split rather than a fold. The proof `Lt(xp, x)` holds only because `x` is `xp + 1` in the arm, and the curried call makes erasure infer `ih(xp, …)` again: a predecessor spelled `x - 1`, with nothing refined, left it asking for `x - 1 < x`.
#[test]
fn a_nat_case_split_is_erased_under_its_successor() {
    let source = r#"
        use /std/{Nat, Bool};
        use /std/Nat/{Lt};
        let f(x: Nat, ih: (y: Nat, r: Lt(y, x)) -> (Nat) -> Nat, m: Nat) -> Nat =
            match x | 0 => m | xp + 1 => ih(xp, Bool/True/qed())(m) end;
        /std/print(Nat/to_str(f(5, (y, _) => (m) => y + m, 10)))
        "#;
    assert_eq!(run(source), b"14");
}

// An expression scrutinee is refined under its own spelling, which is the one the arm's proof mentions. Refining an alias for it instead — a fresh variable defined as `count(x)` — left `Lt(xp, count(x))` undecided even in a fold's step.
#[test]
fn a_nat_fold_over_an_expression_is_erased_under_its_successor() {
    let source = r#"
        use /std/{Nat, Bool};
        use /std/Nat/{Lt};
        let count(n: Nat) -> Nat = match n | 0 => 0 | k + 1 => count(k) + 1 end;
        let f(x: Nat, ih: (y: Nat, r: Lt(y, count(x))) -> (Nat) -> Nat, m: Nat) -> Nat =
            match count(x) | 0 => m | xp + 1; h => h + ih(xp, Bool/True/qed())(m) end;
        /std/print(Nat/to_str(f(3, (y, _) => (m) => y + m, 10)))
        "#;
    assert_eq!(run(source), b"43");
}

// Both at once: a case split over an expression, whose predecessor is computed from the erased operand rather than re-derived from the expression.
#[test]
fn a_nat_case_split_over_an_expression_is_erased_under_its_successor() {
    let source = r#"
        use /std/{Nat, Bool};
        use /std/Nat/{Lt};
        let count(n: Nat) -> Nat = match n | 0 => 0 | k + 1 => count(k) + 1 end;
        let f(x: Nat, ih: (y: Nat, r: Lt(y, count(x))) -> (Nat) -> Nat, m: Nat) -> Nat =
            match count(x) | 0 => m | xp + 1 => ih(xp, Bool/True/qed())(m) end;
        /std/print(Nat/to_str(f(5, (y, _) => (m) => y + m, 10)))
        "#;
    assert_eq!(run(source), b"14");
}

// A sequence case split binds its element and suffix as values the peel fills, and the arm is erased with the scrutinee standing for their cons: `len(t) < len(l)` is decided only there.
#[test]
fn a_list_case_split_is_erased_under_its_cons() {
    let source = r#"
        use /std/{Nat, Bool, List};
        use /std/Nat/{Lt};
        let f(l: List(Nat), g: (t: List(Nat), r: Lt(List/len(t), List/len(l))) -> (Nat) -> Nat) -> Nat =
            match l | [] => 0 | [_, ..t] => g(t, Bool/True/qed())(1) end;
        /std/print(Nat/to_str(f([1, 2, 3], (t, _) => (m) => List/len(t) + m)))
        "#;
    assert_eq!(run(source), b"3");
}

// The packed carrier takes the same path as `List`, through its own cons value.
#[test]
fn a_bytes_case_split_is_erased_under_its_cons() {
    let source = r#"
        use /std/{Nat, Bool, Bytes};
        use /std/Nat/{Lt};
        let f(b: Bytes, g: (t: Bytes, r: Lt(Bytes/len(t), Bytes/len(b))) -> (Nat) -> Nat) -> Nat =
            match b | x[] => 0 | x[_, ..t] => g(t, Bool/True/qed())(1) end;
        /std/print(Nat/to_str(f(x[1, 2, 3], (t, _) => (m) => Bytes/len(t) + m)))
        "#;
    assert_eq!(run(source), b"3");
}

// A sequence fold's step over an expression scrutinee, refined under the expression's spelling as the `Nat` fold's is.
#[test]
fn a_list_fold_over_an_expression_is_erased_under_its_cons() {
    let source = r#"
        use /std/{Nat, Bool, List};
        use /std/Nat/{Lt};
        let f(l: List(Nat), g: (t: List(Nat), r: Lt(List/len(t), List/len(List/reverse(l)))) -> (Nat) -> Nat) -> Nat =
            match List/reverse(l) | [] => 0 | [_, ..t]; h => h + g(t, Bool/True/qed())(1) end;
        /std/print(Nat/to_str(f([1, 2, 3], (t, _) => (m) => List/len(t) + m)))
        "#;
    assert_eq!(run(source), b"6");
}
