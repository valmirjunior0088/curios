//! An arm is erased under what it was elaborated under, however it is lowered: a case split under its scrutinee's refinement, an expression scrutinee as well as a variable, and a fold's step under none, since reduction takes it at every tail. Erasure reads the type of each head it eliminates off the finished term and judges nothing, so a proof an arm passes is erased by its sort whatever is in scope, and what the refinement decides is a type's head: a case split's arm calls what has a function type only at the scrutinee's case, and is read with the scrutinee standing for that case.
//!
//! Mutation-checked: with the refinement taken out of the successor arm and the cons arm of erasure's case splits (`curios-elab`'s `into_ersd/eliminate.rs`), each case split below panics at `erase: applied a non-function`, and the two folds, whose steps refine nothing, are unmoved.

use crate::tests::run;

// The successor arm ignores its hypothesis, so it erases as a case split rather than a fold. `g` is typed at `Step(x)`, a function type at a successor and at nothing else, so the call is read with `x` standing for `xp + 1`: with nothing refined, erasure meets a stuck `match` where it needs the callee's telescope.
#[test]
fn a_nat_case_split_is_erased_under_its_successor() {
    let source = r#"
        use /std/{Nat};
        let Step(n: Nat) -> Type =
            match n: (_) => Type | 0 => Nat | k + 1 => (Nat) -> Nat end;
        let f(x: Nat, g: Step(x), m: Nat) -> Nat =
            match x | 0 => m | xp + 1 => g(m) + xp end;
        /std/print(Nat/to_str(f(5, (k) => k + 5, 4)))
        "#;
    assert_eq!(run(source), b"13");
}

// A fold's step learns nothing of its scrutinee, an expression as a variable: `xp < count(x)` holds at every tail and by no equation, since `count(x)` is `xp + 1` at the outermost step alone. The bound rides in the motive, which restates it at each tail, and the step is erased under no refinement, as it was elaborated: the proofs it passes are erased by their sort, and `h` is called at the function type the motive states.
#[test]
fn a_nat_fold_over_an_expression_carries_its_bound_in_its_motive() {
    let source = r#"
        use /std/{Nat};
        use /std/Bool/{True, Holds};
        let count(n: Nat) -> Nat = match n | 0 => 0 | k + 1 => count(k) + 1 end;
        let f(x: Nat, ih: (y: Nat, r: Holds(y < count(x))) -> (Nat) -> Nat, m: Nat) -> Nat =
            match count(x): (c) => (below: Holds(c <= count(x))) -> Nat
            | 0 => (_) => m
            | xp + 1; h => (below) => h(True/proved()) + ih(xp, True/proved())(m)
            end(True/qed());
        /std/print(Nat/to_str(f(3, (y, _) => (m) => y + m, 10)))
        "#;
    assert_eq!(run(source), b"43");
}

// Both at once: a case split over an expression, whose predecessor is computed from the erased operand rather than re-derived from the expression. The equation is recorded under `count(x)` as written, which is how the callee's type spells it.
#[test]
fn a_nat_case_split_over_an_expression_is_erased_under_its_successor() {
    let source = r#"
        use /std/{Nat};
        let Step(n: Nat) -> Type =
            match n: (_) => Type | 0 => Nat | k + 1 => (Nat) -> Nat end;
        let count(n: Nat) -> Nat = match n | 0 => 0 | k + 1 => count(k) + 1 end;
        let f(x: Nat, g: Step(count(x)), m: Nat) -> Nat =
            match count(x) | 0 => m | xp + 1 => g(m) + xp end;
        /std/print(Nat/to_str(f(5, (k) => k + 5, 4)))
        "#;
    assert_eq!(run(source), b"13");
}

// A sequence case split binds its element and suffix as values the peel fills, and the arm is erased with the scrutinee standing for their cons: `Shape(l)` is a function type only there.
#[test]
fn a_list_case_split_is_erased_under_its_cons() {
    let source = r#"
        use /std/{Nat, List};
        let Shape(l: List(Nat)) -> Type =
            match l: (_) => Type | [] => Nat | [_, ..t] => (Nat) -> Nat end;
        let f(l: List(Nat), g: Shape(l)) -> Nat =
            match l | [] => 0 | [_, ..t] => g(List/len(t)) end;
        /std/print(Nat/to_str(f([1, 2, 3], (k) => k + 1)))
        "#;
    assert_eq!(run(source), b"3");
}

// The packed carrier takes the same path as `List`, through its own cons value.
#[test]
fn a_bytes_case_split_is_erased_under_its_cons() {
    let source = r#"
        use /std/{Nat, Bytes};
        let Shape(b: Bytes) -> Type =
            match b: (_) => Type | x[] => Nat | x[_, ..t] => (Nat) -> Nat end;
        let f(b: Bytes, g: Shape(b)) -> Nat =
            match b | x[] => 0 | x[_, ..t] => g(Bytes/len(t)) end;
        /std/print(Nat/to_str(f(x[1, 2, 3], (k) => k + 1)))
        "#;
    assert_eq!(run(source), b"3");
}

// A sequence fold's step over an expression scrutinee, erased under no refinement as the `Nat` fold's is: the tail's length is below the scrutinee's by the bound the motive carries.
#[test]
fn a_list_fold_over_an_expression_carries_its_bound_in_its_motive() {
    let source = r#"
        use /std/{Nat, List};
        use /std/Bool/{True, Holds};
        let f(l: List(Nat), g: (t: List(Nat), r: Holds(List/len(t) < List/len(List/reverse(l)))) -> (Nat) -> Nat) -> Nat =
            match List/reverse(l): (s) => (below: Holds(List/len(s) <= List/len(List/reverse(l)))) -> Nat
            | [] => (_) => 0
            | [_, ..t]; h => (below) => h(True/proved()) + g(t, True/proved())(1)
            end(True/qed());
        /std/print(Nat/to_str(f([1, 2, 3], (t, _) => (m) => List/len(t) + m)))
        "#;
    assert_eq!(run(source), b"6");
}
