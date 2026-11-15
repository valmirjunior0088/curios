//! An arm is erased under what it was elaborated under, however it is lowered: a case split under its scrutinee's refinement, an expression scrutinee as well as a variable, and a fold's step under none, since reduction takes it at every tail. Erasure reads the type of each head it eliminates off the finished term and judges nothing, so a proof an arm passes is erased by its sort whatever is in scope, and what the refinement decides is a type's head: a case split's arm calls what has a function type only at the scrutinee's case, and is read with the scrutinee standing for that case.
//!
//! Mutation-checked: with the refinement taken out of the successor arm and the cons arm of erasure's case splits (`curios-erase/src/lower/eliminate.rs`), each case split below panics at `erase: applied a non-function`, and the two folds, whose steps refine nothing, are unmoved.

use crate::tests::test_support::*;

// The successor arm ignores its hypothesis, so it erases as a case split rather than a fold. `g` is typed at `Step(x)`, a function type at a successor and at nothing else, so the call is read with `x` standing for `xp + 1`: with nothing refined, erasure meets a stuck `match` where it needs the callee's telescope.
#[test]
fn a_nat_case_split_is_erased_under_its_successor() {
    let source = r#"
        use /std/{Nat};
        let Step(n: Nat) -> Type =
            match n | 0 => Nat | k + 1 => (Nat) -> Nat end;
        let f(x: Nat, g: Step(x), m: Nat) -> Nat =
            match x | 0 => m | xp + 1 => g(m) + xp end;
        /std/print(Nat/to_str(f(5, (k) => k + 5, 4)))
        "#;
    assert_eq!(run(source), b"13");
}

// Both at once: a case split over an expression, whose predecessor is computed from the erased operand rather than re-derived from the expression. The equation is recorded under `count(x)` as written, which is how the callee's type spells it.
#[test]
fn a_nat_case_split_over_an_expression_is_erased_under_its_successor() {
    let source = r#"
        use /std/{Nat};
        let Step(n: Nat) -> Type =
            match n | 0 => Nat | k + 1 => (Nat) -> Nat end;
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
            match l | [] => Nat | [_, ..t] => (Nat) -> Nat end;
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
            match b | x[] => Nat | x[_, ..t] => (Nat) -> Nat end;
        let f(b: Bytes, g: Shape(b)) -> Nat =
            match b | x[] => 0 | x[_, ..t] => g(Bytes/len(t)) end;
        /std/print(Nat/to_str(f(x[1, 2, 3], (k) => k + 1)))
        "#;
    assert_eq!(run(source), b"3");
}
