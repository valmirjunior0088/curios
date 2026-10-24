//! A guard and the statement its fact proves, apart in how an argument is spelled: an implicit written on one side and left out on the other, at a structure's element and through a structure's proof field.

use {crate::tests::typecheck, curios_utilities::fill};

/// A recursive predicate over a list of any element type: every written call takes a universe level of its own, so a guard and a statement over it are two calls apart in their instance, which their arguments decide.
const EVERY: &str = "let every(@T: Type, l: List(T), pred: (T) -> Bool) -> Bool =
    match l
    | [] => true
    | [h, ..t] => match pred(h) | true => every(t, pred) | false => false end
    end;";

/// The implicit written where the statement infers it and the converse, at a structure: the written one is the structure's name and the inferred one the node a list's element type reduces to, and the guard proves the statement either way.
#[test]
fn a_guard_at_a_structure_proves_its_fact_whichever_side_writes_the_implicit() {
    let source = fill!(
        r#"use /std/{Nat, List, Bool};
use /std/Bool/{True, Holds};

pub struct Point: Type { x: Nat }

${every}

let small(p: Point) -> Bool = p.x < 10;

let inferred(l: List(Point), @ok: Holds(every(l, small))) -> Nat = 0;
let written(l: List(Point), @ok: Holds(every(@Point, l, small))) -> Nat = 0;

pub let f(l: List(Point)) -> Nat = match every(@Point, l, small) | true => inferred(l, @True/qed()) | false => 1 end;
pub let g(l: List(Point)) -> Nat = match every(l, small) | true => written(l, @True/qed()) | false => 1 end;

/std/print("held")"#,
        every = EVERY,
    );

    assert_eq!(typecheck(&source), Ok(()));
}

/// The shape of `/std/Tui/Glyph/of_cluster`: a structure's hidden proof field states a call with its implicit inferred, and the guard that fills it writes the implicit. Both checkers hold the field filled.
#[test]
fn a_proof_field_is_filled_from_a_guard_that_writes_the_implicit_the_field_infers() {
    let source = fill!(
        r#"use /std/{Nat, List, Bool, Option};
use /std/Bool/{Holds};

${every}

let small(n: Nat) -> Bool = n < 10;

pub struct G: Type {
    chars: List(Nat),
    @ok: Holds(every(chars, small)),
}

pub let of(chars: List(Nat)) -> Option(G) =
    match every(@Nat, chars, small)
    | false => Option/none()
    | true => Option/some(G { chars = chars })
    end;

/std/print("held")"#,
        every = EVERY,
    );

    assert_eq!(typecheck(&source), Ok(()));
}

/// The control: a guard over another list is no fact about this one, its implicit written or not.
#[test]
fn a_guard_over_another_list_proves_nothing_of_this_one() {
    let source = fill!(
        r#"use /std/{Nat, List, Bool};
use /std/Bool/{True, Holds};

pub struct Point: Type { x: Nat }

${every}

let small(p: Point) -> Bool = p.x < 10;

let inferred(l: List(Point), @ok: Holds(every(l, small))) -> Nat = 0;

pub let f(l: List(Point), m: List(Point)) -> Nat = match every(@Point, m, small) | true => inferred(l, @True/qed()) | false => 1 end;

/std/print("held")"#,
        every = EVERY,
    );

    let refusal = typecheck(&source).expect_err("a guard over another list proves nothing");
    assert!(refusal.contains("type mismatch"), "{refusal}");
}
