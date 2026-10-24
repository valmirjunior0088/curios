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

/// A guard written at each kind of type former proves the statement that infers it: a family, a recursive family, an indexed family at its indices and an intrinsic former closed or over an operand, beside the structure above. The written former and the inferred one are one node.
#[test]
fn a_guard_at_each_kind_of_former_proves_the_statement_that_infers_it() {
    for element in [
        "Option(Nat)",
        "Tree(Nat)",
        "Vec(Nat)(2)",
        "Nat",
        "List(Nat)",
        "Io({})",
    ] {
        let source = fill!(
            r#"use /std/{Nat, List, Bool, Option, Io};
use /std/Bool/{True, Holds};

pub induct Tree(T: Type): Type
| leaf()
| node(Tree(T), T, Tree(T))
end

pub induct Vec(T: Type): (Nat) -> pub Type
| nil(): (0)
| cons(@n: Nat, head: T, tail: Vec(T)(n)): (n + 1)
end

${every}

let always(@T: Type, t: T) -> Bool = true;

let inferred(l: List(${element}), @ok: Holds(every(l, (x) => always(x)))) -> Nat = 0;

pub let f(l: List(${element})) -> Nat = match every(@${element}, l, (x) => always(x)) | true => inferred(l, @True/qed()) | false => 1 end;

/std/print("held")"#,
            every = EVERY,
            element,
        );

        assert_eq!(typecheck(&source), Ok(()), "{element}");
    }
}

/// A former passed unapplied to a higher-kinded parameter is applied by substitution, and the application is the node a written one is: a guard over `lift(@Option, o)` proves a statement that writes the element `Option(Nat)`, and the converse.
#[test]
fn a_former_applied_by_substitution_is_the_node_a_written_one_is() {
    for (guard, statement) in [
        (
            "every(lift(@Option, o), l)",
            "every(@Option(Nat), lift(@Option, o), l)",
        ),
        (
            "every(@Option(Nat), lift(@Option, o), l)",
            "every(lift(@Option, o), l)",
        ),
    ] {
        let source = fill!(
            r#"use /std/{Nat, List, Bool, Option};
use /std/Bool/{True, Holds};

pub struct Box(A: Type): Type { a: A }

let every(@T: Type, b: Box(T), l: List(Nat)) -> Bool =
    match l
    | [] => true
    | [_, ..t] => every(b, t)
    end;

let lift(@F: (Type) -> Type, x: F(Nat)) -> Box(F(Nat)) = Box { a = x };

let checked(o: Option(Nat), l: List(Nat), @ok: Holds(${statement})) -> Nat = 0;

pub let f(o: Option(Nat), l: List(Nat)) -> Nat =
    match ${guard} | true => checked(o, l, @True/qed()) | false => 1 end;

/std/print("held")"#,
            guard,
            statement,
        );

        assert_eq!(typecheck(&source), Ok(()), "{guard} against {statement}");
    }
}
