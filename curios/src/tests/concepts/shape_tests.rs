//! Keying a witness on an anonymous type — a tuple's shape: what registers, what resolves, and the surprise the key's identity holds. A function type is not keyable, and the refusal that says so closes the file.

use crate::tests::{error, run};

// The base case: a tuple type has no name to be headed by, so its shape — the label at each position — is the head. `Tag/tag(z)` reduces the parameter to `{Nat, Bool}`, keys it as `{_, _}`, and finds the entry; the field types were never in the key and are checked by unification after the lookup.
#[test]
fn a_concept_resolves_on_a_tuple_value() {
    let source = r#"
        use /std/{Nat, Bool, Str};
        pub concept Tag(A: Type): pub Type {
            tag(A) -> Str,
        }
        satisfy Tag({Nat, Bool}) {
            tag(t) = "pair",
        }
        let z: {Nat, Bool} = (1, true);
        /std/print(Tag/tag(z))
        "#;

    assert_eq!(run(source), b"pair");
}

// Labels are part of a tuple type's identity, so a positional witness does not cover a labeled goal of the same arity. That is the one surprise this key has, so the miss carries the rule rather than leaving the reader to infer it.
#[test]
fn a_labeled_goal_does_not_reach_the_positional_witness() {
    let source = r#"
        use /std/{Nat, Bool, Str};
        pub concept Tag(A: Type): pub Type {
            tag(A) -> Str,
        }
        satisfy Tag({Nat, Bool}) {
            tag(t) = "pair",
        }
        let z: {x: Nat, y: Bool} = (x = 1, y = true);
        /std/print(Tag/tag(z))
        "#;

    assert!(error(source).contains(
        "no witness of Tag({x: Nat, y: Bool}) found\n  \
         labels are part of the type: the witness for {_, _} does not cover {x: _, y: _}\n  \
         name a struct for the labeled product, or declare the witness for this shape"
    ));
}

// A keyed goal is asked of every witness the unit declares, so a witness declared later in the module serves an earlier use — the standing a nominal goal has: the shape is read off the witness's signature as it is written, before it elaborates.
#[test]
fn a_later_declared_tuple_witness_serves_an_earlier_use() {
    let source = r#"
        use /std/{Nat, Bool, Str};
        pub concept Tag(A: Type): pub Type {
            tag(A) -> Str,
        }
        let z: {Nat, Bool} = (1, true);
        let named: Str = Tag/tag(z);
        satisfy Tag({Nat, Bool}) {
            tag(t) = "pair",
        }
        /std/print(named)
        "#;

    assert_eq!(run(source), b"pair");
}

#[test]
fn the_unit_type_is_a_key() {
    let source = r#"
        use /std/{Str};
        pub concept Tag(A: Type): pub Type {
            tag(A) -> Str,
        }
        satisfy Tag({}) {
            tag(t) = "unit",
        }
        /std/print(Tag/tag(()))
        "#;

    assert_eq!(run(source), b"unit");
}

// The higher-kinded position keys on the constructor's body, so a constructor whose body is a tuple type keys on that body's shape — where a nominal one keys on its name. Symmetry with `Monad(Option)`, and the reason the refusal needs no sentence carving the case out.
#[test]
fn a_constructor_whose_body_is_a_tuple_type_is_keyed() {
    let source = r#"
        use /std/{Nat, Str};
        let Pair(A: Type) -> Type = {Nat, A};
        pub concept Fun(M: (Type) -> Type): pub Type {
            name() -> Str,
        }
        satisfy Fun(Pair) {
            name() = "Pair",
        }
        /std/print(Fun/name(@Pair))
        "#;

    assert_eq!(run(source), b"Pair");
}

// A head that is none of the keyable kinds is refused: a witness over a bare variable has nothing rigid to key on.
#[test]
fn a_variable_head_is_still_not_a_key() {
    let source = r#"
        use /std/{Str};
        pub concept Tag(A: Type): pub Type {
            tag(A) -> Str,
        }
        satisfy (@A: Type) => Tag(A) {
            tag(t) = "any",
        }
        /std/print("x")
        "#;

    assert!(error(source).contains(
        "every parameter's head must be an inductive, a struct, an intrinsic type, or a tuple type"
    ));
}

/// A function type is not a key. Its useful key space is nearly one point — `(_) -> _` above all — so a concept's owner claiming a shape would claim it program-wide and forever, and the standard library has no consumer for it. Tuple shapes are unaffected: their space is large and ownership partitions naturally.
#[test]
fn a_witness_keyed_on_a_function_type_cannot_be_keyed() {
    let error = error(
        r#"
        use /std/{Nat, Str};
        concept Tag(A: Type): Type {
            name() -> Str,
        }
        satisfy Tag((Nat) -> Nat) {
            name() = "a function",
        }
        /std/print("hi\n")
        "#,
    );
    assert!(
        error.contains("this witness cannot be keyed") && !error.contains("or a function type"),
        "unexpected error: {error}"
    );
}
