//! Keying a witness on an anonymous type — a tuple's shape or a function type's plicity vector: what registers, what resolves, and the surprise each key's identity holds.

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

// A keyed goal with no entry defers to the end-of-module sweep instead of failing at the call, so a witness declared later in the module serves an earlier use — the standing a nominal goal has, which a tuple goal did not have while it was unkeyable.
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
        /std/print(Fun/name(@Pair)())
        "#;

    assert_eq!(run(source), b"Pair");
}

// The base case, function side: a function type has no name to be headed by either, so its plicity vector is the head. `Tag/tag` reduces the parameter to `(Nat) -> Nat`, keys it as `(_) -> _`, and finds the entry; the domains and result were never in the key and are checked by unification after the lookup.
#[test]
fn a_concept_resolves_on_a_function_value() {
    let source = r#"
        use /std/{Nat, Str};
        pub concept Tag(A: Type): pub Type {
            tag(A) -> Str,
        }
        satisfy Tag((Nat) -> Nat) {
            tag(f) = "func",
        }
        /std/print(Tag/tag((n) => n + 1))
        "#;

    assert_eq!(run(source), b"func");
}

// A keyed function goal has the standing a nominal goal has: no entry defers to the end-of-module sweep instead of failing at the call.
#[test]
fn a_later_declared_function_witness_serves_an_earlier_use() {
    let source = r#"
        use /std/{Nat, Str};
        pub concept Tag(A: Type): pub Type {
            tag(A) -> Str,
        }
        let f: (Nat) -> Nat = (n) => n + 1;
        let named: Str = Tag/tag(f);
        satisfy Tag((Nat) -> Nat) {
            tag(f) = "func",
        }
        /std/print(named)
        "#;

    assert_eq!(run(source), b"func");
}

// Curios does not curry at the type level, so `() -> A` is a distinct type from `A` and the empty vector is its own table entry.
#[test]
fn a_nullary_function_type_is_its_own_key() {
    let source = r#"
        use /std/{Nat, Str};
        pub concept Tag(A: Type): pub Type {
            tag(A) -> Str,
        }
        satisfy Tag(() -> Nat) {
            tag(f) = "thunk",
        }
        let f() -> Nat = 1;
        /std/print(Tag/tag(f))
        "#;

    assert_eq!(run(source), b"thunk");
}

// Plicity is part of a function type's identity — `(Nat) -> Nat` and `(@n: Nat) -> Nat` do not convert — so two vectors of one arity are two table entries, each reached by the goals of its own type.
#[test]
fn plicity_distinct_shapes_are_distinct_entries() {
    let source = r#"
        use /std/{Nat, Str};
        pub concept Tag(A: Type): pub Type {
            tag(A) -> Str,
        }
        satisfy Tag((Nat) -> Nat) {
            tag(f) = "explicit",
        }
        satisfy Tag((@n: Nat) -> Nat) {
            tag(f) = "implicit",
        }
        let g: (@n: Nat) -> Nat = (@n) => n;
        /std/print(Str/concat(Tag/tag((n) => n), Tag/tag(g)))
        "#;

    assert_eq!(run(source), b"explicitimplicit");
}

// Plicity is part of a function type's identity, so an all-explicit witness does not cover a goal with hidden slots. That is the surprise this key has, so the miss carries the rule — the plicity twin of the labeled-tuple hint above.
#[test]
fn a_marked_goal_does_not_reach_the_explicit_witness() {
    let source = r#"
        use /std/{Nat, Str};
        pub concept Tag(A: Type): pub Type {
            tag(A) -> Str,
        }
        satisfy Tag((Nat) -> Nat) {
            tag(f) = "func",
        }
        let g: (@n: Nat) -> Nat = (@n) => n;
        /std/print(Tag/tag(g))
        "#;

    assert!(error(source).contains(
        "no witness of Tag((@n: Nat) -> Nat) found\n  \
         plicity marks are part of the type: the witness for (_) -> _ does not cover (@_) -> _\n  \
         declare the witness for this shape"
    ));
}

// The higher-kinded position keys on the constructor's body, so a constructor whose body is a function type keys on that body's plicity vector — beside the tuple-bodied constructor above and `Monad(Option)`.
#[test]
fn a_constructor_whose_body_is_a_function_type_is_keyed() {
    let source = r#"
        use /std/{Nat, Str};
        let Reader(A: Type) -> Type = (Nat) -> A;
        pub concept Fun(M: (Type) -> Type): pub Type {
            name() -> Str,
        }
        satisfy Fun(Reader) {
            name() = "Reader",
        }
        /std/print(Fun/name(@Reader)())
        "#;

    assert_eq!(run(source), b"Reader");
}

// Every other rigid head still keys as it did, and a head that is none of them is still refused — with the roster the refusal names now listing function types. A witness over a bare variable is the shape that stays out: nothing rigid remains to key on.
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
        "every parameter's head must be an inductive, a struct, an intrinsic type, a tuple type, or a function type"
    ));
}
