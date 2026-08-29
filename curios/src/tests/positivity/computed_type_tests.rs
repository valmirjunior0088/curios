//! A field or payload whose type a type-level `rec` computes.
//!
//! The walk forces a `rec` head because a mutual `induct` group lowers its type constructors into one. Forcing a `rec` whose member is a *function* exposes the group one level deeper instead, and `RecGroup::member_body` substitutes the group into its own body, so every turn hands the walk the same node against a fresh binder. These are the shapes that closes over, with two green rows saying what the guard may not cost.

use crate::tests::run;

// A field whose type a self-calling type-level `rec` computes. The walk forces the `rec` head, cannot see through the stuck `match` it exposes, descends into the arm holding the recursive call, opens that arm against a fresh binder — and forces the same node again, one level deeper, forever. `RecGroup::member_body` substitutes `Term::rec_proj` for every recursive occurrence, so each unfolding hands the walk a structurally identical group; nothing but a guard keyed on that group stops the descent, and `unfolded` cannot be it because an inline group has no name to key on.
#[test]
fn a_field_type_a_self_calling_rec_computes_is_admitted() {
    let source = r#"
        use /std/{Nat, Str};

        induct Labels : pub Type
        | nil()
        | cons(Str, Labels)
        end

        let Count(L : Labels) -> Type =
            match L : (_) => Type
            | nil() => {}
            | cons(l, rest) => {Nat, Count(rest)}
            end;

        struct Row(L : Labels) : pub Type { value : Count(L) }

        let one : Row(Labels/cons("a", Labels/nil())) =
            Row(Labels/cons("a", Labels/nil())) { value = (1, ()) };

        /std/print("computed")
        "#;
    assert_eq!(run(source), b"computed");
}

// The recursive call under another type former rather than in a bare tuple field. Polarity composition walks into `Option`'s parameter, so the forced node is reached by a different route and must be guarded on the same key.
#[test]
fn a_computed_field_type_under_a_parameterized_family_is_admitted() {
    let source = r#"
        use /std/{Str, Option};

        induct Labels : pub Type
        | nil()
        | cons(Str, Labels)
        end

        let Count(L : Labels) -> Type =
            match L : (_) => Type
            | nil() => {}
            | cons(l, rest) => Option(Count(rest))
            end;

        struct Row(L : Labels) : pub Type { value : Count(L) }

        let one : Row(Labels/nil()) = Row(Labels/nil()) { value = () };

        /std/print("optioned")
        "#;
    assert_eq!(run(source), b"optioned");
}

// The arm is the recursive call and nothing else — no former to descend through, no product to open. The shortest program that closes the cycle, and the one that shows the guard is about the `rec` head rather than about what encloses it.
#[test]
fn a_computed_field_type_that_is_a_bare_recursive_call_is_admitted() {
    let source = r#"
        use /std/{Str};

        induct Labels : pub Type
        | nil()
        | cons(Str, Labels)
        end

        let Count(L : Labels) -> Type =
            match L : (_) => Type
            | nil() => {}
            | cons(l, rest) => Count(rest)
            end;

        struct Row(L : Labels) : pub Type { value : Count(L) }

        let one : Row(Labels/nil()) = Row(Labels/nil()) { value = () };

        /std/print("bare")
        "#;
    assert_eq!(run(source), b"bare");
}

// The same shape over an intrinsic carrier: the description the type is computed from is a numeral rather than a declared family, so nothing about the loop depends on the index being inductive.
#[test]
fn a_field_type_computed_by_recursion_over_a_numeral_is_admitted() {
    let source = r#"
        use /std/{Nat, Bool};

        let Rep(n : Nat) -> Type =
            match n : (_) => Type
            | 0 => {}
            | p + 1 => {Bool, Rep(p)}
            end;

        struct Vector(n : Nat) : pub Type { value : Rep(n) }

        let one : Vector(1) = Vector(1) { value = (true, ()) };

        /std/print("counted")
        "#;
    assert_eq!(run(source), b"counted");
}

// A constructor payload rather than a struct field. Both declaration forms are split into parts and walked by the same traversal, so a guard that fixed only one of them would be fixing the caller instead of the walk.
#[test]
fn a_constructor_payload_a_self_calling_rec_computes_is_admitted() {
    let source = r#"
        use /std/{Nat, Str};

        induct Labels : pub Type
        | nil()
        | cons(Str, Labels)
        end

        let Count(L : Labels) -> Type =
            match L : (_) => Type
            | nil() => {}
            | cons(l, rest) => {Nat, Count(rest)}
            end;

        induct Boxed(L : Labels) : pub Type
        | mk(Count(L))
        end

        let boxed : Boxed(Labels/nil()) = Boxed/mk(@Labels/nil(), ());

        match boxed
        | mk(_) => /std/print("boxed")
        end
        "#;
    assert_eq!(run(source), b"boxed");
}

// A user's own refinement over a packed carrier — the shape `/std/BigNat` ships, with nothing of `BigNat` in it: a struct field whose proposition applies a self-calling fold to an earlier field.
//
// The cost is the walk's, not the declaration checker's: put a type error in a later item and the diagnostic arrives, because the module fold stops before the whole-module passes run. Nothing here turns on how much native stack a thread has, and the tree sets none anywhere. What keeps the standard library clear of this shape is the contrast in the test below.
#[test]
fn a_refinement_field_over_a_self_calling_fold_is_admitted() {
    let source = r#"
        use /std/{Bytes, Bool};
        use /syn/{True, False};

        let always(b : Bytes) -> Bool =
            match b
            | x[] => true
            | x[h, ..t] => always(t)
            end;

        let Certified(b : Bytes) -> Prop =
            match always(b) | true => True | false => False end;

        struct Wrapped : Type {
            bytes : Bytes,
            ok : Certified(bytes),
        }

        let w : Wrapped = Wrapped { bytes = x[0x61], ok = True/qed() };

        /std/print("certified")
        "#;
    assert_eq!(run(source), b"certified");
}

// The green row that explains why the standard library never tripped any of the above. `/std/BigNat/is_trimmed` is this fold, and the `; ih` binding is why: a fold hypothesis is a *binder*, not a self-application, so forcing the member exposes no recursive call to force again. Swap this one arm for the explicit `always(t)` of the test above and the identical program diverges.
#[test]
fn a_refinement_field_over_a_fold_hypothesis_is_admitted() {
    let source = r#"
        use /std/{Bytes, Bool};
        use /syn/{True, False};

        let always(b : Bytes) -> Bool =
            match b
            | x[] => true
            | x[h, ..t]; ih => ih
            end;

        let Certified(b : Bytes) -> Prop =
            match always(b) | true => True | false => False end;

        struct Wrapped : Type {
            bytes : Bytes,
            ok : Certified(bytes),
        }

        let w : Wrapped = Wrapped { bytes = x[0x61], ok = True/qed() };

        /std/print("folded")
        "#;
    assert_eq!(run(source), b"folded");
}

// The other green row, and the reason the walk forces a head at all: a field type bound by a plain type-level `let` must still be unfolded to the type former it names. A guard that declined to force would take this with it.
#[test]
fn a_field_type_bound_by_a_plain_alias_is_admitted() {
    let source = r#"
        use /std/{Nat, Bool};

        let Pairing : Type = {Nat, Bool};

        struct Row : pub Type { value : Pairing }

        let one : Row = Row { value = (1, true) };

        /std/print("aliased")
        "#;
    assert_eq!(run(source), b"aliased");
}
