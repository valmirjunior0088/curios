//! Resolving a goal through a superclass edge of a `use` binder in scope, and an edge's scope over the fields of its own concept.

use crate::tests::run;

// A superclass edge resolved by projection: inside `same`, the goal `Eql(A)` has a bound-variable head (no table entry), so it is solved by projecting the local `use Ordered(A)` binder's (anonymous) superclass field, keyed by index. The `use Ordered(A)` slot itself resolves through the table to `ord_nat`, whose own omitted superclass field resolves to `eql_nat` — no field names a witness anywhere.
#[test]
fn superclass_projection_resolves() {
    let source = r#"
        use /std/{Nat, Bool, Ordering};
        pub concept Equal(A : Type) : pub Type {
            eql(A, A) -> Bool
        }
        pub concept Ordered(A : Type) : pub Type {
            use Equal(A),
            cmp(A, A) -> Ordering
        }
        satisfy Equal(Nat) {
            eql(a, b) = a == b
        }
        satisfy Ordered(Nat) {
            cmp(a, b) = Ordering/lt()
        }
        pub let same(@A : Type, use Ordered(A), x : A, y : A) -> Bool = Equal/eql(x, y);
        let n : Nat = 3;
        /std/print(Bool/to_str(same(n, n)))
        "#;

    assert_eq!(run(source), b"true");
}

// The prelude `Ord` concept resolves, and its `Eql` superclass is reachable by projection from an `Ord` in scope.
#[test]
fn prelude_ord_superclass_projects() {
    let source = r#"
        use /std/{Nat, Bool, Ord};
        use /std/ops/{Eql};
        pub let equal(@A : Type, use Ord(A), x : A, y : A) -> Bool = Eql/eql(x, y);
        let n : Nat = 4;
        /std/print(Bool/to_str(equal(n, n)))
        "#;

    assert_eq!(run(source), b"true");
}

// A higher-kinded superclass: inside the generic function the goal `Monad(M)` (M a bound variable) resolves through step 2's superclass projection of the local `use MonadPlus(M)` binder. The witness's own omitted `monad` field resolves through the table to the std `Monad(Option)` witness — a higher-kinded auto-fill.
#[test]
fn higher_kinded_superclass_projects() {
    let source = r#"
        use /std/{Nat, Str, Option, Monad};
        pub concept MonadPlus(M : (Type) -> Type) : Type {
            use Monad(M),
            empty(@A : Type) -> M(A)
        }
        satisfy MonadPlus(Option) {
            empty(@A) = Option/none()
        }
        pub let wrap(@M : (Type) -> Type, use MonadPlus(M), m : M(Nat)) -> M(Nat) =
            Monad/bind(m, (x) => Monad/pure(x));
        let o : Option(Nat) = wrap(Option/some(11));
        /std/print(Nat/to_str(Option/unwrap_or(o, 0)))
        "#;

    assert_eq!(run(source), b"11");
}

// A function telescope puts a `use` premise into the witness scope of every *later domain*, so a later parameter's type may name the premise's methods (`binding`'s `assume_slot`). A concept's field telescope does the same: the edge declared one field above is in scope while `injective`'s type elaborates, so `Encoded/encode`'s own `use` premise resolves to it rather than going unfound. The two are one construct in `documentation/syntax.md`, and this is where they agree — the witness below then writes no encoding at all, since there is only the edge's.
#[test]
fn a_superclass_edge_is_in_scope_for_a_later_field_type() {
    let source = r#"
        use /std/{Bytes, Eq, Str};
        pub concept Encoded(A : Type) : pub Type {
            encode(A) -> Bytes
        }
        pub concept Keyed(A : Type) : pub Type {
            use Encoded(A),
            injective(a : A, b : A, same : Eq(Encoded/encode(a), Encoded/encode(b))) -> Eq(a, b)
        }
        satisfy Encoded(Str) {
            encode = Str/to_bytes
        }
        satisfy Keyed(Str) {
            injective(a, b, same) = Str/eq_of_bytes(a, b, same)
        }
        /std/print("resolved")
        "#;

    assert_eq!(run(source), b"resolved");
}
