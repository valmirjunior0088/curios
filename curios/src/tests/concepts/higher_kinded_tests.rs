//! Resolving a witness keyed on a type former: by imitation, through a written former or a type-level lambda, and through a partially applied family.

use crate::tests::run;

// The full higher-kinded chain: `Monad/bind(o, f)` parks its `Monad(?M)` goal on the flex parameter, checking `o : Option(Nat)` against `?M(?A)` fires the flex-apply imitation rule inside the conversion checker, and the committed `?M := Option` wakes the parked goal, which the table resolves to the prelude's `Monad(Option)` witness. Also covers the cached-prelude replay of a higher-kinded witness.
#[test]
fn prelude_monad_resolves_by_imitation() {
    let source = r#"
        use /std/{Nat, Str, Option, Monad};
        let o : Option(Nat) = Monad/bind(Option/some(20), (x) => Monad/pure(Nat/add(x, 1)));
        /std/print(Nat/to_str(Option/unwrap_or(o, 0)))
        "#;

    assert_eq!(run(source), b"21");
}

// A *written* higher-kinded instantiation: `@Option` fills `M` at the call, so the `use Monad(M)` goal is minted from the telescope opened at the written argument. That argument must enter the telescope rebuilt (an `Instance` at its fresh levels): substituted raw, the bare polymorphic `Option` reference is inert under the reducer's monomorphic-variable gate, resolution's global-table step finds no rigid head to key on, and the registered witness is missed while the inferred path (`lift(7)` against an expected `Option(Nat)`) resolves fine.
#[test]
fn written_higher_kinded_argument_resolves_the_witness() {
    let source = r#"
        use /std/{Monad};
        use /std/{Nat, Str, Option};
        pub let lift(@M : (Type) -> Type, use Monad(M), seed : Nat) -> M(Nat) =
            Monad/pure(seed);
        let o : Option(Nat) = lift(@Option, 7);
        /std/print(Nat/to_str(Option/unwrap_or(o, 0)))
        "#;

    assert_eq!(run(source), b"7");
}

// A written hidden argument *behind an explicit slot*: the materialization walk substitutes the explicit `7` raw, so the trailing `use` binder's goal cannot be typed there — it is minted in the checking walk, where its domain is opened through the elaborated `@Option` rather than the raw spelling.
#[test]
fn written_hidden_argument_after_an_explicit_slot_resolves() {
    let source = r#"
        use /std/{Monad};
        use /std/{Nat, Str, Option};
        pub let lift2(seed : Nat, @M : (Type) -> Type, use Monad(M)) -> M(Nat) =
            Monad/pure(seed);
        let o : Option(Nat) = lift2(7, @Option);
        /std/print(Nat/to_str(Option/unwrap_or(o, 0)))
        "#;

    assert_eq!(run(source), b"7");
}

// A written type-level *lambda* as the carrier: an intro form keeps its postponement path, so the witness goal must be minted only after the lambda's own elaboration turns its body into the nominal normal form the head key can read (`Box`) — the raw application spelling inside the unelaborated lambda keys on nothing.
#[test]
fn written_type_lambda_argument_resolves_the_witness() {
    let source = r#"
        use /std/{Monad};
        use /std/{Nat, Str, Result};
        struct Box(A : Type) : pub Type {
            A
        }
        satisfy Monad(Box) {
            pure(x) = Box { x },
            bind(m, f) = f(m.0)
        }
        pub let lift(@M : (Type) -> Type, use Monad(M), seed : Nat) -> M(Nat) =
            Monad/pure(seed);
        let b : Box(Nat) = lift(@((A : Type) => Box(A)), 3);
        /std/print(Nat/to_str(b.0))
        "#;

    assert_eq!(run(source), b"3");
}

// A bare reference to an all-hidden generic function, checked against a rigid concrete carrier: the check turnaround inserts the implicit carrier and the witness goal, imitation pins `M := Option` from the expectation, and the goal resolves through the table.
#[test]
fn bare_generic_reference_resolves_toward_a_rigid_expectation() {
    let source = r#"
        use /std/{Monad};
        use /std/{Nat, Str, Option};
        pub let mk(@M : (Type) -> Type, use Monad(M)) -> M(Nat) =
            Monad/pure(5);
        let z : Option(Nat) = mk;
        /std/print(Nat/to_str(Option/unwrap_or(z, 0)))
        "#;

    assert_eq!(run(source), b"5");
}

// `Intrinsic`-headed type constructors (`List`, `Cell`) carry their argument inside the `Intrinsic` node; the imitation rule rebuilds the node over the binder (`?M := λT. List(T)`), so `Monad/bind` over a `List` pins the witness from the action's type like any nominal constructor would.
#[test]
fn monad_over_intrinsic_constructor_resolves_by_imitation() {
    let source = r#"
        use /std/{Nat, List, Str, Monad};
        let a : List(Nat) = [1];
        let b : List(Nat) = Monad/bind(a, (x) => a);
        /std/print(Nat/to_str(List/len(b)))
        "#;

    assert_eq!(run(source), b"1");
}

// A witness over a *partially applied* family: `(A : Type) => Box(S, A)` leaves a stuck application under the binder, and the key reads that application's head — so a two-parameter monad can register its parametric witness. Resolution then finds it when a goal's parameter is pinned to the same partial lambda, unifying the arguments below the head.
#[test]
fn a_witness_keys_through_a_partially_applied_family() {
    let source = r#"
        use /std/{Nat, Str, Monad};
        induct Box(S : Type, A : Type) : Type
        | wrap(A)
        end
        satisfy (@S : Type) => Monad((A : Type) => Box(S, A)) {
            pure(@A, a) = Box/wrap(a),
            bind(@A, @B, m, f) =
                match m : (_) => Box(S, B)
                | wrap(a) => f(a)
                end,
        }
        let unwrap(@S : Type, @A : Type, m : Box(S, A)) -> A =
            match m : (_) => A
            | wrap(a) => a
            end;
        let doubled(@M : (Type) -> Type, use Monad(M), m : M(Nat)) -> M(Nat) =
            Monad/bind(m, (n) => Monad/pure(Nat/add(n, n)));
        let boxed : Box(Str, Nat) = Box/wrap(21);
        /std/print(Nat/to_str(unwrap(doubled(@(A : Type) => Box(Str, A), boxed))))
        "#;

    assert_eq!(run(source), b"42");
}
