//! `!` sequencing through a user monad witness, including a two-parameter region.

use super::super::run;

// The List witness: bind is concat-map.
#[test]
fn prelude_monad_arr_binds() {
    let source = r#"
        use /std/{Nat, Handle, Str, List, Monad};
        let l : List(Nat) = [1, 2];
        let doubled : List(Nat) = Monad/bind(l, (x) => [x, x]);
        /std/print(Nat/to_str(List/len(doubled)))
        "#;

    assert_eq!(run(source), b"4");
}

// The monadic sugar: each `e!` desugars to `/syn/Monad/bind(e, cont)`, whose `use` binder resolves the `Monad` witness from the action's type — no header, no imports needed for the dispatch itself.
#[test]
fn monadic_sugar_binds_through_the_concept() {
    let source = r#"
        use /std/{Nat, Handle, Str, Option, Monad};
        pub let chain(a : Option(Nat), b : Option(Nat)) -> Option(Nat) =
            let x = a!;
            let y = b!;
            Monad/pure(Nat/add(x, y));
        /std/print(Nat/to_str(Option/unwrap_or(chain(Option/some(20), Option/some(22)), 0)))
        "#;

    assert_eq!(run(source), b"42");
}

// Generic do-notation: `!` inside a function that is generic over the monad. Each site's `Monad(M)` goal (M a bound variable) resolves against the local `use` binder — impossible with a concrete bind function, and the payoff of dispatching `!` through the concept.
#[test]
fn bang_works_in_monad_generic_code() {
    let source = r#"
        use /syn/{Monad};
        use /std/{Nat, Handle, Str, Option, List};
        pub let add_both(@M : (Type) -> Type, use Monad(M), a : M(Nat), b : M(Nat)) -> M(Nat) =
            Monad/pure(a! + b!);
        let o : Option(Nat) = add_both(Option/some(20), Option/some(22));
        let l : List(Nat) = add_both([1, 2], [10]);
        /std/print(Str/concat(
            Nat/to_str(Option/unwrap_or(o, 0)),
            Nat/to_str(List/len(l))))
        "#;

    assert_eq!(run(source), b"422");
}

// The use side of the partial family: a `!` inside a `Box(Str, Nat)` region pins the bind's monad by right-biased partial imitation (`?M := (A) => Box(Str, A)`), which the parametric witness then answers. This is the spec's own reproduction for the rule, flipped to acceptance.
#[test]
fn a_bang_sequences_in_a_two_parameter_monad_region() {
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
        pub let prog : Box(Str, Nat) =
            let v = Monad/pure(3)!;
            Monad/pure(Nat/add(v, v));
        let out =
            match prog : (_) => Nat
            | wrap(value) => value
            end;
        /std/print(Nat/to_str(out))
        "#;

    assert_eq!(run(source), b"6");
}
