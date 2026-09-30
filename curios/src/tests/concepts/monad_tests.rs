//! `!` sequencing through a user monad witness, including a two-parameter region.

use crate::tests::{error, run};

// The List witness: bind is concat-map.
#[test]
fn prelude_monad_arr_binds() {
    let source = r#"
        use /std/{Nat, Str, List, Monad};
        let l : List(Nat) = [1, 2];
        let doubled : List(Nat) = Monad/bind(l, (x) => [x, x]);
        /std/print(Nat/to_str(List/len(doubled)))
        "#;

    assert_eq!(run(source), b"4");
}

// The monadic sugar: each `e!` desugars to `/std/Monad/bind(e, cont)`, whose `use` binder resolves the `Monad` witness from the action's type — no header, no imports needed for the dispatch itself.
#[test]
fn monadic_sugar_binds_through_the_concept() {
    let source = r#"
        use /std/{Nat, Str, Option, Monad};
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
        use /std/{Monad};
        use /std/{Nat, Str, Option, List};
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

/// Three ways to sequence a `Result(Str, Type)`, whose payload is a type and so sits a level above the types it names: `walk` with `!` in an arm, `flat` with `!` in a flat body, and `spelled` through `Monad/bind` itself. All three go through `Result`'s `Monad` witness, which pinned the method levels at zero — `bind` took `A, B : Type 0` only — so each was refused, the arm as "this Type would need to be strictly below itself". A concept's method level is now its family's domain (`UniverseSolver::identify_bounded_choices`), which `Result`'s witness leaves to the caller. `tag` reads each outcome back, so the three are run rather than only checked.
const LARGE_PAYLOAD: &str = r#"
    use /std/{Str, Nat, List, Result, Monad};
    pub let walk(x: Result(Str, Type), n: List(Str)) -> Result(Str, Type) =
        match n
        | [] => x
        | [_, .._] =>
            let t = x!;
            Result/success(t)
        end;
    pub let flat(x: Result(Str, Type)) -> Result(Str, Type) =
        let t = x!;
        Result/success(t);
    pub let spelled(x: Result(Str, Type)) -> Result(Str, Type) =
        Monad/bind(x, (t) => Result/success(t));
    let tag(r: Result(Str, Type)) -> Str =
        match r
        | success(_) => "s"
        | failure(_) => "f"
        end;
"#;

#[test]
fn a_bang_sequences_a_large_payload() {
    let source = format!(
        r#"{LARGE_PAYLOAD}
        /std/print(Str/flatten([
            tag(walk(Result/success(Nat), ["a"])),
            tag(flat(Result/success(Str))),
            tag(spelled(Result/failure("refused"))),
        ]))
        "#
    );

    assert_eq!(run(&source), b"ssf");
}

// The control: `Result/bind` names no witness, so it sequenced the same payload before the witness's method levels were its domain, and still does.
#[test]
fn a_large_payload_binds_without_the_witness() {
    let source = format!(
        r#"{LARGE_PAYLOAD}
        let direct(x: Result(Str, Type)) -> Result(Str, Type) = Result/bind(x, (t) => Result/success(t));
        /std/print(Str/flatten([tag(direct(Result/success(Nat))), tag(direct(Result/failure("refused")))]))
        "#
    );

    assert_eq!(run(&source), b"sf");
}

// `!` holds its region at the level of the action it binds. A region's monad is one nominal instance, and both checkers compare a nominal type's universe levels for equality, so `small`'s `Result(Str, Nat)`, at zero, pins the region below the `Type` it answers with. `Result/bind` names no witness and instantiates each side apart, so it accepts the same program. `Result`'s levels only type its parameters, and comparing them by variance — Rocq infers such a level irrelevant — would accept both; until then `/std/Cli`'s `fill` binds through `Result/bind`, and this refusal is the fixture that flips.
#[test]
fn a_bang_holds_its_region_at_a_lower_nominal_actions_level() {
    let bang = r#"
        use /std/{Str, Nat, Result};
        let small(n: Nat) -> Result(Str, Nat) = Result/success(n);
        pub let big(n: Nat) -> Result(Str, Type) =
            let _ = small(n)!;
            Result/success(Nat);
        /std/print("bound")
        "#;
    let message = error(bang);
    assert!(message.contains("strictly below itself"), "got: {message}");
    assert!(message.contains("/big"), "got: {message}");

    let bind = r#"
        use /std/{Str, Nat, Result};
        let small(n: Nat) -> Result(Str, Nat) = Result/success(n);
        pub let big(n: Nat) -> Result(Str, Type) = Result/bind(small(n), (_) => Result/success(Nat));
        /std/print("bound")
        "#;
    assert_eq!(run(bind), b"bound");
}
