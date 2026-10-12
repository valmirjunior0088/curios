//! An application of a metavariable against a type former's node: the function it stands for is taken where the term that awaits it completes.
//!
//! `M(A)` against `Two(Nat, Bool)` holds at `M := (x) => Two(Nat, x)`, the imitation, and at `(x) => Two(x, Bool)` as well: one function among several, so conversion leaves the pair parked, and the call takes the imitation only once it has read every argument, where nothing else stated the function and every such pair gives it the same. Each program that compiles is run, so the kernel has judged what the elaborator admitted.

use crate::tests::{error, run};

/// `body` over the declarations these programs share, its last line the text printed.
fn program(body: &str) -> String {
    format!(
        r#"
        use /std/{{Nat, Bool, Str, Option, Monad, print}};
        struct Two(A: Type, B: Type): pub Type {{ a: A, b: B }}
        struct Holder(M: (Type) -> Type): pub Type {{ n: Nat }}
        let h: Holder((X: Type) => Two(X, Bool)) = Holder {{ n = 7 }};
        let x: Two(Nat, Bool) = Two {{ a = 1, b = true }};
        let y: Two(Nat, Nat) = Two {{ a = 1, b = 2 }};
        let z: Two(Bool, Bool) = Two {{ a = true, b = true }};
        let and(s: Str, t: Str) -> Str = Str/concat(s, Str/concat(" ", t));
        {body}
        "#
    )
}

// An argument whose type states the function gives it, whichever side of it the application is declared on, and the function it states is not the imitation: `M(A)` against `Two(Nat, Bool)` waits, and is read at `M := (X) => Two(X, Bool)`. So do two parts of one expected type, which no call stands around.
#[test]
fn a_function_an_argument_states_is_not_taken_from_an_imitation() {
    let source = program(
        r#"
        let first(@M: (Type) -> Type, @A: Type, x: M(A), w: Holder(M)) -> Nat = w.n;
        let second(@M: (Type) -> Type, @A: Type, w: Holder(M), x: M(A)) -> Nat = w.n;
        let before(@M: (Type) -> Type, p: Two(M(Nat), Holder(M))) -> Nat = p.b.n;
        let after(@M: (Type) -> Type, p: Two(Holder(M), M(Nat))) -> Nat = p.a.n;
        let one: (Two(Two(Nat, Bool), Holder((X: Type) => Two(X, Bool)))) -> Nat = before;
        let other: (Two(Holder((X: Type) => Two(X, Bool)), Two(Nat, Bool))) -> Nat = after;
        print(Str/join(" ", [
            Nat/to_str(first(x, h)),
            Nat/to_str(second(h, x)),
            Nat/to_str(one(Two { a = x, b = h })),
            Nat/to_str(other(Two { a = h, b = x })),
        ]))
        "#,
    );

    assert_eq!(run(&source), b"7 7 7 7");
}

// Where nothing else states the function, the call takes the imitation once it has read its arguments, ahead of a lambda with a domain left open: the imitation is what types the lambda's parameter, which its body projects.
#[test]
fn a_function_only_an_imitation_gives_is_taken_where_the_call_completes() {
    let source = program(
        r#"
        let lone(@M: (Type) -> Type, @A: Type, x: M(A), f: (M(A)) -> M(Bool)) -> M(Bool) = f(x);
        let r = lone(x, (p) => Two { a = p.a + 1, b = p.b });
        print(Nat/to_str(r.a))
        "#,
    );

    assert_eq!(run(&source), b"2");
}

// A generic value named bare where its type is declared is read by no call: its application waits for the item, which completes as a term does, before its levels close, in a declaration and inside a body alike.
#[test]
fn an_application_no_call_awaits_is_imitated_where_its_item_completes() {
    let source = program(
        r#"
        let five(@M: (Type) -> Type, use Monad(M)) -> M(Nat) = Monad/pure(5);
        let declared: Option(Nat) = five;
        let inside(n: Nat) -> Nat =
            let local: Option(Nat) = five;
            Option/unwrap_or(local, n);
        print(and(Nat/to_str(Option/unwrap_or(declared, 0)), Nat/to_str(inside(0))))
        "#,
    );

    assert_eq!(run(&source), b"5 5");
}

// An imitation pairs the application's arguments with the node's, and a pair that stands between two open types waits, conversion solving neither to the other. The imitation is kept: it is the key the slot's witness is found by, which gives the type the pair waited for, and a later line gives it where no slot does.
#[test]
fn an_imitation_is_kept_where_its_arguments_still_wait() {
    let source = program(
        r#"
        struct Fn(S: Type, T: Type): pub Type { run: (S) -> T }
        let same(@S: Type) -> Fn(S, S) = Fn { run = (s) => s };
        concept Starts(M: (Type) -> Type): pub Type {
            start(@A: Type, M(A)) -> Nat,
        }
        satisfy Starts((T: Type) => Fn(Nat, T)) {
            start(f) = 3,
        }
        let keep(@M: (Type) -> Type, @A: Type, f: M(A)) -> M(A) = f;
        let kept = keep(same());
        print(and(Nat/to_str(Starts/start(same())), Nat/to_str(kept.run(4))))
        "#,
    );

    assert_eq!(run(&source), b"3 4");
}

// Two arguments whose imitations give the implicit one function agree, and the implicit written says which of two is meant where they would not.
#[test]
fn two_imitations_that_give_an_implicit_one_function_agree() {
    let source = program(
        r#"
        let both(@M: (Type) -> Type, @A: Type, @B: Type, x: M(A), y: M(B)) -> Nat = 3;
        print(Str/join(" ", [
            Nat/to_str(both(x, y)),
            Nat/to_str(both(y, x)),
            Nat/to_str(both(@(X: Type) => Two(X, Bool), x, z)),
        ]))
        "#,
    );

    assert_eq!(run(&source), b"3 3 3");
}

// Two that give it two functions refuse the call, in either order of the arguments, naming the implicit and both: `(X) => Two(X, Bool)` answers both arguments where neither imitation does, so taking either would be taking the order the arguments were written in.
#[test]
fn two_imitations_that_give_an_implicit_two_functions_refuse_the_call() {
    for (arguments, one, another) in [("x, z", "Nat", "Bool"), ("z, x", "Bool", "Nat")] {
        let report = error(&program(&format!(
            r#"
            let both(@M: (Type) -> Type, @A: Type, @B: Type, x: M(A), y: M(B)) -> Nat = 3;
            print(Nat/to_str(both({arguments})))
            "#
        )));

        assert!(
            report.contains("two types each give the implicit argument 'M' of 'both' a value, and the values differ")
                && report.contains(&format!("one: (x) => Two({one}, x)"))
                && report.contains(&format!("another: (x) => Two({another}, x)")),
            "unexpected report for `both({arguments})`:\n{report}"
        );
    }
}
