//! What may be matched on: tuples, structs, opaque families, and an effectful scrutinee.

use {
    crate::tests::{error, run, run_text},
    curios_runtime::MockHost,
};

#[test]
fn opaque_inductive_is_usable_through_declaring_module_api() {
    let source = r#"
        use /std/{Nat};
        pub mod Secret
            use /std/{Nat};
            pub induct T : Type
            | wrap(Nat)
            end
            pub let make(n : Nat) -> T = T/wrap(n);
            pub let reveal(t : T) -> Nat =
                match t
                | wrap(n) => n
                end;
        end
        /std/print(Nat/to_str(Secret/reveal(Secret/make(7))))
        "#;

    assert_eq!(run(source), b"7");
}

#[test]
fn opaque_inductive_empty_elimination_is_private() {
    let source = r#"
        use /std/{Nat};
        pub mod Secret
            use /std/{Nat};
            pub induct T : Type
            | wrap(Nat)
            end
            pub let make(n : Nat) -> T = T/wrap(n);
        end
        let reveal(t : Secret/T) -> Nat = match t : (_) => Nat end;
        /std/print(Nat/to_str(reveal(Secret/make(7))))
        "#;

    let error = error(source);
    assert!(
        error.contains("representation of type '/Secret/T' is private"),
        "unexpected error: {error}"
    );
}

// A tuple value used as a match target directly — no constructor tag at all — desugars to plain projection, never a core `Match` node.
#[test]
fn tuple_match_target_projects_fields() {
    let source = r#"
        use /std/{Nat};
        let f(p : { Nat, Nat }) -> Nat =
            match p
            | (x, y) => x + y
            end;
        /std/print(Nat/to_str(f((3, 4))))
        "#;

    assert_eq!(run(source), b"7");
}

// A struct value used as a match target directly, including field-punning.
#[test]
fn struct_match_target_projects_fields() {
    let source = r#"
        use /std/{Nat};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let f(p : Pair(Nat, Nat)) -> Nat =
            match p
            | Pair { fst, snd } => fst + snd
            end;
        /std/print(Nat/to_str(f(Pair { fst = 3, snd = 4 })))
        "#;

    assert_eq!(run(source), b"7");
}

// A struct match-arm pattern desugars to the same `proj`/`proj_label` calls an ordinary projection uses, so representation privacy is inherited automatically and unmodified — matching `struct_private_projection_rejected` in `structs/visibility_tests.rs`, but reached through a match arm instead of `.0`.
#[test]
fn struct_arm_privacy_is_enforced() {
    let source = r#"
        use /std/{Nat};
        mod Celsius
            use /std/{Nat};
            pub struct Celsius : Type { Nat }
            pub let of_nat(n : Nat) -> Celsius = Celsius { n };
        end
        let c : Celsius/Celsius = Celsius/of_nat(42);
        match c
        | Celsius/Celsius { n } => /std/print(Nat/to_str(n))
        end
        "#;

    let error = error(source);
    assert!(
        error.contains("field") && error.contains("private"),
        "unexpected error: {error}"
    );
}

#[test]
fn effectful_match_scrutinee_runs_once() {
    let source = r#"
        use /std/{File, Path, Try};
        match Try/run(File/with(Path/of_str("log.txt"), File/Mode/append(), (f) => File/write(f, /std/Str/to_bytes("x"))))!
        | success(_) => /std/print("ok")
        | failure(_) => /std/print("error")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"ok");
    assert_eq!(io.file(b"log.txt"), Some(b"x".to_vec()));
}

// Whether a `choose` evaluates its conditions lazily has no test at this layer: a condition is a `Bool`, no function returning one performs an effect, and evaluating a *pure* condition twice, or not at all, is unobservable by any means the language offers. What is observable is the emitted shape, and that is `tests::codegen`'s to state.

// A headed inductive match with a `| _ =>` catch-all: enumerated constructors take their arm, everything else the default. rand-tainted so it runs as wasm.
#[test]
fn inductive_match_catch_all_covers_unenumerated_constructors() {
    let source = r#"
        use /std/{Option, Nat, Bytes, rand};
        let f(o : Option(Nat)) -> Nat =
            match o
            | some(x) => x + 10
            | _ => 99
            end;
        let z = Bytes/len(rand/bytes(0)!);
        /std/print(Nat/to_str((f(Option/some(5)) + f(Option/none())) + z))
        "#;

    // some(5) → 15 via its arm; none() → 99 via the catch-all; 15 + 99 = 114.
    assert_eq!(run(source), b"114");
}

// An arm naming no constructor of the scrutinee's type is told which it has, each as a pattern writes it — a plain payload as `_`, a hidden one left out.
#[test]
fn an_arm_naming_no_constructor_is_told_which_there_are() {
    let nullary = error(
        r#"
        use /std/{Nat, Ordering};
        let f(o : Ordering) -> Nat = match o | less() => 0 end;
        /std/print(Nat/to_str(f(Ordering/lt())))
        "#,
    );
    assert!(
        nullary.contains("match arm 'less' is not a constructor of")
            && nullary.contains("its constructors are lt(), eq(), gt()"),
        "unexpected error: {nullary}"
    );

    let payloads = error(
        r#"
        use /std/{Nat};
        induct Sized(T : Type) : (length : Nat) -> pub Type
        | empty() : (0)
        | push(@n : Nat, head : T, tail : Sized(T)(n)) : (n + 1)
        end
        let f(@n : Nat, s : Sized(Nat)(n)) -> Nat = match s | cons(@m, h, t) => h end;
        /std/print("no")
        "#,
    );
    assert!(
        payloads.contains("its constructors are empty(), push(_, _)"),
        "unexpected error: {payloads}"
    );
}

// A match with no arms eliminates a type with no constructors and nothing else, so over a carrier it says that rather than naming constructors nobody wrote.
#[test]
fn a_match_with_no_arms_over_a_carrier_says_what_it_eliminates() {
    let error = error(
        r#"
        use /std/{Nat};
        let f(n : Nat) -> Nat = match n end;
        /std/print(Nat/to_str(f(1)))
        "#,
    );
    assert!(
        error.contains("a match with no arms eliminates only a type with no constructors")
            && error.contains("head has type: Nat"),
        "unexpected error: {error}"
    );
}
