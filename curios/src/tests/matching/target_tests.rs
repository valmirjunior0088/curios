//! What may be matched on: tuples, structs, opaque families, and an effectful scrutinee.

use {
    super::super::{error, run, run_text},
    curios_runtime::MockHost,
};

#[test]
fn opaque_inductive_is_usable_through_declaring_module_api() {
    let source = r#"
        use /std/{Nat, Handle};
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
        use /std/{Nat, Handle};
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
        use /std/{Nat, Handle};
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
        use /std/{Nat, Handle};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let f(p : Pair(Nat, Nat)) -> Nat =
            match p
            | Pair { fst, snd } => fst + snd
            end;
        /std/print(Nat/to_str(f(Pair { fst = 3, snd = 4 })))
        "#;

    assert_eq!(run(source), b"7");
}

// A struct match-arm pattern desugars to the same `proj`/`proj_label` calls an ordinary projection uses, so representation privacy is inherited automatically and unmodified — matching `struct_private_projection_rejected` in `structs.rs`, but reached through a match arm instead of `.0`.
#[test]
fn struct_arm_privacy_is_enforced() {
    let source = r#"
        use /std/{Nat, Handle};
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
        use /std/{File, Handle, Async};
        match Async/block_on(File/with("log.txt", File/Mode/append(), (f) => File/write(f, /std/Str/to_bytes("x"))))!
        | failure(_) => /std/print("deadlock")
        | success(outcome) =>
            match outcome
            | success(_) => /std/print("ok")
            | failure(_) => /std/print("error")
            end
        end
        "#;

    let (system, io) = MockHost::builder().build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"ok");
    assert_eq!(io.file(b"log.txt"), Some(b"x".to_vec()));
}

// `choose_evaluates_conditions_lazily` stood here. It observed the nested `Bool` lowering by giving a condition a side effect — `probe` printed its tag — so a ladder that evaluated every condition printed every tag. A condition is a `Bool`, and post-`Io` a `(Str, Bool) -> Bool` cannot perform an effect at all, so the fixture is not merely broken but unwritable. Nor is there a replacement at this layer: evaluating a *pure* condition twice, or not at all, is unobservable by any means the language offers, which is the retype working rather than coverage lost. What remains observable is the emitted shape, and that is `tests::codegen`'s to state.

// A headed inductive match with a `| _ =>` catch-all: enumerated constructors take their arm, everything else the default. rand-tainted so it runs as wasm.
#[test]
fn inductive_match_catch_all_covers_unenumerated_constructors() {
    let source = r#"
        use /std/{Option, Nat, Bytes, rand, Handle};
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
