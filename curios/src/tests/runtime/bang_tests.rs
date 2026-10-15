//! `!` regions at runtime: dispatch through a user witness, left-to-right threading, and mixed action types.

use {
    crate::tests::{run, run_entrypoint},
    curios_runtime::MockHost,
    curios_text::{Entrypoint, RootSource},
};

#[test]
fn dispatches_through_a_user_monad_witness() {
    // A user-declared Identity monad: `Box(A)` wraps a value, its witness's `bind` just applies the continuation. Each `!` desugars to `/std/Monad/bind(action, cont)`; the action's `Box(Nat)` type pins `M := Box` (flex-apply imitation) and resolves the `Monad(Box)` witness — the same path a std monad takes, exercised end-to-end on a user type.
    let source = r#"
        use /std/{Nat, Str, Monad};
        pub struct Box(A : Type) : pub Type { unbox : A }
        satisfy Monad(Box) {
            pure(@A, x) = Box { unbox = x },
            bind(@A, @B, m, f) = f(m.unbox)
        }
        let a : Box(Nat) = Box { unbox = 3 };
        let b : Box(Nat) = Box { unbox = 4 };
        let result : Box(Nat) = Monad/pure(Nat/add(a!, b!));
        /std/print(Nat/to_str(result.unbox))
        "#;

    assert_eq!(run(source), b"7");
}

#[test]
fn std_parse_threads_bangs_left_to_right() {
    // The real `/std/Parse` monad, sequenced with bare `!` — each site resolves the `Monad(Parse)` witness from the action's type. Two `Parse/bytes/byte!`s read consecutive bytes; reflecting through `Byte/to_nat` and using a *non-commutative* `Nat/sub` pins the evaluation order: on "BA" the first byte is 'B' (66) and the second 'A' (65), so the result is 66 - 65 = 1 (the reversed order would saturate to 0).
    let source = r#"
        use /std/{Parse, Byte, Bytes, Nat, Result};

        let parser : Parse/Parse(Bytes, Nat) =
            Parse/pure(Nat/sub(Byte/to_nat(Parse/bytes/byte!), Byte/to_nat(Parse/bytes/byte!)));

        match Parse/run(parser, /std/Str/to_bytes("BA")) : (_) => /std/Io({})
        | success(n) => /std/print(Nat/to_str(n))
        | failure(e) => /std/print(e.message)
        end
        "#;

    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");
    // These fixtures declare no file-backed module, so the resolver has nothing to answer for.
    let loader = RootSource::none();

    let (system, io) = MockHost::builder().build();
    run_entrypoint(&entrypoint, &loader, system).expect("expected result");
    assert_eq!(io.output(), b"1");
}

#[test]
fn region_mixes_action_types() {
    // A single region sequences two actions of *different* payload types: a `Parse(Bytes, Bytes)` (`take_while`) and a `Parse(Bytes, Byte)` (`byte`). Each `!` site elaborates its own `/std/Monad/bind` application with fresh implicits (`?A := Bytes` for the first, `?A := Byte` for the second), while the shared continuation typing forces one monad for the region. On "AB": `take_while(is_a)` reads "A" (stops at 'B'), then `byte` reads 'B' (66); splicing the byte onto the run gives "AB".
    let source = r#"
        use /std/{Parse, Byte, Bytes, Bool, Result, Str};

        let is_a : (Byte) -> Bool = (b) => b == 0x41;

        let parser : Parse/Parse(Bytes, Bytes) =
            Parse/pure(x[..Parse/bytes/take_while(is_a)!, Parse/bytes/byte!]);

        match Parse/run(parser, /std/Str/to_bytes("AB")) : (_) => /std/Io({})
        | success(s) =>
            match Str/of_bytes(s) : (_) => /std/Io({})
            | success(t) => /std/print(t)
            | failure(_) => /std/print("invalid utf-8")
            end
        | failure(e) => /std/print(e.message)
        end
        "#;

    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");
    // These fixtures declare no file-backed module, so the resolver has nothing to answer for.
    let loader = RootSource::none();

    let (system, io) = MockHost::builder().build();
    run_entrypoint(&entrypoint, &loader, system).expect("expected result");
    assert_eq!(io.output(), b"AB");
}

/// A nested block's first statement has no privilege over the statements after it: every action belongs to the enclosing function's region.
#[test]
fn unused_bindings_do_not_change_where_nested_actions_sequence() {
    for prefix in ["", "let _ = 0;", "let _ = 0; let _ = 1;"] {
        let source = format!(
            r#"
            use /std/{{Io, Nat, print}};
            let main() -> Io({{}}) =
                let task: Io(Nat) = (
                    {prefix}
                    let _ = print("task")!;
                    Io/pure(7));
                print("made");
            main()
            "#
        );
        assert_eq!(run(&source), b"taskmade");
    }
}

#[test]
fn nested_bindings_keep_their_names_without_capturing_sibling_arguments() {
    let source = r#"
        use /std/{Io, Nat, print};
        let main() -> Io({}) =
            let n = 10;
            let result = Nat/add((
                let n = 1;
                let next: Nat = Io/pure(n + 1)!;
                next), n);
            print(Nat/to_str(result));
        main()
        "#;
    assert_eq!(run(source), b"12");
}

#[test]
fn destructured_values_remain_in_scope_after_nested_bangs() {
    let source = r#"
        use /std/{Io, Nat, print};
        struct Pair: Type { first: Nat, second: Nat }
        let main() -> Io({}) =
            let result = Nat/add((
                let Pair { first = a, second = b } = Io/pure(Pair { first = 3, second = 4 })!;
                let _ = print("x")!;
                a + b), 2);
            print(Nat/to_str(result));
        main()
        "#;
    assert_eq!(run(source), b"x9");
}

#[test]
fn a_nested_failure_stops_the_enclosing_result_computation() {
    let source = r#"
        use /std/{Result, Str, Nat, print};
        let result() -> Result(Str, Nat) =
            Result/success(Nat/add((
                let _ = 0;
                let n = Result/failure(@Str, @Nat, "failed")!;
                n), 1));
        match result()
        | success(n) => print(Nat/to_str(n))
        | failure(message) => print(message)
        end
        "#;
    assert_eq!(run(source), b"failed");
}

#[test]
fn a_nested_recursive_function_keeps_the_value_bound_before_it() {
    let source = r#"
        use /std/{Io, Nat, print};
        let main() -> Io({}) =
            let result = Nat/add((
                let base = Io/pure(3)!;
                let count(n: Nat) -> Nat =
                    match n | 0 => base | k + 1 => count(k) + 1 end;
                count(2)), 4);
            print(Nat/to_str(result));
        main()
        "#;
    assert_eq!(run(source), b"9");
}

#[test]
fn a_function_constructs_a_nested_description_before_its_bracket_runs() {
    let source = r#"
        use /std/{Io, print};
        let main() -> Io({}) =
            let body() -> Io({}) =
                let _ = print("body")!;
                Io/pure(());
            let _ = print("acquire")!;
            Io/using((_) => print("release"), body());
        main()
        "#;
    assert_eq!(run(source), b"acquirebodyrelease");
}
