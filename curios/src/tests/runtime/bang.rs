//! `!` regions at runtime: dispatch through a user witness, left-to-right threading, and mixed action types.

use {
    crate::tests::test_support::*, curios_module::RootSource, curios_native::test_support::*,
    curios_surface::Entrypoint,
};

#[test]
fn functions_entrypoints_and_witness_methods_sequence_in_their_expected_monads() {
    for source in [
        r#"
        use /std/{Io, print};
        let task() -> Io({}) = let _ = print("a")!; print("b");
        let _ = task()!;
        print("c")
        "#,
        r#"
        use /std/{Nat, Io, print};
        concept Task(A: Type): pub Type { action() -> Io(A) }
        satisfy Task(Nat) { action() = let _ = print("a")!; let _ = print("b")!; Io/pure(3) }
        let _ = Task/action(@Nat)!;
        print("c")
        "#,
        r#"
        use /std/{Io, print};
        let _ = print("a")!;
        let _ = match true | true => let _ = print("b")!; Io/pure(()) | false => print("unreached") end!;
        print("c")
        "#,
    ] {
        assert_eq!(run(source), b"abc");
    }
}

#[test]
fn dispatches_through_a_user_monad_witness() {
    // A user-declared Identity monad: `Box(A)` wraps a value, its witness's `bind` just applies the continuation. Each `!` desugars to `/std/Monad/bind(action, cont)`; the function's `Box(Nat)` result fixes the region's monad as `Box` and resolves the `Monad(Box)` witness — the same path a std monad takes, exercised end-to-end on a user type.
    let source = r#"
        use /std/{Nat, Str, Monad};
        pub struct Box(A : Type) : pub Type { unbox : A }
        satisfy Monad(Box) {
            pure(@A, x) = Box { unbox = x },
            bind(@A, @B, m, f) = f(m.unbox)
        }
        let a : Box(Nat) = Box { unbox = 3 };
        let b : Box(Nat) = Box { unbox = 4 };
        let result() -> Box(Nat) = Monad/pure(Nat/add(a!, b!));
        /std/print(Nat/to_str(result().unbox))
        "#;

    assert_eq!(run(source), b"7");
}

#[test]
fn std_parse_threads_bangs_left_to_right() {
    // The real `/std/Parse` monad, sequenced with bare `!` — each site resolves the witness for `Parse(Bytes)` from the function's expected result type. Two `Parse/bytes/byte!`s read consecutive bytes; reflecting through `Byte/to_nat` and using a *non-commutative* `Nat/sub` pins the evaluation order: on "BA" the first byte is 'B' (66) and the second 'A' (65), so the result is 66 - 65 = 1 (the reversed order would saturate to 0).
    let source = r#"
        use /std/{Parse, Byte, Bytes, Nat, Result};

        let parser() -> Parse/Parse(Bytes, Nat) =
            Parse/pure(Nat/sub(Byte/to_nat(Parse/bytes/byte!), Byte/to_nat(Parse/bytes/byte!)));

        match Parse/run(parser(), /std/Str/to_bytes("BA"))
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

        let parser() -> Parse/Parse(Bytes, Bytes) =
            Parse/pure(0x[..Parse/bytes/take_while(is_a)!, Parse/bytes/byte!]);

        match Parse/run(parser(), /std/Str/to_bytes("AB"))
        | success(s) =>
            match Str/of_bytes(s)
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

#[test]
fn a_failure_inside_an_argument_stops_the_enclosing_result_computation() {
    let source = r#"
        use /std/{Result, Str, Nat, print};
        let result() -> Result(Str, Nat) =
            Result/success(Nat/add(Result/failure(@Str, @Nat, "failed")!, 1));
        match result()
        | success(n) => print(Nat/to_str(n))
        | failure(message) => print(message)
        end
        "#;
    assert_eq!(run(source), b"failed");
}

#[test]
fn a_local_recursive_function_keeps_the_value_bound_before_it() {
    let source = r#"
        use /std/{Io, Nat, print};
        let main() -> Io({}) =
            let base = Io/pure(3)!;
            let count(n: Nat) -> Nat =
                match n | 0 => base | k + 1 => count(k) + 1 end;
            let result = Nat/add(count(2), 4);
            print(Nat/to_str(result));
        main()
        "#;
    assert_eq!(run(source), b"9");
}

#[test]
fn a_function_constructs_a_nested_description_before_its_scope_runs() {
    let source = r#"
        use /std/{Io, print};
        let main() -> Io({}) =
            let body() -> Io({}) =
                let _ = print("body")!;
                Io/pure(());
            let _ = print("acquire")!;
            Io/scope((scope) => Io/bind(Io/Scope/defer(scope, (_) => print("release")), (_) => body()));
        main()
        "#;
    assert_eq!(run(source), b"acquirebodyrelease");
}
