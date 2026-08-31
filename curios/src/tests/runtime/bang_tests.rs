//! `!` regions at runtime: dispatch through a user witness, left-to-right threading, and mixed action types.

use {
    crate::tests::{run, run_entrypoint},
    curios_runtime::MockHost,
    curios_text::{Entrypoint, RootSource},
};

#[test]
fn dispatches_through_a_user_monad_witness() {
    // A user-declared Identity monad: `Box(A)` wraps a value, its witness's `bind` just applies the continuation. Each `!` desugars to `/syn/Monad/bind(action, cont)`; the action's `Box(Nat)` type pins `M := Box` (flex-apply imitation) and resolves `monad_box` — the same path a std monad takes, exercised end-to-end on a user type.
    let source = r#"
        use /std/{Nat, Handle, Str, Monad};
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
    // The real `std/Parse` monad, sequenced with bare `!` — each site resolves the `Monad(Parse)` witness from the action's type. Two `any_byte!`s read consecutive bytes; reflecting through `Byte/to_nat` and using a *non-commutative* `Nat/sub` pins the evaluation order: on "BA" the first byte is 'B' (66) and the second 'A' (65), so the result is 66 - 65 = 1 (the reversed order would saturate to 0).
    let source = r#"
        use /std/{Parse, Byte, Nat, Result, Handle};

        let parser : Parse/Parse(Nat) =
            Parse/pure(Nat/sub(Byte/to_nat(Parse/any_byte!), Byte/to_nat(Parse/any_byte!)));

        match Parse/run(parser, /std/Str/to_bytes("BA")) : (_) => /std/Io({})
        | success(n) => /std/print(Nat/to_str(n))
        | failure(msg) => /std/print(msg)
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
    // A single region sequences two actions of *different* payload types: a `Parse(Bytes)` (`take_while`) and a `Parse(Byte)` (`any_byte`). Each `!` site elaborates its own `/syn/Monad/bind` application with fresh implicits (`?A := Bytes` for the first, `?A := Byte` for the second), while the shared continuation typing forces one monad for the region. On "AB": `take_while(is_a)` reads "A" (stops at 'B'), then `any_byte` reads 'B' (66); splicing the byte onto the run gives "AB".
    let source = r#"
        use /std/{Parse, Byte, Bytes, Bool, Result, Handle, Str};

        let is_a : (Byte) -> Bool = (b) => b == 0x41;

        let parser : Parse/Parse(Bytes) =
            Parse/pure(x[..Parse/take_while(is_a)!, Parse/any_byte!]);

        match Parse/run(parser, /std/Str/to_bytes("AB")) : (_) => /std/Io({})
        | success(s) =>
            match Str/of_bytes(s) : (_) => /std/Io({})
            | some(t) => /std/print(t)
            | none() => /std/print("invalid utf-8")
            end
        | failure(msg) => /std/print(msg)
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
