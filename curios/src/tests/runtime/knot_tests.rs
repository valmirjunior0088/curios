//! A recursive value knot: what is forced before it is read, and what traps rather than reading a hole.

use super::super::{error, run};

/// A knot mixing a function member with value members whose initializers build closures *through a call* — `wrap(…)` here, the `bind`/`peek` combinators in `/std/Toml/values` — where one value's closure captures, via the function member, a value initialized later. Every knot ties through cells now; this knot once lowered to a `RecInit` node whose machine lowering patched only *member* closures at the ready point, and `second`'s closure — a non-member born inside an initializer, calling `helper`, which reads `first` — captured a value that did not exist yet. The random byte keeps both closures live as values, so neither is folded away before the knot is lowered.
#[test]
fn a_closure_built_by_a_call_inside_a_knot_reaches_a_later_member() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Bytes, Byte, Io, rand, print};
        let wrap(f: (Nat) -> Nat) -> (Nat) -> Nat = (n) => f(n);
        let main: Io({}) =
            let bs = rand/bytes(1)!;
            rec first: (Nat) -> Nat =
                wrap((n) => match n | 0 => 0 | p + 1; _ => second(p) end)
            and second: (Nat) -> Nat =
                wrap((n) => helper(n))
            and helper(n: Nat) -> Nat = first(n);
            let f =
                match bs
                | x[b, ..rest] => match Byte/to_nat(b) % 2 | 0 => first | _ => second end
                | x[] => first
                end;
            print(Nat/to_str(f(3)));
        main
        "#),
        b"0"
    );
}

/// A knot's computed members are forced by need, so the order they are written in decides nothing: `table = build(len)` reads `size` through `build`, and reading it runs `size`'s initializer first — `43` whether `size` is written before or after. This program once printed `1` (the unfilled cell's placeholder), then trapped, then was refused at the erase boundary; it is the language's program and computes the language's answer now.
#[test]
fn an_initializer_calling_a_function_that_reads_a_later_member_forces_it_first() {
    for (first, second) in [
        ("table: Nat = build(Bytes/len(bs))", "size: Nat = base + 1"),
        ("size: Nat = base + 1", "table: Nat = build(Bytes/len(bs))"),
    ] {
        assert_eq!(
            run(&format!(
                r#"
                use /std/{{Nat, Str, Bytes, Byte, Io, rand, print}};
                let main: Io({{}}) =
                    let bs = rand/bytes(1)!;
                    rec base: Nat = Bytes/len(bs) + 40
                    and {first}
                    and build(n: Nat) -> Nat = n + size
                    and {second};
                    print(Nat/to_str(table));
                main
                "#
            )),
            b"43"
        );
    }
}

/// The stepper is never called by the initializer: it is handed to `List/fold`, which applies it through the `go` it nests, reaching it as a capture. Forced by need, the read of `size` inside the stepper runs `size`'s initializer, in either order.
#[test]
fn a_stepper_a_fold_applies_inside_an_initializer_forces_what_it_reads() {
    for (first, second) in [
        (
            "table: Nat = List/fold([Bytes/len(bs)], 0, (x, acc) => x + acc + size)",
            "size: Nat = Bytes/len(bs) + 40",
        ),
        (
            "size: Nat = Bytes/len(bs) + 40",
            "table: Nat = List/fold([Bytes/len(bs)], 0, (x, acc) => x + acc + size)",
        ),
    ] {
        assert_eq!(
            run(&format!(
                r#"
                use /std/{{Nat, Str, Bytes, Byte, List, Io, rand, print}};
                let main: Io({{}}) =
                    let bs = rand/bytes(1)!;
                    rec {first}
                    and {second};
                    print(Nat/to_str(table));
                main
                "#
            )),
            b"42"
        );
    }
}

/// The read no analysis sees: `p` is a parser over the later member `size`, and `n`'s initializer runs it at once through `Parse/run`, which applies `p`'s step as a *projected closure*. Forced by need that read simply runs `size`'s initializer — `42`, where the same program once computed `1` and then trapped on an empty cell. What remains for the runtime is the cycle below.
#[test]
fn a_member_read_through_a_closure_the_verifier_cannot_see_is_forced() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Bytes, Byte, Io, Result, Parse, rand, print};
        let main: Io({}) =
            let bs = rand/bytes(1)!;
            rec p: Parse(Nat) = Parse/map(Parse/any_byte, (b) => Byte/to_nat(b) + size)
            and n: Nat =
                match Parse/run(p, x[0x01])
                | success(v) => v
                | failure(_) => 0
                end
            and size: Nat = Bytes/len(bs) + 40;
            print(Nat/to_str(n));
        main
        "#),
        b"42"
    );
}

/// An evaluation cycle hidden where the verifier cannot see it — `n`'s initializer runs `p`, whose step reads `n` — is met by forcing: `n` is read while its own initializer runs, and the cell's *forcing* state is the trap. The frame is the member's force function, which is what names the member in the report.
///
/// Gated on `profile` because that report names a frame only when the wasm name section survived Binaryen, and `to_cwasm` keeps it for a profiling build alone — the same shape as `fixpoint` and `churn`, each gated on the spans that supply what it reads. Ungated it failed under a plain `cargo test -p curios` with the frame rendered `<wasm function 10>`, which reads like a codegen regression and is not one.
#[cfg(feature = "profile")]
#[test]
fn a_cycle_hidden_behind_a_closure_traps_at_the_member_being_forced() {
    let error = error(
        r#"
        use /std/{Nat, Str, Bytes, Byte, Io, Result, Parse, rand, print};
        let main: Io({}) =
            let bs = rand/bytes(1)!;
            rec p: Parse(Nat) = Parse/map(Parse/any_byte, (b) => Byte/to_nat(b) + n)
            and n: Nat =
                match Parse/run(p, bs)
                | success(v) => v
                | failure(_) => 0
                end;
            print(Nat/to_str(n));
        main
        "#,
    );
    assert!(
        error.contains("execution failed: wasm trap: wasm `unreachable` instruction executed")
            && error.contains("/force"),
        "the cycle must trap in a member's force function, and the report must name the trap before the frames: {error}"
    );
}

#[test]
fn a_member_reading_itself_is_refused_before_it_can_trap() {
    // The cycle the verifier can see without any summary: `p`'s initializer hands `p` to `Parse/map`, which reads it, so forcing `p` forces `p`. This compiled and trapped as a black hole while a direct self-read was exempt from the cycle graph for the sake of the *unused* `rec loop = loop`; the exemption is now the unused member's alone.
    let error = error(
        r#"
        use /std/{Nat, Str, Bytes, Io, Result, Parse, rand, print};
        let main: Io({}) =
            let bs = rand/bytes(1)!;
            rec p: Parse(Nat) = Parse/or(Parse/map(Parse/any_byte, (_) => 1), Parse/map(p, (n) => n));
            match Parse/run(p, bs)
            | success(v) => print(Nat/to_str(v))
            | failure(_) => print("0")
            end;
        main
        "#,
    );
    assert!(
        error.contains("evaluates itself") && !error.contains("execution failed"),
        "the self-read must be refused at compile time: {error}"
    );
}
