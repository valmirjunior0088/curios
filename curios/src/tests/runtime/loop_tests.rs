//! Loops that stay linear, and the values they carry through storage and retrieval.

use {
    crate::tests::{cont_optm, run, run_text},
    curios_runtime::MockHost,
};

// `match_reads_an_effectful_scrutinee_once` stood here. It pinned erasure aliasing a non-variable scrutinee before projecting, and it could only *observe* that through a re-read: matching `Cell/get(c)` and writing the cell in the arm made a second erasure of the scrutinee visible as a wrong binder value. `Cell/get(c) : Io(Nat)` is no longer a scrutinee at all — `Io` has no eliminator — so the program is a type error rather than a regression fixture, and a scrutinee that type-checks is now pure, which makes re-erasing one unobservable. The aliasing itself still happens and still matters for code size; `tests::codegen` is where a claim about emitted shape belongs. `io_monad::an_io_scrutinee_is_refused` holds the typing half.

#[test]
fn accumulation_loops_are_linear_by_construction() {
    // The rope representation's whole promise: a naive packed-concatenation accumulation loop is O(n) with no optimizer recognition anywhere — each step is one node allocation, and the single read at the end forces once. The pre-rope representation copied the accumulator per step, Θ(n²), which at this count still burns minutes where the loop takes milliseconds; a regression fails on the suite's patience. The final slice + print also pins the force → memo → host-write path end to end.
    //
    // **This is the direct spelling, restored, and its compiling at all is the closed machine's living proof.** `Bytes/slice` states `10 <= Bytes/len(b)`, a *decided* proposition, so `built` stands in a type and the compiler runs the whole accumulation at elaboration — a closed evaluation the machine affords inside the default budget where the recursive strategy refused it sixteen times over at the historical count, and where the pre-cap fusion before that exhausted the host. The test therefore proves both halves at once: the type level evaluates a twenty-five-thousand-step closed fold without a frame row, and the emitted program runs the same loop over the rope in linear time. A `head_of` indirection kept the subject opaque through both earlier eras; `tests::numeric`'s `a_bound_behind_a_parameter_evaluates_nothing` still holds that spelling, as the proof that opacity computes nothing rather than as anyone's workaround.
    //
    // **The count moved from the historical 100 000 when the direct spelling returned**, because the spelling makes elaboration run the loop too, at a measured ~160 units an iteration across the two checkers — the historical count costs the type level sixteen million units and this one four, which keeps the test seconds while leaving a quadratic regression minutes. The discrimination the count exists for is unchanged.
    //
    // `documentation/design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md` states the design this follows from.
    assert_eq!(
        run(r#"
        use /std/{Handle, Bytes, Nat, Str};
        let go(i : Nat, acc : Bytes) -> Bytes =
            match i
            | 0 => acc
            | k + 1; ih => go(k, x[..acc, ..Str/to_bytes("0123456789")])
            end;
        let built = go(25000, x[]);
        let head = Bytes/slice(built, 0, 10);
        let _ = Handle/write(Handle/stdout, head)!;
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(Bytes/len(built))))!;
        /std/Io/pure(())
        "#),
        b"0123456789250000"
    );
}

#[test]
fn peel_loops_are_linear_by_construction() {
    // The window (`view`) shape's whole promise, the consumption-side mirror of `accumulation_loops_are_linear_by_construction`: a naive head/tail peel over 100k bytes is O(n) with no optimizer recognition anywhere — the first read forces once, then every tail is an O(1) collapsed window and every head an O(1) read-through. The tail escapes through a `Cell` each step, so no compile-time pass (worker_wrapper's cursor, slice forwarding) can rescue it: a copying slice would be Θ(n²) and fail on the timeout. Matching directly on `Cell/get(c)` also leans on erasure's scrutinee alias — the cell must be read once per match, not once per projection (the head read lands *after* the `Cell/set` otherwise).
    assert_eq!(
        run(r#"
        use /std/{Handle, Byte, Bytes, Nat, Str, Cell, Io};
        let build(i : Nat, acc : Bytes) -> Bytes =
            match i
            | 0 => acc
            | k + 1; ih => build(k, x[..acc, ..Str/to_bytes("0123456789")])
            end;
        let built = build(10000, x[]);
        let c = Cell/new(built)!;
        let drain(fuel : Nat, acc : Nat) -> Io(Nat) =
            match fuel
            | 0 => Io/pure(acc)
            | f + 1; ih =>
                match Cell/get(c)!
                | x[] => Io/pure(acc)
                | x[h, ..t]; ih2 =>
                    let _ = Cell/set(c, t)!;
                    drain(f, acc + (Byte/to_nat(h) - 48))
                end
            end;
        let total = drain(Bytes/len(built) + 1, 0)!;
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(total)))!;
        /std/Io/pure(())
        "#),
        b"450000"
    );
}

#[test]
fn a_collapsed_wrapper_survives_storage_and_retrieval() {
    // The collapsed encoding end to end, at rest — the case no call-pattern specializer reaches: single-constructor wrapper values stored in a `List` and read back, a two-payload constructor riding an untagged tuple, and a nullary one riding the `Nat` zero. Every payload is runtime-tainted through stdin so nothing folds at compile time, and the printed sum proves each value round-tripped through its collapsed representation.
    let (system, io) = MockHost::builder().stdin_lines(["3"]).build();
    run_text(
        r#"
        use /std/{Str, Nat, Option, List, Io};
        induct Meters : Type
        | m(Nat)
        end
        induct Both : Type
        | both(Nat, Meters)
        end
        induct Only : Type
        | only()
        end
        let input = /std/read()!;
        match input : (_) => Io({})
        | some(bytes) =>
            match Str/of_bytes(bytes) : (_) => Io({})
            | some(s) =>
                match Nat/of_str(Str/trim(s)) : (_) => Io({})
                | some(d) =>
                    let stored : List(Meters) = [Meters/m(d), Meters/m(d + 1)];
                    let sum = List/fold(stored, 0, (x, acc) =>
                        match x : (_) => Nat
                        | m(n) => acc + n
                        end);
                    let extra =
                        match Both/both(d + 2, Meters/m(d + 3)) : (_) => Nat
                        | both(a, w) =>
                            match w : (_) => Nat
                            | m(n) => a + n
                            end
                        end;
                    let z =
                        match Only/only() : (_) => Nat
                        | only() => d
                        end;
                    /std/print(Nat/to_str(sum + extra + z))
                | none() => /std/print("bad input")
                end
            | none() => /std/print("bad utf8")
            end
        | none() => /std/print("no input")
        end
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output().to_vec(), b"21");
}

#[test]
fn an_immediate_leaf_tree_builds_and_sums_at_runtime() {
    // The immediate encoding end to end: leaves ride bare i31 payloads, nodes stay tagged tuples, and the match's kind test reunites them — over a runtime-tainted depth so the tree is genuinely built and walked in emitted code. Depth 4 numbers its 31 nodes 1..31, so the sum prints 496.
    let (system, io) = MockHost::builder().stdin_lines(["4"]).build();
    run_text(
        r#"
        use /std/{Str, Nat, Option, Io};
        induct Tree : Type
        | leaf(Nat)
        | node(Nat, Tree, Tree)
        end
        let build(d : Nat, v : Nat) -> Tree =
            match d : (_) => Tree
            | 0 => Tree/leaf(v)
            | dp + 1; ih => Tree/node(v, build(dp, v * 2), build(dp, v * 2 + 1))
            end;
        let sum(t : Tree) -> Nat =
            match t : (_) => Nat
            | leaf(n) => n % 1000003
            | node(n, l, r) => (n + sum(l) + sum(r)) % 1000003
            end;
        let input = /std/read()!;
        match input : (_) => Io({})
        | some(bytes) =>
            match Str/of_bytes(bytes) : (_) => Io({})
            | some(s) =>
                match Nat/of_str(Str/trim(s)) : (_) => Io({})
                | some(d) => /std/print(Nat/to_str(sum(build(d, 1))))
                | none() => /std/print("bad input")
                end
            | none() => /std/print("bad utf8")
            end
        | none() => /std/print("no input")
        end
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output().to_vec(), b"496");
}

/// Narrowed through `try_to_nat` because the subject *is* a NaN on one edge — the fixture unwraps against `Flt/nan` deliberately — so `NonNeg` is undischargeable by construction rather than by cost, and the deciding pair is the only correct shape. (`flt_of_str_returns_option` is the other side of that line: its subject is computed but always a number, so it takes the bounded form.)
///
/// An `Option(Flt)` built in a bind's continuation, unwrapped against `Flt/nan`, is the shape that ran the Cont fixpoint to its 1024-round backstop: the NaN default rides a switch edge, and with `CpsLiteral::Flt` compared under IEEE equality `forward_continuations` read that untouched edge as rewritten on every round. The literal is bitwise now; this is the program that found it, kept so the fixpoint's convergence on a NaN-carrying edge is asserted end-to-end rather than only at the pass.
#[test]
fn a_nan_default_on_a_runtime_option_converges() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Flt, Str, Bytes, Option};
        let taint = Bytes/len(/std/rand/bytes(3)!);
        let f(s : Str) -> Option(Flt) =
            let n = Nat/of_str(s)!;
            Option/some(Nat/to_flt(n));
        /std/print(Nat/to_str(
            Option/unwrap_or(Flt/try_to_nat(Option/unwrap_or(f(Nat/to_str(taint)), Flt/nan)), 0)))
        "#),
        b"3"
    );
}

/// Effect hugging: a deep closed computation beside a host effect collapses to a constant while the effect stays, in order — emergent from ANF plus the effect contract, no dedicated pass.
#[test]
fn arena_pure_computation_hugs_a_host_effect() {
    let source = r#"
        use /std/{Handle, Nat, Str};
        let triangle(n : Nat) -> Nat =
            match n : (_) => Nat
            | 0 => 0
            | p + 1; ih => n + ih
            end;
        let before = Handle/write(Handle/stdout, Str/to_bytes("a"))!;
        let pure = triangle(100);
        let after = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(pure)))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"a10000".to_vec());

    let cont_optm = cont_optm(source);
    assert!(
        !cont_optm.contains("triangle"),
        "the closed recursion must collapse to its constant around the effects:\n{}",
        &cont_optm[..cont_optm.len().min(4000)]
    );
}

/// The worker/wrapper gate: a monoid-deferred recursion (`count(t) + 1`) runs at a depth where an unrebased recursion overflows the runtime stack — the rebase threads the deferred addition into a tail accumulator.
#[test]
fn arena_deferred_context_recursion_is_stack_safe_at_depth() {
    let program = |depth: u32| {
        format!(
            r#"
        use /std/{{Handle, Nat, Str, Bytes}};
        let count(b : Bytes) -> Nat =
            match b : (_) => Nat
            | x[] => 0
            | x[h, ..t]; ih => count(t) + 1
            end;
        let build(n : Nat, acc : Bytes) -> Bytes =
            match n : (_) => Bytes
            | 0 => acc
            | p + 1; ih => build(p, x[0x61, ..acc])
            end;
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(count(build({depth}, x[])))))!;
        /std/Io/pure(())
        "#
        )
    };

    assert_eq!(run(&program(1_000)), b"1000".to_vec());
    assert_eq!(
        run(&program(60_000)),
        b"60000".to_vec(),
        "the rebased recursion must not overflow at depth"
    );
}
