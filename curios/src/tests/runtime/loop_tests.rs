//! Loops that stay linear, and the values they carry through storage and retrieval.

use {
    crate::tests::{cont_optm, cont_optm_module, run, run_text},
    curios_cont::{Callee, Node, ValueExpr},
    curios_runtime::MockHost,
    std::collections::BTreeSet,
};

// Erasure aliases a non-variable scrutinee before projecting, which matters for code size and is unobservable at run time: a scrutinee that type-checks is pure, since `Io` has no eliminator (`effects::an_io_scrutinee_is_refused` holds that half), so re-erasing one changes nothing a program sees. `tests::codegen` is where a claim about emitted shape belongs.

#[test]
fn accumulation_loops_are_linear_by_construction() {
    // The rope representation's whole promise: a naive packed-concatenation accumulation loop is O(n) with no optimizer recognition anywhere — each step is one node allocation, and the single read at the end forces once. A representation copying the accumulator per step would be Θ(n²), minutes at this count where the loop takes milliseconds, so a regression fails on the suite's patience. The final slice + print also pins the force → memo → host-write path end to end.
    //
    // **This is the direct spelling.** `Bytes/slice` states `10 <= Bytes/len(b)`, a *decided* proposition, so `built` stands in a type and the compiler runs the whole accumulation at elaboration — a closed evaluation the machine affords inside the default budget. The test therefore proves both halves at once: the type level evaluates a twenty-five-thousand-step closed fold without a frame row, and the emitted program runs the same loop over the rope in linear time. `tests::numeric`'s `a_bound_behind_a_parameter_evaluates_nothing` holds the opaque spelling, as the proof that opacity computes nothing.
    //
    // **The count is 25 000 because the spelling makes elaboration run the loop too**, at a measured ~160 units an iteration across the two checkers — four million units at this count, which keeps the test seconds while leaving a quadratic regression minutes.
    //
    // The design this follows from is `documentation/design/arithmetic/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md`.
    assert_eq!(
        run(r#"
        use /std/{Bytes, Nat, Str, Io};
        let go(i : Nat, acc : Bytes) -> Bytes =
            match i
            | 0 => acc
            | k + 1; ih => go(k, x[..acc, ..Str/to_bytes("0123456789")])
            end;
        let built = go(25000, x[]);
        let head = Bytes/slice(built, 0, 10);
        let _ = Io/write(Io/stdout, head)!;
        let _ = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(Bytes/len(built))))!;
        /std/Io/pure(())
        "#),
        b"0123456789250000"
    );
}

#[test]
fn peel_loops_are_linear_by_construction() {
    // The window (`view`) shape's whole promise, the consumption-side mirror of `accumulation_loops_are_linear_by_construction`: a naive head/tail peel over 100k bytes is O(n) with no optimizer recognition anywhere — the first read forces once, then every tail is an O(1) collapsed window and every head an O(1) read-through. The tail escapes through a fresh write-once `Cell` each step, so no compile-time pass can rescue it: a copying slice would be Θ(n²) and fail on the timeout.
    assert_eq!(
        run(r#"
        use /std/{Byte, Bytes, Nat, Str, Cell, Option, Io};
        let build(i : Nat, acc : Bytes) -> Bytes =
            match i
            | 0 => acc
            | k + 1; ih => build(k, x[..acc, ..Str/to_bytes("0123456789")])
            end;
        let built = build(10000, x[]);
        let c = Cell/new()!;
        let _ = Cell/fill(c, built)!;
        let drain(c: Cell(Bytes), fuel : Nat, acc : Nat) -> Io(Nat) =
            match fuel
            | 0 => Io/pure(acc)
            | f + 1; ih =>
                match Option/unwrap_or(Cell/poll(c)!, x[])
                | x[] => Io/pure(acc)
                | x[h, ..t]; ih2 =>
                    let next = Cell/new()!;
                    let _ = Cell/fill(next, t)!;
                    drain(next, f, acc + (Byte/to_nat(h) - 48))
                end
            end;
        let total = drain(c, Bytes/len(built) + 1, 0)!;
        let _ = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(total)))!;
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
        let standard_input = /std/Io/Reader/new(/std/Io/stdin)!;
        let input = /std/Result/unwrap_or(/std/Try/run(/std/Io/Reader/line(standard_input))!, /std/Option/none());
        match input : (_) => Io({})
        | some(bytes) =>
            match Str/of_bytes(bytes) : (_) => Io({})
            | success(s) =>
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
            | failure(_) => /std/print("bad utf8")
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
    // The immediate encoding end to end: leaves ride bare i31 payloads, nodes stay tagged rows, and the match's kind test reunites them — over a runtime-tainted depth so the tree is genuinely built and walked in emitted code. Depth 4 numbers its 31 nodes 1..31, so the sum prints 496.
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
        let standard_input = /std/Io/Reader/new(/std/Io/stdin)!;
        let input = /std/Result/unwrap_or(/std/Try/run(/std/Io/Reader/line(standard_input))!, /std/Option/none());
        match input : (_) => Io({})
        | some(bytes) =>
            match Str/of_bytes(bytes) : (_) => Io({})
            | success(s) =>
                match Nat/of_str(Str/trim(s)) : (_) => Io({})
                | some(d) => /std/print(Nat/to_str(sum(build(d, 1))))
                | none() => /std/print("bad input")
                end
            | failure(_) => /std/print("bad utf8")
            end
        | none() => /std/print("no input")
        end
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output().to_vec(), b"496");
}

/// Narrowed through `try_to_nat` because the subject *is* a NaN on one edge — the fixture unwraps against `+nan.0` deliberately — so `NonNeg` is undischargeable by construction rather than by cost, and the deciding pair is the only correct shape. (`flt_of_str_returns_option` is the other side of that line: its subject is computed but always a number, so it takes the bounded form.)
///
/// An `Option(Flt)` built in a bind's continuation, unwrapped against `+nan.0`, is a shape that would run the Cont fixpoint to its 1024-round backstop: the NaN default rides a switch edge, and were `curios_cont::Literal::Flt` compared under IEEE equality, `forward_continuations` would read that untouched edge as rewritten on every round. The literal compares bitwise, and this program keeps the fixpoint's convergence on a NaN-carrying edge asserted end-to-end rather than only at the pass.
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
            Option/unwrap_or(Flt/try_to_nat(Option/unwrap_or(f(Nat/to_str(taint)), +nan.0)), 0)))
        "#),
        b"3"
    );
}

/// Effect hugging: a deep closed computation beside a host effect collapses to a constant while the effect stays, in order — emergent from ANF plus the effect contract, no dedicated pass.
#[test]
fn arena_pure_computation_hugs_a_host_effect() {
    let source = r#"
        use /std/{Nat, Str, Io};
        let triangle(n : Nat) -> Nat =
            match n : (_) => Nat
            | 0 => 0
            | p + 1; ih => n + ih
            end;
        let before = Io/write(Io/stdout, Str/to_bytes("a"))!;
        let pure = triangle(100);
        let after = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(pure)))!;
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

/// The worker/wrapper gate: a monoid-deferred recursion (`count(t) + 1`) is a loop — the rebase threads the deferred addition into a tail accumulator — so it runs at depth with no frame an element. The depth alone no longer tells it from a recursion left as written, which a program's stack now holds, so the worker is looked for by name in the optimized program.
#[test]
fn arena_deferred_context_recursion_is_stack_safe_at_depth() {
    let program = |depth: u32| {
        format!(
            r#"
        use /std/{{Nat, Str, Bytes, Io}};
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
        let _ = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(count(build({depth}, x[])))))!;
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

    let cont_optm = cont_optm(&program(60_000));
    assert!(
        cont_optm.contains("/count@w("),
        "the deferred recursion must be the rebase's worker, a loop over an accumulator:\n{}",
        &cont_optm[..cont_optm.len().min(4000)]
    );
}

/// A span builds its answer once, where its walk stops: what a step's recursion returns to constructs nothing, so no step rebuilds the answer of the one after it and a character passed keeps no frame. A walk that restates its proof around its recursion erases to that rebuild, a pair a character under a frame a character, and a program's stack holds the depth, so the shape is looked for and not the depth, as the worker is above. The text is read from standard input, or the closed walk would be evaluated where the program is compiled.
#[test]
fn a_span_builds_its_answer_where_its_walk_stops() {
    let module = cont_optm_module(
        r#"
        use /std/{Str, Nat, Char, Option, Result, Io};
        use /std/Str/{At};
        let standard_input = /std/Io/Reader/new(/std/Io/stdin)!;
        let input = /std/Result/unwrap_or(/std/Try/run(/std/Io/Reader/line(standard_input))!, /std/Option/none());
        match input: (_) => Io({})
        | some(bytes) =>
            match Str/of_bytes(bytes): (_) => Io({})
            | success(s) =>
                let run = Str/span(s, At/start(s), (c) => Char/to_nat(c) != 10);
                /std/print(Nat/to_str(run.past.offset))
            | failure(_) => /std/print("invalid utf-8")
            end
        | none() => /std/print("no input")
        end
        "#,
    );

    // The walk recurs by calling what its step is handed, a value the library names `again`: the program reaches no other walk, and the function that holds the step is the optimizer's to name.
    let recursions = module
        .value_ids()
        .filter(|id| {
            module
                .value(*id)
                .is_some_and(|value| value.debug_name.as_deref() == Some("again"))
        })
        .collect::<BTreeSet<_>>();

    let mut calls = 0;
    for node in module.nodes().iter().flatten() {
        let Node::ApplyFun {
            callee: Callee::Closure(callee),
            return_to,
            ..
        } = node
        else {
            continue;
        };
        if !recursions.contains(callee) {
            continue;
        }
        calls += 1;

        // The bindings the call returns to, in a line: a return continuation has none.
        let mut next = module
            .continuation(*return_to)
            .map(|continuation| continuation.body);
        while let Some(id) = next {
            next = match module.node(id).expect("a continuation's body is live") {
                Node::LetValue {
                    value: ValueExpr::Tuple(_) | ValueExpr::Row(..) | ValueExpr::List(_),
                    ..
                } => panic!("a step of the span builds a value after its recursion:\n{module}"),
                Node::LetValue { next, .. } | Node::LetIntrinsic { next, .. } => Some(*next),
                _ => None,
            };
        }
    }
    assert!(
        calls > 0,
        "the walk recurs through what its step is handed:\n{module}"
    );
}

/// A recursion as deep as its data runs: a list built under a cons, which no rebase row makes a loop, and an `Io` walk that binds its tail, each a frame an element. `curios-runtime`'s README, "A module runs on a stack of its own", is the decision; under the engine's default stack each of these stopped near five thousand elements.
#[test]
fn a_recursion_as_deep_as_its_data_runs() {
    assert_eq!(
        run(r#"
        use /std/{Nat, List, Str, Io};
        let numbers = List/range(0, 100000);
        let pairs = List/zip(numbers, List/take(numbers, 100000));
        let walked = List/traverse(numbers, (n: Nat) => Io/pure(n + 1))!;
        let _ = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(List/len(pairs))))!;
        let _ = Io/write(Io/stdout, Str/to_bytes(" "))!;
        let _ = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(List/len(walked))))!;
        /std/Io/pure(())
        "#),
        b"100000 100000".to_vec()
    );
}

/// A recursion with no end still stops, on the engine's own trap, once the stack it was given is spent: the limit is a bound a program meets, never the thread's.
#[test]
fn a_recursion_with_no_end_stops_on_the_stack_it_was_given() {
    let (system, _io) = MockHost::builder().build();
    let refusal = run_text(
        r#"
        use /std/{Nat, List, Str, Io};
        let forever(n : Nat) -> List(Nat) = [n, ..forever(n + 1)];
        let _ = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(List/len(forever(0)))))!;
        /std/Io/pure(())
        "#,
        system,
    )
    .expect_err("a recursion with no end cannot answer");

    assert!(
        refusal.contains("call stack exhausted"),
        "the program must stop on the engine's trap: {refusal}"
    );
}
