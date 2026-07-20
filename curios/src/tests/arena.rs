//! The arena behavior-identity corpus: representative programs run through
//! production (archived-prefix replay) and fresh arena erasure, compared on
//! observable runtime output — never on bytes. One program per behavior class
//! the Ersd specification named; a divergence here is an archive round-trip
//! bug before it is anything else.

use super::{run, run_arena};

/// Assert production (archived-prefix replay) and fresh arena erasure produce
/// the same observable behavior — the probe that catches a bad archive round
/// trip.
fn behavior_matches(source: &str) {
    assert_eq!(run(source), run_arena(source), "replay differs from fresh");
}

#[test]
fn arena_matches_scalar_arithmetic_and_strings() {
    // Scalar operations, string formatting, and the whole `/std/Str` proof
    // machinery (`Utf8` certificates, newtype collapse) behind `to_str`.
    behavior_matches(
        r#"std/Io/write(std/Io/stdout, /std/Str/to_bytes(/std/Nat/to_str(75 * 41 % 65537)))"#,
    );
}

#[test]
fn arena_matches_inductive_matching_and_recursion() {
    // Inductive construction and matching, self-recursion, runtime-tainted
    // input so the kernel is not constant-folded on the legacy path.
    behavior_matches(
        r#"
        use /std/{Io, Nat, Lst, Str, proc};
        induct Tree : Type
        | leaf(Nat)
        | node(Nat, Tree, Tree)
        end
        rec build(d : Nat, v : Nat) -> Tree =
            match d : Tree
            | 0 => Tree/leaf(v)
            | dp + 1; ih => Tree/node(v, build(dp, v * 2), build(dp, v * 2 + 1))
            end;
        rec sum(t : Tree) -> Nat =
            match t : Nat
            | leaf(v) => v
            | node(v, l, r) => v + sum(l) + sum(r)
            end;
        let n : Nat = Lst/len(proc/args());
        Io/write(Io/stdout, Str/to_bytes(Nat/to_str(sum(build(n + 4, 1)))))
        "#,
    );
}

#[test]
fn arena_matches_folds_and_closures() {
    // Sequence folds over strings, closures and higher-order application.
    behavior_matches(
        r#"
        use /std/{Io, Nat, Str};
        let count = Str/fold(@Nat, "abcdef", 0, (c, acc) => acc + 1);
        Io/write(Io/stdout, Str/to_bytes(Nat/to_str(count)))
        "#,
    );
}

#[test]
fn arena_matches_effect_order_and_cells() {
    // Cells and effect ordering: writes interleaved with cell state must
    // appear in exactly the production order.
    behavior_matches(
        r#"
        use /std/{Io, Nat, Str, Cell};
        let c = Cell/new(@Nat, 1);
        let a = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(Cell/get(@Nat, c))));
        let s = Cell/set(@Nat, c, 2);
        let b = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(Cell/get(@Nat, c))));
        ()
        "#,
    );
}

#[test]
fn arena_is_deterministic() {
    let source = r#"std/Io/write(std/Io/stdout, /std/Str/to_bytes("determinism"))"#;
    assert_eq!(run_arena(source), run_arena(source));
}

/// The structural property gate through the arena vertical: the LCG hot
/// kernel — a runtime-tainted self-recursive dispatch — stays one natural
/// loop of direct scalar arithmetic, with no closure allocation, no indirect
/// dispatch, and no irreducible-cycle dispatcher, exactly as the legacy
/// structural suite pins for production. Properties, never bytes.
#[test]
fn arena_lcg_kernel_keeps_its_structural_properties() {
    let source = r#"
        use /std/{Io, Nat, Lst, proc};
        rec loop(k : Nat, x : Nat) -> Nat =
            match k : Nat
            | 0 => x
            | kp + 1; ih => loop(kp, 75 * x % 65537)
            end;
        let n : Nat = Lst/len(proc/args());
        Io/print(Nat/to_str(loop(n, 1)))
        "#;

    let entrypoint = source
        .parse::<curios_text::Entrypoint>()
        .expect("fixture parses");
    let (module, _foreigns) = curios_pipeline::compile_entrypoint_via_arena(
        std::time::Duration::from_secs(60),
        &entrypoint,
        curios_text::RootSource::none(),
        |_| {},
    )
    .expect("fixture compiles through the arena");
    let wat = module.to_string();

    // The innermost natural loop holding the kernel constant (the kernel is
    // contified — into main or a caller — so the loop, not a function
    // boundary, is the structural unit).
    let kernel = innermost_loop(&wat, "65537");
    assert_eq!(
        kernel.matches("loop ").count(),
        1,
        "the kernel must be a single natural loop, not nested loops:\n{kernel}"
    );
    assert!(
        !kernel.contains("$dispatch/"),
        "the backedge must not be a dispatcher selector:\n{kernel}"
    );
    assert!(
        !kernel.contains("call_ref"),
        "the loop body must not dispatch indirectly:\n{kernel}"
    );
}

/// The innermost `loop … end` region containing `needle` — the same slicing
/// the legacy structural suite uses.
fn innermost_loop<'a>(wat: &'a str, needle: &str) -> &'a str {
    let needle_offset = wat.find(needle).expect("the needle is emitted");
    let lines: Vec<(usize, &str)> = {
        let mut offset = 0;
        wat.lines()
            .map(|line| {
                let start = offset;
                offset += line.len() + 1;
                (start, line.trim())
            })
            .collect()
    };
    let is_opener =
        |t: &str| t.starts_with("loop ") || t.starts_with("block ") || t.starts_with("if ");
    let is_closer = |t: &str| t == "end";

    let needle_line = lines
        .iter()
        .rposition(|&(start, _)| start <= needle_offset)
        .expect("the needle has a line");

    let mut depth = 0usize;
    let mut loop_line = None;
    for index in (0..needle_line).rev() {
        let text = lines[index].1;
        if is_closer(text) {
            depth += 1;
        } else if is_opener(text) {
            if depth == 0 {
                if text.starts_with("loop ") {
                    loop_line = Some(index);
                    break;
                }
            } else {
                depth -= 1;
            }
        }
    }
    let loop_line = loop_line.expect("the needle sits inside a loop");

    let mut depth = 0usize;
    let mut end_line = None;
    for (index, &(_, text)) in lines.iter().enumerate().skip(loop_line) {
        if is_opener(text) {
            depth += 1;
        } else if is_closer(text) {
            depth -= 1;
            if depth == 0 {
                end_line = Some(index);
                break;
            }
        }
    }
    let end_line = end_line.expect("the loop is balanced");

    let start = lines[loop_line].0;
    let end = lines.get(end_line + 1).map_or(wat.len(), |&(next, _)| next);
    &wat[start..end]
}

/// The partial-evaluation gate: `Fmt/print("literal")` with constant
/// arguments collapses at the arena level — the emitted Cont carries no
/// `/std/Fmt/` or `/std/Parse/` machinery, just the residual host call chain.
#[test]
fn arena_fmt_print_constant_args_collapses() {
    let source = r#"/std/Fmt/print("hello world")"#;
    behavior_matches(source);

    let entrypoint = source
        .parse::<curios_text::Entrypoint>()
        .expect("fixture parses");
    let mut cont_optm = String::new();
    let (_module, _foreigns) = curios_pipeline::compile_entrypoint_via_arena(
        std::time::Duration::from_secs(60),
        &entrypoint,
        curios_text::RootSource::none(),
        |stage| {
            if let curios_pipeline::Stage::ContOptm(module) = stage {
                cont_optm = module.to_string();
            }
        },
    )
    .expect("fixture compiles through the arena");

    assert!(
        !cont_optm.contains("/std/Fmt/") && !cont_optm.contains("/std/Parse/"),
        "the format machinery must collapse at compile time:\n{}",
        &cont_optm[..cont_optm.len().min(4000)]
    );
}

/// Runtime arguments: the parse of the literal directive spine still runs at
/// compile time (the residual is the specialized first-order chain), and the
/// output matches production.
#[test]
fn arena_fmt_print_runtime_args_specializes_spine() {
    let source = r#"
        use /std/{Fmt, Nat, Lst, proc};
        Fmt/print("count: %s")(Nat/to_str(Lst/len(proc/args())))
        "#;
    behavior_matches(source);

    let entrypoint = source
        .parse::<curios_text::Entrypoint>()
        .expect("fixture parses");
    let mut cont_optm = String::new();
    let (_module, _foreigns) = curios_pipeline::compile_entrypoint_via_arena(
        std::time::Duration::from_secs(60),
        &entrypoint,
        curios_text::RootSource::none(),
        |stage| {
            if let curios_pipeline::Stage::ContOptm(module) = stage {
                cont_optm = module.to_string();
            }
        },
    )
    .expect("fixture compiles through the arena");

    assert!(
        !cont_optm.contains("/std/Fmt/") && !cont_optm.contains("/std/Parse/"),
        "the parse of the literal spine must collapse at compile time:\n{}",
        &cont_optm[..cont_optm.len().min(4000)]
    );
}

/// Effect hugging: a deep closed computation beside a host effect collapses
/// to a constant while the effect stays, in order — emergent from ANF plus
/// the effect contract, no dedicated pass.
#[test]
fn arena_pure_computation_hugs_a_host_effect() {
    let source = r#"
        use /std/{Io, Nat, Str};
        rec triangle(n : Nat) -> Nat =
            match n : Nat
            | 0 => 0
            | p + 1; ih => n + ih
            end;
        let before = Io/write(Io/stdout, Str/to_bytes("a"));
        let pure = triangle(100);
        let after = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(pure)));
        ()
        "#;
    behavior_matches(source);

    let entrypoint = source
        .parse::<curios_text::Entrypoint>()
        .expect("fixture parses");
    let mut cont_optm = String::new();
    let (_module, _foreigns) = curios_pipeline::compile_entrypoint_via_arena(
        std::time::Duration::from_secs(60),
        &entrypoint,
        curios_text::RootSource::none(),
        |stage| {
            if let curios_pipeline::Stage::ContOptm(module) = stage {
                cont_optm = module.to_string();
            }
        },
    )
    .expect("fixture compiles through the arena");

    assert!(
        !cont_optm.contains("triangle"),
        "the closed recursion must collapse to its constant around the effects:\n{}",
        &cont_optm[..cont_optm.len().min(4000)]
    );
}

/// An effectful fold body is never folded or residualized out of order: the
/// per-iteration writes all happen, in order, matching production.
#[test]
fn arena_effectful_fold_body_bails_and_runs_per_iteration() {
    behavior_matches(
        r#"
        use /std/{Io, Nat, Str};
        let out = Str/fold(@Nat, "abc", 0, (c, acc) =>
            let w = Io/write(Io/stdout, Str/to_bytes("x"));
            acc + 1);
        Io/write(Io/stdout, Str/to_bytes(Nat/to_str(out)))
        "#,
    );
}

/// The worker/wrapper gate. A monoid-deferred recursion (`count(t) + 1`)
/// matches production at a depth both paths handle, and — the load-bearing
/// half — runs at a depth where an unrebased recursion overflows the runtime
/// stack. (The legacy path's own worker/wrapper does not cover
/// entrypoint-local recursion, so the deep half runs through the arena
/// alone: the rebase threads the deferred addition into a tail accumulator.)
#[test]
fn arena_deferred_context_recursion_is_stack_safe_at_depth() {
    let program = |depth: u32| {
        format!(
            r#"
        use /std/{{Io, Nat, Str, Bytes}};
        rec count(b : Bytes) -> Nat =
            match b : Nat
            | x\ => 0
            | x\h\..t; ih => count(t) + 1
            end;
        rec build(n : Nat, acc : Bytes) -> Bytes =
            match n : Bytes
            | 0 => acc
            | p + 1; ih => build(p, Bytes/cons(97, acc))
            end;
        Io/write(Io/stdout, Str/to_bytes(Nat/to_str(count(build({depth}, x\)))))
        "#
        )
    };

    behavior_matches(&program(1_000));
    assert_eq!(
        run_arena(&program(60_000)),
        b"60000".to_vec(),
        "the rebased recursion must not overflow at depth"
    );
}
