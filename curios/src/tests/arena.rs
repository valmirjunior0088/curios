//! Production-path property gates for the arena vertical:
//! partial-evaluation collapse of the format machinery, pure computation
//! folding around ordered host effects, and the worker/wrapper rebase at
//! stack-breaking depth. Properties, never bytes.

use super::run;

/// Compile through production and capture the optimized Cont printout.
fn cont_optm(source: &str) -> String {
    let entrypoint = source
        .parse::<curios_text::Entrypoint>()
        .expect("fixture parses");
    let mut printed = String::new();
    let (_module, _foreigns) = curios_pipeline::compile_entrypoint(
        curios_pipeline::DEFAULT_STEP_BUDGET,
        &entrypoint,
        curios_text::RootSource::none(),
        |stage| {
            if let curios_pipeline::Stage::ContOptm(module) = stage {
                printed = module.to_string();
            }
        },
    )
    .expect("fixture compiles");
    printed
}

/// The partial-evaluation gate: `Fmt/print("literal")` with constant
/// arguments collapses at the arena level — the emitted Cont carries no
/// `/std/Fmt/` or `/std/Parse/` machinery, just the residual host call chain.
#[test]
fn arena_fmt_print_constant_args_collapses() {
    let source = r#"/std/Fmt/print("hello world")"#;
    assert_eq!(run(source), b"hello world".to_vec());

    let cont_optm = cont_optm(source);
    assert!(
        !cont_optm.contains("/std/Fmt/") && !cont_optm.contains("/std/Parse/"),
        "the format machinery must collapse at compile time:\n{}",
        &cont_optm[..cont_optm.len().min(4000)]
    );
}

/// Runtime arguments: the parse of the literal directive spine still runs at
/// compile time — the residual is the specialized first-order chain.
#[test]
fn arena_fmt_print_runtime_args_specializes_spine() {
    let source = r#"
        use /std/{Fmt, Nat, Lst, proc};
        Fmt/print("count: %")(Nat/to_str(Lst/len(proc/args())))
        "#;
    assert_eq!(run(source), b"count: 0".to_vec());

    let cont_optm = cont_optm(source);
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
        use /std/{Handle, Nat, Str};
        rec triangle(n : Nat) -> Nat =
            match n : (_) => Nat
            | 0 => 0
            | p + 1; ih => n + ih
            end;
        let before = Handle/write(Handle/stdout, Str/to_bytes("a"));
        let pure = triangle(100);
        let after = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(pure)));
        ()
        "#;
    assert_eq!(run(source), b"a10000".to_vec());

    let cont_optm = cont_optm(source);
    assert!(
        !cont_optm.contains("triangle"),
        "the closed recursion must collapse to its constant around the effects:\n{}",
        &cont_optm[..cont_optm.len().min(4000)]
    );
}

/// The worker/wrapper gate: a monoid-deferred recursion (`count(t) + 1`) runs
/// at a depth where an unrebased recursion overflows the runtime stack — the
/// rebase threads the deferred addition into a tail accumulator.
#[test]
fn arena_deferred_context_recursion_is_stack_safe_at_depth() {
    let program = |depth: u32| {
        format!(
            r#"
        use /std/{{Handle, Nat, Str, Bytes}};
        rec count(b : Bytes) -> Nat =
            match b : (_) => Nat
            | x\ => 0
            | x\h\..t; ih => count(t) + 1
            end;
        rec build(n : Nat, acc : Bytes) -> Bytes =
            match n : (_) => Bytes
            | 0 => acc
            | p + 1; ih => build(p, Bytes/cons(97, acc))
            end;
        Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(count(build({depth}, x\)))))
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
