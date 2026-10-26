//! What survives to codegen once a `/std/Fmt` format string is a literal: the residual after partial evaluation, and the absence of any formatter in the emitted CPS. What a `%` slot shows is the corpus's `/data/fmt`.
//!
//! The rendering half is the witness each slot dispatches through. The specialization half is that a literal format string parses at compile time — the ANF `evaluate` pass folds the parse to a constant spine and `specialize` unrolls the fold over it, so neither the parser nor the generic fold reaches the backend.

use {
    super::{cps_optm, run},
    curios_native::{ForeignBindings, MockHost},
    curios_pipeline::{Stage, compile_with_prelude},
    curios_text::{Entrypoint, RootSource},
};

/// Whether the printed ANF still holds any of `/std/Parse`'s *code*. The printer lists every registered schema whether or not anything uses it, so `/std/Parse/Error`'s product line is not evidence the parser web survived; a function, value or group named under `/std/Parse/` is.
fn names_parse_code(anf: &str) -> bool {
    anf.lines()
        .filter(|line| line.contains("/std/Parse/"))
        .any(|line| !line.starts_with("family ") && !line.starts_with("product "))
}

#[test]
fn print_partial_evaluation_reduces_residual() {
    // End-to-end residue guard for the staging stack on `Fmt/print("% is %")(name)(30)` with a *runtime* first argument. The ANF `evaluate` pass folds the closed prefix — the format-string parse (Parse combinators and the segment UTF-8 revalidation included) runs at compile time and `Fmt/print(lit)` reifies as the curried hole-filling closure over a constant `Fmt` spine. What stays runtime is exactly the runtime work: specialized `go_with` over the spine, the runtime `Str` slot (`Str/trim` and stdin UTF-8 validation through `classify`, shown by `Show(Str)` identity), and the `Nat` slot (`Show(Nat)` = `Nat/to_str`'s digit producer). The single-entry `go_with` spine is then contified into the entry, so the boundary is pinned by the surviving `Nat/to_str` digit producer together with the absence of the generic `Fmt/print` driver and the compile-time `Parse` combinators.
    let source = r#"
        use /std/{Str, Bytes, Fmt, Io, Try, Process};

        let entry() -> /std/Io({}) =
            match Try/run(Io/read(Process/stdin, 1024))! : (_) => /std/Io({})
            | success(bytes(bytes, @_)) =>
                match Str/of_bytes(bytes) : (_) => /std/Io({})
                | success(s) => Fmt/print("% is %")(Str/trim(s))(30)
                | failure(_) => /std/print("invalid input")
                end
            | success(ended()) => /std/print("invalid input")
            | failure(_) => /std/print("invalid input")
            end;
        entry()
        "#;

    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");

    let mut cps_optm = None;

    let (wasm_module, foreigns) = compile_with_prelude(
        curios_pipeline::DEFAULT_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |stage| {
            if let Stage::CpsOptm(module) = stage {
                cps_optm = Some(format!("{module}"));
            }
        },
    )
    .expect("compile succeeded");

    let cps = cps_optm.expect("Stage::CpsOptm observed");
    assert!(
        cps.contains("/std/Nat/to_str")
            && !cps.contains("/std/Fmt/print")
            && !cps.contains("/std/Parse/"),
        "expected only the specialized formatting spine after staging, got:\n{cps}",
    );

    let (system, io) = MockHost::builder().stdin_lines(["Alice"]).build();
    crate::run_wasm(&wasm_module, foreigns, system, ForeignBindings::empty())
        .expect("execution succeeded");
    assert_eq!(io.output(), b"Alice is 30");
}

#[test]
fn print_runtime_args_specializes_spine() {
    // The mixed case: a literal format string with runtime hole arguments. The ANF `evaluate` pass folds the parse to a constant `Fmt` spine, and `specialize` unrolls `go_with` over it — the anf-optm module carries the minted spine items and neither the format-string parser nor the generic fold survives to codegen.
    let source = r#"
        use /std/{Str, Bytes, Fmt, Io, Try, Process};

        let entry() -> /std/Io({}) =
            match Try/run(Io/read(Process/stdin, 1024))! : (_) => /std/Io({})
            | success(bytes(bytes, @_)) =>
                match Str/of_bytes(bytes) : (_) => /std/Io({})
                | success(s) => Fmt/print("% is %")(Str/trim(s))(30)
                | failure(_) => /std/print("invalid input")
                end
            | success(ended()) => /std/print("invalid input")
            | failure(_) => /std/print("invalid input")
            end;
        entry()
        "#;

    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");

    let mut anf_optm = None;

    let (wasm_module, foreigns) = compile_with_prelude(
        curios_pipeline::DEFAULT_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |stage| {
            if let Stage::AnfOptm(module) = stage {
                anf_optm = Some(format!("{module}"));
            }
        },
    )
    .expect("compile succeeded");

    let anf = anf_optm.expect("Stage::AnfOptm observed");
    assert!(
        anf.contains("$/std/Fmt/go_with("),
        "expected the spine-specialized fold called, got:\n{anf}",
    );
    assert!(
        !anf.contains("parse_fmt") && !names_parse_code(&anf) && !anf.contains("rec ~"),
        "expected the parser and the generic fold pruned, got:\n{anf}",
    );

    let (system, io) = MockHost::builder().stdin_lines(["Bob"]).build();
    crate::run_wasm(&wasm_module, foreigns, system, ForeignBindings::empty())
        .expect("execution succeeded");
    assert_eq!(io.output(), b"Bob is 30");
}

#[test]
fn print_err_formats_to_stderr() {
    // Same staging as `Fmt/print`, routed through `/std/print_err`. MockIo captures stdout and stderr concatenated in write order, so the ordering also shows the stderr write really happened between the stdout ones.
    assert_eq!(
        run(r#"
        use /std/{Fmt, Io};
        let entry() -> /std/Io({}) =
            let a = /std/print("before;")!;
            let b = Fmt/print_err("%: %;")("code")(3)!;
            /std/print("after");
        entry()
        "#),
        b"before;code: 3;after"
    );
}

#[test]
fn print_constant_args_collapses_at_anf() {
    // The fully-constant case: every input to `Fmt/print` is a literal, so the ANF `evaluate` pass runs the *entire* program at compile time and residualizes the one effect boundary — the anf-optm module's body is a single `#/std/print(<final bytes>)` call, the `Parse` combinator web and `Fmt`'s parser are pruned, and only the `/std/print` → `Handle/write` plumbing reaches codegen.
    let source = r#"
        use /std/{Fmt};

        Fmt/print("x = %, s = %\n")(42)("hello")
        "#;

    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");

    let mut anf_optm = None;
    let mut cps_optm = None;

    let (wasm_module, foreigns) = compile_with_prelude(
        curios_pipeline::DEFAULT_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |stage| match stage {
            Stage::AnfOptm(module) => anf_optm = Some(format!("{module}")),
            Stage::CpsOptm(module) => cps_optm = Some(format!("{module}")),
            _ => {}
        },
    )
    .expect("compile succeeded");

    let anf = anf_optm.expect("Stage::AnfOptm observed");
    // "x = 42, s = hello\n", already formatted, as the operand of the host write itself, ahead of a first call's lane. `print` is an `Io`-returning wrapper and inlines away with the rest, so what survives is the write inside the description thunk it erases to. Dead spine leftovers linger in the entry block (pruning drops items, not block statements) — the CPS sweep below is where they must be gone.
    assert!(
        anf.contains(
            "ffi//std/internal/Handle/handle_write(io:1, x\"78203d2034322c2073203d2068656c6c6f0a\", "
        ),
        "expected the folded write residual, got:\n{anf}",
    );
    assert!(
        !names_parse_code(&anf),
        "expected the parser web pruned, got:\n{anf}",
    );

    let cps = cps_optm.expect("Stage::CpsOptm observed");
    assert!(
        !cps.contains("/std/Fmt/") && !cps.contains("/std/Parse/") && !cps.contains("[lambda]"),
        "expected the constant program to collapse to the write loop, got:\n{cps}",
    );

    let (system, io) = MockHost::builder().build();
    crate::run_wasm(&wasm_module, foreigns, system, ForeignBindings::empty())
        .expect("execution succeeded");
    assert_eq!(io.output(), b"x = 42, s = hello\n");
}

/// The partial-evaluation gate: `Fmt/print("literal")` with constant arguments collapses at the arena level — the emitted CPS carries no `/std/Fmt/` or `/std/Parse/` machinery, just the residual host call chain.
#[test]
fn constant_args_leave_no_formatter_in_cps() {
    let source = r#"/std/Fmt/print("hello world")"#;
    assert_eq!(run(source), b"hello world".to_vec());

    let cps_optm = cps_optm(source);
    assert!(
        !cps_optm.contains("/std/Fmt/") && !cps_optm.contains("/std/Parse/"),
        "the format machinery must collapse at compile time:\n{}",
        &cps_optm[..cps_optm.len().min(4000)]
    );
}

/// The library's write is a loop that waits and resends, and the description a constant `print` holds of it is copied under the tight cap the closed evaluator keeps for descriptions. Past that cap the copy is declined, the `print` stays a call, and the collapses this module pins fail with it; this states the margin, so an edit to `/std/internal/Handle`'s write that crosses the cap fails here by the cap's name.
#[test]
fn a_constant_prints_descriptions_are_copied_within_the_evaluators_cap() {
    let entrypoint = r#"/std/print("hello world")"#.parse::<Entrypoint>().expect("fixture parses");

    let mut linked = None;
    compile_with_prelude(
        curios_pipeline::DEFAULT_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |stage| {
            if let Stage::Anf(module) = stage {
                linked = Some(module.clone());
            }
        },
    )
    .expect("fixture compiles");
    let copy =
        curios_anf::test_support::heaviest_description_copy(&linked.expect("Stage::ANF observed"))
            .expect("a `print` holds a description");

    assert!(
        copy.charged <= copy.cap,
        "the heaviest description a constant `print` copies weighs {} nodes, past the {} of `curios-anf`'s `DESCRIPTION_COPY_NODE_LIMIT`: the evaluator declines the copy, and no constant `print` folds",
        copy.charged,
        copy.cap,
    );
}

/// A format the evaluator declines, one past its step budget, is left to the CPS optimizer, which specializes the formatter itself: the program compiles, and prints the format.
#[test]
fn a_format_past_the_evaluators_budget_still_prints() {
    let text = "a".repeat(2000);

    assert_eq!(
        run(&format!(r#"/std/Fmt/print("{text}")"#)),
        text.into_bytes()
    );
}

/// Runtime arguments: the parse of the literal directive spine still runs at compile time — the residual is the specialized first-order chain.
#[test]
fn runtime_args_leave_no_formatter_in_cps() {
    let source = r#"
        use /std/{Fmt, Nat, List, Process};
        let entry() -> /std/Io({}) =
            Fmt/print("count: %")(Nat/to_str(List/len(Process/args()!)));
        entry()
        "#;
    assert_eq!(run(source), b"count: 0".to_vec());

    let cps_optm = cps_optm(source);
    assert!(
        !cps_optm.contains("/std/Fmt/") && !cps_optm.contains("/std/Parse/"),
        "the parse of the literal spine must collapse at compile time:\n{}",
        &cps_optm[..cps_optm.len().min(4000)]
    );
}

/// A format string the program reads at run time has a type nothing can reduce, and the refusal spells that type as written — `format_type_with(Str, Fmt/parse(s))` — rather than as the unfolded definition stuck on `s`, which is what the reduced form is.
#[test]
fn a_runtime_format_string_is_refused_by_its_written_type() {
    let refused = super::error(
        r#"
        use /std/{Str, Fmt, Io, Try, Process};

        let entry() -> /std/Io({}) =
            match Try/run(Io/read(Process/stdin, 1024))! : (_) => /std/Io({})
            | success(bytes(bytes, @_)) =>
                match Str/of_bytes(bytes) : (_) => /std/Io({})
                | success(s) => Fmt/print(s)(3)
                | failure(_) => /std/print("invalid input")
                end
            | success(ended()) => /std/print("invalid input")
            | failure(_) => /std/print("invalid input")
            end;
        entry()
        "#,
    );

    assert!(refused.contains("applied a non-function"), "{refused}");
    assert!(
        refused.contains("head has type: Fmt/format_type_with(Io({}), Fmt/parse(s))"),
        "the type as written:\n{refused}"
    );
    assert!(
        refused.contains("which does not reduce to a function type"),
        "{refused}"
    );
    assert!(!refused.contains("rec #"), "no unfolding:\n{refused}");
}
