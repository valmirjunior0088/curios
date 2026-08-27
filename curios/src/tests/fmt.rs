//! `/std/Fmt`: what a `%` slot shows, and what survives to codegen once the format string is a literal.
//!
//! The rendering half is the witness each slot dispatches through. The specialization half is that a literal format string parses at compile time — the ersd `evaluate` pass folds the parse to a constant spine and `specialize` unrolls the fold over it, so neither the parser nor the generic fold reaches the backend. These were three files before: the slot cases, four probes filed under `runtime`, and two more filed under an `arena` module whose subject was the vertical rather than the formatter.

use {
    super::{cont_optm, run},
    curios_pipeline::{Stage, compile_with_prelude},
    curios_runtime::{ForeignBindings, MockHost},
    curios_text::{Entrypoint, RootSource},
};

/// Whether the printed Ersd still holds any of `/std/Parse`'s *code*. The printer lists every registered schema whether or not anything uses it, so `/std/Parse/Refusal`'s family line is not evidence the parser web survived; a function, value or group named under `/std/Parse/` is.
fn names_parse_code(ersd: &str) -> bool {
    ersd.lines()
        .filter(|line| line.contains("/std/Parse/"))
        .any(|line| !line.starts_with("family ") && !line.starts_with("product "))
}

// A bare `%` is the generic show-slot: each argument renders through its own `Show` witness, so one format string mixes types freely.
#[test]
fn percent_slot_shows_each_argument_through_its_witness() {
    let source = r#"
        use /std/{Fmt};
        Fmt/print("% / % / %")(42)(true)(3.5)
    "#;
    assert_eq!(run(source), b"42 / true / +3.5");
}

// `Show(Str)` is identity, so a `Str` argument renders verbatim.
#[test]
fn percent_slot_shows_a_string_verbatim() {
    let source = r#"
        use /std/{Fmt};
        Fmt/print("hello, %!")("world")
    "#;
    assert_eq!(run(source), b"hello, world!");
}

// `\%` (spelled `\\%` in source) escapes a literal percent, and — because the escape lead character differs from the placeholder — composes unambiguously next to a slot in either order.
#[test]
fn escaped_percent_renders_literally() {
    let slot_then_literal = r#"
        use /std/{Fmt};
        Fmt/print("%\\% off")(50)
    "#;
    assert_eq!(run(slot_then_literal), b"50% off");

    let literal_then_slot = r#"
        use /std/{Fmt};
        Fmt/print("\\%%")(50)
    "#;
    assert_eq!(run(literal_then_slot), b"%50");
}

// The scalar `Show` witnesses (`Byte` as decimal, `Bytes` as lowercase hex, `Order` by constructor) render through the same slot.
#[test]
fn percent_slot_shows_scalar_witnesses() {
    let source = r#"
        use /std/{Fmt, Nat, Str, Ord};
        Fmt/print("byte=% bytes=% ord=%")(Nat/to_byte(65))(Str/to_bytes("ABC"))(Ord/cmp(9, 4))
    "#;
    assert_eq!(run(source), b"byte=65 bytes=414243 ord=gt");
}

// The container `Show` witnesses render structurally, recursing through the element witness resolved from their `use Show(A)` premise.
#[test]
fn percent_slot_shows_containers() {
    let source = r#"
        use /std/{Fmt, Option, List};
        Fmt/print("list=% opt=%")([1, 2, 3])(Option/some(7))
    "#;
    assert_eq!(run(source), b"list=[1, 2, 3] opt=some(7)");
}

#[test]
fn print_partial_evaluation_reduces_residual() {
    // End-to-end residue guard for the staging stack on `Fmt/print("% is %")(name)(30)` with a *runtime* first argument. The ersd `evaluate` pass folds the closed prefix — the format-string parse (Parse combinators and the segment UTF-8 revalidation included) runs at compile time and `Fmt/print(lit)` reifies as the curried hole-filling closure over a constant `Fmt` spine. What stays runtime is exactly the runtime work: specialized `go_with` over the spine, the runtime `Str` slot (`Str/trim` and stdin UTF-8 validation through `classify`, shown by `Show(Str)` identity), and the `Nat` slot (`Show(Nat)` = `Nat/to_str`'s digit producer). The single-entry `go_with` spine is then contified into the entry, so the boundary is pinned by the surviving `Nat/to_str` digit producer together with the absence of the generic `Fmt/print` driver and the compile-time `Parse` combinators, without depending on a legacy backend function-count metric.
    let source = r#"
        use /std/{Str, Handle, Bytes, Fmt};

        match Handle/read(Handle/stdin, 1024)! : (_) => /std/Io({})
        | chunk(bytes) =>
            match Str/of_bytes(bytes) : (_) => /std/Io({})
            | some(s) => Fmt/print("% is %")(Str/trim(s))(30)
            | none() => /std/print("invalid input")
            end
        | eof() => /std/print("invalid input")
        | error(_) => /std/print("invalid input")
        end
        "#;

    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");

    let mut cont_optm = None;

    let (wasm_module, _foreigns) = compile_with_prelude(
        curios_pipeline::DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |stage| {
            if let Stage::ContOptm(module) = stage {
                cont_optm = Some(format!("{module}"));
            }
        },
    )
    .expect("compile succeeded");

    let cont = cont_optm.expect("Stage::ContOptm observed");
    assert!(
        cont.contains("/std/Nat/to_str")
            && !cont.contains("/std/Fmt/print")
            && !cont.contains("/std/Parse/"),
        "expected only the specialized formatting spine after staging, got:\n{cont}",
    );

    let (system, io) = MockHost::builder().stdin_lines(["Alice"]).build();
    crate::run_wasm(&wasm_module, system, ForeignBindings::empty()).expect("execution succeeded");
    assert_eq!(io.output(), b"Alice is 30");
}

#[test]
fn print_runtime_args_specializes_spine() {
    // The mixed case: a literal format string with runtime hole arguments. The ersd `evaluate` pass folds the parse to a constant `Fmt` spine, and `specialize` unrolls `go_with` over it — the ersd-optm module carries the minted spine items and neither the format-string parser nor the generic fold survives to codegen.
    let source = r#"
        use /std/{Str, Handle, Bytes, Fmt};

        match Handle/read(Handle/stdin, 1024)! : (_) => /std/Io({})
        | chunk(bytes) =>
            match Str/of_bytes(bytes) : (_) => /std/Io({})
            | some(s) => Fmt/print("% is %")(Str/trim(s))(30)
            | none() => /std/print("invalid input")
            end
        | eof() => /std/print("invalid input")
        | error(_) => /std/print("invalid input")
        end
        "#;

    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");

    let mut ersd_optm = None;

    let (wasm_module, _foreigns) = compile_with_prelude(
        curios_pipeline::DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |stage| {
            if let Stage::ErsdOptm(module) = stage {
                ersd_optm = Some(format!("{module}"));
            }
        },
    )
    .expect("compile succeeded");

    let ersd = ersd_optm.expect("Stage::ErsdOptm observed");
    assert!(
        ersd.contains("$/std/Fmt/go_with("),
        "expected the spine-specialized fold called, got:\n{ersd}",
    );
    assert!(
        !ersd.contains("parse_fmt") && !names_parse_code(&ersd) && !ersd.contains("rec ~"),
        "expected the parser and the generic fold pruned, got:\n{ersd}",
    );

    let (system, io) = MockHost::builder().stdin_lines(["Bob"]).build();
    crate::run_wasm(&wasm_module, system, ForeignBindings::empty()).expect("execution succeeded");
    assert_eq!(io.output(), b"Bob is 30");
}

#[test]
fn print_err_formats_to_stderr() {
    // Same staging as `Fmt/print`, routed through `/std/print_err`. MockIo captures stdout and stderr concatenated in write order, so the ordering also shows the stderr write really happened between the stdout ones.
    assert_eq!(
        run(r#"
        use /std/{Fmt, Handle};
        let a = /std/print("before;")!;
        let b = Fmt/print_err("%: %;")("code")(3)!;
        /std/print("after")
        "#),
        b"before;code: 3;after"
    );
}

#[test]
fn print_constant_args_collapses_at_ersd() {
    // The fully-constant case: every input to `Fmt/print` is a literal, so the ersd `evaluate` pass runs the *entire* program at compile time and residualizes the one effect boundary — the ersd-optm module's body is a single `#/std/print(<final bytes>)` call, the `Parse` combinator web and `Fmt`'s parser are pruned, and only the `/std/print` → `Handle/write` plumbing reaches codegen.
    let source = r#"
        use /std/{Fmt};

        Fmt/print("x = %, s = %\n")(42)("hello")
        "#;

    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");

    let mut ersd_optm = None;
    let mut cont_optm = None;

    let (wasm_module, _foreigns) = compile_with_prelude(
        curios_pipeline::DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |stage| match stage {
            Stage::ErsdOptm(module) => ersd_optm = Some(format!("{module}")),
            Stage::ContOptm(module) => cont_optm = Some(format!("{module}")),
            _ => {}
        },
    )
    .expect("compile succeeded");

    let ersd = ersd_optm.expect("Stage::ErsdOptm observed");
    // "x = 42, s = hello\n", already formatted, as the operand of the host write itself. The residual used to be a call to `/std/print`; `print` is an `Io`-returning wrapper now and inlines away with the rest, so what survives is the write inside the description thunk it erases to — one step further than before, not one less. Dead spine leftovers linger in the entry block (pruning drops items, not block statements) — the Cont sweep below is where they must be gone.
    assert!(
        ersd.contains("foreign sys/write(io:1, x\"78203d2034322c2073203d2068656c6c6f0a\")"),
        "expected the folded write residual, got:\n{ersd}",
    );
    assert!(
        !names_parse_code(&ersd),
        "expected the parser web pruned, got:\n{ersd}",
    );

    let cont = cont_optm.expect("Stage::ContOptm observed");
    assert!(
        !cont.contains("/std/Fmt/") && !cont.contains("/std/Parse/") && !cont.contains("[lambda]"),
        "expected the constant program to collapse to the write loop, got:\n{cont}",
    );

    let (system, io) = MockHost::builder().build();
    crate::run_wasm(&wasm_module, system, ForeignBindings::empty()).expect("execution succeeded");
    assert_eq!(io.output(), b"x = 42, s = hello\n");
}

/// The partial-evaluation gate: `Fmt/print("literal")` with constant arguments collapses at the arena level — the emitted Cont carries no `/std/Fmt/` or `/std/Parse/` machinery, just the residual host call chain.
#[test]
fn constant_args_leave_no_formatter_in_cont() {
    let source = r#"/std/Fmt/print("hello world")"#;
    assert_eq!(run(source), b"hello world".to_vec());

    let cont_optm = cont_optm(source);
    assert!(
        !cont_optm.contains("/std/Fmt/") && !cont_optm.contains("/std/Parse/"),
        "the format machinery must collapse at compile time:\n{}",
        &cont_optm[..cont_optm.len().min(4000)]
    );
}

/// Runtime arguments: the parse of the literal directive spine still runs at compile time — the residual is the specialized first-order chain.
#[test]
fn runtime_args_leave_no_formatter_in_cont() {
    let source = r#"
        use /std/{Fmt, Nat, List, proc};
        Fmt/print("count: %")(Nat/to_str(List/len(proc/args!)))
        "#;
    assert_eq!(run(source), b"count: 0".to_vec());

    let cont_optm = cont_optm(source);
    assert!(
        !cont_optm.contains("/std/Fmt/") && !cont_optm.contains("/std/Parse/"),
        "the parse of the literal spine must collapse at compile time:\n{}",
        &cont_optm[..cont_optm.len().min(4000)]
    );
}
