//! What erasure carries into the arena, and what a repeated compilation restores unmutated.

use {
    super::test_support::*,
    crate::*,
    curios_ersd::{Analysis, Rhs, Statement, test_support::shape},
    curios_text::RootSource,
    curios_wasm::to_bytes,
};

#[test]
fn repeated_compilation_restores_an_unmutated_ersd_prefix() {
    let source = "let subject : /std/Nat = /std/Nat/add(20, 22);
/std/Io/pure(())";
    let first = compile(source).unwrap();
    let second = compile(source).unwrap();
    assert_eq!(to_bytes(&first), to_bytes(&second));
}

/// `Stage::NAMES`, `Stage::name`, and the driver's emission order are three spellings of one fact, and only `name` is forced by the compiler when a stage is added — a variant missing from `NAMES` would leave `wonder stage`'s roster silently incomplete. One compile pins all three to each other.
#[test]
fn every_stage_is_observed_once_in_names_order() {
    let entrypoint = entrypoint_of(
        "let subject : /std/Nat = /std/Nat/add(20, 22);
/std/Io/pure(())",
    );
    let mut seen = Vec::new();

    compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |stage| seen.push(stage.name()),
    )
    .unwrap();

    // Every name but the last: `wasm-optm` is the downstream-emitted observation `Stage::WasmOptm`'s rustdoc records — the pure pipeline has no Binaryen, so its absence here is the deliberate deviation, pinned rather than allowed.
    assert_eq!(seen, &Stage::NAMES[..Stage::NAMES.len() - 1]);
}

#[test]
fn meta_free_prelude_program_compiles_without_overflow() {
    // A meta-free entrypoint (no holes) that still pulls in the whole prelude: an N-deep nested term would overflow the stack during construction and in every pass, and the flat `curios_core::Module`/`curios_ersd::Module` representation lowers it end-to-end to wasm without overflow.
    let source = r#"
        let id(A : Type, a : A) -> A = a;
        let subject : /std/Nat = id(/std/Nat, 5);
        /std/Io/pure(())
    "#;

    assert!(compile(source).is_ok());
}

#[test]
fn dead_user_definition_is_still_typechecked() {
    // A user-authored top-level binding the body never references is still type-checked (every item is, before any reachability is considered), so its error is reported. (`write` returns an `Io` description of writing, which `Bytes` mismatches.)
    let error = typecheck(
        r#"
        let dead : /std/Bytes = /std/Io/write(/std/Io/stdout, /std/Str/to_bytes("x"));
        /std/print("ok")
        "#,
    )
    .unwrap_err();

    assert!(error.contains("mismatch"), "unexpected error: {error}");
}

#[test]
fn arena_erasure_covers_the_fixed_prelude() {
    // The entrypoint pulls in string formatting, so the erased module carries the whole fixed prelude — every construct the corpus uses — replayed onto the erased prefix and verified as one module.
    let module = erase_to_ersd(r#"/std/Fmt/print("hello")"#);
    assert!(
        module.functions().len() > 100,
        "the fixed prelude erased with the program: {} functions",
        module.functions().len()
    );
}

#[test]
fn arena_erasure_is_deterministic_across_compiles() {
    let source = "let subject : /std/Nat = /std/Nat/add(20, 22);
/std/Io/pure(())";
    let first = shape(&erase_to_ersd(source));
    let second = shape(&erase_to_ersd(source));
    assert_eq!(first, second);
}

#[test]
fn arena_erasure_stores_no_captures_for_the_prelude() {
    // Functions carry no capture lists anywhere in the erased prelude; free values are derived on demand. The analysis on the full module is the witness that derivation covers every function.
    let module = erase_to_ersd(
        "let subject : /std/Str = /std/Nat/to_str(7);
/std/Io/pure(())",
    );
    let analysis = Analysis::analyze(&module);
    let counted = module.function_ids().count();
    assert!(counted > 0);
    for function in module.function_ids() {
        let _ = analysis.free_values(function);
    }
}

#[test]
fn arena_erasure_handles_deep_input_on_the_default_stack() {
    // A wide flat block (the shape an N-deep nesting would overflow on); erasure, verification, and printing all stay on the default test-thread stack. Sized so quadratic *elaboration* cost — shared by both paths and out of erasure's scope — stays testable.
    const BINDINGS: usize = 500;

    let mut source = String::new();
    for index in 0..BINDINGS {
        source.push_str(&format!("let x{index} = {index} + 1;\n"));
    }
    source.push_str("/std/Io/pure(())");
    let module = erase_to_ersd(&source);

    // Printing is half of what must survive the depth, so the deep module is still rendered.
    let _ = module.to_string();

    // What the fixture erased to is asked of the entry block, not of the module: the module carries the whole prelude, so a module-wide scan for an addition answers about `/sys/Nat/succ` and would pass with the fixture erased to nothing. Each binding is still an `Apply` of `/sys/Nat/add` at this rung — inlining is a later stage's — and the statements of the closing `Io/pure(())` follow them.
    let entry = module
        .block(module.entry().expect("the program has an entry"))
        .expect("the entry block is live");
    assert!(entry.statements.len() >= BINDINGS);
    assert!(
        entry
            .statements
            .iter()
            .take(BINDINGS)
            .all(|&statement| matches!(
                module.statement(statement),
                Some(Statement::Let {
                    rhs: Rhs::Apply { .. },
                    ..
                })
            ))
    );
}

/// A concept method's wrapper is a shim, and a call to one erases to the projection call it forwards to: the dictionary's method, applied. Left as a call to the wrapper, a top-level one calls a closure the effect summary cannot see, so pruning keeps it and everything it reaches — `curios`'s `a_trivial_program_retains_none_of_the_parser_web` is the program that measures the cost.
#[test]
fn a_method_call_erases_to_the_projection_it_forwards_to() {
    let source = r#"
        use /std/{Nat};
        pub concept Twice(A: Type): pub Type {
            twice(A) -> A,
        }
        satisfy Twice(Nat) {
            twice(n) = n + n,
        }
        let subject : /std/Nat = Twice/twice(21);
        /std/Io/pure(())
    "#;

    let (ersd, _) = compile_printed_stages(source).unwrap();

    // The wrapper's own definition is still there, for pruning to drop; what must be gone is every call to it.
    let calls = ersd
        .lines()
        .filter(|line| line.contains("$/Twice/twice(") && !line.trim_start().starts_with("let ~f"))
        .collect::<Vec<_>>();
    assert!(calls.is_empty(), "the wrapper is still called: {calls:#?}");
}
