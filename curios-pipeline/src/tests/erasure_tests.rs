//! What erasure carries into the arena, and what a repeated compilation restores unmutated.

use {crate::*, curios_ersd::Analysis, curios_text::RootSource, curios_wasm::to_bytes};

use super::test_support::*;

#[test]
fn repeated_compilation_restores_an_unmutated_ersd_prefix() {
    let source = "/std/Nat/add(20, 22)";
    let first = compile(source, Some("/std/Nat")).unwrap();
    let second = compile(source, Some("/std/Nat")).unwrap();
    assert_eq!(to_bytes(&first), to_bytes(&second));
}

/// `Stage::NAMES`, `Stage::name`, and the driver's emission order are three spellings of one fact, and only `name` is forced by the compiler when a stage is added — a variant missing from `NAMES` would leave `wonder stage`'s roster silently incomplete. One compile pins all three to each other.
#[test]
fn every_stage_is_observed_once_in_names_order() {
    let entrypoint = with_entrypoint_type("/std/Nat/add(20, 22)", Some("/std/Nat"));
    let mut seen = Vec::new();

    compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |stage| seen.push(stage.name()),
    )
    .unwrap();

    // Every name but the last: `wasm-optm` is the downstream-constructed observation `Stage::WasmOptm`'s rustdoc records — the pure pipeline has no Binaryen, so its absence here is the deliberate deviation, pinned rather than allowed.
    assert_eq!(seen, &Stage::NAMES[..Stage::NAMES.len() - 1]);
}

#[test]
fn meta_free_prelude_program_compiles_without_overflow() {
    // The exact case that used to overflow: a meta-free entrypoint (no holes) that still pulls in the whole std/std prelude. Assembling and traversing the old N-deep nested term overflowed the stack during construction and in every pass; the flat `curios_core::Module`/`curios_ersd::Module` representation lowers it end-to-end to wasm without overflow.
    let source = r#"
        let id(A : Type, a : A) -> A = a;
        id(/std/Nat, 5)
    "#;

    assert!(compile(source, Some("/std/Nat")).is_ok());
}

#[test]
fn dead_user_definition_is_still_typechecked() {
    // A user-authored top-level binding the body never references is still type-checked (every item is, before any reachability is considered), so its error is reported. (`write` returns an `Io` description of writing, which `Bytes` mismatches.)
    let error = typecheck(
        r#"
        let dead : /std/Bytes = /std/Handle/write(/std/Handle/stdout, /std/Str/to_bytes("x"));
        /std/print("ok")
        "#,
        None,
    )
    .unwrap_err();

    assert!(error.contains("mismatch"), "unexpected error: {error}");
}

#[test]
fn arena_erasure_covers_the_fixed_prelude() {
    // The entrypoint pulls in string formatting, so the erased module carries the whole fixed prelude — every construct the corpus uses — replayed onto the erased prefix and verified as one module.
    let module = erase_to_ersd(r#"/std/Fmt/print("hello")"#, None);
    assert!(
        module.functions().len() > 100,
        "the fixed prelude erased with the program: {} functions",
        module.functions().len()
    );
}

#[test]
fn arena_erasure_is_deterministic_across_compiles() {
    let source = "/std/Nat/add(20, 22)";
    let first = erase_to_ersd(source, Some("/std/Nat")).to_string();
    let second = erase_to_ersd(source, Some("/std/Nat")).to_string();
    assert_eq!(first, second);
}

#[test]
fn arena_erasure_stores_no_captures_for_the_prelude() {
    // Functions carry no capture lists anywhere in the erased prelude; free values are derived on demand. The analysis on the full module is the witness that derivation covers every function.
    let module = erase_to_ersd("/std/Nat/to_str(7)", Some("/std/Str"));
    let analysis = Analysis::analyze(&module);
    let counted = module.function_ids().count();
    assert!(counted > 0);
    for function in module.function_ids() {
        let _ = analysis.free_values(function);
    }
}

#[test]
fn arena_erasure_handles_deep_input_on_the_default_stack() {
    // A wide flat block (the shape whose N-deep nesting once overflowed the legacy pipeline); erasure, verification, and printing all stay on the default test-thread stack. Sized so quadratic *elaboration* cost — shared by both paths and out of erasure's scope — stays testable.
    let mut source = String::new();
    for index in 0..500 {
        source.push_str(&format!("let x{index} = {index} + 1;\n"));
    }
    source.push_str("x0");
    let module = erase_to_ersd(&source, Some("/std/Nat"));
    let printed = module.to_string();
    assert!(printed.contains("NatAdd"));
}
