use {
    crate::DEFAULT_STEP_BUDGET,
    curios_binaryen::optimize,
    curios_pipeline::compile_entrypoint,
    curios_text::{Entrypoint, RootSource},
    curios_wasm::to_bytes,
};

#[test]
fn optimizes_to_a_smaller_valid_module() {
    // `curios_binaryen::optimize` is bytes -> bytes; producing a real input
    // still needs the compiler, hence this test living alongside the pipeline
    // rather than in `curios-binaryen` itself. That the optimized module still
    // *behaves* identically is covered by the rest of this suite, whose run
    // path optimizes on every execution.
    let source = r#"
        rec sum(n : /std/Nat) -> /std/Nat =
            match n : (_) => /std/Nat
            | 0 => 0
            | pred + 1; ih => /std/Nat/add(/std/Nat/succ(pred), ih)
            end;

        /std/Fmt/print("%")(sum(10))
    "#;

    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");

    let (module, _foreigns) =
        compile_entrypoint(DEFAULT_STEP_BUDGET, &entrypoint, RootSource::none(), |_| {})
            .expect("expected wasm module");

    let bytes = to_bytes(&module);
    // `optimize` validates the result internally (asserting on an invalid module).
    let optimized = optimize(bytes.clone());

    assert!(optimized.starts_with(b"\0asm"));
    assert!(
        optimized.len() < bytes.len(),
        "expected the optimized module ({} bytes) to be smaller than the input ({} bytes)",
        optimized.len(),
        bytes.len()
    );
}
