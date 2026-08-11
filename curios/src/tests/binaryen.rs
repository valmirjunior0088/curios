use {
    curios_binaryen::optimize,
    curios_pipeline::DEFAULT_STEP_BUDGET,
    curios_pipeline::compile_with_prelude,
    curios_text::{Entrypoint, RootSource},
    curios_wasm::to_bytes,
};

#[test]
fn optimizes_to_a_smaller_valid_module() {
    // `curios_binaryen::optimize` is bytes -> bytes; producing a real input still needs the compiler, hence this test living alongside the pipeline rather than in `curios-binaryen` itself. That the optimized module still *behaves* identically is covered by the rest of this suite, whose run path optimizes on every execution.
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
        compile_with_prelude(DEFAULT_STEP_BUDGET, &entrypoint, RootSource::none(), |_| {})
            .expect("expected wasm module");

    let bytes = to_bytes(&module);
    // `optimize` validates the result internally (asserting on an invalid module).
    let optimized = optimize(bytes.clone(), false);

    assert!(optimized.starts_with(b"\0asm"));
    assert!(
        optimized.len() < bytes.len(),
        "expected the optimized module ({} bytes) to be smaller than the input ({} bytes)",
        optimized.len(),
        bytes.len()
    );

    // The `names` flag is the difference between a profile that reads `$func/<N>$hint` and one that reads bare addresses, and it is off for shipped binaries — so what pins it is that asking for names produces a *larger* module than not asking. Binaryen drops the section by default, which is what made every runtime profile of a Curios program unreadable until this was threaded through.
    let named = optimize(bytes, true);

    assert!(named.starts_with(b"\0asm"));
    assert!(
        named.len() > optimized.len(),
        "expected the named module ({} bytes) to retain the name section the unnamed one ({} bytes) drops",
        named.len(),
        optimized.len()
    );
}
