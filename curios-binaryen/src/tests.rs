use {
    super::optimize,
    curios::{compile_entrypoint, text, wasm},
    std::time::Duration,
};

#[test]
fn optimizes_to_a_smaller_valid_module() {
    // `optimize` is bytes -> bytes; producing a real input still needs the
    // compiler, hence the dev-dep on `curios`. That the optimized module still
    // *behaves* identically is covered by `curios-compiler`'s suite, whose run
    // path optimizes on every execution.
    let source = r#"
        rec sum(n : /std/Nat) -> /std/Nat =
            match n : /std/Nat
            | 0 => 0
            | pred + 1, ih => /std/Nat/add(/std/Nat/succ(pred), ih)
            end;

        /std/Fmt/printf("%d")(sum(10))
    "#;

    let entrypoint = source
        .parse::<text::Entrypoint>()
        .expect("failed to parse source");

    let module = compile_entrypoint(
        Duration::from_secs(60),
        &entrypoint,
        &text::NullLoader,
        |_| {},
    )
    .expect("expected wasm module");

    let bytes = wasm::to_bytes(&module);
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
