use {
    super::optimize,
    crate::{ChannelHost, compile_entrypoint, run_wasm, text, wasm},
    std::time::Duration,
};

#[test]
fn optimizes_to_a_smaller_module_that_behaves_identically() {
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
    let optimized = optimize(bytes.clone());

    assert!(optimized.starts_with(b"\0asm"));
    assert!(
        optimized.len() < bytes.len(),
        "expected the optimized module ({} bytes) to be smaller than the input ({} bytes)",
        optimized.len(),
        bytes.len()
    );

    // `run_wasm` optimizes internally, so this executes the optimized module.
    let (host, receiver) = ChannelHost::out();
    run_wasm(&module, host).expect("run failed");
    let printed = String::from_utf8(receiver.try_iter().flatten().collect()).unwrap();
    assert_eq!(printed, "55");
}
