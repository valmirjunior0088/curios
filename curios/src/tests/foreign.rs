use {
    curios_rt::{ForeignBindings, MockHost},
    std::time::Duration,
};

#[test]
fn foreign_declaration_runs_through_supplied_bindings() {
    // `double` has no host meaning at all — it's purely an embedder-supplied
    // function, wired up via the store `compile_entrypoint` hands back and
    // exercised end to end through `run_wasm`, not the compiler's own `sys`
    // implementations.
    let source = r#"
        foreign double : (Nat) -> Nat;
        let _ : std/False = /std/Proc/exit(double(21));
        std/Io/write(std/Io/stdout, /std/Str/to_bin("unreachable"))
        "#
    .parse::<crate::text::Entrypoint>()
    .expect("failed to parse source");

    let (module, foreigns) = crate::compile_entrypoint(
        Duration::from_secs(10),
        &source,
        crate::text::RootSource::None,
        |_| {},
    )
    .expect("compile succeeded");

    let mut bindings = ForeignBindings::new(foreigns);
    bindings.define("double", |x: u32| x * 2);

    let (system, io) = MockHost::builder().build();
    let code = crate::run_wasm(&module, system, bindings).expect("execution succeeded");

    assert_eq!(code, 42);
    assert!(io.output().is_empty());
}
