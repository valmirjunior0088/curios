use {
    curios::{cont, core, ersd, text, wasm},
    std::time::Duration,
    wasmtime::{AnyRef, Config, Engine, Instance, Module, Rooted, Store},
};

#[test]
fn nat_fold_computes_triangular_sum() {
    let text_term = r#"
        Nat.fold 5 : _ => Nat;
        | 0 => 0;
        | pred ih => Nat.add ih pred;
        "#
    .parse::<text::Term>()
    .expect("expected text term");

    println!("=== text ===");
    println!("{text_term}");

    let core_term = text::elaborate(&text_term);

    println!();
    println!("=== core ===");
    println!("{core_term}");

    let ersd_term = core::erase(
        &mut core::Context::new(Duration::from_secs(5)),
        &core_term,
        &text::elaborate(&"Nat".parse().expect("expected result type")),
    )
    .expect("expected erased term");

    println!();
    println!("=== ersd ===");
    println!("{ersd_term}");

    let cont_module = ersd::to_cont(&ersd_term);

    println!();
    println!("=== cont ===");
    println!("{cont_module}");

    let wasm_module = cont::to_wasm(&cont_module);

    println!();
    println!("=== wasm ===");
    println!("{wasm_module}");

    let mut config = Config::new();
    config.wasm_reference_types(true);
    config.wasm_function_references(true);
    config.wasm_gc(true);
    config.wasm_tail_call(true);

    let engine = Engine::new(&config).expect("expected wasmtime engine");

    let module =
        Module::from_binary(&engine, &wasm::to_bytes(&wasm_module)).expect("expected wasm module");

    let mut store = Store::new(&engine, ());

    let instance = Instance::new(&mut store, &module, &[]).expect("expected instance");

    let run = instance
        .get_typed_func::<(), Rooted<AnyRef>>(&mut store, "func/main")
        .expect("expected exported func/main");

    let result = run.call(&mut store, ()).expect("expected call result");

    let result = result
        .unwrap_i31(&store)
        .expect("expected i31 result")
        .get_i32();

    println!();
    println!("=== result ===");
    println!("{result}");

    assert_eq!(result, 10);
}
