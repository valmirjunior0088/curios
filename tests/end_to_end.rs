use {
    curios::{cont, core, ersd, text, wasm},
    std::time::Duration,
    wasmtime::{AnyRef, Config, Engine, Instance, Module, Rooted, Store},
};

#[test]
fn pipeline_lowers_and_runs_core_term() {
    let text_entrypoint = r#"
        let pair_ty : Type = {
            label : '[left, right],
            match label : _ => Type;
            | 'left => Int;
            | 'right => Flt; };
        let pair : pair_ty = ('left, +42);
        let score : (_ : pair_ty) -> Int = p =>
            split p : _ => Int; | (label, value) =>
            match label : _ => Int;
            | 'left => +42;
            | 'right => +7;;
        score pair
        "#
    .parse::<text::Entrypoint>()
    .expect("expected text term");

    let core_term = text::elaborate(&text_entrypoint);

    println!("=== core ===");
    println!("{core_term}");

    let ersd_term = core::erase(
        &mut core::Context::new(Duration::from_secs(5)),
        &core_term,
        &text::elaborate(&"Int".parse().expect("expected result type")),
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

    assert_eq!(result, 42);
}
