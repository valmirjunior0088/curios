use {
    curios::{cont, core, ersd, wasm},
    std::time::Duration,
    wasmtime::{AnyRef, Config, Engine, Instance, Module, Rooted, Store},
};

#[test]
fn elim_computes_triangular_sum() {
    // sum(5) = 0 + 1 + 2 + 3 + 4 = 10
    // succ_case(pred, ih) = ih + pred; zero_case = 0
    let term = "elim 5n with _ => Nat;
        | 0n => 0n;
        | pred ih => Nat.add ih pred;"
        .parse()
        .expect("expected core term");

    let cont_module = ersd::to_cont(
        &core::erase(
            &mut core::Context::new(Duration::from_secs(1)),
            &term,
            &"Nat".parse().expect("expected result type"),
        )
        .expect("expected erased term"),
    );

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
