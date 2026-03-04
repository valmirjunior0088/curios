use {
    curios::{cont, core, wasm},
    std::time::Duration,
    wasmtime::{AnyRef, Config, Engine, Instance, Module, OwnedRooted, Store},
};

fn main() {
    let term = "
        let pair_ty : Type =
          (tag : {:left, :right},
            match tag with k => Type;
            case :left => Int;
            case :right => Flt;);
        let pair : pair_ty = (:left, 42);
        let score : (p : pair_ty) -> Int = p =>
            let (tag, payload) with q => Int = p;
            match tag with k => Int;
            case :left => 42;
            case :right => 7;;
        score pair
        "
    .parse()
    .expect("expected core term");

    let wasm_module = cont::to_wasm(&core::to_cont(
        &core::erase(
            &mut core::Context::new(Duration::from_secs(1)),
            &term,
            &"Int".parse().expect("expected result type"),
        )
        .expect("expected erased term"),
    ));

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
        .get_typed_func::<(), OwnedRooted<AnyRef>>(&mut store, "func/main")
        .expect("expected exported func/main");

    let result = run.call(&mut store, ()).expect("expected call result");

    let result = result
        .unwrap_i31(&store)
        .expect("expected i31 result")
        .get_i32();

    assert_eq!(result, 42);
}
