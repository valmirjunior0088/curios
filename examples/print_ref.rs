use {
    curios::{cont, core, print_ref, wasm},
    std::time::Duration,
    wasmtime::{AnyRef, Config, Engine, Instance, Module, OwnedRooted, Store},
};

fn main() {
    let source = "
        let { id : (x : Type) -> Type = x => x };
        let witness : Type = id Int;
        let pair_ty : Type =
          (tag : {:left, :right},
            match tag with k => Type;
            case :left => Int;
            case :right => Flt;);
        let payload : pair_ty = (:left, Int.mul 20 2);
        let decoded : Int =
          let (tag, value) with q => Int = payload;
          match tag with k => Int;
          case :left => Int.add 40 2;
          case :right => 7;;
        let make : (x : Int) -> (n : witness, Flt) = x => (x, Flt.add 0.25 0.5);
        make decoded
        ";

    let term = source.parse().expect("expected core term");

    let inferred =
        core::infer(&mut core::Context::new(Duration::from_secs(1)), &term).expect("expected type");

    let erased = core::erase(
        &mut core::Context::new(Duration::from_secs(1)),
        &term,
        &inferred,
    )
    .expect("expected erased term");

    let wasm_module = cont::to_wasm(&core::to_cont(&erased));

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

    println!(
        "{}",
        print_ref(&mut store, &result).expect("expected printable anyref")
    );
}
