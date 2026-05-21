mod macros;

mod monads;
pub use monads::*;

mod print;

pub mod text;

pub mod core;

pub mod ersd;

pub mod cont;

pub mod wasm;

use {
    print::{RefIds, print_ref},
    std::time::Duration,
    wasmtime::{Config, Engine, Instance, Module, Store},
};

pub fn run(timeout: Duration, source: &str) -> Result<String, String> {
    let term = text::to_core(
        &source
            .parse()
            .map_err(|error| format!("failed to parse source: {error:?}"))?,
    );

    let type_ = core::infer(&mut core::Context::new(timeout), &term)
        .map_err(|error| format!("failed to infer type: {error:?}"))?;

    let term = core::erase(&mut core::Context::new(timeout), &term, &type_)
        .map_err(|error| format!("failed to erase term: {error:?}"))?;

    let mut config = Config::new();
    config.wasm_reference_types(true);
    config.wasm_function_references(true);
    config.wasm_gc(true);
    config.wasm_tail_call(true);

    let engine =
        Engine::new(&config).map_err(|error| format!("failed to create engine: {error}"))?;

    let module = Module::from_binary(
        &engine,
        &wasm::to_bytes(&cont::to_wasm(&ersd::to_cont(&term))),
    )
    .map_err(|error| format!("failed to load wasm module: {error}"))?;

    let mut store = Store::new(&engine, ());

    let instance = Instance::new(&mut store, &module, &[])
        .map_err(|error| format!("failed to instantiate module: {error}"))?;

    let function = instance
        .get_typed_func(&mut store, "func/main")
        .map_err(|error| format!("failed to access func/main: {error}"))?;

    let result = function
        .call(&mut store, ())
        .map_err(|error| format!("execution failed: {error}"))?;

    Ok(print_ref(&mut RefIds::new(), &mut store, &result)
        .display()
        .to_string())
}
