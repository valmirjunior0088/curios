use {
    clap::Parser,
    curios::{cont, core, print_ref, wasm},
    std::{fs, path::PathBuf, time::Duration},
    wasmtime::{AnyRef, Config, Engine, Instance, Module, OwnedRooted, Store},
};

fn parse_timeout(input: &str) -> Result<Duration, String> {
    input
        .parse::<u64>()
        .map(Duration::from_millis)
        .map_err(|error| format!("invalid timeout in milliseconds: {error}"))
}

#[derive(Parser)]
struct Cli {
    #[arg(long, default_value = "1000", value_name = "MILLIS", value_parser = parse_timeout)]
    timeout: Duration,

    #[arg()]
    path: PathBuf,
}

fn main() -> Result<(), String> {
    let cli = Cli::parse();

    let source = fs::read_to_string(&cli.path)
        .map_err(|error| format!("failed to read {}: {error}", cli.path.display()))?;

    let term: core::Term = source
        .parse()
        .map_err(|error| format!("failed to parse {}: {error:?}", cli.path.display()))?;

    let inferred = core::infer(&mut core::Context::new(cli.timeout), &term)
        .map_err(|error| format!("failed to infer type: {error:?}"))?;

    let erased = core::erase(&mut core::Context::new(cli.timeout), &term, &inferred)
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
        &wasm::to_bytes(&cont::to_wasm(&core::to_cont(&erased))),
    )
    .map_err(|error| format!("failed to load wasm module: {error}"))?;

    let mut store = Store::new(&engine, ());

    let instance = Instance::new(&mut store, &module, &[])
        .map_err(|error| format!("failed to instantiate module: {error}"))?;

    let function = instance
        .get_typed_func::<(), OwnedRooted<AnyRef>>(&mut store, "func/main")
        .map_err(|error| format!("failed to access func/main: {error}"))?;

    let result = function
        .call(&mut store, ())
        .map_err(|error| format!("execution failed: {error}"))?;

    println!("{}", print_ref(&mut store, &result)?);

    Ok(())
}
