mod aggregates;
mod arena;
mod big_int;
mod big_nat;
mod binaryen;
mod binders;
mod codegen;
mod concepts;
mod erasure;
mod fmt;
mod foreign;
mod inference;
mod io;
mod io_monad;
mod lift;
mod map;
mod matching;
mod mutation;
mod network;
mod numeric;
mod operators;
mod packages;
mod perimeter;
mod plicity;
mod positivity;
mod runtime;
mod scheduler;
mod soundness;
mod state;
mod strings;
mod structs;
mod throw;
mod toml;
mod universes;

use {
    crate::compile_with_prelude,
    curios_pipeline::Stage,
    curios_runtime::MockHost,
    curios_text::{Entrypoint, RootSource},
};

fn run(source: &str) -> Vec<u8> {
    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    io.output().to_vec()
}

/// Compile-only, for the programs whose point is that they are refused.
fn typecheck(source: &str) -> Result<(), String> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");

    compile_with_prelude(
        crate::DEFAULT_STEP_BUDGET,
        &entrypoint,
        RootSource::none(),
        |_| {},
    )
    .map(|_| ())
    .map_err(|error| error.to_string())
}

/// Run expecting failure, returning the diagnostic.
fn error(source: &str) -> String {
    let (system, _io) = MockHost::builder().build();
    match crate::run_text(source, system) {
        Ok(_) => panic!("expected an error, program succeeded"),
        Err(error) => error.to_string(),
    }
}

/// Compile through production and capture the optimized Cont printout.
fn cont_optm(source: &str) -> String {
    let entrypoint = source.parse::<Entrypoint>().expect("fixture parses");

    let mut printed = String::new();
    compile_with_prelude(
        crate::DEFAULT_STEP_BUDGET,
        &entrypoint,
        RootSource::none(),
        |stage| {
            if let Stage::ContOptm(module) = stage {
                printed = module.to_string();
            }
        },
    )
    .expect("fixture compiles");

    printed
}
