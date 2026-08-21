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
mod reduction;
mod runtime;
mod scheduler;
mod soundness;
mod state;
mod strings;
mod structs;
mod throw;
mod toml;
mod unfolding;
mod universes;
mod wasm_conformance;

use {
    crate::run_wasm,
    curios_pipeline::{DEFAULT_STEP_BUDGET, Stage, compile_with_prelude},
    curios_runtime::{ForeignBindings, HostOps, MockHost},
    curios_text::{Entrypoint, RootSource},
};

/// Compile an already-parsed entrypoint under `loader` and run it.
///
/// Drops any `foreign` declarations' `ForeignStore` — this is the fused compile-and-run path the suites want, with no point to hand it back; a test that has `foreign` declarations to satisfy calls [`compile_with_prelude`] itself, builds [`ForeignBindings`] from the returned store, and calls [`run_wasm`].
fn run_entrypoint<H: HostOps + Send + Sync + 'static>(
    entrypoint: &Entrypoint,
    loader: &RootSource,
    host: H,
) -> Result<(), String> {
    let (module, _foreigns) =
        compile_with_prelude(DEFAULT_STEP_BUDGET, entrypoint, loader, |_| {})?;

    run_wasm(&module, host, ForeignBindings::empty()).map(|_| ())
}

/// Parse `source` (no external modules) and run it.
fn run_text<H: HostOps + Send + Sync + 'static>(source: &str, host: H) -> Result<(), String> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .map_err(|error| error.format())?;

    run_entrypoint(&entrypoint, &RootSource::none(), host)
}

fn run(source: &str) -> Vec<u8> {
    let (system, io) = MockHost::builder().build();
    run_text(source, system).expect("expected result");
    io.output().to_vec()
}

/// Compile-only, for the programs whose point is that they are refused.
fn typecheck(source: &str) -> Result<(), String> {
    typecheck_within(DEFAULT_STEP_BUDGET, source)
}

/// [`typecheck`] under a stated work budget, for a program whose point is that elaboration *evaluates* something.
///
/// The budget is the only bound reduction has, and it counts *priced work*: a transition costs one unit and a construction costs what it builds, so the memory a reduction allocates as it goes is bounded by the same number that bounds how often it moves. A fixture that would otherwise run a program's whole computation at the type level states a small budget and asserts the refusal instead. Sizing it is the fixture's job: large enough that the rest of the program elaborates, small enough that the evaluation under test cannot finish.
fn typecheck_within(budget: u64, source: &str) -> Result<(), String> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");

    compile_with_prelude(budget, &entrypoint, &RootSource::none(), |_| {})
        .map(|_| ())
        .map_err(|error| error.to_string())
}

/// Run expecting failure, returning the diagnostic.
fn error(source: &str) -> String {
    let (system, _io) = MockHost::builder().build();
    match run_text(source, system) {
        Ok(_) => panic!("expected an error, program succeeded"),
        Err(error) => error.to_string(),
    }
}

/// Compile through production and hand back the optimized Ersd arena — the module the door's sequence-usage census judges.
fn ersd_optm(source: &str) -> curios_ersd::Module {
    let entrypoint = source.parse::<Entrypoint>().expect("fixture parses");

    let mut captured = None;
    compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |stage| {
            if let Stage::ErsdOptm(module) = stage {
                captured = Some(module.clone());
            }
        },
    )
    .expect("fixture compiles");

    captured.expect("the pipeline emits the optimized Ersd stage")
}

/// Whether the census settles `family`'s `constructor` field named `field` in `source` — the named assertion surface over the door's verdict, so a fixture pins the settling by name instead of grepping a dump for the op the settling later becomes.
fn census_settles(source: &str, family: &str, constructor: &str, field: &str) -> bool {
    curios_ersd::test_support::census_settles_constructor_field(
        &ersd_optm(source),
        family,
        constructor,
        field,
    )
}

/// Compile through production and capture the optimized Cont printout.
fn cont_optm(source: &str) -> String {
    let entrypoint = source.parse::<Entrypoint>().expect("fixture parses");

    let mut printed = String::new();
    compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |stage| {
            if let Stage::ContOptm(module) = stage {
                printed = module.to_string();
            }
        },
    )
    .expect("fixture compiles");

    printed
}
