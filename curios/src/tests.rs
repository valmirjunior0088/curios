mod aggregates;
mod big_num;
mod binders;
mod characters;
mod codegen;
mod concepts;
mod effects;
mod erasure;
// The fixpoint probe reads the per-pass spans `curios_cont::optimize` carries only under `curios-profile`'s `enabled` feature, reached through this crate's `profile` feature as `churn` is.
#[cfg(feature = "profile")]
mod fixpoint;
mod fmt;
mod harness;
mod host;
mod inference;
mod laws;
mod map;
mod matching;
mod numeric;
mod operators;
mod packages;
mod perimeter;
mod positivity;
mod recursion;
mod reduction;
mod runtime;
mod scheduler;
mod soundness;
mod strings;
mod structs;
mod toml;
mod unfolding;
mod universes;
mod wasm_conformance;

use {
    crate::{run_wasm, to_cwasm},
    curios_pipeline::{
        DEFAULT_STEP_BUDGET, EntryTail, Stage, compile_tests_with_units, compile_with_prelude,
    },
    curios_runtime::{ForeignBindings, HostOps, MockHost, run_bytes},
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

/// A program compiled once — pipeline, Binaryen, Cranelift — for a fixture that runs it under more than one host. The compile is what a fixture pays for, seconds over the prelude; a run of the `.cwasm` is milliseconds. So a table whose rows the host input selects costs one compile rather than one per row, which is the difference between `numeric.rs` taking a minute and taking eight.
struct Compiled {
    cwasm: Vec<u8>,
}

/// Parse `source` (no external modules) and compile it to its `.cwasm`, without running it.
fn compile(source: &str) -> Result<Compiled, String> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .map_err(|error| error.format())?;
    let (module, _foreigns) = compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |_| {},
    )?;

    Ok(Compiled {
        cwasm: to_cwasm(&module)?,
    })
}

impl Compiled {
    /// Run the compiled program under `host` — the deserialize-and-execute half of [`run_wasm`].
    fn run<H: HostOps + Send + Sync + 'static>(&self, host: H) -> Result<i32, String> {
        run_bytes(&self.cwasm, host, ForeignBindings::empty())
    }
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

/// Compile `source` as its own test program — the synthesized `Test/main` tail over its declared tests in place of the authored one — and run it under a fresh mock host with no arguments, returning what it wrote. Success is asserted: a fixture about a failing run reaches for the pieces itself.
fn run_tests_program(source: &str) -> Vec<u8> {
    let entrypoint = source.parse::<Entrypoint>().expect("fixture parses");
    let (module, _foreigns, _records) = compile_tests_with_units(
        DEFAULT_STEP_BUDGET,
        &[],
        &entrypoint,
        &RootSource::none(),
        None,
        EntryTail::Tests,
        |_| {},
        |_| {},
    )
    .expect("fixture compiles as a test program");

    let (system, io) = MockHost::builder().build();
    run_wasm(&module, system, ForeignBindings::empty()).expect("the test program runs");

    io.output().to_vec()
}

/// [`ersd_optm`]'s sibling on the synthesized-tail path: the optimized Ersd arena of `source` compiled as its own test program.
fn ersd_optm_tests(source: &str) -> curios_ersd::Module {
    let entrypoint = source.parse::<Entrypoint>().expect("fixture parses");

    let mut captured = None;
    compile_tests_with_units(
        DEFAULT_STEP_BUDGET,
        &[],
        &entrypoint,
        &RootSource::none(),
        None,
        EntryTail::Tests,
        |stage| {
            if let Stage::ErsdOptm(module) = stage {
                captured = Some(module.clone());
            }
        },
        |_| {},
    )
    .expect("fixture compiles as a test program");

    captured.expect("the pipeline emits the optimized Ersd stage")
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
