//! What the CPS fixpoint costs, pass by pass.
//!
//! Instrumented because the per-pass spans and fired-samples `curios_cps::optimize` carries are recorded only while a trace listens. It reports costs without asserting thresholds, in the shape of `unfolding.rs`'s measurements, and its subject is the census's `TOML_DRIVER`, so the program a figure is taken over is named.

use {
    super::*,
    crate::{phase, read_sample, read_span},
    curios::TALLIES,
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_pipeline::compile_with_prelude,
    curios_surface::Entrypoint,
};

/// Where the fixpoint's time goes, and which passes keep it running: one compilation of `TOML_DRIVER` taken as a phase, its instructions read whole, each span the compile entry and `curios_cps` carry read as its figures — its time, its calls, what its thread took and the looks its work made — and each sample the CPS optimizer takes. `cargo xbench measure fixpoint_pass_measurements` takes it.
///
/// A pass's boolean sample total counts rounds in which it changes the graph; its observations count invocations. A sweep can rewrite many candidates in one invocation. Its fired count neither counts rewrites nor establishes one-candidate draining or independence: the direct pass tests hold the snapshot and deferral contracts. Samples ending in `::nodes`, `::conts`, `::cap` and `::values` are graph sizes, not fired counts. Every span must close.
///
/// Parsing precedes the phase. The phase wraps `compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |_| {})`, with no package cache or stage rendering. The `compile_entrypoint` span starts after prelude restoration and covers the pipeline through Wasm emission; it excludes Binaryen, Cranelift and guest execution.
///
/// Times include the recorder's own work, and allocations leave it out. Allocations are cumulative work, including nested spans, rather than peak resident memory. Per-pass times and allocations are inclusive as well; nested spans cannot be added to their parent.
///
/// # Its records
///
/// `cargo xbench report fixpoint_pass_measurements` reads them: each figure's readings by group, and what each sitting was taken under.
pub fn fixpoint_pass_measurements() {
    let entrypoint = TOML_DRIVER.parse::<Entrypoint>().expect("driver parses");
    let compiled = phase(TALLIES, || {
        compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |_| {})
    });
    compiled.answer.as_ref().expect("driver compiles");
    assert!(compiled.profile.open.is_empty(), "every span closed");

    compiled.read("compile_with_prelude", None);
    for summary in compiled.profile.summaries.iter().filter(|summary| {
        summary.name == "compile_entrypoint" || summary.target.starts_with("curios_cps")
    }) {
        read_span(&summary.name, summary, None);
    }
    for sample in compiled
        .profile
        .samples
        .iter()
        .filter(|sample| sample.name.starts_with("cps"))
    {
        read_sample(&sample.name, sample, None);
    }
}
