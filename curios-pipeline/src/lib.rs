//! The compile driver: the one crate that strings the pipeline stages together, from a parsed `curios_text::Entrypoint` to a `curios_wasm::Module`. [`compile_entrypoint`] runs the full `to_core → elaborate → zonk → erase → ersd optm → to_cont → cont optm → to_wasm` sequence. Each stage is passed to the caller's observer as a borrowed [`Stage`], which is how `--print` dumps IRs without the driver retaining them.
//!
//! The fixed `sys`/`syn`/`std` prelude is elaborated once per thread and cached; every compile replays it into a fresh context and type-checks only the user code on top. Everything wasm-native — Binaryen, Cranelift precompilation, execution — lives downstream in `curios`/`curios-rt`: this crate stops at the wasm module plus the program's harvested `ForeignStore`.

use {
    curios_abi::{ForeignStore, sys_io},
    std::{fmt, time::Duration},
};

/// A borrowed view of one intermediate representation, handed to the caller's `observe` callback the moment that stage is produced. This is the pipeline's only introspection surface — the CLI's `--print` stage dumps and the test suites' IR assertions both hang off it — and borrowing keeps the driver from retaining any stage it has already lowered past.
pub enum Stage<'a> {
    Text(&'a curios_text::Entrypoint),
    Core(&'a curios_core::Module),
    Ersd(&'a curios_ersd::Module),
    ErsdOptm(&'a curios_ersd::Module),
    Cont(&'a curios_cont::Module),
    ContOptm(&'a curios_cont::Module),
    Wasm(&'a curios_wasm::Module),
}

impl<'a> Stage<'a> {
    /// Every stage name, in pipeline order — the single source the CLI's
    /// `--print` default/help text is derived from, so it cannot drift from
    /// [`Stage::name`].
    pub const NAMES: [&'static str; 7] = [
        "text",
        "core",
        "ersd",
        "ersd-optm",
        "cont",
        "cont-optm",
        "wasm",
    ];

    /// This stage's name, matching its entry in [`Stage::NAMES`].
    pub fn name(&self) -> &'static str {
        match self {
            Stage::Text(_) => "text",
            Stage::Core(_) => "core",
            Stage::Ersd(_) => "ersd",
            Stage::ErsdOptm(_) => "ersd-optm",
            Stage::Cont(_) => "cont",
            Stage::ContOptm(_) => "cont-optm",
            Stage::Wasm(_) => "wasm",
        }
    }
}

impl fmt::Display for Stage<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stage::Text(entrypoint) => write!(f, "{entrypoint}"),
            Stage::Core(module) => write!(f, "{module}"),
            Stage::Ersd(module) => write!(f, "{module}"),
            Stage::ErsdOptm(module) => write!(f, "{module}"),
            Stage::Cont(module) => write!(f, "{module}"),
            Stage::ContOptm(module) => write!(f, "{module}"),
            Stage::Wasm(module) => write!(f, "{module}"),
        }
    }
}

thread_local! {
    // The fixed `sys`/`syn`/`std` prelude, elaborated and zonked once per thread
    // and reused for every compile. Since the reachability prune no longer runs
    // in `curios_text::to_core`, every program lowers the *same* prelude prefix, so its
    // (expensive) type-checking is program-independent and cacheable — the whole
    // point of removing the prune. `Term` is not `Sync`, so this is thread-local
    // (like the loader's parse caches), built once per `cargo test` worker thread
    // rather than once per test. A malformed prelude is a compiler invariant, so
    // a failure here is a `panic!`.
    static PRELUDE: curios_core::Module = build_prelude();
}

/// Elaborate the bare `sys`/`syn`/`std` prelude (no user code) once, to seed
/// [`PRELUDE`]. Lowers a trivial entrypoint under the standard prelude loader —
/// the embedded prelude is identical regardless of any inner loader — then
/// elaborates and zonks it. The result is an ordinary (meta-free) `curios_core::Module`
/// whose `items` are the whole prelude; its trivial `body`/`type_` go unused.
/// A generous timeout: this is a one-time, per-thread cost amortized across
/// every compile.
fn build_prelude() -> curios_core::Module {
    let entrypoint = "0"
        .parse::<curios_text::Entrypoint>()
        .expect("the trivial prelude entrypoint parses");

    // The trivial entrypoint declares no `foreign` items, so the harvested
    // store is always empty here — this is the fixed `sys`/`syn`/`std`
    // prelude, cached independently of any user compilation.
    let (module, metavars, _foreigns) = curios_text::to_core(
        &entrypoint,
        &curios_text::prelude(&sys_io(), curios_text::RootSource::none()),
    )
    .unwrap_or_else(|error| panic!("the embedded prelude failed to lower: {}", error.format()));

    let mut context = curios_core::Context::new(Duration::from_secs(300));

    let (module, _) =
        curios_core::elaborate_module(&mut context, &module, metavars, curios_core::Mode::Infer)
            .unwrap_or_else(|error| {
                panic!(
                    "the embedded prelude failed to elaborate: {}",
                    error.format_with(&module)
                )
            });

    curios_core::zonk_module(&context, &module).unwrap_or_else(|error| {
        panic!(
            "the embedded prelude failed to zonk: {}",
            error.format_with(&module)
        )
    })
}

/// The type-checking prologue of [`compile_entrypoint`] (and the tests'
/// typecheck-only path): lower to core, elaborate (checking against the
/// entrypoint's type when it carries one, else synthesizing), then zonk
/// metavariable solutions in so the module is meta-free — the `elaborate → zonk`
/// half of the `elaborate → zonk → erase` data flow (§9). Elaboration is
/// authoritative: it returns a rebuilt module (lambda domains solved, binders
/// re-closed), and it is *that* module — not the lowered one — that zonk makes
/// meta-free. `zonk` is also where an unsolved hole is rejected, so a program that
/// merely *type-checks* is fully validated by the time this returns. Elaboration
/// and zonking share one context (the solutions live in its `MetaStore`); the
/// returned module is self-contained, so the caller's `erase` runs over a fresh one.
///
/// The `sys`/`syn`/`std` prelude is not re-elaborated per call: the cached
/// [`PRELUDE`] is replayed into the fresh context and only the user code is
/// type-checked on top (see [`curios_core::elaborate_and_zonk_with_prelude`]).
fn elaborate_and_zonk<O>(
    timeout: Duration,
    entrypoint: &curios_text::Entrypoint,
    loader: curios_text::RootSource,
    observe: &mut O,
) -> Result<(curios_core::Module, curios_core::Term, ForeignStore), String>
where
    O: FnMut(Stage<'_>),
{
    observe(Stage::Text(entrypoint));

    // The built-in `/sys/Io` store: the prelude mints the `/sys/Io`
    // declarations from it, and the rows ride the IR nodes from there, so no
    // later stage needs *this* store back. `to_core` separately harvests any
    // user-declared `foreign` items into its own store as it walks the
    // program's module graph, returned below — the two are never merged into
    // one (see `curios-abi::host`'s store-per-provenance note).
    let sys_foreigns = sys_io();

    let (lowered, metavars, user_foreigns) =
        curios_text::to_core(entrypoint, &curios_text::prelude(&sys_foreigns, loader))
            .map_err(|error| error.format())?;

    observe(Stage::Core(&lowered));

    let core_mode = match &lowered.type_ {
        Some(type_) => curios_core::Mode::Check(type_.clone()),
        None => curios_core::Mode::Infer,
    };

    // Reuse the cached, pre-elaborated prelude: only the user items and the
    // entrypoint body are type-checked here, not the whole `sys`/`syn`/`std`
    // prelude (the bulk of the work). See `curios_core::elaborate_and_zonk_with_prelude`.
    // The timed context is created *inside* `with` so the one-time, per-thread
    // prelude build (which `with` triggers on first access) does not count
    // against the caller's `timeout` — the deadline bounds only the user work.
    let (module, core_type) = PRELUDE
        .with(|prelude| {
            let mut context = curios_core::Context::new(timeout);

            curios_core::elaborate_and_zonk_with_prelude(
                &mut context,
                prelude,
                &lowered,
                metavars,
                core_mode,
            )
        })
        .map_err(|error| error.format_with(&lowered))?;

    Ok((module, core_type, user_foreigns))
}

/// Compile a parsed entrypoint through the full pipeline to a wasm module, feeding every [`Stage`] to `observe` in order. The result pairs the module with the [`ForeignStore`] harvested from the program's own `foreign` declarations — an embedder that will run the module builds its `ffi`-tier bindings (`curios-rt`'s `ForeignBindings`) from exactly this store, or drops it when the program declares none. Binaryen optimization and Cranelift precompilation are deliberately *not* here — they live downstream in the `curios` crate (`to_cwasm`), keeping this crate free of native backends.
pub fn compile_entrypoint<O>(
    timeout: Duration,
    entrypoint: &curios_text::Entrypoint,
    loader: curios_text::RootSource,
    mut observe: O,
) -> Result<(curios_wasm::Module, ForeignStore), String>
where
    O: FnMut(Stage<'_>),
{
    let (module, core_type, foreigns) =
        elaborate_and_zonk(timeout, entrypoint, loader, &mut observe)?;

    // The whole elaborated module (the entire `sys`/`syn`/`std` prelude included)
    // is erased: there is no source-level prune, so std is type-checked in full on
    // every compile and its bugs surface instead of hiding behind an unreached
    // path.
    let ersd_module =
        curios_core::erase_module(&mut curios_core::Context::new(timeout), &module, &core_type)
            .map_err(|error| error.format_with(&module))?;

    observe(Stage::Ersd(&ersd_module));

    // *After* erase has type-checked everything, run the Ersd optimization
    // pipeline in place: drop the items the entrypoint cannot reach, then re-base
    // self-recursion onto accumulators and offsets (see `curios_ersd::optimize`).
    let mut ersd_optm_module = ersd_module;
    curios_ersd::optimize(&mut ersd_optm_module);

    observe(Stage::ErsdOptm(&ersd_optm_module));

    let cont_module = curios_ersd::to_cont(&ersd_optm_module).map_err(|error| error.to_string())?;

    observe(Stage::Cont(&cont_module));

    // Run the Cont optimization pipeline in place (see `curios_cont::optimize`).
    let mut cont_optm_module = cont_module;
    curios_cont::optimize(&mut cont_optm_module);

    observe(Stage::ContOptm(&cont_optm_module));

    let wasm_module = curios_cont::to_wasm(&cont_optm_module);

    observe(Stage::Wasm(&wasm_module));

    Ok((wasm_module, foreigns))
}

#[cfg(test)]
mod tests;
