//! The compile driver: the one crate that strings the pipeline stages together, from a parsed `curios_text::Entrypoint` to a `curios_wasm::Module`. [`compile_entrypoint`] runs the full `into_core → elaborate → zonk → erase → ersd optimize → into_cont → cont optimize → into_wasm` sequence. Each stage is passed to the caller's observer as a borrowed [`Stage`], which is how `--print` dumps IRs without the driver retaining them.
//!
//! The fixed `sys`/`syn`/`std` prelude is restored from `curios-prelude`'s build-scoped archive; every compile replays prepared Text/Core state and restores a fresh Ersd prefix, so production compilation never source-builds the prelude. Everything wasm-native — Binaryen, Cranelift precompilation, execution — lives downstream in `curios`/`curios-runtime`: this crate stops at the wasm module plus the program's harvested `ForeignStore`.

#[cfg(test)]
mod tests;

use {
    curios_abi::ForeignStore,
    std::{fmt, time::Duration},
};

/// A borrowed view of one intermediate representation, handed to the caller's `observe` callback the moment that stage is produced. This is the pipeline's only introspection surface — the CLI's `--print` stage dumps and the test suites' IR assertions both hang off it — and borrowing keeps the driver from retaining any stage it has already lowered past.
pub enum Stage<'a> {
    Text(&'a curios_text::Entrypoint),
    Core(&'a curios_core::Module),
    Ersd(&'a curios_ersd::Module),
    ErsdOptm(&'a curios_ersd::Module),
    Cont(&'a curios_cont::CpsModule),
    ContOptm(&'a curios_cont::CpsModule),
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
/// The `sys`/`syn`/`std` prelude is neither lowered nor elaborated per call:
/// prepared Text state is merged with the user graph, then the archived Core
/// prefix is replayed and only the user suffix is type-checked.
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
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

    let (lowered, metavars, user_foreigns) = curios_prelude::with_prelude(|prelude| {
        curios_text::into_core_with_prelude(
            entrypoint,
            &loader,
            prelude.prepared(),
            &curios_prelude::SYNTAX,
        )
    })
    .map_err(|error| error.format())?;

    observe(Stage::Core(&lowered));

    let core_mode = match &lowered.type_ {
        Some(type_) => curios_core::Mode::Check(type_.clone()),
        None => curios_core::Mode::Infer,
    };

    let (module, core_type) = curios_prelude::with_prelude(|prelude| {
        let mut context = curios_core::Context::new(timeout);

        curios_core::elaborate_and_zonk_with_prelude(
            &mut context,
            prelude.core(),
            &lowered,
            metavars,
            core_mode,
        )
    })
    .map_err(|error| error.format_with(&lowered))?;

    Ok((module, core_type, user_foreigns))
}

/// Compile a parsed entrypoint through the arena erased representation — the
/// Ersd v2 migration's vertical: fresh whole-module erasure into the verified
/// arena `ErasedModule`, the direct lowering into Cont, then the shared Cont
/// optimizer and Wasm emitter. Coexists with [`compile_entrypoint`] (the
/// legacy production path) until the flip; the behavior-identity corpus in
/// `curios` compares the two at runtime. The arena stages are not observable
/// through [`Stage`] until the flip repoints the `ersd` observers.
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
pub fn compile_entrypoint_via_arena<O>(
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

    let ersd_module = curios_core::erase_module_to_ir(
        &mut curios_core::Context::new(timeout),
        &module,
        &core_type,
    )
    .map_err(|error| error.format_with(&module))?;

    let cont_module = curios_ersd::lower_to_cont(&ersd_module);
    observe(Stage::Cont(&cont_module));

    let mut cont_optm_module = cont_module;
    curios_cont::optimize(&mut cont_optm_module);
    observe(Stage::ContOptm(&cont_optm_module));

    let wasm_module = curios_cont::into_wasm(&cont_optm_module);
    observe(Stage::Wasm(&wasm_module));

    Ok((wasm_module, foreigns))
}

/// Compile a parsed entrypoint through the full pipeline to a wasm module, feeding every [`Stage`] to `observe` in order. The result pairs the module with the [`ForeignStore`] harvested from the program's own `foreign` declarations — an embedder that will run the module builds its `ffi`-tier bindings (`curios-runtime`'s `ForeignBindings`) from exactly this store, or drops it when the program declares none. Binaryen optimization and Cranelift precompilation are deliberately *not* here — they live downstream in the `curios` crate (`to_cwasm`), keeping this crate free of native backends.
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
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

    let prefix = curios_prelude::restore_ersd_items();
    let ersd_module = curios_prelude::with_prelude(|prelude| {
        curios_core::erase_module_with_prelude(
            &mut curios_core::Context::new(timeout),
            prelude.core(),
            &module,
            &core_type,
            prefix,
        )
    })
    .map_err(|error| error.format_with(&module))?;

    observe(Stage::Ersd(&ersd_module));

    // *After* erase has type-checked everything, run the Ersd optimization
    // pipeline in place: drop the items the entrypoint cannot reach, then re-base
    // self-recursion onto accumulators and offsets (see `curios_ersd::optimize`).
    let mut ersd_optm_module = ersd_module;
    curios_ersd::optimize(&mut ersd_optm_module);

    observe(Stage::ErsdOptm(&ersd_optm_module));

    let cont_module =
        curios_ersd::into_cont(&ersd_optm_module).map_err(|error| error.to_string())?;

    observe(Stage::Cont(&cont_module));

    // Run the Cont optimization pipeline in place (see `curios_cont::optimize`).
    let mut cont_optm_module = cont_module;
    curios_cont::optimize(&mut cont_optm_module);

    observe(Stage::ContOptm(&cont_optm_module));

    let wasm_module = curios_cont::into_wasm(&cont_optm_module);

    observe(Stage::Wasm(&wasm_module));

    Ok((wasm_module, foreigns))
}
