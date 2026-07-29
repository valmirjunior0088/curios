//! The compile driver: the one crate that strings the pipeline stages together, from a parsed `curios_text::Entrypoint` to a `curios_wasm::Module`. [`compile_entrypoint`] runs the full `into_core → elaborate → zonk → erase → ersd optimize → into_cont → cont optimize → into_wasm` sequence. Each stage is passed to the caller's observer as a borrowed [`Stage`], which is how `--print` dumps IRs without the driver retaining them.
//!
//! The fixed `sys`/`syn`/`std` prelude is restored from `curios-prelude`'s build-scoped archive; every compile replays prepared Text/Core state and restores a fresh Ersd prefix, so production compilation never source-builds the prelude. Everything wasm-native — Binaryen, Cranelift precompilation, execution — lives downstream in `curios`/`curios-runtime`: this crate stops at the wasm module plus the program's harvested `ForeignStore`.

#[cfg(test)]
mod tests;

use {curios_abi::ForeignStore, std::fmt};

/// A borrowed view of one intermediate representation, handed to the caller's `observe` callback the moment that stage is produced. This is the pipeline's only introspection surface — the CLI's `--print` stage dumps and the test suites' IR assertions both hang off it — and borrowing keeps the driver from retaining any stage it has already lowered past.
/// The default reduction budget, re-exported so every caller of
/// [`compile_entrypoint`] can name it without depending on `curios-elab`.
pub use curios_elab::DEFAULT_STEP_BUDGET;

pub enum Stage<'a> {
    Text(&'a curios_text::Entrypoint),
    /// Core as `curios_text::into_core` produced it: syntax that nothing has
    /// checked. It carries term metavariables, lowering-time universe seeds,
    /// and unresolved `Infix` and `NumLit` nodes, and its registries are
    /// unelaborated. Useful for debugging the lowering; not a typed program.
    Core(&'a curios_elab::Module),
    /// Core after elaboration and zonking, which is the module every later
    /// stage consumes. Metavariable-free by construction — `zonk_module` errors
    /// on an unsolved hole — with universes validated, positivity checked,
    /// totality recorded, and both erasure obligations gated. The prelude
    /// prefix is spliced back in from the archive.
    ///
    /// The difference from [`Stage::Core`] is the absence of `Metavar`,
    /// `Infix`, and `NumLit`, which is exactly what the independent kernel
    /// requires of an input: this is the stage `curios_elab::recheck_module`
    /// takes, and [`Stage::Core`] is not.
    CoreElab(&'a curios_elab::Module),
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
    pub const NAMES: [&'static str; 8] = [
        "text",
        "core",
        "core-elab",
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
            Stage::CoreElab(_) => "core-elab",
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
            Stage::CoreElab(module) => write!(f, "{module}"),
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
    budget: u64,
    entrypoint: &curios_text::Entrypoint,
    loader: curios_text::RootSource,
    observe: &mut O,
) -> Result<(curios_elab::Module, curios_elab::Term, ForeignStore), String>
where
    O: FnMut(Stage<'_>),
{
    observe(Stage::Text(entrypoint));

    let (lowered, metavars, universe_floor, user_foreigns) =
        curios_prelude::with_prelude(|prelude| {
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
        Some(type_) => curios_elab::Mode::Check(type_.clone()),
        None => curios_elab::Mode::Infer,
    };

    let (module, core_type) = curios_prelude::with_prelude(|prelude| {
        let mut context = curios_elab::Context::new(budget);

        curios_elab::elaborate_and_zonk_with_prelude(
            &mut context,
            prelude.core(),
            &lowered,
            metavars,
            universe_floor,
            core_mode,
        )
    })
    .map_err(|error| error.format_with(&lowered))?;

    observe(Stage::CoreElab(&module));

    Ok((module, core_type, user_foreigns))
}

/// The back half of [`compile_entrypoint`]: from a verified erased module
/// through optimization, the lowering into Cont, Cont optimization, and wasm
/// emission, observing every stage in order.
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
fn lower_from_ersd<O>(mut ersd_module: curios_ersd::Module, observe: &mut O) -> curios_wasm::Module
where
    O: FnMut(Stage<'_>),
{
    observe(Stage::Ersd(&ersd_module));

    // Shrink before lowering: drop the items the program neither reaches nor
    // runs for effect, so Cont's whole-module fixpoint sees only the live
    // slice (see `curios_ersd::optimize_ir`).
    curios_ersd::optimize_ir(&mut ersd_module);

    observe(Stage::ErsdOptm(&ersd_module));

    let cont_module = curios_ersd::lower_to_cont(&ersd_module);

    observe(Stage::Cont(&cont_module));

    let mut cont_optm_module = cont_module;
    curios_cont::optimize(&mut cont_optm_module);

    observe(Stage::ContOptm(&cont_optm_module));

    let wasm_module = curios_cont::into_wasm(&cont_optm_module);

    observe(Stage::Wasm(&wasm_module));

    wasm_module
}

/// Compile a parsed entrypoint through the full pipeline to a wasm module, feeding every [`Stage`] to `observe` in order. The result pairs the module with the [`ForeignStore`] harvested from the program's own `foreign` declarations — an embedder that will run the module builds its `ffi`-tier bindings (`curios-runtime`'s `ForeignBindings`) from exactly this store, or drops it when the program declares none. Binaryen optimization and Cranelift precompilation are deliberately *not* here — they live downstream in the `curios` crate (`to_cwasm`), keeping this crate free of native backends.
///
/// Production runs the arena erased representation: the archived prelude
/// prefix is restored and replayed, only the user suffix erases, the arena
/// transformations shrink and rebase the module, and the lowering into Cont
/// makes every encoding decision once (see `curios_ersd::lower_to_cont`).
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
pub fn compile_entrypoint<O>(
    budget: u64,
    entrypoint: &curios_text::Entrypoint,
    loader: curios_text::RootSource,
    mut observe: O,
) -> Result<(curios_wasm::Module, ForeignStore), String>
where
    O: FnMut(Stage<'_>),
{
    let (module, core_type, foreigns) =
        elaborate_and_zonk(budget, entrypoint, loader, &mut observe)?;

    let ersd_module = curios_prelude::with_prelude(|prelude| {
        curios_elab::erase_module_with_prelude(
            &mut curios_elab::Context::new(budget),
            prelude.core(),
            &module,
            &core_type,
            prelude.ersd(),
        )
    })
    .map_err(|error| error.format_with(&module))?;

    Ok((lower_from_ersd(ersd_module, &mut observe), foreigns))
}
