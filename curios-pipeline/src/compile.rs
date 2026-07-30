//! The stage sequence itself: [`compile_entrypoint`] and the two halves it is assembled from — the type-checking prologue and the lowering back half — each observing every [`Stage`] it produces before moving past it.

use {
    super::Stage,
    curios_abi::ForeignStore,
    curios_cont::{into_wasm, optimize},
    curios_core::Term,
    curios_elab::{
        Context, Mode, elaborate_and_zonk_with_prelude, erase_module_with_prelude,
        recheck_module_suffix,
    },
    curios_ersd::{lower_to_cont, optimize_ir},
    curios_prelude::{SYNTAX, with_prelude},
    curios_text::{Entrypoint, RootSource, into_core_with_prelude},
};

/// The type-checking prologue of [`compile_entrypoint`] (and the tests' typecheck-only path): lower to core, elaborate (checking against the entrypoint's type when it carries one, else synthesizing), then zonk metavariable solutions in so the module is meta-free — the `elaborate → zonk` half of the `elaborate → zonk → erase` data flow. Elaboration is authoritative: it returns a rebuilt module (lambda domains solved, binders re-closed), and it is *that* module — not the lowered one — that zonk makes meta-free. `zonk` is also where an unsolved hole is rejected, so a program that merely *type-checks* is fully validated by the time this returns. Elaboration and zonking share one context (the solutions live in its `MetaStore`); the returned module is self-contained, so the caller's `erase` runs over a fresh one.
///
/// The `sys`/`syn`/`std` prelude is neither lowered nor elaborated per call: prepared Text state is merged with the user graph, then the archived Core prefix is replayed and only the user suffix is type-checked.
pub(crate) fn elaborate_and_zonk<O>(
    budget: u64,
    entrypoint: &Entrypoint,
    loader: RootSource,
    observe: &mut O,
) -> Result<(curios_elab::Module, Term, ForeignStore), String>
where
    O: FnMut(Stage<'_>),
{
    curios_profile::profile!("elaborate_and_zonk");
    observe(Stage::Text(entrypoint));

    let (lowered, metavars, universe_floor, user_foreigns) = with_prelude(|prelude| {
        into_core_with_prelude(entrypoint, &loader, prelude.prepared(), &SYNTAX)
    })
    .map_err(|error| error.format())?;

    observe(Stage::Core(&lowered));

    let core_mode = match &lowered.type_ {
        Some(type_) => Mode::Check(type_.clone()),
        None => Mode::Infer,
    };

    let (module, core_type) = with_prelude(|prelude| {
        let mut context = Context::new(budget);

        elaborate_and_zonk_with_prelude(
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

/// The back half of [`compile_entrypoint`]: from a verified erased module through optimization, the lowering into Cont, Cont optimization, and wasm emission, observing every stage in order.
fn lower_from_ersd<O>(mut ersd_module: curios_ersd::Module, observe: &mut O) -> curios_wasm::Module
where
    O: FnMut(Stage<'_>),
{
    curios_profile::profile!("lower_from_ersd");
    observe(Stage::Ersd(&ersd_module));

    // Shrink before lowering: drop the items the program neither reaches nor runs for effect, so Cont's whole-module fixpoint sees only the live slice (see `curios_ersd::optimize_ir`).
    optimize_ir(&mut ersd_module);

    observe(Stage::ErsdOptm(&ersd_module));

    let cont_module = lower_to_cont(&ersd_module);

    observe(Stage::Cont(&cont_module));

    let mut cont_optm_module = cont_module;
    optimize(&mut cont_optm_module);

    observe(Stage::ContOptm(&cont_optm_module));

    let wasm_module = into_wasm(&cont_optm_module);

    observe(Stage::Wasm(&wasm_module));

    wasm_module
}

/// Compile a parsed entrypoint through the full pipeline to a wasm module, feeding every [`Stage`] to `observe` in order. The result pairs the module with the [`ForeignStore`] harvested from the program's own `foreign` declarations — an embedder that will run the module builds its `ffi`-tier bindings (`curios-runtime`'s `ForeignBindings`) from exactly this store, or drops it when the program declares none. Binaryen optimization and Cranelift precompilation are deliberately *not* here — they live downstream in the `curios` crate (`to_cwasm`), keeping this crate free of native backends.
///
/// Production runs the arena erased representation: the archived prelude prefix is restored and replayed, only the user suffix erases, the arena transformations shrink and rebase the module, and the lowering into Cont makes every encoding decision once (see `curios_ersd::lower_to_cont`).
pub fn compile_entrypoint<O>(
    budget: u64,
    entrypoint: &Entrypoint,
    loader: RootSource,
    mut observe: O,
) -> Result<(curios_wasm::Module, ForeignStore), String>
where
    O: FnMut(Stage<'_>),
{
    curios_profile::profile!("compile_entrypoint");
    let (module, core_type, foreigns) =
        elaborate_and_zonk(budget, entrypoint, loader, &mut observe)?;

    // The independent kernel's second opinion, on the compile path: the archive-replayed prelude prefix was walked when the archive was built, so only the entry suffix is judged here — a refusal fails the compile.
    {
        curios_profile::profile!("recheck_suffix");
        let checked_from = with_prelude(|prelude| prelude.core().items.len());
        if let Some(verdict) = recheck_module_suffix(&module, budget, checked_from)
            .into_iter()
            .next()
        {
            return Err(match &verdict.name {
                Some(name) => format!("the kernel refused {name}: {}", verdict.error),
                None => format!("the kernel refused the entrypoint: {}", verdict.error),
            });
        }
    }

    let ersd_module = with_prelude(|prelude| {
        erase_module_with_prelude(
            &mut Context::new(budget),
            prelude.core(),
            &module,
            &core_type,
            prelude.ersd(),
        )
    })
    .map_err(|error| error.format_with(&module))?;

    Ok((lower_from_ersd(ersd_module, &mut observe), foreigns))
}
