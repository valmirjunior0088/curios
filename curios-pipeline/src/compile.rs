//! The stage sequence itself: [`compile_entrypoint`] and the two halves it is assembled from — the type-checking prologue and the lowering back half — each observing every [`Stage`] it produces before moving past it.

use {
    super::Stage,
    curios_abi::ForeignStore,
    curios_cert::{Prefix, Verdict, recheck_module_suffix},
    curios_cont::{into_wasm, optimize},
    curios_core::{Intrinsic, Term},
    curios_elab::{
        Context, Mode, elaborate_and_zonk_with_prelude, elaborate_and_zonk_with_prelude_reporting,
        erase_module_with_prelude,
    },
    curios_ersd::{lower_to_cont, optimize_ir},
    curios_prelude::{SYNTAX, with_prelude},
    curios_text::{Entrypoint, RootSource, into_core_with_prelude},
    std::fmt,
};

/// A compile failure, split for process-level reporting: a written-goal batch is *incomplete* development state, everything else a hard *failure*. The CLI maps the two to distinct exit codes — 2 for incomplete, 1 for failure — so tooling can distinguish "here is your goal batch" from "something is wrong" without parsing stderr. Both carry the fully formatted report; an embedder that does not care converts to it via `Display` or the `String` conversion.
#[derive(Debug)]
pub enum CompileError {
    Incomplete(String),
    Failure(String),
}

impl CompileError {
    /// Classify a front-end error by [`curios_elab::Error::is_incomplete`], pairing it with its already-formatted report.
    fn of(error: &curios_elab::Error, message: String) -> Self {
        match error.is_incomplete() {
            true => Self::Incomplete(message),
            false => Self::Failure(message),
        }
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Incomplete(message) | Self::Failure(message) => formatter.write_str(message),
        }
    }
}

impl From<CompileError> for String {
    fn from(error: CompileError) -> Self {
        match error {
            CompileError::Incomplete(message) | CompileError::Failure(message) => message,
        }
    }
}

/// Put `module`'s user suffix to the independent kernel, defining the archived prelude prefix on the archive's word.
///
/// The [`Prefix`] descriptor is assembled here because it borrows the restored prelude, which exists only inside `with_prelude`'s scope — so every caller that wants the compile path's rechecking gets the same prefix rather than reconstructing one.
pub fn recheck_suffix(module: &curios_core::Module, budget: u64) -> Vec<Verdict> {
    with_prelude(|prelude| {
        recheck_module_suffix(
            module,
            budget,
            Prefix {
                module: prelude.core(),
                binder_floor: prelude.binder_floor(),
            },
        )
    })
}

/// Lower and type-check `entrypoint`, reporting the erasure obligations rather than raising them.
///
/// The elaborated module comes back even when this stage's own (T)/(V) verdicts refuse it, which is what lets one fixture be put to *both* checkers: `curios-cert` decides the same two obligations independently, and a program only this side refuses would otherwise yield no module for the kernel to judge — leaving the most consequential disagreement, the trusted base resting on an elaborator-only analysis, unobservable. Nothing else about type-checking is relaxed; every other error still short-circuits.
///
/// The verdicts are rendered against the lowered module, so they read as they would on the compile path. The returned index is where the archived prelude prefix ends, so a caller can put the suffix to the kernel exactly as `compile_entrypoint` does rather than re-walking the standard library.
pub fn typecheck_reporting(
    budget: u64,
    entrypoint: &Entrypoint,
    loader: RootSource,
) -> Result<(curios_core::Module, usize, Vec<String>), CompileError> {
    let (lowered, metavars, universe_floor, _foreigns) = with_prelude(|prelude| {
        into_core_with_prelude(entrypoint, &loader, prelude.prepared(), &SYNTAX)
    })
    .map_err(|error| CompileError::Failure(error.format()))?;

    let core_mode = match &lowered.type_ {
        Some(type_) => Mode::Check(type_.clone()),
        None => Mode::Infer,
    };

    let (module, _core_type, obligations) = with_prelude(|prelude| {
        let mut context = Context::new(budget, SYNTAX);

        elaborate_and_zonk_with_prelude_reporting(
            &mut context,
            prelude.core(),
            &lowered,
            metavars,
            universe_floor,
            core_mode,
        )
    })
    .map_err(|error| CompileError::of(&error, error.format_with(&lowered)))?;

    let obligations = obligations
        .into_iter()
        .map(|error| error.format_with(&lowered))
        .collect();

    let checked_from = with_prelude(|prelude| prelude.core().items.len());

    Ok((module, checked_from, obligations))
}

/// The type-checking prologue of [`compile_entrypoint`] (and the tests' typecheck-only path): lower to core, elaborate (checking against the entrypoint's type when it carries one, else synthesizing), then zonk metavariable solutions in so the module is meta-free — the `elaborate → zonk` half of the `elaborate → zonk → erase` data flow. Elaboration is authoritative: it returns a rebuilt module (lambda domains solved, binders re-closed), and it is *that* module — not the lowered one — that zonk makes meta-free. `zonk` is also where an unsolved hole is rejected, so a program that merely *type-checks* is fully validated by the time this returns. Elaboration and zonking share one context (the solutions live in its `MetaStore`); the returned module is self-contained, so the caller's `erase` runs over a fresh one.
///
/// The `sys`/`syn`/`std` prelude is neither lowered nor elaborated per call: prepared Text state is merged with the user graph, then the archived Core prefix is replayed and only the user suffix is type-checked.
pub(crate) fn elaborate_and_zonk<O>(
    budget: u64,
    entrypoint: &Entrypoint,
    loader: RootSource,
    observe: &mut O,
) -> Result<(curios_core::Module, Term, ForeignStore), CompileError>
where
    O: FnMut(Stage<'_>),
{
    curios_profile::profile!("elaborate_and_zonk");
    observe(Stage::Text(entrypoint));

    let (lowered, metavars, universe_floor, user_foreigns) = with_prelude(|prelude| {
        into_core_with_prelude(entrypoint, &loader, prelude.prepared(), &SYNTAX)
    })
    .map_err(|error| CompileError::Failure(error.format()))?;

    observe(Stage::Core(&lowered));

    // The entrypoint contract, as an ordinary expectation rather than a judgment after the fact: a program *is* a description of doing something and yielding nothing. An embedder that states its own type still gets it — that is how the typecheck-only fixtures reach both checkers with deliberately odd tails.
    //
    // `Io({})` is closed, which is what makes this a `Mode::Check` at all. Checking against `Io(?T)` would need a metavariable minted before the elaboration context exists, and that is why this contract used to be a post-hoc head test on the inferred type instead. Stating the unit payload removes the metavariable, and checking rather than inferring is what lets a tail spell itself `Io/pure(())` — the payload comes from the expectation exactly as it does under a written match motive.
    let core_mode = match &lowered.type_ {
        Some(type_) => Mode::Check(type_.clone()),
        None => Mode::Check(Term::intrinsic(Intrinsic::io_type(Term::tuple_type_unit()))),
    };

    let (module, core_type) = with_prelude(|prelude| {
        let mut context = Context::new(budget, SYNTAX);

        elaborate_and_zonk_with_prelude(
            &mut context,
            prelude.core(),
            &lowered,
            metavars,
            universe_floor,
            core_mode,
        )
    })
    .map_err(|error| CompileError::of(&error, error.format_with(&lowered)))?;

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
) -> Result<(curios_wasm::Module, ForeignStore), CompileError>
where
    O: FnMut(Stage<'_>),
{
    curios_profile::profile!("compile_entrypoint");
    let (module, core_type, foreigns) =
        elaborate_and_zonk(budget, entrypoint, loader, &mut observe)?;

    // The independent kernel's second opinion, on the compile path: the archive-replayed prelude prefix was walked when the archive was built, so only the entry suffix is judged here — a refusal fails the compile.
    {
        curios_profile::profile!("recheck_suffix");
        if let Some(verdict) = recheck_suffix(&module, budget).into_iter().next() {
            let refusal = verdict.error.format_with(&module);
            return Err(CompileError::Failure(match &verdict.name {
                Some(name) => format!("the kernel refused {name}: {refusal}"),
                None => format!("the kernel refused the entrypoint: {refusal}"),
            }));
        }
    }

    let ersd_module = with_prelude(|prelude| {
        erase_module_with_prelude(
            &mut Context::new(budget, SYNTAX),
            prelude.core(),
            &module,
            &core_type,
            prelude.ersd(),
        )
    })
    .map_err(|error| CompileError::Failure(error.format_with(&module)))?;

    Ok((lower_from_ersd(ersd_module, &mut observe), foreigns))
}
