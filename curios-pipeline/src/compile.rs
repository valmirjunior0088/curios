//! The stage sequence itself: [`compile_entrypoint`] and the two halves it is assembled from — the type-checking prologue and the lowering back half — each observing every [`Stage`] it produces before moving past it.

use {
    super::Stage,
    curios_abi::ForeignStore,
    curios_base::{Qualifier, SyntaxRegistry},
    curios_cert::{Globals, Verdict, recheck_module_verdicts},
    curios_cont::into_wasm,
    curios_core::derived_binder_floor,
    curios_core::{Intrinsic, Term},
    curios_elab::{
        Context, Established, Mode, Resumed, elaborate_and_zonk_unit,
        elaborate_and_zonk_unit_reporting, erase_unit,
    },
    curios_ersd::lower_to_cont,
    curios_text::{Entrypoint, RootSource, UnitSource, into_core_unit, into_core_with_prelude},
    curios_unit::{Prefix, Unit},
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

/// Put `module` to the independent kernel with `scope` already in scope, so only what its units do not already answer for is judged — their own items resting on the verdict recorded when each was built.
///
/// The [`Globals`] environment is assembled here rather than in `curios-unit`, because a unit is defined to stay below the kernel and cannot name it. Every caller that wants the compile path's rechecking gets this one rather than reconstructing it.
pub fn recheck(module: &curios_core::Module, budget: u64, scope: Prefix<'_>) -> Vec<Verdict> {
    recheck_module_verdicts(module, budget, &globals(scope))
}

/// The kernel's environment for `scope`: every unit mounted, at the binder floor its own walk derived.
fn globals(scope: Prefix<'_>) -> Globals {
    let mut globals = Globals::default();
    for unit in scope.units() {
        globals.mount(unit.core(), unit.binder_floor());
    }

    globals
}

/// Lower and type-check `entrypoint`, reporting the erasure obligations rather than raising them.
///
/// The elaborated module comes back even when this stage's own (T)/(V) verdicts refuse it, which is what lets one fixture be put to *both* checkers: `curios-cert` decides the same two obligations independently, and a program only this side refuses would otherwise yield no module for the kernel to judge — leaving the most consequential disagreement, the trusted base resting on an elaborator-only analysis, unobservable. Nothing else about type-checking is relaxed; every other error still short-circuits.
///
/// The verdicts are rendered against the lowered module, so they read as they would on the compile path. A caller that wants the kernel's opinion on the result puts it to [`recheck`], which supplies the same environment `compile_entrypoint` does rather than re-walking the standard library.
pub fn typecheck_reporting(
    budget: u64,
    scope: Prefix<'_>,
    syntax: &SyntaxRegistry,
    entrypoint: &Entrypoint,
    loader: RootSource,
) -> Result<(curios_core::Module, Vec<String>), CompileError> {
    let text = scope.text();
    let cores = scope.cores();
    let (lowered, metavars, universe_floor, _foreigns) =
        into_core_with_prelude(entrypoint, &loader, &text, syntax)
            .map_err(|error| CompileError::Failure(error.format()))?;

    let core_mode = match &lowered.type_ {
        Some(type_) => Mode::Check(type_.clone()),
        None => Mode::Infer,
    };

    let (module, _core_type, obligations) = elaborate_and_zonk_unit_reporting(
        &mut Context::new(budget, *syntax),
        Established::over(&cores),
        &lowered,
        metavars,
        universe_floor,
        core_mode,
    )
    .map_err(|error| CompileError::of(&error, error.format_with(&lowered, &cores)))?;

    let obligations = obligations
        .into_iter()
        .map(|error| error.format_with(&lowered, &cores))
        .collect();

    Ok((module, obligations))
}

/// The type-checking prologue of [`compile_entrypoint`] (and the tests' typecheck-only path): lower to core, elaborate (checking against the entrypoint's type when it carries one, else synthesizing), then zonk metavariable solutions in so the module is meta-free — the `elaborate → zonk` half of the `elaborate → zonk → erase` data flow. Elaboration is authoritative: it returns a rebuilt module (lambda domains solved, binders re-closed), and it is *that* module — not the lowered one — that zonk makes meta-free. `zonk` is also where an unsolved hole is rejected, so a program that merely *type-checks* is fully validated by the time this returns. Elaboration and zonking share one context (the solutions live in its `MetaStore`); the returned module is self-contained, so the caller's `erase` runs over a fresh one.
///
/// The `sys`/`syn`/`std` prelude is neither lowered nor elaborated per call: prepared Text state is merged with the user graph, then the archived Core prelude is replayed into the elaboration context and only the entry's own items are type-checked.
pub(crate) fn elaborate_and_zonk<O>(
    budget: u64,
    scope: Prefix<'_>,
    syntax: &SyntaxRegistry,
    entrypoint: &Entrypoint,
    loader: RootSource,
    observe: &mut O,
) -> Result<(curios_core::Module, Term, ForeignStore), CompileError>
where
    O: FnMut(Stage<'_>),
{
    curios_profile::profile!("elaborate_and_zonk");
    observe(Stage::Text(entrypoint));

    let text = scope.text();
    let cores = scope.cores();
    let (lowered, metavars, universe_floor, user_foreigns) =
        into_core_with_prelude(entrypoint, &loader, &text, syntax)
            .map_err(|error| CompileError::Failure(error.format()))?;

    observe(Stage::Core(&lowered));

    // The entrypoint contract, as an ordinary expectation rather than a judgment after the fact: a program *is* a description of doing something and yielding nothing. An embedder that states its own type still gets it — that is how the typecheck-only fixtures reach both checkers with deliberately odd tails.
    //
    // `Io({})` is closed, which is what makes this a `Mode::Check` at all. Checking against `Io(?T)` would need a metavariable minted before the elaboration context exists, and that is why this contract used to be a post-hoc head test on the inferred type instead. Stating the unit payload removes the metavariable, and checking rather than inferring is what lets a tail spell itself `Io/pure(())` — the payload comes from the expectation exactly as it does under a written match motive.
    let core_mode = match &lowered.type_ {
        Some(type_) => Mode::Check(type_.clone()),
        None => Mode::Check(Term::intrinsic(Intrinsic::io_type(Term::tuple_type_unit()))),
    };

    let (module, core_type) = elaborate_and_zonk_unit(
        &mut Context::new(budget, *syntax),
        Established::over(&cores),
        &lowered,
        metavars,
        universe_floor,
        core_mode,
    )
    .map_err(|error| CompileError::of(&error, error.format_with(&lowered, &cores)))?;

    observe(Stage::CoreElab(&module));

    // The entry is the unit with an entrypoint, and `elaborate_and_zonk` is only ever asked for one.
    let core_type = core_type.expect("the entrypoint's type comes back with its body");

    Ok((module, core_type, user_foreigns))
}

/// The back half of [`compile_entrypoint`]: from a verified erased module through optimization, the lowering into Cont, Cont optimization, and wasm emission, observing every stage in order.
fn lower_from_ersd<O>(mut ersd_module: curios_ersd::Module, observe: &mut O) -> curios_wasm::Module
where
    O: FnMut(Stage<'_>),
{
    curios_profile::profile!("lower_from_ersd");
    observe(Stage::Ersd(&ersd_module));

    // Shrink before lowering: drop the items the program neither reaches nor runs for effect, so Cont's whole-module fixpoint sees only the live slice (see `curios_ersd::optimize`).
    curios_ersd::optimize(&mut ersd_module);

    observe(Stage::ErsdOptm(&ersd_module));

    let cont_module = lower_to_cont(&ersd_module);

    observe(Stage::Cont(&cont_module));

    let mut cont_optm_module = cont_module;
    curios_cont::optimize(&mut cont_optm_module);

    observe(Stage::ContOptm(&cont_optm_module));

    let wasm_module = into_wasm(&cont_optm_module);

    observe(Stage::Wasm(&wasm_module));

    wasm_module
}

/// Compile one unit against `scope`: lower, elaborate, judge, erase.
///
/// **The judgment sits between elaboration and erasure and that ordering is the point.** A module the kernel refuses never reaches erasure's budget, and a refusal reads as a refusal rather than as whatever erasure made of an ill-typed term. It is also why this is a fold step rather than one operation: the producer of a stored unit runs the same sequence *without* the judge, because the crate that writes an image deliberately cannot reach the kernel.
pub fn compile_unit(
    budget: u64,
    scope: Prefix<'_>,
    syntax: &SyntaxRegistry,
    source: &UnitSource<'_>,
) -> Result<Unit, CompileError> {
    curios_profile::profile!("compile_unit");
    let text = scope.text();
    let cores = scope.cores();

    let lowered = into_core_unit(source, &text, syntax)
        .map_err(|error| CompileError::Failure(error.format()))?;

    let (core, _body_type) = elaborate_and_zonk_unit(
        &mut Context::new(budget, *syntax),
        Established::over(&cores),
        lowered.core(),
        lowered.metavariable_floor(),
        lowered.universe_floor(),
        Mode::Infer,
    )
    .map_err(|error| CompileError::of(&error, error.format_with(lowered.core(), &cores)))?;

    if let Some(verdict) = recheck(&core, budget, scope).into_iter().next() {
        let refusal = verdict.error.format_with(&core, &cores);
        return Err(CompileError::Failure(match &verdict.name {
            Some(name) => format!("the kernel refused {name}: {refusal}"),
            None => format!("the kernel refused a unit: {refusal}"),
        }));
    }

    let ersd = erase_unit(
        &mut Context::new(budget, *syntax),
        Resumed::of(&cores, scope.arena()),
        &core,
        None,
    )
    .map_err(|error| CompileError::Failure(error.format_with(&core, &cores)))?;

    let binder_floor = derived_binder_floor(&core);

    Ok(Unit::new(lowered, core, ersd, binder_floor))
}

/// Where a judged unit is kept between compilations.
///
/// Declared here and implemented in `curios-package`, because the fold is what consults one and this crate must never learn what a project is. What crosses the boundary is a unit — never a path, never a manifest, and never a key this crate would have to know how to build.
///
/// **A unit handed back is one that was judged when it was recorded, and taking it is taking that on trust.** That is a change to what the compiler believes rather than an optimization of what it does, which is why the argument for it lives in [SOUNDNESS.md](../../documentation/SOUNDNESS.md) under *Cached verdicts* rather than here: the implementation chooses the key, and the key is the whole of what the argument rests on.
pub trait Cache {
    /// The unit already recorded for `source`, if one is.
    fn get(&self, source: &UnitSource<'_>) -> Option<Unit>;

    /// Record what `source` compiled to. Best effort — a store that cannot be written costs the next compilation the work, and nothing else.
    fn put(&self, source: &UnitSource<'_>, unit: &Unit);
}

/// Compile `sources` in dependency order, each against `base` and everything before it — the fold this whole design is named for.
///
/// Returns the units it produced, not the ones it was given: the caller owns `base` and this cannot take it. A scope is rebuilt per step from pointers to both, which is free.
///
/// A `cache` short-circuits the step entirely: a recorded unit was judged when it was recorded, so neither elaboration nor the kernel re-runs for it. `None` compiles everything, which is what every caller without a project does.
pub fn compile_units<'a, P>(
    budget: u64,
    base: Prefix<'a>,
    syntax: &SyntaxRegistry,
    sources: &[UnitSource<'_>],
    cache: Option<&dyn Cache>,
    mut progress: P,
) -> Result<Vec<Unit>, CompileError>
where
    P: FnMut(Progress<'_>),
{
    let mut produced: Vec<Unit> = Vec::new();

    for source in sources {
        // Announced *after* the cache is consulted and *before* the work, so a reported operation is one that is actually about to happen and its timing starts where it does.
        if let Some(unit) = cache.and_then(|cache| cache.get(source)) {
            progress(Progress::Reused(&source.prefix()));
            produced.push(unit);
            continue;
        }

        progress(Progress::Compiling(&source.prefix()));

        let scope = base
            .units()
            .iter()
            .copied()
            .chain(produced.iter())
            .collect::<Vec<_>>();

        let unit = compile_unit(budget, Prefix::over(&scope), syntax, source)?;
        progress(Progress::Compiled);

        if let Some(cache) = cache {
            cache.put(source, &unit);
        }

        produced.push(unit);
    }

    Ok(produced)
}

/// What the fold is doing, for a caller that reports it.
///
/// Sequential by construction — the fold compiles one unit at a time — so [`Progress::Reused`] and [`Progress::Compiled`] refer to whichever subject was announced last and need not name it again.
///
/// Deliberately separate from the [`Stage`] observer, which belongs to `--print`: one reports *what is happening* and the other *what was produced*, and a caller wants either without the other.
pub enum Progress<'a> {
    /// A mounted unit is about to be compiled, named by the prefix it claims.
    Compiling(&'a Qualifier),
    /// The entry program is about to be compiled. Unnamed: it owns the empty prefix, so only the caller knows what was asked for.
    Entry,
    /// A mounted unit came from the store instead; nothing is compiled for it, and no [`Progress::Compiled`] follows.
    Reused(&'a Qualifier),
    /// The subject just announced finished.
    Compiled,
}

/// Compile a parsed entrypoint through the full pipeline to a wasm module, feeding every [`Stage`] to `observe` in order. The result pairs the module with the [`ForeignStore`] harvested from the program's own `foreign` declarations — an embedder that will run the module builds its `ffi`-tier bindings (`curios-runtime`'s `ForeignBindings`) from exactly this store, or drops it when the program declares none. Binaryen optimization and Cranelift precompilation are deliberately *not* here — they live downstream in the `curios` crate (`to_cwasm`), keeping this crate free of native backends.
///
/// Production runs the arena erased representation: the archived erased prelude is restored and replayed, only the entry's own items erase, the arena transformations shrink and rebase the module, and the lowering into Cont makes every encoding decision once (see `curios_ersd::lower_to_cont`).
pub fn compile_entrypoint<O>(
    budget: u64,
    scope: Prefix<'_>,
    syntax: &SyntaxRegistry,
    entrypoint: &Entrypoint,
    loader: RootSource,
    mut observe: O,
) -> Result<(curios_wasm::Module, ForeignStore), CompileError>
where
    O: FnMut(Stage<'_>),
{
    curios_profile::profile!("compile_entrypoint");
    let (module, core_type, foreigns) =
        elaborate_and_zonk(budget, scope, syntax, entrypoint, loader, &mut observe)?;
    let cores = scope.cores();

    // The independent kernel's second opinion, on the compile path: each unit in scope was walked when it was built and arrives here as environment, so only what it does not already answer for is judged — a refusal fails the compile.
    {
        curios_profile::profile!("recheck");
        if let Some(verdict) = recheck(&module, budget, scope).into_iter().next() {
            let refusal = verdict.error.format_with(&module, &cores);
            return Err(CompileError::Failure(match &verdict.name {
                Some(name) => format!("the kernel refused {name}: {refusal}"),
                None => format!("the kernel refused the entrypoint: {refusal}"),
            }));
        }
    }

    let ersd_module = erase_unit(
        &mut Context::new(budget, *syntax),
        Resumed::of(&cores, scope.arena()),
        &module,
        Some(&core_type),
    )
    .map(|erased| erased.into_module())
    .map_err(|error| CompileError::Failure(error.format_with(&module, &cores)))?;

    // Every unit's rows, not the entry's alone: an embedder binds one registry, and a dependency that declares a `foreign` row has to reach it. Disjoint by mount, so the union cannot collide.
    let mut all_foreigns = scope.foreigns();
    all_foreigns.absorb(&foreigns);

    Ok((lower_from_ersd(ersd_module, &mut observe), all_foreigns))
}
