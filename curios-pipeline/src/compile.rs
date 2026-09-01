//! The stage sequence itself: [`compile_entrypoint`] and the two halves it is assembled from — the type-checking prologue and the lowering back half — each observing every [`Stage`] it produces before moving past it.

use {
    super::Stage,
    curios_abi::ForeignStore,
    curios_cert::{Globals, Kernel, Verdict, recheck_module_measured, recheck_module_verdicts},
    curios_cont::into_wasm,
    curios_core::derived_binder_floor,
    curios_core::{Consumption, Intrinsic, Term},
    curios_elab::{
        Context, Established, Mode, Resumed, elaborate_and_zonk_unit,
        elaborate_and_zonk_unit_reporting, erase_unit,
    },
    curios_ersd::lower_to_cont,
    curios_text::{
        Entrypoint, LoweredEntry, RootSource, UnitSource, into_core_unit, into_core_with_prelude,
    },
    curios_unit::{Prefix, Unit},
    curios_utilities::{Qualifier, Report, SyntaxRegistry},
    std::fmt,
};

/// A compile failure, split for process-level reporting: a written-goal batch is *incomplete* development state, everything else a hard *failure*. The CLI maps the two to distinct exit codes — 2 for incomplete, 1 for failure — so tooling can distinguish "here is your goal batch" from "something is wrong" without parsing stderr.
///
/// Both carry what was said as located [`Report`]s — one per goal for a batch, one otherwise — rather than as text, so a consumer placing a diagnostic in a buffer reads the span instead of parsing the `-->` header back out. The text is the reports rendered, through `Display` or the `String` conversion, and it is exactly the text the CLI prints: the located form and the printed form are one value.
#[derive(Debug)]
pub enum CompileError {
    Incomplete(Vec<Report>),
    Failure(Vec<Report>),
}

impl CompileError {
    /// Classify a front-end error by [`curios_elab::Error::is_incomplete`], pairing it with its already-located reports.
    fn of(error: &curios_elab::Error, reports: Vec<Report>) -> Self {
        match error.is_incomplete() {
            true => Self::Incomplete(reports),
            false => Self::Failure(reports),
        }
    }

    /// A hard failure about nothing in particular — a store, a manifest, a backend — which is what every message that arrives as plain text is.
    pub fn failure(message: impl Into<String>) -> Self {
        Self::Failure(vec![Report::unlocated(message)])
    }

    /// What was said, located, whichever way it was classified.
    pub fn reports(&self) -> &[Report] {
        match self {
            Self::Incomplete(reports) | Self::Failure(reports) => reports,
        }
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&Report::render_all(self.reports()))
    }
}

impl From<CompileError> for String {
    fn from(error: CompileError) -> Self {
        error.to_string()
    }
}

/// Put `module` to the independent kernel with `scope` already in scope, so only what its units do not already answer for is judged — their own items resting on the verdict recorded when each was built.
///
/// The [`Globals`] environment is assembled here rather than in `curios-unit`, because a unit is defined to stay below the kernel and cannot name it. Every caller that wants the compile path's rechecking gets this one rather than reconstructing it.
pub fn recheck(
    module: &curios_core::Zonked<curios_core::Module>,
    budget: u64,
    scope: Prefix<'_>,
    syntax: &SyntaxRegistry,
) -> Vec<Verdict> {
    recheck_module_verdicts(module, budget, &globals(scope), *syntax)
}

/// [`recheck`], handing back the walk's own kernel for a measurement to read rather than only its verdicts. See `curios_cert::recheck_module_measured`.
pub fn recheck_measured(
    module: &curios_core::Zonked<curios_core::Module>,
    budget: u64,
    scope: Prefix<'_>,
    syntax: &SyntaxRegistry,
) -> (Vec<Verdict>, Kernel) {
    recheck_module_measured(module, budget, &globals(scope), *syntax)
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
    loader: &RootSource,
) -> Result<(curios_core::Module, Vec<String>), CompileError> {
    typecheck_measured(budget, scope, syntax, entrypoint, loader)
        .map(|(module, obligations, _, _)| (module, obligations))
}

/// [`typecheck_reporting`], reporting what elaboration consumed as well: the heaviest declaration, and the retention allowance the whole compilation used.
///
/// The measurement entry point on this side of the seam, matching `curios-cert`'s `recheck_module_measured` on the other. The heaviest declaration's units and peak depth are what `DEFAULT_STEP_BUDGET` is set against and the only figures that say whether a program's cost is depth or work; the retention figure is what `DEFAULT_RETENTION_QUOTA` is set against, and the two are coupled from one side — a memo that cannot be stored is re-derived against the *work* budget, so a compilation that exhausts its allowance stops being linear in what it spends.
///
/// A measurement's entry point, never a control. Nothing in the compiler reads the last two components, and [`typecheck_reporting`] drops them.
pub fn typecheck_measured(
    budget: u64,
    scope: Prefix<'_>,
    syntax: &SyntaxRegistry,
    entrypoint: &Entrypoint,
    loader: &RootSource,
) -> Result<(curios_core::Module, Vec<String>, Consumption, u64), CompileError> {
    let text = scope.text();
    let cores = scope.cores();
    let LoweredEntry {
        core: lowered,
        metavariable_floor: metavars,
        universe_floor,
        unbound,
        imports,
        ..
    } = into_core_with_prelude(entrypoint, loader, &text, syntax)
        .map_err(|error| CompileError::Failure(vec![error.report()]))?;

    let core_mode = match lowered
        .entry
        .as_ref()
        .and_then(|entry| entry.type_.as_ref())
    {
        Some(type_) => Mode::Check(type_.clone()),
        None => Mode::Infer,
    };

    let mut context = Context::new(budget, *syntax);
    context.set_imports(imports.clone());
    let (module, _core_type, obligations) = elaborate_and_zonk_unit_reporting(
        &mut context,
        Established::over(&cores),
        &lowered,
        metavars,
        universe_floor,
        core_mode,
    )
    .map_err(|error| {
        CompileError::of(
            &error,
            error.reports_with_hints(&lowered, &cores, &unbound, &imports),
        )
    })?;

    let obligations = obligations
        .into_iter()
        .map(|error| error.format_with_hints(&lowered, &cores, &unbound, &imports))
        .collect();

    Ok((
        module,
        obligations,
        context.heaviest_declaration(),
        context.retained(),
    ))
}

/// The type-checking prologue of [`compile_entrypoint`] (and the tests' typecheck-only path): lower to core, elaborate (checking against the entrypoint's type when it carries one, else synthesizing), then zonk metavariable solutions in so the module is meta-free — the `elaborate → zonk` half of the `elaborate → zonk → erase` data flow. Elaboration is authoritative: it returns a rebuilt module (lambda domains solved, binders re-closed), and it is *that* module — not the lowered one — that zonk makes meta-free. `zonk` is also where an unsolved hole is rejected, so a program that merely *type-checks* is fully validated by the time this returns. Elaboration and zonking share one context (the solutions live in its `MetaStore`); the returned module is self-contained, so the caller's `erase` runs over a fresh one.
///
/// The `sys`/`syn`/`std` prelude is neither lowered nor elaborated per call: prepared Text state is merged with the user graph, then the archived Core prelude is replayed into the elaboration context and only the entry's own items are type-checked.
/// Which final term a compilation checks: the one the author wrote, or the test tail synthesized from a unit's registered tests. A policy rather than a boolean at each call site, so the program shapes a unit can compile to are named where they diverge.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EntryTail {
    /// The authored entrypoint, exactly as parsed — the ordinary program.
    Authored,
    /// The synthesized `Test/main([...])` over the entry unit's own registered tests, replacing an executable's authored tail. A unit with no tests gets `Test/main([])`, which runs nothing and exits 0.
    Tests,
    /// The same synthesized tail over the last mounted unit's registered tests — the library-under-test case, where the entry is an empty program and the subject is the scope's final unit.
    LastUnitTests,
}

/// One registered test, as the runner reports it: the path that names it, and its body as written — sliced from the span the authored body carries, empty when no span survives (a unit restored from a store may carry none).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TestRecord {
    pub path: String,
    pub body: String,
}

/// The declaration-ordered paths of a module's registered tests — how `curios test` and `wonder tests` name them, read off [`Module::tests`](curios_core::Module) so the two cannot disagree with the schedule.
pub fn declared_test_paths(module: &curios_core::Module) -> Vec<String> {
    module
        .tests
        .iter()
        .map(|test| {
            test.qualifier()
                .map(|qualifier| qualifier.join())
                .unwrap_or_default()
        })
        .collect()
}

/// The definition a registered test names, among the unit's items.
fn test_definition<'a>(
    items: &'a [curios_core::Item],
    test: &curios_core::Global,
) -> Option<&'a curios_core::Definition> {
    items.iter().find_map(|item| match item {
        curios_core::Item::Let(def) if def.name == *test => Some(def),
        _ => None,
    })
}

/// Each registered test as the tail schedules it, read off the definition that carries it: the arity of the lambda its declaration lowered to, and the span of the authored body — the lambda's interior, since the wrapper node is synthesized and spans nothing. One lookup serves the tail and the runner's records, so the two cannot disagree about what the tests are.
fn scheduled_tests(
    tests: &[curios_core::Global],
    items: &[curios_core::Item],
) -> Vec<curios_elab::ScheduledTest> {
    tests
        .iter()
        .map(|test| {
            let lambda = test_definition(items, test).and_then(|def| match &*def.body {
                curios_core::Subterm::Func(func) => Some(func),
                _ => None,
            });

            curios_elab::ScheduledTest {
                name: test.clone(),
                arity: lambda.map(|func| func.telescope.len()).unwrap_or(0),
                span: lambda.and_then(|func| func.telescope.terminal().span()),
            }
        })
        .collect()
}

/// The report metadata of each scheduled test: the path that names it, and its body as written, sliced from the span the authored body carries.
fn test_records(scheduled: &[curios_elab::ScheduledTest]) -> Vec<TestRecord> {
    scheduled
        .iter()
        .map(|test| TestRecord {
            path: test
                .name
                .qualifier()
                .map(|qualifier| qualifier.join())
                .unwrap_or_default(),
            body: test
                .span
                .as_ref()
                .map(|span| span.source.text[span.start..span.end].to_string())
                .unwrap_or_default(),
        })
        .collect()
}

pub(crate) fn elaborate_and_zonk<O>(
    budget: u64,
    scope: Prefix<'_>,
    syntax: &SyntaxRegistry,
    entrypoint: &Entrypoint,
    loader: &RootSource,
    tail: EntryTail,
    observe: &mut O,
) -> Result<(curios_core::Module, Term, ForeignStore, Vec<TestRecord>), CompileError>
where
    O: FnMut(Stage<'_>),
{
    curios_profile::profile!("elaborate_and_zonk");
    observe(Stage::Text(entrypoint));

    let text = scope.text();
    let cores = scope.cores();
    let LoweredEntry {
        core: mut lowered,
        metavariable_floor: mut metavars,
        universe_floor,
        foreigns: user_foreigns,
        unbound,
        imports,
    } = into_core_with_prelude(entrypoint, loader, &text, syntax)
        .map_err(|error| CompileError::Failure(vec![error.report()]))?;

    // The test tail is synthesized in Core, on the lowered module, and checked exactly as an authored one is: `type_: None` routes it into the `Io({})` expectation below, and the tail's single list-element hole is minted at the module's metavariable floor so elaboration solves it bidirectionally from `Test/main`'s parameter like any written literal's. The records for the runner are read off the same definitions the tail schedules, so the two cannot disagree about what the tests are.
    let scheduled = match tail {
        EntryTail::Authored => Vec::new(),
        EntryTail::Tests => scheduled_tests(&lowered.tests, &lowered.items),
        EntryTail::LastUnitTests => match cores.last() {
            Some(core) => scheduled_tests(&core.tests, &core.items),
            None => Vec::new(),
        },
    };
    let records = test_records(&scheduled);
    if tail != EntryTail::Authored {
        let body = curios_elab::test_program_tail(syntax, &scheduled, metavars);
        metavars += 1;
        lowered.entry = Some(curios_core::Entrypoint { body, type_: None });
    }

    observe(Stage::Core(&lowered));

    // The entrypoint contract, as an ordinary expectation rather than a judgment after the fact: a program *is* a description of doing something and yielding nothing. An embedder that states its own type still gets it — that is how the typecheck-only fixtures reach both checkers with deliberately odd tails.
    //
    // `Io({})` is closed, which is what makes this a `Mode::Check` at all. Checking against `Io(?T)` would need a metavariable minted before the elaboration context exists, and that is why this contract used to be a post-hoc head test on the inferred type instead. Stating the unit payload removes the metavariable, and checking rather than inferring is what lets a tail spell itself `Io/pure(())` — the payload comes from the expectation exactly as it does under a written match motive.
    let core_mode = match lowered
        .entry
        .as_ref()
        .and_then(|entry| entry.type_.as_ref())
    {
        Some(type_) => Mode::Check(type_.clone()),
        None => Mode::Check(Term::intrinsic(Intrinsic::io_type(Term::tuple_type_unit()))),
    };

    let mut context = Context::new(budget, *syntax);
    context.set_imports(imports.clone());
    let (module, core_type) = elaborate_and_zonk_unit(
        &mut context,
        Established::over(&cores),
        &lowered,
        metavars,
        universe_floor,
        core_mode,
    )
    .map_err(|error| {
        CompileError::of(
            &error,
            error.reports_with_hints(&lowered, &cores, &unbound, &imports),
        )
    })?;

    observe(Stage::CoreElab(&module));

    // The entry is the unit with an entrypoint, and `elaborate_and_zonk` is only ever asked for one.
    let core_type = core_type.expect("the entrypoint's type comes back with its body");

    Ok((module, core_type, user_foreigns, records))
}

/// Everything [`compile_entrypoint`] decides *about* a program before it builds anything from it: lowered, elaborated, zonked, and judged by the kernel, with a refusal from either checker reported as the compile path reports it. What a question about a program's correctness is answered by — the stages below this one produce the program, and change no verdict.
pub fn check_entrypoint(
    budget: u64,
    scope: Prefix<'_>,
    syntax: &SyntaxRegistry,
    entrypoint: &Entrypoint,
    loader: &RootSource,
) -> Result<curios_core::Module, CompileError> {
    check_observed(
        budget,
        scope,
        syntax,
        entrypoint,
        loader,
        EntryTail::Authored,
        &mut |_| {},
    )
    .map(|(module, _core_type, _foreigns, _records)| module.into_module())
}

/// [`check_entrypoint`] with the stages it passes observed, and the entry's type and foreign rows kept for the lowering that follows it.
fn check_observed<O>(
    budget: u64,
    scope: Prefix<'_>,
    syntax: &SyntaxRegistry,
    entrypoint: &Entrypoint,
    loader: &RootSource,
    tail: EntryTail,
    observe: &mut O,
) -> Result<
    (
        curios_core::Zonked<curios_core::Module>,
        Term,
        ForeignStore,
        Vec<TestRecord>,
    ),
    CompileError,
>
where
    O: FnMut(Stage<'_>),
{
    let (module, core_type, foreigns, records) =
        elaborate_and_zonk(budget, scope, syntax, entrypoint, loader, tail, observe)?;

    // The zonk evidence the kernel and erasure consume, established where the module is final. A refusal here is a compiler invariant — `zonk_module` just enforced the same claim — surfacing as a failure rather than a panic.
    let module = curios_core::Zonked::project(&module)
        .map_err(|refusal| CompileError::failure(refusal.to_string()))?;

    // The independent kernel's second opinion, on the compile path: each unit in scope was walked when it was built and arrives here as environment, so only what it does not already answer for is judged — a refusal fails the compile.
    {
        curios_profile::profile!("recheck");
        if let Some(verdict) = recheck(&module, budget, scope, syntax).into_iter().next() {
            let refusal = verdict
                .error
                .format_with(module.as_module(), &scope.cores());
            return Err(CompileError::failure(match &verdict.name {
                Some(name) => format!("the kernel refused {name}: {refusal}"),
                None => format!("the kernel refused the entrypoint: {refusal}"),
            }));
        }
    }

    Ok((module, core_type, foreigns, records))
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
        .map_err(|error| CompileError::Failure(vec![error.report()]))?;

    let mut context = Context::new(budget, *syntax);
    context.set_imports(lowered.imports().clone());
    let (core, _body_type) = elaborate_and_zonk_unit(
        &mut context,
        Established::over(&cores),
        lowered.core(),
        lowered.metavariable_floor(),
        lowered.universe_floor(),
        Mode::Infer,
    )
    .map_err(|error| {
        CompileError::of(
            &error,
            error.reports_with_hints(lowered.core(), &cores, lowered.unbound(), lowered.imports()),
        )
    })?;

    let core = curios_core::Zonked::project(&core)
        .map_err(|refusal| CompileError::failure(refusal.to_string()))?;

    if let Some(verdict) = recheck(&core, budget, scope, syntax).into_iter().next() {
        let refusal = verdict.error.format_with(core.as_module(), &cores);
        return Err(CompileError::failure(match &verdict.name {
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
    .map_err(|error| CompileError::Failure(error.reports_with(core.as_module(), &cores)))?;

    let core = core.into_module();
    let binder_floor = derived_binder_floor(&core);

    Ok(Unit::new(lowered, core, ersd, binder_floor))
}

/// Where a judged unit is kept between compilations.
///
/// Declared here and implemented in `curios-package`, because the fold is what consults one and this crate must never learn what a project is. What crosses the boundary is a unit — never a path, never a manifest, and never a key this crate would have to know how to build.
///
/// **A unit handed back is one that was judged when it was recorded, and taking it is taking that on trust.** That is a change to what the compiler believes rather than an optimization of what it does, which is why the argument for it lives in [Cached verdicts](../../documentation/soundness/admission-without-judgment/cached-verdicts.md) rather than here: the implementation chooses the key, and the key is the whole of what the argument rests on.
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
/// Production erases onto the archived erased prelude: it is restored and replayed, only the entry's own items erase, the Ersd optimizer shrinks and rebases the module, and the lowering into Cont makes every encoding decision once (see `curios_ersd::lower_to_cont`).
///
/// **`loader` is borrowed rather than taken, so the caller still owns it when this returns.** Resolution records what it read through `&self` — the log is interior-mutable precisely so that lowering never has to thread `&mut` — and a caller filing what this compilation produced needs that log *after* the fold, exactly as it needs the cache handle's refusal after the fold. Consuming the loader would put the read set out of reach at the only moment it is worth anything, and nothing here wants ownership of it.
pub fn compile_entrypoint<O>(
    budget: u64,
    scope: Prefix<'_>,
    syntax: &SyntaxRegistry,
    entrypoint: &Entrypoint,
    loader: &RootSource,
    observe: O,
) -> Result<(curios_wasm::Module, ForeignStore), CompileError>
where
    O: FnMut(Stage<'_>),
{
    compile_with_tail(
        budget,
        scope,
        syntax,
        entrypoint,
        loader,
        EntryTail::Authored,
        observe,
    )
    .map(|(module, foreigns, _records)| (module, foreigns))
}

/// [`compile_entrypoint`] with the unit compiled as a test program: the authored tail — or a module's absence of one — is replaced by the synthesized `Test/main([...])` over the registered tests `tail` selects, and everything else is the ordinary pipeline, kernel judgment included. No file is written and nothing about the surface changes; which tail a unit compiles under is the caller's question alone. Beside the program, the caller gets one [`TestRecord`] per scheduled test, in schedule order — the report metadata execution alone cannot recover.
pub fn compile_unit_as_tests<O>(
    budget: u64,
    scope: Prefix<'_>,
    syntax: &SyntaxRegistry,
    entrypoint: &Entrypoint,
    loader: &RootSource,
    tail: EntryTail,
    observe: O,
) -> Result<(curios_wasm::Module, ForeignStore, Vec<TestRecord>), CompileError>
where
    O: FnMut(Stage<'_>),
{
    compile_with_tail(budget, scope, syntax, entrypoint, loader, tail, observe)
}

fn compile_with_tail<O>(
    budget: u64,
    scope: Prefix<'_>,
    syntax: &SyntaxRegistry,
    entrypoint: &Entrypoint,
    loader: &RootSource,
    tail: EntryTail,
    mut observe: O,
) -> Result<(curios_wasm::Module, ForeignStore, Vec<TestRecord>), CompileError>
where
    O: FnMut(Stage<'_>),
{
    curios_profile::profile!("compile_entrypoint");
    let (module, core_type, foreigns, records) = check_observed(
        budget,
        scope,
        syntax,
        entrypoint,
        loader,
        tail,
        &mut observe,
    )?;
    let cores = scope.cores();

    let ersd_module = erase_unit(
        &mut Context::new(budget, *syntax),
        Resumed::of(&cores, scope.arena()),
        &module,
        Some(&core_type),
    )
    .map(|erased| erased.into_module())
    .map_err(|error| CompileError::Failure(error.reports_with(module.as_module(), &cores)))?;

    // Every unit's rows, not the entry's alone: an embedder binds one registry, and a dependency that declares a `foreign` row has to reach it. Disjoint by mount, so the union cannot collide.
    let mut all_foreigns = scope.foreigns();
    all_foreigns.absorb(&foreigns);

    Ok((
        lower_from_ersd(ersd_module, &mut observe),
        all_foreigns,
        records,
    ))
}
