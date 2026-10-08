//! The stage sequence itself: [`compile_entrypoint`] and the two halves it is assembled from — the type-checking prologue and the lowering back half — each observing every [`Stage`] it produces before moving past it.

use {
    super::{Stage, compile_unit_over},
    curios_abi::ForeignStore,
    curios_cert::{
        Globals, Kernel, Rechecked, Verdict, certify_module, certify_program,
        recheck_program_measured,
    },
    curios_core::{Anchors, Certification, Consumption, Free, Global, Intrinsic, Program, Term},
    curios_elab::{
        Context, Erasures, Established, FinalizedProgram, Tail, elaborate_and_zonk_program,
        elaborate_and_zonk_program_reporting, elaborate_and_zonk_unit, erase_program, erase_unit,
    },
    curios_emit::into_wasm,
    curios_ersd::lower_to_cont,
    curios_text::{
        BrokenItem, Entrypoint, Lint, LoweredEntry, PreparedText, RootSource, UnitSource,
        into_core_unit, into_core_with_prelude,
    },
    curios_unit::{Predecessors, Uncertified, Unit},
    curios_utilities::{Qualifier, Report, SyntaxRegistry},
    std::{cell::LazyCell, collections::BTreeSet, fmt, rc::Rc},
};

/// What a unit's lowering decided beside its module: its lints, and the prefix of every mount some reference of it resolved into — see [`Lint`]. Taken off the lowering before elaboration consumes it and credited with the binders elaboration's proofs read however elaboration ended, so a question about a program has them whether or not elaboration accepted it.
#[derive(Debug, Clone, Default)]
pub struct Findings {
    pub lints: Vec<Lint>,
    pub reached: BTreeSet<Qualifier>,
}

impl Findings {
    /// A stored unit's, as its lowering left them and its elaboration credited them.
    pub fn of_text(text: &PreparedText) -> Self {
        Self {
            lints: text.lints().to_vec(),
            reached: text.reached().clone(),
        }
    }

    fn take(lowered: &mut LoweredEntry) -> Self {
        Self {
            lints: std::mem::take(&mut lowered.lints),
            reached: std::mem::take(&mut lowered.reached),
        }
    }

    /// Drop the `unused-binder` lint of every binder a proof the elaborator wrote read — see [`PreparedText::credit`].
    fn credit(&mut self, credited: &BTreeSet<(Option<curios_core::Global>, u32)>) {
        self.lints.retain(|lint| !lint.is_credited(credited));
    }
}

/// What checking a program decides: the entry's findings, and the verdict — which is a `Result` of its own because a lint is not a refusal, and an entry that lowers has findings whatever elaboration then says about it.
#[derive(Debug)]
pub struct Checked {
    pub entry: Findings,
    pub verdict: Result<curios_core::Program, CompileError>,
}

/// A compile failure, split for process-level reporting: a written-goal batch is *incomplete* development state, everything else a hard *failure*, and hard failures beside written goals are *mixed*. The CLI maps them to exit codes — 2 for incomplete, 1 for the other two, since a refusal is one whatever stands beside it — so tooling can distinguish "here is your goal batch" from "something is wrong" without parsing stderr.
///
/// All three carry what was said as located [`Report`]s — one per goal for a batch, one per refusal otherwise — rather than as text, so a consumer placing a diagnostic in a buffer reads the span instead of parsing the `-->` header back out. The text is the reports rendered, through `Display` or the `String` conversion, and it is exactly the text the CLI prints: the located form and the printed form are one value.
#[derive(Debug)]
pub enum CompileError {
    Incomplete(Vec<Report>),
    Failure(Vec<Report>),
    /// The first `failures` reports are the hard ones and the rest the goals, each half in item order. A transport placing diagnostics reads the split off the count; a reader of the printed form gets the refusals first, since those are what stops the program compiling once the goals are filled.
    Mixed {
        reports: Vec<Report>,
        failures: usize,
    },
}

impl CompileError {
    /// Classify a front-end error by [`curios_elab::Error::is_incomplete`], member by member: incomplete when every member of a batch is, a failure when none is, mixed otherwise. `reports` locates one member.
    pub(crate) fn of(
        error: &curios_elab::Error,
        reports: impl Fn(&curios_elab::Error) -> Vec<Report>,
    ) -> Self {
        let (goals, refusals): (Vec<_>, Vec<_>) =
            error.each().partition(|member| member.is_incomplete());
        let located = |members: Vec<&curios_elab::Error>| {
            members.into_iter().flat_map(&reports).collect::<Vec<_>>()
        };

        match (refusals.is_empty(), goals.is_empty()) {
            (true, _) => Self::Incomplete(located(goals)),
            (false, true) => Self::Failure(located(refusals)),
            (false, false) => {
                let mut reports = located(refusals);
                let failures = reports.len();
                reports.extend(located(goals));
                Self::Mixed { reports, failures }
            }
        }
    }

    /// `failures` — hard ones, reported first — ahead of what this error carries; the classification follows, a goal batch behind a failure being mixed.
    pub fn after_failures(self, mut failures: Vec<Report>) -> Self {
        let count = failures.len();
        match self {
            Self::Incomplete(goals) => {
                failures.extend(goals);
                Self::Mixed {
                    reports: failures,
                    failures: count,
                }
            }
            Self::Failure(more) => {
                failures.extend(more);
                Self::Failure(failures)
            }
            Self::Mixed {
                reports,
                failures: hard,
            } => {
                failures.extend(reports);
                Self::Mixed {
                    reports: failures,
                    failures: count + hard,
                }
            }
        }
    }

    /// A hard failure about nothing in particular — a store, a manifest, a backend — which is what every message that arrives as plain text is.
    pub fn failure(message: impl Into<String>) -> Self {
        Self::Failure(vec![Report::unlocated(message)])
    }

    /// What was said, located, whichever way it was classified.
    pub fn reports(&self) -> &[Report] {
        match self {
            Self::Incomplete(reports) | Self::Failure(reports) | Self::Mixed { reports, .. } => {
                reports
            }
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

/// What a unit holding items the parser could not read compiles to: a refusal listing every broken item, ahead of whatever elaboration said of the rest — a broken item is refused as a refused item is, and the reader fixing it is owed the goals and refusals beside it in the same run. Nothing past elaboration is reached, so no unit is judged, erased or filed with a hole in it.
pub(crate) fn with_broken<T>(
    broken: &[BrokenItem],
    result: Result<T, CompileError>,
) -> Result<T, CompileError> {
    if broken.is_empty() {
        return result;
    }

    let reports = broken.iter().map(|item| item.report.clone()).collect();
    Err(match result {
        Ok(_) => CompileError::Failure(reports),
        Err(error) => error.after_failures(reports),
    })
}

/// Put `program` to the independent kernel with `predecessors` already in scope, so only what its units do not already answer for is judged — their own items resting on the verdict recorded when each was built.
///
/// The [`Globals`] environment is assembled here rather than in `curios-unit`, because a unit is defined to stay below the kernel and cannot name it. Every caller that wants the compile path's rechecking gets this one rather than reconstructing it.
pub fn recheck(
    program: &curios_core::Zonked<Program>,
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
) -> Vec<Verdict> {
    certify_program(program, budget, &globals(predecessors), *syntax).verdicts
}

/// The kernel's walk over one unit, with the record it leaves of what it concluded — what a unit files beside its definitions for a later walk to read. See `curios_cert::certify_module`.
pub(crate) fn certify(
    module: &curios_core::Zonked<curios_core::Module>,
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
) -> Rechecked {
    certify_module(module, budget, &globals(predecessors), *syntax)
}

/// [`recheck`], handing the walk's own kernel to `read` for a measurement to take what it wants of it rather than only its verdicts. See `curios_cert::recheck_program_measured`.
///
/// A reader rather than the kernel handed back: the kernel reads its scope where the units hold it, so it lives no longer than the environment assembled here.
pub fn recheck_measured<T>(
    program: &curios_core::Zonked<Program>,
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    read: impl FnOnce(&Kernel<'_>) -> T,
) -> (Vec<Verdict>, T) {
    let globals = globals(predecessors);
    let (rechecked, kernel) = recheck_program_measured(program, budget, &globals, *syntax);

    (rechecked.verdicts, read(&kernel))
}

/// Where a report about the unit lowered as `text` is placed: the unit's own declarations first, then those of the units in its scope.
pub(crate) fn anchors_of<'a>(
    text: &'a PreparedText,
    predecessors: Predecessors<'a>,
) -> Vec<&'a Anchors> {
    std::iter::once(text.anchors())
        .chain(predecessors.anchors())
        .collect()
}

/// The kernel's environment for `predecessors`: every unit in scope where it stands, read by the places it keeps for its names, with the record the certifier filed with it. Nothing of a unit is copied.
pub(crate) fn globals(predecessors: Predecessors<'_>) -> Globals<'_> {
    let mut globals = Globals::default();
    for unit in predecessors.units() {
        globals.mount(unit.core(), unit.places(), unit.certification());
    }

    globals
}

/// The compile error a kernel verdict is reported as: the refusal, named for the item it is about where there is one.
pub(crate) fn kernel_refusal(
    verdict: &Verdict,
    module: &curios_core::Module,
    predecessors: &[&curios_core::Module],
    syntax: &SyntaxRegistry,
) -> CompileError {
    let refusal = verdict.error.format_with(module, predecessors, syntax);

    CompileError::failure(match &verdict.name {
        Some(name) => format!("the kernel refused {name}: {refusal}"),
        None => format!("the kernel refused a unit: {refusal}"),
    })
}

/// What type-checking refused a program with: the elaborator's own error where it was the elaborator that refused, beside the refusal as a compilation reports it.
#[derive(Debug)]
pub struct Refused {
    /// The elaborator's error as it raised it. Absent where the program never reached a verdict of the elaborator's alone: it did not lower, or holds an item the parser could not read and nothing else failed.
    pub error: Option<Box<curios_elab::Error>>,
    pub reported: CompileError,
}

impl fmt::Display for Refused {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.reported.fmt(formatter)
    }
}

/// One erasure obligation the elaborator raised over a program it built: its error as raised, and the diagnostic a compilation prints for it.
#[derive(Debug)]
pub struct Obligation {
    pub error: curios_elab::Error,
    pub reported: String,
}

/// A program type-checked and not yet put to the kernel.
#[derive(Debug)]
pub struct Typechecked {
    pub program: Program,
    /// The (T) and (V) obligations the elaborator decided over the program, reported rather than raised. Empty when it carries none.
    pub obligations: Vec<Obligation>,
    /// What the heaviest declaration consumed: the units and peak depth `DEFAULT_STEP_BUDGET` is set against, and the only figures that say whether a program's cost is depth or work. A measurement's, which nothing in the compiler reads.
    pub consumption: Consumption,
}

/// Lower and type-check `entrypoint` as a compilation does, stopping short of the kernel, with the erasure obligations reported rather than raised.
///
/// The elaborated program comes back even when this stage's own (T)/(V) verdicts refuse it, which is what lets one program be put to *both* checkers: `curios-cert` decides the same two obligations independently, and a program only this side refuses would otherwise yield no module for the kernel to judge — leaving the most consequential disagreement, the trusted base resting on an elaborator-only analysis, unobservable. Nothing else about type-checking differs from the compile path, the entry's `Io({})` contract included: both elaborate through `elaborate_entry`, so there is one statement of what a program's tail is checked against.
///
/// Each refusal carries the elaborator's error as it raised it beside the diagnostic a compilation prints, since rendering is where a consumer that matches the error's variant would lose it. A caller that wants the kernel's opinion on the result puts it to [`recheck`], which supplies the same environment `compile_entrypoint` does rather than re-walking the standard library.
pub fn typecheck_entrypoint(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    entrypoint: &Entrypoint,
    loader: &RootSource,
) -> Result<Typechecked, Refused> {
    let lowered =
        lower_entry(predecessors, syntax, entrypoint, loader, &mut |_| {}).map_err(|reported| {
            Refused {
                error: None,
                reported,
            }
        })?;

    let established = Established::placed(&predecessors.placed());
    let mut context = Context::new(budget, *syntax);
    let ((program, _, _, _), obligations) = elaborate_entry(
        &mut context,
        &established,
        &predecessors.text(),
        lowered,
        EntryTail::Authored,
        Obligations::Reported,
        &mut |_| {},
    )?;

    Ok(Typechecked {
        program,
        obligations,
        consumption: context.heaviest_declaration(),
    })
}

/// Why a program's elaborated entry is present wherever it is read: elaboration withholds an entry, recording no refusal, only when it reaches an item the lowering could not read, and `with_broken` turns any such item into a failure first.
const WITHHELD_ONLY_WHEN_BROKEN: &str = "an entry is withheld only for reaching an item that did not parse, which fails the compile first";

/// Which final term a compilation checks, and what it is judged at: the one the author wrote, as a program or as a proof, or the test tail synthesized from a unit's registered tests. A policy rather than a boolean at each call site, so the program shapes a unit can compile to are named where they diverge — and the whole of what an entry can be judged at, since nothing else states its type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EntryTail {
    /// The authored entrypoint, exactly as parsed — the ordinary program, judged at `Io({})`.
    Authored,
    /// The authored entrypoint as an alleged proof of the empty proposition, judged at `False`: how the soundness board puts a witness, through [`examine_entrypoint`].
    Proof,
    /// The synthesized `Test/main([...])` over the entry unit's own registered tests, replacing an executable's authored tail. A unit with no tests gets `Test/main([])`, which runs nothing and exits 0.
    Tests,
    /// The same synthesized tail over the last mounted unit's registered tests — the library-under-test case, where the entry is an empty program and the subject is the last predecessor.
    LastUnitTests,
}

/// One registered test, as the runner reports it: the path that names it, and its body as written — sliced from where the anchors in hand place the extent its authored body carries, empty where it carries none (a unit restored from a store may carry none) or none of them holds its declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TestRecord {
    pub path: String,
    pub body: String,
}

/// The declaration-ordered paths of a module's registered tests — how `curios test` and `wonder tests` name them, read off [`Module::tests`](curios_core::Module) so the two cannot disagree with the schedule.
pub fn declared_test_paths(module: &curios_core::Module) -> Vec<String> {
    module.tests.iter().map(curios_core::Global::path).collect()
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

/// Each registered test as the tail schedules it, read off the lowered definition that carries it: the plicity vector of the lambda its declaration lowered to, and the extent of the authored body — the lambda's interior, since the wrapper node is synthesized and was written nowhere. One lookup serves the tail and the runner's records, so the two cannot disagree about what the tests are.
fn scheduled_tests(
    tests: &[curios_core::Global],
    items: &[curios_core::Item],
) -> Vec<curios_elab::ScheduledTest> {
    tests
        .iter()
        .map(|test| {
            // The declaration lowers to a `() -> Test` thunk, so the authored body is the lambda's terminal — the extent a failing test's record is sliced from.
            let lambda = test_definition(items, test).and_then(|def| match &*def.body {
                curios_core::Subterm::Func(func) => Some(func),
                _ => None,
            });

            curios_elab::ScheduledTest {
                name: *test,
                extent: lambda.and_then(|func| func.telescope.terminal().extent()),
            }
        })
        .collect()
}

/// The report metadata of each scheduled test: the path that names it, and its body as written, sliced from where `anchors` place the extent the authored body carries.
fn test_records(scheduled: &[curios_elab::ScheduledTest], anchors: &[&Anchors]) -> Vec<TestRecord> {
    scheduled
        .iter()
        .map(|test| TestRecord {
            path: test.name.path(),
            body: test
                .extent
                .and_then(|extent| extent.resolved(anchors))
                .map(|span| span.source.text[span.start..span.end].to_string())
                .unwrap_or_default(),
        })
        .collect()
}

/// The type-checking prologue of [`compile_entrypoint`] (and the tests' typecheck-only path): lower to core, elaborate (checking against the entrypoint's type when it carries one, else synthesizing), then zonk metavariable solutions in so the module is meta-free — the `elaborate → zonk` half of the `elaborate → zonk → erase` data flow. Elaboration is authoritative: it returns a rebuilt module (lambda domains solved, binders re-closed), and it is *that* module — not the lowered one — that zonk makes meta-free. `zonk` is also where an unsolved hole is rejected, so a program that merely *type-checks* is fully validated by the time this returns. Elaboration and zonking share one context (the solutions live in its solution store); the returned module is self-contained, so the caller's `erase` runs over a fresh one.
///
/// The `/sys`/`/std` prelude is neither lowered nor elaborated per call: prepared Text state is merged with the user graph, then the archived Core prelude is replayed into the elaboration context and only the entry's own items are type-checked.
#[cfg(test)]
pub(crate) fn elaborate_and_zonk<O>(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    entrypoint: &Entrypoint,
    loader: &RootSource,
    tail: EntryTail,
    observe: &mut O,
) -> Result<Elaborated, CompileError>
where
    O: FnMut(Stage<'_>),
{
    let lowered = lower_entry(predecessors, syntax, entrypoint, loader, observe)?;
    elaborate_lowered(
        budget,
        predecessors,
        syntax,
        lowered,
        tail,
        Obligations::Raised,
        observe,
    )
    .1
    .map(|(elaborated, _)| elaborated)
    .map_err(|refused| refused.reported)
}

/// The lowering half of the prologue: the surface tree to a core module, observed as the `text` rung.
fn lower_entry<O>(
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    entrypoint: &Entrypoint,
    loader: &RootSource,
    observe: &mut O,
) -> Result<LoweredEntry, CompileError>
where
    O: FnMut(Stage<'_>),
{
    observe(Stage::Text(entrypoint));

    let text = predecessors.text();
    into_core_with_prelude(entrypoint, loader, &text, syntax)
        .map_err(|error| CompileError::Failure(vec![error.report()]))
}

/// An entry elaborated: its program, the `foreign` rows it declares, one record per test it schedules, and where its declarations lie, for what a later stage reports of it.
type Elaborated = (Program, ForeignStore, Vec<TestRecord>, Anchors);

/// The elaborating half of the prologue, over an entry already lowered: the lowering's findings, credited with what elaboration's proofs read whatever its verdict, beside that verdict.
fn elaborate_lowered<O>(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    mut lowered: LoweredEntry,
    tail: EntryTail,
    obligations: Obligations,
    observe: &mut O,
) -> (Findings, Result<(Elaborated, Vec<Obligation>), Refused>)
where
    O: FnMut(Stage<'_>),
{
    curios_profile::profile!("elaborate_and_zonk");

    let mut findings = Findings::take(&mut lowered);
    let established = Established::placed(&predecessors.placed());
    let mut context = Context::new(budget, *syntax);
    let verdict = elaborate_entry(
        &mut context,
        &established,
        &predecessors.text(),
        lowered,
        tail,
        obligations,
        observe,
    );
    findings.credit(&context.credited());
    (findings, verdict)
}

/// Whether the erasure obligations elaboration decides stop it, as a compilation has them, or come back beside the program, for a caller that puts the same program to the kernel.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Obligations {
    Raised,
    Reported,
}

/// The one elaboration of an entry, in `context`, which the caller reads its credits and its consumption from however this ends: the compile path's and the type-checking path's alike, so the entry's contract is stated once. Beside the program come the obligations it reported, none where they are raised.
///
/// `scope` is the text part of each unit in scope: where its declarations lie, which a report placed in one of them is resolved against, after the entry's own, and the tables the path table a report spells a name by is made from.
fn elaborate_entry<'s, O>(
    context: &mut Context<'s>,
    established: &'s Established<'_>,
    scope: &[&PreparedText],
    lowered: LoweredEntry,
    tail: EntryTail,
    obligations: Obligations,
    observe: &mut O,
) -> Result<(Elaborated, Vec<Obligation>), Refused>
where
    O: FnMut(Stage<'_>),
{
    let cores = established.modules();
    // The registry the context was made with, which is the one its elaboration synthesizes names from.
    let syntax = &context.syntax();
    let LoweredEntry {
        program: mut lowered,
        minted,
        foreigns: user_foreigns,
        unbound,
        names,
        lints: _,
        reached: _,
        broken,
        anchors: own,
    } = lowered;
    let anchors = std::iter::once(&own)
        .chain(scope.iter().map(|text| text.anchors()))
        .collect::<Vec<_>>();
    // Made by the first report that spells a name, and by no elaboration that reports nothing.
    let spellings = LazyCell::new(|| Rc::new(names.spellings(scope)));

    // A test program's tail is synthesized by the elaborator, in Core, once the unit's items are defined — it schedules those definitions and chooses each test's discharge from their elaborated form, so it cannot exist before them. What is decided here is only *which* tests it schedules; the elaborator states the synthesized entry's `Io({})` type, as this stage states an authored entry's below. The records for the runner are read off the same lowered definitions the tail schedules, so the two cannot disagree about what the tests are.
    let scheduled = match tail {
        EntryTail::Authored | EntryTail::Proof => Vec::new(),
        EntryTail::Tests => scheduled_tests(&lowered.module.tests, &lowered.module.items),
        EntryTail::LastUnitTests => match cores.last() {
            Some(core) => scheduled_tests(&core.tests, &core.items),
            None => Vec::new(),
        },
    };
    let records = test_records(&scheduled, &anchors);

    observe(Stage::Core(&lowered));

    // The entrypoint contract, stated in the entry: a program *is* a description of doing something and yielding nothing, so an authored entry states `Io({})`, and elaboration checks the body against it, the kernel rechecks it and erasure seals at it — each reading it off the entry. An entry put as a proof states the empty proposition instead, and those two are the whole choice: the surface entry carries no type, so no caller states another. A test program's tail replaces the written entry, and the elaborator that synthesizes it states its type.
    //
    // `Io({})` is closed, which is what lets it be stated before elaboration at all. `Io(?T)` would need a metavariable minted before the elaboration context exists, which would leave the contract a post-hoc head test on the inferred type. Stating the unit payload removes the metavariable, and checking rather than inferring is what lets a tail spell itself `Io/pure(())` — the payload comes from the expectation exactly as it does under a written match motive.
    match tail {
        EntryTail::Authored => {
            lowered.entry.type_ =
                Some(Term::intrinsic(Intrinsic::io_type(Term::tuple_type_unit())));
        }
        EntryTail::Proof => {
            let empty = Global::Authored(syntax.proof.false_type.qualifier());
            lowered.entry.type_ = Some(Term::free_var(&Free::from(&empty)));
        }
        EntryTail::Tests | EntryTail::LastUnitTests => {}
    }
    let elab_tail = match tail {
        EntryTail::Authored | EntryTail::Proof => Tail::Entry(&lowered.entry),
        EntryTail::Tests | EntryTail::LastUnitTests => Tail::Tests(&scheduled),
    };

    context.set_imports(names.imports().clone());
    context.set_broken(broken.iter().filter_map(|item| item.declares).collect());
    let elaborated = match obligations {
        Obligations::Raised => {
            elaborate_and_zonk_program(context, established, &lowered.module, &minted, elab_tail)
                .map(|(module, entry)| (module, entry, Vec::new()))
        }
        Obligations::Reported => elaborate_and_zonk_program_reporting(
            context,
            established,
            &lowered.module,
            &minted,
            elab_tail,
        )
        .map(
            |FinalizedProgram {
                 module,
                 entry,
                 obligations,
             }| (module, entry, obligations),
        ),
    };
    // The error is kept as raised beside what it renders to: rendering is where its variant is lost.
    let (error, elaborated) = match elaborated {
        Ok(built) => (None, Ok(built)),
        Err(error) => {
            let reported = CompileError::of(&error, |member| {
                member.reports_with_hints(
                    &lowered.module,
                    cores,
                    syntax,
                    &unbound,
                    &spellings,
                    &anchors,
                )
            });

            (Some(Box::new(error)), Err(reported))
        }
    };
    let (module, entry, obligations) =
        with_broken(&broken, elaborated).map_err(|reported| Refused { error, reported })?;

    let obligations = obligations
        .into_iter()
        .map(|error| Obligation {
            reported: error.format_with_hints(
                &lowered.module,
                cores,
                syntax,
                &unbound,
                &spellings,
                &anchors,
            ),
            error,
        })
        .collect();
    let program = Program {
        module,
        entry: entry.expect(WITHHELD_ONLY_WHEN_BROKEN),
    };

    observe(Stage::CoreElab(&program));

    Ok(((program, user_foreigns, records, own), obligations))
}

/// Everything [`compile_entrypoint`] decides *about* a program before it builds anything from it: lowered, elaborated, zonked, judged by the kernel, and **erased**, with a refusal from any of them reported as the compile path reports it. What a question about a program's correctness is answered by.
///
/// **Erasure is a verdict, which is why the line is drawn under it rather than under the kernel.** It narrows every numeral into the erased carriers and refuses one that does not fit, and it hands the module to the erased representation's verifier, which rejects the recursion classes the language does not admit — a mutual value group no forcing order satisfies among them. Stopping at the kernel would let `wonder diagnostics` report a clean program that `run` then refuses, which is the one thing that query may not do. Below here nothing decides: `lower_from_ersd` — private, so named here rather than linked — returns no `Result` at all.
///
/// The erased module is discarded. Producing it is the whole cost, and the caller wants the Core module.
pub fn check_entrypoint(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    entrypoint: &Entrypoint,
    loader: &RootSource,
    tail: EntryTail,
) -> Result<Checked, CompileError> {
    let lowered = lower_entry(predecessors, syntax, entrypoint, loader, &mut |_| {})?;
    let (entry, judged) = check_lowered(budget, predecessors, syntax, lowered, tail, &mut |_| {});
    let verdict = judged.and_then(|judged| {
        erase_checked(budget, predecessors, syntax, &judged)?;
        Ok(judged.program.as_program().clone())
    });

    Ok(Checked { entry, verdict })
}

/// An entry lowered, elaborated, zonked and accepted by the kernel: what [`erase_checked`] erases, with what the lowering after it needs of the entry.
struct Judged {
    /// The program, zonked, every item and its entry accepted by the kernel — the entry stating the type the body was checked against, which erasure seals it at.
    program: curios_core::Zonked<Program>,
    /// The `foreign` rows the entry declares.
    foreigns: ForeignStore,
    /// One per scheduled test, when the entry is compiled as a test program.
    records: Vec<TestRecord>,
    /// The certifier's record of the entry's own definitions, which marks the sealed program's functions beside every unit's.
    certification: Certification,
    /// Where the entry's declarations lie, for a refusal erasure places in one.
    anchors: Anchors,
}

/// The erase step both the check and the compile path take, so neither can hold a verdict the other does not.
///
/// The entry's own items and its final term are erased here, and linked with what they reach of the predecessors' erased forms: the module handed back holds that and no declaration the program does not reach.
///
/// The linked program's termination flags are marked here, from the record of everything it was erased from — every predecessor and the entry — and nowhere else: erasure marks nothing, and nothing reads a flag before the back half lowers what this returns. See `curios_ersd::Function::total`.
fn erase_checked(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    judged: &Judged,
) -> Result<curios_ersd::Module, CompileError> {
    let cores = predecessors.cores();
    let erased = predecessors.erased();

    let mut linked = erase_program(
        budget,
        *syntax,
        Erasures::of(&cores, &erased),
        &judged.program,
    )
    .map_err(|error| {
        let anchors = std::iter::once(&judged.anchors)
            .chain(predecessors.anchors())
            .collect::<Vec<_>>();

        CompileError::Failure(error.reports_with(
            &judged.program.as_program().module,
            &cores,
            syntax,
            &anchors,
        ))
    })?;
    for unit in predecessors.units() {
        linked.mark_total(unit.certification());
    }
    linked.mark_total(&judged.certification);

    Ok(linked.into_module())
}

/// [`check_entrypoint`] with the stages it passes observed, and the entry's foreign rows kept for the lowering that follows it.
fn check_observed<O>(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    entrypoint: &Entrypoint,
    loader: &RootSource,
    tail: EntryTail,
    observe: &mut O,
) -> Result<Judged, CompileError>
where
    O: FnMut(Stage<'_>),
{
    let lowered = lower_entry(predecessors, syntax, entrypoint, loader, observe)?;
    // Compiling reports no lint, so the findings go unread.
    check_lowered(budget, predecessors, syntax, lowered, tail, observe).1
}

/// The back half of [`check_observed`], from a lowered entry: elaborate, zonk, and put the result to the kernel — beside the lowering's findings, credited by elaboration.
fn check_lowered<O>(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    lowered: LoweredEntry,
    tail: EntryTail,
    observe: &mut O,
) -> (Findings, Result<Judged, CompileError>)
where
    O: FnMut(Stage<'_>),
{
    let (findings, put) = put_lowered(
        budget,
        predecessors,
        syntax,
        lowered,
        tail,
        Obligations::Raised,
        observe,
    );
    (
        findings,
        put.map_err(|refused| refused.reported)
            .and_then(|put| put.accepted(predecessors, syntax)),
    )
}

/// An entry put to the elaborator and then to the kernel: the program, with the zonk evidence the kernel and erasure consume, and what each said of it beyond building it.
struct Put {
    program: curios_core::Zonked<Program>,
    foreigns: ForeignStore,
    records: Vec<TestRecord>,
    /// The erasure obligations the elaborator reported: none where they are raised.
    obligations: Vec<Obligation>,
    /// Every refusal the kernel reached.
    verdicts: Vec<Verdict>,
    certification: Certification,
    /// Where the entry's declarations lie.
    anchors: Anchors,
}

impl Put {
    /// The entry as the compile path takes it on: refused by the kernel's first verdict, or judged.
    fn accepted(
        self,
        predecessors: Predecessors<'_>,
        syntax: &SyntaxRegistry,
    ) -> Result<Judged, CompileError> {
        if let Some(verdict) = self.verdicts.first() {
            let refusal = verdict.error.format_with(
                &self.program.as_program().module,
                &predecessors.cores(),
                syntax,
            );
            return Err(CompileError::failure(match &verdict.name {
                Some(name) => format!("the kernel refused {name}: {refusal}"),
                None => format!("the kernel refused the entrypoint: {refusal}"),
            }));
        }

        Ok(Judged {
            program: self.program,
            foreigns: self.foreigns,
            records: self.records,
            certification: self.certification,
            anchors: self.anchors,
        })
    }
}

/// The one sequence every path that decides about a program takes, from a lowered entry: elaborate, zonk, and put the result to the kernel — beside the lowering's findings, credited by elaboration. `obligations` is the only thing a caller chooses: whether the elaborator's erasure obligations stop it, as a compilation has them, or come back beside the kernel's verdicts.
fn put_lowered<O>(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    lowered: LoweredEntry,
    tail: EntryTail,
    obligations: Obligations,
    observe: &mut O,
) -> (Findings, Result<Put, Refused>)
where
    O: FnMut(Stage<'_>),
{
    let (findings, elaborated) = elaborate_lowered(
        budget,
        predecessors,
        syntax,
        lowered,
        tail,
        obligations,
        observe,
    );
    (
        findings,
        elaborated.and_then(|((program, foreigns, records, anchors), obligations)| {
            // The zonk evidence the kernel and erasure consume, established where the program is final. A refusal here is a compiler invariant — zonk just enforced the same claim — surfacing as a failure rather than a panic.
            let program = curios_core::Zonked::project(&program).map_err(|refusal| Refused {
                error: None,
                reported: CompileError::failure(refusal.to_string()),
            })?;

            // The independent kernel's second opinion: each predecessor was walked when it was built and arrives here as environment, so only what it does not already answer for is judged.
            let rechecked = {
                curios_profile::profile!("recheck");
                certify_program(&program, budget, &globals(predecessors), *syntax)
            };

            Ok(Put {
                program,
                foreigns,
                records,
                obligations,
                verdicts: rechecked.verdicts,
                certification: rechecked.certification,
                anchors,
            })
        }),
    )
}

/// What each checker said of a program the elaborator built.
#[derive(Debug)]
pub struct Examined {
    /// The (T) and (V) obligations the elaborator decided over the program, reported rather than raised.
    pub obligations: Vec<Obligation>,
    /// Every refusal the kernel reached over the same program.
    pub verdicts: Vec<Verdict>,
}

/// Put `entrypoint` to both checkers as a proof of the empty proposition — lowering, elaboration at `False`, the kernel — and hand back what each said rather than stopping at the first refusal.
///
/// The steps are the compile path's own, taken in its order through `put_lowered`, with one difference: the elaborator's erasure obligations are reported rather than raised, so a program only it refuses still reaches the kernel. That is what a consumer asking which checker refused a program needs, and what [`check_entrypoint`] cannot say, since it stops at the first refusal. It stops under the kernel: erasure decides whether a program can be built, which is no part of whether its entry has the type it states.
pub fn examine_entrypoint(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    entrypoint: &Entrypoint,
    loader: &RootSource,
) -> Result<Examined, Refused> {
    let lowered =
        lower_entry(predecessors, syntax, entrypoint, loader, &mut |_| {}).map_err(|reported| {
            Refused {
                error: None,
                reported,
            }
        })?;
    let (_, put) = put_lowered(
        budget,
        predecessors,
        syntax,
        lowered,
        EntryTail::Proof,
        Obligations::Reported,
        &mut |_| {},
    );
    let put = put?;

    Ok(Examined {
        obligations: put.obligations,
        verdicts: put.verdicts,
    })
}

/// The back half of [`compile_entrypoint`]: from a verified erased module through optimization, the lowering into Cont, Cont optimization, and wasm emission, observing every stage in order.
fn lower_from_ersd<O>(mut ersd_module: curios_ersd::Module, observe: &mut O) -> curios_wasm::Module
where
    O: FnMut(Stage<'_>),
{
    curios_profile::profile!("lower_from_ersd");
    observe(Stage::Ersd(&ersd_module));

    // Shrink before lowering: drop the items the program neither reaches nor runs for effect, so Cont's whole-module fixpoint sees only the live slice (see `curios_ersd::optimize`). Verified rather than verifying: erasure sealed this very module through `finalize`, which verified it.
    curios_ersd::optimize_verified(&mut ersd_module);

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

/// Compile one unit against `predecessors`: lower, elaborate, judge, erase.
///
/// **The judgment sits between elaboration and erasure and that ordering is the point.** A module the kernel refuses never reaches erasure's budget, and a refusal reads as a refusal rather than as whatever erasure made of an ill-typed term. It is also why this is a fold step rather than one operation: the producer of a stored unit runs the same sequence *without* the judge, because the crate that writes an image deliberately cannot reach the kernel.
pub fn compile_unit(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    source: &UnitSource<'_>,
) -> Result<Unit, CompileError> {
    curios_profile::profile!("compile_unit");
    let text = predecessors.text();
    let cores = predecessors.cores();

    let mut lowered = into_core_unit(source, &text, syntax)
        .map_err(|error| CompileError::Failure(vec![error.report()]))?;

    let established = Established::placed(&predecessors.placed());
    let mut context = Context::new(budget, *syntax);
    context.set_imports(lowered.imports().clone());
    context.set_broken(lowered.broken_names());
    let (core, traces) = with_broken(
        lowered.broken(),
        elaborate_and_zonk_unit(&mut context, &established, lowered.core(), lowered.minted())
            .map_err(|error| {
                let spellings = Rc::new(lowered.spellings(&text));
                CompileError::of(&error, |member| {
                    member.reports_with_hints(
                        lowered.core(),
                        &cores,
                        syntax,
                        lowered.unbound(),
                        &spellings,
                        &anchors_of(&lowered, predecessors),
                    )
                })
            }),
    )?;
    lowered.credit(&context.credited());

    let core = curios_core::Zonked::project(&core)
        .map_err(|refusal| CompileError::failure(refusal.to_string()))?;

    let rechecked = certify(&core, budget, predecessors, syntax);
    if let Some(verdict) = rechecked.verdicts.first() {
        return Err(kernel_refusal(verdict, core.as_module(), &cores, syntax));
    }

    let erased = predecessors.erased();
    let ersd =
        erase_unit(budget, *syntax, Erasures::of(&cores, &erased), &core).map_err(|error| {
            CompileError::Failure(error.reports_with(
                core.as_module(),
                &cores,
                syntax,
                &anchors_of(&lowered, predecessors),
            ))
        })?;

    let core = core.into_module();

    Ok(Uncertified::new(lowered, core, ersd, traces).certified(rechecked.certification))
}

/// Where a judged unit is kept between compilations.
///
/// Declared here and implemented in `curios-verdicts`, because the fold is what consults one and this crate must never learn what a project is. What crosses the boundary is a unit — never a path, never a manifest, and never a key this crate would have to know how to build.
///
/// **A unit handed back is one that was judged when it was recorded, and taking it is taking that on trust.** That is a change to what the compiler believes rather than an optimization of what it does, which is why the argument for it lives in [Cached verdicts](../../documentation/design/soundness/cached-verdicts.md) rather than here: the implementation chooses the key, and the key is the whole of what the argument rests on.
pub trait Cache {
    /// The unit already recorded for `source`, if one is.
    fn get(&self, source: &UnitSource<'_>) -> Option<Unit>;

    /// A unit to compile `source` over when it is not a hit: one this cache holds from an earlier text of the same sources, or `offered`, which whoever assembled the fold offers for this source — the archived unit, for a package taking a prelude root's place. `None`, the default, is a whole compile, whatever was offered.
    ///
    /// **Offered rather than imposed, and lent rather than handed over.** The cache decides whether the unit is compiled over the offer, and a cache holding something nearer copies nothing. Whatever tree the package is, the offer is a correct baseline: an item is reused only where its lowered form matches the offered one and nothing it reaches changed, so a tree far from the archive's is simply a larger closure.
    ///
    /// **What came of a baseline is handed to `put` like any other unit**, and is one: a unit compiled over a baseline is stored as the bytes a whole compile of the same text stores, so a cache that answers may file it where it would file the other. The store's own cache answers, for a build and for a question alike; a cache that keeps the default compiles whole.
    fn baseline(&self, source: &UnitSource<'_>, offered: Option<&Unit>) -> Option<Unit> {
        let _ = (source, offered);
        None
    }

    /// Record what `source` compiled to. Best effort — a store that cannot be written costs the next compilation the work, and nothing else.
    ///
    /// `followed` is whether another unit follows this one in the fold, which is the one reader a placement has *within* it: the next unit's slot is addressed after this one's. A cache that files, or whose chain is read after the fold — a payload is filed under the whole chain — places either way; one that does not file a unit has no use for its placement where nothing follows, and a placement serializes the unit whole.
    fn put(&self, source: &UnitSource<'_>, unit: &Unit, followed: bool);
}

/// Compile `sources` in dependency order, each against `base` and everything before it — the fold this whole design is named for.
///
/// Returns the units it produced, not the ones it was given: the caller owns `base` and this cannot take it. The predecessors are rebuilt per step from pointers to both, which is free.
///
/// A `cache` short-circuits the step entirely: a recorded unit was judged when it was recorded, so neither elaboration nor the kernel re-runs for it. `None` compiles everything, which is what every caller without a project does.
///
/// Each source arrives beside the baseline its assembler offers for it — `None` for every unit but one taking a prelude root's place, which is decided where the sources are built rather than recognized here. The offer goes to `cache` on a miss, which decides what becomes of it, and with no cache nothing is taken.
pub fn compile_units<'a, P>(
    budget: u64,
    base: Predecessors<'a>,
    syntax: &SyntaxRegistry,
    sources: &[(UnitSource<'_>, Option<&Unit>)],
    cache: Option<&dyn Cache>,
    mut progress: P,
) -> Result<Vec<Unit>, CompileError>
where
    P: FnMut(Progress<'_>),
{
    let mut produced: Vec<Unit> = Vec::new();

    for (index, (source, offered)) in sources.iter().enumerate() {
        // Announced *after* the cache is consulted and *before* the work, so a reported operation is one that is actually about to happen and its timing starts where it does.
        let hit =
            curios_profile::profile!("cache::get" => cache.and_then(|cache| cache.get(source)));
        if let Some(unit) = hit {
            progress(Progress::Reused(&source.prefix()));
            produced.push(unit);
            continue;
        }

        // A baseline is asked for only on a miss: a hit is the empty case of a recompile, where nothing changed and everything is reused.
        let baseline = curios_profile::profile!(
            "cache::baseline" => cache.and_then(|cache| cache.baseline(source, *offered))
        );
        let prefix = source.prefix();
        progress(match &baseline {
            Some(_) => Progress::Recompiling(&prefix),
            None => Progress::Compiling(&prefix),
        });

        let predecessors = base
            .units()
            .iter()
            .copied()
            .chain(produced.iter())
            .collect::<Vec<_>>();

        let unit = match &baseline {
            Some(baseline) => compile_unit_over(
                budget,
                Predecessors::over(&predecessors),
                syntax,
                source,
                baseline,
            )?,
            None => compile_unit(budget, Predecessors::over(&predecessors), syntax, source)?,
        };
        progress(Progress::Compiled);

        if let Some(cache) = cache {
            curios_profile::profile!(
                "cache::put" => cache.put(source, &unit, index + 1 < sources.len())
            );
        }

        produced.push(unit);
    }

    Ok(produced)
}

/// What the fold is doing, for a caller that reports it.
///
/// Sequential by construction — the fold compiles one unit at a time — so [`Progress::Reused`] and [`Progress::Compiled`] refer to whichever subject was announced last and need not name it again.
///
/// Deliberately separate from the [`Stage`] observer, which belongs to `wonder stage`: one reports *what is happening* and the other *what was produced*, and a caller wants either without the other.
pub enum Progress<'a> {
    /// A mounted unit is about to be compiled, named by the prefix it claims.
    Compiling(&'a Qualifier),
    /// A mounted unit is about to be compiled over a baseline — an earlier compilation of its sources, every item the edit did not reach reused from it — named by the prefix it claims. Followed by [`Progress::Compiled`] as a [`Progress::Compiling`] is.
    Recompiling(&'a Qualifier),
    /// The entry program is about to be compiled. Unnamed: it owns the empty prefix, so only the caller knows what was asked for.
    Entry,
    /// A mounted unit came from the store instead; nothing is compiled for it, and no [`Progress::Compiled`] follows.
    Reused(&'a Qualifier),
    /// The subject just announced finished.
    Compiled,
}

/// Compile a parsed entrypoint through the full pipeline to a wasm module, feeding every [`Stage`] to `observe` in order. The result pairs the module with the [`ForeignStore`] harvested from the `foreign` declarations of the program and of every predecessor — an embedder that will run the module builds its `ffi`-tier bindings (`curios-runtime`'s `ForeignBindings`) from exactly this store, or drops it when the program declares none. Binaryen optimization and Cranelift precompilation are deliberately *not* here — they live downstream in the `curios` crate (`to_cwasm`), keeping this crate free of native backends.
///
/// Production erases over the archived prelude's erased forms: only the entry's own items erase, the program is linked from what they reach, the Ersd optimizer shrinks and rebases the module, and the lowering into Cont makes every encoding decision once (see `curios_ersd::lower_to_cont`).
///
/// **`loader` is borrowed rather than taken, so the caller still owns it when this returns.** Resolution records what it read through `&self` — the log is interior-mutable precisely so that lowering never has to thread `&mut` — and a caller filing what this compilation produced needs that log *after* the fold, exactly as it needs the cache handle's refusal after the fold. Consuming the loader would put the read set out of reach at the only moment it is worth anything, and nothing here wants ownership of it.
pub fn compile_entrypoint<O>(
    budget: u64,
    predecessors: Predecessors<'_>,
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
        predecessors,
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
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    entrypoint: &Entrypoint,
    loader: &RootSource,
    tail: EntryTail,
    observe: O,
) -> Result<(curios_wasm::Module, ForeignStore, Vec<TestRecord>), CompileError>
where
    O: FnMut(Stage<'_>),
{
    compile_with_tail(
        budget,
        predecessors,
        syntax,
        entrypoint,
        loader,
        tail,
        observe,
    )
}

fn compile_with_tail<O>(
    budget: u64,
    predecessors: Predecessors<'_>,
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
    let judged = check_observed(
        budget,
        predecessors,
        syntax,
        entrypoint,
        loader,
        tail,
        &mut observe,
    )?;
    let ersd_module = erase_checked(budget, predecessors, syntax, &judged)?;

    // Every unit's rows, not the entry's alone: an embedder binds one registry, and a dependency that declares a `foreign` row has to reach it. Disjoint by mount, so the union cannot collide.
    let mut all_foreigns = predecessors.foreigns();
    all_foreigns.absorb(&judged.foreigns);

    Ok((
        lower_from_ersd(ersd_module, &mut observe),
        all_foreigns,
        judged.records,
    ))
}
