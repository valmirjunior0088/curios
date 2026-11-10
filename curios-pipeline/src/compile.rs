//! The stage sequence itself: [`compile_entrypoint`] and the two halves it is assembled from — the type-checking prologue and the lowering back half — each observing every [`Stage`] it produces before moving past it.

#[cfg(test)]
pub mod test_support;

use {
    super::{DumpScope, Over, Stage, compile_lowered_over, compile_unit_over},
    curios_abi::ForeignStore,
    curios_anf::lower_to_cps,
    curios_cert::{Globals, Verdict, certify_definitions, certify_program},
    curios_common::{Candidates, Qualifier, Report, SyntaxRegistry, recurse},
    curios_core::{
        Anchors, Func, Global, Intrinsic, Let, Matrix, MetavarOrigin, Program, Rec, Row, Subterm,
        Term, Transient, Zonked,
    },
    curios_elab::{
        Context, Established, FinalizedProgram, Reading, Tail, elaborate_and_zonk_entry,
        elaborate_and_zonk_program, elaborate_and_zonk_program_reporting, elaborate_and_zonk_unit,
        validate_entry_universes,
    },
    curios_emit::into_wasm,
    curios_erase::{Erasures, erase_program, erase_unit},
    curios_module::{
        BrokenItem, Lint, LoweredEntry, PreparedText, RootSource, UnitSource, into_core_entry,
        into_core_unit, into_core_unit_over, into_core_with_prelude,
    },
    curios_surface::{Entrypoint, Spelling},
    curios_unit::{Predecessors, Unit},
    std::{cell::LazyCell, collections::BTreeSet, fmt, rc::Rc},
};

/// What a unit's lowering decided beside its module: its lints, and the prefix of every mount some reference of it resolved into — see [`Lint`]. Taken off the lowering before elaboration consumes it and credited with the binders elaboration's proofs read however elaboration ended, so a question about a program has them whether or not elaboration accepted it.
#[derive(Debug, Clone, Default)]
pub struct Findings {
    pub lints: Vec<Lint>,
    /// Where the unit's texts lie, which every lint of it stands in.
    pub anchors: Anchors,
    pub reached: BTreeSet<Qualifier>,
}

impl Findings {
    /// A stored unit's, as its lowering left them and its elaboration credited them: none of a unit read from Core text, which no lowering made ([`Unit::text`]).
    pub fn of_unit(unit: &Unit) -> Self {
        unit.text().map(Self::of_text).unwrap_or_default()
    }

    /// A lowering's, as it left them and its elaboration credited them.
    pub fn of_text(text: &PreparedText) -> Self {
        Self {
            lints: text.lints().to_vec(),
            anchors: text.anchors().clone(),
            reached: text.reached().clone(),
        }
    }

    fn take(lowered: &mut LoweredEntry) -> Self {
        Self {
            lints: std::mem::take(&mut lowered.lints),
            anchors: lowered.anchors.clone(),
            reached: std::mem::take(&mut lowered.reached),
        }
    }

    /// Each lint as a reader sees it, placed where the unit's texts lie now.
    pub fn reports(&self) -> Vec<Report> {
        self.lints
            .iter()
            .map(|lint| lint.report(&[&self.anchors]))
            .collect()
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
pub fn with_broken<T>(
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
/// The [`Globals`] environment is assembled here, beside the walk that reads it, rather than in `curios-unit`, which holds what the stages produced and judges nothing. Every caller that wants the compile path's rechecking gets this one rather than reconstructing it.
pub fn recheck(
    program: &curios_core::Zonked<Program>,
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
) -> Vec<Verdict> {
    certify_program(program, budget, &globals(predecessors), *syntax)
        .0
        .verdicts
}

/// Where a report about the unit lowered as `text` is placed: the unit's own declarations first, then those of the units in its scope.
fn anchors_of<'a>(text: &'a PreparedText, predecessors: Predecessors<'a>) -> Vec<&'a Anchors> {
    std::iter::once(text.anchors())
        .chain(predecessors.anchors())
        .collect()
}

/// What elaboration or erasure refused `module` with, as a compilation reports it: every member read against one set of tables, so a name is spelled and a declaration placed alike whichever stage refused. `text` is the lowering the refused terms came from and `predecessors` the units it was lowered against, whose tables say how a name is written and where a declaration lies; `lexicons` are those of every unit `module` was read over, which is the lowering's own unit too where `module` is a term elaborated over it.
pub fn elaborator_refusal(
    error: &curios_elab::Error,
    definitions: &curios_core::Definitions,
    lexicons: &[&curios_core::Lexicon],
    text: &PreparedText,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
) -> CompileError {
    refusal_read(
        error,
        definitions,
        None,
        lexicons,
        text,
        predecessors,
        syntax,
    )
}

/// [`elaborator_refusal`] of the module the lowering `text` made, read where its unit keeps what a report spells the unit's own names by and places its reader in ([`PreparedText::lexicon`], [`PreparedText::places`]): over a baseline, those of the baseline revised, so a refusal reads the declarations the edit reached.
pub fn lowering_refusal(
    error: &curios_elab::Error,
    lexicons: &[&curios_core::Lexicon],
    text: &PreparedText,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
) -> CompileError {
    let lexicon = text.lexicon();
    refusal_read(
        error,
        text.core(),
        Some((&lexicon, text.places())),
        lexicons,
        text,
        predecessors,
        syntax,
    )
}

/// What elaboration refused `module` with, read against `kept`, the lexicon and places its unit keeps for it, where there are any.
fn refusal_read(
    error: &curios_elab::Error,
    definitions: &curios_core::Definitions,
    kept: Option<(&curios_core::Lexicon, &curios_core::Places)>,
    lexicons: &[&curios_core::Lexicon],
    text: &PreparedText,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
) -> CompileError {
    read_for_report(
        definitions,
        kept,
        lexicons,
        text,
        predecessors,
        syntax,
        |reading| CompileError::of(error, |member| member.reports(reading)),
    )
}

/// What erasure refused `module` with, as the one report each of its refusals is: in erasure's words, the program having been certified.
pub fn erasure_refusal(
    error: &curios_erase::Error,
    definitions: &curios_core::Definitions,
    lexicons: &[&curios_core::Lexicon],
    text: &PreparedText,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
) -> CompileError {
    read_for_report(
        definitions,
        None,
        lexicons,
        text,
        predecessors,
        syntax,
        |reading| CompileError::Failure(error.reports(reading.spelling(), reading.anchors)),
    )
}

/// `report` over the reading a refusal of `module` is spelled and placed by: its unit's names spelled as resolution reaches them from the reader's place, and its positions placed in the files they were written in.
fn read_for_report(
    definitions: &curios_core::Definitions,
    kept: Option<(&curios_core::Lexicon, &curios_core::Places)>,
    lexicons: &[&curios_core::Lexicon],
    text: &PreparedText,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    report: impl FnOnce(&Reading<'_>) -> CompileError,
) -> CompileError {
    let spellings = Rc::new(text.spellings(&predecessors.text()));
    let reading = Reading {
        definitions,
        kept,
        scope: lexicons,
        syntax,
        spellings: Some(&spellings),
        anchors: &anchors_of(text, predecessors),
    };

    report(&reading)
}

/// The kernel's environment for `predecessors`: every unit in scope where it stands, read by the places it keeps for its names, with the record the certifier filed with it. Nothing of a unit is copied.
pub fn globals(predecessors: Predecessors<'_>) -> Globals<'_> {
    globals_after(predecessors, &vec![false; predecessors.units().len()])
}

/// [`globals`] for a walk over a baseline: `moved` says of each predecessor whether it is other than the unit the baseline's walk read.
pub fn globals_after<'a>(predecessors: Predecessors<'a>, moved: &[bool]) -> Globals<'a> {
    let mut globals = Globals::default();
    for unit in predecessors.units() {
        globals.mount(unit.core(), unit.places(), unit.certification());
    }
    globals.standing(&predecessors.fixed(), moved);

    globals
}

/// The compile error a kernel verdict is reported as: the refusal, named for the item it is about where there is one.
pub fn kernel_refusal(
    verdict: &Verdict,
    definitions: &curios_core::Definitions,
    scope: &[&curios_core::Lexicon],
    syntax: &SyntaxRegistry,
) -> CompileError {
    let refusal = verdict.rendered(
        definitions,
        &Spelling::for_refusal(definitions, scope, syntax),
    );

    CompileError::failure(match &verdict.name {
        Some(name) => format!("the kernel refused {}: {refusal}", name.faithful()),
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

/// One erasure obligation the elaborator raised over a program it built: its error as raised, and the reports a compilation makes of it, each with its place.
#[derive(Debug)]
pub struct Obligation {
    pub error: curios_elab::Error,
    pub reported: Vec<Report>,
}

/// Put `entry` to what `tail` judges an authored entry at: the program contract, with what closes the program executed ([`executed`]), or the empty proposition where it is put as a proof. A test program's entry is left as it stands: its tail is the elaborator's to synthesize and to state the type of.
fn state(entry: &mut curios_core::Entrypoint, tail: &EntryTail, syntax: &SyntaxRegistry) {
    match tail {
        EntryTail::Program => {
            entry.body = executed(&entry.body, syntax);
            entry.type_ = Some(Term::intrinsic(Intrinsic::io_type(Term::tuple_type_unit())));
        }
        EntryTail::False => {
            let empty = Global::Authored(syntax.proof.false_type.qualifier());

            entry.type_ = Some(Term::written(empty));
        }
        EntryTail::Test { .. } => {}
    }
}

/// `term` with each of its tails handed to `/std/Program`'s `execute`, which says what a value of the tail's type does as a whole program: the tail's type is the call's implicit, read by inference, and its witness is resolved as an operator's is.
///
/// A tail is reached through a block of local definitions to what the block closes with, through a `!` to what its continuation closes with, and through a `match` that writes no motive to each of its arms. Anything else is one — a `match` whose motive is written among them, being one term of the type it states. A written goal is left as it stands, so it is asked at the entry's own type.
///
/// The whole term is not what is executed: a `!` reads its region from the type it is checked against, and a term handed to `execute` is checked against that call's unsolved implicit. With the tails executed, the entry's stated type is every region's on the way to them, as it is where nothing is executed.
///
/// The call binds nothing, so no scope is opened: each body is rewritten where it lies.
fn executed(term: &Term, syntax: &SyntaxRegistry) -> Term {
    let rebuilt = recurse(|| match &**term {
        Subterm::Let(block) => Term::from(Subterm::Let(Let {
            bindings: block.bindings.clone(),
            tail: block.tail.map_body(|tail| executed(tail, syntax)),
        })),
        // A block whose tail selects a member is that member's occurrence: one term, and so a tail.
        Subterm::Rec(block) if block.as_proj().is_none() => Term::from(Subterm::Rec(Rec {
            group: block.group.clone(),
            tail: block.tail.map_body(|tail| executed(tail, syntax)),
        })),
        Subterm::Transient(Transient::Bang(bang)) => {
            Term::bang(bang.action.clone(), closing(&bang.continuation, syntax))
        }
        Subterm::Transient(Transient::Matrix(matrix)) if matrix.motive.is_none() => {
            Term::from(Subterm::Transient(Transient::Matrix(Matrix {
                head: matrix.head.clone(),
                motive: None,
                rows: matrix
                    .rows
                    .iter()
                    .map(|row| Row {
                        pattern: row.pattern.clone(),
                        body: closing(&row.body, syntax),
                    })
                    .collect(),
            })))
        }
        Subterm::Metavar(metavar) if matches!(metavar.origin, MetavarOrigin::Goal) => term.clone(),
        _ => {
            let execute = Term::registered_method(syntax.program.execute);

            Term::apply(execute, vec![term.clone()])
        }
    });

    match term.extent() {
        Some(extent) => Term::placed(extent, rebuilt),
        None => rebuilt,
    }
}

/// A `!`'s continuation or a match arm's body, each a function over the binders written before it, with the tails of what it answers executed.
fn closing(function: &Term, syntax: &SyntaxRegistry) -> Term {
    let Subterm::Func(func) = &**function else {
        panic!("a continuation and an arm's body are lowered as function literals");
    };
    let rebuilt = Term::from(Subterm::Func(Func {
        telescope: func.telescope.map_terminal(|body| executed(body, syntax)),
    }));

    match function.extent() {
        Some(extent) => Term::placed(extent, rebuilt),
        None => rebuilt,
    }
}

/// Why a program's elaborated entry is present wherever it is read: elaboration withholds an entry, recording no refusal, only when it reaches an item the lowering could not read, and `with_broken` turns any such item into a failure first.
const WITHHELD_ONLY_WHEN_BROKEN: &str = "an entry is withheld only for reaching an item that did not parse, which fails the compile first";

/// Which final term a compilation checks, and what it is judged at: the one the author wrote, as a program or at `False`, or the test tail synthesized from a unit's registered tests. A policy rather than a boolean at each call site, so the program shapes a unit can compile to are named where they diverge — and the whole of what an entry can be judged at, since nothing else states its type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EntryTail {
    /// The authored entrypoint as a program: judged at `Io({})`, each of its tails run through `/std/Program`.
    Program,
    /// The authored entrypoint as an alleged proof of the empty proposition, judged at `/sys/Bool/False`: how the soundness board puts a witness through [`examine_entrypoint`].
    False,
    /// The synthesized `Test/main([...])` over the tests in scope at or under any of `under`, a whole segment at a time, replacing an executable's authored tail, or a library's absence of one, where the entry is an empty program. Only the unit compiled tested holds tests ([`curios_module::RootSource::tested`]), so they are that unit's, wherever it stands in the fold. A tail selecting none is `Test/main([])`, which runs nothing and answers that every test held.
    ///
    /// **Which tests is the program's, never the unit's.** The unit holds every test it declares whatever is selected, so one tested unit serves every selection, `wonder` and `lint`; what the tail does not reach is linked into nothing, so a selection narrows the program's back end, where most of a test program's cost is.
    Test { under: Vec<Qualifier> },
}

impl EntryTail {
    /// The tests in scope, module by module, those the tail selects scheduled and the rest skipped: the tests `own`, the entry's unit, and `scope` hold, each a lowered module, in the fold's order and each unit's [module order](in_module_order); a program and an entry at `False` schedule none and skip none.
    ///
    /// A test program's tail is synthesized by the elaborator, in Core, once the unit's items are defined — it schedules those definitions and chooses each test's discharge from their elaborated form, so it cannot exist before them. What is decided here is *which* tests it schedules, and in what order. The records for the runner are read off the same lowered definitions the tail schedules, so the two cannot disagree about what the tests are.
    fn schedule(
        &self,
        own: &curios_core::Definitions,
        scope: &[&curios_core::Definitions],
    ) -> Vec<ScheduledModule> {
        let EntryTail::Test { under } = self else {
            return Vec::new();
        };
        let selected = |test: &Global| under.iter().any(|prefix| selects(prefix, test));

        scope
            .iter()
            .copied()
            .chain([own])
            .flat_map(|definitions| {
                in_module_order(definitions.tests())
                    .into_iter()
                    .map(|(path, tests)| {
                        let (scheduled, skipped): (Vec<_>, Vec<_>) =
                            tests.into_iter().partition(|test| selected(test));

                        ScheduledModule {
                            path,
                            scheduled: scheduled_tests(&scheduled, &definitions.items),
                            skipped: skipped
                                .iter()
                                .filter_map(|test| test.qualifier().copied())
                                .collect(),
                        }
                    })
            })
            .collect()
    }

    /// Each prefix the tail selects tests at or under that selects none of `schedule`'s: what a run names where no subject's tail selects anything at it. None for a program or an entry at `False`.
    fn unmatched(&self, schedule: &[ScheduledModule]) -> Vec<Qualifier> {
        let EntryTail::Test { under } = self else {
            return Vec::new();
        };

        under
            .iter()
            .filter(|prefix| {
                !schedule
                    .iter()
                    .flat_map(|module| &module.scheduled)
                    .any(|test| selects(prefix, &test.name))
            })
            .copied()
            .collect()
    }

    /// What the elaborator closes the program with: the authored `entry`, or the test tail over `scheduled`, which the elaborator synthesizes and states the type of.
    fn closing<'t>(
        &self,
        entry: &'t curios_core::Entrypoint,
        scheduled: &'t [curios_elab::ScheduledTest],
    ) -> Tail<'t> {
        match self {
            EntryTail::Program | EntryTail::False => Tail::Entry(entry),
            EntryTail::Test { .. } => Tail::Tests(scheduled),
        }
    }
}

/// One registered test, as the runner reports it: the path that names it, and its body as written — sliced from where the anchors in hand place the extent its authored body carries, empty where it carries none (a unit restored from a store may carry none) or none of them holds its declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TestRecord {
    pub path: Qualifier,
    pub body: String,
}

/// One module's tests, as a test program's runner reports them: the module they stand in, a record of each the tail schedules, in the order it runs them, and the path of each it leaves out.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TestModule {
    pub path: Qualifier,
    pub scheduled: Vec<TestRecord>,
    pub skipped: Vec<Qualifier>,
}

/// Whether `prefix` selects `test`: a whole segment at a time, a test at or under it. The one statement of what a test tail's prefixes select, for what it schedules and for the prefixes that select nothing.
fn selects(prefix: &Qualifier, test: &Global) -> bool {
    matches!(test, Global::Authored(path) if path.is_within(prefix))
}

/// One module's tests as [`EntryTail::schedule`] decides them, before the scheduled ones' bodies are sliced into records.
struct ScheduledModule {
    path: Qualifier,
    scheduled: Vec<curios_elab::ScheduledTest>,
    skipped: Vec<Qualifier>,
}

/// The tests a schedule's tail runs, in the order it runs them: each module's scheduled tests, a module after the other.
fn tail_tests(schedule: &[ScheduledModule]) -> Vec<curios_elab::ScheduledTest> {
    schedule
        .iter()
        .flat_map(|module| module.scheduled.iter().cloned())
        .collect()
}

/// `tests` by the module each stands in: the modules in the order their first tests come, and each module's tests in the order they come. The order a test program schedules a unit's tests in and `wonder --tests` lists them in, so a module's tests run and report together however its declarations interleave with another's.
fn in_module_order(tests: Vec<Global>) -> Vec<(Qualifier, Vec<Global>)> {
    let mut modules: Vec<(Qualifier, Vec<Global>)> = Vec::new();
    for (path, test) in tests
        .into_iter()
        .filter_map(|test| Some((test.qualifier()?.without_last(), test)))
    {
        match modules.iter_mut().find(|(module, _)| *module == path) {
            Some((_, held)) => held.push(test),
            None => modules.push((path, vec![test])),
        }
    }

    modules
}

/// The paths of a lowered module's tests, in the order a test program schedules them — how `wonder --tests` lists them, grouped by module as the schedule groups them, so the list cannot disagree with it.
pub fn declared_test_paths(definitions: &curios_core::Definitions) -> Vec<String> {
    in_module_order(definitions.tests())
        .into_iter()
        .flat_map(|(_, tests)| tests)
        .map(|test| test.path())
        .collect()
}

/// The definition a registered test names, among the unit's items.
fn test_definition<'a>(
    items: &'a curios_core::Items,
    test: &curios_core::Global,
) -> Option<&'a curios_core::Definition> {
    items.iter().find_map(|item| match item {
        curios_core::Item::Let(def) if def.name == *test => Some(def.as_ref()),
        _ => None,
    })
}

/// Each registered test as the tail schedules it, read off the lowered definition that carries it: the generated thunk's extent covers the whole authored body, while its interior retains each action's narrower diagnostic extent. One lookup serves the tail and the runner's records, so the two cannot disagree about what the tests are.
fn scheduled_tests(
    tests: &[curios_core::Global],
    items: &curios_core::Items,
) -> Vec<curios_elab::ScheduledTest> {
    tests
        .iter()
        .map(|test| curios_elab::ScheduledTest {
            name: *test,
            extent: test_definition(items, test).and_then(|def| def.body.extent()),
        })
        .collect()
}

/// The report metadata of each module's tests: a record of each scheduled one, the path that names it and its body as written, sliced from where `anchors` place the extent the authored body carries, and the path of each skipped one.
fn test_modules(schedule: Vec<ScheduledModule>, anchors: &[&Anchors]) -> Vec<TestModule> {
    schedule
        .into_iter()
        .map(|module| TestModule {
            path: module.path,
            scheduled: module
                .scheduled
                .iter()
                .filter_map(|test| {
                    Some(TestRecord {
                        path: *test.name.qualifier()?,
                        body: test
                            .extent
                            .and_then(|extent| extent.resolved(anchors))
                            .map(|span| span.source.text()[span.start..span.end].to_string())
                            .unwrap_or_default(),
                    })
                })
                .collect(),
            skipped: module.skipped,
        })
        .collect()
}

/// The lowering half of the prologue: the surface tree to a core module, observed as the `text` rung.
pub fn lower_entry<O>(
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

/// The elaborating half of the prologue, over an entry already lowered: the lowering's findings, credited with what elaboration's proofs read whatever its verdict, beside that verdict.
fn elaborate_lowered<O>(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    mut lowered: LoweredEntry,
    tail: EntryTail,
    observe: &mut O,
) -> (Findings, Result<(Program, Vec<Obligation>), Refused>)
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
        predecessors,
        lowered,
        tail,
        observe,
    );
    findings.credit(&context.credited());
    (findings, verdict)
}

/// The elaboration of a program put whole, its declarations and the term that closes it as one module, in `context`, which the caller reads its credits and its consumption from however this ends. Beside the program come the erasure obligations elaboration decided over it, reported and never raised: what puts a program whole is an instrument that hands the same program to the kernel (`typecheck_entrypoint`, [`examine_entrypoint`]), where a compilation takes a program's declarations as the unit they are (`enter`). What the entry is judged at is stated once for both (`state`).
///
/// `predecessors` are the units in scope, read where a report is made: where each one's declarations lie, which a report placed in one of them is resolved against, after the entry's own, the paths each gives its names, and the lexicon each keeps, which a report spells a name by.
pub fn elaborate_entry<'s, O>(
    context: &mut Context<'s>,
    established: &'s Established<'_>,
    predecessors: Predecessors<'_>,
    lowered: LoweredEntry,
    tail: EntryTail,
    observe: &mut O,
) -> Result<(Program, Vec<Obligation>), Refused>
where
    O: FnMut(Stage<'_>),
{
    let scope = predecessors.text();
    // The registry the context was made with, which is the one its elaboration synthesizes names from.
    let syntax = &context.syntax();
    // Read by the first report, each unit's where the unit keeps it, and by no elaboration that reports nothing.
    let lexicons = LazyCell::new(|| predecessors.lexicons());
    let LoweredEntry {
        program: mut lowered,
        minted,
        foreigns: _,
        names,
        lints: _,
        reached: _,
        broken,
        anchors: own,
    } = lowered;
    // Read by the first report placed in a declaration: an elaboration that reports nothing restores where no unit's declarations lie.
    let anchors = LazyCell::new(|| {
        std::iter::once(&own)
            .chain(scope.iter().map(|text| text.anchors()))
            .collect::<Vec<_>>()
    });
    // Made by the first report that spells a name, and by no elaboration that reports nothing.
    let spellings = LazyCell::new(|| Rc::new(names.spellings(&scope)));

    let scheduled = tail_tests(&tail.schedule(&lowered.definitions, &predecessors.lowered()));

    let dumped = DumpScope {
        syntax,
        predecessors,
    };
    observe(Stage::Core(&lowered, dumped));

    // The entrypoint contract, stated in the entry: a program *is* a description of doing something and yielding nothing, so an authored entry states `Io({})`, and elaboration checks the body against it, the kernel rechecks it and erasure seals at it — each reading it off the entry. An entry put as a proof states the empty proposition instead, and those two are the whole choice: the surface entry carries no type, so no caller states another. A test program's tail replaces the written entry, and the elaborator that synthesizes it states its type.
    //
    // `Io({})` is closed, which is what lets it be stated before elaboration at all. `Io(?T)` would need a metavariable minted before the elaboration context exists, which would leave the contract a post-hoc head test on the inferred type. What closes a program is of any type `/std/Program` executes, and that costs the contract nothing: the dispatch is a call inside the entry, at each of its tails, and the call answers `Io({})`.
    state(&mut lowered.entrypoint, &tail, syntax);
    let elab_tail = tail.closing(&lowered.entrypoint, &scheduled);

    context.set_imports(names.imports().clone());
    context.set_broken(broken.iter().filter_map(|item| item.declares).collect());
    let elaborated = elaborate_and_zonk_program_reporting(
        context,
        established,
        &lowered.definitions,
        names.mounts(),
        &minted,
        elab_tail,
    )
    .map(
        |FinalizedProgram {
             definitions,
             entry,
             obligations,
         }| (definitions, entry, obligations),
    );
    // What a report of this program is read against, each table made by the first report that reads one.
    let reading = || Reading {
        definitions: &lowered.definitions,
        kept: None,
        scope: &lexicons,
        syntax,
        spellings: Some(&*spellings),
        anchors: &anchors,
    };
    // The error is kept as raised beside what it renders to: rendering is where its variant is lost.
    let (error, elaborated) = match elaborated {
        Ok(built) => (None, Ok(built)),
        Err(error) => {
            let reported = CompileError::of(&error, |member| member.reports(&reading()));

            (Some(Box::new(error)), Err(reported))
        }
    };
    let (definitions, entry, obligations) =
        with_broken(&broken, elaborated).map_err(|reported| Refused { error, reported })?;

    let obligations = obligations
        .into_iter()
        .map(|error| Obligation {
            reported: error.reports(&reading()),
            error,
        })
        .collect();
    let program = Program {
        definitions,
        entrypoint: entry.expect(WITHHELD_ONLY_WHEN_BROKEN),
    };

    observe(Stage::CoreElab(&program, dumped));

    Ok((program, obligations))
}

/// Everything [`compile_entrypoint`] decides *about* a program before it builds anything from it: lowered, elaborated, zonked, judged by the kernel, and **erased**, with a refusal from any of them reported as the compile path reports it. What a question about a program's correctness is answered by.
///
/// **Erasure is a verdict, which is why the line is drawn under it rather than under the kernel.** It narrows every numeral into the erased carriers and refuses one that does not fit, and it hands the module to the erased representation's verifier, which rejects the recursion classes the language does not admit — a mutual value group no forcing order satisfies among them. Stopping at the kernel would let `wonder` report a clean program that `run` then refuses, which is the one thing that query may not do. Below here nothing decides: `lower_from_anf` — private, so named here rather than linked — returns no `Result` at all.
///
/// The erased module is discarded. Producing it is the whole cost, and the caller wants the Core module. `read`, where there is one, is told the program's lowering as soon as it is made ([`ReadLowering`]).
pub fn check_entrypoint(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    source: &UnitSource<'_>,
    cache: Option<&dyn Cache>,
    tail: EntryTail,
    read: Option<ReadLowering<'_>>,
) -> Result<Checked, CompileError> {
    let (entry, entered) = enter(
        budget,
        predecessors,
        syntax,
        source,
        cache,
        tail,
        Told {
            observe: &mut |_: Stage<'_>| {},
            read,
        },
    )?;
    let verdict = entered.and_then(|mut entered| {
        if let Some(erased) = entered.erased.take() {
            erased?;
        }
        Ok(entered.program())
    });

    Ok(Checked { entry, verdict })
}

/// A module of nothing, mounted nowhere: what a program's final term is elaborated, judged and erased beside where its declarations are a unit in scope.
fn nothing() -> curios_core::Definitions {
    curios_core::Definitions {
        items: Vec::new().into(),
    }
}

/// What compiling a unit as a program's declarations adds to compiling it as any unit: the term that closes the program, elaborated in the unit's walk, and what is told of the unit on the way.
pub struct Closing<'a> {
    pub tail: Tail<'a>,
    /// Told the unit's module and the entry as they elaborated, before anything judges them.
    pub elaborated: &'a mut dyn FnMut(&curios_core::Definitions, &curios_core::Entrypoint),
    /// Told the unit's lowering, credited by its elaboration, where the unit is refused: what a question reads the program's findings off whatever the verdict.
    pub refused: &'a mut dyn FnMut(&PreparedText),
}

/// What erasing an entered program came to: the module it links to, or what erasing it refused it for.
type Erasure = Result<curios_anf::Module, CompileError>;

/// A program entered: the unit its declarations are, and the term it closes with, elaborated, accepted by the kernel and erased through the kernel that accepted it ([`Entered::erased`]): what the lowering after it needs of the entry.
///
/// **A program's declarations are a unit, and its final term is compiled over it.** The declarations are kept between compilations as any unit's are — handed back by a cache where nothing they were compiled from moved, compiled over a baseline where something did — and the term is elaborated, judged and erased on every compile: it is no declaration, and has no name to be kept under.
struct Entered {
    /// The unit of the program's declarations: restored, compiled over a baseline, or compiled whole.
    unit: Unit,
    /// The term the program closes with beside a module of nothing, zonked and accepted by the kernel, stating the type it was judged at.
    program: Zonked<Program>,
    /// The program erased and linked through the kernel that judged it, or what erasing it refused it for: every program but a test program that schedules no test, which nothing is compiled of. The erase step both the check and the compile path take, so neither can hold a verdict the other does not.
    erased: Option<Erasure>,
    /// Every test in scope, module by module, scheduled or skipped, when the entry is compiled as a test program ([`EntryTail::schedule`]).
    modules: Vec<TestModule>,
    /// Each prefix of the test tail's that selects no test ([`EntryTail::unmatched`]).
    unmatched: Vec<Qualifier>,
}

impl Entered {
    /// The program as one module and its entry: the unit's declarations, and the term that closes it.
    fn program(&self) -> Program {
        Program {
            definitions: self.unit.core().clone(),
            entrypoint: self.program.as_program().entrypoint.clone(),
        }
    }
}

/// Enter the program `source` is: its declarations as the unit they are — the one `cache` hands back, compiled over the baseline it offers, or compiled whole and handed to it — and the term `tail` closes it with, elaborated, put to the kernel over that unit and erased through that kernel ([`judged`]). Beside it come the lowering's findings, credited by elaboration, whatever the verdict; the outer refusal is a program that did not lower.
///
/// **Where the unit is compiled the term is elaborated in its walk**, so a program holding a refused declaration reports every refusal and what its final term said beside them, as one batch. Where the unit is restored nothing of it is elaborated, and the term is elaborated alone, over the units in scope and the one restored.
fn enter<O>(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    source: &UnitSource<'_>,
    cache: Option<&dyn Cache>,
    tail: EntryTail,
    told: Told<'_, O>,
) -> Result<(Findings, Result<Entered, CompileError>), CompileError>
where
    O: FnMut(Stage<'_>),
{
    let Told { observe, read } = told;
    let tested = matches!(tail, EntryTail::Test { .. });
    let entrypoint = source
        .entrypoint()
        .expect("a program is entered from the source that carries its entrypoint");
    observe(Stage::Text(entrypoint));

    let (hit, baseline) = held(source, None, cache);
    // A hit was compiled after the units in scope, which is what made it one; a baseline after the ones its offer names.
    let earlier = hit
        .as_ref()
        .map(|unit| (unit, true))
        .or(baseline.as_ref().map(|earlier| {
            let settled = predecessors
                .units()
                .iter()
                .all(|unit| !earlier.after.moved(unit));

            (&earlier.unit, settled)
        }));
    let (lowered, mut written) = lower_program(predecessors, syntax, source, earlier)?;
    if let Some(read) = read {
        read(predecessors.units(), &lowered, Some(&written));
    }

    let schedule = tail.schedule(lowered.core(), &predecessors.lowered());
    let scheduled = tail_tests(&schedule);
    let unmatched = tail.unmatched(&schedule);
    let modules = match scheduled.is_empty() {
        true => test_modules(schedule, &[]),
        false => test_modules(schedule, &anchors_of(&lowered, predecessors)),
    };

    // The entrypoint contract, stated in the entry: a program *is* a description of doing something and yielding nothing, so an authored entry states `Io({})`, and elaboration checks the body against it, the kernel rechecks it and erasure seals at it — each reading it off the entry.
    state(&mut written, &tail, syntax);
    let dumped = DumpScope {
        syntax,
        predecessors,
    };
    observe(Stage::Core(
        &Program {
            definitions: lowered.core().clone(),
            entrypoint: written.clone(),
        },
        dumped,
    ));
    let closing = tail.closing(&written, &scheduled);

    let mut findings = None;
    let closed = match (hit, baseline) {
        (Some(unit), _) => {
            let mut restored = Findings::of_unit(&unit);
            let closed = close_over(
                budget,
                predecessors,
                syntax,
                &unit,
                &lowered,
                closing,
                &mut restored,
            );
            findings = Some(restored);

            closed.map(|entry| (unit, Some(lowered), entry))
        }
        (None, earlier) => {
            let mut elaborated =
                |definitions: &curios_core::Definitions, entry: &curios_core::Entrypoint| {
                    observe(Stage::CoreElab(
                        &Program {
                            definitions: definitions.clone(),
                            entrypoint: entry.clone(),
                        },
                        dumped,
                    ));
                };
            let mut refused = |lowered: &PreparedText| findings = Some(Findings::of_text(lowered));
            let closing = Some(Closing {
                tail: closing,
                elaborated: &mut elaborated,
                refused: &mut refused,
            });
            let compiled = match &earlier {
                Some(Earlier {
                    unit: baseline,
                    after,
                    candidates,
                }) => compile_lowered_over(
                    budget,
                    predecessors,
                    syntax,
                    lowered,
                    Over {
                        baseline,
                        moved: &predecessors
                            .units()
                            .iter()
                            .map(|unit| after.moved(unit))
                            .collect::<Vec<_>>(),
                        candidates: candidates.as_deref(),
                    },
                    closing,
                ),
                None => compile_lowered(budget, predecessors, syntax, lowered, closing),
            };

            compiled.map(|(unit, entry)| {
                // Handed over where its declarations are judged and erased, whatever is then said of the term that closes the program.
                if let Some(cache) = cache {
                    curios_profile::profile!("cache::put" => cache.put(source, &unit));
                }

                (unit, None, entry.expect(WITHHELD_ONLY_WHEN_BROKEN))
            })
        }
    };

    Ok(match closed {
        Err(error) => (findings.unwrap_or_default(), Err(error)),
        Ok((unit, lowered, entry)) => {
            // Read off the unit before the term that closes the program is judged: a lint is no part of that verdict either.
            let findings = findings.unwrap_or_else(|| Findings::of_unit(&unit));
            if lowered.is_some() {
                observe(Stage::CoreElab(
                    &Program {
                        definitions: unit.core().clone(),
                        entrypoint: entry.clone(),
                    },
                    dumped,
                ));
            }
            // A test program that schedules no test is compiled to nothing, so it is not erased.
            let erases = !(tested && scheduled.is_empty());
            let judged = {
                let units = predecessors
                    .units()
                    .iter()
                    .copied()
                    .chain([&unit])
                    .collect::<Vec<_>>();
                let text = lowered.as_ref().or(unit.text());

                judged(
                    budget,
                    (Predecessors::over(&units), predecessors),
                    syntax,
                    entry,
                    text.filter(|_| erases),
                )
            };

            (
                findings,
                judged.map(|(program, erased)| Entered {
                    unit,
                    program,
                    erased,
                    modules,
                    unmatched,
                }),
            )
        }
    })
}

/// The term a program closes with, elaborated alone over `unit`, the unit of the program's declarations as a cache handed it back: nothing of the unit is elaborated, and the term reads it as it reads any unit in scope. `lowered` is this compile's lowering of the program, which a report is placed and spelled by, and `findings` are credited with what the term's elaboration read.
fn close_over(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    unit: &Unit,
    lowered: &PreparedText,
    closing: Tail<'_>,
    findings: &mut Findings,
) -> Result<curios_core::Entrypoint, CompileError> {
    curios_profile::profile!("elaborate_and_zonk");
    let units = predecessors
        .units()
        .iter()
        .copied()
        .chain([unit])
        .collect::<Vec<_>>();
    let scope = Predecessors::over(&units);
    let fixed = scope.fixed();
    let established = Established::placed(&scope.placed()).fixed(&fixed);
    let mut context = Context::new(budget, *syntax);
    context.set_imports(lowered.imports().clone());

    let own = nothing();
    let closed = elaborate_and_zonk_program(
        &mut context,
        &established,
        &own,
        &[],
        lowered.minted(),
        closing,
    )
    .and_then(|(_, entry)| {
        let entry = entry.expect(WITHHELD_ONLY_WHEN_BROKEN);
        // Held to the schemes of the declarations it names, which the module it was elaborated beside holds none of.
        validate_entry_universes(unit.core(), &entry)?;

        Ok(entry)
    })
    .map_err(|error| {
        elaborator_refusal(
            &error,
            &own,
            &scope.lexicons(),
            lowered,
            predecessors,
            syntax,
        )
    });
    findings.credit(&context.credited());

    closed
}

/// The term `entry` closes the program with, over `scope`, the units before it and the unit of the program's declarations last, zonked and judged by the kernel; and, where `erasing` holds the program's lowering, erased and linked through the kernel that judged it, with what erasing it refused it for reported against that lowering and `predecessors`, the units before the program's own.
///
/// **One walk.** The kernel that judged the term is the one erasure reads it through, as a unit's is (`compile_lowered`): its environment borrows the scope built here, so it cannot leave this call, and what it erased can. The linked program's termination flags are marked here, from the record of every unit it was erased from, and nowhere else: erasure marks nothing, and nothing reads a flag before the back half lowers what this returns. See `curios_anf::Function::total`.
fn judged(
    budget: u64,
    (scope, predecessors): (Predecessors<'_>, Predecessors<'_>),
    syntax: &SyntaxRegistry,
    entry: curios_core::Entrypoint,
    erasing: Option<&PreparedText>,
) -> Result<(Zonked<Program>, Option<Erasure>), CompileError> {
    // The zonk evidence the kernel and erasure consume, established where the program is final. A refusal here is a compiler invariant — zonk just enforced the same claim — surfacing as a failure rather than a panic.
    let program = Zonked::project(&Program {
        definitions: nothing(),
        entrypoint: entry,
    })
    .map_err(|refusal| CompileError::failure(refusal.to_string()))?;

    // The independent kernel's second opinion: each unit was walked when it was built and arrives here as environment, so only the term that closes the program is judged.
    let globals = globals(scope);
    let (rechecked, mut kernel) = {
        curios_profile::profile!("recheck");
        certify_program(&program, budget, &globals, *syntax)
    };
    if let Some(verdict) = rechecked.verdicts.first() {
        let definitions = &program.as_program().definitions;
        let refusal = verdict.rendered(
            definitions,
            &Spelling::for_refusal(definitions, &scope.lexicons(), syntax),
        );
        return Err(CompileError::failure(match &verdict.name {
            Some(name) => format!("the kernel refused {}: {refusal}", name.faithful()),
            None => format!("the kernel refused the entrypoint: {refusal}"),
        }));
    }
    // The program as the kernel stated it, the one it is erased from.
    let program = Zonked::project(&Program {
        definitions: rechecked.definitions,
        entrypoint: rechecked
            .entrypoint
            .expect("a program's walk checks the term it closes with"),
    })
    .map_err(|refusal| CompileError::failure(refusal.to_string()))?;

    // The program's final term erased and linked with what it reaches of the units in scope, the unit of its own declarations the last of them: the module handed back holds that and no declaration the program does not reach.
    let erased = erasing.map(|text| {
        let (cores, places, certifications, forms) = (
            scope.cores(),
            scope.places(),
            scope.certifications(),
            scope.erased(),
        );
        let mut linked = erase_program(
            &mut kernel,
            Erasures::of(&cores, &places, &certifications, &forms),
            &program,
        )
        .map_err(|error| {
            erasure_refusal(
                &error,
                &program.as_program().definitions,
                &scope.lexicons(),
                text,
                predecessors,
                syntax,
            )
        })?;
        for unit in scope.units() {
            linked.mark_total(unit.certification());
        }

        Ok(linked.into_module())
    });

    Ok((program, erased))
}

/// What each checker said of a program the elaborator built.
#[derive(Debug)]
pub struct Examined {
    /// The (T) and (V) obligations the elaborator decided over the program, reported rather than raised.
    pub obligations: Vec<Obligation>,
    /// Every refusal the kernel reached over the same program.
    pub verdicts: Vec<Verdict>,
}

/// Put `entrypoint` to both checkers at what `tail` judges it at — the program contract, as a suite guarding a rule puts a program, or `False`, as the soundness board puts a witness — and hand back what each said rather than stopping at the first refusal: lowering, elaboration, the kernel.
///
/// The program is put whole, its declarations and the term that closes it elaborated as one module and judged as one, with one difference from the compile path: the elaborator's erasure obligations are reported rather than raised, so a program only it refuses still reaches the kernel. That is what a consumer asking which checker refused a program needs, and what `check_entrypoint` cannot say, since it stops at the first refusal. It stops under the kernel: erasure decides whether a program can be built, which is no part of whether its entry has the type it states.
pub fn examine_entrypoint(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    entrypoint: &Entrypoint,
    loader: &RootSource,
    tail: EntryTail,
) -> Result<Examined, Refused> {
    let lowered =
        lower_entry(predecessors, syntax, entrypoint, loader, &mut |_| {}).map_err(|reported| {
            Refused {
                error: None,
                reported,
            }
        })?;
    let (_, elaborated) =
        elaborate_lowered(budget, predecessors, syntax, lowered, tail, &mut |_| {});
    let (program, obligations) = elaborated?;

    // The zonk evidence the kernel consumes, established where the program is final. A refusal here is a compiler invariant — zonk just enforced the same claim — surfacing as a failure rather than a panic.
    let program = Zonked::project(&program).map_err(|refusal| Refused {
        error: None,
        reported: CompileError::failure(refusal.to_string()),
    })?;
    // The independent kernel's second opinion: each predecessor was walked when it was built and arrives here as environment, so only what it does not already answer for is judged.
    let rechecked = {
        curios_profile::profile!("recheck");
        certify_program(&program, budget, &globals(predecessors), *syntax).0
    };

    Ok(Examined {
        obligations,
        verdicts: rechecked.verdicts,
    })
}

/// The back half of [`compile_entrypoint`]: from a verified erased module through optimization, the lowering into CPS, CPS optimization and the rebuild in place it settles, and wasm emission, observing every stage in order.
fn lower_from_anf<O>(
    mut anf_module: curios_anf::Module,
    observe: &mut O,
) -> (curios_wasm::Module, ForeignStore)
where
    O: FnMut(Stage<'_>),
{
    curios_profile::profile!("lower_from_anf");
    observe(Stage::Anf(&anf_module));

    // Shrink before lowering: drop the items the program neither reaches nor runs for effect, so CPS's whole-module fixpoint sees only the live slice (see `curios_anf::optimize`). Verified rather than verifying: erasure sealed this very module through `finalize`, which verified it.
    curios_anf::optimize(&mut anf_module);

    observe(Stage::AnfOptm(&anf_module));

    let cps_module = lower_to_cps(&anf_module);

    observe(Stage::Cps(&cps_module));

    let mut cps_optm_module = cps_module;
    curios_cps::optimize(&mut cps_optm_module);
    curios_cps::rebuild_in_place(&mut cps_optm_module);

    observe(Stage::CpsOptm(&cps_optm_module));

    let (wasm_module, imported) = into_wasm(&cps_optm_module);

    observe(Stage::Wasm(&wasm_module));

    (wasm_module, imported)
}

/// Compile one unit against `predecessors`: lower, elaborate, judge, erase, `told` the unit's lowering as soon as it is made, whatever its compile goes on to conclude.
///
/// **The judgment sits between elaboration and erasure and that ordering is the point.** A module the kernel refuses never reaches erasure's budget, and a refusal reads as a refusal rather than as whatever erasure made of an ill-typed term. It is also why this is a fold step rather than one operation: the producer of a stored unit runs the same sequence *without* the judge, because the crate that writes an image deliberately cannot reach the kernel.
pub fn compile_unit(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    source: &UnitSource<'_>,
    told: &dyn Fn(&PreparedText),
) -> Result<Unit, CompileError> {
    curios_profile::profile!("compile_unit");
    let lowered = into_core_unit(source, &predecessors.text(), syntax)
        .map_err(|error| CompileError::Failure(vec![error.report()]))?;
    told(&lowered);

    compile_lowered(budget, predecessors, syntax, lowered, None).map(|(unit, _)| unit)
}

/// [`compile_unit`] from the unit's lowering on: elaborate, judge, erase. With `closing`, the unit is a program's declarations and the term that closes the program is elaborated in their walk and handed back beside the unit ([`Closing`]).
pub fn compile_lowered(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    mut lowered: PreparedText,
    closing: Option<Closing<'_>>,
) -> Result<(Unit, Option<curios_core::Entrypoint>), CompileError> {
    let cores = predecessors.cores();
    let (tail, mut elaborated, mut refused) = match closing {
        Some(Closing {
            tail,
            elaborated,
            refused,
        }) => (Some(tail), Some(elaborated), Some(refused)),
        None => (None, None, None),
    };

    let fixed = predecessors.fixed();
    let established = Established::placed(&predecessors.placed()).fixed(&fixed);
    let mut context = Context::new(budget, *syntax);
    context.set_imports(lowered.imports().clone());
    context.set_broken(lowered.broken_names());
    let walked = match tail {
        Some(tail) => elaborate_and_zonk_entry(
            &mut context,
            &established,
            lowered.core(),
            lowered.mounts(),
            lowered.minted(),
            tail,
        ),
        None => elaborate_and_zonk_unit(
            &mut context,
            &established,
            lowered.core(),
            lowered.mounts(),
            lowered.minted(),
        )
        .map(|(core, traces)| (core, None, traces)),
    };
    let walked = with_broken(
        lowered.broken(),
        walked.map_err(|error| {
            lowering_refusal(
                &error,
                &predecessors.lexicons(),
                &lowered,
                predecessors,
                syntax,
            )
        }),
    );
    // Whatever elaboration said: a lint is no part of its verdict.
    lowered.credit(&context.credited());
    let mut refuse = |lowered: &PreparedText, error: CompileError| {
        if let Some(refused) = &mut refused {
            refused(lowered);
        }

        error
    };
    let (core, entry, traces) = walked.map_err(|error| refuse(&lowered, error))?;
    if let (Some(elaborated), Some(entry)) = (&mut elaborated, &entry) {
        elaborated(&core, entry);
    }

    let core = Zonked::project(&core)
        .map_err(|refusal| refuse(&lowered, CompileError::failure(refusal.to_string())))?;

    // The unit is erased through the kernel that certified it.
    let globals = globals(predecessors);
    let (rechecked, mut kernel) = certify_definitions(&core, budget, &globals, *syntax);
    if let Some(verdict) = rechecked.verdicts.first() {
        let refusal = kernel_refusal(
            verdict,
            core.as_definitions(),
            &predecessors.lexicons(),
            syntax,
        );
        return Err(refuse(&lowered, refusal));
    }
    // The unit is the module the kernel stated, so every mark it carries is the walk's own.
    let core = core.map(|_| rechecked.definitions);

    let places = predecessors.places();
    let certifications = predecessors.certifications();
    let erased = predecessors.erased();
    let scope = Erasures::of(&cores, &places, &certifications, &erased).fixed(&fixed);
    let anf = erase_unit(&mut kernel, scope, &core).map_err(|error| {
        let refusal = erasure_refusal(
            &error,
            core.as_definitions(),
            &predecessors.lexicons(),
            &lowered,
            predecessors,
            syntax,
        );

        refuse(&lowered, refusal)
    })?;

    let core = core.into_definitions();

    Ok((
        Unit::new(lowered, core, anf, traces, rechecked.certification),
        entry,
    ))
}

/// Where a judged unit is kept between compilations.
///
/// Declared here and implemented in `curios-verdict`, because the fold is what consults one and this crate must never learn what a project is. What crosses the boundary is a unit — never a path, never a manifest, and never a key this crate would have to know how to build.
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
    fn baseline(&self, source: &UnitSource<'_>, offered: Option<&Unit>) -> Option<Earlier> {
        let _ = (source, offered);
        None
    }

    /// Record what `source` compiled to. Best effort — a store that cannot be written costs the next compilation the work, and nothing else.
    fn put(&self, source: &UnitSource<'_>, unit: &Unit);
}

/// A unit to compile another text of its sources over, with what it was compiled after and what the cache holds of what was made before: what a cache offers the fold on a miss.
pub struct Earlier {
    pub unit: Unit,
    pub after: After,
    /// What a declaration's elaboration, an item's erasure and its verdict may be taken from (`compile_unit_over`): `None` for a cache that keeps nothing of the kind.
    pub candidates: Option<Rc<dyn Candidates>>,
}

/// Which units in scope a baseline was compiled after, as whoever offers it can say.
///
/// What a baseline read of a unit it was compiled after stands, and what it read of any other is asked again, by each stage of its own record (`compile_unit_over`). So the answer decides how much is asked and nothing of what is concluded: an offer that names fewer units than it could asks more.
pub enum After {
    /// Every unit in scope: the baseline is an image of the unit as it stands among them.
    Scope,
    /// The units in scope claiming these prefixes, each as its unit claims them, and no other.
    Units(Vec<Vec<Qualifier>>),
}

impl After {
    /// Whether `unit`, in scope, is other than the one the baseline was compiled after. The compiler's own never is: a baseline is offered by the compiler that made it ([`Unit::fixed`]).
    pub fn moved(&self, unit: &Unit) -> bool {
        match self {
            After::Scope => false,
            After::Units(units) => {
                !unit.fixed()
                    && !units.iter().any(|prefixes| {
                        unit.mounts()
                            .iter()
                            .map(|mount| &mount.prefix)
                            .eq(prefixes.iter())
                    })
            }
        }
    }
}

/// Compile `sources` in dependency order, each against `base` and everything before it — the fold this whole design is named for.
///
/// Returns the units it produced, not the ones it was given: the caller owns `base` and this cannot take it. The predecessors are rebuilt per step from pointers to both, which is free.
///
/// A `cache` short-circuits the step entirely: a recorded unit was judged when it was recorded, so neither elaboration nor the kernel re-runs for it. `None` compiles everything, which is what every caller without a project does.
///
/// Each source arrives beside the baseline its assembler offers for it — `None` for every unit but one taking a prelude root's place, which is decided where the sources are built rather than recognized here. The offer goes to `cache` on a miss, which decides what becomes of it, and with no cache nothing is taken.
///
/// `read`, where there is one, is told the last unit's lowering as soon as it is made or restored (`ReadLowering`).
pub fn compile_units<'a, P>(
    budget: u64,
    base: Predecessors<'a>,
    syntax: &SyntaxRegistry,
    sources: &[(UnitSource<'_>, Option<&Unit>)],
    cache: Option<&dyn Cache>,
    mut progress: P,
    read: Option<ReadLowering<'_>>,
) -> Result<Vec<Unit>, CompileError>
where
    P: FnMut(Progress<'_>),
{
    let mut produced: Vec<Unit> = Vec::new();

    for (index, (source, offered)) in sources.iter().enumerate() {
        // The last unit is the fold's subject, and the one whose lowering a reader is told.
        let read = read.filter(|_| index + 1 == sources.len());
        let predecessors = base
            .units()
            .iter()
            .copied()
            .chain(produced.iter())
            .collect::<Vec<_>>();
        let told = |text: &PreparedText| {
            if let Some(read) = read {
                read(&predecessors, text, None);
            }
        };

        // Announced *after* the cache is consulted and *before* the work, so a reported operation is one that is actually about to happen and its timing starts where it does.
        let (hit, baseline) = held(source, *offered, cache);
        if let Some(unit) = hit {
            progress(Progress::Reused(&source.prefix()));
            // A unit read from Core text holds no lowering to tell of.
            if let Some(text) = unit.text() {
                told(text);
            }
            answered(source, &unit)?;
            produced.push(unit);
            continue;
        }

        let prefix = source.prefix();
        progress(match &baseline {
            Some(_) => Progress::Recompiling(&prefix),
            None => Progress::Compiling(&prefix),
        });

        let unit = match &baseline {
            Some(Earlier {
                unit: baseline,
                after,
                candidates,
            }) => compile_unit_over(
                budget,
                Predecessors::over(&predecessors),
                syntax,
                source,
                Over {
                    baseline,
                    moved: &predecessors
                        .iter()
                        .map(|unit| after.moved(unit))
                        .collect::<Vec<_>>(),
                    candidates: candidates.as_deref(),
                },
                &told,
            )?,
            None => compile_unit(
                budget,
                Predecessors::over(&predecessors),
                syntax,
                source,
                &told,
            )?,
        };
        progress(Progress::Compiled);

        if let Some(cache) = cache {
            curios_profile::profile!("cache::put" => cache.put(source, &unit));
        }

        answered(source, &unit)?;
        produced.push(unit);
    }

    Ok(produced)
}

/// What a compilation of `source` starts from: the unit `cache` holds for it, or the baseline `cache` compiles it over, `offered` being what whoever assembled the fold offers for the source. A baseline is asked for only on a miss: a hit is the empty case of a recompile, where nothing changed and everything is reused.
fn held(
    source: &UnitSource<'_>,
    offered: Option<&Unit>,
    cache: Option<&dyn Cache>,
) -> (Option<Unit>, Option<Earlier>) {
    let hit = curios_profile::profile!("cache::get" => cache.and_then(|cache| cache.get(source)));
    let baseline = match hit {
        Some(_) => None,
        None => curios_profile::profile!(
            "cache::baseline" => cache.and_then(|cache| cache.baseline(source, offered))
        ),
    };

    (hit, baseline)
}

/// The program `source` is, lowered over `earlier`, the unit [`held`] of its declarations with whether the units in scope are the ones it was compiled after: on every compile, over whatever earlier unit there is, the final term being no part of a unit and a declaration whose text and resolutions stand being the earlier one's.
fn lower_program(
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    source: &UnitSource<'_>,
    earlier: Option<(&Unit, bool)>,
) -> Result<(PreparedText, curios_core::Entrypoint), CompileError> {
    into_core_entry(
        source,
        &predecessors.text(),
        syntax,
        // A unit read from Core text offers no lowering to lower over, so a program is lowered whole over one.
        earlier.and_then(|(unit, settled)| unit.text().map(|text| (text, settled))),
    )
    .map_err(|error| CompileError::Failure(vec![error.report()]))
}

/// `source` lowered over `predecessors` and no further, handed to `then`: the unit's text stage, and beside it the term a program closes with, as the lowering wrote it, where `source` is one. A mounted unit `cache` holds is read as it stands; any other source is lowered over what `cache` holds or offers for it, a declaration whose text and resolutions stand being the earlier unit's.
///
/// **What a question about what is written reads.** A written name is resolved where it is lowered, so nothing here elaborates, judges or erases, nothing is handed to `cache`, and a subject that would be refused answers as one that checks: the outer refusal is a subject that did not lower.
pub fn lower_subject<T>(
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    source: &UnitSource<'_>,
    offered: Option<&Unit>,
    cache: Option<&dyn Cache>,
    then: impl FnOnce(&PreparedText, Option<&curios_core::Entrypoint>) -> T,
) -> Result<T, CompileError> {
    let (hit, baseline) = held(source, offered, cache);
    // Whether the units in scope are the ones `earlier` was compiled after.
    let settled = |earlier: &Earlier| {
        predecessors
            .units()
            .iter()
            .all(|unit| !earlier.after.moved(unit))
    };
    if source.is_entry() {
        // A hit was compiled after the units in scope, which is what made it one.
        let earlier = hit.as_ref().map(|unit| (unit, true)).or(baseline
            .as_ref()
            .map(|earlier| (&earlier.unit, settled(earlier))));
        let (lowered, entry) = lower_program(predecessors, syntax, source, earlier)?;

        return Ok(then(&lowered, Some(&entry)));
    }

    let text = predecessors.text();
    // A unit read from Core text holds no lowering, so where one is held or offered the source is lowered whole.
    let hit = hit.filter(|unit| unit.text().is_some());
    let baseline = baseline.filter(|earlier| earlier.unit.text().is_some());
    Ok(match (hit.as_ref().and_then(Unit::text), baseline) {
        (Some(lowered), _) => then(lowered, None),
        (None, Some(earlier)) => then(
            &into_core_unit_over(
                source,
                &text,
                syntax,
                earlier
                    .unit
                    .text()
                    .expect("a baseline holding no lowering is not offered"),
                settled(&earlier),
            )
            .map_err(|error| CompileError::Failure(vec![error.report()]))?,
            None,
        ),
        (None, None) => then(
            &into_core_unit(source, &text, syntax)
                .map_err(|error| CompileError::Failure(vec![error.report()]))?,
            None,
        ),
    })
}

/// Hold what `source`'s package says a module answers to the rows `unit` declares, a unit compiled now or restored alike.
///
/// A manifest names a declaration from its package's own root, so a claim lies under the unit's prefix by its spelling. Whether it names a `foreign` is asked here, of the rows the unit itself holds, so a manifest that has not caught up with a source file is refused where its unit is compiled and before the program that would reach the row.
fn answered(source: &UnitSource<'_>, unit: &Unit) -> Result<(), CompileError> {
    for (declaration, row) in source.answered() {
        if unit.foreigns().get(declaration).is_none() {
            let declared = unit
                .foreigns()
                .iter()
                .map(|function| format!("`{}`", function.name()))
                .collect::<Vec<_>>();

            return Err(CompileError::failure(format!(
                "the foreign row {row:?} says an export answers `{declaration}`, and {} declares no such `foreign`: {}",
                source.prefix().join(),
                match declared.is_empty() {
                    true => "it declares none".to_string(),
                    false => format!("it declares {}", declared.join(", ")),
                },
            )));
        }
    }

    Ok(())
}

/// Told a subject's lowering as soon as it is made — or restored, where the store held the unit — beside the units it was lowered against and, for a program, the term it closes with: what a question about what the subject writes reads, whatever the rest of its compile goes on to conclude.
///
/// **What was produced, told once, for the subject alone.** Beside [`Progress`] and the [`Stage`] observer rather than inside either: the first reports what is happening and the second the rungs `wonder --stage` prints, while this hands a reader the lowering a refused subject still has, so a question about what a file writes is answered from the check that compiled it.
pub type ReadLowering<'a> = &'a dyn Fn(&[&Unit], &PreparedText, Option<&curios_core::Entrypoint>);

/// What the fold is doing, for a caller that reports it.
///
/// Sequential by construction — the fold compiles one unit at a time — so [`Progress::Reused`] and [`Progress::Compiled`] refer to whichever subject was announced last and need not name it again.
///
/// Deliberately separate from the [`Stage`] observer, which belongs to `wonder --stage`: one reports *what is happening* and the other *what was produced*, and a caller wants either without the other.
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

/// What a caller is told of a program's compile as it goes: each rung it produced ([`Stage`]), and its lowering, as soon as it is made ([`ReadLowering`]).
pub struct Told<'a, O> {
    pub observe: &'a mut O,
    pub read: Option<ReadLowering<'a>>,
}

/// A program compiled under its tail, with what a test program's tail read: what [`Fold::tests`](crate::Fold::tests) hands back, and, its modules empty, what a program's compile is.
#[derive(Debug)]
pub struct TestProgram {
    /// Every test in scope, module by module, scheduled or skipped, in the order the tail runs them — the report metadata execution alone cannot recover. A test's index among every module's scheduled tests is the one it is run at.
    pub modules: Vec<TestModule>,
    /// Each prefix the tail selects tests at or under that selects none: what a run refuses where no subject's tail selects anything at it.
    pub unmatched: Vec<Qualifier>,
    /// The program and the rows it imports — `None` for a test tail that schedules no test, since a program running nothing is built past nothing but the kernel's judgment of it.
    pub compiled: Option<(curios_wasm::Module, ForeignStore)>,
}

/// Why a program's compile holds the program: only a test tail scheduling nothing leaves it unbuilt.
const A_PROGRAM_IS_BUILT: &str = "a program closing with its own term is built";

impl TestProgram {
    /// The program, from the compile of one closing with its own term.
    pub(crate) fn built(self) -> (curios_wasm::Module, ForeignStore) {
        self.compiled.expect(A_PROGRAM_IS_BUILT)
    }
}

/// Compile the program `source` is through the full pipeline under `tail` to a wasm module, telling the caller what `told` asks for: every [`Stage`] in order, and the program's lowering where it reads one. Beside the program, a test tail answers the tests it schedules and every test in scope; one scheduling no test is judged and built no further.
///
/// The program comes with the [`ForeignStore`] harvested from the `foreign` declarations of the program and of every predecessor ([`TestProgram::built`]) — an embedder that will run the module builds its `ffi`-tier bindings (`curios-native`'s `ForeignBindings`) from exactly this store, or drops it when the program declares none. Binaryen optimization and Cranelift precompilation are deliberately *not* here — they live downstream in the `curios` crate (`to_cwasm`), keeping this crate free of native backends.
///
/// Production erases over the archived prelude's erased forms: the program's declarations are compiled as the unit they are, taken from `cache` where it holds them and handed to it where they were compiled, only the term that closes the program is erased beside them, the program is linked from what that term reaches, the ANF optimizer shrinks and rebases the module, and the lowering into CPS makes every encoding decision once (see `curios_anf::lower_to_cps`).
///
/// **`source` is borrowed, so the caller still owns the loader it was made over when this returns.** Resolution records what it read through `&self` — the log is interior-mutable precisely so that lowering never has to thread `&mut` — and a caller filing what this compilation produced needs that log *after* the fold, exactly as it needs the cache handle's refusal after the fold.
pub fn compile_entrypoint<O>(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    source: &UnitSource<'_>,
    cache: Option<&dyn Cache>,
    tail: EntryTail,
    told: Told<'_, O>,
) -> Result<TestProgram, CompileError>
where
    O: FnMut(Stage<'_>),
{
    curios_profile::profile!("compile_entrypoint");
    let Told { observe, read } = told;
    // Compiling reports no lint, so the findings go unread.
    let (_, entered) = enter(
        budget,
        predecessors,
        syntax,
        source,
        cache,
        tail,
        Told {
            observe: &mut *observe,
            read,
        },
    )?;
    let mut entered = entered?;
    let compiled = match entered.erased.take() {
        None => None,
        Some(anf_module) => {
            let anf_module = anf_module?;

            // What an embedder is handed is the rows the module imports, which the emitter knows as it writes them: a program asks for an implementation of what it reaches, so a row a dependency declares and nothing calls costs its consumers nothing — no module read, linked or carried in an executable.
            Some(lower_from_anf(anf_module, observe))
        }
    };

    Ok(TestProgram {
        modules: entered.modules,
        unmatched: entered.unmatched,
        compiled,
    })
}
