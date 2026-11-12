//! `curios test` as the command line shows it: the subjects a selection declares tests in, each compiled as a test program and narrated as `run` narrates a compile, and one printer of what their tests come to — each module's `↳ Testing` and `↳ Tested` steps, one line per test as it ends, a failing test's body, the `Finished` tally — fed by a source of verdicts. The native source runs each test here, in an instantiation of its own, `[argv0, index]` as the program's arguments, what is compiled, filed and run being the library's (`curios::test_subjects`). The guest prints its own outcome line and answers its verdict, which is read off what its entry hands back and never off an exit code; what only the compiler knows — the failing declaration's body as written, the module each test stands in, the tests the selection skipped — is printed here from the modules the compile hands back.
//!
//! **The printer has one output.** Every line it writes goes through one path, to the process's streams and, where the printer keeps its report, onto that in the order written: a page's runner is sent the report, so no line reaches the terminal alone and the terminal and the page cannot drift.

#[cfg(test)]
mod tests;

use {
    crate::{
        Access, Failure, Heading, Line, Reporting, Run, Served, Steps, Subject, bound, drained,
        nested, note_unlinked, processing, report, serve_run, serving, unnested, url,
    },
    curios::{Named, TestSubject, Tested, test_subjects},
    curios_browser::{Posted, Step},
    curios_common::Qualifier,
    curios_native::{OsHost, TestEnd},
    curios_package::{Selection, Target},
    curios_pipeline::{CompileError, TestModule, TestRecord},
    curios_verdict::Verdicts,
    std::{
        cmp::Reverse,
        collections::VecDeque,
        io::{self, Write},
    },
};

/// The tally a `↳ Tested` step and the `Finished` line report, in the order they spell it.
#[derive(Default)]
struct Totals {
    passed: usize,
    failed: usize,
    trapped: usize,
    exited: usize,
    skipped: usize,
}

impl Totals {
    fn all_green(&self) -> bool {
        self.failed == 0 && self.trapped == 0 && self.exited == 0
    }

    fn add(&mut self, other: &Self) {
        self.passed += other.passed;
        self.failed += other.failed;
        self.trapped += other.trapped;
        self.exited += other.exited;
        self.skipped += other.skipped;
    }

    /// The tally: `passed` and `failed` always, since that pair is what the exit code turns on, and the rarer `trapped`, `exited` and `skipped` only when they happened.
    fn line(&self) -> String {
        let mut parts = vec![
            format!("{} passed", self.passed),
            format!("{} failed", self.failed),
        ];
        for (count, word) in [
            (self.trapped, "trapped"),
            (self.exited, "exited"),
            (self.skipped, "skipped"),
        ] {
            if count > 0 {
                parts.push(format!("{count} {word}"));
            }
        }

        parts.join(", ")
    }
}

/// How one test ended, in the vocabulary every source reports in: the verdict its entry answered, the exit that left it unanswered, or the trap that stopped it. Natively it is what `TestRun::run` answers, read into this once.
pub enum Outcome {
    Answered(bool),
    Exited(u8),
    Trapped(String),
}

impl From<Result<TestEnd, String>> for Outcome {
    fn from(end: Result<TestEnd, String>) -> Self {
        match end {
            Ok(TestEnd::Answered(held)) => Outcome::Answered(held),
            Ok(TestEnd::Exited(code)) => Outcome::Exited(code),
            Err(trap) => Outcome::Trapped(trap),
        }
    }
}

/// What a guest wrote on the two standard streams, where a source hands its output over rather than the guest writing to the process's streams itself.
pub struct Streams {
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
}

/// Run the tests `selection` declares natively, narrowed to those at or under any of the paths `filters` where any is given, each declared subject filing what it compiled into a store `access` opens. `Ok(true)` when every selected test passed.
pub fn run_tests(
    budget: u64,
    selection: Selection,
    access: Access,
    filters: &[String],
) -> Result<bool, CompileError> {
    if let Selection::Program(program) = &selection {
        note_unlinked(program, "is tested");
    }
    let mut printer = Printer::new(filters);

    for subject in test_subjects(selection, drained)? {
        let shown = shown(subject.named());
        let store = subject.filed(|root| access.filed(root));
        let tested = compiled(
            budget,
            &subject,
            &shown,
            store.as_ref(),
            printer.under(),
            Target::Native,
        )?;
        printer.took(store.as_ref(), &tested);

        let run = tested.loaded(&subject).map_err(CompileError::failure)?;
        printer.opened(tested.modules());
        // A subject that schedules nothing has no program to run, and every module it declares closed as it opened.
        let Some(run) = run else {
            continue;
        };

        for index in 0..tested.scheduled().count() {
            printer.ended(run.run(index, OsHost::with_args).into(), None);
        }
    }

    printer.finish()
}

/// What a subject is reported as.
pub fn shown(named: &Named) -> Subject {
    match named {
        Named::Library(package) => Subject::package(package),
        Named::Executable(name) => Subject::Executable(name.clone()),
        Named::File(path) => Subject::File(path.clone()),
        Named::Stdin => Subject::Stdin,
    }
}

/// One subject's test program for `target`, compiled or reused, narrated as `run` narrates a compile: `Processing`, a `↳ Compiling` step for each unit it stands on, and `↳ Linking` for the program itself, which lasts until its payload exists — a test program is the whole program the units link into, and its back end is most of what a cold run takes.
pub fn compiled(
    budget: u64,
    subject: &TestSubject,
    shown: &Subject,
    store: Option<&Verdicts>,
    under: &[Qualifier],
    target: Target,
) -> Result<Tested, CompileError> {
    processing(shown, subject.manifest());

    let mut steps = Steps::default();
    let tested = subject.compile(budget, store, under, target, |progress| {
        report(&mut steps, shown, progress);
    });
    match &tested {
        Ok(tested) if tested.reused() => {
            Line::nested(Heading::Linking, shown).outcome("reused");
            eprintln!();
        }
        Ok(_) => steps.finish(),
        Err(_) => steps.interrupted(),
    }

    tested
}

/// The prefix a `--filter` argument names: its path's segments, so `/app/Map`, `/app/Map/` and `app/Map` are one prefix and `/` is the root, which every test stands under.
fn prefix(filter: &str) -> Qualifier {
    Qualifier::from(filter.split('/').filter(|segment| !segment.is_empty()))
}

/// How many of the nearest paths a refusal names.
const NAMED: usize = 3;

/// The refusal of a `filter` no test of `declared` stands at or under, naming the paths nearest it: those sharing the most of its trailing segments, then those the fewest edits away.
fn matching_nothing(filter: &str, declared: &[Qualifier]) -> String {
    let mut nearest = declared.iter().map(Qualifier::join).collect::<Vec<_>>();
    // Ordered by path first, so a path two subjects declare is named once and ties fall in its order.
    nearest.sort();
    nearest.dedup();
    nearest.sort_by_key(|path| (Reverse(shared_tail(path, filter)), edits(path, filter)));
    nearest.truncate(NAMED);

    let named = nearest
        .iter()
        .map(|path| format!("'{path}'"))
        .collect::<Vec<_>>();
    match named.as_slice() {
        [] => format!("no test matches '{filter}'"),
        [only] => format!("no test matches '{filter}'; the nearest is {only}"),
        [rest @ .., last] => format!(
            "no test matches '{filter}'; the nearest are {} and {last}",
            rest.join(", ")
        ),
    }
}

/// How many trailing segments two test paths share.
fn shared_tail(path: &str, filter: &str) -> usize {
    path.rsplit('/')
        .zip(filter.trim_end_matches('/').rsplit('/'))
        .take_while(|(this, that)| this == that && !this.is_empty())
        .count()
}

/// The edits that turn one path into the other: insertions, deletions and substitutions of a character.
fn edits(path: &str, filter: &str) -> usize {
    let filter = filter.chars().collect::<Vec<_>>();
    let mut row = (0..=filter.len()).collect::<Vec<_>>();
    for (i, left) in path.chars().enumerate() {
        let mut diagonal = row[0];
        row[0] = i + 1;
        for (j, right) in filter.iter().enumerate() {
            let substituted = diagonal + usize::from(left != *right);
            diagonal = row[j + 1];
            row[j + 1] = substituted.min(row[j] + 1).min(diagonal + 1);
        }
    }

    row[filter.len()]
}

/// What a run of tests comes to, printed as it goes: the `↳ Testing` and `↳ Tested` steps around each module's tests, one line per test as it ends, and the `Finished` tally, each written to the process's streams and, where the printer keeps its report, kept in the order written — with the tally, every test the subjects declare, and the one reason a store refused filing.
pub struct Printer<'a> {
    /// Everything written so far, in the order written, where the printer keeps its report: a browser run's, which every page that drives it is sent.
    report: Option<Vec<u8>>,
    /// The `--filter` arguments as written, for a refusal to name.
    filters: &'a [String],
    /// The prefixes every subject's test program schedules the tests at or under: those `filters` name, or the root where none is given.
    under: Vec<Qualifier>,
    totals: Totals,
    /// The module in hand's tally, folded into the run's at its `↳ Tested` step.
    tally: Totals,
    /// The modules of the subject in hand not yet closed, the one in hand first.
    modules: VecDeque<TestModule>,
    /// How many of the scheduled tests of the module in hand have ended: the index of the next one's record.
    ended: usize,
    /// How many `↳ Tested` steps the run wrote: `Finished` would repeat the one, so it closes a run of any other number.
    reported: usize,
    /// Whether a subject's compile so far selected a test under each of `filters`: one none selected under is refused.
    matched: Vec<bool>,
    /// Every test the subjects compiled so far declare, selected or not: the paths a refused filter is answered with the nearest of.
    declared: Vec<Qualifier>,
    /// The first reason a store refused filing, since a store nobody can write refuses every subject for one reason.
    refusal: Option<String>,
}

impl<'a> Printer<'a> {
    pub fn new(filters: &'a [String]) -> Self {
        let under = match filters.is_empty() {
            true => vec![Qualifier::empty()],
            false => filters.iter().map(|filter| prefix(filter)).collect(),
        };

        Self {
            report: None,
            filters,
            under,
            totals: Totals::default(),
            tally: Totals::default(),
            modules: VecDeque::new(),
            ended: 0,
            reported: 0,
            matched: vec![false; filters.len()],
            declared: Vec::new(),
            refusal: None,
        }
    }

    /// A printer that keeps its report from its first write.
    pub fn keeping(filters: &'a [String]) -> Self {
        Self {
            report: Some(Vec::new()),
            ..Self::new(filters)
        }
    }

    /// Everything written so far, in the order written: all of it where the printer keeps its report, and nothing where it does not.
    pub fn report(&self) -> &[u8] {
        self.report.as_deref().unwrap_or_default()
    }

    /// The one path every line takes: `bytes` written to `stream`, one of the process's own, and kept where the printer keeps its report.
    fn write(&mut self, mut stream: impl Write, bytes: &[u8]) {
        stream.write_all(bytes).expect("failed printing");
        if let Some(report) = &mut self.report {
            report.extend_from_slice(bytes);
        }
    }

    /// The prefixes a subject's test program schedules the tests at or under.
    pub fn under(&self) -> &[Qualifier] {
        &self.under
    }

    /// Take what a subject's compile declared and which filters it selected a test under, and keep the first reason its store refused filing.
    pub fn took(&mut self, store: Option<&Verdicts>, tested: &Tested) {
        self.refusal = self
            .refusal
            .take()
            .or_else(|| store.and_then(Verdicts::refused));
        for (prefix, matched) in self.under.iter().zip(&mut self.matched) {
            *matched |= !tested.unmatched().contains(prefix);
        }
        self.declared.extend(tested.declared().copied());
    }

    /// The subject whose tests are `modules` taken on, and every line up to its first scheduled test: each module's `↳ Testing` step, followed at once by its `↳ Tested` step where it schedules none.
    pub fn opened(&mut self, modules: &[TestModule]) {
        self.modules.extend(modules.iter().cloned());

        let lines = self.advanced();
        self.write(io::stderr(), lines.as_bytes());
    }

    /// The lines that take the subject in hand to its next scheduled test: each module's `↳ Testing` step, followed at once by its `↳ Tested` step where it schedules none.
    fn advanced(&mut self) -> String {
        let mut lines = String::new();
        while let Some(module) = self.modules.front() {
            lines.push_str(&format!(
                "{}\n",
                nested(Heading::Testing, &Subject::Mounted(module.path), None)
            ));
            match self
                .modules
                .pop_front_if(|module| module.scheduled.is_empty())
            {
                Some(skipped) => lines.push_str(&self.closed(skipped)),
                None => break,
            }
        }

        lines
    }

    /// `module`'s `↳ Tested` step: the tally of its tests that ran and of those it skipped, folded into the run's.
    fn closed(&mut self, module: TestModule) -> String {
        self.tally.skipped = module.skipped.len();
        let line = format!(
            "{}\n",
            nested(
                Heading::Tested,
                &Subject::Mounted(module.path),
                Some(&self.tally.line())
            )
        );
        self.totals.add(&self.tally);
        self.tally = Totals::default();
        self.ended = 0;
        self.reported += 1;

        line
    }

    /// Whether a module of the subject in hand is still open: one of its scheduled tests is yet to end.
    pub fn in_hand(&self) -> bool {
        !self.modules.is_empty()
    }

    /// The next scheduled test of the module in hand ended as `outcome`: `guest`'s streams first, where a source hands the guest's output over rather than the guest writing to the process's streams itself, then what only the runner knows, and the module's `↳ Tested` step where it was the module's last. The guest prints the outcome line of a test it survives; a trap or a stray exit never reaches the printing, so those lines are written here.
    pub fn ended(&mut self, outcome: Outcome, guest: Option<&Streams>) {
        if let Some(guest) = guest {
            self.write(io::stdout(), &guest.stdout);
            self.write(io::stderr(), &guest.stderr);
        }

        let record = &self.modules[0].scheduled[self.ended];
        let mut lines = String::new();
        match outcome {
            // The guest printed `path: passed` and answered that the test held.
            Outcome::Answered(true) => self.tally.passed += 1,
            // The guest printed `path: failed` and its report, and answered that the test did not hold; the body as written is what only the records know.
            Outcome::Answered(false) => {
                self.tally.failed += 1;
                body(record, &mut lines);
            }
            // The test exited on its own, before the scheduler could report — its line is written here, whatever the code: a verdict is answered, never exited with.
            Outcome::Exited(code) => {
                self.tally.exited += 1;
                lines.push_str(&format!("{}: exited {code}\n", record.path.join()));
                body(record, &mut lines);
            }
            Outcome::Trapped(trap) => {
                self.tally.trapped += 1;
                lines.push_str(&format!("{}: trapped\n", record.path.join()));
                for line in trap.lines() {
                    lines.push_str(&format!("  {line}\n"));
                }
                body(record, &mut lines);
            }
        }
        self.write(io::stdout(), lines.as_bytes());

        self.ended += 1;
        let ended = self.ended;
        if let Some(module) = self
            .modules
            .pop_front_if(|module| module.scheduled.len() == ended)
        {
            let lines = self.closed(module) + &self.advanced();
            self.write(io::stderr(), lines.as_bytes());
        }
    }

    /// A store's refusal, said once.
    pub fn skipped(&mut self) {
        if let Some(refusal) = &self.refusal {
            let line = format!(
                "{}\n",
                unnested(
                    Heading::Skipped,
                    format!("storing what this built; {refusal}")
                )
            );
            self.write(io::stderr(), line.as_bytes());
        }
    }

    /// Refuse each filter no subject's compile selected a test under, by name.
    pub fn unmatched(&self) -> Result<(), CompileError> {
        let unmatched = self
            .filters
            .iter()
            .zip(&self.matched)
            .filter(|(_, matched)| !**matched)
            .map(|(filter, _)| matching_nothing(filter, &self.declared))
            .collect::<Vec<_>>();

        match unmatched.is_empty() {
            true => Ok(()),
            false => Err(CompileError::failure(unmatched.join("\n"))),
        }
    }

    /// The `Finished` line, closing the run with its tally — left out where it would repeat the one `↳ Tested` step the run wrote.
    pub fn finished(&mut self) {
        if self.reported != 1 {
            let line = format!("{}\n", unnested(Heading::Finished, self.totals.line()));
            self.write(io::stderr(), line.as_bytes());
        }
    }

    /// Whether every test that ran passed.
    pub fn green(&self) -> bool {
        self.totals.all_green()
    }

    /// The end of a native run: a store's refusal said once, each filter that matched nothing refused by name, and the `Finished` line.
    pub fn finish(mut self) -> Result<bool, CompileError> {
        self.skipped();
        self.unmatched()?;
        self.finished();

        Ok(self.green())
    }
}

/// The failing declaration's body as written, indented beneath the report — absent when no span survived to slice it from.
fn body(record: &TestRecord, lines: &mut String) {
    for line in record.body.lines() {
        lines.push_str(&format!("    {line}\n"));
    }
}

/// Serve the tests `selection` declares to a browser on `port`, narrowed to those at or under any of the paths `filters` where any is given, each declared subject filing what it compiled into a store `access` opens: every subject built as a page before anything is served, then the runner at the root, the socket it drives the run over, and the pages beside them one subject at a time, each outcome taken as the step in hand's and printed as native prints a test's end. `Ok(true)` when every selected test passed.
pub fn serve_tests(
    budget: u64,
    selection: Selection,
    access: Access,
    filters: &[String],
    port: u16,
) -> Result<bool, Failure> {
    if let Selection::Program(program) = &selection {
        note_unlinked(program, "is tested");
    }
    let mut printer = Printer::keeping(filters);
    // Each served subject's modules, in the order the runner opens its pages.
    let mut planned = Vec::new();
    let mut pages = Vec::new();

    for subject in test_subjects(selection, drained)? {
        let shown = shown(subject.named());
        let store = subject.filed(|root| access.filed(root));
        let tested = compiled(
            budget,
            &subject,
            &shown,
            store.as_ref(),
            printer.under(),
            Target::Browser,
        )?;
        printer.took(store.as_ref(), &tested);

        // A subject that schedules nothing is never served, and every module it declares closes as it opens.
        match tested.page(&subject)? {
            Some(files) => {
                planned.push(tested.modules().to_vec());
                pages.push((Served { files }, tested.scheduled().count()));
            }
            None => {
                printer.opened(tested.modules());
            }
        }
    }

    // What native says at its end is said before anything is served: a store's refusal, and a filter matching nothing, which ends the run.
    printer.skipped();
    printer.unmatched()?;
    if planned.is_empty() {
        printer.finished();

        return Ok(printer.green());
    }

    let listener = bound(port)?;
    let port = listener
        .local_addr()
        .map_err(|error| format!("cannot read the address served on: {error}"))?
        .port();

    serving(&url(port, &[]));
    printer.opened(&planned[0]);

    let run = Run::new(
        pages,
        Printing {
            printer: &mut printer,
            planned: &planned,
        },
    );
    serve_run(&listener, run)?;

    Ok(printer.green())
}

/// A browser run's printer, with each served subject's modules in the order the runner opens its pages: what the run hands each outcome to, and whose report it sends.
struct Printing<'p, 'a> {
    printer: &'p mut Printer<'a>,
    planned: &'p [Vec<TestModule>],
}

impl Reporting for Printing<'_, '_> {
    fn took(&mut self, step: Step, guest: Streams, outcome: &Posted) {
        self.printer.ended(outcome_of(outcome), Some(&guest));

        // The subject's last test closes it, and opens the next subject or ends the run.
        if !self.printer.in_hand() {
            match self.planned.get(step.subject + 1) {
                Some(next) => self.printer.opened(next),
                None => self.printer.finished(),
            }
        }
    }

    fn written(&self) -> &[u8] {
        self.printer.report()
    }
}

/// How a test ended, read off what its page posted: a trap, the word its entry answered, or the exit that left it unanswered — read once here, as native's end is read into [`Outcome`] once. A page that posted none of the three, which the harness never does, is read as a trap saying so.
fn outcome_of(posted: &Posted) -> Outcome {
    match (&posted.trap, posted.held, posted.exit_code) {
        (Some(trap), _, _) => Outcome::Trapped(trap.clone()),
        (None, Some(held), _) => Outcome::Answered(held),
        (None, None, Some(code)) => Outcome::Exited(code),
        (None, None, None) => {
            Outcome::Trapped("the page reported neither a word, an exit nor a trap".to_string())
        }
    }
}
