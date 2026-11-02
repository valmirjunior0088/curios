//! `curios test` as the command line shows it: the subjects a selection declares tests in, each compiled as a test program and narrated as `run` narrates a compile, and one printer of what their tests come to — a subject's `↳ Testing` and `↳ Tested` steps, one line per test as it ends, a failing test's body, the count line — fed by a source of verdicts. The native source runs each test here, in an instantiation of its own, `[argv0, index]` as the program's arguments, what is compiled, filed and run being the library's (`curios::test_subjects`). The guest prints its own outcome line and answers its verdict, which is read off what its entry hands back and never off an exit code; what only the compiler knows — the failing declaration's body as written, the count line — is printed here from the records the compile hands back.
//!
//! **The printer writes each fact once and hands it back.** Every line it writes to the process's streams it also returns as the bytes written, so a source that answers a report elsewhere — a page's runner — carries the same rendering, and the terminal and the page cannot drift.

#[cfg(test)]
mod tests;

use {
    crate::{
        Access, Heading, Line, Steps, Subject, drained, fact, nested, note_unlinked, processing,
        report,
    },
    curios::{Named, TestSubject, Tested, test_subjects},
    curios_native::{OsHost, TestEnd},
    curios_package::Selection,
    curios_pipeline::{CompileError, TestRecord},
    curios_utilities::Qualifier,
    curios_verdicts::Verdicts,
    std::{
        cmp::Reverse,
        io::{self, Write},
    },
};

/// The tally a `↳ Tested` step and the count line report, in the order they spell it.
#[derive(Default)]
struct Totals {
    passed: usize,
    failed: usize,
    trapped: usize,
    exited: usize,
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
    }

    /// The tally: `passed` and `failed` always, since that pair is what the exit code turns on, and the rarer `trapped` and `exited` only when they happened.
    fn line(&self) -> String {
        let mut parts = vec![
            format!("{} passed", self.passed),
            format!("{} failed", self.failed),
        ];
        for (count, word) in [(self.trapped, "trapped"), (self.exited, "exited")] {
            if count > 0 {
                parts.push(format!("{count} {word}"));
            }
        }

        parts.join(", ")
    }
}

/// How one test ended, in the vocabulary every source reports in: the verdict its entry answered, the exit that left it unanswered, or the trap that stopped it. Natively it is what `TestRun::run` answers, read into this once.
pub(crate) enum Outcome {
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

/// Bytes on the two standard streams: what the printer wrote for one event, handed back so a reply can carry the same bytes, and what a guest wrote where a source hands its output over rather than the guest writing to the process's streams itself.
#[derive(Default)]
pub(crate) struct Streams {
    pub(crate) stdout: Vec<u8>,
    pub(crate) stderr: Vec<u8>,
}

impl Streams {
    /// Write both to the process's own streams, each whole.
    fn emit(&self) {
        io::stdout()
            .write_all(&self.stdout)
            .expect("failed printing to stdout");
        io::stderr()
            .write_all(&self.stderr)
            .expect("failed printing to stderr");
    }
}

/// Run the tests `selection` declares natively, narrowed to those at or under any of the paths `filters` where any is given, each declared subject filing what it compiled into a store `access` opens. `Ok(true)` when every selected test passed.
pub(crate) fn run_tests(
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
        let tested = compiled(budget, &subject, &shown, store.as_ref(), printer.under())?;
        printer.took(store.as_ref(), &tested);

        // A subject with nothing to run reports its compile and nothing more: a tally of zeros would only restate the absence of lines above it.
        let Some(run) = tested.loaded(&subject).map_err(CompileError::failure)? else {
            continue;
        };

        printer.testing(&shown);
        for (index, record) in tested.records().iter().enumerate() {
            printer.ended(record, run.run(index, OsHost::with_args).into(), None);
        }
        printer.tested(&shown);
    }

    printer.finish()
}

/// What a subject is reported as.
pub(crate) fn shown(named: &Named) -> Subject {
    match named {
        Named::Library(package) => Subject::package(package),
        Named::Executable(name) => Subject::Executable(name.clone()),
        Named::File(path) => Subject::File(path.clone()),
        Named::Stdin => Subject::Stdin,
    }
}

/// One subject's test program, compiled or reused, narrated as `run` narrates a compile: `Processing`, a `↳ Compiling` step for each unit it stands on, and `↳ Linking` for the program itself, which lasts until its machine code exists — a test program is the whole program the units link into, and its back end is most of what a cold run takes.
pub(crate) fn compiled(
    budget: u64,
    subject: &TestSubject,
    shown: &Subject,
    store: Option<&Verdicts>,
    under: &[Qualifier],
) -> Result<Tested, CompileError> {
    processing(shown, subject.manifest());

    let mut steps = Steps::default();
    let tested = subject.compile(budget, store, under, |progress| {
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

/// What a run of tests comes to, printed as it goes: the `↳ Testing` and `↳ Tested` steps around each subject's tests, one line per test as it ends, and the count line, each written to the process's streams and handed back as the bytes written — with the tally, every test the subjects declare, and the one reason a store refused filing.
pub(crate) struct Printer<'a> {
    /// The `--filter` arguments as written, for a refusal to name.
    filters: &'a [String],
    /// The prefixes every subject's test program schedules the tests at or under: those `filters` name, or the root where none is given.
    under: Vec<Qualifier>,
    totals: Totals,
    /// The subject in hand's tally, folded into the run's at its `↳ Tested` step.
    unit: Totals,
    /// Every test the subjects compiled so far declare, selected or not: what a filter meeting none is answered with.
    declared: Vec<Qualifier>,
    /// The first reason a store refused filing, since a store nobody can write refuses every subject for one reason.
    refusal: Option<String>,
}

impl<'a> Printer<'a> {
    pub(crate) fn new(filters: &'a [String]) -> Self {
        let under = match filters.is_empty() {
            true => vec![Qualifier::empty()],
            false => filters.iter().map(|filter| prefix(filter)).collect(),
        };

        Self {
            filters,
            under,
            totals: Totals::default(),
            unit: Totals::default(),
            declared: Vec::new(),
            refusal: None,
        }
    }

    /// The prefixes a subject's test program schedules the tests at or under.
    pub(crate) fn under(&self) -> &[Qualifier] {
        &self.under
    }

    /// Take what a subject's compile declared, and keep the first reason its store refused filing.
    pub(crate) fn took(&mut self, store: Option<&Verdicts>, tested: &Tested) {
        self.refusal = self
            .refusal
            .take()
            .or_else(|| store.and_then(Verdicts::refused));
        self.declared.extend(tested.declared().iter().copied());
    }

    /// The handover of `shown`'s tests to their scheduler: the `↳ Testing` step, opening the subject's tally.
    pub(crate) fn testing(&mut self, shown: &Subject) -> Streams {
        self.unit = Totals::default();

        let written = Streams {
            stdout: Vec::new(),
            stderr: format!("{}\n", nested(Heading::Testing, shown, None)).into_bytes(),
        };
        written.emit();

        written
    }

    /// One test of the subject in hand ended as `outcome`, `record` naming it: `guest`'s streams first, where a source hands the guest's output over rather than the guest writing to the process's streams itself, then what only the runner knows. The guest prints the outcome line of a test it survives; a trap or a stray exit never reaches the printing, so those lines are written here.
    pub(crate) fn ended(
        &mut self,
        record: &TestRecord,
        outcome: Outcome,
        guest: Option<&Streams>,
    ) -> Streams {
        let mut written = Streams::default();
        if let Some(guest) = guest {
            written.stdout.extend_from_slice(&guest.stdout);
            written.stderr.extend_from_slice(&guest.stderr);
        }

        let mut lines = String::new();
        match outcome {
            // The guest printed `path: passed` and answered that the test held.
            Outcome::Answered(true) => self.unit.passed += 1,
            // The guest printed `path: failed` and its report, and answered that the test did not hold; the body as written is what only the records know.
            Outcome::Answered(false) => {
                self.unit.failed += 1;
                body(record, &mut lines);
            }
            // The test exited on its own, before the scheduler could report — its line is written here, whatever the code: a verdict is answered, never exited with.
            Outcome::Exited(code) => {
                self.unit.exited += 1;
                lines.push_str(&format!("{}: exited {code}\n", record.path));
                body(record, &mut lines);
            }
            Outcome::Trapped(trap) => {
                self.unit.trapped += 1;
                lines.push_str(&format!("{}: trapped\n", record.path));
                for line in trap.lines() {
                    lines.push_str(&format!("  {line}\n"));
                }
                body(record, &mut lines);
            }
        }
        written.stdout.extend_from_slice(lines.as_bytes());
        written.emit();

        written
    }

    /// `shown`'s tests finished: the `↳ Tested` step with the subject's tally, folded into the run's.
    pub(crate) fn tested(&mut self, shown: &Subject) -> Streams {
        let written = Streams {
            stdout: Vec::new(),
            stderr: format!(
                "{}\n",
                nested(Heading::Tested, shown, Some(&self.unit.line()))
            )
            .into_bytes(),
        };
        self.totals.add(&self.unit);
        written.emit();

        written
    }

    /// A store's refusal, said once.
    pub(crate) fn skipped(&self) {
        if let Some(refusal) = &self.refusal {
            fact(
                Heading::Skipped,
                format!("storing what this built; {refusal}"),
            );
        }
    }

    /// Refuse each filter no test the subjects declare stands at or under, by name.
    pub(crate) fn unmatched(&self) -> Result<(), CompileError> {
        let unmatched = self
            .filters
            .iter()
            .filter(|filter| {
                let under = prefix(filter);
                !self.declared.iter().any(|test| test.is_within(&under))
            })
            .map(|filter| matching_nothing(filter, &self.declared))
            .collect::<Vec<_>>();

        match unmatched.is_empty() {
            true => Ok(()),
            false => Err(CompileError::failure(unmatched.join("\n"))),
        }
    }

    /// The count line, closing the run.
    pub(crate) fn tally(&self) -> Streams {
        let written = Streams {
            stdout: format!("{}\n", self.totals.line()).into_bytes(),
            stderr: Vec::new(),
        };
        written.emit();

        written
    }

    /// Whether every test that ran passed.
    pub(crate) fn green(&self) -> bool {
        self.totals.all_green()
    }

    /// The end of a native run: a store's refusal said once, each filter that matched nothing refused by name, and the count line.
    pub(crate) fn finish(self) -> Result<bool, CompileError> {
        self.skipped();
        self.unmatched()?;
        self.tally();

        Ok(self.green())
    }
}

/// The failing declaration's body as written, indented beneath the report — absent when no span survived to slice it from.
fn body(record: &TestRecord, lines: &mut String) {
    for line in record.body.lines() {
        lines.push_str(&format!("    {line}\n"));
    }
}
