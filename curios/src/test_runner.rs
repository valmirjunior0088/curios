//! `curios test` as the command line shows it: the subjects a selection declares tests in, each compiled as a test program and narrated as `run` narrates a compile, and one printer of what their tests come to, on each subject's own tree after its build — a node for each test, opened by its path before it runs and closed by how it ended, the subject's tally as the node that closes its tree, and the run's as the last line, each with what its tests took — fed by a source of verdicts. The native source runs each test here, in an instantiation of its own, `[argv0, index]` as the program's arguments, what is compiled, filed and run being the library's (`curios::test_subjects`). A test says what went wrong itself, on its own standard error, and answers its verdict, which is read off what its entry hands back and never off an exit code; what only the runner knows — the test's path, how it ended, the failing declaration's body as written, the tests the selection skipped — is written here from the modules the compile hands back.
//!
//! **A test's own output is never held.** Natively it goes where the test wrote it, as it is written, and the test's host ([`TestHost`]) tells the runner that it wrote; a page's is relayed when its outcome arrives. Either way the first byte ends the line the test's node opened on, and the node closes beneath what was written.
//!
//! **Everything written here is the transcript's**, so a browser run, which keeps its transcript, sends every page that drives it what the terminal showed.

#[cfg(test)]
mod tests;

mod host;
use host::*;

use {
    crate::{
        Access, Column, Failure, Reporting, Run, Served, Steps, Subject, Then, Tree, Wrote, alone,
        answered, bound, cut, drained, framed, keep, kept, port_of, reader, relayed, serve_run,
        total, troubled, url,
    },
    curios::{TestSubject, Tested, test_subjects},
    curios_browser::{Posted, Step, Stream},
    curios_common::{Qualifier, Report, Style},
    curios_native::{OsHost, TestEnd},
    curios_package::{Refused, Selection, Target},
    curios_pipeline::{CompileError, TestModule},
    curios_verdict::Verdicts,
    std::{
        cell::RefCell,
        cmp::Reverse,
        collections::VecDeque,
        io, mem,
        sync::Arc,
        time::{Duration, Instant},
    },
};

/// The tally a subject's closing node and the run's last line report, in the order they spell it.
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

    /// The tally: `passed` and `failed` always, since that pair is what the exit code turns on, and the rarer `trapped`, `exited` and `skipped` only when they happened. A count of what went wrong is painted as trouble where it is not zero.
    fn line(&self) -> String {
        let wrong = |count: usize, word: &str| match count {
            0 => format!("0 {word}"),
            count => troubled(format!("{count} {word}")),
        };
        let mut parts = vec![
            format!("{} passed", self.passed),
            wrong(self.failed, "failed"),
        ];
        for (count, word) in [(self.trapped, "trapped"), (self.exited, "exited")] {
            if count > 0 {
                parts.push(wrong(count, word));
            }
        }
        if self.skipped > 0 {
            parts.push(format!("{} skipped", self.skipped));
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

/// Run the tests `selection` declares natively, narrowed to those at or under any of the paths `filters` where any is given, each declared subject filing what it compiled into a store `access` opens. `Ok(true)` when every selected test passed.
pub fn run_tests(
    budget: u64,
    selection: Selection,
    access: Access,
    filters: &[String],
) -> Result<bool, CompileError> {
    let mut printer = Printer::new(filters);

    for subject in test_subjects(selection, || Ok(drained()?))? {
        let shown = Subject::of(subject.named());
        let store = subject.filed(|root| access.filed(root));
        let (tested, tree) = compiled(
            budget,
            &subject,
            &shown,
            store.as_ref(),
            printer.under(),
            Target::Native,
            Then::More,
        )?;
        printer.took(&tested);

        let run = tested.loaded(&subject).map_err(CompileError::failure)?;
        printer.opened(tree, tested.modules());
        // A subject that schedules nothing has no program to run, and closed as it opened.
        let Some(run) = run else {
            continue;
        };

        for index in 0..tested.scheduled().count() {
            let frame = Arc::new(Frame::default());

            printer.began();
            let began = Instant::now();
            let end = run.run(index, |arguments| {
                TestHost::new(OsHost::with_args(arguments), Arc::clone(&frame))
            });
            printer.ended(end.into(), frame.wrote(), began.elapsed());
        }
    }

    printer.finish()
}

/// One subject's test program for `target`, compiled or reused, narrated as `run` narrates a compile: a tree for each unit it had to compile, and the subject's own, whose `emit` node lasts until its payload exists — a test program is the whole program the units link into, and its back end is most of what a cold run takes. One the store held narrates nothing. The subject's tree comes back with the program, for the tests `then` says follow on it.
pub fn compiled(
    budget: u64,
    subject: &TestSubject,
    shown: &Subject,
    store: Option<&Verdicts>,
    under: &[Qualifier],
    target: Target,
    then: Then,
) -> Result<(Tested, Tree), CompileError> {
    // Two callbacks write one narration, so it is lent to each in turn.
    let steps = RefCell::new(Steps::program(shown, subject.manifest(), then));
    let tested = subject.compile(
        budget,
        store,
        under,
        target,
        |stage| steps.borrow_mut().reached(&stage),
        |progress| steps.borrow_mut().told(progress, store),
    );
    let mut steps = steps.into_inner();

    steps.ended(&tested);

    tested.map(|tested| (tested, steps.into_tree()))
}

/// The prefix a `--filter` argument names: its path's segments, so `/app/Map`, `/app/Map/` and `app/Map` are one prefix and `/` is the root, which every test stands under.
fn prefix(filter: &str) -> Qualifier {
    Qualifier::from(filter.split('/').filter(|segment| !segment.is_empty()))
}

/// How many of the nearest paths a refusal names.
const NAMED: usize = 3;

/// The refusal of a `filter` no test of `declared` stands at or under, naming the paths nearest it: those sharing the most of its trailing segments, then those the fewest edits away.
fn matching_nothing(filter: &str, declared: &[Qualifier]) -> Box<Report> {
    let mut nearest = declared.iter().map(Qualifier::join).collect::<Vec<_>>();
    // Ordered by path first, so a path two subjects declare is named once and ties fall in its order.
    nearest.sort();
    nearest.dedup();
    nearest.sort_by_key(|path| (Reverse(shared_tail(path, filter)), edits(path, filter)));
    nearest.truncate(NAMED);

    let refused = Refused::of("unknown test").stating(format_args!("no test matches `{filter}`"));
    match nearest.is_empty() {
        true => refused,
        false => refused.stating(format_args!(
            "the nearest: {}",
            nearest
                .iter()
                .map(|path| format!("`{path}`"))
                .collect::<Vec<_>>()
                .join(", ")
        )),
    }
    .about_named("--filter")
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

/// A subject whose tests are being run: its tree, its scheduled tests yet to end, and what those that ended came to.
struct Hand {
    tree: Tree,
    /// The scheduled tests yet to end, in schedule order, the one in hand first.
    scheduled: VecDeque<Qualifier>,
    tally: Totals,
    /// What the tests that ended took together, where any ran.
    spent: Option<Duration>,
}

/// The printer is told of a test only while the subject that schedules it is in hand.
const IN_HAND: &str = "a test is told of while its subject is in hand";

/// What a run of tests comes to, written as it goes on each subject's tree: a node for each scheduled test, the subject's tally as the node that closes its tree, and the run's as the last line — with every test the subjects declare, and the one reason a store refused filing.
pub struct Printer<'a> {
    /// The `--filter` arguments as written, for a refusal to name.
    filters: &'a [String],
    /// The prefixes every subject's test program schedules the tests at or under: those `filters` name, or the root where none is given.
    under: Vec<Qualifier>,
    /// The run's tally: that of every subject closed so far.
    totals: Totals,
    /// What the tests of every subject closed so far took together, where any ran.
    spent: Option<Duration>,
    /// The subject in hand, from its being taken on to its last scheduled test's end.
    hand: Option<Hand>,
    /// Whether a subject's compile so far selected a test under each of `filters`: one none selected under is refused.
    matched: Vec<bool>,
    /// Every test the subjects compiled so far declare, selected or not: the paths a refused filter is answered with the nearest of.
    declared: Vec<Qualifier>,
}

impl<'a> Printer<'a> {
    pub fn new(filters: &'a [String]) -> Self {
        let under = match filters.is_empty() {
            true => vec![Qualifier::empty()],
            false => filters.iter().map(|filter| prefix(filter)).collect(),
        };

        Self {
            filters,
            under,
            totals: Totals::default(),
            spent: None,
            hand: None,
            matched: vec![false; filters.len()],
            declared: Vec::new(),
        }
    }

    /// The prefixes a subject's test program schedules the tests at or under.
    pub fn under(&self) -> &[Qualifier] {
        &self.under
    }

    /// Take what a subject's compile declared and which filters it selected a test under.
    pub fn took(&mut self, tested: &Tested) {
        for (prefix, matched) in self.under.iter().zip(&mut self.matched) {
            *matched |= !tested.unmatched().contains(prefix);
        }
        self.declared.extend(tested.declared().copied());
    }

    /// Take on the subject whose tree is `tree` and whose tests are `modules`: the tests it schedules in the order its program runs them, those it skips counted. One that schedules none closes at once, with its tally.
    pub fn opened(&mut self, tree: Tree, modules: &[TestModule]) {
        self.hand = Some(Hand {
            tree,
            scheduled: modules
                .iter()
                .flat_map(|module| module.scheduled.iter().cloned())
                .collect(),
            tally: Totals {
                skipped: modules.iter().map(|module| module.skipped.len()).sum(),
                ..Totals::default()
            },
            spent: None,
        });
        self.settled();
    }

    /// Close the subject in hand where its last scheduled test has ended: its tally and what its tests took, as the node that closes its tree, folded into the run's.
    fn settled(&mut self) {
        if let Some(hand) = self.hand.take_if(|hand| hand.scheduled.is_empty()) {
            hand.tree.summed(&hand.tally.line(), hand.spent);
            self.totals.add(&hand.tally);
            self.spent = match (self.spent, hand.spent) {
                (Some(run), Some(subject)) => Some(run + subject),
                (run, subject) => run.or(subject),
            };
        }
    }

    /// Whether a subject is in hand: one of its scheduled tests is yet to end.
    pub fn in_hand(&self) -> bool {
        self.hand.is_some()
    }

    /// Open the node of the subject in hand's next scheduled test, by its path, before it runs: a test that never ends is the one whose line stands open.
    pub fn began(&mut self) {
        let hand = self.hand.as_mut().expect(IN_HAND);
        let path = hand.scheduled.front().expect(IN_HAND).join();

        hand.tree.open(&path, false);
    }

    /// The test whose node is open ended as `outcome` after `taken`, having `wrote` of its own or not: its node closed with how it ended, then a trap's text, which no test writes and only the runner holds, and the subject's tally where it was the subject's last.
    pub fn ended(&mut self, outcome: Outcome, wrote: Wrote, taken: Duration) {
        let hand = self.hand.as_mut().expect(IN_HAND);
        hand.scheduled.pop_front().expect(IN_HAND);
        let (verdict, beneath) = match outcome {
            Outcome::Answered(true) => {
                hand.tally.passed += 1;

                ("passed".to_string(), Vec::new())
            }
            // The test said what went wrong itself, on its own stream, above its verdict.
            Outcome::Answered(false) => {
                hand.tally.failed += 1;

                ("failed".to_string(), Vec::new())
            }
            // The test exited on its own before its entry answered, whatever the code: a verdict is answered, never exited with.
            Outcome::Exited(code) => {
                hand.tally.exited += 1;

                (format!("exited {code}"), Vec::new())
            }
            Outcome::Trapped(trap) => {
                hand.tally.trapped += 1;

                (
                    "trapped".to_string(),
                    trap.lines().map(str::to_string).collect(),
                )
            }
        };

        hand.tree.ended(&verdict, taken, wrote, &beneath);
        hand.spent = Some(hand.spent.unwrap_or_default() + taken);
        self.settled();
    }

    /// Refuse each filter no subject's compile selected a test under, by name.
    pub fn unmatched(&self) -> Result<(), CompileError> {
        let unmatched = self
            .filters
            .iter()
            .zip(&self.matched)
            .filter(|(_, matched)| !**matched)
            .map(|(filter, _)| *matching_nothing(filter, &self.declared))
            .collect::<Vec<_>>();

        match unmatched.is_empty() {
            true => Ok(()),
            false => Err(CompileError::Failure(unmatched)),
        }
    }

    /// The run's tally, as its last line, with what every test took together where any ran.
    pub fn finished(&self) {
        total(self.totals.line(), self.spent);
    }

    /// Whether every test that ran passed.
    pub fn green(&self) -> bool {
        self.totals.all_green()
    }

    /// The end of a native run: each filter that matched nothing refused by name, and the run's tally.
    pub fn finish(self) -> Result<bool, CompileError> {
        self.unmatched()?;
        self.finished();

        Ok(self.green())
    }
}

/// Serve the tests `selection` declares to a browser on `port`, narrowed to those at or under any of the paths `filters` where any is given, each declared subject filing what it compiled into a store `access` opens: every subject built as a page before anything is served, then the runner at the root, the socket it drives the run over, and the pages beside them one subject at a time, each outcome taken as the step in hand's and written as native writes a test's end, on its subject's tree, named again past the address served. `Ok(true)` when every selected test passed.
pub fn serve_tests(
    budget: u64,
    selection: Selection,
    access: Access,
    filters: &[String],
    port: u16,
) -> Result<bool, Failure> {
    // Before the first line, so a page is sent the run whole.
    keep();
    let mut printer = Printer::new(filters);
    // Each served subject's tree and modules, in the order the runner opens its pages.
    let mut planned = VecDeque::new();
    let mut pages = Vec::new();

    for subject in test_subjects(selection, || Ok(drained()?))? {
        let shown = Subject::of(subject.named());
        let store = subject.filed(|root| access.filed(root));
        let (tested, tree) = compiled(
            budget,
            &subject,
            &shown,
            store.as_ref(),
            printer.under(),
            Target::Browser,
            Then::Nothing,
        )?;
        printer.took(&tested);

        match tested.page(&subject)? {
            // Its tests are run once every subject is built and the address is said, so its build's tree ends here and its tests stand on one of their own.
            Some(files) => {
                planned.push_back((
                    Tree::of(&shown, subject.manifest()),
                    tested.modules().to_vec(),
                ));
                pages.push((Served { files }, tested.scheduled().count()));
            }
            // A subject that schedules nothing is never served, and closes as it opens.
            None => printer.opened(tree, tested.modules()),
        }
    }

    // What native says at its end is said before anything is served: a filter matching nothing, which ends the run.
    printer.unmatched()?;
    let Some((tree, modules)) = planned.pop_front() else {
        printer.finished();

        return Ok(printer.green());
    };

    let listener = bound(port)?;
    let port = port_of(&listener)?;

    alone("serve", url(port, &[]));
    printer.opened(tree, &modules);

    let run = Run::new(
        pages,
        Printing {
            printer: &mut printer,
            planned,
            wrote: false,
            column: Column::Start,
        },
    );
    serve_run(&listener, run)?;

    Ok(printer.green())
}

/// A browser run's printer, with each served subject yet to be taken on, its tree and its modules, in the order the runner opens its pages: what the run tells of each step, and whose transcript it sends.
struct Printing<'p, 'a> {
    printer: &'p mut Printer<'a>,
    planned: VecDeque<(Tree, Vec<TestModule>)>,
    /// Whether the test in hand has written anything, and where its writes stand in their line: what a native test's host holds of it.
    wrote: bool,
    column: Column,
}

impl Reporting for Printing<'_, '_> {
    fn began(&mut self, _step: Step) {
        self.printer.began();
        self.wrote = false;
        self.column = Column::Start;
    }

    /// Relay what a page's test wrote as a native test's own writes fall: the line its node opened on ended ahead of the first byte, then the bytes, each line behind its frame, on this process's stream of the same name.
    fn wrote(&mut self, stream: Stream, bytes: &[u8]) {
        if bytes.is_empty() {
            return;
        }
        if !mem::replace(&mut self.wrote, true) {
            cut();
        }

        // What is kept is what was shown, unpainted, from the same column.
        let kept = framed(&mut self.column.clone(), bytes, &Style::plain());
        match stream {
            Stream::Stdout => relayed(
                io::stdout(),
                &framed(&mut self.column, bytes, answered()),
                &kept,
            ),
            Stream::Stderr => relayed(
                io::stderr(),
                &framed(&mut self.column, bytes, reader()),
                &kept,
            ),
        }
    }

    fn took(&mut self, _step: Step, outcome: &Posted, taken: Duration) {
        self.printer.ended(
            outcome_of(outcome),
            Wrote::of(self.wrote, self.column),
            taken,
        );

        // The subject's last test closes it, and opens the next subject or ends the run.
        if !self.printer.in_hand() {
            match self.planned.pop_front() {
                Some((tree, modules)) => self.printer.opened(tree, &modules),
                None => self.printer.finished(),
            }
        }
    }

    fn written(&self) -> Vec<u8> {
        kept()
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
