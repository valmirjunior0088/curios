//! `curios test` as the command line shows it: the subjects a selection declares tests in, each compiled as a test program and every one of its tests run in an instantiation of its own, `[argv0, index]` as the program's arguments — what is compiled, filed and run being the library's (`curios::test_subjects`). The guest prints its own outcome line and answers its verdict, which is read off what its entry hands back and never off an exit code; what only the compiler knows — the failing declaration's body as written, the count line — is printed here from the records the compile hands back.

#[cfg(test)]
mod tests;

use {
    crate::{
        Access, Heading, Line, Steps, Subject, drained, fact, note_unlinked, processing, report,
        step,
    },
    curios::{Named, TestSubject, test_subjects},
    curios_native::{OsHost, TestEnd},
    curios_package::Selection,
    curios_pipeline::{CompileError, TestRecord},
    curios_utilities::Qualifier,
    curios_verdicts::Verdicts,
    std::cmp::Reverse,
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

/// Run the tests `selection` declares, narrowed to those at or under any of the paths `filters` where any is given, each declared subject filing what it compiled into a store `access` opens. `Ok(true)` when every selected test passed.
pub(crate) fn run_tests(
    budget: u64,
    selection: Selection,
    access: Access,
    filters: &[String],
) -> Result<bool, CompileError> {
    if let Selection::Program(program) = &selection {
        note_unlinked(program, "is tested");
    }
    let under = match filters.is_empty() {
        true => vec![Qualifier::empty()],
        false => filters.iter().map(|filter| prefix(filter)).collect(),
    };
    let mut run = Run {
        budget,
        access,
        filters,
        under,
        totals: Totals::default(),
        declared: Vec::new(),
        refusal: None,
    };

    for subject in test_subjects(selection, drained)? {
        run.subject(&subject)?;
    }

    run.finish()
}

/// What a subject is reported as.
fn shown(named: &Named) -> Subject {
    match named {
        Named::Library(package) => Subject::package(package),
        Named::Executable(name) => Subject::Executable(name.clone()),
        Named::File(path) => Subject::File(path.clone()),
        Named::Stdin => Subject::Stdin,
    }
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

/// One `curios test` invocation as it goes: which of each subject's tests run, and what has been tallied.
struct Run<'a> {
    budget: u64,
    access: Access,
    /// The `--filter` arguments as written, for a refusal to name.
    filters: &'a [String],
    /// The prefixes every subject's test program schedules the tests at or under: those `filters` name, or the root where none is given.
    under: Vec<Qualifier>,
    totals: Totals,
    /// Every test the subjects compiled so far declare, selected or not: what a filter meeting none is answered with.
    declared: Vec<Qualifier>,
    /// The first reason a store refused filing, since a store nobody can write refuses every subject for one reason.
    refusal: Option<String>,
}

impl Run<'_> {
    /// One subject: its test program compiled or reused, narrated as `run` narrates a compile, and every test it schedules run, one instantiation each, between a `↳ Testing` step and a `↳ Tested` step carrying the subject's tally, which is folded into the run's. The guest prints the outcome line for what it survives; a trap or a stray exit never reaches the printing, so those lines are written here.
    fn subject(&mut self, subject: &TestSubject) -> Result<(), CompileError> {
        let shown = shown(subject.named());
        let store = subject.filed(|root| self.access.filed(root));
        processing(&shown, subject.manifest());

        // The program's own step lasts until its machine code exists: a test program is the whole program the units link into, and its back end is most of what a cold run takes.
        let mut steps = Steps::default();
        let tested = subject.compile(self.budget, store.as_ref(), &self.under, |progress| {
            report(&mut steps, &shown, progress);
        });
        match &tested {
            Ok(tested) if tested.reused() => {
                Line::nested(Heading::Compiling, &shown).outcome("reused");
                eprintln!();
            }
            Ok(_) => steps.finish(),
            Err(_) => steps.interrupted(),
        }
        let tested = tested?;
        self.refused(store.as_ref());
        self.declared.extend(tested.declared().iter().copied());

        // A subject with nothing to run reports its compile and nothing more: a tally of zeros would only restate the absence of lines above it.
        let Some(run) = tested.loaded(subject).map_err(CompileError::failure)? else {
            return Ok(());
        };

        step(Heading::Testing, &shown);
        let mut unit = Totals::default();
        for (index, record) in tested.records().iter().enumerate() {
            match run.run(index, OsHost::with_args) {
                // The guest printed `path: passed` and answered that the test held.
                Ok(TestEnd::Answered(true)) => unit.passed += 1,
                // The guest printed `path: failed` and its report, and answered that the test did not hold; the body as written is what only the records know.
                Ok(TestEnd::Answered(false)) => {
                    unit.failed += 1;
                    body(record);
                }
                // The test exited on its own, before the scheduler could report — its line is written here, whatever the code: a verdict is answered, never exited with.
                Ok(TestEnd::Exited(code)) => {
                    unit.exited += 1;
                    println!("{}: exited {code}", record.path);
                    body(record);
                }
                Err(trap) => {
                    unit.trapped += 1;
                    println!("{}: trapped", record.path);
                    for line in trap.lines() {
                        println!("  {line}");
                    }
                    body(record);
                }
            }
        }

        let mut line = Line::nested(Heading::Tested, &shown);
        line.outcome(&unit.line());
        eprintln!();
        self.totals.add(&unit);

        Ok(())
    }

    /// Keep the first reason a store refused filing.
    fn refused(&mut self, store: Option<&Verdicts>) {
        self.refusal = self
            .refusal
            .take()
            .or_else(|| store.and_then(Verdicts::refused));
    }

    /// The end of the run: a store's refusal said once, each filter that matched nothing refused by name, and the count line.
    fn finish(self) -> Result<bool, CompileError> {
        if let Some(refusal) = self.refusal {
            fact(
                Heading::Skipped,
                format!("storing what this built; {refusal}"),
            );
        }

        let unmatched = self
            .filters
            .iter()
            .filter(|filter| {
                let under = prefix(filter);
                !self.declared.iter().any(|test| test.is_within(&under))
            })
            .map(|filter| matching_nothing(filter, &self.declared))
            .collect::<Vec<_>>();
        if !unmatched.is_empty() {
            return Err(CompileError::failure(unmatched.join("\n")));
        }

        println!("{}", self.totals.line());

        Ok(self.totals.all_green())
    }
}

/// The failing declaration's body as written, indented beneath the report — absent when no span survived to slice it from.
fn body(record: &TestRecord) {
    for line in record.body.lines() {
        println!("    {line}");
    }
}
