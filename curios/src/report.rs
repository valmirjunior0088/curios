//! What the CLI says while it works.
//!
//! One fixed heading column, then a subject. A top-level line takes a target on: it is the header of the steps beneath it, or a complete fact of its own. A line marked with `↳` is a step of the header above it — one action on that target, closed by what it came to:
//!
//! ```text
//! Processing     hello
//! ↳ Compiling    /dep; reused
//! ↳ Compiling    /hello; 1.4s
//! ↳ Running      hello
//! ```
//!
//! **Everything here goes to stderr.** `curios run` hands stdout to the program it executes, so a status line written there would corrupt `curios run report.crs > report.json`.
//!
//! **A step's line is built by unterminated writes.** The prefix reaches the reader *before* the work it announces rather than after, which is the whole point — and a compiler that dies mid-operation leaves the line unterminated, so the last thing on screen is exactly how far it got. Rust's stderr is unbuffered, so this needs no flushing of its own. It also needs no cursor control, no repainting and no terminal detection: the bytes are the same in a pipe, a log file and a CI transcript as they are on a terminal.
//!
//! **A header is the one line terminated before its work is done.** Nesting costs that much of the rule above: `Processing hello` has to close so the `↳` lines can follow it, and the group ends by dedent rather than by a closing line of its own. What the rule protects survives and sharpens — the unterminated line is now the innermost one, so an interrupted compile names the step it died in rather than the target around it. A header carries no outcome of its own: what became of the target is what its steps say.

use {
    curios::STDIN_LABEL,
    curios_utilities::Qualifier,
    std::{fmt, path::PathBuf, time::Instant},
};

/// What a status line is about.
///
/// Three namespaces, and only the first is the program's — which is where a leading `/` comes from, and the only place one is ever written. A mount prefix is a name a program can spell, an executable's is an identifier its manifest chose, and a file's is whatever was typed or wherever it landed. Rendering all three through one type is what keeps a print site from spelling a slash it did not derive.
pub(crate) enum Subject {
    /// A unit, by the prefix it mounts.
    Mounted(Qualifier),
    /// A declared executable, by the name its manifest row gives it.
    Executable(String),
    /// A file, as it was written on the command line or as it landed on disk.
    File(PathBuf),
    /// The program on standard input, which was asked for as `-` and is reported as what that means: `↳ Compiling -` reads as a line the compiler failed to finish writing.
    Stdin,
}

impl Subject {
    /// The subject a package is reported as. Its declared name *is* its mount prefix, so it is reported as one — which keeps the slash derived here as it is everywhere else instead of written by the caller.
    pub(crate) fn package(name: &str) -> Self {
        Self::Mounted(Qualifier::empty().with(name))
    }
}

impl fmt::Display for Subject {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Mounted(prefix) => formatter.write_str(&prefix.join()),
            Self::Executable(name) => formatter.write_str(name),
            Self::File(path) => write!(formatter, "{}", path.display()),
            Self::Stdin => formatter.write_str(STDIN_LABEL),
        }
    }
}

/// Declare every heading once — the variant, its documentation, and the word it prints — so the enum and the list the column is measured from cannot drift apart. The list used to be kept by hand beside the enum, where the compiler could not check it and a missing entry only made the column too narrow.
macro_rules! headings {
    ($($(#[$note:meta])* $variant:ident => $word:literal,)+) => {
        /// Every word that can head a status line. The tense is the contract: a present participle opens a line before its work and may accrue what it came to, a past participle states a settled fact after it, and the imperative addresses the reader.
        #[derive(Clone, Copy, PartialEq, Eq)]
        pub(crate) enum Heading {
            $($(#[$note])* $variant,)+
        }

        impl Heading {
            /// The word itself.
            const fn text(self) -> &'static str {
                match self {
                    $(Self::$variant => $word,)+
                }
            }
        }

        /// Every heading, so the column below is measured rather than kept by hand.
        const HEADINGS: &[Heading] = &[$(Heading::$variant,)+];
    };
}

headings! {
    /// A target being taken on: the header of the steps that deal with it. Deliberately neutral about which steps those are, since what follows may be a compile or a reuse, then a run or a test.
    Processing => "Processing",
    /// A unit or a target's payload being obtained — closed by the time the compile took, or by `reused` when the store already held it.
    Compiling => "Compiling",
    /// The handover of a target's tests to their scheduler, whose report follows on stdout.
    Testing => "Testing",
    /// A target's tests finished, with their tally.
    Tested => "Tested",
    /// The handover to the program itself.
    Running => "Running",
    /// Work that could not be done and was not required — never a failure, always a reason.
    Skipped => "Skipped",
    /// The whole invocation, summarized by what it produced.
    Finished => "Finished",
    /// A dependency brought into the store.
    Fetched => "Fetched",
    /// One file or directory scaffolding produced.
    Created => "Created",
    /// The command to run next.
    Try => "Try",
}

/// The marker a nested line wears, in the columns a top-level line leaves empty in front of its heading.
const NESTED: &str = "↳ ";

/// How many columns [`NESTED`] occupies. Written out rather than measured: `str::len` counts the arrow's three UTF-8 bytes, and it prints in one.
const NESTED_WIDTH: usize = 2;

/// The widest heading in `headings`.
const fn widest(headings: &[Heading]) -> usize {
    let (mut max, mut index) = (0, 0);

    while index < headings.len() {
        let width = headings[index].text().len();

        if width > max {
            max = width;
        }

        index += 1;
    }

    max
}

/// The heading column: the widest heading, the columns a nested line's marker takes in front of it, and the three spaces every line keeps to its right.
pub(crate) const HEADING_WIDTH: usize = widest(HEADINGS) + NESTED_WIDTH + 3;

/// A line's heading column, padded out to where its subject begins. `marker` is [`NESTED`] for a step of the header above and empty for a line of its own.
///
/// Padded once here rather than at each write site, and after the marker is joined on: `{:<}` measures characters, so the arrow counts for the one column it prints in whatever its byte length.
fn head(marker: &str, heading: Heading) -> String {
    format!("{:<HEADING_WIDTH$}", format!("{marker}{}", heading.text()))
}

/// One complete fact, on a line of its own.
pub(crate) fn fact(heading: Heading, detail: impl fmt::Display) {
    eprintln!("{}{detail}", head("", heading));
}

/// One complete step of the header above, on a line of its own — `↳ Running hello`, whose consequence is what follows on stdout rather than an outcome of its own.
pub(crate) fn step(heading: Heading, subject: &Subject) {
    eprintln!("{}{subject}", head(NESTED, heading));
}

/// A subject being worked on, whose outcome arrives when the work is done.
///
/// **The caller terminates the line**, with an `eprintln!()` once the subject is finished with — there is no method for it, because closing a line is a bare newline and a method taking `self` to write one carries no data worth the ceremony. A line never terminated stays unterminated, which is the intended reading of an interrupted compilation rather than an omission to repair.
pub(crate) struct Line {
    /// When the line opened, for the [`Line::done`] that closes it with what the work took — gone once an outcome has said the work did not happen.
    started: Option<Instant>,
}

impl Line {
    /// Open a line for `subject` — `Finished hello` — leaving it unterminated.
    pub(crate) fn open(heading: Heading, subject: &Subject) -> Self {
        Self::marked("", heading, subject)
    }

    /// Open a line for a step of the header above it — `↳ Compiling /hello`.
    pub(crate) fn nested(heading: Heading, subject: &Subject) -> Self {
        Self::marked(NESTED, heading, subject)
    }

    fn marked(marker: &str, heading: Heading, subject: &Subject) -> Self {
        eprint!("{}{subject}", head(marker, heading));

        Self {
            started: Some(Instant::now()),
        }
    }

    /// Close the work the line announced, with what it took — `; 1.4s`.
    pub(crate) fn done(&mut self) {
        let elapsed = self
            .started
            .take()
            .map_or(0.0, |at| at.elapsed().as_secs_f64());

        eprint!("; {elapsed:.1}s");
    }

    /// Report what happened instead of the work — `; reused` — or what it came to — `; 3 passed, 1 failed`.
    ///
    /// The timing the line opened with is abandoned: an outcome is what happened *instead* of the work, so there is nothing left to time.
    pub(crate) fn outcome(&mut self, outcome: &str) {
        self.started = None;
        eprint!("; {outcome}");
    }
}
