//! What the CLI says while it works.
//!
//! One fixed heading column, then either a complete fact or a subject that accrues operations as they happen. A line marked with `↳` is a step of the line above it:
//!
//! ```text
//! Building       hello
//! ↳ Processing   /hello; compiling... done 1.4s
//! Finished       .curios/bin/hello/hello; done 1.6s
//! ```
//!
//! **Everything here goes to stderr.** `curios run` hands stdout to the program it executes, so a status line written there would corrupt `curios run report.crs > report.json`.
//!
//! **A subject's line is built by unterminated writes.** The prefix reaches the reader *before* the work it announces rather than after, which is the whole point — and a compiler that dies mid-operation leaves the line unterminated, so the last thing on screen is exactly how far it got. Rust's stderr is unbuffered, so this needs no flushing of its own. It also needs no cursor control, no repainting and no terminal detection: the bytes are the same in a pipe, a log file and a CI transcript as they are on a terminal.
//!
//! **A group's header is the one line terminated before its work is done.** Nesting costs that much of the rule above: `Building hello` has to close so the `↳` lines can follow it, and the group ends by dedent rather than by a closing line of its own. What the rule protects survives and sharpens — the unterminated line is now the innermost one, so an interrupted compile names the step it died in rather than the group around it.

use {
    curios_base::Qualifier,
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
        }
    }
}

/// Declare every heading once — the variant, its documentation, and the word it prints — so the enum and the list the column is measured from cannot drift apart. The list used to be kept by hand beside the enum, where the compiler could not check it and a missing entry only made the column too narrow.
macro_rules! headings {
    ($($(#[$note:meta])* $variant:ident => $word:literal,)+) => {
        /// Every word that can head a status line.
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
    /// The target being produced, and the header of the steps that produce it.
    Building => "Building",
    /// A unit being folded into the compilation.
    Processing => "Processing",
    /// A dependency being brought into the store.
    Fetching => "Fetching",
    /// The handover to the program itself.
    Running => "Running",
    /// Work that could not be done and was not required — never a failure, always a reason.
    Skipped => "Skipped",
    /// The whole invocation, summarized by what it produced.
    Finished => "Finished",
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

/// A line's heading column, padded out to where its subject begins. `marker` is [`NESTED`] for a step of the group above and empty for a line of its own.
///
/// Padded once here rather than at each write site, and after the marker is joined on: `{:<}` measures characters, so the arrow counts for the one column it prints in whatever its byte length.
fn head(marker: &str, heading: Heading) -> String {
    format!("{:<HEADING_WIDTH$}", format!("{marker}{}", heading.text()))
}

/// One complete fact, on a line of its own.
pub(crate) fn fact(heading: Heading, detail: impl fmt::Display) {
    eprintln!("{}{detail}", head("", heading));
}

/// A subject being worked on, whose operations arrive one at a time.
///
/// **The caller terminates the line**, with an `eprintln!()` once the subject is finished with — there is no method for it, because closing a line is a bare newline and a method taking `self` to write one carries no data worth the ceremony. A line never terminated stays unterminated, which is the intended reading of an interrupted compilation rather than an omission to repair.
pub(crate) struct Line {
    /// When the operation currently announced began, for the `done` that closes it.
    started: Option<Instant>,
}

impl Line {
    /// Open a line for `subject` — `Building   hello` — leaving it unterminated.
    pub(crate) fn open(heading: Heading, subject: &Subject) -> Self {
        Self::marked("", heading, subject)
    }

    /// Open a line for a step of the group above it — `↳ Processing   /hello`.
    pub(crate) fn nested(heading: Heading, subject: &Subject) -> Self {
        Self::marked(NESTED, heading, subject)
    }

    fn marked(marker: &str, heading: Heading, subject: &Subject) -> Self {
        eprint!("{}{subject}", head(marker, heading));

        Self { started: None }
    }

    /// Announce an operation — `; compiling... ` — and start timing it.
    pub(crate) fn step(&mut self, operation: &str) {
        eprint!("; {operation}... ");
        self.started = Some(Instant::now());
    }

    /// Close the operation [`Line::step`] announced, with what it took.
    pub(crate) fn done(&mut self) {
        let elapsed = self
            .started
            .take()
            .map_or(0.0, |at| at.elapsed().as_secs_f64());

        eprint!("done {elapsed:.1}s");
    }

    /// Report an outcome that took no work worth timing — `; reused`.
    ///
    /// Any operation announced and not yet closed is abandoned with it: an outcome is what happened *instead* of the work, so there is nothing left to time.
    pub(crate) fn outcome(&mut self, outcome: &str) {
        self.started = None;
        eprint!("; {outcome}");
    }
}
