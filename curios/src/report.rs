//! What the CLI says while it works.
//!
//! One fixed heading column, then either a complete fact or a subject that accrues operations as they happen:
//!
//! ```text
//! Processing   /hello; compiling... done 1.4s
//! Written      .curios/bin/hello/hello
//! ```
//!
//! **Everything here goes to stderr.** `curios run` hands stdout to the program it executes, so a status line written there would corrupt `curios run report.crs > report.json`.
//!
//! **A subject's line is built by unterminated writes.** The prefix reaches the reader *before* the work it announces rather than after, which is the whole point — and a compiler that dies mid-operation leaves the line unterminated, so the last thing on screen is exactly how far it got. Rust's stderr is unbuffered, so this needs no flushing of its own. It also needs no cursor control, no repainting and no terminal detection: the bytes are the same in a pipe, a log file and a CI transcript as they are on a terminal.

use std::time::Instant;

/// Every word that can head a status line.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Heading {
    /// A unit being folded into the compilation.
    Processing,
    /// A dependency being brought into the store.
    Fetching,
    /// The handover to the program itself.
    Running,
    /// An artifact that landed on disk.
    Written,
    /// The whole invocation, summarized.
    Finished,
    /// One file or directory scaffolding produced.
    Created,
    /// The command to run next.
    Try,
}

impl Heading {
    /// The word itself. An exhaustive `match` rather than an index into [`HEADINGS`]: a new variant does not compile until it is spelled here, where an index would compile and then panic.
    pub(crate) const fn text(self) -> &'static str {
        match self {
            Self::Processing => "Processing",
            Self::Fetching => "Fetching",
            Self::Running => "Running",
            Self::Written => "Written",
            Self::Finished => "Finished",
            Self::Created => "Created",
            Self::Try => "Try",
        }
    }
}

/// Every heading, so the column below is measured rather than kept by hand.
///
/// This is the one hand-kept list, and the compiler cannot check it: a variant missing here only makes the column too narrow, which the first line of output shows.
const HEADINGS: [Heading; 7] = [
    Heading::Processing,
    Heading::Fetching,
    Heading::Running,
    Heading::Written,
    Heading::Finished,
    Heading::Created,
    Heading::Try,
];

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

/// The heading column: the widest heading, plus the three spaces every line keeps to its right.
pub(crate) const HEADING_WIDTH: usize = widest(&HEADINGS) + 3;

/// One complete fact, on a line of its own.
pub(crate) fn fact(heading: Heading, detail: &str) {
    eprintln!("{:<HEADING_WIDTH$}{detail}", heading.text());
}

/// A subject being worked on, whose operations arrive one at a time.
///
/// **The caller terminates the line**, with an `eprintln!()` once the subject is finished with — there is no method for it, because closing a line is a bare newline and a method taking `self` to write one carries no data worth the ceremony. A line never terminated stays unterminated, which is the intended reading of an interrupted compilation rather than an omission to repair.
pub(crate) struct Line {
    /// When the operation currently announced began, for the `done` that closes it.
    started: Option<Instant>,
}

impl Line {
    /// Open a line for `subject` — `Processing   /hello` — leaving it unterminated.
    pub(crate) fn open(heading: Heading, subject: &str) -> Self {
        eprint!("{:<HEADING_WIDTH$}{subject}", heading.text());

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
