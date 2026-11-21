//! What the transcript is told: an event a line's or a block's worth, each naming the tree it is of and carrying its words as text, so it holds nothing of the compiler's and is drawn on whichever thread tells it.

use {
    crate::{Piece, Wrote},
    std::{
        sync::atomic::{AtomicU64, Ordering},
        time::Duration,
    },
};

/// Which tree an event is of. A subject's name is no key: a library is checked, then built into the program its tests run in, two trees under one name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Opened(u64);

impl Opened {
    /// A tree no other in this process is.
    pub fn minted() -> Self {
        static NEXT: AtomicU64 = AtomicU64::new(0);

        Self(NEXT.fetch_add(1, Ordering::Relaxed))
    }
}

/// A step done for a subject, which each drawing spells its own way: `check` in the tree, `Checking` and `Checked` plain.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Step {
    /// A unit or a program checked, `tested` where it is checked with the tests it declares.
    Check { tested: bool },
    /// A unit handed to the cabinet.
    Store,
    /// A program lowered and optimized.
    Build,
    /// A program written as WebAssembly, optimized by Binaryen and, natively, compiled to machine code.
    Emit,
    /// What a command made, written where it stands.
    Write,
    /// What a command made, served at an address.
    Serve,
    /// A tree a manifest names, fetched.
    Fetch,
    /// A module a row names, delivered.
    Deliver,
    /// A manifest's row, pinned.
    Pin,
    /// A file, formatted.
    Format,
    /// The command to run next.
    Try,
}

impl Step {
    /// The node's label in the tree: `check with its tests`.
    pub fn label(self) -> &'static str {
        match self {
            Step::Check { tested: false } => "check",
            Step::Check { tested: true } => "check with its tests",
            Step::Store => "store",
            Step::Build => "build",
            Step::Emit => "emit",
            Step::Write => "write",
            Step::Serve => "serve",
            Step::Fetch => "fetch",
            Step::Deliver => "deliver",
            Step::Pin => "pin",
            Step::Format => "format",
            Step::Try => "try",
        }
    }

    /// The verb a plain line opens on where the step begins: `Checking`.
    pub fn doing(self) -> &'static str {
        match self {
            Step::Check { .. } => "Checking",
            Step::Store => "Storing",
            Step::Build => "Building",
            Step::Emit => "Emitting",
            Step::Write => "Writing",
            Step::Serve => "Serving",
            Step::Fetch => "Fetching",
            Step::Deliver => "Delivering",
            Step::Pin => "Pinning",
            Step::Format => "Formatting",
            Step::Try => "Try",
        }
    }

    /// The verb a plain line opens on where the step ends, or where it is said at once: `Checked`.
    pub fn done(self) -> &'static str {
        match self {
            Step::Check { .. } => "Checked",
            Step::Store => "Stored",
            Step::Build => "Built",
            Step::Emit => "Emitted",
            Step::Write => "Wrote",
            Step::Serve => "Serving",
            Step::Fetch => "Fetched",
            Step::Deliver => "Delivered",
            Step::Pin => "Pinned",
            Step::Format => "Formatted",
            Step::Try => "Try",
        }
    }

    /// What a plain line says after the subject: ` with its tests`.
    pub fn rest(self) -> &'static str {
        match self {
            Step::Check { tested: true } => " with its tests",
            _ => "",
        }
    }
}

/// What a node is: a step done for its subject, or a test run, by its path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    Step(Step),
    Test(String),
}

/// How a node closed.
#[derive(Debug, Clone)]
pub enum Closing {
    /// Its work finished, taking this long.
    Done(Duration),
    /// Its work ended as the word says, where it did not simply finish — `refused`, `failed`, `1 goal` —, taking this long.
    Ended(String, Duration),
    /// Its work came to stand as this, the one fact it is read for: a place, a count, `unchanged`.
    Stands(Piece),
}

/// One thing the transcript is told.
#[derive(Debug, Clone)]
pub enum Event {
    /// A tree for `subject`, beside the manifest that governs it as the reader would spell it where the invocation stands elsewhere: its line is written with its first node, or as it is left where it names the manifest.
    Named {
        tree: Opened,
        subject: String,
        manifest: Option<String>,
    },
    /// A node of `tree` begins, `last` where its tree plans no node after it.
    Began {
        tree: Opened,
        node: Node,
        last: bool,
    },
    /// The node open on `tree` closes, with the lines it came to beneath it, one block with its closing.
    Closed {
        tree: Opened,
        closing: Closing,
        beneath: Vec<Piece>,
    },
    /// A step of `tree` said at once, its outcome known as it begins, with the lines it came to beneath it: what a scaffold wrote, the address a page is served at.
    Stood {
        tree: Opened,
        step: Step,
        last: bool,
        stands: Piece,
        beneath: Vec<Piece>,
    },
    /// The test whose node is open on `tree` ended as `ending` after `taken`, said where its name is written at `place` where it did not pass, its own writes having left the line as `wrote` says, with what only its runner holds beneath.
    Tested {
        tree: Opened,
        ending: String,
        taken: Duration,
        place: Option<String>,
        wrote: Wrote,
        beneath: Vec<String>,
    },
    /// `tree` closes on the node that sums it up, with the time `taken` by all it sums where any of it ran.
    Summed {
        tree: Opened,
        sum: Piece,
        taken: Option<Duration>,
    },
    /// `tree` is left as it stands.
    Left { tree: Opened },
    /// A step taken for no one subject: `serve; http://…`.
    Alone { step: Step, outcome: Piece },
    /// What the whole invocation came to, its last line.
    Total {
        text: Piece,
        taken: Option<Duration>,
    },
    /// A blank line, between a tree and the report of what stopped it.
    Apart,
    /// A block written whole as its writer rendered it, under no subject.
    Said { text: Piece },
}
