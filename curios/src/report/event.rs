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
        label: String,
        last: bool,
    },
    /// The node open on `tree` closes, with the lines it came to beneath it, one block with its closing.
    Closed {
        tree: Opened,
        closing: Closing,
        beneath: Vec<Piece>,
    },
    /// The test whose node is open ended as `ending` after `taken`, said where its name is written at `place` where it did not pass, its own writes having left the line as `wrote` says, with what only its runner holds beneath.
    Tested {
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
    Alone { label: String, outcome: Piece },
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
