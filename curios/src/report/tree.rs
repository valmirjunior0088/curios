//! The tree drawing: each subject a tree, its line alone and a node a line beneath it, `label; outcome`.
//!
//! ```text
//! /geometry
//! ├─ check; 1.6s
//! └─ store; 0.1s
//! ```
//!
//! **The subject's line is written with its first node**, so a subject nothing had to be done for says nothing: what the cabinet already held is not narrated.
//!
//! **A node's line is built by unterminated writes.** The label reaches the reader before the work it names, and what closes the line is what the work came to. A compiler that dies mid-step leaves the line open, so the last thing written is how far it got.
//!
//! **The connector is the plan's.** `├─` is written where more nodes are planned and `└─` at the last, before either's work is done, so a tree a refusal cuts short ends on `├─`: the steps it planned and never reached are the ones missing.
//!
//! **What a node's work writes of its own is never held back.** Its first byte ends the node's line, each line of it stands quoted as it is written ([`framed_run`]), and the outcome closes the node beneath it, its mark in the quote's column. A test's ending leads with its mark wherever it stands, `✓` where it passed and `✗` where it did not:
//!
//! ```text
//! ├─ /geometry/is_square; ✓ passed in 0.0s
//! ├─ /geometry/draws_a_square
//! │  > ■■■■
//! │  ✓ passed in 0.1s
//! ```

use {
    super::{Closing, Event, Opened, bold, dim, seconds, trouble, troubled, worded},
    crate::Piece,
    curios_common::Style,
    std::collections::BTreeMap,
};

/// The connector of a node more are planned after.
const BRANCH: &str = "├─";

/// The connector of the last node a tree plans.
const CORNER: &str = "└─";

/// The trunk, beside what stands beneath a node more are planned after.
const TRUNK: &str = "│";

/// The mark a test's own lines stand behind, in the column its ending closes in: what is written there is quoted from the test, not said by the runner.
const QUOTE: &str = ">";

/// The mark of a test that passed, where its ending is said.
const PASSED: &str = "✓";

/// The mark of a test that did not pass — it failed, trapped or exited — where its ending is said.
const UNPASSED: &str = "✗";

/// The standard stream a test wrote a line of its own on, which its quote mark is painted for: one on standard error stands apart from one on standard output.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Quoted {
    Output,
    Error,
}

/// Where a test's writes stand in the line they are on, which says whether its next byte opens a line and so stands behind the frame.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Column {
    /// At a line's start.
    #[default]
    Start,
    /// Past a line's start.
    Within,
    /// Past a carriage return, which left the line standing: the next byte writes over it from its start, unless it ends it.
    Returned,
}

impl Column {
    /// The column a stream is left in by `written`, the bytes last written from this one.
    pub fn after(self, written: &[u8]) -> Self {
        match written.last() {
            None => self,
            Some(b'\n') => Column::Start,
            Some(b'\r') => Column::Returned,
            Some(_) => Column::Within,
        }
    }
}

/// What a line of a test's own is owed before the run that opens it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Owed {
    /// Nothing: the line is under way.
    Nothing,
    /// The frame, and a blank before the run.
    Frame,
    /// The frame alone, before a run that is only its line's end.
    Bare,
}

/// The first run of `bytes`, which a test writes from `column`: what the line it goes on is owed before it, and how many of the bytes go on that line, through the one that ends or leaves it.
pub fn framed_run(column: Column, bytes: &[u8]) -> (Owed, usize) {
    let length = bytes
        .iter()
        .position(|byte| matches!(byte, b'\n' | b'\r'))
        .map_or(bytes.len(), |end| end + 1);
    let owed = match (column, bytes.first()) {
        // A line's end after a carriage return is the one line's end, written in two bytes.
        (_, None) | (Column::Within, _) | (Column::Returned, Some(b'\n')) => Owed::Nothing,
        (Column::Start | Column::Returned, Some(b'\n' | b'\r')) => Owed::Bare,
        (Column::Start | Column::Returned, Some(_)) => Owed::Frame,
    };

    (owed, length)
}

/// What a line the test wrote on `quoted` is `owed`, as a reader of `style` sees it: the tree's trunk and the quote mark, which stands in the column the test's ending closes in.
pub fn frame_of(owed: Owed, quoted: Quoted, style: &Style) -> String {
    let mark = match quoted {
        Quoted::Output => style.dim(QUOTE),
        Quoted::Error => style.aside(QUOTE),
    };
    let frame = format!("{}  {mark}", style.dim(TRUNK));

    match owed {
        Owed::Nothing => String::new(),
        Owed::Bare => frame,
        Owed::Frame => format!("{frame} "),
    }
}

/// What a node's work wrote of its own on the standard streams while the node's line stood open, as whoever watched them saw it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Wrote {
    /// Nothing: the node's line is open still, and its outcome closes it.
    Nothing,
    /// Something, the last of it a line's end.
    Lines,
    /// Something that left a line open.
    Open,
}

impl Wrote {
    /// What a node's work wrote, from whether it `wrote` anything and the `column` its writes left the streams in.
    pub fn of(wrote: bool, column: Column) -> Self {
        match (wrote, column) {
            (false, _) => Wrote::Nothing,
            (true, Column::Start) => Wrote::Lines,
            (true, Column::Within | Column::Returned) => Wrote::Open,
        }
    }
}

/// What the drawing holds of one tree between its events.
#[derive(Default)]
struct Drawn {
    /// The subject's line, until the first node writes it.
    line: Option<Piece>,
    /// Whether the line names the manifest that governs the subject, which is said however the tree ends.
    governed: bool,
    /// Whether the node last opened is the last its tree plans, which is whether the trunk goes on beside what stands beneath it.
    last: bool,
}

impl Drawn {
    /// The subject's line where nothing has written it yet, and nothing where something has.
    fn headed(&mut self) -> Piece {
        self.line
            .take()
            .map_or_else(Piece::default, |line| line.then("\n"))
    }
}

/// Today's drawing of the transcript, with what it holds of each tree it was told of.
#[derive(Default)]
pub struct Trees {
    drawn: BTreeMap<Opened, Drawn>,
}

impl Trees {
    /// A drawing told of no tree yet: what the transcript starts from, made where a static is.
    pub const fn new() -> Self {
        Self {
            drawn: BTreeMap::new(),
        }
    }

    /// What `event` is drawn as on standard error.
    pub fn draw(&mut self, event: Event) -> Piece {
        match event {
            Event::Named {
                tree,
                subject,
                manifest,
            } => {
                let line = match &manifest {
                    Some(manifest) => bold(subject)
                        .then(" ")
                        .then(dim(format_args!("({manifest})"))),
                    None => bold(subject),
                };
                self.drawn.insert(
                    tree,
                    Drawn {
                        line: Some(line),
                        governed: manifest.is_some(),
                        last: false,
                    },
                );

                Piece::default()
            }
            Event::Began { tree, label, last } => {
                let drawn = self.drawn.entry(tree).or_default();
                drawn.last = last;
                let connector = match last {
                    true => CORNER,
                    false => BRANCH,
                };

                drawn
                    .headed()
                    .then(dim(connector))
                    .then(format_args!(" {label}"))
            }
            Event::Closed {
                tree,
                closing,
                beneath,
            } => {
                let last = self.drawn.get(&tree).is_some_and(|drawn| drawn.last);
                let closed = match closing {
                    Closing::Done(taken) => Piece::from("; ").then(dim(seconds(taken))),
                    Closing::Ended(word, taken) => Piece::from("; ")
                        .then(worded(&word))
                        .then(" ")
                        .then(dim(format_args!("in {}", seconds(taken)))),
                    Closing::Stands(stands) => Piece::from("; ").then(stands),
                };

                beneath
                    .into_iter()
                    .fold(closed.then("\n"), |block, line| match last {
                        true => block.then("     ").then(line).then("\n"),
                        false => block.then(dim(TRUNK)).then("    ").then(line).then("\n"),
                    })
            }
            Event::Tested {
                ending,
                taken,
                place,
                wrote,
                beneath,
            } => {
                let mark = match trouble(&ending) {
                    true => troubled(UNPASSED),
                    false => PASSED.into(),
                };
                let closing = mark
                    .then(" ")
                    .then(worded(&ending))
                    .then(" ")
                    .then(dim(format_args!("in {}", seconds(taken))));
                let closing = match place {
                    Some(place) => closing.then("; ").then(place),
                    None => closing,
                };
                let closed = match wrote {
                    Wrote::Nothing => Piece::from("; ").then(closing).then("\n"),
                    Wrote::Lines => dim(TRUNK).then("  ").then(closing).then("\n"),
                    Wrote::Open => Piece::from("\n")
                        .then(dim(TRUNK))
                        .then("  ")
                        .then(closing)
                        .then("\n"),
                };

                beneath.into_iter().fold(closed, |block, line| {
                    block.then(dim(TRUNK)).then(format_args!("    {line}\n"))
                })
            }
            Event::Summed { tree, sum, taken } => {
                let mut drawn = self.drawn.remove(&tree).unwrap_or_default();
                let summed = drawn.headed().then(dim(CORNER)).then(" ").then(sum);

                match taken {
                    Some(taken) => summed.then("; ").then(dim(seconds(taken))).then("\n"),
                    None => summed.then("\n"),
                }
            }
            Event::Left { tree } => {
                let mut drawn = self.drawn.remove(&tree).unwrap_or_default();

                match drawn.governed {
                    true => drawn.headed(),
                    false => Piece::default(),
                }
            }
            Event::Alone { label, outcome } => Piece::from(format_args!("{label}; "))
                .then(outcome)
                .then("\n"),
            Event::Total {
                text,
                taken: Some(taken),
            } => text.then("; ").then(dim(seconds(taken))).then("\n"),
            Event::Total { text, taken: None } => text.then("\n"),
            Event::Apart => Piece::from("\n"),
            Event::Said { text } => text.then("\n"),
        }
    }
}
