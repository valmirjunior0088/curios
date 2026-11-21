//! The plain drawing: a line an event, each standing alone — a verb, the subject's name in backticks, then what the event carries.
//!
//! ```text
//! Checking `/app` with its tests
//! Checked `/app` with its tests in 0.1s
//! Testing `/app/draws_a_square`
//! `/app/draws_a_square`> ■■■■
//! Tested `/app/draws_a_square`: passed in 0.0s
//! ```
//!
//! **A step's two lines share its verb**, its present participle where it begins and its past where it ends, and how it ended stands after the subject where it did not simply finish. A step said at once, its outcome known as it begins, is its closing line alone.
//!
//! **Nothing depends on the line before it.** The lines of different subjects interleave in the order their events happen, and a block — a report, what a failed test found, the lines a node came to — is written whole, so no line of another subject splits it. Nothing is moved, erased or redrawn, and nothing of the terminal is read.

use {
    super::{Closing, Event, Node, Opened, Owed, bold, dim, seconds, worded},
    crate::{Piece, Quoted, Wrote},
    curios_common::Style,
    std::collections::BTreeMap,
};

/// What the drawing holds of one tree between its events.
#[derive(Default)]
struct Named {
    subject: String,
    /// The manifest beside the subject, until a line has named it.
    manifest: Option<String>,
    /// The node open on the tree, by which its closing and what its work writes are drawn.
    open: Option<Node>,
}

impl Named {
    /// The subject in backticks, the manifest beside it where no line has named it yet: `` `hello` (../curios.toml) ``.
    fn subject(&mut self) -> Piece {
        let subject = quoted(&self.subject);

        match self.manifest.take() {
            Some(manifest) => subject.then(" ").then(dim(format_args!("({manifest})"))),
            None => subject,
        }
    }

    /// The line a closing of the open node opens on: a step's past verb and the subject, or a test's path.
    fn closing(&mut self) -> Piece {
        match self.open.take() {
            Some(Node::Step(step)) => Piece::from(step.done())
                .then(" ")
                .then(self.subject())
                .then(step.rest()),
            Some(Node::Test(path)) => Piece::from("Tested ").then(quoted(&path)),
            None => self.subject(),
        }
    }
}

/// A name as a plain line spells it: in backticks, bold where the reader sees colour, as a subject is.
fn quoted(name: &str) -> Piece {
    bold(format_args!("`{name}`"))
}

/// The plain drawing, with what it holds of each tree it was told of.
#[derive(Default)]
pub struct Lines {
    named: BTreeMap<Opened, Named>,
}

impl Lines {
    /// A drawing told of no tree yet.
    pub const fn new() -> Self {
        Self {
            named: BTreeMap::new(),
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
                self.named.insert(
                    tree,
                    Named {
                        subject,
                        manifest,
                        open: None,
                    },
                );

                Piece::default()
            }
            Event::Began { tree, node, .. } => {
                let named = self.named.entry(tree).or_default();
                let began = match &node {
                    Node::Step(step) => Piece::from(step.doing())
                        .then(" ")
                        .then(named.subject())
                        .then(step.rest()),
                    Node::Test(path) => Piece::from("Testing ").then(quoted(path)),
                };
                named.open = Some(node);

                began.then("\n")
            }
            Event::Closed {
                tree,
                closing,
                beneath,
            } => {
                let line = self.named.entry(tree).or_default().closing();

                indented(closed(line, closing).then("\n"), beneath)
            }
            Event::Stood {
                tree,
                step,
                stands,
                beneath,
                ..
            } => {
                let line = Piece::from(step.done())
                    .then(" ")
                    .then(self.named.entry(tree).or_default().subject())
                    .then(step.rest());

                indented(closed(line, Closing::Stands(stands)).then("\n"), beneath)
            }
            Event::Tested {
                tree,
                ending,
                taken,
                place,
                wrote,
                beneath,
            } => {
                // A line the test left open is ended first, so its ending stands on a line of its own.
                let open = match wrote {
                    Wrote::Open => Piece::from("\n"),
                    Wrote::Nothing | Wrote::Lines => Piece::default(),
                };
                let line = self.named.entry(tree).or_default().closing();
                let ended = open.then(closed(line, Closing::Ended(ending, taken)));
                // Where its name is written closes the line, after `;` as in the tree.
                let ended = match place {
                    Some(place) => ended.then("; ").then(place),
                    None => ended,
                };

                indented(
                    ended.then("\n"),
                    beneath.into_iter().map(Piece::from).collect(),
                )
            }
            Event::Summed { tree, sum, taken } => {
                let mut named = self.named.remove(&tree).unwrap_or_default();
                let summed = Piece::from("Tested ")
                    .then(named.subject())
                    .then(": ")
                    .then(sum);

                match taken {
                    Some(taken) => summed
                        .then(" ")
                        .then(dim(format_args!("in {}", seconds(taken))))
                        .then("\n"),
                    None => summed.then("\n"),
                }
            }
            // A subject nothing was done for is said only where its line names the manifest that governs it, which nothing else would tell.
            Event::Left { tree } => {
                let mut named = self.named.remove(&tree).unwrap_or_default();

                match named.manifest.is_some() {
                    true => named.subject().then("\n"),
                    false => Piece::default(),
                }
            }
            Event::Alone { step, outcome } => {
                Piece::from(step.done()).then(" ").then(outcome).then("\n")
            }
            Event::Total {
                text,
                taken: Some(taken),
            } => text.then("; ").then(dim(seconds(taken))).then("\n"),
            Event::Total { text, taken: None } => text.then("\n"),
            Event::Apart => Piece::from("\n"),
            Event::Said { text } => text.then("\n"),
        }
    }

    /// What a line the work of `tree`'s open node wrote is `owed`, as a reader of `style` sees it: the test's path in backticks and the quote mark, painted as the tree paints it.
    pub fn frame(&self, tree: Opened, owed: Owed, quoted: Quoted, style: &Style) -> String {
        let named = self.named.get(&tree);
        let path = match named.and_then(|named| named.open.as_ref()) {
            Some(Node::Test(path)) => path.as_str(),
            _ => named.map_or("", |named| named.subject.as_str()),
        };
        let mark = match quoted {
            Quoted::Output => style.dim(">"),
            Quoted::Error => style.aside(">"),
        };
        let frame = format!("{}{mark}", style.bold(format!("`{path}`")));

        match owed {
            Owed::Nothing => String::new(),
            Owed::Bare => frame,
            Owed::Frame => format!("{frame} "),
        }
    }
}

/// `line` closed as `closing` says: with its time alone where its work finished, how it ended where it did not, what it came to stand as where that is what it is read for.
fn closed(line: Piece, closing: Closing) -> Piece {
    match closing {
        Closing::Done(taken) => line
            .then(" ")
            .then(dim(format_args!("in {}", seconds(taken)))),
        Closing::Ended(word, taken) => line
            .then(": ")
            .then(worded(&word))
            .then(" ")
            .then(dim(format_args!("in {}", seconds(taken)))),
        Closing::Stands(stands) => line.then(": ").then(stands),
    }
}

/// `line`, and each of `beneath` two spaces in under it: one block.
fn indented(line: Piece, beneath: Vec<Piece>) -> Piece {
    beneath
        .into_iter()
        .fold(line, |block, under| block.then("  ").then(under).then("\n"))
}
