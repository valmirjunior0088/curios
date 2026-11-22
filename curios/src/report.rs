//! What the CLI says while it works.
//!
//! One transcript, told as events ([`Event`]) and drawn where it is told ([`tell`]), live on a terminal ([`Live`]) — each subject a tree, and beneath it a node for each thing done for it, written `label; outcome` ([`Trees`]), what is still running redrawn beneath — and as plain lines everywhere else ([`Lines`]):
//!
//! ```text
//! /geometry
//! ├─ check; 1.6s
//! └─ store; 0.1s
//!
//! hello
//! ├─ check; 0.1s
//! ├─ build; 0.2s
//! └─ emit; 0.4s
//! ```
//!
//! **Everything here goes to stderr.** `curios run` hands stdout to the program it executes, so a line of the transcript written there would corrupt `curios run report.crs > report.json`.
//!
//! **An event is data until it is drawn**, as a report is: what happened to which tree, its words as text, so whoever tells it holds nothing of the drawing and the drawing nothing of the compiler. Nothing written for good is taken back: the plain bytes are the same in a pipe, a log file and a CI transcript as they are on a terminal, colour apart, and the live drawing redraws only the region beneath what it has written.
//!
//! **Colour is a layer, decided once** ([`paint`]): a subject bold, the tree and the times dim, and what went wrong red. It changes no byte beneath it, and a line is made of pieces built in both forms ([`Piece`]), as whoever reads standard error sees one and plain, which is what is kept of the transcript for a page ([`keep`]).
//!
//! **What is said of no one subject stands under none**: a step taken for every subject ([`alone`]), what the whole invocation came to, as its last line ([`total`]), and a block written whole ([`said`]), such as the report of what stopped a command.

#[cfg(test)]
mod tests;

mod event;
use event::*;
pub use event::{Opened, Step};

mod live;
use live::*;

mod plain;
use plain::*;

mod transcript;
use transcript::*;
pub use transcript::{
    Drawing, PROGRESS, Relayed, finish, keep, kept, relay, settle, transcribe, written_past,
};

mod tree;
use tree::*;
pub use tree::{Quoted, Wrote};

use {
    curios_common::{Qualifier, STDIN_NAME, Style},
    curios_package::Named,
    std::{
        env, fmt,
        path::{Path, PathBuf},
        sync::OnceLock,
        time::{Duration, Instant},
    },
};

/// The reader no style was decided for: standing nowhere, seeing no colour.
static PLAIN: Style = Style {
    here: None,
    painted: false,
};

/// Whoever reads standard error, decided once as the command starts.
static READER: OnceLock<Style> = OnceLock::new();

/// Whoever reads standard output, decided beside [`READER`].
static ANSWERED: OnceLock<Style> = OnceLock::new();

/// Write for `reader` on standard error and for `answered` on standard output from here on: where each stands, which a path is spelled from, and whether each sees colour. Decided once, as the command starts, from what was asked and where each stream goes.
pub fn paint(reader: Style, answered: Style) {
    let _ = READER.set(reader);
    let _ = ANSWERED.set(answered);
}

/// Whoever reads standard error: what the transcript, and a report beside it, is written for.
pub fn reader() -> &'static Style {
    READER.get().unwrap_or(&PLAIN)
}

/// Whoever reads standard output: what a line a test writes there is framed for.
pub fn answered() -> &'static Style {
    ANSWERED.get().unwrap_or(&PLAIN)
}

/// A stretch of a transcript's line in both its forms: as whoever reads standard error sees it, and plain, as it is kept for a page.
///
/// Built in both where it is made, and never read back out of what was painted: a line may carry an escape sequence of its own, in a path or a test's name, which a reading of the painted form would take for paint.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Piece {
    shown: String,
    plain: String,
}

impl Piece {
    /// `text` as `paint` paints it for `style`'s reader, beside `text` itself.
    fn painted(style: &Style, text: impl fmt::Display, paint: fn(&Style, &str) -> String) -> Self {
        let plain = text.to_string();

        Self {
            shown: paint(style, &plain),
            plain,
        }
    }

    /// What `render` makes of something for whoever reads standard error, beside what it makes of it for a reader who sees no colour: a report, rendered for each.
    pub fn rendered(render: impl Fn(&Style) -> String) -> Self {
        Self {
            shown: render(reader()),
            plain: render(&PLAIN),
        }
    }

    /// This piece and `next` after it.
    pub fn then(mut self, next: impl Into<Piece>) -> Self {
        let next = next.into();
        self.shown.push_str(&next.shown);
        self.plain.push_str(&next.plain);

        self
    }

    /// `pieces` in order, with `between` between each two.
    pub fn joined(pieces: impl IntoIterator<Item = Piece>, between: &str) -> Self {
        pieces.into_iter().enumerate().fold(
            Piece::default(),
            |joined, (index, piece)| match index {
                0 => joined.then(piece),
                _ => joined.then(between).then(piece),
            },
        )
    }
}

/// Text no one paints is the same in both forms.
impl<T: fmt::Display> From<T> for Piece {
    fn from(text: T) -> Self {
        let text = text.to_string();

        Self {
            shown: text.clone(),
            plain: text,
        }
    }
}

/// `text` as a subject is painted.
fn bold(text: impl fmt::Display) -> Piece {
    Piece::painted(reader(), text, |style, text| style.bold(text))
}

/// `text` as the tree and a time are painted.
fn dim(text: impl fmt::Display) -> Piece {
    Piece::painted(reader(), text, |style, text| style.dim(text))
}

/// `text` as what went wrong is painted: a refusal, a failure, a count of either that is not zero.
pub fn troubled(text: impl fmt::Display) -> Piece {
    Piece::painted(reader(), text, |style, text| style.troubled(text))
}

/// Whether `ending` says something went wrong: a rule refused, the machine failed, a test trapped or exited.
fn trouble(ending: &str) -> bool {
    matches!(ending, "refused" | "failed" | "trapped") || ending.starts_with("exited ")
}

/// `ending` as it is painted: as trouble where it says something went wrong.
fn worded(ending: &str) -> Piece {
    match trouble(ending) {
        true => troubled(ending),
        false => ending.into(),
    }
}

/// What a tree is about, named on its first line.
///
/// Four namespaces, and only the first is the program's — which is where a leading `/` comes from, and the only place one is ever written. A mount prefix is a name a program can spell, an executable's and a foreign module's are identifiers their manifest rows chose, and a file's is whatever was typed or wherever it landed. Rendering all of them through one type is what keeps a print site from spelling a slash it did not derive.
#[derive(Clone, PartialEq, Eq)]
pub enum Subject {
    /// A unit, by the prefix it mounts, or a module, by its path: `/` for the root, which joins to nothing.
    Mounted(Qualifier),
    /// A declared executable, by the name its manifest row gives it.
    Executable(String),
    /// A declared foreign module, by the name its manifest row gives it. Never a prefix: nothing in Curios source refers to it, since a `foreign` declaration is reached by its own qualified name rather than through whatever implements it.
    Module(String),
    /// A file, as it was written on the command line or as it landed on disk.
    File(PathBuf),
    /// A directory, as it was written: the separator it is shown closed by is what tells a reader the nodes beneath are what landed in it.
    Directory(PathBuf),
    /// The text on standard input, which was asked for as `-` and is reported as what that means: a `-` alone on a line reads as one the compiler failed to finish writing.
    Stdin,
}

impl Subject {
    /// The subject a package is reported as. Its declared name *is* its mount prefix, so it is reported as one — which keeps the slash derived here as it is everywhere else instead of written by the caller.
    pub fn package(name: &str) -> Self {
        Self::Mounted(Qualifier::empty().with(name))
    }

    /// What a program or a library is reported as: the name that was asked for, never the file it resolved to. A declared executable resolves to an absolute path somewhere under the governing root, and echoing that back fills a line with what the reader already knew; a loose file is what was asked for, so it is reported as written.
    pub fn of(named: &Named) -> Self {
        match named {
            Named::Library(package) => Self::package(package),
            Named::Executable(name) => Self::Executable(name.clone()),
            Named::File(path) => Self::File(path.clone()),
            Named::Stdin => Self::Stdin,
        }
    }
}

impl fmt::Display for Subject {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Mounted(prefix) if prefix.is_root() => formatter.write_str("/"),
            Self::Mounted(prefix) => formatter.write_str(&prefix.join()),
            Self::Executable(name) | Self::Module(name) => formatter.write_str(name),
            Self::File(path) => write!(formatter, "{}", path.display()),
            Self::Directory(path) => write!(
                formatter,
                "{}/",
                path.display().to_string().trim_end_matches('/')
            ),
            Self::Stdin => formatter.write_str(STDIN_NAME),
        }
    }
}

/// A step taken for no one subject, on a line of its own under none: `serve; http://127.0.0.1:8000/`, where what is served is every subject's.
pub fn alone(step: Step, outcome: impl Into<Piece>) {
    tell(Event::Alone {
        step,
        outcome: outcome.into(),
    });
}

/// What a whole invocation came to, as its last line, with the time `taken` by all it sums where that was measured: `3 passed, 1 failed; 0.2s`.
pub fn total(text: impl Into<Piece>, taken: Option<Duration>) {
    tell(Event::Total {
        text: text.into(),
        taken,
    });
}

/// A blank line, between a tree and the report of what stopped it.
pub fn apart() {
    tell(Event::Apart);
}

/// `text` written whole, under no subject: the report of what stopped a command, a running program's stop, what stopped a rung's compile past it.
pub fn said(text: impl Into<Piece>) {
    tell(Event::Said { text: text.into() });
}

/// A fold plans `units` units: what the live drawing counts what is done against.
pub fn planned(units: usize) {
    tell(Event::Planned { units });
}

/// A unit a fold planned is settled: compiled, or taken from the cabinet.
pub fn settled() {
    tell(Event::Settled);
}

/// `manifest` as a reader standing in `here` would spell it: nothing when it sits right there, a climb of `../` when it sits above, and the path itself anywhere else — `--manifest` can name a file off to the side.
fn spelled_from(manifest: &Path, here: &Path) -> Option<String> {
    let directory = manifest.parent()?;

    if directory == here {
        return None;
    }

    Some(match here.strip_prefix(directory) {
        Ok(below) => format!(
            "{}{}",
            "../".repeat(below.components().count()),
            manifest.file_name()?.to_string_lossy()
        ),
        Err(_) => manifest.display().to_string(),
    })
}

/// `path` as whoever reads standard error would spell it from where they stand ([`Style::spelled`]).
pub fn from_here(path: &Path) -> String {
    reader().spelled(path)
}

/// One subject's tree, as whoever tells it holds it: which tree it is, and when its open node's work began. The transcript draws it ([`Trees`]).
pub struct Tree {
    opened: Opened,
    /// When the open node's work began, where one is open.
    began: Option<Instant>,
}

impl Tree {
    /// A tree for `subject`, naming `manifest` beside it when the invocation does not stand in the manifest's directory, which is the one case where what governs could otherwise be a guess.
    pub fn of(subject: &Subject, manifest: Option<&Path>) -> Self {
        let here = env::current_dir().and_then(|here| here.canonicalize());
        let opened = Opened::minted();
        tell(Event::Named {
            tree: opened,
            subject: subject.to_string(),
            manifest: manifest
                .zip(here.ok())
                .and_then(|(manifest, here)| spelled_from(manifest, &here)),
        });

        Self {
            opened,
            began: None,
        }
    }

    /// Leave the tree as it stands. A subject no node was written for says nothing, unless its line names the manifest that governs it: that is said whatever was done, since nothing else tells which package the invocation reached.
    pub fn close(self) {
        tell(Event::Left { tree: self.opened });
    }

    /// Which tree this is, for whoever relays what the work of its open node writes ([`Relayed`]).
    pub fn opened(&self) -> Opened {
        self.opened
    }

    /// Open a node for `step`, the last its tree plans or not.
    pub fn open(&mut self, step: Step, last: bool) {
        self.began(Node::Step(step), last);
    }

    /// Open a node for the test at `path`, before it runs: a test that never ends is the one whose line stands open.
    pub fn test(&mut self, path: &str) {
        self.began(Node::Test(path.to_string()), false);
    }

    /// Tell the transcript a node begins.
    fn began(&mut self, node: Node, last: bool) {
        self.began = Some(Instant::now());
        tell(Event::Began {
            tree: self.opened,
            node,
            last,
        });
    }

    /// Say a node for `step` at once, the last its tree plans or not, with what it came to stand as and the lines beneath it: nothing runs between its label and its outcome, so a plain line says it once.
    pub fn stood(&mut self, step: Step, last: bool, stands: impl Into<Piece>, beneath: Vec<Piece>) {
        tell(Event::Stood {
            tree: self.opened,
            step,
            last,
            stands: stands.into(),
            beneath,
        });
    }

    /// Whether a node is open: its label written and its outcome not yet.
    pub fn is_open(&self) -> bool {
        self.began.is_some()
    }

    /// Close the open node with what its work took: `; 1.4s`.
    pub fn done(&mut self) {
        let taken = self.taken();
        self.closed(Closing::Done(taken), Vec::new());
    }

    /// Close the open node with how its work ended where it did not simply finish, and what it took: `; refused in 0.3s`.
    pub fn ending(&mut self, ending: &str) {
        self.ending_beneath(ending, Vec::new());
    }

    /// [`Tree::ending`], with the lines of `beneath` under it, one block with the node's line: why a cabinet took nothing.
    pub fn ending_beneath(&mut self, ending: &str, beneath: Vec<Piece>) {
        let taken = self.taken();
        self.closed(Closing::Ended(ending.to_string(), taken), beneath);
    }

    /// Close the open node with what its work came to stand as, where that is the one fact it is read for: `; .curios/native/hello/hello`.
    pub fn outcome(&mut self, outcome: impl Into<Piece>) {
        self.outcome_beneath(outcome, Vec::new());
    }

    /// [`Tree::outcome`], with the lines of `beneath` under it, one block with the node's line: the keys a `pin` wrote.
    pub fn outcome_beneath(&mut self, outcome: impl Into<Piece>, beneath: Vec<Piece>) {
        self.began = None;
        self.closed(Closing::Stands(outcome.into()), beneath);
    }

    /// Tell the transcript the open node closed.
    fn closed(&self, closing: Closing, beneath: Vec<Piece>) {
        tell(Event::Closed {
            tree: self.opened,
            closing,
            beneath,
        });
    }

    /// Close the open node with where its work came to stand, or as `failed` where it could not be done: why is `done`'s to say, handed back for whoever reports it beneath.
    pub fn stands<E>(&mut self, done: Result<(), E>, place: impl Into<Piece>) -> Result<(), E> {
        match &done {
            Ok(()) => self.outcome(place),
            Err(_) => {
                self.ending("failed");
                apart();
            }
        }

        done
    }

    /// Close the open test's node with its ending, the time `taken` whoever ran it measured and the `place` its name is written at where one is said, as what the test `wrote` of its own leaves the node's line, then what only they know of it, a line of `beneath` each: `; ✓ passed in 0.1s` on the node's own line, `; ✗ failed in 0.1s; tests.crs:4:6` with a place, or beneath what the test wrote.
    pub fn ended(
        &mut self,
        ending: &str,
        taken: Duration,
        place: Option<&str>,
        wrote: Wrote,
        beneath: &[String],
    ) {
        self.began = None;
        tell(Event::Tested {
            tree: self.opened,
            ending: ending.to_string(),
            taken,
            place: place.map(str::to_string),
            wrote,
            beneath: beneath.to_vec(),
        });
    }

    /// Close the tree with the node that sums it up, and the time `taken` by all it sums where any of it ran: `└─ 2 passed, 1 failed; 0.1s`.
    pub fn summed(self, sum: impl Into<Piece>, taken: Option<Duration>) {
        tell(Event::Summed {
            tree: self.opened,
            sum: sum.into(),
            taken,
        });
    }

    /// What the open node's work took, which closes it.
    fn taken(&mut self) -> Duration {
        self.began.take().map_or(Duration::ZERO, |at| at.elapsed())
    }
}

/// A count as the transcript spells one, its thousands apart: `4,312`.
pub fn grouped(count: u64) -> String {
    let digits = count.to_string();

    digits
        .char_indices()
        .flat_map(|(at, digit)| {
            let apart = at > 0 && (digits.len() - at).is_multiple_of(3);

            apart.then_some(',').into_iter().chain([digit])
        })
        .collect()
}

/// A time as the transcript spells one: seconds, to a tenth.
fn seconds(taken: Duration) -> String {
    format!("{:.1}s", taken.as_secs_f64())
}
