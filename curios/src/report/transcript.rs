//! The transcript: one value behind a lock, the drawing it is made in with what a run keeps of what it writes, drawn by whoever tells an event, on the thread that tells it.
//!
//! **Drawn where it is told.** A label reaches the reader before the work it names, and a process that dies mid-step leaves its last line written, with nothing queued to lose. Whoever tells an event holds the lock for a line, and whoever relays what a test wrote for the write. Rust's standard error is unbuffered, so a line reaches the reader with no flushing of its own.
//!
//! **One transcript, drawn either way** ([`Drawing`]): as trees ([`Trees`]) or as plain lines ([`Lines`]), chosen once, as the command starts ([`transcribe`]), before anything is written.
//!
//! **A run that sends pages its report keeps the transcript** ([`keep`]): every byte written from then on is, plain, what a page is sent, so the page shows what the terminal showed.

use {
    super::{Column, Event, Lines, Opened, Owed, Trees, frame_of, framed_run},
    crate::{Piece, Quoted, Wrote, answered, reader},
    curios_common::Style,
    std::{
        ffi::OsStr,
        mem,
        sync::{Mutex, MutexGuard, PoisonError},
    },
};

/// The environment variable naming the drawing where `--progress` does not: a reader's standing choice, a screen reader's plain lines on a terminal among them.
pub const PROGRESS: &str = "CURIOS_PROGRESS";

/// Which drawing a transcript is made in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Drawing {
    /// A line an event, each standing alone: what a pipe, a log or a screen reader is written.
    Plain,
    /// The trees, as a person watching a terminal reads them.
    Live,
}

impl Drawing {
    /// Every drawing, in the order the flag lists them.
    pub const ALL: [Drawing; 2] = [Drawing::Plain, Drawing::Live];

    /// The drawing's one spelling, which the flag and the environment variable both take.
    pub fn spelled(self) -> &'static str {
        match self {
            Drawing::Plain => "plain",
            Drawing::Live => "live",
        }
    }

    /// The drawing a command's transcript is made in: the one `asked` on the command line, else the one `named` in the environment, else live where standard error is a `terminal` whose `term` is not `dumb` and no `ci` runs it, and plain everywhere else — a pipe, a log, a dumb terminal, a continuous-integration page. `Err` hands back what `named` was where it spells no drawing.
    pub fn chosen(
        asked: Option<Drawing>,
        named: Option<&OsStr>,
        terminal: bool,
        term: Option<&OsStr>,
        ci: bool,
    ) -> Result<Drawing, String> {
        let named = named.map(|named| {
            Drawing::ALL
                .into_iter()
                .find(|drawing| named == drawing.spelled())
                .ok_or_else(|| named.to_string_lossy().into_owned())
        });
        let smart = terminal && term.is_none_or(|term| term != "dumb") && !ci;

        match (asked, named) {
            (Some(asked), _) => Ok(asked),
            (None, Some(named)) => named,
            (None, None) if smart => Ok(Drawing::Live),
            (None, None) => Ok(Drawing::Plain),
        }
    }
}

/// A drawing with what it holds of the trees it was told of.
enum Drawer {
    Trees(Trees),
    Lines(Lines),
}

impl Drawer {
    /// What `event` is drawn as on standard error.
    fn draw(&mut self, event: Event) -> Piece {
        match self {
            Drawer::Trees(trees) => trees.draw(event),
            Drawer::Lines(lines) => lines.draw(event),
        }
    }

    /// What a line the work of `tree`'s open node wrote is `owed`, as a reader of `style` sees it.
    fn frame(&self, tree: Opened, owed: Owed, quoted: Quoted, style: &Style) -> String {
        match self {
            Drawer::Trees(_) => frame_of(owed, quoted, style),
            Drawer::Lines(lines) => lines.frame(tree, owed, quoted, style),
        }
    }

    /// What the work of an open node is owed ahead of the first byte it writes: the end of the line its label stands open on in a tree, and nothing on a plain line, which is ended already.
    fn cut(&self) -> Piece {
        match self {
            Drawer::Trees(_) => Piece::from("\n"),
            Drawer::Lines(_) => Piece::default(),
        }
    }
}

/// The drawing, and what a run keeps of what it writes.
struct Transcript {
    drawer: Drawer,
    /// What was written since a run asked for it to be kept, plain, and nothing where none did.
    kept: Option<Vec<u8>>,
}

impl Transcript {
    /// Write `piece` on standard error, as its reader sees it, and plain onto what is kept where a run keeps it.
    fn say(&mut self, piece: &Piece) {
        eprint!("{}", piece.shown);
        if let Some(kept) = self.kept.as_mut() {
            kept.extend_from_slice(piece.plain.as_bytes());
        }
    }
}

/// The one transcript of the process, behind its lock. Trees until the command chooses, so a line written before it does — a refusal of the command line itself — is drawn as one was before there were two.
static TRANSCRIPT: Mutex<Transcript> = Mutex::new(Transcript {
    drawer: Drawer::Trees(Trees::new()),
    kept: None,
});

/// The transcript, held. A teller that panicked left every byte it wrote, so its poison is passed over.
fn held() -> MutexGuard<'static, Transcript> {
    TRANSCRIPT.lock().unwrap_or_else(PoisonError::into_inner)
}

/// Make the transcript in `drawing` from here on. Chosen once, as the command starts, before anything is written.
pub fn transcribe(drawing: Drawing) {
    held().drawer = match drawing {
        Drawing::Plain => Drawer::Lines(Lines::new()),
        Drawing::Live => Drawer::Trees(Trees::new()),
    };
}

/// Draw `event`.
pub fn tell(event: Event) {
    let mut transcript = held();
    let drawn = transcript.drawer.draw(event);
    transcript.say(&drawn);
}

/// Keep what the transcript writes from here on. A run that sends pages its report asks before its first line, so what a page is sent is what the terminal showed and the two cannot drift.
pub fn keep() {
    held().kept.get_or_insert_default();
}

/// What the transcript wrote since it was asked kept, in the order written, and nothing where it was not.
pub fn kept() -> Vec<u8> {
    held().kept.clone().unwrap_or_default()
}

/// What the work of an open node — a test's — has written of its own on the standard streams: whether it wrote, and where its writes stand. The two streams are read as one line, which is what they are on a terminal and in a pipe that joins them.
#[derive(Debug, Default)]
pub struct Relayed {
    wrote: bool,
    column: Column,
}

impl Relayed {
    /// What the work wrote, as its node is closed by.
    pub fn wrote(&self) -> Wrote {
        Wrote::of(self.wrote, self.column)
    }

    /// Note a write: the first is owed what the drawing owes a node's work ahead of it.
    fn writing(&mut self, transcript: &mut Transcript) {
        if !mem::replace(&mut self.wrote, true) {
            let cut = transcript.drawer.cut();
            transcript.say(&cut);
        }
    }
}

/// The work `relayed` is of writes past the transcript — a child given one of its streams, a terminal taken —, where its bytes are neither seen nor framed: the node's line is ended ahead of them, and where they leave the line is taken as where the work left it.
pub fn written_past(relayed: &mut Relayed) {
    relayed.writing(&mut held());
}

/// Relay `bytes`, which the work of `tree`'s open node wrote on the stream `quoted` names, each line behind its frame as that stream's reader sees it, through `write`, which writes a run of bytes on that stream and answers how many it took; `relayed` is what that work wrote before. Answers how many of the work's own bytes were taken: a frame is never left half written before them, a run the stream took only part of ends the relay there, and a failure after the first run is met at the next write.
pub fn relay<E>(
    tree: Opened,
    relayed: &mut Relayed,
    quoted: Quoted,
    bytes: &[u8],
    mut write: impl FnMut(&[u8]) -> Result<usize, E>,
) -> Result<usize, E> {
    // An empty write is no write: it ends no line.
    if bytes.is_empty() {
        return Ok(0);
    }
    let style = match quoted {
        Quoted::Output => answered(),
        Quoted::Error => reader(),
    };
    let mut transcript = held();
    relayed.writing(&mut transcript);

    let mut taken = 0;
    while taken < bytes.len() {
        let (owed, length) = framed_run(relayed.column, &bytes[taken..]);
        let own = &bytes[taken..taken + length];
        let frame = transcript.drawer.frame(tree, owed, quoted, style);
        let wrote = match run(&frame, own, &mut relayed.column, &mut write) {
            Ok(wrote) => wrote,
            Err(failure) if taken == 0 => return Err(failure),
            // What the stream took before it failed was written: the work is told of that much, and meets the failure at its next write.
            Err(_) => break,
        };
        let plain = transcript.drawer.frame(tree, owed, quoted, &Style::plain());
        if let Some(kept) = transcript.kept.as_mut() {
            kept.extend_from_slice(plain.as_bytes());
            kept.extend_from_slice(&own[..wrote]);
        }
        taken += wrote;
        if wrote < own.len() {
            break;
        }
    }

    Ok(taken)
}

/// Write `own`, one run of the work's bytes, behind `frame`, what its line is owed, and answer how many of them `write` took, leaving `column` where the stream stands.
fn run<E>(
    frame: &str,
    own: &[u8],
    column: &mut Column,
    write: &mut impl FnMut(&[u8]) -> Result<usize, E>,
) -> Result<usize, E> {
    if !frame.is_empty() {
        let mut rest = frame.as_bytes();
        while !rest.is_empty() {
            let wrote = write(rest)?.min(rest.len());
            if wrote == 0 {
                break;
            }
            rest = &rest[wrote..];
        }
        // The frame opened the line, whatever becomes of the bytes behind it.
        *column = Column::Within;
    }
    let wrote = write(own)?.min(own.len());
    *column = column.after(&own[..wrote]);

    Ok(wrote)
}
