//! The transcript: one value behind a lock, the drawing it is made in with what a run keeps of what it writes, drawn by whoever tells an event, on the thread that tells it.
//!
//! **Drawn where it is told.** A label reaches the reader before the work it names, and a process that dies mid-step leaves its last line written, with nothing queued to lose. Whoever tells an event holds the lock for a line, and whoever relays what a test wrote for the write. Rust's standard error is unbuffered, so a line reaches the reader with no flushing of its own.
//!
//! **One transcript, drawn either way** ([`Drawing`]): live ([`Live`]) or as plain lines ([`Lines`]), chosen once, as the command starts ([`transcribe`]), before anything is written. The live drawing's clock is a thread of its own, which takes the same lock to redraw the status; what writes past the transcript — an answer, a program run, a child given a test's stream — has the status cleared first ([`settle`]), the command's end clears it for good ([`finish`]), and a panic or a terminating signal that ends the command writes in its place what that end would have written ([`standard_error_leaving`]).
//!
//! **A run that sends pages its report keeps the transcript** ([`keep`]): every byte written from then on is, plain, what a page is sent, so the page shows what the terminal showed.

use {
    super::{Column, Event, Frame, Lines, Live, Opened, Owed, Trees, frame_of, framed_run},
    crate::{Piece, Quoted, Wrote, answered, reader},
    curios_common::Style,
    curios_native::{standard_error_farewell, standard_error_leaving, standard_error_size},
    std::{
        ffi::OsStr,
        mem, panic,
        sync::{Mutex, MutexGuard, PoisonError},
        thread,
        time::{Duration, Instant},
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
    /// The trees, written as they are told and never redrawn: what is drawn before a command chooses.
    Trees(Trees),
    Lines(Lines),
    Live(Live),
}

impl Drawer {
    /// What a line the work of `tree`'s open node wrote is `owed`, as a reader of `style` sees it.
    fn frame(&self, tree: Opened, owed: Owed, quoted: Quoted, style: &Style) -> String {
        match self {
            Drawer::Trees(_) | Drawer::Live(_) => frame_of(owed, quoted, style),
            Drawer::Lines(lines) => lines.frame(tree, owed, quoted, style),
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
    /// Write `frame` on standard error as its reader sees it, and what it writes for good, plain, onto what is kept where a run keeps it: never the status, which is redrawn.
    fn say(&mut self, frame: Frame) {
        self.show(&frame.shown);
        if let Some(kept) = self.kept.as_mut() {
            kept.extend_from_slice(frame.above.plain.as_bytes());
        }
    }

    /// Write `shown` on standard error, leaving what the live drawing's finish would write should a signal end the command after it.
    fn show(&self, shown: &str) {
        let farewell = match &self.drawer {
            Drawer::Live(live) => live.farewell(Instant::now()),
            Drawer::Trees(_) | Drawer::Lines(_) => String::new(),
        };
        standard_error_leaving(shown.as_bytes(), farewell.into_bytes());
    }

    /// Draw `event`, told at `now`.
    fn draw(&mut self, event: Event, now: Instant) -> Frame {
        match &mut self.drawer {
            Drawer::Trees(trees) => written(trees.draw(event)),
            Drawer::Lines(lines) => written(lines.draw(event)),
            Drawer::Live(live) => live.draw(event, now, measured()),
        }
    }

    /// What the work of the open node is owed ahead of the first byte it writes: the end of the line its label stands open on in a tree, and nothing on a plain line, which is ended already.
    fn cut(&mut self) -> Frame {
        match &mut self.drawer {
            Drawer::Trees(_) => written(Piece::from("\n")),
            Drawer::Lines(_) => Frame::default(),
            Drawer::Live(live) => live.cut(),
        }
    }
}

/// A frame of a drawing that only ever writes: what it writes, written for good.
fn written(piece: Piece) -> Frame {
    Frame {
        shown: piece.shown.clone(),
        above: piece,
    }
}

/// The terminal standard error is, measured now.
fn measured() -> (u16, u16) {
    sized(standard_error_size())
}

/// The size the status is drawn for where a terminal reports `size`: as reported, or as a terminal is taken to be where there is none — a live drawing forced onto a pipe — and in each measure a terminal reports as nothing, as a pseudo-terminal no one sized does.
pub fn sized(size: Option<(u16, u16)>) -> (u16, u16) {
    let (columns, rows) = size.unwrap_or(UNMEASURED);
    let given = |measure: u16, unmeasured: u16| match measure {
        0 => unmeasured,
        measure => measure,
    };

    (given(columns, UNMEASURED.0), given(rows, UNMEASURED.1))
}

/// How a terminal is measured where it is none.
const UNMEASURED: (u16, u16) = (80, 24);

/// How long the live drawing's clock waits between frames: ten a second at most.
const TICK: Duration = Duration::from_millis(100);

/// The one transcript of the process, behind its lock. Trees until the command chooses, so a line written before it does — a refusal of the command line itself — is drawn as one was before there were two.
static TRANSCRIPT: Mutex<Transcript> = Mutex::new(Transcript {
    drawer: Drawer::Trees(Trees::new()),
    kept: None,
});

/// The transcript, held. A teller that panicked left every byte it wrote, so its poison is passed over.
fn held() -> MutexGuard<'static, Transcript> {
    TRANSCRIPT.lock().unwrap_or_else(PoisonError::into_inner)
}

/// Make the transcript in `drawing` from here on. Chosen once, as the command starts, before anything is written; a live drawing starts its clock, which redraws the status as running nodes' times move, and has a panic write what the drawing's finish would, ahead of what the panic says.
pub fn transcribe(drawing: Drawing) {
    held().drawer = match drawing {
        Drawing::Plain => Drawer::Lines(Lines::new()),
        Drawing::Live => Drawer::Live(Live::new(Instant::now())),
    };
    if drawing == Drawing::Live {
        thread::spawn(clock);
        // The farewell the last frame left is written, never one drawn now: a panic inside a frame holds the transcript's lock.
        let said = panic::take_hook();
        panic::set_hook(Box::new(move |panicked| {
            standard_error_farewell();
            said(panicked);
        }));
    }
}

/// The live drawing's clock: the status drawn again each tick, for as long as the drawing is live. Taking the transcript's lock, it never draws inside another's line.
fn clock() {
    loop {
        thread::sleep(TICK);
        let mut transcript = held();
        let Drawer::Live(live) = &mut transcript.drawer else {
            return;
        };
        let shown = live.tick(Instant::now(), measured());
        transcript.show(&shown);
    }
}

/// Draw `event`.
pub fn tell(event: Event) {
    let mut transcript = held();
    let frame = transcript.draw(event, Instant::now());
    transcript.say(frame);
}

/// Clear the live status and draw none until a node closes again: what follows writes past the transcript — an answer on standard output, the program a command runs.
pub fn settle() {
    let mut transcript = held();
    if let Drawer::Live(live) = &mut transcript.drawer {
        let shown = live.settle();
        transcript.show(&shown);
    }
}

/// The command is ending: whatever the live drawing still holds is written, then its tally, and its status is cleared for good. Called once, as the process ends, whatever the command came to.
pub fn finish() {
    let mut transcript = held();
    if let Drawer::Live(live) = &mut transcript.drawer {
        let frame = live.finish(Instant::now());
        transcript.say(frame);
    }
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

    /// Note a write of the work of the open node: the first is owed what the drawing owes a node's work ahead of it.
    fn writing(&mut self, transcript: &mut Transcript) {
        if !mem::replace(&mut self.wrote, true) {
            let cut = transcript.cut();
            transcript.say(cut);
        }
    }
}

/// The work of the open node, which `relayed` is of, writes past the transcript — a child given one of its streams, a terminal taken —, where its bytes are neither seen nor framed: the node's line is ended ahead of them, the live status is cleared and drawn no more until the node closes, and where they leave the line is taken as where the work left it.
pub fn written_past(relayed: &mut Relayed) {
    let mut transcript = held();
    relayed.writing(&mut transcript);
    if let Drawer::Live(live) = &mut transcript.drawer {
        let shown = live.settle();
        transcript.show(&shown);
    }
}

/// Relay `bytes`, which the work of `tree`'s open node wrote on the stream `quoted` names, each line behind its frame as that stream's reader sees it, through `write`, which takes a run of bytes whole for that stream or fails; `relayed` is what that work wrote before. The first failure ends the relay and is answered, so the work learns its bytes were not taken whole, and the runs before it stand relayed.
pub fn relay<E>(
    tree: Opened,
    relayed: &mut Relayed,
    quoted: Quoted,
    bytes: &[u8],
    mut write: impl FnMut(&[u8]) -> Result<(), E>,
) -> Result<(), E> {
    // An empty write is no write: it ends no line.
    if bytes.is_empty() {
        return Ok(());
    }
    let style = match quoted {
        Quoted::Output => answered(),
        Quoted::Error => reader(),
    };
    let mut transcript = held();
    // The live status is cleared, so the work's lines stand where it stood, and drawn again once they leave the stream at a line's start.
    if let Drawer::Live(live) = &mut transcript.drawer {
        let shown = live.clear();
        transcript.show(&shown);
    }
    relayed.writing(&mut transcript);

    let mut rest = bytes;
    let mut relayed_whole = Ok(());
    while !rest.is_empty() {
        let (owed, length) = framed_run(relayed.column, rest);
        let (own, after) = rest.split_at(length);
        let frame = transcript.drawer.frame(tree, owed, quoted, style);
        if let Err(failure) = run(&frame, own, &mut relayed.column, &mut write) {
            relayed_whole = Err(failure);
            break;
        }
        let plain = transcript.drawer.frame(tree, owed, quoted, &Style::plain());
        if let Some(kept) = transcript.kept.as_mut() {
            kept.extend_from_slice(plain.as_bytes());
            kept.extend_from_slice(own);
        }
        rest = after;
    }
    if let (Drawer::Live(live), Column::Start) = (&mut transcript.drawer, relayed.column) {
        let shown = live.tick(Instant::now(), measured());
        transcript.show(&shown);
    }

    relayed_whole
}

/// Write `own`, one run of the work's bytes, behind `frame`, what its line is owed, each taken whole by `write`, leaving `column` where the stream stands.
fn run<E>(
    frame: &str,
    own: &[u8],
    column: &mut Column,
    write: &mut impl FnMut(&[u8]) -> Result<(), E>,
) -> Result<(), E> {
    if !frame.is_empty() {
        write(frame.as_bytes())?;
        // The frame opened the line, whatever becomes of the bytes behind it.
        *column = Column::Within;
    }
    write(own)?;
    *column = column.after(own);

    Ok(())
}
