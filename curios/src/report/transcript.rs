//! The transcript: one value behind a lock, the drawing with what a run keeps of what it writes, drawn by whoever tells an event, on the thread that tells it.
//!
//! **Drawn where it is told.** A label reaches the reader before the work it names, and a process that dies mid-step leaves its last line written, with nothing queued to lose. Whoever tells an event holds the lock for a line, and whoever relays what a test wrote for the write. Rust's standard error is unbuffered, so a line reaches the reader with no flushing of its own.
//!
//! **A run that sends pages its report keeps the transcript** ([`keep`]): every byte written from then on is, plain, what a page is sent, so the page shows what the terminal showed.

use {
    super::{Column, Event, Owed, Trees, frame_of, framed_run},
    crate::{Piece, Quoted, Wrote, answered, reader},
    curios_common::Style,
    std::{
        mem,
        sync::{Mutex, MutexGuard, PoisonError},
    },
};

/// The drawing, and what a run keeps of what it writes.
struct Transcript {
    trees: Trees,
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

static TRANSCRIPT: Mutex<Transcript> = Mutex::new(Transcript {
    trees: Trees::new(),
    kept: None,
});

/// The transcript, held. A teller that panicked left every byte it wrote, so its poison is passed over.
fn held() -> MutexGuard<'static, Transcript> {
    TRANSCRIPT.lock().unwrap_or_else(PoisonError::into_inner)
}

/// Draw `event`.
pub fn tell(event: Event) {
    let mut transcript = held();
    let drawn = transcript.trees.draw(event);
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

/// What a node's work has written of its own on the standard streams while the node's line stood open: whether it wrote, and where its writes stand. The two streams are read as one line, which is what they are on a terminal and in a pipe that joins them.
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

    /// Note a write: the first ends the line the node's label stands open on, so what follows stands beneath it.
    fn writing(&mut self, transcript: &mut Transcript) {
        if !mem::replace(&mut self.wrote, true) {
            transcript.say(&Piece::from("\n"));
        }
    }
}

/// The work `relayed` is of writes past the transcript — a child given one of its streams, a terminal taken —, where its bytes are neither seen nor framed: the node's line is ended ahead of them, and where they leave the line is taken as where the work left it.
pub fn written_past(relayed: &mut Relayed) {
    relayed.writing(&mut held());
}

/// Relay `bytes`, which the work `relayed` is of wrote on the stream `quoted` names, each line behind its frame as that stream's reader sees it, through `write`, which writes a run of bytes on that stream and answers how many it took. Answers how many of the work's own bytes were taken: a frame is never left half written before them, a run the stream took only part of ends the relay there, and a failure after the first run is met at the next write.
pub fn relay<E>(
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
        let wrote = match run(owed, own, quoted, style, &mut relayed.column, &mut write) {
            Ok(wrote) => wrote,
            Err(failure) if taken == 0 => return Err(failure),
            // What the stream took before it failed was written: the work is told of that much, and meets the failure at its next write.
            Err(_) => break,
        };
        if let Some(kept) = transcript.kept.as_mut() {
            kept.extend_from_slice(frame_of(owed, quoted, &Style::plain()).as_bytes());
            kept.extend_from_slice(&own[..wrote]);
        }
        taken += wrote;
        if wrote < own.len() {
            break;
        }
    }

    Ok(taken)
}

/// Write `own`, one run of the work's bytes, behind what its line is `owed`, and answer how many of them `write` took, leaving `column` where the stream stands.
fn run<E>(
    owed: Owed,
    own: &[u8],
    quoted: Quoted,
    style: &Style,
    column: &mut Column,
    write: &mut impl FnMut(&[u8]) -> Result<usize, E>,
) -> Result<usize, E> {
    if owed != Owed::Nothing {
        let frame = frame_of(owed, quoted, style);
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
