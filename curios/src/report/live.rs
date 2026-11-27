//! The live drawing: what is written for good, and beneath it a status redrawn.
//!
//! **Above, for good, the tree drawing's bytes as it writes them**: each subject's line, each node once it closes, what its tests write quoted beneath their nodes. Subjects are narrated one at a time, so the scrollback is the tree drawing's bytes exactly.
//!
//! **Beneath, the status**: the running node as it will read when it closes, its time after `;`, and of a check behind `·` how far it has come, `├─ check; 2.4s · 416 of 1,040 · `/std/Map/insert``, which the clock alone draws; then what is done, `2 of 7 units; 2.4s`, directly beneath the node, or directly beneath what is written for good where no node runs. Each line is clipped at the terminal's width and drawn dim: the status is what is not settled yet.
//!
//! **The cursor is the boundary.** Between frames the cursor stands at the start of the status's first line, where what is written for good ends, so a terminal that reflows on a resize carries the boundary with the text it stands in. A frame erases from the cursor to the end of the screen, which takes the status however a narrowing wrapped it, writes what is for good, then writes the status with autowrap off, so each line takes one row whatever width the terminal has by the time it reads the frame, and moves back up over the lines that write drew alone: nothing is counted across frames, and no mode is left set.
//!
//! **Every ending erases it.** The finish writes the tally for good, a blank line above it, in the regular colour; a signal or a panic that ends the command writes what the finish would have written then ([`Live::farewell`]), which each frame leaves for it.
//!
//! **Time and the terminal are handed in**: every frame is drawn for a moment and a size the caller reads, so a frame is a function of what the drawing was told and when.

use {
    super::{Event, Trees, grouped, seconds},
    crate::{Piece, reader},
    curios_common::clusters,
    std::{mem, time::Instant},
};

/// A node that runs, as the status shows it until it closes.
struct Running {
    /// The node's connector and label, as the tree draws them: `├─ check`.
    label: String,
    began: Instant,
    /// How far the check it is has come, where it is a check that said: shown behind `·`.
    walked: Option<Walk>,
}

/// How far a check has come: `done` of the `of` declarations it walks, and the one it is on, where it is on one.
struct Walk {
    done: usize,
    of: usize,
    on: Option<String>,
}

impl Running {
    /// The node's line at `now`, as it will read when it closes, its time after `;`, then how far its check has come: `├─ check; 2.4s · 416 of 1,040 · `/std/Map/insert``.
    fn line(&self, now: Instant) -> String {
        let mut line = format!(
            "{}; {}",
            self.label,
            seconds(now.saturating_duration_since(self.began))
        );
        if let Some(walk) = &self.walked {
            line.push_str(&format!(
                " · {} of {}",
                grouped(walk.done as u64),
                grouped(walk.of as u64)
            ));
            if let Some(on) = &walk.on {
                line.push_str(&format!(" · `{on}`"));
            }
        }

        line
    }
}

/// What a frame writes: everything, as the reader sees it, and what of it is written for good, which a run keeps for a page.
#[derive(Debug, Default)]
pub struct Frame {
    pub shown: String,
    pub above: Piece,
}

/// The live drawing, with what it holds of the open node and of the status it drew last.
pub struct Live {
    trees: Trees,
    /// What the tree drawing wrote and is not written above yet: the open node's label, unterminated, until a byte must follow it.
    held: Piece,
    running: Option<Running>,
    /// The units the folds planned, and how many of them are settled.
    planned: usize,
    settled: usize,
    started: Instant,
    /// The status as last drawn, so an unchanged one is not drawn again.
    status: Vec<String>,
    /// Whether something writes past the drawing: no status is drawn until a node closes again.
    quiet: bool,
    /// Whether the command has finished: no status is drawn again.
    finished: bool,
}

impl Live {
    /// A live drawing of a command that `started` then, told of nothing yet.
    pub fn new(started: Instant) -> Self {
        Self {
            trees: Trees::new(),
            held: Piece::default(),
            running: None,
            planned: 0,
            settled: 0,
            started,
            status: Vec::new(),
            quiet: false,
            finished: false,
        }
    }

    /// The frame `event`, told at `now`, is drawn as on a terminal of `size`, its columns and rows.
    pub fn draw(&mut self, event: Event, now: Instant, size: (u16, u16)) -> Frame {
        // How far a check has come moves its running node alone, which the clock draws at its next tick: told for each declaration, it would otherwise redraw the status as often.
        if let Event::Walked { done, of, on, .. } = event {
            if let Some(running) = self.running.as_mut() {
                running.walked = Some(Walk { done, of, on });
            }
            return Frame::default();
        }
        match &event {
            Event::Planned { units } => self.planned += units,
            Event::Settled => self.settled += 1,
            _ => {}
        }
        if event.tree().is_none() {
            // Under no subject, what is said is written above at once, beside whatever the open node holds.
            let above = self.trees.draw(event);
            return self.framed(above, now, size);
        }
        let opening = matches!(event, Event::Began { .. });
        let drawn = self.trees.draw(event);
        // A tree named and nothing written of it says nothing, and moves nothing: what the cabinet held, a tree yet to open.
        if drawn.plain.is_empty() {
            return self.framed(Piece::default(), now, size);
        }

        self.held = mem::take(&mut self.held).then(drawn);
        let (above, unterminated) = split(&self.held);
        self.held = unterminated;
        self.running = opening.then(|| Running {
            label: self.held.plain.clone(),
            began: now,
            walked: None,
        });
        // A closing is the drawing's own again, whatever wrote past it while the node ran.
        self.quiet &= opening;

        self.framed(above, now, size)
    }

    /// A frame writing `above`, erasing the status before it and drawing it again after; nothing where neither changed.
    fn framed(&mut self, above: Piece, now: Instant, size: (u16, u16)) -> Frame {
        let status = match self.quiet || self.finished {
            true => Vec::new(),
            false => self.lines(now, size),
        };
        if above.plain.is_empty() && status == self.status {
            return Frame::default();
        }
        let mut shown = self.erased();
        shown.push_str(&above.shown);
        shown.push_str(&below(&status));
        self.status = status;

        Frame { shown, above }
    }

    /// What erases the status as last drawn: everything from the cursor, which stands where what is written for good ends, to the end of the screen.
    fn erased(&mut self) -> String {
        let erased = match self.status.is_empty() {
            true => String::new(),
            false => "\x1b[J".to_string(),
        };
        self.status.clear();

        erased
    }

    /// The status at `now`, on a terminal of `size`, each line clipped at its width: the running node, and what is done; nothing where neither stands.
    fn lines(&self, now: Instant, size: (u16, u16)) -> Vec<String> {
        let running = self.running.as_ref().map(|running| running.line(now));

        [running, self.tally(now)]
            .into_iter()
            .flatten()
            .map(|line| clipped(&line, usize::from(size.0)))
            .collect()
    }

    /// What is done at `now`, where the folds planned any units: `2 of 7 units; 24.5s`.
    fn tally(&self, now: Instant) -> Option<String> {
        (self.planned > 0).then(|| {
            format!(
                "{} of {} units; {}",
                self.settled,
                self.planned,
                seconds(now.saturating_duration_since(self.started))
            )
        })
    }

    /// Erase the status and draw none until a node closes again: something is about to write past the drawing — standard output, a program run, a child given a test's stream, a terminal taken.
    pub fn settle(&mut self) -> String {
        self.quiet = true;

        self.erased()
    }

    /// The status erased ahead of a write past the drawing, which then stands where the status stood until it is drawn again.
    pub fn clear(&mut self) -> String {
        self.erased()
    }

    /// What the status's clock draws at `now`: the status again where it changed or a running node's time moved, and nothing otherwise.
    pub fn tick(&mut self, now: Instant, size: (u16, u16)) -> String {
        self.framed(Piece::default(), now, size).shown
    }

    /// The line the open node stands on, ended ahead of the first line its work writes, and written above.
    pub fn cut(&mut self) -> Frame {
        self.running = None;
        let above = mem::take(&mut self.held).then("\n");
        let mut shown = self.erased();
        shown.push_str(&above.shown);

        Frame { shown, above }
    }

    /// What the command's finish would write at `now`, which a signal or a panic that ends it writes in its place: the status erased, the open node's line ended, and the tally frozen; nothing once the command has finished.
    pub fn farewell(&self, now: Instant) -> String {
        if self.finished {
            return String::new();
        }
        let mut shown = match self.status.is_empty() {
            true => String::new(),
            false => "\x1b[J".to_string(),
        };
        shown.push_str(&self.remaining(now).shown);

        shown
    }

    /// What the open node holds, its line ended, then the tally at `now` frozen beneath a blank line.
    fn remaining(&self, now: Instant) -> Piece {
        let mut above = self.held.clone();
        if !above.plain.is_empty() {
            above = above.then("\n");
        }
        if let Some(tally) = self.tally(now) {
            above = above.then("\n").then(tally).then("\n");
        }

        above
    }

    /// The command finished at `now`: what the open node holds is written above, its line ended, then the tally, and the status erased for good.
    pub fn finish(&mut self, now: Instant) -> Frame {
        let above = self.remaining(now);
        self.held = Piece::default();
        self.running = None;
        self.finished = true;
        let mut shown = self.erased();
        shown.push_str(&above.shown);

        Frame { shown, above }
    }
}

/// `held`, split after its last line's end: the lines it holds whole, and what follows them, unterminated.
fn split(held: &Piece) -> (Piece, Piece) {
    let at = |text: &str| text.rfind('\n').map_or(0, |end| end + 1);
    let (shown, plain) = (at(&held.shown), at(&held.plain));

    (
        Piece {
            shown: held.shown[..shown].to_string(),
            plain: held.plain[..plain].to_string(),
        },
        Piece {
            shown: held.shown[shown..].to_string(),
            plain: held.plain[plain..].to_string(),
        },
    )
}

/// The status drawn where the cursor stands, at the start of the line below what is written for good, and the cursor brought back to that start: autowrap off while it is written, so each line takes one row whatever width the terminal has by the time it reads the frame, which makes the move back up over the lines this write drew exact; nothing is counted across frames.
fn below(status: &[String]) -> String {
    if status.is_empty() {
        return String::new();
    }
    let lines = status
        .iter()
        .map(|line| reader().dim(line))
        .collect::<Vec<_>>()
        .join("\r\n");
    let up = match status.len() - 1 {
        0 => String::new(),
        more => format!("\x1b[{more}A"),
    };

    format!("\x1b[?7l{lines}\x1b[?7h{up}\r")
}

/// `line` clipped at `columns`, by the cells each of its clusters takes.
fn clipped(line: &str, columns: usize) -> String {
    let mut kept = String::new();
    let mut taken = 0;
    for (cluster, wide) in clusters(line) {
        if taken + wide > columns {
            break;
        }
        kept.push_str(cluster);
        taken += wide;
    }

    kept
}
