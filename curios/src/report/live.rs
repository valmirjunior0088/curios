//! The live drawing: what is settled above what is open.
//!
//! **Above, for good, the oldest open subject streams as the tree drawing writes it**: its line, each node once it closes, what its tests write quoted beneath their nodes. A subject that begins while an older one is open is held, shown in the region, until every subject opened before it has ended; then what it holds is written and it streams in turn. So the scrollback is the tree drawing's bytes in the order subjects opened, and with one subject open at a time it is the tree drawing's bytes exactly.
//!
//! **Beneath, a region redrawn**: the streaming subject's running node, then each held subject's tree so far, capped at a third of the terminal's rows with `+ N more`, then what is done, `2 of 7 units; 2.4s`. A running node reads as it will when it closes, its time after `;`. Every row is cut at the terminal's width, `…` where cut, and drawn dim: the region is what is not settled yet.
//!
//! **Nothing to restore.** A frame is a carriage return, a cursor up over the rows the region took, an erase to the end of the screen, what is written above, and the region again: no alternate screen, no raw mode, no hidden cursor. An interrupted command leaves its last frame; a finished one leaves none ([`Live::finish`]).
//!
//! **Time and the terminal are handed in**: every frame is drawn for a moment and a size the caller reads, so a frame is a function of what the drawing was told and when.

use {
    super::{Event, Opened, Trees, seconds},
    crate::{Piece, reader},
    curios_common::clusters,
    std::{collections::BTreeMap, mem, time::Instant},
};

/// The fewest rows a region is given, whatever a third of the terminal comes to.
const FEWEST_ROWS: usize = 3;

/// A node a subject runs, as the region shows it until it closes.
struct Running {
    /// The node's connector and label, as the tree draws them: `├─ check`.
    label: String,
    began: Instant,
}

/// What the drawing holds of one open subject.
#[derive(Default)]
struct Open {
    /// What the tree drawing wrote of it and is not written above yet: all of it while the subject is held, and for the streaming subject its open node's label, unterminated, until a byte must follow it.
    held: Piece,
    running: Option<Running>,
    /// Whether the node it last opened is the last its tree plans, whose closing ends the subject.
    last: bool,
    /// Whether it ended while held, to be written whole once it streams.
    ended: bool,
}

/// What a frame writes: everything, as the reader sees it, and what of it is written for good, which a run keeps for a page.
#[derive(Debug, Default)]
pub struct Frame {
    pub shown: String,
    pub above: Piece,
}

/// The live drawing, with what it holds of each open subject and of the region it drew last.
pub struct Live {
    trees: Trees,
    /// The open subjects, in the order they opened: the first streams.
    order: Vec<Opened>,
    open: BTreeMap<Opened, Open>,
    /// The units the folds planned, and how many of them are settled.
    planned: usize,
    settled: usize,
    started: Instant,
    /// The rows the region took when last drawn, which the next frame moves up over.
    drawn: usize,
    /// The region as last drawn, so an unchanged one is not drawn again.
    region: Vec<String>,
    /// Whether something writes past the drawing: no region is drawn until a node closes again.
    quiet: bool,
    /// Whether the command has finished: no region is drawn again.
    finished: bool,
}

impl Live {
    /// A live drawing of a command that `started` then, told of nothing yet.
    pub fn new(started: Instant) -> Self {
        Self {
            trees: Trees::new(),
            order: Vec::new(),
            open: BTreeMap::new(),
            planned: 0,
            settled: 0,
            started,
            drawn: 0,
            region: Vec::new(),
            quiet: false,
            finished: false,
        }
    }

    /// The frame `event`, told at `now`, is drawn as on a terminal of `size`, its columns and rows.
    pub fn draw(&mut self, event: Event, now: Instant, size: (u16, u16)) -> Frame {
        match &event {
            Event::Planned { units } => self.planned += units,
            Event::Settled => self.settled += 1,
            _ => {}
        }
        let Some(tree) = event.tree() else {
            // Under no subject, what is said is written above at once.
            let above = self.trees.draw(event);
            return self.framed(above, now, size);
        };
        let opening = match &event {
            Event::Began { last, .. } => Some(*last),
            _ => None,
        };
        let ends = match &event {
            Event::Closed { .. } | Event::Tested { .. } => {
                self.open.get(&tree).is_some_and(|open| open.last)
            }
            Event::Stood { last, .. } => *last,
            Event::Summed { .. } | Event::Left { .. } => true,
            _ => false,
        };
        let drawn = self.trees.draw(event);
        // A tree named and nothing written of it is no open subject: what the cabinet held says nothing.
        if drawn.plain.is_empty() && !self.order.contains(&tree) {
            return self.framed(Piece::default(), now, size);
        }

        let open = self.opened(tree);
        open.held = mem::take(&mut open.held).then(drawn);
        open.running = opening.map(|_| Running {
            label: split(&open.held).1.plain,
            began: now,
        });
        if let Some(last) = opening {
            open.last = last;
        }
        let mut above = match self.order.first() == Some(&tree) {
            true => self.streamed(),
            false => Piece::default(),
        };
        if ends {
            above = above.then(self.ended(tree));
        }
        // A closing is the drawing's own again, whatever wrote past it while the node ran.
        self.quiet &= opening.is_some();

        self.framed(above, now, size)
    }

    /// The subject `tree`, opened where it was not: it streams or waits from its first written line.
    fn opened(&mut self, tree: Opened) -> &mut Open {
        if !self.order.contains(&tree) {
            self.order.push(tree);
        }
        self.open.entry(tree).or_default()
    }

    /// What the streaming subject holds up to its last line's end, which is written above; the rest, its open node's label, stays held.
    fn streamed(&mut self) -> Piece {
        let Some(open) = self.order.first().and_then(|tree| self.open.get_mut(tree)) else {
            return Piece::default();
        };
        let (settled, unterminated) = split(&open.held);
        open.held = unterminated;

        settled
    }

    /// `tree` ended: where it streams, what it holds is written, and each subject opened after it streams in turn, written whole where it ended while it waited.
    fn ended(&mut self, tree: Opened) -> Piece {
        if self.order.first() != Some(&tree) {
            if let Some(open) = self.open.get_mut(&tree) {
                open.ended = true;
                open.running = None;
            }
            return Piece::default();
        }
        let mut above = Piece::default();
        while let Some(next) = self.order.first().copied() {
            let Some(open) = self.open.get(&next) else {
                break;
            };
            if next != tree && !open.ended {
                above = above.then(self.streamed());
                break;
            }
            let held = self
                .open
                .remove(&next)
                .map(|open| open.held)
                .unwrap_or_default();
            above = above.then(held);
            self.order.remove(0);
        }

        above
    }

    /// A frame writing `above`, erasing the region before it and drawing it again after; nothing where neither changed.
    fn framed(&mut self, above: Piece, now: Instant, size: (u16, u16)) -> Frame {
        let region = match self.quiet || self.finished {
            true => Vec::new(),
            false => self.rows(now, size),
        };
        if above.plain.is_empty() && region == self.region {
            return Frame::default();
        }
        let mut shown = self.erased();
        shown.push_str(&above.shown);
        for row in &region {
            shown.push_str(&reader().dim(row));
            shown.push('\n');
        }
        self.drawn = region.len();
        self.region = region;

        Frame { shown, above }
    }

    /// What erases the region as last drawn: back to the start of its first row, and everything from there down.
    fn erased(&mut self) -> String {
        let erased = match self.drawn {
            0 => String::new(),
            rows => format!("\r\x1b[{rows}A\x1b[J"),
        };
        self.drawn = 0;
        self.region.clear();

        erased
    }

    /// The region at `now`, on a terminal of `size`.
    fn rows(&self, now: Instant, size: (u16, u16)) -> Vec<String> {
        if self.order.is_empty() {
            return Vec::new();
        }
        let (columns, rows) = (usize::from(size.0), usize::from(size.1));
        let tally = (self.planned > 0).then(|| {
            format!(
                "{} of {} units; {}",
                self.settled,
                self.planned,
                seconds(now.saturating_duration_since(self.started))
            )
        });
        // A third of the terminal, less the tally's row and the row that counts what does not fit.
        let room = (rows / 3).max(FEWEST_ROWS) - usize::from(tally.is_some()) - 1;
        let mut region = Vec::new();
        let mut shown = 0;
        for (at, tree) in self.order.iter().enumerate() {
            let open = &self.open[tree];
            let mut block = match at {
                // The streaming subject's settled lines stand above already.
                0 => Vec::new(),
                // The blank line that parts a tree from what stands above it costs the region a row it has none to spare for.
                _ => split(&open.held)
                    .0
                    .plain
                    .lines()
                    .filter(|line| !line.is_empty())
                    .map(str::to_string)
                    .collect(),
            };
            if let Some(running) = &open.running {
                block.push(format!(
                    "{}; {}",
                    running.label,
                    seconds(now.saturating_duration_since(running.began))
                ));
            }
            if shown > 0 && region.len() + block.len() > room {
                break;
            }
            region.extend(block);
            shown += 1;
        }
        let rest = self.order.len() - shown;
        if rest > 0 {
            region.push(format!("+ {rest} more"));
        }
        region.extend(tally);

        region.iter().map(|row| cut(row, columns)).collect()
    }

    /// Erase the region and draw none until a node closes again: something is about to write past the drawing — standard output, a program run, a child given a test's stream, a terminal taken.
    pub fn settle(&mut self) -> String {
        self.quiet = true;

        self.erased()
    }

    /// The region erased ahead of a write past the drawing, which then stands where the region stood until it is drawn again.
    pub fn clear(&mut self) -> String {
        self.erased()
    }

    /// What the region's clock draws at `now`: the region again where it changed or a running node's time moved, and nothing otherwise.
    pub fn tick(&mut self, now: Instant, size: (u16, u16)) -> String {
        self.framed(Piece::default(), now, size).shown
    }

    /// The line `tree`'s open node stands on, ended ahead of the first line its work writes: written above where `tree` streams, held where it waits.
    pub fn cut(&mut self, tree: Opened) -> Frame {
        let streams = self.order.first() == Some(&tree);
        let open = self.opened(tree);
        open.running = None;
        open.held = mem::take(&mut open.held).then("\n");
        let above = match streams {
            true => self.streamed(),
            false => Piece::default(),
        };
        let mut shown = self.erased();
        shown.push_str(&above.shown);

        Frame { shown, above }
    }

    /// The command finished: every subject still held is written above in the order they opened, its open line ended, and the region erased for good.
    pub fn finish(&mut self) -> Frame {
        let mut above = Piece::default();
        for tree in mem::take(&mut self.order) {
            let held = self
                .open
                .remove(&tree)
                .map(|open| open.held)
                .unwrap_or_default();
            above = above.then(held);
        }
        if !above.plain.is_empty() && !above.plain.ends_with('\n') {
            above = above.then("\n");
        }
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

/// `row` cut at its end to `columns`, `…` taking the last column where it is cut, and never inside a cluster.
pub fn cut(row: &str, columns: usize) -> String {
    let wide: usize = clusters(row).map(|(_, wide)| wide).sum();
    if wide <= columns {
        return row.to_string();
    }
    let mut kept = String::new();
    let mut taken = 0;
    for (cluster, wide) in clusters(row) {
        if taken + wide + 1 > columns {
            break;
        }
        kept.push_str(cluster);
        taken += wide;
    }
    kept.push('…');

    kept
}
