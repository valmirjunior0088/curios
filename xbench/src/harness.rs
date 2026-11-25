//! The harness a measurement names its figures through: each reading printed as a line of its record ([`read`]), a time it took among them ([`read_elapsed`]), the variants it compares turned round-robin after an untimed round ([`turn`]), and a phase of it run under the recorder with its instructions counted ([`phase`]), whose spans and samples are read as figures by [`read_span`] and [`read_sample`].
//!
//! A measurement stays a test beside the code it measures; what it prints is its record's readings, which the runner files, an invocation of it a group.

#[cfg(test)]
mod tests;

use {
    crate::{Line, Unit},
    curios_profile::{
        Destination, Instructions, Profile, SampleSummary, Summary, Tally, fold, trace,
    },
    std::{
        cell::Cell,
        io::{self, Write},
        sync::{Arc, Mutex},
        time::Duration,
    },
};

thread_local! {
    /// Whether the round under way is the untimed one [`turn`] opens with, whose readings no record holds.
    static UNTIMED: Cell<bool> = const { Cell::new(false) };
}

/// Print a reading of the figure `name`, counted in `unit`, at `size` where it is taken at one: a line of the record the runner files, or nothing in an untimed round.
pub fn read(name: &str, unit: Unit, size: Option<u64>, value: u64) {
    print(Line {
        name: name.to_string(),
        unit,
        size,
        value,
    });
}

/// Print a reading of the figure `name`, the time `elapsed` in whole microseconds, at `size` where it is taken at one.
pub fn read_elapsed(name: &str, size: Option<u64>, elapsed: Duration) {
    read(name, Unit::Microseconds, size, microseconds(elapsed));
}

/// `elapsed` in whole microseconds, the unit a time is read in: what a microsecond holds past its last whole one is dropped, and a time past what `u64` holds reads as its largest.
fn microseconds(elapsed: Duration) -> u64 {
    u64::try_from(elapsed.as_micros()).unwrap_or(u64::MAX)
}

/// Print `line`, unless the round under way is untimed.
fn print(line: Line) {
    if !UNTIMED.with(Cell::get) {
        println!("{}", line.printed());
    }
}

/// Run each of `variants` once untimed, then `rounds` times round-robin, each round starting one variant later than the one before, so no variant always runs first: the order a group interleaves the variants it compares in.
pub fn turn(rounds: usize, variants: &mut [&mut dyn FnMut()]) {
    UNTIMED.with(|untimed| untimed.set(true));
    for variant in variants.iter_mut() {
        variant();
    }
    UNTIMED.with(|untimed| untimed.set(false));

    let count = variants.len();
    for round in 0..rounds {
        for place in 0..count {
            (variants[(round + place) % count])();
        }
    }
}

/// What one phase of a measurement gave: what its work answered, the fold of the trace it recorded, and the instructions it retired.
pub struct Phase<T> {
    pub answer: T,
    pub profile: Profile,
    /// The user-space instructions the phase's thread and every thread it started retired, the recorder's own work among them; `None` where the kernel opens no count, which is absent evidence, never nought.
    pub instructions: Option<u64>,
}

/// Run `work` as one phase of a measurement: under the recorder, handed `tallies`, its stream kept in memory and folded once it ends, and an instruction count opened before it and read after.
pub fn phase<T>(tallies: &'static [Tally], work: impl FnOnce() -> T) -> Phase<T> {
    let stream = Stream::default();
    let counted = Instructions::open();
    let answer = trace(Destination::Stream(Box::new(stream.clone())), tallies, work)
        .expect("a stream kept in memory opens");
    let instructions = counted.as_ref().map(Instructions::read);
    let profile = fold(stream.bytes().as_slice()).expect("the recorder's own stream folds");

    Phase {
        answer,
        profile,
        instructions,
    }
}

/// Print a span's figures off a fold, each named after the span: its time in microseconds, how many times it was entered, where the allocator counted, what its thread took, and what its work added to each tally its program handed the recorder — `<span>`, `<span>/calls`, `<span>/allocated`, `<span>/allocations` and `<span>/<tally>`, the looks among them.
pub fn read_span(summary: &Summary, size: Option<u64>) {
    span_lines(summary, size).into_iter().for_each(print);
}

/// The lines [`read_span`] prints of `summary`.
fn span_lines(summary: &Summary, size: Option<u64>) -> Vec<Line> {
    let name = &summary.name;
    let line = |suffix: &str, unit, value| Line {
        name: format!("{name}{suffix}"),
        unit,
        size,
        value,
    };
    let mut lines = vec![
        line("", Unit::Microseconds, microseconds(summary.total)),
        line("/calls", Unit::Count, summary.calls),
    ];
    if let Some(memory) = summary.memory {
        lines.push(line("/allocated", Unit::Bytes, memory.allocated));
        lines.push(line("/allocations", Unit::Allocations, memory.allocations));
    }
    for tallied in &summary.tallies {
        let suffix = format!("/{}", tallied.name);
        lines.push(line(&suffix, Unit::Count, tallied.added));
    }

    lines
}

/// Print a sample's figures off a fold, each named after the sample: the total of what it observed, and how many observations — `<sample>` and `<sample>/observations`.
pub fn read_sample(sample: &SampleSummary, size: Option<u64>) {
    sample_lines(sample, size).into_iter().for_each(print);
}

/// The lines [`read_sample`] prints of `sample`.
fn sample_lines(sample: &SampleSummary, size: Option<u64>) -> Vec<Line> {
    let name = &sample.name;

    vec![
        Line {
            name: name.clone(),
            unit: Unit::Count,
            size,
            value: sample.total,
        },
        Line {
            name: format!("{name}/observations"),
            unit: Unit::Count,
            size,
            value: sample.count,
        },
    ]
}

/// A destination that keeps a phase's stream in memory, so the phase folds it back without a file.
#[derive(Clone, Default)]
struct Stream(Arc<Mutex<Vec<u8>>>);

impl Stream {
    fn bytes(&self) -> Vec<u8> {
        self.0.lock().expect("the stream's lock").clone()
    }
}

impl Write for Stream {
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
        self.0
            .lock()
            .expect("the stream's lock")
            .extend_from_slice(bytes);

        Ok(bytes.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}
