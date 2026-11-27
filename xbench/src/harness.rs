//! The harness a measurement names its figures through: each reading printed as a line of its record ([`read`]), the variants it compares turned round-robin after an untimed round ([`turn`]), and a phase of it run under the recorder inside a span of the harness's own ([`phase`]): the phase is read as its span's figures ([`Phase::read`]), and the spans and samples inside it by [`read_span`] and [`read_sample`]. A time, an allocation and a tally are read off the recorder's trace, and nowhere else.
//!
//! A measurement is a function of the bench's; what it prints is its record's readings, which the runner files, a process a group.

#[cfg(test)]
mod tests;

use {
    crate::{Line, Measure},
    curios_profile::{Destination, Profile, SampleSummary, Summary, Tally, fold, trace},
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

/// Print a reading of the figure `name`, counted in `measure`, at `size` where it is taken at one: a line of the record the runner files, or nothing in an untimed round.
pub fn read(name: &str, measure: Measure, size: Option<u64>, value: u64) {
    print(Line {
        name: name.to_string(),
        measure,
        size,
        value,
    });
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

/// What one phase of a measurement gave: what its work answered and the fold of the trace it recorded.
pub struct Phase<T> {
    pub answer: T,
    pub profile: Profile,
}

/// Run `work` as one phase of a measurement: inside a span of the harness's own, under the recorder, handed `tallies`, its stream kept in memory and folded once it ends.
pub fn phase<T>(tallies: &'static [Tally], work: impl FnOnce() -> T) -> Phase<T> {
    let stream = Stream::default();
    let answer = trace(
        Destination::Stream(Box::new(stream.clone())),
        tallies,
        || curios_tracing::profile!("phase" => work()),
    )
    .expect("a stream kept in memory opens");
    let profile = fold(stream.bytes().as_slice()).expect("the recorder's own stream folds");

    Phase { answer, profile }
}

impl<T> Phase<T> {
    /// Print the phase's figures, each named after `name`: what [`read_span`] reads of the span the phase opened.
    pub fn read(&self, name: &str, size: Option<u64>) {
        self.lines(name, size).into_iter().for_each(print);
    }

    /// The span the phase opened around its work, which [`Phase::read`] reads.
    pub fn span(&self) -> &Summary {
        self.profile
            .summaries
            .iter()
            // The one span this module opens.
            .find(|summary| summary.target == module_path!())
            .expect("a phase's span closes inside its own fold")
    }

    /// The lines [`Phase::read`] prints.
    fn lines(&self, name: &str, size: Option<u64>) -> Vec<Line> {
        span_lines(name, self.span(), size)
    }
}

/// Print a span's figures off a fold, each named after `name`: its time in microseconds, how many times it was entered, where the allocator counted, what its thread took, and what its work added to each tally its program handed the recorder — `<name>`, `<name>/calls`, `<name>/allocated`, `<name>/allocations` and `<name>/<tally>`, the looks among them.
pub fn read_span(name: &str, summary: &Summary, size: Option<u64>) {
    span_lines(name, summary, size).into_iter().for_each(print);
}

/// The lines [`read_span`] prints of `summary` under `name`.
fn span_lines(name: &str, summary: &Summary, size: Option<u64>) -> Vec<Line> {
    let line = |suffix: &str, measure, value| Line {
        name: format!("{name}{suffix}"),
        measure,
        size,
        value,
    };
    let mut lines = vec![
        line("", Measure::Microseconds, microseconds(summary.total)),
        line("/calls", Measure::Count, summary.calls),
    ];
    if let Some(memory) = summary.memory {
        lines.push(line("/allocated", Measure::Bytes, memory.allocated));
        lines.push(line(
            "/allocations",
            Measure::Allocations,
            memory.allocations,
        ));
    }
    for tallied in &summary.tallies {
        let suffix = format!("/{}", tallied.name);
        lines.push(line(&suffix, Measure::Count, tallied.added));
    }

    lines
}

/// Print a sample's figures off a fold, each named after `name`: the total of what it observed, and how many observations — `<name>` and `<name>/observations`.
pub fn read_sample(name: &str, sample: &SampleSummary, size: Option<u64>) {
    sample_lines(name, sample, size).into_iter().for_each(print);
}

/// The lines [`read_sample`] prints of `sample` under `name`.
fn sample_lines(name: &str, sample: &SampleSummary, size: Option<u64>) -> Vec<Line> {
    vec![
        Line {
            name: name.to_string(),
            measure: Measure::Count,
            size,
            value: sample.total,
        },
        Line {
            name: format!("{name}/observations"),
            measure: Measure::Count,
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
