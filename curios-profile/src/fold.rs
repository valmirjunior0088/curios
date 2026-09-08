//! Aggregation, as one consumer of the record stream rather than as what the library produces: [`fold`] reads the rows [`trace`](crate::trace()) wrote and recomputes the timings, allocation figures and magnitude distributions the old collector used to keep in memory.
//!
//! Every statistic here is derived, which is the point. A duration is an exit differenced against its entry, a retained byte count the same subtraction over the allocator's readings, and a sample distribution a pass over the `V` rows — so a question the columns below do not answer is asked of the file directly rather than by changing what a capture keeps.
//!
//! **A truncated stream folds.** Rotation discards the older file, so the surviving one can open in the middle of a span's life: an entry with no creation, an exit with no entry, a span that never closes. Each is taken for what it says and nothing is invented — an unpaired exit is ignored, a span with no creation is named by the callsite its id carries, and a span still entered when the rows run out is reported in [`ProfileReport::open`], which is what a killed run was inside.
//!
//! To fold a rotated pair, hand the `.prev` file's rows and then the current file's to one [`fold`]: the callsite table is restated at the head of each, so concatenating them is well defined.

use std::{
    collections::BTreeMap,
    io::{self, BufRead},
    time::Duration,
};

/// Timings, allocation figures and magnitude samples recomputed from one record stream.
#[derive(Debug, Default)]
pub struct ProfileReport {
    /// Timings ordered from greatest to least total duration.
    pub summaries: Vec<ProfileSummary>,
    /// Sampled magnitudes ordered from greatest to least total, one entry per [`sample!`](crate::sample) site that fired.
    pub samples: Vec<SampleSummary>,
    /// The greatest live-byte total any row reported, which is the process's high-water mark up to the last row the file holds.
    pub peak: usize,
    /// The spans still entered when the rows ran out, outermost first.
    ///
    /// Empty for a stream whose operation returned. Non-empty is the useful case: it is the stack a compilation was inside when it was killed, and the reason a record stream is worth keeping over an aggregate that only exists once everything has closed.
    pub open: Vec<OpenSpan>,
}

/// A span that was entered and never left before the rows ended.
#[derive(Debug)]
pub struct OpenSpan {
    /// The tracing target that owns the span.
    pub target: String,
    /// The static span name.
    pub name: String,
    /// The group the span declared, for one carrying a `group` field.
    pub group: Option<String>,
    /// How deep the span was entered — above one for a span re-entered within itself.
    pub depth: usize,
    /// When it was entered, as nanoseconds since the stream opened.
    pub entered: u128,
}

/// The distribution of one [`sample!`](crate::sample) site's observations.
///
/// Time and memory say an operation is expensive; these say *what it was given*. The pair is what distinguishes an operation that is individually wasteful from one that is being handed inputs it should never have seen — a distinction neither duration nor byte count can make alone.
#[derive(Debug)]
pub struct SampleSummary {
    /// The module that recorded the observations.
    pub target: String,
    /// The sample site's name.
    pub name: String,
    /// How many observations were recorded.
    pub count: u64,
    /// Their sum, which is the total magnitude the site saw.
    pub total: u64,
    /// The smallest observation.
    pub min: u64,
    /// The largest observation. Read against [`mean`](Self::mean): a mean near the min with a far larger max is a tail, and a mean that tracks the max is uniform growth.
    pub max: u64,
}

impl SampleSummary {
    /// The mean observation, or zero when nothing was recorded.
    pub fn mean(&self) -> f64 {
        match self.count {
            0 => 0.0,
            count => self.total as f64 / count as f64,
        }
    }
}

/// Aggregate statistics for every span with the same target, name and group.
///
/// Every figure counts nested spans within the span's extent, exactly as [`total`](Self::total) does: an outer stage's [`retained`](Self::retained) includes what the passes inside it retained. The allocation columns are differences of the readings the rows carry, so they are all zero unless the binary that wrote the stream installed [`CountingAllocator`](crate::CountingAllocator).
#[derive(Debug)]
pub struct ProfileSummary {
    /// The tracing target that owns the span.
    pub target: String,
    /// The static span name.
    pub name: String,
    /// The group this row aggregates, for a span that declared one — the value of its `group` field. `None` for every span that declared none.
    pub group: Option<String>,
    /// Number of closed spans included in the aggregate.
    pub calls: u64,
    /// Sum of the time for which the spans were entered.
    pub total: Duration,
    /// Shortest closed span.
    pub min: Duration,
    /// Longest closed span.
    pub max: Duration,
    /// Net bytes still held at exit: what the spans took minus what they returned. Negative where they freed more than they took, which is how a pass that consumes a representation reports itself.
    pub retained: i64,
    /// Bytes the spans took while entered, whether or not later returned. A pass that allocates heavily and frees as it goes shows large `allocated` beside near-zero [`retained`](Self::retained).
    pub allocated: u64,
    /// Trips through the allocator while the spans were entered. Divided into [`allocated`](Self::allocated) it gives the average request size, which is what separates a pass that wants fewer allocations from one that wants a smaller structure.
    pub allocations: u64,
}

/// Recompute a report from the rows of one record stream.
///
/// The error is the reader's alone. A row this version does not understand is skipped rather than refused, because a stream is written by a process that may have been killed mid-row and a partial last line is the normal ending, not a corruption.
pub fn fold(rows: impl BufRead) -> io::Result<ProfileReport> {
    let mut state = Fold::default();
    for row in rows.lines() {
        state.row(&row?);
    }

    Ok(state.finish())
}

impl ProfileReport {
    /// The report as tab-separated tables: the spans, the magnitude sites beneath them, and — for a stream whose run never returned — the stack it was inside.
    ///
    /// One renderer, because there were two. The CLI and the prelude build script each formatted these columns by hand and had already drifted apart in which ones they printed, which is the drift a second implementation of one format buys.
    pub fn render(&self) -> String {
        let mut rendered = format!(
            "total_ms\tcalls\tmin_ms\tmax_ms\tretained_mb\tallocated_mb\tallocs\ttarget\tname\tgroup\t(peak {:.1} MiB)\n",
            self.peak as f64 / (1024.0 * 1024.0),
        );
        for summary in &self.summaries {
            rendered.push_str(&format!(
                "{:.3}\t{}\t{:.3}\t{:.3}\t{:.1}\t{:.1}\t{}\t{}\t{}\t{}\n",
                summary.total.as_secs_f64() * 1_000.0,
                summary.calls,
                summary.min.as_secs_f64() * 1_000.0,
                summary.max.as_secs_f64() * 1_000.0,
                summary.retained as f64 / (1024.0 * 1024.0),
                summary.allocated as f64 / (1024.0 * 1024.0),
                summary.allocations,
                summary.target,
                summary.name,
                summary.group.as_deref().unwrap_or(""),
            ));
        }

        if !self.samples.is_empty() {
            rendered.push_str("\ncount\ttotal\tmin\tmean\tmax\ttarget\tname\n");
            for sample in &self.samples {
                rendered.push_str(&format!(
                    "{}\t{}\t{}\t{:.1}\t{}\t{}\t{}\n",
                    sample.count,
                    sample.total,
                    sample.min,
                    sample.mean(),
                    sample.max,
                    sample.target,
                    sample.name,
                ));
            }
        }

        if !self.open.is_empty() {
            rendered.push_str("\nentered_ms\tdepth\ttarget\tname\tgroup\t(still open)\n");
            for span in &self.open {
                rendered.push_str(&format!(
                    "{:.3}\t{}\t{}\t{}\t{}\n",
                    span.entered as f64 / 1_000_000.0,
                    span.depth,
                    span.target,
                    span.name,
                    span.group.as_deref().unwrap_or(""),
                ));
            }
        }

        rendered
    }
}

/// One span's identity and what its entries have accumulated so far.
#[derive(Default)]
struct Open {
    callsite: u32,
    group: Option<String>,
    /// One reading set per entry not yet matched by an exit, so a span re-entered within itself is measured over each extent rather than the outermost.
    entered: Vec<Readings>,
    elapsed: u128,
    retained: i64,
    allocated: u64,
    allocations: u64,
}

#[derive(Clone, Copy, Default)]
struct Readings {
    at: u128,
    live: u64,
    allocated: u64,
    allocations: u64,
    /// The process's high-water mark as of this row, which is a reading of its own rather than the greatest `live` any row happened to catch.
    peak: u64,
}

#[derive(Default)]
struct Aggregate {
    calls: u64,
    total: u128,
    min: Option<u128>,
    max: u128,
    retained: i64,
    allocated: u64,
    allocations: u64,
}

#[derive(Default)]
struct Distribution {
    count: u64,
    total: u64,
    min: Option<u64>,
    max: u64,
}

#[derive(Default)]
struct Fold {
    callsites: BTreeMap<u32, (String, String)>,
    open: BTreeMap<u64, Open>,
    spans: BTreeMap<(String, String, Option<String>), Aggregate>,
    samples: BTreeMap<(String, String), Distribution>,
    peak: u64,
}

impl Fold {
    fn named(&self, callsite: u32) -> (String, String) {
        self.callsites
            .get(&callsite)
            .cloned()
            .unwrap_or_else(|| (String::new(), format!("callsite {callsite}")))
    }

    fn row(&mut self, row: &str) {
        let mut columns = row.split('\t');
        let Some(kind) = columns.next() else {
            return;
        };

        match kind {
            "D" => self.define(columns),
            "S" => self.create(columns),
            "E" => self.enter(columns),
            "X" => self.exit(columns),
            "C" => self.close(columns),
            "V" => self.event(columns),
            // "H" opens a file and "R" records a value onto a live span; neither bears on an aggregate, and an unknown kind is a newer writer's row.
            _ => {}
        }
    }

    fn define<'row>(&mut self, mut columns: impl Iterator<Item = &'row str>) {
        let (Some(callsite), Some(target), Some(name)) =
            (columns.next(), columns.next(), columns.next())
        else {
            return;
        };
        let Ok(callsite) = callsite.parse() else {
            return;
        };

        self.callsites
            .insert(callsite, (unescape(target), unescape(name)));
    }

    fn create<'row>(&mut self, mut columns: impl Iterator<Item = &'row str>) {
        let (Some(id), Some(callsite), Some(_at)) =
            (columns.next(), columns.next(), columns.next())
        else {
            return;
        };
        let (Ok(id), Ok(callsite)) = (id.parse(), callsite.parse()) else {
            return;
        };

        let group = field(columns, "group");
        let span = self.open.entry(id).or_default();
        span.callsite = callsite;
        span.group = group;
    }

    fn enter<'row>(&mut self, columns: impl Iterator<Item = &'row str>) {
        let Some((id, callsite, readings)) = boundary(columns) else {
            return;
        };

        self.peak = self.peak.max(readings.peak);
        let span = self.open.entry(id).or_default();
        // A stream that opened mid-life has no creation row for this span; the id still names its callsite.
        span.callsite = callsite;
        span.entered.push(readings);
    }

    fn exit<'row>(&mut self, columns: impl Iterator<Item = &'row str>) {
        let Some((id, _callsite, readings)) = boundary(columns) else {
            return;
        };

        self.peak = self.peak.max(readings.peak);
        let Some(span) = self.open.get_mut(&id) else {
            return;
        };
        let Some(entry) = span.entered.pop() else {
            return;
        };

        span.elapsed += readings.at.saturating_sub(entry.at);
        span.retained += readings.live as i64 - entry.live as i64;
        span.allocated += readings.allocated.saturating_sub(entry.allocated);
        span.allocations += readings.allocations.saturating_sub(entry.allocations);
    }

    fn close<'row>(&mut self, mut columns: impl Iterator<Item = &'row str>) {
        let Some(id) = columns.next().and_then(|id| id.parse().ok()) else {
            return;
        };
        let Some(span) = self.open.remove(&id) else {
            return;
        };

        let (target, name) = self.named(span.callsite);
        let aggregate = self.spans.entry((target, name, span.group)).or_default();
        aggregate.calls += 1;
        aggregate.total += span.elapsed;
        aggregate.min = Some(
            aggregate
                .min
                .map_or(span.elapsed, |least| least.min(span.elapsed)),
        );
        aggregate.max = aggregate.max.max(span.elapsed);
        aggregate.retained = aggregate.retained.saturating_add(span.retained);
        aggregate.allocated = aggregate.allocated.saturating_add(span.allocated);
        aggregate.allocations = aggregate.allocations.saturating_add(span.allocations);
    }

    fn event<'row>(&mut self, mut columns: impl Iterator<Item = &'row str>) {
        let (Some(callsite), Some(_at)) = (columns.next(), columns.next()) else {
            return;
        };
        let Ok(callsite) = callsite.parse() else {
            return;
        };
        let Some(value) = field(columns, "value") else {
            return;
        };
        // A magnitude is unsigned; a site that reported a negative one is read as having reported none of it, exactly as the collector this replaced did.
        let value = value
            .parse::<i64>()
            .map(|value| value.max(0) as u64)
            .or_else(|_| value.parse::<u64>());
        let Ok(value) = value else {
            return;
        };

        let (target, name) = self.named(callsite);
        let distribution = self.samples.entry((target, name)).or_default();
        distribution.count += 1;
        distribution.total = distribution.total.saturating_add(value);
        distribution.min = Some(distribution.min.map_or(value, |least| least.min(value)));
        distribution.max = distribution.max.max(value);
    }

    fn finish(self) -> ProfileReport {
        let mut summaries = self
            .spans
            .iter()
            .map(|((target, name, group), aggregate)| ProfileSummary {
                target: target.clone(),
                name: name.clone(),
                group: group.clone(),
                calls: aggregate.calls,
                total: nanos(aggregate.total),
                min: nanos(aggregate.min.unwrap_or_default()),
                max: nanos(aggregate.max),
                retained: aggregate.retained,
                allocated: aggregate.allocated,
                allocations: aggregate.allocations,
            })
            .collect::<Vec<_>>();

        summaries.sort_by(|left, right| {
            right
                .total
                .cmp(&left.total)
                .then_with(|| left.target.cmp(&right.target))
                .then_with(|| left.name.cmp(&right.name))
                .then_with(|| left.group.cmp(&right.group))
        });

        let mut samples = self
            .samples
            .iter()
            .map(|((target, name), distribution)| SampleSummary {
                target: target.clone(),
                name: name.clone(),
                count: distribution.count,
                total: distribution.total,
                min: distribution.min.unwrap_or_default(),
                max: distribution.max,
            })
            .collect::<Vec<_>>();

        samples.sort_by(|left, right| {
            right
                .total
                .cmp(&left.total)
                .then_with(|| left.target.cmp(&right.target))
                .then_with(|| left.name.cmp(&right.name))
        });

        let mut open = self
            .open
            .values()
            .filter_map(|span| {
                let entered = span.entered.first()?;
                let (target, name) = self.named(span.callsite);

                Some(OpenSpan {
                    target,
                    name,
                    group: span.group.clone(),
                    depth: span.entered.len(),
                    entered: entered.at,
                })
            })
            .collect::<Vec<_>>();

        // Outermost first: the span entered earliest is the one the others are nested inside.
        open.sort_by_key(|span| span.entered);

        ProfileReport {
            summaries,
            samples,
            peak: self.peak as usize,
            open,
        }
    }
}

/// The identity and the four readings an `E` or `X` row carries.
fn boundary<'row>(mut columns: impl Iterator<Item = &'row str>) -> Option<(u64, u32, Readings)> {
    let id = columns.next()?.parse().ok()?;
    let callsite = columns.next()?.parse().ok()?;
    let at = columns.next()?.parse().ok()?;
    let live = columns.next()?.parse().ok()?;
    let allocated = columns.next()?.parse().ok()?;
    let allocations = columns.next()?.parse().ok()?;
    let peak = columns.next()?.parse().ok()?;

    Some((
        id,
        callsite,
        Readings {
            at,
            live,
            allocated,
            allocations,
            peak,
        },
    ))
}

/// The value of one `name=value` field among a row's tail, unescaped.
fn field<'row>(columns: impl Iterator<Item = &'row str>, wanted: &str) -> Option<String> {
    columns
        .filter_map(|pair| pair.split_once('='))
        .find(|(name, _)| *name == wanted)
        .map(|(_, value)| unescape(value))
}

fn nanos(value: u128) -> Duration {
    Duration::from_nanos(value.min(u128::from(u64::MAX)) as u64)
}

/// The inverse of the writer's escaping. A trailing backslash is a row cut short by a kill, and is dropped.
fn unescape(value: &str) -> String {
    let mut unescaped = String::with_capacity(value.len());
    let mut characters = value.chars();
    while let Some(character) = characters.next() {
        match character {
            '\\' => match characters.next() {
                Some('t') => unescaped.push('\t'),
                Some('n') => unescaped.push('\n'),
                Some('r') => unescaped.push('\r'),
                Some('\\') => unescaped.push('\\'),
                Some(other) => unescaped.push(other),
                None => {}
            },
            _ => unescaped.push(character),
        }
    }

    unescaped
}

#[cfg(test)]
mod tests;
