//! Scoped collection of profiling spans: [`capture`] owns one subscriber on the current thread and returns aggregate timings and allocation figures for every span the operation emitted, without changing the process-global subscriber.

use {
    crate::{allocated_bytes, allocation_count, live_bytes, peak_bytes},
    std::{
        collections::BTreeMap,
        fmt,
        sync::{Arc, Mutex},
        time::{Duration, Instant},
    },
    tracing::{
        Event, Metadata, Subscriber,
        field::{Field, Visit},
        span,
    },
    tracing_subscriber::{Layer, layer::Context, prelude::*, registry::LookupSpan},
};

/// Aggregate timings, allocation figures, and magnitude samples collected during one call to [`capture`].
#[derive(Debug)]
pub struct ProfileReport {
    /// Timings ordered from greatest to least total duration.
    pub summaries: Vec<ProfileSummary>,
    /// Sampled magnitudes ordered from greatest to least total, one entry per [`sample!`](crate::sample) site that fired.
    pub samples: Vec<SampleSummary>,
    /// The greatest live-byte total the process reached, from [`peak_bytes`]. Process-wide and not scoped to the capture, so a peak set before it is reported here too.
    pub peak: usize,
}

/// The distribution of one [`sample!`](crate::sample) site's observations.
///
/// Time and memory say an operation is expensive; these say *what it was given*. The pair is what distinguishes an operation that is individually wasteful from one that is being handed inputs it should never have seen — a distinction neither duration nor byte count can make alone.
#[derive(Debug)]
pub struct SampleSummary {
    /// The module that recorded the observations.
    pub target: &'static str,
    /// The sample site's name.
    pub name: &'static str,
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

/// Aggregate statistics for every span with the same target and name.
///
/// Every figure counts nested spans within the span's extent, exactly as [`total`](Self::total) does: an outer stage's [`retained`](Self::retained) includes what the passes inside it retained. The allocation columns read [`live_bytes`] and [`allocated_bytes`], so they are all zero unless the binary installed [`CountingAllocator`](crate::CountingAllocator).
#[derive(Debug)]
pub struct ProfileSummary {
    /// The tracing target that owns the span.
    pub target: &'static str,
    /// The static span name.
    pub name: &'static str,
    /// The group this row aggregates, for a span that declared one — the value of its `group` field. `None` for every ungrouped span, which is all of them unless [`profile_group!`](crate::profile_group) was used.
    pub group: Option<Arc<str>>,
    /// Number of completed spans included in the aggregate.
    pub calls: u64,
    /// Sum of the time for which the spans were entered.
    pub total: Duration,
    /// Shortest completed span.
    pub min: Duration,
    /// Longest completed span.
    pub max: Duration,
    /// Net bytes still held at exit: what the spans took minus what they returned. Negative where they freed more than they took, which is how a pass that consumes a representation reports itself.
    pub retained: i64,
    /// Bytes the spans took while entered, whether or not later returned. A pass that allocates heavily and frees as it goes shows large `allocated` beside near-zero [`retained`](Self::retained).
    pub allocated: u64,
    /// Trips through the allocator while the spans were entered. Divided into [`allocated`](Self::allocated) it gives the average request size, which is what separates a pass that wants fewer allocations from one that wants a smaller structure.
    pub allocations: u64,
}

/// Run `operation` with a profiling subscriber on the current thread and return aggregate timings for every span it emits.
pub fn capture<T>(operation: impl FnOnce() -> T) -> (T, ProfileReport) {
    let aggregates = Arc::new(Mutex::new(BTreeMap::new()));
    let samples = Arc::new(Mutex::new(BTreeMap::new()));
    let layer = ProfileLayer::new(Arc::clone(&aggregates), Arc::clone(&samples));
    let subscriber = tracing_subscriber::registry().with(layer);
    let result = tracing::subscriber::with_default(subscriber, operation);
    let report = finish_report(&aggregates, &samples);

    (result, report)
}

/// What a span aggregates under: its target, its static name, and — for a span carrying one — the group it declared.
type SpanKey = (&'static str, &'static str, Option<Arc<str>>);

/// What an event aggregates under. Deliberately not grouped: an event carries a magnitude, and grouping one is a second design rather than the same one.
type SampleKey = (&'static str, &'static str);

type Aggregates = Arc<Mutex<BTreeMap<SpanKey, Aggregate>>>;

type Samples = Arc<Mutex<BTreeMap<SampleKey, SampleAggregate>>>;

#[derive(Default)]
struct SampleAggregate {
    count: u64,
    total: u64,
    min: Option<u64>,
    max: u64,
}

impl SampleAggregate {
    fn record(&mut self, value: u64) {
        self.count += 1;
        self.total = self.total.saturating_add(value);
        self.min = Some(self.min.map_or(value, |minimum| minimum.min(value)));
        self.max = self.max.max(value);
    }

    fn summary(&self, target: &'static str, name: &'static str) -> SampleSummary {
        SampleSummary {
            target,
            name,
            count: self.count,
            total: self.total,
            min: self.min.unwrap_or_default(),
            max: self.max,
        }
    }
}

/// Reads the `group` field off a span's creation attributes, ignoring anything else it carries.
///
/// **Both arms are load-bearing.** A group written as `group = %name` is a `Display` value, and `tracing` records those through [`Visit::record_debug`] rather than [`Visit::record_str`] — so a visitor implementing only the latter compiles, runs, and silently captures nothing. That is exactly how [`SampleValue`] below reads its own field and why its `record_debug` is a deliberate no-op rather than an oversight.
#[derive(Default)]
struct GroupValue(Option<Arc<str>>);

impl GroupValue {
    const FIELD: &'static str = "group";
}

impl Visit for GroupValue {
    fn record_str(&mut self, field: &Field, value: &str) {
        if field.name() == Self::FIELD {
            self.0 = Some(Arc::from(value));
        }
    }

    fn record_debug(&mut self, field: &Field, value: &dyn fmt::Debug) {
        // Formatted only on a name match: this runs at every span creation, and a group is a string allocation per span.
        if field.name() == Self::FIELD {
            self.0 = Some(Arc::from(format!("{value:?}").as_str()));
        }
    }
}

/// Reads the `value` field off a [`sample!`](crate::sample) event, ignoring anything else the event carries.
#[derive(Default)]
struct SampleValue(Option<u64>);

impl Visit for SampleValue {
    fn record_u64(&mut self, field: &Field, value: u64) {
        if field.name() == "value" {
            self.0 = Some(value);
        }
    }

    fn record_i64(&mut self, field: &Field, value: i64) {
        if field.name() == "value" {
            self.0 = Some(value.max(0) as u64);
        }
    }

    fn record_debug(&mut self, _: &Field, _: &dyn fmt::Debug) {}
}

#[derive(Default)]
struct Aggregate {
    calls: u64,
    total: Duration,
    min: Option<Duration>,
    max: Duration,
    retained: i64,
    allocated: u64,
    allocations: u64,
}

impl Aggregate {
    fn record(&mut self, timing: &SpanTiming) {
        let elapsed = timing.elapsed();
        self.calls += 1;
        self.total += elapsed;
        self.min = Some(self.min.map_or(elapsed, |minimum| minimum.min(elapsed)));
        self.max = self.max.max(elapsed);
        self.retained = self.retained.saturating_add(timing.retained);
        self.allocated = self.allocated.saturating_add(timing.allocated);
        self.allocations = self.allocations.saturating_add(timing.allocations);
    }

    fn summary(
        &self,
        target: &'static str,
        name: &'static str,
        group: Option<Arc<str>>,
    ) -> ProfileSummary {
        ProfileSummary {
            target,
            name,
            group,
            calls: self.calls,
            total: self.total,
            min: self.min.unwrap_or_default(),
            max: self.max,
            retained: self.retained,
            allocated: self.allocated,
            allocations: self.allocations,
        }
    }
}

/// What one entry into a span sampled on the way in, popped and differenced against the same three readings on the way out.
struct Entry {
    at: Instant,
    live: usize,
    allocated: usize,
    allocations: usize,
}

struct SpanTiming {
    metadata: &'static Metadata<'static>,
    group: Option<Arc<str>>,
    entered: Vec<Entry>,
    elapsed: Duration,
    retained: i64,
    allocated: u64,
    allocations: u64,
}

impl SpanTiming {
    fn new(metadata: &'static Metadata<'static>, group: Option<Arc<str>>) -> Self {
        Self {
            metadata,
            group,
            entered: Vec::new(),
            elapsed: Duration::ZERO,
            retained: 0,
            allocated: 0,
            allocations: 0,
        }
    }

    fn enter(&mut self) {
        self.entered.push(Entry {
            at: Instant::now(),
            live: live_bytes(),
            allocated: allocated_bytes(),
            allocations: allocation_count(),
        });
    }

    fn exit(&mut self) {
        if let Some(entered) = self.entered.pop() {
            self.elapsed += entered.at.elapsed();
            self.retained += live_bytes() as i64 - entered.live as i64;
            self.allocated += allocated_bytes().saturating_sub(entered.allocated) as u64;
            self.allocations += allocation_count().saturating_sub(entered.allocations) as u64;
        }
    }

    fn key(&self) -> SpanKey {
        (
            self.metadata.target(),
            self.metadata.name(),
            self.group.clone(),
        )
    }

    fn elapsed(&self) -> Duration {
        self.elapsed
    }
}

struct ProfileLayer {
    aggregates: Aggregates,
    samples: Samples,
}

impl ProfileLayer {
    fn new(aggregates: Aggregates, samples: Samples) -> Self {
        Self {
            aggregates,
            samples,
        }
    }

    fn record(&self, timing: SpanTiming) {
        let mut aggregates = self
            .aggregates
            .lock()
            .expect("profiling aggregate lock poisoned");

        aggregates.entry(timing.key()).or_default().record(&timing);
    }
}

impl<S> Layer<S> for ProfileLayer
where
    S: Subscriber + for<'lookup> LookupSpan<'lookup>,
{
    fn on_event(&self, event: &Event<'_>, _context: Context<'_, S>) {
        let mut visitor = SampleValue::default();
        event.record(&mut visitor);
        let Some(value) = visitor.0 else {
            return;
        };

        let metadata = event.metadata();
        self.samples
            .lock()
            .expect("profiling sample lock poisoned")
            .entry((metadata.target(), metadata.name()))
            .or_default()
            .record(value);
    }

    fn on_new_span(
        &self,
        attributes: &span::Attributes<'_>,
        id: &span::Id,
        context: Context<'_, S>,
    ) {
        if let Some(span) = context.span(id) {
            let mut group = GroupValue::default();
            attributes.record(&mut group);
            span.extensions_mut()
                .insert(SpanTiming::new(attributes.metadata(), group.0));
        }
    }

    fn on_enter(&self, id: &span::Id, context: Context<'_, S>) {
        if let Some(span) = context.span(id)
            && let Some(timing) = span.extensions_mut().get_mut::<SpanTiming>()
        {
            timing.enter();
        }
    }

    fn on_exit(&self, id: &span::Id, context: Context<'_, S>) {
        if let Some(span) = context.span(id)
            && let Some(timing) = span.extensions_mut().get_mut::<SpanTiming>()
        {
            timing.exit();
        }
    }

    fn on_close(&self, id: span::Id, context: Context<'_, S>) {
        let Some(span) = context.span(&id) else {
            return;
        };

        let Some(timing) = span.extensions_mut().remove::<SpanTiming>() else {
            return;
        };

        self.record(timing);
    }
}

fn finish_report(aggregates: &Aggregates, samples: &Samples) -> ProfileReport {
    let mut samples = samples
        .lock()
        .expect("profiling sample lock poisoned")
        .iter()
        .map(|(&(target, name), aggregate)| aggregate.summary(target, name))
        .collect::<Vec<_>>();

    samples.sort_by(|left, right| {
        right
            .total
            .cmp(&left.total)
            .then_with(|| left.target.cmp(right.target))
            .then_with(|| left.name.cmp(right.name))
    });

    let aggregates = aggregates
        .lock()
        .expect("profiling aggregate lock poisoned");

    let mut summaries = aggregates
        .iter()
        .map(|((target, name, group), aggregate)| aggregate.summary(target, name, group.clone()))
        .collect::<Vec<_>>();

    summaries.sort_by(|left, right| {
        right
            .total
            .cmp(&left.total)
            .then_with(|| left.target.cmp(right.target))
            .then_with(|| left.name.cmp(right.name))
            .then_with(|| left.group.cmp(&right.group))
    });

    ProfileReport {
        summaries,
        samples,
        peak: peak_bytes(),
    }
}

#[cfg(test)]
mod tests;
