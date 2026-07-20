//! Scoped collection of Curios compiler profiling spans.
//!
//! Compiler crates emit spans but never install a subscriber. [`capture`] owns
//! one profiling session on the current thread, returning aggregate timings to
//! its caller without changing the process-global tracing subscriber.
//!
//! The `profile` feature fans out from `curios` through `curios-pipeline` to
//! every compiler crate: each crate exposes `profile = ["dep:tracing"]`, so
//! spans exist only in profiling builds. A measurement point is a function
//! attributed with `#[cfg_attr(feature = "profile", tracing::instrument(level
//! = "trace", skip_all))]`; without the feature the attribute compiles to
//! nothing. To break down the individual steps of a loop — where instrumenting
//! the stepped function would still aggregate all its calls — wrap each call
//! site with `curios_base::profile_span!` (see its documentation).
//!
//! Stage entrypoints and optimizer passes carry permanent spans. A span added
//! to isolate one investigation — attribute or `profile_span!` alike — is
//! temporary instrumentation, removed once the question is answered, never
//! left as a metrics API.

use {
    std::{
        collections::BTreeMap,
        sync::{Arc, Mutex},
        time::{Duration, Instant},
    },
    tracing::{Metadata, Subscriber, span},
    tracing_subscriber::{Layer, layer::Context, prelude::*, registry::LookupSpan},
};

/// Aggregate timings collected during one call to [`capture`].
#[derive(Debug, Default)]
pub struct ProfileReport {
    summaries: Vec<ProfileSummary>,
}

impl ProfileReport {
    /// Create a report from aggregate span summaries.
    pub fn new(summaries: Vec<ProfileSummary>) -> Self {
        Self { summaries }
    }

    /// Timings ordered from greatest to least total duration.
    pub fn summaries(&self) -> &[ProfileSummary] {
        &self.summaries
    }
}

/// Aggregate timing statistics for every span with the same target and name.
#[derive(Debug)]
pub struct ProfileSummary {
    target: &'static str,
    name: &'static str,
    calls: u64,
    total: Duration,
    min: Duration,
    max: Duration,
}

impl ProfileSummary {
    /// Create aggregate timing statistics for one span target and name.
    pub fn new(
        target: &'static str,
        name: &'static str,
        calls: u64,
        total: Duration,
        min: Duration,
        max: Duration,
    ) -> Self {
        Self {
            target,
            name,
            calls,
            total,
            min,
            max,
        }
    }

    /// The tracing target that owns the span.
    pub fn target(&self) -> &'static str {
        self.target
    }

    /// The static span name.
    pub fn name(&self) -> &'static str {
        self.name
    }

    /// Number of completed spans included in the aggregate.
    pub fn calls(&self) -> u64 {
        self.calls
    }

    /// Sum of the time for which the spans were entered.
    pub fn total(&self) -> Duration {
        self.total
    }

    /// Shortest completed span.
    pub fn min(&self) -> Duration {
        self.min
    }

    /// Longest completed span.
    pub fn max(&self) -> Duration {
        self.max
    }
}

/// Run `operation` with a profiling subscriber on the current thread and return
/// aggregate timings for every span it emits.
pub fn capture<T>(operation: impl FnOnce() -> T) -> (T, ProfileReport) {
    let aggregates = Arc::new(Mutex::new(BTreeMap::new()));
    let layer = ProfileLayer::new(Arc::clone(&aggregates));
    let subscriber = tracing_subscriber::registry().with(layer);
    let result = tracing::subscriber::with_default(subscriber, operation);
    let report = finish_report(&aggregates);

    (result, report)
}

type Key = (&'static str, &'static str);

type Aggregates = Arc<Mutex<BTreeMap<Key, Aggregate>>>;

struct Aggregate {
    calls: u64,
    total: Duration,
    min: Option<Duration>,
    max: Duration,
}

impl Aggregate {
    fn new() -> Self {
        Self {
            calls: 0,
            total: Duration::ZERO,
            min: None,
            max: Duration::ZERO,
        }
    }

    fn record(&mut self, elapsed: Duration) {
        self.calls += 1;
        self.total += elapsed;
        self.min = Some(self.min.map_or(elapsed, |minimum| minimum.min(elapsed)));
        self.max = self.max.max(elapsed);
    }

    fn summary(&self, target: &'static str, name: &'static str) -> ProfileSummary {
        ProfileSummary::new(
            target,
            name,
            self.calls,
            self.total,
            self.min.unwrap_or_default(),
            self.max,
        )
    }
}

struct SpanTiming {
    metadata: &'static Metadata<'static>,
    entered: Vec<Instant>,
    elapsed: Duration,
}

impl SpanTiming {
    fn new(metadata: &'static Metadata<'static>) -> Self {
        Self {
            metadata,
            entered: Vec::new(),
            elapsed: Duration::ZERO,
        }
    }

    fn enter(&mut self) {
        self.entered.push(Instant::now());
    }

    fn exit(&mut self) {
        if let Some(entered) = self.entered.pop() {
            self.elapsed += entered.elapsed();
        }
    }

    fn key(&self) -> Key {
        (self.metadata.target(), self.metadata.name())
    }

    fn elapsed(&self) -> Duration {
        self.elapsed
    }
}

struct ProfileLayer {
    aggregates: Aggregates,
}

impl ProfileLayer {
    fn new(aggregates: Aggregates) -> Self {
        Self { aggregates }
    }

    fn record(&self, timing: SpanTiming) {
        let mut aggregates = self
            .aggregates
            .lock()
            .expect("profiling aggregate lock poisoned");

        aggregates
            .entry(timing.key())
            .or_insert_with(Aggregate::new)
            .record(timing.elapsed());
    }
}

impl<S> Layer<S> for ProfileLayer
where
    S: Subscriber + for<'lookup> LookupSpan<'lookup>,
{
    fn on_new_span(
        &self,
        attributes: &span::Attributes<'_>,
        id: &span::Id,
        context: Context<'_, S>,
    ) {
        if let Some(span) = context.span(id) {
            span.extensions_mut()
                .insert(SpanTiming::new(attributes.metadata()));
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

fn finish_report(aggregates: &Aggregates) -> ProfileReport {
    let aggregates = aggregates
        .lock()
        .expect("profiling aggregate lock poisoned");

    let mut summaries = aggregates
        .iter()
        .map(|(&(target, name), aggregate)| aggregate.summary(target, name))
        .collect::<Vec<_>>();

    summaries.sort_by(|left, right| {
        right
            .total()
            .cmp(&left.total())
            .then_with(|| left.target().cmp(right.target()))
            .then_with(|| left.name().cmp(right.name()))
    });

    ProfileReport::new(summaries)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tracing::instrument(level = "trace", skip_all)]
    fn outer() {
        inner();
        inner();
    }

    #[tracing::instrument(level = "trace", skip_all)]
    fn inner() {}

    #[test]
    fn capture_aggregates_nested_profile_spans() {
        let (_, report) = capture(outer);

        let outer = report
            .summaries()
            .iter()
            .find(|summary| summary.name() == "outer")
            .expect("outer span was collected");
        let inner = report
            .summaries()
            .iter()
            .find(|summary| summary.name() == "inner")
            .expect("inner spans were collected");

        assert_eq!(outer.calls(), 1);
        assert_eq!(outer.target(), module_path!());
        assert_eq!(inner.calls(), 2);
    }
}
