//! Scoped collection of profiling spans: [`capture`] owns one subscriber on the current thread and returns aggregate timings for every span the operation emitted, without changing the process-global subscriber.

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
#[derive(Debug)]
pub struct ProfileReport {
    /// Timings ordered from greatest to least total duration.
    pub summaries: Vec<ProfileSummary>,
}

/// Aggregate timing statistics for every span with the same target and name.
#[derive(Debug)]
pub struct ProfileSummary {
    /// The tracing target that owns the span.
    pub target: &'static str,
    /// The static span name.
    pub name: &'static str,
    /// Number of completed spans included in the aggregate.
    pub calls: u64,
    /// Sum of the time for which the spans were entered.
    pub total: Duration,
    /// Shortest completed span.
    pub min: Duration,
    /// Longest completed span.
    pub max: Duration,
}

/// Run `operation` with a profiling subscriber on the current thread and return aggregate timings for every span it emits.
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
        ProfileSummary {
            target,
            name,
            calls: self.calls,
            total: self.total,
            min: self.min.unwrap_or_default(),
            max: self.max,
        }
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
            .total
            .cmp(&left.total)
            .then_with(|| left.target.cmp(right.target))
            .then_with(|| left.name.cmp(right.name))
    });

    ProfileReport { summaries }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn outer() {
        let _span = tracing::trace_span!("outer").entered();
        inner();
        inner();
    }

    fn inner() {
        let _span = tracing::trace_span!("inner").entered();
    }

    #[test]
    fn capture_aggregates_nested_profile_spans() {
        let (_, report) = capture(outer);

        let outer = report
            .summaries
            .iter()
            .find(|summary| summary.name == "outer")
            .expect("outer span was collected");
        let inner = report
            .summaries
            .iter()
            .find(|summary| summary.name == "inner")
            .expect("inner spans were collected");

        assert_eq!(outer.calls, 1);
        assert_eq!(outer.target, module_path!());
        assert_eq!(inner.calls, 2);
    }
}
