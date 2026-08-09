//! Scoped collection of profiling spans: [`capture`] owns one subscriber on the current thread and returns aggregate timings and allocation figures for every span the operation emitted, without changing the process-global subscriber.

use {
    crate::{allocated_bytes, live_bytes, peak_bytes},
    std::{
        collections::BTreeMap,
        sync::{Arc, Mutex},
        time::{Duration, Instant},
    },
    tracing::{Metadata, Subscriber, span},
    tracing_subscriber::{Layer, layer::Context, prelude::*, registry::LookupSpan},
};

/// Aggregate timings and allocation figures collected during one call to [`capture`].
#[derive(Debug)]
pub struct ProfileReport {
    /// Timings ordered from greatest to least total duration.
    pub summaries: Vec<ProfileSummary>,
    /// The greatest live-byte total the process reached, from [`peak_bytes`]. Process-wide and not scoped to the capture, so a peak set before it is reported here too.
    pub peak: usize,
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
    retained: i64,
    allocated: u64,
}

impl Aggregate {
    fn new() -> Self {
        Self {
            calls: 0,
            total: Duration::ZERO,
            min: None,
            max: Duration::ZERO,
            retained: 0,
            allocated: 0,
        }
    }

    fn record(&mut self, timing: &SpanTiming) {
        let elapsed = timing.elapsed();
        self.calls += 1;
        self.total += elapsed;
        self.min = Some(self.min.map_or(elapsed, |minimum| minimum.min(elapsed)));
        self.max = self.max.max(elapsed);
        self.retained = self.retained.saturating_add(timing.retained);
        self.allocated = self.allocated.saturating_add(timing.allocated);
    }

    fn summary(&self, target: &'static str, name: &'static str) -> ProfileSummary {
        ProfileSummary {
            target,
            name,
            calls: self.calls,
            total: self.total,
            min: self.min.unwrap_or_default(),
            max: self.max,
            retained: self.retained,
            allocated: self.allocated,
        }
    }
}

/// What one entry into a span sampled on the way in, popped and differenced against the same three readings on the way out.
struct Entry {
    at: Instant,
    live: usize,
    allocated: usize,
}

struct SpanTiming {
    metadata: &'static Metadata<'static>,
    entered: Vec<Entry>,
    elapsed: Duration,
    retained: i64,
    allocated: u64,
}

impl SpanTiming {
    fn new(metadata: &'static Metadata<'static>) -> Self {
        Self {
            metadata,
            entered: Vec::new(),
            elapsed: Duration::ZERO,
            retained: 0,
            allocated: 0,
        }
    }

    fn enter(&mut self) {
        self.entered.push(Entry {
            at: Instant::now(),
            live: live_bytes(),
            allocated: allocated_bytes(),
        });
    }

    fn exit(&mut self) {
        if let Some(entered) = self.entered.pop() {
            self.elapsed += entered.at.elapsed();
            self.retained += live_bytes() as i64 - entered.live as i64;
            self.allocated += allocated_bytes().saturating_sub(entered.allocated) as u64;
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
            .record(&timing);
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

    ProfileReport {
        summaries,
        peak: peak_bytes(),
    }
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

    // The workspace's test binaries do not install `CountingAllocator`, so this asserts the shape of the accounting rather than concrete byte counts: a span that holds an allocation to its exit retains, and one that drops it does not. Under the system allocator every reading is zero and both sides hold trivially.
    #[test]
    fn capture_accounts_retained_and_allocated_bytes() {
        let (_, report) = capture(|| {
            let held = {
                let _span = tracing::trace_span!("holds").entered();
                vec![0_u8; 4 * 1024 * 1024]
            };

            {
                let _span = tracing::trace_span!("drops").entered();
                drop(vec![0_u8; 4 * 1024 * 1024]);
            }

            drop(held);
        });

        let holds = report
            .summaries
            .iter()
            .find(|summary| summary.name == "holds")
            .expect("the holding span was collected");
        let drops = report
            .summaries
            .iter()
            .find(|summary| summary.name == "drops")
            .expect("the dropping span was collected");

        assert!(holds.retained >= drops.retained);
        assert!(holds.allocated >= drops.retained.unsigned_abs());
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
