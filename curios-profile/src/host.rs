//! Scoped capture of the records a host library emits through the `log` facade: [`capture_host_records`] bridges `log` into `tracing` for the duration of one closure and returns the messages that matched one target, leaving the `log` side disabled afterwards.
//!
//! This exists because the workspace's own instrumentation is `tracing` spans, but the engine underneath the runtime — wasmtime — reports through `log`: each collection announces itself at trace level under `wasmtime::runtime::store::gc`, and counting those announcements is what decomposes a workload's wall clock into its birth path and its collection work. The bridge is scoped the way [`capture`](crate::capture) is scoped, within the one constraint the `log` facade imposes: it admits exactly one process-global logger, so `LogTracer` is installed lazily and permanently, but `log`'s max level stays `Off` except inside a capture and the collecting subscriber is thread-local. A build that never captures therefore pays one relaxed atomic load per suppressed record, and a concurrent thread's records during someone else's capture go to that thread's own (absent) subscriber rather than into the collection.
//!
//! Like [`capture`](crate::capture), this is single-consumer by design: two overlapping captures would fight over the max level, and a measurement is specified at its call site, not configured from outside.

use {
    std::sync::{Arc, Mutex, Once},
    tracing::{
        Event, Subscriber,
        field::{Field, Visit},
    },
    tracing_log::{
        LogTracer, NormalizeEvent,
        log::{LevelFilter, set_max_level},
    },
    tracing_subscriber::{Layer, layer::Context, prelude::*, registry::LookupSpan},
};

/// Run `operation` with the `log`-to-`tracing` bridge raised to trace level and return the message of every record whose target equals `target`, in emission order.
///
/// The target filter is applied before any message is formatted, because a collection-heavy run emits per-object trace records by the million on unrelated targets, and materializing them would swamp both memory and the measured operation.
pub fn capture_host_records<T>(target: &str, operation: impl FnOnce() -> T) -> (T, Vec<String>) {
    static INSTALL: Once = Once::new();
    INSTALL.call_once(|| {
        // `init` raises `log`'s max level as it installs the logger; drop it straight back so the bridge is inert until a capture raises it deliberately.
        let _ = LogTracer::init();
        set_max_level(LevelFilter::Off);
    });

    let records = Arc::new(Mutex::new(Vec::new()));
    let layer = HostRecordLayer {
        target: target.to_string(),
        records: Arc::clone(&records),
    };
    let subscriber = tracing_subscriber::registry().with(layer);

    set_max_level(LevelFilter::Trace);
    let result = tracing::subscriber::with_default(subscriber, operation);
    set_max_level(LevelFilter::Off);

    let records = Arc::try_unwrap(records)
        .expect("the capture subscriber was dropped with the scope")
        .into_inner()
        .expect("host record lock poisoned");

    (result, records)
}

/// Collects the `message` field of every event on one target. A bridged event's own metadata is `LogTracer`'s per-level static callsite — its target is literally `log` — with the record's origin riding in `log.*` fields, so the real target comes from [`NormalizeEvent::normalized_metadata`], which also answers `None` for native `tracing` events and thereby keeps the workspace's own spans out of the capture.
struct HostRecordLayer {
    target: String,
    records: Arc<Mutex<Vec<String>>>,
}

impl<S> Layer<S> for HostRecordLayer
where
    S: Subscriber + for<'lookup> LookupSpan<'lookup>,
{
    fn on_event(&self, event: &Event<'_>, _context: Context<'_, S>) {
        let Some(metadata) = event.normalized_metadata() else {
            return;
        };
        if metadata.target() != self.target {
            return;
        }

        let mut message = MessageValue::default();
        event.record(&mut message);

        if let Some(message) = message.0 {
            self.records
                .lock()
                .expect("host record lock poisoned")
                .push(message);
        }
    }
}

/// Reads the `message` field off a bridged event, ignoring anything else it carries.
///
/// **Both arms are load-bearing**, for the reason `collect.rs`'s `GroupValue` states: the bridge records the message as a `fmt::Arguments` through [`Visit::record_debug`] — whose `Debug` output is the formatted text, unquoted — so a visitor implementing only `record_str` compiles, runs, and silently captures nothing.
#[derive(Default)]
struct MessageValue(Option<String>);

impl Visit for MessageValue {
    fn record_str(&mut self, field: &Field, value: &str) {
        if field.name() == "message" {
            self.0 = Some(value.to_string());
        }
    }

    fn record_debug(&mut self, field: &Field, value: &dyn std::fmt::Debug) {
        // Formatted only on a name match: this runs at every captured event, and a message is a string allocation per record.
        if field.name() == "message" {
            self.0 = Some(format!("{value:?}"));
        }
    }
}

#[cfg(test)]
mod tests {
    use {
        super::*,
        tracing_log::log::{Level, log},
    };

    #[test]
    fn capture_host_records_filters_by_target_and_scopes_the_level() {
        let ((), records) = capture_host_records("probe::target", || {
            log!(target: "probe::target", Level::Trace, "Begin GC");
            log!(target: "probe::other", Level::Trace, "elsewhere");
            log!(target: "probe::target", Level::Trace, "new size is {:#x} bytes", 0x100000);
        });

        assert_eq!(records, ["Begin GC", "new size is 0x100000 bytes"]);

        // Outside a capture the max level is `Off`, so this reaches no logger and, with a fresh capture, leaves no residue.
        log!(target: "probe::target", Level::Trace, "between captures");

        let ((), records) = capture_host_records("probe::target", || {});
        assert!(records.is_empty());
    }
}
