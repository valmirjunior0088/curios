//! The record stream: [`trace`] runs a closure under a subscriber that writes one tab-separated row per span and event as it happens, so a run that never returns, or dies, still leaves everything it did on disk.
//!
//! Nothing is aggregated here. A row is what the callback had in hand — an identity, a timestamp, and the allocator's readings — and every statistic is [`fold`](crate::fold())'s to recompute from the file. `README.md` states why the library emits records and leaves aggregation to a consumer; what follows is what a reader of the file needs to know.
//!
//! **The row shapes.** The first column is the kind, so `awk '$1 == "V"'` is a whole analysis:
//!
//! ```text
//! H  version  unix_nanos                                    the stream opens
//! D  cs       target  name                                  a callsite, named once
//! S  id       cs      ns  [k=v …]                           a span is created
//! E  id       cs      ns  live  allocated  allocations  peak    entered
//! X  id       cs      ns  live  allocated  allocations  peak    exited
//! R  id       cs      ns  [k=v …]                           a value recorded after creation
//! C  id       cs      ns                                    closed; the id retires
//! V  cs       ns      [k=v …]                               an event
//! ```
//!
//! **A span carries no state.** The callsite index is packed into the span id, so `enter` recovers a span's identity from the id alone and this subscriber keeps nothing per span — no registry, no extensions, no map. That is why `D` rows exist: the name is stated once and referred to by index thereafter.
//!
//! **A stream is one file, whole.** Every row is flushed as it is written, so a run that is killed has filed all it did up to the kill, and nothing a run wrote is discarded while it goes on: a stream grows for as long as its run records, and what bounds it is whoever asked for it.

use {
    crate::{allocated_bytes, allocation_count, live_bytes, peak_bytes},
    std::{
        collections::HashMap,
        fmt,
        fs::{self, File},
        io::{self, Write},
        path::PathBuf,
        sync::{
            Mutex,
            atomic::{AtomicU64, Ordering},
        },
        time::{Instant, SystemTime, UNIX_EPOCH},
    },
    tracing::{
        Event, Metadata, Subscriber,
        field::{Field, Visit},
        span,
        subscriber::Interest,
    },
};

/// The row shapes this module writes, so a reader can refuse a file it does not understand.
const VERSION: u32 = 1;

/// Bits of a span id reserved for the callsite index, leaving the rest a sequence number.
const CALLSITE_BITS: u32 = 24;

const SEQUENCE_BITS: u32 = u64::BITS - CALLSITE_BITS;

const SEQUENCE_MASK: u64 = (1 << SEQUENCE_BITS) - 1;

/// Where a trace is written.
pub enum Destination {
    /// A writer its caller holds: standard output, or a buffer a caller folds back in the same process.
    Stream(Box<dyn Write + Send>),
    /// The file at this path, made with the directories that lead to it and started over where one stands.
    File(PathBuf),
}

/// Run `operation` with a record-writing subscriber on the current thread.
///
/// The error is the destination's: a file that cannot be opened is worth refusing, because a caller asked for a trace and would otherwise get a silent compile. A write that fails *after* that is dropped rather than raised — a span callback has no caller to return to, and taking a compilation down because a disk filled would be a worse failure than losing the tail of a measurement.
pub fn trace<T>(destination: Destination, operation: impl FnOnce() -> T) -> io::Result<T> {
    let recorder = Recorder::new(destination)?;
    let result = tracing::subscriber::with_default(recorder, operation);

    Ok(result)
}

/// Record every span and event of this process, for a binary with no closure to wrap.
///
/// [`trace`] scopes a capture to one operation, which is what a build script and a probe want: they measure a compilation they call themselves. A *binary* has no such closure — the work is whatever subcommand the arguments selected — so this installs the same recorder as the process-global default and lets the whole invocation be the scope.
///
/// The two compose rather than compete. `set_global_default` is consulted only where no thread-local subscriber is set, so a [`trace`] on any thread still overrides this for the duration of its closure, and the callers that need a scoped capture keep it.
///
/// Configuration is still in code: the `profile` feature decides that this is called at all, and the [`Destination`] its caller names decides where it writes. Neither is readable from the environment, so there is no second specification to disagree with the first.
pub fn install(destination: Destination) -> io::Result<()> {
    let recorder = Recorder::new(destination)?;

    tracing::subscriber::set_global_default(recorder).map_err(io::Error::other)
}

/// What a span's identity is, once the file has named it: an index into the callsite table.
type Callsite = u32;

/// A span id carries its callsite in its high bits, so [`Subscriber::enter`] — which is handed an id and nothing else — can name the span without this subscriber storing anything per span.
///
/// The sequence is masked into the low bits and starts at one, so no id is ever zero, which `tracing` forbids. Wrapping would need 2^40 spans in one capture; the prelude's elaboration emits about three million (counted at `374325495`: the `S` rows of the stream `cargo xtask profile-prelude elaboration <PATH>` files).
fn pack(callsite: Callsite, sequence: u64) -> u64 {
    ((callsite as u64) << SEQUENCE_BITS) | (sequence & SEQUENCE_MASK).max(1)
}

fn callsite_of(id: &span::Id) -> Callsite {
    (id.into_u64() >> SEQUENCE_BITS) as Callsite
}

struct Recorder {
    sink: Mutex<Sink>,
    /// Callsite indices, assigned on first sight. Read on every span creation and every event; never on enter or exit, which take the index out of the id.
    callsites: Mutex<HashMap<tracing::callsite::Identifier, Callsite>>,
    /// Spans someone cloned a handle to. Empty for every span the workspace's own macros create, which is why the common path never touches it: a span closes on its first `try_close` unless it appears here.
    shared: Mutex<HashMap<u64, usize>>,
    sequence: AtomicU64,
    opened: Instant,
}

impl Recorder {
    fn new(destination: Destination) -> io::Result<Self> {
        let opened = Instant::now();

        Ok(Self {
            sink: Mutex::new(Sink::new(destination)?),
            callsites: Mutex::new(HashMap::new()),
            shared: Mutex::new(HashMap::new()),
            sequence: AtomicU64::new(1),
            opened,
        })
    }

    fn elapsed(&self) -> u128 {
        self.opened.elapsed().as_nanos()
    }

    /// The index this metadata is named by, defining it in the file the first time it is seen.
    fn callsite(&self, metadata: &'static Metadata<'static>) -> Callsite {
        let mut callsites = self.callsites.lock().expect("profiling callsite lock");
        let next = callsites.len() as Callsite;

        match callsites.entry(metadata.callsite()) {
            std::collections::hash_map::Entry::Occupied(entry) => *entry.get(),
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(next);
                self.write(|sink| sink.define(next, metadata.target(), metadata.name()));

                next
            }
        }
    }

    fn write(&self, row: impl FnOnce(&mut Sink)) {
        let mut sink = self.sink.lock().expect("profiling sink lock");
        row(&mut sink);
    }
}

impl Subscriber for Recorder {
    // Every callsite is wanted and none is ever filtered, so interest is answered once per callsite and never rechecked.
    fn register_callsite(&self, _: &'static Metadata<'static>) -> Interest {
        Interest::always()
    }

    fn enabled(&self, _: &Metadata<'_>) -> bool {
        true
    }

    fn max_level_hint(&self) -> Option<tracing::level_filters::LevelFilter> {
        Some(tracing::level_filters::LevelFilter::TRACE)
    }

    fn new_span(&self, attributes: &span::Attributes<'_>) -> span::Id {
        let callsite = self.callsite(attributes.metadata());
        let sequence = self.sequence.fetch_add(1, Ordering::Relaxed);
        let id = pack(callsite, sequence);
        let at = self.elapsed();

        let mut fields = Fields::default();
        attributes.record(&mut fields);
        self.write(|sink| sink.span(b'S', id, callsite, at, &fields.0));

        span::Id::from_u64(id)
    }

    fn record(&self, span: &span::Id, values: &span::Record<'_>) {
        let at = self.elapsed();
        let mut fields = Fields::default();
        values.record(&mut fields);
        let (id, callsite) = (span.into_u64(), callsite_of(span));
        self.write(|sink| sink.span(b'R', id, callsite, at, &fields.0));
    }

    // A causal edge between spans is not a cost, and nothing downstream reads one.
    fn record_follows_from(&self, _: &span::Id, _: &span::Id) {}

    fn event(&self, event: &Event<'_>) {
        let callsite = self.callsite(event.metadata());
        let at = self.elapsed();
        let mut fields = Fields::default();
        event.record(&mut fields);
        self.write(|sink| sink.event(callsite, at, &fields.0));
    }

    fn enter(&self, span: &span::Id) {
        let at = self.elapsed();
        let (id, callsite) = (span.into_u64(), callsite_of(span));
        self.write(|sink| sink.boundary(b'E', id, callsite, at));
    }

    fn exit(&self, span: &span::Id) {
        let at = self.elapsed();
        let (id, callsite) = (span.into_u64(), callsite_of(span));
        self.write(|sink| sink.boundary(b'X', id, callsite, at));
    }

    fn clone_span(&self, span: &span::Id) -> span::Id {
        *self
            .shared
            .lock()
            .expect("profiling sharing lock")
            .entry(span.into_u64())
            .or_insert(1) += 1;

        span.clone()
    }

    fn try_close(&self, span: span::Id) -> bool {
        let mut shared = self.shared.lock().expect("profiling sharing lock");
        if let Some(holders) = shared.get_mut(&span.into_u64()) {
            *holders -= 1;
            if *holders > 0 {
                return false;
            }
            shared.remove(&span.into_u64());
        }
        drop(shared);

        let at = self.elapsed();
        let (id, callsite) = (span.into_u64(), callsite_of(&span));
        self.write(|sink| sink.closed(id, callsite, at));

        true
    }
}

/// The open stream.
struct Sink {
    writer: Box<dyn Write + Send>,
}

impl Sink {
    fn new(destination: Destination) -> io::Result<Self> {
        let writer: Box<dyn Write + Send> = match destination {
            Destination::Stream(writer) => writer,
            Destination::File(path) => {
                // The directory is made here rather than by each caller, so a caller naming a path, as `--profile` does, need not know which of its components exist.
                if let Some(parent) = path.parent() {
                    fs::create_dir_all(parent)?;
                }

                Box::new(File::create(&path)?)
            }
        };

        let mut sink = Self { writer };
        sink.header();

        Ok(sink)
    }

    /// Every row is flushed as it is written. A buffer is lost to anything that ends the process without unwinding — a stack overflow, an abort, a `SIGKILL` — and those are the runs whose last rows are worth the most, so a row reaches the file before the step after it runs. A failed write is dropped; [`trace`] says why it is not raised.
    fn put(&mut self, row: &str) {
        let _ = self.writer.write_all(row.as_bytes());
        let _ = self.writer.flush();
    }

    fn header(&mut self) {
        let wall = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos();
        let row = format!("H\t{VERSION}\t{wall}\n");
        self.put(&row);
    }

    fn define(&mut self, callsite: Callsite, target: &str, name: &str) {
        let row = format!("D\t{callsite}\t{}\t{}\n", escape(target), escape(name));
        self.put(&row);
    }

    fn span(&mut self, kind: u8, id: u64, callsite: Callsite, at: u128, fields: &str) {
        let row = format!("{}\t{id}\t{callsite}\t{at}{fields}\n", kind as char);
        self.put(&row);
    }

    fn event(&mut self, callsite: Callsite, at: u128, fields: &str) {
        let row = format!("V\t{callsite}\t{at}{fields}\n");
        self.put(&row);
    }

    /// An entry or an exit, with the four readings a fold differences into what the span retained, took and reached.
    fn boundary(&mut self, kind: u8, id: u64, callsite: Callsite, at: u128) {
        let row = format!(
            "{}\t{id}\t{callsite}\t{at}\t{}\t{}\t{}\t{}\n",
            kind as char,
            live_bytes(),
            allocated_bytes(),
            allocation_count(),
            peak_bytes(),
        );
        self.put(&row);
    }

    /// A close, which carries no readings: nothing happens between a span's last exit and its close.
    fn closed(&mut self, id: u64, callsite: Callsite, at: u128) {
        let row = format!("C\t{id}\t{callsite}\t{at}\n");
        self.put(&row);
    }
}

impl Drop for Sink {
    fn drop(&mut self) {
        let _ = self.writer.flush();
    }
}

/// Every field a span or an event carries, rendered as tab-separated `name=value` pairs.
///
/// **Generic on purpose.** A visitor that kept only the fields it knew would silently discard one added at a call site; writing whatever arrives means a field is either in the file or it is not, and a reader can see which.
#[derive(Default)]
struct Fields(String);

impl Fields {
    fn put(&mut self, field: &Field, value: fmt::Arguments<'_>) {
        self.0.push('\t');
        self.0.push_str(&escape(field.name()));
        self.0.push('=');
        self.0.push_str(&escape(&value.to_string()));
    }
}

impl Visit for Fields {
    fn record_debug(&mut self, field: &Field, value: &dyn fmt::Debug) {
        self.put(field, format_args!("{value:?}"));
    }

    fn record_str(&mut self, field: &Field, value: &str) {
        self.put(field, format_args!("{value}"));
    }

    fn record_u64(&mut self, field: &Field, value: u64) {
        self.put(field, format_args!("{value}"));
    }

    fn record_i64(&mut self, field: &Field, value: i64) {
        self.put(field, format_args!("{value}"));
    }

    fn record_bool(&mut self, field: &Field, value: bool) {
        self.put(field, format_args!("{value}"));
    }
}

/// The four characters a tab-separated row cannot hold bare. A value reaches here through `Debug`, so this is about what a field *can* contain rather than what the workspace's fields do.
pub(crate) fn escape(value: &str) -> String {
    let mut escaped = String::with_capacity(value.len());
    for character in value.chars() {
        match character {
            '\\' => escaped.push_str("\\\\"),
            '\t' => escaped.push_str("\\t"),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            _ => escaped.push(character),
        }
    }

    escaped
}

#[cfg(test)]
mod tests;
