//! The record stream: [`trace`] runs a closure under a subscriber that writes one tab-separated row per span and event as it happens, so a run that never returns still leaves everything it did on disk.
//!
//! Nothing is aggregated here. A row is what the callback had in hand — an identity, a timestamp, and the allocator's readings — and every statistic the old collector computed is [`fold`](crate::fold)'s to recompute from the file. `README.md` states why the library emits records and leaves aggregation to a consumer; what follows is what a reader of the file needs to know.
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
//! **A span carries no state.** The callsite index is packed into the span id, so `enter` recovers a span's identity from the id alone and this subscriber keeps nothing per span — no registry, no extensions, no map. That is what makes a record cheaper than the aggregate row it replaced, and it is why `D` rows exist: the name is stated once and referred to by index thereafter.
//!
//! **Every file stands alone.** A rotation re-emits the header and every `D` row seen so far, so the surviving file is readable without the one that was discarded — which is the point of rotating rather than capping, since a hang's tail is what names the loop it is stuck in.

use {
    crate::{allocated_bytes, allocation_count, live_bytes, peak_bytes},
    std::{
        collections::HashMap,
        fmt,
        fs::File,
        io::{self, BufWriter, Write},
        path::PathBuf,
        sync::{
            Mutex,
            atomic::{AtomicU64, Ordering},
        },
        time::{Duration, Instant, SystemTime, UNIX_EPOCH},
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

/// How long a row may sit in the buffer before it reaches the file.
///
/// The deadline is compared against the timestamp the row already took, so the cadence costs no clock read of its own. It bounds what a `SIGKILL` loses, which is the only thing standing between a hung compile and an empty file.
const FLUSH_EVERY: Duration = Duration::from_secs(1);

/// Bits of a span id reserved for the callsite index, leaving the rest a sequence number.
const CALLSITE_BITS: u32 = 24;

const SEQUENCE_BITS: u32 = u64::BITS - CALLSITE_BITS;

const SEQUENCE_MASK: u64 = (1 << SEQUENCE_BITS) - 1;

/// Where a trace is written.
pub enum Destination {
    /// One writer that is never rotated: standard output, or a buffer a caller folds back in the same process.
    Stream(Box<dyn Write + Send>),
    /// A file at `path` and its predecessor at `path` with `.prev` appended, each closed at `cap` bytes.
    ///
    /// Rotation keeps the tail. A compile that has to be killed is stuck in whatever it was doing last, so the rows worth keeping are the newest ones; what a discarded file held is recoverable from the source, and what the surviving one holds is not.
    Rotating {
        /// The file rows are written to.
        path: PathBuf,
        /// The size at which the current file becomes `.prev` and a fresh one opens.
        cap: u64,
    },
}

/// Run `operation` with a record-writing subscriber on the current thread.
///
/// The error is the destination's: a file that cannot be opened is worth refusing, because a caller asked for a trace and would otherwise get a silent compile. A write that fails *after* that is dropped rather than raised — a span callback has no caller to return to, and taking a compilation down because a disk filled would be a worse failure than losing the tail of a measurement.
pub fn trace<T>(destination: Destination, operation: impl FnOnce() -> T) -> io::Result<T> {
    let recorder = Recorder::new(destination)?;
    let result = tracing::subscriber::with_default(recorder, operation);

    Ok(result)
}

/// What a span's identity is, once the file has named it: an index into the callsite table.
type Callsite = u32;

/// A span id carries its callsite in its high bits, so [`Subscriber::enter`] — which is handed an id and nothing else — can name the span without this subscriber storing anything per span.
///
/// The sequence is masked into the low bits and starts at one, so no id is ever zero, which `tracing` forbids. Wrapping would need 2^40 spans in one capture; the whole standard library's elaboration emits about 350 thousand.
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

/// The open file and what a rotation has to restate: the callsite table, so the file that survives is readable on its own.
struct Sink {
    writer: Box<dyn Write + Send>,
    /// The path and cap of a rotating destination; `None` for a stream, which is never rotated.
    rotate: Option<(PathBuf, u64)>,
    /// Every callsite named so far, indexed by the number it was named with, so a rotation can state them again.
    defined: Vec<(String, String)>,
    written: u64,
    /// When the buffer last reached the file, as nanoseconds since the capture opened — the same clock the rows carry, so the deadline needs no reading of its own.
    flushed: u128,
}

impl Sink {
    fn new(destination: Destination) -> io::Result<Self> {
        let (writer, rotate): (Box<dyn Write + Send>, _) = match destination {
            Destination::Stream(writer) => (writer, None),
            Destination::Rotating { path, cap } => {
                let file = BufWriter::new(File::create(&path)?);

                (Box::new(file), Some((path, cap)))
            }
        };

        let mut sink = Self {
            writer,
            rotate,
            defined: Vec::new(),
            written: 0,
            flushed: 0,
        };
        sink.header();

        Ok(sink)
    }

    /// Bytes handed to the writer, which is what a rotation is measured in. A failed write is counted as nothing and dropped; the module documentation says why it is not raised.
    fn put(&mut self, row: &str) {
        if self.writer.write_all(row.as_bytes()).is_ok() {
            self.written += row.len() as u64;
        }
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
        self.defined.push((target.to_string(), name.to_string()));
        self.state(callsite, target, name);
    }

    fn state(&mut self, callsite: Callsite, target: &str, name: &str) {
        let row = format!("D\t{callsite}\t{}\t{}\n", escape(target), escape(name));
        self.put(&row);
    }

    fn span(&mut self, kind: u8, id: u64, callsite: Callsite, at: u128, fields: &str) {
        let row = format!("{}\t{id}\t{callsite}\t{at}{fields}\n", kind as char);
        self.put(&row);
        self.settle(at);
    }

    fn event(&mut self, callsite: Callsite, at: u128, fields: &str) {
        let row = format!("V\t{callsite}\t{at}{fields}\n");
        self.put(&row);
        self.settle(at);
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
        self.settle(at);
    }

    /// A close, which carries no readings: nothing happens between a span's last exit and its close.
    fn closed(&mut self, id: u64, callsite: Callsite, at: u128) {
        let row = format!("C\t{id}\t{callsite}\t{at}\n");
        self.put(&row);
        self.settle(at);
    }

    /// Rotate if the file has grown past its cap, then flush if the deadline has passed. Both decisions ride on the timestamp the row already took, so neither reads a clock of its own.
    fn settle(&mut self, at: u128) {
        if self
            .rotate
            .as_ref()
            .is_some_and(|&(_, cap)| self.written >= cap)
        {
            self.turn();
        }

        if at.saturating_sub(self.flushed) >= FLUSH_EVERY.as_nanos() {
            let _ = self.writer.flush();
            self.flushed = at;
        }
    }

    /// Close the current file as `.prev` and open a fresh one carrying the header and the whole callsite table.
    fn turn(&mut self) {
        let Some((path, _)) = self.rotate.clone() else {
            return;
        };

        // The writer must let go of the file before it is renamed, so it is replaced by a sink for the length of the rename.
        let _ = self.writer.flush();
        self.writer = Box::new(io::sink());
        let mut previous = path.clone().into_os_string();
        previous.push(".prev");
        let _ = std::fs::rename(&path, PathBuf::from(previous));

        let Ok(file) = File::create(&path) else {
            return;
        };
        self.writer = Box::new(BufWriter::new(file));
        self.written = 0;
        self.header();

        for (callsite, (target, name)) in std::mem::take(&mut self.defined).into_iter().enumerate()
        {
            self.state(callsite as Callsite, &target, &name);
            self.defined.push((target, name));
        }
    }
}

impl Drop for Sink {
    fn drop(&mut self) {
        let _ = self.writer.flush();
    }
}

/// Every field a span or an event carries, rendered as tab-separated `name=value` pairs.
///
/// **Generic on purpose.** The collector this replaced kept a span's metadata and dropped its attribute values, visiting exactly one field of one event — so a field added at a call site was written and silently discarded, which is what `profile_group!` was invented to work around. Writing whatever arrives ends that class: a field is either in the file or it is not, and a reader can see which.
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

/// The four characters a tab-separated row cannot hold bare. A value reaches here through `Debug`, so this is about what a field *can* contain rather than what today's fields do contain.
fn escape(value: &str) -> String {
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
