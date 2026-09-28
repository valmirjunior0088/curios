//! The record stream: [`trace`] runs a closure under a subscriber that writes one tab-separated row per span and event as it happens, so a run that never returns, or dies, still leaves everything it did on disk.
//!
//! Nothing is aggregated here. A row is what the callback had in hand — an identity, a timestamp, and the allocator's readings — and every statistic the old collector computed is [`fold`](crate::fold())'s to recompute from the file. `README.md` states why the library emits records and leaves aggregation to a consumer; what follows is what a reader of the file needs to know.
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
        fs::{self, File},
        io::{self, Write},
        path::{Path, PathBuf},
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

/// The size at which a [`Destination::Rotating`] stream is rotated, keeping the current file and its predecessor. One constant for every caller rather than a choice each makes: it bounds what an endless run may write, which is not a thing a caller has a reason to choose, and the destination is the only part of a measurement that is the caller's.
pub const ROTATION_CAP: u64 = 512 * 1024 * 1024;

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

/// The file a rotation moved the previous rows to: `path` with `.prev` appended.
///
/// Spelled here because rotation is this module's decision, and read back through [`fold_at`](crate::fold_at) so nothing derives it a second time. A reader that opened the set itself would be restating a convention it does not own — and because [`fold`](crate::fold) is deliberately tolerant of a truncated stream, a restatement that drifted would not fail. It would quietly report half a run as a whole one.
pub(crate) fn predecessor(path: &Path) -> PathBuf {
    let mut previous = path.to_path_buf().into_os_string();
    previous.push(".prev");

    PathBuf::from(previous)
}

/// Run `operation` with a record-writing subscriber on the current thread.
///
/// The error is the destination's: a file that cannot be opened is worth refusing, because a caller asked for a trace and would otherwise get a silent compile. A write that fails *after* that is dropped rather than raised — a span callback has no caller to return to, and taking a compilation down because a disk filled would be a worse failure than losing the tail of a measurement.
pub fn trace<T>(destination: Destination, operation: impl FnOnce() -> T) -> io::Result<T> {
    let recorder = Recorder::new(destination)?;
    let result = tracing::subscriber::with_default(recorder, operation);

    Ok(result)
}

/// Run a build script's `operation` under a record stream filed at `.artifacts/profile.tsv` beside the crate being built, then fold the stream and report where it landed, on one `cargo:warning` line.
///
/// **The one path this crate names, because a build script has no caller to take one from.** Every other capture is handed its destination; a build script is run by Cargo, whose arguments say nothing about profiling. The path is the repository's rule for a build product that outlives its build — `.artifacts/` beside its owner — applied to the crate `CARGO_MANIFEST_DIR` names, so each build script files beside itself and no two collide. That variable says *where*, never *whether*: the calling crate's `profile` feature is what decides a stream is filed at all.
///
/// The fold runs after `operation` returns and reads the file back, so a build that hangs has still filed every row it made; the report is for the build that finished. A stream that cannot be opened fails the build, as it would any build script whose feature asked for it.
pub fn trace_build_script<T>(operation: impl FnOnce() -> T) -> T {
    let manifest = std::env::var_os("CARGO_MANIFEST_DIR")
        .expect("Cargo runs a build script with `CARGO_MANIFEST_DIR` set");
    let path = PathBuf::from(manifest)
        .join(".artifacts")
        .join("profile.tsv");

    let result = trace(
        Destination::Rotating {
            path: path.clone(),
            cap: ROTATION_CAP,
        },
        operation,
    )
    .expect("the build profile opens");

    let report = crate::fold_at(&path).expect("the build profile folds");
    println!(
        "cargo:warning=build profile written to {} (peak {:.1} MiB)",
        path.display(),
        report.peak as f64 / (1024.0 * 1024.0),
    );
    result
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
}

impl Sink {
    fn new(destination: Destination) -> io::Result<Self> {
        let (writer, rotate): (Box<dyn Write + Send>, _) = match destination {
            Destination::Stream(writer) => (writer, None),
            Destination::Rotating { path, cap } => {
                // The directory is made here rather than by each caller, because the path is derived by `stream_path!` rather than chosen: a caller that cannot spell the path should not have to know which of its components exist.
                if let Some(parent) = path.parent() {
                    fs::create_dir_all(parent)?;
                }

                // A stream is the current file and its predecessor, and `fold_at` reads the two as one run. Truncating the current file alone would leave an earlier run's predecessor to be folded in front of this one's rows, so the pair is started over together.
                match fs::remove_file(predecessor(&path)) {
                    Err(error) if error.kind() != io::ErrorKind::NotFound => return Err(error),
                    _ => {}
                }
                let file = File::create(&path)?;

                (Box::new(file), Some((path, cap)))
            }
        };

        let mut sink = Self {
            writer,
            rotate,
            defined: Vec::new(),
            written: 0,
        };
        sink.header();

        Ok(sink)
    }

    /// Bytes handed to the writer, which is what a rotation is measured in. A failed write is counted as nothing and dropped; the module documentation says why it is not raised.
    ///
    /// Every row is flushed as it is written. A buffer is lost to anything that ends the process without unwinding — a stack overflow, an abort, a `SIGKILL` — and those are the runs whose last rows are worth the most, so a row reaches the file before the step after it runs.
    fn put(&mut self, row: &str) {
        if self.writer.write_all(row.as_bytes()).is_ok() {
            self.written += row.len() as u64;
        }
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
        self.settle();
    }

    fn event(&mut self, callsite: Callsite, at: u128, fields: &str) {
        let row = format!("V\t{callsite}\t{at}{fields}\n");
        self.put(&row);
        self.settle();
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
        self.settle();
    }

    /// A close, which carries no readings: nothing happens between a span's last exit and its close.
    fn closed(&mut self, id: u64, callsite: Callsite, at: u128) {
        let row = format!("C\t{id}\t{callsite}\t{at}\n");
        self.put(&row);
        self.settle();
    }

    /// Rotate if the file has grown past its cap.
    fn settle(&mut self) {
        if self
            .rotate
            .as_ref()
            .is_some_and(|&(_, cap)| self.written >= cap)
        {
            self.turn();
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
        let _ = fs::rename(&path, predecessor(&path));

        let Ok(file) = File::create(&path) else {
            return;
        };
        self.writer = Box::new(file);
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
