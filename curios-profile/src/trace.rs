//! The record stream: [`trace`] runs a closure under a subscriber that writes Perfetto's trace as it happens, one packet a span boundary and an event, so a run that never returns, or dies, still leaves a whole prefix of what it did on disk.
//!
//! Nothing is aggregated here. A packet is what the callback had in hand — a track, a timestamp, the names it interns, a span's fields and the allocator's readings — and every statistic is [`fold`](crate::fold())'s to recompute from the file. `README.md` states why the library emits records and leaves aggregation to a consumer; what follows is what a reader of the file needs to know.
//!
//! **One sequence, a track a thread.** Every packet is on one packet sequence, written behind one lock, so a name or a string interned once is referred to by its id from any thread after it. The first packet clears the sequence's state and describes the process; each thread is described as a track of its own the first time it records, and its spans are slices on it, begun where the span is entered and ended where it is exited, which nest because a thread's spans do.
//!
//! **The allocator's readings ride each slice.** Where the counting allocator counts (`CountingAllocator`), four counter tracks under the process — live and allocated bytes, allocations and the peak — are described once, and each slice's beginning and end carries their readings as extra counter values, so a reader differences them as it differences the timestamps. Where it does not count, no counter track is described and no reading carried.
//!
//! **A stream is a whole prefix of its run.** Each packet is flushed as it is written, so a run that is killed has filed all it did up to its last whole packet; and the first write that fails ends the stream, the run going on, so a disk that filled leaves a file read the same way, never one with packets missing from its middle.

use {
    crate::{
        allocated_bytes, allocation_count, allocator_counts, begin_counting, end_counting,
        live_bytes, peak_bytes,
        perfetto::{
            CounterDescriptor, DebugAnnotation, DebugAnnotationName, EventCategory, EventName,
            InternedData, InternedString, ProcessDescriptor, ThreadDescriptor, Trace, TracePacket,
            TrackDescriptor, TrackEvent, counter_descriptor, debug_annotation, trace_packet,
            track_descriptor, track_event,
        },
    },
    prost::Message,
    std::{
        collections::HashMap,
        fmt,
        fs::{self, File},
        io::{self, Write},
        panic::{self, AssertUnwindSafe},
        path::PathBuf,
        process,
        sync::{
            Mutex,
            atomic::{AtomicU64, Ordering},
        },
        thread::{self, ThreadId},
        time::Instant,
    },
    tracing::{
        Event, Metadata, Subscriber,
        field::{Field, Visit},
        span,
        subscriber::Interest,
    },
};

/// The one packet sequence every packet of a stream is written on.
const SEQUENCE: u32 = 1;

/// The process's track, which every other track hangs from.
const PROCESS_TRACK: u64 = 1;

/// The allocator's four counter tracks, in the order a slice carries their readings: live bytes, allocated bytes, allocations, the peak.
pub const COUNTER_TRACKS: [u64; 4] = [2, 3, 4, 5];

/// The first id a thread's track is given; the threads that record take the ones after it in the order they first record.
const FIRST_THREAD_TRACK: u64 = 16;

/// Where a trace is written.
pub enum Destination {
    /// A writer its caller holds: standard output, or a buffer a caller folds back in the same process.
    Stream(Box<dyn Write + Send>),
    /// The file at this path, made with the directories that lead to it and started over where one stands.
    File(PathBuf),
}

/// Run `operation` with a trace-writing subscriber on the current thread.
///
/// The error is the destination's: a file that cannot be opened is worth refusing, because a caller asked for a trace and would otherwise get a silent compile. A write that fails *after* that ends the stream rather than raising — a span callback has no caller to return to, and taking a compilation down because a disk filled would be a worse failure than losing the rest of a measurement.
///
/// The allocator counts for as long as `operation` runs, and stops however it ends: a panic passes on through, once counting has stopped.
pub fn trace<T>(destination: Destination, operation: impl FnOnce() -> T) -> io::Result<T> {
    begin_counting();
    let recorder = match Recorder::new(destination, allocator_counts()) {
        Ok(recorder) => recorder,
        Err(error) => {
            end_counting();
            return Err(error);
        }
    };
    let ended = panic::catch_unwind(AssertUnwindSafe(|| {
        tracing::subscriber::with_default(recorder, operation)
    }));
    end_counting();

    match ended {
        Ok(result) => Ok(result),
        Err(payload) => panic::resume_unwind(payload),
    }
}

/// Record every span and event of this process, for a binary with no closure to wrap.
///
/// [`trace`] scopes a capture to one operation, which is what a build script and a probe want: they measure a compilation they call themselves. A *binary* has no such closure — the work is whatever subcommand the arguments selected — so this installs the same recorder as the process-global default and lets the whole invocation be the scope.
///
/// The two compose rather than compete. `set_global_default` is consulted only where no thread-local subscriber is set, so a [`trace`] on any thread still overrides this for the duration of its closure, and the callers that need a scoped capture keep it.
///
/// Configuration is still in code: the caller's argument decides that this is called at all, and the [`Destination`] it names decides where it writes. Neither is readable from the environment, so there is no second specification to disagree with the first. Once it is installed, the allocator counts for the rest of the process.
pub fn install(destination: Destination) -> io::Result<()> {
    begin_counting();
    let recorder =
        Recorder::new(destination, allocator_counts()).inspect_err(|_| end_counting())?;
    tracing::subscriber::set_global_default(recorder).map_err(io::Error::other)
}

struct Recorder {
    sink: Mutex<Sink>,
    /// Spans someone cloned a handle to. Empty for every span the workspace's macros create, which is why the common path never touches it: a span closes on its first `try_close` unless it appears here.
    shared: Mutex<HashMap<u64, usize>>,
    sequence: AtomicU64,
}

impl Recorder {
    fn new(destination: Destination, counted: bool) -> io::Result<Self> {
        Ok(Self {
            sink: Mutex::new(Sink::new(destination, counted)?),
            shared: Mutex::new(HashMap::new()),
            sequence: AtomicU64::new(1),
        })
    }

    fn sink(&self) -> std::sync::MutexGuard<'_, Sink> {
        self.sink.lock().expect("profiling sink lock")
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
        let id = self.sequence.fetch_add(1, Ordering::Relaxed);
        let mut fields = Fields::default();
        attributes.record(&mut fields);
        self.sink().created(id, attributes.metadata(), fields.0);

        span::Id::from_u64(id)
    }

    fn record(&self, span: &span::Id, values: &span::Record<'_>) {
        let mut fields = Fields::default();
        values.record(&mut fields);
        self.sink().recorded(span.into_u64(), fields.0);
    }

    // A causal edge between spans is not a cost, and nothing downstream reads one.
    fn record_follows_from(&self, _: &span::Id, _: &span::Id) {}

    fn event(&self, event: &Event<'_>) {
        let mut fields = Fields::default();
        event.record(&mut fields);
        self.sink().event(event.metadata(), fields.0);
    }

    fn enter(&self, span: &span::Id) {
        self.sink()
            .boundary(span.into_u64(), track_event::Type::SliceBegin);
    }

    fn exit(&self, span: &span::Id) {
        self.sink()
            .boundary(span.into_u64(), track_event::Type::SliceEnd);
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
        self.sink().closed(span.into_u64());

        true
    }
}

/// What a span was created with, kept until it closes: its name's and target's interned ids and its fields, written on each slice it is entered as.
struct Created {
    name: u64,
    category: u64,
    fields: Vec<Value>,
}

/// The open stream, and everything a packet refers to by an id the stream gave it.
struct Sink {
    /// Where packets go, and `None` once a write to it failed: nothing is written after the first write that fails.
    writer: Option<Box<dyn Write + Send>>,
    opened: Instant,
    counted: bool,
    names: Interned,
    categories: Interned,
    annotation_names: Interned,
    strings: Interned,
    threads: HashMap<ThreadId, u64>,
    spans: HashMap<u64, Created>,
}

impl Sink {
    fn new(destination: Destination, counted: bool) -> io::Result<Self> {
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

        let mut sink = Self {
            writer: Some(writer),
            opened: Instant::now(),
            counted,
            names: Interned::default(),
            categories: Interned::default(),
            annotation_names: Interned::default(),
            strings: Interned::default(),
            threads: HashMap::new(),
            spans: HashMap::new(),
        };
        sink.opening();

        Ok(sink)
    }

    /// Nanoseconds since the stream opened: the timestamp of what is written now, taken behind the lock so a sequence's packets are in time order.
    fn now(&self) -> u64 {
        self.opened.elapsed().as_nanos().min(u128::from(u64::MAX)) as u64
    }

    /// The first packet, which clears the sequence's state and describes the process, then the allocator's counter tracks where it counts.
    fn opening(&mut self) {
        let process = TrackDescriptor {
            uuid: Some(PROCESS_TRACK),
            process: Some(ProcessDescriptor {
                pid: Some(process::id() as i32),
                process_name: None,
            }),
            ..TrackDescriptor::default()
        };
        self.put(TracePacket {
            first_packet_on_sequence: Some(true),
            sequence_flags: Some(trace_packet::SequenceFlags::SeqIncrementalStateCleared as u32),
            data: Some(trace_packet::Data::TrackDescriptor(process)),
            ..self.packet()
        });

        if self.counted {
            let counters = [
                ("live bytes", counter_descriptor::Unit::SizeBytes),
                ("allocated bytes", counter_descriptor::Unit::SizeBytes),
                ("allocations", counter_descriptor::Unit::Count),
                ("peak bytes", counter_descriptor::Unit::SizeBytes),
            ];
            for (uuid, (name, unit)) in COUNTER_TRACKS.into_iter().zip(counters) {
                let counter = TrackDescriptor {
                    uuid: Some(uuid),
                    parent_uuid: Some(PROCESS_TRACK),
                    static_or_dynamic_name: Some(track_descriptor::StaticOrDynamicName::Name(
                        name.to_string(),
                    )),
                    counter: Some(CounterDescriptor {
                        unit: Some(unit as i32),
                    }),
                    ..TrackDescriptor::default()
                };
                self.put(TracePacket {
                    data: Some(trace_packet::Data::TrackDescriptor(counter)),
                    ..self.packet()
                });
            }
        }
    }

    /// A packet of this stream's sequence, stamped now, which reads the state the sequence has interned.
    fn packet(&self) -> TracePacket {
        TracePacket {
            timestamp: Some(self.now()),
            optional_trusted_packet_sequence_id: Some(
                trace_packet::OptionalTrustedPacketSequenceId::TrustedPacketSequenceId(SEQUENCE),
            ),
            sequence_flags: Some(trace_packet::SequenceFlags::SeqNeedsIncrementalState as u32),
            ..TracePacket::default()
        }
    }

    /// The track of the thread recording now, described the first time it records.
    fn track(&mut self) -> u64 {
        let current = thread::current();
        if let Some(&uuid) = self.threads.get(&current.id()) {
            return uuid;
        }

        let uuid = FIRST_THREAD_TRACK + self.threads.len() as u64;
        self.threads.insert(current.id(), uuid);
        let described = TrackDescriptor {
            uuid: Some(uuid),
            parent_uuid: Some(PROCESS_TRACK),
            thread: Some(ThreadDescriptor {
                pid: Some(process::id() as i32),
                // Numbered by the recorder rather than asked of the system, which names a thread differently on each: a track needs a number no other thread of the stream has.
                tid: Some((uuid - FIRST_THREAD_TRACK + 1) as i64),
                thread_name: current.name().map(str::to_string),
            }),
            ..TrackDescriptor::default()
        };
        self.put(TracePacket {
            data: Some(trace_packet::Data::TrackDescriptor(described)),
            ..self.packet()
        });

        uuid
    }

    fn created(&mut self, id: u64, metadata: &'static Metadata<'static>, fields: Vec<Value>) {
        let mut interned = InternedData::default();
        let name = self.names.id(metadata.name(), |iid, name| {
            interned.event_names.push(EventName {
                iid: Some(iid),
                name: Some(name.to_string()),
            });
        });
        let category = self.categories.id(metadata.target(), |iid, name| {
            interned.event_categories.push(EventCategory {
                iid: Some(iid),
                name: Some(name.to_string()),
            });
        });
        self.intern(interned);
        self.spans.insert(
            id,
            Created {
                name,
                category,
                fields,
            },
        );
    }

    /// A value recorded onto a live span after its creation, written on every slice it is entered as from here on.
    fn recorded(&mut self, id: u64, fields: Vec<Value>) {
        if let Some(span) = self.spans.get_mut(&id) {
            span.fields.extend(fields);
        }
    }

    fn closed(&mut self, id: u64) {
        self.spans.remove(&id);
    }

    /// A span entered or exited: a slice begun or ended on the thread's track, the allocator's readings beside it where it counts.
    fn boundary(&mut self, id: u64, kind: track_event::Type) {
        let track = self.track();
        let Some(span) = self.spans.get(&id) else {
            return;
        };
        let (name, category) = (span.name, span.category);
        let annotations = match kind {
            track_event::Type::SliceBegin => {
                let fields = span.fields.clone();
                self.annotations(&fields)
            }
            _ => Vec::new(),
        };
        let (extra_counter_track_uuids, extra_counter_values) = match self.counted {
            true => (
                COUNTER_TRACKS.to_vec(),
                vec![
                    live_bytes() as i64,
                    allocated_bytes() as i64,
                    allocation_count() as i64,
                    peak_bytes() as i64,
                ],
            ),
            false => (Vec::new(), Vec::new()),
        };
        let event = TrackEvent {
            r#type: Some(kind as i32),
            track_uuid: Some(track),
            name_field: Some(track_event::NameField::NameIid(name)),
            category_iids: vec![category],
            debug_annotations: annotations,
            extra_counter_track_uuids,
            extra_counter_values,
        };
        self.put(TracePacket {
            data: Some(trace_packet::Data::TrackEvent(event)),
            ..self.packet()
        });
    }

    /// An event: an instant on the thread's track, its fields as annotations. One carrying a `value` is a magnitude, `curios_common::sample!`'s.
    fn event(&mut self, metadata: &'static Metadata<'static>, fields: Vec<Value>) {
        let track = self.track();
        let mut interned = InternedData::default();
        let name = self.names.id(metadata.name(), |iid, name| {
            interned.event_names.push(EventName {
                iid: Some(iid),
                name: Some(name.to_string()),
            });
        });
        let category = self.categories.id(metadata.target(), |iid, name| {
            interned.event_categories.push(EventCategory {
                iid: Some(iid),
                name: Some(name.to_string()),
            });
        });
        self.intern(interned);
        let annotations = self.annotations(&fields);
        let event = TrackEvent {
            r#type: Some(track_event::Type::Instant as i32),
            track_uuid: Some(track),
            name_field: Some(track_event::NameField::NameIid(name)),
            category_iids: vec![category],
            debug_annotations: annotations,
            ..TrackEvent::default()
        };
        self.put(TracePacket {
            data: Some(trace_packet::Data::TrackEvent(event)),
            ..self.packet()
        });
    }

    /// `fields` as annotations, each name and string value interned, what is interned for the first time written ahead of them.
    fn annotations(&mut self, fields: &[Value]) -> Vec<DebugAnnotation> {
        let mut interned = InternedData::default();
        let annotations = fields
            .iter()
            .map(|field| {
                let name = self.annotation_names.id(field.name, |iid, name| {
                    interned.debug_annotation_names.push(DebugAnnotationName {
                        iid: Some(iid),
                        name: Some(name.to_string()),
                    });
                });
                let value = match &field.reading {
                    Reading::Bool(value) => debug_annotation::Value::BoolValue(*value),
                    Reading::Unsigned(value) => debug_annotation::Value::UintValue(*value),
                    Reading::Signed(value) => debug_annotation::Value::IntValue(*value),
                    Reading::Text(text) => debug_annotation::Value::StringValueIid(
                        self.strings.id(text, |iid, text| {
                            interned
                                .debug_annotation_string_values
                                .push(InternedString {
                                    iid: Some(iid),
                                    str: Some(text.as_bytes().to_vec()),
                                });
                        }),
                    ),
                };

                DebugAnnotation {
                    name_field: Some(debug_annotation::NameField::NameIid(name)),
                    value: Some(value),
                }
            })
            .collect();
        self.intern(interned);

        annotations
    }

    /// Write what was interned for the first time, in a packet ahead of the one that refers to it.
    fn intern(&mut self, interned: InternedData) {
        if interned != InternedData::default() {
            self.put(TracePacket {
                interned_data: Some(interned),
                ..self.packet()
            });
        }
    }

    /// One packet, written as a `Trace` of that packet alone, which a stream of them concatenates into one `Trace`, and flushed before the step after it runs, since a buffer is lost to anything that ends the process without unwinding — a stack overflow, an abort, a `SIGKILL` — and those are the runs whose last packets are worth the most. The first write that fails drops the writer, and nothing is written after it.
    fn put(&mut self, packet: TracePacket) {
        let Some(writer) = &mut self.writer else {
            return;
        };
        let bytes = Trace {
            packet: vec![packet],
        }
        .encode_to_vec();
        if writer
            .write_all(&bytes)
            .and_then(|()| writer.flush())
            .is_err()
        {
            self.writer = None;
        }
    }
}

/// Ids a stream gave the strings it interned of one kind, from one, each written once.
#[derive(Default)]
struct Interned(HashMap<String, u64>);

impl Interned {
    /// The id of `text`, given and handed to `first` the first time it is seen.
    fn id(&mut self, text: &str, first: impl FnOnce(u64, &str)) -> u64 {
        if let Some(&iid) = self.0.get(text) {
            return iid;
        }
        let iid = self.0.len() as u64 + 1;
        self.0.insert(text.to_string(), iid);
        first(iid, text);

        iid
    }
}

/// One field a span or an event carries.
#[derive(Clone)]
struct Value {
    name: &'static str,
    reading: Reading,
}

#[derive(Clone)]
enum Reading {
    Bool(bool),
    Unsigned(u64),
    Signed(i64),
    Text(String),
}

/// Every field a span or an event carries, whatever its type.
///
/// **Generic on purpose.** A visitor that kept only the fields it knew would silently discard one added at a call site; writing whatever arrives means a field is either in the file or it is not, and a reader can see which.
#[derive(Default)]
struct Fields(Vec<Value>);

impl Fields {
    fn put(&mut self, field: &Field, reading: Reading) {
        self.0.push(Value {
            name: field.name(),
            reading,
        });
    }
}

impl Visit for Fields {
    fn record_debug(&mut self, field: &Field, value: &dyn fmt::Debug) {
        self.put(field, Reading::Text(format!("{value:?}")));
    }

    fn record_str(&mut self, field: &Field, value: &str) {
        self.put(field, Reading::Text(value.to_string()));
    }

    fn record_u64(&mut self, field: &Field, value: u64) {
        self.put(field, Reading::Unsigned(value));
    }

    fn record_i64(&mut self, field: &Field, value: i64) {
        self.put(field, Reading::Signed(value));
    }

    fn record_bool(&mut self, field: &Field, value: bool) {
        self.put(field, Reading::Bool(value));
    }
}

#[cfg(test)]
mod tests;
