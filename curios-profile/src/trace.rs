//! The record stream: [`trace`] runs a closure under a subscriber that writes Perfetto's trace as it happens, one packet a span boundary and an event, so a run that never returns, or dies, still leaves a whole prefix of what it did on disk.
//!
//! Nothing is aggregated here. A packet is what the callback had in hand — a timestamp, the names it interns, a span's fields and the allocator's readings — and every statistic is [`fold`](crate::fold())'s to recompute from the file. `README.md` states why the library emits records and leaves aggregation to a consumer; what follows is what a reader of the file needs to know.
//!
//! **A sequence a thread, as Perfetto's own writer keeps one.** The process's packet sequence describes the process and, where the allocator counts, its two gauges. Each thread opens a sequence of its own the first time it records. Its first packet clears the sequence's state, declares an incremental clock, and states as defaults what every later packet of it shares: its thread's track and the counter tracks its slices read. So every later packet is stamped with the step since the one before it and names neither its track nor its counters. Names and strings are interned on the sequence that uses them. A thread's spans are slices on its track, begun where the span is entered and ended where it is exited, which nest because a thread's spans do. Every packet is written behind one lock, into one stream.
//!
//! **The allocator's readings ride each slice.** Where the counting allocator counts (`CountingAllocator`), each slice's beginning and end carries four readings as extra counter values. Two are its thread's allocated bytes and allocations, counters of the thread written as steps on its sequence, as Perfetto writes a thread's time. The other two are the process's live bytes and peak, gauges every sequence reads, so each is written whole. Where it does not count, no counter track is described and no reading carried.
//!
//! **No slice is charged the recorder's own work.** Every callback the recorder answers allocates — a span's fields kept, a name interned, a packet built — on the thread of the span it records, inside whatever span that thread has entered. So each callback counts apart what it took, and a slice reads its thread's two counts less what was counted apart: a span is charged what the work inside it took, however many spans and samples the recorder wrote there.
//!
//! **A stream is a whole prefix of its run.** Each packet is flushed as it is written, so a run that is killed has filed all it did up to its last whole packet; and the first write that fails ends the stream, the run going on, so a disk that filled leaves a file read the same way, never one with packets missing from its middle.

use {
    crate::{
        allocator_counts, begin_counting, end_counting, live_bytes, peak_bytes,
        perfetto::{
            BuiltinClock, ClockSnapshot, CounterDescriptor, DebugAnnotation, DebugAnnotationName,
            EventCategory, EventName, InternedData, InternedString, ProcessDescriptor,
            ThreadDescriptor, Trace, TracePacket, TracePacketDefaults, TrackDescriptor, TrackEvent,
            TrackEventDefaults, clock_snapshot, counter_descriptor, debug_annotation, trace_packet,
            track_descriptor, track_event,
        },
        thread_allocated_bytes, thread_allocation_count,
    },
    prost::Message,
    std::{
        cell::Cell,
        collections::{HashMap, hash_map::Entry},
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

/// The process's packet sequence; each thread's takes an id after it, in the order the threads first record.
const PROCESS_SEQUENCE: u32 = 1;

/// The process's track, which every other track hangs from.
const PROCESS_TRACK: u64 = 1;

/// The tracks of the process's two gauges.
const LIVE_TRACK: u64 = 2;
const PEAK_TRACK: u64 = 3;

/// The first thread's track. Each thread takes [`TRACKS_A_THREAD`] ids in the order the threads first record: its own track's, then its two counters'.
const FIRST_THREAD_TRACK: u64 = 16;
const TRACKS_A_THREAD: u64 = 3;

/// The clock a thread's sequence stamps its packets on, each a step from the one before: an id in the range Perfetto keeps for a sequence's own clocks, 64 to 127.
const STEP_CLOCK: u32 = 64;

/// What a counter track of a stream counts, by the name its descriptor gives it: the one statement of those names, which the reader tells the readings apart by.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Counter {
    /// Bytes a thread took while counting, each reallocation's growth once.
    Allocated,
    /// Trips a thread made through the allocator while counting.
    Allocations,
    /// Bytes the process holds, counted from where counting began.
    Live,
    /// The greatest the process held while counting.
    Peak,
}

impl Counter {
    /// Every counter, in the order a slice carries their readings: its thread's two, then the process's.
    pub const ALL: [Self; 4] = [Self::Allocated, Self::Allocations, Self::Live, Self::Peak];

    pub const fn name(self) -> &'static str {
        match self {
            Self::Allocated => "allocated bytes",
            Self::Allocations => "allocations",
            Self::Live => "live bytes",
            Self::Peak => "peak bytes",
        }
    }

    /// The counter's track, `uuid` under `parent`: a thread's counter written as steps, the process's whole.
    fn track(self, uuid: u64, parent: u64) -> TrackDescriptor {
        let unit = match self {
            Self::Allocations => counter_descriptor::Unit::Count,
            Self::Allocated | Self::Live | Self::Peak => counter_descriptor::Unit::SizeBytes,
        };

        TrackDescriptor {
            uuid: Some(uuid),
            parent_uuid: Some(parent),
            static_or_dynamic_name: Some(track_descriptor::StaticOrDynamicName::Name(
                self.name().to_string(),
            )),
            counter: Some(CounterDescriptor {
                unit: Some(unit as i32),
                is_incremental: matches!(self, Self::Allocated | Self::Allocations).then_some(true),
            }),
            ..TrackDescriptor::default()
        }
    }
}

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
        unheard(|| {
            let id = self.sequence.fetch_add(1, Ordering::Relaxed);
            let mut fields = Fields::default();
            attributes.record(&mut fields);
            self.sink().created(id, attributes.metadata(), fields.0);

            span::Id::from_u64(id)
        })
    }

    fn record(&self, span: &span::Id, values: &span::Record<'_>) {
        unheard(|| {
            let mut fields = Fields::default();
            values.record(&mut fields);
            self.sink().recorded(span.into_u64(), fields.0);
        });
    }

    // A causal edge between spans is not a cost, and nothing downstream reads one.
    fn record_follows_from(&self, _: &span::Id, _: &span::Id) {}

    fn event(&self, event: &Event<'_>) {
        unheard(|| {
            let mut fields = Fields::default();
            event.record(&mut fields);
            self.sink().event(event.metadata(), fields.0);
        });
    }

    fn enter(&self, span: &span::Id) {
        unheard(|| {
            self.sink()
                .boundary(span.into_u64(), track_event::Type::SliceBegin);
        });
    }

    fn exit(&self, span: &span::Id) {
        unheard(|| {
            self.sink()
                .boundary(span.into_u64(), track_event::Type::SliceEnd);
        });
    }

    fn clone_span(&self, span: &span::Id) -> span::Id {
        unheard(|| {
            *self
                .shared
                .lock()
                .expect("profiling sharing lock")
                .entry(span.into_u64())
                .or_insert(1) += 1;

            span.clone()
        })
    }

    fn try_close(&self, span: span::Id) -> bool {
        unheard(|| {
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
        })
    }
}

thread_local! {
    /// Bytes the recorder took for itself on this thread, as its allocator count reads them: what its callbacks allocated. Initialized as a constant with no destructor, as the allocator's own counts are, so reading it never allocates.
    static UNHEARD_ALLOCATED: Cell<u64> = const { Cell::new(0) };
    /// Trips the recorder made through the allocator for itself on this thread.
    static UNHEARD_ALLOCATIONS: Cell<u64> = const { Cell::new(0) };
}

/// Run one of the recorder's callbacks, counting apart what it takes from the allocator as the recorder's own, which [`Counts::now`] leaves out of every slice's readings.
fn unheard<T>(callback: impl FnOnce() -> T) -> T {
    let bytes = thread_allocated_bytes();
    let trips = thread_allocation_count();
    let answer = callback();
    UNHEARD_ALLOCATED.with(|own| own.set(own.get() + (thread_allocated_bytes() - bytes)));
    UNHEARD_ALLOCATIONS.with(|own| own.set(own.get() + (thread_allocation_count() - trips)));

    answer
}

/// What a span was created with, kept until it closes and written on each slice it is entered as: its callsite, whose name and target are interned on the sequence of the thread that enters it, and its fields.
struct Created {
    callsite: &'static Metadata<'static>,
    fields: Vec<Value>,
}

/// The open stream: where its packets go, when it opened, whether the allocator counts, each thread's sequence and each live span.
struct Sink {
    out: Out,
    opened: Instant,
    counted: bool,
    threads: HashMap<ThreadId, Sequence>,
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
            out: Out(Some(writer)),
            opened: Instant::now(),
            counted,
            threads: HashMap::new(),
            spans: HashMap::new(),
        };
        sink.opening();

        Ok(sink)
    }

    /// Nanoseconds since the stream opened: the time of what is written now, taken behind the lock so each sequence's packets are in time order.
    fn now(&self) -> u64 {
        self.opened.elapsed().as_nanos().min(u128::from(u64::MAX)) as u64
    }

    /// The process's sequence: a packet clearing its state and describing the process, then the process's gauges where the allocator counts, each stamped whole.
    fn opening(&mut self) {
        let now = self.now();
        let process = TrackDescriptor {
            uuid: Some(PROCESS_TRACK),
            process: Some(ProcessDescriptor {
                pid: Some(process::id() as i32),
                process_name: None,
            }),
            ..TrackDescriptor::default()
        };
        self.out.put(TracePacket {
            timestamp: Some(now),
            optional_trusted_packet_sequence_id: Some(sequenced(PROCESS_SEQUENCE)),
            first_packet_on_sequence: Some(true),
            sequence_flags: Some(trace_packet::SequenceFlags::SeqIncrementalStateCleared as u32),
            data: Some(trace_packet::Data::TrackDescriptor(process)),
            ..TracePacket::default()
        });

        if self.counted {
            for (uuid, counter) in [(LIVE_TRACK, Counter::Live), (PEAK_TRACK, Counter::Peak)] {
                self.out.put(TracePacket {
                    timestamp: Some(now),
                    optional_trusted_packet_sequence_id: Some(sequenced(PROCESS_SEQUENCE)),
                    data: Some(trace_packet::Data::TrackDescriptor(
                        counter.track(uuid, PROCESS_TRACK),
                    )),
                    ..TracePacket::default()
                });
            }
        }
    }

    /// The sequence of the thread recording now, opened the first time it records.
    fn sequence(&mut self, now: u64) -> &mut Sequence {
        let current = thread::current();
        let index = self.threads.len() as u64;
        match self.threads.entry(current.id()) {
            Entry::Occupied(open) => open.into_mut(),
            Entry::Vacant(first) => first.insert(Sequence::opened(
                &mut self.out,
                self.counted,
                index,
                current.name(),
                now,
            )),
        }
    }

    fn created(&mut self, id: u64, callsite: &'static Metadata<'static>, fields: Vec<Value>) {
        self.spans.insert(id, Created { callsite, fields });
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
        // Read before anything of the packet is built: what building it takes is counted apart as the recorder's once this callback returns, so a reading taken after would charge it to the slice.
        let counts = self.counted.then(Counts::now);
        let Some(span) = self.spans.get(&id) else {
            return;
        };
        let callsite = span.callsite;
        let fields = match kind {
            track_event::Type::SliceBegin => span.fields.clone(),
            _ => Vec::new(),
        };
        let now = self.now();
        let sequence = self.sequence(now);
        let mut interned = InternedData::default();
        let event = TrackEvent {
            r#type: Some(kind as i32),
            ..sequence.event(callsite, &fields, &mut interned)
        };
        let extra_counter_values = counts.map_or_else(Vec::new, |counts| sequence.carried(counts));
        let packet = sequence.packet(
            now,
            interned,
            trace_packet::Data::TrackEvent(TrackEvent {
                extra_counter_values,
                ..event
            }),
        );
        self.out.put(packet);
    }

    /// An event: an instant on the thread's track, its fields as annotations. One carrying a `value` is a magnitude, `curios_common::sample!`'s.
    fn event(&mut self, callsite: &'static Metadata<'static>, fields: Vec<Value>) {
        let now = self.now();
        let sequence = self.sequence(now);
        let mut interned = InternedData::default();
        let event = TrackEvent {
            r#type: Some(track_event::Type::Instant as i32),
            ..sequence.event(callsite, &fields, &mut interned)
        };
        let packet = sequence.packet(now, interned, trace_packet::Data::TrackEvent(event));
        self.out.put(packet);
    }
}

/// The allocator's counts as one slice carries them.
struct Counts {
    allocated: u64,
    allocations: u64,
    live: isize,
    peak: usize,
}

impl Counts {
    /// The counts as they stand, the thread's two less what the recorder took for itself on it ([`unheard`]).
    fn now() -> Self {
        Self {
            allocated: thread_allocated_bytes() - UNHEARD_ALLOCATED.with(Cell::get),
            allocations: thread_allocation_count() - UNHEARD_ALLOCATIONS.with(Cell::get),
            live: live_bytes(),
            peak: peak_bytes(),
        }
    }
}

/// A thread's packet sequence: its id, the strings interned on it, the time its last packet was stamped at, and its thread's counts as its last slice carried them.
struct Sequence {
    id: u32,
    names: Interned,
    categories: Interned,
    annotation_names: Interned,
    strings: Interned,
    stamped: u64,
    allocated: u64,
    allocations: u64,
}

impl Sequence {
    /// The sequence of the `index`th thread to record, named `name`. Its first packet clears its state, declares its clock beside the stream's and states its defaults; its track, and its counters where the allocator counts, are described after it.
    fn opened(out: &mut Out, counted: bool, index: u64, name: Option<&str>, now: u64) -> Self {
        let track = FIRST_THREAD_TRACK + index * TRACKS_A_THREAD;
        let counters = [
            (track + 1, Counter::Allocated),
            (track + 2, Counter::Allocations),
        ];
        let mut sequence = Self {
            id: PROCESS_SEQUENCE + 1 + index as u32,
            names: Interned::default(),
            categories: Interned::default(),
            annotation_names: Interned::default(),
            strings: Interned::default(),
            stamped: now,
            allocated: 0,
            allocations: 0,
        };
        let read = match counted {
            true => vec![track + 1, track + 2, LIVE_TRACK, PEAK_TRACK],
            false => Vec::new(),
        };
        let stream = BuiltinClock::Boottime as u32;
        out.put(TracePacket {
            timestamp: Some(now),
            timestamp_clock_id: Some(stream),
            optional_trusted_packet_sequence_id: Some(sequenced(sequence.id)),
            first_packet_on_sequence: Some(true),
            sequence_flags: Some(trace_packet::SequenceFlags::SeqIncrementalStateCleared as u32),
            trace_packet_defaults: Some(TracePacketDefaults {
                timestamp_clock_id: Some(STEP_CLOCK),
                track_event_defaults: Some(TrackEventDefaults {
                    track_uuid: Some(track),
                    extra_counter_track_uuids: read,
                }),
            }),
            data: Some(trace_packet::Data::ClockSnapshot(ClockSnapshot {
                clocks: vec![
                    clock_snapshot::Clock {
                        clock_id: Some(stream),
                        timestamp: Some(now),
                        is_incremental: None,
                    },
                    clock_snapshot::Clock {
                        clock_id: Some(STEP_CLOCK),
                        timestamp: Some(now),
                        is_incremental: Some(true),
                    },
                ],
            })),
            ..TracePacket::default()
        });

        let described = TrackDescriptor {
            uuid: Some(track),
            parent_uuid: Some(PROCESS_TRACK),
            thread: Some(ThreadDescriptor {
                pid: Some(process::id() as i32),
                // Numbered by the recorder rather than asked of the system, which names a thread differently on each: a track needs a number no other thread of the stream has.
                tid: Some(index as i64 + 1),
                thread_name: name.map(str::to_string),
            }),
            ..TrackDescriptor::default()
        };
        out.put(sequence.packet(
            now,
            InternedData::default(),
            trace_packet::Data::TrackDescriptor(described),
        ));
        if counted {
            for (uuid, counter) in counters {
                out.put(sequence.packet(
                    now,
                    InternedData::default(),
                    trace_packet::Data::TrackDescriptor(counter.track(uuid, track)),
                ));
            }
        }

        sequence
    }

    /// A packet of this sequence stamped `now`, as the step since the one before it, carrying `data` and what was interned for it for the first time.
    fn packet(
        &mut self,
        now: u64,
        interned: InternedData,
        data: trace_packet::Data,
    ) -> TracePacket {
        let step = now - self.stamped;
        self.stamped = now;

        TracePacket {
            timestamp: Some(step),
            optional_trusted_packet_sequence_id: Some(sequenced(self.id)),
            sequence_flags: Some(trace_packet::SequenceFlags::SeqNeedsIncrementalState as u32),
            interned_data: (interned != InternedData::default()).then_some(interned),
            data: Some(data),
            ..TracePacket::default()
        }
    }

    /// An event of `callsite` carrying `fields`, its name, target, annotation names and string values interned on this sequence, what is interned for the first time added to `interned`.
    fn event(
        &mut self,
        callsite: &Metadata<'_>,
        fields: &[Value],
        interned: &mut InternedData,
    ) -> TrackEvent {
        let name = self.names.id(callsite.name(), |iid, name| {
            interned.event_names.push(EventName {
                iid: Some(iid),
                name: Some(name.to_string()),
            });
        });
        let category = self.categories.id(callsite.target(), |iid, name| {
            interned.event_categories.push(EventCategory {
                iid: Some(iid),
                name: Some(name.to_string()),
            });
        });
        let debug_annotations = fields
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

        TrackEvent {
            name_field: Some(track_event::NameField::NameIid(name)),
            category_iids: vec![category],
            debug_annotations,
            ..TrackEvent::default()
        }
    }

    /// `counts` in the order this sequence's defaults name their tracks, [`Counter::ALL`]'s: the thread's two as steps from what its last slice carried, which only grow, and the process's two whole.
    fn carried(&mut self, counts: Counts) -> Vec<i64> {
        let allocated = counts.allocated - self.allocated;
        let allocations = counts.allocations - self.allocations;
        self.allocated = counts.allocated;
        self.allocations = counts.allocations;

        vec![
            allocated as i64,
            allocations as i64,
            counts.live as i64,
            counts.peak as i64,
        ]
    }
}

/// The packet sequence `id` names.
fn sequenced(id: u32) -> trace_packet::OptionalTrustedPacketSequenceId {
    trace_packet::OptionalTrustedPacketSequenceId::TrustedPacketSequenceId(id)
}

/// Where packets go, and `None` once a write to it failed: nothing is written after the first write that fails.
struct Out(Option<Box<dyn Write + Send>>);

impl Out {
    /// One packet, written as a `Trace` of that packet alone, which a stream of them concatenates into one `Trace`, and flushed before the step after it runs, since a buffer is lost to anything that ends the process without unwinding — a stack overflow, an abort, a `SIGKILL` — and those are the runs whose last packets are worth the most. The first write that fails drops the writer, and nothing is written after it.
    fn put(&mut self, packet: TracePacket) {
        let Some(writer) = &mut self.0 else {
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
            self.0 = None;
        }
    }
}

/// Ids a sequence gave the strings it interned of one kind, from one, each written once.
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
