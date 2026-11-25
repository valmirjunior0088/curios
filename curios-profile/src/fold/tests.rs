//! What a fold recomputes from packets: the per-span aggregates, the distributions beside them, the two things only a stream can say — what a stream cut short still yields, and what a killed run was inside — each thread's sequence and track kept apart, a stream that is not the recorder's refused, and a filed stream read back from its path.

use {
    super::*,
    crate::perfetto::{
        CounterDescriptor, DebugAnnotation, DebugAnnotationName, EventCategory, EventName,
        InternedData, InternedString, ThreadDescriptor, Trace, TracePacketDefaults,
        TrackDescriptor, TrackEventDefaults, clock_snapshot,
    },
    curios_common::test_support::*,
    std::fs,
};

/// The track a synthetic stream's first thread records on, and a second thread's; each thread's two counters take the ids after its track.
const THREAD: u64 = 16;
const OTHER: u64 = 19;

/// The tracks of the process's two gauges.
const LIVE: u64 = 2;
const PEAK: u64 = 3;

/// The incremental clock each thread's sequence is stamped on.
const STEPS: u32 = 64;

/// A field a synthetic slice carries.
enum Field<'a> {
    Text(&'a str),
    Number(u64),
}

/// What a synthetic stream keeps of one thread's sequence as it writes it: its id, the strings interned on it, the time its last packet stood at, and its counters as its last slice carried them.
struct Written {
    id: u32,
    interned: HashMap<(u8, String), u64>,
    stamped: u64,
    allocated: i64,
    allocations: i64,
}

/// A stream written packet by packet as the recorder writes one: the process's gauges described on its own sequence, and each of two threads on a sequence of its own, opened with its defaults and its step clock, its track and its two counters described. Each name and string is interned in the packet that first uses it. A test states its slices' times and readings whole, and the stream writes what the recorder would: each time as a step on its thread's clock, each thread counter as a step from its last reading.
struct Stream {
    packets: Vec<TracePacket>,
    threads: HashMap<u64, Written>,
}

impl Stream {
    fn new() -> Self {
        let mut stream = Self {
            packets: Vec::new(),
            threads: HashMap::new(),
        };
        stream.packets.push(TracePacket {
            optional_trusted_packet_sequence_id: Some(sequenced(1)),
            sequence_flags: Some(trace_packet::SequenceFlags::SeqIncrementalStateCleared as u32),
            ..TracePacket::default()
        });
        for (uuid, counter) in [(LIVE, Counter::Live), (PEAK, Counter::Peak)] {
            stream
                .packets
                .push(described(1, counter_track(uuid, counter, false)));
        }
        for (id, track, name) in [(2, THREAD, "main"), (3, OTHER, "other")] {
            stream.packets.push(TracePacket {
                timestamp: Some(0),
                timestamp_clock_id: Some(STREAM_CLOCK),
                optional_trusted_packet_sequence_id: Some(sequenced(id)),
                sequence_flags: Some(
                    trace_packet::SequenceFlags::SeqIncrementalStateCleared as u32,
                ),
                trace_packet_defaults: Some(TracePacketDefaults {
                    timestamp_clock_id: Some(STEPS),
                    track_event_defaults: Some(TrackEventDefaults {
                        track_uuid: Some(track),
                        extra_counter_track_uuids: vec![track + 1, track + 2, LIVE, PEAK],
                    }),
                }),
                data: Some(trace_packet::Data::ClockSnapshot(ClockSnapshot {
                    clocks: vec![
                        clock_snapshot::Clock {
                            clock_id: Some(STREAM_CLOCK),
                            timestamp: Some(0),
                            is_incremental: None,
                        },
                        clock_snapshot::Clock {
                            clock_id: Some(STEPS),
                            timestamp: Some(0),
                            is_incremental: Some(true),
                        },
                    ],
                })),
                ..TracePacket::default()
            });
            stream.packets.push(described(
                id,
                TrackDescriptor {
                    uuid: Some(track),
                    thread: Some(ThreadDescriptor {
                        thread_name: Some(name.to_string()),
                        ..ThreadDescriptor::default()
                    }),
                    ..TrackDescriptor::default()
                },
            ));
            stream.packets.push(described(
                id,
                counter_track(track + 1, Counter::Allocated, true),
            ));
            stream.packets.push(described(
                id,
                counter_track(track + 2, Counter::Allocations, true),
            ));
            stream.threads.insert(
                track,
                Written {
                    id,
                    interned: HashMap::new(),
                    stamped: 0,
                    allocated: 0,
                    allocations: 0,
                },
            );
        }
        stream
    }

    /// The event of `name` that `track`'s thread writes at `at`: of `kind`, carrying `fields`, and `readings` — live bytes, allocated bytes, allocations and the peak, as the allocator stood — where the allocator counted.
    fn event(
        mut self,
        track: u64,
        at: u64,
        kind: track_event::Type,
        name: &str,
        fields: &[(&str, Field<'_>)],
        readings: Option<[i64; 4]>,
    ) -> Self {
        let thread = self
            .threads
            .get_mut(&track)
            .expect("a thread of the stream");
        let mut interned = InternedData::default();
        let name = thread.iid(&mut interned, 0, name);
        let category = thread.iid(&mut interned, 1, "curios_test");
        let debug_annotations = fields
            .iter()
            .map(|(field, value)| DebugAnnotation {
                name_field: Some(debug_annotation::NameField::NameIid(thread.iid(
                    &mut interned,
                    2,
                    field,
                ))),
                value: Some(match value {
                    Field::Text(text) => {
                        debug_annotation::Value::StringValueIid(thread.iid(&mut interned, 3, text))
                    }
                    Field::Number(number) => debug_annotation::Value::UintValue(*number),
                }),
            })
            .collect();
        let extra_counter_values =
            readings.map_or(Vec::new(), |[live, allocated, allocations, peak]| {
                let steps = vec![
                    allocated - thread.allocated,
                    allocations - thread.allocations,
                    live,
                    peak,
                ];
                thread.allocated = allocated;
                thread.allocations = allocations;
                steps
            });
        let step = at - thread.stamped;
        thread.stamped = at;
        let packet = TracePacket {
            timestamp: Some(step),
            optional_trusted_packet_sequence_id: Some(sequenced(thread.id)),
            interned_data: (interned != InternedData::default()).then_some(interned),
            data: Some(trace_packet::Data::TrackEvent(TrackEvent {
                r#type: Some(kind as i32),
                name_field: Some(track_event::NameField::NameIid(name)),
                category_iids: vec![category],
                debug_annotations,
                extra_counter_values,
            })),
            ..TracePacket::default()
        };
        self.packets.push(packet);
        self
    }

    fn begin(self, at: u64, name: &str) -> Self {
        self.event(THREAD, at, track_event::Type::SliceBegin, name, &[], None)
    }

    fn end(self, at: u64, name: &str) -> Self {
        self.event(THREAD, at, track_event::Type::SliceEnd, name, &[], None)
    }

    /// One slice of `name` on the first thread, from `at` to `at + elapsed`.
    fn slice(self, at: u64, elapsed: u64, name: &str) -> Self {
        self.begin(at, name).end(at + elapsed, name)
    }

    fn bytes(&self) -> Vec<u8> {
        Trace {
            packet: self.packets.clone(),
        }
        .encode_to_vec()
    }

    fn folded(&self) -> Profile {
        fold(self.bytes().as_slice()).expect("the stream folds")
    }
}

impl Written {
    /// The id of `text` among this sequence's strings of `kind` — an event name, a category, an annotation name, a string value — added to `interned` the first time.
    fn iid(&mut self, interned: &mut InternedData, kind: u8, text: &str) -> u64 {
        if let Some(&iid) = self.interned.get(&(kind, text.to_string())) {
            return iid;
        }
        let iid = self.interned.len() as u64 + 1;
        self.interned.insert((kind, text.to_string()), iid);
        match kind {
            0 => interned.event_names.push(EventName {
                iid: Some(iid),
                name: Some(text.to_string()),
            }),
            1 => interned.event_categories.push(EventCategory {
                iid: Some(iid),
                name: Some(text.to_string()),
            }),
            2 => interned.debug_annotation_names.push(DebugAnnotationName {
                iid: Some(iid),
                name: Some(text.to_string()),
            }),
            _ => interned
                .debug_annotation_string_values
                .push(InternedString {
                    iid: Some(iid),
                    str: Some(text.as_bytes().to_vec()),
                }),
        }

        iid
    }
}

fn sequenced(id: u32) -> trace_packet::OptionalTrustedPacketSequenceId {
    trace_packet::OptionalTrustedPacketSequenceId::TrustedPacketSequenceId(id)
}

/// A packet of sequence `id` describing `track`.
fn described(id: u32, track: TrackDescriptor) -> TracePacket {
    TracePacket {
        optional_trusted_packet_sequence_id: Some(sequenced(id)),
        data: Some(trace_packet::Data::TrackDescriptor(track)),
        ..TracePacket::default()
    }
}

/// The track of `counter` at `uuid`, its values written as steps where `stepped`.
fn counter_track(uuid: u64, counter: Counter, stepped: bool) -> TrackDescriptor {
    TrackDescriptor {
        uuid: Some(uuid),
        static_or_dynamic_name: Some(track_descriptor::StaticOrDynamicName::Name(
            counter.name().to_string(),
        )),
        counter: Some(CounterDescriptor {
            unit: None,
            is_incremental: stepped.then_some(true),
        }),
        ..TrackDescriptor::default()
    }
}

/// The summary `curios profile` gives of `report`'s stream, filed at `stream.pftrace`, drawn plain.
fn answered(report: &Profile) -> String {
    report.answer(Path::new("stream.pftrace"), &Style::plain())
}

fn row<'a>(report: &'a Profile, name: &str) -> &'a Summary {
    report
        .summaries
        .iter()
        .find(|summary| summary.name == name)
        .unwrap_or_else(|| panic!("the span {name} folds"))
}

// The round trip the whole design rests on: a stream written by `trace` folds back into per-span summaries, an outer span's extent covering the inner spans within it.
#[test]
fn a_written_stream_folds_back_into_its_summaries() {
    let buffer = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
    let sink = buffer.clone();
    crate::trace(
        crate::Destination::Stream(Box::new(Sink(sink))),
        &[],
        || {
            let _outer = tracing::trace_span!("outer").entered();
            for _ in 0..2 {
                let _inner = tracing::trace_span!("inner").entered();
            }
        },
    )
    .expect("a stream destination opens");

    let bytes = buffer.lock().expect("buffer lock").clone();
    let report = fold(bytes.as_slice()).expect("the stream folds");

    let outer = row(&report, "outer");
    let inner = row(&report, "inner");
    assert_eq!(outer.calls, 1);
    assert_eq!(inner.calls, 2);
    assert!(outer.total >= inner.total, "{outer:?} {inner:?}");
    assert!(outer.memory.is_some(), "the suite's allocator counts");
}

struct Sink(std::sync::Arc<std::sync::Mutex<Vec<u8>>>);

impl std::io::Write for Sink {
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
        self.0.lock().expect("buffer lock").extend_from_slice(bytes);

        Ok(bytes.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

// A span's group is what its slices aggregate under, so two spans of one name with different groups are two rows rather than an average that hides which item cost the time.
#[test]
fn spans_of_one_name_aggregate_under_their_group() {
    let report = Stream::new()
        .event(
            THREAD,
            0,
            track_event::Type::SliceBegin,
            "declaration",
            &[("group", Field::Text("Nat/add"))],
            None,
        )
        .event(
            THREAD,
            100,
            track_event::Type::SliceEnd,
            "declaration",
            &[],
            None,
        )
        .event(
            THREAD,
            100,
            track_event::Type::SliceBegin,
            "declaration",
            &[("group", Field::Text("Nat/mul"))],
            None,
        )
        .event(
            THREAD,
            900,
            track_event::Type::SliceEnd,
            "declaration",
            &[],
            None,
        )
        .folded();

    assert_eq!(report.summaries.len(), 2);
    assert_eq!(
        report.summaries[0].group.as_deref(),
        Some("Nat/mul"),
        "the costlier group sorts first"
    );
    assert_eq!(report.summaries[0].total, Duration::from_nanos(800));
    assert_eq!(report.summaries[1].total, Duration::from_nanos(100));
}

// The readings a slice carries are differenced, so what a span retained is what it still held at its end and what it allocated is everything it took while entered.
#[test]
fn the_readings_a_slice_carries_are_differenced_into_bytes() {
    let report = Stream::new()
        .event(
            THREAD,
            0,
            track_event::Type::SliceBegin,
            "held",
            &[],
            Some([1000, 5000, 10, 1000]),
        )
        .event(
            THREAD,
            500,
            track_event::Type::SliceEnd,
            "held",
            &[],
            Some([1400, 6000, 18, 2048]),
        )
        .folded();

    let memory = report.summaries[0]
        .memory
        .expect("the slice carries readings");
    assert_eq!(memory.retained, 400);
    assert_eq!(memory.allocated, 1000);
    assert_eq!(memory.allocations, 8);
    assert_eq!(report.peak, Some(2048));
}

// Live bytes count from where counting began, so a span that returns blocks taken before it reads them below zero, and retains less than nothing.
#[test]
fn a_span_returning_more_than_counting_saw_taken_retains_below_zero() {
    let report = Stream::new()
        .event(
            THREAD,
            0,
            track_event::Type::SliceBegin,
            "returns",
            &[],
            Some([-100, 0, 0, 0]),
        )
        .event(
            THREAD,
            500,
            track_event::Type::SliceEnd,
            "returns",
            &[],
            Some([-300, 0, 0, 0]),
        )
        .folded();

    assert_eq!(report.summaries[0].memory.expect("readings").retained, -200);
}

// A stream whose recorder's allocator did not count carries no readings, and its rows read no memory rather than memory of nought, which its summary says: no bytes taken, no peak.
#[test]
fn a_stream_with_no_readings_has_no_memory_figures() {
    let report = Stream::new().slice(0, 500, "uncounted").folded();

    assert!(report.summaries[0].memory.is_none());
    assert_eq!(report.peak, None);
    let answer = answered(&report);
    assert!(
        answer.contains("allocations not counted\n")
            && !answer.contains("taken")
            && !answer.contains("peak:"),
        "{answer}"
    );
}

// A magnitude site's distribution, which is the whole reason `sample!` exists beside a span: the count says how often and the spread says what it was handed.
#[test]
fn an_instant_carrying_a_value_folds_into_its_distribution() {
    let mut stream = Stream::new();
    for (at, value) in [(0, 3), (1, 11), (2, 1)] {
        stream = stream.event(
            THREAD,
            at,
            track_event::Type::Instant,
            "universe::finalize_store_len",
            &[("value", Field::Number(value))],
            None,
        );
    }
    let report = stream.folded();

    let sample = &report.samples[0];
    assert_eq!((sample.count, sample.total), (3, 15));
    assert_eq!((sample.min, sample.max), (1, 11));
    assert_eq!(sample.mean(), 5.0);
}

// An end a track holds no beginning for ends a slice the stream does not hold, and nothing is invented for it: the slices it does hold keep their own time, and the end credits no one.
#[test]
fn an_end_with_nothing_begun_on_its_track_is_read_as_nothing() {
    let report = Stream::new()
        .slice(100, 200, "certifier::convert")
        .end(900, "certifier::convert")
        .folded();

    let summary = &report.summaries[0];
    assert_eq!(summary.calls, 1);
    assert_eq!(summary.self_total, Duration::from_nanos(200));
}

// The case the record stream exists for. A killed run leaves its slices open, and what they name is the stack it was inside — reported outermost first, which reads as the path down to whatever it could not finish.
#[test]
fn the_spans_a_killed_run_left_open_are_reported_outermost_first() {
    let report = Stream::new()
        .begin(10, "compile_entrypoint")
        .event(
            THREAD,
            30,
            track_event::Type::SliceBegin,
            "elaborate_module_item",
            &[("group", Field::Text("Nat/add"))],
            None,
        )
        .begin(50, "universe::finalize")
        .folded();

    assert!(report.summaries.is_empty(), "nothing ended");
    let open = report
        .open
        .iter()
        .map(|span| span.name.as_str())
        .collect::<Vec<_>>();
    assert_eq!(
        open,
        vec![
            "compile_entrypoint",
            "elaborate_module_item",
            "universe::finalize"
        ]
    );
    assert_eq!(report.open[1].group.as_deref(), Some("Nat/add"));
}

// A span re-entered within itself is measured over each extent, a slice each: the inner extent is 100 and the outer 400, and both are counted, which is the same nesting rule an outer stage's row already follows.
#[test]
fn a_span_re_entered_within_itself_measures_each_extent() {
    let report = Stream::new()
        .begin(0, "nat::sum")
        .slice(100, 100, "nat::sum")
        .end(400, "nat::sum")
        .folded();

    assert_eq!(report.summaries[0].calls, 2);
    assert_eq!(report.summaries[0].total, Duration::from_nanos(500));
}

// Self time is what a row per judgment can be added up by: each span keeps only the part of its extent no span begun directly inside it on its track took, in time and in bytes alike.
#[test]
fn self_time_excludes_the_spans_entered_directly_inside() {
    let report = Stream::new()
        .event(
            THREAD,
            0,
            track_event::Type::SliceBegin,
            "certify_declaration",
            &[],
            Some([0, 0, 0, 0]),
        )
        .event(
            THREAD,
            100,
            track_event::Type::SliceBegin,
            "certifier::convert",
            &[],
            Some([0, 100, 0, 0]),
        )
        .event(
            THREAD,
            400,
            track_event::Type::SliceEnd,
            "certifier::convert",
            &[],
            Some([0, 400, 0, 0]),
        )
        .event(
            THREAD,
            500,
            track_event::Type::SliceBegin,
            "certifier::convert",
            &[],
            Some([0, 500, 0, 0]),
        )
        .event(
            THREAD,
            700,
            track_event::Type::SliceEnd,
            "certifier::convert",
            &[],
            Some([0, 600, 0, 0]),
        )
        .event(
            THREAD,
            1000,
            track_event::Type::SliceEnd,
            "certify_declaration",
            &[],
            Some([0, 1000, 0, 0]),
        )
        .folded();

    let declaration = row(&report, "certify_declaration");
    let convert = row(&report, "certifier::convert");
    let bytes = |summary: &Summary| {
        let memory = summary.memory.expect("readings");
        (memory.allocated, memory.self_allocated)
    };
    assert_eq!(declaration.total, Duration::from_nanos(1000));
    assert_eq!(declaration.self_total, Duration::from_nanos(500));
    assert_eq!(bytes(declaration), (1000, 600));
    assert_eq!(convert.total, Duration::from_nanos(500));
    assert_eq!(convert.self_total, Duration::from_nanos(500));
    assert_eq!(bytes(convert), (400, 400));
}

// The case inclusive time cannot serve: a span entered within itself counts the inner extent in both slices' totals, and once in their self times, so a recursive judgment's self row is the time it actually took.
#[test]
fn a_span_re_entered_within_itself_counts_each_nanosecond_once_in_its_self_time() {
    let report = Stream::new()
        .begin(0, "certifier::convert")
        .slice(200, 400, "certifier::convert")
        .end(1000, "certifier::convert")
        .folded();

    let summary = &report.summaries[0];
    assert_eq!(summary.total, Duration::from_nanos(1400));
    assert_eq!(summary.self_total, Duration::from_nanos(1000));
}

// Two threads' spans are slices on two tracks, each its own stack: spans that overlap in time without nesting keep their own extents, and neither is credited to the other.
#[test]
fn two_threads_read_as_two_tracks_each_with_its_own_stack() {
    let report = Stream::new()
        .event(THREAD, 0, track_event::Type::SliceBegin, "first", &[], None)
        .event(
            OTHER,
            100,
            track_event::Type::SliceBegin,
            "second",
            &[],
            None,
        )
        .event(THREAD, 300, track_event::Type::SliceEnd, "first", &[], None)
        .event(OTHER, 500, track_event::Type::SliceEnd, "second", &[], None)
        .folded();

    assert_eq!(row(&report, "first").self_total, Duration::from_nanos(300));
    assert_eq!(row(&report, "second").self_total, Duration::from_nanos(400));
}

// A slice is charged what its own thread took, each thread's counters read as steps on its own sequence, and what the process retained meanwhile, whichever thread took it: the other thread's 5,000 bytes, taken while the first slice was entered, are the second slice's and not the first's.
#[test]
fn a_slice_is_charged_its_own_thread_s_allocations() {
    let report = Stream::new()
        .event(
            THREAD,
            0,
            track_event::Type::SliceBegin,
            "first",
            &[],
            Some([0, 100, 1, 0]),
        )
        .event(
            OTHER,
            10,
            track_event::Type::SliceBegin,
            "second",
            &[],
            Some([0, 0, 0, 0]),
        )
        .event(
            OTHER,
            20,
            track_event::Type::SliceEnd,
            "second",
            &[],
            Some([5000, 5000, 9, 5000]),
        )
        .event(
            THREAD,
            30,
            track_event::Type::SliceEnd,
            "first",
            &[],
            Some([5000, 300, 3, 5000]),
        )
        .folded();

    let memory = |name: &str| {
        let memory = row(&report, name).memory.expect("readings");
        (memory.retained, memory.allocated, memory.allocations)
    };
    assert_eq!(memory("first"), (5000, 200, 2));
    assert_eq!(memory("second"), (5000, 5000, 9));
    assert_eq!(report.peak, Some(5000));
}

// The summary ranks span names by the time each took itself, every group of a name folded into it: self time partitions what the spans cover, so each name's share and the share so far add up to the whole, and a name that declared groups names its costliest.
#[test]
fn the_summary_ranks_span_names_by_their_own_time_with_their_groups_folded_in() {
    let report = Stream::new()
        .event(
            THREAD,
            0,
            track_event::Type::SliceBegin,
            "outer",
            &[],
            Some([0, 0, 0, 0]),
        )
        .event(
            THREAD,
            100,
            track_event::Type::SliceBegin,
            "inner",
            &[],
            Some([0, 100, 1, 100]),
        )
        .event(
            THREAD,
            700,
            track_event::Type::SliceEnd,
            "inner",
            &[],
            Some([50, 400, 3, 400]),
        )
        .event(
            THREAD,
            1000,
            track_event::Type::SliceEnd,
            "outer",
            &[],
            Some([20, 500, 4, 400]),
        )
        .event(
            THREAD,
            1000,
            track_event::Type::SliceBegin,
            "grouped",
            &[("group", Field::Text("Nat/add"))],
            None,
        )
        .event(
            THREAD,
            1200,
            track_event::Type::SliceEnd,
            "grouped",
            &[],
            None,
        )
        .event(
            THREAD,
            1200,
            track_event::Type::SliceBegin,
            "grouped",
            &[("group", Field::Text("Nat/mul"))],
            None,
        )
        .event(
            THREAD,
            1500,
            track_event::Type::SliceEnd,
            "grouped",
            &[],
            None,
        )
        .folded();

    let expected = [
        "stream.pftrace",
        "│  every span closed, allocations counted",
        "│  in spans: 1.5 µs on 2 threads, 500 B taken",
        "│  peak: 400 B",
        "│  spans: 3 names, each below",
        "├─ inner in curios_test",
        "│  self: 0.60 µs, 40.0%, 40.0% so far; in all: 0.60 µs; calls: 1; taken: 300 B",
        "├─ grouped in curios_test",
        "│  self: 0.50 µs, 33.3%, 73.3% so far; in all: 0.50 µs; calls: 2",
        "│  costliest group: Nat/mul, 0.30 µs",
        "├─ outer in curios_test",
        "│  self: 0.40 µs, 26.7%, 100.0% so far; in all: 1.00 µs; calls: 1; taken: 200 B",
        "└─ [trace] stream.pftrace",
    ]
    .join("\n");
    assert_eq!(answered(&report), format!("{expected}\n"));
}

// A summary lists the names that took the most time themselves and no more than ten, saying how many the stream holds, so a run of thousands of spans answers in a screenful.
#[test]
fn a_summary_lists_the_ten_names_that_took_the_most_time() {
    let mut stream = Stream::new();
    for n in 0..12 {
        stream = stream.slice(n * 100, 10 + n, &format!("span{n}"));
    }
    let answer = answered(&stream.folded());

    assert!(
        answer.contains("│  spans: 12 names, the 10 that took the most time themselves below\n"),
        "{answer}"
    );
    let listed = answer
        .lines()
        .filter_map(|line| line.strip_prefix("├─ "))
        .collect::<Vec<_>>();
    assert_eq!(listed.len(), SUMMARIZED, "{answer}");
    assert_eq!(listed[0], "span11 in curios_test");
    assert!(
        !answer.contains("span0 in") && !answer.contains("span1 in"),
        "{answer}"
    );
}

// A stopped run's summary says how many spans it was still inside and, a fact a thread, each thread's open stack, outermost first, with what each span declared and when it was entered: what a killed run was doing, which is what its stream is kept for.
#[test]
fn a_stopped_run_s_summary_states_each_thread_s_open_stack() {
    let report = Stream::new()
        .begin(10, "compile_entrypoint")
        .event(OTHER, 20, track_event::Type::SliceBegin, "serve", &[], None)
        .event(
            THREAD,
            30,
            track_event::Type::SliceBegin,
            "elaborate_module_item",
            &[("group", Field::Text("Nat/add"))],
            None,
        )
        .folded();

    let expected = [
        "stream.pftrace",
        "│  3 spans open, allocations not counted",
        "│  open on main:",
        "│    compile_entrypoint in curios_test, entered at 0.01 µs",
        "│    elaborate_module_item in curios_test, group Nat/add, entered at 0.03 µs",
        "│  open on other:",
        "│    serve in curios_test, entered at 0.02 µs",
        "└─ [trace] stream.pftrace",
    ]
    .join("\n");
    assert_eq!(answered(&report), format!("{expected}\n"));
}

// A packet cut short by a kill, or by a writer that stopped, is the normal ending of a stream, not a corruption: it is dropped, and everything before it still folds.
#[test]
fn a_packet_cut_short_is_dropped() {
    let stream = Stream::new().slice(0, 900, "nat::sum").event(
        THREAD,
        900,
        track_event::Type::Instant,
        "cut",
        &[("value", Field::Number(9))],
        None,
    );
    let mut bytes = stream.bytes();
    bytes.truncate(bytes.len() - 1);

    let report = fold(bytes.as_slice()).expect("the stream folds up to its last whole packet");
    assert_eq!(report.summaries[0].calls, 1);
    assert!(report.samples.is_empty());
}

// A packet that names what no packet before it defined is from a stream that is not this recorder's, or not whole from its first packet, and is refused rather than read with an invented name.
#[test]
fn a_name_used_before_it_was_defined_is_refused() {
    let mut stream = Stream::new().slice(0, 100, "defined");
    stream.packets.push(TracePacket {
        timestamp: Some(100),
        optional_trusted_packet_sequence_id: Some(sequenced(stream.threads[&THREAD].id)),
        data: Some(trace_packet::Data::TrackEvent(TrackEvent {
            r#type: Some(track_event::Type::SliceBegin as i32),
            name_field: Some(track_event::NameField::NameIid(99)),
            ..TrackEvent::default()
        })),
        ..TracePacket::default()
    });

    let refusal = fold(stream.bytes().as_slice()).expect_err("an undefined name is refused");
    assert_eq!(refusal.kind(), io::ErrorKind::InvalidData);
    assert!(refusal.to_string().contains("event name 99"), "{refusal}");
}

// A filed stream is read back from the path it was filed at: the one file is the whole of it.
#[test]
fn a_filed_stream_folds_from_its_path() {
    let directory = Temporary::new("profile", "filed");
    fs::create_dir_all(&directory).expect("the directory is created");
    let path = directory.join("profile.pftrace");

    fs::write(&path, Stream::new().slice(0, 700, "only").bytes()).expect("the stream is filed");

    let report = fold_at(&path).expect("the file folds");
    assert_eq!(
        report
            .summaries
            .iter()
            .map(|summary| summary.name.as_str())
            .collect::<Vec<_>>(),
        ["only"]
    );
}
