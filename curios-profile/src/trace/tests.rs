//! What the writer puts in the file: a sequence a thread, opened with its defaults and stamped in steps, a span's slices on its thread's track, a name and a string interned once, the fields a span and an event carry, the allocator's readings where it counts and a thread's own allocations, a filed stream kept whole and started over, and a stream ended at the first write that fails.

use {
    super::*,
    curios_common::test_support::*,
    std::{
        collections::BTreeSet,
        io::Read,
        sync::{Arc, Mutex},
        time::Duration,
    },
};

/// A destination that keeps its bytes in memory, so a test can read what was written without a file.
#[derive(Clone, Default)]
struct Buffer(Arc<Mutex<Vec<u8>>>);

impl Buffer {
    fn packets(&self) -> Vec<TracePacket> {
        Trace::decode(self.0.lock().expect("buffer lock").as_slice())
            .expect("a whole stream is a trace")
            .packet
    }

    fn destination(&self) -> Destination {
        Destination::Stream(Box::new(self.clone()))
    }

    fn folded(&self) -> crate::Profile {
        crate::fold(self.0.lock().expect("buffer lock").as_slice()).expect("the stream folds")
    }
}

/// The sequence a packet is on.
fn sequence_of(packet: &TracePacket) -> u32 {
    match packet.optional_trusted_packet_sequence_id {
        Some(trace_packet::OptionalTrustedPacketSequenceId::TrustedPacketSequenceId(id)) => id,
        None => panic!("every packet names its sequence"),
    }
}

/// The packets that open a thread's sequence, each with the defaults it states.
fn opened(packets: &[TracePacket]) -> Vec<(u32, &TrackEventDefaults)> {
    packets
        .iter()
        .filter_map(|packet| {
            let defaults = packet.trace_packet_defaults.as_ref()?;
            Some((
                sequence_of(packet),
                defaults
                    .track_event_defaults
                    .as_ref()
                    .expect("a thread's defaults name its track"),
            ))
        })
        .collect()
}

impl Write for Buffer {
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
        self.0.lock().expect("buffer lock").extend_from_slice(bytes);

        Ok(bytes.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

fn events(packets: &[TracePacket]) -> Vec<&TrackEvent> {
    packets
        .iter()
        .filter_map(|packet| match &packet.data {
            Some(trace_packet::Data::TrackEvent(event)) => Some(event),
            _ => None,
        })
        .collect()
}

fn tracks(packets: &[TracePacket]) -> Vec<&TrackDescriptor> {
    packets
        .iter()
        .filter_map(|packet| match &packet.data {
            Some(trace_packet::Data::TrackDescriptor(track)) => Some(track),
            _ => None,
        })
        .collect()
}

fn interned(packets: &[TracePacket]) -> InternedData {
    let mut all = InternedData::default();
    for interned in packets
        .iter()
        .filter_map(|packet| packet.interned_data.clone())
    {
        all.event_names.extend(interned.event_names);
        all.event_categories.extend(interned.event_categories);
        all.debug_annotation_names
            .extend(interned.debug_annotation_names);
        all.debug_annotation_string_values
            .extend(interned.debug_annotation_string_values);
    }
    all
}

// One span's life, in the order the file states it: the process's sequence cleared and the process described first, then the thread's sequence opened with its track as its events' default, the span's name interned, and a slice begun and ended on that sequence, which names no track of its own.
#[test]
fn a_span_is_written_as_a_slice_begun_and_ended_on_its_thread_s_track() {
    let buffer = Buffer::default();
    trace(buffer.destination(), || {
        let _span = tracing::trace_span!("outer").entered();
    })
    .expect("a stream destination opens");

    let packets = buffer.packets();
    assert_eq!(
        packets[0].sequence_flags,
        Some(trace_packet::SequenceFlags::SeqIncrementalStateCleared as u32)
    );
    let thread = tracks(&packets)
        .into_iter()
        .find(|track| track.thread.is_some())
        .expect("the thread's track")
        .uuid();
    let [(sequence, defaults)] = opened(&packets)[..] else {
        panic!("one thread's sequence opens");
    };
    assert_eq!(defaults.track_uuid, Some(thread));
    let kinds = packets
        .iter()
        .filter_map(|packet| match &packet.data {
            Some(trace_packet::Data::TrackEvent(event)) => {
                Some((event.r#type(), sequence_of(packet)))
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        kinds,
        [
            (track_event::Type::SliceBegin, sequence),
            (track_event::Type::SliceEnd, sequence)
        ]
    );
    assert_eq!(
        interned(&packets)
            .event_names
            .iter()
            .map(|name| name.name())
            .collect::<Vec<_>>(),
        ["outer"]
    );
}

// A name is interned once however many spans it names: every later slice refers to it by its id, which keeps every other packet narrow.
#[test]
fn a_name_is_interned_once_however_many_spans_it_names() {
    let buffer = Buffer::default();
    trace(buffer.destination(), || {
        for _ in 0..3 {
            let _span = tracing::trace_span!("repeated").entered();
        }
    })
    .expect("a stream destination opens");

    let packets = buffer.packets();
    assert_eq!(interned(&packets).event_names.len(), 1);
    assert_eq!(
        events(&packets)
            .iter()
            .filter(|event| event.r#type() == track_event::Type::SliceBegin)
            .count(),
        3
    );
}

// Every field is written, whatever its type and whatever its name, so a field added at a call site cannot be silently discarded; and a magnitude is written as `sample!` writes it, an instant carrying `value`.
#[test]
fn every_field_a_span_or_an_event_carries_is_written() {
    let buffer = Buffer::default();
    trace(buffer.destination(), || {
        let _span = tracing::trace_span!("grouped", group = %"Nat/add", extra = 7).entered();
        curios_common::sample!("magnitude", 42);
    })
    .expect("a stream destination opens");

    let packets = buffer.packets();
    let interned = interned(&packets);
    let name = |iid: u64| {
        interned
            .debug_annotation_names
            .iter()
            .find(|name| name.iid() == iid)
            .map(|name| name.name().to_string())
            .expect("an interned annotation name")
    };
    let text = |iid: u64| {
        interned
            .debug_annotation_string_values
            .iter()
            .find(|text| text.iid() == iid)
            .map(|text| String::from_utf8_lossy(text.str()).into_owned())
            .expect("an interned string")
    };
    let written = events(&packets)
        .into_iter()
        .filter(|event| event.r#type() != track_event::Type::SliceEnd)
        .flat_map(|event| event.debug_annotations.iter())
        .map(|annotation| {
            let Some(debug_annotation::NameField::NameIid(iid)) = annotation.name_field else {
                panic!("an annotation's name is interned");
            };
            let value = match annotation.value {
                Some(debug_annotation::Value::StringValueIid(iid)) => text(iid),
                Some(debug_annotation::Value::UintValue(value)) => value.to_string(),
                Some(debug_annotation::Value::IntValue(value)) => value.to_string(),
                ref other => panic!("an unexpected value {other:?}"),
            };
            (name(iid), value)
        })
        .collect::<Vec<_>>();
    assert_eq!(
        written,
        [
            ("group".to_string(), "Nat/add".to_string()),
            ("extra".to_string(), "7".to_string()),
            ("value".to_string(), "42".to_string()),
        ]
    );
}

// A string value many spans carry is written once, and each slice refers to it by its id: a term a span carries, settled again and again, costs the stream its text once.
#[test]
fn a_string_value_many_spans_carry_is_written_once() {
    let buffer = Buffer::default();
    trace(buffer.destination(), || {
        for _ in 0..3 {
            let _span = tracing::trace_span!("settled", key = %"the same term").entered();
        }
    })
    .expect("a stream destination opens");

    let packets = buffer.packets();
    let strings = interned(&packets).debug_annotation_string_values;
    assert_eq!(strings.len(), 1);
    assert_eq!(strings[0].str(), b"the same term");
}

// A slice's beginning and end carry the allocator's four readings, one for each counter track its sequence's defaults name, the thread's two written as steps: the end's step of allocated bytes is what the thread took while the span was entered, its own allocation and none of the recorder's.
#[test]
fn a_slice_carries_the_allocator_readings() {
    const HELD: usize = 4 * 1024 * 1024;

    let buffer = Buffer::default();
    trace(buffer.destination(), || {
        let _span = tracing::trace_span!("holds").entered();
        let held = vec![0_u8; HELD];

        drop(held);
    })
    .expect("a stream destination opens");

    let packets = buffer.packets();
    let counters = tracks(&packets)
        .into_iter()
        .filter(|track| track.counter.is_some())
        .map(|track| track.uuid())
        .collect::<BTreeSet<_>>();
    assert_eq!(counters.len(), Counter::ALL.len());
    let [(_, defaults)] = opened(&packets)[..] else {
        panic!("one thread's sequence opens");
    };
    assert_eq!(
        defaults
            .extra_counter_track_uuids
            .iter()
            .copied()
            .collect::<BTreeSet<_>>(),
        counters
    );
    let ended = events(&packets)
        .into_iter()
        .find(|event| event.r#type() == track_event::Type::SliceEnd)
        .expect("the slice ends");
    assert_eq!(ended.extra_counter_values.len(), Counter::ALL.len());
    assert_eq!(
        ended.extra_counter_values[0] as usize, HELD,
        "the end's step reads what the span allocated: {:?}",
        ended.extra_counter_values
    );
}

// No span is charged the recorder's own work: one whose work allocates nothing, entered around spans and samples whose work allocates nothing either, keeps no byte and no trip through the allocator, whatever the recorder took to write them down.
#[test]
fn a_span_is_charged_nothing_the_recorder_takes() {
    let buffer = Buffer::default();
    trace(buffer.destination(), || {
        let _span = tracing::trace_span!("around").entered();
        for round in 0..64_u64 {
            let _inside = tracing::trace_span!("inside", round).entered();
            curios_common::sample!("inside::round", round);
        }
    })
    .expect("a stream destination opens");

    let report = buffer.folded();
    for name in ["around", "inside"] {
        let memory = report
            .summaries
            .iter()
            .find(|summary| summary.name == name)
            .and_then(|summary| summary.memory)
            .unwrap_or_else(|| panic!("the span {name} folds with its memory"));
        assert_eq!((memory.allocated, memory.allocations), (0, 0), "{name}");
    }
}

// A span is charged what its own thread took, read back through the stream: another thread's allocation made while it is entered is that thread's span's, and what its thread took before it entered is no part of it.
#[test]
fn a_span_is_charged_what_its_own_thread_allocated() {
    const HELD: usize = 4 * 1024 * 1024;

    let buffer = Buffer::default();
    trace(buffer.destination(), || {
        let dispatch = tracing::dispatcher::get_default(Clone::clone);
        drop(std::hint::black_box(vec![0_u8; 2 * HELD]));
        let _span = tracing::trace_span!("here").entered();
        std::thread::scope(|scope| {
            scope.spawn(|| {
                tracing::dispatcher::with_default(&dispatch, || {
                    let _span = tracing::trace_span!("there").entered();
                    drop(std::hint::black_box(vec![0_u8; HELD]));
                });
            });
        });
    })
    .expect("a stream destination opens");

    let report = buffer.folded();
    let allocated = |name: &str| {
        report
            .summaries
            .iter()
            .find(|summary| summary.name == name)
            .and_then(|summary| summary.memory)
            .map(|memory| memory.allocated as usize)
            .unwrap_or_else(|| panic!("the span {name} folds with its memory"))
    };
    assert!(allocated("there") >= HELD, "{}", allocated("there"));
    assert!(allocated("here") < HELD, "{}", allocated("here"));
}

// A thread's packets are stamped as steps on its sequence's clock, each from the one before: summed from where the sequence's opening packet stands, they reach the time its last packet was written, at least what the run slept and at most what it took.
#[test]
fn a_thread_s_packets_are_stamped_as_steps_from_the_one_before() {
    const SLEPT: Duration = Duration::from_millis(20);

    let buffer = Buffer::default();
    let began = Instant::now();
    trace(buffer.destination(), || {
        for _ in 0..2 {
            let _span = tracing::trace_span!("sleeps").entered();
            std::thread::sleep(SLEPT);
        }
    })
    .expect("a stream destination opens");
    let took = began.elapsed();

    let packets = buffer.packets();
    let [(sequence, _)] = opened(&packets)[..] else {
        panic!("one thread's sequence opens");
    };
    let last = packets
        .iter()
        .filter(|packet| sequence_of(packet) == sequence)
        .map(TracePacket::timestamp)
        .sum::<u64>();
    assert!(Duration::from_nanos(last) >= 2 * SLEPT, "{last}");
    assert!(Duration::from_nanos(last) <= took, "{last} past {took:?}");
}

// A recorder whose allocator does not count describes no counter track and carries no reading, so its stream reads no memory rather than memory of nought.
#[test]
fn a_recorder_whose_allocator_does_not_count_carries_no_reading() {
    let buffer = Buffer::default();
    let recorder = Recorder::new(buffer.destination(), false).expect("a stream destination opens");
    tracing::subscriber::with_default(recorder, || {
        let _span = tracing::trace_span!("uncounted").entered();
    });

    let packets = buffer.packets();
    assert!(tracks(&packets).iter().all(|track| track.counter.is_none()));
    assert!(
        opened(&packets)
            .iter()
            .all(|(_, defaults)| defaults.extra_counter_track_uuids.is_empty())
    );
    assert!(
        events(&packets)
            .iter()
            .all(|event| event.extra_counter_values.is_empty())
    );
}

// Each thread that records opens a sequence of its own, whose defaults name a track of its own, so the spans of two threads are slices on two tracks, never one stack.
#[test]
fn two_threads_record_on_two_sequences_and_two_tracks() {
    let buffer = Buffer::default();
    trace(buffer.destination(), || {
        let dispatch = tracing::dispatcher::get_default(Clone::clone);
        let _span = tracing::trace_span!("here").entered();
        std::thread::scope(|scope| {
            scope.spawn(|| {
                tracing::dispatcher::with_default(&dispatch, || {
                    let _span = tracing::trace_span!("there").entered();
                });
            });
        });
    })
    .expect("a stream destination opens");

    let packets = buffer.packets();
    let threads = tracks(&packets)
        .iter()
        .filter(|track| track.thread.is_some())
        .map(|track| track.uuid())
        .collect::<BTreeSet<_>>();
    assert_eq!(threads.len(), 2, "{threads:?}");
    let opened = opened(&packets);
    assert_eq!(
        opened
            .iter()
            .filter_map(|(_, defaults)| defaults.track_uuid)
            .collect::<BTreeSet<_>>(),
        threads
    );
    let used = packets
        .iter()
        .filter(|packet| matches!(packet.data, Some(trace_packet::Data::TrackEvent(_))))
        .map(sequence_of)
        .collect::<BTreeSet<_>>();
    assert_eq!(
        used,
        opened
            .iter()
            .map(|(sequence, _)| *sequence)
            .collect::<BTreeSet<_>>()
    );
    assert_eq!(used.len(), 2, "{used:?}");
}

// A filed stream is one file and holds every packet its run wrote, however many: nothing is set aside while the run goes on, so what a reader counts in the file is what happened. The directories that lead to the file are made with it.
#[test]
fn a_filed_stream_keeps_every_packet_its_run_wrote() {
    const SPANS: usize = 2_000;
    let directory = Temporary::new("profile", "whole");
    let path = directory.join("nested").join("profile.pftrace");

    trace(Destination::File(path.clone()), || {
        for _ in 0..SPANS {
            let _span = tracing::trace_span!("churn").entered();
        }
    })
    .expect("a file destination opens");

    let mut bytes = Vec::new();
    File::open(&path)
        .expect("the stream's file")
        .read_to_end(&mut bytes)
        .expect("the stream reads");
    let packets = Trace::decode(bytes.as_slice())
        .expect("a whole stream is a trace")
        .packet;

    assert_eq!(
        events(&packets)
            .iter()
            .filter(|event| event.r#type() == track_event::Type::SliceBegin)
            .count(),
        SPANS
    );
    assert_eq!(
        std::fs::read_dir(path.parent().expect("the file has a directory"))
            .expect("the directory lists")
            .count(),
        1,
        "the stream is the one file"
    );
}

// A file destination starts its stream over: an earlier run's packets left at the path would be folded in front of this run's, a certification's reported ahead of the next one's.
#[test]
fn opening_a_file_destination_starts_the_stream_over() {
    let directory = Temporary::new("profile", "started-over");
    let path = directory.join("profile.pftrace");
    trace(Destination::File(path.clone()), || {
        let _span = tracing::trace_span!("earlier").entered();
    })
    .expect("a file destination opens");

    trace(Destination::File(path.clone()), || {
        let _span = tracing::trace_span!("later").entered();
    })
    .expect("a file destination opens");

    let names = crate::fold_at(&path)
        .expect("the stream folds")
        .summaries
        .into_iter()
        .map(|summary| summary.name)
        .collect::<Vec<_>>();
    assert_eq!(names, ["later"]);
}

/// A destination with room for `room` bytes: the write that would pass it is refused, as a full disk refuses one, and it counts each write asked of it after that.
#[derive(Clone)]
struct Filling {
    taken: Arc<Mutex<Vec<u8>>>,
    room: usize,
    refused: Arc<Mutex<bool>>,
    after: Arc<Mutex<usize>>,
}

impl Write for Filling {
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
        let mut refused = self.refused.lock().expect("refusal lock");
        if *refused {
            *self.after.lock().expect("count lock") += 1;
        }
        let mut taken = self.taken.lock().expect("buffer lock");
        if !*refused && taken.len() + bytes.len() > self.room {
            *refused = true;
            return Err(io::Error::other("the destination is full"));
        }
        taken.extend_from_slice(bytes);

        Ok(bytes.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

// The first write that fails ends the stream and the run goes on: nothing is written after it, so the file is a whole prefix of the run, read up to its last whole packet as a killed run's is, never a stream with packets missing from its middle.
#[test]
fn a_write_that_fails_ends_the_stream_and_the_run_goes_on() {
    let filling = Filling {
        taken: Arc::default(),
        room: 512,
        refused: Arc::default(),
        after: Arc::default(),
    };
    let ran = trace(Destination::Stream(Box::new(filling.clone())), || {
        for _ in 0..200 {
            let _span = tracing::trace_span!("fills", group = %"the disk").entered();
        }
        "returned"
    })
    .expect("a stream destination opens");

    assert_eq!(ran, "returned");
    assert!(*filling.refused.lock().expect("refusal lock"));
    assert_eq!(
        *filling.after.lock().expect("count lock"),
        0,
        "written past a failed write"
    );
    Trace::decode(filling.taken.lock().expect("buffer lock").as_slice())
        .expect("what was written is a whole prefix");
}
