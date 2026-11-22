//! What the writer puts in the file: a span's slices on its thread's track, a name and a string interned once, the fields a span and an event carry, the allocator's readings where it counts, a filed stream kept whole and started over, and a stream ended at the first write that fails.

use {
    super::*,
    curios_common::test_support::*,
    std::{
        io::Read,
        sync::{Arc, Mutex},
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

// One span's life, in the order the file states it: the sequence cleared and the process described first, then the thread's track, the span's name interned, and a slice begun and ended on that track.
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
    let kinds = events(&packets)
        .into_iter()
        .map(|event| (event.r#type(), event.track_uuid()))
        .collect::<Vec<_>>();
    assert_eq!(
        kinds,
        [
            (track_event::Type::SliceBegin, thread),
            (track_event::Type::SliceEnd, thread)
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

// A slice's beginning and end carry the allocator's four readings, which a fold differences into a byte count. The counters are the process's, so what the span took shows as at least its own allocation between the two cumulative readings, whatever another test allocated beside it.
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
    assert_eq!(
        tracks(&packets)
            .iter()
            .filter(|track| track.counter.is_some())
            .count(),
        COUNTER_TRACKS.len()
    );
    let allocated = events(&packets)
        .into_iter()
        .map(|event| {
            assert_eq!(event.extra_counter_track_uuids, COUNTER_TRACKS);
            event.extra_counter_values[1]
        })
        .collect::<Vec<_>>();
    assert!(
        (allocated[1] - allocated[0]) as usize >= HELD,
        "the end reads what the span allocated: {allocated:?}"
    );
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
        events(&packets)
            .iter()
            .all(|event| event.extra_counter_values.is_empty())
    );
}

// Each thread that records is a track of its own, so the spans of two threads are slices on two tracks, never one stack.
#[test]
fn two_threads_record_on_two_tracks() {
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
        .collect::<Vec<_>>();
    assert_eq!(threads.len(), 2, "{threads:?}");
    let used = events(&packets)
        .iter()
        .map(|event| event.track_uuid())
        .collect::<std::collections::BTreeSet<_>>();
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
