//! What the writer puts in the file: the row shapes, the fields a span and an event carry, the escaping, and the rotation that leaves the surviving file readable on its own.

use {
    super::*,
    std::{
        io::Read,
        sync::{Arc, Mutex as StdMutex},
    },
};

/// A destination that keeps its rows in memory, so a test can read what was written without a file.
#[derive(Clone, Default)]
struct Buffer(Arc<StdMutex<Vec<u8>>>);

impl Buffer {
    fn rows(&self) -> String {
        String::from_utf8(self.0.lock().expect("buffer lock").clone()).expect("rows are utf-8")
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

fn kinds(rows: &str) -> Vec<&str> {
    rows.lines()
        .filter_map(|row| row.split('\t').next())
        .collect()
}

// One span's life, in the order the file states it: the header, the callsite named once, then creation, entry, exit and close. The close carries no readings, which is what distinguishes it from a boundary.
#[test]
fn a_span_is_written_as_creation_entry_exit_and_close() {
    let buffer = Buffer::default();
    trace(buffer.destination(), || {
        let _span = tracing::trace_span!("outer").entered();
    })
    .expect("a stream destination opens");

    let rows = buffer.rows();
    assert_eq!(kinds(&rows), vec!["H", "D", "S", "E", "X", "C"]);
    assert!(
        rows.lines()
            .any(|row| row.starts_with("D\t0\t") && row.ends_with("\touter")),
        "{rows}"
    );
}

// A callsite is named once however many spans it opens: the `D` row is the definition and every later row refers to it by index. This is what keeps a row narrow enough to write 350 thousand of them.
#[test]
fn a_callsite_is_defined_once_and_referred_to_by_index() {
    let buffer = Buffer::default();
    trace(buffer.destination(), || {
        for _ in 0..3 {
            let _span = tracing::trace_span!("repeated").entered();
        }
    })
    .expect("a stream destination opens");

    let rows = buffer.rows();
    assert_eq!(kinds(&rows).iter().filter(|kind| **kind == "D").count(), 1);
    assert_eq!(kinds(&rows).iter().filter(|kind| **kind == "S").count(), 3);
    for row in rows.lines().filter(|row| row.starts_with('S')) {
        assert_eq!(row.split('\t').nth(2), Some("0"), "{row}");
    }
}

// Every field is written, whatever its type and whatever its name. The collector this replaced visited exactly one field of one event and dropped the rest, so a field added at a call site was silently discarded; the point of the stream is that it cannot be.
#[test]
fn every_field_a_span_or_an_event_carries_is_written() {
    let buffer = Buffer::default();
    trace(buffer.destination(), || {
        let _span = tracing::trace_span!("grouped", group = %"Nat/add", extra = 7).entered();
        tracing::event!(name: "magnitude", target: "test", tracing::Level::TRACE, value = 42_u64);
    })
    .expect("a stream destination opens");

    let rows = buffer.rows();
    let created = rows
        .lines()
        .find(|row| row.starts_with('S'))
        .expect("a creation row");
    assert!(created.contains("\tgroup=Nat/add"), "{created}");
    assert!(created.contains("\textra=7"), "{created}");

    let event = rows
        .lines()
        .find(|row| row.starts_with('V'))
        .expect("an event row");
    assert!(event.contains("\tvalue=42"), "{event}");
}

// A tab or a newline inside a value would end the row or the record; both are escaped on the way out, and `fold::unescape` is what puts them back.
#[test]
fn a_field_holding_a_tab_or_a_newline_is_escaped() {
    let buffer = Buffer::default();
    trace(buffer.destination(), || {
        let _span = tracing::trace_span!("awkward", group = %"a\tb\nc\\d").entered();
    })
    .expect("a stream destination opens");

    let rows = buffer.rows();
    let created = rows
        .lines()
        .find(|row| row.starts_with('S'))
        .expect("a creation row");
    assert!(created.contains(r"group=a\tb\nc\\d"), "{created}");
    assert_eq!(rows.lines().count(), 6, "{rows}");
}

// An entry and an exit carry the allocator's four readings, which is what a fold differences into a duration and a byte count. Falsifiable only because this crate's test binary installs `CountingAllocator`; see `count.rs`.
#[test]
fn a_boundary_carries_the_allocator_readings() {
    let buffer = Buffer::default();
    trace(buffer.destination(), || {
        let _span = tracing::trace_span!("holds").entered();
        let held = vec![0_u8; 4 * 1024 * 1024];

        drop(held);
    })
    .expect("a stream destination opens");

    let rows = buffer.rows();
    for kind in ['E', 'X'] {
        let row = rows
            .lines()
            .find(|row| row.starts_with(kind))
            .unwrap_or_else(|| panic!("a {kind} row"));
        assert_eq!(row.split('\t').count(), 8, "{row}");
    }
}

// The file that survives a rotation opens with the header and the whole callsite table, so it is readable without the one that was discarded. That is the property that makes rotation preferable to a cap: a hang's tail is what names the loop it is stuck in.
#[test]
fn a_rotation_restates_the_header_and_the_callsite_table() {
    let directory =
        std::env::temp_dir().join(format!("curios-profile-rotation-{}", std::process::id()));
    std::fs::create_dir_all(&directory).expect("a temporary directory");
    let path = directory.join("profile.tsv");

    trace(
        Destination::Rotating {
            path: path.clone(),
            cap: 256,
        },
        || {
            for _ in 0..200 {
                let _span = tracing::trace_span!("churn").entered();
            }
        },
    )
    .expect("a rotating destination opens");

    let mut current = String::new();
    File::open(&path)
        .expect("the current file")
        .read_to_string(&mut current)
        .expect("the current file reads");

    let mut previous = path.clone().into_os_string();
    previous.push(".prev");
    assert!(PathBuf::from(&previous).exists(), "a previous file");

    assert!(current.starts_with("H\t1\t"), "{current}");
    assert!(
        current.lines().any(|row| row.starts_with("D\t0\t")),
        "the table is restated: {current}"
    );

    std::fs::remove_dir_all(&directory).expect("the temporary directory is removed");
}
