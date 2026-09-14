//! The stored-unit framing, and what a record says about where a unit came from.

use super::*;

fn of_reads(reads: &[&str]) -> Record {
    Record {
        reads: reads
            .iter()
            .map(|path| (path.to_string(), "digest".to_string()))
            .collect(),
        predecessors: Vec::new(),
        unit: "unit".to_string(),
    }
}

#[test]
fn a_slot_frames_its_record_ahead_of_the_artifact_and_reads_both_back() {
    let slot = framed(b"the record", b"the artifact");

    assert_eq!(
        segments(&slot),
        Some((b"the record".as_slice(), b"the artifact".as_slice()))
    );
}

#[test]
fn bytes_that_do_not_open_with_the_magic_are_not_a_slot() {
    assert_eq!(segments(b"an archive of something else"), None);
    assert_eq!(segments(b""), None);
}

#[test]
fn a_slot_truncated_past_its_record_is_not_read() {
    let slot = framed(b"the record", b"the artifact");
    let truncated = &slot[..MAGIC.len() + 8 + 3];

    assert_eq!(segments(truncated), None);
}

#[test]
fn a_record_names_the_directory_its_header_lies_in() {
    let record = of_reads(&["/w/std/lib.crs", "/w/std/Nat.crs", "/w/std/Nat/add.crs"]);

    assert_eq!(record.directory(), Some(Path::new("/w/std")));
}

#[test]
fn a_record_of_no_reads_names_no_directory() {
    assert_eq!(of_reads(&[]).directory(), None);
}

/// Containment is checked per read against every directory, so one read outside all of them is enough to refuse the record.
#[test]
fn a_read_outside_every_directory_is_not_within_them() {
    let record = of_reads(&["/w/std/lib.crs", "/elsewhere/std/Nat.crs"]);
    let directories = [Path::new("/w/std"), Path::new("/w/other")];

    assert!(!read_within(&directories, &record.reads));
    assert!(read_within(
        &directories,
        &of_reads(&["/w/std/lib.crs", "/w/other/lib.crs"]).reads
    ));
    assert!(read_within(&directories, &of_reads(&[]).reads));
}
