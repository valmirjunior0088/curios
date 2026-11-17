//! The stored-unit framing, and what a record says about where a unit came from.

use {super::*, std::path::Path};

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
    let truncated = &slot[..PREAMBLE + 3];

    assert_eq!(segments(truncated), None);
}

/// A reader deciding whether to believe a slot reads its preamble and its record and nothing behind them.
#[test]
fn a_slots_record_is_found_from_its_preamble_alone() {
    let slot = framed(b"the record", b"the artifact");
    let preamble = slot
        .first_chunk::<PREAMBLE>()
        .expect("a slot opens with its preamble");

    assert_eq!(record_length(preamble), Some(b"the record".len()));
    assert_eq!(record_length(&[0; PREAMBLE]), None);
}

/// A record names each file by its name, in the order of the names whatever order the files were read in, and a file read for no package leaves no record to name it.
#[test]
fn a_record_names_its_reads_by_name_and_none_read_for_no_package() {
    let named = |path: &str, text: &str| {
        let name = SourceName::of("app", Path::new(path)).expect("a plain path");
        let file = PathBuf::from("/w/app").join(path);

        (
            file.clone(),
            Arc::new(Source::of(Some(name), Some(file), text)),
        )
    };
    let reads = [named("util.crs", "b"), named("lib.crs", "a")];

    assert_eq!(
        recorded(&reads).expect("every read is named"),
        [
            (
                SourceName::of("app", Path::new("lib.crs")).unwrap(),
                digest(b"a")
            ),
            (
                SourceName::of("app", Path::new("util.crs")).unwrap(),
                digest(b"b")
            ),
        ]
    );

    let loose = (
        PathBuf::from("/w/scratch.crs"),
        Source::held("/w/scratch.crs", "c"),
    );
    assert_eq!(recorded(&[reads[0].clone(), loose]), None);
}
