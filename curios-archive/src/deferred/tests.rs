//! Several values' bytes laid out in one buffer, and read back as runs of it.

use super::{Bytes, joined, parted};

fn parts_of(bytes: Vec<u8>) -> Option<Vec<Vec<u8>>> {
    let parts = parted(&Bytes::from(bytes))?;

    Some(parts.iter().map(|part| part.as_slice().to_vec()).collect())
}

#[test]
fn parts_are_read_back_as_they_were_joined() {
    let parts = vec![b"one".to_vec(), Vec::new(), b"three".to_vec()];

    assert_eq!(parts_of(joined(&parts)), Some(parts));
}

#[test]
fn no_part_is_a_layout_of_none() {
    let none: [&[u8]; 0] = [];

    assert_eq!(parts_of(joined(&none)), Some(Vec::new()));
}

#[test]
fn a_part_is_a_run_of_the_buffer_it_was_read_from() {
    let stored = Bytes::from(joined(&[b"one".as_slice(), b"three".as_slice()]));
    let parts = parted(&stored).expect("a layout");
    let (Bytes::Run { buffer: whole, .. }, Bytes::Run { buffer: part, run }) = (&stored, &parts[1])
    else {
        panic!("bytes read for a value are a run of their buffer")
    };

    assert!(std::rc::Rc::ptr_eq(whole, part));
    assert_eq!(&whole[run.clone()], b"three");
}

#[test]
fn a_layout_cut_short_is_refused() {
    let mut bytes = joined(&[b"one".as_slice(), b"three".as_slice()]);
    bytes.pop();

    assert_eq!(parts_of(bytes), None);
}

#[test]
fn bytes_after_the_last_part_are_refused() {
    let mut bytes = joined(&[b"one".as_slice()]);
    bytes.push(0);

    assert_eq!(parts_of(bytes), None);
}

#[test]
fn a_count_the_bytes_cannot_hold_is_refused_before_it_is_reserved() {
    let bytes = u64::MAX.to_le_bytes().to_vec();

    assert_eq!(parts_of(bytes), None);
}

#[test]
fn a_length_past_the_buffer_is_refused() {
    let mut bytes = 1u64.to_le_bytes().to_vec();
    bytes.extend_from_slice(&u64::MAX.to_le_bytes());

    assert_eq!(parts_of(bytes), None);
}
