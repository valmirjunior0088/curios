//! What the counting allocator counts: nothing while no one reads the counts, everything while a recorder listens or a measurement has asked, and the bytes held from where counting began.
//!
//! Each test runs in a process of its own, as nextest runs them, so no other test's recorder listens beside it.

use {
    super::*,
    crate::*,
    std::{hint::black_box, io, panic},
};

const HELD: usize = 1 << 20;

/// A block of `HELD` bytes, taken and returned.
fn take_and_return() {
    black_box(vec![0_u8; HELD]);
}

fn sink() -> Destination {
    Destination::Stream(Box::new(io::sink()))
}

// No recorder listens and no measurement has asked, so a trip through the allocator counts nothing: what every build pays for it is the load that says so.
#[test]
fn an_allocation_no_one_reads_is_not_counted() {
    let before = (allocation_count(), allocated_bytes());
    take_and_return();

    assert_eq!((allocation_count(), allocated_bytes()), before);
}

// A trace counts while its closure runs, and stops when the closure returns.
#[test]
fn counting_ends_with_the_trace() {
    let counted = trace(sink(), || {
        let before = allocated_bytes();
        take_and_return();

        allocated_bytes() - before
    })
    .expect("a stream destination opens");
    assert!(counted >= HELD, "the trace counted {counted} bytes");

    let before = allocated_bytes();
    take_and_return();
    assert_eq!(allocated_bytes(), before, "counting outlived the trace");
}

// However a trace's closure ends, counting ends with it: a panic passes through the trace once counting has stopped.
#[test]
fn counting_ends_with_a_trace_whose_closure_panics() {
    let ended = panic::catch_unwind(|| trace(sink(), || panic!("the closure fails")));
    assert!(ended.is_err(), "the panic passes through the trace");

    let before = allocated_bytes();
    take_and_return();
    assert_eq!(allocated_bytes(), before, "counting outlived the trace");
}

// A measurement that asks is counted for with no recorder listening.
#[test]
fn a_measurement_that_asks_is_counted_for() {
    count_allocations();
    let before = allocated_bytes();
    take_and_return();

    assert!(allocated_bytes() - before >= HELD);
}

// Live bytes count from where counting began: a block taken before it and returned since lowers them below where they began, where an unsigned count would wrap.
#[test]
fn a_block_taken_before_counting_lowers_live_bytes_when_returned() {
    let held = black_box(vec![0_u8; HELD]);
    count_allocations();
    let before = live_bytes();
    drop(held);

    assert!(
        live_bytes() <= before - HELD as isize,
        "live bytes went from {before} to {}",
        live_bytes()
    );
}
