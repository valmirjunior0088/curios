//! What the counting allocator counts: nothing while no one reads the counts, everything while a recorder listens or a measurement has asked, each thread's own beside the process's, and the bytes held from where counting began.
//!
//! Each test runs in a process of its own, as nextest runs them, so no other test's recorder listens beside it.

use {
    super::*,
    crate::*,
    std::{hint::black_box, io, panic},
};

// A program installs the allocator, and this crate's suite is one: these tests and the recorder's read what it counts.
#[global_allocator]
static ALLOCATOR: CountingAllocator = CountingAllocator;

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
    let counted = trace(sink(), &[], || {
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
    let ended = panic::catch_unwind(|| trace(sink(), &[], || panic!("the closure fails")));
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

// A thread's counts are what it took itself: a block another thread takes is counted for that thread and the process, and not for this one.
#[test]
fn a_thread_counts_only_what_it_takes() {
    count_allocations();
    let (here, process) = (thread_allocated_bytes(), allocated_bytes());
    let there = std::thread::scope(|scope| {
        scope
            .spawn(|| {
                let before = thread_allocated_bytes();
                take_and_return();
                thread_allocated_bytes() - before
            })
            .join()
            .expect("the thread returns")
    });

    assert!(there >= HELD as u64, "{there}");
    assert!(allocated_bytes() - process >= HELD);
    assert!(thread_allocated_bytes() - here < HELD as u64);
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
