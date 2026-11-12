//! What a test's host tells the frame of what the test writes.

use {super::*, curios_native::test_support::*};

/// A host in front of a mock, the frame it tells, and what the mock was handed.
fn fronted() -> (TestHost<MockHost>, Arc<Frame>, MockIo) {
    let frame = Arc::new(Frame::default());
    let (host, io) = MockHost::builder().build();

    (TestHost::new(host, Arc::clone(&frame)), frame, io)
}

#[test]
fn a_write_on_a_standard_stream_is_seen_and_what_it_ends_on_remembered() {
    let (host, frame, io) = fronted();
    assert_eq!(frame.wrote(), Wrote::Nothing);

    // An empty write writes nothing, so it is no write.
    HostOps::handle_write(&host, Handle::Stdout, Vec::new()).unwrap();
    assert_eq!(frame.wrote(), Wrote::Nothing);

    HostOps::handle_write(&host, Handle::Stdout, b"drawn\n".to_vec()).unwrap();
    assert_eq!(frame.wrote(), Wrote::Lines);
    HostOps::handle_write(&host, Handle::Stderr, b"half".to_vec()).unwrap();
    assert_eq!(frame.wrote(), Wrote::Open);
    HostOps::handle_write(&host, Handle::Stdout, b" a line\n".to_vec()).unwrap();
    assert_eq!(frame.wrote(), Wrote::Lines);

    // Every byte reached the host behind as it was handed over.
    assert_eq!(io.output(), b"drawn\nhalf a line\n");
}

#[test]
fn a_child_that_inherits_an_output_is_a_write_and_one_that_inherits_none_is_not() {
    let spawned = |stdout, stderr| {
        let (host, frame, _) = fronted();
        // Whether the mock starts anything is beside the point: the frame is told before the row is answered.
        let _ = HostOps::proc_spawn(
            &host,
            vec![b"child".to_vec()],
            Vec::new(),
            Vec::new(),
            StdioMode::Inherit,
            stdout,
            stderr,
        );

        frame.wrote()
    };

    assert_eq!(spawned(StdioMode::Pipe, StdioMode::Null), Wrote::Nothing);
    assert_eq!(spawned(StdioMode::Inherit, StdioMode::Null), Wrote::Lines);
    assert_eq!(spawned(StdioMode::Pipe, StdioMode::Inherit), Wrote::Lines);
}
