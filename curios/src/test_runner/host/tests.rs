//! What a test's host writes of what the test writes, and what it tells the frame of it.

use {super::*, curios_native::test_support::*};

/// A host in front of a mock, the frame it tells, and what the mock was handed.
fn fronted() -> (TestHost<MockHost>, Arc<Frame>, MockIo) {
    let frame = Arc::new(Frame::default());
    let (host, io) = MockHost::builder().build();

    (TestHost::new(host, Arc::clone(&frame)), frame, io)
}

#[test]
fn a_write_on_a_standard_stream_is_framed_a_line_at_a_time() {
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

    // Each line reached the host behind its frame, the one the two streams share written once.
    assert_eq!(
        String::from_utf8(io.output()).unwrap(),
        "│  ╎ drawn\n│  ╎ half a line\n"
    );
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

/// A line with nothing on it stands behind the frame alone, a carriage return leaves its line to be written over from the frame, and a line's end after one is that line's end.
#[test]
fn an_empty_line_and_a_line_written_over_keep_the_frame() {
    let (host, frame, io) = fronted();

    HostOps::handle_write(&host, Handle::Stdout, b"one\n\ntwo".to_vec()).unwrap();
    assert_eq!(frame.wrote(), Wrote::Open);
    HostOps::handle_write(&host, Handle::Stdout, b"\r2\r\n".to_vec()).unwrap();
    assert_eq!(frame.wrote(), Wrote::Lines);

    assert_eq!(
        String::from_utf8(io.output()).unwrap(),
        "│  ╎ one\n│  ╎\n│  ╎ two\r│  ╎ 2\r\n"
    );
}

/// A write is answered with how many of the test's own bytes were taken, whatever the frame added beside them.
#[test]
fn a_framed_write_answers_for_the_tests_own_bytes() {
    let (host, _, _) = fronted();

    assert_eq!(
        HostOps::handle_write(&host, Handle::Stderr, b"a\nb\n".to_vec()).unwrap(),
        4
    );
}
