//! A host held to its rows' contracts: a reply outside its row refuses the call, naming the import and what the host answered, rather than handing the guest a reply its row does not allow.

use {
    super::test_support::Lying,
    crate::tests::run_text,
    curios_abi::{Failure, Handle, HostOp, Poll, Timestamp},
    curios_runtime::MockHost,
};

/// Run `source` against a mock whose `op` answers `reply` once, and answer the refusal it must end in.
fn refused<R: Send + 'static>(source: &str, host: MockHost, op: HostOp, reply: R) -> String {
    run_text(source, Lying::new(host, op, reply)).expect_err("the lying host is refused")
}

const READ: &str = "
    let _ = /std/Io/read(/std/Io/stdin, 8)!;
    /std/Io/pure(())
";

const OPEN: &str = r#"
    let _ = /std/Try/run(/std/File/open(/std/Path/of_str("a.txt"), /std/File/Mode/read()))!;
    /std/Io/pure(())
"#;

#[test]
fn a_read_past_its_request_is_refused() {
    let (host, _) = MockHost::builder().stdin_lines(["hello"]).build();
    let error = refused(
        READ,
        host,
        HostOp::HandleRead,
        Ok::<_, Failure>(Some(vec![b'x'; 9])),
    );

    assert!(
        error.contains("sys.handle_read: answered 9 bytes to a request for 8"),
        "{error}"
    );
}

#[test]
fn a_write_past_its_buffer_is_refused() {
    let (host, _) = MockHost::builder().build();
    let error = refused(
        r#"/std/print("hi")"#,
        host,
        HostOp::HandleWrite,
        Ok::<u64, Failure>(3),
    );

    assert!(
        error.contains("sys.handle_write: accepted 3 of 2 bytes"),
        "{error}"
    );
}

/// `file/open` is not marked as blocking, so a host that answers it `would_block` has broken the row's contract, however ordinary the status is elsewhere.
#[test]
fn a_failure_the_row_never_answers_is_refused() {
    let (host, _) = MockHost::builder().build();
    let error = refused(
        OPEN,
        host,
        HostOp::FileOpen,
        Err::<Handle, _>(Failure::WouldBlock),
    );

    assert!(
        error.contains("sys.file_open: answered status 6, which this row never answers"),
        "{error}"
    );
}

#[test]
fn a_success_with_the_empty_token_is_refused() {
    let (host, _) = MockHost::builder().build();
    let error = refused(
        OPEN,
        host,
        HostOp::FileOpen,
        Ok::<_, Failure>(Handle::none()),
    );

    assert!(
        error.contains("sys.file_open: succeeded with the empty token as `handle`"),
        "{error}"
    );
}

#[test]
fn a_clock_past_a_second_of_nanoseconds_is_refused() {
    let (host, _) = MockHost::builder().build();
    let error = refused(
        "
        let _ = /std/time/Instant/now()!;
        /std/Io/pure(())
        ",
        host,
        HostOp::ClockMono,
        Timestamp {
            secs: 0,
            nanos: 1_000_000_000,
        },
    );

    assert!(
        error.contains(
            "sys.clock_mono: answered `nanos` 1000000000, which must be below 1000000000"
        ),
        "{error}"
    );
}

/// The second read waits for its chunk, so the program polls standard input — and a poll that answers no mask for it is refused.
#[test]
fn a_poll_that_answers_for_other_handles_is_refused() {
    let (host, _) = MockHost::builder()
        .stdin_chunks(vec![b"a".as_slice(), b"b".as_slice()])
        .build();
    let error = refused(
        "
        let _ = /std/Io/read(/std/Io/stdin, 8)!;
        let _ = /std/Io/read(/std/Io/stdin, 8)!;
        /std/Io/pure(())
        ",
        host,
        HostOp::HandlePoll,
        Vec::<Poll>::new(),
    );

    assert!(
        error.contains("sys.handle_poll: answered 0 elements for the 1 of `handles`"),
        "{error}"
    );
}

#[test]
fn randomness_short_of_its_request_is_refused() {
    let (host, _) = MockHost::builder().build();
    let error = refused(
        "
        let _ = /std/rand/bytes(4)!;
        /std/Io/pure(())
        ",
        host,
        HostOp::RandBytes,
        vec![0u8; 3],
    );

    assert!(
        error.contains("sys.rand_bytes: answered 3 bytes to a request for exactly 4"),
        "{error}"
    );
}
