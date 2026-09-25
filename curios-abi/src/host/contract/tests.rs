//! The contract evaluator: which statuses a row may answer, and what a success answers to — never the padding a failure carries.

use {
    super::super::{Handle, HostOp, WireValue},
    crate::status,
};

fn nat(value: u64) -> WireValue {
    WireValue::Nat(value)
}

fn bytes(length: usize) -> WireValue {
    WireValue::Bytes(vec![b'x'; length])
}

#[test]
fn a_read_answers_between_one_byte_and_its_request() {
    let read = |length, request| {
        HostOp::HandleRead.check_reply(&[nat(status::OK), bytes(length)], &[None, Some(request)])
    };

    assert!(read(1, 8).is_ok());
    assert!(read(8, 8).is_ok());
    assert!(read(0, 0).is_ok());
    assert!(read(9, 8).is_err());
    assert!(read(0, 8).is_err());
    assert!(read(1, 0).is_err());
}

#[test]
fn a_write_accepts_between_one_byte_and_its_buffer() {
    let write = |accepted, offered| {
        HostOp::HandleWrite.check_reply(&[nat(status::OK), nat(accepted)], &[None, Some(offered)])
    };

    assert!(write(1, 3).is_ok());
    assert!(write(3, 3).is_ok());
    assert!(write(0, 0).is_ok());
    assert!(write(0, 3).is_err());
    assert!(write(4, 3).is_err());
    assert!(write(1, 0).is_err());
}

/// A failure carries padding the guest never reads, so none of a success's checks apply to it: a failed read of a positive request is empty, and a failed open answers the empty token.
#[test]
fn padding_answers_to_no_check() {
    assert!(
        HostOp::HandleRead
            .check_reply(&[nat(status::WOULD_BLOCK), bytes(0)], &[None, Some(8)])
            .is_ok()
    );
    assert!(
        HostOp::FileOpen
            .check_reply(
                &[nat(status::NOT_FOUND), WireValue::Handle(Handle::none())],
                &[None, None]
            )
            .is_ok()
    );
}

#[test]
fn a_successful_handle_is_never_the_empty_token() {
    let open = |handle| {
        HostOp::FileOpen.check_reply(&[nat(status::OK), WireValue::Handle(handle)], &[None, None])
    };

    assert!(open(Handle::Other(vec![3])).is_ok());
    assert!(open(Handle::Stdin).is_ok());
    assert!(open(Handle::none()).is_err());
}

/// Every fallible row answers the operating system's statuses and the errno lane; `would_block` and `tls` only where the row is marked, `eof` only from a stream, and nothing past the lane's last errno.
#[test]
fn a_row_answers_only_the_statuses_its_contract_names() {
    let open = |code| {
        HostOp::FileOpen.check_reply(
            &[nat(code), WireValue::Handle(Handle::none())],
            &[None, None],
        )
    };
    let read = |code| HostOp::HandleRead.check_reply(&[nat(code), bytes(0)], &[None, Some(8)]);

    assert!(open(status::NOT_FOUND).is_ok());
    assert!(open(status::NOT_DIRECTORY).is_ok());
    assert!(open(status::OTHER_BASE).is_ok());
    assert!(open(status::OTHER_BASE + status::ERRNO_MAX).is_ok());
    assert!(open(status::OTHER_BASE + status::ERRNO_MAX + 1).is_err());
    assert!(open(status::WOULD_BLOCK).is_err());
    assert!(open(status::TLS_ERROR).is_err());
    assert!(open(status::EOF).is_err());

    assert!(read(status::WOULD_BLOCK).is_ok());
    assert!(read(status::TLS_ERROR).is_ok());
    assert!(read(status::EOF).is_ok());
}

#[test]
fn a_lookup_answers_ok_or_not_found_and_nothing_else() {
    let lookup = |code| HostOp::ProcEnv.check_reply(&[nat(code), bytes(0)], &[Some(4)]);

    assert!(lookup(status::OK).is_ok());
    assert!(lookup(status::NOT_FOUND).is_ok());
    assert!(lookup(status::PERMISSION_DENIED).is_err());
    assert!(lookup(status::OTHER_BASE).is_err());
}

#[test]
fn a_poll_takes_parallel_lists_and_answers_one_mask_per_handle() {
    assert!(HostOp::HandlePoll.admit(&[Some(2), Some(2), None]).is_ok());
    assert!(HostOp::HandlePoll.admit(&[Some(2), Some(1), None]).is_err());

    let poll = |masks: &[u8]| {
        HostOp::HandlePoll.check_reply(
            &[WireValue::Bytes(masks.to_vec())],
            &[Some(2), Some(2), None],
        )
    };

    assert!(poll(&[0b0001, 0b1110]).is_ok());
    assert!(poll(&[0b0001]).is_err());
    assert!(poll(&[0b0001, 0b1_0000]).is_err());
}

#[test]
fn randomness_answers_exactly_the_bytes_asked_for() {
    assert!(
        HostOp::RandBytes
            .check_reply(&[bytes(4)], &[Some(4)])
            .is_ok()
    );
    assert!(
        HostOp::RandBytes
            .check_reply(&[bytes(3)], &[Some(4)])
            .is_err()
    );
}

#[test]
fn a_resolved_lookup_holds_an_address() {
    let resolve = |addresses: Vec<Vec<u8>>| {
        HostOp::DnsResolve.check_reply(&[nat(status::OK), WireValue::BytesList(addresses)], &[None])
    };

    assert!(resolve(vec![b"127.0.0.1:80".to_vec()]).is_ok());
    assert!(resolve(vec![]).is_err());
}

#[test]
fn a_nanosecond_count_stays_below_a_second() {
    assert!(
        HostOp::ClockWall
            .check_reply(&[nat(5), nat(999_999_999)], &[])
            .is_ok()
    );
    assert!(
        HostOp::ClockMono
            .check_reply(&[nat(5), nat(1_000_000_000)], &[])
            .is_err()
    );

    let stat = |kind, nanos| {
        HostOp::FileStat.check_reply(
            &[nat(status::OK), nat(kind), nat(0), nat(0), nat(nanos)],
            &[Some(1)],
        )
    };

    assert!(stat(3, 0).is_ok());
    assert!(stat(4, 0).is_err());
    assert!(stat(0, 1_000_000_000).is_err());
}

#[test]
fn a_child_ends_by_a_code_or_by_a_signal() {
    let wait = |code, signal| {
        HostOp::ProcWait.check_reply(&[nat(status::OK), nat(code), nat(signal)], &[None])
    };

    assert!(wait(255, 0).is_ok());
    assert!(wait(0, 9).is_ok());
    assert!(wait(256, 0).is_err());
    assert!(wait(3, 9).is_err());
}
