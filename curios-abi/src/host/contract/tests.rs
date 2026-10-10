//! What a row asks of a call's operands: a call that breaks a requirement is refused before its host is asked.

use crate::HostOp;

#[test]
fn a_poll_takes_parallel_lists() {
    assert!(HostOp::HandlePoll.admit(&[Some(2), Some(2), None]).is_ok());
    assert!(HostOp::HandlePoll.admit(&[Some(2), Some(1), None]).is_err());
}

/// A row with no requirement admits whatever its types let cross.
#[test]
fn a_row_with_no_requirement_admits_every_call() {
    assert!(HostOp::HandleRead.admit(&[None, Some(8)]).is_ok());
}
