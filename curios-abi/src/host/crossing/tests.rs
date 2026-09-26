//! How a reply encodes: the status first, a payload's values or the padding in their place, and a termination or a refusal in place of any value.

use {
    super::{
        super::{ChildExit, ChildHandles, Failure, Handle, Refusal, Termination, TtySize},
        Encoded, WireReply, WireValue,
    },
    crate::status,
    std::num::NonZeroU32,
};

#[test]
fn a_failure_pads_the_payload_it_does_not_carry() {
    assert_eq!(
        Err::<Handle, _>(Failure::WouldBlock).encode(),
        Encoded::Reply(vec![
            WireValue::Nat(status::WOULD_BLOCK),
            WireValue::Handle(Handle::none())
        ])
    );
    assert_eq!(
        Err::<TtySize, _>(Failure::Other(25)).encode(),
        Encoded::Reply(vec![
            WireValue::Nat(status::OTHER_BASE + 25),
            WireValue::Nat(0),
            WireValue::Nat(0)
        ])
    );
}

#[test]
fn a_stream_answers_its_bytes_or_its_end() {
    assert_eq!(
        Ok::<_, Failure>(Some(b"ab".to_vec())).encode(),
        Encoded::Reply(vec![
            WireValue::Nat(status::OK),
            WireValue::Bytes(b"ab".to_vec())
        ])
    );
    assert_eq!(
        Ok::<Option<Vec<u8>>, Failure>(None).encode(),
        Encoded::Reply(vec![WireValue::Nat(status::EOF), WireValue::Bytes(vec![])])
    );
}

#[test]
fn an_absence_crosses_as_not_found() {
    assert_eq!(
        None::<Vec<u8>>.encode(),
        Encoded::Reply(vec![
            WireValue::Nat(status::NOT_FOUND),
            WireValue::Bytes(vec![])
        ])
    );
}

/// The pair `(code, signal)` has one field that applies, and the other crosses as zero.
#[test]
fn a_child_exit_fills_the_field_that_applies() {
    assert_eq!(
        Ok::<_, Failure>(ChildExit::Code(3)).encode(),
        Encoded::Reply(vec![
            WireValue::Nat(status::OK),
            WireValue::Nat(3),
            WireValue::Nat(0)
        ])
    );
    assert_eq!(
        Ok::<_, Failure>(ChildExit::Signal(NonZeroU32::new(9).unwrap())).encode(),
        Encoded::Reply(vec![
            WireValue::Nat(status::OK),
            WireValue::Nat(0),
            WireValue::Nat(9)
        ])
    );
}

/// A stream the call did not pipe crosses as the empty token, which is also what every slot of a failed spawn crosses as.
#[test]
fn a_spawn_answers_the_empty_token_for_a_stream_it_did_not_pipe() {
    let none = || WireValue::Handle(Handle::none());

    assert_eq!(
        Ok::<_, Failure>(ChildHandles {
            child: Handle::Other(vec![7]),
            stdin: None,
            stdout: Some(Handle::Other(vec![8])),
            stderr: None,
        })
        .encode(),
        Encoded::Reply(vec![
            WireValue::Nat(status::OK),
            WireValue::Handle(Handle::Other(vec![7])),
            none(),
            WireValue::Handle(Handle::Other(vec![8])),
            none()
        ])
    );
    assert_eq!(
        Err::<ChildHandles, _>(Failure::NotFound).encode(),
        Encoded::Reply(vec![
            WireValue::Nat(status::NOT_FOUND),
            none(),
            none(),
            none(),
            none()
        ])
    );
}

/// A row with no failure lane answers its value, or refuses the call in its place rather than answer something its host does not have.
#[test]
fn a_refusal_stands_in_place_of_any_value() {
    assert_eq!(
        Ok::<_, Refusal>(vec![7u8]).encode(),
        Encoded::Reply(vec![WireValue::Bytes(vec![7])])
    );
    assert_eq!(
        Err::<Vec<u8>, _>(Refusal("no entropy".to_string())).encode(),
        Encoded::Refused("no entropy".to_string())
    );
}

#[test]
fn a_termination_ends_the_instance_in_place_of_any_value() {
    assert_eq!(Termination(7).encode(), Encoded::Terminate(7));
}
