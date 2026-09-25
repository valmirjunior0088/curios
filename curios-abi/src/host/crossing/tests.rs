//! How a reply encodes: the status first, a payload's slots or the padding in their place, and a termination in place of any slot.

use {
    super::{
        super::{ChildExit, Failure, Handle, Termination, TtySize, WireSink, WireType},
        WireReply, results,
    },
    crate::status,
    std::{convert::Infallible, num::NonZeroU32},
};

/// One slot as a sink received it; a handle as its token bytes.
#[derive(Debug, PartialEq)]
enum Slot {
    Nat(u64),
    Bytes(Vec<u8>),
    Handle(Vec<u8>),
    Terminate(u8),
}

/// A sink that records what it is handed, in order.
#[derive(Default)]
struct Recording(Vec<Slot>);

impl WireSink for Recording {
    type Error = Infallible;

    fn nat(&mut self, value: u64) -> Result<(), Infallible> {
        self.0.push(Slot::Nat(value));

        Ok(())
    }

    fn bytes(&mut self, value: Vec<u8>) -> Result<(), Infallible> {
        self.0.push(Slot::Bytes(value));

        Ok(())
    }

    fn handle(&mut self, value: Handle) -> Result<(), Infallible> {
        self.0.push(Slot::Handle(value.bytes()));

        Ok(())
    }

    fn bytes_list(&mut self, _: Vec<Vec<u8>>) -> Result<(), Infallible> {
        unreachable!("no reply here carries a list")
    }

    fn terminate(&mut self, code: u8) -> Result<(), Infallible> {
        self.0.push(Slot::Terminate(code));

        Ok(())
    }
}

fn encoded(reply: impl WireReply) -> Vec<Slot> {
    let mut sink = Recording::default();
    let Ok(()) = reply.encode(&mut sink);

    sink.0
}

#[test]
fn a_failure_pads_the_payload_it_does_not_carry() {
    assert_eq!(
        encoded(Err::<Handle, _>(Failure::WouldBlock)),
        [Slot::Nat(status::WOULD_BLOCK), Slot::Handle(vec![])]
    );
    assert_eq!(
        encoded(Err::<TtySize, _>(Failure::Other(25))),
        [
            Slot::Nat(status::OTHER_BASE + 25),
            Slot::Nat(0),
            Slot::Nat(0)
        ]
    );
}

#[test]
fn a_stream_answers_its_bytes_or_its_end() {
    assert_eq!(
        encoded(Ok::<_, Failure>(Some(b"ab".to_vec()))),
        [Slot::Nat(status::OK), Slot::Bytes(b"ab".to_vec())]
    );
    assert_eq!(
        encoded(Ok::<Option<Vec<u8>>, Failure>(None)),
        [Slot::Nat(status::EOF), Slot::Bytes(vec![])]
    );
}

#[test]
fn an_absence_crosses_as_not_found() {
    assert_eq!(
        encoded(None::<Vec<u8>>),
        [Slot::Nat(status::NOT_FOUND), Slot::Bytes(vec![])]
    );
}

/// The pair `(code, signal)` has one field that applies, and the other crosses as zero.
#[test]
fn a_child_exit_fills_the_field_that_applies() {
    assert_eq!(
        encoded(Ok::<_, Failure>(ChildExit::Code(3))),
        [Slot::Nat(status::OK), Slot::Nat(3), Slot::Nat(0)]
    );
    assert_eq!(
        encoded(Ok::<_, Failure>(ChildExit::Signal(
            NonZeroU32::new(9).unwrap()
        ))),
        [Slot::Nat(status::OK), Slot::Nat(0), Slot::Nat(9)]
    );
}

#[test]
fn a_termination_ends_the_instance_in_place_of_any_slot() {
    assert_eq!(encoded(Termination(7)), [Slot::Terminate(7)]);
}

/// Codegen embeds only the final result back into a rope, so a payload that put a reference anywhere else would be a row no stage could lower; the roster refuses it when it is built.
#[test]
#[should_panic(expected = "only the last result may be one")]
fn a_reference_result_crosses_only_last() {
    results(vec![
        ("bytes".to_string(), WireType::Bytes),
        ("count".to_string(), WireType::Nat),
    ]);
}
