//! How each Rust type a builtin row is written in crosses the wire — the knowledge the table's rows read off their types rather than spell as columns.
//!
//! An operand type states the [`WireType`] it arrives as ([`WireOperand`]). A payload states the labelled slots a success fills, how it fills them, and the padding a reply without it fills them with ([`WirePayload`]). A reply type, built around a payload, states the [`Outcome`] it crosses as and encodes the whole reply ([`WireReply`]): a bare payload returns, `Result<T, Failure>` is fallible, `Result<Option<T>, Failure>` a stream whose `None` is its end, `Option<T>` a lookup whose `None` is absence, and [`Termination`] diverges. The four generic shapes are disjoint because no `Option` is a payload, which coherence holds rather than a comment.
//!
//! A reply encodes into a [`WireSink`], one call per slot in the order the slots cross. The runtime implements the sink over wasm values, so this leaf names none, and the table's types agree with the bindings built from them by construction: both are read off the same row.

use {
    super::{
        ChildExit, Failure, FileStat, Handle, Mode, Poll, Termination, Timestamp, TtySize,
        WireLeaf, WireResults, WireShape, WireType,
    },
    crate::status,
};

#[cfg(test)]
mod tests;

/// What a builtin's reply promises, read off its Rust type by [`WireReply`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Outcome {
    /// A value; the call cannot fail.
    Returns,
    /// A value, or a [`Failure`].
    Fallible,
    /// A value, the end of the stream, or a [`Failure`].
    Stream,
    /// A value, or its absence.
    Lookup,
    /// Nothing: the call ends the instance.
    Diverges,
}

/// A type an operand of a builtin row is written in, and the wire type it arrives as.
pub trait WireOperand {
    const WIRE: WireType;
}

impl WireOperand for Handle {
    const WIRE: WireType = WireType::Handle;
}

impl WireOperand for u64 {
    const WIRE: WireType = WireType::Nat;
}

impl WireOperand for i64 {
    const WIRE: WireType = WireType::Int;
}

/// A `Bool` arrives as its word.
impl WireOperand for u32 {
    const WIRE: WireType = WireType::Bool;
}

impl WireOperand for u8 {
    const WIRE: WireType = WireType::Byte;
}

impl WireOperand for Vec<u8> {
    const WIRE: WireType = WireType::Bytes;
}

impl WireOperand for Vec<Vec<u8>> {
    const WIRE: WireType = WireType::List(WireLeaf::Bytes);
}

impl WireOperand for Vec<Handle> {
    const WIRE: WireType = WireType::List(WireLeaf::Handle);
}

/// The masks of a poll cross as one byte each.
impl WireOperand for Vec<Poll> {
    const WIRE: WireType = WireType::Bytes;
}

/// A mode arrives as its [`open_mode`](crate::open_mode) tag.
impl WireOperand for Mode {
    const WIRE: WireType = WireType::Nat;
}

/// Where a reply is written, one call per slot in the order the slots cross — the scalars, then the one reference. The runtime writes each into the import's results as it arrives; a failure to write, such as a failed allocation, stops the encoding.
pub trait WireSink {
    type Error;

    fn nat(&mut self, value: u64) -> Result<(), Self::Error>;

    fn bytes(&mut self, value: Vec<u8>) -> Result<(), Self::Error>;

    fn handle(&mut self, value: Handle) -> Result<(), Self::Error>;

    fn bytes_list(&mut self, value: Vec<Vec<u8>>) -> Result<(), Self::Error>;

    /// End the instance with `code`: what a diverging reply writes in place of any slot.
    fn terminate(&mut self, code: u8) -> Result<(), Self::Error>;
}

/// What a successful call carries: the labelled slots it fills, how it fills them, and what a reply without it fills them with.
pub trait WirePayload: Sized {
    /// The slots, in the order they cross. A payload of one value fills one slot, labelled `label`; a payload of several fields labels each with its field's name, which is what the guest projects.
    fn slots(label: &str) -> Vec<(String, WireType)>;

    fn encode<S: WireSink>(self, sink: &mut S) -> Result<(), S::Error>;

    /// Fill the slots of a reply that carries no payload — a failure, an end of stream or an absence. The values are inert: the guest reads the status first and never projects them.
    fn pad<S: WireSink>(sink: &mut S) -> Result<(), S::Error>;
}

impl WirePayload for () {
    fn slots(_: &str) -> Vec<(String, WireType)> {
        vec![]
    }

    fn encode<S: WireSink>(self, _: &mut S) -> Result<(), S::Error> {
        Ok(())
    }

    fn pad<S: WireSink>(_: &mut S) -> Result<(), S::Error> {
        Ok(())
    }
}

impl WirePayload for u64 {
    fn slots(label: &str) -> Vec<(String, WireType)> {
        vec![(label.to_string(), WireType::Nat)]
    }

    fn encode<S: WireSink>(self, sink: &mut S) -> Result<(), S::Error> {
        sink.nat(self)
    }

    fn pad<S: WireSink>(sink: &mut S) -> Result<(), S::Error> {
        sink.nat(0)
    }
}

impl WirePayload for Vec<u8> {
    fn slots(label: &str) -> Vec<(String, WireType)> {
        vec![(label.to_string(), WireType::Bytes)]
    }

    fn encode<S: WireSink>(self, sink: &mut S) -> Result<(), S::Error> {
        sink.bytes(self)
    }

    fn pad<S: WireSink>(sink: &mut S) -> Result<(), S::Error> {
        sink.bytes(Vec::new())
    }
}

/// A handle pads as [`Handle::none`], the empty token no host mints.
impl WirePayload for Handle {
    fn slots(label: &str) -> Vec<(String, WireType)> {
        vec![(label.to_string(), WireType::Handle)]
    }

    fn encode<S: WireSink>(self, sink: &mut S) -> Result<(), S::Error> {
        sink.handle(self)
    }

    fn pad<S: WireSink>(sink: &mut S) -> Result<(), S::Error> {
        sink.handle(Handle::none())
    }
}

impl WirePayload for Vec<Vec<u8>> {
    fn slots(label: &str) -> Vec<(String, WireType)> {
        vec![(label.to_string(), WireType::List(WireLeaf::Bytes))]
    }

    fn encode<S: WireSink>(self, sink: &mut S) -> Result<(), S::Error> {
        sink.bytes_list(self)
    }

    fn pad<S: WireSink>(sink: &mut S) -> Result<(), S::Error> {
        sink.bytes_list(Vec::new())
    }
}

/// Poll masks cross as one `Bytes`, byte `i` the mask of handle `i`.
impl WirePayload for Vec<Poll> {
    fn slots(label: &str) -> Vec<(String, WireType)> {
        vec![(label.to_string(), WireType::Bytes)]
    }

    fn encode<S: WireSink>(self, sink: &mut S) -> Result<(), S::Error> {
        sink.bytes(self.into_iter().map(Poll::bits).collect())
    }

    fn pad<S: WireSink>(sink: &mut S) -> Result<(), S::Error> {
        sink.bytes(Vec::new())
    }
}

impl WirePayload for Timestamp {
    fn slots(_: &str) -> Vec<(String, WireType)> {
        vec![
            ("secs".to_string(), WireType::Nat),
            ("nanos".to_string(), WireType::Nat),
        ]
    }

    fn encode<S: WireSink>(self, sink: &mut S) -> Result<(), S::Error> {
        sink.nat(self.secs)?;
        sink.nat(self.nanos)
    }

    fn pad<S: WireSink>(sink: &mut S) -> Result<(), S::Error> {
        sink.nat(0)?;
        sink.nat(0)
    }
}

impl WirePayload for TtySize {
    fn slots(_: &str) -> Vec<(String, WireType)> {
        vec![
            ("cols".to_string(), WireType::Nat),
            ("rows".to_string(), WireType::Nat),
        ]
    }

    fn encode<S: WireSink>(self, sink: &mut S) -> Result<(), S::Error> {
        sink.nat(self.cols)?;
        sink.nat(self.rows)
    }

    fn pad<S: WireSink>(sink: &mut S) -> Result<(), S::Error> {
        sink.nat(0)?;
        sink.nat(0)
    }
}

/// The kind crosses as its [`file_kind`](crate::file_kind) code.
impl WirePayload for FileStat {
    fn slots(_: &str) -> Vec<(String, WireType)> {
        vec![
            ("kind".to_string(), WireType::Nat),
            ("size".to_string(), WireType::Nat),
            ("mtime_secs".to_string(), WireType::Nat),
            ("mtime_nanos".to_string(), WireType::Nat),
        ]
    }

    fn encode<S: WireSink>(self, sink: &mut S) -> Result<(), S::Error> {
        sink.nat(self.kind.code())?;
        sink.nat(self.size)?;
        sink.nat(self.mtime_secs)?;
        sink.nat(self.mtime_nanos)
    }

    fn pad<S: WireSink>(sink: &mut S) -> Result<(), S::Error> {
        (0..4).try_for_each(|_| sink.nat(0))
    }
}

/// An exit crosses as the pair `(code, signal)`, the field that does not apply zero: `signal` is nonzero exactly when a signal ended the child.
impl WirePayload for ChildExit {
    fn slots(_: &str) -> Vec<(String, WireType)> {
        vec![
            ("code".to_string(), WireType::Nat),
            ("signal".to_string(), WireType::Nat),
        ]
    }

    fn encode<S: WireSink>(self, sink: &mut S) -> Result<(), S::Error> {
        let (code, signal) = match self {
            ChildExit::Code(code) => (u64::from(code), 0),
            ChildExit::Signal(signal) => (0, u64::from(signal.get())),
        };

        sink.nat(code)?;
        sink.nat(signal)
    }

    fn pad<S: WireSink>(sink: &mut S) -> Result<(), S::Error> {
        sink.nat(0)?;
        sink.nat(0)
    }
}

/// A builtin's whole reply: the outcome it crosses as, its result slots, and its encoding.
pub trait WireReply: Sized {
    const OUTCOME: Outcome;

    /// The result slots, a lone payload's labelled `label`.
    fn results(label: &str) -> WireResults;

    fn encode<S: WireSink>(self, sink: &mut S) -> Result<(), S::Error>;
}

/// The slots a reply crosses with that carries a status: the status first, then the payload's.
fn with_status<T: WirePayload>(label: &str) -> WireResults {
    results(
        [("status".to_string(), WireType::Nat)]
            .into_iter()
            .chain(T::slots(label))
            .collect(),
    )
}

/// `slots` as [`WireResults`], which holds a reference result only last. A row whose payload puts one anywhere else is a table that cannot be read, so the roster refuses it when it is built.
fn results(mut slots: Vec<(String, WireType)>) -> WireResults {
    let Some((label, last)) = slots.pop() else {
        return WireResults::none();
    };

    let scalars = slots
        .into_iter()
        .map(|(label, wire_type)| match wire_type.shape() {
            WireShape::Scalar(scalar) => (label, scalar),
            WireShape::Reference(_) => {
                panic!("`{label}` is a reference result, and only the last result may be one")
            }
        })
        .collect();

    WireResults::ending(scalars, label, last.shape())
}

impl<T: WirePayload> WireReply for T {
    const OUTCOME: Outcome = Outcome::Returns;

    fn results(label: &str) -> WireResults {
        results(T::slots(label))
    }

    fn encode<S: WireSink>(self, sink: &mut S) -> Result<(), S::Error> {
        WirePayload::encode(self, sink)
    }
}

impl<T: WirePayload> WireReply for Result<T, Failure> {
    const OUTCOME: Outcome = Outcome::Fallible;

    fn results(label: &str) -> WireResults {
        with_status::<T>(label)
    }

    fn encode<S: WireSink>(self, sink: &mut S) -> Result<(), S::Error> {
        match self {
            Ok(value) => {
                sink.nat(status::OK)?;
                value.encode(sink)
            }
            Err(failure) => {
                sink.nat(failure.code())?;
                T::pad(sink)
            }
        }
    }
}

/// The end of the stream crosses as `EOF` beside the payload's padding.
impl<T: WirePayload> WireReply for Result<Option<T>, Failure> {
    const OUTCOME: Outcome = Outcome::Stream;

    fn results(label: &str) -> WireResults {
        with_status::<T>(label)
    }

    fn encode<S: WireSink>(self, sink: &mut S) -> Result<(), S::Error> {
        match self {
            Ok(Some(value)) => {
                sink.nat(status::OK)?;
                value.encode(sink)
            }
            Ok(None) => {
                sink.nat(status::EOF)?;
                T::pad(sink)
            }
            Err(failure) => {
                sink.nat(failure.code())?;
                T::pad(sink)
            }
        }
    }
}

/// Absence crosses as `NOT_FOUND` beside the payload's padding.
impl<T: WirePayload> WireReply for Option<T> {
    const OUTCOME: Outcome = Outcome::Lookup;

    fn results(label: &str) -> WireResults {
        with_status::<T>(label)
    }

    fn encode<S: WireSink>(self, sink: &mut S) -> Result<(), S::Error> {
        match self {
            Some(value) => {
                sink.nat(status::OK)?;
                value.encode(sink)
            }
            None => {
                sink.nat(Failure::NotFound.code())?;
                T::pad(sink)
            }
        }
    }
}

/// A diverging call crosses nothing back: its reply ends the instance.
impl WireReply for Termination {
    const OUTCOME: Outcome = Outcome::Diverges;

    fn results(_: &str) -> WireResults {
        WireResults::none()
    }

    fn encode<S: WireSink>(self, sink: &mut S) -> Result<(), S::Error> {
        sink.terminate(self.0)
    }
}
