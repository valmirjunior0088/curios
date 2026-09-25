//! How each Rust type a builtin row is written in crosses the wire — the knowledge the table's rows read off their types rather than spell as columns.
//!
//! An operand type states the [`WireType`] it arrives as and, where a row's contract relates a reply to it, its measure ([`WireOperand`]). A payload states the labelled slots a success fills, how it fills them, the padding a reply without it fills them with, and the checks its own values answer to ([`WirePayload`]). A reply type, built around a payload, states the [`Outcome`] it crosses as and encodes the whole reply ([`WireReply`]): a bare payload returns, `Result<T, Failure>` is fallible, `Result<Option<T>, Failure>` a stream whose `None` is its end, `Option<T>` a lookup whose `None` is absence, and [`Termination`] diverges. The four generic shapes are disjoint because no `Option` is a payload, which coherence holds rather than a comment.
//!
//! A reply encodes to the [`WireValue`]s it crosses with, in slot order ([`Encoded`]), which is what the row's contract is checked against before the runtime writes them into wasm values: this leaf names no wasm value, and the table's types agree with the bindings built from them by construction, since both are read off the same row.

use {
    super::{
        Check, ChildExit, ChildStream, Failure, FileKind, FileStat, Handle, Mode, Poll, Refusal,
        SerialFlow, SerialOp, SerialParity, StdioMode, Termination, Timestamp, TtySize, WireLeaf,
        WireResults, WireShape, WireType,
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

/// A type an operand of a builtin row is written in: the wire type it arrives as, and the measure a contract relates a reply to.
pub trait WireOperand {
    const WIRE: WireType;

    /// What a row's checks read of the operand — a count's value, a buffer's or a list's length — or `None` for an operand no check reads.
    fn measure(&self) -> Option<u64> {
        None
    }
}

impl WireOperand for Handle {
    const WIRE: WireType = WireType::Handle;
}

impl WireOperand for u64 {
    const WIRE: WireType = WireType::Nat;

    fn measure(&self) -> Option<u64> {
        Some(*self)
    }
}

impl WireOperand for i64 {
    const WIRE: WireType = WireType::Int;
}

impl WireOperand for bool {
    const WIRE: WireType = WireType::Bool;
}

impl WireOperand for u8 {
    const WIRE: WireType = WireType::Byte;
}

impl WireOperand for Vec<u8> {
    const WIRE: WireType = WireType::Bytes;

    fn measure(&self) -> Option<u64> {
        Some(self.len() as u64)
    }
}

impl WireOperand for Vec<Vec<u8>> {
    const WIRE: WireType = WireType::List(WireLeaf::Bytes);

    fn measure(&self) -> Option<u64> {
        Some(self.len() as u64)
    }
}

impl WireOperand for Vec<Handle> {
    const WIRE: WireType = WireType::List(WireLeaf::Handle);

    fn measure(&self) -> Option<u64> {
        Some(self.len() as u64)
    }
}

/// The masks of a poll cross as one byte each.
impl WireOperand for Vec<Poll> {
    const WIRE: WireType = WireType::Bytes;

    fn measure(&self) -> Option<u64> {
        Some(self.len() as u64)
    }
}

impl WireOperand for Mode {
    const WIRE: WireType = WireType::Nat;
}

impl WireOperand for StdioMode {
    const WIRE: WireType = WireType::Nat;
}

impl WireOperand for ChildStream {
    const WIRE: WireType = WireType::Nat;
}

impl WireOperand for SerialParity {
    const WIRE: WireType = WireType::Nat;
}

impl WireOperand for SerialFlow {
    const WIRE: WireType = WireType::Nat;
}

impl WireOperand for SerialOp {
    const WIRE: WireType = WireType::Nat;
}

/// One value a reply crosses with, in the vocabulary a builtin's results are written in.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WireValue {
    Nat(u64),
    Bytes(Vec<u8>),
    Handle(Handle),
    BytesList(Vec<Vec<u8>>),
}

/// A reply as it crosses: its values in slot order, the termination a diverging reply is, or the refusal of a call its host could not answer.
#[derive(Debug, PartialEq, Eq)]
pub enum Encoded {
    Reply(Vec<WireValue>),
    Terminate(u8),
    Refused(String),
}

/// A second, in the nanoseconds a clock reading or a modification time counts below it.
const NANOS_PER_SECOND: u64 = 1_000_000_000;

/// What a successful call carries: the labelled slots it fills, how it fills them, what a reply without it fills them with, and what its own values answer to.
pub trait WirePayload: Sized {
    /// The slots, in the order they cross. A payload of one value fills one slot, labelled `label`; a payload of several fields labels each with its field's name, which is what the guest projects.
    fn slots(label: &'static str) -> Vec<(String, WireType)>;

    fn encode(self, values: &mut Vec<WireValue>);

    /// Fill the slots of a reply that carries no payload — a failure, an end of stream or an absence. The values are inert: the guest reads the status first and never projects them, so no check reads them either.
    fn pad(values: &mut Vec<WireValue>);

    /// The checks a success answers to whatever row carries it, reading the slots by the labels [`slots`](Self::slots) gives them.
    fn checks(_label: &'static str) -> Vec<Check> {
        Vec::new()
    }
}

impl WirePayload for () {
    fn slots(_: &'static str) -> Vec<(String, WireType)> {
        vec![]
    }

    fn encode(self, _: &mut Vec<WireValue>) {}

    fn pad(_: &mut Vec<WireValue>) {}
}

impl WirePayload for u64 {
    fn slots(label: &'static str) -> Vec<(String, WireType)> {
        vec![(label.to_string(), WireType::Nat)]
    }

    fn encode(self, values: &mut Vec<WireValue>) {
        values.push(WireValue::Nat(self));
    }

    fn pad(values: &mut Vec<WireValue>) {
        values.push(WireValue::Nat(0));
    }
}

impl WirePayload for Vec<u8> {
    fn slots(label: &'static str) -> Vec<(String, WireType)> {
        vec![(label.to_string(), WireType::Bytes)]
    }

    fn encode(self, values: &mut Vec<WireValue>) {
        values.push(WireValue::Bytes(self));
    }

    fn pad(values: &mut Vec<WireValue>) {
        values.push(WireValue::Bytes(Vec::new()));
    }
}

/// A handle pads as [`Handle::none`], the empty token no host mints, which is why a successful handle is never that token.
impl WirePayload for Handle {
    fn slots(label: &'static str) -> Vec<(String, WireType)> {
        vec![(label.to_string(), WireType::Handle)]
    }

    fn encode(self, values: &mut Vec<WireValue>) {
        values.push(WireValue::Handle(self));
    }

    fn pad(values: &mut Vec<WireValue>) {
        values.push(WireValue::Handle(Handle::none()));
    }

    fn checks(label: &'static str) -> Vec<Check> {
        vec![Check::Present { field: label }]
    }
}

impl WirePayload for Vec<Vec<u8>> {
    fn slots(label: &'static str) -> Vec<(String, WireType)> {
        vec![(label.to_string(), WireType::List(WireLeaf::Bytes))]
    }

    fn encode(self, values: &mut Vec<WireValue>) {
        values.push(WireValue::BytesList(self));
    }

    fn pad(values: &mut Vec<WireValue>) {
        values.push(WireValue::BytesList(Vec::new()));
    }
}

/// Poll masks cross as one `Bytes`, byte `i` the mask of handle `i`, each within the bits a host may report.
impl WirePayload for Vec<Poll> {
    fn slots(label: &'static str) -> Vec<(String, WireType)> {
        vec![(label.to_string(), WireType::Bytes)]
    }

    fn encode(self, values: &mut Vec<WireValue>) {
        values.push(WireValue::Bytes(self.into_iter().map(Poll::bits).collect()));
    }

    fn pad(values: &mut Vec<WireValue>) {
        values.push(WireValue::Bytes(Vec::new()));
    }

    fn checks(label: &'static str) -> Vec<Check> {
        vec![Check::Mask {
            field: label,
            allowed: Poll::READINESS,
        }]
    }
}

impl WirePayload for Timestamp {
    fn slots(_: &'static str) -> Vec<(String, WireType)> {
        vec![
            ("secs".to_string(), WireType::Nat),
            ("nanos".to_string(), WireType::Nat),
        ]
    }

    fn encode(self, values: &mut Vec<WireValue>) {
        values.extend([WireValue::Nat(self.secs), WireValue::Nat(self.nanos)]);
    }

    fn pad(values: &mut Vec<WireValue>) {
        values.extend([WireValue::Nat(0), WireValue::Nat(0)]);
    }

    fn checks(_: &'static str) -> Vec<Check> {
        vec![Check::Below {
            field: "nanos",
            bound: NANOS_PER_SECOND,
        }]
    }
}

impl WirePayload for TtySize {
    fn slots(_: &'static str) -> Vec<(String, WireType)> {
        vec![
            ("cols".to_string(), WireType::Nat),
            ("rows".to_string(), WireType::Nat),
        ]
    }

    fn encode(self, values: &mut Vec<WireValue>) {
        values.extend([WireValue::Nat(self.cols), WireValue::Nat(self.rows)]);
    }

    fn pad(values: &mut Vec<WireValue>) {
        values.extend([WireValue::Nat(0), WireValue::Nat(0)]);
    }
}

/// The kind crosses as its [`file_kind`](crate::file_kind) code.
impl WirePayload for FileStat {
    fn slots(_: &'static str) -> Vec<(String, WireType)> {
        vec![
            ("kind".to_string(), WireType::Nat),
            ("size".to_string(), WireType::Nat),
            ("mtime_secs".to_string(), WireType::Nat),
            ("mtime_nanos".to_string(), WireType::Nat),
        ]
    }

    fn encode(self, values: &mut Vec<WireValue>) {
        values.extend([
            WireValue::Nat(self.kind.code()),
            WireValue::Nat(self.size),
            WireValue::Nat(self.mtime_secs),
            WireValue::Nat(self.mtime_nanos),
        ]);
    }

    fn pad(values: &mut Vec<WireValue>) {
        values.extend([0, 0, 0, 0].map(WireValue::Nat));
    }

    fn checks(_: &'static str) -> Vec<Check> {
        vec![
            Check::Code {
                field: "kind",
                codes: FileKind::WIRE_CODES,
            },
            Check::Below {
                field: "mtime_nanos",
                bound: NANOS_PER_SECOND,
            },
        ]
    }
}

/// An exit crosses as the pair `(code, signal)`, the field that does not apply zero: `signal` is nonzero exactly when a signal ended the child.
impl WirePayload for ChildExit {
    fn slots(_: &'static str) -> Vec<(String, WireType)> {
        vec![
            ("code".to_string(), WireType::Nat),
            ("signal".to_string(), WireType::Nat),
        ]
    }

    fn encode(self, values: &mut Vec<WireValue>) {
        let (code, signal) = match self {
            ChildExit::Code(code) => (u64::from(code), 0),
            ChildExit::Signal(signal) => (0, u64::from(signal.get())),
        };

        values.extend([WireValue::Nat(code), WireValue::Nat(signal)]);
    }

    fn pad(values: &mut Vec<WireValue>) {
        values.extend([WireValue::Nat(0), WireValue::Nat(0)]);
    }

    fn checks(_: &'static str) -> Vec<Check> {
        vec![Check::Exit {
            code: "code",
            signal: "signal",
        }]
    }
}

/// A builtin's whole reply: the outcome it crosses as, its result slots, the checks its success answers to, and its encoding.
pub trait WireReply: Sized {
    const OUTCOME: Outcome;

    /// The result slots, a lone payload's labelled `label`.
    fn results(label: &'static str) -> WireResults;

    /// The checks a success answers to by its payload's type, a lone payload's slot labelled `label`.
    fn checks(label: &'static str) -> Vec<Check>;

    fn encode(self) -> Encoded;
}

/// The slots a reply crosses with that carries a status: the status first, then the payload's.
fn with_status<T: WirePayload>(label: &'static str) -> WireResults {
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

/// A status, then either the payload or its padding.
fn with_payload<T: WirePayload>(status: u64, payload: Option<T>) -> Encoded {
    let mut values = vec![WireValue::Nat(status)];

    match payload {
        Some(value) => value.encode(&mut values),
        None => T::pad(&mut values),
    }

    Encoded::Reply(values)
}

impl<T: WirePayload> WireReply for T {
    const OUTCOME: Outcome = Outcome::Returns;

    fn results(label: &'static str) -> WireResults {
        results(T::slots(label))
    }

    fn checks(label: &'static str) -> Vec<Check> {
        T::checks(label)
    }

    fn encode(self) -> Encoded {
        let mut values = Vec::new();
        WirePayload::encode(self, &mut values);

        Encoded::Reply(values)
    }
}

impl<T: WirePayload> WireReply for Result<T, Failure> {
    const OUTCOME: Outcome = Outcome::Fallible;

    fn results(label: &'static str) -> WireResults {
        with_status::<T>(label)
    }

    fn checks(label: &'static str) -> Vec<Check> {
        T::checks(label)
    }

    fn encode(self) -> Encoded {
        match self {
            Ok(value) => with_payload(status::OK, Some(value)),
            Err(failure) => with_payload::<T>(failure.code(), None),
        }
    }
}

/// The end of the stream crosses as `EOF` beside the payload's padding.
impl<T: WirePayload> WireReply for Result<Option<T>, Failure> {
    const OUTCOME: Outcome = Outcome::Stream;

    fn results(label: &'static str) -> WireResults {
        with_status::<T>(label)
    }

    fn checks(label: &'static str) -> Vec<Check> {
        T::checks(label)
    }

    fn encode(self) -> Encoded {
        match self {
            Ok(Some(value)) => with_payload(status::OK, Some(value)),
            Ok(None) => with_payload::<T>(status::EOF, None),
            Err(failure) => with_payload::<T>(failure.code(), None),
        }
    }
}

/// Absence crosses as `NOT_FOUND` beside the payload's padding.
impl<T: WirePayload> WireReply for Option<T> {
    const OUTCOME: Outcome = Outcome::Lookup;

    fn results(label: &'static str) -> WireResults {
        with_status::<T>(label)
    }

    fn checks(label: &'static str) -> Vec<Check> {
        T::checks(label)
    }

    fn encode(self) -> Encoded {
        match self {
            Some(value) => with_payload(status::OK, Some(value)),
            None => with_payload::<T>(Failure::NotFound.code(), None),
        }
    }
}

/// A value, or a refusal: a row with no failure lane whose host could not answer is refused rather than answered with something the host does not have. The refusal never crosses, so the row returns exactly as its payload does.
impl<T: WirePayload> WireReply for Result<T, Refusal> {
    const OUTCOME: Outcome = Outcome::Returns;

    fn results(label: &'static str) -> WireResults {
        results(T::slots(label))
    }

    fn checks(label: &'static str) -> Vec<Check> {
        T::checks(label)
    }

    fn encode(self) -> Encoded {
        match self {
            Ok(value) => WireReply::encode(value),
            Err(Refusal(sentence)) => Encoded::Refused(sentence),
        }
    }
}

/// A diverging call crosses nothing back: its reply ends the instance.
impl WireReply for Termination {
    const OUTCOME: Outcome = Outcome::Diverges;

    fn results(_: &'static str) -> WireResults {
        WireResults::none()
    }

    fn checks(_: &'static str) -> Vec<Check> {
        Vec::new()
    }

    fn encode(self) -> Encoded {
        Encoded::Terminate(self.0)
    }
}
