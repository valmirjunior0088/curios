//! How each Rust type a host's row is written in crosses the wire — the knowledge the table's rows read off their types rather than spell as columns.
//!
//! An operand type states the [`WireType`] it arrives as ([`WireOperand`]). A payload states the slots a success fills, how it fills them, and the padding a reply without it fills them with ([`WirePayload`]). A reply type, built around a payload, states what crosses back, short of the one label its row states, and encodes the whole reply ([`WireReply`]): a bare payload crosses as its own slots; `Result<T, Failure>` puts a status ahead of them, as do `Result<Option<T>, Failure>`, a stream whose `None` is its end, and `Option<T>`, a lookup whose `None` is absence; `Result<T, Refusal>` crosses as its payload, its host free to refuse the call instead; and [`Termination`] crosses nothing, its row one that does not return. The generic shapes are disjoint because no `Option` or `Result` is a payload, which coherence holds rather than a comment.
//!
//! A reply encodes to the [`WireValue`]s it crosses with, in slot order ([`Encoded`]), which the runtime writes into wasm values: this leaf names no wasm value, and the table's types agree with the bindings built from them by construction, since both are read off the same row.

use {
    super::{
        ChildExit, ChildHandles, Failure, FileStat, Handle, Mode, Polled, Readiness, Refusal,
        SerialFlow, SerialOp, SerialParity, StdioMode, Termination, Timestamp, TtySize, WireLeaf,
        WireResults, WireReturn, WireType,
    },
    crate::status,
    std::convert::identity,
};

#[cfg(test)]
mod tests;

/// A type an operand of a host's row is written in: the wire type it arrives as.
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

impl WireOperand for bool {
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

impl WireOperand for Mode {
    const WIRE: WireType = WireType::Nat;
}

impl WireOperand for StdioMode {
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

/// One value a reply crosses with, in the vocabulary a row's results are written in.
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

/// The slots a payload fills, in the order they cross, as its type states them. `L` is what a row states to label them.
pub enum PayloadSlots<L> {
    /// No slot.
    None,
    /// One value. It has no label of its own: alone it crosses bare, and beside a status it crosses under the name read off what its row states.
    Value(WireType, fn(L) -> &'static str),
    /// Several fields, each under its field's name, which is what the guest projects: the first, the second, then the rest.
    Fields(
        (&'static str, WireType),
        (&'static str, WireType),
        Vec<(&'static str, WireType)>,
    ),
}

fn labelled((label, wire_type): (&'static str, WireType)) -> (String, WireType) {
    (label.to_string(), wire_type)
}

impl<L> PayloadSlots<L> {
    /// The results a reply of the payload alone crosses as.
    fn alone(self) -> WireResults {
        match self {
            PayloadSlots::None => WireResults::none(),
            PayloadSlots::Value(wire_type, _) => WireResults::single(wire_type),
            PayloadSlots::Fields(first, second, rest) => WireResults::record(
                labelled(first),
                labelled(second),
                rest.into_iter().map(labelled).collect(),
            ),
        }
    }

    /// The results a reply crosses as that carries a status: the status first, alone where the payload has no slot, then the payload's slots, a lone value under the name `label` gives it.
    fn after_status(self, label: L) -> WireResults {
        let status = ("status".to_string(), WireType::Nat);

        match self {
            PayloadSlots::None => WireResults::single(status.1),
            PayloadSlots::Value(wire_type, name) => {
                WireResults::record(status, labelled((name(label), wire_type)), Vec::new())
            }
            PayloadSlots::Fields(first, second, rest) => WireResults::record(
                status,
                labelled(first),
                [second].into_iter().chain(rest).map(labelled).collect(),
            ),
        }
    }

    /// The wire type of each slot, in order.
    fn types(&self) -> Vec<WireType> {
        match self {
            PayloadSlots::None => Vec::new(),
            PayloadSlots::Value(wire_type, _) => vec![*wire_type],
            PayloadSlots::Fields(first, second, rest) => [first, second]
                .into_iter()
                .chain(rest)
                .map(|(_, wire_type)| *wire_type)
                .collect(),
        }
    }
}

/// What a successful call carries: the slots it fills, how it fills them, and what a reply without it fills them with.
pub trait WirePayload: Sized {
    /// What a row states for every slot to have a label where one is read: a name for a lone value, and nothing for a payload of no slot or of fields that name themselves.
    type Label;

    fn slots() -> PayloadSlots<Self::Label>;

    fn encode(self, values: &mut Vec<WireValue>);

    /// Fill the slots of a reply that carries no payload — a failure, an end of stream or an absence. The values are inert: the guest reads the status first and never projects them.
    fn pad(values: &mut Vec<WireValue>);
}

impl WirePayload for () {
    type Label = ();

    fn slots() -> PayloadSlots<()> {
        PayloadSlots::None
    }

    fn encode(self, _: &mut Vec<WireValue>) {}

    fn pad(_: &mut Vec<WireValue>) {}
}

impl WirePayload for u64 {
    type Label = &'static str;

    fn slots() -> PayloadSlots<&'static str> {
        PayloadSlots::Value(WireType::Nat, identity)
    }

    fn encode(self, values: &mut Vec<WireValue>) {
        values.push(WireValue::Nat(self));
    }

    fn pad(values: &mut Vec<WireValue>) {
        values.push(WireValue::Nat(0));
    }
}

impl WirePayload for Vec<u8> {
    type Label = &'static str;

    fn slots() -> PayloadSlots<&'static str> {
        PayloadSlots::Value(WireType::Bytes, identity)
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
    type Label = &'static str;

    fn slots() -> PayloadSlots<&'static str> {
        PayloadSlots::Value(WireType::Handle, identity)
    }

    fn encode(self, values: &mut Vec<WireValue>) {
        values.push(WireValue::Handle(self));
    }

    fn pad(values: &mut Vec<WireValue>) {
        values.push(WireValue::Handle(Handle::none()));
    }
}

impl WirePayload for Vec<Vec<u8>> {
    type Label = &'static str;

    fn slots() -> PayloadSlots<&'static str> {
        PayloadSlots::Value(WireType::List(WireLeaf::Bytes), identity)
    }

    fn encode(self, values: &mut Vec<WireValue>) {
        values.push(WireValue::BytesList(self));
    }

    fn pad(values: &mut Vec<WireValue>) {
        values.push(WireValue::BytesList(Vec::new()));
    }
}

/// A poll's reports cross as two `Bytes`, one for each of its lists, byte `i` the report of that list's handle `i`.
impl WirePayload for Polled {
    type Label = ();

    fn slots() -> PayloadSlots<()> {
        PayloadSlots::Fields(
            ("readable", WireType::Bytes),
            ("writable", WireType::Bytes),
            Vec::new(),
        )
    }

    fn encode(self, values: &mut Vec<WireValue>) {
        values.extend(
            [self.readable, self.writable].map(|reports| {
                WireValue::Bytes(reports.into_iter().map(Readiness::bits).collect())
            }),
        );
    }

    fn pad(values: &mut Vec<WireValue>) {
        values.extend([WireValue::Bytes(Vec::new()), WireValue::Bytes(Vec::new())]);
    }
}

impl WirePayload for Timestamp {
    type Label = ();

    fn slots() -> PayloadSlots<()> {
        PayloadSlots::Fields(
            ("secs", WireType::Nat),
            ("nanos", WireType::Nat),
            Vec::new(),
        )
    }

    fn encode(self, values: &mut Vec<WireValue>) {
        values.extend([WireValue::Nat(self.secs), WireValue::Nat(self.nanos)]);
    }

    fn pad(values: &mut Vec<WireValue>) {
        values.extend([WireValue::Nat(0), WireValue::Nat(0)]);
    }
}

impl WirePayload for TtySize {
    type Label = ();

    fn slots() -> PayloadSlots<()> {
        PayloadSlots::Fields(("cols", WireType::Nat), ("rows", WireType::Nat), Vec::new())
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
    type Label = ();

    fn slots() -> PayloadSlots<()> {
        PayloadSlots::Fields(
            ("kind", WireType::Nat),
            ("size", WireType::Nat),
            vec![
                ("mtime_secs", WireType::Nat),
                ("mtime_nanos", WireType::Nat),
            ],
        )
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
}

/// An exit crosses as the pair `(code, signal)`, the field that does not apply zero: `signal` is nonzero exactly when a signal ended the child.
impl WirePayload for ChildExit {
    type Label = ();

    fn slots() -> PayloadSlots<()> {
        PayloadSlots::Fields(
            ("code", WireType::Nat),
            ("signal", WireType::Nat),
            Vec::new(),
        )
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
}

/// A stream the call did not pipe crosses as [`Handle::none`], the padding a failure fills every slot with: which streams a success holds is the call's to say.
impl WirePayload for ChildHandles {
    type Label = ();

    fn slots() -> PayloadSlots<()> {
        PayloadSlots::Fields(
            ("child", WireType::Handle),
            ("stdin", WireType::Handle),
            vec![("stdout", WireType::Handle), ("stderr", WireType::Handle)],
        )
    }

    fn encode(self, values: &mut Vec<WireValue>) {
        values.push(WireValue::Handle(self.child));
        values.extend(
            [self.stdin, self.stdout, self.stderr]
                .map(|stream| WireValue::Handle(stream.unwrap_or_else(Handle::none))),
        );
    }

    fn pad(values: &mut Vec<WireValue>) {
        values.extend([(); 4].map(|()| WireValue::Handle(Handle::none())));
    }
}

/// What a reply's type states of what crosses back, short of the label a row states for it. `L` is that label.
pub enum ReplyShape<L> {
    /// Nothing, ever: the reply ends the instance.
    Never,
    /// A payload's slots alone.
    Alone(WireResults),
    /// A status, then a payload's slots.
    AfterStatus(PayloadSlots<L>),
}

impl<L> ReplyShape<L> {
    /// What the call comes back with, under the label its row states.
    pub fn returns(self, label: L) -> WireReturn {
        match self {
            ReplyShape::Never => WireReturn::Never,
            ReplyShape::Alone(results) => WireReturn::Answers(results),
            ReplyShape::AfterStatus(slots) => WireReturn::Answers(slots.after_status(label)),
        }
    }

    /// The wire types that cross back, in order, which no label changes.
    pub fn types(&self) -> Vec<WireType> {
        match self {
            ReplyShape::Never => Vec::new(),
            ReplyShape::Alone(results) => results.types(),
            ReplyShape::AfterStatus(slots) => {
                [WireType::Nat].into_iter().chain(slots.types()).collect()
            }
        }
    }
}

/// A row's whole reply: what its call comes back with, and its encoding.
pub trait WireReply: Sized {
    /// What a row states beside the reply's type, in its `yields` clause: the name of a lone value that crosses beside a status, and nothing for every other reply. A row that states a label its reply has no slot for, or none where it has one, hands [`ReplyShape::returns`] the wrong type and does not compile.
    type Label;

    /// A payload's own slots, a status ahead of them where the reply carries one, or nothing ever for a reply that ends the instance.
    fn shape() -> ReplyShape<Self::Label>;

    fn encode(self) -> Encoded;
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
    type Label = ();

    fn shape() -> ReplyShape<()> {
        ReplyShape::Alone(T::slots().alone())
    }

    fn encode(self) -> Encoded {
        let mut values = Vec::new();
        WirePayload::encode(self, &mut values);

        Encoded::Reply(values)
    }
}

impl<T: WirePayload> WireReply for Result<T, Failure> {
    type Label = T::Label;

    fn shape() -> ReplyShape<T::Label> {
        ReplyShape::AfterStatus(T::slots())
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
    type Label = T::Label;

    fn shape() -> ReplyShape<T::Label> {
        ReplyShape::AfterStatus(T::slots())
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
    type Label = T::Label;

    fn shape() -> ReplyShape<T::Label> {
        ReplyShape::AfterStatus(T::slots())
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
    type Label = ();

    fn shape() -> ReplyShape<()> {
        ReplyShape::Alone(T::slots().alone())
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
    type Label = ();

    fn shape() -> ReplyShape<()> {
        ReplyShape::Never
    }

    fn encode(self) -> Encoded {
        Encoded::Terminate(self.0)
    }
}
