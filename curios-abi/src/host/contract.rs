//! What a builtin row promises beyond its types, and the one evaluator that holds a native call to it.
//!
//! A row's contract has four parts. Its outcome is its reply type's ([`Outcome`]). Its failures are the operating system's named statuses and the errno lane on every fallible row, with `would_block` only on a row marked [`Mark::Blocks`] and `tls` only on one marked [`Mark::Tls`]; a stream alone ends, and a lookup answers `ok` or `not_found` and nothing else. Its requirements relate its operands before the call ([`Requirement`]). Its checks relate a successful reply to its operands or to itself ([`Check`]): the row's own, which read its lone payload, and its payload type's, which read the payload's fields by label.
//!
//! [`HostOp::admit`] holds a call's operands to the requirements, and [`HostOp::check_reply`] its encoded reply to the rest, each answering the sentence a refusal reports. A check is data rather than a predicate, so whatever reads the table reads the same contract.

use {
    super::{HostOp, Outcome, WireValue},
    crate::status,
};

#[cfg(test)]
mod tests;

/// A failure a row may answer beyond the operating system's statuses and the errno lane, which every fallible row may answer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mark {
    /// The row may answer `would_block`: a peer, a resolver or a child decides when it can progress, and `handle_poll` is where the wait happens.
    Blocks,
    /// The row may answer `tls`: `rustls` does its work.
    Tls,
}

/// A relation a call's operands must hold before the host is asked at all; a call that breaks one is malformed, and refused.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Requirement {
    /// `a` and `b` have one length.
    SameLength { a: &'static str, b: &'static str },
}

/// What a successful reply answers to. The first five read the row's lone payload and are the row's own; the rest name the fields they read and come from the payload's type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Check {
    /// A read's bytes: between one and `request` when `request` is positive, and none when it is zero.
    Progress { request: &'static str },
    /// A write's count: between one and the length of `buffer` when `buffer` is nonempty, and zero when it is empty.
    Accepted { buffer: &'static str },
    /// Exactly `request` bytes.
    Exact { request: &'static str },
    /// One element for each element of `list`.
    Parallel { list: &'static str },
    /// At least one element.
    NonEmpty,
    /// A handle that is not the empty token.
    Present { field: &'static str },
    /// Every byte of `field` within `allowed`.
    Mask { field: &'static str, allowed: u8 },
    /// `field` below `bound`.
    Below { field: &'static str, bound: u64 },
    /// `field` one of `codes`.
    Code {
        field: &'static str,
        codes: &'static [u64],
    },
    /// A child's end, exactly one field active: `code` at most 255 with `signal` zero, or `signal` positive with `code` zero.
    Exit {
        code: &'static str,
        signal: &'static str,
    },
}

/// The named statuses every fallible row may answer: the operating system's own.
const SYSTEM: &[u64] = &[
    status::NOT_FOUND,
    status::PERMISSION_DENIED,
    status::ALREADY_EXISTS,
    status::CONNECTION_REFUSED,
    status::NOT_EMPTY,
    status::IS_DIRECTORY,
    status::NOT_DIRECTORY,
];

impl HostOp {
    /// Refuse a call whose operands break a requirement of the row, `operands` their [`measure`](super::WireOperand::measure)s in the row's order.
    pub fn admit(self, operands: &[Option<u64>]) -> Result<(), String> {
        for requirement in self.requirements() {
            match *requirement {
                Requirement::SameLength { a, b } => {
                    let (a_length, b_length) =
                        (self.operand(operands, a), self.operand(operands, b));

                    if a_length != b_length {
                        return Err(format!(
                            "`{a}` holds {a_length} elements and `{b}` {b_length}, which must be as many"
                        ));
                    }
                }
            }
        }

        Ok(())
    }

    /// Refuse a reply outside the row's contract: a status it cannot answer, or a success whose values break a check. `values` are the reply's in slot order and `operands` the call's measures, as [`admit`](Self::admit) read them.
    pub fn check_reply(self, values: &[WireValue], operands: &[Option<u64>]) -> Result<(), String> {
        let labels = self.signature().results.iter().map(|(label, _)| label);
        let mut fields = labels.zip(values).collect::<Vec<_>>();

        match self.outcome() {
            Outcome::Diverges => return Ok(()),
            Outcome::Returns => {}
            outcome => {
                let (_, status) = fields.remove(0);

                if !self.succeeded(outcome, nat(status))? {
                    return Ok(());
                }
            }
        }

        let payload = || {
            fields
                .last()
                .map(|(_, value)| *value)
                .expect("a row check reads a payload")
        };
        let field = |name: &str| {
            fields
                .iter()
                .find(|(label, _)| *label == name)
                .map(|(_, value)| *value)
                .unwrap_or_else(|| panic!("`{}` checks a field it has no `{name}` of", self.name()))
        };

        for check in self.checks() {
            match *check {
                Check::Progress { request } => {
                    let (length, request) = (length(payload()), self.operand(operands, request));

                    if (request > 0 && !(1..=request).contains(&length))
                        || (request == 0 && length > 0)
                    {
                        return Err(format!(
                            "answered {length} bytes to a request for {request}"
                        ));
                    }
                }
                Check::Accepted { buffer } => {
                    let (accepted, offered) = (nat(payload()), self.operand(operands, buffer));

                    if (offered > 0 && !(1..=offered).contains(&accepted))
                        || (offered == 0 && accepted > 0)
                    {
                        return Err(format!("accepted {accepted} of {offered} bytes"));
                    }
                }
                Check::Exact { request } => {
                    let (length, request) = (length(payload()), self.operand(operands, request));

                    if length != request {
                        return Err(format!(
                            "answered {length} bytes to a request for exactly {request}"
                        ));
                    }
                }
                Check::Parallel { list } => {
                    let (length, expected) = (length(payload()), self.operand(operands, list));

                    if length != expected {
                        return Err(format!(
                            "answered {length} elements for the {expected} of `{list}`"
                        ));
                    }
                }
                Check::NonEmpty => {
                    if length(payload()) == 0 {
                        return Err(
                            "succeeded with nothing where a success holds something".to_string()
                        );
                    }
                }
                Check::Present { field: name } => {
                    if let WireValue::Handle(handle) = field(name)
                        && handle.is_none()
                    {
                        return Err(format!("succeeded with the empty token as `{name}`"));
                    }
                }
                Check::Mask {
                    field: name,
                    allowed,
                } => {
                    if let WireValue::Bytes(bytes) = field(name)
                        && let Some(byte) = bytes.iter().find(|byte| **byte & !allowed != 0)
                    {
                        return Err(format!(
                            "answered `{name}` mask {byte:#06b}, outside {allowed:#06b}"
                        ));
                    }
                }
                Check::Below { field: name, bound } => {
                    let value = nat(field(name));

                    if value >= bound {
                        return Err(format!(
                            "answered `{name}` {value}, which must be below {bound}"
                        ));
                    }
                }
                Check::Code { field: name, codes } => {
                    let value = nat(field(name));

                    if !codes.contains(&value) {
                        return Err(format!(
                            "answered `{name}` {value}, which is none of its codes"
                        ));
                    }
                }
                Check::Exit { code, signal } => {
                    let (code, signal) = (nat(field(code)), nat(field(signal)));

                    if !((signal == 0 && code <= 255) || (signal > 0 && code == 0)) {
                        return Err(format!(
                            "answered code {code} beside signal {signal}, where exactly one ends a child"
                        ));
                    }
                }
            }
        }

        Ok(())
    }

    /// Whether `code` is a success, or a failure the row may answer — refusing any other.
    fn succeeded(self, outcome: Outcome, code: u64) -> Result<bool, String> {
        let answers = match code {
            status::OK => return Ok(true),
            status::NOT_FOUND if outcome == Outcome::Lookup => true,
            _ if outcome == Outcome::Lookup => false,
            status::EOF => outcome == Outcome::Stream,
            status::WOULD_BLOCK => self.blocks(),
            status::TLS_ERROR => self.tls(),
            _ if SYSTEM.contains(&code) => true,
            _ => (status::OTHER_BASE..=status::OTHER_BASE + status::ERRNO_MAX).contains(&code),
        };

        match answers {
            true => Ok(false),
            false => Err(format!(
                "answered status {code}, which this row never answers"
            )),
        }
    }

    /// The measure of the operand the row names `name`.
    fn operand(self, operands: &[Option<u64>], name: &str) -> u64 {
        self.signature()
            .params
            .iter()
            .position(|(param, _)| param == name)
            .and_then(|index| operands[index])
            .unwrap_or_else(|| {
                panic!(
                    "`{}` reads `{name}`, which it has no measure of",
                    self.name()
                )
            })
    }
}

/// A `Nat` slot's value.
fn nat(value: &WireValue) -> u64 {
    match value {
        WireValue::Nat(value) => *value,
        other => panic!("{other:?} is not a `Nat`"),
    }
}

/// How many bytes or elements a reference slot holds, or a `Nat` slot's value where a count is what crosses.
fn length(value: &WireValue) -> u64 {
    match value {
        WireValue::Nat(value) => *value,
        WireValue::Bytes(bytes) => bytes.len() as u64,
        WireValue::BytesList(list) => list.len() as u64,
        WireValue::Handle(_) => panic!("a handle has no length"),
    }
}
