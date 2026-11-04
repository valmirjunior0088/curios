//! An intrinsic node: `%` and its Core variant's name, then its operands in the order Core holds them; in a reader's spelling, as a program writes it.
//!
//! **A literal form builds its own node.** A numeral is the `Nat` it spells, `true` and `false` the `Bool`s, and `x[…]` the `Bytes` holding those bytes; every other value of a carrier states its variant: `%Int(-3)`, `%Byte(7)`, `%Flt(+1.5)`, `%Handle(2)`. A `Nat` over a term is `%Nat(Succ(3, n))`. An operation's operands are its term operands, a rounding as its variant's name, a list of operands between brackets, and an instance as a decorator's universes are written: `%FltAdd(TiesToEven, a, b)`, `%BytesConcat([a, b])`. A `Flt` is the literal that reads back as it bit for bit, or its bits where no literal spells it: a NaN other than the default one of either sign.

use {
    super::{bracketed, universes_written},
    crate::{Frame, IntrinsicForm, LiteralEntry, intrinsic_form, listed, marked_arguments, sub},
    curios_core::{Intrinsic, Nat, Subterm, Term, Transient, Universes},
    curios_num::Floating,
    curios_print::{Printer, flat, pure, sep_flat},
};

/// `intrinsic` as Core text.
pub fn intrinsic_written(intrinsic: Intrinsic, frame: Frame) -> Printer {
    if !frame.core_text() {
        return reader_intrinsic(intrinsic, frame);
    }
    match intrinsic {
        Intrinsic::Bool(value) => pure(value.to_string()),
        Intrinsic::Nat(nat) => nat_written(nat, frame),
        Intrinsic::Byte(byte) => called("Byte", vec![pure(byte.to_string())]),
        Intrinsic::Int(integer) => called("Int", vec![pure(integer.to_string())]),
        Intrinsic::Flt(flt) => called("Flt", vec![pure(flt_written(flt))]),
        Intrinsic::Bytes(bytes) => bracketed_packed(bytes.as_bytes()),
        Intrinsic::Handle(handle) => called("Handle", vec![pure(handle.to_string())]),
        Intrinsic::BoolType => called("BoolType", Vec::new()),
        Intrinsic::BoolAnd(operand0, operand1) => {
            called("BoolAnd", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::BoolOr(operand0, operand1) => {
            called("BoolOr", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::BoolXor(operand0, operand1) => {
            called("BoolXor", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::BoolEql(operand0, operand1) => {
            called("BoolEql", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::BoolNeq(operand0, operand1) => {
            called("BoolNeq", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::NatType => called("NatType", Vec::new()),
        Intrinsic::NatEql(operand0, operand1) => {
            called("NatEql", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::NatNeq(operand0, operand1) => {
            called("NatNeq", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::NatAdd(operand0, operand1) => {
            called("NatAdd", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::NatSub(operand0, operand1) => {
            called("NatSub", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::NatMul(operand0, operand1) => {
            called("NatMul", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::NatLt(operand0, operand1) => {
            called("NatLt", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::NatDiv {
            dividend,
            divisor,
            non_zero,
        } => called(
            "NatDiv",
            vec![
                sub(dividend, frame),
                sub(divisor, frame),
                sub(non_zero, frame),
            ],
        ),
        Intrinsic::NatRem {
            dividend,
            divisor,
            non_zero,
        } => called(
            "NatRem",
            vec![
                sub(dividend, frame),
                sub(divisor, frame),
                sub(non_zero, frame),
            ],
        ),
        Intrinsic::NatLe(operand0, operand1) => {
            called("NatLe", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::NatAnd(operand0, operand1) => {
            called("NatAnd", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::NatOr(operand0, operand1) => {
            called("NatOr", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::NatXor(operand0, operand1) => {
            called("NatXor", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::NatMin(operand0, operand1) => {
            called("NatMin", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::NatMax(operand0, operand1) => {
            called("NatMax", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::NatPow(operand0, operand1) => {
            called("NatPow", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::NatShl(operand0, operand1) => {
            called("NatShl", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::NatShr(operand0, operand1) => {
            called("NatShr", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::ByteType => called("ByteType", Vec::new()),
        Intrinsic::ByteToNat(operand0) => called("ByteToNat", vec![sub(operand0, frame)]),
        Intrinsic::NatToByte { nat, below } => {
            called("NatToByte", vec![sub(nat, frame), sub(below, frame)])
        }
        Intrinsic::IntType => called("IntType", Vec::new()),
        Intrinsic::IntEql(operand0, operand1) => {
            called("IntEql", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::IntNeq(operand0, operand1) => {
            called("IntNeq", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::IntAdd(operand0, operand1) => {
            called("IntAdd", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::IntSub(operand0, operand1) => {
            called("IntSub", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::IntMul(operand0, operand1) => {
            called("IntMul", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::IntDiv {
            dividend,
            divisor,
            non_zero,
        } => called(
            "IntDiv",
            vec![
                sub(dividend, frame),
                sub(divisor, frame),
                sub(non_zero, frame),
            ],
        ),
        Intrinsic::IntRem {
            dividend,
            divisor,
            non_zero,
        } => called(
            "IntRem",
            vec![
                sub(dividend, frame),
                sub(divisor, frame),
                sub(non_zero, frame),
            ],
        ),
        Intrinsic::IntLt(operand0, operand1) => {
            called("IntLt", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::IntLe(operand0, operand1) => {
            called("IntLe", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::IntAnd(operand0, operand1) => {
            called("IntAnd", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::IntOr(operand0, operand1) => {
            called("IntOr", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::IntXor(operand0, operand1) => {
            called("IntXor", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::IntMin(operand0, operand1) => {
            called("IntMin", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::IntMax(operand0, operand1) => {
            called("IntMax", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::IntAbs(operand0) => called("IntAbs", vec![sub(operand0, frame)]),
        Intrinsic::IntSign(operand0) => called("IntSign", vec![sub(operand0, frame)]),
        Intrinsic::IntShl(operand0, operand1) => {
            called("IntShl", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::IntShr(operand0, operand1) => {
            called("IntShr", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::FltType => called("FltType", Vec::new()),
        Intrinsic::FltAdd(operand0, operand1, operand2) => called(
            "FltAdd",
            vec![
                pure(format!("{operand0:?}")),
                sub(operand1, frame),
                sub(operand2, frame),
            ],
        ),
        Intrinsic::FltSub(operand0, operand1, operand2) => called(
            "FltSub",
            vec![
                pure(format!("{operand0:?}")),
                sub(operand1, frame),
                sub(operand2, frame),
            ],
        ),
        Intrinsic::FltMul(operand0, operand1, operand2) => called(
            "FltMul",
            vec![
                pure(format!("{operand0:?}")),
                sub(operand1, frame),
                sub(operand2, frame),
            ],
        ),
        Intrinsic::FltDiv(operand0, operand1, operand2) => called(
            "FltDiv",
            vec![
                pure(format!("{operand0:?}")),
                sub(operand1, frame),
                sub(operand2, frame),
            ],
        ),
        Intrinsic::FltFma(operand0, operand1, operand2, operand3) => called(
            "FltFma",
            vec![
                pure(format!("{operand0:?}")),
                sub(operand1, frame),
                sub(operand2, frame),
                sub(operand3, frame),
            ],
        ),
        Intrinsic::FltRem(operand0, operand1) => {
            called("FltRem", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::FltEql(operand0, operand1) => {
            called("FltEql", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::FltNeq(operand0, operand1) => {
            called("FltNeq", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::FltLt(operand0, operand1) => {
            called("FltLt", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::FltLe(operand0, operand1) => {
            called("FltLe", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::FltMin(operand0, operand1) => {
            called("FltMin", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::FltMax(operand0, operand1) => {
            called("FltMax", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::FltNeg(operand0) => called("FltNeg", vec![sub(operand0, frame)]),
        Intrinsic::FltAbs(operand0) => called("FltAbs", vec![sub(operand0, frame)]),
        Intrinsic::FltSqrt(operand0, operand1) => called(
            "FltSqrt",
            vec![pure(format!("{operand0:?}")), sub(operand1, frame)],
        ),
        Intrinsic::FltRoundIntegral(operand0, operand1) => called(
            "FltRoundIntegral",
            vec![pure(format!("{operand0:?}")), sub(operand1, frame)],
        ),
        Intrinsic::FltCopysign(operand0, operand1) => called(
            "FltCopysign",
            vec![sub(operand0, frame), sub(operand1, frame)],
        ),
        Intrinsic::NatToInt(operand0) => called("NatToInt", vec![sub(operand0, frame)]),
        Intrinsic::NatToFlt(operand0, operand1) => called(
            "NatToFlt",
            vec![pure(format!("{operand0:?}")), sub(operand1, frame)],
        ),
        Intrinsic::IntToNat { int, non_neg } => {
            called("IntToNat", vec![sub(int, frame), sub(non_neg, frame)])
        }
        Intrinsic::IntToFlt(operand0, operand1) => called(
            "IntToFlt",
            vec![pure(format!("{operand0:?}")), sub(operand1, frame)],
        ),
        Intrinsic::FltToNat { flt, non_neg } => {
            called("FltToNat", vec![sub(flt, frame), sub(non_neg, frame)])
        }
        Intrinsic::FltToLeBytes(operand0) => called("FltToLeBytes", vec![sub(operand0, frame)]),
        Intrinsic::FltOfLeBytes { bytes, eight_bytes } => called(
            "FltOfLeBytes",
            vec![sub(bytes, frame), sub(eight_bytes, frame)],
        ),
        Intrinsic::FltToInt { flt, finite } => {
            called("FltToInt", vec![sub(flt, frame), sub(finite, frame)])
        }
        Intrinsic::FltMantissa { flt, finite } => {
            called("FltMantissa", vec![sub(flt, frame), sub(finite, frame)])
        }
        Intrinsic::FltExponent { flt, finite } => {
            called("FltExponent", vec![sub(flt, frame), sub(finite, frame)])
        }
        Intrinsic::BytesType => called("BytesType", Vec::new()),
        Intrinsic::BytesLen(operand0) => called("BytesLen", vec![sub(operand0, frame)]),
        Intrinsic::BytesEql(operand0, operand1) => {
            called("BytesEql", vec![sub(operand0, frame), sub(operand1, frame)])
        }
        Intrinsic::BytesGet {
            bytes,
            index,
            in_range,
        } => called(
            "BytesGet",
            vec![sub(bytes, frame), sub(index, frame), sub(in_range, frame)],
        ),
        Intrinsic::BytesSlice {
            bytes,
            start,
            length,
            within,
        } => called(
            "BytesSlice",
            vec![
                sub(bytes, frame),
                sub(start, frame),
                sub(length, frame),
                sub(within, frame),
            ],
        ),
        Intrinsic::BytesAppend { bytes, element } => {
            called("BytesAppend", vec![sub(bytes, frame), sub(element, frame)])
        }
        Intrinsic::BytesConcat { operands } => {
            called("BytesConcat", vec![listed_terms(operands, frame)])
        }
        Intrinsic::BytesReplicate { count, atom } => {
            called("BytesReplicate", vec![sub(count, frame), sub(atom, frame)])
        }
        Intrinsic::BytesAnd {
            left,
            right,
            same_length,
        } => called(
            "BytesAnd",
            vec![sub(left, frame), sub(right, frame), sub(same_length, frame)],
        ),
        Intrinsic::BytesOr {
            left,
            right,
            same_length,
        } => called(
            "BytesOr",
            vec![sub(left, frame), sub(right, frame), sub(same_length, frame)],
        ),
        Intrinsic::BytesXor {
            left,
            right,
            same_length,
        } => called(
            "BytesXor",
            vec![sub(left, frame), sub(right, frame), sub(same_length, frame)],
        ),
        Intrinsic::ListType(operand0) => called("ListType", vec![sub(operand0, frame)]),
        Intrinsic::List { element, items } => called(
            "List",
            vec![sub(element, frame), listed_terms(items, frame)],
        ),
        Intrinsic::ListLen { element, list } => {
            called("ListLen", vec![sub(element, frame), sub(list, frame)])
        }
        Intrinsic::ListGet {
            element,
            list,
            index,
            in_range,
        } => called(
            "ListGet",
            vec![
                sub(element, frame),
                sub(list, frame),
                sub(index, frame),
                sub(in_range, frame),
            ],
        ),
        Intrinsic::ListSlice {
            element,
            list,
            start,
            length,
            within,
        } => called(
            "ListSlice",
            vec![
                sub(element, frame),
                sub(list, frame),
                sub(start, frame),
                sub(length, frame),
                sub(within, frame),
            ],
        ),
        Intrinsic::ListAppend {
            element,
            list,
            item,
        } => called(
            "ListAppend",
            vec![sub(element, frame), sub(list, frame), sub(item, frame)],
        ),
        Intrinsic::ListConcat { element, operands } => called(
            "ListConcat",
            vec![sub(element, frame), listed_terms(operands, frame)],
        ),
        Intrinsic::ListMap {
            from,
            to,
            list,
            function,
        } => called(
            "ListMap",
            vec![
                sub(from, frame),
                sub(to, frame),
                sub(list, frame),
                sub(function, frame),
            ],
        ),
        Intrinsic::ListFold {
            element,
            result,
            list,
            init,
            function,
        } => called(
            "ListFold",
            vec![
                sub(element, frame),
                sub(result, frame),
                sub(list, frame),
                sub(init, frame),
                sub(function, frame),
            ],
        ),
        Intrinsic::HandleType => called("HandleType", Vec::new()),
        Intrinsic::CellType(operand0) => called("CellType", vec![sub(operand0, frame)]),
        Intrinsic::Cell { element } => called("Cell", vec![sub(element, frame)]),
        Intrinsic::CellFill {
            element,
            cell,
            value,
        } => called(
            "CellFill",
            vec![sub(element, frame), sub(cell, frame), sub(value, frame)],
        ),
        Intrinsic::CellPoll {
            element,
            cell,
            universes,
        } => called(
            "CellPoll",
            vec![
                sub(element, frame),
                sub(cell, frame),
                universes_value(&universes, frame),
            ],
        ),
        Intrinsic::ChannelType(operand0) => called("ChannelType", vec![sub(operand0, frame)]),
        Intrinsic::Channel {
            element,
            capacity,
            positive,
        } => called(
            "Channel",
            vec![
                sub(element, frame),
                sub(capacity, frame),
                sub(positive, frame),
            ],
        ),
        Intrinsic::ChannelPush {
            element,
            channel,
            value,
        } => called(
            "ChannelPush",
            vec![sub(element, frame), sub(channel, frame), sub(value, frame)],
        ),
        Intrinsic::ChannelTake {
            element,
            channel,
            universes,
        } => called(
            "ChannelTake",
            vec![
                sub(element, frame),
                sub(channel, frame),
                universes_value(&universes, frame),
            ],
        ),
        Intrinsic::ChannelClose { element, channel } => called(
            "ChannelClose",
            vec![sub(element, frame), sub(channel, frame)],
        ),
        Intrinsic::ChannelClosed { element, channel } => called(
            "ChannelClosed",
            vec![sub(element, frame), sub(channel, frame)],
        ),
        Intrinsic::ChannelCount { element, channel } => called(
            "ChannelCount",
            vec![sub(element, frame), sub(channel, frame)],
        ),
        Intrinsic::ChannelCapacity { element, channel } => called(
            "ChannelCapacity",
            vec![sub(element, frame), sub(channel, frame)],
        ),
        Intrinsic::IoType(operand0) => called("IoType", vec![sub(operand0, frame)]),
        Intrinsic::IoPure { result, value } => {
            called("IoPure", vec![sub(result, frame), sub(value, frame)])
        }
        Intrinsic::IoBind {
            from,
            to,
            action,
            continuation,
        } => called(
            "IoBind",
            vec![
                sub(from, frame),
                sub(to, frame),
                sub(action, frame),
                sub(continuation, frame),
            ],
        ),
        Intrinsic::IoSuspend {
            result,
            on,
            filled,
            cancel,
            rest,
        } => called(
            "IoSuspend",
            vec![
                sub(result, frame),
                sub(on, frame),
                sub(filled, frame),
                sub(cancel, frame),
                sub(rest, frame),
            ],
        ),
        Intrinsic::IoAdvance {
            from,
            to,
            action,
            done,
            suspended,
        } => called(
            "IoAdvance",
            vec![
                sub(from, frame),
                sub(to, frame),
                sub(action, frame),
                sub(done, frame),
                sub(suspended, frame),
            ],
        ),
    }
}

/// A `Nat`: a numeral where it is one, and its variant over the term it offsets otherwise.
fn nat_written(nat: Nat, frame: Frame) -> Printer {
    match nat {
        Nat::Zero => pure("0"),
        Nat::Succ(offset, tail) => match &*tail {
            Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) => pure(offset.to_string()),
            _ => called(
                "Nat",
                vec![flat([
                    pure(format!("Succ({offset}, ")),
                    sub(tail, frame),
                    pure(")"),
                ])],
            ),
        },
    }
}

/// A `Flt` as the literal that reads back as it, or its bits as hexadecimal where none does.
fn flt_written(flt: Floating) -> String {
    let sign = match flt.abs().to_bits() == flt.to_bits() {
        true => "+",
        false => "-",
    };
    if flt.is_nan() {
        return match flt.abs().to_bits() == Floating::nan().to_bits() {
            true => format!("{sign}nan.0"),
            false => format!("0x{:016x}", flt.to_bits()),
        };
    }
    if !flt.is_finite() {
        return format!("{sign}inf.0");
    }
    let mut written = format!("{:+}", f64::from(flt));
    let digits = &written[1..];
    match digits.find(['e', 'E']) {
        Some(exponent) if !digits[..exponent].contains('.') => {
            written.insert_str(1 + exponent, ".0");
        }
        Some(_) => {}
        None if !digits.contains('.') => written.push_str(".0"),
        None => {}
    }

    written
}

/// A packed literal over `bytes`.
fn bracketed_packed(bytes: &[u8]) -> Printer {
    flat([
        pure("x"),
        bracketed(
            bytes
                .iter()
                .map(|byte| pure(format!("0x{byte:02x}")))
                .collect(),
        ),
    ])
}

/// A list of term operands between brackets.
fn listed_terms(terms: Vec<Term>, frame: Frame) -> Printer {
    bracketed(terms.into_iter().map(|term| sub(term, frame)).collect())
}

/// An instance an operation states, as a decorator's universes are written, `{}` where it states none.
fn universes_value(universes: &Universes, frame: Frame) -> Printer {
    universes_written(universes, frame).unwrap_or_else(|| pure("{}"))
}

/// Whether `intrinsic` is written with a list of operands after its name, which a call's own list could not then be read as: a carrier's type is its name alone.
pub fn writes_operands(intrinsic: &Intrinsic) -> bool {
    !matches!(
        intrinsic,
        Intrinsic::BoolType
            | Intrinsic::NatType
            | Intrinsic::ByteType
            | Intrinsic::IntType
            | Intrinsic::FltType
            | Intrinsic::BytesType
            | Intrinsic::HandleType
    )
}

/// `%` and a variant's name, then its operands where it has any.
fn called(name: &str, operands: Vec<Printer>) -> Printer {
    let head = format!("%{name}");
    match operands.is_empty() {
        true => pure(head),
        false => listed(format!("{head}("), false, operands, ")"),
    }
}

/// An intrinsic as a reader writes it, read off the one table of how one is written ([`intrinsic_form`]): a type former or a constant by the path under `/sys` its declaration stands at, spelled as the reader reaches it; an operation as its call or its operator; a literal as itself.
fn reader_intrinsic(intrinsic: Intrinsic, frame: Frame) -> Printer {
    // The constant atoms as the surface writes them: hexadecimal numerals for bytes.
    let entry = |entry: LiteralEntry| match entry {
        LiteralEntry::Byte(byte) => pure(format!("0x{byte:X}")),
        LiteralEntry::Item(item) => sub(item, frame),
        LiteralEntry::Spread(run) => flat([pure(".."), sub(run, frame)]),
    };

    match intrinsic_form(intrinsic, frame.spelling.keeps_grouping()) {
        IntrinsicForm::Named(path) => pure(frame.spelling.intrinsic_symbol(path)),
        IntrinsicForm::Call {
            path,
            implicits,
            explicits,
        } => reader_call(path, implicits, explicits, frame),
        IntrinsicForm::Infix(op, left, right) => reader_infix(op.symbol(), left, right, frame),
        IntrinsicForm::Bool(value) => pure(value.to_string()),
        IntrinsicForm::Nat(value) => pure(value.to_string()),
        // Spelled infix to match the operator intrinsics, its tail parenthesized when it too is an operator: `n + 1`, `(n + m) + 3`.
        IntrinsicForm::NatAbove { tail, floor } => {
            flat([reader_operand(tail, frame), pure(format!(" + {floor}"))])
        }
        IntrinsicForm::Byte(value) => pure(format!("0x{value:02X}")),
        IntrinsicForm::Int(value) => pure(format!("{value:+}")),
        IntrinsicForm::Flt(value) => reader_flt(value, frame),
        IntrinsicForm::Bytes(entries) => flat([
            pure("x["),
            sep_flat(entries.into_iter().map(entry).collect::<Vec<_>>(), || {
                pure(", ")
            }),
            pure("]"),
        ]),
        IntrinsicForm::List(entries) => flat([
            pure("["),
            sep_flat(entries.into_iter().map(entry).collect::<Vec<_>>(), || {
                pure(", ")
            }),
            pure("]"),
        ]),
        // A token no source can spell, spelled rather than abort the diagnostic it is inside.
        IntrinsicForm::Handle(token) => pure(format!("Handle({token})")),
    }
}

/// An operation called as the surface calls it — `Nat/shl(a, b)` — by the path `/sys` declares it at, spelled as the reader reaches it, its type arguments under `@`. The proof an operation carries is no argument here: the reader never wrote one.
fn reader_call(
    path: impl AsRef<str>,
    implicits: Vec<Term>,
    explicits: Vec<Term>,
    frame: Frame,
) -> Printer {
    let name = frame.spelling.intrinsic_symbol(path.as_ref());
    let arguments = marked_arguments(implicits, explicits, frame);
    // A row with no parameters is a constant in `/sys`, not a nullary function, and is named the way a constant is.
    if arguments.is_empty() {
        return pure(name);
    }

    flat([pure(name), listed("(".into(), false, arguments, ")")])
}

/// An operator as `left <symbol> right`, each operand parenthesized where it is itself an operator, so nesting stays unambiguous — `(a + b) * c`, never `a + b * c`.
pub fn reader_infix(symbol: &'static str, left: Term, right: Term, frame: Frame) -> Printer {
    flat([
        reader_operand(left, frame),
        pure(format!(" {symbol} ")),
        reader_operand(right, frame),
    ])
}

/// An operand of [`reader_infix`], parenthesized where it too prints infix — an operator intrinsic, an operator elaboration has not settled, or a successor over a term, which prints as `k + 1` — and bare where it delimits itself.
fn reader_operand(term: Term, frame: Frame) -> Printer {
    let parenthesize = match &*term {
        Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(_, tail))) => {
            !matches!(tail.as_ref(), Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)))
        }
        Subterm::Intrinsic(intrinsic) => matches!(
            intrinsic_form(intrinsic.clone(), false),
            IntrinsicForm::Infix(..)
        ),
        Subterm::Transient(Transient::Infix(_)) => true,
        _ => false,
    };

    match parenthesize {
        true => flat([pure("("), sub(term, frame), pure(")")]),
        false => sub(term, frame),
    }
}

/// A `Flt` as what reads back as it bit for bit: a finite value as its signed decimal, an infinity and the default NaN of either sign as their literals, and any other NaN, which no literal spells, as the call building it from its bytes.
fn reader_flt(flt: Floating, frame: Frame) -> Printer {
    if flt.is_nan() && flt.abs().to_bits() != Floating::nan().to_bits() {
        let bytes = Term::intrinsic(Intrinsic::Bytes(flt.to_le_bytes()));
        return reader_call("Flt/of_le_bytes", vec![], vec![bytes], frame);
    }

    pure(flt_written(flt))
}
