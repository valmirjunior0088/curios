//! An intrinsic node: `#` and its Core variant's name, then its operands in the order Core holds them; in a reader's spelling, as a program writes it.
//!
//! **A literal form builds its own node.** A numeral is the `Nat` it spells, `true` and `false` the `Bool`s, and `x[…]` the `Bytes` holding those bytes; every other value of a carrier states its variant: `#Int(-3)`, `#Byte(7)`, `#Flt(+1.5)`, `#Handle(2)`. A `Nat` over a term is `#Nat(Succ(3, n))`. An operation's operands are its term operands, a rounding as its variant's name, a list of operands between brackets, and an instance as a decorator's universes are written: `#FltAdd(TiesToEven, a, b)`, `#BytesConcat([a, b])`. A `Flt` is the literal that reads back as it bit for bit, or its bits where no literal spells it: a NaN other than the default one of either sign.

use {
    super::{
        ReadError, Reader, Refusal, bracketed, core_read, malformed, term_word, universes_read,
        universes_written, value_term, value_unmarked, value_word,
    },
    crate::{
        BytesSegment, CoreIntrinsic, CoreText, CoreValue, Frame, IntrinsicForm, Literal,
        LiteralEntry, NumLit, intrinsic_form, listed, marked_arguments, sub,
    },
    curios_common::{Sign, Span},
    curios_core::{Intrinsic, Nat, Subterm, Term, Transient, Universes},
    curios_num::{Binary, Floating, Integer, Rounding},
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

/// `#` and a variant's name, then its operands where it has any.
fn called(name: &str, operands: Vec<Printer>) -> Printer {
    let head = format!("#{name}");
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

/// An intrinsic node as Core text writes it, `#` and its variant's name over its operands, each read as its variant holds it: a term, a rounding by its name, a list of terms, an instance as a decorator's universes are written; a carrier's value by what it is.
pub fn intrinsic_read(
    intrinsic: &CoreIntrinsic,
    reader: &mut Reader<'_>,
    span: Option<&Span>,
) -> Result<Term, ReadError> {
    let read = match intrinsic.variant.as_str() {
        "Nat" => {
            let [nat] = operands(intrinsic, span)?;
            Intrinsic::Nat(nat_read(nat, reader)?)
        }
        "Byte" => {
            let [byte] = operands(intrinsic, span)?;
            Intrinsic::Byte(
                unsigned(byte)
                    .and_then(|byte| u8::try_from(&byte).ok())
                    .ok_or_else(|| malformed("a byte's value", byte))?,
            )
        }
        "Int" => {
            let [integer] = operands(intrinsic, span)?;
            Intrinsic::Int(int_read(integer)?)
        }
        "Flt" => {
            let [flt] = operands(intrinsic, span)?;
            Intrinsic::Flt(flt_read(flt)?)
        }
        "Handle" => {
            let [handle] = operands(intrinsic, span)?;
            Intrinsic::Handle(
                unsigned(handle)
                    .and_then(|handle| u32::try_from(&handle).ok())
                    .ok_or_else(|| malformed("a handle's number", handle))?,
            )
        }
        "BoolType" => {
            operands::<0>(intrinsic, span)?;
            Intrinsic::BoolType
        }
        "BoolAnd" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::BoolAnd(term(operand0, reader)?, term(operand1, reader)?)
        }
        "BoolOr" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::BoolOr(term(operand0, reader)?, term(operand1, reader)?)
        }
        "BoolXor" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::BoolXor(term(operand0, reader)?, term(operand1, reader)?)
        }
        "BoolEql" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::BoolEql(term(operand0, reader)?, term(operand1, reader)?)
        }
        "BoolNeq" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::BoolNeq(term(operand0, reader)?, term(operand1, reader)?)
        }
        "NatType" => {
            operands::<0>(intrinsic, span)?;
            Intrinsic::NatType
        }
        "NatEql" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::NatEql(term(operand0, reader)?, term(operand1, reader)?)
        }
        "NatNeq" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::NatNeq(term(operand0, reader)?, term(operand1, reader)?)
        }
        "NatAdd" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::NatAdd(term(operand0, reader)?, term(operand1, reader)?)
        }
        "NatSub" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::NatSub(term(operand0, reader)?, term(operand1, reader)?)
        }
        "NatMul" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::NatMul(term(operand0, reader)?, term(operand1, reader)?)
        }
        "NatLt" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::NatLt(term(operand0, reader)?, term(operand1, reader)?)
        }
        "NatDiv" => {
            let [dividend, divisor, non_zero] = operands(intrinsic, span)?;
            Intrinsic::NatDiv {
                dividend: term(dividend, reader)?,
                divisor: term(divisor, reader)?,
                non_zero: term(non_zero, reader)?,
            }
        }
        "NatRem" => {
            let [dividend, divisor, non_zero] = operands(intrinsic, span)?;
            Intrinsic::NatRem {
                dividend: term(dividend, reader)?,
                divisor: term(divisor, reader)?,
                non_zero: term(non_zero, reader)?,
            }
        }
        "NatLe" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::NatLe(term(operand0, reader)?, term(operand1, reader)?)
        }
        "NatAnd" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::NatAnd(term(operand0, reader)?, term(operand1, reader)?)
        }
        "NatOr" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::NatOr(term(operand0, reader)?, term(operand1, reader)?)
        }
        "NatXor" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::NatXor(term(operand0, reader)?, term(operand1, reader)?)
        }
        "NatMin" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::NatMin(term(operand0, reader)?, term(operand1, reader)?)
        }
        "NatMax" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::NatMax(term(operand0, reader)?, term(operand1, reader)?)
        }
        "NatPow" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::NatPow(term(operand0, reader)?, term(operand1, reader)?)
        }
        "NatShl" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::NatShl(term(operand0, reader)?, term(operand1, reader)?)
        }
        "NatShr" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::NatShr(term(operand0, reader)?, term(operand1, reader)?)
        }
        "ByteType" => {
            operands::<0>(intrinsic, span)?;
            Intrinsic::ByteType
        }
        "ByteToNat" => {
            let [operand0] = operands(intrinsic, span)?;
            Intrinsic::ByteToNat(term(operand0, reader)?)
        }
        "NatToByte" => {
            let [nat, below] = operands(intrinsic, span)?;
            Intrinsic::NatToByte {
                nat: term(nat, reader)?,
                below: term(below, reader)?,
            }
        }
        "IntType" => {
            operands::<0>(intrinsic, span)?;
            Intrinsic::IntType
        }
        "IntEql" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::IntEql(term(operand0, reader)?, term(operand1, reader)?)
        }
        "IntNeq" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::IntNeq(term(operand0, reader)?, term(operand1, reader)?)
        }
        "IntAdd" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::IntAdd(term(operand0, reader)?, term(operand1, reader)?)
        }
        "IntSub" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::IntSub(term(operand0, reader)?, term(operand1, reader)?)
        }
        "IntMul" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::IntMul(term(operand0, reader)?, term(operand1, reader)?)
        }
        "IntDiv" => {
            let [dividend, divisor, non_zero] = operands(intrinsic, span)?;
            Intrinsic::IntDiv {
                dividend: term(dividend, reader)?,
                divisor: term(divisor, reader)?,
                non_zero: term(non_zero, reader)?,
            }
        }
        "IntRem" => {
            let [dividend, divisor, non_zero] = operands(intrinsic, span)?;
            Intrinsic::IntRem {
                dividend: term(dividend, reader)?,
                divisor: term(divisor, reader)?,
                non_zero: term(non_zero, reader)?,
            }
        }
        "IntLt" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::IntLt(term(operand0, reader)?, term(operand1, reader)?)
        }
        "IntLe" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::IntLe(term(operand0, reader)?, term(operand1, reader)?)
        }
        "IntAnd" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::IntAnd(term(operand0, reader)?, term(operand1, reader)?)
        }
        "IntOr" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::IntOr(term(operand0, reader)?, term(operand1, reader)?)
        }
        "IntXor" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::IntXor(term(operand0, reader)?, term(operand1, reader)?)
        }
        "IntMin" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::IntMin(term(operand0, reader)?, term(operand1, reader)?)
        }
        "IntMax" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::IntMax(term(operand0, reader)?, term(operand1, reader)?)
        }
        "IntAbs" => {
            let [operand0] = operands(intrinsic, span)?;
            Intrinsic::IntAbs(term(operand0, reader)?)
        }
        "IntSign" => {
            let [operand0] = operands(intrinsic, span)?;
            Intrinsic::IntSign(term(operand0, reader)?)
        }
        "IntShl" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::IntShl(term(operand0, reader)?, term(operand1, reader)?)
        }
        "IntShr" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::IntShr(term(operand0, reader)?, term(operand1, reader)?)
        }
        "FltType" => {
            operands::<0>(intrinsic, span)?;
            Intrinsic::FltType
        }
        "FltAdd" => {
            let [operand0, operand1, operand2] = operands(intrinsic, span)?;
            Intrinsic::FltAdd(
                rounding(operand0)?,
                term(operand1, reader)?,
                term(operand2, reader)?,
            )
        }
        "FltSub" => {
            let [operand0, operand1, operand2] = operands(intrinsic, span)?;
            Intrinsic::FltSub(
                rounding(operand0)?,
                term(operand1, reader)?,
                term(operand2, reader)?,
            )
        }
        "FltMul" => {
            let [operand0, operand1, operand2] = operands(intrinsic, span)?;
            Intrinsic::FltMul(
                rounding(operand0)?,
                term(operand1, reader)?,
                term(operand2, reader)?,
            )
        }
        "FltDiv" => {
            let [operand0, operand1, operand2] = operands(intrinsic, span)?;
            Intrinsic::FltDiv(
                rounding(operand0)?,
                term(operand1, reader)?,
                term(operand2, reader)?,
            )
        }
        "FltFma" => {
            let [operand0, operand1, operand2, operand3] = operands(intrinsic, span)?;
            Intrinsic::FltFma(
                rounding(operand0)?,
                term(operand1, reader)?,
                term(operand2, reader)?,
                term(operand3, reader)?,
            )
        }
        "FltRem" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::FltRem(term(operand0, reader)?, term(operand1, reader)?)
        }
        "FltEql" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::FltEql(term(operand0, reader)?, term(operand1, reader)?)
        }
        "FltNeq" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::FltNeq(term(operand0, reader)?, term(operand1, reader)?)
        }
        "FltLt" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::FltLt(term(operand0, reader)?, term(operand1, reader)?)
        }
        "FltLe" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::FltLe(term(operand0, reader)?, term(operand1, reader)?)
        }
        "FltMin" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::FltMin(term(operand0, reader)?, term(operand1, reader)?)
        }
        "FltMax" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::FltMax(term(operand0, reader)?, term(operand1, reader)?)
        }
        "FltNeg" => {
            let [operand0] = operands(intrinsic, span)?;
            Intrinsic::FltNeg(term(operand0, reader)?)
        }
        "FltAbs" => {
            let [operand0] = operands(intrinsic, span)?;
            Intrinsic::FltAbs(term(operand0, reader)?)
        }
        "FltSqrt" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::FltSqrt(rounding(operand0)?, term(operand1, reader)?)
        }
        "FltRoundIntegral" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::FltRoundIntegral(rounding(operand0)?, term(operand1, reader)?)
        }
        "FltCopysign" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::FltCopysign(term(operand0, reader)?, term(operand1, reader)?)
        }
        "NatToInt" => {
            let [operand0] = operands(intrinsic, span)?;
            Intrinsic::NatToInt(term(operand0, reader)?)
        }
        "NatToFlt" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::NatToFlt(rounding(operand0)?, term(operand1, reader)?)
        }
        "IntToNat" => {
            let [int, non_neg] = operands(intrinsic, span)?;
            Intrinsic::IntToNat {
                int: term(int, reader)?,
                non_neg: term(non_neg, reader)?,
            }
        }
        "IntToFlt" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::IntToFlt(rounding(operand0)?, term(operand1, reader)?)
        }
        "FltToNat" => {
            let [flt, non_neg] = operands(intrinsic, span)?;
            Intrinsic::FltToNat {
                flt: term(flt, reader)?,
                non_neg: term(non_neg, reader)?,
            }
        }
        "FltToLeBytes" => {
            let [operand0] = operands(intrinsic, span)?;
            Intrinsic::FltToLeBytes(term(operand0, reader)?)
        }
        "FltOfLeBytes" => {
            let [bytes, eight_bytes] = operands(intrinsic, span)?;
            Intrinsic::FltOfLeBytes {
                bytes: term(bytes, reader)?,
                eight_bytes: term(eight_bytes, reader)?,
            }
        }
        "FltToInt" => {
            let [flt, finite] = operands(intrinsic, span)?;
            Intrinsic::FltToInt {
                flt: term(flt, reader)?,
                finite: term(finite, reader)?,
            }
        }
        "FltMantissa" => {
            let [flt, finite] = operands(intrinsic, span)?;
            Intrinsic::FltMantissa {
                flt: term(flt, reader)?,
                finite: term(finite, reader)?,
            }
        }
        "FltExponent" => {
            let [flt, finite] = operands(intrinsic, span)?;
            Intrinsic::FltExponent {
                flt: term(flt, reader)?,
                finite: term(finite, reader)?,
            }
        }
        "BytesType" => {
            operands::<0>(intrinsic, span)?;
            Intrinsic::BytesType
        }
        "BytesLen" => {
            let [operand0] = operands(intrinsic, span)?;
            Intrinsic::BytesLen(term(operand0, reader)?)
        }
        "BytesEql" => {
            let [operand0, operand1] = operands(intrinsic, span)?;
            Intrinsic::BytesEql(term(operand0, reader)?, term(operand1, reader)?)
        }
        "BytesGet" => {
            let [bytes, index, in_range] = operands(intrinsic, span)?;
            Intrinsic::BytesGet {
                bytes: term(bytes, reader)?,
                index: term(index, reader)?,
                in_range: term(in_range, reader)?,
            }
        }
        "BytesSlice" => {
            let [bytes, start, length, within] = operands(intrinsic, span)?;
            Intrinsic::BytesSlice {
                bytes: term(bytes, reader)?,
                start: term(start, reader)?,
                length: term(length, reader)?,
                within: term(within, reader)?,
            }
        }
        "BytesAppend" => {
            let [bytes, element] = operands(intrinsic, span)?;
            Intrinsic::BytesAppend {
                bytes: term(bytes, reader)?,
                element: term(element, reader)?,
            }
        }
        "BytesConcat" => {
            let [operands] = operands(intrinsic, span)?;
            Intrinsic::BytesConcat {
                operands: terms(operands, reader)?,
            }
        }
        "BytesReplicate" => {
            let [count, atom] = operands(intrinsic, span)?;
            Intrinsic::BytesReplicate {
                count: term(count, reader)?,
                atom: term(atom, reader)?,
            }
        }
        "BytesAnd" => {
            let [left, right, same_length] = operands(intrinsic, span)?;
            Intrinsic::BytesAnd {
                left: term(left, reader)?,
                right: term(right, reader)?,
                same_length: term(same_length, reader)?,
            }
        }
        "BytesOr" => {
            let [left, right, same_length] = operands(intrinsic, span)?;
            Intrinsic::BytesOr {
                left: term(left, reader)?,
                right: term(right, reader)?,
                same_length: term(same_length, reader)?,
            }
        }
        "BytesXor" => {
            let [left, right, same_length] = operands(intrinsic, span)?;
            Intrinsic::BytesXor {
                left: term(left, reader)?,
                right: term(right, reader)?,
                same_length: term(same_length, reader)?,
            }
        }
        "ListType" => {
            let [operand0] = operands(intrinsic, span)?;
            Intrinsic::ListType(term(operand0, reader)?)
        }
        "List" => {
            let [element, items] = operands(intrinsic, span)?;
            Intrinsic::List {
                element: term(element, reader)?,
                items: terms(items, reader)?,
            }
        }
        "ListLen" => {
            let [element, list] = operands(intrinsic, span)?;
            Intrinsic::ListLen {
                element: term(element, reader)?,
                list: term(list, reader)?,
            }
        }
        "ListGet" => {
            let [element, list, index, in_range] = operands(intrinsic, span)?;
            Intrinsic::ListGet {
                element: term(element, reader)?,
                list: term(list, reader)?,
                index: term(index, reader)?,
                in_range: term(in_range, reader)?,
            }
        }
        "ListSlice" => {
            let [element, list, start, length, within] = operands(intrinsic, span)?;
            Intrinsic::ListSlice {
                element: term(element, reader)?,
                list: term(list, reader)?,
                start: term(start, reader)?,
                length: term(length, reader)?,
                within: term(within, reader)?,
            }
        }
        "ListAppend" => {
            let [element, list, item] = operands(intrinsic, span)?;
            Intrinsic::ListAppend {
                element: term(element, reader)?,
                list: term(list, reader)?,
                item: term(item, reader)?,
            }
        }
        "ListConcat" => {
            let [element, operands] = operands(intrinsic, span)?;
            Intrinsic::ListConcat {
                element: term(element, reader)?,
                operands: terms(operands, reader)?,
            }
        }
        "ListMap" => {
            let [from, to, list, function] = operands(intrinsic, span)?;
            Intrinsic::ListMap {
                from: term(from, reader)?,
                to: term(to, reader)?,
                list: term(list, reader)?,
                function: term(function, reader)?,
            }
        }
        "ListFold" => {
            let [element, result, list, init, function] = operands(intrinsic, span)?;
            Intrinsic::ListFold {
                element: term(element, reader)?,
                result: term(result, reader)?,
                list: term(list, reader)?,
                init: term(init, reader)?,
                function: term(function, reader)?,
            }
        }
        "HandleType" => {
            operands::<0>(intrinsic, span)?;
            Intrinsic::HandleType
        }
        "CellType" => {
            let [operand0] = operands(intrinsic, span)?;
            Intrinsic::CellType(term(operand0, reader)?)
        }
        "Cell" => {
            let [element] = operands(intrinsic, span)?;
            Intrinsic::Cell {
                element: term(element, reader)?,
            }
        }
        "CellFill" => {
            let [element, cell, value] = operands(intrinsic, span)?;
            Intrinsic::CellFill {
                element: term(element, reader)?,
                cell: term(cell, reader)?,
                value: term(value, reader)?,
            }
        }
        "CellPoll" => {
            let [element, cell, universes] = operands(intrinsic, span)?;
            Intrinsic::CellPoll {
                element: term(element, reader)?,
                cell: term(cell, reader)?,
                universes: universes_read(universes)?,
            }
        }
        "ChannelType" => {
            let [operand0] = operands(intrinsic, span)?;
            Intrinsic::ChannelType(term(operand0, reader)?)
        }
        "Channel" => {
            let [element, capacity, positive] = operands(intrinsic, span)?;
            Intrinsic::Channel {
                element: term(element, reader)?,
                capacity: term(capacity, reader)?,
                positive: term(positive, reader)?,
            }
        }
        "ChannelPush" => {
            let [element, channel, value] = operands(intrinsic, span)?;
            Intrinsic::ChannelPush {
                element: term(element, reader)?,
                channel: term(channel, reader)?,
                value: term(value, reader)?,
            }
        }
        "ChannelTake" => {
            let [element, channel, universes] = operands(intrinsic, span)?;
            Intrinsic::ChannelTake {
                element: term(element, reader)?,
                channel: term(channel, reader)?,
                universes: universes_read(universes)?,
            }
        }
        "ChannelClose" => {
            let [element, channel] = operands(intrinsic, span)?;
            Intrinsic::ChannelClose {
                element: term(element, reader)?,
                channel: term(channel, reader)?,
            }
        }
        "ChannelClosed" => {
            let [element, channel] = operands(intrinsic, span)?;
            Intrinsic::ChannelClosed {
                element: term(element, reader)?,
                channel: term(channel, reader)?,
            }
        }
        "ChannelCount" => {
            let [element, channel] = operands(intrinsic, span)?;
            Intrinsic::ChannelCount {
                element: term(element, reader)?,
                channel: term(channel, reader)?,
            }
        }
        "ChannelCapacity" => {
            let [element, channel] = operands(intrinsic, span)?;
            Intrinsic::ChannelCapacity {
                element: term(element, reader)?,
                channel: term(channel, reader)?,
            }
        }
        "IoType" => {
            let [operand0] = operands(intrinsic, span)?;
            Intrinsic::IoType(term(operand0, reader)?)
        }
        "IoPure" => {
            let [result, value] = operands(intrinsic, span)?;
            Intrinsic::IoPure {
                result: term(result, reader)?,
                value: term(value, reader)?,
            }
        }
        "IoBind" => {
            let [from, to, action, continuation] = operands(intrinsic, span)?;
            Intrinsic::IoBind {
                from: term(from, reader)?,
                to: term(to, reader)?,
                action: term(action, reader)?,
                continuation: term(continuation, reader)?,
            }
        }
        "IoSuspend" => {
            let [result, on, filled, cancel, rest] = operands(intrinsic, span)?;
            Intrinsic::IoSuspend {
                result: term(result, reader)?,
                on: term(on, reader)?,
                filled: term(filled, reader)?,
                cancel: term(cancel, reader)?,
                rest: term(rest, reader)?,
            }
        }
        "IoAdvance" => {
            let [from, to, action, done, suspended] = operands(intrinsic, span)?;
            Intrinsic::IoAdvance {
                from: term(from, reader)?,
                to: term(to, reader)?,
                action: term(action, reader)?,
                done: term(done, reader)?,
                suspended: term(suspended, reader)?,
            }
        }

        variant => {
            return Err(ReadError::at(
                Refusal::UnknownIntrinsic(variant.to_string()),
                intrinsic.variant.span().or(span),
            ));
        }
    };

    Ok(Term::intrinsic(read))
}

/// The operands of `intrinsic`, as many as its variant takes: none written where it takes none, and a list of them otherwise.
fn operands<'i, const N: usize>(
    intrinsic: &'i CoreIntrinsic,
    span: Option<&Span>,
) -> Result<[&'i CoreValue; N], ReadError> {
    let refused = || ReadError::at(Refusal::Malformed("the operands its variant takes"), span);
    let written = match (&intrinsic.operands, N) {
        (None, 0) => Vec::new(),
        (Some(values), 1..) => values.iter().collect::<Vec<_>>(),
        _ => return Err(refused()),
    };

    written.try_into().map_err(|_| refused())
}

/// An operand that is a term.
fn term(value: &CoreValue, reader: &mut Reader<'_>) -> Result<Term, ReadError> {
    core_read(&value_term(value)?, reader)
}

/// An operand that is a list of terms, `[a, b]`.
fn terms(value: &CoreValue, reader: &mut Reader<'_>) -> Result<Vec<Term>, ReadError> {
    value_unmarked(value)?
        .into_iter()
        .map(|entry| term(entry, reader))
        .collect()
}

/// An operand that is a rounding, by its variant's name.
fn rounding(value: &CoreValue) -> Result<Rounding, ReadError> {
    let word = value_word(value)?;
    Rounding::ALL
        .into_iter()
        .find(|rounding| format!("{rounding:?}") == word)
        .ok_or_else(|| malformed("a rounding, as Core names it", value))
}

/// The unsigned numeral an operand is.
fn unsigned(value: &CoreValue) -> Option<curios_num::Natural> {
    match value_term(value).ok()?.as_subterm() {
        crate::Subterm::NumLit(NumLit {
            magnitude,
            sign: Sign::Unmarked,
            ..
        }) => Some(magnitude.clone()),
        _ => None,
    }
}

/// A `Nat` over a term, `Succ(k, n)`: `n` offset by `k`.
fn nat_read(value: &CoreValue, reader: &mut Reader<'_>) -> Result<Nat, ReadError> {
    let refused = || malformed("a `Nat` over a term, `Succ(k, n)`", value);
    let operand = value_term(value)?;
    let crate::Subterm::Apply(crate::Apply { head, arguments }) = operand.as_subterm() else {
        return Err(refused());
    };
    if term_word(head) != Some("Succ") {
        return Err(refused());
    }
    let [offset, tail] = arguments.as_slice() else {
        return Err(refused());
    };
    let offset = unsigned(&CoreValue::Term(offset.term.clone())).ok_or_else(refused)?;

    Ok(Nat::Succ(offset, core_read(&tail.term, reader)?))
}

/// An `Int`'s value, a numeral under its sign.
fn int_read(value: &CoreValue) -> Result<Integer, ReadError> {
    match value_term(value)?.as_subterm() {
        crate::Subterm::NumLit(NumLit {
            magnitude, sign, ..
        }) => {
            let integer = Integer::from(magnitude.clone());
            Ok(match sign {
                Sign::Negative => -integer,
                Sign::Unmarked | Sign::Positive => integer,
            })
        }
        _ => Err(malformed("an integer", value)),
    }
}

/// A `Flt`'s value: the literal that reads back as it, or its bits as a hexadecimal numeral.
fn flt_read(value: &CoreValue) -> Result<Floating, ReadError> {
    match value_term(value)?.as_subterm() {
        crate::Subterm::Literal(Literal::Flt(flt)) => Ok(*flt),
        crate::Subterm::NumLit(NumLit {
            magnitude,
            sign: Sign::Unmarked,
            ..
        }) => u64::try_from(magnitude)
            .map(Floating::from_bits)
            .map_err(|_| malformed("a float's bits", value)),
        _ => Err(malformed("a float", value)),
    }
}

/// A packed literal, `x[0x01, 0x02]`: its bytes, each a numeral.
pub fn packed_read(
    segments: &[BytesSegment<CoreText>],
    span: Option<&Span>,
) -> Result<Term, ReadError> {
    let bytes = segments
        .iter()
        .map(|segment| match segment {
            BytesSegment::Atom(atom) => unsigned(&CoreValue::Term(atom.clone()))
                .and_then(|byte| u8::try_from(&byte).ok())
                .ok_or_else(|| malformed("a byte", &CoreValue::Term(atom.clone()))),
            BytesSegment::Spread(_) => Err(ReadError::at(Refusal::ProgramForm("a spread"), span)),
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Term::intrinsic(Intrinsic::Bytes(Binary::from_bytes(bytes))))
}
