//! What each intrinsic is algebraically: the operation `curios-algebra` gives it a meaning as, over the operands that meaning reads — or opaque.
//!
//! **The match is exhaustive and states opacity outright**, as [`Intrinsic::signature`]'s does: a new intrinsic has to say what it is to the algebra, possibly nothing, before it compiles, so an operation can neither gain a law by being forgotten nor lose one by being added. What each [`Operation`] means — which operands bound it, which it never exceeds — is `curios-algebra`'s; this is only which intrinsic is which, and where its operands are. Only the implemented reasoning is declared: an intrinsic is declared where some rule reads it at its carrier, and declaring one never enables an identity nothing implements — which is why `Int`'s bitwise operations are opaque, no identity of theirs being implemented.

use {
    super::Intrinsic,
    crate::Term,
    curios_algebra::{Carrier, Operation},
};

/// An intrinsic as the algebra reads it.
pub enum Declaration<'a> {
    /// Nothing the algebra reasons about.
    Opaque,
    /// An operation over a carrier, with the operands its meaning reads, in the operation's order.
    Operation {
        carrier: Carrier,
        operation: Operation,
        operands: Operands<'a>,
    },
}

/// The operands a declaration's meaning reads, in its operation's order: a quotient's and a remainder's are the dividend and the divisor, their proof being no operand the algebra reads.
pub enum Operands<'a> {
    One([&'a Term; 1]),
    Two([&'a Term; 2]),
}

impl<'a> Operands<'a> {
    /// The operands, in order.
    pub fn as_slice(&self) -> &[&'a Term] {
        match self {
            Operands::One(operands) => operands,
            Operands::Two(operands) => operands,
        }
    }
}

impl Intrinsic {
    /// What this intrinsic is algebraically.
    pub fn algebra(&self) -> Declaration<'_> {
        let declared = |carrier, operation, operands| Declaration::Operation {
            carrier,
            operation,
            operands,
        };
        match self {
            Intrinsic::BoolAnd(left, right) => declared(
                Carrier::Boolean,
                Operation::And,
                Operands::Two([left, right]),
            ),
            Intrinsic::BoolOr(left, right) => declared(
                Carrier::Boolean,
                Operation::Or,
                Operands::Two([left, right]),
            ),
            Intrinsic::BoolXor(left, right) => declared(
                Carrier::Boolean,
                Operation::Xor,
                Operands::Two([left, right]),
            ),
            Intrinsic::BoolEql(left, right) => declared(
                Carrier::Boolean,
                Operation::Equal,
                Operands::Two([left, right]),
            ),
            Intrinsic::BoolNeq(left, right) => declared(
                Carrier::Boolean,
                Operation::Unequal,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatAdd(left, right) => declared(
                Carrier::Natural,
                Operation::Sum,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatSub(left, right) => declared(
                Carrier::Natural,
                Operation::Difference,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatMul(left, right) => declared(
                Carrier::Natural,
                Operation::Product,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatDiv {
                dividend, divisor, ..
            } => declared(
                Carrier::Natural,
                Operation::Quotient,
                Operands::Two([dividend, divisor]),
            ),
            Intrinsic::NatRem {
                dividend, divisor, ..
            } => declared(
                Carrier::Natural,
                Operation::Remainder,
                Operands::Two([dividend, divisor]),
            ),
            Intrinsic::NatAnd(left, right) => declared(
                Carrier::Natural,
                Operation::And,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatOr(left, right) => declared(
                Carrier::Natural,
                Operation::Or,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatXor(left, right) => declared(
                Carrier::Natural,
                Operation::Xor,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatShl(left, right) => declared(
                Carrier::Natural,
                Operation::ShiftLeft,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatShr(left, right) => declared(
                Carrier::Natural,
                Operation::ShiftRight,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatEql(left, right) => declared(
                Carrier::Natural,
                Operation::Equal,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatNeq(left, right) => declared(
                Carrier::Natural,
                Operation::Unequal,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatLt(left, right) => declared(
                Carrier::Natural,
                Operation::Less,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatLe(left, right) => declared(
                Carrier::Natural,
                Operation::AtMost,
                Operands::Two([left, right]),
            ),
            Intrinsic::ByteToNat(operand) => declared(
                Carrier::Natural,
                Operation::FromByte,
                Operands::One([operand]),
            ),
            Intrinsic::NatToInt(operand) => declared(
                Carrier::Integer,
                Operation::Widening,
                Operands::One([operand]),
            ),
            Intrinsic::IntAdd(left, right) => declared(
                Carrier::Integer,
                Operation::Sum,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntSub(left, right) => declared(
                Carrier::Integer,
                Operation::Difference,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntMul(left, right) => declared(
                Carrier::Integer,
                Operation::Product,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntDiv {
                dividend, divisor, ..
            } => declared(
                Carrier::Integer,
                Operation::Quotient,
                Operands::Two([dividend, divisor]),
            ),
            Intrinsic::IntRem {
                dividend, divisor, ..
            } => declared(
                Carrier::Integer,
                Operation::Remainder,
                Operands::Two([dividend, divisor]),
            ),
            Intrinsic::IntShl(left, right) => declared(
                Carrier::Integer,
                Operation::ShiftLeft,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntShr(left, right) => declared(
                Carrier::Integer,
                Operation::ShiftRight,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntEql(left, right) => declared(
                Carrier::Integer,
                Operation::Equal,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntNeq(left, right) => declared(
                Carrier::Integer,
                Operation::Unequal,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntLt(left, right) => declared(
                Carrier::Integer,
                Operation::Less,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntLe(left, right) => declared(
                Carrier::Integer,
                Operation::AtMost,
                Operands::Two([left, right]),
            ),
            Intrinsic::BoolType
            | Intrinsic::Bool { .. }
            | Intrinsic::NatType
            | Intrinsic::Nat { .. }
            | Intrinsic::ByteType
            | Intrinsic::Byte { .. }
            | Intrinsic::NatToByte { .. }
            | Intrinsic::IntType
            | Intrinsic::IntAnd { .. }
            | Intrinsic::IntOr { .. }
            | Intrinsic::IntXor { .. }
            | Intrinsic::Int { .. }
            | Intrinsic::FltType
            | Intrinsic::Flt { .. }
            | Intrinsic::FltAdd { .. }
            | Intrinsic::FltSub { .. }
            | Intrinsic::FltMul { .. }
            | Intrinsic::FltDiv { .. }
            | Intrinsic::FltFma { .. }
            | Intrinsic::FltRem { .. }
            | Intrinsic::FltEql { .. }
            | Intrinsic::FltNeq { .. }
            | Intrinsic::FltLt { .. }
            | Intrinsic::FltLe { .. }
            | Intrinsic::FltMin { .. }
            | Intrinsic::FltMax { .. }
            | Intrinsic::FltNeg { .. }
            | Intrinsic::FltAbs { .. }
            | Intrinsic::FltSqrt { .. }
            | Intrinsic::FltRoundIntegral { .. }
            | Intrinsic::FltCopysign { .. }
            | Intrinsic::NatToFlt { .. }
            | Intrinsic::IntToNat { .. }
            | Intrinsic::IntToFlt { .. }
            | Intrinsic::FltToNat { .. }
            | Intrinsic::FltToLeBytes { .. }
            | Intrinsic::FltOfLeBytes { .. }
            | Intrinsic::FltToInt { .. }
            | Intrinsic::FltMantissa { .. }
            | Intrinsic::FltExponent { .. }
            | Intrinsic::BinType { .. }
            | Intrinsic::Bin { .. }
            | Intrinsic::BinLen { .. }
            | Intrinsic::BinEql { .. }
            | Intrinsic::BinGet { .. }
            | Intrinsic::BinSlice { .. }
            | Intrinsic::BinAppend { .. }
            | Intrinsic::BinConcat { .. }
            | Intrinsic::BinReplicate { .. }
            | Intrinsic::BinAnd { .. }
            | Intrinsic::BinOr { .. }
            | Intrinsic::BinXor { .. }
            | Intrinsic::BinReinterp { .. }
            | Intrinsic::ListType { .. }
            | Intrinsic::List { .. }
            | Intrinsic::ListLen { .. }
            | Intrinsic::ListGet { .. }
            | Intrinsic::ListSlice { .. }
            | Intrinsic::ListAppend { .. }
            | Intrinsic::ListConcat { .. }
            | Intrinsic::ListMap { .. }
            | Intrinsic::ListFold { .. }
            | Intrinsic::HandleType
            | Intrinsic::Handle { .. }
            | Intrinsic::CellType { .. }
            | Intrinsic::Cell { .. }
            | Intrinsic::CellFill { .. }
            | Intrinsic::CellPoll { .. }
            | Intrinsic::ChannelType { .. }
            | Intrinsic::Channel { .. }
            | Intrinsic::ChannelPush { .. }
            | Intrinsic::ChannelTake { .. }
            | Intrinsic::ChannelClose { .. }
            | Intrinsic::ChannelClosed { .. }
            | Intrinsic::ChannelCount { .. }
            | Intrinsic::ChannelCapacity { .. }
            | Intrinsic::IoType { .. }
            | Intrinsic::IoPure { .. }
            | Intrinsic::IoBind { .. } => Declaration::Opaque,
        }
    }

    /// The comparison of `carrier` that `operation` names, over `left` and `right` — the way back from a declaration to the intrinsic it declares, for the comparisons the algebra hands back by operation: `<`, `<=`, `==` and `!=` over `Nat` and `Int`, and `==`, `!=` and `xor` over `Bool`. `None` for any other pair.
    pub fn comparison(
        carrier: Carrier,
        operation: Operation,
        left: Term,
        right: Term,
    ) -> Option<Intrinsic> {
        Some(match (carrier, operation) {
            (Carrier::Natural, Operation::Less) => Intrinsic::NatLt(left, right),
            (Carrier::Natural, Operation::AtMost) => Intrinsic::NatLe(left, right),
            (Carrier::Natural, Operation::Equal) => Intrinsic::NatEql(left, right),
            (Carrier::Natural, Operation::Unequal) => Intrinsic::NatNeq(left, right),
            (Carrier::Integer, Operation::Less) => Intrinsic::IntLt(left, right),
            (Carrier::Integer, Operation::AtMost) => Intrinsic::IntLe(left, right),
            (Carrier::Integer, Operation::Equal) => Intrinsic::IntEql(left, right),
            (Carrier::Integer, Operation::Unequal) => Intrinsic::IntNeq(left, right),
            (Carrier::Boolean, Operation::Equal) => Intrinsic::BoolEql(left, right),
            (Carrier::Boolean, Operation::Unequal) => Intrinsic::BoolNeq(left, right),
            (Carrier::Boolean, Operation::Xor) => Intrinsic::BoolXor(left, right),
            _ => return None,
        })
    }
}
