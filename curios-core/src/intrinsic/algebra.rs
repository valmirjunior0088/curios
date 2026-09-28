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
    /// An operation over a numeric carrier, with the operands its meaning reads, in the operation's order.
    Numeric {
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
        let numeric = |carrier, operation, operands| Declaration::Numeric {
            carrier,
            operation,
            operands,
        };
        match self {
            Intrinsic::NatAdd(left, right) => numeric(
                Carrier::Natural,
                Operation::Sum,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatSub(left, right) => numeric(
                Carrier::Natural,
                Operation::Difference,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatMul(left, right) => numeric(
                Carrier::Natural,
                Operation::Product,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatDiv {
                dividend, divisor, ..
            } => numeric(
                Carrier::Natural,
                Operation::Quotient,
                Operands::Two([dividend, divisor]),
            ),
            Intrinsic::NatRem {
                dividend, divisor, ..
            } => numeric(
                Carrier::Natural,
                Operation::Remainder,
                Operands::Two([dividend, divisor]),
            ),
            Intrinsic::NatAnd(left, right) => numeric(
                Carrier::Natural,
                Operation::And,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatOr(left, right) => numeric(
                Carrier::Natural,
                Operation::Or,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatXor(left, right) => numeric(
                Carrier::Natural,
                Operation::Xor,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatShl(left, right) => numeric(
                Carrier::Natural,
                Operation::ShiftLeft,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatShr(left, right) => numeric(
                Carrier::Natural,
                Operation::ShiftRight,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatEql(left, right) => numeric(
                Carrier::Natural,
                Operation::Equal,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatNeq(left, right) => numeric(
                Carrier::Natural,
                Operation::Unequal,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatLt(left, right) => numeric(
                Carrier::Natural,
                Operation::Less,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatLe(left, right) => numeric(
                Carrier::Natural,
                Operation::AtMost,
                Operands::Two([left, right]),
            ),
            Intrinsic::ByteToNat(operand) => numeric(
                Carrier::Natural,
                Operation::FromByte,
                Operands::One([operand]),
            ),
            Intrinsic::NatToInt(operand) => numeric(
                Carrier::Integer,
                Operation::Widening,
                Operands::One([operand]),
            ),
            Intrinsic::IntAdd(left, right) => numeric(
                Carrier::Integer,
                Operation::Sum,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntSub(left, right) => numeric(
                Carrier::Integer,
                Operation::Difference,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntMul(left, right) => numeric(
                Carrier::Integer,
                Operation::Product,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntDiv {
                dividend, divisor, ..
            } => numeric(
                Carrier::Integer,
                Operation::Quotient,
                Operands::Two([dividend, divisor]),
            ),
            Intrinsic::IntRem {
                dividend, divisor, ..
            } => numeric(
                Carrier::Integer,
                Operation::Remainder,
                Operands::Two([dividend, divisor]),
            ),
            Intrinsic::IntShl(left, right) => numeric(
                Carrier::Integer,
                Operation::ShiftLeft,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntShr(left, right) => numeric(
                Carrier::Integer,
                Operation::ShiftRight,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntEql(left, right) => numeric(
                Carrier::Integer,
                Operation::Equal,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntNeq(left, right) => numeric(
                Carrier::Integer,
                Operation::Unequal,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntLt(left, right) => numeric(
                Carrier::Integer,
                Operation::Less,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntLe(left, right) => numeric(
                Carrier::Integer,
                Operation::AtMost,
                Operands::Two([left, right]),
            ),
            Intrinsic::BoolType
            | Intrinsic::Bool { .. }
            | Intrinsic::BoolAnd { .. }
            | Intrinsic::BoolOr { .. }
            | Intrinsic::BoolXor { .. }
            | Intrinsic::BoolEql { .. }
            | Intrinsic::BoolNeq { .. }
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
}
