//! The Cont operation each erased operation names.
//!
//! A total table, one arm per [`Operation`], plus the three small families the sequence operations fan out into by grain. Nothing here reduces or decides: the erased roster and the Cont roster are two spellings of the same set, and this is the translation between them, kept apart from the lowering so that adding an operation is an edit to a table rather than to a walk.

use super::{CellOperation, Operation, SequenceGrain, SequenceOp};

/// The Cont intrinsic of a scalar [`Operation`]. `Bool` operations run on the `0`/`1` `Nat` carrier (`BoolNeq` is xor on a single bit) and `Byte` comparisons on the `Nat` carrier. The `Byte` conversions are handled before this table.
pub(super) fn operation_intrinsic(operation: Operation) -> curios_cont::Intrinsic {
    match operation {
        Operation::BoolAnd => curios_cont::Intrinsic::NatAnd,
        Operation::BoolOr => curios_cont::Intrinsic::NatOr,
        Operation::BoolXor => curios_cont::Intrinsic::NatXor,
        Operation::BoolEql => curios_cont::Intrinsic::NatEql,
        Operation::BoolNeq => curios_cont::Intrinsic::NatXor,
        Operation::NatEql => curios_cont::Intrinsic::NatEql,
        Operation::NatNeq => curios_cont::Intrinsic::NatNeq,
        Operation::NatAdd => curios_cont::Intrinsic::NatAdd,
        Operation::NatSub => curios_cont::Intrinsic::NatSub,
        Operation::NatMul => curios_cont::Intrinsic::NatMul,
        Operation::NatLt => curios_cont::Intrinsic::NatLt,
        Operation::NatDiv => curios_cont::Intrinsic::NatDiv,
        Operation::NatRem => curios_cont::Intrinsic::NatRem,
        Operation::NatLe => curios_cont::Intrinsic::NatLe,
        Operation::NatAnd => curios_cont::Intrinsic::NatAnd,
        Operation::NatOr => curios_cont::Intrinsic::NatOr,
        Operation::NatXor => curios_cont::Intrinsic::NatXor,
        Operation::NatShl => curios_cont::Intrinsic::NatShl,
        Operation::NatShr => curios_cont::Intrinsic::NatShr,
        Operation::IntEql => curios_cont::Intrinsic::IntEql,
        Operation::IntNeq => curios_cont::Intrinsic::IntNeq,
        Operation::IntAdd => curios_cont::Intrinsic::IntAdd,
        Operation::IntSub => curios_cont::Intrinsic::IntSub,
        Operation::IntMul => curios_cont::Intrinsic::IntMul,
        Operation::IntDiv => curios_cont::Intrinsic::IntDiv,
        Operation::IntRem => curios_cont::Intrinsic::IntRem,
        Operation::IntLt => curios_cont::Intrinsic::IntLt,
        Operation::IntLe => curios_cont::Intrinsic::IntLe,
        Operation::IntAnd => curios_cont::Intrinsic::IntAnd,
        Operation::IntOr => curios_cont::Intrinsic::IntOr,
        Operation::IntXor => curios_cont::Intrinsic::IntXor,
        Operation::IntShl => curios_cont::Intrinsic::IntShl,
        Operation::IntShr => curios_cont::Intrinsic::IntShr,
        Operation::FltAdd(rounding) => curios_cont::Intrinsic::FltAdd(rounding),
        Operation::FltSub(rounding) => curios_cont::Intrinsic::FltSub(rounding),
        Operation::FltMul(rounding) => curios_cont::Intrinsic::FltMul(rounding),
        Operation::FltDiv(rounding) => curios_cont::Intrinsic::FltDiv(rounding),
        Operation::FltFma(rounding) => curios_cont::Intrinsic::FltFma(rounding),
        Operation::FltRem => curios_cont::Intrinsic::FltRem,
        Operation::FltEql => curios_cont::Intrinsic::FltEql,
        Operation::FltNeq => curios_cont::Intrinsic::FltNeq,
        Operation::FltLt => curios_cont::Intrinsic::FltLt,
        Operation::FltLe => curios_cont::Intrinsic::FltLe,
        Operation::FltMin => curios_cont::Intrinsic::FltMin,
        Operation::FltMax => curios_cont::Intrinsic::FltMax,
        Operation::FltCopysign => curios_cont::Intrinsic::FltCopysign,
        Operation::FltNeg => curios_cont::Intrinsic::FltNeg,
        Operation::FltAbs => curios_cont::Intrinsic::FltAbs,
        Operation::FltSqrt(rounding) => curios_cont::Intrinsic::FltSqrt(rounding),
        Operation::FltRoundIntegral(rounding) => curios_cont::Intrinsic::FltRoundIntegral(rounding),
        Operation::NatToInt => curios_cont::Intrinsic::NatToInt,
        Operation::NatToFlt(rounding) => curios_cont::Intrinsic::NatToFlt(rounding),
        Operation::IntToNat => curios_cont::Intrinsic::IntToNat,
        Operation::IntToFlt(rounding) => curios_cont::Intrinsic::IntToFlt(rounding),
        Operation::FltToNat => curios_cont::Intrinsic::FltToNat,
        Operation::FltToInt => curios_cont::Intrinsic::FltToInt,
        Operation::FltMantissa => curios_cont::Intrinsic::FltMantissa,
        Operation::FltExponent => curios_cont::Intrinsic::FltExponent,
        Operation::FltToLeBytes => curios_cont::Intrinsic::FltToLeBytes,
        Operation::FltOfLeBytes => curios_cont::Intrinsic::FltOfLeBytes,
        Operation::ByteToNat | Operation::NatToByte => {
            unreachable!("Byte conversions are lowered before the intrinsic table")
        }
    }
}

/// The Cont intrinsic of a [`SequenceOp`], threading the operand count into the variadic concatenations. `ListBuild` is a list value, never an intrinsic.
pub(super) fn sequence_intrinsic(operation: SequenceOp, arity: usize) -> curios_cont::Intrinsic {
    match operation {
        SequenceOp::BinLen(grain) => curios_cont::Intrinsic::BinLen(grain),
        SequenceOp::BinEql(grain) => curios_cont::Intrinsic::BinEql(grain),
        SequenceOp::BinGet(grain) => curios_cont::Intrinsic::BinGet(grain),
        SequenceOp::BinSlice(grain) => curios_cont::Intrinsic::BinSlice(grain),
        SequenceOp::BinAppend(grain) => curios_cont::Intrinsic::BinAppend(grain),
        SequenceOp::BinConcat(grain) => curios_cont::Intrinsic::BinConcat(grain, arity),
        SequenceOp::BinReplicate(grain) => curios_cont::Intrinsic::BinReplicate(grain),
        SequenceOp::BinReinterp(grain) => curios_cont::Intrinsic::BinReinterp(grain),
        SequenceOp::BinAnd(grain) => curios_cont::Intrinsic::BinAnd(grain),
        SequenceOp::BinOr(grain) => curios_cont::Intrinsic::BinOr(grain),
        SequenceOp::BinXor(grain) => curios_cont::Intrinsic::BinXor(grain),
        SequenceOp::ListLen => curios_cont::Intrinsic::ListLen,
        SequenceOp::ListGet => curios_cont::Intrinsic::ListGet,
        SequenceOp::ListSlice => curios_cont::Intrinsic::ListSlice,
        SequenceOp::ListAppend => curios_cont::Intrinsic::ListAppend,
        SequenceOp::ListConcat => curios_cont::Intrinsic::ListConcat(arity),
        SequenceOp::ListBuild => unreachable!("ListBuild is lowered as a list value"),
    }
}

pub(super) fn cell_op(operation: CellOperation) -> curios_cont::CellOp {
    match operation {
        CellOperation::New => curios_cont::CellOp::Reserve,
        CellOperation::Poll { .. } => curios_cont::CellOp::Poll,
        CellOperation::Fill => curios_cont::CellOp::Fill,
    }
}

pub(super) fn sequence_len_op(grain: SequenceGrain) -> curios_cont::Intrinsic {
    match grain {
        SequenceGrain::List => curios_cont::Intrinsic::ListLen,
        SequenceGrain::Bin(grain) => curios_cont::Intrinsic::BinLen(grain),
    }
}

pub(super) fn sequence_get_op(grain: SequenceGrain) -> curios_cont::Intrinsic {
    match grain {
        SequenceGrain::List => curios_cont::Intrinsic::ListGet,
        SequenceGrain::Bin(grain) => curios_cont::Intrinsic::BinGet(grain),
    }
}

pub(super) fn sequence_rest_op(grain: SequenceGrain) -> curios_cont::Intrinsic {
    match grain {
        SequenceGrain::List => curios_cont::Intrinsic::ListRest,
        SequenceGrain::Bin(grain) => curios_cont::Intrinsic::BinRest(grain),
    }
}
