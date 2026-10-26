//! The CPS operation each erased operation names.
//!
//! A total table, one arm per [`Operation`], plus the three small families the sequence operations fan out into by grain. Nothing here reduces or decides: the erased roster and the CPS roster are two spellings of the same set, and this is the translation between them, kept apart from the lowering so that adding an operation is an edit to a table rather than to a walk.

use super::{CellOperation, Operation, SequenceGrain, SequenceOp};

/// The CPS intrinsic of a scalar [`Operation`]. `Bool` operations run on the `0`/`1` `Nat` carrier (`BoolNeq` is xor on a single bit) and `Byte` comparisons on the `Nat` carrier. The `Byte` conversions are handled before this table.
pub(super) fn operation_intrinsic(operation: Operation) -> curios_cps::Intrinsic {
    match operation {
        Operation::BoolAnd => curios_cps::Intrinsic::NatAnd,
        Operation::BoolOr => curios_cps::Intrinsic::NatOr,
        Operation::BoolXor => curios_cps::Intrinsic::NatXor,
        Operation::BoolEql => curios_cps::Intrinsic::NatEql,
        Operation::BoolNeq => curios_cps::Intrinsic::NatXor,
        Operation::NatEql => curios_cps::Intrinsic::NatEql,
        Operation::NatNeq => curios_cps::Intrinsic::NatNeq,
        Operation::NatAdd => curios_cps::Intrinsic::NatAdd,
        Operation::NatSub => curios_cps::Intrinsic::NatSub,
        Operation::NatMul => curios_cps::Intrinsic::NatMul,
        Operation::NatLt => curios_cps::Intrinsic::NatLt,
        Operation::NatDiv => curios_cps::Intrinsic::NatDiv,
        Operation::NatRem => curios_cps::Intrinsic::NatRem,
        Operation::NatLe => curios_cps::Intrinsic::NatLe,
        Operation::NatAnd => curios_cps::Intrinsic::NatAnd,
        Operation::NatOr => curios_cps::Intrinsic::NatOr,
        Operation::NatXor => curios_cps::Intrinsic::NatXor,
        Operation::NatMin => curios_cps::Intrinsic::NatMin,
        Operation::NatMax => curios_cps::Intrinsic::NatMax,
        Operation::NatShl => curios_cps::Intrinsic::NatShl,
        Operation::NatPow => curios_cps::Intrinsic::NatPow,
        Operation::NatShr => curios_cps::Intrinsic::NatShr,
        Operation::IntEql => curios_cps::Intrinsic::IntEql,
        Operation::IntNeq => curios_cps::Intrinsic::IntNeq,
        Operation::IntAdd => curios_cps::Intrinsic::IntAdd,
        Operation::IntSub => curios_cps::Intrinsic::IntSub,
        Operation::IntMul => curios_cps::Intrinsic::IntMul,
        Operation::IntDiv => curios_cps::Intrinsic::IntDiv,
        Operation::IntRem => curios_cps::Intrinsic::IntRem,
        Operation::IntLt => curios_cps::Intrinsic::IntLt,
        Operation::IntLe => curios_cps::Intrinsic::IntLe,
        Operation::IntAnd => curios_cps::Intrinsic::IntAnd,
        Operation::IntOr => curios_cps::Intrinsic::IntOr,
        Operation::IntXor => curios_cps::Intrinsic::IntXor,
        Operation::IntMin => curios_cps::Intrinsic::IntMin,
        Operation::IntMax => curios_cps::Intrinsic::IntMax,
        Operation::IntShl => curios_cps::Intrinsic::IntShl,
        Operation::IntShr => curios_cps::Intrinsic::IntShr,
        Operation::FltAdd(rounding) => curios_cps::Intrinsic::FltAdd(rounding),
        Operation::FltSub(rounding) => curios_cps::Intrinsic::FltSub(rounding),
        Operation::FltMul(rounding) => curios_cps::Intrinsic::FltMul(rounding),
        Operation::FltDiv(rounding) => curios_cps::Intrinsic::FltDiv(rounding),
        Operation::FltFma(rounding) => curios_cps::Intrinsic::FltFma(rounding),
        Operation::FltRem => curios_cps::Intrinsic::FltRem,
        Operation::FltEql => curios_cps::Intrinsic::FltEql,
        Operation::FltNeq => curios_cps::Intrinsic::FltNeq,
        Operation::FltLt => curios_cps::Intrinsic::FltLt,
        Operation::FltLe => curios_cps::Intrinsic::FltLe,
        Operation::FltMin => curios_cps::Intrinsic::FltMin,
        Operation::FltMax => curios_cps::Intrinsic::FltMax,
        Operation::FltCopysign => curios_cps::Intrinsic::FltCopysign,
        Operation::FltNeg => curios_cps::Intrinsic::FltNeg,
        Operation::FltAbs => curios_cps::Intrinsic::FltAbs,
        Operation::FltSqrt(rounding) => curios_cps::Intrinsic::FltSqrt(rounding),
        Operation::FltRoundIntegral(rounding) => curios_cps::Intrinsic::FltRoundIntegral(rounding),
        Operation::NatToInt => curios_cps::Intrinsic::NatToInt,
        Operation::IntAbs => curios_cps::Intrinsic::IntAbs,
        Operation::IntSign => curios_cps::Intrinsic::IntSign,
        Operation::NatToFlt(rounding) => curios_cps::Intrinsic::NatToFlt(rounding),
        Operation::IntToNat => curios_cps::Intrinsic::IntToNat,
        Operation::IntToFlt(rounding) => curios_cps::Intrinsic::IntToFlt(rounding),
        Operation::FltToNat => curios_cps::Intrinsic::FltToNat,
        Operation::FltToInt => curios_cps::Intrinsic::FltToInt,
        Operation::FltMantissa => curios_cps::Intrinsic::FltMantissa,
        Operation::FltExponent => curios_cps::Intrinsic::FltExponent,
        Operation::FltToLeBytes => curios_cps::Intrinsic::FltToLeBytes,
        Operation::FltOfLeBytes => curios_cps::Intrinsic::FltOfLeBytes,
        Operation::ByteToNat | Operation::NatToByte => {
            unreachable!("Byte conversions are lowered before the intrinsic table")
        }
    }
}

/// The CPS intrinsic of a [`SequenceOp`], threading the operand count into the variadic concatenations. `ListBuild` is a list value, never an intrinsic.
pub(super) fn sequence_intrinsic(operation: SequenceOp, arity: usize) -> curios_cps::Intrinsic {
    match operation {
        SequenceOp::BytesLen => curios_cps::Intrinsic::BytesLen,
        SequenceOp::BytesEql => curios_cps::Intrinsic::BytesEql,
        SequenceOp::BytesGet => curios_cps::Intrinsic::BytesGet,
        SequenceOp::BytesSlice => curios_cps::Intrinsic::BytesSlice,
        SequenceOp::BytesAppend => curios_cps::Intrinsic::BytesAppend,
        SequenceOp::BytesConcat => curios_cps::Intrinsic::BytesConcat(arity),
        SequenceOp::BytesReplicate => curios_cps::Intrinsic::BytesReplicate,
        SequenceOp::BytesAnd => curios_cps::Intrinsic::BytesAnd,
        SequenceOp::BytesOr => curios_cps::Intrinsic::BytesOr,
        SequenceOp::BytesXor => curios_cps::Intrinsic::BytesXor,
        SequenceOp::ListLen => curios_cps::Intrinsic::ListLen,
        SequenceOp::ListGet => curios_cps::Intrinsic::ListGet,
        SequenceOp::ListSlice => curios_cps::Intrinsic::ListSlice,
        SequenceOp::ListAppend => curios_cps::Intrinsic::ListAppend,
        SequenceOp::ListConcat => curios_cps::Intrinsic::ListConcat(arity),
        SequenceOp::ListBuild => unreachable!("ListBuild is lowered as a list value"),
    }
}

pub(super) fn cell_op(operation: CellOperation) -> curios_cps::CellOp {
    match operation {
        CellOperation::New => curios_cps::CellOp::Reserve,
        CellOperation::Poll { .. } => curios_cps::CellOp::Poll,
        CellOperation::Fill => curios_cps::CellOp::Fill,
    }
}

pub(super) fn sequence_len_op(grain: SequenceGrain) -> curios_cps::Intrinsic {
    match grain {
        SequenceGrain::List => curios_cps::Intrinsic::ListLen,
        SequenceGrain::Bytes => curios_cps::Intrinsic::BytesLen,
    }
}

pub(super) fn sequence_get_op(grain: SequenceGrain) -> curios_cps::Intrinsic {
    match grain {
        SequenceGrain::List => curios_cps::Intrinsic::ListGet,
        SequenceGrain::Bytes => curios_cps::Intrinsic::BytesGet,
    }
}

pub(super) fn sequence_rest_op(grain: SequenceGrain) -> curios_cps::Intrinsic {
    match grain {
        SequenceGrain::List => curios_cps::Intrinsic::ListRest,
        SequenceGrain::Bytes => curios_cps::Intrinsic::BytesRest,
    }
}
