//! The Cont operation each erased operation names.
//!
//! A total table, one arm per [`Operation`], plus the three small families the sequence operations fan out into by grain. Nothing here reduces or decides: the erased roster and the Cont roster are two spellings of the same set, and this is the translation between them, kept apart from the lowering so that adding an operation is an edit to a table rather than to a walk.

use super::{CellOperation, Operation, SequenceGrain, SequenceOp};

/// The Cont intrinsic of a scalar [`Operation`]. `Bool` operations run on the `0`/`1` `Nat` carrier (`BoolNeq` is xor on a single bit) and `Byte` comparisons on the `Nat` carrier. The `Byte` conversions are handled before this table.
pub(super) fn operation_intrinsic(operation: Operation) -> curios_cont::CpsIntrinsic {
    use Operation as O;
    match operation {
        O::BoolAnd => curios_cont::CpsIntrinsic::NatAnd,
        O::BoolOr => curios_cont::CpsIntrinsic::NatOr,
        O::BoolXor => curios_cont::CpsIntrinsic::NatXor,
        O::BoolEql => curios_cont::CpsIntrinsic::NatEql,
        O::BoolNeq => curios_cont::CpsIntrinsic::NatXor,
        O::NatEql => curios_cont::CpsIntrinsic::NatEql,
        O::NatNeq => curios_cont::CpsIntrinsic::NatNeq,
        O::NatAdd => curios_cont::CpsIntrinsic::NatAdd,
        O::NatSub => curios_cont::CpsIntrinsic::NatSub,
        O::NatMul => curios_cont::CpsIntrinsic::NatMul,
        O::NatLt => curios_cont::CpsIntrinsic::NatLt,
        O::NatDiv => curios_cont::CpsIntrinsic::NatDiv,
        O::NatRem => curios_cont::CpsIntrinsic::NatRem,
        O::NatLe => curios_cont::CpsIntrinsic::NatLe,
        O::NatAnd => curios_cont::CpsIntrinsic::NatAnd,
        O::NatOr => curios_cont::CpsIntrinsic::NatOr,
        O::NatXor => curios_cont::CpsIntrinsic::NatXor,
        O::NatShl => curios_cont::CpsIntrinsic::NatShl,
        O::NatShr => curios_cont::CpsIntrinsic::NatShr,
        O::ByteEql => curios_cont::CpsIntrinsic::NatEql,
        O::ByteLt => curios_cont::CpsIntrinsic::NatLt,
        O::ByteLe => curios_cont::CpsIntrinsic::NatLe,
        O::IntEql => curios_cont::CpsIntrinsic::IntEql,
        O::IntNeq => curios_cont::CpsIntrinsic::IntNeq,
        O::IntAdd => curios_cont::CpsIntrinsic::IntAdd,
        O::IntSub => curios_cont::CpsIntrinsic::IntSub,
        O::IntMul => curios_cont::CpsIntrinsic::IntMul,
        O::IntDiv => curios_cont::CpsIntrinsic::IntDiv,
        O::IntRem => curios_cont::CpsIntrinsic::IntRem,
        O::IntLt => curios_cont::CpsIntrinsic::IntLt,
        O::IntLe => curios_cont::CpsIntrinsic::IntLe,
        O::IntAnd => curios_cont::CpsIntrinsic::IntAnd,
        O::IntOr => curios_cont::CpsIntrinsic::IntOr,
        O::IntXor => curios_cont::CpsIntrinsic::IntXor,
        O::IntShl => curios_cont::CpsIntrinsic::IntShl,
        O::IntShr => curios_cont::CpsIntrinsic::IntShr,
        O::FltAdd => curios_cont::CpsIntrinsic::FltAdd,
        O::FltSub => curios_cont::CpsIntrinsic::FltSub,
        O::FltMul => curios_cont::CpsIntrinsic::FltMul,
        O::FltDiv => curios_cont::CpsIntrinsic::FltDiv,
        O::FltRem => curios_cont::CpsIntrinsic::FltRem,
        O::FltEql => curios_cont::CpsIntrinsic::FltEql,
        O::FltNeq => curios_cont::CpsIntrinsic::FltNeq,
        O::FltLt => curios_cont::CpsIntrinsic::FltLt,
        O::FltLe => curios_cont::CpsIntrinsic::FltLe,
        O::FltMin => curios_cont::CpsIntrinsic::FltMin,
        O::FltMax => curios_cont::CpsIntrinsic::FltMax,
        O::FltCopysign => curios_cont::CpsIntrinsic::FltCopysign,
        O::FltNeg => curios_cont::CpsIntrinsic::FltNeg,
        O::FltAbs => curios_cont::CpsIntrinsic::FltAbs,
        O::FltSqrt => curios_cont::CpsIntrinsic::FltSqrt,
        O::FltFloor => curios_cont::CpsIntrinsic::FltFloor,
        O::FltCeil => curios_cont::CpsIntrinsic::FltCeil,
        O::FltTrunc => curios_cont::CpsIntrinsic::FltTrunc,
        O::FltNearest => curios_cont::CpsIntrinsic::FltNearest,
        O::NatToInt => curios_cont::CpsIntrinsic::NatToInt,
        O::NatToFlt => curios_cont::CpsIntrinsic::NatToFlt,
        O::IntToNat => curios_cont::CpsIntrinsic::IntToNat,
        O::IntToFlt => curios_cont::CpsIntrinsic::IntToFlt,
        O::FltToNat => curios_cont::CpsIntrinsic::FltToNat,
        O::FltToInt => curios_cont::CpsIntrinsic::FltToInt,
        O::FltToLeBytes => curios_cont::CpsIntrinsic::FltToLeBytes,
        O::FltOfLeBytes => curios_cont::CpsIntrinsic::FltOfLeBytes,
        O::ByteToNat | O::NatToByte => {
            unreachable!("Byte conversions are lowered before the intrinsic table")
        }
    }
}

/// The Cont intrinsic of a [`SequenceOp`], threading the operand count into the variadic concatenations. `ListBuild` is a list value, never an intrinsic.
pub(super) fn sequence_intrinsic(operation: SequenceOp, arity: usize) -> curios_cont::CpsIntrinsic {
    use SequenceOp as S;
    match operation {
        S::BinLen(grain) => curios_cont::CpsIntrinsic::BinLen(grain),
        S::BinEql(grain) => curios_cont::CpsIntrinsic::BinEql(grain),
        S::BinGet(grain) => curios_cont::CpsIntrinsic::BinGet(grain),
        S::BinSlice(grain) => curios_cont::CpsIntrinsic::BinSlice(grain),
        S::BinAppend(grain) => curios_cont::CpsIntrinsic::BinAppend(grain),
        S::BinConcat(grain) => curios_cont::CpsIntrinsic::BinConcat(grain, arity),
        S::ListLen => curios_cont::CpsIntrinsic::ListLen,
        S::ListGet => curios_cont::CpsIntrinsic::ListGet,
        S::ListSlice => curios_cont::CpsIntrinsic::ListSlice,
        S::ListAppend => curios_cont::CpsIntrinsic::ListAppend,
        S::ListConcat => curios_cont::CpsIntrinsic::ListConcat(arity),
        S::ListBuild => unreachable!("ListBuild is lowered as a list value"),
    }
}

pub(super) fn cell_op(operation: CellOperation) -> curios_cont::CpsCellOp {
    match operation {
        CellOperation::New => curios_cont::CpsCellOp::New,
        CellOperation::Get => curios_cont::CpsCellOp::Get,
        CellOperation::Set => curios_cont::CpsCellOp::Set,
    }
}

pub(super) fn sequence_len_op(grain: SequenceGrain) -> curios_cont::CpsIntrinsic {
    match grain {
        SequenceGrain::List => curios_cont::CpsIntrinsic::ListLen,
        SequenceGrain::Bin(grain) => curios_cont::CpsIntrinsic::BinLen(grain),
    }
}

pub(super) fn sequence_get_op(grain: SequenceGrain) -> curios_cont::CpsIntrinsic {
    match grain {
        SequenceGrain::List => curios_cont::CpsIntrinsic::ListGet,
        SequenceGrain::Bin(grain) => curios_cont::CpsIntrinsic::BinGet(grain),
    }
}

pub(super) fn sequence_rest_op(grain: SequenceGrain) -> curios_cont::CpsIntrinsic {
    match grain {
        SequenceGrain::List => curios_cont::CpsIntrinsic::ListRest,
        SequenceGrain::Bin(grain) => curios_cont::CpsIntrinsic::BinRest(grain),
    }
}
