//! The Cont operation each erased operation names.
//!
//! A total table, one arm per [`Operation`], plus the three small families the sequence operations fan out into by grain. Nothing here reduces or decides: the erased roster and the Cont roster are two spellings of the same set, and this is the translation between them, kept apart from the lowering so that adding an operation is an edit to a table rather than to a walk.

use super::{CellOperation, Operation, SequenceGrain, SequenceOp};

/// The Cont intrinsic of a scalar [`Operation`]. `Bool` operations run on the `0`/`1` `Nat` carrier (`BoolNeq` is xor on a single bit) and `Byte` comparisons on the `Nat` carrier. The `Byte` conversions are handled before this table.
pub(super) fn operation_intrinsic(operation: Operation) -> curios_cont::Intrinsic {
    use Operation as O;
    match operation {
        O::BoolAnd => curios_cont::Intrinsic::NatAnd,
        O::BoolOr => curios_cont::Intrinsic::NatOr,
        O::BoolXor => curios_cont::Intrinsic::NatXor,
        O::BoolEql => curios_cont::Intrinsic::NatEql,
        O::BoolNeq => curios_cont::Intrinsic::NatXor,
        O::NatEql => curios_cont::Intrinsic::NatEql,
        O::NatNeq => curios_cont::Intrinsic::NatNeq,
        O::NatAdd => curios_cont::Intrinsic::NatAdd,
        O::NatSub => curios_cont::Intrinsic::NatSub,
        O::NatMul => curios_cont::Intrinsic::NatMul,
        O::NatLt => curios_cont::Intrinsic::NatLt,
        O::NatDiv => curios_cont::Intrinsic::NatDiv,
        O::NatRem => curios_cont::Intrinsic::NatRem,
        O::NatLe => curios_cont::Intrinsic::NatLe,
        O::NatAnd => curios_cont::Intrinsic::NatAnd,
        O::NatOr => curios_cont::Intrinsic::NatOr,
        O::NatXor => curios_cont::Intrinsic::NatXor,
        O::NatShl => curios_cont::Intrinsic::NatShl,
        O::NatShr => curios_cont::Intrinsic::NatShr,
        O::IntEql => curios_cont::Intrinsic::IntEql,
        O::IntNeq => curios_cont::Intrinsic::IntNeq,
        O::IntAdd => curios_cont::Intrinsic::IntAdd,
        O::IntSub => curios_cont::Intrinsic::IntSub,
        O::IntMul => curios_cont::Intrinsic::IntMul,
        O::IntDiv => curios_cont::Intrinsic::IntDiv,
        O::IntRem => curios_cont::Intrinsic::IntRem,
        O::IntLt => curios_cont::Intrinsic::IntLt,
        O::IntLe => curios_cont::Intrinsic::IntLe,
        O::IntAnd => curios_cont::Intrinsic::IntAnd,
        O::IntOr => curios_cont::Intrinsic::IntOr,
        O::IntXor => curios_cont::Intrinsic::IntXor,
        O::IntShl => curios_cont::Intrinsic::IntShl,
        O::IntShr => curios_cont::Intrinsic::IntShr,
        O::FltAdd(rounding) => curios_cont::Intrinsic::FltAdd(rounding),
        O::FltSub(rounding) => curios_cont::Intrinsic::FltSub(rounding),
        O::FltMul(rounding) => curios_cont::Intrinsic::FltMul(rounding),
        O::FltDiv(rounding) => curios_cont::Intrinsic::FltDiv(rounding),
        O::FltFma(rounding) => curios_cont::Intrinsic::FltFma(rounding),
        O::FltRem => curios_cont::Intrinsic::FltRem,
        O::FltEql => curios_cont::Intrinsic::FltEql,
        O::FltNeq => curios_cont::Intrinsic::FltNeq,
        O::FltLt => curios_cont::Intrinsic::FltLt,
        O::FltLe => curios_cont::Intrinsic::FltLe,
        O::FltMin => curios_cont::Intrinsic::FltMin,
        O::FltMax => curios_cont::Intrinsic::FltMax,
        O::FltCopysign => curios_cont::Intrinsic::FltCopysign,
        O::FltNeg => curios_cont::Intrinsic::FltNeg,
        O::FltAbs => curios_cont::Intrinsic::FltAbs,
        O::FltSqrt(rounding) => curios_cont::Intrinsic::FltSqrt(rounding),
        O::FltRoundIntegral(rounding) => curios_cont::Intrinsic::FltRoundIntegral(rounding),
        O::NatToInt => curios_cont::Intrinsic::NatToInt,
        O::NatToFlt(rounding) => curios_cont::Intrinsic::NatToFlt(rounding),
        O::IntToNat => curios_cont::Intrinsic::IntToNat,
        O::IntToFlt(rounding) => curios_cont::Intrinsic::IntToFlt(rounding),
        O::FltToNat => curios_cont::Intrinsic::FltToNat,
        O::FltToInt => curios_cont::Intrinsic::FltToInt,
        O::FltMantissa => curios_cont::Intrinsic::FltMantissa,
        O::FltExponent => curios_cont::Intrinsic::FltExponent,
        O::FltToLeBytes => curios_cont::Intrinsic::FltToLeBytes,
        O::FltOfLeBytes => curios_cont::Intrinsic::FltOfLeBytes,
        O::ByteToNat | O::NatToByte => {
            unreachable!("Byte conversions are lowered before the intrinsic table")
        }
    }
}

/// The Cont intrinsic of a [`SequenceOp`], threading the operand count into the variadic concatenations. `ListBuild` is a list value, never an intrinsic.
pub(super) fn sequence_intrinsic(operation: SequenceOp, arity: usize) -> curios_cont::Intrinsic {
    use SequenceOp as S;
    match operation {
        S::BinLen(grain) => curios_cont::Intrinsic::BinLen(grain),
        S::BinEql(grain) => curios_cont::Intrinsic::BinEql(grain),
        S::BinGet(grain) => curios_cont::Intrinsic::BinGet(grain),
        S::BinSlice(grain) => curios_cont::Intrinsic::BinSlice(grain),
        S::BinAppend(grain) => curios_cont::Intrinsic::BinAppend(grain),
        S::BinConcat(grain) => curios_cont::Intrinsic::BinConcat(grain, arity),
        S::BinReplicate(grain) => curios_cont::Intrinsic::BinReplicate(grain),
        S::BinReinterp(grain) => curios_cont::Intrinsic::BinReinterp(grain),
        S::BinAnd(grain) => curios_cont::Intrinsic::BinAnd(grain),
        S::BinOr(grain) => curios_cont::Intrinsic::BinOr(grain),
        S::BinXor(grain) => curios_cont::Intrinsic::BinXor(grain),
        S::ListLen => curios_cont::Intrinsic::ListLen,
        S::ListGet => curios_cont::Intrinsic::ListGet,
        S::ListSlice => curios_cont::Intrinsic::ListSlice,
        S::ListAppend => curios_cont::Intrinsic::ListAppend,
        S::ListConcat => curios_cont::Intrinsic::ListConcat(arity),
        S::ListBuild => unreachable!("ListBuild is lowered as a list value"),
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
