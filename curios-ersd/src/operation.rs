//! The scalar operation alphabet.
//!
//! One fieldless variant per intrinsic operation over the scalar shapes — `Bool`, `Nat` (exact `u32`), `Byte`, `Int` (exact `i32`), `Flt` (bit-preserving binary32), and `Handle` — transcribed one-to-one from Core's intrinsic vocabulary. Every shape stays distinct: there are no carrier choices here (`Bool`→`Nat`, `Byte`→`Nat`, `Handle`→`Bin` belong exclusively to the lowering), and no 31-bit fact appears anywhere in this alphabet. Sequence operations live in their own family ([`super::SequenceOp`]).

/// A scalar intrinsic operation. [`arity`](Operation::arity) is the single authoritative operand count; the verifier and every later consumer delegate to it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum Operation {
    BoolAnd,
    BoolOr,
    BoolXor,
    BoolEql,
    BoolNeq,
    NatEql,
    NatNeq,
    NatAdd,
    NatSub,
    NatMul,
    NatLt,
    NatDiv,
    NatRem,
    NatLe,
    NatAnd,
    NatOr,
    NatXor,
    NatShl,
    NatShr,
    ByteToNat,
    NatToByte,
    ByteEql,
    ByteLt,
    ByteLe,
    IntEql,
    IntNeq,
    IntAdd,
    IntSub,
    IntMul,
    IntDiv,
    IntRem,
    IntLt,
    IntLe,
    IntAnd,
    IntOr,
    IntXor,
    IntShl,
    IntShr,
    FltAdd,
    FltSub,
    FltMul,
    FltDiv,
    FltRem,
    FltEql,
    FltNeq,
    FltLt,
    FltLe,
    FltMin,
    FltMax,
    FltCopysign,
    FltNeg,
    FltAbs,
    FltSqrt,
    FltFloor,
    FltCeil,
    FltTrunc,
    FltNearest,
    NatToInt,
    NatToFlt,
    IntToNat,
    IntToFlt,
    FltToNat,
    FltToInt,
    FltToLeBytes,
    FltOfLeBytes,
    HandleEql,
}

impl Operation {
    /// The exact operand count of this operation.
    pub fn arity(self) -> usize {
        match self {
            Self::ByteToNat
            | Self::NatToByte
            | Self::FltNeg
            | Self::FltAbs
            | Self::FltSqrt
            | Self::FltFloor
            | Self::FltCeil
            | Self::FltTrunc
            | Self::FltNearest
            | Self::NatToInt
            | Self::NatToFlt
            | Self::IntToNat
            | Self::IntToFlt
            | Self::FltToNat
            | Self::FltToInt
            | Self::FltToLeBytes
            | Self::FltOfLeBytes => 1,
            Self::BoolAnd
            | Self::BoolOr
            | Self::BoolXor
            | Self::BoolEql
            | Self::BoolNeq
            | Self::NatEql
            | Self::NatNeq
            | Self::NatAdd
            | Self::NatSub
            | Self::NatMul
            | Self::NatLt
            | Self::NatDiv
            | Self::NatRem
            | Self::NatLe
            | Self::NatAnd
            | Self::NatOr
            | Self::NatXor
            | Self::NatShl
            | Self::NatShr
            | Self::ByteEql
            | Self::ByteLt
            | Self::ByteLe
            | Self::IntEql
            | Self::IntNeq
            | Self::IntAdd
            | Self::IntSub
            | Self::IntMul
            | Self::IntDiv
            | Self::IntRem
            | Self::IntLt
            | Self::IntLe
            | Self::IntAnd
            | Self::IntOr
            | Self::IntXor
            | Self::IntShl
            | Self::IntShr
            | Self::FltAdd
            | Self::FltSub
            | Self::FltMul
            | Self::FltDiv
            | Self::FltRem
            | Self::FltEql
            | Self::FltNeq
            | Self::FltLt
            | Self::FltLe
            | Self::FltMin
            | Self::FltMax
            | Self::FltCopysign
            | Self::HandleEql => 2,
        }
    }
}
