//! The scalar operation alphabet.
//!
//! One fieldless variant per primitive operation over the scalar shapes —
//! `Bln`, `Nat` (exact `u32`), `Byte`, `Int` (exact `i32`), `Flt` (bit-preserving
//! binary32), and `Io` — transcribed one-to-one from Core's primitive
//! vocabulary. Every shape stays distinct: there are no carrier choices here
//! (`Bln`→`Nat`, `Byte`→`Nat`, `Io`→`Bin` belong exclusively to the lowering),
//! and no 31-bit fact appears anywhere in this alphabet. Sequence operations
//! live in their own family ([`super::SequenceOp`]).

/// A scalar primitive operation. [`arity`](Operation::arity) is the single
/// authoritative operand count; the verifier and every later consumer delegate
/// to it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum Operation {
    BlnAnd,
    BlnOr,
    BlnXor,
    BlnEql,
    BlnNeq,
    NatEql,
    NatNeq,
    NatAdd,
    NatSub,
    NatMul,
    NatLt,
    NatDiv,
    NatRem,
    NatGt,
    NatLte,
    NatGte,
    NatAnd,
    NatOr,
    NatXor,
    NatShl,
    NatShr,
    NatRotl,
    NatRotr,
    NatClz,
    NatCtz,
    NatPopcnt,
    ByteToNat,
    NatToByte,
    ByteEql,
    ByteLt,
    ByteLte,
    ByteGt,
    ByteGte,
    IntEql,
    IntNeq,
    IntAdd,
    IntSub,
    IntMul,
    IntDiv,
    IntRem,
    IntLt,
    IntGt,
    IntLte,
    IntGte,
    IntAnd,
    IntOr,
    IntXor,
    IntShl,
    IntShr,
    IntRotl,
    IntRotr,
    IntClz,
    IntCtz,
    IntPopcnt,
    FltAdd,
    FltSub,
    FltMul,
    FltDiv,
    FltRem,
    FltEql,
    FltNeq,
    FltLt,
    FltGt,
    FltLte,
    FltGte,
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
    IoEql,
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
            | Self::FltOfLeBytes
            | Self::NatClz
            | Self::NatCtz
            | Self::NatPopcnt
            | Self::IntClz
            | Self::IntCtz
            | Self::IntPopcnt => 1,
            Self::BlnAnd
            | Self::BlnOr
            | Self::BlnXor
            | Self::BlnEql
            | Self::BlnNeq
            | Self::NatEql
            | Self::NatNeq
            | Self::NatAdd
            | Self::NatSub
            | Self::NatMul
            | Self::NatLt
            | Self::NatDiv
            | Self::NatRem
            | Self::NatGt
            | Self::NatLte
            | Self::NatGte
            | Self::NatAnd
            | Self::NatOr
            | Self::NatXor
            | Self::NatShl
            | Self::NatShr
            | Self::NatRotl
            | Self::NatRotr
            | Self::ByteEql
            | Self::ByteLt
            | Self::ByteLte
            | Self::ByteGt
            | Self::ByteGte
            | Self::IntEql
            | Self::IntNeq
            | Self::IntAdd
            | Self::IntSub
            | Self::IntMul
            | Self::IntDiv
            | Self::IntRem
            | Self::IntLt
            | Self::IntGt
            | Self::IntLte
            | Self::IntGte
            | Self::IntAnd
            | Self::IntOr
            | Self::IntXor
            | Self::IntShl
            | Self::IntShr
            | Self::IntRotl
            | Self::IntRotr
            | Self::FltAdd
            | Self::FltSub
            | Self::FltMul
            | Self::FltDiv
            | Self::FltRem
            | Self::FltEql
            | Self::FltNeq
            | Self::FltLt
            | Self::FltGt
            | Self::FltLte
            | Self::FltGte
            | Self::FltMin
            | Self::FltMax
            | Self::FltCopysign
            | Self::IoEql => 2,
        }
    }
}
