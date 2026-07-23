//! Atomic operands and the leaf constant domain.
//!
//! Every operand position in the representation holds an [`Atom`]: a
//! reference to a bound value, a bound function, or an interned constant.
//! Compound computation never nests — it is bound by a statement first (the
//! operand law's structural half).

use {
    super::{ConstantId, FunctionId, ValueId},
    curios_base::{Flt, Grain, PackedBin},
};

/// An atomic operand: a lexically bound value, a bound function used as a
/// value, or an interned constant.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum Atom {
    Value(ValueId),
    Function(FunctionId),
    Constant(ConstantId),
}

/// A leaf constant — the scalar and packed-binary domain of erased Core.
///
/// Identity is exact and bitwise: `Nat`/`Int` are the full 32-bit domains,
/// `Flt` compares by bit pattern (NaN payloads and signed zeros included), and
/// `Bin` compares by logical bit content independent of its backing window.
/// Interning in the module's constant arena keys on this identity, so equal
/// constants share one [`ConstantId`]. There are no aggregate constants:
/// products, variants, and lists with constant elements are built by
/// construction statements over these leaves.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum Constant {
    /// The unit value — the value of a retained-but-erased slot.
    Unit,
    Bool(bool),
    Nat(u32),
    Int(i32),
    Flt(Flt),
    Byte(u8),
    /// A packed binary of the given grain.
    Bin(Grain, PackedBin),
    /// An opaque host handle token.
    Handle(u32),
}
