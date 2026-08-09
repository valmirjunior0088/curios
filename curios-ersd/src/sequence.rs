//! The sequence operation alphabet and the sequence kinds.
//!
//! Sequence operations form their own family beside the scalar [`Operation`](super::Operation)s: the packed-binary forms carry their [`Grain`] so `Bool`- and `Byte`-grained binaries stay distinct shapes, and the variadic construction/concatenation forms carry their whole operand list in the statement's operand vector.

use curios_base::Grain;

/// A packed-binary or list operation. Operand order is documented per variant; [`arity`](SequenceOp::arity) is the single authoritative operand contract.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum SequenceOp {
    /// `(bin) -> Nat`: the element count.
    BinLen(Grain),
    /// `(a, b) -> Bool`: logical element-wise equality.
    BinEql(Grain),
    /// `(bin, index) -> element`: the element at `index`; out of bounds traps.
    BinGet(Grain),
    /// `(bin, start, end) -> bin`: the `[start, end)` view; out of bounds traps.
    BinSlice(Grain),
    /// `(bin, element) -> bin`: append one element.
    BinAppend(Grain),
    /// `(bins…) -> bin`: concatenate any number of binaries.
    BinConcat(Grain),
    /// `(elements…) -> list`: build a list from any number of elements.
    ListBuild,
    /// `(list) -> Nat`: the element count.
    ListLen,
    /// `(list, index) -> element`: the element at `index`; out of bounds traps.
    ListGet,
    /// `(list, start, end) -> list`: the `[start, end)` view; out of bounds traps.
    ListSlice,
    /// `(list, element) -> list`: append one element.
    ListAppend,
    /// `(lists…) -> list`: concatenate any number of lists.
    ListConcat,
}

/// The operand contract of a [`SequenceOp`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SequenceArity {
    /// Exactly this many operands.
    Exactly(usize),
    /// Any number of operands, including zero.
    AnyCount,
}

impl SequenceOp {
    /// The operand contract of this operation.
    pub fn arity(self) -> SequenceArity {
        match self {
            Self::BinLen(_) | Self::ListLen => SequenceArity::Exactly(1),
            Self::BinEql(_)
            | Self::BinGet(_)
            | Self::BinAppend(_)
            | Self::ListGet
            | Self::ListAppend => SequenceArity::Exactly(2),
            Self::BinSlice(_) | Self::ListSlice => SequenceArity::Exactly(3),
            Self::BinConcat(_) | Self::ListBuild | Self::ListConcat => SequenceArity::AnyCount,
        }
    }
}

/// The sequence kind a fold eliminates — a homogeneous list or a packed binary of a given grain — enough to recover exact element and suffix behavior.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum SequenceGrain {
    List,
    Bin(Grain),
}
