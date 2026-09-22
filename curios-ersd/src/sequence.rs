//! The sequence operation alphabet and the sequence kinds.
//!
//! Sequence operations form their own family beside the scalar [`Operation`](super::Operation)s: the packed-binary forms carry their [`Grain`] so `Bool`- and `Byte`-grained binaries stay distinct shapes, and the variadic construction/concatenation forms carry their whole operand list in the statement's operand vector.

use curios_num::Grain;

/// A packed-binary or list operation. Operand order is documented per variant; [`arity`](SequenceOp::arity) is the single authoritative operand contract.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum SequenceOp {
    /// `(bin) -> Nat`: the element count.
    BinLen(Grain),
    /// `(a, b) -> Bool`: logical element-wise equality.
    BinEql(Grain),
    /// `(bin, index) -> element`: the element at `index`; out of bounds traps.
    BinGet(Grain),
    /// `(bin, start, length) -> bin`: the `length`-long view at `start`; running past the end traps. A count rather than a stop, so a reversed window is unrepresentable rather than rejected.
    BinSlice(Grain),
    /// `(bin, element) -> bin`: append one element.
    BinAppend(Grain),
    /// `(bins…) -> bin`: concatenate any number of binaries.
    BinConcat(Grain),
    /// `(count, element) -> bin`: `count` copies of one element.
    BinReplicate(Grain),
    /// `(bin) -> bin`: the run read at the other grain, eight bits to the byte. The [`Grain`] is the operand's, so the result is the one it is not.
    BinReinterp(Grain),
    /// `(a, b) -> bin`: the element-wise conjunction of two binaries of one length.
    BinAnd(Grain),
    /// `(a, b) -> bin`: the element-wise disjunction, as [`SequenceOp::BinAnd`].
    BinOr(Grain),
    /// `(a, b) -> bin`: the element-wise difference, as [`SequenceOp::BinAnd`].
    BinXor(Grain),
    /// `(elements…) -> list`: build a list from any number of elements.
    ListBuild,
    /// `(list) -> Nat`: the element count.
    ListLen,
    /// `(list, index) -> element`: the element at `index`; out of bounds traps.
    ListGet,
    /// `(list, start, length) -> list`: the `List` mirror of [`SequenceOp::BinSlice`].
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
            Self::BinLen(_) | Self::BinReinterp(_) | Self::ListLen => SequenceArity::Exactly(1),
            Self::BinEql(_)
            | Self::BinGet(_)
            | Self::BinAppend(_)
            | Self::BinReplicate(_)
            | Self::BinAnd(_)
            | Self::BinOr(_)
            | Self::BinXor(_)
            | Self::ListGet
            | Self::ListAppend => SequenceArity::Exactly(2),
            Self::BinSlice(_) | Self::ListSlice => SequenceArity::Exactly(3),
            Self::BinConcat(_) | Self::ListBuild | Self::ListConcat => SequenceArity::AnyCount,
        }
    }
}

/// The sequence kind a fold eliminates — a homogeneous list or a packed binary of a given grain — enough to recover exact element and suffix behavior.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum SequenceGrain {
    List,
    Bin(Grain),
}
