//! The sequence operation alphabet and the sequence kinds.
//!
//! Sequence operations form their own family beside the scalar [`Operation`](super::Operation)s: the variadic construction/concatenation forms carry their whole operand list in the statement's operand vector.

/// A packed-binary or list operation. Operand order is documented per variant; [`arity`](SequenceOp::arity) is the single authoritative operand contract.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum SequenceOp {
    /// `(bytes) -> Nat`: the element count.
    BytesLen,
    /// `(a, b) -> Bool`: logical element-wise equality.
    BytesEql,
    /// `(bytes, index) -> element`: the element at `index`; out of bounds traps.
    BytesGet,
    /// `(bytes, start, length) -> bytes`: the `length`-long view at `start`; running past the end traps. A count rather than a stop, so a reversed window is unrepresentable rather than rejected.
    BytesSlice,
    /// `(bytes, element) -> bytes`: append one element.
    BytesAppend,
    /// `(bins…) -> bytes`: concatenate any number of binaries.
    BytesConcat,
    /// `(count, element) -> bytes`: `count` copies of one element.
    BytesReplicate,
    /// `(a, b) -> bytes`: the element-wise conjunction of two binaries of one length.
    BytesAnd,
    /// `(a, b) -> bytes`: the element-wise disjunction, as [`SequenceOp::BytesAnd`].
    BytesOr,
    /// `(a, b) -> bytes`: the element-wise difference, as [`SequenceOp::BytesAnd`].
    BytesXor,
    /// `(elements…) -> list`: build a list from any number of elements.
    ListBuild,
    /// `(list) -> Nat`: the element count.
    ListLen,
    /// `(list, index) -> element`: the element at `index`; out of bounds traps.
    ListGet,
    /// `(list, start, length) -> list`: the `List` mirror of [`SequenceOp::BytesSlice`].
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
            Self::BytesLen | Self::ListLen => SequenceArity::Exactly(1),
            Self::BytesEql
            | Self::BytesGet
            | Self::BytesAppend
            | Self::BytesReplicate
            | Self::BytesAnd
            | Self::BytesOr
            | Self::BytesXor
            | Self::ListGet
            | Self::ListAppend => SequenceArity::Exactly(2),
            Self::BytesSlice | Self::ListSlice => SequenceArity::Exactly(3),
            Self::BytesConcat | Self::ListBuild | Self::ListConcat => SequenceArity::AnyCount,
        }
    }
}

/// The sequence kind a fold eliminates — a homogeneous list or a packed binary — enough to recover exact element and suffix behavior.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum SequenceGrain {
    List,
    Bytes,
}
