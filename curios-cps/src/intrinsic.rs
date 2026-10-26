//! The primitive operations: each one's arity and effect, and the representation it reads every operand and produces its result at.

use {
    super::{Atom, Bound, Literal, RowId},
    curios_num::Rounding,
};

/// Intrinsic identity without operands. Operand order and arity live on the surrounding `LetIntrinsic`, so every analysis sees one uniform operand vector.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Intrinsic {
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
    NatMin,
    NatMax,
    NatShl,
    NatPow,
    NatShr,
    NatEqz,
    NatToInt,
    NatToFlt(Rounding),
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
    IntMin,
    IntMax,
    IntAbs,
    IntSign,
    IntShl,
    IntShr,
    IntEqz,
    IntToNat,
    IntToFlt(Rounding),
    FltAdd(Rounding),
    FltSub(Rounding),
    FltMul(Rounding),
    FltDiv(Rounding),
    FltFma(Rounding),
    FltRem,
    FltEql,
    FltNeq,
    FltLt,
    FltLe,
    FltMin,
    FltMax,
    FltNeg,
    FltAbs,
    FltSqrt(Rounding),
    FltRoundIntegral(Rounding),
    FltCopysign,
    FltToNat,
    FltToLeBytes,
    FltOfLeBytes,
    FltToInt,
    FltMantissa,
    FltExponent,
    BytesLen,
    BytesEql,
    BytesGet,
    BytesSlice,
    /// `(bytes, start) -> bytes`: the suffix from `start`, whose extent is the value's own — there is no count operand to supply, so the one thing a caller could get wrong about a suffix it cannot say. Every compiler-emitted window is a suffix (`into_cps`'s peel is the only producer), and this is what keeps two lowerings from having to agree about how a count is derived; the derivation happens once, against the rope's own length.
    BytesRest,
    BytesAppend,
    BytesConcat(usize),
    /// `(count, element) -> bytes`: one flat leaf of `count` copies of `element`. The size is an operand's value rather than an operand's extent, which is what separates it from every other construction here.
    BytesReplicate,
    /// `(a, b) -> bytes`: the element-wise conjunction of two binaries the type has already held to one length. Total — the bound is discharged above erasure and nothing here can fail it.
    BytesAnd,
    /// `(a, b) -> bytes`: the element-wise disjunction, as [`Intrinsic::BytesAnd`].
    BytesOr,
    /// `(a, b) -> bytes`: the element-wise difference, as [`Intrinsic::BytesAnd`].
    BytesXor,
    /// `(element…) -> bytes`: one flat leaf holding exactly these elements — the fused form of an append chain, minted only by the optimizer's `fuse_append_chains` and never by the door. The arity is the element count, one per payload byte.
    BytesChunk(usize),
    ListLen,
    ListGet,
    ListSlice,
    /// The `List` mirror of [`Intrinsic::BytesRest`].
    ListRest,
    ListAppend,
    ListConcat(usize),
    /// `(list) -> list`: the same value, flat — a leaf answers itself, and anything else answers a fresh leaf over its forced payload (an O(1) wrap, since payload arrays are filled once and never rewritten). Semantically the identity; representationally the settle the door inserts on stores into fields the ANF census marked indexed-only, so the values a program only ever indexes are flat by the time they are stored.
    ListSettle,
    /// `(list…) -> list`: one exact-length flat leaf holding every element of every operand in order — the eager concatenation `fuse_append_chains` builds where the reads that would have paid the gather are already in evidence. Minted only by the optimizer, like [`Intrinsic::BytesChunk`].
    ListFlat(usize),
    TupleGet(usize),
    /// `(row) -> value`: slot `index` of a [`ValueExpr::Row`](super::ValueExpr::Row) of `row`. Which slot holds what is the row's to say — a family's slot zero is its tag — and the door is what knows it. Distinct from [`Intrinsic::TupleGet`] so a row read names the row whose final type the emitter casts to exactly, and so a structural projection can never silently read a row value through the roster cascade: the two vocabularies meet only in the verifier, which refuses a mismatch.
    RowGet(RowId, usize),
    /// The virtual-window bounds guard: `(start, count, len) -> count`, trapping unless the window ends inside `len` — the eager trap a physical slice would have performed, kept at the original evaluation point when the slice itself is virtualized away. It answers the count unchanged rather than a difference, because a window is a start and a count everywhere above this too; what it contributes is the trap, not the arithmetic.
    WindowExtent,
    /// Whether the operand is a bare payload — an i31, or the boxed magnitude of a `Nat` or `Int` past it — (1) or a row struct (0): the dispatch of a variant encoding whose one scalar-payload constructor rides bare. The boxed magnitude is its own final type, which no row shares, so admitting it keeps the two answers disjoint for a `Nat` or `Int` payload as for a word. A representation question, which is why it exists in this crate's vocabulary and not in ANF: the lowering that chose the encoding is the only producer, and it guarantees the two answers are disjoint over every value the test can reach.
    IsImmediate,
    /// `(value) -> value`: the bare payload of the constructor [`Intrinsic::IsImmediate`] just answered for, passed through unchanged.
    ///
    /// Representationally the identity, and that is the whole point: it exists so the payload has a *definition* instead of being aliased to the scrutinee. The representation analysis fixes a value's carrier from whatever produced it, so a payload with no producer of its own carries its uses' raw demand back onto the scrutinee — which on the boxed path is a tuple, not a scalar. That is not a missed optimization but a miscompile: an arm's `NatAdd` demanding the raw carrier would reach the scrutinee's own definition, and the emitter would coerce a `struct.new` with a `ref.cast` to `i31`. Answering `Repr::Ref` makes this definition's offer `Never`, so the demand coerces at the use where it belongs and the scrutinee is never demanded raw.
    ImmediateGet,
    /// `(value) -> Bool`: whether the operand is a value of the given row. A row's heap type is final and shares its recursion group with every other row, so no other row, tuple or carrier answers yes, and the test is exact over every value. What tells a forced description's suspension, a row the door registers for it, from the value the description yields.
    IsRow(RowId),
}

/// The representation a value is read or produced at — the carrier, not the type.
///
/// This is the vocabulary the backend's coercions translate — `LoadAs` into a carrier and `box_instr` back out of one: `Nat` and `Flt` name raw machine carriers a Wasm register can hold, and the rest name references. Stated here, on the IR, rather than in the emitter, because the *optimizer* has to be able to ask what an operation demands of its operands without running codegen to find out — and because an emitter that restates the demand at every use site is an emitter that can disagree with the analysis.
///
/// **A `Nat` or `Int` is not a machine word.** Either is a reference — an i31 or a boxed magnitude, see [`ENVELOPE_BITS`](super::ENVELOPE_BITS) — so an operation on them reads [`Repr::Number`] and produces `Repr::Ref`; the word is what a `Bool`, a byte, a bit and a tag are, and every word lies below `2³⁰`, so it boxes to a `Nat` as the i31 it already is. The exception is a `Nat` the representation analysis bounds below `2³⁰`, from what reaches the operation that computes it (`Intrinsic::result_bound`), which a word may hold. A length is not a word, since a sequence may outgrow the i31, and is produced as the `Nat` it is, or held in a word read unsigned, [`Repr::Length`], where the analysis bounds it below `2³²`, as it does what a monus of a length leaves. A sequence's logical length and a channel's capacity are held to `2³² - 1` before a construction narrows or allocates; a larger value refuses as [`Panic::Capacity`](super::Panic::Capacity). A `Nat` becomes a word where a position, a window count or a key is asked for, exact below `2³² - 1` and saturating there, so an index no sequence can reach fails the bounds check it meets. A shift count instead crosses as an unsigned 64-bit value saturating at `2⁶⁴ - 1`, beyond every magnitude the limb array can hold; a nonzero left shift refuses a result past that array's capacity.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Repr {
    /// A raw machine word holding a natural below `2³⁰`: a `Bool`, a byte, a tag, or a `Nat` the representation analysis bounds there.
    Nat,
    /// A raw binary64 carrier.
    Flt,
    /// A raw machine word read unsigned, holding a natural below `2³²`: a sequence's length and the positions and counts a length bounds. A word is one too, every word lying below `2³⁰`; arithmetic that takes an i31 apart takes this boxed, since it may lie past the i31.
    Length,
    /// A `Nat` or `Int` operand: a reference in general, and the word it is where the representation analysis holds one there — a small literal, a bounded result, or a parameter every argument reaching it is one of those. A demand, never a carrier: no value is held at it, and a use reading it tests a reference before unboxing and takes a word as it is.
    Number,
    /// A packed-binary reference: a small-canonical immediate or a rope.
    Bytes,
    /// A list rope reference.
    List,
    /// An opaque reference: nothing is read of it, so nothing constrains it.
    Ref,
}

impl Intrinsic {
    /// The representation this operation reads its `index`-th operand at.
    ///
    /// Indexed rather than returning a sequence because the concatenations are variadic and every operand of one shares a representation, so a list would allocate to say what a match arm already says.
    pub fn operand_repr(&self, index: usize) -> Repr {
        match (self, index) {
            // The sequence operations are the only ones whose operands differ from one another: a rope first, then positions.
            (Self::BytesGet | Self::BytesSlice | Self::BytesRest | Self::BytesAppend, 0) => {
                Repr::Bytes
            }
            (Self::BytesGet | Self::BytesSlice | Self::BytesRest, _) => Repr::Length,
            // An appended element is one packed byte.
            (Self::BytesAppend, _) => Repr::Nat,
            // Both operands of a pointwise combination are ropes, which is what separates it from every other sequence row here — there is no position among them.
            (Self::BytesAnd | Self::BytesOr | Self::BytesXor, _) => Repr::Bytes,
            // A fill takes a count and a generator, and neither is a rope: the element rides the `Nat` carrier as an append's does.
            (Self::BytesReplicate, _) => Repr::Nat,
            (Self::ListGet | Self::ListSlice | Self::ListRest | Self::ListAppend, 0) => Repr::List,
            (Self::ListGet | Self::ListSlice | Self::ListRest, _) => Repr::Length,
            // A chunk element is one packed byte, carried at the `Nat` carrier like an append's.
            (Self::BytesChunk(_), _) => Repr::Nat,
            // A list element is carried, never interpreted — unlike a `Bytes` element, which rides the `Nat` carrier.
            (Self::ListAppend, _) => Repr::Ref,
            (Self::BytesConcat(_), _) | (Self::BytesEql | Self::BytesLen, _) => Repr::Bytes,
            (Self::FltOfLeBytes, _) => Repr::Bytes,
            (Self::WindowExtent, _) => Repr::Length,
            // The whole point of a test is to look at the reference uncoerced, and the read that follows the immediate one hands that same reference on.
            (Self::IsImmediate | Self::ImmediateGet | Self::IsRow(_), _) => Repr::Ref,
            (Self::ListConcat(_) | Self::ListLen | Self::ListSettle | Self::ListFlat(_), _) => {
                Repr::List
            }
            (Self::TupleGet(_) | Self::RowGet(..), _) => Repr::Ref,

            // A `Nat` or `Int` is a reference whatever its size — an i31 or a boxed magnitude — and each lowering takes it apart itself, the small case inline, or takes the word it already is; a shift count is such a `Nat` too.
            (
                Self::NatEql
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
                | Self::NatMin
                | Self::NatMax
                | Self::NatShl
                | Self::NatPow
                | Self::NatShr
                | Self::NatEqz
                | Self::NatToInt
                | Self::IntAbs
                | Self::IntSign
                | Self::NatToFlt(_)
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
                | Self::IntMin
                | Self::IntMax
                | Self::IntShl
                | Self::IntShr
                | Self::IntEqz
                | Self::IntToNat
                | Self::IntToFlt(_),
                _,
            ) => Repr::Number,

            (
                Self::FltAdd(_)
                | Self::FltSub(_)
                | Self::FltMul(_)
                | Self::FltDiv(_)
                | Self::FltFma(_)
                | Self::FltRem
                | Self::FltEql
                | Self::FltNeq
                | Self::FltLt
                | Self::FltLe
                | Self::FltMin
                | Self::FltMax
                | Self::FltNeg
                | Self::FltAbs
                | Self::FltSqrt(_)
                | Self::FltRoundIntegral(_)
                | Self::FltCopysign
                | Self::FltToNat
                | Self::FltToLeBytes
                | Self::FltToInt
                | Self::FltMantissa
                | Self::FltExponent,
                _,
            ) => Repr::Flt,
        }
    }

    /// Whether this operation's result may ride a machine word where the representation analysis bounds it below `2³⁰`: the `Nat` arithmetic whose lowering settles its fast path's `i32`, which is the value itself under that bound, against which the emitter checks a word-held result. A left shift and a power settle no fast path into a word, and an `Int` may be negative.
    pub fn may_bound_result(self) -> bool {
        matches!(
            self,
            Intrinsic::NatAdd
                | Intrinsic::NatSub
                | Intrinsic::NatMul
                | Intrinsic::NatDiv
                | Intrinsic::NatRem
                | Intrinsic::NatAnd
                | Intrinsic::NatOr
                | Intrinsic::NatXor
                | Intrinsic::NatMin
                | Intrinsic::NatMax
                | Intrinsic::NatShr
        )
    }

    /// Whether this operation's result may be held at [`Repr::Length`] where the representation analysis bounds it below `2³²`: a length, a window's checked count and a monus, whose lowerings compute in a word read unsigned.
    pub fn may_hold_length(self) -> bool {
        matches!(
            self,
            Intrinsic::BytesLen | Intrinsic::ListLen | Intrinsic::WindowExtent | Intrinsic::NatSub
        )
    }

    /// What this operation's result lies below, `bound` answering each operand's: the representation analysis's transfer.
    ///
    /// A sum lies a bit past its wider operand and a product within both; a remainder, a conjunction and a minimum within the narrower, and a disjunction, an exclusive or and a maximum within the wider; a monus, a quotient and a right shift within their first operand, as many bits lower as a literal divisor or count takes off, and a left shift by a literal count that many higher. A length lies below `2³²`, a byte below `2⁸` and a predicate's answer below 2, and a window's checked count within both its count and the length it is checked against. Everything else is bounded by nothing here: an `Int`, which may be negative, a left shift by a count no literal fixes, a power, and what is stored and read back, whose bound is the store's to say.
    pub(crate) fn result_bound(&self, args: &[Atom], bound: impl Fn(&Atom) -> Bound) -> Bound {
        let operand = |index: usize| bound(&args[index]);
        let literal = |index: usize| match &args[index] {
            Atom::Literal(Literal::Nat(value)) => Some(value),
            _ => None,
        };
        // A shift count past `u64` lowers anything to zero and raises anything past every bound.
        let count =
            |index: usize| literal(index).map(|value| u64::try_from(value).unwrap_or(u64::MAX));

        match self {
            Self::NatAdd => operand(0).wider(operand(1)).raised(1),
            Self::NatMul => operand(0).times(operand(1)),
            // A remainder lies below its divisor, which for a literal is tighter than the bound the literal itself lies below where it is a power of two.
            Self::NatRem => operand(0).narrower(literal(1).map_or(operand(1), Bound::under)),
            Self::NatAnd | Self::NatMin => operand(0).narrower(operand(1)),
            Self::NatOr | Self::NatXor | Self::NatMax => operand(0).wider(operand(1)),
            Self::NatSub => operand(0),
            // A divisor of at least `2^m` takes `m` bits off; a zero one traps where it is met.
            Self::NatDiv => {
                operand(0).lowered(literal(1).map_or(0, |divisor| divisor.bits().saturating_sub(1)))
            }
            Self::NatShr => operand(0).lowered(count(1).unwrap_or(0)),
            Self::NatShl => count(1).map_or(Bound::UNBOUNDED, |count| operand(0).raised(count)),
            Self::BytesLen | Self::ListLen => Bound::LENGTH,
            Self::WindowExtent => operand(1).narrower(operand(2)),
            Self::BytesGet => Bound::BYTE,
            Self::NatEql
            | Self::NatNeq
            | Self::NatLt
            | Self::NatLe
            | Self::NatEqz
            | Self::IntEql
            | Self::IntNeq
            | Self::IntLt
            | Self::IntLe
            | Self::IntEqz
            | Self::FltEql
            | Self::FltNeq
            | Self::FltLt
            | Self::FltLe
            | Self::BytesEql
            | Self::IsImmediate
            | Self::IsRow(_) => Bound::BIT,
            _ => Bound::UNBOUNDED,
        }
    }

    /// The representation this operation produces.
    pub fn result_repr(&self) -> Repr {
        match self {
            // Every comparison and predicate answers a `Bool`, whose carrier is a machine word.
            Self::NatEql
            | Self::NatNeq
            | Self::NatLt
            | Self::NatLe
            | Self::NatEqz
            | Self::IntEql
            | Self::IntNeq
            | Self::IntLt
            | Self::IntLe
            | Self::IntEqz
            | Self::FltEql
            | Self::FltNeq
            | Self::FltLt
            | Self::FltLe
            | Self::BytesEql => Repr::Nat,

            // Every `Nat` or `Int` an operation computes is a reference, since no operation bounds its result's size in general: a length and a window's checked extent included, which a sequence past the i31 would leave.
            Self::NatAdd
            | Self::NatSub
            | Self::NatMul
            | Self::NatDiv
            | Self::NatRem
            | Self::NatAnd
            | Self::NatOr
            | Self::NatXor
            | Self::NatMin
            | Self::NatMax
            | Self::NatShl
            | Self::NatPow
            | Self::NatShr
            | Self::IntToNat
            | Self::FltToNat
            | Self::IntAdd
            | Self::IntSub
            | Self::IntMul
            | Self::IntDiv
            | Self::IntRem
            | Self::IntAnd
            | Self::IntOr
            | Self::IntXor
            | Self::IntMin
            | Self::IntMax
            | Self::IntShl
            | Self::IntShr
            | Self::NatToInt
            | Self::IntAbs
            | Self::IntSign
            | Self::FltToInt
            | Self::FltMantissa
            | Self::FltExponent
            | Self::BytesLen
            | Self::ListLen
            | Self::WindowExtent => Repr::Ref,

            Self::FltAdd(_)
            | Self::FltSub(_)
            | Self::FltMul(_)
            | Self::FltDiv(_)
            | Self::FltFma(_)
            | Self::FltRem
            | Self::FltMin
            | Self::FltMax
            | Self::FltNeg
            | Self::FltAbs
            | Self::FltSqrt(_)
            | Self::FltRoundIntegral(_)
            | Self::FltCopysign
            | Self::NatToFlt(_)
            | Self::IntToFlt(_)
            | Self::FltOfLeBytes => Repr::Flt,

            // The two tests join the predicates: each answers a `Bool`, whose carrier is a `Nat`.
            Self::BytesGet | Self::IsImmediate | Self::IsRow(_) => Repr::Nat,
            Self::BytesSlice
            | Self::BytesRest
            | Self::BytesAppend
            | Self::BytesConcat(_)
            | Self::BytesChunk(_)
            | Self::BytesReplicate
            | Self::BytesAnd
            | Self::BytesOr
            | Self::BytesXor => Repr::Bytes,
            Self::FltToLeBytes => Repr::Bytes,
            Self::ListSlice
            | Self::ListRest
            | Self::ListAppend
            | Self::ListConcat(_)
            | Self::ListSettle
            | Self::ListFlat(_) => Repr::List,
            // A list read, a tuple or variant projection and an immediate arm's payload all yield whatever was stored, uninterpreted.
            Self::ListGet | Self::TupleGet(_) | Self::RowGet(..) | Self::ImmediateGet => Repr::Ref,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntrinsicEffect {
    Total,
    MayTrap,
    Allocates,
}

impl Intrinsic {
    /// How many operands this operation takes, which the arena verifier checks every emitted call against.
    ///
    /// **Exhaustive on purpose**, for [`Intrinsic::effect`]'s reason one accessor over. A wildcard defaulting to `2` would let a row taking any other count inherit it silently — the verifier would catch it, but at the far end of a lowering rather than at the definition, reporting a shape mismatch against a node whose author was never asked the question. A binary majority is what makes the default tempting and is exactly why it is wrong: the common case is the one nobody checks.
    pub fn arity(self) -> usize {
        match self {
            Self::NatEqz
            | Self::NatToInt
            | Self::IntAbs
            | Self::IntSign
            | Self::NatToFlt(_)
            | Self::IntEqz
            | Self::IntToNat
            | Self::IntToFlt(_)
            | Self::FltNeg
            | Self::FltAbs
            | Self::FltSqrt(_)
            | Self::FltRoundIntegral(_)
            | Self::FltToNat
            | Self::FltToLeBytes
            | Self::FltOfLeBytes
            | Self::FltToInt
            | Self::FltMantissa
            | Self::FltExponent
            | Self::BytesLen
            | Self::ListLen
            | Self::TupleGet(_)
            | Self::RowGet(..)
            | Self::IsImmediate
            | Self::ImmediateGet
            | Self::IsRow(_)
            | Self::ListSettle => 1,
            Self::NatEql
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
            | Self::NatMin
            | Self::NatMax
            | Self::NatShl
            | Self::NatPow
            | Self::NatShr
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
            | Self::IntMin
            | Self::IntMax
            | Self::IntShl
            | Self::IntShr
            | Self::FltAdd(_)
            | Self::FltSub(_)
            | Self::FltMul(_)
            | Self::FltDiv(_)
            | Self::FltRem
            | Self::FltEql
            | Self::FltNeq
            | Self::FltLt
            | Self::FltLe
            | Self::FltMin
            | Self::FltMax
            | Self::FltCopysign
            | Self::BytesEql
            | Self::BytesGet
            | Self::BytesRest
            | Self::BytesAppend
            | Self::BytesReplicate
            | Self::BytesAnd
            | Self::BytesOr
            | Self::BytesXor
            | Self::ListGet
            | Self::ListRest
            | Self::ListAppend => 2,
            Self::BytesSlice | Self::ListSlice | Self::WindowExtent | Self::FltFma(_) => 3,
            Self::BytesConcat(arity)
            | Self::ListConcat(arity)
            | Self::BytesChunk(arity)
            | Self::ListFlat(arity) => arity,
        }
    }

    /// What this operation does beyond producing its result, *as emitted* — which is not what it means in the language.
    ///
    /// The `MayTrap` set is what is partial in the language — a division, a narrowing of an `Flt` to `Nat` or `Int`, an index and a projection — which `curios-anf`'s `Semantics` says too. `Nat` and `Int` arithmetic is not in it: both are unbounded at run time, so a sum or a product that outgrows the i31 becomes a boxed magnitude rather than a failure, and allocating one is invisible, as an `Flt` box is.
    ///
    /// Exhaustive on purpose: a wildcard defaulting to `Total` would silently classify every guarded operation it missed as deletable — the same hazard the representation table is exhaustive to avoid, one accessor over.
    pub fn effect(self) -> IntrinsicEffect {
        match self {
            // Partial in the language: a zero divisor, a non-finite conversion, an index or a projection out of bounds, a decode of the wrong length.
            Self::NatDiv
            | Self::NatRem
            | Self::IntDiv
            | Self::IntRem
            | Self::FltToNat
            | Self::FltToInt
            | Self::FltMantissa
            | Self::FltExponent
            | Self::FltOfLeBytes
            | Self::BytesGet
            | Self::BytesSlice
            | Self::BytesRest
            | Self::ListGet
            | Self::ListSlice
            | Self::ListRest
            | Self::TupleGet(_)
            | Self::RowGet(..)
            | Self::WindowExtent => IntrinsicEffect::MayTrap,

            // Allocates a *sequence*. An `Flt` result is boxed too, but every `Flt` producer below is treated as total, so the category means a rope or a list rather than any heap traffic at all.
            Self::BytesAppend
            | Self::BytesConcat(_)
            | Self::BytesChunk(_)
            | Self::ListAppend
            | Self::ListConcat(_)
            | Self::ListSettle
            | Self::ListFlat(_)
            | Self::BytesReplicate
            | Self::BytesAnd
            | Self::BytesOr
            | Self::BytesXor
            | Self::FltToLeBytes => IntrinsicEffect::Allocates,

            Self::NatEql
            | Self::NatNeq
            | Self::NatAdd
            | Self::NatSub
            | Self::NatMul
            | Self::NatShl
            | Self::NatPow
            | Self::NatToInt
            | Self::IntAbs
            | Self::IntSign
            | Self::IntAdd
            | Self::IntSub
            | Self::IntMul
            | Self::IntShl
            | Self::IntToNat
            | Self::NatLt
            | Self::NatLe
            | Self::NatAnd
            | Self::NatOr
            | Self::NatXor
            | Self::NatMin
            | Self::NatMax
            | Self::NatShr
            | Self::NatEqz
            | Self::NatToFlt(_)
            | Self::IntEql
            | Self::IntNeq
            | Self::IntLt
            | Self::IntLe
            | Self::IntAnd
            | Self::IntOr
            | Self::IntXor
            | Self::IntMin
            | Self::IntMax
            | Self::IntShr
            | Self::IntEqz
            | Self::IntToFlt(_)
            | Self::FltAdd(_)
            | Self::FltFma(_)
            | Self::FltSub(_)
            | Self::FltMul(_)
            | Self::FltDiv(_)
            | Self::FltRem
            | Self::FltEql
            | Self::FltNeq
            | Self::FltLt
            | Self::FltLe
            | Self::FltMin
            | Self::FltMax
            | Self::FltNeg
            | Self::FltAbs
            | Self::FltSqrt(_)
            | Self::FltRoundIntegral(_)
            | Self::FltCopysign
            | Self::BytesLen
            | Self::BytesEql
            | Self::ListLen
            | Self::IsImmediate
            | Self::ImmediateGet
            | Self::IsRow(_) => IntrinsicEffect::Total,
        }
    }

    pub fn is_total(self) -> bool {
        self.effect() == IntrinsicEffect::Total
    }

    /// Whether this operation's result is a reference it reads out of a value — a row's or a tuple's slot, a list's element, an immediate arm's payload — rather than one it computes. Such a result may be any value at all, a row's struct among them; every other result is a number, a word, a float or a sequence, which no row test answers yes for.
    ///
    /// Exhaustive on purpose, for [`Intrinsic::effect`]'s reason: a wildcard answering no would let a new read pass every row test it reaches as the struct it never is.
    pub fn reads_reference(self) -> bool {
        match self {
            Self::ListGet | Self::TupleGet(_) | Self::RowGet(_, _) | Self::ImmediateGet => true,
            Self::NatEql
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
            | Self::NatMin
            | Self::NatMax
            | Self::NatShl
            | Self::NatPow
            | Self::NatShr
            | Self::NatEqz
            | Self::NatToInt
            | Self::NatToFlt(_)
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
            | Self::IntMin
            | Self::IntMax
            | Self::IntAbs
            | Self::IntSign
            | Self::IntShl
            | Self::IntShr
            | Self::IntEqz
            | Self::IntToNat
            | Self::IntToFlt(_)
            | Self::FltAdd(_)
            | Self::FltSub(_)
            | Self::FltMul(_)
            | Self::FltDiv(_)
            | Self::FltFma(_)
            | Self::FltRem
            | Self::FltEql
            | Self::FltNeq
            | Self::FltLt
            | Self::FltLe
            | Self::FltMin
            | Self::FltMax
            | Self::FltNeg
            | Self::FltAbs
            | Self::FltSqrt(_)
            | Self::FltRoundIntegral(_)
            | Self::FltCopysign
            | Self::FltToNat
            | Self::FltToLeBytes
            | Self::FltOfLeBytes
            | Self::FltToInt
            | Self::FltMantissa
            | Self::FltExponent
            | Self::BytesLen
            | Self::BytesEql
            | Self::BytesGet
            | Self::BytesSlice
            | Self::BytesRest
            | Self::BytesAppend
            | Self::BytesConcat(_)
            | Self::BytesReplicate
            | Self::BytesAnd
            | Self::BytesOr
            | Self::BytesXor
            | Self::BytesChunk(_)
            | Self::ListLen
            | Self::ListSlice
            | Self::ListRest
            | Self::ListAppend
            | Self::ListConcat(_)
            | Self::ListSettle
            | Self::ListFlat(_)
            | Self::WindowExtent
            | Self::IsImmediate
            | Self::IsRow(_) => false,
        }
    }

    pub fn may_trap(self) -> bool {
        self.effect() == IntrinsicEffect::MayTrap
    }

    pub fn allocates(self) -> bool {
        self.effect() == IntrinsicEffect::Allocates
    }

    pub fn is_commutative(self) -> bool {
        matches!(
            self,
            Self::NatEql
                | Self::NatNeq
                | Self::NatAdd
                | Self::NatMul
                | Self::NatAnd
                | Self::NatOr
                | Self::NatXor
                | Self::NatMin
                | Self::NatMax
                | Self::IntEql
                | Self::IntNeq
                | Self::IntAdd
                | Self::IntMul
                | Self::IntAnd
                | Self::IntOr
                | Self::IntXor
                | Self::IntMin
                | Self::IntMax
        )
    }

    /// Whether a dominated duplicate of this op may reuse the dominating result. Every non-allocating op qualifies, `MayTrap` included: the ops are deterministic, and the dominating occurrence has already produced the identical value or already trapped, so the duplicate can neither observe a different result nor trap differently. Allocating ops are excluded to keep each construction's identity, even though nothing observes it.
    pub fn cse_eligible(self) -> bool {
        !self.allocates()
    }
}
