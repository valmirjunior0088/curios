//! The primitive operations: each one's arity and effect, and the representation it reads every operand and produces its result at.

use {
    super::{Atom, Literal, RowId, nat_is_small},
    curios_num::{Grain, Rounding},
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
    NatShl,
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
    BinLen(Grain),
    BinEql(Grain),
    BinGet(Grain),
    BinSlice(Grain),
    /// `(bin, start) -> bin`: the suffix from `start`, whose extent is the value's own — there is no count operand to supply, so the one thing a caller could get wrong about a suffix it cannot say. Every compiler-emitted window is a suffix (`into_cont`'s peel is the only producer), and this is what keeps two lowerings from having to agree about how a count is derived; the derivation happens once, against the rope's own length.
    BinRest(Grain),
    BinAppend(Grain),
    BinConcat(Grain, usize),
    /// `(count, element) -> bin`: one flat leaf of `count` copies of `element`. The size is an operand's value rather than an operand's extent, which is what separates it from every other construction here.
    BinReplicate(Grain),
    /// `(bin) -> bin`: the run read at the other grain, eight bits to the byte. The [`Grain`] is the operand's, so the result is the one it is not — the only row here whose result grain differs from the one it names.
    BinReinterp(Grain),
    /// `(a, b) -> bin`: the element-wise conjunction of two binaries the type has already held to one length. Total — the bound is discharged above erasure and nothing here can fail it.
    BinAnd(Grain),
    /// `(a, b) -> bin`: the element-wise disjunction, as [`Intrinsic::BinAnd`].
    BinOr(Grain),
    /// `(a, b) -> bin`: the element-wise difference, as [`Intrinsic::BinAnd`].
    BinXor(Grain),
    /// `(element…) -> bin`: one flat leaf holding exactly these elements at the given grain — the fused form of an append chain, minted only by the optimizer's `fuse_append_chains` and never by the door. The arity is the element count in grain units; the byte grain stores one element per payload byte, the bit grain packs eight.
    BinChunk(Grain, usize),
    ListLen,
    ListGet,
    ListSlice,
    /// The `List` mirror of [`Intrinsic::BinRest`].
    ListRest,
    ListAppend,
    ListConcat(usize),
    /// `(list) -> list`: the same value, flat — a leaf answers itself, and anything else answers a fresh leaf over its forced payload (an O(1) wrap, since payload arrays are filled once and never rewritten). Semantically the identity; representationally the settle the door inserts on stores into fields the Ersd census marked indexed-only, so the values a program only ever indexes are flat by the time they are stored.
    ListSettle,
    /// `(list…) -> list`: one exact-length flat leaf holding every element of every operand in order — the eager concatenation `fuse_append_chains` builds where the reads that would have paid the gather are already in evidence. Minted only by the optimizer, like [`Intrinsic::BinChunk`].
    ListFlat(usize),
    TupleGet(usize),
    /// `(row) -> value`: slot `index` of a [`ValueExpr::Row`](super::ValueExpr::Row) of `row`. Which slot holds what is the row's to say — a family's slot zero is its tag — and the door is what knows it. Distinct from [`Intrinsic::TupleGet`] so a row read names the row whose final type the emitter casts to exactly, and so a structural projection can never silently read a row value through the roster cascade: the two vocabularies meet only in the verifier, which refuses a mismatch.
    RowGet(RowId, usize),
    /// The virtual-window bounds guard: `(start, count, len) -> count`, trapping unless the window ends inside `len` — the eager trap a physical slice would have performed, kept at the original evaluation point when the slice itself is virtualized away. It answers the count unchanged rather than a difference, because a window is a start and a count everywhere above this too; what it contributes is the trap, not the arithmetic.
    WindowExtent,
    /// Whether the operand is a bare payload — an i31, or the boxed magnitude of a `Nat` or `Int` past it — (1) or a row struct (0): the dispatch of a variant encoding whose one scalar-payload constructor rides bare. The boxed magnitude is its own final type, which no row shares, so admitting it keeps the two answers disjoint for a `Nat` or `Int` payload as for a word. A representation question, which is why it exists in this crate's vocabulary and not in Ersd: the lowering that chose the encoding is the only producer, and it guarantees the two answers are disjoint over every value the test can reach.
    IsImmediate,
    /// `(value) -> value`: the bare payload of the constructor [`Intrinsic::IsImmediate`] just answered for, passed through unchanged.
    ///
    /// Representationally the identity, and that is the whole point: it exists so the payload has a *definition* instead of being aliased to the scrutinee. The representation analysis fixes a value's carrier from whatever produced it, so a payload with no producer of its own carries its uses' raw demand back onto the scrutinee — which on the boxed path is a tuple, not a scalar. That is not a missed optimization but a miscompile: an arm's `NatAdd` demanding the raw carrier would reach the scrutinee's own definition, and the emitter would coerce a `struct.new` with a `ref.cast` to `i31`. Answering `Repr::Ref` makes this definition's offer `Never`, so the demand coerces at the use where it belongs and the scrutinee is never demanded raw.
    ImmediateGet,
}

/// The representation a value is read or produced at — the carrier, not the type.
///
/// This is the vocabulary the backend's coercions translate — `LoadAs` into a carrier and `box_instr` back out of one: `Nat` and `Flt` name raw machine carriers a Wasm register can hold, and the rest name references. Stated here, on the IR, rather than in the emitter, because the *optimizer* has to be able to ask what an operation demands of its operands without running codegen to find out — and because an emitter that restates the demand at every use site is an emitter that can disagree with the analysis.
///
/// **A `Nat` or `Int` is not a machine word.** Either is a reference — an i31 or a boxed magnitude, see [`ENVELOPE_BITS`](super::ENVELOPE_BITS) — so an operation on them reads [`Repr::Number`] and produces `Repr::Ref`; the word is what a `Bool`, a byte, a bit and a tag are, and every word lies below `2³⁰`, so it boxes to a `Nat` as the i31 it already is. The exception is a `Nat` its literal operands bound below `2³⁰`, which [`Intrinsic::bounds_result`] names and a word may hold. A length is not a word, since a sequence may outgrow the i31, and is produced as the `Nat` it is. A `Nat` becomes a word where a position, a count or a key is asked for, exact below `2³² - 1` and saturating there, so an index no sequence can reach fails the bounds check it meets.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Repr {
    /// A raw unsigned 32-bit machine word.
    Nat,
    /// A raw binary64 carrier.
    Flt,
    /// A `Nat` or `Int` operand: a reference in general, and the word it is where the representation analysis holds one there — a small literal, a bounded result, or a parameter every argument reaching it is one of those. A demand, never a carrier: no value is held at it, and a use reading it tests a reference before unboxing and takes a word as it is.
    Number,
    /// A packed-binary reference at the given grain: a small-canonical immediate or a rope. The grain rides the carrier because the two immediate layouts share no runtime discrimination — only the static type keeps them apart, so the coercion tables must be unable to confuse them.
    Bin(Grain),
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
            (
                Self::BinGet(grain)
                | Self::BinSlice(grain)
                | Self::BinRest(grain)
                | Self::BinAppend(grain),
                0,
            ) => Repr::Bin(*grain),
            (Self::BinGet(_) | Self::BinSlice(_) | Self::BinRest(_) | Self::BinAppend(_), _) => {
                Repr::Nat
            }
            // Both operands of a pointwise combination are ropes, which is what separates it from every other sequence row here — there is no position among them.
            (Self::BinAnd(grain) | Self::BinOr(grain) | Self::BinXor(grain), _) => {
                Repr::Bin(*grain)
            }
            // A fill takes a count and a generator, and neither is a rope: the element rides the `Nat` grain as an append's does.
            (Self::BinReplicate(_), _) => Repr::Nat,
            (Self::BinReinterp(grain), _) => Repr::Bin(*grain),
            (Self::ListGet | Self::ListSlice | Self::ListRest | Self::ListAppend, 0) => Repr::List,
            (Self::ListGet | Self::ListSlice | Self::ListRest, _) => Repr::Nat,
            // A chunk element is one packed byte, carried at the `Nat` grain like an append's.
            (Self::BinChunk(_, _), _) => Repr::Nat,
            // A list element is carried, never interpreted — unlike a `Bytes` element, which is a `Nat` grain.
            (Self::ListAppend, _) => Repr::Ref,
            (Self::BinConcat(grain, _), _) | (Self::BinEql(grain) | Self::BinLen(grain), _) => {
                Repr::Bin(*grain)
            }
            (Self::FltOfLeBytes, _) => Repr::Bin(Grain::X),
            (Self::WindowExtent, _) => Repr::Nat,
            // The whole point of the test is to look at the reference uncoerced, and the read that follows it hands that same reference on.
            (Self::IsImmediate | Self::ImmediateGet, _) => Repr::Ref,
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
                | Self::NatShl
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

    /// Whether this operation's result may be a `Nat` its literal operands bound below `2³⁰`: a remainder by a small divisor, a conjunction with a small mask, and a monus, quotient or right shift of a small dividend. [`Intrinsic::bounds_result`] reads the literals; this is the operation half, which the emitter checks a word-held result against.
    pub fn may_bound_result(self) -> bool {
        matches!(
            self,
            Intrinsic::NatRem
                | Intrinsic::NatAnd
                | Intrinsic::NatSub
                | Intrinsic::NatDiv
                | Intrinsic::NatShr
        )
    }

    /// Whether this operation over `args` produces a `Nat` below `2³⁰` whatever its other operands are, so the result may ride a machine word rather than the reference [`Intrinsic::result_repr`] names. The bound is read off a small literal: `x % k` and `x & k` lie below `k`, and `k - x`, `k / x` and `k >> x` at or below it.
    pub fn bounds_result(&self, args: &[Atom]) -> bool {
        let small =
            |atom: &Atom| matches!(atom, Atom::Literal(Literal::Nat(value)) if nat_is_small(value));

        self.may_bound_result()
            && match (self, args) {
                (Intrinsic::NatRem, [_, divisor]) => small(divisor),
                (Intrinsic::NatAnd, [left, right]) => small(left) || small(right),
                (Intrinsic::NatSub | Intrinsic::NatDiv | Intrinsic::NatShr, [value, _]) => {
                    small(value)
                }
                _ => false,
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
            | Self::BinEql(_) => Repr::Nat,

            // Every `Nat` or `Int` an operation computes is a reference, since no operation bounds its result's size in general: a length and a window's checked extent included, which a sequence past the i31 would leave.
            Self::NatAdd
            | Self::NatSub
            | Self::NatMul
            | Self::NatDiv
            | Self::NatRem
            | Self::NatAnd
            | Self::NatOr
            | Self::NatXor
            | Self::NatShl
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
            | Self::IntShl
            | Self::IntShr
            | Self::NatToInt
            | Self::FltToInt
            | Self::FltMantissa
            | Self::FltExponent
            | Self::BinLen(_)
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

            // `IsImmediate` joins the predicates: it answers a `Bool`, whose carrier is a `Nat`.
            Self::BinGet(_) | Self::IsImmediate => Repr::Nat,
            Self::BinSlice(grain)
            | Self::BinRest(grain)
            | Self::BinAppend(grain)
            | Self::BinConcat(grain, _)
            | Self::BinChunk(grain, _)
            | Self::BinReplicate(grain)
            | Self::BinAnd(grain)
            | Self::BinOr(grain)
            | Self::BinXor(grain) => Repr::Bin(*grain),
            // The one row whose result grain is not the one it names: the grain is the operand's, and reading it at the other is the whole operation.
            Self::BinReinterp(grain) => Repr::Bin(grain.other()),
            Self::FltToLeBytes => Repr::Bin(Grain::X),
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
            | Self::BinLen(_)
            | Self::BinReinterp(_)
            | Self::ListLen
            | Self::TupleGet(_)
            | Self::RowGet(..)
            | Self::IsImmediate
            | Self::ImmediateGet
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
            | Self::NatShl
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
            | Self::BinEql(_)
            | Self::BinGet(_)
            | Self::BinRest(_)
            | Self::BinAppend(_)
            | Self::BinReplicate(_)
            | Self::BinAnd(_)
            | Self::BinOr(_)
            | Self::BinXor(_)
            | Self::ListGet
            | Self::ListRest
            | Self::ListAppend => 2,
            Self::BinSlice(_) | Self::ListSlice | Self::WindowExtent | Self::FltFma(_) => 3,
            Self::BinConcat(_, arity)
            | Self::ListConcat(arity)
            | Self::BinChunk(_, arity)
            | Self::ListFlat(arity) => arity,
        }
    }

    /// What this operation does beyond producing its result, *as emitted* — which is not what it means in the language.
    ///
    /// The `MayTrap` set is what is partial in the language — a division, a narrowing of an `Flt` to `Nat` or `Int`, an index and a projection — which `curios-ersd`'s `Semantics` says too. `Nat` and `Int` arithmetic is not in it: both are unbounded at run time, so a sum or a product that outgrows the i31 becomes a boxed magnitude rather than a failure, and allocating one is invisible, as an `Flt` box is.
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
            | Self::BinGet(_)
            | Self::BinSlice(_)
            | Self::BinRest(_)
            | Self::ListGet
            | Self::ListSlice
            | Self::ListRest
            | Self::TupleGet(_)
            | Self::RowGet(..)
            | Self::WindowExtent => IntrinsicEffect::MayTrap,

            // Allocates a *sequence*. An `Flt` result is boxed too, but every `Flt` producer below is treated as total, so the category means a rope or a list rather than any heap traffic at all.
            Self::BinAppend(_)
            | Self::BinConcat(_, _)
            | Self::BinChunk(_, _)
            | Self::ListAppend
            | Self::ListConcat(_)
            | Self::ListSettle
            | Self::ListFlat(_)
            | Self::BinReplicate(_)
            | Self::BinReinterp(_)
            | Self::BinAnd(_)
            | Self::BinOr(_)
            | Self::BinXor(_)
            | Self::FltToLeBytes => IntrinsicEffect::Allocates,

            Self::NatEql
            | Self::NatNeq
            | Self::NatAdd
            | Self::NatSub
            | Self::NatMul
            | Self::NatShl
            | Self::NatToInt
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
            | Self::BinLen(_)
            | Self::BinEql(_)
            | Self::ListLen
            | Self::IsImmediate
            | Self::ImmediateGet => IntrinsicEffect::Total,
        }
    }

    pub fn is_total(self) -> bool {
        self.effect() == IntrinsicEffect::Total
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
                | Self::IntEql
                | Self::IntNeq
                | Self::IntAdd
                | Self::IntMul
                | Self::IntAnd
                | Self::IntOr
                | Self::IntXor
        )
    }

    /// Whether a dominated duplicate of this op may reuse the dominating result. Every non-allocating op qualifies, `MayTrap` included: the ops are deterministic, and the dominating occurrence has already produced the identical value or already trapped, so the duplicate can neither observe a different result nor trap differently. Allocating ops are excluded to keep each construction's identity, even though nothing observes it.
    pub fn cse_eligible(self) -> bool {
        !self.allocates()
    }
}
