//! Arena-backed high CPS.
//!
//! The surface of this module is intentionally small: Ersd lowering constructs a [`Module`], the optimizer mutates that graph through its checked mutation API, and `curios-emit`'s lowering to WebAssembly consumes it. Stable integer identities, tombstoned arena entries, and deterministic traversal are representation invariants rather than optimizer conventions. Use information is derived on demand (see [`Module::value_use_counts`]) rather than maintained as a shadow arena.

use {
    curios_abi::ForeignFunction,
    curios_num::{Binary, Floating, Grain, Integer, Natural, Rounding},
    curios_utilities::{Arena, ArenaId, id},
    std::{
        collections::{BTreeMap, BTreeSet},
        fmt,
        sync::Arc,
    },
};

/// How many value bits a small `Nat` or `Int` holds — the width of an `i31ref`'s payload, read signed, and the one width in the whole pipeline that is a fact about the target rather than about the language.
///
/// A `Nat` and an `Int` share one runtime form, and it is a reference: an i31 when the value lies in `[-2³⁰, 2³⁰)`, and a boxed magnitude otherwise, never both. This width is where the two meet, so it bounds the fast path every arithmetic lowering keeps inline and nothing else — no value refuses at it. Above this crate nothing knows the number: `curios-core` computes unbounded and `curios-ersd`'s constants carry whatever the theory produced.
pub const ENVELOPE_BITS: i32 = 31;

/// Whether `value` is a `Nat` the i31 holds: below `2³⁰`, since the i31 is read signed for both carriers.
///
/// Stated here beside the width because two readers ask it — the emitter, which spells a small constant as an i31 and any other as a boxed magnitude, and the representation analysis, which lets only a small literal ride a machine word.
pub fn nat_is_small(value: &Natural) -> bool {
    u32::try_from(value)
        .ok()
        .is_some_and(|value| value >> (ENVELOPE_BITS - 1) == 0)
}

/// Whether `value` is an `Int` the i31 holds: in range exactly when the bit below the sign agrees with it.
pub fn int_is_small(value: &Integer) -> bool {
    i32::try_from(value)
        .ok()
        .is_some_and(|value| value >> (ENVELOPE_BITS - 1) == value >> ENVELOPE_BITS)
}

// Sigils follow the naming scheme shared with `curios-ersd` and `curios-wasm` — see `documentation/design/toolchain/one-naming-scheme-for-compiler-identities.md`.
id!(NodeId, "~n");
id!(ValueId, "~v");
id!(FunctionId, "~f");
id!(ContinuationId, "~k");
id!(RowId, "~r");

impl FunctionId {
    pub fn from_index(index: usize) -> Self {
        Self(index as u32)
    }
}

/// A literal operand. `Flt` holds the bitwise [`Floating`] rather than an `f64` so that the derived equality is identity on the bit pattern: under IEEE equality a NaN literal is unequal to itself, and a pass comparing an edge it rebuilt against the edge it read would report a change on every round — `forward_continuations` did exactly that, and the fixpoint ran to its backstop on any module carrying a `NaN` through a jump.
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Nat(Natural),
    Int(Integer),
    Flt(Floating),
    Bin(Grain, Binary),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Value(ValueId),
    Fun(FunctionId),
    Literal(Literal),
    /// No value: a reference slot belonging to a wider constructor than the edge or call carrying it fills. It travels as null, which is what the field the constructor never wrote holds, and every reference position admits it; a register slot is padded with its zero literal instead, since a register has no null and the field holds zero there. [`Module::pad`] is the one place that chooses.
    ///
    /// The register the *parameter* is held at is decided by `represent` during backend lowering, from the uses of the parameter it feeds — strictly after the passes that create fillers — so a filler reaching a register-held parameter is the emitter's to materialise as that register's zero.
    Filler,
}

#[derive(Debug, Clone)]
pub enum ValueExpr {
    Literal(Literal),
    List(Vec<Atom>),
    Tuple(Vec<Atom>),
    /// A construction of a *nominal* row — a variant family or a product schema — at that row's full width, padded with [`Atom::Filler`] wherever the constructor building it is narrower than the row. A family's slot zero is its tag; a product has none. The Ersd door is the only mint and pads every construction, so a row value's arity is a fact of the row rather than of the site that built it — which is what lets the emitter key one final heap type per row and read it with an exact cast instead of the structural roster cascade.
    Row(RowId, Vec<Atom>),
}

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
    /// `(row) -> value`: slot `index` of a [`ValueExpr::Row`] of `row`. Which slot holds what is the row's to say — a family's slot zero is its tag — and the door is what knows it. Distinct from [`Intrinsic::TupleGet`] so a row read names the row whose final type the emitter casts to exactly, and so a structural projection can never silently read a row value through the roster cascade: the two vocabularies meet only in the verifier, which refuses a mismatch.
    RowGet(RowId, usize),
    /// The virtual-window bounds guard: `(start, count, len) -> count`, trapping unless the window ends inside `len` — the eager trap a physical slice would have performed, kept at the original evaluation point when the slice itself is virtualized away. It answers the count unchanged rather than a difference, because a window is a start and a count everywhere above this too; what it contributes is the trap, not the arithmetic.
    WindowExtent,
    /// Whether the operand is a bare payload — an i31, or the boxed magnitude of a `Nat` or `Int` past it — (1) or a row struct (0): the dispatch of a variant encoding whose one scalar-payload constructor rides bare. The boxed magnitude is its own final type, which no row shares, so admitting it keeps the two answers disjoint for a `Nat` or `Int` payload as for a word. A representation question, which is why it exists in this crate's vocabulary and not in Ersd: the lowering that chose the encoding is the only producer, and it guarantees the two answers are disjoint over every value the test can reach.
    IsImmediate,
    /// `(value) -> value`: the bare payload of the constructor [`Intrinsic::IsImmediate`] just answered for, passed through unchanged.
    ///
    /// Representationally the identity, and that is the whole point: it exists so the payload has a *definition* instead of being aliased to the scrutinee. The representation analysis fixes a value's carrier from whatever produced it, so a payload with no producer of its own carries its uses' raw demand back onto the scrutinee — which on the boxed path is a tuple, not a scalar. That is not a missed optimization but a miscompile: an arm's `NatAdd` demanded the raw carrier, the demand reached the scrutinee's own definition, and the emitter coerced a `struct.new` with a `ref.cast` to `i31`. Answering `Repr::Ref` makes this definition's offer `Never`, so the demand coerces at the use where it belongs and the scrutinee is never demanded raw.
    ImmediateGet,
}

/// The representation a value is read or produced at — the carrier, not the type.
///
/// This is the vocabulary the backend's `LoadAs`/`WrapAs` coercions translate: `Nat` and `Flt` name raw machine carriers a Wasm register can hold, and the rest name references. Stated here, on the IR, rather than in the emitter, because the *optimizer* has to be able to ask what an operation demands of its operands without running codegen to find out — and because an emitter that restates the demand at every use site is an emitter that can disagree with the analysis.
///
/// **A `Nat` or `Int` is not a machine word.** Either is a reference — an i31 or a boxed magnitude, see [`ENVELOPE_BITS`] — so an operation on them reads [`Repr::Number`] and produces `Repr::Ref`; the word is what a `Bool`, a byte, a bit and a tag are, and every word lies below `2³⁰`, so it boxes to a `Nat` as the i31 it already is. The exception is a `Nat` its literal operands bound below `2³⁰`, which [`Intrinsic::bounds_result`] names and a word may hold. A length is not a word, since a sequence may outgrow the i31, and is produced as the `Nat` it is. A `Nat` becomes a word where a position, a count or a key is asked for, exact below `2³² - 1` and saturating there, so an index no sequence can reach fails the bounds check it meets.
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
        use Intrinsic::*;

        match (self, index) {
            // The sequence operations are the only ones whose operands differ from one another: a rope first, then positions.
            (BinGet(grain) | BinSlice(grain) | BinRest(grain) | BinAppend(grain), 0) => {
                Repr::Bin(*grain)
            }
            (BinGet(_) | BinSlice(_) | BinRest(_) | BinAppend(_), _) => Repr::Nat,
            // Both operands of a pointwise combination are ropes, which is what separates it from every other sequence row here — there is no position among them.
            (BinAnd(grain) | BinOr(grain) | BinXor(grain), _) => Repr::Bin(*grain),
            // A fill takes a count and a generator, and neither is a rope: the element rides the `Nat` grain as an append's does.
            (BinReplicate(_), _) => Repr::Nat,
            (BinReinterp(grain), _) => Repr::Bin(*grain),
            (ListGet | ListSlice | ListRest | ListAppend, 0) => Repr::List,
            (ListGet | ListSlice | ListRest, _) => Repr::Nat,
            // A chunk element is one packed byte, carried at the `Nat` grain like an append's.
            (BinChunk(_, _), _) => Repr::Nat,
            // A list element is carried, never interpreted — unlike a `Bytes` element, which is a `Nat` grain.
            (ListAppend, _) => Repr::Ref,
            (BinConcat(grain, _), _) | (BinEql(grain) | BinLen(grain), _) => Repr::Bin(*grain),
            (FltOfLeBytes, _) => Repr::Bin(Grain::X),
            (WindowExtent, _) => Repr::Nat,
            // The whole point of the test is to look at the reference uncoerced, and the read that follows it hands that same reference on.
            (IsImmediate | ImmediateGet, _) => Repr::Ref,
            (ListConcat(_) | ListLen | ListSettle | ListFlat(_), _) => Repr::List,
            (TupleGet(_) | RowGet(..), _) => Repr::Ref,

            // A `Nat` or `Int` is a reference whatever its size — an i31 or a boxed magnitude — and each lowering takes it apart itself, the small case inline, or takes the word it already is; a shift count is such a `Nat` too.
            (
                NatEql | NatNeq | NatAdd | NatSub | NatMul | NatLt | NatDiv | NatRem | NatLe
                | NatAnd | NatOr | NatXor | NatShl | NatShr | NatEqz | NatToInt | NatToFlt(_)
                | IntEql | IntNeq | IntAdd | IntSub | IntMul | IntDiv | IntRem | IntLt | IntLe
                | IntAnd | IntOr | IntXor | IntShl | IntShr | IntEqz | IntToNat | IntToFlt(_),
                _,
            ) => Repr::Number,

            (
                FltAdd(_) | FltSub(_) | FltMul(_) | FltDiv(_) | FltFma(_) | FltRem | FltEql
                | FltNeq | FltLt | FltLe | FltMin | FltMax | FltNeg | FltAbs | FltSqrt(_)
                | FltRoundIntegral(_) | FltCopysign | FltToNat | FltToLeBytes | FltToInt
                | FltMantissa | FltExponent,
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
        use Intrinsic::*;

        match self {
            // Every comparison and predicate answers a `Bool`, whose carrier is a machine word.
            NatEql | NatNeq | NatLt | NatLe | NatEqz | IntEql | IntNeq | IntLt | IntLe | IntEqz
            | FltEql | FltNeq | FltLt | FltLe | BinEql(_) => Repr::Nat,

            // Every `Nat` or `Int` an operation computes is a reference, since no operation bounds its result's size in general: a length and a window's checked extent included, which a sequence past the i31 would leave.
            NatAdd | NatSub | NatMul | NatDiv | NatRem | NatAnd | NatOr | NatXor | NatShl
            | NatShr | IntToNat | FltToNat | IntAdd | IntSub | IntMul | IntDiv | IntRem
            | IntAnd | IntOr | IntXor | IntShl | IntShr | NatToInt | FltToInt | FltMantissa
            | FltExponent | BinLen(_) | ListLen | WindowExtent => Repr::Ref,

            FltAdd(_) | FltSub(_) | FltMul(_) | FltDiv(_) | FltFma(_) | FltRem | FltMin
            | FltMax | FltNeg | FltAbs | FltSqrt(_) | FltRoundIntegral(_) | FltCopysign
            | NatToFlt(_) | IntToFlt(_) | FltOfLeBytes => Repr::Flt,

            // `IsImmediate` joins the predicates: it answers a `Bool`, whose carrier is a `Nat`.
            BinGet(_) | IsImmediate => Repr::Nat,
            BinSlice(grain)
            | BinRest(grain)
            | BinAppend(grain)
            | BinConcat(grain, _)
            | BinChunk(grain, _)
            | BinReplicate(grain)
            | BinAnd(grain)
            | BinOr(grain)
            | BinXor(grain) => Repr::Bin(*grain),
            // The one row whose result grain is not the one it names: the grain is the operand's, and reading it at the other is the whole operation.
            BinReinterp(grain) => Repr::Bin(grain.other()),
            FltToLeBytes => Repr::Bin(Grain::X),
            ListSlice | ListRest | ListAppend | ListConcat(_) | ListSettle | ListFlat(_) => {
                Repr::List
            }
            // A list read, a tuple or variant projection and an immediate arm's payload all yield whatever was stored, uninterpreted.
            ListGet | TupleGet(_) | RowGet(..) | ImmediateGet => Repr::Ref,
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
    /// **Exhaustive on purpose**, for [`Intrinsic::effect`]'s reason one accessor over. This was a wildcard defaulting to `2`, and a row that took any other count inherited it silently — the verifier caught it, but at the far end of a lowering rather than at the definition, reporting a shape mismatch against a node whose author had never been asked the question. A binary majority is what made the default tempting and is exactly why it was wrong: the common case is the one nobody checks.
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
    /// Exhaustive on purpose. This was a wildcard defaulting to `Total`, which silently classified seven guarded operations as deletable — the same hazard the representation table is exhaustive to avoid, one accessor over.
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

    /// Whether a dominated duplicate of this op may reuse the dominating result. Every non-allocating op qualifies, `MayTrap` included: the ops are deterministic, and the dominating occurrence has already produced the identical value or already trapped, so the duplicate can neither observe a different result nor trap differently. Allocating ops are excluded to keep each construction's identity, even though nothing observes it today.
    pub fn cse_eligible(self) -> bool {
        !self.allocates()
    }
}

#[derive(Debug, Clone)]
pub enum Callee {
    Known(FunctionId),
    Closure(ValueId),
}

#[derive(Debug, Clone)]
pub struct Edge {
    pub target: ContinuationId,
    pub args: Vec<Atom>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CellOp {
    New,
    /// A cell allocated *empty*, to be filled by a later `Set`: what ties a recursive knot, whose members' cells must exist before any initializer runs and hold nothing meaningful until their own has. Reading one before its fill traps, which is the point — a knot read out of order once computed with the placeholder `New` had been handed, and `Get`'s emission already refuses a null for free. Nothing a program writes mints one; only the erased lowering does.
    Reserve,
    Set,
    Get,
}

impl CellOp {
    pub fn operand_arity(self) -> usize {
        match self {
            Self::Reserve => 0,
            Self::New | Self::Get => 1,
            Self::Set => 2,
        }
    }

    pub fn result_arity(self) -> usize {
        match self {
            Self::New | Self::Reserve | Self::Get => 1,
            Self::Set => 0,
        }
    }
}

/// A call-like intrinsic. `ListMap` takes the list then the mapper — the carrier-first order of the whole sequence row, matched by the erased representation so the lowering transcribes without reordering — and runs the mapper once per element, in order.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntrinsicCall {
    ListMap,
}

#[derive(Debug, Clone)]
pub enum Node {
    LetValue {
        result: ValueId,
        value: ValueExpr,
        next: NodeId,
    },
    LetIntrinsic {
        result: ValueId,
        op: Intrinsic,
        args: Vec<Atom>,
        next: NodeId,
    },
    LetFun {
        functions: Vec<FunctionId>,
        body: NodeId,
    },
    LetCont {
        continuations: Vec<ContinuationId>,
        body: NodeId,
    },
    ApplyFun {
        callee: Callee,
        args: Vec<Atom>,
        return_to: ContinuationId,
    },
    ApplyCont(Edge),
    Switch {
        scrutinee: Atom,
        cases: BTreeMap<u32, Edge>,
        default: Option<Edge>,
    },
    Foreign {
        function: Arc<ForeignFunction>,
        args: Vec<Atom>,
        return_to: ContinuationId,
    },
    Cell {
        op: CellOp,
        args: Vec<Atom>,
        return_to: ContinuationId,
    },
    Intrinsic {
        op: IntrinsicCall,
        args: Vec<Atom>,
        return_to: ContinuationId,
    },
    Exit {
        value: Option<Atom>,
    },
    /// A deliberate runtime failure of the given class: the block ends by reporting it and never continues. A lowering seats one where the program can reach a state it has to refuse — today the knot's forcing state, a member read while its own initializer runs — and the emitter renders every class as its sentence through the `sys.panic` import. Distinct from [`Node::Unreachable`], which marks an arm the theory proved impossible: reaching a `Panic` is the program's doing, reaching an `Unreachable` is the compiler's.
    Panic(Panic),
    /// An arm the theory proved impossible. Never reached by a sound compilation; the emitter renders it as [`Panic::Invariant`]'s sentence so that a compiler bug says so.
    Unreachable,
}

/// The classes of failure a compiled program can stop with, each rendered by the emitter as one sentence naming the rule, the carrier and the remedy. A `Node::Panic` carries one; the emitter's own checks — a narrowing to the host wire, a read past the end, a `Flt` decode — reach for the same classes as instruction sequences, since they are decided while lowering an intrinsic rather than as nodes. The sentences themselves are the emitter's (`curios-emit`'s `into_wasm/refusal.rs`), so what the IR states is the vocabulary and what the emitter states is the text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Panic {
    /// A `Nat` argument to a host function the wire's `i32` cannot carry. The one place a `Nat` is narrowed by refusing: everywhere inside the program it is unbounded.
    NatWire,
    /// An `Int` argument to a host function the wire's `i32` cannot carry.
    IntWire,
    /// A packed or list read, or a window, past the end of its value.
    OutOfBounds,
    /// A `Flt` decoded from a byte string that is not eight bytes long.
    FltDecode,
    /// A recursive value read while its own initializer is still running — a cycle the eager verifier could not see through a closure, met by forcing.
    Cycle,
    /// An arm the theory proved impossible was taken: a compiler bug, never the program's.
    Invariant,
}

impl Panic {
    /// Every class, in declaration order: the order the emitter writes the refusal helpers a module reaches.
    pub const ALL: [Panic; 6] = [
        Panic::NatWire,
        Panic::IntWire,
        Panic::OutOfBounds,
        Panic::FltDecode,
        Panic::Cycle,
        Panic::Invariant,
    ];
}

impl fmt::Display for Panic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Panic::NatWire => "nat_wire",
            Panic::IntWire => "int_wire",
            Panic::OutOfBounds => "bounds",
            Panic::FltDecode => "flt",
            Panic::Cycle => "cycle",
            Panic::Invariant => "invariant",
        })
    }
}

#[derive(Debug, Clone)]
pub struct ValueDef {
    pub debug_name: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub debug_name: Option<String>,
    pub params: Vec<ValueId>,
    pub return_cont: ContinuationId,
    pub body: NodeId,
    /// Whether a call of this function whose result nothing reads may simply not happen: it terminates, and running it a second time — or not at all — is not an event a program can observe.
    ///
    /// **A conclusion, not a fact this stage could reach.** Termination is the size-change engine's, decided above Core and carried down; freedom from effects is the erased stage's interprocedural summary. Both are in hand at the lowering that builds this, and only their conjunction crosses, so nothing here needs a lattice or a notion of purity of its own. A trap or a divergence is *not* excluded by it — an occurrence that already ran is what licenses dropping a later one, and a dead call has no dominating occurrence, so this must mean total.
    ///
    /// False is the safe reading and the default: a function this could not be filled from is kept.
    pub droppable: bool,
}

#[derive(Debug, Clone)]
pub struct Continuation {
    pub debug_name: Option<String>,
    pub params: Vec<ValueId>,
    pub body: NodeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum UseTarget {
    Value(ValueId),
    Fun(FunctionId),
    Cont(ContinuationId),
}

/// What the module's functions state about returning: which continuation is whose sentinel, and how many values each hands back.
///
/// The two travel together because every arity question needs both — whether a transfer is a return at all, and how wide a return is — so they are one parameter rather than two threaded in parallel through the verifier.
struct ReturnFacts<'a> {
    owners: &'a BTreeMap<ContinuationId, FunctionId>,
    arities: &'a BTreeMap<FunctionId, usize>,
}

impl ReturnFacts<'_> {
    /// How many values `function` returns, reading absence as the single value a function carried before any protocol widened it.
    fn arity(&self, function: FunctionId) -> usize {
        self.arities.get(&function).copied().unwrap_or(1)
    }
}

#[derive(Debug, Clone)]
pub struct VerifyError(pub String);

impl fmt::Display for VerifyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for VerifyError {}

/// The recorded fields representation: `width` consecutive parameters of a continuation, starting at `start`, that *are* the fields of one former aggregate parameter.
///
/// The record is what makes a split a fact of the program rather than a convention between passes: [`Module::verify`] holds every group to its continuation's parameter list the way it already holds arities, so a pass that reshapes a recorded parameter list without maintaining the record fails loudly instead of silently disagreeing with the split.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FieldGroup {
    pub start: usize,
    pub width: usize,
}

/// A value a node binds, bound here and nowhere else; `noun` names it in the duplicate-binding message.
struct ScopeBinding {
    value: ValueId,
    noun: &'static str,
}

/// A pending region in a walk over lexical structure, carrying the scope that region sees.
enum ScopeTask {
    Function {
        function: FunctionId,
        values: BTreeSet<ValueId>,
        functions: BTreeSet<FunctionId>,
    },
    Node {
        /// The function the node belongs to. It names the region in a verification message and is needed for nothing else, so a walk that reports nothing has none.
        owner: Option<FunctionId>,
        node: NodeId,
        values: BTreeSet<ValueId>,
        functions: BTreeSet<FunctionId>,
        continuations: BTreeSet<ContinuationId>,
    },
}

/// What one region contributes to a lexical walk: the names it binds, and the regions below it.
#[derive(Default)]
struct ScopeStep {
    values: Vec<ScopeBinding>,
    functions: Vec<FunctionId>,
    tasks: Vec<ScopeTask>,
}

type NodeTask = (
    Option<FunctionId>,
    NodeId,
    BTreeSet<ValueId>,
    BTreeSet<FunctionId>,
    BTreeSet<ContinuationId>,
);

/// The bookkeeping a lexical *verification* walk carries on top of the scope rules: which names have been bound, and which regions are still to visit.
#[derive(Default)]
struct ScopeVerifier {
    bound_functions: BTreeSet<FunctionId>,
    bound_values: BTreeSet<ValueId>,
    function_work: Vec<(FunctionId, BTreeSet<ValueId>, BTreeSet<FunctionId>)>,
    node_work: Vec<NodeTask>,
}

impl ScopeVerifier {
    /// Record what `step` binds, rejecting a name bound twice, then queue the regions below it.
    fn admit(&mut self, step: ScopeStep) -> Result<(), VerifyError> {
        let ScopeStep {
            values,
            functions,
            tasks,
        } = step;
        for function in functions {
            if !self.bound_functions.insert(function) {
                return Err(VerifyError(format!(
                    "function {function} is bound more than once"
                )));
            }
        }
        for ScopeBinding { value, noun } in values {
            if !self.bound_values.insert(value) {
                return Err(VerifyError(format!(
                    "{noun} {value} is bound more than once"
                )));
            }
        }
        self.queue(tasks);
        Ok(())
    }

    /// Queue the regions below a step without deciding anything about what it binds. The recording walk takes this route rather than [`Self::admit`]: it reports nothing, so a name bound twice is not its to refuse, and refusing one would drop the whole region beneath it from a set whose incompleteness costs optimizations.
    fn queue(&mut self, tasks: Vec<ScopeTask>) {
        for task in tasks {
            match task {
                ScopeTask::Function {
                    function,
                    values,
                    functions,
                } => self.function_work.push((function, values, functions)),
                ScopeTask::Node {
                    owner,
                    node,
                    values,
                    functions,
                    continuations,
                } => self
                    .node_work
                    .push((owner, node, values, functions, continuations)),
            }
        }
    }
}

/// The production Cont representation. Arena slots never move or get reused; deletion writes `None` and deterministic compaction is explicit.
#[derive(Debug, Clone, Default)]
pub struct Module {
    nodes: Arena<NodeId, Node>,
    values: Arena<ValueId, ValueDef>,
    functions: Arena<FunctionId, Function>,
    continuations: Arena<ContinuationId, Continuation>,
    field_groups: BTreeMap<ContinuationId, Vec<FieldGroup>>,
    /// The nominal rows this module's [`ValueExpr::Row`]s belong to, appended by the Ersd door and never removed — a row that loses its last construction is simply an unreferenced entry, so the ids stay stable without tombstones.
    rows: Vec<Option<Row>>,
    entry: Option<FunctionId>,
}

/// One nominal row — a variant family or a product schema: its debug name, and the carrier of every slot of its heap type. A family carries its tag at slot zero and a product does not; either way this is the width every [`ValueExpr::Row`] naming it is padded to.
#[derive(Debug, Clone)]
pub struct Row {
    pub debug_name: Option<String>,
    pub slots: Vec<Slot>,
}

impl Row {
    /// The arity every construction of this row carries.
    pub fn width(&self) -> usize {
        self.slots.len()
    }
}

/// What one slot of a row's heap type holds.
///
/// The door decides this from the erased shape recorded on each constructor's fields, and it is the whole point of keying a heap type by row: an arity-keyed type is shared by every constructor of that arity module-wide, so the join over any slot's stores is the top type and nothing can be said about it. A row's slots are written by that row alone, so a slot whose every writer agrees names a carrier — a register for the scalars, a declared heap type for the shapes — and the emitter declares the wasm field at it.
///
/// Slots are assigned by carrier rather than by field position, which is what keeps a family from widening: a constructor's fields are distributed into the slot range their carrier owns, so two constructors sharing a carrier share its slots and only a disagreement costs width. Positional assignment would have been free but types almost nothing — over the standard library it settles 11 slots against this rule's 22 — while giving each constructor a disjoint range types only five more and costs 18 slots more than this.
///
/// Three shapes stay [`Slot::Opaque`] deliberately. A packed carrier is *sometimes* an immediate, so no single heap type names its population. A closure's runtime arity is not something the recorded shape is yet entitled to promise, since the erased arity is read off the declared type and the passes above may raise it. A row-typed field would need the field's row identity, which erasure does not record.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Slot {
    /// A variant family's discriminant, at slot zero. Stored packed and read unsigned, since a family's constructor count is bounded far below the byte the tag occupies; a product row carries none.
    Tag,
    /// A raw unsigned machine-word payload — a `Bool`, a `Byte`, or a payload-less constructor riding the zero. Never a `Nat` or `Int`, which is a reference and takes [`Slot::Opaque`].
    Nat,
    /// A raw binary64 payload — the one slot that deletes an allocation rather than a coercion, since the boxed `Flt` it replaces is a heap object of its own.
    Flt,
    /// A list rope. The base type is not final, so this is the slot that deletes an `is_subtype` libcall rather than an inline check.
    List,
    /// A closure of the given arity. Its environment base is *not* final — it is the supertype of every per-closure environment of that arity — so, like [`Slot::List`], this is a slot that deletes an `is_subtype` libcall rather than an inline check.
    Closure(usize),
    /// A value of the named nominal row. A row's heap type is final, so this is the slot whose read needs no cast at all once Binaryen has the static type.
    Row(RowId),
    /// The uniform reference: a polymorphic payload, or one whose shape names no single heap type.
    Opaque,
}

impl Slot {
    /// The representation a read of this slot produces.
    pub fn repr(self) -> Repr {
        match self {
            Slot::Tag | Slot::Nat => Repr::Nat,
            Slot::Flt => Repr::Flt,
            Slot::List => Repr::List,
            Slot::Closure(_) | Slot::Row(_) | Slot::Opaque => Repr::Ref,
        }
    }
}

impl Module {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn entry(&self) -> Option<FunctionId> {
        self.entry
    }

    /// The recorded fields representations, by continuation.
    pub fn field_groups(&self) -> &BTreeMap<ContinuationId, Vec<FieldGroup>> {
        &self.field_groups
    }

    /// Register a nominal row and hand back its identity. The Ersd door is the only caller; see [`ValueExpr::Row`].
    pub fn add_row(&mut self, row: Row) -> RowId {
        let id = self.reserve_row();
        self.define_row(id, row);
        id
    }

    /// Claim an identity before the row it names is known.
    ///
    /// A row's slots may name other rows, and a self-referential declaration names its own — so the identity has to exist before the slots are computed, or computing them would not terminate. An undefined row is a compiler bug, and [`Module::row`] says so rather than carrying an `Option` every caller would unwrap.
    pub fn reserve_row(&mut self) -> RowId {
        let id = RowId::from_index(self.rows.len());
        self.rows.push(None);
        id
    }

    pub fn define_row(&mut self, id: RowId, row: Row) {
        self.rows[id.index()] = Some(row);
    }

    pub fn row(&self, id: RowId) -> &Row {
        self.rows[id.index()]
            .as_ref()
            .unwrap_or_else(|| panic!("{id} was reserved and never defined"))
    }

    /// What a transfer hands a slot its construction never wrote: what the constructed row's field holds there — zero for a register slot, since a register has no null, and null, as [`Atom::Filler`], for a reference. `None` is a tuple, whose every field is a reference.
    pub fn pad(&self, row: Option<RowId>, index: usize) -> Atom {
        match row.map(|row| self.row(row).slots[index]) {
            Some(Slot::Tag | Slot::Nat) => Atom::Literal(Literal::Nat(Natural::zero())),
            Some(Slot::Flt) => Atom::Literal(Literal::Flt(Floating::zero(false))),
            Some(Slot::List | Slot::Closure(_) | Slot::Row(_) | Slot::Opaque) | None => {
                Atom::Filler
            }
        }
    }

    /// The representation a read of `row`'s slot at `index` produces. The one result representation that is a fact of the module rather than of the operation, which is why [`Intrinsic::result_repr`] cannot answer it alone.
    pub fn slot_repr(&self, row: RowId, index: usize) -> Repr {
        self.row(row).slots[index].repr()
    }

    /// The representation `op` produces, resolving a row read against this module's slot carriers.
    pub fn result_repr(&self, op: &Intrinsic) -> Repr {
        match op {
            Intrinsic::RowGet(row, index) => self.slot_repr(*row, *index),
            _ => op.result_repr(),
        }
    }

    pub fn rows(&self) -> impl Iterator<Item = (RowId, &Row)> {
        (0..self.rows.len())
            .map(RowId::from_index)
            .map(|id| (id, self.row(id)))
    }

    /// Record that `continuation`'s parameter at `start` was spliced into `width` fields: the new group, *and* every group past it shifted by the parameters the splice added.
    ///
    /// Recording and shifting are one operation because they are one fact. They were two, and the shift lived in the one caller that had needed it so far — which left every other caller silently recording stale starts, reachable as soon as two parameters of one continuation were split in the same pass. Groups are kept sorted by start; [`Module::verify`] holds them to the parameter list.
    pub fn record_split(&mut self, continuation: ContinuationId, start: usize, width: usize) {
        let groups = self.field_groups.entry(continuation).or_default();
        for group in groups.iter_mut() {
            if group.start > start {
                group.start += width - 1;
            }
        }
        groups.push(FieldGroup { start, width });
        groups.sort_by_key(|group| group.start);
    }

    /// Maintain the record across a parameter removal: shift groups past each removed index down, shrink groups losing a member, and drop groups emptied entirely. The caller removes the parameters; this keeps the record telling the truth about what remains.
    pub fn remove_params_from_record(
        &mut self,
        continuation: ContinuationId,
        removed: &BTreeSet<usize>,
    ) {
        let Some(groups) = self.field_groups.get_mut(&continuation) else {
            return;
        };
        for group in groups.iter_mut() {
            let inside = removed
                .iter()
                .filter(|&&index| index >= group.start && index < group.start + group.width)
                .count();
            let before = removed.iter().filter(|&&index| index < group.start).count();
            group.start -= before;
            group.width -= inside;
        }
        groups.retain(|group| group.width > 0);
        if groups.is_empty() {
            self.field_groups.remove(&continuation);
        }
    }

    pub fn set_entry(&mut self, entry: FunctionId) {
        self.entry = Some(entry);
    }

    pub fn nodes(&self) -> &[Option<Node>] {
        self.nodes.slots()
    }

    pub fn values(&self) -> &[Option<ValueDef>] {
        self.values.slots()
    }

    pub fn functions(&self) -> &[Option<Function>] {
        self.functions.slots()
    }

    pub fn continuations(&self) -> &[Option<Continuation>] {
        self.continuations.slots()
    }

    pub fn node(&self, id: NodeId) -> Option<&Node> {
        self.nodes.get(id)
    }

    pub fn function(&self, id: FunctionId) -> Option<&Function> {
        self.functions.get(id)
    }

    pub fn continuation(&self, id: ContinuationId) -> Option<&Continuation> {
        self.continuations.get(id)
    }

    /// Count, per value, how many times it is referenced across the module. A value's use sites are its operand occurrences plus its use as an indirect callee; definitions (`LetValue`/`LetIntrinsic` results, parameters) are not uses, so an unreferenced value is absent from the map. Derived on demand rather than maintained incrementally.
    pub(crate) fn value_use_counts(&self) -> BTreeMap<ValueId, usize> {
        let mut counts = BTreeMap::new();
        for (_, node) in self.nodes.iter_live() {
            for atom in atoms(node) {
                if let Atom::Value(value) = atom {
                    *counts.entry(*value).or_insert(0) += 1;
                }
            }
            if let Node::ApplyFun {
                callee: Callee::Closure(value),
                ..
            } = node
            {
                *counts.entry(*value).or_insert(0) += 1;
            }
        }
        counts
    }

    /// How many values each live function hands back to its caller.
    ///
    /// A function's returns are its edges to its own return sentinel, so the arity those edges carry *is* its result count — nothing declares it, and adding a field to say so would mean restating it at every construction site rather than reading it off the one place that already knows. A function with no such edge returns through some tail position instead: a foreign call, a cell operation, or a `ListMap` hands back what that operation produces, a closure call hands back the one value its shared type carries, and a tail call to a known function hands back whatever *that* function does — which is why the last of those is resolved by propagation rather than locally. A function with none of those neither returns nor is called for a result, and takes the one value every function carried before any protocol widened it.
    ///
    /// Where a function has both a return edge and a constrained tail position, the edge is taken and the disagreement is left to [`Module::verify`], whose business it is to report rather than to paper over.
    pub fn return_arities(&self) -> BTreeMap<FunctionId, usize> {
        let mut settled = BTreeMap::<FunctionId, usize>::new();
        let mut inherits = BTreeMap::<FunctionId, BTreeSet<FunctionId>>::new();

        for (function, definition) in self.functions.iter_live() {
            let sentinel = definition.return_cont;
            let mut edges = None;
            let mut operation = None;
            let mut tail_calls = BTreeSet::new();

            for node_id in analysis::nodes_from(self, definition.body) {
                let mut returning = |edge: &Edge| {
                    if edge.target == sentinel {
                        edges.get_or_insert(edge.args.len());
                    }
                };
                match self.node(node_id).unwrap() {
                    Node::ApplyCont(edge) => returning(edge),
                    Node::Switch { cases, default, .. } => {
                        cases.values().chain(default.as_ref()).for_each(returning);
                    }
                    Node::ApplyFun {
                        callee,
                        return_to: to,
                        ..
                    } if *to == sentinel => match callee {
                        Callee::Known(callee) => {
                            tail_calls.insert(*callee);
                        }
                        Callee::Closure(_) => operation = operation.or(Some(1)),
                    },
                    Node::Foreign {
                        function,
                        return_to,
                        ..
                    } if *return_to == sentinel => {
                        operation = operation.or(Some(function.signature.results.len()));
                    }
                    Node::Cell { op, return_to, .. } if *return_to == sentinel => {
                        operation = operation.or(Some(op.result_arity()));
                    }
                    Node::Intrinsic { return_to, .. } if *return_to == sentinel => {
                        operation = operation.or(Some(1));
                    }
                    _ => {}
                }
            }

            match edges.or(operation) {
                Some(arity) => {
                    settled.insert(function, arity);
                }
                None => {
                    inherits.insert(function, tail_calls);
                }
            }
        }

        // Propagate along tail calls until nothing more resolves. Whatever is left over is mutually tail-recursive with nothing that ever returns, so no edge constrains it.
        while inherits
            .values()
            .flatten()
            .any(|to| settled.contains_key(to))
        {
            for (function, tail_calls) in &inherits {
                if let Some(arity) = tail_calls.iter().find_map(|to| settled.get(to)).copied() {
                    settled.insert(*function, arity);
                }
            }
            inherits.retain(|function, _| !settled.contains_key(function));
        }
        for function in inherits.into_keys() {
            settled.insert(function, 1);
        }
        settled
    }

    pub fn reserve_node(&mut self) -> NodeId {
        self.nodes.reserve()
    }

    pub fn add_node(&mut self, node: Node) -> NodeId {
        let id = self.reserve_node();
        self.define_node(id, node);
        id
    }

    pub fn define_node(&mut self, id: NodeId, node: Node) {
        self.nodes.define(id, node);
    }

    pub fn add_value(&mut self, debug_name: Option<String>) -> ValueId {
        self.values.mint(ValueDef { debug_name })
    }

    pub fn reserve_function(&mut self) -> FunctionId {
        self.functions.reserve()
    }

    pub fn define_function(&mut self, id: FunctionId, function: Function) {
        self.functions.define(id, function);
    }

    pub fn add_function(&mut self, function: Function) -> FunctionId {
        self.functions.mint(function)
    }

    pub fn reserve_continuation(&mut self) -> ContinuationId {
        self.continuations.reserve()
    }

    pub fn define_continuation(&mut self, id: ContinuationId, continuation: Continuation) {
        self.continuations.define(id, continuation);
    }

    pub fn add_continuation(&mut self, continuation: Continuation) -> ContinuationId {
        self.continuations.mint(continuation)
    }

    pub fn remove_node(&mut self, id: NodeId) -> Option<Node> {
        self.nodes.remove(id)
    }

    pub fn replace_atom(&mut self, from: UseTarget, replacement: Atom) {
        for (_, node) in self.nodes.iter_live_mut() {
            visit_atoms_mut(node, &mut |atom| {
                let matches = match (&from, &*atom) {
                    (UseTarget::Value(a), Atom::Value(b)) => a == b,
                    (UseTarget::Fun(a), Atom::Fun(b)) => a == b,
                    _ => false,
                };
                if matches {
                    *atom = replacement.clone();
                }
            });
        }
    }

    pub fn tombstones(&self) -> (usize, usize, usize, usize) {
        let return_continuations = self
            .functions
            .iter_live()
            .map(|(_, function)| function.return_cont)
            .collect::<BTreeSet<_>>();
        (
            self.nodes.tombstone_count(),
            self.values.tombstone_count(),
            self.functions.tombstone_count(),
            self.continuations
                .slots()
                .iter()
                .enumerate()
                .filter(|(index, slot)| {
                    slot.is_none() && !return_continuations.contains(&ContinuationId(*index as u32))
                })
                .count(),
        )
    }

    pub fn verify(&self) -> Result<(), VerifyError> {
        self.verify_with(true)
    }

    /// The round-boundary subset of [`Module::verify`]: every structural clause, without the row-vocabulary one.
    ///
    /// A round's close leaves scoping, ownership and arities canonical, but the vocabulary clause holds only of the *converged* module: constant folding pushes a decided reply's payload into both arms of its dispatch, so until a later round threads the decided switch and prunes behind it, the dead arm legitimately reads that payload in the other vocabulary — the tag the fold decided is what keeps it honest, and no per-round rewrite is obliged to have cleaned it up yet. `/std/Parse`'s reply dispatches reach this state on every `pure`-fed combinator, which is how the full check at the boundary broke half the cross-stage corpus while the exit gate stayed green. The entry and exit verifies keep the full set, so a mismatch that survives convergence is still refused where its premise actually holds.
    pub fn verify_structure(&self) -> Result<(), VerifyError> {
        self.verify_with(false)
    }

    fn verify_with(&self, rows: bool) -> Result<(), VerifyError> {
        let entry = self
            .entry
            .ok_or_else(|| VerifyError("module has no entry function".into()))?;
        self.require_fun(entry, "entry")?;

        let mut returns = BTreeMap::<ContinuationId, FunctionId>::new();
        for (id, function) in self.functions.iter_live() {
            if function.return_cont.index() >= self.continuations.len() {
                return Err(VerifyError(format!(
                    "{id} return continuation {} was not minted by this module",
                    function.return_cont
                )));
            }
            if self.continuation(function.return_cont).is_some() {
                return Err(VerifyError(format!(
                    "{id} return continuation {} also identifies a local continuation",
                    function.return_cont
                )));
            }
            if let Some(previous) = returns.insert(function.return_cont, id) {
                return Err(VerifyError(format!(
                    "{} is the return continuation of both {previous} and {id}",
                    function.return_cont
                )));
            }
            self.require_node(function.body, "function body")?;
            for &param in &function.params {
                self.require_value(param, "function parameter")?;
            }
        }

        for (_, continuation) in self.continuations.iter_live() {
            self.require_node(continuation.body, "continuation body")?;
            for &param in &continuation.params {
                self.require_value(param, "continuation parameter")?;
            }
        }

        let arities = self.return_arities();
        let facts = ReturnFacts {
            owners: &returns,
            arities: &arities,
        };
        let mut node_owners = BTreeMap::<NodeId, FunctionId>::new();
        let mut bound_continuations = BTreeSet::<ContinuationId>::new();
        for (id, function) in self.functions.iter_live() {
            self.verify_function_body(
                id,
                function,
                &facts,
                &mut node_owners,
                &mut bound_continuations,
            )?;
        }
        self.verify_lexical_scopes(entry)?;
        if rows {
            self.verify_rows()?;
        }

        let live_nodes = self.nodes.live_ids().collect::<BTreeSet<_>>();
        let owned_nodes = node_owners.keys().copied().collect::<BTreeSet<_>>();
        if live_nodes != owned_nodes {
            return Err(VerifyError(
                "node arena contains an unowned node or an owner references a tombstone".into(),
            ));
        }

        let live_continuations = self.continuations.live_ids().collect::<BTreeSet<_>>();
        if live_continuations != bound_continuations {
            return Err(VerifyError(
                "local-continuation arena and lexical LetCont bindings disagree".into(),
            ));
        }

        // The recorded fields representations hold: every group names a live continuation and lies inside its parameter list without overlapping a neighbour, so a pass that reshaped a recorded parameter list without maintaining the record fails here rather than silently disagreeing with the split.
        for (continuation, groups) in &self.field_groups {
            let Some(definition) = self.continuation(*continuation) else {
                return Err(VerifyError(format!(
                    "field group records dead continuation {continuation}"
                )));
            };
            let mut end = 0;
            for group in groups {
                if group.width == 0 {
                    return Err(VerifyError(format!(
                        "{continuation} records an empty field group at {}",
                        group.start
                    )));
                }
                if group.start < end {
                    return Err(VerifyError(format!(
                        "{continuation} records overlapping field groups at {}",
                        group.start
                    )));
                }
                end = group.start + group.width;
            }
            if end > definition.params.len() {
                return Err(VerifyError(format!(
                    "{continuation} records a field group past its {} parameters",
                    definition.params.len()
                )));
            }
        }

        Ok(())
    }

    /// The row vocabulary's coherence: every row named by a construction or a read exists, every construction carries exactly its row's width, every read is in range of it — and a read of a value this module visibly constructs is in the vocabulary that construction was minted in.
    ///
    /// This is what the distinct [`ValueExpr::Row`] buys over an annotation on `Tuple`. A row value read at a structural projection, or a construction one slot short of its row, would be a `ref.cast` trap in emitted code far from the pass that caused it; here it is a verifier failure at the boundary that produced it. Padding is the door's job, so a mismatch is always a compiler bug rather than a program's.
    ///
    /// The last clause was documented here before it was checked, and the gap was found the way the paragraph above predicts: `split_returns` rebuilt a resume's `Tuple` for a class returning an `Option` row, the `RowGet` below it cast `$tuple/2` to the row's final type, and the only symptom was an HTTP client trapping on its first response header. The check covers direct operands — a value constructed by a `LetValue` in this module and read by a `TupleGet` or `RowGet` in it — which is every case a pass's own rebuild can produce; a value that arrives through a parameter is the emitter's cast to decide, as before.
    fn verify_rows(&self) -> Result<(), VerifyError> {
        // What every visible construction built, so a read can be checked against the vocabulary its operand was actually minted in rather than only against the row's own width.
        let mut built = BTreeMap::<ValueId, Option<RowId>>::new();
        for (_, node) in self.nodes.iter_live() {
            if let Node::LetValue { result, value, .. } = node {
                match value {
                    ValueExpr::Row(row, _) => {
                        built.insert(*result, Some(*row));
                    }
                    ValueExpr::Tuple(_) => {
                        built.insert(*result, None);
                    }
                    ValueExpr::Literal(_) | ValueExpr::List(_) => {}
                }
            }
        }
        for (_, node) in self.nodes.iter_live() {
            if let Node::LetIntrinsic { op, args, .. } = node
                && let [Atom::Value(operand)] = args.as_slice()
                && let Some(&minted) = built.get(operand)
            {
                let read = match op {
                    Intrinsic::RowGet(row, _) => Some(Some(*row)),
                    Intrinsic::TupleGet(_) => Some(None),
                    _ => None,
                };
                if let Some(read) = read
                    && read != minted
                {
                    return Err(VerifyError(format!(
                        "{operand} was built as {} but is read as {}",
                        match minted {
                            Some(row) => format!("{row}"),
                            None => "a structural tuple".into(),
                        },
                        match read {
                            Some(row) => format!("{row}"),
                            None => "a structural tuple".into(),
                        },
                    )));
                }
            }
        }
        for (_, node) in self.nodes.iter_live() {
            match node {
                Node::LetValue {
                    value: ValueExpr::Row(row, atoms),
                    ..
                } => {
                    let Some(Some(definition)) = self.rows.get(row.index()) else {
                        return Err(VerifyError(format!(
                            "row construction names {row}, which was not minted by this module"
                        )));
                    };
                    if atoms.len() != definition.width() {
                        return Err(VerifyError(format!(
                            "row construction of {row} carries {} slots, but the row is {} wide",
                            atoms.len(),
                            definition.width(),
                        )));
                    }
                }
                Node::LetIntrinsic {
                    op: Intrinsic::RowGet(row, index),
                    ..
                } => {
                    let Some(Some(definition)) = self.rows.get(row.index()) else {
                        return Err(VerifyError(format!(
                            "row read names {row}, which was not minted by this module"
                        )));
                    };
                    if *index >= definition.width() {
                        return Err(VerifyError(format!(
                            "row read of {row} at slot {index}, but the row is {} wide",
                            definition.width(),
                        )));
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn verify_lexical_scopes(&self, entry: FunctionId) -> Result<(), VerifyError> {
        let mut walk = ScopeVerifier {
            bound_functions: BTreeSet::from([entry]),
            function_work: vec![(entry, BTreeSet::new(), BTreeSet::from([entry]))],
            ..ScopeVerifier::default()
        };
        let mut visited_nodes = BTreeSet::new();

        while !walk.function_work.is_empty() || !walk.node_work.is_empty() {
            while let Some((function, values, functions)) = walk.function_work.pop() {
                walk.admit(self.function_scope(function, values, functions))?;
            }

            let Some((owner, node_id, values, functions, continuations)) = walk.node_work.pop()
            else {
                continue;
            };
            if !visited_nodes.insert(node_id) {
                continue;
            }
            let owner = owner.expect("a verification task names the function it walks");
            let node = self.node(node_id).unwrap();
            for atom in atoms(node) {
                match atom {
                    Atom::Value(value) if !values.contains(value) => {
                        return Err(VerifyError(format!(
                            "{owner} node {node_id} uses out-of-scope {value}"
                        )));
                    }
                    Atom::Fun(function) if !functions.contains(function) => {
                        return Err(VerifyError(format!(
                            "{owner} node {node_id} uses out-of-scope {function}"
                        )));
                    }
                    Atom::Value(_) | Atom::Fun(_) | Atom::Literal(_) | Atom::Filler => {}
                }
            }
            if let Node::ApplyFun { callee, .. } = node {
                match callee {
                    Callee::Known(function) if !functions.contains(function) => {
                        return Err(VerifyError(format!(
                            "{owner} node {node_id} calls out-of-scope {function}"
                        )));
                    }
                    Callee::Closure(value) if !values.contains(value) => {
                        return Err(VerifyError(format!(
                            "{owner} node {node_id} calls out-of-scope {value}"
                        )));
                    }
                    Callee::Known(_) | Callee::Closure(_) => {}
                }
            }

            walk.admit(self.scope_step(Some(owner), node, values, functions, continuations))?;
        }

        let live_functions = self.functions.live_ids().collect::<BTreeSet<_>>();
        if live_functions != walk.bound_functions {
            return Err(VerifyError(
                "function arena and lexical function bindings disagree".into(),
            ));
        }
        let live_values = self.values.live_ids().collect::<BTreeSet<_>>();
        if live_values != walk.bound_values {
            return Err(VerifyError(
                "value arena and lexical value bindings disagree".into(),
            ));
        }
        Ok(())
    }

    /// The functions each body may name, by the rule [`Self::scope_step`] states and [`Self::verify_lexical_scopes`] enforces: its own `LetFun` group, every group enclosing it, and every group bound *before* it along the chain from the entry. Recorded rather than checked, so a pass forwarding a function reference into a body can ask whether that body may legally name it.
    ///
    /// A function the walk does not reach is absent rather than empty, and the caller decides what to answer for it. This runs mid-round, where the module is transiently unscoped by design and only a round boundary promises a walk from the entry reaches every live function.
    fn lexical_scopes(&self) -> BTreeMap<FunctionId, BTreeSet<FunctionId>> {
        let Some(entry) = self.entry else {
            return BTreeMap::new();
        };
        let mut scopes = BTreeMap::new();
        let mut walk = ScopeVerifier {
            function_work: vec![(entry, BTreeSet::new(), BTreeSet::from([entry]))],
            ..ScopeVerifier::default()
        };
        let mut visited_nodes = BTreeSet::new();

        while !walk.function_work.is_empty() || !walk.node_work.is_empty() {
            while let Some((function, values, functions)) = walk.function_work.pop() {
                scopes.insert(function, functions.clone());
                let step = self.function_scope(function, values, functions);
                walk.queue(step.tasks);
            }

            let Some((_, node_id, values, functions, continuations)) = walk.node_work.pop() else {
                continue;
            };
            if !visited_nodes.insert(node_id) {
                continue;
            }
            let Some(node) = self.node(node_id) else {
                continue;
            };
            let step = self.scope_step(None, node, values, functions, continuations);
            walk.queue(step.tasks);
        }
        scopes
    }

    /// The scope a function's own body sees: its parameters join the values it inherits, and no continuation crosses the boundary.
    fn function_scope(
        &self,
        function: FunctionId,
        mut values: BTreeSet<ValueId>,
        functions: BTreeSet<FunctionId>,
    ) -> ScopeStep {
        let definition = self.function(function).unwrap();
        let mut step = ScopeStep::default();
        for value in &definition.params {
            step.values.push(ScopeBinding {
                value: *value,
                noun: "function parameter",
            });
            values.insert(*value);
        }
        step.tasks.push(ScopeTask::Node {
            owner: Some(function),
            node: definition.body,
            values,
            functions,
            continuations: BTreeSet::new(),
        });
        step
    }

    /// What `node` binds, and the regions below it with the scope each one sees.
    ///
    /// This is the single statement of the lexical scoping rules, which [`Self::verify_lexical_scopes`] enforces.
    fn scope_step(
        &self,
        owner: Option<FunctionId>,
        node: &Node,
        values: BTreeSet<ValueId>,
        functions: BTreeSet<FunctionId>,
        continuations: BTreeSet<ContinuationId>,
    ) -> ScopeStep {
        let mut step = ScopeStep::default();
        match node {
            Node::LetValue { result, next, .. } | Node::LetIntrinsic { result, next, .. } => {
                step.values.push(ScopeBinding {
                    value: *result,
                    noun: "node result",
                });
                let mut inner = values;
                inner.insert(*result);
                step.tasks.push(ScopeTask::Node {
                    owner,
                    node: *next,
                    values: inner,
                    functions,
                    continuations,
                });
            }
            Node::LetFun {
                functions: members,
                body,
            } => {
                let mut inner = functions;
                for function in members {
                    step.functions.push(*function);
                    inner.insert(*function);
                }
                for function in members.iter().rev() {
                    step.tasks.push(ScopeTask::Function {
                        function: *function,
                        values: values.clone(),
                        functions: inner.clone(),
                    });
                }
                step.tasks.push(ScopeTask::Node {
                    owner,
                    node: *body,
                    values,
                    functions: inner,
                    continuations,
                });
            }
            Node::LetCont {
                continuations: members,
                body,
            } => {
                let mut inner = continuations;
                inner.extend(members.iter().copied());
                for continuation in members.iter().rev() {
                    // `verify_node` has already rejected a `LetCont` naming a missing member, so the walk may read it.
                    let definition = self.continuation(*continuation).unwrap();
                    let mut continuation_values = values.clone();
                    for value in &definition.params {
                        step.values.push(ScopeBinding {
                            value: *value,
                            noun: "continuation parameter",
                        });
                        continuation_values.insert(*value);
                    }
                    step.tasks.push(ScopeTask::Node {
                        owner,
                        node: definition.body,
                        values: continuation_values,
                        functions: functions.clone(),
                        continuations: inner.clone(),
                    });
                }
                step.tasks.push(ScopeTask::Node {
                    owner,
                    node: *body,
                    values,
                    functions,
                    continuations: inner,
                });
            }
            Node::ApplyFun { .. }
            | Node::ApplyCont(_)
            | Node::Switch { .. }
            | Node::Foreign { .. }
            | Node::Cell { .. }
            | Node::Intrinsic { .. }
            | Node::Exit { .. }
            | Node::Panic(_)
            | Node::Unreachable => {}
        }
        step
    }

    fn verify_function_body(
        &self,
        owner: FunctionId,
        function: &Function,
        facts: &ReturnFacts<'_>,
        node_owners: &mut BTreeMap<NodeId, FunctionId>,
        bound_continuations: &mut BTreeSet<ContinuationId>,
    ) -> Result<(), VerifyError> {
        let mut work = vec![(function.body, BTreeSet::<ContinuationId>::new())];
        let mut visited = BTreeSet::<NodeId>::new();

        while let Some((id, scope)) = work.pop() {
            // Every node has exactly one structural parent: a `next`, a `body`, or a continuation's. The walk once skipped a node it had already reached, which caught a node shared between two *functions* through `node_owners` and let a node shared within one function pass — and a shared node is a region that runs on two paths while binding its values once, which the scope check cannot see either, since it admits the bindings on whichever path reached it first. It is also what a nesting printer would duplicate.
            if !visited.insert(id) {
                return Err(VerifyError(format!(
                    "{id} is reached from more than one place in {owner}"
                )));
            }
            if let Some(previous) = node_owners.insert(id, owner)
                && previous != owner
            {
                return Err(VerifyError(format!(
                    "{id} is owned by both {previous} and {owner}"
                )));
            }
            let node = self
                .node(id)
                .ok_or_else(|| VerifyError(format!("function body references missing {id}")))?;
            self.verify_node(owner, function.return_cont, facts, &scope, id, node)?;

            match node {
                Node::LetValue { next, .. } | Node::LetIntrinsic { next, .. } => {
                    work.push((*next, scope));
                }
                Node::LetFun { body, .. } => {
                    work.push((*body, scope));
                }
                Node::LetCont {
                    continuations,
                    body,
                } => {
                    let mut inner = scope;
                    for &continuation in continuations {
                        if facts.owners.contains_key(&continuation) {
                            return Err(VerifyError(format!(
                                "return ID {continuation} cannot be bound as a local continuation"
                            )));
                        }
                        self.require_cont(continuation, "LetCont member")?;
                        if !bound_continuations.insert(continuation) {
                            return Err(VerifyError(format!(
                                "local continuation {continuation} is bound more than once"
                            )));
                        }
                        inner.insert(continuation);
                    }
                    work.push((*body, inner.clone()));
                    for &continuation in continuations.iter().rev() {
                        work.push((self.continuation(continuation).unwrap().body, inner.clone()));
                    }
                }
                Node::ApplyFun { .. }
                | Node::ApplyCont(_)
                | Node::Switch { .. }
                | Node::Foreign { .. }
                | Node::Cell { .. }
                | Node::Intrinsic { .. }
                | Node::Exit { .. }
                | Node::Panic(_)
                | Node::Unreachable => {}
            }
        }
        Ok(())
    }

    fn verify_node(
        &self,
        current_function: FunctionId,
        return_cont: ContinuationId,
        facts: &ReturnFacts<'_>,
        scope: &BTreeSet<ContinuationId>,
        id: NodeId,
        node: &Node,
    ) -> Result<(), VerifyError> {
        match node {
            Node::LetValue { result, next, .. } => {
                self.require_value(*result, "let-value result")?;
                self.require_node(*next, "let-value successor")?;
            }
            Node::LetIntrinsic {
                result,
                op,
                args,
                next,
            } => {
                self.require_value(*result, "let-intrinsic result")?;
                self.require_node(*next, "let-intrinsic successor")?;
                if args.len() != op.arity() {
                    return Err(VerifyError(format!(
                        "{id} intrinsic {op:?} expects {} operands, got {}",
                        op.arity(),
                        args.len()
                    )));
                }
            }
            Node::LetFun { functions, body } => {
                for &function in functions {
                    self.require_fun(function, "let-fun member")?;
                }
                self.require_node(*body, "let-fun body")?;
            }
            Node::LetCont {
                continuations,
                body,
            } => {
                for &continuation in continuations {
                    self.require_cont(continuation, "let-cont member")?;
                }
                self.require_node(*body, "let-cont body")?;
            }
            Node::ApplyFun {
                callee,
                args,
                return_to,
            } => {
                match callee {
                    Callee::Known(function) => {
                        self.require_fun(*function, "known callee")?;
                        let arity = self.function(*function).unwrap().params.len();
                        if arity != args.len() {
                            return Err(VerifyError(format!(
                                "{id} calls {function} with {} arguments; expected {arity}",
                                args.len()
                            )));
                        }
                    }
                    Callee::Closure(value) => self.require_value(*value, "closure callee")?,
                }
                // A closure is reached through the shared type of its arity, which carries one result whatever the function behind it returns.
                let results = match callee {
                    Callee::Known(function) => facts.arity(*function),
                    Callee::Closure(_) => 1,
                };
                let params = self.continuation_arity(
                    current_function,
                    return_cont,
                    facts,
                    scope,
                    *return_to,
                )?;
                if params != results {
                    return Err(VerifyError(format!(
                        "{id} user call return continuation {return_to} accepts {params} values, callee returns {results}"
                    )));
                }
            }
            Node::ApplyCont(edge) => {
                self.verify_edge(current_function, return_cont, facts, scope, id, edge)?
            }
            Node::Switch { cases, default, .. } => {
                for edge in cases.values() {
                    self.verify_edge(current_function, return_cont, facts, scope, id, edge)?;
                }
                if let Some(edge) = default {
                    self.verify_edge(current_function, return_cont, facts, scope, id, edge)?;
                }
            }
            Node::Foreign {
                function,
                args,
                return_to,
            } => {
                if args.len() != function.signature.params.len() {
                    return Err(VerifyError(format!(
                        "{id} foreign call expects {} operands, got {}",
                        function.signature.params.len(),
                        args.len()
                    )));
                }
                let results = function.signature.results.len();
                let params = self.continuation_arity(
                    current_function,
                    return_cont,
                    facts,
                    scope,
                    *return_to,
                )?;
                if results != params {
                    return Err(VerifyError(format!(
                        "{id} foreign return continuation expects {params} values, call returns {results}"
                    )));
                }
            }
            Node::Cell {
                op,
                args,
                return_to,
            } => {
                if args.len() != op.operand_arity() {
                    return Err(VerifyError(format!(
                        "{id} cell {op:?} expects {} operands, got {}",
                        op.operand_arity(),
                        args.len()
                    )));
                }
                if self.continuation_arity(
                    current_function,
                    return_cont,
                    facts,
                    scope,
                    *return_to,
                )? != op.result_arity()
                {
                    return Err(VerifyError(format!(
                        "{id} cell {op:?} continuation arity mismatch"
                    )));
                }
            }
            Node::Intrinsic {
                op: IntrinsicCall::ListMap,
                args,
                return_to,
            } => {
                if args.len() != 2 {
                    return Err(VerifyError(format!(
                        "{id} ListMap expects two operands, got {}",
                        args.len()
                    )));
                }
                if self.continuation_arity(
                    current_function,
                    return_cont,
                    facts,
                    scope,
                    *return_to,
                )? != 1
                {
                    return Err(VerifyError(format!(
                        "{id} ListMap continuation must accept one value"
                    )));
                }
            }
            Node::Exit { .. } | Node::Panic(_) | Node::Unreachable => {}
        }

        for atom in atoms(node) {
            match atom {
                Atom::Value(value) => {
                    // Naming the referencing statement turns a dangling-operand refusal from a value id into a site: which node, and — through its spelled form — which rewrite left it behind.
                    self.require_value(*value, &format!("statement {id} ({node:?}) operand"))?
                }
                Atom::Fun(function) => self.require_fun(*function, "function atom")?,
                Atom::Literal(_) | Atom::Filler => {}
            }
        }
        Ok(())
    }

    /// Check one transfer's argument count against its target's. A return edge is covered by the same rule: a transfer to the enclosing function's own return continuation carries its return arity, read off [`Module::return_arities`] — so an edge that disagrees with its siblings is reported here rather than reaching the emitter.
    fn verify_edge(
        &self,
        function: FunctionId,
        return_cont: ContinuationId,
        facts: &ReturnFacts<'_>,
        scope: &BTreeSet<ContinuationId>,
        owner: NodeId,
        edge: &Edge,
    ) -> Result<(), VerifyError> {
        let arity = self.continuation_arity(function, return_cont, facts, scope, edge.target)?;
        if arity != edge.args.len() {
            return Err(VerifyError(format!(
                "{owner} edge to {} carries {} arguments; expected {arity}",
                edge.target,
                edge.args.len()
            )));
        }
        Ok(())
    }

    /// How many values a transfer to `target` carries; a transfer to the enclosing function's own return continuation carries its return.
    fn continuation_arity(
        &self,
        function: FunctionId,
        return_cont: ContinuationId,
        facts: &ReturnFacts<'_>,
        scope: &BTreeSet<ContinuationId>,
        target: ContinuationId,
    ) -> Result<usize, VerifyError> {
        if target == return_cont {
            return Ok(facts.arity(function));
        }
        if let Some(owner) = facts.owners.get(&target) {
            return Err(VerifyError(format!(
                "{function} references {owner}'s return continuation {target}"
            )));
        }
        if !scope.contains(&target) {
            return Err(VerifyError(format!(
                "{function} references undefined or out-of-scope continuation {target}"
            )));
        }
        self.continuation(target)
            .map(|continuation| continuation.params.len())
            .ok_or_else(|| VerifyError(format!("undefined non-return continuation {target}")))
    }

    fn require_node(&self, id: NodeId, what: &str) -> Result<(), VerifyError> {
        self.node(id)
            .map(|_| ())
            .ok_or_else(|| VerifyError(format!("{what} references missing {id}")))
    }

    fn require_value(&self, id: ValueId, what: &str) -> Result<(), VerifyError> {
        self.values
            .get(id)
            .map(|_| ())
            .ok_or_else(|| VerifyError(format!("{what} references missing {id}")))
    }

    fn require_fun(&self, id: FunctionId, what: &str) -> Result<(), VerifyError> {
        self.function(id)
            .map(|_| ())
            .ok_or_else(|| VerifyError(format!("{what} references missing {id}")))
    }

    fn require_cont(&self, id: ContinuationId, what: &str) -> Result<(), VerifyError> {
        self.continuation(id)
            .map(|_| ())
            .ok_or_else(|| VerifyError(format!("{what} references missing {id}")))
    }
}

pub fn atoms(node: &Node) -> Vec<&Atom> {
    let mut output = Vec::new();
    match node {
        Node::LetValue { value, .. } => match value {
            ValueExpr::Literal(_) => {}
            ValueExpr::List(values) | ValueExpr::Tuple(values) | ValueExpr::Row(_, values) => {
                output.extend(values)
            }
        },
        Node::LetIntrinsic { args, .. }
        | Node::ApplyFun { args, .. }
        | Node::Foreign { args, .. }
        | Node::Cell { args, .. }
        | Node::Intrinsic { args, .. } => output.extend(args),
        Node::ApplyCont(edge) => output.extend(&edge.args),
        Node::Switch {
            scrutinee,
            cases,
            default,
        } => {
            output.push(scrutinee);
            for edge in cases.values() {
                output.extend(&edge.args);
            }
            if let Some(edge) = default {
                output.extend(&edge.args);
            }
        }
        Node::Exit { value, .. } => output.extend(value),
        Node::LetFun { .. } | Node::LetCont { .. } | Node::Panic(_) | Node::Unreachable => {}
    }
    output
}

pub(crate) fn visit_atoms_mut(node: &mut Node, visitor: &mut impl FnMut(&mut Atom)) {
    match node {
        Node::LetValue { value, .. } => match value {
            ValueExpr::Literal(_) => {}
            ValueExpr::List(values) | ValueExpr::Tuple(values) | ValueExpr::Row(_, values) => {
                values.iter_mut().for_each(visitor)
            }
        },
        Node::LetIntrinsic { args, .. }
        | Node::ApplyFun { args, .. }
        | Node::Foreign { args, .. }
        | Node::Cell { args, .. }
        | Node::Intrinsic { args, .. } => args.iter_mut().for_each(visitor),
        Node::ApplyCont(edge) => edge.args.iter_mut().for_each(visitor),
        Node::Switch {
            scrutinee,
            cases,
            default,
        } => {
            visitor(scrutinee);
            for edge in cases.values_mut() {
                edge.args.iter_mut().for_each(&mut *visitor);
            }
            if let Some(edge) = default {
                edge.args.iter_mut().for_each(visitor);
            }
        }
        Node::Exit { value, .. } => {
            if let Some(value) = value {
                visitor(value);
            }
        }
        Node::LetFun { .. } | Node::LetCont { .. } | Node::Panic(_) | Node::Unreachable => {}
    }
}

mod analysis;
mod clone;
mod contify;
mod cse;
mod dataflow;
mod demand;
mod evaluate;
mod fields;
mod inline;
mod optimize;
mod origin;
mod print;
mod protocol;
mod reachable;
mod represent;
mod simplify;
mod specialize;
mod uncurry;

// The pass suites, each beside the pass it tests. They were one file under `optimize` — the driver — while testing eight of these modules; `test_support` holds the module builders they share.
#[cfg(test)]
mod analysis_tests;
#[cfg(test)]
mod contify_tests;
#[cfg(test)]
mod cse_tests;
#[cfg(test)]
mod evaluate_tests;
#[cfg(test)]
mod inline_tests;
#[cfg(test)]
mod optimize_tests;
#[cfg(test)]
mod simplify_tests;
#[cfg(test)]
mod specialize_tests;
#[cfg(test)]
mod test_support;

pub(crate) use dataflow::*;
pub(crate) use demand::*;
pub use optimize::optimize;
pub(crate) use origin::*;
pub use represent::*;

#[cfg(test)]
mod tests;
