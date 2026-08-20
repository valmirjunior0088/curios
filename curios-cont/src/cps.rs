//! Arena-backed high CPS.
//!
//! The surface of this module is intentionally small: Ersd lowering constructs a [`CpsModule`], the optimizer mutates that graph through its checked mutation API, and backend lowering consumes it. Stable integer identities, tombstoned arena entries, and deterministic traversal are representation invariants rather than optimizer conventions. Use information is derived on demand (see [`CpsModule::value_use_counts`]) rather than maintained as a shadow arena.

use {
    curios_abi::ForeignFunction,
    curios_utilities::{Arena, ArenaId, Grain, PackedBin, id},
    std::{
        collections::{BTreeMap, BTreeSet},
        fmt,
        sync::Arc,
    },
};

// Sigils follow the naming scheme shared with `curios-ersd` and `curios-wasm` — see `documentation/design/toolchain/one-naming-scheme-for-compiler-identities.md`.
id!(CpsNodeId, "~n");
id!(CpsValueId, "~v");
id!(CpsFunId, "~f");
id!(CpsContId, "~k");
id!(CpsFamilyId, "~d");

impl CpsFunId {
    pub(crate) fn from_index(index: usize) -> Self {
        Self(index as u32)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CpsLiteral {
    Nat(u32),
    Int(i32),
    Flt(f32),
    Bin(Grain, PackedBin),
}

#[derive(Debug, Clone, PartialEq)]
pub enum CpsAtom {
    Value(CpsValueId),
    Fun(CpsFunId),
    Literal(CpsLiteral),
    /// No value: a slot belonging to a wider constructor than the edge or call carrying it fills.
    ///
    /// It is not a literal zero, and was one until an `Option(Flt)` trapped on the `none` edge. The carrier a slot is held at is decided by [`represent`](crate::cps::represent) during backend lowering, from the *uses* of the parameter it feeds — which is strictly after the passes that create fillers, so no constant chosen here can know it. A `Nat(0)` was therefore right only where the slot happened to be `Nat`-carried or boxed, and where it was a raw `Flt` the edge coerced the `i31` with a `ref.cast` that trapped. Saying "nothing" instead defers the choice to the one place the carrier is known: the emitter materialises it as the zero of whatever the destination holds.
    Filler,
}

#[derive(Debug, Clone)]
pub enum CpsValueExpr {
    Literal(CpsLiteral),
    List(Vec<CpsAtom>),
    Tuple(Vec<CpsAtom>),
    /// A tagged variant construction at its family's width: `fields[0]` is the tag, then the payloads, padded with [`CpsAtom::Filler`] past the constructor's own row. The Ersd door is the only mint and pads every construction, so a family value's arity is a fact of the family rather than of the constructor that built it — which is what lets the emitter key one final heap type per family and read it with an exact cast instead of the structural roster cascade.
    Variant(CpsFamilyId, Vec<CpsAtom>),
}

/// Intrinsic identity without operands. Operand order and arity live on the surrounding `LetIntrinsic`, so every analysis sees one uniform operand vector.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CpsIntrinsic {
    NatEql,
    NatNeq,
    NatAdd,
    NatSub,
    NatMul,
    NatLt,
    NatDiv,
    NatRem,
    NatGt,
    NatLe,
    NatGe,
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
    NatEqz,
    NatToInt,
    NatToFlt,
    IntEql,
    IntNeq,
    IntAdd,
    IntSub,
    IntMul,
    IntDiv,
    IntRem,
    IntLt,
    IntGt,
    IntLe,
    IntGe,
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
    IntEqz,
    IntToNat,
    IntToFlt,
    FltAdd,
    FltSub,
    FltMul,
    FltDiv,
    FltRem,
    FltEql,
    FltNeq,
    FltLt,
    FltGt,
    FltLe,
    FltGe,
    FltMin,
    FltMax,
    FltNeg,
    FltAbs,
    FltSqrt,
    FltFloor,
    FltCeil,
    FltTrunc,
    FltNearest,
    FltCopysign,
    FltToNat,
    FltToLeBytes,
    FltOfLeBytes,
    FltToInt,
    BinLen(Grain),
    BinEql(Grain),
    BinGet(Grain),
    BinSlice(Grain),
    /// `(bin, start) -> bin`: the suffix from `start`, whose extent is the value's own — there is no count operand to supply, so the one thing a caller could get wrong about a suffix it cannot say. Every compiler-emitted window is a suffix (`into_cont`'s peel is the only producer), and this is what keeps two lowerings from having to agree about how a count is derived; the derivation happens once, against the rope's own length.
    BinRest(Grain),
    BinAppend(Grain),
    BinConcat(Grain, usize),
    /// `(element…) -> bin`: one flat leaf holding exactly these elements at the given grain — the fused form of an append chain, minted only by the optimizer's `fuse_append_chains` and never by the door. The arity is the element count in grain units; the byte grain stores one element per payload byte, the bit grain packs eight.
    BinChunk(Grain, usize),
    ListLen,
    ListGet,
    ListSlice,
    /// The `List` mirror of [`CpsIntrinsic::BinRest`].
    ListRest,
    ListAppend,
    ListConcat(usize),
    /// `(list) -> list`: the same value, flat — a leaf answers itself, and anything else answers a fresh leaf over its forced payload (an O(1) wrap, since payload arrays are filled once and never rewritten). Semantically the identity; representationally the settle the door inserts on stores into fields the Ersd census marked indexed-only, so the values a program only ever indexes are flat by the time they are stored.
    ListSettle,
    /// `(list…) -> list`: one exact-length flat leaf holding every element of every operand in order — the eager concatenation `fuse_settle_trees` builds where the reads that would have paid the gather are already in evidence. Minted only by the optimizer, like [`CpsIntrinsic::BinChunk`].
    ListFlat(usize),
    TupleGet(usize),
    /// `(variant) -> value`: slot `index` of a [`CpsValueExpr::Variant`] of `family` — 0 the tag, `1 + i` payload `i`. Distinct from [`CpsIntrinsic::TupleGet`] so a family read names the family whose final type the emitter casts to exactly, and so a structural projection can never silently read a family value through the roster cascade: the two vocabularies meet only in the verifier, which refuses a mismatch.
    VariantGet(CpsFamilyId, usize),
    /// The virtual-window bounds guard: `(start, count, len) -> count`, trapping unless the window ends inside `len` — the eager trap a physical slice would have performed, kept at the original evaluation point when the slice itself is virtualized away. It answers the count unchanged rather than a difference, because a window is a start and a count everywhere above this too; what it contributes is the trap, not the arithmetic.
    WindowExtent,
    /// Whether the operand is an unboxed scalar (1) or an aggregate reference (0) — the dispatch of a variant encoding whose one scalar-payload constructor rides bare. A representation question, which is why it exists in this crate's vocabulary and not in Ersd: the lowering that chose the encoding is the only producer, and it guarantees the two answers are disjoint over every value the test can reach.
    IsImmediate,
    /// `(value) -> value`: the bare payload of the constructor [`CpsIntrinsic::IsImmediate`] just answered for, passed through unchanged.
    ///
    /// Representationally the identity, and that is the whole point: it exists so the payload has a *definition* instead of being aliased to the scrutinee. The representation analysis fixes a value's carrier from whatever produced it, so a payload with no producer of its own carries its uses' raw demand back onto the scrutinee — which on the boxed path is a tuple, not a scalar. That is not a missed optimization but a miscompile: an arm's `NatAdd` demanded the raw carrier, the demand reached the scrutinee's own definition, and the emitter coerced a `struct.new` with a `ref.cast` to `i31`. Answering `Repr::Ref` makes this definition's offer `Never`, so the demand coerces at the use where it belongs and the scrutinee is never demanded raw.
    ImmediateGet,
}

/// The representation a value is read or produced at — the carrier, not the type.
///
/// This is the vocabulary the backend's `LoadAs`/`WrapAs` coercions translate: `Nat`, `Int`, and `Flt` name raw machine carriers a Wasm register can hold, and the rest name references. Stated here, on the IR, rather than in the emitter, because the *optimizer* has to be able to ask what an operation demands of its operands without running codegen to find out — and because an emitter that restates the demand at every use site is an emitter that can disagree with the analysis.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Repr {
    /// A raw unsigned 32-bit carrier.
    Nat,
    /// A raw signed 32-bit carrier.
    Int,
    /// A raw binary32 carrier.
    Flt,
    /// A packed-binary reference at the given grain: a small-canonical immediate or a rope. The grain rides the carrier because the two immediate layouts share no runtime discrimination — only the static type keeps them apart, so the coercion tables must be unable to confuse them.
    Bin(Grain),
    /// A list rope reference.
    List,
    /// An opaque reference: nothing is read of it, so nothing constrains it.
    Ref,
}

impl CpsIntrinsic {
    /// The representation this operation reads its `index`-th operand at.
    ///
    /// Indexed rather than returning a sequence because the concatenations are variadic and every operand of one shares a representation, so a list would allocate to say what a match arm already says.
    pub fn operand_repr(&self, index: usize) -> Repr {
        use CpsIntrinsic::*;

        match (self, index) {
            // The sequence operations are the only ones whose operands differ from one another: a rope first, then positions.
            (BinGet(grain) | BinSlice(grain) | BinRest(grain) | BinAppend(grain), 0) => {
                Repr::Bin(*grain)
            }
            (BinGet(_) | BinSlice(_) | BinRest(_) | BinAppend(_), _) => Repr::Nat,
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
            (TupleGet(_) | VariantGet(..), _) => Repr::Ref,

            (
                NatEql | NatNeq | NatAdd | NatSub | NatMul | NatLt | NatDiv | NatRem | NatGt
                | NatLe | NatGe | NatAnd | NatOr | NatXor | NatShl | NatShr | NatRotl | NatRotr
                | NatClz | NatCtz | NatPopcnt | NatEqz | NatToInt | NatToFlt,
                _,
            ) => Repr::Nat,

            (
                IntEql | IntNeq | IntAdd | IntSub | IntMul | IntDiv | IntRem | IntLt | IntGt
                | IntLe | IntGe | IntAnd | IntOr | IntXor | IntShl | IntShr | IntRotl | IntRotr
                | IntClz | IntCtz | IntPopcnt | IntEqz | IntToNat | IntToFlt,
                _,
            ) => Repr::Int,

            (
                FltAdd | FltSub | FltMul | FltDiv | FltRem | FltEql | FltNeq | FltLt | FltGt
                | FltLe | FltGe | FltMin | FltMax | FltNeg | FltAbs | FltSqrt | FltFloor | FltCeil
                | FltTrunc | FltNearest | FltCopysign | FltToNat | FltToLeBytes | FltToInt,
                _,
            ) => Repr::Flt,
        }
    }

    /// The representation this operation produces.
    pub fn result_repr(&self) -> Repr {
        use CpsIntrinsic::*;

        match self {
            // Every comparison and predicate answers a `Bool`, whose carrier is a `Nat`.
            NatEql | NatNeq | NatLt | NatGt | NatLe | NatGe | NatEqz | IntEql | IntNeq | IntLt
            | IntGt | IntLe | IntGe | IntEqz | FltEql | FltNeq | FltLt | FltGt | FltLe | FltGe
            | BinEql(_) => Repr::Nat,

            NatAdd | NatSub | NatMul | NatDiv | NatRem | NatAnd | NatOr | NatXor | NatShl
            | NatShr | NatRotl | NatRotr | NatClz | NatCtz | NatPopcnt | IntToNat | FltToNat
            | BinLen(_) | ListLen => Repr::Nat,

            IntAdd | IntSub | IntMul | IntDiv | IntRem | IntAnd | IntOr | IntXor | IntShl
            | IntShr | IntRotl | IntRotr | IntClz | IntCtz | IntPopcnt | NatToInt | FltToInt => {
                Repr::Int
            }

            FltAdd | FltSub | FltMul | FltDiv | FltRem | FltMin | FltMax | FltNeg | FltAbs
            | FltSqrt | FltFloor | FltCeil | FltTrunc | FltNearest | FltCopysign | NatToFlt
            | IntToFlt | FltOfLeBytes => Repr::Flt,

            // `IsImmediate` joins the predicates: it answers a `Bool`, whose carrier is a `Nat`.
            BinGet(_) | WindowExtent | IsImmediate => Repr::Nat,
            BinSlice(grain)
            | BinRest(grain)
            | BinAppend(grain)
            | BinConcat(grain, _)
            | BinChunk(grain, _) => Repr::Bin(*grain),
            FltToLeBytes => Repr::Bin(Grain::X),
            ListSlice | ListRest | ListAppend | ListConcat(_) | ListSettle | ListFlat(_) => {
                Repr::List
            }
            // A list read, a tuple or variant projection and an immediate arm's payload all yield whatever was stored, uninterpreted.
            ListGet | TupleGet(_) | VariantGet(..) | ImmediateGet => Repr::Ref,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CpsIntrinsicEffect {
    Total,
    MayTrap,
    Allocates,
}

impl CpsIntrinsic {
    pub fn arity(self) -> usize {
        match self {
            Self::NatClz
            | Self::NatCtz
            | Self::NatPopcnt
            | Self::NatEqz
            | Self::NatToInt
            | Self::NatToFlt
            | Self::IntClz
            | Self::IntCtz
            | Self::IntPopcnt
            | Self::IntEqz
            | Self::IntToNat
            | Self::IntToFlt
            | Self::FltNeg
            | Self::FltAbs
            | Self::FltSqrt
            | Self::FltFloor
            | Self::FltCeil
            | Self::FltTrunc
            | Self::FltNearest
            | Self::FltToNat
            | Self::FltToLeBytes
            | Self::FltOfLeBytes
            | Self::FltToInt
            | Self::BinLen(_)
            | Self::ListLen
            | Self::TupleGet(_)
            | Self::VariantGet(..)
            | Self::IsImmediate
            | Self::ImmediateGet => 1,
            Self::BinSlice(_) | Self::ListSlice | Self::WindowExtent => 3,
            Self::BinConcat(_, arity)
            | Self::ListConcat(arity)
            | Self::BinChunk(_, arity)
            | Self::ListFlat(arity) => arity,
            Self::ListSettle => 1,
            _ => 2,
        }
    }

    /// What this operation does beyond producing its result, *as emitted* — which is not what it means in the language.
    ///
    /// The `MayTrap` set is the union of two unrelated reasons, and both belong at this layer rather than above it. A division, a float-to-integer conversion, an index and a projection are partial in the language, and `curios-ersd`'s `Semantics` says so too. The arithmetic entries are not: `Nat` addition wraps its `u32` carrier and cannot fail, and it is *this crate's* i31 envelope that makes a result leaving 31 bits trap instead of changing, per `documentation/design/toolchain/numeric-carriers-narrow-by-refusing-never-by-changing-a-value.md`. So every operation `into_wasm` guards belongs here, and none of it may travel upward.
    ///
    /// Exhaustive on purpose. This was a wildcard defaulting to `Total`, which silently classified seven guarded operations as deletable — the same hazard the representation table is exhaustive to avoid, one accessor over.
    pub fn effect(self) -> CpsIntrinsicEffect {
        match self {
            // Partial in the language: a zero divisor, a signed-division overflow, a non-finite or out-of-range conversion, an index or a projection out of bounds, a decode of the wrong length.
            Self::NatDiv
            | Self::NatRem
            | Self::IntDiv
            | Self::IntRem
            | Self::FltToNat
            | Self::FltToInt
            | Self::FltOfLeBytes
            | Self::BinGet(_)
            | Self::BinSlice(_)
            | Self::BinRest(_)
            | Self::ListGet
            | Self::ListSlice
            | Self::ListRest
            | Self::TupleGet(_)
            | Self::VariantGet(..)
            | Self::WindowExtent
            // Total in the language, guarded by the emitter because the result can leave the i31 envelope. `NatSub` is monus and `NatShr`/`IntShr` only clear bits, so neither needs a guard.
            | Self::NatAdd
            | Self::NatMul
            | Self::NatShl
            | Self::NatRotl
            | Self::NatRotr
            | Self::NatToInt
            | Self::IntAdd
            | Self::IntSub
            | Self::IntMul
            | Self::IntShl
            | Self::IntRotl
            | Self::IntRotr
            | Self::IntToNat => CpsIntrinsicEffect::MayTrap,

            // Allocates a *sequence*. An `Flt` result is boxed too, but every `Flt` producer below is treated as total, so the category means a rope or a list rather than any heap traffic at all.
            Self::BinAppend(_)
            | Self::BinConcat(_, _)
            | Self::BinChunk(_, _)
            | Self::ListAppend
            | Self::ListConcat(_)
            | Self::ListSettle
            | Self::ListFlat(_)
            | Self::FltToLeBytes => CpsIntrinsicEffect::Allocates,

            Self::NatEql
            | Self::NatNeq
            | Self::NatSub
            | Self::NatLt
            | Self::NatGt
            | Self::NatLe
            | Self::NatGe
            | Self::NatAnd
            | Self::NatOr
            | Self::NatXor
            | Self::NatShr
            | Self::NatClz
            | Self::NatCtz
            | Self::NatPopcnt
            | Self::NatEqz
            | Self::NatToFlt
            | Self::IntEql
            | Self::IntNeq
            | Self::IntLt
            | Self::IntGt
            | Self::IntLe
            | Self::IntGe
            | Self::IntAnd
            | Self::IntOr
            | Self::IntXor
            | Self::IntShr
            | Self::IntClz
            | Self::IntCtz
            | Self::IntPopcnt
            | Self::IntEqz
            | Self::IntToFlt
            | Self::FltAdd
            | Self::FltSub
            | Self::FltMul
            | Self::FltDiv
            | Self::FltRem
            | Self::FltEql
            | Self::FltNeq
            | Self::FltLt
            | Self::FltGt
            | Self::FltLe
            | Self::FltGe
            | Self::FltMin
            | Self::FltMax
            | Self::FltNeg
            | Self::FltAbs
            | Self::FltSqrt
            | Self::FltFloor
            | Self::FltCeil
            | Self::FltTrunc
            | Self::FltNearest
            | Self::FltCopysign
            | Self::BinLen(_)
            | Self::BinEql(_)
            | Self::ListLen
            | Self::IsImmediate
            | Self::ImmediateGet => CpsIntrinsicEffect::Total,
        }
    }

    pub fn is_total(self) -> bool {
        self.effect() == CpsIntrinsicEffect::Total
    }

    pub fn may_trap(self) -> bool {
        self.effect() == CpsIntrinsicEffect::MayTrap
    }

    pub fn allocates(self) -> bool {
        self.effect() == CpsIntrinsicEffect::Allocates
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
pub enum CpsCallee {
    Known(CpsFunId),
    Closure(CpsValueId),
}

#[derive(Debug, Clone)]
pub struct CpsEdge {
    pub target: CpsContId,
    pub args: Vec<CpsAtom>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CpsCellOp {
    New,
    Set,
    Get,
}

impl CpsCellOp {
    pub fn operand_arity(self) -> usize {
        match self {
            Self::New | Self::Get => 1,
            Self::Set => 2,
        }
    }

    pub fn result_arity(self) -> usize {
        match self {
            Self::New | Self::Get => 1,
            Self::Set => 0,
        }
    }
}

/// A call-like intrinsic. `ListMap` takes the list then the mapper — the carrier-first order of the whole sequence family, matched by the erased representation so the lowering transcribes without reordering — and runs the mapper once per element, in order.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CpsIntrinsicCall {
    ListMap,
}

#[derive(Debug, Clone)]
pub enum CpsNode {
    LetValue {
        result: CpsValueId,
        value: CpsValueExpr,
        next: CpsNodeId,
    },
    LetIntrinsic {
        result: CpsValueId,
        op: CpsIntrinsic,
        args: Vec<CpsAtom>,
        next: CpsNodeId,
    },
    LetFun {
        functions: Vec<CpsFunId>,
        body: CpsNodeId,
    },
    LetCont {
        continuations: Vec<CpsContId>,
        body: CpsNodeId,
    },
    ApplyFun {
        callee: CpsCallee,
        args: Vec<CpsAtom>,
        return_to: CpsContId,
    },
    ApplyCont(CpsEdge),
    Switch {
        scrutinee: CpsAtom,
        cases: BTreeMap<u32, CpsEdge>,
        default: Option<CpsEdge>,
    },
    Foreign {
        function: Arc<ForeignFunction>,
        args: Vec<CpsAtom>,
        return_to: CpsContId,
    },
    Cell {
        op: CpsCellOp,
        args: Vec<CpsAtom>,
        return_to: CpsContId,
    },
    Intrinsic {
        op: CpsIntrinsicCall,
        args: Vec<CpsAtom>,
        return_to: CpsContId,
    },
    Exit {
        value: Option<CpsAtom>,
    },
    Unreachable,
    RecInit {
        functions: Vec<CpsFunId>,
        values: Vec<CpsValueId>,
        ready: CpsNodeId,
        body: CpsNodeId,
    },
}

#[derive(Debug, Clone)]
pub struct CpsValueDef {
    pub debug_name: Option<String>,
}

#[derive(Debug, Clone)]
pub struct CpsFunction {
    pub debug_name: Option<String>,
    pub params: Vec<CpsValueId>,
    pub return_cont: CpsContId,
    pub body: CpsNodeId,
}

#[derive(Debug, Clone)]
pub struct CpsContinuation {
    pub debug_name: Option<String>,
    pub params: Vec<CpsValueId>,
    pub body: CpsNodeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum CpsUseTarget {
    Value(CpsValueId),
    Fun(CpsFunId),
    Cont(CpsContId),
}

/// What the module's functions state about returning: which continuation is whose sentinel, and how many values each hands back.
///
/// The two travel together because every arity question needs both — whether a transfer is a return at all, and how wide a return is — so they are one parameter rather than two threaded in parallel through the verifier.
struct ReturnFacts<'a> {
    owners: &'a BTreeMap<CpsContId, CpsFunId>,
    arities: &'a BTreeMap<CpsFunId, usize>,
}

impl ReturnFacts<'_> {
    /// How many values `function` returns, reading absence as the single value a function carried before any protocol widened it.
    fn arity(&self, function: CpsFunId) -> usize {
        self.arities.get(&function).copied().unwrap_or(1)
    }
}

#[derive(Debug, Clone)]
pub struct CpsVerifyError(pub String);

impl fmt::Display for CpsVerifyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for CpsVerifyError {}

/// The recorded fields representation: `width` consecutive parameters of a continuation, starting at `start`, that *are* the fields of one former aggregate parameter.
///
/// The record is what makes a split a fact of the program rather than a convention between passes: [`CpsModule::verify`] holds every group to its continuation's parameter list the way it already holds arities, so a pass that reshapes a recorded parameter list without maintaining the record fails loudly instead of silently disagreeing with the split.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FieldGroup {
    pub start: usize,
    pub width: usize,
}

/// The production Cont representation. Arena slots never move or get reused; deletion writes `None` and deterministic compaction is explicit.
#[derive(Debug, Clone, Default)]
pub struct CpsModule {
    nodes: Arena<CpsNodeId, CpsNode>,
    values: Arena<CpsValueId, CpsValueDef>,
    functions: Arena<CpsFunId, CpsFunction>,
    continuations: Arena<CpsContId, CpsContinuation>,
    field_groups: BTreeMap<CpsContId, Vec<FieldGroup>>,
    /// The variant families this module's [`CpsValueExpr::Variant`]s belong to, appended by the Ersd door and never removed — a family that loses its last construction is simply an unreferenced row, so the ids stay stable without tombstones.
    families: Vec<CpsFamily>,
    entry: Option<CpsFunId>,
}

/// One variant family: its debug name, and the carrier of every slot of its heap type — slot zero the tag, the rest the payload row every [`CpsValueExpr::Variant`] of the family is padded to.
#[derive(Debug, Clone)]
pub struct CpsFamily {
    pub debug_name: Option<String>,
    pub slots: Vec<CpsSlot>,
}

impl CpsFamily {
    /// The arity every construction of this family carries.
    pub fn width(&self) -> usize {
        self.slots.len()
    }
}

/// What one slot of a family's heap type holds.
///
/// The door decides this from the erased shape recorded on each constructor's fields, and it is the whole point of keying a heap type by family: an arity-keyed type is shared by every constructor of that arity module-wide, so the join over any slot's stores is the top type and nothing can be said about it. A family's slots are written by that family alone, so a slot whose every writer agrees names a carrier — a register for the scalars, a declared heap type for the shapes — and the emitter declares the wasm field at it.
///
/// Slots are assigned by carrier rather than by field position, which is what keeps the family from widening: a constructor's fields are distributed into the slot range their carrier owns, so two constructors sharing a carrier share its slots and only a disagreement costs width. Positional assignment would have been free but types almost nothing — over the standard library it settles 11 slots against this rule's 22 — while giving each constructor a disjoint range types only five more and costs 18 slots more than this.
///
/// Three shapes stay [`CpsSlot::Opaque`] deliberately. A packed carrier is *sometimes* an immediate, so no single heap type names its population. A closure's runtime arity is not something the recorded shape is yet entitled to promise, since the erased arity is read off the declared type and the passes above may raise it. A family-typed field would need the field's family identity, which erasure does not record.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum CpsSlot {
    /// The discriminant. Stored packed and read unsigned, since a family's constructor count is bounded far below the byte the tag occupies.
    Tag,
    /// A raw unsigned 32-bit payload.
    Nat,
    /// A raw signed 32-bit payload.
    Int,
    /// A raw binary32 payload — the one slot that deletes an allocation rather than a coercion, since the boxed `Flt` it replaces is a heap object of its own.
    Flt,
    /// A list rope. The base type is not final, so this is the slot that deletes an `is_subtype` libcall rather than an inline check.
    List,
    /// A boxed product row at the given relevant width.
    Product(usize),
    /// The uniform reference: a polymorphic payload, or one whose shape names no single heap type.
    Opaque,
}

impl CpsSlot {
    /// The representation a read of this slot produces.
    pub fn repr(self) -> Repr {
        match self {
            CpsSlot::Tag | CpsSlot::Nat => Repr::Nat,
            CpsSlot::Int => Repr::Int,
            CpsSlot::Flt => Repr::Flt,
            CpsSlot::List => Repr::List,
            CpsSlot::Product(_) | CpsSlot::Opaque => Repr::Ref,
        }
    }
}

impl CpsModule {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn entry(&self) -> Option<CpsFunId> {
        self.entry
    }

    /// The recorded fields representations, by continuation.
    pub fn field_groups(&self) -> &BTreeMap<CpsContId, Vec<FieldGroup>> {
        &self.field_groups
    }

    /// Register a variant family and hand back its identity. The Ersd door is the only caller; see [`CpsValueExpr::Variant`].
    pub fn add_family(&mut self, family: CpsFamily) -> CpsFamilyId {
        let id = CpsFamilyId::from_index(self.families.len());
        self.families.push(family);
        id
    }

    pub fn family(&self, id: CpsFamilyId) -> &CpsFamily {
        &self.families[id.index()]
    }

    /// The representation a read of `family`'s slot at `index` produces. The one result representation that is a fact of the module rather than of the operation, which is why [`CpsIntrinsic::result_repr`] cannot answer it alone.
    pub fn slot_repr(&self, family: CpsFamilyId, index: usize) -> Repr {
        self.families[family.index()].slots[index].repr()
    }

    /// The representation `op` produces, resolving a family read against this module's slot carriers.
    pub fn result_repr(&self, op: &CpsIntrinsic) -> Repr {
        match op {
            CpsIntrinsic::VariantGet(family, index) => self.slot_repr(*family, *index),
            _ => op.result_repr(),
        }
    }

    pub fn families(&self) -> impl Iterator<Item = (CpsFamilyId, &CpsFamily)> {
        self.families
            .iter()
            .enumerate()
            .map(|(index, family)| (CpsFamilyId::from_index(index), family))
    }

    /// Record that `continuation`'s parameter at `start` was spliced into `width` fields: the new group, *and* every group past it shifted by the parameters the splice added.
    ///
    /// Recording and shifting are one operation because they are one fact. They were two, and the shift lived in the one caller that had needed it so far — which left every other caller silently recording stale starts, reachable as soon as two parameters of one continuation were split in the same pass. Groups are kept sorted by start; [`CpsModule::verify`] holds them to the parameter list.
    pub fn record_split(&mut self, continuation: CpsContId, start: usize, width: usize) {
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
        continuation: CpsContId,
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

    pub fn set_entry(&mut self, entry: CpsFunId) {
        self.entry = Some(entry);
    }

    pub fn nodes(&self) -> &[Option<CpsNode>] {
        self.nodes.slots()
    }

    pub fn values(&self) -> &[Option<CpsValueDef>] {
        self.values.slots()
    }

    pub fn functions(&self) -> &[Option<CpsFunction>] {
        self.functions.slots()
    }

    pub fn continuations(&self) -> &[Option<CpsContinuation>] {
        self.continuations.slots()
    }

    pub fn node(&self, id: CpsNodeId) -> Option<&CpsNode> {
        self.nodes.get(id)
    }

    pub fn function(&self, id: CpsFunId) -> Option<&CpsFunction> {
        self.functions.get(id)
    }

    pub fn continuation(&self, id: CpsContId) -> Option<&CpsContinuation> {
        self.continuations.get(id)
    }

    /// Count, per value, how many times it is referenced across the module. A value's use sites are its operand occurrences plus its use as an indirect callee; definitions (`LetValue`/`LetIntrinsic` results, parameters) are not uses, so an unreferenced value is absent from the map. Derived on demand rather than maintained incrementally.
    pub(crate) fn value_use_counts(&self) -> BTreeMap<CpsValueId, usize> {
        let mut counts = BTreeMap::new();
        for (_, node) in self.nodes.iter_live() {
            for atom in atoms(node) {
                if let CpsAtom::Value(value) = atom {
                    *counts.entry(*value).or_insert(0) += 1;
                }
            }
            if let CpsNode::ApplyFun {
                callee: CpsCallee::Closure(value),
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
    /// Where a function has both a return edge and a constrained tail position, the edge is taken and the disagreement is left to [`CpsModule::verify`], whose business it is to report rather than to paper over.
    pub(crate) fn return_arities(&self) -> BTreeMap<CpsFunId, usize> {
        let mut settled = BTreeMap::<CpsFunId, usize>::new();
        let mut inherits = BTreeMap::<CpsFunId, BTreeSet<CpsFunId>>::new();

        for (function, definition) in self.functions.iter_live() {
            let sentinel = definition.return_cont;
            let mut edges = None;
            let mut operation = None;
            let mut tail_calls = BTreeSet::new();

            for node_id in analysis::nodes_from(self, definition.body) {
                let mut returning = |edge: &CpsEdge| {
                    if edge.target == sentinel {
                        edges.get_or_insert(edge.args.len());
                    }
                };
                match self.node(node_id).unwrap() {
                    CpsNode::ApplyCont(edge) => returning(edge),
                    CpsNode::Switch { cases, default, .. } => {
                        cases.values().chain(default.as_ref()).for_each(returning);
                    }
                    CpsNode::ApplyFun {
                        callee,
                        return_to: to,
                        ..
                    } if *to == sentinel => match callee {
                        CpsCallee::Known(callee) => {
                            tail_calls.insert(*callee);
                        }
                        CpsCallee::Closure(_) => operation = operation.or(Some(1)),
                    },
                    CpsNode::Foreign {
                        function,
                        return_to,
                        ..
                    } if *return_to == sentinel => {
                        operation = operation.or(Some(function.signature.results.len()));
                    }
                    CpsNode::Cell { op, return_to, .. } if *return_to == sentinel => {
                        operation = operation.or(Some(op.result_arity()));
                    }
                    CpsNode::Intrinsic { return_to, .. } if *return_to == sentinel => {
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

    pub fn reserve_node(&mut self) -> CpsNodeId {
        self.nodes.reserve()
    }

    pub fn add_node(&mut self, node: CpsNode) -> CpsNodeId {
        let id = self.reserve_node();
        self.define_node(id, node);
        id
    }

    pub fn define_node(&mut self, id: CpsNodeId, node: CpsNode) {
        self.nodes.define(id, node);
    }

    pub fn add_value(&mut self, debug_name: Option<String>) -> CpsValueId {
        self.values.mint(CpsValueDef { debug_name })
    }

    pub fn reserve_function(&mut self) -> CpsFunId {
        self.functions.reserve()
    }

    pub fn define_function(&mut self, id: CpsFunId, function: CpsFunction) {
        self.functions.define(id, function);
    }

    pub fn add_function(&mut self, function: CpsFunction) -> CpsFunId {
        self.functions.mint(function)
    }

    pub fn reserve_continuation(&mut self) -> CpsContId {
        self.continuations.reserve()
    }

    pub fn define_continuation(&mut self, id: CpsContId, continuation: CpsContinuation) {
        self.continuations.define(id, continuation);
    }

    pub fn add_continuation(&mut self, continuation: CpsContinuation) -> CpsContId {
        self.continuations.mint(continuation)
    }

    pub fn remove_node(&mut self, id: CpsNodeId) -> Option<CpsNode> {
        self.nodes.remove(id)
    }

    pub fn replace_atom(&mut self, from: CpsUseTarget, replacement: CpsAtom) {
        for (_, node) in self.nodes.iter_live_mut() {
            visit_atoms_mut(node, &mut |atom| {
                let matches = match (&from, &*atom) {
                    (CpsUseTarget::Value(a), CpsAtom::Value(b)) => a == b,
                    (CpsUseTarget::Fun(a), CpsAtom::Fun(b)) => a == b,
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
                    slot.is_none() && !return_continuations.contains(&CpsContId(*index as u32))
                })
                .count(),
        )
    }

    pub fn verify(&self) -> Result<(), CpsVerifyError> {
        let entry = self
            .entry
            .ok_or_else(|| CpsVerifyError("module has no entry function".into()))?;
        self.require_fun(entry, "entry")?;

        let mut returns = BTreeMap::<CpsContId, CpsFunId>::new();
        for (id, function) in self.functions.iter_live() {
            if function.return_cont.index() >= self.continuations.len() {
                return Err(CpsVerifyError(format!(
                    "{id} return continuation {} was not minted by this module",
                    function.return_cont
                )));
            }
            if self.continuation(function.return_cont).is_some() {
                return Err(CpsVerifyError(format!(
                    "{id} return continuation {} also identifies a local continuation",
                    function.return_cont
                )));
            }
            if let Some(previous) = returns.insert(function.return_cont, id) {
                return Err(CpsVerifyError(format!(
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
        let mut node_owners = BTreeMap::<CpsNodeId, CpsFunId>::new();
        let mut bound_continuations = BTreeSet::<CpsContId>::new();
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
        self.verify_families()?;

        let live_nodes = self.nodes.live_ids().collect::<BTreeSet<_>>();
        let owned_nodes = node_owners.keys().copied().collect::<BTreeSet<_>>();
        if live_nodes != owned_nodes {
            return Err(CpsVerifyError(
                "node arena contains an unowned node or an owner references a tombstone".into(),
            ));
        }

        let live_continuations = self.continuations.live_ids().collect::<BTreeSet<_>>();
        if live_continuations != bound_continuations {
            return Err(CpsVerifyError(
                "local-continuation arena and lexical LetCont bindings disagree".into(),
            ));
        }

        // The recorded fields representations hold: every group names a live continuation and lies inside its parameter list without overlapping a neighbour, so a pass that reshaped a recorded parameter list without maintaining the record fails here rather than silently disagreeing with the split.
        for (continuation, groups) in &self.field_groups {
            let Some(definition) = self.continuation(*continuation) else {
                return Err(CpsVerifyError(format!(
                    "field group records dead continuation {continuation}"
                )));
            };
            let mut end = 0;
            for group in groups {
                if group.width == 0 {
                    return Err(CpsVerifyError(format!(
                        "{continuation} records an empty field group at {}",
                        group.start
                    )));
                }
                if group.start < end {
                    return Err(CpsVerifyError(format!(
                        "{continuation} records overlapping field groups at {}",
                        group.start
                    )));
                }
                end = group.start + group.width;
            }
            if end > definition.params.len() {
                return Err(CpsVerifyError(format!(
                    "{continuation} records a field group past its {} parameters",
                    definition.params.len()
                )));
            }
        }

        Ok(())
    }

    /// The family vocabulary's coherence: every family named by a construction or a read exists, every construction carries exactly its family's width, and every read is in range of it.
    ///
    /// This is what the distinct [`CpsValueExpr::Variant`] buys over an annotation on `Tuple`. A family value read at a structural projection, or a construction one slot short of its family, would be a `ref.cast` trap in emitted code far from the pass that caused it; here it is a verifier failure at the boundary that produced it. Padding is the door's job, so a mismatch is always a compiler bug rather than a program's.
    fn verify_families(&self) -> Result<(), CpsVerifyError> {
        for (_, node) in self.nodes.iter_live() {
            match node {
                CpsNode::LetValue {
                    value: CpsValueExpr::Variant(family, atoms),
                    ..
                } => {
                    let Some(definition) = self.families.get(family.index()) else {
                        return Err(CpsVerifyError(format!(
                            "variant construction names {family}, which was not minted by this module"
                        )));
                    };
                    if atoms.len() != definition.width() {
                        return Err(CpsVerifyError(format!(
                            "variant construction of {family} carries {} slots, but the family is {} wide",
                            atoms.len(),
                            definition.width(),
                        )));
                    }
                }
                CpsNode::LetIntrinsic {
                    op: CpsIntrinsic::VariantGet(family, index),
                    ..
                } => {
                    let Some(definition) = self.families.get(family.index()) else {
                        return Err(CpsVerifyError(format!(
                            "variant read names {family}, which was not minted by this module"
                        )));
                    };
                    if *index >= definition.width() {
                        return Err(CpsVerifyError(format!(
                            "variant read of {family} at slot {index}, but the family is {} wide",
                            definition.width(),
                        )));
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn verify_lexical_scopes(&self, entry: CpsFunId) -> Result<(), CpsVerifyError> {
        type NodeTask = (
            CpsFunId,
            CpsNodeId,
            BTreeSet<CpsValueId>,
            BTreeSet<CpsFunId>,
            BTreeSet<CpsContId>,
        );

        let mut bound_functions = BTreeSet::from([entry]);
        let mut bound_values = BTreeSet::new();
        let mut pending_recursive_values = BTreeSet::new();
        let mut function_work = vec![(entry, BTreeSet::new(), BTreeSet::from([entry]))];
        let mut node_work = Vec::<NodeTask>::new();
        let mut visited_nodes = BTreeSet::new();

        while !function_work.is_empty() || !node_work.is_empty() {
            while let Some((function, mut values, functions)) = function_work.pop() {
                let definition = self.function(function).unwrap();
                for value in &definition.params {
                    if !bound_values.insert(*value) {
                        return Err(CpsVerifyError(format!(
                            "function parameter {value} is bound more than once"
                        )));
                    }
                    values.insert(*value);
                }
                node_work.push((
                    function,
                    definition.body,
                    values,
                    functions,
                    BTreeSet::new(),
                ));
            }

            let Some((owner, node_id, values, functions, continuations)) = node_work.pop() else {
                continue;
            };
            if !visited_nodes.insert(node_id) {
                continue;
            }
            let node = self.node(node_id).unwrap();
            for atom in atoms(node) {
                match atom {
                    CpsAtom::Value(value) if !values.contains(value) => {
                        return Err(CpsVerifyError(format!(
                            "{owner} node {node_id} uses out-of-scope {value}"
                        )));
                    }
                    CpsAtom::Fun(function) if !functions.contains(function) => {
                        return Err(CpsVerifyError(format!(
                            "{owner} node {node_id} uses out-of-scope {function}"
                        )));
                    }
                    CpsAtom::Value(_) | CpsAtom::Fun(_) | CpsAtom::Literal(_) | CpsAtom::Filler => {
                    }
                }
            }
            if let CpsNode::ApplyFun { callee, .. } = node {
                match callee {
                    CpsCallee::Known(function) if !functions.contains(function) => {
                        return Err(CpsVerifyError(format!(
                            "{owner} node {node_id} calls out-of-scope {function}"
                        )));
                    }
                    CpsCallee::Closure(value) if !values.contains(value) => {
                        return Err(CpsVerifyError(format!(
                            "{owner} node {node_id} calls out-of-scope {value}"
                        )));
                    }
                    CpsCallee::Known(_) | CpsCallee::Closure(_) => {}
                }
            }

            match node {
                CpsNode::LetValue { result, next, .. }
                | CpsNode::LetIntrinsic { result, next, .. } => {
                    if !bound_values.insert(*result) {
                        return Err(CpsVerifyError(format!(
                            "node result {result} is bound more than once"
                        )));
                    }
                    let mut inner = values;
                    inner.insert(*result);
                    node_work.push((owner, *next, inner, functions, continuations));
                }
                CpsNode::LetFun {
                    functions: members,
                    body,
                } => {
                    let mut inner = functions;
                    for function in members {
                        if !bound_functions.insert(*function) {
                            return Err(CpsVerifyError(format!(
                                "function {function} is bound more than once"
                            )));
                        }
                        inner.insert(*function);
                    }
                    for function in members.iter().rev() {
                        function_work.push((*function, values.clone(), inner.clone()));
                    }
                    node_work.push((owner, *body, values, inner, continuations));
                }
                CpsNode::RecInit {
                    functions: members,
                    values: recursive_values,
                    body,
                    ..
                } => {
                    let mut inner_functions = functions;
                    for function in members {
                        if !bound_functions.insert(*function) {
                            return Err(CpsVerifyError(format!(
                                "function {function} is bound more than once"
                            )));
                        }
                        inner_functions.insert(*function);
                    }
                    let mut inner_values = values;
                    for value in recursive_values {
                        if !bound_values.insert(*value) {
                            return Err(CpsVerifyError(format!(
                                "recursive value {value} is bound more than once"
                            )));
                        }
                        pending_recursive_values.insert(*value);
                        inner_values.insert(*value);
                    }
                    for function in members.iter().rev() {
                        function_work.push((
                            *function,
                            inner_values.clone(),
                            inner_functions.clone(),
                        ));
                    }
                    node_work.push((owner, *body, inner_values, inner_functions, continuations));
                }
                CpsNode::LetCont {
                    continuations: members,
                    body,
                } => {
                    let mut inner = continuations;
                    inner.extend(members.iter().copied());
                    for continuation in members.iter().rev() {
                        let definition = self.continuation(*continuation).unwrap();
                        let mut continuation_values = values.clone();
                        for value in &definition.params {
                            if !bound_values.insert(*value)
                                && !pending_recursive_values.remove(value)
                            {
                                return Err(CpsVerifyError(format!(
                                    "continuation parameter {value} is bound more than once"
                                )));
                            }
                            continuation_values.insert(*value);
                        }
                        node_work.push((
                            owner,
                            definition.body,
                            continuation_values,
                            functions.clone(),
                            inner.clone(),
                        ));
                    }
                    node_work.push((owner, *body, values, functions, inner));
                }
                CpsNode::ApplyFun { .. }
                | CpsNode::ApplyCont(_)
                | CpsNode::Switch { .. }
                | CpsNode::Foreign { .. }
                | CpsNode::Cell { .. }
                | CpsNode::Intrinsic { .. }
                | CpsNode::Exit { .. }
                | CpsNode::Unreachable => {}
            }
        }

        let live_functions = self.functions.live_ids().collect::<BTreeSet<_>>();
        if live_functions != bound_functions {
            return Err(CpsVerifyError(
                "function arena and lexical function bindings disagree".into(),
            ));
        }
        let live_values = self.values.live_ids().collect::<BTreeSet<_>>();
        if live_values != bound_values {
            return Err(CpsVerifyError(
                "value arena and lexical value bindings disagree".into(),
            ));
        }
        if !pending_recursive_values.is_empty() {
            return Err(CpsVerifyError(
                "recursive initializer value lacks its computed binding".into(),
            ));
        }
        Ok(())
    }

    fn verify_function_body(
        &self,
        owner: CpsFunId,
        function: &CpsFunction,
        facts: &ReturnFacts<'_>,
        node_owners: &mut BTreeMap<CpsNodeId, CpsFunId>,
        bound_continuations: &mut BTreeSet<CpsContId>,
    ) -> Result<(), CpsVerifyError> {
        let mut work = vec![(function.body, BTreeSet::<CpsContId>::new())];
        let mut visited = BTreeSet::<CpsNodeId>::new();

        while let Some((id, scope)) = work.pop() {
            if !visited.insert(id) {
                continue;
            }
            if let Some(previous) = node_owners.insert(id, owner)
                && previous != owner
            {
                return Err(CpsVerifyError(format!(
                    "{id} is owned by both {previous} and {owner}"
                )));
            }
            let node = self
                .node(id)
                .ok_or_else(|| CpsVerifyError(format!("function body references missing {id}")))?;
            self.verify_node(owner, function.return_cont, facts, &scope, id, node)?;

            match node {
                CpsNode::LetValue { next, .. } | CpsNode::LetIntrinsic { next, .. } => {
                    work.push((*next, scope));
                }
                CpsNode::LetFun { body, .. } | CpsNode::RecInit { body, .. } => {
                    work.push((*body, scope));
                }
                CpsNode::LetCont {
                    continuations,
                    body,
                } => {
                    let mut inner = scope;
                    for &continuation in continuations {
                        if facts.owners.contains_key(&continuation) {
                            return Err(CpsVerifyError(format!(
                                "return ID {continuation} cannot be bound as a local continuation"
                            )));
                        }
                        self.require_cont(continuation, "LetCont member")?;
                        if !bound_continuations.insert(continuation) {
                            return Err(CpsVerifyError(format!(
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
                CpsNode::ApplyFun { .. }
                | CpsNode::ApplyCont(_)
                | CpsNode::Switch { .. }
                | CpsNode::Foreign { .. }
                | CpsNode::Cell { .. }
                | CpsNode::Intrinsic { .. }
                | CpsNode::Exit { .. }
                | CpsNode::Unreachable => {}
            }
        }
        Ok(())
    }

    fn verify_node(
        &self,
        current_function: CpsFunId,
        return_cont: CpsContId,
        facts: &ReturnFacts<'_>,
        scope: &BTreeSet<CpsContId>,
        id: CpsNodeId,
        node: &CpsNode,
    ) -> Result<(), CpsVerifyError> {
        match node {
            CpsNode::LetValue { result, next, .. } => {
                self.require_value(*result, "let-value result")?;
                self.require_node(*next, "let-value successor")?;
            }
            CpsNode::LetIntrinsic {
                result,
                op,
                args,
                next,
            } => {
                self.require_value(*result, "let-intrinsic result")?;
                self.require_node(*next, "let-intrinsic successor")?;
                if args.len() != op.arity() {
                    return Err(CpsVerifyError(format!(
                        "{id} intrinsic {op:?} expects {} operands, got {}",
                        op.arity(),
                        args.len()
                    )));
                }
            }
            CpsNode::LetFun { functions, body } => {
                for &function in functions {
                    self.require_fun(function, "let-fun member")?;
                }
                self.require_node(*body, "let-fun body")?;
            }
            CpsNode::LetCont {
                continuations,
                body,
            } => {
                for &continuation in continuations {
                    self.require_cont(continuation, "let-cont member")?;
                }
                self.require_node(*body, "let-cont body")?;
            }
            CpsNode::ApplyFun {
                callee,
                args,
                return_to,
            } => {
                match callee {
                    CpsCallee::Known(function) => {
                        self.require_fun(*function, "known callee")?;
                        let arity = self.function(*function).unwrap().params.len();
                        if arity != args.len() {
                            return Err(CpsVerifyError(format!(
                                "{id} calls {function} with {} arguments; expected {arity}",
                                args.len()
                            )));
                        }
                    }
                    CpsCallee::Closure(value) => self.require_value(*value, "closure callee")?,
                }
                // A closure is reached through the shared type of its arity, which carries one result whatever the function behind it returns.
                let results = match callee {
                    CpsCallee::Known(function) => facts.arity(*function),
                    CpsCallee::Closure(_) => 1,
                };
                let params = self.continuation_arity(
                    current_function,
                    return_cont,
                    facts,
                    scope,
                    *return_to,
                )?;
                if params != results {
                    return Err(CpsVerifyError(format!(
                        "{id} user call return continuation {return_to} accepts {params} values, callee returns {results}"
                    )));
                }
            }
            CpsNode::ApplyCont(edge) => {
                self.verify_edge(current_function, return_cont, facts, scope, id, edge)?
            }
            CpsNode::Switch { cases, default, .. } => {
                for edge in cases.values() {
                    self.verify_edge(current_function, return_cont, facts, scope, id, edge)?;
                }
                if let Some(edge) = default {
                    self.verify_edge(current_function, return_cont, facts, scope, id, edge)?;
                }
            }
            CpsNode::Foreign {
                function,
                args,
                return_to,
            } => {
                if args.len() != function.signature.params.len() {
                    return Err(CpsVerifyError(format!(
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
                    return Err(CpsVerifyError(format!(
                        "{id} foreign return continuation expects {params} values, call returns {results}"
                    )));
                }
            }
            CpsNode::Cell {
                op,
                args,
                return_to,
            } => {
                if args.len() != op.operand_arity() {
                    return Err(CpsVerifyError(format!(
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
                    return Err(CpsVerifyError(format!(
                        "{id} cell {op:?} continuation arity mismatch"
                    )));
                }
            }
            CpsNode::Intrinsic {
                op: CpsIntrinsicCall::ListMap,
                args,
                return_to,
            } => {
                if args.len() != 2 {
                    return Err(CpsVerifyError(format!(
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
                    return Err(CpsVerifyError(format!(
                        "{id} ListMap continuation must accept one value"
                    )));
                }
            }
            CpsNode::Exit { .. } | CpsNode::Unreachable => {}
            CpsNode::RecInit {
                functions,
                values,
                ready,
                body,
            } => {
                if functions.is_empty() || values.is_empty() {
                    return Err(CpsVerifyError(format!(
                        "{id} recursive initializer must be a mixed function/value group"
                    )));
                }
                for &function in functions {
                    self.require_fun(function, "recursive initializer function")?;
                }
                for &value in values {
                    self.require_value(value, "recursive initializer value")?;
                }
                self.require_node(*ready, "recursive initializer ready point")?;
                self.require_node(*body, "recursive initializer body")?;
            }
        }

        for atom in atoms(node) {
            match atom {
                CpsAtom::Value(value) => self.require_value(*value, "operand")?,
                CpsAtom::Fun(function) => self.require_fun(*function, "function atom")?,
                CpsAtom::Literal(_) | CpsAtom::Filler => {}
            }
        }
        Ok(())
    }

    /// Check one transfer's argument count against its target's. A return edge is covered by the same rule, `self_arity` being the arity [`CpsModule::return_arities`] read off the function's own edges — so an edge that disagrees with its siblings is reported here rather than reaching the emitter.
    fn verify_edge(
        &self,
        function: CpsFunId,
        return_cont: CpsContId,
        facts: &ReturnFacts<'_>,
        scope: &BTreeSet<CpsContId>,
        owner: CpsNodeId,
        edge: &CpsEdge,
    ) -> Result<(), CpsVerifyError> {
        let arity = self.continuation_arity(function, return_cont, facts, scope, edge.target)?;
        if arity != edge.args.len() {
            return Err(CpsVerifyError(format!(
                "{owner} edge to {} carries {} arguments; expected {arity}",
                edge.target,
                edge.args.len()
            )));
        }
        Ok(())
    }

    /// How many values a transfer to `target` carries, `self_arity` being what a transfer to the enclosing function's own sentinel carries — its return.
    fn continuation_arity(
        &self,
        function: CpsFunId,
        return_cont: CpsContId,
        facts: &ReturnFacts<'_>,
        scope: &BTreeSet<CpsContId>,
        target: CpsContId,
    ) -> Result<usize, CpsVerifyError> {
        if target == return_cont {
            return Ok(facts.arity(function));
        }
        if let Some(owner) = facts.owners.get(&target) {
            return Err(CpsVerifyError(format!(
                "{function} references {owner}'s return continuation {target}"
            )));
        }
        if !scope.contains(&target) {
            return Err(CpsVerifyError(format!(
                "{function} references undefined or out-of-scope continuation {target}"
            )));
        }
        self.continuation(target)
            .map(|continuation| continuation.params.len())
            .ok_or_else(|| CpsVerifyError(format!("undefined non-return continuation {target}")))
    }

    fn require_node(&self, id: CpsNodeId, what: &str) -> Result<(), CpsVerifyError> {
        self.node(id)
            .map(|_| ())
            .ok_or_else(|| CpsVerifyError(format!("{what} references missing {id}")))
    }

    fn require_value(&self, id: CpsValueId, what: &str) -> Result<(), CpsVerifyError> {
        self.values
            .get(id)
            .map(|_| ())
            .ok_or_else(|| CpsVerifyError(format!("{what} references missing {id}")))
    }

    fn require_fun(&self, id: CpsFunId, what: &str) -> Result<(), CpsVerifyError> {
        self.function(id)
            .map(|_| ())
            .ok_or_else(|| CpsVerifyError(format!("{what} references missing {id}")))
    }

    fn require_cont(&self, id: CpsContId, what: &str) -> Result<(), CpsVerifyError> {
        self.continuation(id)
            .map(|_| ())
            .ok_or_else(|| CpsVerifyError(format!("{what} references missing {id}")))
    }
}

pub(crate) fn atoms(node: &CpsNode) -> Vec<&CpsAtom> {
    let mut output = Vec::new();
    match node {
        CpsNode::LetValue { value, .. } => match value {
            CpsValueExpr::Literal(_) => {}
            CpsValueExpr::List(values)
            | CpsValueExpr::Tuple(values)
            | CpsValueExpr::Variant(_, values) => output.extend(values),
        },
        CpsNode::LetIntrinsic { args, .. }
        | CpsNode::ApplyFun { args, .. }
        | CpsNode::Foreign { args, .. }
        | CpsNode::Cell { args, .. }
        | CpsNode::Intrinsic { args, .. } => output.extend(args),
        CpsNode::ApplyCont(edge) => output.extend(&edge.args),
        CpsNode::Switch {
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
        CpsNode::Exit { value, .. } => output.extend(value),
        CpsNode::LetFun { .. }
        | CpsNode::LetCont { .. }
        | CpsNode::Unreachable
        | CpsNode::RecInit { .. } => {}
    }
    output
}

pub(crate) fn visit_atoms_mut(node: &mut CpsNode, visitor: &mut impl FnMut(&mut CpsAtom)) {
    match node {
        CpsNode::LetValue { value, .. } => match value {
            CpsValueExpr::Literal(_) => {}
            CpsValueExpr::List(values)
            | CpsValueExpr::Tuple(values)
            | CpsValueExpr::Variant(_, values) => values.iter_mut().for_each(visitor),
        },
        CpsNode::LetIntrinsic { args, .. }
        | CpsNode::ApplyFun { args, .. }
        | CpsNode::Foreign { args, .. }
        | CpsNode::Cell { args, .. }
        | CpsNode::Intrinsic { args, .. } => args.iter_mut().for_each(visitor),
        CpsNode::ApplyCont(edge) => edge.args.iter_mut().for_each(visitor),
        CpsNode::Switch {
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
        CpsNode::Exit { value, .. } => {
            if let Some(value) = value {
                visitor(value);
            }
        }
        CpsNode::LetFun { .. }
        | CpsNode::LetCont { .. }
        | CpsNode::Unreachable
        | CpsNode::RecInit { .. } => {}
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
mod protocol;
mod reachable;
pub(crate) mod represent;
mod simplify;
mod specialize;
mod uncurry;
pub(crate) use dataflow::*;
pub(crate) use demand::*;
pub use optimize::optimize;
pub(crate) use origin::*;

impl fmt::Display for CpsModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "entry {}",
            self.entry
                .map_or_else(|| "<none>".into(), |id| id.to_string())
        )?;
        for (id, function) in self.functions.iter_live() {
            write!(f, "fun {id}")?;
            if let Some(name) = &function.debug_name {
                write!(f, "${name}")?;
            }
            write!(f, "(")?;
            params(self, f, &function.params)?;
            writeln!(f, ") -> {} = {}", function.return_cont, function.body)?;
        }
        for (id, continuation) in self.continuations.iter_live() {
            write!(f, "cont {id}(")?;
            params(self, f, &continuation.params)?;
            writeln!(f, ") = {}", continuation.body)?;
        }
        for (id, node) in self.nodes.iter_live() {
            writeln!(f, "{id} = {}", CpsDisplayNode(node))?;
        }
        Ok(())
    }
}

/// Render a parameter list, spelling each binder's source hint as `$name` — the definition-site form that matches function names and the wasm scheme, so a value's origin is legible where it is bound. A binder with no hint prints bare.
fn params(module: &CpsModule, f: &mut fmt::Formatter<'_>, params: &[CpsValueId]) -> fmt::Result {
    for (index, &param) in params.iter().enumerate() {
        if index != 0 {
            write!(f, ", ")?;
        }
        write!(f, "{param}")?;
        if let Some(name) = module
            .values()
            .get(param.index())
            .and_then(Option::as_ref)
            .and_then(|def| def.debug_name.as_ref())
        {
            write!(f, "${name}")?;
        }
    }
    Ok(())
}

struct CpsDisplayNode<'a>(&'a CpsNode);

impl fmt::Display for CpsDisplayNode<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            CpsNode::LetValue {
                result,
                value,
                next,
            } => write!(f, "let {result} = {value:?}; {next}"),
            CpsNode::LetIntrinsic {
                result,
                op,
                args,
                next,
            } => {
                write!(f, "let {result} = {op:?}{args:?}; {next}")
            }
            CpsNode::LetFun { functions, body } => write!(f, "let-fun {functions:?}; {body}"),
            CpsNode::LetCont {
                continuations,
                body,
            } => write!(f, "let-cont {continuations:?}; {body}"),
            CpsNode::ApplyFun {
                callee,
                args,
                return_to,
            } => {
                write!(f, "apply {callee:?}{args:?} -> {return_to}")
            }
            CpsNode::ApplyCont(edge) => write!(f, "jump {}{:?}", edge.target, edge.args),
            CpsNode::Switch {
                scrutinee,
                cases,
                default,
            } => {
                write!(f, "switch {scrutinee:?} {cases:?} default {default:?}")
            }
            CpsNode::Foreign {
                function,
                args,
                return_to,
            } => {
                write!(f, "foreign {}{args:?} -> {return_to}", function.name)
            }
            CpsNode::Cell {
                op,
                args,
                return_to,
            } => write!(f, "cell.{op:?}{args:?} -> {return_to}"),
            CpsNode::Intrinsic {
                op,
                args,
                return_to,
            } => {
                write!(f, "intrinsic.{op:?}{args:?} -> {return_to}")
            }
            CpsNode::Exit { value } => write!(f, "exit {value:?}"),
            CpsNode::Unreachable => f.write_str("unreachable"),
            CpsNode::RecInit {
                functions,
                values,
                ready,
                body,
            } => {
                write!(f, "rec-init {functions:?} {values:?} ready {ready}; {body}")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        CpsAtom, CpsContId, CpsContinuation, CpsEdge, CpsFunId, CpsFunction, CpsIntrinsic,
        CpsLiteral, CpsModule, CpsNode, CpsNodeId, CpsUseTarget, CpsValueExpr, CpsValueId,
        FieldGroup,
    };

    /// Splitting a lower parameter after a higher one moves the higher group along: recording a start without shifting what follows it leaves a record the verifier reads as overlapping, which is how this was found.
    #[test]
    fn a_later_split_moves_every_group_past_it() {
        let mut module = CpsModule::new();
        let continuation = CpsContId(0);
        module.record_split(continuation, 3, 3);
        module.record_split(continuation, 1, 3);
        assert_eq!(
            module.field_groups().get(&continuation),
            Some(&vec![
                FieldGroup { start: 1, width: 3 },
                FieldGroup { start: 5, width: 3 },
            ]),
        );
    }

    fn minimal_module() -> CpsModule {
        let mut module = CpsModule::new();
        let fun = module.reserve_function();
        let return_cont = module.reserve_continuation();
        let result = module.add_value(Some("result".into()));
        let return_node = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: return_cont,
            args: vec![CpsAtom::Value(result)],
        }));
        let body = module.add_node(CpsNode::LetValue {
            result,
            value: CpsValueExpr::Literal(CpsLiteral::Nat(0)),
            next: return_node,
        });
        module.define_function(
            fun,
            CpsFunction {
                debug_name: Some("main".into()),
                params: vec![],
                return_cont,
                body,
            },
        );
        module.set_entry(fun);
        module
    }

    #[test]
    fn registers_and_rewires_uses() {
        let mut module = minimal_module();
        let old = module
            .values()
            .iter()
            .enumerate()
            .find_map(|(index, value)| {
                (value.as_ref()?.debug_name.as_deref() == Some("result"))
                    .then_some(CpsValueId(index as u32))
            })
            .unwrap();
        let replacement = module.add_value(Some("replacement".into()));
        let entry = module.entry().unwrap();
        module
            .functions
            .get_mut(entry)
            .unwrap()
            .params
            .push(replacement);
        let count = |module: &CpsModule, value| module.value_use_counts().get(&value).copied();
        assert_eq!(count(&module, old), Some(1));
        module.replace_atom(CpsUseTarget::Value(old), CpsAtom::Value(replacement));
        assert_eq!(count(&module, old), None);
        assert_eq!(count(&module, replacement), Some(1));
        module.verify().unwrap();
    }

    #[test]
    fn verifier_rejects_an_existing_but_out_of_scope_value() {
        let mut module = minimal_module();
        let result = module
            .values()
            .iter()
            .enumerate()
            .find_map(|(index, value)| {
                (value.as_ref()?.debug_name.as_deref() == Some("result"))
                    .then_some(CpsValueId(index as u32))
            })
            .unwrap();
        let orphan = module.add_value(Some("orphan".into()));
        module.replace_atom(CpsUseTarget::Value(result), CpsAtom::Value(orphan));

        let error = module.verify().unwrap_err();
        assert!(error.to_string().contains("out-of-scope"));
    }

    #[test]
    fn node_ids_are_not_reused_after_tombstoning() {
        let mut module = minimal_module();
        let removed = CpsNodeId(0);
        module.remove_node(removed).unwrap();
        let fresh = module.add_node(CpsNode::Unreachable);
        assert!(fresh.0 > removed.0);
    }

    #[test]
    fn verifier_rejects_intrinsic_arity_mismatch() {
        let mut module = minimal_module();
        let result = module.add_value(None);
        let next = module.add_node(CpsNode::Unreachable);
        module.add_node(CpsNode::LetIntrinsic {
            result,
            op: CpsIntrinsic::NatAdd,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(1))],
            next,
        });
        let bad = CpsNodeId((module.nodes.len() - 1) as u32);
        module.functions.get_mut(CpsFunId(0)).unwrap().body = bad;
        assert!(
            module
                .verify()
                .unwrap_err()
                .0
                .contains("expects 2 operands")
        );
    }

    #[test]
    fn list_map_is_not_an_intrinsic_opcode() {
        assert!(CpsIntrinsic::ListAppend.allocates());
        assert!(!CpsIntrinsic::NatAdd.is_total());
    }

    #[test]
    fn every_guarded_operation_is_classified_as_trapping() {
        // Found by reading `into_wasm`'s emission against this table rather than by a failure: each of these emits a guard — the first four through the same checked helpers as siblings already listed, the last three through an inline `Unreachable` — while the wildcard this match replaced answered `Total` for all seven, which is `eliminate_dead_bindings` deleting a refusal.
        for op in [
            CpsIntrinsic::NatRotr,
            CpsIntrinsic::IntShl,
            CpsIntrinsic::IntRotl,
            CpsIntrinsic::IntRotr,
            CpsIntrinsic::NatToInt,
            CpsIntrinsic::IntToNat,
            CpsIntrinsic::FltOfLeBytes,
        ] {
            assert!(op.may_trap(), "{op:?} emits a guard but is not `MayTrap`");
            assert!(!op.is_total(), "{op:?} must not be deletable when dead");
        }

        // The controls that keep the rule from being "guard everything": monus saturates and a right shift only clears bits, so neither can leave the envelope.
        assert!(CpsIntrinsic::NatSub.is_total());
        assert!(CpsIntrinsic::NatShr.is_total());
        assert!(CpsIntrinsic::IntShr.is_total());
    }

    #[test]
    fn return_continuation_is_a_bodyless_non_tombstone_slot() {
        let module = minimal_module();
        let function = module.function(module.entry().unwrap()).unwrap();
        assert!(module.continuation(function.return_cont).is_none());
        assert_eq!(module.tombstones().3, 0);
        module.verify().unwrap();
    }

    #[test]
    fn verifier_rejects_shared_return_continuations() {
        let mut module = minimal_module();
        let shared_return = module
            .function(module.entry().unwrap())
            .unwrap()
            .return_cont;
        let second = module.reserve_function();
        let body = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: shared_return,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(1))],
        }));
        module.define_function(
            second,
            CpsFunction {
                debug_name: Some("second".into()),
                params: vec![],
                return_cont: shared_return,
                body,
            },
        );
        assert!(
            module
                .verify()
                .unwrap_err()
                .0
                .contains("return continuation of both")
        );
    }

    #[test]
    fn verifier_rejects_another_functions_return_target() {
        let mut module = minimal_module();
        let second = module.reserve_function();
        let second_return = module.reserve_continuation();
        let second_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: second_return,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(1))],
        }));
        module.define_function(
            second,
            CpsFunction {
                debug_name: Some("second".into()),
                params: vec![],
                return_cont: second_return,
                body: second_body,
            },
        );
        let entry = module.entry().unwrap();
        let entry_body = module.function(entry).unwrap().body;
        module.nodes.set(
            entry_body,
            CpsNode::ApplyCont(CpsEdge {
                target: second_return,
                args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
            }),
        );
        assert!(
            module
                .verify()
                .unwrap_err()
                .0
                .contains("references ~f1's return continuation")
        );
    }

    #[test]
    fn verifier_rejects_undefined_non_return_continuation() {
        let mut module = minimal_module();
        let undefined = module.reserve_continuation();
        let entry = module.entry().unwrap();
        let entry_body = module.function(entry).unwrap().body;
        module.nodes.set(
            entry_body,
            CpsNode::ApplyCont(CpsEdge {
                target: undefined,
                args: vec![],
            }),
        );
        assert!(
            module
                .verify()
                .unwrap_err()
                .0
                .contains("undefined or out-of-scope continuation")
        );
    }

    #[test]
    fn verifier_rejects_local_body_at_return_id() {
        let mut module = minimal_module();
        let entry = module.entry().unwrap();
        let return_cont = module.function(entry).unwrap().return_cont;
        let local_body = module.add_node(CpsNode::Unreachable);
        module.define_continuation(
            return_cont,
            CpsContinuation {
                debug_name: Some("invalid-return-body".into()),
                params: vec![],
                body: local_body,
            },
        );
        assert!(
            module
                .verify()
                .unwrap_err()
                .0
                .contains("also identifies a local continuation")
        );
    }
}
