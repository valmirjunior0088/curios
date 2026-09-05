//! Arena-backed high CPS.
//!
//! The surface of this module is intentionally small: Ersd lowering constructs a [`CpsModule`], the optimizer mutates that graph through its checked mutation API, and backend lowering consumes it. Stable integer identities, tombstoned arena entries, and deterministic traversal are representation invariants rather than optimizer conventions. Use information is derived on demand (see [`CpsModule::value_use_counts`]) rather than maintained as a shadow arena.

use {
    curios_abi::ForeignFunction,
    curios_num::Floating,
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
id!(CpsRowId, "~r");

impl CpsFunId {
    pub(crate) fn from_index(index: usize) -> Self {
        Self(index as u32)
    }
}

/// A literal operand. `Flt` holds the bitwise [`Floating`] rather than an `f32` so that the derived equality is identity on the bit pattern: under IEEE equality a NaN literal is unequal to itself, and a pass comparing an edge it rebuilt against the edge it read would report a change on every round — `forward_continuations` did exactly that, and the fixpoint ran to its backstop on any module carrying a `NaN` through a jump.
#[derive(Debug, Clone, PartialEq)]
pub enum CpsLiteral {
    Nat(u32),
    Int(i32),
    Flt(Floating),
    Bin(Grain, PackedBin),
}

#[derive(Debug, Clone, PartialEq)]
pub enum CpsAtom {
    Value(CpsValueId),
    Fun(CpsFunId),
    Literal(CpsLiteral),
    /// No value: a slot belonging to a wider constructor than the edge or call carrying it fills.
    ///
    /// It is not a literal zero, and was one until an `Option(Flt)` trapped on the `none` edge. The carrier a slot is held at is decided by `represent` during backend lowering, from the *uses* of the parameter it feeds — which is strictly after the passes that create fillers, so no constant chosen here can know it. A `Nat(0)` was therefore right only where the slot happened to be `Nat`-carried or boxed, and where it was a raw `Flt` the edge coerced the `i31` with a `ref.cast` that trapped. Saying "nothing" instead defers the choice to the one place the carrier is known: the emitter materialises it as the zero of whatever the destination holds.
    Filler,
}

#[derive(Debug, Clone)]
pub enum CpsValueExpr {
    Literal(CpsLiteral),
    List(Vec<CpsAtom>),
    Tuple(Vec<CpsAtom>),
    /// A construction of a *nominal* row — a variant family or a product schema — at that row's full width, padded with [`CpsAtom::Filler`] wherever the constructor building it is narrower than the row. A family's slot zero is its tag; a product has none. The Ersd door is the only mint and pads every construction, so a row value's arity is a fact of the row rather than of the site that built it — which is what lets the emitter key one final heap type per row and read it with an exact cast instead of the structural roster cascade.
    Row(CpsRowId, Vec<CpsAtom>),
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
    NatLe,
    NatAnd,
    NatOr,
    NatXor,
    NatShl,
    NatShr,
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
    IntLe,
    IntAnd,
    IntOr,
    IntXor,
    IntShl,
    IntShr,
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
    FltLe,
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
    /// `(list…) -> list`: one exact-length flat leaf holding every element of every operand in order — the eager concatenation `fuse_append_chains` builds where the reads that would have paid the gather are already in evidence. Minted only by the optimizer, like [`CpsIntrinsic::BinChunk`].
    ListFlat(usize),
    TupleGet(usize),
    /// `(row) -> value`: slot `index` of a [`CpsValueExpr::Row`] of `row`. Which slot holds what is the row's to say — a family's slot zero is its tag — and the door is what knows it. Distinct from [`CpsIntrinsic::TupleGet`] so a row read names the row whose final type the emitter casts to exactly, and so a structural projection can never silently read a row value through the roster cascade: the two vocabularies meet only in the verifier, which refuses a mismatch.
    RowGet(CpsRowId, usize),
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
            (TupleGet(_) | RowGet(..), _) => Repr::Ref,
            // A shift count is a `Nat` on both carriers; only the shifted value is signed.
            (IntShl | IntShr, 1) => Repr::Nat,

            (
                NatEql | NatNeq | NatAdd | NatSub | NatMul | NatLt | NatDiv | NatRem | NatLe
                | NatAnd | NatOr | NatXor | NatShl | NatShr | NatEqz | NatToInt | NatToFlt,
                _,
            ) => Repr::Nat,

            (
                IntEql | IntNeq | IntAdd | IntSub | IntMul | IntDiv | IntRem | IntLt | IntLe
                | IntAnd | IntOr | IntXor | IntShl | IntShr | IntEqz | IntToNat | IntToFlt,
                _,
            ) => Repr::Int,

            (
                FltAdd | FltSub | FltMul | FltDiv | FltRem | FltEql | FltNeq | FltLt | FltLe
                | FltMin | FltMax | FltNeg | FltAbs | FltSqrt | FltFloor | FltCeil | FltTrunc
                | FltNearest | FltCopysign | FltToNat | FltToLeBytes | FltToInt,
                _,
            ) => Repr::Flt,
        }
    }

    /// The representation this operation produces.
    pub fn result_repr(&self) -> Repr {
        use CpsIntrinsic::*;

        match self {
            // Every comparison and predicate answers a `Bool`, whose carrier is a `Nat`.
            NatEql | NatNeq | NatLt | NatLe | NatEqz | IntEql | IntNeq | IntLt | IntLe | IntEqz
            | FltEql | FltNeq | FltLt | FltLe | BinEql(_) => Repr::Nat,

            NatAdd | NatSub | NatMul | NatDiv | NatRem | NatAnd | NatOr | NatXor | NatShl
            | NatShr | IntToNat | FltToNat | BinLen(_) | ListLen => Repr::Nat,

            IntAdd | IntSub | IntMul | IntDiv | IntRem | IntAnd | IntOr | IntXor | IntShl
            | IntShr | NatToInt | FltToInt => Repr::Int,

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
            ListGet | TupleGet(_) | RowGet(..) | ImmediateGet => Repr::Ref,
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
            Self::NatEqz
            | Self::NatToInt
            | Self::NatToFlt
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
            | Self::RowGet(..)
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
            | Self::RowGet(..)
            | Self::WindowExtent
            // Total in the language, guarded by the emitter because the result can leave the i31 envelope. `NatSub` is monus and `NatShr`/`IntShr` only clear bits, so neither needs a guard.
            | Self::NatAdd
            | Self::NatMul
            | Self::NatShl
            | Self::NatToInt
            | Self::IntAdd
            | Self::IntSub
            | Self::IntMul
            | Self::IntShl
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
            | Self::NatLe
            | Self::NatAnd
            | Self::NatOr
            | Self::NatXor
            | Self::NatShr
            | Self::NatEqz
            | Self::NatToFlt
            | Self::IntEql
            | Self::IntNeq
            | Self::IntLt
            | Self::IntLe
            | Self::IntAnd
            | Self::IntOr
            | Self::IntXor
            | Self::IntShr
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
            | Self::FltLe
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
    /// A cell allocated *empty*, to be filled by a later `Set`: what ties a recursive knot, whose members' cells must exist before any initializer runs and hold nothing meaningful until their own has. Reading one before its fill traps, which is the point — a knot read out of order once computed with the placeholder `New` had been handed, and `Get`'s emission already refuses a null for free. Nothing a program writes mints one; only the erased lowering does.
    Reserve,
    Set,
    Get,
}

impl CpsCellOp {
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
    /// A deliberate runtime failure of the given class: the block ends by reporting it and never continues. A lowering seats one where the program can reach a state it has to refuse — today the knot's forcing state, a member read while its own initializer runs — and the emitter renders every class as its sentence through the `sys.panic` import. Distinct from [`CpsNode::Unreachable`], which marks an arm the theory proved impossible: reaching a `Panic` is the program's doing, reaching an `Unreachable` is the compiler's.
    Panic(Panic),
    /// An arm the theory proved impossible. Never reached by a sound compilation; the emitter renders it as [`Panic::Invariant`]'s sentence so that a compiler bug says so.
    Unreachable,
}

/// The classes of failure a compiled program can stop with, each rendered by the emitter as one sentence naming the rule, the carrier and the remedy. A `CpsNode::Panic` carries one; the emitter's own checks — an overflow, a read past the end, a `Flt` decode — reach for the same classes as instruction sequences, since they are decided while lowering an intrinsic rather than as nodes. The sentences themselves are the emitter's (`into_wasm/refusal.rs`), so what the IR states is the vocabulary and what the emitter states is the text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Panic {
    /// A `Nat` result or conversion the i31 carrier cannot hold.
    NatCarrier,
    /// An `Int` result or conversion the signed i31 carrier cannot hold.
    IntCarrier,
    /// A packed or list read, or a window, past the end of its value.
    OutOfBounds,
    /// A `Flt` decoded from a byte string that is not four bytes long.
    FltDecode,
    /// A recursive value read while its own initializer is still running — a cycle the eager verifier could not see through a closure, met by forcing.
    Cycle,
    /// An arm the theory proved impossible was taken: a compiler bug, never the program's.
    Invariant,
}

impl fmt::Display for Panic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Panic::NatCarrier => "nat",
            Panic::IntCarrier => "int",
            Panic::OutOfBounds => "bounds",
            Panic::FltDecode => "flt",
            Panic::Cycle => "cycle",
            Panic::Invariant => "invariant",
        })
    }
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

/// A value a node binds, bound here and nowhere else; `noun` names it in the duplicate-binding message.
struct ScopeBinding {
    value: CpsValueId,
    noun: &'static str,
}

/// A pending region in a walk over lexical structure, carrying the scope that region sees.
enum ScopeTask {
    Function {
        function: CpsFunId,
        values: BTreeSet<CpsValueId>,
        functions: BTreeSet<CpsFunId>,
    },
    Node {
        /// The function the node belongs to. It names the region in a verification message and is needed for nothing else, so a walk that reports nothing has none.
        owner: Option<CpsFunId>,
        node: CpsNodeId,
        values: BTreeSet<CpsValueId>,
        functions: BTreeSet<CpsFunId>,
        continuations: BTreeSet<CpsContId>,
    },
}

/// What one region contributes to a lexical walk: the names it binds, and the regions below it.
#[derive(Default)]
struct ScopeStep {
    values: Vec<ScopeBinding>,
    functions: Vec<CpsFunId>,
    tasks: Vec<ScopeTask>,
}

type NodeTask = (
    Option<CpsFunId>,
    CpsNodeId,
    BTreeSet<CpsValueId>,
    BTreeSet<CpsFunId>,
    BTreeSet<CpsContId>,
);

/// The bookkeeping a lexical *verification* walk carries on top of the scope rules: which names have been bound, and which regions are still to visit.
#[derive(Default)]
struct ScopeVerifier {
    bound_functions: BTreeSet<CpsFunId>,
    bound_values: BTreeSet<CpsValueId>,
    function_work: Vec<(CpsFunId, BTreeSet<CpsValueId>, BTreeSet<CpsFunId>)>,
    node_work: Vec<NodeTask>,
}

impl ScopeVerifier {
    /// Record what `step` binds, rejecting a name bound twice, then queue the regions below it.
    fn admit(&mut self, step: ScopeStep) -> Result<(), CpsVerifyError> {
        for function in step.functions {
            if !self.bound_functions.insert(function) {
                return Err(CpsVerifyError(format!(
                    "function {function} is bound more than once"
                )));
            }
        }
        for ScopeBinding { value, noun } in step.values {
            if !self.bound_values.insert(value) {
                return Err(CpsVerifyError(format!(
                    "{noun} {value} is bound more than once"
                )));
            }
        }
        for task in step.tasks {
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
        Ok(())
    }
}

/// The production Cont representation. Arena slots never move or get reused; deletion writes `None` and deterministic compaction is explicit.
#[derive(Debug, Clone, Default)]
pub struct CpsModule {
    nodes: Arena<CpsNodeId, CpsNode>,
    values: Arena<CpsValueId, CpsValueDef>,
    functions: Arena<CpsFunId, CpsFunction>,
    continuations: Arena<CpsContId, CpsContinuation>,
    field_groups: BTreeMap<CpsContId, Vec<FieldGroup>>,
    /// The nominal rows this module's [`CpsValueExpr::Row`]s belong to, appended by the Ersd door and never removed — a row that loses its last construction is simply an unreferenced entry, so the ids stay stable without tombstones.
    rows: Vec<Option<CpsRow>>,
    entry: Option<CpsFunId>,
    /// The results of every *head rebuild*: a construction a split pass writes to stand in for the aggregate it took apart, built from the field parameters that replaced it. Its slots are the one place a padded field is *materialized* rather than merely carried — an edge that projected a nullary constructor's absent payload handed the parameter a filler, and the rebuild loads that parameter into the row's typed slot. The emitter reads this set to load such a slot tolerantly, so the filler lands as the slot's null instead of failing the slot's cast. See [`CpsModule::mark_rebuilt`].
    rebuilt: BTreeSet<CpsValueId>,
}

/// One nominal row — a variant family or a product schema: its debug name, and the carrier of every slot of its heap type. A family carries its tag at slot zero and a product does not; either way this is the width every [`CpsValueExpr::Row`] naming it is padded to.
#[derive(Debug, Clone)]
pub struct CpsRow {
    pub debug_name: Option<String>,
    pub slots: Vec<CpsSlot>,
}

impl CpsRow {
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
/// Three shapes stay [`CpsSlot::Opaque`] deliberately. A packed carrier is *sometimes* an immediate, so no single heap type names its population. A closure's runtime arity is not something the recorded shape is yet entitled to promise, since the erased arity is read off the declared type and the passes above may raise it. A row-typed field would need the field's row identity, which erasure does not record.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum CpsSlot {
    /// A variant family's discriminant, at slot zero. Stored packed and read unsigned, since a family's constructor count is bounded far below the byte the tag occupies; a product row carries none.
    Tag,
    /// A raw unsigned 32-bit payload.
    Nat,
    /// A raw signed 32-bit payload.
    Int,
    /// A raw binary32 payload — the one slot that deletes an allocation rather than a coercion, since the boxed `Flt` it replaces is a heap object of its own.
    Flt,
    /// A list rope. The base type is not final, so this is the slot that deletes an `is_subtype` libcall rather than an inline check.
    List,
    /// A closure of the given arity. Its environment base is *not* final — it is the supertype of every per-closure environment of that arity — so, like [`CpsSlot::List`], this is a slot that deletes an `is_subtype` libcall rather than an inline check.
    Closure(usize),
    /// A value of the named nominal row. A row's heap type is final, so this is the slot whose read needs no cast at all once Binaryen has the static type.
    Row(CpsRowId),
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
            CpsSlot::Closure(_) | CpsSlot::Row(_) | CpsSlot::Opaque => Repr::Ref,
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

    /// Record that `value` is a head rebuild — a row a split pass reconstructs from the field parameters that replaced it.
    ///
    /// Why a rebuild needs saying: a filler is "no value", and every destination but one materializes it as an inhabitant nothing reads — a raw zero, or the boxed `i31` a call or a return carries. A rebuild is the exception, because it *reads* the field into the row's typed slot, and the slot's null-admitting cast refuses an `i31`. So a `Cmd/none` joined with a `Cmd/perform` trapped the moment the join rebuilt its closure slot, a cast failure far from the split that padded the edge. Every other construction keeps its exact cast — a value that fails it there is a compiler fault worth trapping on — which is why the tolerance is keyed to this set and not to the row.
    pub fn mark_rebuilt(&mut self, value: CpsValueId) {
        self.rebuilt.insert(value);
    }

    /// Whether `value` is a head rebuild — see [`CpsModule::mark_rebuilt`].
    pub fn is_rebuilt(&self, value: CpsValueId) -> bool {
        self.rebuilt.contains(&value)
    }

    /// Register a nominal row and hand back its identity. The Ersd door is the only caller; see [`CpsValueExpr::Row`].
    pub fn add_row(&mut self, row: CpsRow) -> CpsRowId {
        let id = self.reserve_row();
        self.define_row(id, row);
        id
    }

    /// Claim an identity before the row it names is known.
    ///
    /// A row's slots may name other rows, and a self-referential declaration names its own — so the identity has to exist before the slots are computed, or computing them would not terminate. An undefined row is a compiler bug, and [`CpsModule::row`] says so rather than carrying an `Option` every caller would unwrap.
    pub fn reserve_row(&mut self) -> CpsRowId {
        let id = CpsRowId::from_index(self.rows.len());
        self.rows.push(None);
        id
    }

    pub fn define_row(&mut self, id: CpsRowId, row: CpsRow) {
        self.rows[id.index()] = Some(row);
    }

    pub fn row(&self, id: CpsRowId) -> &CpsRow {
        self.rows[id.index()]
            .as_ref()
            .unwrap_or_else(|| panic!("{id} was reserved and never defined"))
    }

    /// The representation a read of `row`'s slot at `index` produces. The one result representation that is a fact of the module rather than of the operation, which is why [`CpsIntrinsic::result_repr`] cannot answer it alone.
    pub fn slot_repr(&self, row: CpsRowId, index: usize) -> Repr {
        self.row(row).slots[index].repr()
    }

    /// The representation `op` produces, resolving a row read against this module's slot carriers.
    pub fn result_repr(&self, op: &CpsIntrinsic) -> Repr {
        match op {
            CpsIntrinsic::RowGet(row, index) => self.slot_repr(*row, *index),
            _ => op.result_repr(),
        }
    }

    pub fn rows(&self) -> impl Iterator<Item = (CpsRowId, &CpsRow)> {
        (0..self.rows.len())
            .map(CpsRowId::from_index)
            .map(|id| (id, self.row(id)))
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
        self.verify_with(true)
    }

    /// The round-boundary subset of [`CpsModule::verify`]: every structural clause, without the row-vocabulary one.
    ///
    /// A round's close leaves scoping, ownership and arities canonical, but the vocabulary clause holds only of the *converged* module: constant folding pushes a decided reply's payload into both arms of its dispatch, so until a later round threads the decided switch and prunes behind it, the dead arm legitimately reads that payload in the other vocabulary — the tag the fold decided is what keeps it honest, and no per-round rewrite is obliged to have cleaned it up yet. `/std/Parse`'s reply dispatches reach this state on every `pure`-fed combinator, which is how the full check at the boundary broke half the cross-stage corpus while the exit gate stayed green. The entry and exit verifies keep the full set, so a mismatch that survives convergence is still refused where its premise actually holds.
    pub fn verify_structure(&self) -> Result<(), CpsVerifyError> {
        self.verify_with(false)
    }

    fn verify_with(&self, rows: bool) -> Result<(), CpsVerifyError> {
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
        if rows {
            self.verify_rows()?;
        }

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

    /// The row vocabulary's coherence: every row named by a construction or a read exists, every construction carries exactly its row's width, every read is in range of it — and a read of a value this module visibly constructs is in the vocabulary that construction was minted in.
    ///
    /// This is what the distinct [`CpsValueExpr::Row`] buys over an annotation on `Tuple`. A row value read at a structural projection, or a construction one slot short of its row, would be a `ref.cast` trap in emitted code far from the pass that caused it; here it is a verifier failure at the boundary that produced it. Padding is the door's job, so a mismatch is always a compiler bug rather than a program's.
    ///
    /// The last clause was documented here before it was checked, and the gap was found the way the paragraph above predicts: `split_returns` rebuilt a resume's `Tuple` for a class returning an `Option` row, the `RowGet` below it cast `$tuple/2` to the row's final type, and the only symptom was an HTTP client trapping on its first response header. The check covers direct operands — a value constructed by a `LetValue` in this module and read by a `TupleGet` or `RowGet` in it — which is every case a pass's own rebuild can produce; a value that arrives through a parameter is the emitter's cast to decide, as before.
    fn verify_rows(&self) -> Result<(), CpsVerifyError> {
        // What every visible construction built, so a read can be checked against the vocabulary its operand was actually minted in rather than only against the row's own width.
        let mut built = BTreeMap::<CpsValueId, Option<CpsRowId>>::new();
        for (_, node) in self.nodes.iter_live() {
            if let CpsNode::LetValue { result, value, .. } = node {
                match value {
                    CpsValueExpr::Row(row, _) => {
                        built.insert(*result, Some(*row));
                    }
                    CpsValueExpr::Tuple(_) => {
                        built.insert(*result, None);
                    }
                    CpsValueExpr::Literal(_) | CpsValueExpr::List(_) => {}
                }
            }
        }
        for (_, node) in self.nodes.iter_live() {
            if let CpsNode::LetIntrinsic { op, args, .. } = node
                && let [CpsAtom::Value(operand)] = args.as_slice()
                && let Some(&minted) = built.get(operand)
            {
                let read = match op {
                    CpsIntrinsic::RowGet(row, _) => Some(Some(*row)),
                    CpsIntrinsic::TupleGet(_) => Some(None),
                    _ => None,
                };
                if let Some(read) = read
                    && read != minted
                {
                    return Err(CpsVerifyError(format!(
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
                CpsNode::LetValue {
                    value: CpsValueExpr::Row(row, atoms),
                    ..
                } => {
                    let Some(Some(definition)) = self.rows.get(row.index()) else {
                        return Err(CpsVerifyError(format!(
                            "row construction names {row}, which was not minted by this module"
                        )));
                    };
                    if atoms.len() != definition.width() {
                        return Err(CpsVerifyError(format!(
                            "row construction of {row} carries {} slots, but the row is {} wide",
                            atoms.len(),
                            definition.width(),
                        )));
                    }
                }
                CpsNode::LetIntrinsic {
                    op: CpsIntrinsic::RowGet(row, index),
                    ..
                } => {
                    let Some(Some(definition)) = self.rows.get(row.index()) else {
                        return Err(CpsVerifyError(format!(
                            "row read names {row}, which was not minted by this module"
                        )));
                    };
                    if *index >= definition.width() {
                        return Err(CpsVerifyError(format!(
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

    fn verify_lexical_scopes(&self, entry: CpsFunId) -> Result<(), CpsVerifyError> {
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

            walk.admit(self.scope_step(Some(owner), node, values, functions, continuations))?;
        }

        let live_functions = self.functions.live_ids().collect::<BTreeSet<_>>();
        if live_functions != walk.bound_functions {
            return Err(CpsVerifyError(
                "function arena and lexical function bindings disagree".into(),
            ));
        }
        let live_values = self.values.live_ids().collect::<BTreeSet<_>>();
        if live_values != walk.bound_values {
            return Err(CpsVerifyError(
                "value arena and lexical value bindings disagree".into(),
            ));
        }
        Ok(())
    }

    /// The scope a function's own body sees: its parameters join the values it inherits, and no continuation crosses the boundary.
    fn function_scope(
        &self,
        function: CpsFunId,
        mut values: BTreeSet<CpsValueId>,
        functions: BTreeSet<CpsFunId>,
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
        owner: Option<CpsFunId>,
        node: &CpsNode,
        values: BTreeSet<CpsValueId>,
        functions: BTreeSet<CpsFunId>,
        continuations: BTreeSet<CpsContId>,
    ) -> ScopeStep {
        let mut step = ScopeStep::default();
        match node {
            CpsNode::LetValue { result, next, .. } | CpsNode::LetIntrinsic { result, next, .. } => {
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
            CpsNode::LetFun {
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
            CpsNode::LetCont {
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
            CpsNode::ApplyFun { .. }
            | CpsNode::ApplyCont(_)
            | CpsNode::Switch { .. }
            | CpsNode::Foreign { .. }
            | CpsNode::Cell { .. }
            | CpsNode::Intrinsic { .. }
            | CpsNode::Exit { .. }
            | CpsNode::Panic(_)
            | CpsNode::Unreachable => {}
        }
        step
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
                CpsNode::LetFun { body, .. } => {
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
                | CpsNode::Panic(_)
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
            CpsNode::Exit { .. } | CpsNode::Panic(_) | CpsNode::Unreachable => {}
        }

        for atom in atoms(node) {
            match atom {
                CpsAtom::Value(value) => {
                    // Naming the referencing statement turns a dangling-operand refusal from a value id into a site: which node, and — through its spelled form — which rewrite left it behind.
                    self.require_value(*value, &format!("statement {id} ({node:?}) operand"))?
                }
                CpsAtom::Fun(function) => self.require_fun(*function, "function atom")?,
                CpsAtom::Literal(_) | CpsAtom::Filler => {}
            }
        }
        Ok(())
    }

    /// Check one transfer's argument count against its target's. A return edge is covered by the same rule: a transfer to the enclosing function's own return continuation carries its return arity, read off [`CpsModule::return_arities`] — so an edge that disagrees with its siblings is reported here rather than reaching the emitter.
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

    /// How many values a transfer to `target` carries; a transfer to the enclosing function's own return continuation carries its return.
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
            | CpsValueExpr::Row(_, values) => output.extend(values),
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
        | CpsNode::Panic(_)
        | CpsNode::Unreachable => {}
    }
    output
}

pub(crate) fn visit_atoms_mut(node: &mut CpsNode, visitor: &mut impl FnMut(&mut CpsAtom)) {
    match node {
        CpsNode::LetValue { value, .. } => match value {
            CpsValueExpr::Literal(_) => {}
            CpsValueExpr::List(values)
            | CpsValueExpr::Tuple(values)
            | CpsValueExpr::Row(_, values) => values.iter_mut().for_each(visitor),
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
        | CpsNode::Panic(_)
        | CpsNode::Unreachable => {}
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
            CpsNode::Panic(panic) => write!(f, "panic {panic}"),
            CpsNode::Unreachable => f.write_str("unreachable"),
        }
    }
}

#[cfg(test)]
mod tests;
