//! Arena-backed high CPS.
//!
//! The surface of this module is intentionally small: Ersd lowering constructs a [`CpsModule`], the optimizer mutates that graph through its checked mutation API, and backend lowering consumes it. Stable integer identities, tombstoned arena entries, and deterministic traversal are representation invariants rather than optimizer conventions. Use information is derived on demand (see [`CpsModule::value_use_counts`]) rather than maintained as a shadow arena.

use {
    curios_abi::ForeignFunction,
    curios_utilities::{Arena, Grain, PackedBin, id},
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
}

#[derive(Debug, Clone)]
pub enum CpsValueExpr {
    Literal(CpsLiteral),
    List(Vec<CpsAtom>),
    Tuple(Vec<CpsAtom>),
}

/// Intrinsic identity without operands. Operand order and arity live on the surrounding `LetIntrinsic`, so every analysis sees one uniform operand vector.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CpsIntrinsicOp {
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
    FltLte,
    FltGte,
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
    BinAppend(Grain),
    BinConcat(Grain, usize),
    ListLen,
    ListGet,
    ListSlice,
    ListAppend,
    ListConcat(usize),
    TplGet(usize),
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
    /// A binary rope reference.
    Bytes,
    /// A list rope reference.
    List,
    /// The tuple heap type of the given arity.
    Tpl(usize),
    /// An opaque reference: nothing is read of it, so nothing constrains it.
    Ref,
}

impl CpsIntrinsicOp {
    /// The representation this operation reads its `index`-th operand at.
    ///
    /// Indexed rather than returning a sequence because the concatenations are variadic and every operand of one shares a representation, so a list would allocate to say what a match arm already says.
    pub fn operand_repr(&self, index: usize) -> Repr {
        use CpsIntrinsicOp::*;

        match (self, index) {
            // The sequence operations are the only ones whose operands differ from one another: a rope first, then positions.
            (BinGet(_) | BinSlice(_) | BinAppend(_), 0) => Repr::Bytes,
            (BinGet(_) | BinSlice(_) | BinAppend(_), _) => Repr::Nat,
            (ListGet | ListSlice | ListAppend, 0) => Repr::List,
            (ListGet | ListSlice, _) => Repr::Nat,
            // A list element is carried, never interpreted — unlike a `Bytes` element, which is a `Nat` grain.
            (ListAppend, _) => Repr::Ref,
            (BinConcat(_, _), _) | (BinEql(_) | BinLen(_), _) | (FltOfLeBytes, _) => Repr::Bytes,
            (ListConcat(_) | ListLen, _) => Repr::List,
            (TplGet(index), _) => Repr::Tpl(index + 1),

            (
                NatEql | NatNeq | NatAdd | NatSub | NatMul | NatLt | NatDiv | NatRem | NatGt
                | NatLte | NatGte | NatAnd | NatOr | NatXor | NatShl | NatShr | NatRotl | NatRotr
                | NatClz | NatCtz | NatPopcnt | NatEqz | NatToInt | NatToFlt,
                _,
            ) => Repr::Nat,

            (
                IntEql | IntNeq | IntAdd | IntSub | IntMul | IntDiv | IntRem | IntLt | IntGt
                | IntLte | IntGte | IntAnd | IntOr | IntXor | IntShl | IntShr | IntRotl | IntRotr
                | IntClz | IntCtz | IntPopcnt | IntEqz | IntToNat | IntToFlt,
                _,
            ) => Repr::Int,

            (
                FltAdd | FltSub | FltMul | FltDiv | FltRem | FltEql | FltNeq | FltLt | FltGt
                | FltLte | FltGte | FltMin | FltMax | FltNeg | FltAbs | FltSqrt | FltFloor
                | FltCeil | FltTrunc | FltNearest | FltCopysign | FltToNat | FltToLeBytes
                | FltToInt,
                _,
            ) => Repr::Flt,
        }
    }

    /// The representation this operation produces.
    pub fn result_repr(&self) -> Repr {
        use CpsIntrinsicOp::*;

        match self {
            // Every comparison and predicate answers a `Bool`, whose carrier is a `Nat`.
            NatEql | NatNeq | NatLt | NatGt | NatLte | NatGte | NatEqz | IntEql | IntNeq
            | IntLt | IntGt | IntLte | IntGte | IntEqz | FltEql | FltNeq | FltLt | FltGt
            | FltLte | FltGte | BinEql(_) => Repr::Nat,

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

            BinGet(_) => Repr::Nat,
            BinSlice(_) | BinAppend(_) | BinConcat(_, _) | FltToLeBytes => Repr::Bytes,
            ListSlice | ListAppend | ListConcat(_) => Repr::List,
            // A list read and a tuple projection both yield whatever was stored, uninterpreted.
            ListGet | TplGet(_) => Repr::Ref,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CpsIntrinsicEffect {
    Total,
    MayTrap,
    Allocates,
}

impl CpsIntrinsicOp {
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
            | Self::TplGet(_) => 1,
            Self::BinSlice(_) | Self::ListSlice => 3,
            Self::BinConcat(_, arity) | Self::ListConcat(arity) => arity,
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
            | Self::ListGet
            | Self::ListSlice
            | Self::TplGet(_)
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
            | Self::ListAppend
            | Self::ListConcat(_)
            | Self::FltToLeBytes => CpsIntrinsicEffect::Allocates,

            Self::NatEql
            | Self::NatNeq
            | Self::NatSub
            | Self::NatLt
            | Self::NatGt
            | Self::NatLte
            | Self::NatGte
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
            | Self::IntLte
            | Self::IntGte
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
            | Self::FltLte
            | Self::FltGte
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
            | Self::ListLen => CpsIntrinsicEffect::Total,
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
        op: CpsIntrinsicOp,
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
    entry: Option<CpsFunId>,
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

    /// Record that `continuation`'s parameters `start..start + width` are one former aggregate's fields. Groups are kept sorted by start; [`CpsModule::verify`] holds them to the parameter list.
    pub fn record_field_group(&mut self, continuation: CpsContId, group: FieldGroup) {
        let groups = self.field_groups.entry(continuation).or_default();
        groups.push(group);
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
                    CpsAtom::Value(_) | CpsAtom::Fun(_) | CpsAtom::Literal(_) => {}
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
                CpsAtom::Literal(_) => {}
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
            CpsValueExpr::List(values) | CpsValueExpr::Tuple(values) => output.extend(values),
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
            CpsValueExpr::List(values) | CpsValueExpr::Tuple(values) => {
                values.iter_mut().for_each(visitor)
            }
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
        CpsAtom, CpsContinuation, CpsEdge, CpsFunId, CpsFunction, CpsIntrinsicOp, CpsLiteral,
        CpsModule, CpsNode, CpsNodeId, CpsUseTarget, CpsValueExpr, CpsValueId,
    };

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
            op: CpsIntrinsicOp::NatAdd,
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
        assert!(CpsIntrinsicOp::ListAppend.allocates());
        assert!(!CpsIntrinsicOp::NatAdd.is_total());
    }

    #[test]
    fn every_guarded_operation_is_classified_as_trapping() {
        // Found by reading `into_wasm`'s emission against this table rather than by a failure: each of these emits a guard — the first four through the same checked helpers as siblings already listed, the last three through an inline `Unreachable` — while the wildcard this match replaced answered `Total` for all seven, which is `eliminate_dead_bindings` deleting a refusal.
        for op in [
            CpsIntrinsicOp::NatRotr,
            CpsIntrinsicOp::IntShl,
            CpsIntrinsicOp::IntRotl,
            CpsIntrinsicOp::IntRotr,
            CpsIntrinsicOp::NatToInt,
            CpsIntrinsicOp::IntToNat,
            CpsIntrinsicOp::FltOfLeBytes,
        ] {
            assert!(op.may_trap(), "{op:?} emits a guard but is not `MayTrap`");
            assert!(!op.is_total(), "{op:?} must not be deletable when dead");
        }

        // The controls that keep the rule from being "guard everything": monus saturates and a right shift only clears bits, so neither can leave the envelope.
        assert!(CpsIntrinsicOp::NatSub.is_total());
        assert!(CpsIntrinsicOp::NatShr.is_total());
        assert!(CpsIntrinsicOp::IntShr.is_total());
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
