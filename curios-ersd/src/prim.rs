use {
    super::Term,
    curios_abi::ForeignFunction,
    std::{collections::BTreeSet, sync::Arc},
};

/// The effect-free primitive alphabet: literals plus arithmetic/comparison/conversion over `Nat` (u32), `Int` (i32), and `Flt` (f32), the `Bin`/`Lst` sequence operations, and `Io` descriptor equality. Purity here is structural — `Term::contains_effect` classifies every variant of this enum pure without inspection — so an operation with observable behavior must live in [`HostPrim`] or [`CellPrim`] instead.
#[derive(Debug)]
pub enum PurePrim {
    Nat(u32),
    NatEql(Term, Term),
    NatNeq(Term, Term),
    NatAdd(Term, Term),
    NatSub(Term, Term),
    NatMul(Term, Term),
    NatLt(Term, Term),
    NatDiv(Term, Term),
    NatRem(Term, Term),
    NatGt(Term, Term),
    NatLte(Term, Term),
    NatGte(Term, Term),
    NatAnd(Term, Term),
    NatOr(Term, Term),
    NatXor(Term, Term),
    NatShl(Term, Term),
    NatShr(Term, Term),
    Int(i32),
    IntEql(Term, Term),
    IntNeq(Term, Term),
    IntAdd(Term, Term),
    IntSub(Term, Term),
    IntMul(Term, Term),
    IntDiv(Term, Term),
    IntRem(Term, Term),
    IntLt(Term, Term),
    IntGt(Term, Term),
    IntLte(Term, Term),
    IntGte(Term, Term),
    IntAnd(Term, Term),
    IntOr(Term, Term),
    IntXor(Term, Term),
    IntShl(Term, Term),
    IntShr(Term, Term),
    Flt(f32),
    FltAdd(Term, Term),
    FltSub(Term, Term),
    FltMul(Term, Term),
    FltDiv(Term, Term),
    FltRem(Term, Term),
    FltEql(Term, Term),
    FltNeq(Term, Term),
    FltLt(Term, Term),
    FltGt(Term, Term),
    FltLte(Term, Term),
    FltGte(Term, Term),
    FltMin(Term, Term),
    FltMax(Term, Term),
    FltNeg(Term),
    FltAbs(Term),
    FltSqrt(Term),
    FltFloor(Term),
    FltCeil(Term),
    FltTrunc(Term),
    FltNearest(Term),
    NatToInt(Term),
    NatToFlt(Term),
    IntToNat(Term),
    IntToFlt(Term),
    FltToNat(Term),
    FltToLeBin(Term),
    FltToInt(Term),
    Bin(Vec<u8>),
    BinLen(Term),
    BinEql(Term, Term),
    BinGet(Term, Term),
    BinSlice(Term, Term, Term),
    BinAppend(Term, Term),
    BinConcat(Vec<Term>),
    Lst(Vec<Term>),
    LstLen(Term),
    LstGet(Term, Term),
    LstSlice(Term, Term, Term),
    LstAppend(Term, Term),
    LstConcat(Vec<Term>),
    // `LstMap(src, f)`: map closure `f` over `src`, an O(n) fill. `f` is a
    // closure value; codegen emits one alloc + a fill loop applying `f` per slot.
    LstMap(Term, Term),
    Io(u32),
    IoEql(Term, Term),
}

impl PurePrim {
    pub(crate) fn operands(&self) -> Vec<&Term> {
        use PurePrim::*;

        match self {
            Nat(_) | Int(_) | Flt(_) | Bin(_) | Io(_) => vec![],
            NatToInt(a) | NatToFlt(a) | IntToNat(a) | IntToFlt(a) | FltToNat(a) | FltToLeBin(a)
            | FltToInt(a) | FltNeg(a) | FltAbs(a) | FltSqrt(a) | FltFloor(a) | FltCeil(a)
            | FltTrunc(a) | FltNearest(a) | BinLen(a) | LstLen(a) => vec![a],
            NatEql(a, b)
            | NatNeq(a, b)
            | NatAdd(a, b)
            | NatSub(a, b)
            | NatMul(a, b)
            | NatLt(a, b)
            | NatDiv(a, b)
            | NatRem(a, b)
            | NatGt(a, b)
            | NatLte(a, b)
            | NatGte(a, b)
            | NatAnd(a, b)
            | NatOr(a, b)
            | NatXor(a, b)
            | NatShl(a, b)
            | NatShr(a, b)
            | IntEql(a, b)
            | IntNeq(a, b)
            | IntAdd(a, b)
            | IntSub(a, b)
            | IntMul(a, b)
            | IntDiv(a, b)
            | IntRem(a, b)
            | IntLt(a, b)
            | IntGt(a, b)
            | IntLte(a, b)
            | IntGte(a, b)
            | IntAnd(a, b)
            | IntOr(a, b)
            | IntXor(a, b)
            | IntShl(a, b)
            | IntShr(a, b)
            | FltAdd(a, b)
            | FltSub(a, b)
            | FltMul(a, b)
            | FltDiv(a, b)
            | FltRem(a, b)
            | FltEql(a, b)
            | FltNeq(a, b)
            | FltLt(a, b)
            | FltGt(a, b)
            | FltLte(a, b)
            | FltGte(a, b)
            | FltMin(a, b)
            | FltMax(a, b)
            | BinEql(a, b)
            | IoEql(a, b)
            | BinGet(a, b)
            | BinAppend(a, b)
            | LstGet(a, b)
            | LstAppend(a, b)
            | LstMap(a, b) => vec![a, b],
            BinSlice(a, b, c) | LstSlice(a, b, c) => vec![a, b, c],
            BinConcat(operands) | LstConcat(operands) | Lst(operands) => operands.iter().collect(),
        }
    }

    pub(crate) fn operands_mut(&mut self) -> Vec<&mut Term> {
        use PurePrim::*;

        match self {
            Nat(_) | Int(_) | Flt(_) | Bin(_) | Io(_) => vec![],
            NatToInt(a) | NatToFlt(a) | IntToNat(a) | IntToFlt(a) | FltToNat(a) | FltToLeBin(a)
            | FltToInt(a) | FltNeg(a) | FltAbs(a) | FltSqrt(a) | FltFloor(a) | FltCeil(a)
            | FltTrunc(a) | FltNearest(a) | BinLen(a) | LstLen(a) => vec![a],
            NatEql(a, b)
            | NatNeq(a, b)
            | NatAdd(a, b)
            | NatSub(a, b)
            | NatMul(a, b)
            | NatLt(a, b)
            | NatDiv(a, b)
            | NatRem(a, b)
            | NatGt(a, b)
            | NatLte(a, b)
            | NatGte(a, b)
            | NatAnd(a, b)
            | NatOr(a, b)
            | NatXor(a, b)
            | NatShl(a, b)
            | NatShr(a, b)
            | IntEql(a, b)
            | IntNeq(a, b)
            | IntAdd(a, b)
            | IntSub(a, b)
            | IntMul(a, b)
            | IntDiv(a, b)
            | IntRem(a, b)
            | IntLt(a, b)
            | IntGt(a, b)
            | IntLte(a, b)
            | IntGte(a, b)
            | IntAnd(a, b)
            | IntOr(a, b)
            | IntXor(a, b)
            | IntShl(a, b)
            | IntShr(a, b)
            | FltAdd(a, b)
            | FltSub(a, b)
            | FltMul(a, b)
            | FltDiv(a, b)
            | FltRem(a, b)
            | FltEql(a, b)
            | FltNeq(a, b)
            | FltLt(a, b)
            | FltGt(a, b)
            | FltLte(a, b)
            | FltGte(a, b)
            | FltMin(a, b)
            | FltMax(a, b)
            | BinEql(a, b)
            | IoEql(a, b)
            | BinGet(a, b)
            | BinAppend(a, b)
            | LstGet(a, b)
            | LstAppend(a, b)
            | LstMap(a, b) => vec![a, b],
            BinSlice(a, b, c) | LstSlice(a, b, c) => vec![a, b, c],
            BinConcat(operands) | LstConcat(operands) | Lst(operands) => {
                operands.iter_mut().collect()
            }
        }
    }
}

/// The primitives that leave the module: a store-described foreign import call, or process exit. Both are effectful by definition (`Prim::is_effectful`), and `to_cont` lowers each as the *tail* of its region — the impure boundary of the region tree — never as an in-region value.
#[derive(Debug)]
pub enum HostPrim {
    /// A store-described host call: the function's `WireSignature` fixes the
    /// operand order/types and how many results the continuation receives.
    Foreign(Arc<ForeignFunction>, Vec<Term>),
    IoExit(Term),
}

impl HostPrim {
    fn operands(&self) -> Vec<&Term> {
        use HostPrim::*;

        match self {
            Foreign(_, args) => args.iter().collect(),
            IoExit(a) => vec![a],
        }
    }

    fn operands_mut(&mut self) -> Vec<&mut Term> {
        use HostPrim::*;

        match self {
            Foreign(_, args) => args.iter_mut().collect(),
            IoExit(a) => vec![a],
        }
    }
}

/// The mutable-cell primitives — the IR's only stateful values (the design law confines mutation to single emitted instructions; `Cell` is the sanctioned exception that gives the guest a mutable reference). Classified effectful so no pass folds, reorders, or duplicates cell traffic.
#[derive(Debug)]
pub enum CellPrim {
    New(Term),       // init
    Set(Term, Term), // cell, value
    Get(Term),       // cell
}

impl CellPrim {
    fn operands(&self) -> Vec<&Term> {
        match self {
            CellPrim::New(a) | CellPrim::Get(a) => vec![a],
            CellPrim::Set(a, b) => vec![a, b],
        }
    }

    fn operands_mut(&mut self) -> Vec<&mut Term> {
        match self {
            CellPrim::New(a) | CellPrim::Get(a) => vec![a],
            CellPrim::Set(a, b) => vec![a, b],
        }
    }
}

/// A primitive operation, partitioned by purity: `Pure` computations may be folded, reordered, and duplicated freely, while `Host` and `Cell` are effectful (`is_effectful`) and pin the evaluation order around them. This partition is the IR's entire impurity story — `Term::contains_effect`, `optm`'s effect-taint analyses, and `to_cont`'s synchronous/CPS split all reduce to which arm a primitive sits in.
#[derive(Debug)]
pub enum Prim {
    Pure(PurePrim),
    Host(HostPrim),
    Cell(CellPrim),
}

impl Prim {
    /// Free names of this primitive's operands (see [`Term::free_names`]).
    pub(crate) fn free_names(&self) -> BTreeSet<String> {
        self.operands()
            .into_iter()
            .flat_map(Term::free_names)
            .collect()
    }

    /// The operand terms this primitive evaluates.
    pub(crate) fn operands(&self) -> Vec<&Term> {
        match self {
            Prim::Pure(p) => p.operands(),
            Prim::Host(h) => h.operands(),
            Prim::Cell(c) => c.operands(),
        }
    }

    /// The operand terms, mutably — the [`operands`](Self::operands) mirror used by
    /// passes that rewrite operands in place (e.g. `ersd::introduce_offsets`).
    pub(crate) fn operands_mut(&mut self) -> Vec<&mut Term> {
        match self {
            Prim::Pure(p) => p.operands_mut(),
            Prim::Host(h) => h.operands_mut(),
            Prim::Cell(c) => c.operands_mut(),
        }
    }

    /// Whether evaluating this primitive performs an observable action — a host
    /// effect or a cell operation — rather than a pure computation.
    pub(crate) fn is_effectful(&self) -> bool {
        matches!(self, Prim::Host(_) | Prim::Cell(_))
    }
}
