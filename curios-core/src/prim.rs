use {
    super::{Bound, MetavarId, Nat, Subterm, Term, Var, Visit},
    curios_abi::{ForeignFunction, WireType},
    curios_base::{Flt, Grain, Int, PackedBin},
    std::{collections::BTreeSet, sync::Arc},
};

/// The core type a host-boundary [`WireType`] denotes — the one reading of the
/// signature shared by elaboration (operand checks, result records) and
/// erasure, so the two cannot disagree about what crosses the wire.
pub(crate) fn wire_term(wire_type: &WireType) -> Term {
    let prim = match wire_type {
        WireType::Nat => Prim::NatType,
        WireType::Int => Prim::IntType,
        WireType::Bool => Prim::BoolType,
        WireType::Bin => Prim::BinType(Grain::X),
        WireType::Io => Prim::IoType,
        WireType::Lst(element) => Prim::LstType(wire_term(element)),
    };

    Subterm::Prim(prim).into()
}

/// The closed set of primitives of the core calculus: the built-in types (`BoolType`, `NatType`, `IntType`, `FltType`, `BinType`, `LstType`, `IoType`, `CellType`), their literals, and the operator families over them, plus store-described `Foreign` host calls and `IoExit`. Operand positions hold full [`Term`]s, so a primitive participates like any other subterm: elaboration checks operands against each variant's fixed signature, reduction constant-folds closed operands and rebuilds a canonical neutral otherwise, and erasure lowers each variant to its first-order IR op.
///
/// The `impl` block's constructor helpers (`nat_add`, `bin_slice`, …) take `impl Into<Term>` operands, sparing builder call sites — reduction's neutral rebuilds and curios-text's lowering — the `.into()` noise.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum Prim {
    BoolType,
    Bool(bool),
    BoolAnd(Term, Term),
    BoolOr(Term, Term),
    BoolXor(Term, Term),
    BoolEql(Term, Term),
    BoolNeq(Term, Term),
    NatType,
    Nat(Nat),
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
    NatRotl(Term, Term),
    NatRotr(Term, Term),
    NatClz(Term),
    NatCtz(Term),
    NatPopcnt(Term),
    ByteType,
    Byte(u8),
    ByteToNat(Term),
    NatToByte(Term),
    ByteEql(Term, Term),
    ByteLt(Term, Term),
    ByteLte(Term, Term),
    ByteGt(Term, Term),
    ByteGte(Term, Term),
    IntType,
    Int(Int),
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
    IntRotl(Term, Term),
    IntRotr(Term, Term),
    IntClz(Term),
    IntCtz(Term),
    IntPopcnt(Term),
    FltType,
    Flt(Flt),
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
    FltCopysign(Term, Term),
    NatToInt(Term),
    NatToFlt(Term),
    IntToNat(Term),
    IntToFlt(Term),
    FltToNat(Term),
    FltToLeBytes(Term),
    FltOfLeBytes(Term),
    FltToInt(Term),
    BinType(Grain),
    Bin(Grain, PackedBin),
    BinLen(Grain, Term),
    BinEql(Grain, Term, Term),
    BinGet(Grain, Term, Term),
    BinSlice(Grain, Term, Term, Term),
    BinAppend(Grain, Term, Term),
    BinConcat(Grain, Vec<Term>),
    LstType(Term),
    Lst(Vec<Term>),
    LstLen(Term, Term),
    LstGet(Term, Term, Term),
    LstSlice(Term, Term, Term, Term),
    LstAppend(Term, Term, Term),
    LstConcat(Term, Vec<Term>),
    // (@A, @B, f : (A) -> B, lst : Lst(A)) -> Lst(B): a structural map. Opaque
    // under reduction on a symbolic operand, so it never unfolds a variable
    // during type-checking. Erases to a single O(n) fill loop.
    LstMap(Term, Term, Term, Term),
    IoType,
    Io(u32),
    // (a, b) -> Bool: identity of two handles. The one pure operation on `Io` --
    // handles are opaque i31 tokens, so this erases to the `Nat` equality op.
    IoEql(Term, Term),
    // A store-described host call: the function's `WireSignature` fixes the
    // operand types checked at elaboration and the result shape (unit, bare
    // value, or named record). Effectful, so reducing one at the type level
    // is an error; it becomes a host call only at erasure.
    Foreign(Arc<ForeignFunction>, Vec<Term>),
    // `(@A : Type) -> Nat -> A`: polymorphic bottom. The type argument keeps the
    // kernel from naming `/std/False`; it is dropped at erasure.
    IoExit(Term, Term),
    CellType(Term),
    Cell(Term, Term),          // type, init
    CellSet(Term, Term, Term), // type, cell, value
    CellGet(Term, Term),       // type, cell
}

impl Prim {
    /// A `NatEql` node from anything term-shaped.
    pub fn nat_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatEql(left.into(), right.into())
    }

    /// An `IoEql` node — handle identity, the one pure `Io` operation — from anything term-shaped.
    pub fn io_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IoEql(left.into(), right.into())
    }

    /// A `NatNeq` node from anything term-shaped.
    pub fn nat_neq<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatNeq(left.into(), right.into())
    }

    /// A `NatAdd` node from anything term-shaped.
    pub fn nat_add<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatAdd(left.into(), right.into())
    }

    /// A `NatSub` node from anything term-shaped.
    pub fn nat_sub<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatSub(left.into(), right.into())
    }

    /// A `NatMul` node from anything term-shaped.
    pub fn nat_mul<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatMul(left.into(), right.into())
    }

    /// A `NatDiv` node from anything term-shaped.
    pub fn nat_div<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatDiv(left.into(), right.into())
    }

    /// A `NatRem` node from anything term-shaped.
    pub fn nat_rem<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatRem(left.into(), right.into())
    }

    /// A `NatLt` node from anything term-shaped.
    pub fn nat_lt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatLt(left.into(), right.into())
    }

    /// A `NatGt` node from anything term-shaped.
    pub fn nat_gt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatGt(left.into(), right.into())
    }

    /// A `NatLte` node from anything term-shaped.
    pub fn nat_lte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatLte(left.into(), right.into())
    }

    /// A `NatGte` node from anything term-shaped.
    pub fn nat_gte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatGte(left.into(), right.into())
    }

    /// An `IntEql` node from anything term-shaped.
    pub fn int_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntEql(left.into(), right.into())
    }

    /// An `IntAdd` node from anything term-shaped.
    pub fn int_add<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntAdd(left.into(), right.into())
    }

    /// An `IntSub` node from anything term-shaped.
    pub fn int_sub<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntSub(left.into(), right.into())
    }

    /// An `IntMul` node from anything term-shaped.
    pub fn int_mul<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntMul(left.into(), right.into())
    }

    /// An `IntNeq` node from anything term-shaped.
    pub fn int_neq<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntNeq(left.into(), right.into())
    }

    /// An `IntDiv` node from anything term-shaped.
    pub fn int_div<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntDiv(left.into(), right.into())
    }

    /// An `IntRem` node from anything term-shaped.
    pub fn int_rem<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntRem(left.into(), right.into())
    }

    /// An `IntLt` node from anything term-shaped.
    pub fn int_lt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntLt(left.into(), right.into())
    }

    /// An `IntGt` node from anything term-shaped.
    pub fn int_gt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntGt(left.into(), right.into())
    }

    /// An `IntLte` node from anything term-shaped.
    pub fn int_lte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntLte(left.into(), right.into())
    }

    /// An `IntGte` node from anything term-shaped.
    pub fn int_gte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntGte(left.into(), right.into())
    }

    /// A `FltAdd` node from anything term-shaped.
    pub fn flt_add<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltAdd(left.into(), right.into())
    }

    /// A `FltSub` node from anything term-shaped.
    pub fn flt_sub<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltSub(left.into(), right.into())
    }

    /// A `FltMul` node from anything term-shaped.
    pub fn flt_mul<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltMul(left.into(), right.into())
    }

    /// A `FltNeg` node from anything term-shaped.
    pub fn flt_neg<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltNeg(inner.into())
    }

    /// A `FltAbs` node from anything term-shaped.
    pub fn flt_abs<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltAbs(inner.into())
    }

    /// A `FltSqrt` node from anything term-shaped.
    pub fn flt_sqrt<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltSqrt(inner.into())
    }

    /// A `FltFloor` node from anything term-shaped.
    pub fn flt_floor<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltFloor(inner.into())
    }

    /// A `FltCeil` node from anything term-shaped.
    pub fn flt_ceil<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltCeil(inner.into())
    }

    /// A `FltTrunc` node from anything term-shaped.
    pub fn flt_trunc<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltTrunc(inner.into())
    }

    /// A `FltNearest` (round-ties-to-even) node from anything term-shaped.
    pub fn flt_nearest<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltNearest(inner.into())
    }

    /// A `FltDiv` node from anything term-shaped.
    pub fn flt_div<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltDiv(left.into(), right.into())
    }

    /// A `FltMin` node from anything term-shaped.
    pub fn flt_min<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltMin(left.into(), right.into())
    }

    /// A `FltMax` node from anything term-shaped.
    pub fn flt_max<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltMax(left.into(), right.into())
    }

    /// A `FltEql` node from anything term-shaped.
    pub fn flt_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltEql(left.into(), right.into())
    }

    /// A `FltNeq` node from anything term-shaped.
    pub fn flt_neq<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltNeq(left.into(), right.into())
    }

    /// A `FltLt` node from anything term-shaped.
    pub fn flt_lt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltLt(left.into(), right.into())
    }

    /// A `FltGt` node from anything term-shaped.
    pub fn flt_gt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltGt(left.into(), right.into())
    }

    /// A `FltLte` node from anything term-shaped.
    pub fn flt_lte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltLte(left.into(), right.into())
    }

    /// A `FltGte` node from anything term-shaped.
    pub fn flt_gte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltGte(left.into(), right.into())
    }

    /// A `NatToInt` conversion node from anything term-shaped.
    pub fn nat_to_int<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::NatToInt(inner.into())
    }

    /// An `IntToNat` conversion node from anything term-shaped.
    pub fn int_to_nat<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::IntToNat(inner.into())
    }

    /// An `IntToFlt` conversion node from anything term-shaped.
    pub fn int_to_flt<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::IntToFlt(inner.into())
    }

    /// A `NatToFlt` conversion node from anything term-shaped.
    pub fn nat_to_flt<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::NatToFlt(inner.into())
    }

    /// A `FltToInt` conversion node from anything term-shaped.
    pub fn flt_to_int<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltToInt(inner.into())
    }

    /// A `FltToNat` conversion node from anything term-shaped.
    pub fn flt_to_nat<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltToNat(inner.into())
    }

    /// A `FltToLeBytes` node (a float's four little-endian bytes as a `Bin`) from anything term-shaped.
    pub fn flt_to_le_bytes<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltToLeBytes(inner.into())
    }

    /// A `FltOfLeBytes` node (a float assembled from its four little-endian bytes) from anything term-shaped.
    pub fn flt_of_le_bytes<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltOfLeBytes(inner.into())
    }

    /// A `BinLen` node from anything term-shaped.
    pub fn bin_len<B>(grain: Grain, bin: B) -> Self
    where
        B: Into<Term>,
    {
        Self::BinLen(grain, bin.into())
    }

    /// A `BinEql` node from anything term-shaped.
    pub fn bin_eql<F, S>(grain: Grain, left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::BinEql(grain, left.into(), right.into())
    }

    /// A `BinGet` node from term-shaped bytes and index.
    pub fn bin_get<B, I>(grain: Grain, bin: B, index: I) -> Self
    where
        B: Into<Term>,
        I: Into<Term>,
    {
        Self::BinGet(grain, bin.into(), index.into())
    }

    /// A `BinSlice` node from term-shaped bytes, start, and end.
    pub fn bin_slice<B, S, E>(grain: Grain, bin: B, start: S, end: E) -> Self
    where
        B: Into<Term>,
        S: Into<Term>,
        E: Into<Term>,
    {
        Self::BinSlice(grain, bin.into(), start.into(), end.into())
    }

    /// A `BinAppend` node from term-shaped bytes and byte.
    pub fn bin_append<B, E>(grain: Grain, bin: B, byte: E) -> Self
    where
        B: Into<Term>,
        E: Into<Term>,
    {
        Self::BinAppend(grain, bin.into(), byte.into())
    }

    /// A `BinConcat` node from any iterator of term-shaped operands.
    pub fn bin_concat<I>(grain: Grain, operands: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Term>,
    {
        Self::BinConcat(grain, operands.into_iter().map(|e| e.into()).collect())
    }

    /// A `LstType` node from a term-shaped element type.
    pub fn lst_type<T>(elem: T) -> Self
    where
        T: Into<Term>,
    {
        Self::LstType(elem.into())
    }

    /// A `LstLen` node from term-shaped element type and list.
    pub fn lst_len<T, L>(type_: T, list: L) -> Self
    where
        T: Into<Term>,
        L: Into<Term>,
    {
        Self::LstLen(type_.into(), list.into())
    }

    /// A `LstGet` node from term-shaped element type, list, and index.
    pub fn lst_get<T, L, I>(type_: T, list: L, index: I) -> Self
    where
        T: Into<Term>,
        L: Into<Term>,
        I: Into<Term>,
    {
        Self::LstGet(type_.into(), list.into(), index.into())
    }

    /// A `LstSlice` node from term-shaped element type, list, start, and end.
    pub fn lst_slice<T, L, S, E>(type_: T, list: L, start: S, end: E) -> Self
    where
        T: Into<Term>,
        L: Into<Term>,
        S: Into<Term>,
        E: Into<Term>,
    {
        Self::LstSlice(type_.into(), list.into(), start.into(), end.into())
    }

    /// A `LstAppend` node from term-shaped element type, list, and element.
    pub fn lst_append<T, L, E>(type_: T, list: L, elem: E) -> Self
    where
        T: Into<Term>,
        L: Into<Term>,
        E: Into<Term>,
    {
        Self::LstAppend(type_.into(), list.into(), elem.into())
    }

    /// A `LstConcat` node from a term-shaped element type and any iterator of term-shaped operands.
    pub fn lst_concat<T, O>(type_: T, operands: O) -> Self
    where
        T: Into<Term>,
        O: IntoIterator,
        O::Item: Into<Term>,
    {
        Self::LstConcat(
            type_.into(),
            operands.into_iter().map(|e| e.into()).collect(),
        )
    }

    /// A `LstMap` node from term-shaped source element type, target element
    /// type, list, and function — the collection first, like every other
    /// sequence operation.
    pub fn lst_map<A, B, R, F>(a: A, b: B, lst: R, f: F) -> Self
    where
        A: Into<Term>,
        B: Into<Term>,
        R: Into<Term>,
        F: Into<Term>,
    {
        Self::LstMap(a.into(), b.into(), lst.into(), f.into())
    }

    /// A `CellType` node from a term-shaped element type.
    pub fn cell_type<T>(elem: T) -> Self
    where
        T: Into<Term>,
    {
        Self::CellType(elem.into())
    }

    /// A cell allocation — the `Prim::Cell` variant — from a term-shaped element type and initial value.
    pub fn cell_new<T, I>(type_: T, init: I) -> Self
    where
        T: Into<Term>,
        I: Into<Term>,
    {
        Self::Cell(type_.into(), init.into())
    }

    /// A `CellSet` node from term-shaped element type, cell, and new value.
    pub fn cell_set<T, C, V>(type_: T, cell: C, value: V) -> Self
    where
        T: Into<Term>,
        C: Into<Term>,
        V: Into<Term>,
    {
        Self::CellSet(type_.into(), cell.into(), value.into())
    }

    /// A `CellGet` node from term-shaped element type and cell.
    pub fn cell_get<T, C>(type_: T, cell: C) -> Self
    where
        T: Into<Term>,
        C: Into<Term>,
    {
        Self::CellGet(type_.into(), cell.into())
    }

    /// Visit each `Term` operand of `self`, in field order. The single source of
    /// truth for which fields of a primitive are its term operands — `reach`,
    /// `any_metavar`, and `collect_construction_names` all read it.
    /// (`traverse` keeps its own match: it rebuilds rather than visits.) The
    /// closure is taken `impl FnMut` so it monomorphises and inlines, leaving the
    /// de Bruijn / region hot path allocation- and indirection-free.
    fn for_each_operand(&self, visit: &mut impl FnMut(&Term)) {
        match self {
            Prim::BoolType
            | Prim::Bool(_)
            | Prim::NatType
            | Prim::Nat(Nat::Zero)
            | Prim::ByteType
            | Prim::Byte(_)
            | Prim::IntType
            | Prim::Int(_)
            | Prim::FltType
            | Prim::Flt(_)
            | Prim::BinType(Grain::X)
            | Prim::Bin(Grain::X, _)
            | Prim::BinType(Grain::B)
            | Prim::Bin(Grain::B, _)
            | Prim::IoType
            | Prim::Io(_) => {}

            Prim::Nat(Nat::Succ(_, inner)) => visit(inner),

            Prim::FltToLeBytes(t)
            | Prim::FltOfLeBytes(t)
            | Prim::NatToInt(t)
            | Prim::NatToFlt(t)
            | Prim::IntToNat(t)
            | Prim::IntToFlt(t)
            | Prim::FltToNat(t)
            | Prim::FltToInt(t)
            | Prim::ByteToNat(t)
            | Prim::NatToByte(t)
            | Prim::FltNeg(t)
            | Prim::FltAbs(t)
            | Prim::FltSqrt(t)
            | Prim::FltFloor(t)
            | Prim::FltCeil(t)
            | Prim::FltTrunc(t)
            | Prim::FltNearest(t)
            | Prim::NatClz(t)
            | Prim::NatCtz(t)
            | Prim::NatPopcnt(t)
            | Prim::IntClz(t)
            | Prim::IntCtz(t)
            | Prim::IntPopcnt(t)
            | Prim::BinLen(Grain::X, t)
            | Prim::BinLen(Grain::B, t)
            | Prim::LstType(t) => visit(t),

            Prim::IoEql(a, b)
            | Prim::ByteEql(a, b)
            | Prim::ByteLt(a, b)
            | Prim::ByteLte(a, b)
            | Prim::ByteGt(a, b)
            | Prim::ByteGte(a, b)
            | Prim::NatEql(a, b)
            | Prim::NatNeq(a, b)
            | Prim::NatAdd(a, b)
            | Prim::NatSub(a, b)
            | Prim::NatMul(a, b)
            | Prim::NatLt(a, b)
            | Prim::NatDiv(a, b)
            | Prim::NatRem(a, b)
            | Prim::NatGt(a, b)
            | Prim::NatLte(a, b)
            | Prim::NatGte(a, b)
            | Prim::NatAnd(a, b)
            | Prim::NatOr(a, b)
            | Prim::NatXor(a, b)
            | Prim::NatShl(a, b)
            | Prim::NatShr(a, b)
            | Prim::NatRotl(a, b)
            | Prim::NatRotr(a, b)
            | Prim::BoolAnd(a, b)
            | Prim::BoolOr(a, b)
            | Prim::BoolXor(a, b)
            | Prim::BoolEql(a, b)
            | Prim::BoolNeq(a, b)
            | Prim::IntEql(a, b)
            | Prim::IntNeq(a, b)
            | Prim::IntAdd(a, b)
            | Prim::IntSub(a, b)
            | Prim::IntMul(a, b)
            | Prim::IntDiv(a, b)
            | Prim::IntRem(a, b)
            | Prim::IntLt(a, b)
            | Prim::IntGt(a, b)
            | Prim::IntLte(a, b)
            | Prim::IntGte(a, b)
            | Prim::IntAnd(a, b)
            | Prim::IntOr(a, b)
            | Prim::IntXor(a, b)
            | Prim::IntShl(a, b)
            | Prim::IntShr(a, b)
            | Prim::IntRotl(a, b)
            | Prim::IntRotr(a, b)
            | Prim::FltAdd(a, b)
            | Prim::FltSub(a, b)
            | Prim::FltMul(a, b)
            | Prim::FltDiv(a, b)
            | Prim::FltRem(a, b)
            | Prim::FltEql(a, b)
            | Prim::FltNeq(a, b)
            | Prim::FltLt(a, b)
            | Prim::FltGt(a, b)
            | Prim::FltLte(a, b)
            | Prim::FltGte(a, b)
            | Prim::FltMin(a, b)
            | Prim::FltMax(a, b)
            | Prim::FltCopysign(a, b)
            | Prim::BinEql(Grain::X, a, b)
            | Prim::BinGet(Grain::X, a, b)
            | Prim::BinAppend(Grain::X, a, b)
            | Prim::BinEql(Grain::B, a, b)
            | Prim::BinGet(Grain::B, a, b)
            | Prim::BinAppend(Grain::B, a, b)
            | Prim::LstLen(a, b)
            | Prim::IoExit(a, b) => {
                visit(a);
                visit(b);
            }

            Prim::BinSlice(Grain::X, a, b, c)
            | Prim::BinSlice(Grain::B, a, b, c)
            | Prim::LstGet(a, b, c)
            | Prim::LstAppend(a, b, c) => {
                visit(a);
                visit(b);
                visit(c);
            }

            Prim::LstSlice(a, b, c, d) | Prim::LstMap(a, b, c, d) => {
                visit(a);
                visit(b);
                visit(c);
                visit(d);
            }

            Prim::Foreign(_, args) => args.iter().for_each(&mut *visit),

            Prim::BinConcat(Grain::X, terms)
            | Prim::BinConcat(Grain::B, terms)
            | Prim::Lst(terms) => terms.iter().for_each(&mut *visit),
            Prim::LstConcat(ty, terms) => {
                visit(ty);
                terms.iter().for_each(&mut *visit);
            }

            Prim::CellType(a) => visit(a),
            Prim::Cell(a, b) | Prim::CellGet(a, b) => {
                visit(a);
                visit(b);
            }
            Prim::CellSet(a, b, c) => {
                visit(a);
                visit(b);
                visit(c);
            }
        }
    }

    pub(crate) fn reach(&self) -> usize {
        let mut reach = 0;
        self.for_each_operand(&mut |term| reach = reach.max(term.reach()));
        reach
    }

    pub(crate) fn any_metavar<F: FnMut(MetavarId) -> bool>(&self, pred: &mut F) -> bool {
        let mut found = false;
        self.for_each_operand(&mut |term| found = found || term.any_metavar(pred));
        found
    }

    // Recurse into every operand `Term` so a construction nested inside a primitive
    // (e.g. `Lst(Str)`'s element type) still contributes its head name. Prims own no
    // head names of their own.
    pub(crate) fn collect_construction_names(&self, names: &mut BTreeSet<String>) {
        self.for_each_operand(&mut |term| term.collect_construction_names(names));
    }

    pub(crate) fn traverse<F>(&self, visit: &mut Visit<F>) -> Prim
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        match self {
            Prim::BoolType => Prim::BoolType,
            Prim::Bool(value) => Prim::Bool(*value),
            Prim::NatType => Prim::NatType,
            Prim::Nat(Nat::Zero) => Prim::Nat(Nat::Zero),
            Prim::Nat(Nat::Succ(spine, inner)) => {
                Prim::Nat(Nat::Succ(spine.clone(), visit.visit_subterm(inner)))
            }
            Prim::NatEql(l, r) => traverse_binary(l, r, visit, Prim::NatEql),
            Prim::IoEql(l, r) => traverse_binary(l, r, visit, Prim::IoEql),
            Prim::NatNeq(l, r) => traverse_binary(l, r, visit, Prim::NatNeq),
            Prim::NatAdd(l, r) => traverse_binary(l, r, visit, Prim::NatAdd),
            Prim::NatSub(l, r) => traverse_binary(l, r, visit, Prim::NatSub),
            Prim::NatMul(l, r) => traverse_binary(l, r, visit, Prim::NatMul),
            Prim::NatLt(l, r) => traverse_binary(l, r, visit, Prim::NatLt),
            Prim::NatDiv(l, r) => traverse_binary(l, r, visit, Prim::NatDiv),
            Prim::NatRem(l, r) => traverse_binary(l, r, visit, Prim::NatRem),
            Prim::NatGt(l, r) => traverse_binary(l, r, visit, Prim::NatGt),
            Prim::NatLte(l, r) => traverse_binary(l, r, visit, Prim::NatLte),
            Prim::NatGte(l, r) => traverse_binary(l, r, visit, Prim::NatGte),
            Prim::NatAnd(l, r) => traverse_binary(l, r, visit, Prim::NatAnd),
            Prim::NatOr(l, r) => traverse_binary(l, r, visit, Prim::NatOr),
            Prim::NatXor(l, r) => traverse_binary(l, r, visit, Prim::NatXor),
            Prim::NatShl(l, r) => traverse_binary(l, r, visit, Prim::NatShl),
            Prim::NatShr(l, r) => traverse_binary(l, r, visit, Prim::NatShr),
            Prim::NatRotl(l, r) => traverse_binary(l, r, visit, Prim::NatRotl),
            Prim::NatRotr(l, r) => traverse_binary(l, r, visit, Prim::NatRotr),
            Prim::NatClz(i) => Prim::NatClz(visit.visit_subterm(i)),
            Prim::NatCtz(i) => Prim::NatCtz(visit.visit_subterm(i)),
            Prim::NatPopcnt(i) => Prim::NatPopcnt(visit.visit_subterm(i)),
            Prim::ByteType => Prim::ByteType,
            Prim::Byte(value) => Prim::Byte(*value),
            Prim::ByteToNat(inner) => Prim::ByteToNat(visit.visit_subterm(inner)),
            Prim::NatToByte(inner) => Prim::NatToByte(visit.visit_subterm(inner)),
            Prim::ByteEql(l, r) => traverse_binary(l, r, visit, Prim::ByteEql),
            Prim::ByteLt(l, r) => traverse_binary(l, r, visit, Prim::ByteLt),
            Prim::ByteLte(l, r) => traverse_binary(l, r, visit, Prim::ByteLte),
            Prim::ByteGt(l, r) => traverse_binary(l, r, visit, Prim::ByteGt),
            Prim::ByteGte(l, r) => traverse_binary(l, r, visit, Prim::ByteGte),
            Prim::BoolAnd(l, r) => traverse_binary(l, r, visit, Prim::BoolAnd),
            Prim::BoolOr(l, r) => traverse_binary(l, r, visit, Prim::BoolOr),
            Prim::BoolXor(l, r) => traverse_binary(l, r, visit, Prim::BoolXor),
            Prim::BoolEql(l, r) => traverse_binary(l, r, visit, Prim::BoolEql),
            Prim::BoolNeq(l, r) => traverse_binary(l, r, visit, Prim::BoolNeq),
            Prim::IntType => Prim::IntType,
            Prim::Int(value) => Prim::Int(value.clone()),
            Prim::IntEql(l, r) => traverse_binary(l, r, visit, Prim::IntEql),
            Prim::IntNeq(l, r) => traverse_binary(l, r, visit, Prim::IntNeq),
            Prim::IntAdd(l, r) => traverse_binary(l, r, visit, Prim::IntAdd),
            Prim::IntSub(l, r) => traverse_binary(l, r, visit, Prim::IntSub),
            Prim::IntMul(l, r) => traverse_binary(l, r, visit, Prim::IntMul),
            Prim::IntDiv(l, r) => traverse_binary(l, r, visit, Prim::IntDiv),
            Prim::IntRem(l, r) => traverse_binary(l, r, visit, Prim::IntRem),
            Prim::IntLt(l, r) => traverse_binary(l, r, visit, Prim::IntLt),
            Prim::IntGt(l, r) => traverse_binary(l, r, visit, Prim::IntGt),
            Prim::IntLte(l, r) => traverse_binary(l, r, visit, Prim::IntLte),
            Prim::IntGte(l, r) => traverse_binary(l, r, visit, Prim::IntGte),
            Prim::IntAnd(l, r) => traverse_binary(l, r, visit, Prim::IntAnd),
            Prim::IntOr(l, r) => traverse_binary(l, r, visit, Prim::IntOr),
            Prim::IntXor(l, r) => traverse_binary(l, r, visit, Prim::IntXor),
            Prim::IntShl(l, r) => traverse_binary(l, r, visit, Prim::IntShl),
            Prim::IntShr(l, r) => traverse_binary(l, r, visit, Prim::IntShr),
            Prim::IntRotl(l, r) => traverse_binary(l, r, visit, Prim::IntRotl),
            Prim::IntRotr(l, r) => traverse_binary(l, r, visit, Prim::IntRotr),
            Prim::IntClz(i) => Prim::IntClz(visit.visit_subterm(i)),
            Prim::IntCtz(i) => Prim::IntCtz(visit.visit_subterm(i)),
            Prim::IntPopcnt(i) => Prim::IntPopcnt(visit.visit_subterm(i)),
            Prim::FltType => Prim::FltType,
            Prim::Flt(flt) => Prim::Flt(*flt),
            Prim::FltAdd(l, r) => traverse_binary(l, r, visit, Prim::FltAdd),
            Prim::FltSub(l, r) => traverse_binary(l, r, visit, Prim::FltSub),
            Prim::FltMul(l, r) => traverse_binary(l, r, visit, Prim::FltMul),
            Prim::FltDiv(l, r) => traverse_binary(l, r, visit, Prim::FltDiv),
            Prim::FltRem(l, r) => traverse_binary(l, r, visit, Prim::FltRem),
            Prim::FltEql(l, r) => traverse_binary(l, r, visit, Prim::FltEql),
            Prim::FltNeq(l, r) => traverse_binary(l, r, visit, Prim::FltNeq),
            Prim::FltLt(l, r) => traverse_binary(l, r, visit, Prim::FltLt),
            Prim::FltGt(l, r) => traverse_binary(l, r, visit, Prim::FltGt),
            Prim::FltLte(l, r) => traverse_binary(l, r, visit, Prim::FltLte),
            Prim::FltGte(l, r) => traverse_binary(l, r, visit, Prim::FltGte),
            Prim::FltMin(l, r) => traverse_binary(l, r, visit, Prim::FltMin),
            Prim::FltMax(l, r) => traverse_binary(l, r, visit, Prim::FltMax),
            Prim::FltCopysign(l, r) => traverse_binary(l, r, visit, Prim::FltCopysign),
            Prim::FltNeg(inner) => Prim::FltNeg(visit.visit_subterm(inner)),
            Prim::FltAbs(inner) => Prim::FltAbs(visit.visit_subterm(inner)),
            Prim::FltSqrt(inner) => Prim::FltSqrt(visit.visit_subterm(inner)),
            Prim::FltFloor(inner) => Prim::FltFloor(visit.visit_subterm(inner)),
            Prim::FltCeil(inner) => Prim::FltCeil(visit.visit_subterm(inner)),
            Prim::FltTrunc(inner) => Prim::FltTrunc(visit.visit_subterm(inner)),
            Prim::FltNearest(inner) => Prim::FltNearest(visit.visit_subterm(inner)),
            Prim::FltToLeBytes(inner) => Prim::FltToLeBytes(visit.visit_subterm(inner)),
            Prim::FltOfLeBytes(inner) => Prim::FltOfLeBytes(visit.visit_subterm(inner)),
            Prim::NatToInt(inner) => Prim::NatToInt(visit.visit_subterm(inner)),
            Prim::NatToFlt(inner) => Prim::NatToFlt(visit.visit_subterm(inner)),
            Prim::IntToNat(inner) => Prim::IntToNat(visit.visit_subterm(inner)),
            Prim::IntToFlt(inner) => Prim::IntToFlt(visit.visit_subterm(inner)),
            Prim::FltToNat(inner) => Prim::FltToNat(visit.visit_subterm(inner)),
            Prim::FltToInt(inner) => Prim::FltToInt(visit.visit_subterm(inner)),
            Prim::BinType(grain) => Prim::BinType(*grain),
            Prim::Bin(grain, value) => Prim::Bin(*grain, value.clone()),
            Prim::BinLen(grain, bin) => Prim::BinLen(*grain, visit.visit_subterm(bin)),
            Prim::BinEql(grain, l, r) => {
                traverse_binary(l, r, visit, |l, r| Prim::BinEql(*grain, l, r))
            }
            Prim::BinGet(grain, b, i) => {
                traverse_binary(b, i, visit, |b, i| Prim::BinGet(*grain, b, i))
            }
            Prim::BinSlice(grain, bin, start, end) => Prim::BinSlice(
                *grain,
                visit.visit_subterm(bin),
                visit.visit_subterm(start),
                visit.visit_subterm(end),
            ),
            Prim::BinAppend(grain, b, atom) => {
                traverse_binary(b, atom, visit, |b, atom| Prim::BinAppend(*grain, b, atom))
            }
            Prim::BinConcat(grain, operands) => Prim::BinConcat(
                *grain,
                operands.iter().map(|e| visit.visit_subterm(e)).collect(),
            ),
            Prim::LstType(elem) => Prim::LstType(visit.visit_subterm(elem)),
            Prim::Lst(elems) => Prim::Lst(elems.iter().map(|e| visit.visit_subterm(e)).collect()),
            Prim::LstLen(ty, list) => traverse_binary(ty, list, visit, Prim::LstLen),
            Prim::LstGet(ty, list, index) => Prim::LstGet(
                visit.visit_subterm(ty),
                visit.visit_subterm(list),
                visit.visit_subterm(index),
            ),
            Prim::LstSlice(ty, list, start, end) => Prim::LstSlice(
                visit.visit_subterm(ty),
                visit.visit_subterm(list),
                visit.visit_subterm(start),
                visit.visit_subterm(end),
            ),
            Prim::LstAppend(ty, list, elem) => Prim::LstAppend(
                visit.visit_subterm(ty),
                visit.visit_subterm(list),
                visit.visit_subterm(elem),
            ),
            Prim::LstConcat(ty, operands) => Prim::LstConcat(
                visit.visit_subterm(ty),
                operands.iter().map(|e| visit.visit_subterm(e)).collect(),
            ),
            Prim::LstMap(a, b, lst, f) => Prim::LstMap(
                visit.visit_subterm(a),
                visit.visit_subterm(b),
                visit.visit_subterm(lst),
                visit.visit_subterm(f),
            ),
            Prim::IoType => Prim::IoType,
            Prim::Io(token) => Prim::Io(*token),
            Prim::Foreign(function, args) => Prim::Foreign(
                Arc::clone(function),
                args.iter().map(|arg| visit.visit_subterm(arg)).collect(),
            ),
            Prim::IoExit(type_, code) => traverse_binary(type_, code, visit, Prim::IoExit),
            Prim::CellType(a) => Prim::CellType(visit.visit_subterm(a)),
            Prim::Cell(a, b) => traverse_binary(a, b, visit, Prim::Cell),
            Prim::CellGet(a, b) => traverse_binary(a, b, visit, Prim::CellGet),
            Prim::CellSet(a, b, c) => Prim::CellSet(
                visit.visit_subterm(a),
                visit.visit_subterm(b),
                visit.visit_subterm(c),
            ),
        }
    }
}

/// Which primitive type a match scrutinee is required to have. The legal
/// selectors for `expect_prim_head`/`elaborate_prim_head` — exactly the
/// type-former `Prim`s those helpers accept, as a closed set so an out-of-range
/// selector is unrepresentable rather than an `unreachable!` panic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PrimHead {
    Nat,
    Bool,
    Bin(Grain),
}

#[cfg(test)]
impl Prim {
    /// Test-only shorthand: a `Lst` literal from anything term-shaped.
    pub(crate) fn lst<I, A>(items: I) -> Self
    where
        I: IntoIterator<Item = A>,
        A: Into<Term>,
    {
        Self::Lst(items.into_iter().map(Into::into).collect())
    }
}

/// Visit both operands of a binary primitive and rebuild it through `build`. The
/// constructor is taken generically (not as a `fn` pointer) so every call site
/// monomorphises to the same direct construction — this is the de Bruijn
/// traversal hot path, so the indirection must vanish.
fn traverse_binary<F>(
    left: &Term,
    right: &Term,
    visit: &mut Visit<F>,
    build: impl FnOnce(Term, Term) -> Prim,
) -> Prim
where
    F: FnMut(usize, &Var) -> Option<Subterm>,
{
    build(visit.visit_subterm(left), visit.visit_subterm(right))
}
