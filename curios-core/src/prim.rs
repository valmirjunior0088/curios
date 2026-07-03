use {
    super::{Bound, Flt, Int, MetavarId, Nat, Subterm, Term, Var, Visit},
    curios_abi::{ForeignFunction, WireType},
    std::{collections::BTreeSet, sync::Arc},
};

/// The core type a host-boundary [`WireType`] denotes — the one reading of the
/// signature shared by elaboration (operand checks, result records) and
/// erasure, so the two cannot disagree about what crosses the wire.
pub fn wire_term(wire_type: &WireType) -> Term {
    let prim = match wire_type {
        WireType::Nat => Prim::NatType,
        WireType::Int => Prim::IntType,
        WireType::Bln => Prim::BlnType,
        WireType::Bin => Prim::BinType,
        WireType::Io => Prim::IoType,
        WireType::Arr(element) => Prim::ArrType(wire_term(element)),
    };

    Subterm::Prim(prim).into()
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Prim {
    BlnType,
    Bln(bool),
    BlnAnd(Term, Term),
    BlnOr(Term, Term),
    BlnXor(Term, Term),
    BlnEql(Term, Term),
    BlnNeq(Term, Term),
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
    NatToInt(Term),
    NatToFlt(Term),
    IntToNat(Term),
    IntToFlt(Term),
    FltToNat(Term),
    FltToLeBin(Term),
    FltToInt(Term),
    BinType,
    Bin(Vec<u8>),
    BinLen(Term),
    BinEql(Term, Term),
    BinGet(Term, Term),
    BinSlice(Term, Term, Term),
    BinAppend(Term, Term),
    BinConcat(Vec<Term>),
    ArrType(Term),
    Arr(Vec<Term>),
    ArrLen(Term, Term),
    ArrGet(Term, Term, Term),
    ArrSlice(Term, Term, Term, Term),
    ArrAppend(Term, Term, Term),
    ArrConcat(Term, Vec<Term>),
    // (@A, @B, f : (A) -> B, arr : Arr(A)) -> Arr(B): a structural map. Opaque
    // under reduction on a symbolic operand, so it never unfolds a variable
    // during type-checking. Erases to a single O(n) fill loop.
    ArrMap(Term, Term, Term, Term),
    IoType,
    Io(u32),
    // (a, b) -> Bln: identity of two handles. The one pure operation on `Io` --
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

/// Which primitive type a match scrutinee is required to have. The legal
/// selectors for `expect_prim_head`/`elaborate_prim_head` — exactly the
/// type-former `Prim`s those helpers accept, as a closed set so an out-of-range
/// selector is unrepresentable rather than an `unreachable!` panic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimHead {
    Nat,
    Bln,
    Bin,
}

impl Prim {
    pub fn nat_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatEql(left.into(), right.into())
    }

    pub fn io_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IoEql(left.into(), right.into())
    }

    pub fn nat_neq<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatNeq(left.into(), right.into())
    }

    pub fn nat_add<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatAdd(left.into(), right.into())
    }

    pub fn nat_sub<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatSub(left.into(), right.into())
    }

    pub fn nat_mul<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatMul(left.into(), right.into())
    }

    pub fn nat_div<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatDiv(left.into(), right.into())
    }

    pub fn nat_rem<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatRem(left.into(), right.into())
    }

    pub fn nat_lt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatLt(left.into(), right.into())
    }

    pub fn nat_gt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatGt(left.into(), right.into())
    }

    pub fn nat_lte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatLte(left.into(), right.into())
    }

    pub fn nat_gte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatGte(left.into(), right.into())
    }

    pub fn int_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntEql(left.into(), right.into())
    }

    pub fn int_add<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntAdd(left.into(), right.into())
    }

    pub fn int_sub<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntSub(left.into(), right.into())
    }

    pub fn int_mul<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntMul(left.into(), right.into())
    }

    pub fn int_neq<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntNeq(left.into(), right.into())
    }

    pub fn int_div<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntDiv(left.into(), right.into())
    }

    pub fn int_rem<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntRem(left.into(), right.into())
    }

    pub fn int_lt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntLt(left.into(), right.into())
    }

    pub fn int_gt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntGt(left.into(), right.into())
    }

    pub fn int_lte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntLte(left.into(), right.into())
    }

    pub fn int_gte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntGte(left.into(), right.into())
    }

    pub fn flt_add<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltAdd(left.into(), right.into())
    }

    pub fn flt_sub<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltSub(left.into(), right.into())
    }

    pub fn flt_mul<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltMul(left.into(), right.into())
    }

    pub fn flt_neg<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltNeg(inner.into())
    }

    pub fn flt_abs<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltAbs(inner.into())
    }

    pub fn flt_sqrt<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltSqrt(inner.into())
    }

    pub fn flt_floor<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltFloor(inner.into())
    }

    pub fn flt_ceil<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltCeil(inner.into())
    }

    pub fn flt_trunc<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltTrunc(inner.into())
    }

    pub fn flt_nearest<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltNearest(inner.into())
    }

    pub fn flt_div<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltDiv(left.into(), right.into())
    }

    pub fn flt_min<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltMin(left.into(), right.into())
    }

    pub fn flt_max<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltMax(left.into(), right.into())
    }

    pub fn flt_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltEql(left.into(), right.into())
    }

    pub fn flt_neq<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltNeq(left.into(), right.into())
    }

    pub fn flt_lt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltLt(left.into(), right.into())
    }

    pub fn flt_gt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltGt(left.into(), right.into())
    }

    pub fn flt_lte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltLte(left.into(), right.into())
    }

    pub fn flt_gte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltGte(left.into(), right.into())
    }

    pub fn nat_to_int<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::NatToInt(inner.into())
    }

    pub fn int_to_nat<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::IntToNat(inner.into())
    }

    pub fn int_to_flt<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::IntToFlt(inner.into())
    }

    pub fn nat_to_flt<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::NatToFlt(inner.into())
    }

    pub fn flt_to_int<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltToInt(inner.into())
    }

    pub fn flt_to_nat<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltToNat(inner.into())
    }

    pub fn flt_to_le_bin<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltToLeBin(inner.into())
    }

    pub fn bin_len<B>(bin: B) -> Self
    where
        B: Into<Term>,
    {
        Self::BinLen(bin.into())
    }

    pub fn bin_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::BinEql(left.into(), right.into())
    }

    pub fn bin_get<B, I>(bin: B, index: I) -> Self
    where
        B: Into<Term>,
        I: Into<Term>,
    {
        Self::BinGet(bin.into(), index.into())
    }

    pub fn bin_slice<B, S, E>(bin: B, start: S, end: E) -> Self
    where
        B: Into<Term>,
        S: Into<Term>,
        E: Into<Term>,
    {
        Self::BinSlice(bin.into(), start.into(), end.into())
    }

    pub fn bin_append<B, E>(bin: B, byte: E) -> Self
    where
        B: Into<Term>,
        E: Into<Term>,
    {
        Self::BinAppend(bin.into(), byte.into())
    }

    pub fn bin_concat<I>(operands: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Term>,
    {
        Self::BinConcat(operands.into_iter().map(|e| e.into()).collect())
    }

    pub fn arr<I, A>(items: I) -> Self
    where
        I: IntoIterator<Item = A>,
        A: Into<Term>,
    {
        Self::Arr(items.into_iter().map(Into::into).collect())
    }

    pub fn arr_type<T>(elem: T) -> Self
    where
        T: Into<Term>,
    {
        Self::ArrType(elem.into())
    }

    pub fn arr_len<T, L>(type_: T, list: L) -> Self
    where
        T: Into<Term>,
        L: Into<Term>,
    {
        Self::ArrLen(type_.into(), list.into())
    }

    pub fn arr_get<T, L, I>(type_: T, list: L, index: I) -> Self
    where
        T: Into<Term>,
        L: Into<Term>,
        I: Into<Term>,
    {
        Self::ArrGet(type_.into(), list.into(), index.into())
    }

    pub fn arr_slice<T, L, S, E>(type_: T, list: L, start: S, end: E) -> Self
    where
        T: Into<Term>,
        L: Into<Term>,
        S: Into<Term>,
        E: Into<Term>,
    {
        Self::ArrSlice(type_.into(), list.into(), start.into(), end.into())
    }

    pub fn arr_append<T, L, E>(type_: T, list: L, elem: E) -> Self
    where
        T: Into<Term>,
        L: Into<Term>,
        E: Into<Term>,
    {
        Self::ArrAppend(type_.into(), list.into(), elem.into())
    }

    pub fn arr_concat<T, O>(type_: T, operands: O) -> Self
    where
        T: Into<Term>,
        O: IntoIterator,
        O::Item: Into<Term>,
    {
        Self::ArrConcat(
            type_.into(),
            operands.into_iter().map(|e| e.into()).collect(),
        )
    }

    pub fn arr_map<A, B, F, R>(a: A, b: B, f: F, arr: R) -> Self
    where
        A: Into<Term>,
        B: Into<Term>,
        F: Into<Term>,
        R: Into<Term>,
    {
        Self::ArrMap(a.into(), b.into(), f.into(), arr.into())
    }

    pub fn cell_type<T>(elem: T) -> Self
    where
        T: Into<Term>,
    {
        Self::CellType(elem.into())
    }

    pub fn cell_new<T, I>(type_: T, init: I) -> Self
    where
        T: Into<Term>,
        I: Into<Term>,
    {
        Self::Cell(type_.into(), init.into())
    }

    pub fn cell_set<T, C, V>(type_: T, cell: C, value: V) -> Self
    where
        T: Into<Term>,
        C: Into<Term>,
        V: Into<Term>,
    {
        Self::CellSet(type_.into(), cell.into(), value.into())
    }

    pub fn cell_get<T, C>(type_: T, cell: C) -> Self
    where
        T: Into<Term>,
        C: Into<Term>,
    {
        Self::CellGet(type_.into(), cell.into())
    }

    /// Visit each `Term` operand of `self`, in field order. The single source of
    /// truth for which fields of a primitive are its term operands — `reach`,
    /// `collect_metavars`, and `collect_construction_names` all read it.
    /// (`traverse` keeps its own match: it rebuilds rather than visits.) The
    /// closure is taken `impl FnMut` so it monomorphises and inlines, leaving the
    /// de Bruijn / region hot path allocation- and indirection-free.
    pub fn for_each_operand(&self, visit: &mut impl FnMut(&Term)) {
        match self {
            Prim::BlnType
            | Prim::Bln(_)
            | Prim::NatType
            | Prim::Nat(Nat::Zero)
            | Prim::IntType
            | Prim::Int(_)
            | Prim::FltType
            | Prim::Flt(_)
            | Prim::BinType
            | Prim::Bin(_)
            | Prim::IoType
            | Prim::Io(_) => {}

            Prim::Nat(Nat::Succ(_, inner)) => visit(inner),

            Prim::FltToLeBin(t)
            | Prim::NatToInt(t)
            | Prim::NatToFlt(t)
            | Prim::IntToNat(t)
            | Prim::IntToFlt(t)
            | Prim::FltToNat(t)
            | Prim::FltToInt(t)
            | Prim::FltNeg(t)
            | Prim::FltAbs(t)
            | Prim::FltSqrt(t)
            | Prim::FltFloor(t)
            | Prim::FltCeil(t)
            | Prim::FltTrunc(t)
            | Prim::FltNearest(t)
            | Prim::BinLen(t)
            | Prim::ArrType(t) => visit(t),

            Prim::IoEql(a, b)
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
            | Prim::BlnAnd(a, b)
            | Prim::BlnOr(a, b)
            | Prim::BlnXor(a, b)
            | Prim::BlnEql(a, b)
            | Prim::BlnNeq(a, b)
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
            | Prim::BinEql(a, b)
            | Prim::BinGet(a, b)
            | Prim::BinAppend(a, b)
            | Prim::ArrLen(a, b)
            | Prim::IoExit(a, b) => {
                visit(a);
                visit(b);
            }

            Prim::BinSlice(a, b, c) | Prim::ArrGet(a, b, c) | Prim::ArrAppend(a, b, c) => {
                visit(a);
                visit(b);
                visit(c);
            }

            Prim::ArrSlice(a, b, c, d) | Prim::ArrMap(a, b, c, d) => {
                visit(a);
                visit(b);
                visit(c);
                visit(d);
            }

            Prim::Foreign(_, args) => args.iter().for_each(&mut *visit),

            Prim::BinConcat(terms) | Prim::Arr(terms) => terms.iter().for_each(&mut *visit),
            Prim::ArrConcat(ty, terms) => {
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

    pub fn reach(&self) -> usize {
        let mut reach = 0;
        self.for_each_operand(&mut |term| reach = reach.max(term.reach()));
        reach
    }

    pub fn any_metavar<F: FnMut(MetavarId) -> bool>(&self, pred: &mut F) -> bool {
        let mut found = false;
        self.for_each_operand(&mut |term| found = found || term.any_metavar(pred));
        found
    }

    pub fn collect_metavars(&self, ids: &mut BTreeSet<MetavarId>) {
        self.any_metavar(&mut |id| {
            ids.insert(id);
            false
        });
    }

    // Recurse into every operand `Term` so a construction nested inside a primitive
    // (e.g. `Arr(Str)`'s element type) still contributes its head name. Prims own no
    // head names of their own.
    pub fn collect_construction_names(&self, names: &mut BTreeSet<String>) {
        self.for_each_operand(&mut |term| term.collect_construction_names(names));
    }

    pub fn traverse<F>(&self, visit: &mut Visit<F>) -> Prim
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        match self {
            Prim::BlnType => Prim::BlnType,
            Prim::Bln(value) => Prim::Bln(*value),
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
            Prim::BlnAnd(l, r) => traverse_binary(l, r, visit, Prim::BlnAnd),
            Prim::BlnOr(l, r) => traverse_binary(l, r, visit, Prim::BlnOr),
            Prim::BlnXor(l, r) => traverse_binary(l, r, visit, Prim::BlnXor),
            Prim::BlnEql(l, r) => traverse_binary(l, r, visit, Prim::BlnEql),
            Prim::BlnNeq(l, r) => traverse_binary(l, r, visit, Prim::BlnNeq),
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
            Prim::FltNeg(inner) => Prim::FltNeg(visit.visit_subterm(inner)),
            Prim::FltAbs(inner) => Prim::FltAbs(visit.visit_subterm(inner)),
            Prim::FltSqrt(inner) => Prim::FltSqrt(visit.visit_subterm(inner)),
            Prim::FltFloor(inner) => Prim::FltFloor(visit.visit_subterm(inner)),
            Prim::FltCeil(inner) => Prim::FltCeil(visit.visit_subterm(inner)),
            Prim::FltTrunc(inner) => Prim::FltTrunc(visit.visit_subterm(inner)),
            Prim::FltNearest(inner) => Prim::FltNearest(visit.visit_subterm(inner)),
            Prim::FltToLeBin(inner) => Prim::FltToLeBin(visit.visit_subterm(inner)),
            Prim::NatToInt(inner) => Prim::NatToInt(visit.visit_subterm(inner)),
            Prim::NatToFlt(inner) => Prim::NatToFlt(visit.visit_subterm(inner)),
            Prim::IntToNat(inner) => Prim::IntToNat(visit.visit_subterm(inner)),
            Prim::IntToFlt(inner) => Prim::IntToFlt(visit.visit_subterm(inner)),
            Prim::FltToNat(inner) => Prim::FltToNat(visit.visit_subterm(inner)),
            Prim::FltToInt(inner) => Prim::FltToInt(visit.visit_subterm(inner)),
            Prim::BinType => Prim::BinType,
            Prim::Bin(bytes) => Prim::Bin(bytes.clone()),
            Prim::BinLen(bin) => Prim::BinLen(visit.visit_subterm(bin)),
            Prim::BinEql(l, r) => traverse_binary(l, r, visit, Prim::BinEql),
            Prim::BinGet(b, i) => traverse_binary(b, i, visit, Prim::BinGet),
            Prim::BinSlice(bin, start, end) => Prim::BinSlice(
                visit.visit_subterm(bin),
                visit.visit_subterm(start),
                visit.visit_subterm(end),
            ),
            Prim::BinAppend(b, byte) => traverse_binary(b, byte, visit, Prim::BinAppend),
            Prim::BinConcat(operands) => {
                Prim::BinConcat(operands.iter().map(|e| visit.visit_subterm(e)).collect())
            }
            Prim::ArrType(elem) => Prim::ArrType(visit.visit_subterm(elem)),
            Prim::Arr(elems) => Prim::Arr(elems.iter().map(|e| visit.visit_subterm(e)).collect()),
            Prim::ArrLen(ty, list) => traverse_binary(ty, list, visit, Prim::ArrLen),
            Prim::ArrGet(ty, list, index) => Prim::ArrGet(
                visit.visit_subterm(ty),
                visit.visit_subterm(list),
                visit.visit_subterm(index),
            ),
            Prim::ArrSlice(ty, list, start, end) => Prim::ArrSlice(
                visit.visit_subterm(ty),
                visit.visit_subterm(list),
                visit.visit_subterm(start),
                visit.visit_subterm(end),
            ),
            Prim::ArrAppend(ty, list, elem) => Prim::ArrAppend(
                visit.visit_subterm(ty),
                visit.visit_subterm(list),
                visit.visit_subterm(elem),
            ),
            Prim::ArrConcat(ty, operands) => Prim::ArrConcat(
                visit.visit_subterm(ty),
                operands.iter().map(|e| visit.visit_subterm(e)).collect(),
            ),
            Prim::ArrMap(a, b, f, arr) => Prim::ArrMap(
                visit.visit_subterm(a),
                visit.visit_subterm(b),
                visit.visit_subterm(f),
                visit.visit_subterm(arr),
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
