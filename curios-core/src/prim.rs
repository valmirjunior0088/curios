use {
    super::{Bound, MetaId, Nat, Subterm, Term, Var, Visit},
    curios_abi::WireType,
    curios_base::{Flt, Grain, Int, PackedBin},
    std::collections::BTreeSet,
};

/// The core type a host-boundary [`WireType`] denotes — the one reading of the signature shared by elaboration (operand checks, result records) and erasure, so the two cannot disagree about what crosses the wire.
pub fn wire_term(wire_type: &WireType) -> Term {
    let prim = match wire_type {
        WireType::Nat => Prim::NatType,
        WireType::Int => Prim::IntType,
        WireType::Bool => Prim::BoolType,
        WireType::Bytes => Prim::BinType(Grain::X),
        WireType::Handle => Prim::HandleType,
        WireType::Lst(element) => Prim::LstType(wire_term(&(*element).into())),
    };

    Subterm::Prim(prim).into()
}

/// The closed set of primitives of the core calculus: the built-in types (`BoolType`, `NatType`, `IntType`, `FltType`, `BinType`, `LstType`, `HandleType`, `CellType`, `IoType`), their literals, and the operator families over them, plus `ProcExit`. A host call is *not* here: [`Subterm::Foreign`] is a term former of its own, because what it means is read off an ABI row rather than fixed by this enum. Operand positions hold full [`Term`]s, so a primitive participates like any other subterm: elaboration checks operands against each variant's fixed signature, reduction constant-folds closed operands and rebuilds a canonical neutral otherwise, and erasure lowers each variant to its first-order IR op.
///
/// A primitive that performs a host effect returns an `Io`. That is the invariant the whole effect discipline rests on and it is enforced nowhere but here and in the two checkers' per-variant arms, so a new effectful variant must be given an `IoType` result when it is added.
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
    // A list literal, carrying its element type: the one value form whose elements alone cannot name it — `[]` has nothing to read a type from.
    Lst(Term, Vec<Term>),
    LstLen(Term, Term),
    LstGet(Term, Term, Term),
    LstSlice(Term, Term, Term, Term),
    LstAppend(Term, Term, Term),
    LstConcat(Term, Vec<Term>),
    // (@A, @B, lst : Lst(A), f : (A) -> B) -> Lst(B): a structural map. Opaque under reduction on a symbolic operand, so it never unfolds a variable during type-checking. Erases to a single O(n) fill loop.
    LstMap(Term, Term, Term, Term),
    HandleType,
    Handle(u32),
    // (a, b) -> Bool: identity of two handles. The one pure operation on `Handle` -- handles are opaque i31 tokens, so this erases to the `Nat` equality op.
    HandleEql(Term, Term),
    // `(Nat) -> {}`: end the process. Effectful, so reducing one at the type level is an error; it becomes a host call only at erasure.
    //
    // The result is the unit type, not the caller's choice. `exit` never returns, and a non-returning term is unsound exactly when it inhabits a type nothing total inhabits — it is the forgery that is the problem, not the non-return. At `{}` there is nothing to forge, which is the same property `Foreign` has for free by reading its result off an ABI row.
    ProcExit(Term),
    CellType(Term),
    Cell(Term, Term),          // type, init
    CellSet(Term, Term, Term), // type, cell, value
    CellGet(Term, Term),       // type, cell
    // The opaque carrier of a host effect: `Io(T)` is a *description* of a computation that yields a `T`, never the `T` itself.
    //
    // There is deliberately no eliminator from `Io(T)` to `T`, and there never may be. That absence is the whole of the referential-transparency story: a closure that performs an effect can only be given an `Io`-returning type, so every term of non-`Io` type denotes one value, and a scrutinee's spelling fixes it. The only consumer of an `Io(A)` is `IoBind`, which returns an `Io(B)`; nothing lowers the carrier to its content but the emitted entrypoint boundary, which forces the program's whole description exactly once.
    IoType(Term),
    // (@T, x : T) -> Io(T): the description that performs nothing and yields `x`.
    IoPure(Term, Term), // type, value
    // (@A, @B, m : Io(A), f : (A) -> Io(B)) -> Io(B): the description that performs `m`, then the description `f` computes from its result.
    //
    // Non-dependent in `B`, matching the `/syn/Monad` field it satisfies. Inert like the other two: no monad law holds definitionally, since nothing can be proven about an `Io` for a law to be useful about.
    IoBind(Term, Term, Term, Term), // from, to, action, continuation
}

impl Prim {
    /// A `NatAdd` node from anything term-shaped.
    /// A `NatEql` node from anything term-shaped.
    pub fn nat_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatEql(left.into(), right.into())
    }

    /// A `NatGt` node from anything term-shaped.
    pub fn nat_gt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatGt(left.into(), right.into())
    }

    /// A `NatGte` node from anything term-shaped.
    pub fn nat_gte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatGte(left.into(), right.into())
    }

    /// A `NatLte` node from anything term-shaped.
    pub fn nat_lte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatLte(left.into(), right.into())
    }

    /// A `NatNeq` node from anything term-shaped.
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

    /// A `NatLt` node from anything term-shaped.
    pub fn nat_lt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatLt(left.into(), right.into())
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

    /// A `LstMap` node from term-shaped source element type, target element type, list, and function — the collection first, like every other sequence operation.
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

    /// An `IoType` node from a term-shaped result type.
    pub fn io_type<T>(result: T) -> Self
    where
        T: Into<Term>,
    {
        Self::IoType(result.into())
    }

    /// An `IoPure` node from a term-shaped result type and value.
    pub fn io_pure<T, V>(type_: T, value: V) -> Self
    where
        T: Into<Term>,
        V: Into<Term>,
    {
        Self::IoPure(type_.into(), value.into())
    }

    /// An `IoBind` node from term-shaped source result type, target result type, action, and continuation.
    pub fn io_bind<A, B, M, F>(a: A, b: B, action: M, f: F) -> Self
    where
        A: Into<Term>,
        B: Into<Term>,
        M: Into<Term>,
        F: Into<Term>,
    {
        Self::IoBind(a.into(), b.into(), action.into(), f.into())
    }

    /// Visit each `Term` operand of `self`, in field order. The single source of truth for which fields of a primitive are its term operands — `reach`, `any_metavar`, and `collect_construction_names` all read it. (`traverse` keeps its own match: it rebuilds rather than visits.) The closure is taken `impl FnMut` so it monomorphises and inlines, leaving the de Bruijn / region hot path allocation- and indirection-free.
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
            | Prim::HandleType
            | Prim::Handle(_) => {}

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
            | Prim::LstType(t)
            | Prim::IoType(t)
            | Prim::ProcExit(t) => visit(t),

            Prim::HandleEql(a, b)
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
            | Prim::IoPure(a, b) => {
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

            Prim::LstSlice(a, b, c, d) | Prim::LstMap(a, b, c, d) | Prim::IoBind(a, b, c, d) => {
                visit(a);
                visit(b);
                visit(c);
                visit(d);
            }

            Prim::BinConcat(Grain::X, terms) | Prim::BinConcat(Grain::B, terms) => {
                terms.iter().for_each(&mut *visit)
            }

            Prim::Lst(ty, terms) | Prim::LstConcat(ty, terms) => {
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

    pub(crate) fn any_metavar<F: FnMut(MetaId) -> bool>(&self, pred: &mut F) -> bool {
        let mut found = false;
        self.for_each_operand(&mut |term| found = found || term.any_metavar(pred));
        found
    }

    /// Whether any operand `Term` satisfies `pred` — the `Prim` leg of `Subterm::any_child_term`, layered on the private operand walker like `any_metavar` above.
    pub fn any_term<F: FnMut(&Term) -> bool>(&self, pred: &mut F) -> bool {
        let mut found = false;
        self.for_each_operand(&mut |term| found = found || pred(term));
        found
    }

    // Recurse into every operand `Term` so a construction nested inside a primitive (e.g. `Lst(Str)`'s element type) still contributes its head name. Prims own no head names of their own.
    pub(crate) fn collect_construction_names(&self, names: &mut BTreeSet<crate::Global>) {
        self.for_each_operand(&mut |term| term.collect_construction_names(names));
    }

    pub fn traverse<F>(&self, visit: &mut Visit<F>) -> Prim
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
            Prim::HandleEql(l, r) => traverse_binary(l, r, visit, Prim::HandleEql),
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
            Prim::Lst(elem, elems) => Prim::Lst(
                visit.visit_subterm(elem),
                elems.iter().map(|e| visit.visit_subterm(e)).collect(),
            ),
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
            Prim::HandleType => Prim::HandleType,
            Prim::Handle(token) => Prim::Handle(*token),
            Prim::ProcExit(code) => Prim::ProcExit(visit.visit_subterm(code)),
            Prim::CellType(a) => Prim::CellType(visit.visit_subterm(a)),
            Prim::Cell(a, b) => traverse_binary(a, b, visit, Prim::Cell),
            Prim::CellGet(a, b) => traverse_binary(a, b, visit, Prim::CellGet),
            Prim::CellSet(a, b, c) => Prim::CellSet(
                visit.visit_subterm(a),
                visit.visit_subterm(b),
                visit.visit_subterm(c),
            ),
            Prim::IoType(a) => Prim::IoType(visit.visit_subterm(a)),
            Prim::IoPure(a, b) => traverse_binary(a, b, visit, Prim::IoPure),
            Prim::IoBind(a, b, action, f) => Prim::IoBind(
                visit.visit_subterm(a),
                visit.visit_subterm(b),
                visit.visit_subterm(action),
                visit.visit_subterm(f),
            ),
        }
    }
}

/// Which primitive type a match scrutinee is required to have. The legal selectors for `expect_prim_head`/`elaborate_prim_head` — exactly the type-former `Prim`s those helpers accept, as a closed set so an out-of-range selector is unrepresentable rather than an `unreachable!` panic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimHead {
    Nat,
    Bool,
    Bin(Grain),
}

/// Visit both operands of a binary primitive and rebuild it through `build`. The constructor is taken generically (not as a `fn` pointer) so every call site monomorphises to the same direct construction — this is the de Bruijn traversal hot path, so the indirection must vanish.
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
