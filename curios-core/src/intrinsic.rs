mod signature;
pub use signature::*;

use {
    super::{Bound, Nat, Subterm, Term, Var, Visit},
    curios_abi::WireType,
    curios_num::{Floating, Integer},
    curios_utilities::{Grain, PackedBin},
    std::collections::BTreeSet,
};

/// The core type a host-boundary [`WireType`] denotes — the one reading of the signature shared by elaboration (operand checks, result records) and erasure, so the two cannot disagree about what crosses the wire.
pub fn wire_term(wire_type: &WireType) -> Term {
    let intrinsic = match wire_type {
        WireType::Nat => Intrinsic::NatType,
        WireType::Int => Intrinsic::IntType,
        WireType::Bool => Intrinsic::BoolType,
        WireType::Bytes => Intrinsic::BinType(Grain::X),
        WireType::Handle => Intrinsic::HandleType,
        WireType::List(element) => Intrinsic::ListType(wire_term(&(*element).into())),
    };

    Subterm::Intrinsic(intrinsic).into()
}

/// The closed set of intrinsics of the core calculus: the built-in types (`BoolType`, `NatType`, `IntType`, `FltType`, `BinType`, `ListType`, `HandleType`, `CellType`, `IoType`), their literals, and the operator families over them, plus `ProcExit`. A host call is *not* here: [`Subterm::Foreign`] is a term former of its own, because what it means is read off an ABI row rather than fixed by this enum. Operand positions hold full [`Term`]s, so an intrinsic participates like any other subterm: elaboration checks operands against each variant's fixed signature, reduction constant-folds closed operands and rebuilds a canonical neutral otherwise, and erasure lowers each variant to its first-order IR op.
///
/// An intrinsic that performs a host effect returns an `Io`. That is the invariant the whole effect discipline rests on and it is enforced nowhere but here and in the two checkers' per-variant arms, so a new effectful variant must be given an `IoType` result when it is added.
///
/// The `impl` block's constructor helpers (`nat_add`, `bin_slice`, …) take `impl Into<Term>` operands, sparing builder call sites — reduction's neutral rebuilds and curios-text's lowering — the `.into()` noise.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum Intrinsic {
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
    /// `non_zero` proves `0 < divisor`. Carried as a field rather than left to `/sys`'s declaration so the obligation survives into Core, where the kernel re-checks it; [`Intrinsic::signature`] states the proposition it must inhabit.
    NatDiv {
        dividend: Term,
        divisor: Term,
        non_zero: Term,
    },
    NatRem {
        dividend: Term,
        divisor: Term,
        non_zero: Term,
    },
    NatLe(Term, Term),
    NatAnd(Term, Term),
    NatOr(Term, Term),
    NatXor(Term, Term),
    NatShl(Term, Term),
    NatShr(Term, Term),
    ByteType,
    Byte(u8),
    ByteToNat(Term),
    NatToByte(Term),
    ByteEql(Term, Term),
    ByteLt(Term, Term),
    ByteLe(Term, Term),
    IntType,
    Int(Integer),
    IntEql(Term, Term),
    IntNeq(Term, Term),
    IntAdd(Term, Term),
    IntSub(Term, Term),
    IntMul(Term, Term),
    /// The `Int` twin of [`Intrinsic::NatDiv`]'s bound, stated as `divisor != 0` — a negative divisor is fine, so `0 <` will not serve.
    IntDiv {
        dividend: Term,
        divisor: Term,
        non_zero: Term,
    },
    IntRem {
        dividend: Term,
        divisor: Term,
        non_zero: Term,
    },
    IntLt(Term, Term),
    IntLe(Term, Term),
    IntAnd(Term, Term),
    IntOr(Term, Term),
    IntXor(Term, Term),
    IntShl(Term, Term),
    IntShr(Term, Term),
    FltType,
    Flt(Floating),
    FltAdd(Term, Term),
    FltSub(Term, Term),
    FltMul(Term, Term),
    FltDiv(Term, Term),
    FltRem(Term, Term),
    FltEql(Term, Term),
    FltNeq(Term, Term),
    FltLt(Term, Term),
    FltLe(Term, Term),
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
    /// `non_neg` proves `0 <= int`, the domain the narrowing to `Nat` has. Carried for the reason [`Intrinsic::NatDiv`]'s bound is: a bound stated only on `/sys`'s wrapper stops constraining anything the moment that wrapper unfolds, leaving the kernel a bare narrowing to admit.
    IntToNat {
        int: Term,
        non_neg: Term,
    },
    IntToFlt(Term),
    /// `non_neg` proves the operand is a non-negative *number*, which is what excludes `+inf` and the NaN alongside every negative. Carried for the reason [`Intrinsic::NatDiv`]'s bound is.
    FltToNat {
        flt: Term,
        non_neg: Term,
    },
    FltToLeBytes(Term),
    /// `four_bytes` proves `len(bin) = 4`, which is the whole of what reinterpreting a binary32 needs.
    FltOfLeBytes {
        bin: Term,
        four_bytes: Term,
    },
    /// `finite` proves the operand is a number — neither infinity, and not the NaN — which is the whole of what truncating toward zero needs.
    FltToInt {
        flt: Term,
        finite: Term,
    },
    BinType(Grain),
    Bin(Grain, PackedBin),
    BinLen(Grain, Term),
    BinEql(Grain, Term, Term),
    /// `in_range` proves `index < len(bin)`, carried for the reason [`Intrinsic::NatDiv`]'s bound is.
    BinGet {
        grain: Grain,
        bin: Term,
        index: Term,
        in_range: Term,
    },
    /// `within` proves `start + length <= len(bin)` — the one bound a window has, since a count cannot spell a reversed range.
    BinSlice {
        grain: Grain,
        bin: Term,
        start: Term,
        length: Term,
        within: Term,
    },
    BinAppend {
        grain: Grain,
        bin: Term,
        element: Term,
    },
    BinConcat {
        grain: Grain,
        operands: Vec<Term>,
    },
    ListType(Term),
    // A list literal, carrying its element type: the one value form whose elements alone cannot name it — `[]` has nothing to read a type from.
    List {
        element: Term,
        items: Vec<Term>,
    },
    ListLen {
        element: Term,
        list: Term,
    },
    ListGet {
        element: Term,
        list: Term,
        index: Term,
        in_range: Term,
    },
    ListSlice {
        element: Term,
        list: Term,
        start: Term,
        length: Term,
        within: Term,
    },
    ListAppend {
        element: Term,
        list: Term,
        item: Term,
    },
    ListConcat {
        element: Term,
        operands: Vec<Term>,
    },
    // (@A, @B, list : List(A), f : (A) -> B) -> List(B): a structural map. Opaque under reduction on a symbolic operand, so it never unfolds a variable during type-checking. Erases to a single O(n) fill loop.
    ListMap {
        from: Term,
        to: Term,
        list: Term,
        function: Term,
    },
    HandleType,
    Handle(u32),
    // (a, b) -> Bool: identity of two handles. The one pure operation on `Handle` -- handles are opaque i31 tokens, so this erases to the `Nat` equality op.
    HandleEql(Term, Term),
    // `(Nat) -> Io({})`: end the process. Like every host operation it denotes an inert description here and becomes a host call only at erasure.
    //
    // The description's payload is the unit type, not the caller's choice. `exit` never returns, and a non-returning term is unsound exactly when it inhabits a type nothing total inhabits — it is the forgery that is the problem, not the non-return. At `{}` there is nothing to forge, which is the same property `Foreign` has for free by reading its result off an ABI row.
    ProcExit(Term),
    CellType(Term),
    Cell {
        element: Term,
        initial: Term,
    },
    CellSet {
        element: Term,
        cell: Term,
        value: Term,
    },
    CellGet {
        element: Term,
        cell: Term,
    },
    // The opaque carrier of a host effect: `Io(T)` is a *description* of a computation that yields a `T`, never the `T` itself.
    //
    // There is deliberately no eliminator from `Io(T)` to `T`, and there never may be. That absence is the whole of the referential-transparency story: a closure that performs an effect can only be given an `Io`-returning type, so every term of non-`Io` type denotes one value, and a scrutinee's spelling fixes it. The only consumer of an `Io(A)` is `IoBind`, which returns an `Io(B)`; nothing lowers the carrier to its content but the emitted entrypoint boundary, which forces the program's whole description exactly once.
    IoType(Term),
    // (@T, x : T) -> Io(T): the description that performs nothing and yields `x`.
    IoPure {
        result: Term,
        value: Term,
    },
    // (@A, @B, m : Io(A), f : (A) -> Io(B)) -> Io(B): the description that performs `m`, then the description `f` computes from its result.
    //
    // Non-dependent in `B`, matching the `/syn/Monad` field it satisfies. Inert like the other two: no monad law holds definitionally, since nothing can be proven about an `Io` for a law to be useful about.
    IoBind {
        from: Term,
        to: Term,
        action: Term,
        continuation: Term,
    },
}

impl Intrinsic {
    /// A `NatEql` node from anything term-shaped.
    pub fn nat_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatEql(left.into(), right.into())
    }

    /// A `NatLe` node from anything term-shaped.
    pub fn nat_lte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatLe(left.into(), right.into())
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
    pub fn bin_get<B, I, P>(grain: Grain, bin: B, index: I, in_range: P) -> Self
    where
        B: Into<Term>,
        I: Into<Term>,
        P: Into<Term>,
    {
        Self::BinGet {
            grain,
            bin: bin.into(),
            index: index.into(),
            in_range: in_range.into(),
        }
    }

    /// A `BinSlice` node from term-shaped bytes, start, and length.
    pub fn bin_slice<B, S, L, P>(grain: Grain, bin: B, start: S, length: L, within: P) -> Self
    where
        B: Into<Term>,
        S: Into<Term>,
        L: Into<Term>,
        P: Into<Term>,
    {
        Self::BinSlice {
            grain,
            bin: bin.into(),
            start: start.into(),
            length: length.into(),
            within: within.into(),
        }
    }

    /// A `BinAppend` node from term-shaped bytes and byte.
    pub fn bin_append<B, E>(grain: Grain, bin: B, byte: E) -> Self
    where
        B: Into<Term>,
        E: Into<Term>,
    {
        Self::BinAppend {
            grain,
            bin: bin.into(),
            element: byte.into(),
        }
    }

    /// A `BinConcat` node from any iterator of term-shaped operands.
    pub fn bin_concat<I>(grain: Grain, operands: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Term>,
    {
        Self::BinConcat {
            grain,
            operands: operands.into_iter().map(|e| e.into()).collect(),
        }
    }

    /// A `ListType` node from a term-shaped element type.
    pub fn list_type<T>(elem: T) -> Self
    where
        T: Into<Term>,
    {
        Self::ListType(elem.into())
    }

    /// A `ListLen` node from term-shaped element type and list.
    pub fn list_len<T, L>(type_: T, list: L) -> Self
    where
        T: Into<Term>,
        L: Into<Term>,
    {
        Self::ListLen {
            element: type_.into(),
            list: list.into(),
        }
    }

    /// A `ListGet` node from term-shaped element type, list, and index.
    pub fn list_get<T, L, I, P>(type_: T, list: L, index: I, in_range: P) -> Self
    where
        T: Into<Term>,
        L: Into<Term>,
        I: Into<Term>,
        P: Into<Term>,
    {
        Self::ListGet {
            element: type_.into(),
            list: list.into(),
            index: index.into(),
            in_range: in_range.into(),
        }
    }

    /// A `ListSlice` node from term-shaped element type, list, start, and length.
    pub fn list_slice<T, L, S, N, P>(type_: T, list: L, start: S, length: N, within: P) -> Self
    where
        T: Into<Term>,
        L: Into<Term>,
        S: Into<Term>,
        N: Into<Term>,
        P: Into<Term>,
    {
        Self::ListSlice {
            element: type_.into(),
            list: list.into(),
            start: start.into(),
            length: length.into(),
            within: within.into(),
        }
    }

    /// A `ListAppend` node from term-shaped element type, list, and element.
    pub fn list_append<T, L, E>(type_: T, list: L, elem: E) -> Self
    where
        T: Into<Term>,
        L: Into<Term>,
        E: Into<Term>,
    {
        Self::ListAppend {
            element: type_.into(),
            list: list.into(),
            item: elem.into(),
        }
    }

    /// A `ListConcat` node from a term-shaped element type and any iterator of term-shaped operands.
    pub fn list_concat<T, O>(type_: T, operands: O) -> Self
    where
        T: Into<Term>,
        O: IntoIterator,
        O::Item: Into<Term>,
    {
        Self::ListConcat {
            element: type_.into(),
            operands: operands.into_iter().map(|e| e.into()).collect(),
        }
    }

    /// A `ListMap` node from term-shaped source element type, target element type, list, and function — the collection first, like every other sequence operation.
    pub fn list_map<A, B, R, F>(a: A, b: B, list: R, f: F) -> Self
    where
        A: Into<Term>,
        B: Into<Term>,
        R: Into<Term>,
        F: Into<Term>,
    {
        Self::ListMap {
            from: a.into(),
            to: b.into(),
            list: list.into(),
            function: f.into(),
        }
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
        Self::IoPure {
            result: type_.into(),
            value: value.into(),
        }
    }

    /// An `IoBind` node from term-shaped source result type, target result type, action, and continuation.
    pub fn io_bind<A, B, M, F>(a: A, b: B, action: M, f: F) -> Self
    where
        A: Into<Term>,
        B: Into<Term>,
        M: Into<Term>,
        F: Into<Term>,
    {
        Self::IoBind {
            from: a.into(),
            to: b.into(),
            action: action.into(),
            continuation: f.into(),
        }
    }

    /// This operation's term operands, in the same order `for_each_operand` visits them — which is the order [`signature`](Self::signature) states their types in.
    ///
    /// Collected rather than visited because the checking walks zip it against that table, and a `zip` needs a sequence. Not for the hot paths: `reach` and `any_metavar` keep the visiting form precisely so they allocate nothing.
    pub fn operands(&self) -> Vec<&Term> {
        let mut operands = Vec::new();
        self.for_each_operand(&mut |operand| operands.push(operand));

        operands
    }

    /// Visit each `Term` operand of `self`, in field order. The single source of truth for which fields of an intrinsic are its term operands — `reach`, `any_metavar`, and `collect_construction_names` all read it. (`traverse` keeps its own match: it rebuilds rather than visits.) The closure is taken `impl FnMut` so it monomorphises and inlines, leaving the de Bruijn / region hot path allocation- and indirection-free.
    fn for_each_operand<'a>(&'a self, visit: &mut impl FnMut(&'a Term)) {
        match self {
            Intrinsic::BoolType
            | Intrinsic::Bool(_)
            | Intrinsic::NatType
            | Intrinsic::Nat(Nat::Zero)
            | Intrinsic::ByteType
            | Intrinsic::Byte(_)
            | Intrinsic::IntType
            | Intrinsic::Int(_)
            | Intrinsic::FltType
            | Intrinsic::Flt(_)
            | Intrinsic::BinType(Grain::X)
            | Intrinsic::Bin(Grain::X, _)
            | Intrinsic::BinType(Grain::B)
            | Intrinsic::Bin(Grain::B, _)
            | Intrinsic::HandleType
            | Intrinsic::Handle(_) => {}

            Intrinsic::Nat(Nat::Succ(_, inner)) => visit(inner),

            Intrinsic::FltToLeBytes(t)
            | Intrinsic::NatToInt(t)
            | Intrinsic::NatToFlt(t)
            | Intrinsic::IntToFlt(t)
            | Intrinsic::ByteToNat(t)
            | Intrinsic::NatToByte(t)
            | Intrinsic::FltNeg(t)
            | Intrinsic::FltAbs(t)
            | Intrinsic::FltSqrt(t)
            | Intrinsic::FltFloor(t)
            | Intrinsic::FltCeil(t)
            | Intrinsic::FltTrunc(t)
            | Intrinsic::FltNearest(t)
            | Intrinsic::BinLen(Grain::X, t)
            | Intrinsic::BinLen(Grain::B, t)
            | Intrinsic::ListType(t)
            | Intrinsic::IoType(t)
            | Intrinsic::ProcExit(t) => visit(t),

            Intrinsic::HandleEql(a, b)
            | Intrinsic::ByteEql(a, b)
            | Intrinsic::ByteLt(a, b)
            | Intrinsic::ByteLe(a, b)
            | Intrinsic::NatEql(a, b)
            | Intrinsic::NatNeq(a, b)
            | Intrinsic::NatAdd(a, b)
            | Intrinsic::NatSub(a, b)
            | Intrinsic::NatMul(a, b)
            | Intrinsic::NatLt(a, b)
            | Intrinsic::NatLe(a, b)
            | Intrinsic::NatAnd(a, b)
            | Intrinsic::NatOr(a, b)
            | Intrinsic::NatXor(a, b)
            | Intrinsic::NatShl(a, b)
            | Intrinsic::NatShr(a, b)
            | Intrinsic::BoolAnd(a, b)
            | Intrinsic::BoolOr(a, b)
            | Intrinsic::BoolXor(a, b)
            | Intrinsic::BoolEql(a, b)
            | Intrinsic::BoolNeq(a, b)
            | Intrinsic::IntEql(a, b)
            | Intrinsic::IntNeq(a, b)
            | Intrinsic::IntAdd(a, b)
            | Intrinsic::IntSub(a, b)
            | Intrinsic::IntMul(a, b)
            | Intrinsic::IntLt(a, b)
            | Intrinsic::IntLe(a, b)
            | Intrinsic::IntAnd(a, b)
            | Intrinsic::IntOr(a, b)
            | Intrinsic::IntXor(a, b)
            | Intrinsic::IntShl(a, b)
            | Intrinsic::IntShr(a, b)
            | Intrinsic::FltAdd(a, b)
            | Intrinsic::FltSub(a, b)
            | Intrinsic::FltMul(a, b)
            | Intrinsic::FltDiv(a, b)
            | Intrinsic::FltRem(a, b)
            | Intrinsic::FltEql(a, b)
            | Intrinsic::FltNeq(a, b)
            | Intrinsic::FltLt(a, b)
            | Intrinsic::FltLe(a, b)
            | Intrinsic::FltMin(a, b)
            | Intrinsic::FltMax(a, b)
            | Intrinsic::FltCopysign(a, b)
            | Intrinsic::BinEql(Grain::X, a, b)
            | Intrinsic::BinAppend {
                grain: Grain::X,
                bin: a,
                element: b,
            }
            | Intrinsic::BinEql(Grain::B, a, b)
            | Intrinsic::BinAppend {
                grain: Grain::B,
                bin: a,
                element: b,
            }
            | Intrinsic::ListLen {
                element: a,
                list: b,
            }
            | Intrinsic::IoPure {
                result: a,
                value: b,
            } => {
                visit(a);
                visit(b);
            }

            Intrinsic::ListAppend {
                element: a,
                list: b,
                item: c,
            } => {
                visit(a);
                visit(b);
                visit(c);
            }

            Intrinsic::ListMap {
                from: a,
                to: b,
                list: c,
                function: d,
            }
            | Intrinsic::IoBind {
                from: a,
                to: b,
                action: c,
                continuation: d,
            } => {
                visit(a);
                visit(b);
                visit(c);
                visit(d);
            }

            // The bounded operations walk their proof beside their value operands, and must: substitution reaches a term only through this walk, and a bound's proposition mentions the very operands around it. Congruence still declines to compare the proof *structurally* — it enqueues it at that proposition instead, where irrelevance applies (see [`Intrinsic::signature`]).
            Intrinsic::NatDiv {
                dividend: a,
                divisor: b,
                non_zero: p,
            }
            | Intrinsic::NatRem {
                dividend: a,
                divisor: b,
                non_zero: p,
            }
            | Intrinsic::IntDiv {
                dividend: a,
                divisor: b,
                non_zero: p,
            }
            | Intrinsic::IntRem {
                dividend: a,
                divisor: b,
                non_zero: p,
            } => {
                visit(a);
                visit(b);
                visit(p);
            }

            Intrinsic::FltOfLeBytes {
                bin: a,
                four_bytes: p,
            }
            | Intrinsic::IntToNat { int: a, non_neg: p }
            | Intrinsic::FltToNat { flt: a, non_neg: p }
            | Intrinsic::FltToInt { flt: a, finite: p } => {
                visit(a);
                visit(p);
            }

            // The four sequence accessors, beside the division family for the same reason. Their arity differs by carrier — a `List` operation leads with its element type — so they group by operand count rather than by which one is the proof, and the proof is last in every one of them, matching `signature`'s order.
            Intrinsic::BinGet {
                grain: _,
                bin: a,
                index: b,
                in_range: p,
            } => {
                visit(a);
                visit(b);
                visit(p);
            }

            Intrinsic::BinSlice {
                grain: _,
                bin: a,
                start: b,
                length: c,
                within: p,
            }
            | Intrinsic::ListGet {
                element: a,
                list: b,
                index: c,
                in_range: p,
            } => {
                visit(a);
                visit(b);
                visit(c);
                visit(p);
            }

            Intrinsic::ListSlice {
                element: a,
                list: b,
                start: c,
                length: d,
                within: p,
            } => {
                visit(a);
                visit(b);
                visit(c);
                visit(d);
                visit(p);
            }

            Intrinsic::BinConcat {
                grain: Grain::X,
                operands: terms,
            }
            | Intrinsic::BinConcat {
                grain: Grain::B,
                operands: terms,
            } => terms.iter().for_each(&mut *visit),

            Intrinsic::List {
                element: ty,
                items: terms,
            }
            | Intrinsic::ListConcat {
                element: ty,
                operands: terms,
            } => {
                visit(ty);
                terms.iter().for_each(&mut *visit);
            }

            Intrinsic::CellType(a) => visit(a),
            Intrinsic::Cell {
                element: a,
                initial: b,
            }
            | Intrinsic::CellGet {
                element: a,
                cell: b,
            } => {
                visit(a);
                visit(b);
            }
            Intrinsic::CellSet {
                element: a,
                cell: b,
                value: c,
            } => {
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

    /// The logical units this node's *own* payload occupies, excluding every child term.
    ///
    /// Only three variants carry payload a child term does not: a packed binary run, a natural's successor floor, and an integer. Everything else is either a scalar the node constant already covers or a child whose own footprint counts it — a `List`'s element vector included, since each element is a term with a footprint of its own and a slot costs less than the node it points at.
    ///
    /// Conservative in the one direction that matters: this feeds the retention quota, where overcounting costs a cold cache and undercounting costs the bound.
    pub(crate) fn payload_units(&self) -> u64 {
        match self {
            Intrinsic::Bin(grain, value) => match grain {
                Grain::X => (value.bit_length() as u64).div_ceil(64),
                Grain::B => (value.bit_length() as u64).div_ceil(64),
            },
            Intrinsic::Nat(Nat::Succ(floor, _)) => floor.bits().div_ceil(64),
            Intrinsic::Int(value) => value.bits().div_ceil(64),
            _ => 0,
        }
    }

    /// Whether any operand `Term` satisfies `pred` — the `Intrinsic` leg of `Subterm::any_child_term`, layered on the private operand walker.
    pub fn any_term<F: FnMut(&Term) -> bool>(&self, pred: &mut F) -> bool {
        let mut found = false;
        self.for_each_operand(&mut |term| found = found || pred(term));
        found
    }

    // Recurse into every operand `Term` so a construction nested inside an intrinsic (e.g. `List(Str)`'s element type) still contributes its head name. Intrinsics own no head names of their own.
    pub(crate) fn collect_construction_names(&self, names: &mut BTreeSet<crate::Global>) {
        self.for_each_operand(&mut |term| term.collect_construction_names(names));
    }

    pub fn traverse<F>(&self, visit: &mut Visit<F>) -> Intrinsic
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        match self {
            Intrinsic::BoolType => Intrinsic::BoolType,
            Intrinsic::Bool(value) => Intrinsic::Bool(*value),
            Intrinsic::NatType => Intrinsic::NatType,
            Intrinsic::Nat(Nat::Zero) => Intrinsic::Nat(Nat::Zero),
            Intrinsic::Nat(Nat::Succ(spine, inner)) => {
                Intrinsic::Nat(Nat::Succ(spine.clone(), visit.visit_subterm(inner)))
            }
            Intrinsic::NatEql(l, r) => traverse_binary(l, r, visit, Intrinsic::NatEql),
            Intrinsic::HandleEql(l, r) => traverse_binary(l, r, visit, Intrinsic::HandleEql),
            Intrinsic::NatNeq(l, r) => traverse_binary(l, r, visit, Intrinsic::NatNeq),
            Intrinsic::NatAdd(l, r) => traverse_binary(l, r, visit, Intrinsic::NatAdd),
            Intrinsic::NatSub(l, r) => traverse_binary(l, r, visit, Intrinsic::NatSub),
            Intrinsic::NatMul(l, r) => traverse_binary(l, r, visit, Intrinsic::NatMul),
            Intrinsic::NatLt(l, r) => traverse_binary(l, r, visit, Intrinsic::NatLt),
            Intrinsic::NatDiv {
                dividend: a,
                divisor: b,
                non_zero: p,
            } => Intrinsic::NatDiv {
                dividend: visit.visit_subterm(a),
                divisor: visit.visit_subterm(b),
                non_zero: visit.visit_subterm(p),
            },
            Intrinsic::NatRem {
                dividend: a,
                divisor: b,
                non_zero: p,
            } => Intrinsic::NatRem {
                dividend: visit.visit_subterm(a),
                divisor: visit.visit_subterm(b),
                non_zero: visit.visit_subterm(p),
            },
            Intrinsic::NatLe(l, r) => traverse_binary(l, r, visit, Intrinsic::NatLe),
            Intrinsic::NatAnd(l, r) => traverse_binary(l, r, visit, Intrinsic::NatAnd),
            Intrinsic::NatOr(l, r) => traverse_binary(l, r, visit, Intrinsic::NatOr),
            Intrinsic::NatXor(l, r) => traverse_binary(l, r, visit, Intrinsic::NatXor),
            Intrinsic::NatShl(l, r) => traverse_binary(l, r, visit, Intrinsic::NatShl),
            Intrinsic::NatShr(l, r) => traverse_binary(l, r, visit, Intrinsic::NatShr),
            Intrinsic::ByteType => Intrinsic::ByteType,
            Intrinsic::Byte(value) => Intrinsic::Byte(*value),
            Intrinsic::ByteToNat(inner) => Intrinsic::ByteToNat(visit.visit_subterm(inner)),
            Intrinsic::NatToByte(inner) => Intrinsic::NatToByte(visit.visit_subterm(inner)),
            Intrinsic::ByteEql(l, r) => traverse_binary(l, r, visit, Intrinsic::ByteEql),
            Intrinsic::ByteLt(l, r) => traverse_binary(l, r, visit, Intrinsic::ByteLt),
            Intrinsic::ByteLe(l, r) => traverse_binary(l, r, visit, Intrinsic::ByteLe),
            Intrinsic::BoolAnd(l, r) => traverse_binary(l, r, visit, Intrinsic::BoolAnd),
            Intrinsic::BoolOr(l, r) => traverse_binary(l, r, visit, Intrinsic::BoolOr),
            Intrinsic::BoolXor(l, r) => traverse_binary(l, r, visit, Intrinsic::BoolXor),
            Intrinsic::BoolEql(l, r) => traverse_binary(l, r, visit, Intrinsic::BoolEql),
            Intrinsic::BoolNeq(l, r) => traverse_binary(l, r, visit, Intrinsic::BoolNeq),
            Intrinsic::IntType => Intrinsic::IntType,
            Intrinsic::Int(value) => Intrinsic::Int(value.clone()),
            Intrinsic::IntEql(l, r) => traverse_binary(l, r, visit, Intrinsic::IntEql),
            Intrinsic::IntNeq(l, r) => traverse_binary(l, r, visit, Intrinsic::IntNeq),
            Intrinsic::IntAdd(l, r) => traverse_binary(l, r, visit, Intrinsic::IntAdd),
            Intrinsic::IntSub(l, r) => traverse_binary(l, r, visit, Intrinsic::IntSub),
            Intrinsic::IntMul(l, r) => traverse_binary(l, r, visit, Intrinsic::IntMul),
            Intrinsic::IntDiv {
                dividend: a,
                divisor: b,
                non_zero: p,
            } => Intrinsic::IntDiv {
                dividend: visit.visit_subterm(a),
                divisor: visit.visit_subterm(b),
                non_zero: visit.visit_subterm(p),
            },
            Intrinsic::IntRem {
                dividend: a,
                divisor: b,
                non_zero: p,
            } => Intrinsic::IntRem {
                dividend: visit.visit_subterm(a),
                divisor: visit.visit_subterm(b),
                non_zero: visit.visit_subterm(p),
            },
            Intrinsic::IntLt(l, r) => traverse_binary(l, r, visit, Intrinsic::IntLt),
            Intrinsic::IntLe(l, r) => traverse_binary(l, r, visit, Intrinsic::IntLe),
            Intrinsic::IntAnd(l, r) => traverse_binary(l, r, visit, Intrinsic::IntAnd),
            Intrinsic::IntOr(l, r) => traverse_binary(l, r, visit, Intrinsic::IntOr),
            Intrinsic::IntXor(l, r) => traverse_binary(l, r, visit, Intrinsic::IntXor),
            Intrinsic::IntShl(l, r) => traverse_binary(l, r, visit, Intrinsic::IntShl),
            Intrinsic::IntShr(l, r) => traverse_binary(l, r, visit, Intrinsic::IntShr),
            Intrinsic::FltType => Intrinsic::FltType,
            Intrinsic::Flt(flt) => Intrinsic::Flt(*flt),
            Intrinsic::FltAdd(l, r) => traverse_binary(l, r, visit, Intrinsic::FltAdd),
            Intrinsic::FltSub(l, r) => traverse_binary(l, r, visit, Intrinsic::FltSub),
            Intrinsic::FltMul(l, r) => traverse_binary(l, r, visit, Intrinsic::FltMul),
            Intrinsic::FltDiv(l, r) => traverse_binary(l, r, visit, Intrinsic::FltDiv),
            Intrinsic::FltRem(l, r) => traverse_binary(l, r, visit, Intrinsic::FltRem),
            Intrinsic::FltEql(l, r) => traverse_binary(l, r, visit, Intrinsic::FltEql),
            Intrinsic::FltNeq(l, r) => traverse_binary(l, r, visit, Intrinsic::FltNeq),
            Intrinsic::FltLt(l, r) => traverse_binary(l, r, visit, Intrinsic::FltLt),
            Intrinsic::FltLe(l, r) => traverse_binary(l, r, visit, Intrinsic::FltLe),
            Intrinsic::FltMin(l, r) => traverse_binary(l, r, visit, Intrinsic::FltMin),
            Intrinsic::FltMax(l, r) => traverse_binary(l, r, visit, Intrinsic::FltMax),
            Intrinsic::FltCopysign(l, r) => traverse_binary(l, r, visit, Intrinsic::FltCopysign),
            Intrinsic::FltNeg(inner) => Intrinsic::FltNeg(visit.visit_subterm(inner)),
            Intrinsic::FltAbs(inner) => Intrinsic::FltAbs(visit.visit_subterm(inner)),
            Intrinsic::FltSqrt(inner) => Intrinsic::FltSqrt(visit.visit_subterm(inner)),
            Intrinsic::FltFloor(inner) => Intrinsic::FltFloor(visit.visit_subterm(inner)),
            Intrinsic::FltCeil(inner) => Intrinsic::FltCeil(visit.visit_subterm(inner)),
            Intrinsic::FltTrunc(inner) => Intrinsic::FltTrunc(visit.visit_subterm(inner)),
            Intrinsic::FltNearest(inner) => Intrinsic::FltNearest(visit.visit_subterm(inner)),
            Intrinsic::FltToLeBytes(inner) => Intrinsic::FltToLeBytes(visit.visit_subterm(inner)),
            Intrinsic::FltOfLeBytes { bin, four_bytes } => Intrinsic::FltOfLeBytes {
                bin: visit.visit_subterm(bin),
                four_bytes: visit.visit_subterm(four_bytes),
            },
            Intrinsic::NatToInt(inner) => Intrinsic::NatToInt(visit.visit_subterm(inner)),
            Intrinsic::NatToFlt(inner) => Intrinsic::NatToFlt(visit.visit_subterm(inner)),
            Intrinsic::IntToNat { int, non_neg } => Intrinsic::IntToNat {
                int: visit.visit_subterm(int),
                non_neg: visit.visit_subterm(non_neg),
            },
            Intrinsic::IntToFlt(inner) => Intrinsic::IntToFlt(visit.visit_subterm(inner)),
            Intrinsic::FltToNat { flt, non_neg } => Intrinsic::FltToNat {
                flt: visit.visit_subterm(flt),
                non_neg: visit.visit_subterm(non_neg),
            },
            Intrinsic::FltToInt { flt, finite } => Intrinsic::FltToInt {
                flt: visit.visit_subterm(flt),
                finite: visit.visit_subterm(finite),
            },
            Intrinsic::BinType(grain) => Intrinsic::BinType(*grain),
            Intrinsic::Bin(grain, value) => Intrinsic::Bin(*grain, value.clone()),
            Intrinsic::BinLen(grain, bin) => Intrinsic::BinLen(*grain, visit.visit_subterm(bin)),
            Intrinsic::BinEql(grain, l, r) => {
                traverse_binary(l, r, visit, |l, r| Intrinsic::BinEql(*grain, l, r))
            }
            Intrinsic::BinGet {
                grain,
                bin,
                index,
                in_range,
            } => Intrinsic::BinGet {
                grain: *grain,
                bin: visit.visit_subterm(bin),
                index: visit.visit_subterm(index),
                in_range: visit.visit_subterm(in_range),
            },
            Intrinsic::BinSlice {
                grain,
                bin,
                start,
                length,
                within,
            } => Intrinsic::BinSlice {
                grain: *grain,
                bin: visit.visit_subterm(bin),
                start: visit.visit_subterm(start),
                length: visit.visit_subterm(length),
                within: visit.visit_subterm(within),
            },
            Intrinsic::BinAppend {
                grain,
                bin: b,
                element: atom,
            } => traverse_binary(b, atom, visit, |b, atom| Intrinsic::BinAppend {
                grain: *grain,
                bin: b,
                element: atom,
            }),
            Intrinsic::BinConcat { grain, operands } => Intrinsic::BinConcat {
                grain: *grain,
                operands: operands.iter().map(|e| visit.visit_subterm(e)).collect(),
            },
            Intrinsic::ListType(elem) => Intrinsic::ListType(visit.visit_subterm(elem)),
            Intrinsic::List {
                element: elem,
                items: elems,
            } => Intrinsic::List {
                element: visit.visit_subterm(elem),
                items: elems.iter().map(|e| visit.visit_subterm(e)).collect(),
            },
            Intrinsic::ListLen { element: ty, list } => {
                traverse_binary(ty, list, visit, |element, list| Intrinsic::ListLen {
                    element,
                    list,
                })
            }
            Intrinsic::ListGet {
                element: ty,
                list,
                index,
                in_range,
            } => Intrinsic::ListGet {
                element: visit.visit_subterm(ty),
                list: visit.visit_subterm(list),
                index: visit.visit_subterm(index),
                in_range: visit.visit_subterm(in_range),
            },
            Intrinsic::ListSlice {
                element: ty,
                list,
                start,
                length,
                within,
            } => Intrinsic::ListSlice {
                element: visit.visit_subterm(ty),
                list: visit.visit_subterm(list),
                start: visit.visit_subterm(start),
                length: visit.visit_subterm(length),
                within: visit.visit_subterm(within),
            },
            Intrinsic::ListAppend {
                element: ty,
                list,
                item: elem,
            } => Intrinsic::ListAppend {
                element: visit.visit_subterm(ty),
                list: visit.visit_subterm(list),
                item: visit.visit_subterm(elem),
            },
            Intrinsic::ListConcat {
                element: ty,
                operands,
            } => Intrinsic::ListConcat {
                element: visit.visit_subterm(ty),
                operands: operands.iter().map(|e| visit.visit_subterm(e)).collect(),
            },
            Intrinsic::ListMap {
                from: a,
                to: b,
                list,
                function: f,
            } => Intrinsic::ListMap {
                from: visit.visit_subterm(a),
                to: visit.visit_subterm(b),
                list: visit.visit_subterm(list),
                function: visit.visit_subterm(f),
            },
            Intrinsic::HandleType => Intrinsic::HandleType,
            Intrinsic::Handle(token) => Intrinsic::Handle(*token),
            Intrinsic::ProcExit(code) => Intrinsic::ProcExit(visit.visit_subterm(code)),
            Intrinsic::CellType(a) => Intrinsic::CellType(visit.visit_subterm(a)),
            Intrinsic::Cell {
                element: a,
                initial: b,
            } => traverse_binary(a, b, visit, |element, initial| Intrinsic::Cell {
                element,
                initial,
            }),
            Intrinsic::CellGet {
                element: a,
                cell: b,
            } => traverse_binary(a, b, visit, |element, cell| Intrinsic::CellGet {
                element,
                cell,
            }),
            Intrinsic::CellSet {
                element: a,
                cell: b,
                value: c,
            } => Intrinsic::CellSet {
                element: visit.visit_subterm(a),
                cell: visit.visit_subterm(b),
                value: visit.visit_subterm(c),
            },
            Intrinsic::IoType(a) => Intrinsic::IoType(visit.visit_subterm(a)),
            Intrinsic::IoPure {
                result: a,
                value: b,
            } => traverse_binary(a, b, visit, |result, value| Intrinsic::IoPure {
                result,
                value,
            }),
            Intrinsic::IoBind {
                from: a,
                to: b,
                action,
                continuation: f,
            } => Intrinsic::IoBind {
                from: visit.visit_subterm(a),
                to: visit.visit_subterm(b),
                action: visit.visit_subterm(action),
                continuation: visit.visit_subterm(f),
            },
        }
    }
}

/// Which intrinsic type a match scrutinee is required to have. The legal selectors for `expect_intrinsic_head`/`elaborate_intrinsic_head` — exactly the type-former `Intrinsic`s those helpers accept, as a closed set so an out-of-range selector is unrepresentable rather than an `unreachable!` panic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntrinsicHead {
    Nat,
    Bool,
    Bin(Grain),
}

/// Visit both operands of a binary intrinsic and rebuild it through `build`. The constructor is taken generically (not as a `fn` pointer) so every call site monomorphises to the same direct construction — this is the de Bruijn traversal hot path, so the indirection must vanish.
fn traverse_binary<F>(
    left: &Term,
    right: &Term,
    visit: &mut Visit<F>,
    build: impl FnOnce(Term, Term) -> Intrinsic,
) -> Intrinsic
where
    F: FnMut(usize, &Var) -> Option<Subterm>,
{
    build(visit.visit_subterm(left), visit.visit_subterm(right))
}
