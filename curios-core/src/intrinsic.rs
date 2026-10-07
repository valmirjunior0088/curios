mod algebra;
pub use algebra::*;

mod signature;
pub use signature::*;

use {
    super::{Bound, Free, Level, Nat, Subterm, Term, Var, Visit},
    crate::Global,
    curios_abi::{ForeignFunction, ResultShape, WireResults, WireType},
    curios_num::{Binary, Floating, Grain, Integer, Rounding},
    std::collections::BTreeSet,
};

/// The core type a host-boundary [`WireType`] denotes — the one reading of the signature shared by elaboration (operand checks, result records) and erasure, so the two cannot disagree about what crosses the wire.
pub fn wire_term(wire_type: &WireType) -> Term {
    let intrinsic = match wire_type {
        WireType::Nat => Intrinsic::NatType,
        WireType::Int => Intrinsic::IntType,
        WireType::Bool => Intrinsic::BoolType,
        WireType::Byte => Intrinsic::ByteType,
        WireType::Flt => Intrinsic::FltType,
        WireType::Bytes => Intrinsic::BinType(Grain::X),
        WireType::Bits => Intrinsic::BinType(Grain::B),
        WireType::Handle => Intrinsic::HandleType,
        WireType::List(element) => Intrinsic::ListType(wire_term(&(*element).into())),
    };

    Subterm::Intrinsic(intrinsic).into()
}

/// The core type a row's results denote, as [`wire_term`] reads one wire type: the unit type for no result, the bare type for one, and a record of the named fields otherwise, each label bound by a binder `fresh` mints. The one reading of a row's guest-facing shape — the elaborator and the kernel both build the type of a host call through it, so the two cannot come to different answers about what a call yields, and the arity rule itself is `WireResults`'s to state.
pub fn wire_results_term(results: &WireResults, mut fresh: impl FnMut(&str) -> Free) -> Term {
    match results.shape() {
        ResultShape::Unit => Term::tuple_type_unit(),
        ResultShape::Single(wire_type) => wire_term(&wire_type),
        ResultShape::Record(fields) => Term::tuple_type(
            fields
                .into_iter()
                .map(|(label, wire_type)| (fresh(label), wire_term(&wire_type)))
                .collect::<Vec<_>>(),
        ),
    }
}

/// What a foreign call demands of each operand, in order: its parameter's wire type, after — for a row that diverges — the type its description yields, which erasure drops. Erasure walks these, and [`foreign_signature`] states them for the checkers.
pub fn foreign_operands(function: &ForeignFunction) -> Vec<Operand> {
    let result = function.diverges().then_some(Operand::IsType);

    result
        .into_iter()
        .chain(
            function
                .signature()
                .params
                .iter()
                .map(|wire_type| Operand::At(wire_term(wire_type))),
        )
        .collect()
}

/// The type a foreign call produces over `operands`: the `Io` of the row's result shape, or for a row that diverges the `Io` of the type its first operand names.
///
/// A diverging call yields whatever the region that holds it wanted, which is sound because `Io` has no eliminator: an inhabitant of `Io(False)` is a description that proves nothing. `documentation/design/theory/effects-are-descriptions-and-the-carrier-has-no-eliminator.md` is the decision.
pub fn foreign_produced(
    function: &ForeignFunction,
    operands: &[Term],
    fresh: impl FnMut(&str) -> Free,
) -> Term {
    let result = match function.diverges() {
        true => operands
            .first()
            .expect("a diverging call's first operand is the type it yields")
            .clone(),
        false => wire_results_term(&function.signature().results, fresh),
    };

    Term::intrinsic(Intrinsic::io_type(result))
}

/// A foreign call's operand demands and result, in the vocabulary [`Intrinsic::signature`] states an intrinsic's in — so the kernel and the elaborator walk a host call with the operand handling they already share with the intrinsics, rather than a rule of each checker's own. The result is [`foreign_produced`]'s over `operands`, record labels bound by binders `fresh` mints.
///
/// **The row is the whole statement.** For a builtin it is the roster's, reached through the identity the term carries, and for a declared row its own; nothing the term says beside the row reaches the type.
pub fn foreign_signature(
    function: &ForeignFunction,
    operands: &[Term],
    fresh: impl FnMut(&str) -> Free,
) -> Signature {
    Signature {
        operands: foreign_operands(function),
        produced: Produced::Fixed(foreign_produced(function, operands, fresh)),
    }
}

/// The closed set of intrinsics of the core calculus: the built-in types (`BoolType`, `NatType`, `IntType`, `FltType`, `BinType`, `ListType`, `HandleType`, `CellType`, `ChannelType`, `IoType`), their literals, and the operator families over them. A host call is *not* here, exiting included: [`Subterm::Foreign`] is a term former of its own, because what it means is read off an ABI row rather than fixed by this enum. Operand positions hold full terms, so an intrinsic participates like any other subterm: elaboration checks operands against each variant's fixed signature, reduction constant-folds closed operands and rebuilds a canonical neutral otherwise, and erasure lowers each variant to its first-order IR op.
///
/// An intrinsic that performs an effect returns an `Io`, including guest cell and channel operations. Its result is stated by `Intrinsic::signature` and checked by both drivers; a new effectful variant must be given an `IoType` result when it is added.
///
/// **Generic over what an operand is**, which is [`Term`] in this crate and every stage below it. `curios-text` holds the same enum over the surface term, for the `/sys` roster to plant in a declaration's body, so the surface tree restates no variant and lowering one is [`try_map`](Self::try_map) over its operands. Everything that gives an intrinsic meaning — its signature, its reduction, its algebra — is stated at `Term` alone.
///
/// The `impl` block's constructor helpers (`nat_add`, `bin_slice`, …) take `impl Into<Term>` operands, sparing reduction's neutral rebuilds the `.into()` noise.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum Intrinsic<T = Term> {
    BoolType,
    Bool(bool),
    BoolAnd(T, T),
    BoolOr(T, T),
    BoolXor(T, T),
    BoolEql(T, T),
    BoolNeq(T, T),
    NatType,
    Nat(Nat<T>),
    NatEql(T, T),
    NatNeq(T, T),
    NatAdd(T, T),
    NatSub(T, T),
    NatMul(T, T),
    NatLt(T, T),
    /// `non_zero` proves `0 < divisor`. Carried as a field rather than left to `/sys`'s declaration so the obligation survives into Core, where the kernel re-checks it; [`Intrinsic::signature`] states the proposition it must inhabit.
    NatDiv {
        dividend: T,
        divisor: T,
        non_zero: T,
    },
    NatRem {
        dividend: T,
        divisor: T,
        non_zero: T,
    },
    NatLe(T, T),
    NatAnd(T, T),
    NatOr(T, T),
    NatXor(T, T),
    /// The lesser of two naturals.
    NatMin(T, T),
    /// The greater of two naturals.
    NatMax(T, T),
    NatShl(T, T),
    NatShr(T, T),
    ByteType,
    Byte(u8),
    ByteToNat(T),
    /// `below` proves `nat < 256`, the domain the narrowing to `Byte` has. Carried for the reason [`Intrinsic::NatDiv`]'s bound is: a bound stated only on `/sys`'s wrapper stops constraining anything the moment that wrapper unfolds, leaving the kernel a bare narrowing to admit.
    ///
    /// Masking instead would make the operation total by changing a value, which `documentation/design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md` forbids. Stating the domain is also what makes the constructor invertible, so `Byte/to_nat` can see back through it and a bound established in `Nat` survives the round trip.
    NatToByte {
        nat: T,
        below: T,
    },
    IntType,
    Int(Integer),
    IntEql(T, T),
    IntNeq(T, T),
    IntAdd(T, T),
    IntSub(T, T),
    IntMul(T, T),
    /// The `Int` twin of [`Intrinsic::NatDiv`]'s bound, stated as `divisor != 0` — a negative divisor is fine, so `0 <` will not serve.
    IntDiv {
        dividend: T,
        divisor: T,
        non_zero: T,
    },
    IntRem {
        dividend: T,
        divisor: T,
        non_zero: T,
    },
    IntLt(T, T),
    IntLe(T, T),
    IntAnd(T, T),
    IntOr(T, T),
    IntXor(T, T),
    /// The lesser of two integers.
    IntMin(T, T),
    /// The greater of two integers.
    IntMax(T, T),
    IntShl(T, T),
    IntShr(T, T),
    FltType,
    Flt(Floating),
    /// The rounded operations carry their IEEE rounding direction as static data, as a packed operation carries its [`Grain`]: the direction is fixed where the operation is written, so every folder and the emitter read the same one.
    FltAdd(Rounding, T, T),
    FltSub(Rounding, T, T),
    FltMul(Rounding, T, T),
    FltDiv(Rounding, T, T),
    /// `a · b + c`, rounded once.
    FltFma(Rounding, T, T, T),
    FltRem(T, T),
    FltEql(T, T),
    FltNeq(T, T),
    FltLt(T, T),
    FltLe(T, T),
    FltMin(T, T),
    FltMax(T, T),
    FltNeg(T),
    FltAbs(T),
    FltSqrt(Rounding, T),
    /// The integral value the operand rounds to in the direction named: `floor` is toward negative, `ceil` toward positive, `trunc` toward zero, `nearest` ties to even and `round` ties away.
    FltRoundIntegral(Rounding, T),
    FltCopysign(T, T),
    NatToInt(T),
    NatToFlt(Rounding, T),
    /// `non_neg` proves `0 <= int`, the domain the narrowing to `Nat` has. Carried for the reason [`Intrinsic::NatDiv`]'s bound is: a bound stated only on `/sys`'s wrapper stops constraining anything the moment that wrapper unfolds, leaving the kernel a bare narrowing to admit.
    IntToNat {
        int: T,
        non_neg: T,
    },
    IntToFlt(Rounding, T),
    /// `non_neg` proves the operand is a non-negative *number*, which is what excludes `+inf` and every NaN alongside every negative. Carried for the reason [`Intrinsic::NatDiv`]'s bound is.
    FltToNat {
        flt: T,
        non_neg: T,
    },
    FltToLeBytes(T),
    /// `eight_bytes` proves `len(bin) = 8`, which is the whole of what reinterpreting a binary64 needs.
    FltOfLeBytes {
        bin: T,
        eight_bytes: T,
    },
    /// `finite` proves the operand is a number — neither infinity, and not a NaN — which is the whole of what truncating toward zero needs.
    FltToInt {
        flt: T,
        finite: T,
    },
    /// The signed mantissa of the operand's exact value `mantissa · 2^exponent`, under the same `finite` proof truncation needs. It and [`Intrinsic::FltExponent`] are two operations shaped like [`Intrinsic::FltToInt`] rather than one whose result is a pair, so no stage carries a compound intrinsic result.
    FltMantissa {
        flt: T,
        finite: T,
    },
    /// The exponent of the operand's exact value, the other half of [`Intrinsic::FltMantissa`].
    FltExponent {
        flt: T,
        finite: T,
    },
    BinType(Grain),
    Bin(Grain, Binary),
    BinLen(Grain, T),
    BinEql(Grain, T, T),
    /// `in_range` proves `index < len(bin)`, carried for the reason [`Intrinsic::NatDiv`]'s bound is.
    BinGet {
        grain: Grain,
        bin: T,
        index: T,
        in_range: T,
    },
    /// `within` proves `start + length <= len(bin)` — the one bound a window has, since a count cannot spell a reversed range.
    BinSlice {
        grain: Grain,
        bin: T,
        start: T,
        length: T,
        within: T,
    },
    BinAppend {
        grain: Grain,
        bin: T,
        element: T,
    },
    BinConcat {
        grain: Grain,
        operands: Vec<T>,
    },
    /// `count` copies of one generator — the only way to build a run of a length a program computes without one node per element.
    BinReplicate {
        grain: Grain,
        count: T,
        atom: T,
    },
    /// `same_length` proves `len(left) == len(right)`, which is the one bound a pointwise operation has.
    ///
    /// Stated rather than extended, because no extension is forced: truncating to the shorter and padding to the longer are both total, and they disagree — a padded bit is absorbing for `and` where it is transparent for `or`, so the structure picks neither and a choice would be the author's. That is the test `documentation/design/arithmetic/a-partial-primitive-is-totalized-by-a-canonical-extension-or-it-states-its-domain.md` sets, and the same one that put `NatToByte`'s bound in its type rather than masking to eight bits.
    BinAnd {
        grain: Grain,
        left: T,
        right: T,
        same_length: T,
    },
    /// See [`Intrinsic::BinAnd`] for why the length is a stated bound.
    BinOr {
        grain: Grain,
        left: T,
        right: T,
        same_length: T,
    },
    /// See [`Intrinsic::BinAnd`] for why the length is a stated bound.
    BinXor {
        grain: Grain,
        left: T,
        right: T,
        same_length: T,
    },
    /// One packed run read at the other grain — eight bits to the byte, least significant first, which is the order the payload holds them in either way. `grain` is the operand's, as it is in every other `Bin` row, so the result is the one it is not.
    ///
    /// **`aligned` is stated at both grains and trivially true at one**, which is [`Intrinsic::NatDiv`]'s arrangement rather than a vestige: a bound uniform in its position and decided per grain keeps one row where two would otherwise be needed, and the byte grain pays nothing for it. At `B` it proves `len(bin) % 8 == 0` — the run holds a whole number of bytes; at `X` it proves `true`, because *n* bytes are always exactly *8n* bits and no count of them can leave a remainder.
    ///
    /// The bit-to-byte direction states its domain rather than extending it, and for a sharper reason than [`Intrinsic::BinAnd`]'s: a run ending mid-byte has no byte to answer with at all, so neither truncating nor padding is a *reading* of the operand — each answers for some other value. That is `get`'s case, where no extension exists, rather than `Nat/sub`'s, where one is forced.
    ///
    /// The payload is not rebuilt, but this is not free: a rope carries its length in the grain's own units at every node, so the conversion is one force and one leaf at the scaled length.
    BinReinterp {
        grain: Grain,
        bin: T,
        aligned: T,
    },
    ListType(T),
    // A list literal, carrying its element type: the one value form whose elements alone cannot name it — `[]` has nothing to read a type from.
    List {
        element: T,
        items: Vec<T>,
    },
    ListLen {
        element: T,
        list: T,
    },
    ListGet {
        element: T,
        list: T,
        index: T,
        in_range: T,
    },
    ListSlice {
        element: T,
        list: T,
        start: T,
        length: T,
        within: T,
    },
    ListAppend {
        element: T,
        list: T,
        item: T,
    },
    ListConcat {
        element: T,
        operands: Vec<T>,
    },
    // (@A, @B, list : List(A), f : (A) -> B) -> List(B): a structural map. Opaque under reduction on a symbolic operand, so it never unfolds a variable during type-checking. Erases to a single O(n) fill loop.
    ListMap {
        from: T,
        to: T,
        list: T,
        function: T,
    },
    // (@T, @A, list : List(T), init : A, f : (T, A) -> A) -> A: a structural left fold, `f(x_n, … f(x_1, init))`. Reduces over the free-monoid shape — a literal run applies `f` element by element, a concatenation folds its operands in order threading the accumulator, an append folds its base and then applies `f` to the appended element — and stays neutral on a symbolic operand, so `fold([h, ..t], z, f) = fold(t, f(h, z), f)` is definitional. Erases to a bounded `Nat` fold over the list's length with one read per step.
    ListFold {
        element: T,
        result: T,
        list: T,
        init: T,
        function: T,
    },
    HandleType,
    Handle(u32),
    CellType(T),
    Cell {
        element: T,
    },
    CellFill {
        element: T,
        cell: T,
        value: T,
    },
    CellPoll {
        element: T,
        cell: T,
        universes: Vec<Level>,
    },
    ChannelType(T),
    Channel {
        element: T,
        capacity: T,
        positive: T,
    },
    ChannelPush {
        element: T,
        channel: T,
        value: T,
    },
    ChannelTake {
        element: T,
        channel: T,
        universes: Vec<Level>,
    },
    ChannelClose {
        element: T,
        channel: T,
    },
    ChannelClosed {
        element: T,
        channel: T,
    },
    ChannelCount {
        element: T,
        channel: T,
    },
    ChannelCapacity {
        element: T,
        channel: T,
    },
    // The opaque carrier of a host effect: `Io(T)` is a *description* of a computation that yields a `T`, never the `T` itself.
    //
    // There is deliberately no eliminator from `Io(T)` to `T`, and there never may be. That absence is the whole of the referential-transparency story: a closure that performs an effect can only be given an `Io`-returning type, so every term of non-`Io` type denotes one value, and a scrutinee's spelling fixes it. The only consumer of an `Io(A)` is `IoBind`, which returns an `Io(B)`; nothing lowers the carrier to its content but the emitted entrypoint boundary, which forces the program's whole description exactly once.
    IoType(T),
    // (@T, x : T) -> Io(T): the description that performs nothing and yields `x`.
    IoPure {
        result: T,
        value: T,
    },
    // (@A, @B, m : Io(A), f : (A) -> Io(B)) -> Io(B): the description that performs `m`, then the description `f` computes from its result.
    //
    // Non-dependent in `B`, matching the `/std/Monad` field it satisfies. Inert like the other two: no monad law holds definitionally, since nothing can be proven about an `Io` for a law to be useful about.
    IoBind {
        from: T,
        to: T,
        action: T,
        continuation: T,
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

    /// A `BinReinterp` node from a term-shaped run and the proof it regroups exactly.
    pub fn bin_reinterp<B, P>(grain: Grain, bin: B, aligned: P) -> Self
    where
        B: Into<Term>,
        P: Into<Term>,
    {
        Self::BinReinterp {
            grain,
            bin: bin.into(),
            aligned: aligned.into(),
        }
    }

    /// A `BinReplicate` node from a term-shaped count and generator.
    pub fn bin_replicate<C, A>(grain: Grain, count: C, atom: A) -> Self
    where
        C: Into<Term>,
        A: Into<Term>,
    {
        Self::BinReplicate {
            grain,
            count: count.into(),
            atom: atom.into(),
        }
    }

    /// A `BinAnd` node from two term-shaped runs and the proof their lengths agree.
    pub fn bin_and<L, R, P>(grain: Grain, left: L, right: R, same_length: P) -> Self
    where
        L: Into<Term>,
        R: Into<Term>,
        P: Into<Term>,
    {
        Self::BinAnd {
            grain,
            left: left.into(),
            right: right.into(),
            same_length: same_length.into(),
        }
    }

    /// A `BinOr` node, as [`Intrinsic::bin_and`].
    pub fn bin_or<L, R, P>(grain: Grain, left: L, right: R, same_length: P) -> Self
    where
        L: Into<Term>,
        R: Into<Term>,
        P: Into<Term>,
    {
        Self::BinOr {
            grain,
            left: left.into(),
            right: right.into(),
            same_length: same_length.into(),
        }
    }

    /// A `BinXor` node, as [`Intrinsic::bin_and`].
    pub fn bin_xor<L, R, P>(grain: Grain, left: L, right: R, same_length: P) -> Self
    where
        L: Into<Term>,
        R: Into<Term>,
        P: Into<Term>,
    {
        Self::BinXor {
            grain,
            left: left.into(),
            right: right.into(),
            same_length: same_length.into(),
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

    /// A `ListFold` node from term-shaped element type, result type, list, initial accumulator, and stepper — the collection first, like every other sequence operation.
    pub fn list_fold<T, A, L, I, F>(element: T, result: A, list: L, init: I, function: F) -> Self
    where
        T: Into<Term>,
        A: Into<Term>,
        L: Into<Term>,
        I: Into<Term>,
        F: Into<Term>,
    {
        Self::ListFold {
            element: element.into(),
            result: result.into(),
            list: list.into(),
            init: init.into(),
            function: function.into(),
        }
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
            | Intrinsic::NatToFlt(_, t)
            | Intrinsic::IntToFlt(_, t)
            | Intrinsic::ByteToNat(t)
            | Intrinsic::FltNeg(t)
            | Intrinsic::FltAbs(t)
            | Intrinsic::FltSqrt(_, t)
            | Intrinsic::FltRoundIntegral(_, t)
            | Intrinsic::BinLen(Grain::X, t)
            | Intrinsic::BinLen(Grain::B, t)
            | Intrinsic::ListType(t)
            | Intrinsic::IoType(t) => visit(t),

            Intrinsic::NatEql(a, b)
            | Intrinsic::NatNeq(a, b)
            | Intrinsic::NatAdd(a, b)
            | Intrinsic::NatSub(a, b)
            | Intrinsic::NatMul(a, b)
            | Intrinsic::NatLt(a, b)
            | Intrinsic::NatLe(a, b)
            | Intrinsic::NatAnd(a, b)
            | Intrinsic::NatOr(a, b)
            | Intrinsic::NatXor(a, b)
            | Intrinsic::NatMin(a, b)
            | Intrinsic::NatMax(a, b)
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
            | Intrinsic::IntMin(a, b)
            | Intrinsic::IntMax(a, b)
            | Intrinsic::IntShl(a, b)
            | Intrinsic::IntShr(a, b)
            | Intrinsic::FltAdd(_, a, b)
            | Intrinsic::FltSub(_, a, b)
            | Intrinsic::FltMul(_, a, b)
            | Intrinsic::FltDiv(_, a, b)
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
            | Intrinsic::BinReplicate {
                grain: _,
                count: a,
                atom: b,
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
            }
            | Intrinsic::FltFma(_, a, b, c) => {
                visit(a);
                visit(b);
                visit(c);
            }

            Intrinsic::ListFold {
                element: a,
                result: b,
                list: c,
                init: d,
                function: e,
            } => {
                visit(a);
                visit(b);
                visit(c);
                visit(d);
                visit(e);
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
                eight_bytes: p,
            }
            | Intrinsic::IntToNat { int: a, non_neg: p }
            | Intrinsic::NatToByte { nat: a, below: p }
            | Intrinsic::FltToNat { flt: a, non_neg: p }
            | Intrinsic::FltToInt { flt: a, finite: p }
            | Intrinsic::FltMantissa { flt: a, finite: p }
            | Intrinsic::FltExponent { flt: a, finite: p }
            | Intrinsic::BinReinterp {
                grain: _,
                bin: a,
                aligned: p,
            } => {
                visit(a);
                visit(p);
            }

            // The four sequence accessors, beside the division family for the same reason. Their arity differs by carrier — a `List` operation leads with its element type — so they group by operand count rather than by which one is the proof, and the proof is last in every one of them, matching `signature`'s order.
            Intrinsic::BinGet {
                grain: _,
                bin: a,
                index: b,
                in_range: p,
            }
            | Intrinsic::BinAnd {
                grain: _,
                left: a,
                right: b,
                same_length: p,
            }
            | Intrinsic::BinOr {
                grain: _,
                left: a,
                right: b,
                same_length: p,
            }
            | Intrinsic::BinXor {
                grain: _,
                left: a,
                right: b,
                same_length: p,
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

            Intrinsic::CellType(a) | Intrinsic::ChannelType(a) | Intrinsic::Cell { element: a } => {
                visit(a)
            }
            Intrinsic::CellPoll {
                element: a,
                cell: b,
                ..
            }
            | Intrinsic::ChannelTake {
                element: a,
                channel: b,
                ..
            }
            | Intrinsic::ChannelClose {
                element: a,
                channel: b,
            }
            | Intrinsic::ChannelClosed {
                element: a,
                channel: b,
            }
            | Intrinsic::ChannelCount {
                element: a,
                channel: b,
            }
            | Intrinsic::ChannelCapacity {
                element: a,
                channel: b,
            } => {
                visit(a);
                visit(b);
            }
            Intrinsic::CellFill {
                element: a,
                cell: b,
                value: c,
            }
            | Intrinsic::ChannelPush {
                element: a,
                channel: b,
                value: c,
            }
            | Intrinsic::Channel {
                element: a,
                capacity: b,
                positive: c,
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

    /// The universe instance carried by an operation whose result names an ordinary polymorphic family.
    pub fn result_universes(&self) -> &[Level] {
        match self {
            Self::CellPoll { universes, .. } | Self::ChannelTake { universes, .. } => universes,
            _ => &[],
        }
    }

    /// The instance elaboration fills from the checked result type; later traversals preserve and substitute it like a nominal occurrence's own vector.
    pub fn result_universes_mut(&mut self) -> Option<&mut Vec<Level>> {
        match self {
            Self::CellPoll { universes, .. } | Self::ChannelTake { universes, .. } => {
                Some(universes)
            }
            _ => None,
        }
    }

    /// Whether any operand `Term` satisfies `pred` — the `Intrinsic` leg of `Subterm::any_child_term`, layered on the private operand walker.
    pub fn any_term<F: FnMut(&Term) -> bool>(&self, pred: &mut F) -> bool {
        let mut found = false;
        self.for_each_operand(&mut |term| found = found || pred(term));
        found
    }

    // Recurse into every operand `Term` so a construction nested inside an intrinsic (e.g. `List(Str)`'s element type) still contributes its head name. Intrinsics own no head names of their own.
    pub(crate) fn collect_construction_names(&self, names: &mut BTreeSet<Global>) {
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
            Intrinsic::NatMin(l, r) => traverse_binary(l, r, visit, Intrinsic::NatMin),
            Intrinsic::NatMax(l, r) => traverse_binary(l, r, visit, Intrinsic::NatMax),
            Intrinsic::NatShl(l, r) => traverse_binary(l, r, visit, Intrinsic::NatShl),
            Intrinsic::NatShr(l, r) => traverse_binary(l, r, visit, Intrinsic::NatShr),
            Intrinsic::ByteType => Intrinsic::ByteType,
            Intrinsic::Byte(value) => Intrinsic::Byte(*value),
            Intrinsic::ByteToNat(inner) => Intrinsic::ByteToNat(visit.visit_subterm(inner)),
            Intrinsic::NatToByte { nat, below } => Intrinsic::NatToByte {
                nat: visit.visit_subterm(nat),
                below: visit.visit_subterm(below),
            },
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
            Intrinsic::IntMin(l, r) => traverse_binary(l, r, visit, Intrinsic::IntMin),
            Intrinsic::IntMax(l, r) => traverse_binary(l, r, visit, Intrinsic::IntMax),
            Intrinsic::IntShl(l, r) => traverse_binary(l, r, visit, Intrinsic::IntShl),
            Intrinsic::IntShr(l, r) => traverse_binary(l, r, visit, Intrinsic::IntShr),
            Intrinsic::FltType => Intrinsic::FltType,
            Intrinsic::Flt(flt) => Intrinsic::Flt(*flt),
            Intrinsic::FltAdd(rounding, l, r) => {
                traverse_binary(l, r, visit, |l, r| Intrinsic::FltAdd(*rounding, l, r))
            }
            Intrinsic::FltSub(rounding, l, r) => {
                traverse_binary(l, r, visit, |l, r| Intrinsic::FltSub(*rounding, l, r))
            }
            Intrinsic::FltMul(rounding, l, r) => {
                traverse_binary(l, r, visit, |l, r| Intrinsic::FltMul(*rounding, l, r))
            }
            Intrinsic::FltDiv(rounding, l, r) => {
                traverse_binary(l, r, visit, |l, r| Intrinsic::FltDiv(*rounding, l, r))
            }
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
            Intrinsic::FltSqrt(rounding, inner) => {
                Intrinsic::FltSqrt(*rounding, visit.visit_subterm(inner))
            }
            Intrinsic::FltRoundIntegral(rounding, inner) => {
                Intrinsic::FltRoundIntegral(*rounding, visit.visit_subterm(inner))
            }
            Intrinsic::FltFma(rounding, a, b, c) => Intrinsic::FltFma(
                *rounding,
                visit.visit_subterm(a),
                visit.visit_subterm(b),
                visit.visit_subterm(c),
            ),
            Intrinsic::FltToLeBytes(inner) => Intrinsic::FltToLeBytes(visit.visit_subterm(inner)),
            Intrinsic::FltOfLeBytes { bin, eight_bytes } => Intrinsic::FltOfLeBytes {
                bin: visit.visit_subterm(bin),
                eight_bytes: visit.visit_subterm(eight_bytes),
            },
            Intrinsic::NatToInt(inner) => Intrinsic::NatToInt(visit.visit_subterm(inner)),
            Intrinsic::NatToFlt(rounding, inner) => {
                Intrinsic::NatToFlt(*rounding, visit.visit_subterm(inner))
            }
            Intrinsic::IntToNat { int, non_neg } => Intrinsic::IntToNat {
                int: visit.visit_subterm(int),
                non_neg: visit.visit_subterm(non_neg),
            },
            Intrinsic::IntToFlt(rounding, inner) => {
                Intrinsic::IntToFlt(*rounding, visit.visit_subterm(inner))
            }
            Intrinsic::FltToNat { flt, non_neg } => Intrinsic::FltToNat {
                flt: visit.visit_subterm(flt),
                non_neg: visit.visit_subterm(non_neg),
            },
            Intrinsic::FltToInt { flt, finite } => Intrinsic::FltToInt {
                flt: visit.visit_subterm(flt),
                finite: visit.visit_subterm(finite),
            },
            Intrinsic::FltMantissa { flt, finite } => Intrinsic::FltMantissa {
                flt: visit.visit_subterm(flt),
                finite: visit.visit_subterm(finite),
            },
            Intrinsic::FltExponent { flt, finite } => Intrinsic::FltExponent {
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
            Intrinsic::BinReplicate { grain, count, atom } => {
                traverse_binary(count, atom, visit, |count, atom| Intrinsic::BinReplicate {
                    grain: *grain,
                    count,
                    atom,
                })
            }
            Intrinsic::BinReinterp {
                grain,
                bin,
                aligned,
            } => traverse_binary(bin, aligned, visit, |bin, aligned| Intrinsic::BinReinterp {
                grain: *grain,
                bin,
                aligned,
            }),
            Intrinsic::BinAnd {
                grain,
                left,
                right,
                same_length,
            } => Intrinsic::BinAnd {
                grain: *grain,
                left: visit.visit_subterm(left),
                right: visit.visit_subterm(right),
                same_length: visit.visit_subterm(same_length),
            },
            Intrinsic::BinOr {
                grain,
                left,
                right,
                same_length,
            } => Intrinsic::BinOr {
                grain: *grain,
                left: visit.visit_subterm(left),
                right: visit.visit_subterm(right),
                same_length: visit.visit_subterm(same_length),
            },
            Intrinsic::BinXor {
                grain,
                left,
                right,
                same_length,
            } => Intrinsic::BinXor {
                grain: *grain,
                left: visit.visit_subterm(left),
                right: visit.visit_subterm(right),
                same_length: visit.visit_subterm(same_length),
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
            Intrinsic::ListFold {
                element,
                result,
                list,
                init,
                function,
            } => Intrinsic::ListFold {
                element: visit.visit_subterm(element),
                result: visit.visit_subterm(result),
                list: visit.visit_subterm(list),
                init: visit.visit_subterm(init),
                function: visit.visit_subterm(function),
            },
            Intrinsic::HandleType => Intrinsic::HandleType,
            Intrinsic::Handle(token) => Intrinsic::Handle(*token),
            Intrinsic::CellType(a) => Intrinsic::CellType(visit.visit_subterm(a)),
            Intrinsic::ChannelType(a) => Intrinsic::ChannelType(visit.visit_subterm(a)),
            Intrinsic::Cell { element } => Intrinsic::Cell {
                element: visit.visit_subterm(element),
            },
            Intrinsic::CellFill {
                element,
                cell,
                value,
            } => Intrinsic::CellFill {
                element: visit.visit_subterm(element),
                cell: visit.visit_subterm(cell),
                value: visit.visit_subterm(value),
            },
            Intrinsic::CellPoll {
                element,
                cell,
                universes,
            } => Intrinsic::CellPoll {
                element: visit.visit_subterm(element),
                cell: visit.visit_subterm(cell),
                universes: if visit.erases_universes() {
                    Vec::new()
                } else {
                    universes
                        .iter()
                        .map(|level| visit.visit_level(level))
                        .collect()
                },
            },
            Intrinsic::Channel {
                element,
                capacity,
                positive,
            } => Intrinsic::Channel {
                element: visit.visit_subterm(element),
                capacity: visit.visit_subterm(capacity),
                positive: visit.visit_subterm(positive),
            },
            Intrinsic::ChannelPush {
                element,
                channel,
                value,
            } => Intrinsic::ChannelPush {
                element: visit.visit_subterm(element),
                channel: visit.visit_subterm(channel),
                value: visit.visit_subterm(value),
            },
            Intrinsic::ChannelTake {
                element,
                channel,
                universes,
            } => Intrinsic::ChannelTake {
                element: visit.visit_subterm(element),
                channel: visit.visit_subterm(channel),
                universes: if visit.erases_universes() {
                    Vec::new()
                } else {
                    universes
                        .iter()
                        .map(|level| visit.visit_level(level))
                        .collect()
                },
            },
            Intrinsic::ChannelClose { element, channel } => Intrinsic::ChannelClose {
                element: visit.visit_subterm(element),
                channel: visit.visit_subterm(channel),
            },
            Intrinsic::ChannelClosed { element, channel } => Intrinsic::ChannelClosed {
                element: visit.visit_subterm(element),
                channel: visit.visit_subterm(channel),
            },
            Intrinsic::ChannelCount { element, channel } => Intrinsic::ChannelCount {
                element: visit.visit_subterm(element),
                channel: visit.visit_subterm(channel),
            },
            Intrinsic::ChannelCapacity { element, channel } => Intrinsic::ChannelCapacity {
                element: visit.visit_subterm(element),
                channel: visit.visit_subterm(channel),
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

impl<T> Intrinsic<T> {
    /// This operation over another operand type: every operand through `operand`, in field order, and everything that is not an operand as it stands.
    pub fn try_map<U, E>(
        &self,
        operand: &mut impl FnMut(&T) -> Result<U, E>,
    ) -> Result<Intrinsic<U>, E> {
        Ok(match self {
            Intrinsic::BoolType => Intrinsic::BoolType,
            Intrinsic::Bool(value) => Intrinsic::Bool(*value),
            Intrinsic::NatType => Intrinsic::NatType,
            Intrinsic::Nat(Nat::Zero) => Intrinsic::Nat(Nat::Zero),
            Intrinsic::Nat(Nat::Succ(spine, inner)) => {
                Intrinsic::Nat(Nat::Succ(spine.clone(), operand(inner)?))
            }
            Intrinsic::NatEql(l, r) => try_binary(l, r, operand, Intrinsic::NatEql)?,
            Intrinsic::NatNeq(l, r) => try_binary(l, r, operand, Intrinsic::NatNeq)?,
            Intrinsic::NatAdd(l, r) => try_binary(l, r, operand, Intrinsic::NatAdd)?,
            Intrinsic::NatSub(l, r) => try_binary(l, r, operand, Intrinsic::NatSub)?,
            Intrinsic::NatMul(l, r) => try_binary(l, r, operand, Intrinsic::NatMul)?,
            Intrinsic::NatLt(l, r) => try_binary(l, r, operand, Intrinsic::NatLt)?,
            Intrinsic::NatDiv {
                dividend: a,
                divisor: b,
                non_zero: p,
            } => Intrinsic::NatDiv {
                dividend: operand(a)?,
                divisor: operand(b)?,
                non_zero: operand(p)?,
            },
            Intrinsic::NatRem {
                dividend: a,
                divisor: b,
                non_zero: p,
            } => Intrinsic::NatRem {
                dividend: operand(a)?,
                divisor: operand(b)?,
                non_zero: operand(p)?,
            },
            Intrinsic::NatLe(l, r) => try_binary(l, r, operand, Intrinsic::NatLe)?,
            Intrinsic::NatAnd(l, r) => try_binary(l, r, operand, Intrinsic::NatAnd)?,
            Intrinsic::NatOr(l, r) => try_binary(l, r, operand, Intrinsic::NatOr)?,
            Intrinsic::NatXor(l, r) => try_binary(l, r, operand, Intrinsic::NatXor)?,
            Intrinsic::NatMin(l, r) => try_binary(l, r, operand, Intrinsic::NatMin)?,
            Intrinsic::NatMax(l, r) => try_binary(l, r, operand, Intrinsic::NatMax)?,
            Intrinsic::NatShl(l, r) => try_binary(l, r, operand, Intrinsic::NatShl)?,
            Intrinsic::NatShr(l, r) => try_binary(l, r, operand, Intrinsic::NatShr)?,
            Intrinsic::ByteType => Intrinsic::ByteType,
            Intrinsic::Byte(value) => Intrinsic::Byte(*value),
            Intrinsic::ByteToNat(inner) => Intrinsic::ByteToNat(operand(inner)?),
            Intrinsic::NatToByte { nat, below } => Intrinsic::NatToByte {
                nat: operand(nat)?,
                below: operand(below)?,
            },
            Intrinsic::BoolAnd(l, r) => try_binary(l, r, operand, Intrinsic::BoolAnd)?,
            Intrinsic::BoolOr(l, r) => try_binary(l, r, operand, Intrinsic::BoolOr)?,
            Intrinsic::BoolXor(l, r) => try_binary(l, r, operand, Intrinsic::BoolXor)?,
            Intrinsic::BoolEql(l, r) => try_binary(l, r, operand, Intrinsic::BoolEql)?,
            Intrinsic::BoolNeq(l, r) => try_binary(l, r, operand, Intrinsic::BoolNeq)?,
            Intrinsic::IntType => Intrinsic::IntType,
            Intrinsic::Int(value) => Intrinsic::Int(value.clone()),
            Intrinsic::IntEql(l, r) => try_binary(l, r, operand, Intrinsic::IntEql)?,
            Intrinsic::IntNeq(l, r) => try_binary(l, r, operand, Intrinsic::IntNeq)?,
            Intrinsic::IntAdd(l, r) => try_binary(l, r, operand, Intrinsic::IntAdd)?,
            Intrinsic::IntSub(l, r) => try_binary(l, r, operand, Intrinsic::IntSub)?,
            Intrinsic::IntMul(l, r) => try_binary(l, r, operand, Intrinsic::IntMul)?,
            Intrinsic::IntDiv {
                dividend: a,
                divisor: b,
                non_zero: p,
            } => Intrinsic::IntDiv {
                dividend: operand(a)?,
                divisor: operand(b)?,
                non_zero: operand(p)?,
            },
            Intrinsic::IntRem {
                dividend: a,
                divisor: b,
                non_zero: p,
            } => Intrinsic::IntRem {
                dividend: operand(a)?,
                divisor: operand(b)?,
                non_zero: operand(p)?,
            },
            Intrinsic::IntLt(l, r) => try_binary(l, r, operand, Intrinsic::IntLt)?,
            Intrinsic::IntLe(l, r) => try_binary(l, r, operand, Intrinsic::IntLe)?,
            Intrinsic::IntAnd(l, r) => try_binary(l, r, operand, Intrinsic::IntAnd)?,
            Intrinsic::IntOr(l, r) => try_binary(l, r, operand, Intrinsic::IntOr)?,
            Intrinsic::IntXor(l, r) => try_binary(l, r, operand, Intrinsic::IntXor)?,
            Intrinsic::IntMin(l, r) => try_binary(l, r, operand, Intrinsic::IntMin)?,
            Intrinsic::IntMax(l, r) => try_binary(l, r, operand, Intrinsic::IntMax)?,
            Intrinsic::IntShl(l, r) => try_binary(l, r, operand, Intrinsic::IntShl)?,
            Intrinsic::IntShr(l, r) => try_binary(l, r, operand, Intrinsic::IntShr)?,
            Intrinsic::FltType => Intrinsic::FltType,
            Intrinsic::Flt(flt) => Intrinsic::Flt(*flt),
            Intrinsic::FltAdd(rounding, l, r) => {
                try_binary(l, r, operand, |l, r| Intrinsic::FltAdd(*rounding, l, r))?
            }
            Intrinsic::FltSub(rounding, l, r) => {
                try_binary(l, r, operand, |l, r| Intrinsic::FltSub(*rounding, l, r))?
            }
            Intrinsic::FltMul(rounding, l, r) => {
                try_binary(l, r, operand, |l, r| Intrinsic::FltMul(*rounding, l, r))?
            }
            Intrinsic::FltDiv(rounding, l, r) => {
                try_binary(l, r, operand, |l, r| Intrinsic::FltDiv(*rounding, l, r))?
            }
            Intrinsic::FltRem(l, r) => try_binary(l, r, operand, Intrinsic::FltRem)?,
            Intrinsic::FltEql(l, r) => try_binary(l, r, operand, Intrinsic::FltEql)?,
            Intrinsic::FltNeq(l, r) => try_binary(l, r, operand, Intrinsic::FltNeq)?,
            Intrinsic::FltLt(l, r) => try_binary(l, r, operand, Intrinsic::FltLt)?,
            Intrinsic::FltLe(l, r) => try_binary(l, r, operand, Intrinsic::FltLe)?,
            Intrinsic::FltMin(l, r) => try_binary(l, r, operand, Intrinsic::FltMin)?,
            Intrinsic::FltMax(l, r) => try_binary(l, r, operand, Intrinsic::FltMax)?,
            Intrinsic::FltCopysign(l, r) => try_binary(l, r, operand, Intrinsic::FltCopysign)?,
            Intrinsic::FltNeg(inner) => Intrinsic::FltNeg(operand(inner)?),
            Intrinsic::FltAbs(inner) => Intrinsic::FltAbs(operand(inner)?),
            Intrinsic::FltSqrt(rounding, inner) => Intrinsic::FltSqrt(*rounding, operand(inner)?),
            Intrinsic::FltRoundIntegral(rounding, inner) => {
                Intrinsic::FltRoundIntegral(*rounding, operand(inner)?)
            }
            Intrinsic::FltFma(rounding, a, b, c) => {
                Intrinsic::FltFma(*rounding, operand(a)?, operand(b)?, operand(c)?)
            }
            Intrinsic::FltToLeBytes(inner) => Intrinsic::FltToLeBytes(operand(inner)?),
            Intrinsic::FltOfLeBytes { bin, eight_bytes } => Intrinsic::FltOfLeBytes {
                bin: operand(bin)?,
                eight_bytes: operand(eight_bytes)?,
            },
            Intrinsic::NatToInt(inner) => Intrinsic::NatToInt(operand(inner)?),
            Intrinsic::NatToFlt(rounding, inner) => Intrinsic::NatToFlt(*rounding, operand(inner)?),
            Intrinsic::IntToNat { int, non_neg } => Intrinsic::IntToNat {
                int: operand(int)?,
                non_neg: operand(non_neg)?,
            },
            Intrinsic::IntToFlt(rounding, inner) => Intrinsic::IntToFlt(*rounding, operand(inner)?),
            Intrinsic::FltToNat { flt, non_neg } => Intrinsic::FltToNat {
                flt: operand(flt)?,
                non_neg: operand(non_neg)?,
            },
            Intrinsic::FltToInt { flt, finite } => Intrinsic::FltToInt {
                flt: operand(flt)?,
                finite: operand(finite)?,
            },
            Intrinsic::FltMantissa { flt, finite } => Intrinsic::FltMantissa {
                flt: operand(flt)?,
                finite: operand(finite)?,
            },
            Intrinsic::FltExponent { flt, finite } => Intrinsic::FltExponent {
                flt: operand(flt)?,
                finite: operand(finite)?,
            },
            Intrinsic::BinType(grain) => Intrinsic::BinType(*grain),
            Intrinsic::Bin(grain, value) => Intrinsic::Bin(*grain, value.clone()),
            Intrinsic::BinLen(grain, bin) => Intrinsic::BinLen(*grain, operand(bin)?),
            Intrinsic::BinEql(grain, l, r) => {
                try_binary(l, r, operand, |l, r| Intrinsic::BinEql(*grain, l, r))?
            }
            Intrinsic::BinGet {
                grain,
                bin,
                index,
                in_range,
            } => Intrinsic::BinGet {
                grain: *grain,
                bin: operand(bin)?,
                index: operand(index)?,
                in_range: operand(in_range)?,
            },
            Intrinsic::BinSlice {
                grain,
                bin,
                start,
                length,
                within,
            } => Intrinsic::BinSlice {
                grain: *grain,
                bin: operand(bin)?,
                start: operand(start)?,
                length: operand(length)?,
                within: operand(within)?,
            },
            Intrinsic::BinAppend {
                grain,
                bin: b,
                element: atom,
            } => try_binary(b, atom, operand, |b, atom| Intrinsic::BinAppend {
                grain: *grain,
                bin: b,
                element: atom,
            })?,
            Intrinsic::BinConcat { grain, operands } => Intrinsic::BinConcat {
                grain: *grain,
                operands: operands
                    .iter()
                    .map(&mut *operand)
                    .collect::<Result<_, _>>()?,
            },
            Intrinsic::BinReplicate { grain, count, atom } => {
                try_binary(count, atom, operand, |count, atom| {
                    Intrinsic::BinReplicate {
                        grain: *grain,
                        count,
                        atom,
                    }
                })?
            }
            Intrinsic::BinReinterp {
                grain,
                bin,
                aligned,
            } => try_binary(bin, aligned, operand, |bin, aligned| {
                Intrinsic::BinReinterp {
                    grain: *grain,
                    bin,
                    aligned,
                }
            })?,
            Intrinsic::BinAnd {
                grain,
                left,
                right,
                same_length,
            } => Intrinsic::BinAnd {
                grain: *grain,
                left: operand(left)?,
                right: operand(right)?,
                same_length: operand(same_length)?,
            },
            Intrinsic::BinOr {
                grain,
                left,
                right,
                same_length,
            } => Intrinsic::BinOr {
                grain: *grain,
                left: operand(left)?,
                right: operand(right)?,
                same_length: operand(same_length)?,
            },
            Intrinsic::BinXor {
                grain,
                left,
                right,
                same_length,
            } => Intrinsic::BinXor {
                grain: *grain,
                left: operand(left)?,
                right: operand(right)?,
                same_length: operand(same_length)?,
            },
            Intrinsic::ListType(elem) => Intrinsic::ListType(operand(elem)?),
            Intrinsic::List {
                element: elem,
                items: elems,
            } => Intrinsic::List {
                element: operand(elem)?,
                items: elems.iter().map(&mut *operand).collect::<Result<_, _>>()?,
            },
            Intrinsic::ListLen { element: ty, list } => {
                try_binary(ty, list, operand, |element, list| Intrinsic::ListLen {
                    element,
                    list,
                })?
            }
            Intrinsic::ListGet {
                element: ty,
                list,
                index,
                in_range,
            } => Intrinsic::ListGet {
                element: operand(ty)?,
                list: operand(list)?,
                index: operand(index)?,
                in_range: operand(in_range)?,
            },
            Intrinsic::ListSlice {
                element: ty,
                list,
                start,
                length,
                within,
            } => Intrinsic::ListSlice {
                element: operand(ty)?,
                list: operand(list)?,
                start: operand(start)?,
                length: operand(length)?,
                within: operand(within)?,
            },
            Intrinsic::ListAppend {
                element: ty,
                list,
                item: elem,
            } => Intrinsic::ListAppend {
                element: operand(ty)?,
                list: operand(list)?,
                item: operand(elem)?,
            },
            Intrinsic::ListConcat {
                element: ty,
                operands,
            } => Intrinsic::ListConcat {
                element: operand(ty)?,
                operands: operands
                    .iter()
                    .map(&mut *operand)
                    .collect::<Result<_, _>>()?,
            },
            Intrinsic::ListMap {
                from: a,
                to: b,
                list,
                function: f,
            } => Intrinsic::ListMap {
                from: operand(a)?,
                to: operand(b)?,
                list: operand(list)?,
                function: operand(f)?,
            },
            Intrinsic::ListFold {
                element,
                result,
                list,
                init,
                function,
            } => Intrinsic::ListFold {
                element: operand(element)?,
                result: operand(result)?,
                list: operand(list)?,
                init: operand(init)?,
                function: operand(function)?,
            },
            Intrinsic::HandleType => Intrinsic::HandleType,
            Intrinsic::Handle(token) => Intrinsic::Handle(*token),
            Intrinsic::CellType(a) => Intrinsic::CellType(operand(a)?),
            Intrinsic::ChannelType(a) => Intrinsic::ChannelType(operand(a)?),
            Intrinsic::Cell { element } => Intrinsic::Cell {
                element: operand(element)?,
            },
            Intrinsic::CellFill {
                element,
                cell,
                value,
            } => Intrinsic::CellFill {
                element: operand(element)?,
                cell: operand(cell)?,
                value: operand(value)?,
            },
            Intrinsic::CellPoll {
                element,
                cell,
                universes,
            } => Intrinsic::CellPoll {
                element: operand(element)?,
                cell: operand(cell)?,
                universes: universes.clone(),
            },
            Intrinsic::Channel {
                element,
                capacity,
                positive,
            } => Intrinsic::Channel {
                element: operand(element)?,
                capacity: operand(capacity)?,
                positive: operand(positive)?,
            },
            Intrinsic::ChannelPush {
                element,
                channel,
                value,
            } => Intrinsic::ChannelPush {
                element: operand(element)?,
                channel: operand(channel)?,
                value: operand(value)?,
            },
            Intrinsic::ChannelTake {
                element,
                channel,
                universes,
            } => Intrinsic::ChannelTake {
                element: operand(element)?,
                channel: operand(channel)?,
                universes: universes.clone(),
            },
            Intrinsic::ChannelClose { element, channel } => Intrinsic::ChannelClose {
                element: operand(element)?,
                channel: operand(channel)?,
            },
            Intrinsic::ChannelClosed { element, channel } => Intrinsic::ChannelClosed {
                element: operand(element)?,
                channel: operand(channel)?,
            },
            Intrinsic::ChannelCount { element, channel } => Intrinsic::ChannelCount {
                element: operand(element)?,
                channel: operand(channel)?,
            },
            Intrinsic::ChannelCapacity { element, channel } => Intrinsic::ChannelCapacity {
                element: operand(element)?,
                channel: operand(channel)?,
            },
            Intrinsic::IoType(a) => Intrinsic::IoType(operand(a)?),
            Intrinsic::IoPure {
                result: a,
                value: b,
            } => try_binary(a, b, operand, |result, value| Intrinsic::IoPure {
                result,
                value,
            })?,
            Intrinsic::IoBind {
                from: a,
                to: b,
                action,
                continuation: f,
            } => Intrinsic::IoBind {
                from: operand(a)?,
                to: operand(b)?,
                action: operand(action)?,
                continuation: operand(f)?,
            },
        })
    }

    /// [`try_map`](Self::try_map) where no operand can fail.
    pub fn map<U>(&self, mut operand: impl FnMut(&T) -> U) -> Intrinsic<U> {
        let mapped = self.try_map(&mut |term| Ok::<U, std::convert::Infallible>(operand(term)));

        match mapped {
            Ok(mapped) => mapped,
            Err(never) => match never {},
        }
    }
}

fn try_binary<T, U, E>(
    left: &T,
    right: &T,
    operand: &mut impl FnMut(&T) -> Result<U, E>,
    build: impl FnOnce(U, U) -> Intrinsic<U>,
) -> Result<Intrinsic<U>, E> {
    let left = operand(left)?;
    let right = operand(right)?;

    Ok(build(left, right))
}

/// Which intrinsic type a match scrutinee is required to have. The legal selectors for `curios-elab`'s `check_intrinsic_head`/`expect_intrinsic_head` — exactly the type-former `Intrinsic`s those helpers accept, as a closed set so an out-of-range selector is unrepresentable rather than an `unreachable!` panic.
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
