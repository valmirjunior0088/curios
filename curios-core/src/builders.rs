//! Construction conveniences for the representation: the constructors the surface lowering and the elaborator build terms with, and that no judgment ever calls.
//!
//! They live beside the representation because both of those stages build terms and neither owns what a term is. Kept in the elaborator, they made `curios-text` depend on all of it for a handful of constructors. They admit nothing, so they are here on the terms `print` is: compiled with the representation, never run by a judgment.

use {
    super::{
        Apply, Bang, Free, Func, FuncType, Global, Infix, Intrinsic, Level, Many, NumLit, Scope,
        Struct, StructEntry, StructType, Subterm, Term, Transient, Tuple, Var,
    },
    curios_num::{Binary, Grain, Natural, Rounding},
    curios_utilities::{InfixOp, Sign, StringSyntax, SyntaxName},
};

/// A registered function or constructor `Var`, applied — the absolute core identity a registry slot denotes, so privacy is no obstacle: these are already-resolved core references, not surface names.
pub fn syn_call(name: SyntaxName, args: impl IntoIterator<Item = Term>) -> Term {
    Term::apply(
        Term::var(Var::free(Free::global(name.qualifier()))),
        args.into_iter().collect::<Vec<_>>(),
    )
}

/// The Core expansion of a string literal: the `/std/Str` struct over the packed bytes, its `Valid` field closed by the one inhabitant of `True`, built from the [`StringSyntax`] slots alone. Owned here so the surface lowering, the synthesized test tail and the derivations expand a literal identically.
///
/// `Valid` is decided, so the literal's bytes certify themselves: `True/qed()` checks against `Valid(b)` by running the scan over `b` in both checkers, and the term the compiler writes is the same whatever the literal's length or content. How the library proves validity is the library's; nothing here knows the scan.
pub fn str_literal(syntax: &StringSyntax, bytes: &[u8]) -> Term {
    let packed = Term::intrinsic(Intrinsic::Bin(Grain::X, Binary::from_bytes(bytes.to_vec())));
    let valid = syn_call(syntax.qed, []);

    Term::struct_(
        Global::Authored(syntax.string.qualifier()),
        Vec::<Term>::new(),
        [packed, valid],
    )
}

/// Constructors for [`Intrinsic`] operations no judgment ever builds.
impl Intrinsic {
    /// A `NatDiv` node from anything term-shaped.
    pub fn nat_div<F, S, P>(left: F, right: S, non_zero: P) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
        P: Into<Term>,
    {
        Self::NatDiv {
            dividend: left.into(),
            divisor: right.into(),
            non_zero: non_zero.into(),
        }
    }

    /// A `NatRem` node from anything term-shaped.
    pub fn nat_rem<F, S, P>(left: F, right: S, non_zero: P) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
        P: Into<Term>,
    {
        Self::NatRem {
            dividend: left.into(),
            divisor: right.into(),
            non_zero: non_zero.into(),
        }
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
    pub fn int_div<F, S, P>(left: F, right: S, non_zero: P) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
        P: Into<Term>,
    {
        Self::IntDiv {
            dividend: left.into(),
            divisor: right.into(),
            non_zero: non_zero.into(),
        }
    }

    /// An `IntRem` node from anything term-shaped.
    pub fn int_rem<F, S, P>(left: F, right: S, non_zero: P) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
        P: Into<Term>,
    {
        Self::IntRem {
            dividend: left.into(),
            divisor: right.into(),
            non_zero: non_zero.into(),
        }
    }

    /// An `IntLt` node from anything term-shaped.
    pub fn int_lt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntLt(left.into(), right.into())
    }

    /// An `IntLe` node from anything term-shaped.
    pub fn int_lte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntLe(left.into(), right.into())
    }

    /// A `FltAdd` node rounded in `rounding`, from anything term-shaped.
    pub fn flt_add<F, S>(rounding: Rounding, left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltAdd(rounding, left.into(), right.into())
    }

    /// A `FltSub` node rounded in `rounding`, from anything term-shaped.
    pub fn flt_sub<F, S>(rounding: Rounding, left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltSub(rounding, left.into(), right.into())
    }

    /// A `FltMul` node rounded in `rounding`, from anything term-shaped.
    pub fn flt_mul<F, S>(rounding: Rounding, left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltMul(rounding, left.into(), right.into())
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

    /// A `FltSqrt` node rounded in `rounding`, from anything term-shaped.
    pub fn flt_sqrt<T>(rounding: Rounding, inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltSqrt(rounding, inner.into())
    }

    /// A `FltFma` node, `a · b + c` rounded once in `rounding`, from anything term-shaped.
    pub fn flt_fma<A, B, C>(rounding: Rounding, a: A, b: B, c: C) -> Self
    where
        A: Into<Term>,
        B: Into<Term>,
        C: Into<Term>,
    {
        Self::FltFma(rounding, a.into(), b.into(), c.into())
    }

    /// A `FltRoundIntegral` node in `rounding`, from anything term-shaped.
    pub fn flt_round_integral<T>(rounding: Rounding, inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltRoundIntegral(rounding, inner.into())
    }

    /// A `FltDiv` node rounded in `rounding`, from anything term-shaped.
    pub fn flt_div<F, S>(rounding: Rounding, left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltDiv(rounding, left.into(), right.into())
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

    /// A `FltLe` node from anything term-shaped.
    pub fn flt_lte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltLe(left.into(), right.into())
    }

    /// A `NatToInt` conversion node from anything term-shaped.
    pub fn nat_to_int<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::NatToInt(inner.into())
    }

    /// An `IntToNat` conversion node from anything term-shaped, with the proof that its operand is non-negative.
    pub fn int_to_nat<T, P>(int: T, non_neg: P) -> Self
    where
        T: Into<Term>,
        P: Into<Term>,
    {
        Self::IntToNat {
            int: int.into(),
            non_neg: non_neg.into(),
        }
    }

    /// A `NatToByte` conversion node from anything term-shaped, with the proof that its operand is below `256`.
    pub fn nat_to_byte<T, P>(nat: T, below: P) -> Self
    where
        T: Into<Term>,
        P: Into<Term>,
    {
        Self::NatToByte {
            nat: nat.into(),
            below: below.into(),
        }
    }

    /// An `IntToFlt` conversion node rounded in `rounding`, from anything term-shaped.
    pub fn int_to_flt<T>(rounding: Rounding, inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::IntToFlt(rounding, inner.into())
    }

    /// A `NatToFlt` conversion node rounded in `rounding`, from anything term-shaped.
    pub fn nat_to_flt<T>(rounding: Rounding, inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::NatToFlt(rounding, inner.into())
    }

    /// A `FltToInt` conversion node from anything term-shaped.
    pub fn flt_to_int<T, P>(flt: T, finite: P) -> Self
    where
        T: Into<Term>,
        P: Into<Term>,
    {
        Self::FltToInt {
            flt: flt.into(),
            finite: finite.into(),
        }
    }

    /// A `FltToNat` conversion node from anything term-shaped, with the proof that its operand is a non-negative number.
    pub fn flt_to_nat<T, P>(flt: T, non_neg: P) -> Self
    where
        T: Into<Term>,
        P: Into<Term>,
    {
        Self::FltToNat {
            flt: flt.into(),
            non_neg: non_neg.into(),
        }
    }

    /// A `FltToLeBytes` node (a float's four little-endian bytes as a `Bin`) from anything term-shaped.
    pub fn flt_to_le_bytes<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltToLeBytes(inner.into())
    }

    /// A `FltOfLeBytes` node (a float assembled from its four little-endian bytes) from anything term-shaped.
    pub fn flt_of_le_bytes<T, P>(bin: T, eight_bytes: P) -> Self
    where
        T: Into<Term>,
        P: Into<Term>,
    {
        Self::FltOfLeBytes {
            bin: bin.into(),
            eight_bytes: eight_bytes.into(),
        }
    }

    /// A cell allocation — the `Intrinsic::Cell` variant — from a term-shaped element type and initial value.
    pub fn cell_new<T, I>(type_: T, init: I) -> Self
    where
        T: Into<Term>,
        I: Into<Term>,
    {
        Self::Cell {
            element: type_.into(),
            initial: init.into(),
        }
    }

    /// A `CellSet` node from term-shaped element type, cell, and new value.
    pub fn cell_set<T, C, V>(type_: T, cell: C, value: V) -> Self
    where
        T: Into<Term>,
        C: Into<Term>,
        V: Into<Term>,
    {
        Self::CellSet {
            element: type_.into(),
            cell: cell.into(),
            value: value.into(),
        }
    }

    /// A `CellGet` node from term-shaped element type and cell.
    pub fn cell_get<T, C>(type_: T, cell: C) -> Self
    where
        T: Into<Term>,
        C: Into<Term>,
    {
        Self::CellGet {
            element: type_.into(),
            cell: cell.into(),
        }
    }
}

/// Constructors for [`Term`] shapes no judgment ever builds.
impl Term {
    /// Return the absolute free-variable head of a direct type-family alias.
    ///
    /// The declared type must structurally end in a literal [`Subterm::Type`] or [`Subterm::Prop`] after peeling only function-type telescopes. The body is then peeled through function literals and application spines, again structurally and without reduction or substitution. Computed heads, local heads, and aliased universe annotations are deliberately excluded.
    pub fn direct_type_alias_target(&self, declared_type: &Term) -> Option<&Free> {
        fn ends_in_literal_sort(term: &Term) -> bool {
            match &**term {
                Subterm::Type(_) | Subterm::Prop => true,
                Subterm::FuncType(FuncType { telescope, .. }) => {
                    ends_in_literal_sort(telescope.terminal())
                }
                _ => false,
            }
        }

        fn application_head(term: &Term) -> Option<&Free> {
            match &**term {
                Subterm::Apply(Apply { head, .. }) => application_head(head),
                Subterm::Var(var) => var.as_free(),
                _ => None,
            }
        }

        fn direct_head(term: &Term) -> Option<&Free> {
            match &**term {
                Subterm::Func(Func { telescope, .. }) => direct_head(telescope.terminal()),
                _ => application_head(term),
            }
        }

        ends_in_literal_sort(declared_type)
            .then(|| direct_head(self))
            .flatten()
            .filter(|target| !target.is_local())
    }

    /// An unresolved infix application ([`Infix`]) — elaboration-transient, consumed by `elaborate_infix`.
    pub fn infix(op: InfixOp, left: Term, right: Term) -> Self {
        Self::from(Subterm::Transient(Transient::Infix(Infix {
            op,
            left,
            right,
        })))
    }

    /// A postfix `!` sequencing site ([`Bang`]) — elaboration-transient, consumed by `elaborate_bang`. `action` is the sequenced description; `continuation` is the hoisted rest of the region as an ordinary one-parameter function.
    pub fn bang(action: Term, continuation: Term) -> Self {
        Self::from(Subterm::Transient(Transient::Bang(Bang {
            action,
            continuation,
        })))
    }

    /// The body of a body-less witness ([`Transient::Derive`]) — elaboration-transient, consumed by `elaborate_derive` against the concept application it is checked at.
    pub fn derive() -> Self {
        Self::from(Subterm::Transient(Transient::Derive))
    }

    /// Carry a *written* motive — the surface term `into_core` lowered, before elaboration has closed it into a scope — as an arity-0 [`Scope`].
    ///
    /// Lowering cannot close the scope itself: the motive's arity is `n_indices + 1`, and the eliminated family is only known once the scrutinee's type is inferred. Arity 0 is a free tag for "not yet scoped" because no elaborated motive can have it — every eliminator binds at least the scrutinee, so `check_motive` always re-closes at arity 1 or more. `Scope::constant` performs no capture, so the term goes in and comes back out of `body()` untouched.
    pub fn match_motive_written<M>(motive: M) -> Scope<Many>
    where
        M: Into<Term>,
    {
        Scope::constant(Many(0), motive.into())
    }

    /// A polymorphic numeric literal ([`NumLit`]) — elaboration-transient, resolved to a concrete `Nat`/`Int`/`Flt` intrinsic by `elaborate_num_lit`.
    pub fn num_lit(magnitude: Natural, sign: Sign) -> Self {
        Self::from(Subterm::Transient(Transient::NumLit(NumLit::Number {
            magnitude,
            sign,
        })))
    }

    /// A character-spelled polymorphic literal ([`NumLit::Character`]) — elaboration-transient like the numeral form, defaulting to `/std/Char`.
    pub fn num_lit_char(character: char) -> Self {
        Self::from(Subterm::Transient(Transient::NumLit(NumLit::Character(
            character,
        ))))
    }

    /// A struct literal carrying the written entry shapes from `into_core`; elaboration validates them against the declared fields and rebuilds entry-free, exactly like `tuple_named`.
    pub fn struct_entries<I, P, J, T>(name: Global, params: I, fields: J) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<Term>,
        J: IntoIterator<Item = (StructEntry, T)>,
        T: Into<Term>,
    {
        let (mut entries, fields): (Vec<_>, Vec<_>) = fields
            .into_iter()
            .map(|(entry, term)| (entry, term.into()))
            .unzip();

        if entries.iter().all(|e| *e == StructEntry::Field(None)) {
            entries = vec![];
        }

        Self::from(Subterm::Struct(Struct {
            name,
            universes: vec![],
            params: params.into_iter().map(|p| p.into()).collect(),
            fields,
            entries,
        }))
    }

    /// Build a [`StructType`] normal form — what the generated type-former's body reduces to. Users never write one directly; see the type's docs.
    pub fn struct_type<I, P>(name: Global, params: I) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<Term>,
    {
        Self::struct_type_at(name, Vec::<Level>::new(), params)
    }

    pub fn struct_type_at<U, I, P>(name: Global, universes: U, params: I) -> Self
    where
        U: IntoIterator<Item = Level>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
    {
        Self::from(Subterm::StructType(StructType {
            name,
            universes: universes.into_iter().collect(),
            params: params.into_iter().map(|p| p.into()).collect(),
        }))
    }

    /// A tuple literal carrying its written field names from `into_core`; elaboration checks them against the expected tuple type's labels and rebuilds the literal name-free. An all-`None` name list collapses to the positional normal form of [`Term::tuple`], so syntactic equality never splits on how the literal was spelled.
    pub fn tuple_named<I, T>(fields: I) -> Self
    where
        I: IntoIterator<Item = (Option<String>, T)>,
        T: Into<Term>,
    {
        let (mut names, fields): (Vec<_>, Vec<_>) = fields
            .into_iter()
            .map(|(name, term)| (name, term.into()))
            .unzip();

        if names.iter().all(Option::is_none) {
            names = vec![];
        }

        Self::from(Subterm::Tuple(Tuple { fields, names }))
    }
}
