//! Construction conveniences for the core representation, owned by the stage that exists to make building terms pleasant.
//!
//! Extension traits keep every call site spelling what it always spelled — `Intrinsic::flt_add(…)`, `Term::struct_at(…)` — under a plain trait import (`use curios_elab::IntrinsicBuilders;`). The representation crate keeps only the constructors its own fold table and the certifier name; everything that exists purely so a lowering or an elaborator reads well lives here.

use {
    curios_core::{
        Apply, Bang, Free, Func, FuncType, Global, Infix, Intrinsic, Level, Many, MetaId, Metavar,
        MetavarOrigin, NumLit, Scope, Struct, StructEntry, StructType, Subterm, Term, Transient,
        Tuple,
    },
    curios_num::Natural,
    curios_utilities::InfixOp,
    std::rc::Rc,
};

/// Constructors for [`Intrinsic`] operations no judgment ever builds.
pub trait IntrinsicBuilders {
    /// A `HandleEql` node — handle identity, the one pure `Handle` operation — from anything term-shaped.
    fn handle_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>;

    /// A `NatDiv` node from anything term-shaped.
    fn nat_div<F, S, P>(left: F, right: S, non_zero: P) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
        P: Into<Term>;

    /// A `NatRem` node from anything term-shaped.
    fn nat_rem<F, S, P>(left: F, right: S, non_zero: P) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
        P: Into<Term>;

    /// An `IntEql` node from anything term-shaped.
    fn int_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>;

    /// An `IntAdd` node from anything term-shaped.
    fn int_add<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>;

    /// An `IntSub` node from anything term-shaped.
    fn int_sub<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>;

    /// An `IntMul` node from anything term-shaped.
    fn int_mul<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>;

    /// An `IntNeq` node from anything term-shaped.
    fn int_neq<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>;

    /// An `IntDiv` node from anything term-shaped.
    fn int_div<F, S, P>(left: F, right: S, non_zero: P) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
        P: Into<Term>;

    /// An `IntRem` node from anything term-shaped.
    fn int_rem<F, S, P>(left: F, right: S, non_zero: P) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
        P: Into<Term>;

    /// An `IntLt` node from anything term-shaped.
    fn int_lt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>;

    /// An `IntGt` node from anything term-shaped.
    fn int_gt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>;

    /// An `IntLe` node from anything term-shaped.
    fn int_lte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>;

    /// An `IntGe` node from anything term-shaped.
    fn int_gte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>;

    /// A `FltAdd` node from anything term-shaped.
    fn flt_add<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>;

    /// A `FltSub` node from anything term-shaped.
    fn flt_sub<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>;

    /// A `FltMul` node from anything term-shaped.
    fn flt_mul<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>;

    /// A `FltNeg` node from anything term-shaped.
    fn flt_neg<T>(inner: T) -> Self
    where
        T: Into<Term>;

    /// A `FltAbs` node from anything term-shaped.
    fn flt_abs<T>(inner: T) -> Self
    where
        T: Into<Term>;

    /// A `FltSqrt` node from anything term-shaped.
    fn flt_sqrt<T>(inner: T) -> Self
    where
        T: Into<Term>;

    /// A `FltFloor` node from anything term-shaped.
    fn flt_floor<T>(inner: T) -> Self
    where
        T: Into<Term>;

    /// A `FltCeil` node from anything term-shaped.
    fn flt_ceil<T>(inner: T) -> Self
    where
        T: Into<Term>;

    /// A `FltTrunc` node from anything term-shaped.
    fn flt_trunc<T>(inner: T) -> Self
    where
        T: Into<Term>;

    /// A `FltNearest` (round-ties-to-even) node from anything term-shaped.
    fn flt_nearest<T>(inner: T) -> Self
    where
        T: Into<Term>;

    /// A `FltDiv` node from anything term-shaped.
    fn flt_div<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>;

    /// A `FltMin` node from anything term-shaped.
    fn flt_min<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>;

    /// A `FltMax` node from anything term-shaped.
    fn flt_max<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>;

    /// A `FltEql` node from anything term-shaped.
    fn flt_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>;

    /// A `FltNeq` node from anything term-shaped.
    fn flt_neq<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>;

    /// A `FltLt` node from anything term-shaped.
    fn flt_lt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>;

    /// A `FltGt` node from anything term-shaped.
    fn flt_gt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>;

    /// A `FltLe` node from anything term-shaped.
    fn flt_lte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>;

    /// A `FltGe` node from anything term-shaped.
    fn flt_gte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>;

    /// A `NatToInt` conversion node from anything term-shaped.
    fn nat_to_int<T>(inner: T) -> Self
    where
        T: Into<Term>;

    /// An `IntToNat` conversion node from anything term-shaped, with the proof that its operand is non-negative.
    fn int_to_nat<T, P>(int: T, non_neg: P) -> Self
    where
        T: Into<Term>,
        P: Into<Term>;

    /// An `IntToFlt` conversion node from anything term-shaped.
    fn int_to_flt<T>(inner: T) -> Self
    where
        T: Into<Term>;

    /// A `NatToFlt` conversion node from anything term-shaped.
    fn nat_to_flt<T>(inner: T) -> Self
    where
        T: Into<Term>;

    /// A `FltToInt` conversion node from anything term-shaped.
    fn flt_to_int<T, P>(flt: T, finite: P) -> Self
    where
        T: Into<Term>,
        P: Into<Term>;

    /// A `FltToNat` conversion node from anything term-shaped, with the proof that its operand is a non-negative number.
    fn flt_to_nat<T, P>(flt: T, non_neg: P) -> Self
    where
        T: Into<Term>,
        P: Into<Term>;

    /// A `FltToLeBytes` node (a float's four little-endian bytes as a `Bin`) from anything term-shaped.
    fn flt_to_le_bytes<T>(inner: T) -> Self
    where
        T: Into<Term>;

    /// A `FltOfLeBytes` node (a float assembled from its four little-endian bytes) from anything term-shaped.
    fn flt_of_le_bytes<T, P>(bin: T, four_bytes: P) -> Self
    where
        T: Into<Term>,
        P: Into<Term>;

    /// A cell allocation — the `Intrinsic::Cell` variant — from a term-shaped element type and initial value.
    fn cell_new<T, I>(type_: T, init: I) -> Self
    where
        T: Into<Term>,
        I: Into<Term>;

    /// A `CellSet` node from term-shaped element type, cell, and new value.
    fn cell_set<T, C, V>(type_: T, cell: C, value: V) -> Self
    where
        T: Into<Term>,
        C: Into<Term>,
        V: Into<Term>;

    /// A `CellGet` node from term-shaped element type and cell.
    fn cell_get<T, C>(type_: T, cell: C) -> Self
    where
        T: Into<Term>,
        C: Into<Term>;
}

impl IntrinsicBuilders for Intrinsic {
    fn handle_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::HandleEql(left.into(), right.into())
    }

    fn nat_div<F, S, P>(left: F, right: S, non_zero: P) -> Self
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

    fn nat_rem<F, S, P>(left: F, right: S, non_zero: P) -> Self
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

    fn int_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntEql(left.into(), right.into())
    }

    fn int_add<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntAdd(left.into(), right.into())
    }

    fn int_sub<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntSub(left.into(), right.into())
    }

    fn int_mul<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntMul(left.into(), right.into())
    }

    fn int_neq<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntNeq(left.into(), right.into())
    }

    fn int_div<F, S, P>(left: F, right: S, non_zero: P) -> Self
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

    fn int_rem<F, S, P>(left: F, right: S, non_zero: P) -> Self
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

    fn int_lt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntLt(left.into(), right.into())
    }

    fn int_gt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntGt(left.into(), right.into())
    }

    fn int_lte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntLe(left.into(), right.into())
    }

    fn int_gte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntGe(left.into(), right.into())
    }

    fn flt_add<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltAdd(left.into(), right.into())
    }

    fn flt_sub<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltSub(left.into(), right.into())
    }

    fn flt_mul<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltMul(left.into(), right.into())
    }

    fn flt_neg<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltNeg(inner.into())
    }

    fn flt_abs<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltAbs(inner.into())
    }

    fn flt_sqrt<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltSqrt(inner.into())
    }

    fn flt_floor<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltFloor(inner.into())
    }

    fn flt_ceil<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltCeil(inner.into())
    }

    fn flt_trunc<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltTrunc(inner.into())
    }

    fn flt_nearest<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltNearest(inner.into())
    }

    fn flt_div<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltDiv(left.into(), right.into())
    }

    fn flt_min<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltMin(left.into(), right.into())
    }

    fn flt_max<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltMax(left.into(), right.into())
    }

    fn flt_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltEql(left.into(), right.into())
    }

    fn flt_neq<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltNeq(left.into(), right.into())
    }

    fn flt_lt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltLt(left.into(), right.into())
    }

    fn flt_gt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltGt(left.into(), right.into())
    }

    fn flt_lte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltLe(left.into(), right.into())
    }

    fn flt_gte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltGe(left.into(), right.into())
    }

    fn nat_to_int<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::NatToInt(inner.into())
    }

    fn int_to_nat<T, P>(int: T, non_neg: P) -> Self
    where
        T: Into<Term>,
        P: Into<Term>,
    {
        Self::IntToNat {
            int: int.into(),
            non_neg: non_neg.into(),
        }
    }

    fn int_to_flt<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::IntToFlt(inner.into())
    }

    fn nat_to_flt<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::NatToFlt(inner.into())
    }

    fn flt_to_int<T, P>(flt: T, finite: P) -> Self
    where
        T: Into<Term>,
        P: Into<Term>,
    {
        Self::FltToInt {
            flt: flt.into(),
            finite: finite.into(),
        }
    }

    fn flt_to_nat<T, P>(flt: T, non_neg: P) -> Self
    where
        T: Into<Term>,
        P: Into<Term>,
    {
        Self::FltToNat {
            flt: flt.into(),
            non_neg: non_neg.into(),
        }
    }

    fn flt_to_le_bytes<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltToLeBytes(inner.into())
    }

    fn flt_of_le_bytes<T, P>(bin: T, four_bytes: P) -> Self
    where
        T: Into<Term>,
        P: Into<Term>,
    {
        Self::FltOfLeBytes {
            bin: bin.into(),
            four_bytes: four_bytes.into(),
        }
    }

    fn cell_new<T, I>(type_: T, init: I) -> Self
    where
        T: Into<Term>,
        I: Into<Term>,
    {
        Self::Cell {
            element: type_.into(),
            initial: init.into(),
        }
    }

    fn cell_set<T, C, V>(type_: T, cell: C, value: V) -> Self
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

    fn cell_get<T, C>(type_: T, cell: C) -> Self
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
pub trait TermBuilders {
    /// Return the absolute free-variable head of a direct type-family alias.
    ///
    /// The declared type must structurally end in a literal [`Subterm::Type`] or [`Subterm::Prop`] after peeling only function-type telescopes. The body is then peeled through function literals and application spines, again structurally and without reduction or substitution. Computed heads, local heads, and aliased universe annotations are deliberately excluded.
    fn direct_type_alias_target(&self, declared_type: &Term) -> Option<&Free>;

    /// A written goal `?`, as `into_core` mints one: a bare metavariable (empty spine, like [`Term::metavar`]) whose [`MetavarOrigin::Goal`] origin makes zonk *report* what elaboration determined for it — scope, type, and solution — instead of splicing silently.
    fn goal(id: impl Into<MetaId>) -> Self;

    /// An unresolved infix application ([`Infix`]) — elaboration-transient, consumed by `elaborate_infix`.
    fn infix(op: InfixOp, left: Term, right: Term) -> Self;

    /// A postfix `!` sequencing site ([`Bang`]) — elaboration-transient, consumed by `elaborate_bang`. `action` is the sequenced description; `continuation` is the hoisted rest of the region as an ordinary one-parameter function.
    fn bang(action: Term, continuation: Term) -> Self;

    /// Carry a *written* motive — the surface term `into_core` lowered, before elaboration has closed it into a scope — as an arity-0 [`Scope`].
    ///
    /// Lowering cannot close the scope itself: the motive's arity is `n_indices + 1`, and the eliminated family is only known once the scrutinee's type is inferred. Arity 0 is a free tag for "not yet scoped" because no elaborated motive can have it — every eliminator binds at least the scrutinee, so `check_motive` always re-closes at arity 1 or more. `Scope::constant` performs no capture, so the term goes in and comes back out of `body()` untouched.
    fn match_motive_written<M>(motive: M) -> Scope<Many>
    where
        M: Into<Term>;

    /// A polymorphic numeric literal ([`NumLit`]) — elaboration-transient, resolved to a concrete `Nat`/`Int`/`Flt` intrinsic by `elaborate_numlit`.
    fn num_lit(magnitude: Natural, signed: bool, negative: bool) -> Self;

    /// A struct literal carrying the written entry shapes from `into_core`; elaboration validates them against the declared fields and rebuilds entry-free, exactly like `tuple_named`.
    fn struct_entries<I, P, J, T>(name: Global, params: I, fields: J) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<Term>,
        J: IntoIterator<Item = (StructEntry, T)>,
        T: Into<Term>;

    /// Build a [`StructType`] normal form — what the generated type-former's body reduces to. Users never write one directly; see the type's docs.
    fn struct_type<I, P>(name: Global, params: I) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<Term>;

    fn struct_type_at<U, I, P>(name: Global, universes: U, params: I) -> Self
    where
        U: IntoIterator<Item = Level>,
        I: IntoIterator<Item = P>,
        P: Into<Term>;

    /// A tuple literal carrying its written field names from `into_core`; elaboration checks them against the expected tuple type's labels and rebuilds the literal name-free. An all-`None` name list collapses to the positional normal form of [`Term::tuple`], so syntactic equality never splits on how the literal was spelled.
    fn tuple_named<I, T>(fields: I) -> Self
    where
        I: IntoIterator<Item = (Option<String>, T)>,
        T: Into<Term>;
}

impl TermBuilders for Term {
    fn direct_type_alias_target(&self, declared_type: &Term) -> Option<&Free> {
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

    fn goal(id: impl Into<MetaId>) -> Self {
        Self::from(Subterm::Metavar(Metavar {
            id: id.into(),
            spine: Rc::new(Vec::new()),
            origin: Some(MetavarOrigin::Goal),
        }))
    }

    fn infix(op: InfixOp, left: Term, right: Term) -> Self {
        Self::from(Subterm::Transient(Transient::Infix(Infix {
            op,
            left,
            right,
        })))
    }

    fn bang(action: Term, continuation: Term) -> Self {
        Self::from(Subterm::Transient(Transient::Bang(Bang {
            action,
            continuation,
        })))
    }

    fn match_motive_written<M>(motive: M) -> Scope<Many>
    where
        M: Into<Term>,
    {
        Scope::constant(Many(0), motive.into())
    }

    fn num_lit(magnitude: Natural, signed: bool, negative: bool) -> Self {
        Self::from(Subterm::Transient(Transient::NumLit(NumLit {
            magnitude,
            signed,
            negative,
        })))
    }

    fn struct_entries<I, P, J, T>(name: Global, params: I, fields: J) -> Self
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

    fn struct_type<I, P>(name: Global, params: I) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<Term>,
    {
        Self::struct_type_at(name, Vec::<Level>::new(), params)
    }

    fn struct_type_at<U, I, P>(name: Global, universes: U, params: I) -> Self
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

    fn tuple_named<I, T>(fields: I) -> Self
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
