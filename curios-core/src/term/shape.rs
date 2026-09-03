//! The payload each [`Subterm`] variant carries: one named type per shape, so a variant's fields are read by name at every construction and match rather than by position.
//!
//! Nothing here judges or reduces. The types are plain records with the binder discipline spelled in their field types — a [`Scope`] where one binder is bound, a [`Telescope`] where several are — and the handful of impls are the operations that discipline forces: [`RecGroup`]'s member arithmetic, [`InductArm`]'s and [`LetBinding`]'s accessors, and the two hand-written [`TupleType`] instances that make an anonymous product compare by its fields.

use super::*;

/// An unresolved infix application `left <op> right`. Elaboration infers a shared operand type for the two sides and rebuilds the node as a concept method call (`a + b` ≙ `Add/add(a, b)`; `&&`/`||` alone are hardcoded on `Bool` — see `elaborate_infix`); the node never survives elaboration.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Infix {
    pub op: InfixOp,
    pub left: Term,
    pub right: Term,
}

/// A polymorphic literal, resolved to a concrete carrier by `elaborate_num_lit` once the expected type is known (or defaulted by shape). Decimal literals are *not* `NumLit` — they parse straight to `Intrinsic::Flt`.
///
/// The two spellings are one transient because they share the realization machinery, and an enum because they share nothing else: a numeral has a sign and no scalar constraint, a character has a scalar (Rust's `char` guarantees it) and no sign, and neither combination's absence is a convention — it is unrepresentable.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum NumLit {
    /// A numeral: candidates `Nat`/`Bool`/`Byte`/`Int`/`Flt`, defaulting by [`Sign`] — `Int` when marked, else `Nat`.
    Number { magnitude: Natural, sign: Sign },
    /// A character-spelled scalar value: candidates `Char` (the default), `Nat`, `Byte`, `Int`.
    Character(char),
}

/// A postfix `!` sequencing site, already hoisted by lowering: `action` is the sequenced description, `continuation` the rest of its region as an ordinary one-parameter function (domain a lowering-minted hole). Consumed by `elaborate_bang`, which replaces it with the `/syn/Monad/bind` application the lowerer once spelled directly — the construction moved behind elaboration so the sequencing survives to the stage that can make type-directed decisions about it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Bang {
    pub action: Term,
    pub continuation: Term,
}

/// A lowering-born constructor consumed by elaboration: born in `into_core`, eliminated by `elaborate`, never legitimate in reduced, converted, zonked, or erased terms, and refused at the kernel boundary. Grouping the members under one `Subterm` variant lets every post-elaboration consumer dismiss the class wholesale — one refusal arm at the kernel, one `unreachable!` in each downstream stage — so a future transient extends lowering, elaboration, and display without touching them. `Metavar` is deliberately not a member: conversion parks on metavariables and zonk consumes them, so its lifecycle is elaboration-internal rather than pre-elaboration.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum Transient {
    /// An unresolved infix operator application; consumed by `elaborate_infix`.
    Infix(Infix),
    /// A polymorphic numeric literal; consumed by `elaborate_num_lit`.
    NumLit(NumLit),
    /// A postfix `!` sequencing site; consumed by `elaborate_bang`.
    Bang(Bang),
    /// A witness body the compiler writes — the body position of a body-less `satisfy C(T);`; consumed by `elaborate_derive`, which reads the concept application it is checked against and expands it or refuses. Carries nothing: the expected type is the whole of its input.
    Derive,
}

impl Transient {
    /// The direct child terms, for the structural walks that must traverse a lowered (or display-folded) term. Transients are plain data over their children — none binds a variable of its own (`Bang`'s continuation is an ordinary `Func`, which carries the binder) — so the walks need no scope handling here.
    pub fn subterms(&self) -> impl Iterator<Item = &Term> {
        let children = match self {
            Transient::Infix(Infix { left, right, .. }) => [Some(left), Some(right)],
            Transient::NumLit(_) | Transient::Derive => [None, None],
            Transient::Bang(Bang {
                action,
                continuation,
            }) => [Some(action), Some(continuation)],
        };
        children.into_iter().flatten()
    }

    /// Rebuild this transient with every child term mapped, for the structural rewrites.
    pub fn map_subterms(&self, f: &mut impl FnMut(&Term) -> Term) -> Transient {
        match self {
            Transient::Infix(Infix { op, left, right }) => Transient::Infix(Infix {
                op: *op,
                left: f(left),
                right: f(right),
            }),
            Transient::NumLit(num_lit) => Transient::NumLit(num_lit.clone()),
            Transient::Derive => Transient::Derive,
            Transient::Bang(Bang {
                action,
                continuation,
            }) => Transient::Bang(Bang {
                action: f(action),
                continuation: f(continuation),
            }),
        }
    }
}

/// `plicities` parallels the telescope, one mark per binder — sealed at the crate boundary so the correspondence is enforced by [`FuncType::new`], the one door an outside constructor has. `Telescope` itself is unchanged. Erasure is sort-driven (a proof or a type erases), so a function type carries no runtime-multiplicity marks of its own.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct FuncType {
    pub telescope: Telescope<Term>,
    pub(crate) plicities: Vec<Plicity>,
}

impl FuncType {
    /// The one construction door outside this crate: one mark per telescope binder, asserted here rather than at every use.
    pub fn new(telescope: Telescope<Term>, plicities: Vec<Plicity>) -> Self {
        assert_eq!(plicities.len(), telescope.len());
        Self {
            telescope,
            plicities,
        }
    }

    /// The marks, one per telescope binder by construction.
    pub fn plicities(&self) -> &[Plicity] {
        &self.plicities
    }
}

/// A function literal: the parameter annotations and the body as one [`Telescope`] (each entry a parameter type, the `Done` payload the body), with `plicities` paralleling the telescope one mark per binder — the builder asserts the lengths agree. Plicity is part of a function's identity and calling convention: a lambda carries the marks its binders were written with (before elaboration) and the complete canonical marks of its checked type (after elaboration, once omitted hidden binders are inserted). Derived `Eq`/`Hash` include `plicities` so that two lambdas differing only in a written mark never share an elaboration-cache entry.
///
/// Erasure ignores `plicities`; its keep/drop decisions come from the checked function type and sort information.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Func {
    pub telescope: Telescope<Term>,
    pub(crate) plicities: Vec<Plicity>,
}

impl Func {
    /// The one construction door outside this crate: one mark per telescope binder, asserted here rather than at every use.
    pub fn new(telescope: Telescope<Term>, plicities: Vec<Plicity>) -> Self {
        assert_eq!(plicities.len(), telescope.len());
        Self {
            telescope,
            plicities,
        }
    }

    /// The marks, one per telescope binder by construction.
    pub fn plicities(&self) -> &[Plicity] {
        &self.plicities
    }
}

/// One call-site argument: the term with its written `@`/`use` mark. One vector of these rather than two parallel ones, so a mark can never drift out of correspondence with its term — the pairing `Cases::Induct` arms state for their bodies, applied to the spine. Core must carry the marks (rather than `into_core` resolving them) because `into_core` is type-blind: only the elaborator, holding the head's function type, can decide which binder an `@`-argument fills.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Argument {
    pub term: Term,
    pub plicity: Plicity,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Apply {
    pub head: Term,
    pub arguments: Vec<Argument>,
}

impl Apply {
    /// The argument terms alone, in application order — the dominant read; the marks ride beside them.
    pub fn params(&self) -> impl ExactSizeIterator<Item = &Term> + Clone + '_ {
        self.arguments.iter().map(|argument| &argument.term)
    }

    /// The marks alone, paralleling [`Apply::params`] by construction.
    pub fn plicities(&self) -> impl ExactSizeIterator<Item = Plicity> + Clone + '_ {
        self.arguments.iter().map(|argument| argument.plicity)
    }
}

/// A dependent product (Σ-type). Erasure is sort-driven: a proof or type-valued field is a *subset type* witness — dropped at erasure, leaving the relevant fields (and collapsing to the bare field when only one remains).
///
/// Unlike binder hints elsewhere, field labels are the target of `.label` resolution during elaboration, so they are part of the type's identity: `Eq`/`Hash` reassert them on top of the label-blind [`Telescope`] identity. Otherwise the reduction memo could hand elaboration a twin type whose labels differ, and a well-typed projection would fail to resolve.
#[derive(Debug, Clone, Eq)]
#[curios_archive::archived]
pub struct TupleType {
    pub telescope: Telescope<()>,
}

impl PartialEq for TupleType {
    fn eq(&self, other: &Self) -> bool {
        self.telescope == other.telescope && self.telescope.labels() == other.telescope.labels()
    }
}

impl Hash for TupleType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.telescope.hash(state);
        self.telescope.labels().hash(state);
    }
}

/// `names` carries the literal's written field names (`(status = 0, …)`) from `into_core` to elaboration, which checks them against the expected tuple type's labels and rebuilds the literal name-free. Empty means "no names written" — the invariant for every internally-built and post-elaboration tuple.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Tuple {
    pub fields: Vec<Term>,
    pub names: Vec<Option<String>>,
}

/// A projection's field is positional in every post-elaboration term; the `Label` form exists only between `into_core` and `elaborate`, which resolves it against the head's tuple type and rebuilds it as `Index`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum Field {
    Index(usize),
    Label(String),
}

/// A projection out of a tuple. See [`Field`] for why the field is positional in every post-elaboration term.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Proj {
    pub head: Term,
    pub field: Field,
}

/// An inductive type as an intrinsic normal form. Built inside the automatically-generated type-constructor function's body. Users never write one directly — they write `Result(E, A)` and the type-constructor function reduces to this. Two `InductType`s are convertible iff same `name` and pointwise-convertible `params` and `indices`.
///
/// `params` are uniform across constructors; `indices` are the per-case constrained binders — each constructor's registry terminal states its own index expressions. Use sites never distinguish them (`Vec(Bin, 3)` is one flat application of the type-constructor function); the split lives here and in the registry.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct InductType {
    pub name: Global,
    pub universes: Vec<Level>,
    pub params: Vec<Term>,
    pub indices: Vec<Term>,
}

/// A constructor application as an intrinsic normal form. Built inside the automatically-generated value-constructor function's body. Users never write one directly — they write `Result/success(value)` and the constructor function reduces to this.
///
/// `name` and `params` are recoverable from the term's inferred type; they are stored redundantly on purpose, so `convert` stays purely structural (no context lookups mid-comparison).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Variant {
    pub name: Global,
    pub universes: Vec<Level>,
    pub params: Vec<Term>,
    pub tag: Atom,
    pub payload: Vec<Term>,
}

/// A struct type as an intrinsic normal form (cf. [`InductType`], no indices). Built inside the generated type-former's body; users write `Pair(A, B)` and the former reduces to this. Convertible iff same `name` and pointwise-convertible `params`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct StructType {
    pub name: Global,
    pub universes: Vec<Level>,
    pub params: Vec<Term>,
}

/// One written struct-literal entry, parallel to [`Struct::fields`]: a plain positional field carrying its optional written label, an explicit `use <term>` fill that pairs with the concept's next `use`-marked field position, or a `..base` spread whose paired term is the base to copy the unwritten fields from (riding in `fields` keeps it visible to every term traversal). A `Spread`, if present, is `entries[0]` — enforced at elaboration, not by construction. Pre-elaboration metadata only, like written field names on [`Tuple`]; elaboration rebuilds the value entry-free.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum StructEntry {
    Field(Option<String>),
    Use,
    Spread,
}

/// A struct value as an intrinsic normal form (cf. [`Variant`], no tag). `name`/`params` are recoverable from the inferred type but stored redundantly so `convert` stays purely structural.
///
/// `entries` carries the literal's written entry shapes from `into_core`: elaboration checks plain fields positionally against the declared labels, pairs `use` entries with the concept's `use`-marked positions, and rebuilds the value entry-free. Empty means "all plain, no names written" — the invariant for every internally-built and post-elaboration struct.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Struct {
    pub name: Global,
    pub universes: Vec<Level>,
    pub params: Vec<Term>,
    pub fields: Vec<Term>,
    pub entries: Vec<StructEntry>,
}

/// The unified eliminator: every match form shares a scrutinee and a motive and differs only in its [`Cases`] payload.
///
/// An *elaborated* motive is closed at the eliminator's own arity: the scrutinee's indices in declaration order, then the scrutinee. That is 1 for every intrinsic carrier and for an unindexed inductive, and `n_indices + 1` for an indexed one. Parameters are never abstracted — they are uniform across constructors and fixed by the scrutinee's type, so the motive body refers to them through the ambient scope like any other term.
///
/// Before elaboration the motive is instead the *written term*, carried in an arity-0 scope — see `Term::match_motive_written`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Match {
    pub head: Term,
    pub motive: Scope<Many>,
    pub cases: Cases,
}

/// One enumerated arm of a [`Cases::Induct`]: the arm body closed over its payload binders, plus a plicity vector paralleling those binders one mark per slot. `plicities.len()` equals `body.arity()`. Before elaboration the marks are the written constructor-pattern plicities; after elaboration they are the constructor's canonical payload plicities. Reduction and erasure open the body positionally and never read the marks; conversion compares them alongside the bodies. Kept beside the body (rather than in a second map) and sealed at the crate boundary behind [`InductArm::new`], so the two can never drift apart.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct InductArm {
    pub body: Scope<Many>,
    pub(crate) plicities: Vec<Plicity>,
}

impl InductArm {
    /// The one construction door outside this crate: one mark per payload binder, asserted here rather than at every use.
    pub fn new(body: Scope<Many>, plicities: Vec<Plicity>) -> Self {
        assert_eq!(plicities.len(), body.arity());
        Self { body, plicities }
    }

    /// The marks, one per payload binder by construction.
    pub fn plicities(&self) -> &[Plicity] {
        &self.plicities
    }

    /// The arm's payload arity — equal to `plicities.len()`.
    pub fn arity(&self) -> usize {
        self.body.arity()
    }

    /// Open the arm body at its payload binders, positionally (plicity is not consulted by reduction or erasure).
    pub fn open(&self, args: &[&Term]) -> Term {
        self.body.open(args)
    }

    /// The arm body's free-variable reach, past its payload binders.
    pub(crate) fn reach(&self) -> usize {
        self.body.reach()
    }

    /// The arm's payload binder hints, in order.
    pub fn hint_iter(&self) -> impl Iterator<Item = Option<&str>> {
        self.body.hint_iter()
    }

    /// The arm's payload binders, in order.
    pub(crate) fn binder_iter(&self) -> impl Iterator<Item = Option<&Free>> {
        self.body.binder_iter()
    }

    /// Rebuild the arm with its whole body scope replaced, preserving the plicity vector (the traversal-side reconstruction helper).
    pub fn with_body(&self, body: Scope<Many>) -> Self {
        InductArm {
            body,
            plicities: self.plicities.clone(),
        }
    }
}

/// The arm payload of a [`Match`] — the only part that differs between the elimination forms (the scrutinee and motive live on `Match` itself). Which variant a match carries decides both its reduction rule and how erasure lowers it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum Cases {
    /// Dependent elimination of `Bool`: a false arm and a true arm.
    Bool { false_case: Term, true_case: Term },
    /// Sparse dispatch on specific `Nat` values with a default arm.
    ///
    /// **Keyed by [`Natural`], not by the erased carrier's `u32`.** `Nat` is unbounded here and narrows at the erase boundary, which *refuses* a key it cannot represent rather than wrapping it — see [Numeric carriers narrow by refusing, never by changing a value](../../../documentation/design/toolchain/numeric-carriers-narrow-by-refusing-never-by-changing-a-value.md). A `u32` here wrote `curios-ersd`'s width into the representation a proof is stated over, three stages above the boundary that owns it.
    ///
    /// A sequence rather than a `BTreeMap`, for the reason [`Cases::Induct`]'s arms are one: this enum is archived, and `Natural`'s archived form is its little-endian bytes, whose collation is not its numeric order. Rather than teach the archived form an ordering it does not have, the keys are held **strictly ascending** — the invariant every constructor establishes and every rebuild preserves, and the one that makes term identity independent of the order arms were written in.
    Switch {
        cases: Vec<(Natural, Term)>,
        default: Term,
    },
    /// The intrinsic eliminator of a nominal inductive: one arm per constructor, each arm's arity equal to that constructor's payload arity. `default` is the optional catch-all arm (`| _ =>`, mirroring [`Cases::Switch`]'s): present iff the surface match ended in a bare `_`. It binds nothing and stands in for every constructor tag absent from `cases`; `None` means the arms structurally cover every constructor (a true elimination). The enumerated arms are checked at their own case target indices and the default at the scrutinee's actual ones, so a catch-all is legal on an indexed family too.
    Induct {
        /// The enumerated arms, in the owning inductive's *declaration order* — the same order `InductDecl::constructor_order` reports, which is what makes this a canonical form: two matches whose arms are written in different source order elaborate to the same sequence, so arm order never enters term identity. Elaboration establishes that by building the arms from `constructor_order` rather than from the written order (`elaborate_induct_match`). A subsequence is legal — an arm may be absent under a `default` or a Rung-C prune.
        cases: Vec<(Atom, InductArm)>,
        default: Option<Term>,
    },
    /// Structural induction on a native free-monoid intrinsic (`Nat`/`List`/ `Bin`): the `carrier` selects the intrinsic and carries both its parameters (`List`'s element type) and its two arms — an identity arm plus a cons arm binding the head generator (absent for `Nat`, whose unary generator carries no payload), the tail, and the induction hypothesis at the tail.
    FreeMonoid { carrier: Carrier },
}

/// The native free-monoid intrinsic a `Cases::FreeMonoid` eliminates, with its type parameters and its two eliminator arms. `Nat` is the free monoid on one (payload-less) generator; `Bin` carries none; `List` carries its element type. Each variant pairs an identity arm (`empty_case`) with a cons arm whose arity is fixed by the carrier — `Scope<Two>` for `Nat` (predecessor, ih), `Scope<Three>` for `Bin`/`List` (head, tail, ih).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum Carrier {
    Nat {
        empty_case: Term,
        cons_case: Scope<Two>,
    },
    Bin {
        grain: Grain,
        empty_case: Term,
        cons_case: Scope<Three>,
    },
    List {
        elem: Term,
        empty_case: Term,
        cons_case: Scope<Three>,
    },
}

/// A straight-line block of `let` bindings: `bindings` in written order, then a `tail` continuation in scope of all of them. Binding `i` is stored under the `i` binders before it — its `type_` and `value` may reference bindings `0..i` but never binding `i` itself; a `let` is non-recursive, self- and mutual reference is [`Rec`]'s job. A whole run of source `let`s is one `Let`, not a nest, so every walk over it (`traverse`/`reach`/`reduce`/ `erase`/`elaborate`) is a loop over `bindings` rather than one native stack frame per binding — which is what keeps a long local `let` sequence from overflowing the stack.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Let {
    pub bindings: Vec<LetBinding>,
    pub tail: Scope<Many>,
}

/// One non-recursive local binding: its declared type and its value.
///
/// A local binding is monomorphic. Universe polymorphism is a property of *declarations*, which are frozen into the prelude archive and re-instantiated by later programs; a local binding has no such use sites, and cumulativity already admits the uses a local scheme once served — for `let id : (@A : Type, A) -> A` applied to both `Prop` and `Type 0`, a single `A : Type 1` accepts both, and the level order is linear so a sup always exists.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct LetBinding {
    type_: Term,
    value: Term,
}

impl LetBinding {
    pub fn new(type_: Term, value: Term) -> Self {
        Self { type_, value }
    }

    pub fn type_(&self) -> &Term {
        &self.type_
    }

    pub fn value(&self) -> &Term {
        &self.value
    }

    pub(super) fn into_parts(self) -> (Term, Term) {
        (self.type_, self.value)
    }
}

/// One member of a recursive group as the knot stores it. Both scopes are closed over the whole group, so any member may reference any other.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct RecMemberScopes {
    pub type_: Scope<Many>,
    pub body: Scope<Many>,
}

/// The shared knot of a mutually-recursive group. Every member type and body is scoped over the full group. `Rc` sharing is an implementation detail; equality and hashing remain structural through the scoped items.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct RecGroup {
    scheme: UniverseScheme<Rc<Vec<RecMemberScopes>>>,
}

impl RecGroup {
    pub fn new(items: Vec<RecMemberScopes>) -> Self {
        Self {
            scheme: UniverseScheme::monomorphic(Rc::new(items)),
        }
    }

    /// This group with every member's closed scopes rewritten in place.
    ///
    /// The bodies stay closed throughout. Terms under a scope carry loose de Bruijn indices, and two structurally identical such terms — indices included — denote the same thing at the same depth, so canonicalizing them together is sound without opening.
    pub fn map_members(&self, mut map: impl FnMut(&Term) -> Term) -> Self {
        Self {
            scheme: UniverseScheme {
                context: self.scheme.context.clone(),
                value: Rc::new(
                    self.iter()
                        .map(|member| RecMemberScopes {
                            type_: member.type_.map_body(&mut map),
                            body: member.body.map_body(&mut map),
                        })
                        .collect(),
                ),
            },
        }
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = &RecMemberScopes> + Clone {
        self.scheme.value.iter()
    }

    fn item(&self, index: usize) -> &RecMemberScopes {
        self.scheme
            .value
            .get(index)
            .expect("recursive member index in bounds")
    }

    pub fn universe_context(&self) -> &UniverseContext {
        &self.scheme.context
    }

    /// This group with universe data projected out of every member and its context cleared — the shape under which a generalized group and any instance of it are one group, which is what lets a diagnostic recognize an unfolded `rec` as the definition it came from.
    pub fn projected(&self) -> Self {
        self.map_members(crate::project_erased_universes)
            .with_universe_context(UniverseContext::empty())
    }

    pub fn with_universe_context(mut self, universe_context: UniverseContext) -> Self {
        self.scheme.context = universe_context;
        self
    }

    pub fn length(&self) -> usize {
        self.iter().len()
    }

    /// One term per member, each denoting that member and needing nothing in scope to do it — what every scope in this group opens over.
    pub fn members(&self) -> Vec<Term> {
        (0..self.length())
            .map(|index| Term::rec_proj(self.clone(), index))
            .collect()
    }

    pub fn member_type(&self, index: usize) -> Term {
        let members = self.members();
        let refs = members.iter().collect::<Vec<_>>();
        self.item(index).type_.open(&refs)
    }

    pub fn member_body(&self, index: usize) -> Term {
        let members = self.members();
        let refs = members.iter().collect::<Vec<_>>();
        self.item(index).body.open(&refs)
    }

    /// The universe scheme this group was generalized under — what an instance must satisfy.
    pub fn universes(&self) -> &UniverseContext {
        &self.scheme.context
    }

    pub fn instantiate_universes(&self, arguments: &[Level]) -> Result<Self, UniverseError> {
        if arguments.len() != self.scheme.context.parameter_count {
            return Err(UniverseError::InstanceArity {
                expected: self.scheme.context.parameter_count,
                got: arguments.len(),
            });
        }
        Ok(Self {
            scheme: UniverseScheme {
                value: self
                    .iter()
                    .map(|member| {
                        Ok(RecMemberScopes {
                            type_: member.type_.try_map_body(|body| {
                                instantiate_universe_levels_scoped(body, arguments)
                            })?,
                            body: member.body.try_map_body(|body| {
                                instantiate_universe_levels_scoped(body, arguments)
                            })?,
                        })
                    })
                    .collect::<Result<Vec<_>, UniverseError>>()?
                    .into(),
                context: UniverseContext::empty(),
            },
        })
    }

    pub(super) fn traverse<F>(&self, visit: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        let universe_arity = self.scheme.context.parameter_count;
        visit.enter_universe_scope(universe_arity);
        let mut result = Self::new(
            self.iter()
                .map(|member| RecMemberScopes {
                    type_: visit.visit_scope(&member.type_),
                    body: visit.visit_scope(&member.body),
                })
                .collect(),
        );
        result.scheme.context = if visit.erases_universes() {
            UniverseContext::empty()
        } else {
            self.scheme
                .context
                .map_levels(|level| visit.visit_level(level))
        };
        visit.leave_universe_scope(universe_arity);
        result
    }

    pub(super) fn reach(&self) -> usize {
        self.iter()
            .map(|member| member.type_.reach().max(member.body.reach()))
            .max()
            .unwrap_or(0)
    }
}

/// A block of mutually recursive bindings with a tail in scope of the shared group.
///
/// This is the *only* recursion form. A demanded member occurrence is this same node with a tail that selects one member — see [`Term::rec_proj`] — rather than a form of its own, so the rule that checks a group is the rule that checks an occurrence of it. A self-describing occurrence node was the earlier design, and what it cost is worth recording: a node that is well-formed standing alone is a node no scope gates, and the kernel typed one from the group it carried without ever checking that group.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Rec {
    pub group: RecGroup,
    pub tail: Scope<Many>,
}

impl Rec {
    /// The member index this block's tail selects, when it selects one rather than computing something of its own — see [`Term::rec_proj`].
    pub fn as_proj(&self) -> Option<usize> {
        let Subterm::Var(var) = &**self.tail.body() else {
            return None;
        };

        var.as_bound().filter(|index| *index < self.group.length())
    }
}

/// Provenance of an inserted implicit argument: the applied function (`func`) had no `@`-argument for its implicit binder `binder` at some call site, so the elaborator filled the slot with a fresh metavariable.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct ImplicitOrigin {
    pub func: String,
    pub binder: String,
}

/// Provenance of an inserted witness argument: the applied function (`func`) had no `use`-argument for its witness binder `binder` at some call site, so the elaborator filled the slot with a fresh metavariable and registered a resolution goal for it. An occurrence still unsolved at zonk reports as a missing witness (naming the goal type from the birth record) rather than an uninferred implicit.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct WitnessOrigin {
    pub func: String,
    pub binder: String,
}

/// Provenance of a metavariable — which mechanism minted it, deciding both how zonk reports it unsolved and what an elaboration site may do with it. An unsolved `Implicit`/`Witness` survivor names the binder it filled, an unsolved `Domain` the lambda parameter whose type was never determined, and an unsolved `Hole` is a bare "cannot infer", while a `Goal` is reported unconditionally.
///
/// **A site may special-case a `Hole`; a `Goal` always takes the general path.** Inferring a binding over an elided annotation, synthesizing an elided motive, refusing a lambda whose domain nothing pins — each of those is a decision about a *silent* hole, and each once matched any bare metavariable, which is how a written `?` in those positions was discarded unelaborated and the program compiled with a goal in it. [`Metavar::is_hole`] is the one predicate those sites ask, so the rule is stated in the type rather than re-derived per site.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum MetavarOrigin {
    /// A silent inference hole: an elided annotation, motive, lambda domain or element type (`into_core` mints it via `Term::hole`), or an elaborator placeholder with no provenance of its own — a parked problem's stand-in, a recursive group's slot, the type a goal in synthesis position stands over. Its solution is spliced without comment; unsolved, it is the bare "cannot infer".
    Hole,
    /// A settle-synthesized lambda's unannotated domain, named by its binder: minted when a lambda whose expectation never gained structure is synthesized at a settle tier, replacing the silent hole (which is solved with it). Solved — by the body, or by whatever the settled type later unifies with — it splices silently like a hole; unsolved, zonk reports the parameter whose type was never determined, by name.
    Domain(String),
    Implicit(ImplicitOrigin),
    Witness(WitnessOrigin),
    /// A written goal `?` (`into_core` mints it via `Term::goal`): the user asked what elaboration determines here, so zonk errors with the goal's scope, type, and solution — solved or not — instead of splicing.
    Goal,
}

/// A metavariable's identity: a dense index into the `Context`'s `MetaStore`, minted monotonically by an `Entropy`(Entropy). A newtype so it can never be confused with the other `usize`-shaped notions the kernel juggles (de Bruijn indices, telescope arities, variant tags, `Nat` magnitudes).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord, Hash))]
pub struct MetavarId(pub usize);

impl From<usize> for MetavarId {
    fn from(raw: usize) -> Self {
        Self(raw)
    }
}

impl Mint for MetavarId {
    fn mint(entropy: usize) -> Self {
        Self(entropy)
    }
}

impl fmt::Display for MetavarId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A metavariable: a placeholder term standing for an as-yet-unknown subterm, born from a surface hole `?` and (possibly) solved by unification. The solution, when one exists, lives in the `Context`'s `MetaStore`, keyed by `id`, spelled with the *birth telescope's* free names.
///
/// `origin` rides with the node and says what minted it — a silent hole, an elaborator-inserted implicit/witness argument (zonk's unsolved report then names the binder instead of a bare id), or a written goal `?` (zonk reports it unconditionally). Each id is minted exactly once (`into_core` desugared holes as `Hole` and written goals as `Goal`, core insertions above the floor `into_core` returns with theirs), so every occurrence of an id carries the same origin and the derived equality never splits an id.
///
/// `spine` is the delayed substitution — one term per binder of the birth telescope (`MetaEntry::telescope` order), recording what that binder corresponds to at this occurrence. Identity (`Var::free(name)`) at birth. The entries are ordinary term content: `traverse` walks them, so `close` captures them and `open` substitutes them, and the mapping survives re-closing under fresh names — which is what lets a solution mentioning a sibling binder resolve correctly wherever the occurrence ends up. An empty spine is a not-yet-birthed `into_core` hole and resolves as the identity.
///
/// The spine is `Rc`-shared: every meta born under the same Γ shares one identity-spine allocation (see `Context::identity_snapshot`), which is what keeps minting metavariables O(1) instead of O(|Γ|).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Metavar {
    pub id: MetavarId,
    pub spine: Rc<Vec<Term>>,
    pub origin: MetavarOrigin,
}

impl Metavar {
    /// Whether this is a silent hole — the one origin an elaboration site may special-case (infer over, synthesize for, refuse as unpinned). Every other origin, the written goal above all, takes the site's general path; see [`MetavarOrigin`].
    pub fn is_hole(&self) -> bool {
        matches!(self.origin, MetavarOrigin::Hole)
    }
}

/// An internal, occurrence-specific instantiation of a universe-polymorphic binding: a head at the levels this occurrence chose.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Instance {
    pub head: InstanceHead,
    pub levels: Vec<Level>,
}

/// The two shapes that denote a universe-polymorphic binding: a not-yet-reduced reference to one, or a projection out of a recursive group. Typed rather than held as a general term so an ill-formed head is unrepresentable; the nominal normal forms carry their instance in their own universe vectors instead (see `Term::stamp_declaration_node`).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum InstanceHead {
    Var(Var),
    RecProj(RecGroup, usize),
}

impl InstanceHead {
    /// This head spelled as the term it abbreviates: the variable itself, or the ordinary `Rec` node whose tail selects the member (see [`Term::rec_proj`]). The spelling is span-less; an occurrence's span lives on the wrapping instance term.
    pub fn to_term(&self) -> Term {
        match self {
            InstanceHead::Var(var) => Term::var(var.clone()),
            InstanceHead::RecProj(group, index) => Term::rec_proj(group.clone(), *index),
        }
    }

    /// The free name a variable head references, mirroring `Term::head_name`: a projection head names no free variable, exactly as the `Rec` it abbreviates did.
    pub fn head_name(&self) -> Option<&Free> {
        match self {
            InstanceHead::Var(var) => var.as_free(),
            InstanceHead::RecProj(..) => None,
        }
    }

    /// Classify a term as a head, when it has one of the two head shapes. This is the seam a substitution crosses: replacing a bound variable head with a group projection — the one replacement rec unfolding performs — re-enters the typed representation here.
    pub fn from_subterm(subterm: &Subterm) -> Option<Self> {
        match subterm {
            Subterm::Var(var) => Some(InstanceHead::Var(var.clone())),
            _ => subterm
                .as_rec_proj()
                .map(|(group, index)| InstanceHead::RecProj(group.clone(), index)),
        }
    }
}
