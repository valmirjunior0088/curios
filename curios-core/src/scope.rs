//! De Bruijn machinery for the `core` stage's terms.
//!
//! `Scope`, `Telescope`, `Var`, the `Bound` traversal trait, and the `Visit` driver operate over `core`'s `Term` and `Subterm`. `core` keeps its own `Subterm::traverse` (the big structural match, including its intrinsics) and plugs it into this machinery by implementing `Bound`.

use {
    super::{
        Free, Global, Level, LevelHead, Subterm, Term, UniverseError, UniverseMetaId, UniverseParam,
    },
    curios_utilities::{Span, Symbol},
    std::{
        cell::RefCell,
        collections::{BTreeSet, HashMap, HashSet},
        convert::Infallible,
        fmt,
        hash::Hash,
        mem,
        ops::Deref,
        rc::Rc,
    },
};

mod telescope;
pub use telescope::*;

// === Arity ===================================================================

/// A [`Scope`]'s binder count, lifted to the type level: the fixed arities ([`One`]/[`Two`]/[`Three`]) make `close`/`open` take exactly-sized arrays, so an arity mismatch on the common eliminator shapes is a compile error; [`Many`] defers the check to a runtime assert.
pub trait Arity: Copy {
    /// The parameter-pack shape `close`/`open` accept: a fixed-size array reference for the static arities, a plain slice for [`Many`].
    type Params<'a, T: ?Sized + 'a>: AsRef<[&'a T]>;

    /// The number of binders this arity denotes.
    fn arity(&self) -> usize;
}

/// The static one-binder arity — `let` tails, telescope links, single-scrutinee motives.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct One;

impl One {
    /// The binder count as a constant, so `Params` can be the fixed-size array type `[&T; 1]`.
    pub(crate) const ARITY: usize = 1;
}

impl Arity for One {
    type Params<'a, T: ?Sized + 'a> = &'a [&'a T; Self::ARITY];

    fn arity(&self) -> usize {
        Self::ARITY
    }
}

/// The static two-binder arity — the `(pred, ih)` successor arm of the `Nat` eliminator.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Two;

impl Two {
    /// The binder count as a constant, so `Params` can be the fixed-size array type `[&T; 2]`.
    pub(crate) const ARITY: usize = 2;
}

impl Arity for Two {
    type Params<'a, T: ?Sized + 'a> = &'a [&'a T; Self::ARITY];

    fn arity(&self) -> usize {
        Self::ARITY
    }
}

/// The static three-binder arity — the `(head, tail, ih)` cons arms of the `Bin`/`List` eliminators.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Three;

impl Three {
    /// The binder count as a constant, so `Params` can be the fixed-size array type `[&T; 3]`.
    pub(crate) const ARITY: usize = 3;
}

impl Arity for Three {
    type Params<'a, T: ?Sized + 'a> = &'a [&'a T; Self::ARITY];

    fn arity(&self) -> usize {
        Self::ARITY
    }
}

/// A runtime-chosen binder count, for scopes whose arity is data-dependent (inductive-match arms over constructor payloads, `Rec` blocks, motives). `close`/`open` fall back to slices and assert the length instead of getting it checked at compile time.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Many(pub usize);

impl Arity for Many {
    type Params<'a, T: ?Sized + 'a> = &'a [&'a T];

    fn arity(&self) -> usize {
        self.0
    }
}

// === Var =====================================================================

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
enum VarType {
    Free(Free),
    Bound(usize),
}

/// A locally-nameless variable: free (a [`Free`] identity naming a Γ assumption or global definition) or bound (a de Bruijn index into enclosing [`Scope`]s). The bound form and its accessors are crate-internal — outside code builds free variables and lets the scope machinery convert them.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Var {
    type_: VarType,
}

impl Var {
    /// A free occurrence of `name` — the only form constructible outside the crate; `Scope::close` (via `capture`) is what turns free occurrences into bound indices.
    pub fn free(name: Free) -> Self {
        Self {
            type_: VarType::Free(name),
        }
    }

    /// This occurrence's identity, if it is free.
    pub fn as_free(&self) -> Option<&Free> {
        match &self.type_ {
            VarType::Free(free) => Some(free),
            VarType::Bound(_) => None,
        }
    }

    pub fn bound(index: usize) -> Self {
        Self {
            type_: VarType::Bound(index),
        }
    }

    pub fn as_bound(&self) -> Option<usize> {
        match &self.type_ {
            VarType::Free(_) => None,
            &VarType::Bound(index) => Some(index),
        }
    }

    /// This occurrence's identity, asserting it is free. Callers hold a term the scope machinery has not closed over, where a bound index is an invariant violation rather than a case to handle.
    pub fn unwrap(&self) -> &Free {
        self.as_free().expect("a free occurrence")
    }
}

// === Bound ===================================================================

/// A syntactic category the de Bruijn machinery can operate on: anything that can rebuild itself under a variable-visiting [`Visit`] and report its `reach`. Implemented by `Term`/`Subterm` (the big structural match lives in `term.rs`), [`Telescope`], and `()` (a Σ-telescope's trailing payload); everything else here — `shift`, `capture`, `release`, `free_vars` — is derived from `traverse` alone.
pub trait Bound: Sized + Clone + Eq + Hash + fmt::Debug {
    /// Rebuild the term, invoking the visit callback at every variable with the binder depth it sits under; a `Some(replacement)` substitutes that variable. The single intrinsic the rest of the trait is defined from — implementations must route subterms through `Visit::visit_subterm`/`visit_scope` so depth tracking, pruning, and the rewrite hook fire.
    fn traverse<F>(&self, visit: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>;

    /// Number of outer de Bruijn binders this term depends on: `1 + max escaping bound index`, or `0` if none. A term with `reach <= depth` contains no bound index `>= depth`, so `shift`/`release` at that depth are the identity on it.
    fn reach(&self) -> usize;

    /// Whether an elaboration metavariable occurs in this value.
    fn has_metavar(&self) -> bool;

    /// Whether any elaboration-transient node survives in this value — the sibling of [`Bound::has_metavar`], asked by the same zonk-evidence boundary.
    fn has_transient(&self) -> bool;

    /// `true` iff the term has no loose de Bruijn indices — i.e. it's not floating inside some outer scope.
    fn closed(&self) -> bool {
        self.reach() == 0
    }

    /// De Bruijn weakening: add `amount` to every loose bound index (`>= depth`), making room for that many new enclosing binders when a term is moved under them. Index-monotonic, so the traversal prunes by `reach`, and a node more than one owner holds is shifted once per depth.
    fn shift(&self, amount: usize) -> Self {
        self.traverse(&mut Visit::pruning(|depth, var| {
            var.as_bound()
                .filter(|&index| index >= depth)
                .map(|index| Subterm::Var(Var::bound(index + amount)))
        }))
    }

    /// The closing half of the locally-nameless discipline: turn free occurrences of `binders` into bound indices (position in `binders`, offset by the current depth) while shifting already-loose indices past the new binders. `Scope::close` is this plus the name bookkeeping. Rewrites *free* names, so it can never be pruned by `reach`.
    ///
    /// Memoized on node identity and depth, so a DAG-shaped input — the weak-head form of a web of definitions each naming the one before it twice, whose tree is `2^n` — is captured in its own size: the kernel's conversion history captures every goal it enters, and would capture that web's tree at every one. And where every binder is a local, a subterm with no local free and no loose index to shift is handed back unwalked, since nothing in it is a binder's occurrence: a goal's closed operands are most of it.
    fn capture(&self, binders: &[&Free]) -> Self {
        let local = binders.iter().all(|binder| binder.is_local());
        self.traverse(&mut Visit::capturing(local, |depth, var| {
            var.as_free()
                .and_then(|name| {
                    binders
                        .iter()
                        .position(|&candidate| name == candidate)
                        .map(|index| Subterm::Var(Var::bound(depth + index)))
                })
                .or_else(|| {
                    var.as_bound()
                        .filter(|&index| index >= depth)
                        .map(|index| Subterm::Var(Var::bound(index + binders.len())))
                })
        }))
    }

    /// The opening half of the locally-nameless discipline: substitute the outermost `terms.len()` loose bound indices with `terms` (each shifted by the depth it lands under) and re-tighten the loose indices beyond them. `Scope::open` is this plus the arity check; effects depend only on indices `>= depth`, so the traversal prunes by `reach`, and a node more than one owner holds is released once per depth.
    fn release(&self, terms: &[&Term]) -> Self {
        self.traverse(&mut Visit::pruning(|depth, var| {
            var.as_bound().and_then(|index| {
                index
                    .checked_sub(depth)
                    .map(|delta| match delta < terms.len() {
                        true => terms[delta].deref().shift(depth),
                        false => Subterm::Var(Var::bound(index - terms.len())),
                    })
            })
        }))
    }

    /// The set of free-variable identities occurring anywhere in the term. A pure observation ridden on `traverse` (the callback rewrites nothing), so it must never be pruned — every node has to be seen.
    fn free_vars(&self) -> BTreeSet<Free> {
        let mut vars = BTreeSet::new();
        self.traverse(&mut Visit::new(|_, var| {
            if let Some(name) = var.as_free() {
                vars.insert(*name);
            }
            None
        }));
        vars
    }
}

/// A constructor's index targets, which is all its signature's terminal carries: the family and its parameters are fixed by the declaration, so nothing else in a terminal is information.
impl Bound for Vec<Term> {
    fn traverse<F>(&self, visit: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        self.iter().map(|target| target.traverse(visit)).collect()
    }

    fn reach(&self) -> usize {
        self.iter().map(Bound::reach).max().unwrap_or(0)
    }

    fn has_metavar(&self) -> bool {
        self.iter().any(Bound::has_metavar)
    }

    fn has_transient(&self) -> bool {
        self.iter().any(Bound::has_transient)
    }
}

impl Bound for () {
    fn traverse<F>(&self, _: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
    }

    fn reach(&self) -> usize {
        0
    }

    fn has_metavar(&self) -> bool {
        false
    }

    fn has_transient(&self) -> bool {
        false
    }
}

/// [`rewrite_universe_levels_scoped_shared`] for a hook that reads no depth.
pub(crate) fn rewrite_universe_levels<B: Bound, E: 'static>(
    value: &B,
    rewrite: impl FnMut(&Level) -> Result<Level, E> + 'static,
) -> Result<B, E> {
    let mut rewrite = rewrite;
    rewrite_universe_levels_scoped_shared(value, move |_, level| rewrite(level))
}

/// Structural implementation of universe erasure: nominal vectors, instances, and contexts are removed by their owning nodes. `Type` must still carry a `Level` in Core, so its now-irrelevant payload is rebuilt with Core's private canonical ground representative. It is read two ways. As a projection into a world where levels are irrelevant — the Core-to-Ersd lowering, and goal-report display, since the surface language has no spelling for an instance — it is exact. As an equality key it is a quotient coarser than definitional equality, identifying `Type 0` with `Type 1`; that reading is sound only over `Nat` summands, where no level can reach a number, and `documentation/design/soundness/case-equations-and-their-key.md` records the route it admits anywhere else.
pub fn project_erased_universes<B: Bound>(value: &B) -> B {
    value.traverse(&mut Visit::erasing_universes(|_, _| None))
}

/// Every level `value` carries through `rewrite`, at the universe binder depth it stands at, once per occurrence and in walk order: for a hook whose answer is the occurrences themselves, as the level sequence two spellings are aligned by is. The first error `rewrite` answers is the result, and no level is asked after it.
pub fn rewrite_universe_levels_scoped<B: Bound, E: 'static>(
    value: &B,
    rewrite: impl FnMut(usize, &Level) -> Result<Level, E> + 'static,
) -> Result<B, E> {
    rewrite_levels_through(value, rewrite, Visit::rewriting_levels_scoped)
}

/// [`rewrite_universe_levels_scoped`], once per node and depth rather than once per occurrence: for a hook whose answer is a function of the level and the depth — a substitution, an instantiation, a check — or whose effect is idempotent. Every such caller walks what finalization and certification walk, and a solution stored as a reduct is a graph whose tree can be exponential in its depth. The first error is the same as the per-occurrence walk's, since skipping a revisit leaves the order first occurrences are met in unchanged.
pub fn rewrite_universe_levels_scoped_shared<B: Bound, E: 'static>(
    value: &B,
    rewrite: impl FnMut(usize, &Level) -> Result<Level, E> + 'static,
) -> Result<B, E> {
    rewrite_levels_through(value, rewrite, Visit::rewriting_levels_scoped_shared)
}

/// The variable callback of a walk that rewrites levels and leaves every variable as it is.
type Unchanged = fn(usize, &Var) -> Option<Subterm>;

/// The level walk both entry points share, driven by the visit `visit` builds.
fn rewrite_levels_through<B: Bound, E: 'static>(
    value: &B,
    rewrite: impl FnMut(usize, &Level) -> Result<Level, E> + 'static,
    visit: fn(Unchanged, LevelRewrite) -> Visit<Unchanged>,
) -> Result<B, E> {
    let rewrite = Rc::new(RefCell::new(rewrite));
    let error = Rc::new(RefCell::new(None));
    let rewrite_for_visit = Rc::clone(&rewrite);
    let error_for_visit = Rc::clone(&error);
    let mut visit = visit(
        |_, _| None,
        Box::new(move |depth, level| {
            if error_for_visit.borrow().is_some() {
                return level.clone();
            }
            match rewrite_for_visit.borrow_mut()(depth, level) {
                Ok(level) => level,
                Err(found) => {
                    *error_for_visit.borrow_mut() = Some(found);
                    level.clone()
                }
            }
        }),
    );
    let rewritten = value.traverse(&mut visit);
    match error.borrow_mut().take() {
        Some(error) => Err(error),
        None => Ok(rewritten),
    }
}

pub fn shift_universe_params(level: &Level, amount: usize) -> Result<Level, UniverseError> {
    level.substitute(|head| match head {
        LevelHead::Param(UniverseParam(index)) => index
            .checked_add(amount)
            .map(UniverseParam)
            .map(Level::param),
        LevelHead::Meta(_) => None,
    })
}

/// Substitute a scheme's own universe parameters by `arguments`.
///
/// Universe parameters are innermost-first: beneath the `depth` universe binders this walk has crossed, the scheme's own parameters occupy `depth .. depth + arguments.len()`. An index above that range belongs to an *enclosing* scheme and is shifted down by the parameters this instantiation discharges, exactly as `curios-elab`'s `UniverseSolver::instantiate_at` rewrites the outer references in a nested residual context.
///
/// Instance arity is the owning `UniverseContext`'s contract and is checked against its declared `parameter_count`. Rejecting an out-of-range index here instead would misread every legitimate outer-scheme reference as a missing argument.
pub fn instantiate_universe_levels_scoped<B: Bound>(
    value: &B,
    arguments: &[Level],
) -> Result<B, UniverseError> {
    let arguments = arguments.to_vec();
    rewrite_universe_levels_scoped_shared(value, move |depth, level| {
        let arguments = arguments
            .iter()
            .map(|argument| shift_universe_params(argument, depth))
            .collect::<Result<Vec<_>, _>>()?;
        level.substitute(|head| match head {
            LevelHead::Param(UniverseParam(index)) if index < depth => None,
            LevelHead::Param(UniverseParam(index)) => Some(
                arguments
                    .get(index - depth)
                    .cloned()
                    .unwrap_or_else(|| Level::param(UniverseParam(index - arguments.len()))),
            ),
            LevelHead::Meta(_) => None,
        })
    })
}

pub fn universe_metas<B: Bound>(value: &B) -> BTreeSet<UniverseMetaId> {
    let metas = Rc::new(RefCell::new(BTreeSet::new()));
    let found = Rc::clone(&metas);
    let _: Result<_, Infallible> = rewrite_universe_levels(value, move |level| {
        found.borrow_mut().extend(level.metas());
        Ok(level.clone())
    });
    Rc::try_unwrap(metas)
        .expect("the universe collector releases its traversal closure")
        .into_inner()
}

/// The universe parameters `value` mentions that no universe binder inside it binds, each as its own scheme's index: a parameter met beneath `depth` binders is the scheme's own from `depth` up, and is reported shifted back.
pub fn universe_params<B: Bound>(value: &B) -> BTreeSet<UniverseParam> {
    let params = Rc::new(RefCell::new(BTreeSet::new()));
    let found = Rc::clone(&params);
    let _: Result<_, Infallible> =
        rewrite_universe_levels_scoped_shared(value, move |depth, level| {
            found.borrow_mut().extend(
                level
                    .params()
                    .filter(|param| param.0 >= depth)
                    .map(|param| UniverseParam(param.0 - depth)),
            );
            Ok(level.clone())
        });
    Rc::try_unwrap(params)
        .expect("the universe collector releases its traversal closure")
        .into_inner()
}

/// How a declaration's own name reaches the value being stamped.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SelfReference {
    /// Still a free variable. Nothing else will supply the instance, so the occurrence must carry one explicitly: a later use site instantiates the stored scheme by substituting the declaration's universe parameters, and a bare variable has none to substitute.
    Free,
    /// Already captured by an enclosing `RecGroup`'s binder, which instantiates its own members through `RecGroup::instantiate_universes`. An explicit instance here would be applied a second time when the group is opened, against a group whose parameters that first instantiation already consumed.
    Bound,
}

/// Rewrite every occurrence of a declaration group's own members to denote the universe instance `levels`.
///
/// A declaration's signature, body, and registry telescopes are elaborated before its universe parameters exist: within its own group it is monomorphic, so its self-references carry no instance at all. Finalization mints the parameters, and every internal occurrence must then denote *that* instance rather than a freshly instantiated one — the concrete form of the rule that recursion is monomorphic inside a group.
///
/// Nominal normal forms always carry the instance in their own universe vector. Variable occurrences carry it only when they are still [`SelfReference::Free`].
///
/// The per-node rule is `Term::stamp_declaration_node`; this driver only carries it through the binders and telescopes an arbitrary [`Bound`] holds. An empty instance is the identity, so a monomorphic declaration pays a single comparison rather than a traversal.
pub fn stamp_declaration_instance<B: Bound>(
    value: &B,
    names: &BTreeSet<Global>,
    self_reference: SelfReference,
    levels: &[Level],
) -> B {
    if names.is_empty() {
        return value.clone();
    }
    let names = names.clone();
    let levels = levels.to_vec();
    let mut visit = Visit::rewriting_shared(
        |_, _| None,
        Box::new(move |_, term| term.stamp_declaration_node(&names, self_reference, &levels)),
    );
    value.traverse(&mut visit)
}

// === Scope ===================================================================

/// What a scope remembers of one binder it closed over: a global's name, which means the same in every compilation, or a local's display hint.
///
/// **Never a local's identity.** That is minted by the compilation that closed the scope, and a stored scope keeping it would carry a position into every compilation that restored it. A printer reopening the scope identifies the binder by where the render meets it and by this hint. A written binder's place among its declaration's written binders is kept beside the hint — a function of that declaration's own text, not a counter any compilation shares — so a local opened here can be traced to the binder a lint names.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub(crate) enum Label {
    /// A global a `rec` group's binder closes over, rendered as its path as a free occurrence of it would be.
    Global(Global),
    /// A local binder: its display hint, `None` where it was minted hintless, and where it sits among its declaration's written binders, when the lowering wrote it.
    Local {
        hint: Option<Symbol>,
        written: Option<u32>,
    },
}

impl Label {
    /// What a scope closing over `binder` remembers of it.
    fn of(binder: Free) -> Self {
        match binder {
            Free::Global(global) => Label::Global(global),
            Free::Local(mint) => Label::Local {
                hint: mint.hint_symbol(),
                written: mint.written(),
            },
        }
    }

    /// The same label rendering as `hint`. A global has no hint to replace — its rendering is its path — so it is returned unchanged.
    ///
    /// The empty hint is *no* hint, and restoring it as one is not the same thing. [`Telescope::labels`](crate::Telescope::labels) renders a hintless binder as `""` — the convention a positional field is compared under — so a rebuild that relabels from those labels would otherwise hand every unlabeled position a hint that is present but says nothing. A printer then reads "present" as "labeled" and a rename map disambiguates the shared spelling into `2`, `3`, turning `{Nat, Bool, Str}` into `{: Nat, 2: Bool, 3: Str}` in every report that names one.
    fn relabelled(&self, hint: &str) -> Self {
        match self {
            Label::Local { written, .. } => Label::Local {
                hint: (!hint.is_empty()).then(|| Symbol::new(hint)),
                written: *written,
            },
            Label::Global(_) => *self,
        }
    }

    /// The hint a local binder was written with; `None` for a global, whose rendering is its path, and for a hintless local.
    pub(crate) fn hint(&self) -> Option<&'static str> {
        match self {
            Label::Local { hint, .. } => hint.map(|hint| hint.as_str()),
            Label::Global(_) => None,
        }
    }

    /// Where a local binder sits among its declaration's written binders, when the lowering wrote it.
    pub(crate) fn written(&self) -> Option<u32> {
        match self {
            Label::Local { written, .. } => *written,
            Label::Global(_) => None,
        }
    }
}

/// A body abstracted over `A::arity()` binders, locally nameless: the body stores de Bruijn indices, while `labels` remembers a `Label` per binder it was closed over, for printing and for the hints later rebuilds re-mint from (`None` for a `constant` scope that never had binders written). Like a [`Term`]'s span, `labels` is irrelevant to identity: `Eq`/`Hash` compare arity and body only, so scopes differing solely in binder labels are equal — term equality is α-equivalence. The one place binder *hints* are semantic rather than decoration — tuple-type fields, the target of `.label` resolution — reasserts them in its own node identity (see `TupleType` in `term.rs`). Built by `close` (which captures free occurrences of those identities) and eliminated by `open` (which substitutes terms for the indices); entering a `Scope` is the only place a [`Visit`]'s depth changes, so this type is the unit of binding for the whole crate.
#[curios_archive::archived]
pub struct Scope<A: Arity, B: Bound = Term> {
    arity: A,
    labels: Option<Vec<Label>>,
    body: Box<B>,
}

impl<A: Arity, B: Bound> Scope<A, B> {
    pub fn close<'a>(arity: A, binders: A::Params<'a, Free>, body: B) -> Self {
        assert!(
            arity.arity() == binders.as_ref().len(),
            "scope arity mismatch in `close`: expected {}, got {}",
            arity.arity(),
            binders.as_ref().len()
        );

        Self {
            arity,
            labels: Some(
                binders
                    .as_ref()
                    .iter()
                    .map(|&&name| Label::of(name))
                    .collect(),
            ),
            body: body.capture(binders.as_ref()).into(),
        }
    }

    pub fn arity(&self) -> usize {
        self.arity.arity()
    }

    pub fn body(&self) -> &B {
        &self.body
    }

    pub(crate) fn reach(&self) -> usize {
        self.body.reach().saturating_sub(self.arity())
    }

    pub fn open<'a>(&self, terms: A::Params<'a, Term>) -> B {
        assert!(
            self.arity() == terms.as_ref().len(),
            "scope arity mismatch in `open`: expected {}, got {}",
            self.arity(),
            terms.as_ref().len()
        );

        self.body.release(terms.as_ref())
    }

    pub fn constant(arity: A, body: B) -> Self {
        Self {
            arity,
            labels: None,
            body: body.into(),
        }
    }

    /// Rebuild this scope with `f` applied to its body, preserving arity and binder names. The body keeps its de Bruijn structure, so `f` must be a transformation that does not disturb loose indices — e.g. zonking, which only replaces closed metavariable nodes by closed solutions, or a canonicalization, which replaces a node by a structurally equal one.
    ///
    /// Rewriting here rather than opening and re-closing is what keeps a term's memoized derivations: `open` and `close` each rebuild every node they touch, so the round trip discards all of them to arrive where this arrives without moving.
    pub(crate) fn map_body(&self, f: impl FnOnce(&B) -> B) -> Self {
        Self {
            arity: self.arity,
            labels: self.labels.clone(),
            body: f(&self.body).into(),
        }
    }

    /// Fallible `Self::map_body`, for a rewrite that can reject its input.
    pub fn try_map_body<E>(&self, f: impl FnOnce(&B) -> Result<B, E>) -> Result<Self, E> {
        Ok(Self {
            arity: self.arity,
            labels: self.labels.clone(),
            body: f(&self.body)?.into(),
        })
    }

    /// Whether this scope binds under the names `other` binds under: what tells two texts apart where the two scopes are one as terms, since a scope's equality reads no label.
    pub fn spelled_as<C: Bound>(&self, other: &Scope<A, C>) -> bool {
        self.labels == other.labels
    }

    /// What this scope remembers of the binder at position `index` (0 = first/outermost), for a printer reopening it.
    pub(crate) fn label(&self, index: usize) -> Option<&Label> {
        self.labels.as_deref()?.get(index)
    }

    /// What the binder at position `index` was called where it was written — a rendering aid a rebuild carries onto the binder it re-mints, never a way to recognize which binder this is.
    pub fn hint(&self, index: usize) -> Option<&'static str> {
        self.label(index)?.hint()
    }

    /// Where the binder at position `index` sits among its declaration's written binders, when the lowering wrote it: what the elaborator records of the local it opens from it, so a proof reading the local credits the binder a lint names.
    pub fn written(&self, index: usize) -> Option<u32> {
        self.label(index)?.written()
    }

    pub fn first_hint(&self) -> Option<&'static str> {
        self.hint(0)
    }

    pub fn second_hint(&self) -> Option<&'static str> {
        self.hint(1)
    }

    pub fn third_hint(&self) -> Option<&'static str> {
        self.hint(2)
    }

    pub fn hint_iter(&self) -> impl Iterator<Item = Option<&'static str>> {
        (0..self.arity()).map(move |index| self.hint(index))
    }

    /// What this scope remembers of each binder in order, `None` where the scope was built without them (`constant`).
    pub(crate) fn label_iter(&self) -> impl Iterator<Item = Option<&Label>> {
        (0..self.arity()).map(move |index| self.label(index))
    }

    /// Whether the binder at position `index` (0 = first/outermost label) is referenced anywhere in the body. A bound var refers to this binder iff its de Bruijn index equals `index` plus the number of binders entered since — which `Visit` tracks as `depth`. Used by erasure to spot an eliminator whose induction hypothesis is dead: that arm is a case-split, not a fold.
    ///
    /// A read riding on a pruning visit: a subterm whose `reach` is within the depth it stands at holds no index that could be this binder's, the closed type a `let` states among them, and a node more than one owner holds is read once per depth. Rebuilding every occurrence to read it cost a shared body its tree.
    pub fn uses(&self, index: usize) -> bool {
        let mut used = false;
        self.body.traverse(&mut Visit::pruning(|depth, var: &Var| {
            if var.as_bound() == Some(index + depth) {
                used = true;
            }
            None
        }));
        used
    }
}

impl<B: Bound> Scope<Many, B> {
    /// Prepend `binders` to the front of this scope, outermost first: `binders[0]` becomes index 0 and every existing binder shifts up by their count.
    ///
    /// Done by one direct `capture` on the body — free occurrences of the binders bind to the new leading indices while every existing bound index shifts past them — rather than an open/close round-trip through names, which would have to reopen inner binders into free occurrences and could not tell them from genuine outer references. Taking the whole list at once is what lets a block of `k` bindings prepend in one walk rather than `k`.
    pub fn prepend(&self, binders: &[&Free]) -> Self {
        let labels = self.labels.as_ref().map(|labels| {
            binders
                .iter()
                .map(|&&binder| Label::of(binder))
                .chain(labels.iter().copied())
                .collect()
        });

        Self {
            arity: Many(self.arity() + binders.len()),
            labels,
            body: self.body.capture(binders).into(),
        }
    }
}

impl<A: Arity + fmt::Debug, B: Bound> fmt::Debug for Scope<A, B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Scope")
            .field("arity", &self.arity)
            .field("labels", &self.labels)
            .field("body", &self.body)
            .finish()
    }
}

impl<A: Arity + Clone, B: Bound> Clone for Scope<A, B> {
    fn clone(&self) -> Self {
        Self {
            arity: self.arity,
            labels: self.labels.clone(),
            body: self.body.clone(),
        }
    }
}

impl<A: Arity + PartialEq, B: Bound> PartialEq for Scope<A, B> {
    fn eq(&self, other: &Self) -> bool {
        self.arity == other.arity && self.body == other.body
    }
}

impl<A: Arity + Eq, B: Bound> Eq for Scope<A, B> {}

impl<A: Arity + Hash, B: Bound> Hash for Scope<A, B> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.arity.hash(state);
        self.body.hash(state);
    }
}

// === Visit ===================================================================

/// What tells a node from another of its structure: the names its scopes bind under, in the order a traversal crosses them, and the very nodes it holds.
///
/// A term's equality is α-equivalence and reads no label, so two nodes equal as terms may be two spellings; and it reads through a child to its structure, so two nodes equal as terms may hold two allocations of one child. Neither pair is one stored node.
#[derive(Debug, Default, PartialEq, Eq, Hash)]
struct Spelling {
    labels: Vec<Option<Vec<Label>>>,
    children: Vec<usize>,
}

/// A hash-consing table, shared rather than owned so one canonicalization spans a whole module: two definitions that build the same type collapse onto one node only if they consult the same table.
///
/// **One node for each structure as it is spelled, and no position on any.** A node is adopted unless the table holds one of its structure, under the same binder names, over the very same children; children are canonical before their parent is asked for, so the test is one node deep. Up to α it would hand `pow(base: Nat, exp: Nat)` the node of `min(a: Nat, b: Nat)`, and every report of `pow`'s type would name `a` and `b`. A canonical node sits under no span and holds its children under none: a position is an occurrence's, a canonical node is every occurrence's, and what is consed says what it means and not where it was written.
#[derive(Debug, Clone, Default)]
pub struct Sharing {
    table: Rc<RefCell<HashSet<(Spelling, Term)>>>,
}

impl Sharing {
    pub fn new() -> Self {
        Self::default()
    }

    /// `value` with every term in it replaced by the canonical node of its spelling, under no span.
    ///
    /// One `Sharing` must span every term canonicalized together: the duplication worth collapsing is overwhelmingly *between* definitions, so a table per term would collapse almost none of it.
    pub fn share<B: Bound>(&self, value: &B) -> B {
        value.traverse(&mut Visit::sharing(|_, _| None, self.clone()))
    }

    /// Distinct nodes adopted so far — the census this pass is justified by.
    pub fn structures(&self) -> usize {
        self.table.borrow().len()
    }

    /// The canonical node for `fresh`, a node just built over canonical children: the one adopted under its spelling, or itself.
    fn adopt(&self, spelling: Spelling, fresh: Term) -> Term {
        let mut table = self.table.borrow_mut();
        let key = (spelling, fresh);
        match table.get(&key) {
            Some((_, canonical)) => canonical.clone(),
            None => {
                let canonical = key.1.clone();
                table.insert(key);
                canonical
            }
        }
    }
}

/// A term-level pre-hook for [`Visit`]: `Some(replacement)` substitutes the whole node at the current depth.
type Rewrite = Box<dyn FnMut(usize, &Term) -> Option<Term>>;
type LevelRewrite = Box<dyn FnMut(usize, &Level) -> Level>;

/// The traversal driver threaded through [`Bound::traverse`]: it owns the current binder depth (bumped and restored by `visit_scope` as scopes are crossed), the variable callback, and what the traversal does beyond rewriting variables (`Mode`).
pub struct Visit<F> {
    term_depth: usize,
    universe_depth: usize,
    visit: F,
    mode: Mode,
    memo: Memo,
}

/// What one node of [`Term::level_differences`]'s walk stood down: its children and its own levels, each with the universe-binder depth it sits at below the node, in traversal order.
#[derive(Default)]
pub(crate) struct MaskedLevels {
    pub(crate) children: Vec<(usize, Term)>,
    pub(crate) levels: Vec<(usize, Level)>,
}

/// Whether a traversal remembers what it rebuilt, and under what key.
///
/// **Orthogonal to [`Mode`], and stated separately because it is.** Doubling variants — `Plain` beside `PlainSharedAtDepth`, `Rewriting` beside `RewritingShared` — would make a walk's memo a property of *which mode it picked* rather than a decision its author made, and a mode left without a memoized twin a `2^n` walk waiting to be found, as the machine's forced recursive call and the universe-erased projection a `Nat` comparison takes would be.
///
/// **The law.** Within one pass over an immutable DAG a node's answer is determined by the node and by whatever else the visit is parameterised on, so a revisit of the same key may be skipped. A reduct is a DAG whose *tree* expansion doubles per level — one substitution landing a term in two positions is enough — so a walk that rebuilds per occurrence is exponential in a term the node count, and therefore the unit budget, reads as linear.
///
/// **When it is legal.** [`Memo::ByNode`] needs the variable callback and the rewrite hook pure in the node; [`Memo::ByNodeAndDepth`] needs them pure in the node and the depth. Purity is not the whole condition: a hook with an *effect* may still memoize when the effect is idempotent — a set insert, a first-error latch — and may not when it is not. Three hooks serve operands by position (an `index` or an iterator) and one pushes into a `Vec`; for those the answer differs per occurrence and [`Memo::None`] is the only correct choice.
///
/// Every constructor takes one explicitly. There is no default, deliberately: neither defaulting direction is safe — a wrong `None` costs an exponent and a wrong memo costs a wrong answer.
///
/// **A span is an occurrence's, not a node's.** It sits on the `Term` wrapper, and one node is shared under wrappers spanning different text — the elaborator's cache hands every occurrence of a subterm one node and stamps each with its own span. An unmemoized rebuild keeps each occurrence's span; a hit that handed back the stored rebuild as it was would give every later occurrence the first one's, so a diagnostic at the second occurrence of a shared subterm would point at the first. So an entry keeps the span of the occurrence that filled it ([`Remembered`]), and a hit whose rebuild carries that span — a rebuild, or a hook's answer spanned by the occurrence it replaced — takes the asking occurrence's instead. A hook's own replacement, spanned by something else, is handed back as it was, which is what rebuilding the occurrence would have produced.
enum Memo {
    /// Rebuild every occurrence. Correct for a hook whose answer depends on how many times it has run.
    None,
    /// Keyed on input node identity. Addresses are stable for the traversal because the caller's value holds every node alive.
    ByNode(HashMap<usize, Remembered>),
    /// [`Memo::ByNodeAndDepth`] for the nodes more than one owner holds, and nothing for the rest: `shift` and `release` run at every β step over terms a few nodes large, where a table entry per node would cost more than the walk, and a node is met twice only where two owners hold it. Lean's kernel keeps the same rule in the `replace` its substitution and lifting go through — a cache of the shared subterms, by offset.
    SharedByNodeAndDepth(HashMap<(usize, usize, usize), Remembered>),
    /// Keyed on input node identity *and* both binder depths, for a visit whose effect depends on the depth it runs at — `capture` is the case, where a depth-blind memo would hand a second occurrence the wrong indices. The universe binder depth is in the key beside the term's because a level rewrite reads it: a node under a `rec` group's universe context sits one universe binder deeper at the same term depth.
    ByNodeAndDepth(HashMap<(usize, usize, usize), Remembered>),
}

/// One memo entry: a node's rebuild, and the span of the occurrence whose visit filled it — see [`Memo`] for why a hit needs the second.
struct Remembered {
    filled_at: Option<Span>,
    rebuilt: Term,
}

impl Remembered {
    /// The rebuild as `at`'s own: respanned when it carries the span of the occurrence that filled it, as it was otherwise.
    fn for_occurrence(&self, at: &Term) -> Term {
        match self.rebuilt.span() == self.filled_at {
            true => self.rebuilt.clone().respanned(at.span()),
            false => self.rebuilt.clone(),
        }
    }
}

/// A memo keyed by node identity for a walk written by hand rather than driven by a [`Visit`] — the elaborator's strict zonk is one — under the visit memo's law and its span rule: a hit whose rebuild carries the span of the occurrence that filled it takes the asking occurrence's.
///
/// **It holds every key it was filled from**, where a visit's memo holds none. A visit keys only the nodes of the value it walks, which that value keeps alive; a hand-written walk may key a term it built itself, and a key freed while the memo lives is an address the allocator can hand to another node, whose lookup would then answer with the first one's rebuild.
#[derive(Default)]
pub struct NodeMemo {
    entries: HashMap<usize, (Term, Remembered)>,
}

impl NodeMemo {
    /// The rebuild remembered for `at`'s node, as `at`'s own.
    pub fn get(&self, at: &Term) -> Option<Term> {
        let (_, remembered) = self.entries.get(&at.identity())?;

        Some(remembered.for_occurrence(at))
    }

    /// Remember `rebuilt` as the rebuild of `at`'s node, filled by the occurrence `at`.
    pub fn put(&mut self, at: &Term, rebuilt: Term) {
        let remembered = Remembered {
            filled_at: at.span(),
            rebuilt,
        };
        self.entries.insert(at.identity(), (at.clone(), remembered));
    }
}

/// What a traversal does beyond rewriting variables.
///
/// A closed set, stated as a sum rather than as independent fields — a `prune` flag, optional hooks, more flags — of which only the combinations below mean anything, and which every consumer would test one at a time to learn which combination it was looking at.
///
/// Naming the combinations makes adding one a change the compiler checks: every `match` below stops compiling until the new case has been decided. Closed on purpose — a traversal mode is compiler-internal vocabulary, and all of its construction sites live in this crate.
enum Mode {
    /// Rebuild every node, rewriting variables only.
    Plain,
    /// Skip subtrees whose `reach` proves no loose index can be touched.
    Pruning,
    /// [`Mode::Plain`] for a capture of local binders: skip subtrees with no local free and no loose index the capture would shift.
    Capturing,
    /// A term-level pre-hook substitutes whole nodes before descending. A substituted node is not descended into.
    Rewriting(Rewrite),
    /// [`Mode::Rewriting`], visiting only nodes that carry universe data.
    RewritingUniverses(Rewrite),
    /// A level-level hook, visiting only nodes that carry universe data.
    RewritingLevels(LevelRewrite),
    /// [`Mode::Masking`] that also stands every level of the node itself down to a sentinel, keeping what it removed: the children each with the universe-binder depth it sits at below the node, and the levels each with theirs. Every node is visited, a ground `Type 0` included — a comparison that aligns levels by position has to see the one that carries no universe data. This is one node of [`Term::level_differences`]'s walk over a pair.
    MaskingLevels {
        placeholder: Term,
        children: Vec<(usize, Term)>,
        levels: Vec<(usize, Level)>,
    },
    /// Replace every level with the ground representative, visiting only nodes that carry universe data.
    ErasingUniverses,
    /// Hash-consing: every term comes back as the canonical node of its spelling. `crossed` holds, for each node being rebuilt, the labels of the scopes crossed so far beneath it and above its children. Pairs with [`Memo::ByNode`], which is what keeps the input's sharing as well as the output's.
    Sharing {
        table: Sharing,
        crossed: Vec<Vec<Option<Vec<Label>>>>,
    },
    /// Stand every child term down to `placeholder`, keeping the ones removed in `children`. Because a substituted node is never descended into, the rebuilt node carries this level's own payload and nothing below it — which is what lets [`Term`]'s equality compare one node at a time instead of recursing to the bottom of the term.
    ///
    /// The removed children and the node they came out of are produced by the same pass, so the two can never disagree about what a child is.
    Masking {
        placeholder: Term,
        children: Vec<Term>,
        /// The labels of the scopes the node itself binds through, for a comparison that reads them ([`Visit::masking_labels`]); `None` for one that does not, a term's own equality among them, which is what keeps a label's clone off its path.
        labels: Option<Vec<Option<Vec<Label>>>>,
    },
}

impl<F> Visit<F>
where
    F: FnMut(usize, &Var) -> Option<Subterm>,
{
    pub(crate) fn new(visit: F) -> Self {
        Self {
            term_depth: 0,
            universe_depth: 0,
            visit,
            mode: Mode::Plain,
            memo: Memo::None,
        }
    }

    /// `capture`'s visit: memoized on node identity and binder depth together, since its effect depends on the depth and a depth-blind memo would hand a second occurrence the wrong indices — see [`Memo`] for when that is legal. `local` says every binder being captured is a local, which is what lets it pass over a subterm that has none free.
    fn capturing(local: bool, visit: F) -> Self {
        Self {
            term_depth: 0,
            universe_depth: 0,
            visit,
            mode: match local {
                true => Mode::Capturing,
                false => Mode::Plain,
            },
            memo: Memo::ByNodeAndDepth(HashMap::new()),
        }
    }

    /// Like `new`, but lets a `Term::traverse` impl skip (and structurally share) subtrees the visit provably leaves unchanged. Only sound for index-monotonic visits whose effect depends solely on bound indices `>= depth` — i.e. `shift` and `release`. Must NOT be used for `capture` (rewrites free names) or `free_vars` (must observe every node).
    fn pruning(visit: F) -> Self {
        Self {
            term_depth: 0,
            universe_depth: 0,
            visit,
            mode: Mode::Pruning,
            // `shift` and `release` are pure in the node and the depth, and the read `Scope::uses` rides on them latches, so a depth-keyed memo is legal. A closed subterm never reaches it — `reach` answers that one first — so it is asked only over an *open* shared term: a body under its binders that holds one node twice, which each walked once per path without it.
            memo: Memo::SharedByNodeAndDepth(HashMap::new()),
        }
    }

    /// Like `new`, additionally carrying a term-level rewrite hook fired at every [`Term::traverse`] entry, including terms that are the direct body of a scope or telescope terminal.
    pub fn rewriting(visit: F, rewrite: Rewrite) -> Self {
        Self {
            term_depth: 0,
            universe_depth: 0,
            visit,
            mode: Mode::Rewriting(rewrite),
            // The unmemoized rewrite. Four of its callers serve operands by position and one pushes into a `Vec`, so their answers differ per occurrence — see [`Memo`].
            memo: Memo::None,
        }
    }

    /// Stand every child term down to `placeholder`, keeping what was removed for [`Visit::take_masked_children`]. One visit masks any number of nodes: the placeholder is built once, and the children are taken between nodes.
    pub fn masking(visit: F, placeholder: Term) -> Self {
        Self {
            term_depth: 0,
            universe_depth: 0,
            visit,
            mode: Mode::Masking {
                placeholder,
                children: Vec::new(),
                labels: None,
            },
            // Masking never descends past one level, so there is nothing to revisit.
            memo: Memo::None,
        }
    }

    /// [`Visit::masking`], also keeping the labels of the scopes each node binds through for [`Visit::take_masked_labels`]: what a comparison of two texts reads, where a comparison of two terms does not.
    pub(crate) fn masking_labels(visit: F, placeholder: Term) -> Self {
        Self {
            term_depth: 0,
            universe_depth: 0,
            visit,
            mode: Mode::Masking {
                placeholder,
                children: Vec::new(),
                labels: Some(Vec::new()),
            },
            memo: Memo::None,
        }
    }

    /// Like [`rewriting`](Self::rewriting), but memoized on node identity, so a structurally shared input stays shared in the output instead of being expanded into a tree.
    ///
    /// A rebuilt node is a fresh allocation, so an unmemoized rewrite of a DAG materializes its expansion: a lowered string literal shares one scan-state chain across every `more` link, and rebuilding it unshared costs O(n^2) nodes for an n-byte literal — which then makes every later pass over the term quadratic too.
    ///
    /// Only sound when the hook and the variable callback are pure and depend on the node alone — not on binder depth, and not on how many times they have run. A memoized visit skips both, so a depth-sensitive rewrite would silently reuse a result computed at the wrong depth, and a stateful hook would see each shared node once rather than once per occurrence.
    pub fn rewriting_shared(visit: F, rewrite: Rewrite) -> Self {
        Self {
            term_depth: 0,
            universe_depth: 0,
            visit,
            mode: Mode::Rewriting(rewrite),
            memo: Memo::ByNode(HashMap::new()),
        }
    }

    /// [`rewriting_shared`](Self::rewriting_shared), visiting only nodes that carry universe data. Memoized on node identity, so it is sound for a hook [`rewriting_shared`](Self::rewriting_shared) admits; its one caller latches the first invalid universe context it meets.
    pub fn rewriting_universes_shared(visit: F, rewrite: Rewrite) -> Self {
        Self {
            term_depth: 0,
            universe_depth: 0,
            visit,
            mode: Mode::RewritingUniverses(rewrite),
            memo: Memo::ByNode(HashMap::new()),
        }
    }

    /// A level hook at every level of every node that carries universe data, once per occurrence and in walk order: for a hook whose answer is the occurrences themselves.
    pub(crate) fn rewriting_levels_scoped(visit: F, rewrite: LevelRewrite) -> Self {
        Self {
            term_depth: 0,
            universe_depth: 0,
            visit,
            mode: Mode::RewritingLevels(rewrite),
            memo: Memo::None,
        }
    }

    /// [`rewriting_levels_scoped`](Self::rewriting_levels_scoped), memoized on node identity and both binder depths: for a hook whose answer is a function of the level and the universe depth it stands at, or whose effect is idempotent.
    pub(crate) fn rewriting_levels_scoped_shared(visit: F, rewrite: LevelRewrite) -> Self {
        Self {
            term_depth: 0,
            universe_depth: 0,
            visit,
            mode: Mode::RewritingLevels(rewrite),
            memo: Memo::ByNodeAndDepth(HashMap::new()),
        }
    }

    /// Like [`masking`](Self::masking), additionally standing the node's own levels down — see [`Mode::MaskingLevels`] — for [`Visit::take_masked_levels`].
    pub(crate) fn masking_levels(visit: F, placeholder: Term) -> Self {
        Self {
            term_depth: 0,
            universe_depth: 0,
            visit,
            mode: Mode::MaskingLevels {
                placeholder,
                children: Vec::new(),
                levels: Vec::new(),
            },
            // Masking never descends past one level, so there is nothing to revisit.
            memo: Memo::None,
        }
    }

    fn erasing_universes(visit: F) -> Self {
        Self {
            term_depth: 0,
            universe_depth: 0,
            visit,
            mode: Mode::ErasingUniverses,
            // A comparison projects both operands through this at every `Nat` comparison, and the projection walks the whole term: unmemoized it would be `2^n` in the operand's width while the unit budget read linear.
            memo: Memo::ByNode(HashMap::new()),
        }
    }

    /// A hash-consing traversal: structure-preserving, every term coming back as the canonical node of its spelling.
    ///
    /// The rebuild is post-order — a node is built only after its children are traversed — so asking the table for the node just built canonicalizes bottom-up with no extra pass ([`Term::traverse`] takes a path of its own under this visit).
    pub(crate) fn sharing(visit: F, table: Sharing) -> Self {
        Self {
            term_depth: 0,
            universe_depth: 0,
            visit,
            mode: Mode::Sharing {
                table,
                crossed: Vec::new(),
            },
            memo: Memo::ByNode(HashMap::new()),
        }
    }
}

impl<F> Visit<F>
where
    F: FnMut(usize, &Var) -> Option<Subterm>,
{
    /// Enter `amount` binders without visiting a whole scope body in one call — the peeled-chain counterpart of `visit_scope`, for a `Bound::traverse` impl that walks a `Let`/`Rec` spine one link at a time in a loop instead of recursing once per binding. Pair with `leave_scope` in the reverse order links were entered.
    pub(crate) fn enter_scope(&mut self, amount: usize) {
        self.term_depth += amount;
    }

    pub(crate) fn leave_scope(&mut self, amount: usize) {
        self.term_depth -= amount;
    }

    pub(crate) fn enter_universe_scope(&mut self, amount: usize) {
        self.universe_depth += amount;
    }

    pub(crate) fn leave_universe_scope(&mut self, amount: usize) {
        self.universe_depth -= amount;
    }

    /// Invoke the underlying visit callback on a variable at the current depth.
    pub(crate) fn call(&mut self, var: &Var) -> Option<Subterm> {
        (self.visit)(self.term_depth, var)
    }

    pub(crate) fn visit_level(&mut self, level: &Level) -> Level {
        if self.erases_universes() {
            // Every other level-bearing container is removed structurally in `Subterm::traverse`; this is the unavoidable payload of Core's level-indexed `Type` variant, not an erasure sentinel.
            return Level::zero();
        }
        let universe_depth = self.universe_depth;
        match &mut self.mode {
            Mode::RewritingLevels(rewrite) => rewrite(universe_depth, level),
            Mode::MaskingLevels { levels, .. } => {
                levels.push((universe_depth, level.clone()));
                Level::constant(1)
            }
            _ => level.clone(),
        }
    }

    pub(crate) fn rewrite_term(&mut self, term: &Term) -> Option<Term> {
        let term_depth = self.term_depth;
        let universe_depth = self.universe_depth;
        match &mut self.mode {
            Mode::Rewriting(rewrite) | Mode::RewritingUniverses(rewrite) => {
                rewrite(term_depth, term)
            }
            Mode::Masking {
                placeholder,
                children,
                ..
            } => {
                children.push(term.clone());
                Some(placeholder.clone())
            }
            Mode::MaskingLevels {
                placeholder,
                children,
                ..
            } => {
                children.push((universe_depth, term.clone()));
                Some(placeholder.clone())
            }
            Mode::Plain
            | Mode::Pruning
            | Mode::Capturing
            | Mode::RewritingLevels(_)
            | Mode::ErasingUniverses
            | Mode::Sharing { .. } => None,
        }
    }

    /// The children `Mode::Masking` stood down, in traversal order, leaving the visit ready to mask another node.
    pub fn take_masked_children(&mut self) -> Vec<Term> {
        match &mut self.mode {
            Mode::Masking { children, .. } => mem::take(children),
            _ => Vec::new(),
        }
    }

    /// The labels of the scopes crossed while masking, in traversal order, leaving the visit ready to mask another node; empty for a visit that keeps none.
    pub(crate) fn take_masked_labels(&mut self) -> Vec<Option<Vec<Label>>> {
        match &mut self.mode {
            Mode::Masking {
                labels: Some(labels),
                ..
            } => mem::take(labels),
            _ => Vec::new(),
        }
    }

    /// What `Mode::MaskingLevels` stood down, leaving the visit ready for another node.
    pub(crate) fn take_masked_levels(&mut self) -> MaskedLevels {
        match &mut self.mode {
            Mode::MaskingLevels {
                children, levels, ..
            } => MaskedLevels {
                children: mem::take(children),
                levels: mem::take(levels),
            },
            _ => MaskedLevels::default(),
        }
    }

    pub(crate) fn erases_universes(&self) -> bool {
        matches!(self.mode, Mode::ErasingUniverses)
    }

    pub(crate) fn universes_only(&self) -> bool {
        matches!(
            self.mode,
            Mode::RewritingUniverses(_) | Mode::RewritingLevels(_) | Mode::ErasingUniverses
        )
    }

    pub(crate) fn memoizes(&self) -> bool {
        !matches!(self.memo, Memo::None)
    }

    /// Whether this visit remembers the rebuild of a node `owners` hold: every node for a table keyed on all of them, and only one more than a single owner holds for [`Memo::SharedByNodeAndDepth`].
    pub(crate) fn remembers(&self, owners: usize) -> bool {
        match self.memo {
            Memo::None => false,
            Memo::SharedByNodeAndDepth(_) => owners > 1,
            Memo::ByNode(_) | Memo::ByNodeAndDepth(_) => true,
        }
    }

    /// Whether this visit leaves `term` exactly as it is, which it may then hand back without a look inside: a pruning visit touches nothing below a term whose `reach` is within the depth it stands at, and a capture of local binders nothing in a term that has no local free besides.
    pub(crate) fn passes_over(&self, term: &Term) -> bool {
        match self.mode {
            Mode::Pruning => term.reach() <= self.term_depth,
            Mode::Capturing => !term.has_local_free() && term.reach() <= self.term_depth,
            _ => false,
        }
    }

    /// The memoized rebuild of the input node at `key`, at the depths this visit currently stands at for the modes whose memo is depth-keyed, as the occurrence `at` would have it rebuilt — see [`Memo`] for its span.
    pub(crate) fn memo_get(&self, key: usize, at: &Term) -> Option<Term> {
        let remembered = match &self.memo {
            Memo::None => None,
            Memo::ByNode(memo) => memo.get(&key),
            Memo::SharedByNodeAndDepth(memo) | Memo::ByNodeAndDepth(memo) => {
                memo.get(&(key, self.term_depth, self.universe_depth))
            }
        }?;

        Some(remembered.for_occurrence(at))
    }

    /// Remember `rebuilt` as the rebuild of the input node at `key`, filled by the occurrence `at`.
    pub(crate) fn memo_put(&mut self, key: usize, at: &Term, rebuilt: Term) {
        let remembered = Remembered {
            filled_at: at.span(),
            rebuilt,
        };
        match &mut self.memo {
            Memo::None => {}
            Memo::ByNode(memo) => {
                memo.insert(key, remembered);
            }
            Memo::SharedByNodeAndDepth(memo) | Memo::ByNodeAndDepth(memo) => {
                memo.insert((key, self.term_depth, self.universe_depth), remembered);
            }
        }
    }

    pub(crate) fn rewrites_terms(&self) -> bool {
        matches!(
            self.mode,
            Mode::Rewriting(_) | Mode::RewritingUniverses(_) | Mode::Masking { .. }
        )
    }

    /// Whether this visit hash-conses: a term it meets comes back as the canonical node of its spelling.
    pub(crate) fn conses(&self) -> bool {
        matches!(self.mode, Mode::Sharing { .. })
    }

    /// Begin a node a consing visit is about to rebuild.
    pub(crate) fn begin_node(&mut self) {
        if let Mode::Sharing { crossed, .. } = &mut self.mode {
            crossed.push(Vec::new());
        }
    }

    /// The canonical node for `fresh`, the node begun last, built over the canonical `children` it holds.
    pub(crate) fn adopt(&mut self, fresh: Term, children: Vec<usize>) -> Term {
        let Mode::Sharing { table, crossed } = &mut self.mode else {
            return fresh;
        };
        let labels = crossed.pop().unwrap_or_default();

        table.adopt(Spelling { labels, children }, fresh)
    }

    /// A scope is being crossed: its labels are part of the spelling of the node a consing visit is rebuilding, and of the node a visit that reads labels is masking.
    pub(crate) fn cross(&mut self, labels: &Option<Vec<Label>>) {
        match &mut self.mode {
            Mode::Sharing { crossed, .. } => {
                if let Some(node) = crossed.last_mut() {
                    node.push(labels.clone());
                }
            }
            Mode::Masking {
                labels: Some(read), ..
            } => read.push(labels.clone()),
            _ => {}
        }
    }

    /// The remembered rebuild of the input node at `key` exactly as it was stored. What a consing visit hands every occurrence: its nodes sit under no span, so there is none to make an occurrence's own, and [`Visit::memo_get`] would hand a node first met with no span the span of whoever asks next.
    pub(crate) fn memo_get_as_stored(&self, key: usize) -> Option<Term> {
        match &self.memo {
            Memo::ByNode(memo) => memo.get(&key).map(|remembered| remembered.rebuilt.clone()),
            _ => None,
        }
    }

    pub(crate) fn visit_subterm(&mut self, term: &Term) -> Term {
        term.traverse(self)
    }

    pub(crate) fn visit_scope<A: Arity, B: Bound>(&mut self, scope: &Scope<A, B>) -> Scope<A, B> {
        self.cross(&scope.labels);
        self.term_depth += scope.arity.arity();
        let body = scope.body.traverse(self).into();
        self.term_depth -= scope.arity.arity();

        Scope {
            arity: scope.arity,
            labels: scope.labels.clone(),
            body,
        }
    }
}

#[cfg(test)]
mod tests;
