//! De Bruijn machinery for the `core` stage's terms.
//!
//! `Scope`, `Telescope`, `Var`, the `Bound` traversal trait, and the `Visit` driver operate over `core`'s `Term` and `Subterm`. `core` keeps its own `Subterm::traverse` (the big structural match, including its intrinsics) and plugs it into this machinery by implementing `Bound`.

use {
    super::{
        Free, Global, Level, LevelHead, MetavarId, Subterm, Term, UniverseError, UniverseMetaId,
        UniverseParam,
    },
    std::{
        cell::RefCell,
        collections::{BTreeSet, HashMap},
        convert::Infallible,
        fmt,
        hash::Hash,
        mem,
        ops::Deref,
        rc::Rc,
    },
};

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

    /// De Bruijn weakening: add `amount` to every loose bound index (`>= depth`), making room for that many new enclosing binders when a term is moved under them. Index-monotonic, so the traversal prunes by `reach`.
    fn shift(&self, amount: usize) -> Self {
        curios_profile::sample!("walk::shift", 1);
        self.traverse(&mut Visit::pruning(|depth, var| {
            var.as_bound()
                .filter(|&index| index >= depth)
                .map(|index| Subterm::Var(Var::bound(index + amount)))
        }))
    }

    /// The closing half of the locally-nameless discipline: turn free occurrences of `binders` into bound indices (position in `binders`, offset by the current depth) while shifting already-loose indices past the new binders. `Scope::close` is this plus the name bookkeeping. Rewrites *free* names, so it can never be pruned by `reach`.
    ///
    /// Memoized on node identity and depth, so a DAG-shaped input — the weak-head form of a web of definitions each naming the one before it twice, whose tree is `2^n` — is captured in its own size: the kernel's conversion history captures every goal it enters, and captured that web's tree at every one.
    fn capture(&self, binders: &[&Free]) -> Self {
        curios_profile::sample!("walk::capture", 1);
        self.traverse(&mut Visit::shared_at_depth(|depth, var| {
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

    /// The opening half of the locally-nameless discipline: substitute the outermost `terms.len()` loose bound indices with `terms` (each shifted by the depth it lands under) and re-tighten the loose indices beyond them. `Scope::open` is this plus the arity check; effects depend only on indices `>= depth`, so the traversal prunes by `reach`.
    fn release(&self, terms: &[&Term]) -> Self {
        curios_profile::sample!("walk::release", 1);
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
                vars.insert(name.clone());
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

pub(crate) fn rewrite_universe_levels<B: Bound, E: 'static>(
    value: &B,
    rewrite: impl FnMut(&Level) -> Result<Level, E> + 'static,
) -> Result<B, E> {
    let mut rewrite = rewrite;
    rewrite_universe_levels_scoped(value, move |_, level| rewrite(level))
}

/// Structural implementation of universe erasure: nominal vectors, instances, and contexts are removed by their owning nodes. `Type` must still carry a `Level` in Core, so its now-irrelevant payload is rebuilt with Core's private canonical ground representative. It is read two ways. As a projection into a world where levels are irrelevant — the Core-to-Ersd lowering, and goal-report display, since the surface language has no spelling for an instance — it is exact. As an equality key it is a quotient coarser than definitional equality, identifying `Type 0` with `Type 1`; that reading is sound only over `Nat` summands, where no level can reach a number, and `documentation/soundness/what-the-kernel-consults/the-refinement-key.md` records the route it admits anywhere else.
pub fn project_erased_universes<B: Bound>(value: &B) -> B {
    curios_profile::sample!("walk::project_erased_universes", 1);
    value.traverse(&mut Visit::erasing_universes(|_, _| None))
}

pub fn rewrite_universe_levels_scoped<B: Bound, E: 'static>(
    value: &B,
    rewrite: impl FnMut(usize, &Level) -> Result<Level, E> + 'static,
) -> Result<B, E> {
    let rewrite = Rc::new(RefCell::new(rewrite));
    let error = Rc::new(RefCell::new(None));
    let rewrite_for_visit = Rc::clone(&rewrite);
    let error_for_visit = Rc::clone(&error);
    let mut visit = Visit::rewriting_levels_scoped(
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
    rewrite_universe_levels_scoped(value, move |depth, level| {
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

/// A body abstracted over `A::arity()` binders, locally nameless: the body stores de Bruijn indices, while `names` remembers the [`Free`] identities it was closed over, for printing and for the hints later rebuilds re-mint from (`None` for a `constant` scope that never had binders written). Like a [`Term`]'s span, `names` is irrelevant to identity: `Eq`/`Hash` compare arity and body only, so scopes differing solely in binder names are equal — term equality is α-equivalence. The one place binder *hints* are semantic rather than decoration — tuple-type fields, the target of `.label` resolution — reasserts them in its own node identity (see `TupleType` in `term.rs`). Built by `close` (which captures free occurrences of those identities) and eliminated by `open` (which substitutes terms for the indices); entering a `Scope` is the only place a [`Visit`]'s depth changes, so this type is the unit of binding for the whole crate.
#[curios_archive::archived]
pub struct Scope<A: Arity, B: Bound = Term> {
    arity: A,
    names: Option<Vec<Free>>,
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
            names: Some(binders.as_ref().iter().map(|&name| name.clone()).collect()),
            body: body.capture(binders.as_ref()).into(),
        }
    }

    pub fn arity(&self) -> usize {
        self.arity.arity()
    }

    pub fn body(&self) -> &B {
        &self.body
    }

    pub(crate) fn names(&self) -> Option<&[Free]> {
        self.names.as_deref()
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
            names: None,
            body: body.into(),
        }
    }

    /// Rebuild this scope with `f` applied to its body, preserving arity and binder names. The body keeps its de Bruijn structure, so `f` must be a transformation that does not disturb loose indices — e.g. zonking, which only replaces closed metavariable nodes by closed solutions, or a canonicalization, which replaces a node by a structurally equal one.
    ///
    /// Rewriting here rather than opening and re-closing is what keeps a term's memoized derivations: `open` and `close` each rebuild every node they touch, so the round trip discards all of them to arrive where this arrives without moving.
    pub(crate) fn map_body(&self, f: impl FnOnce(&B) -> B) -> Self {
        Self {
            arity: self.arity,
            names: self.names.clone(),
            body: f(&self.body).into(),
        }
    }

    /// Fallible `Self::map_body`, for a rewrite that can reject its input.
    pub fn try_map_body<E>(&self, f: impl FnOnce(&B) -> Result<B, E>) -> Result<Self, E> {
        Ok(Self {
            arity: self.arity,
            names: self.names.clone(),
            body: f(&self.body)?.into(),
        })
    }

    /// The identity of the binder at position `index` (0 = first/outermost), for a rebuild that must re-close over the very same binders.
    pub(crate) fn binder(&self, index: usize) -> Option<&Free> {
        self.names.as_deref()?.get(index)
    }

    /// What the binder at position `index` was called where it was written — a rendering aid a rebuild carries onto the binder it re-mints, never a way to recognize which binder this is.
    pub fn hint(&self, index: usize) -> Option<&str> {
        self.binder(index)?.hint()
    }

    pub fn first_hint(&self) -> Option<&str> {
        self.hint(0)
    }

    pub fn second_hint(&self) -> Option<&str> {
        self.hint(1)
    }

    pub fn third_hint(&self) -> Option<&str> {
        self.hint(2)
    }

    pub fn hint_iter(&self) -> impl Iterator<Item = Option<&str>> {
        (0..self.arity()).map(move |index| self.hint(index))
    }

    /// The identity of each binder in order, `None` where the scope was built without them (`constant`).
    pub(crate) fn binder_iter(&self) -> impl Iterator<Item = Option<&Free>> {
        (0..self.arity()).map(move |index| self.binder(index))
    }

    /// Whether the binder at position `index` (0 = first/outermost label) is referenced anywhere in the body. A bound var refers to this binder iff its de Bruijn index equals `index` plus the number of binders entered since — which `Visit` tracks as `depth`. Used by erasure to spot an eliminator whose induction hypothesis is dead: that arm is a case-split, not a fold.
    pub fn uses(&self, index: usize) -> bool {
        let mut used = false;
        self.body.traverse(&mut Visit::new(|depth, var: &Var| {
            if var.as_bound() == Some(index + depth) {
                used = true;
            }
            None
        }));
        used
    }
}

impl<B: Bound> Scope<Many, B> {
    /// Prepend `binder` to the front of this scope: it becomes index 0 and every existing binder shifts up by one.
    ///
    /// Done by a direct `capture` on the body — free occurrences of `binder` bind to the new index 0 while every existing bound index shifts by one — rather than an open/close round-trip through names, which would have to reopen inner binders into free occurrences and could not tell them from genuine outer references.
    pub fn prepend(&self, binder: &Free) -> Self {
        let names = self.names.as_ref().map(|names| {
            [binder.clone()]
                .into_iter()
                .chain(names.iter().cloned())
                .collect()
        });

        Self {
            arity: Many(self.arity() + 1),
            names,
            body: self.body.capture(&[binder]).into(),
        }
    }
}

impl<A: Arity + fmt::Debug, B: Bound> fmt::Debug for Scope<A, B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Scope")
            .field("arity", &self.arity)
            .field("names", &self.names)
            .field("body", &self.body)
            .finish()
    }
}

impl<A: Arity + Clone, B: Bound> Clone for Scope<A, B> {
    fn clone(&self) -> Self {
        Self {
            arity: self.arity,
            names: self.names.clone(),
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

// === Telescope ===============================================================

/// A dependent context: a chain of entry types where each `Cons` tail is a one-binder [`Scope`], so every later entry — and the final `Done` payload — may mention the binders before it. Function types, function literals, and tuple types all reuse it and differ only in the payload: a `Term` (the return type or body) for Π/λ, `()` for Σ, where the fields themselves are the point.
#[curios_archive::archived(recursive)]
pub enum Telescope<B: Bound> {
    Done(Box<B>),
    Cons(Term, #[archived_omit_bounds] Scope<One, Telescope<B>>),
}

impl<B: Bound> Telescope<B> {
    pub fn done(body: B) -> Self {
        Telescope::Done(body.into())
    }

    pub(crate) fn cons<T>(binder: &Free, ty: T, rest: Telescope<B>) -> Self
    where
        T: Into<Term>,
    {
        Telescope::Cons(ty.into(), Scope::close(One, &[binder], rest))
    }

    /// Build a telescope from `(binder, type)` entries in written order, right-folding so each entry's scope closes over everything after it — written order mirrors telescope order.
    pub fn build<I, T>(entries: I, body: B) -> Self
    where
        I: IntoIterator<Item = (Free, T)>,
        T: Into<Term>,
    {
        entries
            .into_iter()
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .fold(Telescope::done(body), |rest, (binder, ty)| {
                Telescope::cons(&binder, ty, rest)
            })
    }

    pub fn len(&self) -> usize {
        let mut n = 0;
        let mut cur = self;
        while let Telescope::Cons(_, rest) = cur {
            n += 1;
            cur = &rest.body;
        }
        n
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, Telescope::Done(_))
    }

    /// The final payload beneath every binder, without opening the telescope or substituting for any of its bound variables.
    pub fn terminal(&self) -> &B {
        let mut current = self;
        loop {
            match current {
                Telescope::Done(body) => return body,
                Telescope::Cons(_, rest) => current = &rest.body,
            }
        }
    }

    /// The binder hint at each position (`""` when unnamed), walking the spine without opening — names are structural, no substitution needed.
    pub fn labels(&self) -> Vec<&str> {
        let mut out = Vec::new();
        let mut cur = self;
        while let Telescope::Cons(_, rest) = cur {
            out.push(rest.first_hint().unwrap_or_default());
            cur = &rest.body;
        }
        out
    }

    /// Replace the display hints along the spine, leaving each binder's identity alone. Pure metadata: the de Bruijn structure is untouched and no occurrence changes what it refers to — this restores source labels after a rebuild that had to re-mint its binders (tuple-type labels are part of the type's identity and the target of `.label` resolution, so they must survive elaboration verbatim).
    pub fn relabel(self, labels: &[&str]) -> Self {
        let mut entries = Vec::new();
        let mut labels = labels.iter();
        let mut current = self;
        let body = loop {
            match current {
                Telescope::Done(body) => break body,
                Telescope::Cons(ty, rest) => {
                    let label = labels.next().expect("relabel arity");
                    let names = rest
                        .names
                        .as_ref()
                        .map(|names| names.iter().map(|name| name.relabelled(label)).collect());
                    entries.push((ty, names));
                    current = *rest.body;
                }
            }
        };

        entries
            .into_iter()
            .rev()
            .fold(Telescope::Done(body), |rest, (ty, names)| {
                Telescope::Cons(
                    ty,
                    Scope {
                        arity: One,
                        names,
                        body: Box::new(rest),
                    },
                )
            })
    }

    pub fn open(&self, args: &[&Term]) -> B {
        assert!(
            self.len() == args.len(),
            "telescope arity mismatch in `open`: expected {}, got {}",
            self.len(),
            args.len()
        );

        let mut cur = self.clone();
        for arg in args {
            cur = match cur {
                Telescope::Cons(_, rest) => rest.open(&[arg]),
                Telescope::Done(_) => unreachable!(),
            };
        }
        match cur {
            Telescope::Done(body) => *body,
            Telescope::Cons(_, _) => unreachable!(),
        }
    }

    /// Open the leading binders at successive `params` — one binder per param — returning the residual telescope. Every caller's telescope leads with the type parameters (constructor payloads, struct fields, inductive indices all follow them), so a telescope that runs out early is an invariant violation.
    pub fn open_params(self, params: &[Term]) -> Telescope<B> {
        let mut telescope = self;
        for param in params {
            telescope = match telescope {
                Telescope::Cons(_, rest) => rest.open(&[param]),
                Telescope::Done(_) => unreachable!("telescope must lead with its parameters"),
            };
        }
        telescope
    }

    /// Open the telescope across `args`, invoking `f(arg, ty)` at each binder before substituting that arg into the rest, and return the final `Done` body. The walk is infallible; the error type `E` belongs to the callback.
    pub fn walk<F, E>(self, args: &[Term], mut f: F) -> Result<B, E>
    where
        F: FnMut(usize, &Term, &Term) -> Result<(), E>,
    {
        assert!(
            self.len() == args.len(),
            "telescope arity mismatch in `walk`: expected {}, got {}",
            self.len(),
            args.len()
        );

        let mut tele = self;
        let mut i = 0;
        loop {
            match tele {
                Telescope::Done(body) => return Ok(*body),
                Telescope::Cons(ty, rest) => {
                    f(i, &args[i], &ty)?;
                    tele = rest.open(&[&args[i]]);
                    i += 1;
                }
            }
        }
    }

    /// The type at `index`, with each preceding binder opened at `sub` of its position. The general form; a field telescope read from a value wants [`Telescope::field_type_from`] instead.
    pub fn nth<F>(self, index: usize, mut sub: F) -> Option<Term>
    where
        F: FnMut(usize) -> Term,
    {
        let mut tele = self;
        let mut j = 0;
        loop {
            match tele {
                Telescope::Done(_) => return None,
                Telescope::Cons(ty, rest) => {
                    if j == index {
                        return Some(ty);
                    }
                    tele = rest.open(&[&sub(j)]);
                    j += 1;
                }
            }
        }
    }

    /// The type of field `index` as seen from `value`: every preceding field is opened at its own projection off `value`, so a field type that names an earlier field names *that value's* earlier field rather than a loose binder.
    ///
    /// This is the one answer to "what type does `value.index` have", and every site that asks — inference, sorting, conversion, witness resolution, operator dispatch, and the method wrappers `into_core` generates — reaches it through here. Re-deriving it anywhere else is how the two readings drift: the wrappers once restated a field's written type in a scope binding no sibling, which was well-formed only while no concept had a dependent field telescope.
    pub fn field_type_from(self, value: &Term, index: usize) -> Option<Term> {
        self.nth(index, |j| Term::proj(value.clone(), j))
    }
}

impl Telescope<Term> {
    /// Whether any metavariable in a function/Π telescope (`Func`/`FuncType`) — the parameter types and the trailing body/return type — satisfies `pred`, short-circuiting on the first hit.
    pub fn any_metavar<F: FnMut(MetavarId) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Telescope::Cons(ty, rest) => ty.any_metavar(pred) || rest.body().any_metavar(pred),
            Telescope::Done(body) => body.any_metavar(pred),
        }
    }

    /// Whether any `Term` in a function/Π telescope (`Func`/`FuncType`) — the parameter types and the trailing body/return type — satisfies `pred`, short-circuiting on the first hit. The telescope leg of `Subterm::any_child_term`: `pred` carries the per-node memoized recursion, so this visits each `Term` exactly once.
    pub(crate) fn any_term<F: FnMut(&Term) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Telescope::Cons(ty, rest) => pred(ty) || rest.body().any_term(pred),
            Telescope::Done(body) => pred(body),
        }
    }

    /// Walk a function/Π telescope (`Func`/`FuncType`): the parameter types and the trailing body/return type. Concrete in `Term` — no collector trait needed. See `Subterm::collect_construction_names`.
    pub fn collect_construction_names(&self, names: &mut BTreeSet<Global>) {
        match self {
            Telescope::Cons(ty, rest) => {
                ty.collect_construction_names(names);
                rest.body().collect_construction_names(names);
            }
            Telescope::Done(body) => body.collect_construction_names(names),
        }
    }
}

impl Telescope<Vec<Term>> {
    /// Whether any metavariable in a constructor signature — the payload domains, or one of the index targets it terminates in — satisfies `pred`, short-circuiting on the first hit.
    pub fn any_metavar<F: FnMut(MetavarId) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Telescope::Cons(ty, rest) => ty.any_metavar(pred) || rest.body().any_metavar(pred),
            Telescope::Done(targets) => targets.iter().any(|target| target.any_metavar(pred)),
        }
    }
}

impl Telescope<Telescope<()>> {
    /// Whether any metavariable in a nested arity telescope — a declaration's parameter domains and, at its terminal, its index or field domains — satisfies `pred`, short-circuiting on the first hit.
    pub fn any_metavar<F: FnMut(MetavarId) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Telescope::Cons(ty, rest) => ty.any_metavar(pred) || rest.body().any_metavar(pred),
            Telescope::Done(inner) => inner.any_metavar(pred),
        }
    }
}

impl Telescope<()> {
    /// Whether any metavariable in a Σ telescope (`TupleType`) — only the field types; its `Done` body is `()` — satisfies `pred`, short-circuiting on the first hit.
    pub fn any_metavar<F: FnMut(MetavarId) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Telescope::Cons(ty, rest) => ty.any_metavar(pred) || rest.body().any_metavar(pred),
            // The trailing body is `()`, which holds no metavariables.
            Telescope::Done(_) => false,
        }
    }

    /// Whether any `Term` in a Σ telescope (`TupleType`) — only the field types; its `Done` body is `()` — satisfies `pred`, short-circuiting on the first hit. See the `Telescope<Term>` counterpart above.
    pub(crate) fn any_term<F: FnMut(&Term) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Telescope::Cons(ty, rest) => pred(ty) || rest.body().any_term(pred),
            // The trailing body is `()`, which holds no terms.
            Telescope::Done(_) => false,
        }
    }

    /// Walk a Σ telescope (`TupleType`): only the field types — its `Done` body is `()`, which contributes no names.
    pub fn collect_construction_names(&self, names: &mut BTreeSet<Global>) {
        if let Telescope::Cons(ty, rest) = self {
            ty.collect_construction_names(names);
            rest.body().collect_construction_names(names);
        }
    }
}

impl<B: Bound> fmt::Debug for Telescope<B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Telescope::Done(body) => f.debug_tuple("Done").field(body).finish(),
            Telescope::Cons(ty, rest) => f.debug_tuple("Cons").field(ty).field(rest).finish(),
        }
    }
}

impl<B: Bound> Clone for Telescope<B> {
    fn clone(&self) -> Self {
        match self {
            Telescope::Done(body) => Telescope::Done(body.clone()),
            Telescope::Cons(ty, rest) => Telescope::Cons(ty.clone(), rest.clone()),
        }
    }
}

impl<B: Bound> PartialEq for Telescope<B> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Telescope::Done(a), Telescope::Done(b)) => a == b,
            (Telescope::Cons(ta, ra), Telescope::Cons(tb, rb)) => ta == tb && ra == rb,
            _ => false,
        }
    }
}

impl<B: Bound> Eq for Telescope<B> {}

impl<B: Bound> Hash for Telescope<B> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Telescope::Done(body) => {
                state.write_u8(0);
                body.hash(state);
            }
            Telescope::Cons(ty, rest) => {
                state.write_u8(1);
                ty.hash(state);
                rest.hash(state);
            }
        }
    }
}

/// All three derivations walk the spine in a loop rather than one native frame per parameter.
///
/// A telescope's length is its written arity, and "written depth is a bound the default stack tolerates" is the assumption this file already retired for `Let`/`Rec` spines — `Visit::enter_scope`/`Visit::leave_scope` exist for exactly this shape. The spine is the sibling that kept the recursion, which is invisible in authored signatures and unbounded in generated ones.
impl<B: Bound> Bound for Telescope<B> {
    fn traverse<F>(&self, visit: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        // Each entry type is visited under the binders declared *before* it, so a link's own binders are entered after its type and retracted in the reverse order on the way back up — which is the bracket `visit_scope` used to keep on the native stack.
        let mut entries = Vec::new();
        let mut current = self;
        let body = loop {
            match current {
                Telescope::Cons(ty, rest) => {
                    entries.push((visit.visit_subterm(ty), rest.names.clone(), rest.arity()));
                    visit.enter_scope(rest.arity());
                    current = rest.body();
                }
                Telescope::Done(body) => break body.traverse(visit),
            }
        };

        entries
            .into_iter()
            .rev()
            .fold(Telescope::Done(body.into()), |rest, (ty, names, arity)| {
                visit.leave_scope(arity);
                Telescope::Cons(
                    ty,
                    Scope {
                        arity: One,
                        names,
                        body: Box::new(rest),
                    },
                )
            })
    }

    /// `saturating_sub` is monotone, so it distributes over `max` — which is what lets the nested `max(ty.reach(), rest.reach())` be flattened into one pass that discounts each entry by the binders standing before it.
    fn reach(&self) -> usize {
        let (mut reach, mut depth) = (0, 0);
        let mut current = self;
        loop {
            match current {
                Telescope::Cons(ty, rest) => {
                    reach = reach.max(ty.reach().saturating_sub(depth));
                    depth += rest.arity();
                    current = rest.body();
                }
                Telescope::Done(body) => {
                    return reach.max(body.reach().saturating_sub(depth));
                }
            }
        }
    }

    fn has_metavar(&self) -> bool {
        let mut current = self;
        loop {
            match current {
                Telescope::Cons(ty, rest) => match ty.has_metavar() {
                    true => return true,
                    false => current = rest.body(),
                },
                Telescope::Done(body) => return body.has_metavar(),
            }
        }
    }

    fn has_transient(&self) -> bool {
        let mut current = self;
        loop {
            match current {
                Telescope::Cons(ty, rest) => match ty.has_transient() {
                    true => return true,
                    false => current = rest.body(),
                },
                Telescope::Done(body) => return body.has_transient(),
            }
        }
    }
}

// === Visit ===================================================================

/// A hash-consing table, shared rather than owned so one canonicalization spans a whole module: two definitions that build the same type collapse onto one node only if they consult the same table.
#[derive(Debug, Clone, Default)]
pub struct Sharing {
    table: Rc<RefCell<HashMap<Term, Term>>>,
}

impl Sharing {
    pub fn new() -> Self {
        Self::default()
    }

    /// `value` with every node replaced by the canonical node of its structure.
    ///
    /// One `Sharing` must span every snapshot being canonicalized together: the duplication worth collapsing is overwhelmingly *between* definitions, and between the lowered and elaborated views of the same prelude, so a table per term or per module would collapse almost none of it.
    pub fn share<B: Bound>(&self, value: &B) -> B {
        value.traverse(&mut Visit::sharing(|_, _| None, self.clone()))
    }

    /// Distinct structures adopted so far — the census this pass is justified by.
    pub fn structures(&self) -> usize {
        self.table.borrow().len()
    }
}

impl Sharing {
    /// The canonical node for `rebuilt`, adopting it if this structure is new.
    fn canonical(&self, rebuilt: &Term) -> Option<Term> {
        let mut table = self.table.borrow_mut();
        Some(match table.get(rebuilt) {
            Some(canonical) => canonical.clone(),
            None => {
                table.insert(rebuilt.clone(), rebuilt.clone());
                rebuilt.clone()
            }
        })
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

/// Whether a traversal remembers what it rebuilt, and under what key.
///
/// **Orthogonal to [`Mode`], and stated separately because it is.** It used to be encoded by doubling variants — `Plain` beside `PlainSharedAtDepth`, `Rewriting` beside `RewritingShared` — which made a walk's memo a property of *which mode it picked* rather than a decision its author made. Three modes then had no memoized twin at all, and two of those three were `2^n` waiting to be found: the machine's forced recursive call and the universe-erased projection a `Nat` comparison takes.
///
/// **The law.** Within one pass over an immutable DAG a node's answer is determined by the node and by whatever else the visit is parameterised on, so a revisit of the same key may be skipped. A reduct is a DAG whose *tree* expansion doubles per level — one substitution landing a term in two positions is enough — so a walk that rebuilds per occurrence is exponential in a term the node count, and therefore the unit budget, reads as linear.
///
/// **When it is legal.** [`Memo::ByNode`] needs the variable callback and the rewrite hook pure in the node; [`Memo::ByNodeAndDepth`] needs them pure in the node and the depth. Purity is not the whole condition: a hook with an *effect* may still memoize when the effect is idempotent — a set insert, a first-error latch — and may not when it is not. Three hooks serve operands by position (an `index` or an iterator) and one pushes into a `Vec`; for those the answer differs per occurrence and [`Memo::None`] is the only correct choice.
///
/// Every constructor takes one explicitly. There is no default, deliberately: both defects above were omissions, so neither defaulting direction is safe — a wrong `None` costs an exponent and a wrong memo costs a wrong answer.
enum Memo {
    /// Rebuild every occurrence. Correct for a hook whose answer depends on how many times it has run.
    None,
    /// Keyed on input node identity. Addresses are stable for the traversal because the caller's value holds every node alive.
    ByNode(HashMap<usize, Term>),
    /// Keyed on input node identity *and* binder depth, for a visit whose effect depends on the depth it runs at — `capture` is the case, where a depth-blind memo would hand a second occurrence the wrong indices.
    ByNodeAndDepth(HashMap<(usize, usize), Term>),
}

/// What a traversal does beyond rewriting variables.
///
/// A closed set, stated as a sum. These were six independent fields — a `prune` flag, two optional boxed hooks, two more flags, and an optional memo — of which only the eight combinations below were ever constructed, out of the sixty-four the fields could express. Every consumer re-derived which combination it was looking at by testing the fields one at a time.
///
/// Naming the combinations makes adding a ninth a change the compiler checks: every `match` below stops compiling until the new case has been decided. Closed on purpose — a traversal mode is compiler-internal vocabulary, and all of its construction sites live in this crate.
enum Mode {
    /// Rebuild every node, rewriting variables only.
    Plain,
    /// Skip subtrees whose `reach` proves no loose index can be touched.
    Pruning,
    /// A term-level pre-hook substitutes whole nodes before descending. A substituted node is not descended into.
    Rewriting(Rewrite),
    /// [`Mode::Rewriting`], visiting only nodes that carry universe data.
    RewritingUniverses(Rewrite),
    /// A level-level hook, visiting only nodes that carry universe data.
    RewritingLevels(LevelRewrite),
    /// Replace every level with the ground representative, visiting only nodes that carry universe data.
    ErasingUniverses,
    /// Hash-consing: replace each rebuilt node with the canonical node of its structure. Pairs with [`Memo::ByNode`], which is what keeps the input's sharing as well as the output's.
    Sharing(Sharing),
    /// Stand every child term down to `placeholder`, keeping the ones removed in `children`. Because a substituted node is never descended into, the rebuilt node carries this level's own payload and nothing below it — which is what lets [`Term`]'s equality compare one node at a time instead of recursing to the bottom of the term.
    ///
    /// The removed children and the node they came out of are produced by the same pass, so the two can never disagree about what a child is.
    Masking {
        placeholder: Term,
        children: Vec<Term>,
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

    /// Like `new`, memoized on node identity and binder depth together — for a visit whose effect depends on the depth, which a depth-blind memo would answer wrongly. `capture` is the case. See [`Memo`] for when this is legal.
    fn shared_at_depth(visit: F) -> Self {
        Self {
            term_depth: 0,
            universe_depth: 0,
            visit,
            mode: Mode::Plain,
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
            // **Measured twice as inert, and there is a reason it must be.** `shift` and `release` are pure in the node and the depth, so [`Memo::ByNodeAndDepth`] would be *legal* here — it is not taken because it cannot help. A tree that expands exponentially is a reduction result, and a reduct substituted here is closed, so `reach` is zero and pruning already answers it in O(1) before a memo could. Installed anyway, `str_literal_cost_measurements` reported all ten rows byte-for-byte identical (2026-08-24), matching an earlier swap of `release` alone that moved a `BigNat/sub` ladder not at all. Reopening it wants a workload where a substituted term is *open* and shared, which nothing in the corpus produces.
            memo: Memo::None,
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
            },
            // Masking never descends past one level, so there is nothing to revisit.
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

    pub fn rewriting_universes(visit: F, rewrite: Rewrite) -> Self {
        Self {
            term_depth: 0,
            universe_depth: 0,
            visit,
            mode: Mode::RewritingUniverses(rewrite),
            memo: Memo::None,
        }
    }

    pub(crate) fn rewriting_levels_scoped(visit: F, rewrite: LevelRewrite) -> Self {
        Self {
            term_depth: 0,
            universe_depth: 0,
            visit,
            mode: Mode::RewritingLevels(rewrite),
            memo: Memo::None,
        }
    }

    fn erasing_universes(visit: F) -> Self {
        Self {
            term_depth: 0,
            universe_depth: 0,
            visit,
            mode: Mode::ErasingUniverses,
            // A comparison projects both operands through this at every `Nat` comparison, and the projection walks the whole term: unmemoized it was `2^n` in the operand's width while the unit budget read linear.
            memo: Memo::ByNode(HashMap::new()),
        }
    }

    /// A hash-consing traversal: structure-preserving, but replacing every rebuilt node with the canonical node of its shape.
    ///
    /// The rebuild is already post-order — a node is constructed only after its children are traversed — so consulting the table on the rebuilt node canonicalizes bottom-up with no extra pass. Spans survive: they sit on the `Term` wrapper, outside the shared node, so each occurrence keeps its own while the structure underneath is shared.
    pub(crate) fn sharing(visit: F, table: Sharing) -> Self {
        Self {
            term_depth: 0,
            universe_depth: 0,
            visit,
            mode: Mode::Sharing(table),
            memo: Memo::ByNode(HashMap::new()),
        }
    }
}

impl<F> Visit<F>
where
    F: FnMut(usize, &Var) -> Option<Subterm>,
{
    pub(crate) fn term_depth(&self) -> usize {
        self.term_depth
    }

    pub(crate) fn prune(&self) -> bool {
        matches!(self.mode, Mode::Pruning)
    }

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
            // Every other level-bearing container is removed structurally in `Subterm::traverse`; this is the unavoidable payload of Core's still-level-indexed `Type` variant, not an erasure sentinel.
            return Level::zero();
        }
        match &mut self.mode {
            Mode::RewritingLevels(rewrite) => rewrite(self.universe_depth, level),
            _ => level.clone(),
        }
    }

    pub(crate) fn rewrite_term(&mut self, term: &Term) -> Option<Term> {
        let term_depth = self.term_depth;
        match &mut self.mode {
            Mode::Rewriting(rewrite) | Mode::RewritingUniverses(rewrite) => {
                rewrite(term_depth, term)
            }
            Mode::Masking {
                placeholder,
                children,
            } => {
                children.push(term.clone());
                Some(placeholder.clone())
            }
            Mode::Plain
            | Mode::Pruning
            | Mode::RewritingLevels(_)
            | Mode::ErasingUniverses
            | Mode::Sharing(_) => None,
        }
    }

    /// The children `Mode::Masking` stood down, in traversal order, leaving the visit ready to mask another node.
    pub fn take_masked_children(&mut self) -> Vec<Term> {
        match &mut self.mode {
            Mode::Masking { children, .. } => mem::take(children),
            _ => Vec::new(),
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

    /// The memoized rebuild of the input node at `key`, at the depth this visit currently stands at for the modes whose memo is depth-keyed.
    pub(crate) fn memo_get(&self, key: usize) -> Option<Term> {
        match &self.memo {
            Memo::None => None,
            Memo::ByNode(memo) => memo.get(&key).cloned(),
            Memo::ByNodeAndDepth(memo) => memo.get(&(key, self.term_depth)).cloned(),
        }
    }

    pub(crate) fn memo_put(&mut self, key: usize, term: Term) {
        let depth = self.term_depth;
        match &mut self.memo {
            Memo::None => {}
            Memo::ByNode(memo) => {
                memo.insert(key, term);
            }
            Memo::ByNodeAndDepth(memo) => {
                memo.insert((key, depth), term);
            }
        }
    }

    pub(crate) fn rewrites_terms(&self) -> bool {
        matches!(
            self.mode,
            Mode::Rewriting(_) | Mode::RewritingUniverses(_) | Mode::Masking { .. }
        )
    }

    /// The canonical node for a rebuilt term, or `None` when not hash-consing.
    pub(crate) fn share_structure(&self, rebuilt: &Term) -> Option<Term> {
        match &self.mode {
            Mode::Sharing(sharing) => sharing.canonical(rebuilt),
            _ => None,
        }
    }

    pub(crate) fn visit_subterm(&mut self, term: &Term) -> Term {
        term.traverse(self)
    }

    pub(crate) fn visit_scope<A: Arity, B: Bound>(&mut self, scope: &Scope<A, B>) -> Scope<A, B> {
        self.term_depth += scope.arity.arity();
        let body = scope.body.traverse(self).into();
        self.term_depth -= scope.arity.arity();

        Scope {
            arity: scope.arity,
            names: scope.names.clone(),
            body,
        }
    }
}
