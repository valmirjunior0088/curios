//! De Bruijn machinery for the `core` stage's terms.
//!
//! `Scope`, `Telescope`, `Var`, the `Bound` traversal trait, and the `Visit`
//! driver operate over `core`'s `Term` and `Subterm`. `core` keeps its own
//! `Subterm::traverse` (the big structural match, including its primitives) and
//! plugs it into this machinery by implementing `Bound`.

use {
    super::{
        Context, Error, Free, Global, Level, LevelHead, MetaId, Subterm, Term, UniverseError,
        UniverseMetaId, UniverseParam, UniverseSolver, zonk_term,
    },
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet, HashMap},
        convert::Infallible,
        fmt,
        hash::Hash,
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
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct One;

impl One {
    /// The binder count as a constant, so `Params` can be the fixed-size array type `[&T; 1]`.
    pub const ARITY: usize = 1;
}

impl Arity for One {
    type Params<'a, T: ?Sized + 'a> = &'a [&'a T; Self::ARITY];

    fn arity(&self) -> usize {
        Self::ARITY
    }
}

/// The static two-binder arity — the `(pred, ih)` successor arm of the `Nat` eliminator.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Two;

impl Two {
    /// The binder count as a constant, so `Params` can be the fixed-size array type `[&T; 2]`.
    pub const ARITY: usize = 2;
}

impl Arity for Two {
    type Params<'a, T: ?Sized + 'a> = &'a [&'a T; Self::ARITY];

    fn arity(&self) -> usize {
        Self::ARITY
    }
}

/// The static three-binder arity — the `(head, tail, ih)` cons arms of the `Bin`/`Lst` eliminators.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Three;

impl Three {
    /// The binder count as a constant, so `Params` can be the fixed-size array type `[&T; 3]`.
    pub const ARITY: usize = 3;
}

impl Arity for Three {
    type Params<'a, T: ?Sized + 'a> = &'a [&'a T; Self::ARITY];

    fn arity(&self) -> usize {
        Self::ARITY
    }
}

/// A runtime-chosen binder count, for scopes whose arity is data-dependent (inductive-match arms over constructor payloads, `Rec` blocks, motives). `close`/`open` fall back to slices and assert the length instead of getting it checked at compile time.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Many(pub usize);

impl Arity for Many {
    type Params<'a, T: ?Sized + 'a> = &'a [&'a T];

    fn arity(&self) -> usize {
        self.0
    }
}

// === Var =====================================================================

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
enum VarType {
    Free(Free),
    Bound(usize),
}

/// A locally-nameless variable: free (a [`Free`] identity naming a Γ assumption or global definition) or bound (a de Bruijn index into enclosing [`Scope`]s). The bound form and its accessors are crate-internal — outside code builds free variables and lets the scope machinery convert them.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
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

    pub(crate) fn bound(index: usize) -> Self {
        Self {
            type_: VarType::Bound(index),
        }
    }

    pub(crate) fn as_bound(&self) -> Option<usize> {
        match &self.type_ {
            VarType::Free(_) => None,
            &VarType::Bound(index) => Some(index),
        }
    }

    /// This occurrence's identity, asserting it is free. Callers hold a term the
    /// scope machinery has not closed over, where a bound index is an invariant
    /// violation rather than a case to handle.
    pub(crate) fn unwrap(&self) -> &Free {
        self.as_free().expect("a free occurrence")
    }
}

// === Bound ===================================================================

/// A syntactic category the de Bruijn machinery can operate on: anything that can rebuild itself under a variable-visiting [`Visit`] and report its `reach`. Implemented by `Term`/`Subterm` (the big structural match lives in `term.rs`), [`Telescope`], and `()` (a Σ-telescope's trailing payload); everything else here — `shift`, `capture`, `release`, `free_vars` — is derived from `traverse` alone.
pub trait Bound: Sized + Clone + Eq + Hash + fmt::Debug {
    /// Rebuild the term, invoking the visit callback at every variable with the binder depth it sits under; a `Some(replacement)` substitutes that variable. The single primitive the rest of the trait is defined from — implementations must route subterms through `Visit::visit_subterm`/`visit_scope` so depth tracking, pruning, and the rewrite hook fire.
    fn traverse<F, S: SharingPolicy>(&self, visit: &mut Visit<F, S>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>;

    /// Number of outer de Bruijn binders this term depends on: `1 + max escaping
    /// bound index`, or `0` if none. A term with `reach <= depth` contains no
    /// bound index `>= depth`, so `shift`/`release` at that depth are the
    /// identity on it.
    fn reach(&self) -> usize;

    /// Whether an elaboration metavariable occurs in this value.
    fn has_metavar(&self) -> bool;

    /// `true` iff the term has no loose de Bruijn indices — i.e. it's not
    /// floating inside some outer scope.
    fn closed(&self) -> bool {
        self.reach() == 0
    }

    /// De Bruijn weakening: add `amount` to every loose bound index (`>= depth`), making room for that many new enclosing binders when a term is moved under them. Index-monotonic, so the traversal prunes by `reach`.
    fn shift(&self, amount: usize) -> Self {
        self.traverse(&mut Visit::pruning(|depth, var| {
            var.as_bound()
                .filter(|&index| index >= depth)
                .map(|index| Subterm::Var(Var::bound(index + amount)))
        }))
    }

    /// The closing half of the locally-nameless discipline: turn free occurrences of `binders` into bound indices (position in `binders`, offset by the current depth) while shifting already-loose indices past the new binders. `Scope::close` is this plus the name bookkeeping. Rewrites *free* names, so it can never be pruned by `reach`.
    fn capture(&self, binders: &[&Free]) -> Self {
        self.traverse(&mut Visit::new(|depth, var| {
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

impl Bound for () {
    fn traverse<F, S: SharingPolicy>(&self, _: &mut Visit<F, S>) -> Self
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
}

pub(crate) fn rewrite_universe_levels<B: Bound, E: 'static>(
    value: &B,
    rewrite: impl FnMut(&Level) -> Result<Level, E> + 'static,
) -> Result<B, E> {
    let mut rewrite = rewrite;
    rewrite_universe_levels_scoped(value, move |_, level| rewrite(level))
}

/// Structural implementation used only by the validated Core-to-Ersd
/// projection. Nominal vectors, instances, and contexts are removed by their
/// owning nodes. `Type` must still carry a `Level` in Core, so its now-irrelevant
/// payload is rebuilt with Core's private canonical ground representative.
pub(crate) fn project_erased_universes<B: Bound>(value: &B) -> B {
    value.traverse(&mut Visit::erasing_universes(|_, _| None))
}

pub(crate) fn rewrite_universe_levels_scoped<B: Bound, E: 'static>(
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

fn shift_universe_params(level: &Level, amount: usize) -> Result<Level, UniverseError> {
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
/// Universe parameters are innermost-first: beneath the `depth` universe
/// binders this walk has crossed, the scheme's own parameters occupy
/// `depth .. depth + arguments.len()`. An index above that range belongs to an
/// *enclosing* scheme and is shifted down by the parameters this instantiation
/// discharges, exactly as [`UniverseSolver::instantiate_at`] rewrites the outer
/// references in a nested residual context.
///
/// Instance arity is the owning [`UniverseContext`]'s contract and is checked
/// against its declared `parameter_count`. Rejecting an out-of-range index here
/// instead would misread every legitimate outer-scheme reference as a missing
/// argument.
pub(crate) fn instantiate_universe_levels_scoped<B: Bound>(
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

pub(crate) fn zonk_universe_levels_scoped<B: Bound>(
    value: &B,
    solver: &UniverseSolver,
) -> Result<B, UniverseError> {
    fn zonk_solution(
        solver: &UniverseSolver,
        level: &Level,
        visiting: &mut BTreeSet<UniverseMetaId>,
    ) -> Result<Level, UniverseError> {
        let mut replacements = BTreeMap::new();
        for meta in level.metas() {
            if let Some(solution) = solver.solution(meta)
                && visiting.insert(meta)
            {
                let zonked = zonk_solution(solver, solution, visiting)?;
                visiting.remove(&meta);
                replacements.insert(meta, zonked);
            }
        }
        level.substitute(|head| match head {
            LevelHead::Param(_) => None,
            LevelHead::Meta(meta) => replacements.get(&meta).cloned(),
        })
    }

    let solver = solver.clone();
    rewrite_universe_levels_scoped(value, move |depth, level| {
        let mut replacements = BTreeMap::new();
        for meta in level.metas() {
            if let Some(solution) = solver.solution(meta) {
                let zonked = zonk_solution(&solver, solution, &mut BTreeSet::from([meta]))?;
                replacements.insert(meta, shift_universe_params(&zonked, depth)?);
            }
        }
        level.substitute(|head| match head {
            LevelHead::Param(_) => None,
            LevelHead::Meta(meta) => replacements.get(&meta).cloned(),
        })
    })
}

pub(crate) fn universe_metas<B: Bound>(value: &B) -> BTreeSet<UniverseMetaId> {
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
pub(crate) enum SelfReference {
    /// Still a free variable. Nothing else will supply the instance, so the
    /// occurrence must carry one explicitly: a later use site instantiates the
    /// stored scheme by substituting the declaration's universe parameters,
    /// and a bare variable has none to substitute.
    Free,
    /// Already captured by an enclosing [`RecGroup`]'s binder, which
    /// instantiates its own members through
    /// [`RecGroup::instantiate_universes`]. An explicit instance here would be
    /// applied a second time when the group is opened, against a group whose
    /// parameters that first instantiation already consumed.
    Bound,
}

/// Rewrite every occurrence of a declaration group's own members to denote the
/// universe instance `levels`.
///
/// A declaration's signature, body, and registry telescopes are elaborated
/// before its universe parameters exist: within its own group it is
/// monomorphic, so its self-references carry no instance at all. Finalization
/// mints the parameters, and every internal occurrence must then denote *that*
/// instance rather than a freshly instantiated one — the concrete form of the
/// rule that recursion is monomorphic inside a group.
///
/// Nominal normal forms always carry the instance in their own universe
/// vector. Variable occurrences carry it only when they are still
/// [`SelfReference::Free`].
///
/// The per-node rule is [`Term::stamp_declaration_node`]; this driver only
/// carries it through the binders and telescopes an arbitrary [`Bound`] holds.
/// An empty instance is the identity, so a monomorphic declaration pays a
/// single comparison rather than a traversal.
pub(crate) fn stamp_declaration_instance<B: Bound>(
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
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Scope<A: Arity, B: Bound = Term> {
    arity: A,
    names: Option<Vec<Free>>,
    body: Box<B>,
}

impl<A: Arity, B: Bound> Scope<A, B> {
    pub(crate) fn close<'a>(arity: A, binders: A::Params<'a, Free>, body: B) -> Self {
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

    pub(crate) fn arity(&self) -> usize {
        self.arity.arity()
    }

    pub(crate) fn body(&self) -> &B {
        &self.body
    }

    pub(crate) fn names(&self) -> Option<&[Free]> {
        self.names.as_deref()
    }

    pub(crate) fn reach(&self) -> usize {
        self.body.reach().saturating_sub(self.arity())
    }

    pub(crate) fn open<'a>(&self, terms: A::Params<'a, Term>) -> B {
        assert!(
            self.arity() == terms.as_ref().len(),
            "scope arity mismatch in `open`: expected {}, got {}",
            self.arity(),
            terms.as_ref().len()
        );

        self.body.release(terms.as_ref())
    }

    pub(crate) fn constant(arity: A, body: B) -> Self {
        Self {
            arity,
            names: None,
            body: body.into(),
        }
    }

    /// Rebuild this scope with `f` applied to its body, preserving arity and
    /// binder names. The body keeps its de Bruijn structure, so `f` must be a
    /// transformation that does not disturb loose indices — e.g. zonking, which
    /// only replaces closed metavariable nodes by closed solutions, or a
    /// canonicalization, which replaces a node by a structurally equal one.
    ///
    /// Rewriting here rather than opening and re-closing is what keeps a term's
    /// memoized derivations: `open` and `close` each rebuild every node they
    /// touch, so the round trip discards all of them to arrive where this
    /// arrives without moving.
    pub(crate) fn map_body(&self, f: impl FnOnce(&B) -> B) -> Self {
        Self {
            arity: self.arity,
            names: self.names.clone(),
            body: f(&self.body).into(),
        }
    }

    /// Fallible [`Self::map_body`], for a rewrite that can reject its input.
    pub(crate) fn try_map_body<E>(&self, f: impl FnOnce(&B) -> Result<B, E>) -> Result<Self, E> {
        Ok(Self {
            arity: self.arity,
            names: self.names.clone(),
            body: f(&self.body)?.into(),
        })
    }

    /// The identity of the binder at position `index` (0 = first/outermost),
    /// for a rebuild that must re-close over the very same binders.
    pub(crate) fn binder(&self, index: usize) -> Option<&Free> {
        self.names.as_deref()?.get(index)
    }

    /// What the binder at position `index` was called where it was written — a
    /// rendering aid a rebuild carries onto the binder it re-mints, never a way
    /// to recognize which binder this is.
    pub(crate) fn hint(&self, index: usize) -> Option<&str> {
        self.binder(index)?.hint()
    }

    pub(crate) fn first_hint(&self) -> Option<&str> {
        self.hint(0)
    }

    pub(crate) fn second_hint(&self) -> Option<&str> {
        self.hint(1)
    }

    pub(crate) fn third_hint(&self) -> Option<&str> {
        self.hint(2)
    }

    pub(crate) fn hint_iter(&self) -> impl Iterator<Item = Option<&str>> {
        (0..self.arity()).map(move |index| self.hint(index))
    }

    /// The identity of each binder in order, `None` where the scope was built
    /// without them (`constant`).
    pub(crate) fn binder_iter(&self) -> impl Iterator<Item = Option<&Free>> {
        (0..self.arity()).map(move |index| self.binder(index))
    }

    /// Whether the binder at position `index` (0 = first/outermost label) is
    /// referenced anywhere in the body. A bound var refers to this binder iff its
    /// de Bruijn index equals `index` plus the number of binders entered since —
    /// which `Visit` tracks as `depth`. Used by erasure to spot an eliminator
    /// whose induction hypothesis is dead: that arm is a case-split, not a fold.
    pub(crate) fn uses(&self, index: usize) -> bool {
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
    /// Prepend `binder` to the front of this scope: it becomes index 0 and every
    /// existing binder shifts up by one.
    ///
    /// Done by a direct `capture` on the body — free occurrences of `binder`
    /// bind to the new index 0 while every existing bound index shifts by one —
    /// rather than an open/close round-trip through names, which would have to
    /// reopen inner binders into free occurrences and could not tell them from
    /// genuine outer references.
    pub(crate) fn prepend(&self, binder: &Free) -> Self {
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
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
#[cfg_attr(
    feature = "archive",
    rkyv(
        serialize_bounds(__S: rkyv::ser::Writer + rkyv::ser::Allocator + rkyv::ser::Sharing, __S::Error: rkyv::rancor::Source),
        deserialize_bounds(__D: rkyv::de::Pooling, __D::Error: rkyv::rancor::Source),
        bytecheck(bounds(__C: rkyv::validation::ArchiveContext + rkyv::validation::SharedContext, __C::Error: rkyv::rancor::Source))
    )
)]
pub enum Telescope<B: Bound> {
    Done(Box<B>),
    Cons(
        Term,
        #[cfg_attr(feature = "archive", rkyv(omit_bounds))] Scope<One, Telescope<B>>,
    ),
}

impl<B: Bound> Telescope<B> {
    pub(crate) fn done(body: B) -> Self {
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

    pub(crate) fn len(&self) -> usize {
        let mut n = 0;
        let mut cur = self;
        while let Telescope::Cons(_, rest) = cur {
            n += 1;
            cur = &rest.body;
        }
        n
    }

    pub(crate) fn is_empty(&self) -> bool {
        matches!(self, Telescope::Done(_))
    }

    /// The final payload beneath every binder, without opening the telescope or
    /// substituting for any of its bound variables.
    pub(crate) fn terminal(&self) -> &B {
        let mut current = self;
        loop {
            match current {
                Telescope::Done(body) => return body,
                Telescope::Cons(_, rest) => current = &rest.body,
            }
        }
    }

    /// The binder hint at each position (`""` when unnamed), walking the spine
    /// without opening — names are structural, no substitution needed.
    pub(crate) fn labels(&self) -> Vec<&str> {
        let mut out = Vec::new();
        let mut cur = self;
        while let Telescope::Cons(_, rest) = cur {
            out.push(rest.first_hint().unwrap_or_default());
            cur = &rest.body;
        }
        out
    }

    /// Replace the display hints along the spine, leaving each binder's identity
    /// alone. Pure metadata: the de Bruijn structure is untouched and no
    /// occurrence changes what it refers to — this restores source labels after
    /// a rebuild that had to re-mint its binders (tuple-type labels are part of
    /// the type's identity and the target of `.label` resolution, so they must
    /// survive elaboration verbatim).
    pub(crate) fn relabel(self, labels: &[&str]) -> Self {
        match self {
            Telescope::Done(body) => Telescope::Done(body),
            Telescope::Cons(ty, rest) => {
                let (label, rest_labels) = labels.split_first().expect("relabel arity");
                let names = rest
                    .names
                    .as_ref()
                    .map(|names| names.iter().map(|name| name.relabelled(label)).collect());
                let Scope { arity, body, .. } = rest;
                Telescope::Cons(
                    ty,
                    Scope {
                        arity,
                        names,
                        body: Box::new((*body).relabel(rest_labels)),
                    },
                )
            }
        }
    }

    pub(crate) fn open(&self, args: &[&Term]) -> B {
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

    /// Open the leading binders at successive `params` — one binder per param —
    /// returning the residual telescope. Every caller's telescope leads with the
    /// type parameters (constructor payloads, struct fields, inductive indices all
    /// follow them), so a telescope that runs out early is an invariant violation.
    pub(crate) fn open_params(self, params: &[Term]) -> Telescope<B> {
        let mut telescope = self;
        for param in params {
            telescope = match telescope {
                Telescope::Cons(_, rest) => rest.open(&[param]),
                Telescope::Done(_) => unreachable!("telescope must lead with its parameters"),
            };
        }
        telescope
    }

    /// Open the telescope across `args`, invoking `f(arg, ty)` at each binder
    /// before substituting that arg into the rest, and return the final `Done`
    /// body. The walk is infallible; the error type `E` belongs to the callback.
    pub(crate) fn walk<F, E>(self, args: &[Term], mut f: F) -> Result<B, E>
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

    pub(crate) fn nth<F>(self, index: usize, mut sub: F) -> Option<Term>
    where
        F: FnMut(usize) -> Term,
    {
        fn go<B: Bound, F: FnMut(usize) -> Term>(
            tele: Telescope<B>,
            index: usize,
            j: usize,
            sub: &mut F,
        ) -> Option<Term> {
            match tele {
                Telescope::Done(_) => None,
                Telescope::Cons(ty, rest) => {
                    if j == index {
                        Some(ty)
                    } else {
                        go(rest.open(&[&sub(j)]), index, j + 1, sub)
                    }
                }
            }
        }

        go(self, index, 0, &mut sub)
    }
}

impl Telescope<Term> {
    /// Whether any metavariable in a function/Π telescope (`Func`/`FuncType`) —
    /// the parameter types and the trailing body/return type — satisfies
    /// `pred`, short-circuiting on the first hit.
    pub(crate) fn any_metavar<F: FnMut(MetaId) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Telescope::Cons(ty, rest) => ty.any_metavar(pred) || rest.body().any_metavar(pred),
            Telescope::Done(body) => body.any_metavar(pred),
        }
    }

    /// Whether any `Term` in a function/Π telescope (`Func`/`FuncType`) — the
    /// parameter types and the trailing body/return type — satisfies `pred`,
    /// short-circuiting on the first hit. The telescope leg of
    /// `Subterm::any_child_term`: `pred` carries the per-node memoized
    /// recursion, so this visits each `Term` exactly once.
    pub(crate) fn any_term<F: FnMut(&Term) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Telescope::Cons(ty, rest) => pred(ty) || rest.body().any_term(pred),
            Telescope::Done(body) => pred(body),
        }
    }

    /// Zonk a function/Π telescope (`Func`/`FuncType`): its parameter types and
    /// its trailing body/return type, which is a real term to recurse into.
    pub(crate) fn zonk(&self, context: &Context) -> Result<Self, Error> {
        match self {
            Telescope::Done(body) => Ok(Telescope::Done(zonk_term(context, body)?.into())),
            Telescope::Cons(ty, rest) => Ok(Telescope::Cons(
                zonk_term(context, ty)?,
                rest.try_map_body(|inner| inner.zonk(context))?,
            )),
        }
    }

    /// Walk a function/Π telescope (`Func`/`FuncType`): the parameter types and
    /// the trailing body/return type. Concrete in `Term` — no collector trait
    /// needed. See [`Subterm::collect_construction_names`](super::Subterm::collect_construction_names).
    pub(crate) fn collect_construction_names(&self, names: &mut BTreeSet<Global>) {
        match self {
            Telescope::Cons(ty, rest) => {
                ty.collect_construction_names(names);
                rest.body().collect_construction_names(names);
            }
            Telescope::Done(body) => body.collect_construction_names(names),
        }
    }
}

impl Telescope<()> {
    /// Whether any metavariable in a Σ telescope (`TupleType`) — only the field
    /// types; its `Done` body is `()` — satisfies `pred`, short-circuiting on
    /// the first hit.
    pub(crate) fn any_metavar<F: FnMut(MetaId) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Telescope::Cons(ty, rest) => ty.any_metavar(pred) || rest.body().any_metavar(pred),
            // The trailing body is `()`, which holds no metavariables.
            Telescope::Done(_) => false,
        }
    }

    /// Whether any `Term` in a Σ telescope (`TupleType`) — only the field
    /// types; its `Done` body is `()` — satisfies `pred`, short-circuiting on
    /// the first hit. See the `Telescope<Term>` counterpart above.
    pub(crate) fn any_term<F: FnMut(&Term) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Telescope::Cons(ty, rest) => pred(ty) || rest.body().any_term(pred),
            // The trailing body is `()`, which holds no terms.
            Telescope::Done(_) => false,
        }
    }

    /// Zonk a Σ telescope (`TupleType`): only its field types — its `Done` body
    /// is `()`, which carries no metavariables and is rebuilt as-is.
    pub(crate) fn zonk(&self, context: &Context) -> Result<Self, Error> {
        match self {
            Telescope::Done(_) => Ok(Telescope::Done(Box::new(()))),
            Telescope::Cons(ty, rest) => Ok(Telescope::Cons(
                zonk_term(context, ty)?,
                rest.try_map_body(|inner| inner.zonk(context))?,
            )),
        }
    }

    /// Walk a Σ telescope (`TupleType`): only the field types — its `Done` body
    /// is `()`, which contributes no names.
    pub(crate) fn collect_construction_names(&self, names: &mut BTreeSet<Global>) {
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

impl<B: Bound> Bound for Telescope<B> {
    fn traverse<F, S: SharingPolicy>(&self, visit: &mut Visit<F, S>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        match self {
            Telescope::Cons(ty, rest) => {
                Telescope::Cons(visit.visit_subterm(ty), visit.visit_scope(rest))
            }
            Telescope::Done(body) => Telescope::Done(body.traverse(visit).into()),
        }
    }

    fn reach(&self) -> usize {
        match self {
            Telescope::Cons(ty, rest) => ty.reach().max(rest.reach()),
            Telescope::Done(body) => body.reach(),
        }
    }

    fn has_metavar(&self) -> bool {
        match self {
            Telescope::Cons(ty, rest) => ty.has_metavar() || Bound::has_metavar(rest.body()),
            Telescope::Done(body) => body.has_metavar(),
        }
    }
}

// === Visit ===================================================================

/// Whether a traversal replaces each rebuilt node with the canonical node of
/// its structure, and if so, against which table.
///
/// A type parameter rather than a field, because whether a traversal hash-conses
/// is fixed where it is constructed and never changes mid-flight. [`NoSharing`]
/// is zero-sized and its `canonical` is a constant `None`, so every traversal
/// that does not cons — which is all of the kernel's — carries no field and pays
/// no branch.
pub trait SharingPolicy {
    /// The canonical node for `rebuilt`, or `None` when not consing.
    fn canonical(&self, rebuilt: &Term) -> Option<Term>;
}

/// The default: rebuild nodes as they come.
#[derive(Debug, Clone, Copy, Default)]
pub struct NoSharing;

impl SharingPolicy for NoSharing {
    fn canonical(&self, _rebuilt: &Term) -> Option<Term> {
        None
    }
}

/// A hash-consing table, shared rather than owned so one canonicalization spans
/// a whole module: two definitions that build the same type collapse onto one
/// node only if they consult the same table.
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
    /// One `Sharing` must span every snapshot being canonicalized together: the
    /// duplication worth collapsing is overwhelmingly *between* definitions, and
    /// between the lowered and elaborated views of the same prelude, so a table
    /// per term or per module would collapse almost none of it.
    pub fn share<B: Bound>(&self, value: &B) -> B {
        value.traverse(&mut Visit::sharing(|_, _| None, self.clone()))
    }

    /// Distinct structures adopted so far — the census this pass is justified by.
    pub fn structures(&self) -> usize {
        self.table.borrow().len()
    }
}

impl SharingPolicy for Sharing {
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

/// A term-level pre-hook for [`Visit`]: `Some(replacement)` substitutes the
/// whole node at the current depth.
type Rewrite = Box<dyn FnMut(usize, &Term) -> Option<Term>>;
type LevelRewrite = Box<dyn FnMut(usize, &Level) -> Level>;

/// The traversal driver threaded through [`Bound::traverse`]: it owns the current binder depth (bumped and restored by `visit_scope` as scopes are crossed), the variable callback, the pruning flag (skip subtrees whose `reach` proves the visit cannot touch them), and an optional term-level rewrite hook. `Visit::rewriting` is the crate-visible constructor; `Visit::new` and `Visit::pruning` are module-internal to this file.
pub struct Visit<F, S = NoSharing> {
    depth: usize,
    universe_depth: usize,
    prune: bool,
    visit: F,
    // An optional *term-level* pre-hook, consulted at every recursion point
    // before descending: `Some(replacement)` substitutes the whole node (and
    // is not descended into). Incompatible with pruning, which may skip the
    // very nodes the hook would match.
    rewrite: Option<Rewrite>,
    level_rewrite: Option<LevelRewrite>,
    erase_universes: bool,
    universes_only: bool,
    /// Pointer-keyed memo for a depth-independent rewriting visit: each input
    /// node is rebuilt once and every later occurrence of it reuses that
    /// result. Keys are addresses of *input* nodes, which the caller's value
    /// keeps alive for the whole traversal, so an address cannot be recycled
    /// under the memo.
    shared_memo: Option<HashMap<usize, Term>>,
    sharing: S,
}

impl<F> Visit<F>
where
    F: FnMut(usize, &Var) -> Option<Subterm>,
{
    pub(crate) fn new(visit: F) -> Self {
        Self {
            depth: 0,
            universe_depth: 0,
            prune: false,
            visit,
            rewrite: None,
            level_rewrite: None,
            erase_universes: false,
            universes_only: false,
            shared_memo: None,
            sharing: NoSharing,
        }
    }

    /// Like `new`, but lets a `Term::traverse` impl skip (and structurally
    /// share) subtrees the visit provably leaves unchanged. Only sound for
    /// index-monotonic visits whose effect depends solely on bound indices
    /// `>= depth` — i.e. `shift` and `release`. Must NOT be used for `capture`
    /// (rewrites free names) or `free_vars` (must observe every node).
    fn pruning(visit: F) -> Self {
        Self {
            depth: 0,
            universe_depth: 0,
            prune: true,
            visit,
            rewrite: None,
            level_rewrite: None,
            erase_universes: false,
            universes_only: false,
            shared_memo: None,
            sharing: NoSharing,
        }
    }

    /// Like `new`, additionally carrying a term-level rewrite hook fired at
    /// every [`Term::traverse`] entry, including terms that are the direct
    /// body of a scope or telescope terminal.
    pub(crate) fn rewriting(visit: F, rewrite: Rewrite) -> Self {
        Self {
            depth: 0,
            universe_depth: 0,
            prune: false,
            visit,
            rewrite: Some(rewrite),
            level_rewrite: None,
            erase_universes: false,
            universes_only: false,
            shared_memo: None,
            sharing: NoSharing,
        }
    }

    /// Like [`rewriting`](Self::rewriting), but memoized on node identity, so a
    /// structurally shared input stays shared in the output instead of being
    /// expanded into a tree.
    ///
    /// A rebuilt node is a fresh allocation, so an unmemoized rewrite of a DAG
    /// materializes its expansion: a lowered string literal shares one
    /// scan-state chain across every `more` link, and rebuilding it unshared
    /// costs O(n^2) nodes for an n-byte literal — which then makes every later
    /// pass over the term quadratic too.
    ///
    /// Only sound when the hook and the variable callback are pure and depend
    /// on the node alone — not on binder depth, and not on how many times they
    /// have run. A memoized visit skips both, so a depth-sensitive rewrite
    /// would silently reuse a result computed at the wrong depth, and a
    /// stateful hook would see each shared node once rather than once per
    /// occurrence.
    pub(crate) fn rewriting_shared(visit: F, rewrite: Rewrite) -> Self {
        Self {
            depth: 0,
            universe_depth: 0,
            prune: false,
            visit,
            rewrite: Some(rewrite),
            level_rewrite: None,
            erase_universes: false,
            universes_only: false,
            shared_memo: Some(HashMap::new()),
            sharing: NoSharing,
        }
    }

    pub(crate) fn rewriting_universes(visit: F, rewrite: Rewrite) -> Self {
        Self {
            depth: 0,
            universe_depth: 0,
            prune: false,
            visit,
            rewrite: Some(rewrite),
            level_rewrite: None,
            erase_universes: false,
            universes_only: true,
            shared_memo: None,
            sharing: NoSharing,
        }
    }

    pub(crate) fn rewriting_levels_scoped(visit: F, rewrite: LevelRewrite) -> Self {
        Self {
            depth: 0,
            universe_depth: 0,
            prune: false,
            visit,
            rewrite: None,
            level_rewrite: Some(rewrite),
            erase_universes: false,
            universes_only: true,
            shared_memo: None,
            sharing: NoSharing,
        }
    }

    fn erasing_universes(visit: F) -> Self {
        Self {
            depth: 0,
            universe_depth: 0,
            prune: false,
            visit,
            rewrite: None,
            level_rewrite: None,
            erase_universes: true,
            universes_only: true,
            shared_memo: None,
            sharing: NoSharing,
        }
    }
}

/// A hash-consing traversal: structure-preserving, but replacing every rebuilt
/// node with the canonical node of its shape.
///
/// The rebuild is already post-order — a node is constructed only after its
/// children are traversed — so consulting the table on the rebuilt node
/// canonicalizes bottom-up with no extra pass. Spans survive: they sit on the
/// `Term` wrapper, outside the shared node, so each occurrence keeps its own.
impl<F> Visit<F, Sharing>
where
    F: FnMut(usize, &Var) -> Option<Subterm>,
{
    pub(crate) fn sharing(visit: F, table: Sharing) -> Self {
        Self {
            depth: 0,
            universe_depth: 0,
            prune: false,
            visit,
            rewrite: None,
            level_rewrite: None,
            erase_universes: false,
            universes_only: false,
            shared_memo: Some(HashMap::new()),
            sharing: table,
        }
    }
}

impl<F, S: SharingPolicy> Visit<F, S>
where
    F: FnMut(usize, &Var) -> Option<Subterm>,
{
    /// The canonical node for a rebuilt term, or `None` when not consing.
    pub(crate) fn share_structure(&self, rebuilt: &Term) -> Option<Term> {
        self.sharing.canonical(rebuilt)
    }

    pub(crate) fn depth(&self) -> usize {
        self.depth
    }

    pub(crate) fn prune(&self) -> bool {
        self.prune
    }

    /// Enter `amount` binders without visiting a whole scope body in one call
    /// — the peeled-chain counterpart of `visit_scope`, for a `Bound::traverse`
    /// impl that walks a `Let`/`Rec` spine one link at a time in a loop instead
    /// of recursing once per binding. Pair with `leave_scope` in the reverse
    /// order links were entered.
    pub(crate) fn enter_scope(&mut self, amount: usize) {
        self.depth += amount;
    }

    pub(crate) fn leave_scope(&mut self, amount: usize) {
        self.depth -= amount;
    }

    pub(crate) fn enter_universe_scope(&mut self, amount: usize) {
        self.universe_depth += amount;
    }

    pub(crate) fn leave_universe_scope(&mut self, amount: usize) {
        self.universe_depth -= amount;
    }

    /// Invoke the underlying visit callback on a variable at the current depth.
    pub(crate) fn call(&mut self, var: &Var) -> Option<Subterm> {
        (self.visit)(self.depth, var)
    }

    pub(crate) fn visit_level(&mut self, level: &Level) -> Level {
        if self.erase_universes {
            // Every other level-bearing container is removed structurally in
            // `Subterm::traverse`; this is the unavoidable payload of Core's
            // still-level-indexed `Type` variant, not an erasure sentinel.
            return Level::zero();
        }
        match &mut self.level_rewrite {
            Some(rewrite) => rewrite(self.universe_depth, level),
            None => level.clone(),
        }
    }

    pub(crate) fn rewrite_term(&mut self, term: &Term) -> Option<Term> {
        self.rewrite
            .as_mut()
            .and_then(|rewrite| rewrite(self.depth, term))
    }

    pub(crate) fn erases_universes(&self) -> bool {
        self.erase_universes
    }

    pub(crate) fn universes_only(&self) -> bool {
        self.universes_only
    }

    pub(crate) fn memoizes(&self) -> bool {
        self.shared_memo.is_some()
    }

    pub(crate) fn memo_get(&self, key: usize) -> Option<Term> {
        self.shared_memo.as_ref()?.get(&key).cloned()
    }

    pub(crate) fn memo_put(&mut self, key: usize, term: Term) {
        if let Some(memo) = self.shared_memo.as_mut() {
            memo.insert(key, term);
        }
    }

    pub(crate) fn rewrites_terms(&self) -> bool {
        self.rewrite.is_some()
    }

    pub(crate) fn visit_subterm(&mut self, term: &Term) -> Term {
        term.traverse(self)
    }

    pub(crate) fn visit_scope<A: Arity, B: Bound>(&mut self, scope: &Scope<A, B>) -> Scope<A, B> {
        self.depth += scope.arity.arity();
        let body = scope.body.traverse(self).into();
        self.depth -= scope.arity.arity();

        Scope {
            arity: scope.arity,
            names: scope.names.clone(),
            body,
        }
    }
}
