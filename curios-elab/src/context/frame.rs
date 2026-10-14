//! One frame of the context: everything a binder scope holds, as one value.
//!
//! A frame is entered where a scope opens and left where it closes, and what a lookup reads of the context it reads of the frames that stand, innermost first ([`Frames`](super::Frames)). Each holds its assumptions, its definitions, its refinements of a variable, of a projection and of a stuck application, the reduced spelling each of the last was settled to, why its arm is dead where it is, and its locals and witness binders in the order they were bound.
//!
//! **A frame is shared.** The context holds each behind a count, and so does what waits in it ([`HeldFrames`]): a parked problem where it parks, and a metavariable where it is born. A frame written to while something else holds it is copied for the writer first ([`Frames`](super::Frames) writes through `Rc::make_mut`), so a holder has what stood and nothing written since. What a frame remembers of its guards' reduced spellings is the one thing written through a shared frame, since it is the frame's wherever the frame is held.
//!
//! **A frame is numbered where it is entered**, and a copy made for a writer keeps the number: it is the same frame with more written to it. So whether the frames a metavariable was born under still stand is read off the numbers, outermost first.
//!
//! **A frame may hold a name twice.** An arm assumes its own payload binder again at the type its case's solution gives it (`retype_locals`), so the locals are a list, read by the later entry, with the assumptions beside them by name.

use {
    crate::{DeadArm, UniverseStateToken},
    curios_core::{DefinitionKind, Free, Term, UniverseContext},
    curios_utilities::Keyed,
    std::{cell::RefCell, collections::HashMap, hash::Hash, rc::Rc},
};

/// One definition: the definiens, plus the [`DefinitionKind`] of the module item that introduced it. Every `DefEntry` — whether a plain `let`/`rec` member or mid-window rec-group registration — is treated uniformly; there is no `recursive` marker distinguishing them.
///
/// `kind` is `None` for a genuine *local* binding — a `let` binder, an opened match scrutinee, a lambda parameter — which no module item declared. It is carried rather than re-derived: the kind is elaboration metadata `into_core` attached where the item was generated, and splitting the definition's name apart to recover it would misread an ordinary definition that merely happens to sit under a generated namespace (see [`DefinitionKind`]'s own docs).
#[derive(Debug, Clone)]
pub(crate) struct DefEntry {
    term: Term,
    kind: Option<DefinitionKind>,
    /// Whether the name is a proof its item published ([`curios_core::Definition::proof`]): it stays bound to `term`, which is what says which group it is a member of, and unfolds to nothing.
    proof: bool,
}

impl DefEntry {
    pub(crate) fn new(term: Term, kind: Option<DefinitionKind>) -> Self {
        Self {
            term,
            kind,
            proof: false,
        }
    }

    /// The entry a published item's name is bound by: `term`, and a proof where `proof`.
    pub(crate) fn published(term: Term, kind: DefinitionKind, proof: bool) -> Self {
        Self {
            term,
            kind: Some(kind),
            proof,
        }
    }

    pub(crate) fn body(&self) -> &Term {
        &self.term
    }

    pub(crate) fn kind(&self) -> Option<&DefinitionKind> {
        self.kind.as_ref()
    }

    /// What the name unfolds to: its term, and nothing where it is a proof.
    pub(super) fn reduct(&self) -> Option<&Term> {
        (!self.proof).then_some(&self.term)
    }
}

/// One stuck-application refinement: the scrutinee as *written* (unerased, so a settlement can still unfold its polymorphic heads — erasure strips the `Instance` a global unfolds through, so reduce-then-erase and erase-then-reduce disagree exactly there), and the arm's value.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ScrutineeEntry {
    pub(crate) original: Term,
    pub(crate) value: Term,
    /// Whether this spelling is an alias of an equation recorded under another: the kernel's spelling of a guard written over local definitions, which an occurrence reached by unfolding a definition presents exactly. The exact lookup reads it; settlement skips it, which leaves a reduct over the definitions' values unanswered — `typing`'s registration states the case.
    pub(crate) alias: bool,
    /// Whether this entry withholds the equation its key holds in a frame outside, rather than recording one: an arm that refines a variable leaves it under every equation whose scrutinee, as the kernel spells it under the arm's solution, names no local. `Context::withhold_closed_equations` states the rule. It keeps the equation's spelling and value, which a report on the arm names.
    pub(crate) withheld: bool,
}

/// A scrutinee entry's reduced spelling once settled: the form a probe is compared at — solved metavariables materialized and universe instances erased, once, when it settles — and the unerased reduct a hit reads its instance from.
#[derive(Debug, Clone)]
pub(crate) struct Settled {
    pub(crate) compared: Term,
    pub(crate) unerased: Term,
}

/// What a settlement left for one scrutinee entry.
#[derive(Debug, Clone)]
pub(super) struct Spelling {
    /// The reduced spelling, or `None` where reducing the key refused or outran its allowance.
    pub(super) settled: Option<Settled>,
    /// How many solutions had been committed when it settled, where the key or the spelling held an unsolved metavariable — `None` where neither did, and no solution can change what the key reduces to.
    pub(super) solved: Option<usize>,
    /// The universe solver's state when it settled, where a question put to conversion on the way was declined for a commit: the spelling is then what the key reduces to only while that state stands.
    pub(super) levels: Option<UniverseStateToken>,
    /// How many times every spelling had been forgotten at once when it settled ([`Frames::forget_spellings`](super::Frames::forget_spellings)): it is what its key reduces to only while that count stands.
    pub(super) forgotten: usize,
}

/// One projection refinement: the base as *written* (unerased, so a probe at another universe instance can be told apart from the spelling the arm actually scrutinized), and the arm's value.
///
/// The key beside it is universes-erased, which is what lets two occurrences of one polymorphic base merge while their instances are still undecided. Erasure cannot tell one instance from another, so the base is kept whole and [`Context::proj_reduct`](crate::Context) holds the two to one instance at the read.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ProjectionEntry {
    pub(crate) original: Term,
    pub(crate) value: Term,
}

/// One frame's definitions, or its refinements of one kind, read back in the order they were recorded.
///
/// A hash map's own order differs from one process to the next, and a frame's entries are read in order: the guards a proof the elaborator writes is built from are admitted as they are met, so a frame holding two would yield two proofs of one bound, by the run; and a report names an unfolded `rec` group by the first definition it finds of that structure, so two groups of one structure would be spelled either way.
#[derive(Debug, Clone)]
pub(super) struct Recorded<K, V> {
    places: HashMap<K, usize>,
    entries: Vec<(K, V)>,
}

impl<K: Eq + Hash + Clone, V> Recorded<K, V> {
    fn new() -> Self {
        Self {
            places: HashMap::new(),
            entries: Vec::new(),
        }
    }

    pub(super) fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub(super) fn get(&self, key: &K) -> Option<&V> {
        self.places.get(key).map(|&place| &self.entries[place].1)
    }

    pub(super) fn contains_key(&self, key: &K) -> bool {
        self.places.contains_key(key)
    }

    /// Record `value` under `key`, in the place the key already holds where it was recorded before.
    pub(super) fn insert(&mut self, key: K, value: V) {
        match self.places.get(&key) {
            Some(&place) => self.entries[place].1 = value,
            None => {
                self.places.insert(key.clone(), self.entries.len());
                self.entries.push((key, value));
            }
        }
    }

    pub(super) fn iter(&self) -> impl DoubleEndedIterator<Item = (&K, &V)> {
        self.entries.iter().map(|(key, value)| (key, value))
    }

    pub(super) fn keys(&self) -> impl Iterator<Item = &K> {
        self.entries.iter().map(|(key, _)| key)
    }

    /// Whether the two recorded the same entries in the same order.
    pub(super) fn same_as(&self, other: &Self) -> bool
    where
        K: PartialEq,
        V: PartialEq,
    {
        self.entries == other.entries
    }

    /// Whether every entry here is one `others` recorded, in any of them.
    fn within<'a>(&self, others: impl Iterator<Item = &'a Self> + Clone) -> bool
    where
        K: PartialEq + 'a,
        V: PartialEq + 'a,
    {
        self.entries
            .iter()
            .all(|entry| others.clone().any(|other| other.entries.contains(entry)))
    }

    /// Forget `key`, the entries recorded after it each moving up one place.
    pub(super) fn remove(&mut self, key: &K) {
        let Some(place) = self.places.remove(key) else {
            return;
        };
        self.entries.remove(place);
        for (later, _) in &self.entries[place..] {
            *self.places.get_mut(later).expect("an entry has a place") -= 1;
        }
    }
}

/// The projection a refinement is registered under, its base's universe instances erased: told apart from another by its hash, so a probe compares no base but the one it asks for.
pub(super) type Projected = Keyed<(Term, usize)>;

/// One frame: what the scope it was entered for holds. See the module documentation.
#[derive(Debug, Clone)]
pub(crate) struct Frame {
    /// The number the frame took where it was entered, which no other frame of the elaboration takes: a copy made for a writer keeps it.
    pub(super) number: u64,
    pub(super) assumptions: HashMap<Free, Term>,
    pub(super) assumption_universes: HashMap<Free, UniverseContext>,
    pub(super) definitions: Recorded<Free, DefEntry>,
    /// Counterfactual match-arm refinements of a variable (`refine_head`), kept beside the definitions but suppressible: re-validation of a metavariable solution must keep stable definitions yet ignore these.
    pub(super) refinements: Recorded<Free, Term>,
    pub(super) refinement_projections: Recorded<Projected, ProjectionEntry>,
    /// Counterfactual refinements keyed by a *stuck application* scrutinee — a non-key match head (`classify(c)`, `Nat/in_range(...)`) that `refine_head` could not record. Keyed by the scrutinee as written, its metavariables and universes normalized (`reduce`'s `shallow_scrutinee`); an occurrence that surfaces spelled differently is met at the entry's reduced spelling (`scrutinee_spellings`). The term-keyed analogue of the two stores above, suppressed by the same flag.
    pub(super) refinement_scrutinees: Recorded<Term, ScrutineeEntry>,
    /// Each scrutinee entry's *reduced* spelling once a probe has asked for it, under its key — `reduce::refined_reduct`'s memo, the elaborator's copy of the kernel's per-entry reduct.
    ///
    /// **It lives as long as its entry, and is forgotten by what can change what its key reduces to.** A spelling is settled with its entry's frame and every frame inside it withheld, and read only through the window, so it rests on the refinements of the frames outside its entry — which cannot change while the entry stands: a registration lands in the innermost frame, and a frame leaves after every frame inside it — and on what reduction reads of everything else: the definitions, the solutions and the universe levels. So a registration and the exit of an inner frame leave it alone, and it is forgotten with its frame, where its key is registered again, at a redefinition, a rollback and a universe rewrite, and at a declaration's boundary, which nothing a budget paid for outlives: each of those advances a count every spelling is stamped with, so a frame only a parked problem holds forgets as the live ones do. A fresh definition forgets the spellings naming it, as it does the reducts; and one settled while its key or its reduct held an unsolved metavariable is asked for again once a solution has landed.
    ///
    /// **It is the frame's, behind a cell.** The live context and every judgment that stands on the frame read one memo and settle a key once between them: a spelling rests on the frames below its own, and a frame stands on the same ones wherever it is held.
    ///
    /// **Settled once for each of the two reductions**, a table for each (`Context::plainly`): a probe is compared with the spelling reduced the way the probe was, as the kernel settles an equation's reduct once for each.
    pub(super) scrutinee_spellings: RefCell<[HashMap<Term, Spelling>; 2]>,
    /// Why the frame's arm is never taken, where the frame is an arm its scrutinee's indices rule out — `None` for every other frame. The reason is known where the arm opens, and an implicit bound nothing discharges under it is reported once the arm has closed, so it is kept here for the metavariable born there to read ([`HeldFrames::dead_arm`]).
    pub(super) dead_arm: Option<Rc<DeadArm>>,
    /// The guard and the case of the arm the frame is, where it is an arm that refines its scrutinee to a case — `None` for every other frame. Whether a value reaches such an arm is found by reducing its guard, which nothing does where the arm opens: it is asked where an error leaves the arm, and where a bound nothing discharges under it is given up ([`HeldFrames::arms`]).
    pub(super) arm: Option<(Term, Term)>,
    /// Whether the frame is a function's body, whose result type is the region a `!` within it sequences in. Known where the body opens, and read where a `!` there is refused, which may be a retry standing on the frame.
    pub(super) body: bool,
    /// The frame's assumptions in binding order, a companion to `assumptions`, which is keyed by name and loses order.
    pub(super) locals: Vec<(Free, Term)>,
    /// The frame's `use`-plicity binders in binding order, a subset of `locals`: resolution's search space, scanned innermost first.
    pub(super) witnesses: Vec<(Free, Term)>,
}

impl Frame {
    pub(super) fn new(number: u64) -> Self {
        Self {
            number,
            assumptions: HashMap::new(),
            assumption_universes: HashMap::new(),
            definitions: Recorded::new(),
            refinements: Recorded::new(),
            refinement_projections: Recorded::new(),
            refinement_scrutinees: Recorded::new(),
            scrutinee_spellings: Default::default(),
            dead_arm: None,
            arm: None,
            body: false,
            locals: Vec::new(),
            witnesses: Vec::new(),
        }
    }

    /// Whether the frame registers a refinement of any kind.
    pub(super) fn refined(&self) -> bool {
        !self.refinements.is_empty()
            || !self.refinement_projections.is_empty()
            || !self.refinement_scrutinees.is_empty()
    }

    /// Whether the two frames register the same refinements, of every kind and in the same order.
    fn refined_as(&self, other: &Frame) -> bool {
        self.refinements.same_as(&other.refinements)
            && self
                .refinement_projections
                .same_as(&other.refinement_projections)
            && self
                .refinement_scrutinees
                .same_as(&other.refinement_scrutinees)
    }
}

/// Whether every refinement `these` frames register, of every kind, is one `those` register: what containment between two contexts asks, since a solution resting on a guard is scoped to the arm the guard opens. Asked entry by entry, a frame the two share answering for itself.
pub(super) fn refinements_within(these: &[Rc<Frame>], those: &[Rc<Frame>]) -> bool {
    these
        .iter()
        .filter(|frame| !those.iter().any(|other| Rc::ptr_eq(frame, other)))
        .all(|frame| {
            frame
                .refinements
                .within(those.iter().map(|other| &other.refinements))
                && frame
                    .refinement_projections
                    .within(those.iter().map(|other| &other.refinement_projections))
                && frame
                    .refinement_scrutinees
                    .within(those.iter().map(|other| &other.refinement_scrutinees))
        })
}

/// Whether `live`, the local frames that stand, begin with the frames `held`, outermost first: each the same frame, by its number, with the refinements it had where it was held. What is added past them, binders and definitions, is not asked about here.
pub(super) fn stand(live: &[Rc<Frame>], held: &[Rc<Frame>]) -> bool {
    live.len() >= held.len()
        && live.iter().zip(held).all(|(live, held)| {
            Rc::ptr_eq(live, held) || (live.number == held.number && live.refined_as(held))
        })
}

/// The frames above the base as they stood where something that waits took them: a parked problem where it parks, and a metavariable where it is born. Shared with the context and with every other holder, never copied: a frame written to since was copied for its writer, so these are what stood. A judgment made for what holds them stands on them ([`Context::standing_on`](crate::Context::standing_on)).
///
/// The base frame is the elaboration's and is never held: what waits with no local frame standing holds none.
#[derive(Debug, Clone, Default)]
pub(crate) struct HeldFrames(Rc<[Rc<Frame>]>);

impl HeldFrames {
    pub(super) fn of(frames: &[Rc<Frame>]) -> Self {
        Self(frames.iter().cloned().collect())
    }

    pub(super) fn frames(&self) -> &[Rc<Frame>] {
        &self.0
    }

    /// Whether a frame held registers a refinement of any kind: the residual-constraint report notes it, and the caches are told of a judgment that stands on one.
    pub(crate) fn refined(&self) -> bool {
        self.0.iter().any(|frame| frame.refined())
    }

    /// Whether a frame held binds a local: what a judgment standing on these reads that the base frame does not hold.
    pub(crate) fn binds(&self) -> bool {
        self.0.iter().any(|frame| !frame.locals.is_empty())
    }

    /// Every definition the frames hold, outermost frame first, each frame's in the order it recorded them.
    pub(crate) fn definitions(&self) -> impl Iterator<Item = (&Free, &DefEntry)> {
        self.0.iter().flat_map(|frame| frame.definitions.iter())
    }

    /// The `use`-plicity binders the frames hold, outermost first: the reader position a report about a metavariable born under them is spelled for.
    pub(crate) fn witnesses(&self) -> impl Iterator<Item = &(Free, Term)> {
        self.0.iter().flat_map(|frame| &frame.witnesses)
    }

    /// Why the innermost arm among the frames is never taken, where its scrutinee's indices rule one out: an implicit born there and never solved is reported with it once the arm has closed.
    pub(crate) fn dead_arm(&self) -> Option<&Rc<DeadArm>> {
        self.0
            .iter()
            .rev()
            .find_map(|frame| frame.dead_arm.as_ref())
    }

    /// The arms among the frames that refine a scrutinee to a case, innermost first: each with its guard and its case, and the frames as far as its own, which are the ones its guard is read under.
    pub(crate) fn arms(&self) -> Vec<(HeldFrames, Term, Term)> {
        self.0
            .iter()
            .enumerate()
            .rev()
            .filter_map(|(index, frame)| {
                let (guard, case) = frame.arm.as_ref()?;

                Some((
                    Self(self.0[..=index].iter().cloned().collect()),
                    guard.clone(),
                    case.clone(),
                ))
            })
            .collect()
    }

    /// Whether every refinement these frames register is one `other`'s register: what containment between two birth contexts asks beside their names.
    pub(crate) fn refinements_within(&self, other: &HeldFrames) -> bool {
        refinements_within(&self.0, &other.0)
    }

    /// The frames both hold, outermost first for as far as they are the same frames, each as `self` holds it: what a metavariable is restricted to when a solution born under `other` embeds it, `self` being the earlier of the two births, whose frames are the ones nothing was written to since.
    pub(crate) fn shared_with(&self, other: &HeldFrames) -> HeldFrames {
        Self(
            self.0
                .iter()
                .zip(other.0.iter())
                .take_while(|(mine, theirs)| mine.number == theirs.number)
                .map(|(mine, _)| Rc::clone(mine))
                .collect(),
        )
    }

    /// The frames held beneath the one numbered as `frame` is, the innermost that stands: what a metavariable born in a block is restricted to where the block ends.
    pub(crate) fn beneath(&self, frame: &Frame) -> HeldFrames {
        Self(
            self.0
                .iter()
                .take_while(|held| held.number != frame.number)
                .cloned()
                .collect(),
        )
    }
}
