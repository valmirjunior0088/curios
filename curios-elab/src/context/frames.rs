//! The lexical half of the elaborator's state: assumptions, local definitions, counterfactual refinements, and the witness scope, all bracket-disciplined by `enter`/`leave`.
//!
//! Everything here lives and dies with binder frames — the opposite lifetime from the flat stores in [`Program`](super::Program) and [`Solutions`](super::Solutions). Cache coordination stays with the `Context` façade: a frame write that must clear or stamp the caches does so there, so this type's methods are pure store operations.
//!
//! Three brackets narrow what a lookup reads, each for its own reason. Suppression withholds the refinements a judgment must not rest on — every one but those a checked term was born under; withholding hides a settlement's own frame and every frame inside it; and the retry floor hides the live local frames a parked problem's retry happens to run inside, so the problem is decided in exactly the context it froze.

use {
    super::{SharedSpine, SharedTelescope},
    crate::UniverseStateToken,
    curios_core::{
        DefinitionKind, Free, Global, HeadTag, RecGroup, Term, UniverseContext,
        project_erased_universes,
    },
    curios_utilities::Entropy,
    std::{
        collections::{BTreeMap, HashMap},
        hash::Hash,
        rc::Rc,
    },
};

/// One definition: the definiens, plus the [`DefinitionKind`] of the module item that introduced it. Every `DefEntry` — whether a plain `let`/`rec` member or mid-window rec-group registration — is treated uniformly; there is no `recursive` marker distinguishing them.
///
/// `kind` is `None` for a genuine *local* binding — a `let` binder, an opened match scrutinee, a lambda parameter — which no module item declared. It is carried rather than re-derived: the kind is elaboration metadata `into_core` attached where the item was generated, and splitting the definition's name apart to recover it would misread an ordinary definition that merely happens to sit under a generated namespace (see [`DefinitionKind`]'s own docs).
#[derive(Debug, Clone)]
pub(crate) struct DefEntry {
    term: Term,
    kind: Option<DefinitionKind>,
}

impl DefEntry {
    pub(crate) fn new(term: Term, kind: Option<DefinitionKind>) -> Self {
        Self { term, kind }
    }

    pub(crate) fn body(&self) -> &Term {
        &self.term
    }

    pub(crate) fn kind(&self) -> Option<&DefinitionKind> {
        self.kind.as_ref()
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
#[derive(Debug)]
struct Spelling {
    /// The reduced spelling, or `None` where reducing the key refused or outran its allowance.
    settled: Option<Settled>,
    /// How many solutions had been committed when it settled, where the key or the spelling held an unsolved metavariable — `None` where neither did, and no solution can change what the key reduces to.
    solved: Option<usize>,
    /// The universe solver's state when it settled, where a question put to conversion on the way was declined for a commit: the spelling is then what the key reduces to only while that state stands.
    levels: Option<UniverseStateToken>,
}

/// One projection refinement: the base as *written* (unerased, so a probe at another universe instance can be told apart from the spelling the arm actually scrutinized), and the arm's value.
///
/// The key beside it is universes-erased, which is what lets two occurrences of one polymorphic base merge while their instances are still undecided. Erasure cannot tell a decided disagreement from an undecided one, so the base is kept whole and [`Context::proj_reduct`](crate::Context) compares it at the read.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ProjectionEntry {
    pub(crate) original: Term,
    pub(crate) value: Term,
}

/// Counterfactual match-arm refinements of every kind — of a variable, of a projection, and of a stuck-application scrutinee — each store's frames flattened outermost first, so reinstalling them in order reproduces the shadowing.
///
/// What a metavariable was born under ([`Frames::refinement_snapshot`]), and what a parked problem froze: the one thing a solution may rest on beyond its birth telescope, since an arm's guard holds wherever the metavariable does — inside the arm — and nowhere else.
#[derive(Debug, Clone, Default)]
pub(crate) struct Refinements {
    pub(crate) variables: Vec<(Free, Term)>,
    pub(crate) projections: Vec<((Term, usize), ProjectionEntry)>,
    pub(crate) scrutinees: Vec<(Term, ScrutineeEntry)>,
}

/// A [`Refinements`] shared between every birth under unchanged refinements, as the birth telescope is.
pub(crate) type SharedRefinements = Rc<Refinements>;

impl Refinements {
    pub(crate) fn is_empty(&self) -> bool {
        self.variables.is_empty() && self.projections.is_empty() && self.scrutinees.is_empty()
    }

    /// Whether every refinement here is also one of `other`'s: what containment between two birth contexts asks, since a solution resting on a guard is scoped to the arm the guard opens.
    pub(crate) fn within(&self, other: &Refinements) -> bool {
        self.variables
            .iter()
            .all(|entry| other.variables.contains(entry))
            && self
                .projections
                .iter()
                .all(|entry| other.projections.contains(entry))
            && self
                .scrutinees
                .iter()
                .all(|entry| other.scrutinees.contains(entry))
    }

    /// Whether the two hold the same refinements. Compared as sets: two frames may have recorded the same refinements in different orders.
    pub(crate) fn same_as(&self, other: &Refinements) -> bool {
        self.within(other) && other.within(self)
    }

    /// The refinements here that `other` holds too, in this one's order: what a metavariable is restricted to when a solution born under `other` embeds it ([`Context::restrict_metavar`](crate::Context::restrict_metavar)).
    pub(crate) fn shared_with(&self, other: &Refinements) -> Refinements {
        Refinements {
            variables: self
                .variables
                .iter()
                .filter(|entry| other.variables.contains(entry))
                .cloned()
                .collect(),
            projections: self
                .projections
                .iter()
                .filter(|entry| other.projections.contains(entry))
                .cloned()
                .collect(),
            scrutinees: self
                .scrutinees
                .iter()
                .filter(|entry| other.scrutinees.contains(entry))
                .cloned()
                .collect(),
        }
    }
}

/// The local frame a parked problem froze at park time: assumptions (in binding order), and the non-base-frame definitions and refinements (each outermost frame first, so reapplying in order reproduces the shadowing). A retry runs under exactly what its origin saw — the arm-local refinements included, and nothing of the live context it is scheduled inside (`Context::with_retry_frame`).
#[derive(Debug, Clone)]
pub(crate) struct FrozenFrame {
    pub(crate) assumptions: Vec<(Free, Term)>,
    pub(crate) definitions: Vec<(Free, DefEntry)>,
    pub(crate) refinements: Refinements,
    /// The `use`-plicity binders in scope at park time (a subset of `assumptions`, in the same binding order). Witness resolution scans these; a retry must see the same instance scope its origin saw.
    pub(crate) witness_binders: Vec<(Free, Term)>,
}

impl FrozenFrame {
    /// Whether the frame carried live match-arm refinements at park time — the residual-constraint report notes it.
    pub(crate) fn carries_refinements(&self) -> bool {
        !self.refinements.is_empty()
    }
}

/// One frame's refinements of one kind, read back in the order they were recorded.
///
/// A hash map's own order differs from one process to the next, and a frame's refinements are read in order: the guards a proof the elaborator writes is built from are admitted as they are met, so a frame holding two would yield two proofs of one bound, by the run.
#[derive(Debug)]
struct Recorded<K, V> {
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

    fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    fn get(&self, key: &K) -> Option<&V> {
        self.places.get(key).map(|&place| &self.entries[place].1)
    }

    fn contains_key(&self, key: &K) -> bool {
        self.places.contains_key(key)
    }

    /// Record `value` under `key`, in the place the key already holds where it was recorded before.
    fn insert(&mut self, key: K, value: V) {
        match self.places.get(&key) {
            Some(&place) => self.entries[place].1 = value,
            None => {
                self.places.insert(key.clone(), self.entries.len());
                self.entries.push((key, value));
            }
        }
    }

    fn iter(&self) -> impl DoubleEndedIterator<Item = (&K, &V)> {
        self.entries.iter().map(|(key, value)| (key, value))
    }

    fn keys(&self) -> impl Iterator<Item = &K> {
        self.entries.iter().map(|(key, _)| key)
    }
}

/// The frame-scoped lexical stores. `Context` holds exactly one of these; see the module documentation for the cache-coordination contract.
#[derive(Debug)]
pub(crate) struct Frames {
    assumptions: Vec<HashMap<Free, Term>>,
    assumption_universes: Vec<HashMap<Free, UniverseContext>>,
    definitions: Vec<HashMap<Free, DefEntry>>,
    /// Counterfactual match-arm refinements (`refine_head`), kept parallel to `definitions` but suppressible: re-validation of a metavariable solution must keep stable definitions yet ignore these.
    refinements: Vec<Recorded<Free, Term>>,
    refinement_projections: Vec<Recorded<(Term, usize), ProjectionEntry>>,
    /// Counterfactual refinements keyed by a *stuck application* scrutinee — a non-key match head (`classify(c)`, `Nat/in_range(...)`) that `refine_head` could not record. Keyed by the scrutinee as written, its metavariables and universes normalized (`reduce`'s `shallow_scrutinee`); an occurrence that surfaces spelled differently is met at the entry's reduced spelling (`Frames::scrutinee_spellings`). The term-keyed analogue of the two stores above, suppressed by the same flag.
    refinement_scrutinees: Vec<Recorded<Term, ScrutineeEntry>>,
    /// Each scrutinee entry's *reduced* spelling once a probe has asked for it, beside the entry: in the frame the entry was registered in, under its key — `reduce::refined_reduct`'s memo, the elaborator's copy of the kernel's per-entry reduct.
    ///
    /// **It lives as long as its entry, and is forgotten by what can change what its key reduces to.** A spelling is settled with its entry's frame and every frame inside it withheld, and read only through the window, so it rests on the refinements of the frames outside its entry — which cannot change while the entry stands: a registration lands in the innermost frame, and a frame leaves after every frame inside it — and on what reduction reads of everything else: the definitions, the solutions and the universe levels. So a registration, the exit of an inner frame and a suppression bracket leave it alone, and it is forgotten with its frame, where its key is registered again, at a redefinition, a rollback and a universe rewrite, and at a declaration's boundary, which nothing a budget paid for outlives. A fresh definition forgets the spellings naming it, as it does the reducts; and one settled while its key or its reduct held an unsolved metavariable is asked for again once a solution has landed.
    ///
    /// An entry is visible under one window floor only, the one in force where it was registered: a suppression bracket and a retry frame hide every frame that stood when they began, and leave after every frame entered inside them. So a spelling is never read under a floor other than the one it was settled under.
    ///
    /// **Settled once for each of the two reductions**, the key saying which (`Context::plainly`): a probe is compared with the spelling reduced the way the probe was, as the kernel settles an equation's reduct once for each.
    scrutinee_spellings: Vec<HashMap<(Term, bool), Spelling>>,
    /// How much of the refinement stack is withheld, as the frame depth suppression began at — `None` for none of it.
    ///
    /// A depth rather than a flag, because the refinements a re-validation meets are not all of one kind. The *ambient* ones — the arm the solver is currently inside — are counterfactual with respect to a solution for a metavariable born elsewhere, and withholding them is the whole point (`Convert::solve_at_birth`). Above this depth sit the ones the metavariable was born under, reinstalled by `Context::with_refinements`, and the candidate's own: `check` descending into a match arm of the term being validated re-establishes exactly the equalities that made that arm's body well-typed where it was written. Withholding those rejects correct solutions — a proof discharged by reduction inside an arm, `True/qed()` against `Holds(0 < Bytes/len(b))` in `/std/Str`'s scan fold, fails to re-check and the solution is thrown away — so suppression stops at the depth it started from.
    suppress_refinements_below: Option<usize>,
    /// How much of the refinement stack a settlement withholds from the inside, as the frame the entry being settled was registered in — `None` for none of it.
    ///
    /// The other face of [`Frames::suppress_refinements_below`], and the one the kernel has. A refinement key's *reduced* spelling must rest only on equations that outlive it, which are the frames outside its own: an inner arm's equation retracts first, and the entry's own frame is the equation itself, which a reduction of the key would otherwise meet at its first probe and answer with the case value it is assuming. `curios-cert`'s `Scope::hide_refinements_from` withholds the same span for the same two reasons.
    withhold_refinements_from: Option<usize>,
    /// The frame a retry runs in, while one runs — `None` otherwise.
    ///
    /// Every local frame below it is the live context the retry happens to be scheduled inside, which the parked problem never saw: it is hidden from assumptions, the local list, the witness scope and refinements, so the problem is decided in exactly the context it froze and nothing else. Definitions stay visible, because names are unique mints and a definition never changes — a live definition *is* the frozen one.
    retry_floor: Option<usize>,
    /// The local assumption context in binding order (a companion to `assumptions`, which is keyed by name and loses order). `assume` appends; frames are delimited by `local_marks`.
    local: Vec<(Free, Term)>,
    local_marks: Vec<usize>,
    /// The `use`-plicity binders currently in scope, in binding order (a subset of `local`), with frame boundaries in `witness_marks` — resolution's step-1/2 search space, scanned innermost-first.
    witness_scope: Vec<(Free, Term)>,
    witness_marks: Vec<usize>,
    /// One tick per mutation of `local` (assume, frame exit, reassume) — an `Entropy` used as a version stamp: `fresh()` bumps, `count()` reads. Invalidates `identity_cache`, which shares the frozen telescope and identity spine between every meta born under an unchanged Γ.
    locals_stamp: Entropy,
    identity_cache: Option<(usize, SharedTelescope, SharedSpine)>,
    /// One tick per change to the refinements a lookup may read — one recorded, a frame left, a bracket entered or left — invalidating `refinement_cache`, which shares one [`Refinements`] between every metavariable born under unchanged refinements, as `identity_cache` shares the telescope.
    refinement_stamp: Entropy,
    refinement_cache: Option<(usize, SharedRefinements)>,
}

impl Frames {
    pub(crate) fn new() -> Self {
        Self {
            assumptions: vec![HashMap::new()],
            assumption_universes: vec![HashMap::new()],
            definitions: vec![HashMap::new()],
            refinements: vec![Recorded::new()],
            refinement_projections: vec![Recorded::new()],
            refinement_scrutinees: vec![Recorded::new()],
            scrutinee_spellings: vec![HashMap::new()],
            suppress_refinements_below: None,
            withhold_refinements_from: None,
            retry_floor: None,
            local: Vec::new(),
            local_marks: Vec::new(),
            witness_scope: Vec::new(),
            witness_marks: Vec::new(),
            locals_stamp: Entropy::new(),
            identity_cache: None,
            refinement_stamp: Entropy::new(),
            refinement_cache: None,
        }
    }

    pub(crate) fn enter(&mut self) {
        self.assumptions.push(HashMap::new());
        self.assumption_universes.push(HashMap::new());
        self.definitions.push(HashMap::new());
        self.refinements.push(Recorded::new());
        self.refinement_projections.push(Recorded::new());
        self.refinement_scrutinees.push(Recorded::new());
        self.scrutinee_spellings.push(HashMap::new());
        self.local_marks.push(self.local.len());
        self.witness_marks.push(self.witness_scope.len());
    }

    /// Pop one frame, reporting `(dropped_refinements, dropped_definitions)` so the façade can run the matching cache protocol.
    pub(crate) fn leave(&mut self) -> (bool, bool) {
        self.locals_stamp.fresh();
        self.refinement_stamp.fresh();
        self.assumptions.pop().unwrap();
        self.assumption_universes.pop().unwrap();
        let definitions = self.definitions.pop().unwrap();
        let refinements = self.refinements.pop().unwrap();
        let refinement_projections = self.refinement_projections.pop().unwrap();
        let refinement_scrutinees = self.refinement_scrutinees.pop().unwrap();
        self.scrutinee_spellings.pop().unwrap();
        self.local.truncate(self.local_marks.pop().unwrap());
        self.witness_scope
            .truncate(self.witness_marks.pop().unwrap());

        (
            !refinements.is_empty()
                || !refinement_projections.is_empty()
                || !refinement_scrutinees.is_empty(),
            !definitions.is_empty(),
        )
    }

    /// Assume `label : type_`. Erasure is sort-driven (a proof or a type erases), so a binder carries no runtime-multiplicity mark.
    pub(crate) fn assume(&mut self, name: &Free, type_: &Term) {
        self.locals_stamp.fresh();
        self.local.push((*name, type_.clone()));

        self.assumptions
            .last_mut()
            .unwrap()
            .insert(*name, type_.clone());
        self.assumption_universes
            .last_mut()
            .unwrap()
            .insert(*name, UniverseContext::empty());
    }

    /// Join `name` to the witness scope (it must already be assumed).
    pub(crate) fn push_witness_binder(&mut self, name: &Free, type_: &Term) {
        self.witness_scope.push((*name, type_.clone()));
    }

    /// The `use`-plicity binders in scope, in binding order (innermost last).
    pub(crate) fn witness_scope(&self) -> &[(Free, Term)] {
        &self.witness_scope[self.visible_witness_start()..]
    }

    /// Re-join a frozen frame's witness binders (already re-assumed) to the scope; the enclosing frame's mark truncates them on exit.
    pub(crate) fn extend_witness_scope(&mut self, binders: &[(Free, Term)]) {
        self.witness_scope.extend(binders.iter().cloned());
    }

    /// Replace the type of an existing assumption in place — the innermost binding of `label`. Used by the `rec` elaborators: a group's signatures must be assumed (lowered) before they can be elaborated, since members reference each other, and are then upgraded here to their rebuilt forms — implicit insertion makes the two no longer interchangeable, and a lowered type must never leak into later reduction. Panics if `label` has no prior assumption — every caller is expected to have `assume`d it earlier in the same scope (a construction bug otherwise, not a user-facing case).
    pub(crate) fn reassume(&mut self, name: &Free, type_: &Term) {
        self.locals_stamp.fresh();

        let entry = self
            .local
            .iter_mut()
            .rev()
            .find(|(bound, _)| bound == name)
            .unwrap_or_else(|| panic!("reassume: '{name}' has no local binding to replace"));
        entry.1 = type_.clone();

        let assumptions = self
            .assumptions
            .iter_mut()
            .rev()
            .find(|assumptions| assumptions.contains_key(name))
            .unwrap_or_else(|| {
                panic!("reassume: '{name}' has no assumption-frame entry to replace")
            });
        assumptions.insert(*name, type_.clone());
    }

    pub(crate) fn assumption(&self, name: &Free) -> Option<&Term> {
        self.visible(&self.assumptions)
            .rev()
            .find_map(|assumptions| assumptions.get(name))
    }

    /// The frames a lookup may read: the base frame, then every frame from the retry floor up — all of them when no retry runs.
    fn visible<'a, T>(&self, stores: &'a [T]) -> impl DoubleEndedIterator<Item = &'a T> {
        let floor = self.retry_floor.unwrap_or(1).clamp(1, stores.len());
        stores[..1].iter().chain(&stores[floor..])
    }

    /// Where the part of `local` a lookup may read begins: past the top-level entries, and past every hidden frame's binders while a retry runs.
    fn visible_local_start(&self) -> usize {
        match self.retry_floor {
            Some(floor) => self
                .local_marks
                .get(floor - 1)
                .copied()
                .unwrap_or(self.local.len()),
            None => self.base_locals(),
        }
    }

    /// Where the part of the witness scope a resolution may read begins: the whole of it, or past every hidden frame's binders while a retry runs.
    fn visible_witness_start(&self) -> usize {
        match self.retry_floor {
            Some(floor) => self
                .witness_marks
                .get(floor - 1)
                .copied()
                .unwrap_or(self.witness_scope.len()),
            None => 0,
        }
    }

    /// Hide every local frame below the innermost one from every lookup but definitions, returning the previous floor — the bracket intrinsic for `Context::with_retry_frame`. Answers whether any refinement sits in the frames it hides, which is what the caches must be told.
    pub(crate) fn hide_frames_below_here(&mut self) -> (Option<usize>, bool) {
        self.locals_stamp.fresh();
        self.refinement_stamp.fresh();
        let floor = self.assumptions.len() - 1;
        let hidden = self.retry_floor.unwrap_or(1).min(floor)..floor;
        let refined = self.refinements[hidden.clone()]
            .iter()
            .any(|f| !f.is_empty())
            || self.refinement_projections[hidden.clone()]
                .iter()
                .any(|f| !f.is_empty())
            || self.refinement_scrutinees[hidden]
                .iter()
                .any(|f| !f.is_empty());
        (self.retry_floor.replace(floor), refined)
    }

    /// Restore a floor taken by [`hide_frames_below_here`](Self::hide_frames_below_here).
    pub(crate) fn restore_hidden_frames(&mut self, previous: Option<usize>) {
        self.locals_stamp.fresh();
        self.refinement_stamp.fresh();
        self.retry_floor = previous;
    }

    /// Drop every base-frame binding of `name`: its assumption, its universe context, its definition, and its places in the local and witness scopes. Base frame only — an inner frame is popped whole by [`Frames::leave`] — because the one binding that needs forgetting singly is a top-level declaration's, undone when its item is refused.
    pub(crate) fn forget(&mut self, name: &Free) {
        assert_eq!(
            self.assumptions.len(),
            1,
            "forget: '{name}' outside the base frame"
        );
        self.locals_stamp.fresh();
        self.assumptions[0].remove(name);
        self.assumption_universes[0].remove(name);
        self.definitions[0].remove(name);
        self.local.retain(|(bound, _)| bound != name);
        self.witness_scope.retain(|(bound, _)| bound != name);
    }

    /// The innermost registered universe context for `name`, if any.
    pub(crate) fn assumption_universe_context(&self, name: &Free) -> Option<UniverseContext> {
        self.visible(&self.assumption_universes)
            .rev()
            .find_map(|contexts| contexts.get(name))
            .cloned()
    }

    /// Overwrite the innermost universe context registered for `name`. Panics if none exists.
    pub(crate) fn set_assumption_universe_context(
        &mut self,
        name: &Free,
        universe_context: UniverseContext,
    ) {
        let floor = self
            .retry_floor
            .unwrap_or(1)
            .clamp(1, self.assumption_universes.len());
        let (base, local) = self.assumption_universes.split_at_mut(1);
        let contexts = base
            .iter_mut()
            .chain(&mut local[floor - 1..])
            .rev()
            .find(|contexts| contexts.contains_key(name))
            .unwrap_or_else(|| panic!("'{name}' has no assumption universe context to replace"));
        contexts.insert(*name, universe_context);
    }

    /// Per-frame `(index, parameter_count)` holders of `name`'s universe context — diagnostics for the instantiation mismatch traces.
    #[cfg(feature = "profile")]
    pub(crate) fn assumption_universe_holders(&self, name: &Free) -> (usize, Vec<(usize, usize)>) {
        (
            self.assumption_universes.len(),
            self.assumption_universes
                .iter()
                .enumerate()
                .filter(|(_, contexts)| contexts.contains_key(name))
                .map(|(index, contexts)| (index, contexts[name].parameter_count))
                .collect(),
        )
    }

    /// Whether `label` currently has a definition entry in some frame — the settled-globals gate for the elaboration cache. A name defined here will only ever be *re*defined (which clears both caches wholesale), never freshly defined, so an elaboration entry naming it is safe to keep across a later fresh `define`.
    pub(crate) fn is_defined(&self, name: &Free) -> bool {
        self.definitions
            .iter()
            .any(|frame| frame.contains_key(name))
    }

    /// The local assumption context in binding order (outermost first) — while a retry runs, only the part past its floor, which is the whole of the retried problem's local context. The dependent-match generalizer (`elaborate_match`) walks this to find the hypotheses whose type depends on a scrutinee index being abstracted: they must ride into the motive as Π-binders, or the synthesized motive is ill-typed. Binding order matters — a hypothesis's type can only mention earlier binders, so the telescope it yields is already well-ordered.
    pub(crate) fn locals(&self) -> &[(Free, Term)] {
        match self.retry_floor {
            Some(_) => &self.local[self.visible_local_start()..],
            None => &self.local,
        }
    }

    /// Insert `name`'s definition into the innermost frame. The façade decides the cache protocol from [`Frames::is_defined`] first.
    pub(crate) fn define(&mut self, name: Free, entry: DefEntry) {
        self.definitions.last_mut().unwrap().insert(name, entry);
    }

    /// The [`DefinitionKind`] of the module item that defined `label`, or `None` for a local binding or an undefined name.
    ///
    /// The structural replacement for splitting a definition's qualified name into a family and a case and looking the family up in a registry: the kind was known where the definition was generated, so it is read back rather than re-derived from the name's spelling.
    pub(crate) fn definition_kind(&self, name: &Free) -> Option<&DefinitionKind> {
        self.definitions
            .iter()
            .rev()
            .find_map(|definitions| definitions.get(name))
            .and_then(|entry| entry.kind.as_ref())
    }

    /// What `name` unfolds to through its *definition* alone — never through a refinement. The shared analyses read through this: a definitions-only lookup needs no invariant about when the refinement store happens to be empty, where [`Frames::var_reduct_at`] would silently mean something else inside a match arm.
    pub(crate) fn definition_body(&self, name: &Free) -> Option<&Term> {
        self.definitions
            .iter()
            .rev()
            .find_map(|definitions| definitions.get(name))
            .map(|entry| &entry.term)
    }

    /// Every top-level `rec` group defined in any frame, each with its members' global names in member order. A member's definition is the group with a projecting tail, so the groups are read off the members and their names gathered by index; a group read only partially — a member never defined — is dropped, since a spelling with a gap is no spelling.
    pub(crate) fn rec_definitions(&self) -> Vec<(RecGroup, Vec<Global>)> {
        let mut groups: Vec<(RecGroup, Vec<Option<Global>>)> = Vec::new();
        for definitions in &self.definitions {
            for (name, entry) in definitions {
                let Free::Global(global) = name else {
                    continue;
                };
                let Some((group, index)) = entry.term.as_rec_proj() else {
                    continue;
                };
                let names = match groups.iter_mut().find(|(known, _)| known == group) {
                    Some((_, names)) => names,
                    None => {
                        groups.push((group.clone(), vec![None; group.length()]));
                        &mut groups.last_mut().expect("just pushed").1
                    }
                };
                if let Some(slot) = names.get_mut(index) {
                    *slot = Some(*global);
                }
            }
        }
        groups
            .into_iter()
            .filter_map(|(group, names)| {
                let names = names.into_iter().collect::<Option<Vec<_>>>()?;
                Some((group, names))
            })
            .collect()
    }

    /// The reduct of a variable: its definition, or — from the frames suppression does not withhold — its counterfactual refinement. A name never appears in both stores (definitions name `let`/`rec` binders; refinements name assumed scrutinee heads), so the order between them is immaterial.
    fn raw_var_reduct(&self, name: &Free) -> Option<&Term> {
        if let Some(term) = self.visible_refinements().rev().find_map(|r| r.get(name)) {
            return Some(term);
        }

        self.definitions
            .iter()
            .rev()
            .find_map(|definitions| definitions.get(name))
            .map(|entry| &entry.term)
    }

    /// Reduce a bare variable only when its definition is monomorphic.
    ///
    /// A polymorphic definition's stored body is scoped by its universe context: its parameter levels are not meaningful at an occurrence until elaboration has rebuilt that occurrence as an `Instance`. Letting a raw variable unfold would leak those bound parameters into the ambient solver. The explicit-instance reducer uses [`Frames::var_reduct_at`] after it has the occurrence's level arguments.
    pub(crate) fn var_reduct(&self, name: &Free) -> Option<&Term> {
        let is_polymorphic = self
            .assumption_universes
            .iter()
            .rev()
            .find_map(|contexts| contexts.get(name))
            .is_some_and(|context| context.parameter_count != 0);
        (!is_polymorphic)
            .then(|| self.raw_var_reduct(name))
            .flatten()
    }

    pub(crate) fn var_reduct_at(&self, name: &Free) -> Option<&Term> {
        self.raw_var_reduct(name)
    }

    /// The entry a projection's counterfactual match-arm refinement is registered under, from the frames suppression does not withhold (re-validation).
    ///
    /// The whole entry rather than its value, for the reason [`Frames::scrutinee_entry`] gives: the key cannot decide a universe instance, so the read above compares the unerased bases instead.
    pub(crate) fn projection_entry(&self, base: &Term, index: usize) -> Option<&ProjectionEntry> {
        let base = project_erased_universes(base);
        self.refinement_projections[self.refinement_window()]
            .iter()
            .rev()
            .find_map(|p| p.get(&(base.clone(), index)))
    }

    /// Register a counterfactual match-arm refinement of a variable. Unlike a definition, this lives in a suppressible store so re-validation can ignore it. The façade clears the caches first.
    pub(crate) fn refine(&mut self, name: &Free, term: &Term) {
        self.refinement_stamp.fresh();
        self.refinements
            .last_mut()
            .unwrap()
            .insert(*name, term.clone());
    }

    /// Register a counterfactual refinement of a projection (`refine_head` on a `Proj` scrutinee). The façade clears the caches first.
    pub(crate) fn refine_projection(&mut self, base: Term, index: usize, value: Term) {
        self.refinement_stamp.fresh();
        self.refinement_projections.last_mut().unwrap().insert(
            (project_erased_universes(&base), index),
            ProjectionEntry {
                original: base,
                value,
            },
        );
    }

    /// Register a counterfactual refinement of a stuck-application scrutinee (`refine_head` on a non-key head). `canonical` is the cheap key (as written, metas and universes normalized); `original` is the unerased spelling a settlement reduces; `value` is the arm's constructor. Sound for the same reason `refine` is — the arm is reached only when the scrutinee equals `value` — and non-cyclic because `value` is a constructor of the scrutinee's inductive, a normal form. The façade clears the caches first.
    pub(crate) fn refine_scrutinee(&mut self, canonical: Term, entry: ScrutineeEntry) {
        self.refinement_stamp.fresh();
        // A key registered again is another entry, and what the one before it settled to is not its spelling.
        let spellings = self.scrutinee_spellings.last_mut().unwrap();
        for plain in [false, true] {
            spellings.remove(&(canonical.clone(), plain));
        }
        self.refinement_scrutinees
            .last_mut()
            .unwrap()
            .insert(canonical, entry);
    }

    /// The reduced spelling settled for the entry `key` registered in `frame`, by plain reduction where `plain` and by a judgment's otherwise: `None` where no probe has asked for it — or it held an unsolved metavariable and a solution has landed since, `solved` being how many are committed now — and `Some(None)` where reducing it refused.
    pub(crate) fn settled_spelling(
        &self,
        frame: usize,
        key: &Term,
        plain: bool,
        solved: usize,
        levels: &UniverseStateToken,
    ) -> Option<&Option<Settled>> {
        let spelling = self
            .scrutinee_spellings
            .get(frame)?
            .get(&(key.clone(), plain))?;

        (spelling
            .solved
            .is_none_or(|settled_at| settled_at == solved)
            && spelling
                .levels
                .as_ref()
                .is_none_or(|settled_at| settled_at == levels))
        .then_some(&spelling.settled)
    }

    /// File what the entry `key` registered in `frame` settled to under the reduction `plain` names. `solved` is how many solutions were committed, where the key or the spelling held an unsolved metavariable.
    pub(crate) fn settle_spelling(
        &mut self,
        frame: usize,
        key: Term,
        plain: bool,
        settled: Option<Settled>,
        solved: Option<usize>,
        levels: Option<UniverseStateToken>,
    ) {
        if let Some(spellings) = self.scrutinee_spellings.get_mut(frame) {
            spellings.insert(
                (key, plain),
                Spelling {
                    settled,
                    solved,
                    levels,
                },
            );
        }
    }

    /// Forget every settled spelling: what a key reduces to may have changed everywhere.
    pub(crate) fn forget_spellings(&mut self) {
        for spellings in &mut self.scrutinee_spellings {
            spellings.clear();
        }
    }

    /// Forget the settled spellings a fresh definition of `name` can change: the ones naming it, which were stuck on its absence, and the ones that refused.
    pub(crate) fn forget_spellings_naming(&mut self, name: &Free) {
        for spellings in &mut self.scrutinee_spellings {
            spellings.retain(|_, spelling| {
                spelling
                    .settled
                    .as_ref()
                    .is_some_and(|settled| !settled.unerased.mentions_free(name))
            });
        }
    }

    /// Whether any scrutinee refinement is registered (regardless of suppression). The cheap outer gate for the reducer probe — skipped on the common refinement-free reduction without hashing anything.
    pub(crate) fn has_scrutinee_refinements(&self) -> bool {
        !self.refinement_scrutinees.iter().all(|f| f.is_empty())
    }

    /// Whether some registered scrutinee key shares `head` as its applied-head symbol. The second gate, past `Term::head_key`: only a head that is actually refined justifies building the candidate's key.
    pub(crate) fn scrutinee_head_refined(&self, head: HeadTag<'_>) -> bool {
        self.refinement_scrutinees
            .iter()
            .any(|f| f.keys().any(|k| k.head_key() == Some(head)))
    }

    /// The entry registered under the key `canonical`, from the frames suppression does not withhold (re-validation) — the innermost, so one an arm inside withholds is answered by the entry withholding it.
    ///
    /// The whole entry rather than its value, because the read above this one needs the `original` beside it: the key is universes-erased and cannot decide an instance, so [`Context::scrutinee_reduct`](crate::Context) compares the unerased spellings and declines where they disagree on one both sides have decided.
    pub(crate) fn scrutinee_entry(&self, canonical: &Term) -> Option<&ScrutineeEntry> {
        self.refinement_scrutinees[self.refinement_window()]
            .iter()
            .rev()
            .find_map(|f| f.get(canonical))
    }

    /// Every scrutinee entry the window leaves visible, with the frame it was registered in, innermost frame first — what a stuck reduct is compared against once its written spelling missed, and the frame each must be settled from. Borrowed rather than collected: `reduce::refined_reduct` walks this at every stuck form under a live guard, and a copy per walk was most of what that cost.
    pub(crate) fn visible_scrutinee_entries(
        &self,
    ) -> impl Iterator<Item = (usize, &Term, &ScrutineeEntry)> {
        let window = self.refinement_window();
        let (floor, ceiling) = (window.start, window.end);
        self.refinement_scrutinees[window]
            .iter()
            .enumerate()
            .rev()
            .flat_map(move |(offset, frame)| {
                frame
                    .iter()
                    .filter(move |(key, entry)| {
                        !entry.alias
                            && !entry.withheld
                            && !self.withheld_inside(floor + offset, ceiling, key)
                    })
                    .map(move |(key, entry)| (floor + offset, key, entry))
            })
    }

    /// Every equation the window leaves in force with the key it is held under, aliases among them, innermost frame first: what a variable's refinement is read against.
    pub(crate) fn scrutinee_equations(&self) -> Vec<(Term, ScrutineeEntry)> {
        let window = self.refinement_window();
        let (floor, ceiling) = (window.start, window.end);
        self.refinement_scrutinees[window]
            .iter()
            .enumerate()
            .rev()
            .flat_map(|(offset, frame)| {
                frame
                    .iter()
                    .filter(move |(key, entry)| {
                        !entry.withheld && !self.withheld_inside(floor + offset, ceiling, key)
                    })
                    .map(|(key, entry)| (key.clone(), entry.clone()))
            })
            .collect()
    }

    /// Every equation an arm in the window withholds, as the entry withholding it, innermost frame first.
    pub(crate) fn withheld_equations(&self) -> Vec<ScrutineeEntry> {
        self.refinement_scrutinees[self.refinement_window()]
            .iter()
            .rev()
            .flat_map(|frame| {
                frame
                    .iter()
                    .map(|(_, entry)| entry)
                    .filter(|entry| entry.withheld)
                    .cloned()
            })
            .collect()
    }

    /// Withhold, for as long as the current frame stands, the equation `entry` holds under `key` in a frame outside. The façade clears the caches first.
    pub(crate) fn withhold_scrutinee(&mut self, key: Term, entry: ScrutineeEntry) {
        self.refinement_stamp.fresh();
        self.refinement_scrutinees.last_mut().unwrap().insert(
            key,
            ScrutineeEntry {
                withheld: true,
                ..entry
            },
        );
    }

    /// Whether a frame inside `frame`, below `ceiling`, withholds the equation `key` holds there.
    fn withheld_inside(&self, frame: usize, ceiling: usize, key: &Term) -> bool {
        self.refinement_scrutinees[frame + 1..ceiling]
            .iter()
            .any(|inner| inner.get(key).is_some_and(|entry| entry.withheld))
    }

    /// The value the innermost arm in the window refines `name` to, where one refines it.
    pub(crate) fn refinement_of(&self, name: &Free) -> Option<&Term> {
        self.visible_refinements()
            .rev()
            .find_map(|frame| frame.get(name))
    }

    /// Whether an arm in the window refines any variable.
    pub(crate) fn refines_a_variable(&self) -> bool {
        self.visible_refinements().any(|frame| !frame.is_empty())
    }

    /// The variables the arms in the window refine, each with the value its innermost arm gives it.
    pub(crate) fn refined_variables(&self) -> Vec<(Free, Term)> {
        let mut variables = BTreeMap::new();
        for frame in self.visible_refinements() {
            for (name, value) in frame.iter() {
                variables.insert(*name, value.clone());
            }
        }
        variables.into_iter().collect()
    }

    /// Whether `canonical` is itself a registered scrutinee key — checked *past* suppression. A `Var`/`Proj` key stays neutral under suppression for free (its reduct is withheld, so it does not unfold); an application key would otherwise unfold to its definition body and stop being a key. The reducer consults this to keep such a key neutral while suppressed, so a solution `solve_at_birth` commits with the live refinements suppressed stays a term the live refinement can still fire on.
    pub(crate) fn is_scrutinee_key(&self, canonical: &Term) -> bool {
        self.refinement_scrutinees
            .iter()
            .any(|f| f.contains_key(canonical))
    }

    pub(crate) fn refinements_suppressed(&self) -> bool {
        self.suppress_refinements_below.is_some()
    }

    /// The refinement frames neither suppression nor a settlement withholds: from the depth suppression began at, or the base frame, up to the frame a settlement withholds from, or the top. Both ends clamped, so a frame popped past either point withholds everything rather than slicing out of range.
    fn refinement_window(&self) -> std::ops::Range<usize> {
        let ceiling = self
            .withhold_refinements_from
            .unwrap_or(self.refinements.len())
            .min(self.refinements.len());
        let floor = self
            .suppress_refinements_below
            .unwrap_or(0)
            .max(self.retry_floor.unwrap_or(0))
            .min(ceiling);
        floor..ceiling
    }

    /// The name-keyed refinement frames the window leaves visible, outermost first.
    fn visible_refinements(&self) -> std::slice::Iter<'_, Recorded<Free, Term>> {
        self.refinements[self.refinement_window()].iter()
    }

    /// Begin withholding every refinement registered so far, returning the previous depth — the bracket intrinsic for `Context::with_suppressed_refinements`. Frames entered after this keep their own refinements live, which is what lets a candidate's own match arms re-establish the equalities that made them well-typed.
    pub(crate) fn suppress_refinements_here(&mut self) -> Option<usize> {
        self.refinement_stamp.fresh();
        self.suppress_refinements_below
            .replace(self.refinements.len())
    }

    /// Restore a depth taken by [`suppress_refinements_here`](Self::suppress_refinements_here).
    pub(crate) fn restore_refinement_suppression(&mut self, previous: Option<usize>) {
        self.refinement_stamp.fresh();
        self.suppress_refinements_below = previous;
    }

    /// Begin withholding the refinements of `frame` and every frame inside it, returning the previous limit — the bracket intrinsic for `Context::with_refinements_withheld_from`.
    ///
    /// Always at or outside the current limit, since a settlement is only ever asked for an entry the window already shows, so nesting narrows the window and restoring widens it back.
    pub(crate) fn withhold_refinements_from(&mut self, frame: usize) -> Option<usize> {
        self.refinement_stamp.fresh();
        self.withhold_refinements_from.replace(frame)
    }

    /// Restore a limit taken by [`withhold_refinements_from`](Self::withhold_refinements_from).
    pub(crate) fn restore_withheld_refinements(&mut self, previous: Option<usize>) {
        self.refinement_stamp.fresh();
        self.withhold_refinements_from = previous;
    }

    /// The refinements a lookup may read now, of every kind — what a metavariable born here is born under. Shared while nothing a lookup reads changes, so a birth costs nothing more than the one before it.
    pub(crate) fn refinement_snapshot(&mut self) -> SharedRefinements {
        if let Some((stamp, refinements)) = &self.refinement_cache
            && *stamp == self.refinement_stamp.count()
        {
            return Rc::clone(refinements);
        }

        let window = self.refinement_window();
        let refinements = Rc::new(Refinements {
            variables: flatten_recorded(&self.refinements[window.clone()]),
            projections: flatten_recorded(&self.refinement_projections[window.clone()]),
            scrutinees: flatten_recorded(&self.refinement_scrutinees[window]),
        });
        self.refinement_cache = Some((self.refinement_stamp.count(), Rc::clone(&refinements)));
        refinements
    }

    /// Whether any counterfactual refinement of any kind is registered in any frame, *regardless* of suppression. The cache-contamination gate for `Context::with_suppressed_refinements`: only a registered refinement can make a suppressed reduct differ from the live one.
    pub(crate) fn any_refinements_registered(&self) -> bool {
        self.refinements.iter().any(|frame| !frame.is_empty())
            || self
                .refinement_projections
                .iter()
                .any(|frame| !frame.is_empty())
            || self
                .refinement_scrutinees
                .iter()
                .any(|frame| !frame.is_empty())
    }

    /// The boundary between the top-level (base-frame) entries of `local` and the genuine local binders above them. Top-level definitions are `assume`d into `local` at the base level (never inside a frame), so the outermost frame mark is exactly the count of top-level entries; with no frame open, everything in `local` is top-level. A metavariable's Γ is only the binders past this point (see [`Frames::identity_snapshot`]).
    fn base_locals(&self) -> usize {
        self.local_marks
            .first()
            .copied()
            .unwrap_or(self.local.len())
    }

    /// Whether `name` is bound at the top level (the persistent base frame) — a global definition, always in scope. The metavariable solver admits such names in a solution even though they are not in the metavariable's Γ/spine (which holds only local binders): a solution may freely mention a global constant without that constant being a context binder.
    pub(crate) fn is_top_level(&self, name: &Free) -> bool {
        self.assumptions
            .first()
            .is_some_and(|frame| frame.contains_key(name))
    }

    /// The frozen telescope and identity spine for the *current* Γ, shared: rebuilt only when `local` has changed since the last birth, so minting a metavariable is O(1) amortized instead of O(|Γ|) per mint — the difference between linear and quadratic elaboration over a module.
    ///
    /// Γ is the *local* binders only — `local` past [`Frames::base_locals`]. Top-level definitions are excluded so an item's elaboration is independent of how much else is in scope: a metavariable born deep in a proof carries just its enclosing binders, not the whole prelude, keeping the contextual solve's spine a small pattern (and the prelude cacheable). Globals a solution mentions are admitted by the solver's scope check via [`Frames::is_top_level`] instead.
    pub(crate) fn identity_snapshot(&mut self) -> (SharedTelescope, SharedSpine) {
        if let Some((stamp, telescope, spine)) = &self.identity_cache
            && *stamp == self.locals_stamp.count()
        {
            return (telescope.clone(), spine.clone());
        }

        // One entry per name, at its innermost binding. `retype_locals` re-`assume`s a generalized hypothesis under its case-specialized type and its *original* name, deliberately shadowing the ambient binder, so `local` can hold the same `Free` twice. Γ is a context rather than a stack of bindings: a shadowed entry is unreachable by construction, and leaving it in gives every metavariable born in such an arm a spine with a repeated argument — which `Convert::solve`'s inversion cannot invert, since a name reachable through two slots is not provably determined. The candidate is then refused by the scope check for mentioning a hypothesis that is plainly in scope, and the implicit surfaces as never solved.
        //
        // Keeping the *last* occurrence keeps the telescope well-scoped: everything a generalized hypothesis's type can mention was itself generalized — that is what the generalization set is — so every mentioner is re-assumed after it, and no surviving entry refers to the occurrence that was dropped.
        let locals = &self.local[self.visible_local_start()..];
        let telescope = Rc::new({
            let mut innermost = HashMap::with_capacity(locals.len());
            for (index, (name, _)) in locals.iter().enumerate() {
                innermost.insert(name, index);
            }

            locals
                .iter()
                .enumerate()
                .filter(|(index, (name, _))| innermost[name] == *index)
                .map(|(_, entry)| entry.clone())
                .collect::<Vec<_>>()
        });

        let spine = Rc::new(
            telescope
                .iter()
                .map(|(name, _)| Term::free_var(name))
                .collect::<Vec<_>>(),
        );

        self.identity_cache = Some((self.locals_stamp.count(), telescope.clone(), spine.clone()));

        (telescope, spine)
    }

    /// Freeze the live local frame (the way metavariable birth freezes Γ): the base frame persists for the whole elaboration, so only the local frames are captured, and they are the whole of a retry's context — `Context::with_retry_frame` hides whatever other frames are live when the retry runs.
    pub(crate) fn freeze(&self) -> FrozenFrame {
        // The frames a retry would hide are not this problem's context either, so a problem parked during a retry freezes what the retry sees. Definitions are the exception, as they are for every lookup: a definition restored by being left live sits in a hidden frame, and is still the problem's.
        let from = self.retry_floor.unwrap_or(1);

        FrozenFrame {
            // Past `base_locals`, exactly as `identity_snapshot` slices Γ. The whole of `local` would also carry the top-level binders, and `restore_frame` re-`assume`s whatever it is given — which stamps each restored name with an *empty* universe context in the new frame. A polymorphic global would then be shadowed by a monomorphic copy of itself, and instantiating it at its real levels fails the arity check against the wrong scheme.
            assumptions: self.local[self.visible_local_start()..].to_vec(),
            definitions: flatten_frames(&self.definitions[1..]),
            refinements: Refinements {
                variables: flatten_recorded(&self.refinements[from..]),
                projections: flatten_recorded(&self.refinement_projections[from..]),
                scrutinees: flatten_recorded(&self.refinement_scrutinees[from..]),
            },
            witness_binders: self.witness_scope[self.visible_witness_start()..].to_vec(),
        }
    }
}

/// Every entry of `frames`, outermost frame first.
fn flatten_frames<K: Clone, V: Clone>(frames: &[HashMap<K, V>]) -> Vec<(K, V)> {
    frames
        .iter()
        .flat_map(|frame| frame.iter().map(|(k, v)| (k.clone(), v.clone())))
        .collect()
}

/// Every refinement of `frames`, outermost frame first, each frame's in the order it recorded them.
fn flatten_recorded<K: Eq + Hash + Clone, V: Clone>(frames: &[Recorded<K, V>]) -> Vec<(K, V)> {
    frames
        .iter()
        .flat_map(|frame| frame.iter().map(|(k, v)| (k.clone(), v.clone())))
        .collect()
}
