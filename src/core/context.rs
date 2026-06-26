use {
    super::{Bound, Goal, ImplicitOrigin, Inductive, Metavar, Structure, Term},
    crate::{Entropy, Span},
    std::{
        collections::{BTreeMap, BTreeSet, HashMap},
        mem,
        rc::Rc,
        time::{Duration, Instant},
    },
};

/// Γ frozen in binding order, with birth-time types. `Rc`-shared: every meta
/// born under the same Γ shares one allocation (see
/// [`Context::identity_snapshot`]).
pub type SharedTelescope = Rc<Vec<(String, Term)>>;

/// The identity spine over a [`SharedTelescope`] — one `Var::free` per binder
/// — shared the same way.
pub type SharedSpine = Rc<Vec<Term>>;

/// One metavariable's record in the [`MetaStore`]. Everything here is frozen at
/// birth except `solution`, which transitions `None -> Some(_)` exactly once.
#[derive(Debug)]
pub struct MetaEntry {
    /// Γ frozen at birth: the local assumption context in binding order, with
    /// birth-time types. Drives the scope check and re-validation (§7.3–§7.4).
    pub telescope: SharedTelescope,
    /// The metavariable's type — the `expected` it was checked against at birth.
    pub result: Term,
    /// `None` while unsolved; `Some(t)` once solved. `t`'s free `Var`s are a
    /// subset of `telescope`'s names.
    pub solution: Option<Term>,
}

/// Flat, frame-independent store of metavariable records, indexed by
/// `Metavar::id`. Its contents are monotonic facts about the program being
/// elaborated, not lexically-scoped bindings — so `enter_frame`/`leave_frame`
/// never touch it.
#[derive(Debug, Default)]
pub struct MetaStore {
    entries: Vec<Option<MetaEntry>>,
}

/// The local frame a parked problem froze at park time: assumptions (in
/// binding order), and the non-base-frame definitions, counterfactual
/// refinements, and projection refinements (each outermost frame first, so
/// reapplying in order reproduces the shadowing). A retry must run under the
/// same equalities its origin saw — including the arm-local refinements —
/// while solution re-validation independently suppresses them, keeping
/// committed solutions refinement-free.
#[derive(Debug, Clone)]
pub struct FrozenFrame {
    pub assumptions: Vec<(String, Term)>,
    pub definitions: Vec<(String, Term)>,
    pub refinements: Vec<(String, Term)>,
    pub refinement_projections: Vec<((Term, usize), Term)>,
}

/// The work a parked problem will retry (§8).
#[derive(Debug)]
pub enum ParkedWork {
    /// A conversion constraint that quiesced blocked on unsolved
    /// metavariables.
    Conversion(Goal),
    /// A whole checking problem: a checked-only introduction form met an
    /// expected type whose structure is still an unsolved metavariable. The
    /// `placeholder` metavariable stands in for the rebuilt term in the tree
    /// and is solved with it once the check can run — the spine machinery
    /// then splices it everywhere the occurrence travelled.
    Checking {
        term: Term,
        expected: Term,
        placeholder: usize,
    },
}

/// A problem parked by `expect` (or a blocked intro-form check) to outlive
/// its call (§8). Like a [`MetaEntry`], it freezes the local frame it was
/// born under.
#[derive(Debug)]
pub struct ParkedGoal {
    pub work: ParkedWork,
    /// The term being checked at park time; its span anchors the eventual
    /// error if the problem never resolves.
    pub origin: Term,
    pub frame: FrozenFrame,
    /// The unsolved metavariables whose solutions could unblock this —
    /// solving any of them is the wake signal.
    pub watching: BTreeSet<usize>,
}

#[derive(Debug)]
pub struct Context {
    fresh_names: Entropy,
    deadline: Instant,
    reductions: HashMap<Term, Term>,
    assumptions: Vec<HashMap<String, Term>>,
    definitions: Vec<HashMap<String, Term>>,
    // Counterfactual match-arm refinements (`refine_head`), kept parallel to
    // `definitions` but suppressible: re-validation of a metavariable
    // solution (§7.4) must keep stable definitions yet ignore these.
    refinements: Vec<HashMap<String, Term>>,
    refinement_projections: Vec<HashMap<(Term, usize), Term>>,
    suppress_refinements: bool,
    // The local assumption context in binding order (a companion to
    // `assumptions`, which is keyed by name and loses order). `assume` appends;
    // frames are delimited by `local_marks`.
    local: Vec<(String, Term)>,
    local_marks: Vec<usize>,
    // Parked conversion constraints (§8) — frame-independent, like `metas`.
    parked: Vec<ParkedGoal>,
    // Ids solved since the last `wake_parked` sweep: the wake signal.
    newly_solved: Vec<usize>,
    // Journal of every committed solution id, in commit order — never
    // consumed, only marked and rolled back. The watermark/rollback pair lets
    // re-validation (§7.4) unwind solutions that landed while validating a
    // candidate it then rejected.
    solved_log: Vec<usize>,
    // While set, `expect` may not park: conversion is being used as a yes/no
    // oracle (re-validation) and provisional success would leak into it.
    suppress_parking: bool,
    // One tick per mutation of `local` (assume, frame exit, reassume) —
    // an `Entropy` used as a version stamp: `fresh()` bumps, `count()` reads.
    // Invalidates `identity_cache`, which shares the frozen telescope and
    // identity spine between every meta born under an unchanged Γ.
    locals_stamp: Entropy,
    identity_cache: Option<(usize, SharedTelescope, SharedSpine)>,
    metas: MetaStore,
    // The next metavariable id this context may mint (implicit-argument
    // insertion). Seeded by `elaborate_module` with its `metavar_floor`
    // argument so core-minted ids sit strictly above `to_core`'s.
    next_metavar: Entropy,
    // Inductive declarations, keyed by the type's qualified name ("Result").
    // Like `metas`, a flat store of monotonic facts about the program, not
    // lexically-scoped bindings — `enter_frame`/`leave_frame` never touch it.
    inductives: BTreeMap<String, Inductive>,
    // Struct declarations, keyed the same way — a flat monotonic store like
    // `inductives`. Consulted by `elaborate_struct`/`elaborate_proj`/`erase`.
    structures: BTreeMap<String, Structure>,
    // The module whose item is currently being elaborated — the qualifier
    // prefix of that item's name (the root module is the empty string). Set by
    // `elaborate_module` per item; read by `elaborate_proj` for the struct
    // representation-privacy check (§7).
    island: String,
}

// Safety: `Term` keys contain `OnceCell` fields for caching, which triggers Clippy's
// interior mutability warning. However, the logical value is fully immutable, and the
// hash/equality check remains stable.
#[allow(clippy::mutable_key_type)]
impl Context {
    // The deadline is set once at construction and shared across every
    // `reduce`/`convert`/`infer`/`erase` call that uses this context, so the
    // timeout bounds total work, not per-call work.
    pub fn new(timeout: Duration) -> Self {
        Self {
            fresh_names: Entropy::<usize>::new(),
            deadline: Instant::now() + timeout,
            reductions: HashMap::new(),
            assumptions: vec![HashMap::new()],
            definitions: vec![HashMap::new()],
            refinements: vec![HashMap::new()],
            refinement_projections: vec![HashMap::new()],
            suppress_refinements: false,
            local: Vec::new(),
            local_marks: Vec::new(),
            metas: MetaStore::default(),
            next_metavar: Entropy::<usize>::new(),
            inductives: BTreeMap::new(),
            structures: BTreeMap::new(),
            island: String::new(),
            parked: Vec::new(),
            newly_solved: Vec::new(),
            solved_log: Vec::new(),
            suppress_parking: false,
            locals_stamp: Entropy::new(),
            identity_cache: None,
        }
    }

    pub fn fresh(&mut self, hint: Option<&str>) -> String {
        let counter = self.fresh_names.fresh();

        match hint {
            Some(h) => format!("{h}#{counter}"),
            None => format!("#{counter}"),
        }
    }

    pub fn deadline(&self) -> Instant {
        self.deadline
    }

    pub fn get_or_init_reduced<E>(
        &mut self,
        term: Term,
        compute: impl FnOnce(&mut Self, Term) -> Result<Term, E>,
    ) -> Result<Term, E> {
        if let Some(cached) = self.reductions.get(&term) {
            return Ok(cached.clone());
        }

        let result = compute(self, term.clone())?;

        if term.closed() {
            self.reductions.insert(term, result.clone());
        }

        Ok(result)
    }

    fn enter_frame(&mut self) {
        self.assumptions.push(HashMap::new());
        self.definitions.push(HashMap::new());
        self.refinements.push(HashMap::new());
        self.refinement_projections.push(HashMap::new());
        self.local_marks.push(self.local.len());
    }

    fn leave_frame(&mut self) {
        self.locals_stamp.fresh();
        self.assumptions.pop().unwrap();
        let definitions = self.definitions.pop().unwrap();
        let refinements = self.refinements.pop().unwrap();
        let refinement_projections = self.refinement_projections.pop().unwrap();
        self.local.truncate(self.local_marks.pop().unwrap());

        if !definitions.is_empty() || !refinements.is_empty() || !refinement_projections.is_empty()
        {
            self.reductions.clear();
        }
    }

    pub fn with_frame<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.enter_frame();
        let result = f(self);
        self.leave_frame();

        result
    }

    /// Assume `label : type_`. Erasure is sort-driven (a proof or a type erases),
    /// so a binder carries no runtime-multiplicity mark.
    pub fn assume<A>(&mut self, label: A, type_: &Term)
    where
        A: Into<String>,
    {
        let label = label.into();
        self.locals_stamp.fresh();
        self.local.push((label.clone(), type_.clone()));

        self.assumptions
            .last_mut()
            .unwrap()
            .insert(label, type_.clone());
    }

    /// Replace the type of an existing assumption in place — the innermost
    /// binding of `label`. Used by the `rec` elaborators: a group's signatures
    /// must be assumed (lowered) before they can be elaborated, since members
    /// reference each other, and are then upgraded here to their rebuilt forms
    /// — implicit insertion makes the two no longer interchangeable, and a
    /// lowered type must never leak into later reduction.
    pub fn reassume(&mut self, label: &str, type_: &Term) {
        self.locals_stamp.fresh();

        if let Some(entry) = self.local.iter_mut().rev().find(|(name, _)| name == label) {
            entry.1 = type_.clone();
        }

        if let Some(assumptions) = self
            .assumptions
            .iter_mut()
            .rev()
            .find(|assumptions| assumptions.contains_key(label))
        {
            assumptions.insert(label.to_string(), type_.clone());
        }
    }

    pub fn assumption(&self, label: &str) -> Option<&Term> {
        self.assumptions
            .iter()
            .rev()
            .find_map(|assumptions| assumptions.get(label))
    }

    pub fn define<A>(&mut self, label: A, term: &Term)
    where
        A: Into<String>,
    {
        self.definitions
            .last_mut()
            .unwrap()
            .insert(label.into(), term.clone());

        self.reductions.clear();
    }

    pub fn definition(&self, label: &str) -> Option<&Term> {
        self.definitions
            .iter()
            .rev()
            .find_map(|definitions| definitions.get(label))
    }

    pub fn define_assuming<A>(&mut self, label: A, type_: &Term, term: &Term)
    where
        A: Into<String>,
    {
        let label = label.into();
        self.assume(label.as_str(), type_);
        self.define(label, term);
    }

    // === Refinements ========================================================

    /// Register a counterfactual match-arm refinement of a variable. Unlike
    /// `define`, this lives in a suppressible store so re-validation can ignore
    /// it. Clears the reduction cache, as the variable now reduces differently.
    pub fn refine<A>(&mut self, label: A, term: &Term)
    where
        A: Into<String>,
    {
        self.refinements
            .last_mut()
            .unwrap()
            .insert(label.into(), term.clone());

        self.reductions.clear();
    }

    /// Register a counterfactual refinement of a projection (`refine_head` on a
    /// `Proj` scrutinee).
    pub fn refine_projection(&mut self, base: Term, index: usize, value: Term) {
        self.refinement_projections
            .last_mut()
            .unwrap()
            .insert((base, index), value);

        self.reductions.clear();
    }

    /// The reduct of a variable: its definition, or — unless refinements are
    /// suppressed — its counterfactual refinement. Labels never appear in both
    /// stores (definitions name `let`/`rec` binders; refinements name assumed
    /// scrutinee heads), so the order between them is immaterial.
    pub fn var_reduct(&self, label: &str) -> Option<&Term> {
        if !self.suppress_refinements
            && let Some(term) = self.refinements.iter().rev().find_map(|r| r.get(label))
        {
            return Some(term);
        }

        self.definition(label)
    }

    /// The reduct of a projection: its counterfactual match-arm refinement,
    /// unless refinements are suppressed (re-validation, §7.4).
    pub fn proj_reduct(&self, base: &Term, index: usize) -> Option<&Term> {
        if self.suppress_refinements {
            return None;
        }

        self.refinement_projections
            .iter()
            .rev()
            .find_map(|p| p.get(&(base.clone(), index)))
    }

    /// Whether any counterfactual refinement is currently registered (and not
    /// already suppressed) — the gate for the refinement-free candidate
    /// re-reduction in `Convert::solve_refinement_free`, so the common
    /// refinement-free path pays nothing.
    pub fn has_refinements(&self) -> bool {
        !self.suppress_refinements
            && (self.refinements.iter().any(|frame| !frame.is_empty())
                || self
                    .refinement_projections
                    .iter()
                    .any(|frame| !frame.is_empty()))
    }

    /// Run `f` with refinements suppressed (re-validation, §7.4). Brackets the
    /// region with reduction-cache clears so refinement-applied and
    /// refinement-suppressed reducts never contaminate each other's cache.
    pub fn with_suppressed_refinements<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let previous = self.suppress_refinements;
        self.reductions.clear();
        self.suppress_refinements = true;
        let result = f(self);
        self.suppress_refinements = previous;
        self.reductions.clear();

        result
    }

    // === Inductive registry =================================================

    /// Record an inductive declaration's metadata. Called once per `induct`
    /// declaration as the module's items are processed, alongside the
    /// `define`s for its type-constructor and value-constructor functions.
    pub fn register_inductive<N>(&mut self, name: N, inductive: Inductive)
    where
        N: Into<String>,
    {
        self.inductives.insert(name.into(), inductive);
    }

    /// Look up an inductive declaration by the type's qualified name.
    pub fn inductive(&self, name: &str) -> Option<&Inductive> {
        self.inductives.get(name)
    }

    // === Struct registry ====================================================

    /// Record a struct declaration's metadata. Called once per `struct`
    /// declaration as the module's items are processed (and again when the
    /// module is seeded into a fresh `Context` for erasure).
    pub fn register_structure<N>(&mut self, name: N, structure: Structure)
    where
        N: Into<String>,
    {
        self.structures.insert(name.into(), structure);
    }

    /// Look up a struct declaration by the type's qualified name.
    pub fn structure(&self, name: &str) -> Option<&Structure> {
        self.structures.get(name)
    }

    /// The module whose item is currently being elaborated (the qualifier
    /// prefix of its name; empty for the root). Used by the struct projection
    /// privacy check.
    pub fn island(&self) -> &str {
        &self.island
    }

    /// Set the current module before elaborating an item (see
    /// `elaborate_module`).
    pub fn set_island<N: Into<String>>(&mut self, module: N) {
        self.island = module.into();
    }

    // === Metavariable store =================================================

    /// Materialize a metavariable's birth record (§5). The store grows to cover
    /// `id`; births happen exactly once per id (each `_` is distinct and occurs
    /// once).
    pub fn birth_metavar(
        &mut self,
        id: usize,
        telescope: impl Into<SharedTelescope>,
        result: Term,
    ) {
        if id >= self.metas.entries.len() {
            self.metas.entries.resize_with(id + 1, || None);
        }

        self.metas.entries[id] = Some(MetaEntry {
            telescope: telescope.into(),
            result,
            solution: None,
        });
    }

    /// The boundary between the top-level (base-frame) entries of `local` and
    /// the genuine local binders above them. Top-level definitions are
    /// `assume`d into `local` at the base level (never inside a frame), so the
    /// outermost frame mark is exactly the count of top-level entries; with no
    /// frame open, everything in `local` is top-level. A metavariable's Γ is
    /// only the binders past this point (see [`Context::identity_snapshot`]).
    fn base_locals(&self) -> usize {
        self.local_marks.first().copied().unwrap_or(self.local.len())
    }

    /// Whether `name` is bound at the top level (the persistent base frame) —
    /// a global definition, always in scope. The metavariable solver admits
    /// such names in a solution even though they are not in the metavariable's
    /// Γ/spine (which holds only local binders): a solution may freely mention
    /// a global constant without that constant being a context binder.
    pub fn is_top_level(&self, name: &str) -> bool {
        self.assumptions
            .first()
            .is_some_and(|frame| frame.contains_key(name))
    }

    /// The frozen telescope and identity spine for the *current* Γ, shared:
    /// rebuilt only when `local` has changed since the last birth, so minting
    /// a metavariable is O(1) amortized instead of O(|Γ|) per mint — the
    /// difference between linear and quadratic elaboration over a module.
    ///
    /// Γ is the *local* binders only — `local` past [`Context::base_locals`].
    /// Top-level definitions are excluded so an item's elaboration is
    /// independent of how much else is in scope: a metavariable born deep in a
    /// proof carries just its enclosing binders, not the whole prelude, keeping
    /// the contextual solve's spine a small pattern (and the prelude cacheable).
    /// Globals a solution mentions are admitted by the solver's scope check via
    /// [`Context::is_top_level`] instead.
    pub fn identity_snapshot(&mut self) -> (SharedTelescope, SharedSpine) {
        if let Some((stamp, telescope, spine)) = &self.identity_cache
            && *stamp == self.locals_stamp.count()
        {
            return (telescope.clone(), spine.clone());
        }

        let telescope = Rc::new(self.local[self.base_locals()..].to_vec());

        let spine = Rc::new(
            telescope
                .iter()
                .map(|(name, _)| Term::free_var(name))
                .collect::<Vec<_>>(),
        );

        self.identity_cache = Some((self.locals_stamp.count(), telescope.clone(), spine.clone()));

        (telescope, spine)
    }

    /// Raise the minting floor: every id `fresh_metavar` hands out will be
    /// `>= floor`. Called by `elaborate_module` with its `metavar_floor`
    /// argument (the count `to_core` minted) before any item is elaborated.
    pub fn seed_metavars(&mut self, floor: usize) {
        self.next_metavar.seed(floor);
    }

    /// Mint a metavariable for an omitted implicit argument and birth it
    /// immediately — frozen local Γ, the binder's instantiated type as
    /// `result` — so the id always has a birth record. Returns the
    /// metavariable term carrying the *call site's* span and the insertion
    /// provenance (which rides on the node; see [`Metavar::origin`]).
    pub fn fresh_metavar(
        &mut self,
        result: Term,
        span: Option<Span>,
        origin: ImplicitOrigin,
    ) -> Term {
        let id = self.next_metavar.fresh();
        let (telescope, spine) = self.identity_snapshot();
        self.birth_metavar(id, telescope, result);
        let metavar = Term::metavar_inserted(id, origin, spine);

        match span {
            Some(span) => metavar.with_span(span),
            None => metavar,
        }
    }

    pub fn metavar_entry(&self, id: usize) -> Option<&MetaEntry> {
        self.metas.entries.get(id).and_then(Option::as_ref)
    }

    pub fn metavar_solution(&self, id: usize) -> Option<&Term> {
        self.metavar_entry(id).and_then(|e| e.solution.as_ref())
    }

    /// Resolve a solved metavariable *at its occurrence*: the stored solution
    /// is spelled with the birth telescope's names, and the occurrence's spine
    /// records what each of those binders corresponds to here — so resolution
    /// is the solution with birth names rewritten through the spine. An empty
    /// spine (a never-rebuilt `to_core` hole) resolves as the identity.
    /// `None` while unsolved.
    pub fn resolve_metavar(&self, metavar: &Metavar) -> Option<Term> {
        let entry = self.metavar_entry(metavar.id)?;
        let solution = entry.solution.as_ref()?;

        if metavar.spine.is_empty() {
            return Some(solution.clone());
        }

        let labels = entry
            .telescope
            .iter()
            .map(|(name, _)| name.as_str())
            .collect::<Vec<_>>();

        let spine = metavar.spine.iter().collect::<Vec<_>>();

        Some(solution.capture(&labels).release(&spine))
    }

    /// Commit a metavariable's solution. Clears the reduction cache, since a
    /// bare metavariable is `reach == 0` (hence cacheable) and may have cached
    /// as itself while unsolved (§7.2). Records the id as newly solved — the
    /// wake signal for parked constraints (§8) — and journals it for
    /// [`Context::rollback_solutions`].
    pub fn solve_metavar(&mut self, id: usize, term: Term) {
        if let Some(Some(entry)) = self.metas.entries.get_mut(id) {
            entry.solution = Some(term);
            self.newly_solved.push(id);
            self.solved_log.push(id);
            self.reductions.clear();
        }
    }

    /// Watermark for [`Context::rollback_solutions`]: how many solutions have
    /// been committed so far.
    pub fn solution_mark(&self) -> usize {
        self.solved_log.len()
    }

    /// Unwind every solution committed since `mark` — the transactional
    /// bracket around re-validation (§7.4). Validating a candidate runs full
    /// elaboration, which can solve *other* metavariables along the way; if
    /// the candidate is ultimately rejected, those nested solutions were
    /// derived from an equation that never held and must not survive the
    /// verdict. Removes the unwound ids from the wake signals and clears the
    /// reduction cache, which may have cached reducts through them.
    pub fn rollback_solutions(&mut self, mark: usize) {
        if self.solved_log.len() <= mark {
            return;
        }

        let unwound = self.solved_log.split_off(mark);

        for id in &unwound {
            if let Some(Some(entry)) = self.metas.entries.get_mut(*id) {
                entry.solution = None;
            }
        }

        self.newly_solved.retain(|id| !unwound.contains(id));
        self.reductions.clear();
    }

    // === Parked constraints (§8) ============================================

    /// Freeze the live local frame (the way `fresh_metavar` freezes Γ): the
    /// base frame persists for the whole elaboration, so only the local
    /// frames — which pop before a retry can happen — are captured.
    pub fn freeze_frame(&self) -> FrozenFrame {
        fn flatten_frames<K: Clone, V: Clone>(frames: &[HashMap<K, V>]) -> Vec<(K, V)> {
            frames
                .iter()
                .skip(1)
                .flat_map(|frame| frame.iter().map(|(k, v)| (k.clone(), v.clone())))
                .collect()
        }

        FrozenFrame {
            assumptions: self.local.clone(),
            definitions: flatten_frames(&self.definitions),
            refinements: flatten_frames(&self.refinements),
            refinement_projections: flatten_frames(&self.refinement_projections),
        }
    }

    /// Reapply a frozen frame inside a fresh `with_frame`, restoring the
    /// equalities the parked problem's origin saw.
    pub fn restore_frame(&mut self, frame: &FrozenFrame) {
        for (name, type_) in &frame.assumptions {
            self.assume(name, type_);
        }

        for (name, value) in &frame.definitions {
            self.define(name, value);
        }

        for (name, value) in &frame.refinements {
            self.refine(name, value);
        }

        for ((base, index), value) in &frame.refinement_projections {
            self.refine_projection(base.clone(), *index, value.clone());
        }
    }

    /// Park blocked work: freeze the live local frame around it and record
    /// which unsolved metavariables could unblock it.
    pub fn park(&mut self, work: ParkedWork, origin: Term) {
        let frame = self.freeze_frame();
        self.repark(work, origin, frame);
    }

    /// Re-park work that is still blocked after a retry, keeping its
    /// originally frozen frame. The watch set is recomputed from the work's
    /// current unsolved metavariables.
    pub fn repark(&mut self, work: ParkedWork, origin: Term, frame: FrozenFrame) {
        let watching = match &work {
            ParkedWork::Conversion(goal) => goal
                .this
                .metavars()
                .into_iter()
                .chain(goal.that.metavars())
                .filter(|id| self.metavar_solution(*id).is_none())
                .collect(),
            ParkedWork::Checking { expected, .. } => expected
                .metavars()
                .into_iter()
                .filter(|id| self.metavar_solution(*id).is_none())
                .collect(),
        };

        self.parked.push(ParkedGoal {
            work,
            origin,
            frame,
            watching,
        });
    }

    /// Mint the placeholder metavariable for a parked checking problem (§8):
    /// birthed like any hole — frozen Γ, identity spine — with no insertion
    /// provenance. If it survives unsolved, the item drain reports the parked
    /// problem at its origin before zonk could ever meet the placeholder.
    pub fn fresh_placeholder(&mut self, result: Term, span: Option<Span>) -> (usize, Term) {
        let id = self.next_metavar.fresh();
        let (telescope, spine) = self.identity_snapshot();
        self.birth_metavar(id, telescope, result);
        let term = Term::metavar_birthed(id, None, spine);

        let term = match span {
            Some(span) => term.with_span(span),
            None => term,
        };

        (id, term)
    }

    /// Take the parked goals woken by solutions landed since the last sweep.
    /// Consumes the wake signals; empty when nothing new has been solved.
    pub fn wake_parked(&mut self) -> Vec<ParkedGoal> {
        if self.newly_solved.is_empty() || self.parked.is_empty() {
            self.newly_solved.clear();

            return Vec::new();
        }

        let solved = self.newly_solved.drain(..).collect::<BTreeSet<_>>();

        let (woken, kept) = mem::take(&mut self.parked)
            .into_iter()
            .partition(|p| p.watching.iter().any(|id| solved.contains(id)));

        self.parked = kept;

        woken
    }

    /// Take every parked goal — the drain's final sweep.
    pub fn take_parked(&mut self) -> Vec<ParkedGoal> {
        mem::take(&mut self.parked)
    }

    pub fn parked_len(&self) -> usize {
        self.parked.len()
    }

    pub fn has_newly_solved(&self) -> bool {
        !self.newly_solved.is_empty()
    }

    pub fn parking_suppressed(&self) -> bool {
        self.suppress_parking
    }

    /// Run `f` as a yes/no *oracle* around full elaboration (re-validation):
    /// parking is suppressed — `expect` treats `Blocked` as a mismatch and
    /// `retry_parked` is a no-op, so provisional success can neither leak into
    /// the verdict nor consume a parked obligation whose error the oracle
    /// would swallow — and counterfactual refinements are suppressed with it.
    /// The two suppressions are a package: an oracle that set only one would
    /// be subtly unsound, which is why the parking half has no public setter.
    pub fn with_oracle<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.with_suppressed_parking(|context| context.with_suppressed_refinements(f))
    }

    fn with_suppressed_parking<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let previous = self.suppress_parking;
        self.suppress_parking = true;
        let result = f(self);
        self.suppress_parking = previous;

        result
    }
}
