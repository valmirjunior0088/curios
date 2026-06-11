use {
    super::{Bound, Goal, ImplicitOrigin, Inductive, Metavar, Term, Var},
    crate::{Entropy, Span},
    std::{
        collections::{BTreeMap, BTreeSet, HashMap},
        time::{Duration, Instant},
    },
};

/// One metavariable's record in the [`MetaStore`]. Everything here is frozen at
/// birth except `solution`, which transitions `None -> Some(_)` exactly once.
#[derive(Debug)]
pub struct MetaEntry {
    /// Γ frozen at birth: the local assumption context in binding order, with
    /// birth-time types. Drives the scope check and re-validation (§7.3–§7.4).
    pub telescope: Vec<(String, Term)>,
    /// The metavariable's type — the `expected` it was checked against at birth.
    pub result: Term,
    /// `None` while unsolved; `Some(t)` once solved. `t`'s free `Var`s are a
    /// subset of `telescope`'s names.
    pub solution: Option<Term>,
    /// Birth-site span, for `cannot_infer` / unsolved-hole errors.
    pub span: Option<Span>,
}

/// Flat, frame-independent store of metavariable records, indexed by
/// `Metavar::id`. Its contents are monotonic facts about the program being
/// elaborated, not lexically-scoped bindings — so `enter_frame`/`leave_frame`
/// never touch it.
#[derive(Debug, Default)]
pub struct MetaStore {
    entries: Vec<Option<MetaEntry>>,
}

/// A conversion constraint that quiesced blocked on unsolved metavariables,
/// parked by `expect` to outlive its call (§8). Like a [`MetaEntry`], it
/// freezes the local frame it was born under — assumptions *and* local
/// definitions, since retry-time reduction needs both. Counterfactual
/// refinements are deliberately not frozen: solutions are
/// refinement-independent by design (re-validation suppresses them), so a
/// retry without them can only conservatively fail, never wrongly succeed.
#[derive(Debug)]
pub struct ParkedGoal {
    pub goal: Goal,
    /// The term `expect` was checking; its span anchors the eventual error if
    /// the constraint never resolves.
    pub origin: Term,
    /// The local assumption context frozen at park time, in binding order.
    pub assumptions: Vec<(String, Term)>,
    /// The non-base-frame definitions frozen at park time (outermost frame
    /// first, so re-defining in order reproduces the shadowing).
    pub definitions: Vec<(String, Term)>,
    /// The unsolved metavariables either side mentions — solving any of them
    /// is the wake signal.
    pub watching: BTreeSet<usize>,
}

#[derive(Debug)]
pub struct Context {
    entropy: Entropy,
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
    // While set, `expect` may not park: conversion is being used as a yes/no
    // oracle (re-validation) and provisional success would leak into it.
    suppress_parking: bool,
    metas: MetaStore,
    // The next metavariable id this context may mint (implicit-argument
    // insertion). Seeded by `elaborate_module` from `Module::metavars` so
    // core-minted ids sit strictly above `to_core`'s.
    next_metavar: Entropy,
    // Inductive declarations, keyed by the type's qualified name ("Result").
    // Like `metas`, a flat store of monotonic facts about the program, not
    // lexically-scoped bindings — `enter_frame`/`leave_frame` never touch it.
    inductives: BTreeMap<String, Inductive>,
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
            entropy: Entropy::<usize>::new(),
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
            parked: Vec::new(),
            newly_solved: Vec::new(),
            suppress_parking: false,
        }
    }

    pub fn fresh(&mut self, hint: Option<&str>) -> String {
        let counter = self.entropy.fresh();

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

    pub fn assume<A>(&mut self, label: A, type_: &Term)
    where
        A: Into<String>,
    {
        let label = label.into();
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

    /// The local assumption context in binding order — the Γ a metavariable
    /// freezes at birth (§5). Includes every `assume`d binder currently in
    /// scope, across all open frames.
    pub fn local_context(&self) -> &[(String, Term)] {
        &self.local
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

    /// Record an inductive declaration's metadata. Called once per `union`
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

    // === Metavariable store =================================================

    /// Materialize a metavariable's birth record (§5). The store grows to cover
    /// `id`; births happen exactly once per id (each `_` is distinct and occurs
    /// once).
    pub fn birth_metavar(
        &mut self,
        id: usize,
        telescope: Vec<(String, Term)>,
        result: Term,
        span: Option<Span>,
    ) {
        if id >= self.metas.entries.len() {
            self.metas.entries.resize_with(id + 1, || None);
        }

        self.metas.entries[id] = Some(MetaEntry {
            telescope,
            result,
            solution: None,
            span,
        });
    }

    /// Raise the minting floor: every id `fresh_metavar` hands out will be
    /// `>= floor`. Called by `elaborate_module` with `Module::metavars` (the
    /// count `to_core` minted) before any item is elaborated.
    pub fn seed_metavars(&mut self, floor: usize) {
        self.next_metavar.seed(floor);
    }

    /// Mint a metavariable for an omitted implicit argument and birth it
    /// immediately — frozen local Γ, the binder's instantiated type as
    /// `result`, and the *call site's* span — so the id always has a useful
    /// birth record. Returns the metavariable term carrying that span and the
    /// insertion provenance (which rides on the node; see [`Metavar::origin`]).
    pub fn fresh_metavar(
        &mut self,
        result: Term,
        span: Option<Span>,
        origin: ImplicitOrigin,
    ) -> Term {
        let id = self.next_metavar.fresh();

        let telescope = self.local_context().to_vec();
        let spine = telescope
            .iter()
            .map(|(name, _)| Term::var(Var::free(name)))
            .collect();
        self.birth_metavar(id, telescope, result, span.clone());

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
        self.metas
            .entries
            .get(id)
            .and_then(Option::as_ref)
            .and_then(|e| e.solution.as_ref())
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
    /// wake signal for parked constraints (§8).
    pub fn solve_metavar(&mut self, id: usize, term: Term) {
        if let Some(Some(entry)) = self.metas.entries.get_mut(id) {
            entry.solution = Some(term);
            self.newly_solved.push(id);
            self.reductions.clear();
        }
    }

    // === Parked constraints (§8) ============================================

    /// Park a blocked conversion goal: freeze the live local frame around it
    /// (the way `fresh_metavar` freezes Γ — assumptions plus the local
    /// definitions retry-time reduction will need) and record which unsolved
    /// metavariables could unblock it.
    pub fn park(&mut self, goal: Goal, origin: Term) {
        let assumptions = self.local.clone();
        // The base frame persists for the whole elaboration; only the local
        // frames pop before a retry can happen. Outermost first, so
        // re-defining in order reproduces the shadowing.
        let definitions = self
            .definitions
            .iter()
            .skip(1)
            .flat_map(|frame| frame.iter().map(|(n, t)| (n.clone(), t.clone())))
            .collect();

        self.repark(goal, origin, assumptions, definitions);
    }

    /// Re-park a goal that is still blocked after a retry, keeping its
    /// originally frozen frame. The watch set is recomputed from the goal's
    /// current unsolved metavariables.
    pub fn repark(
        &mut self,
        goal: Goal,
        origin: Term,
        assumptions: Vec<(String, Term)>,
        definitions: Vec<(String, Term)>,
    ) {
        let watching = goal
            .this
            .metavars()
            .into_iter()
            .chain(goal.that.metavars())
            .filter(|id| self.metavar_solution(*id).is_none())
            .collect();

        self.parked.push(ParkedGoal {
            goal,
            origin,
            assumptions,
            definitions,
            watching,
        });
    }

    /// Take the parked goals woken by solutions landed since the last sweep.
    /// Consumes the wake signals; empty when nothing new has been solved.
    pub fn wake_parked(&mut self) -> Vec<ParkedGoal> {
        if self.newly_solved.is_empty() || self.parked.is_empty() {
            self.newly_solved.clear();
            return Vec::new();
        }

        let solved = self.newly_solved.drain(..).collect::<BTreeSet<_>>();
        let (woken, kept) = std::mem::take(&mut self.parked)
            .into_iter()
            .partition(|p| p.watching.iter().any(|id| solved.contains(id)));
        self.parked = kept;

        woken
    }

    /// Take every parked goal — the drain's final sweep.
    pub fn take_parked(&mut self) -> Vec<ParkedGoal> {
        std::mem::take(&mut self.parked)
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
