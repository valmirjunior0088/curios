mod caches;
pub(crate) use caches::*;

mod frames;
pub(crate) use frames::*;

mod program;
pub(crate) use program::*;

mod solutions;
pub(crate) use solutions::*;

#[cfg(test)]
mod tests;

use {
    super::{
        Error, HeadKey, UniverseMark, UniverseSolver, UniverseStateToken, Witness, WitnessKey,
    },
    curios_core::ReduceError,
    curios_core::{
        Bound, ConceptDecl, Consumption, Cost, DEFAULT_RETENTION_QUOTA, DefinitionKind, Free,
        Global, HeadTag, ImplicitOrigin, InductDecl, Level, MetaId, Metavar, MetavarOrigin,
        Retention, StructDecl, Term, Totality, UniverseConstraintKind, UniverseConstraintOrigin,
        UniverseContext, UniverseError, UniverseMetaId, UniverseRole, UniverseSeed, WitnessOrigin,
        instantiate_universe_levels_scoped,
    },
    curios_utilities::{Entropy, Mount, Qualifier, Span, SyntaxRegistry},
    std::{
        cell::Cell,
        collections::{BTreeMap, BTreeSet},
        mem,
        ops::{Deref, DerefMut},
        rc::Rc,
    },
};

/// Units of reduction work one declaration may spend before its budget is exhausted.
///
/// **Provisional, and it is the pricing that made it so.** A transition still costs one unit, but a construction now costs what it builds and a new peak of reduction depth costs the native frame it takes, so an old figure no longer buys what it used to and this was recalibrated rather than retained.
///
/// # What it is set against
///
/// **The prelude floor.** The heaviest declaration in the fixed prelude — still in `/std/BigNat/add` — measured between 2 500 000 and 3 000 000 units by bisecting this constant against the prelude build, where it was about 91 000 *steps* before. Thirty million keeps roughly the tenfold margin the previous figure held over the worst real declaration, which is the property that figure was chosen for. That bisection predates the closed machine and has not been retaken: the whole prelude still builds and certifies under this constant with the machine live, which is the fact the margin exists to protect, and the machine moves closed-fold costs down by an order of magnitude while moving some memo-amortized certifications up.
///
/// That is the *elaborator's* floor, which is the one this bisection reaches. The kernel's is readable directly rather than by bisection — `curios-prelude-archive`'s `stored_prelude_measurements` reports the heaviest declaration a whole-unit certification makes, **512 455 units at a peak depth of 6**, taken 2026-08-16 with the closed machine live (189 294 before it: the machine re-derives within a run where the strategy's memo hits were free, and buys that back a thousandfold on closed folds). The two floors are not comparable as a ratio: the elaborator solves metavariables, resolves witnesses and zonks where the kernel rechecks a finished term.
///
/// **What a unit costs in bytes.** Measured on the same machine, a payload-heavy program costs about **28 bytes of process memory per unit** — the logical unit is eight, and the rest is copies and term traffic the price list deliberately does not model. So this figure admits roughly 780 MB of construction in one declaration.
///
/// # Two things it does not buy, stated rather than left to be discovered
///
/// **A single oversized construction is still affordable, and no default the prelude can build under would refuse it.** `Nat/shl(1, 400000000)` prices at 6 250 004 units and builds fifty megabytes; refusing it outright needs a default of six million, which is twice the prelude's own floor with no margin at all. What the charge bought is a ceiling where there was none — the same term at a larger numeral now refuses instead of taking the machine — not one low enough to call fifty megabytes unreasonable. Squeezing both ends onto one number is the weighted single limit the specification's *Refused alternatives* accepts, and these are its numbers.
///
/// **A `Str` literal's ceiling is this constant divided by a per-character price the closed machine sets in transitions, not frames.** The scan used to nest one native reduction level per byte, and the frame row was nearly the whole of a character's 1 088-unit price, capping a literal near 16 700 characters; on the machine, guarded depth is flat in the length and the ceiling moved by an order of magnitude. The current price and ceiling live with their dates in `curios`' `str_literal_cost_measurements`, and `a_str_literal_costs_transitions_rather_than_frames` holds the shape. Raising this constant still raises the ceiling proportionally, and the trade it would make is the one stated above rather than anything about strings.
///
/// What "provisional" still means: this has *not* been set against a corpus that replays a memoized construction rather than evaluating it once, which the specification says must also decide it. That is the calibration milestone's work.
pub const DEFAULT_STEP_BUDGET: u64 = 30_000_000;

/// Γ frozen in binding order, with birth-time types. `Rc`-shared: every meta born under the same Γ shares one allocation (see [`Context::identity_snapshot`]).
type SharedTelescope = Rc<Vec<(Free, Term)>>;

/// The identity spine over a [`SharedTelescope`] — one `Var::free` per binder — shared the same way.
type SharedSpine = Rc<Vec<Term>>;

pub(crate) struct UniverseMutation<'a> {
    solver: &'a mut UniverseSolver,
    stamp: &'a Entropy,
    before: UniverseStateToken,
}

impl Deref for UniverseMutation<'_> {
    type Target = UniverseSolver;

    fn deref(&self) -> &Self::Target {
        self.solver
    }
}

impl DerefMut for UniverseMutation<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.solver
    }
}

impl Drop for UniverseMutation<'_> {
    fn drop(&mut self) {
        if self.solver.state_token() != self.before {
            self.stamp.fresh();
        }
    }
}

/// A transaction watermark spanning both unification stores.
#[derive(Debug, Clone, Copy)]
pub(crate) struct SolutionMark {
    term_solution_log_len: usize,
    universe: UniverseMark,
}

/// The kernel's ambient state, threaded mutably through elaboration, typing, reduction, conversion, and erasure. Two lifetimes coexist: the *frame-scoped* lexical state (`Frames`), pushed and popped as binders and match arms are entered, and the *flat monotonic facts* about the program (`Solutions`, `Program`), which frames never touch. The `Caches` police both with their write stamps, and this façade is where the two halves coordinate: any method that writes a store *and* must stamp or clear a cache lives here, naming both sub-stores explicitly. Reduction is bounded by a step budget restored at every declaration boundary — see [`Context::new`].
#[derive(Debug)]
pub struct Context {
    fresh_names: Entropy,
    /// Units of reduction work each declaration may spend, restored by [`Context::restore_budget`] at every declaration boundary. A transition costs one; a construction costs what it builds, per [`Cost`].
    budget: u64,
    /// Work left in the current declaration's budget. `Cell` because the conversion queue spends through a shared borrow.
    remaining: Cell<u64>,
    /// How many guarded reduction levels are live, and the deepest this declaration has reached. `Cell` for [`Context::remaining`]'s reason; see [`Context::enter_level`].
    depth: Cell<usize>,
    peak_depth: Cell<usize>,
    /// The heaviest declaration elaborated so far, for a measurement to read. See [`Consumption`]; nothing in elaboration consults it.
    heaviest: Cell<Consumption>,
    /// What this compilation may still retain in the caches below. Compilation-scoped and never restored, unlike the budget beside it — see [`Retention`].
    retention: Retention,
    // The reduction and elaboration memo tables with their write stamps and the named invalidation protocol every mutation site routes through; see [`Caches`].
    caches: Caches,
    // The frame-scoped lexical stores — assumptions, local definitions, refinements, the witness scope; see [`Frames`].
    frames: Frames,
    // The unification state — metavariable records, the solve journal, and parked/deferred work; flat and frame-independent. See [`Solutions`].
    solutions: Solutions,
    universe_solver: UniverseSolver,
    // The program-wide declaration registries, witness table, and totality verdicts — flat stores of monotonic facts about the program, not lexically-scoped bindings; `enter_frame`/`leave_frame` never touch them. See [`Program`].
    program: Program,
    // The module whose item is currently being elaborated — the qualifier prefix of that item's name (a fresh context starts at the root, the empty qualifier). Set by `elaborate_module_suffix` per item; read by the representation-privacy checks. `None` arises only through `with_suppressed_privacy` and means there is no surface use site to judge from, which suppresses the checks structurally: privacy is a property of *surface elaboration*, and machinery that re-derives types from already-elaborated terms — erasure, the metavariable oracle — walks compiler-built projections (witness splices, eta-expansions) that must not be re-adjudicated. A machinery path that forgets its bracket fails loudly (a spurious privacy error), never silently.
    island: Option<Qualifier>,
    // Every term elaboration settled, with the type it settled at — the seed of obligation (V). Recorded here rather than reconstructed afterwards because "what type was this checked against" is a fact elaboration computes for every term and a later walk can only re-derive, incompletely (see `crate::totality`). The site travels as an `Rc<str>` so recording is three pointer bumps.
    checked: Vec<(Term, Term, Rc<str>)>,
    // The definition whose body is currently elaborating, for those sites.
    checked_site: Rc<str>,
    // The `/syn` names the type-directed features synthesize — infix dispatch and row subsumption. Supplied rather than spelled: the elaborator knows *which* declaration it needs, and `curios-prelude` knows what that declaration is called. See [`Context::syntax`].
    syntax: SyntaxRegistry,
}

impl Context {
    /// A fresh, empty context at [`DEFAULT_STEP_BUDGET`] — what every caller that is not threading a user-supplied budget wants.
    pub fn with_default_budget(syntax: SyntaxRegistry) -> Self {
        Self::new(DEFAULT_STEP_BUDGET, syntax)
    }

    /// A fresh, empty context in which each declaration may spend `budget` reduction steps, synthesizing the `/syn` names `syntax` registers. Declarations, definitions, and the metavariable floor arrive later, seeded by `elaborate_module_suffix` as it walks the lowered module.
    ///
    /// The budget is *per declaration*, not per compilation: `elaborate_module_suffix` calls `Context::restore_budget` at every item boundary. A cumulative budget would make whether one declaration typechecks depend on how much the declarations before it had already spent, which is not a property of the declaration. Counting steps rather than elapsed time is what makes the answer a fact about the program instead of about the machine that ran it, so acceptance is reproducible across hosts, loads, and runs.
    ///
    /// The registry is a constructor argument rather than something a later call may or may not install, so an embedding cannot reach a type-directed feature with no vocabulary to emit. Whether the named declarations are actually *in scope* is a separate question the features already answer for themselves — a missing concept registration reports `no witness`, and row subsumption declines and lets the original mismatch speak.
    pub fn new(budget: u64, syntax: SyntaxRegistry) -> Self {
        Self {
            fresh_names: Entropy::<usize>::new(),
            budget,
            remaining: Cell::new(budget),
            depth: Cell::new(0),
            peak_depth: Cell::new(0),
            heaviest: Cell::new(Consumption::default()),
            retention: Retention::new(DEFAULT_RETENTION_QUOTA),
            caches: Caches::new(),
            frames: Frames::new(),
            solutions: Solutions::new(),
            universe_solver: UniverseSolver::new(0),
            program: Program::new(),
            island: Some(Qualifier::empty()),
            checked: Vec::new(),
            checked_site: Rc::from("the entrypoint"),
            syntax,
        }
    }

    /// The `/syn` names this elaboration may synthesize.
    pub(crate) fn syntax(&self) -> SyntaxRegistry {
        self.syntax
    }

    /// Record one settled term and the type it settled at — obligation (V)'s seed, collected where elaboration already knows the answer.
    ///
    /// Sort-hood is *not* decided here. The type may still carry unsolved metavariables, and deciding would both reduce on the hot path and risk an answer a later solution invalidates; the gate classifies post-zonk, memoized per distinct type.
    pub(crate) fn record_checked(&mut self, term: &Term, type_: &Term) {
        self.checked
            .push((term.clone(), type_.clone(), Rc::clone(&self.checked_site)));
    }

    /// Name the definition whose body is elaborating, for (V)'s diagnostics. Returns the previous site so the caller can restore it.
    pub(crate) fn set_checked_site(&mut self, site: &str) -> Rc<str> {
        mem::replace(&mut self.checked_site, Rc::from(site))
    }

    /// Restore a site saved by [`Context::set_checked_site`].
    pub(crate) fn restore_checked_site(&mut self, site: Rc<str>) {
        self.checked_site = site;
    }

    /// Read the recorded terms without consuming them — obligation (T) reads them first, and (V) drains afterwards.
    pub(crate) fn checked(&self) -> &[(Term, Term, Rc<str>)] {
        &self.checked
    }

    pub(crate) fn record_definition_totality(&mut self, name: &Global, totality: Totality) {
        self.program.record_definition_totality(name, totality);
    }

    pub(crate) fn definition_totality(&self, name: &Global) -> Option<Totality> {
        self.program.definition_totality(name)
    }

    pub(crate) fn seed_totality(&mut self, inherited: &BTreeMap<Global, Totality>) {
        self.program.seed_totality(inherited);
    }

    /// Drain the recorded terms. The gate takes them once per module.
    pub(crate) fn take_checked(&mut self) -> Vec<(Term, Term, Rc<str>)> {
        mem::take(&mut self.checked)
    }

    /// Mint a binder nothing else can name, rendering as `hint`.
    ///
    /// The counter is seeded above every index `into_core` minted ([`Context::set_local_floor`]), so a lowered binder and an elaborated one can never be the same identity.
    pub(crate) fn fresh(&mut self, hint: Option<&str>) -> Free {
        let index = u32::try_from(self.fresh_names.fresh()).expect("binder space exhausted");

        Free::local(index, hint)
    }

    /// Raise the binder counter above every index already minted elsewhere.
    ///
    /// `into_core` mints the binders of every lowered scope, and `core` mints more while elaborating them; both draw from one identity space, so the second source must start above the first. The archived prelude replays terms whose binders were minted in an earlier compiler run, and this is what keeps a fresh mint from aliasing one of them.
    pub fn set_local_floor(&mut self, floor: usize) {
        self.fresh_names.seed(floor);
    }

    /// Charge `cost` against the current declaration's budget, failing when it cannot be afforded.
    ///
    /// [`Cost::STEP`] at the three loops that drive reduction and conversion is what makes the budget bound every route into unbounded computation; a construction charge at every allocating fold is what makes it bound the *memory* those routes reach, which a transition count could not see.
    ///
    /// A saturated cost is refused without being compared, so a size that overflowed while being computed can never look affordable. That is the one case where the budget is not consulted at all, and [`Cost`]'s module documentation carries why.
    pub(crate) fn spend(&self, cost: Cost) -> Result<(), ReduceError> {
        if cost.is_refused() {
            return Err(ReduceError::exhausted(self.remaining.get(), cost));
        }

        match self.remaining.get().checked_sub(cost.get()) {
            Some(remaining) => {
                self.remaining.set(remaining);
                Ok(())
            }
            None => {
                // Built before the budget moves, from bounded metadata alone — see `curios-cert`'s `Spend::spend`, which does the same thing for the same reason.
                let refusal = ReduceError::exhausted(self.remaining.get(), cost);
                self.remaining.set(0);

                Err(refusal)
            }
        }
    }

    /// Enter one guarded reduction level, charging [`Cost::FRAME`] when it is deeper than any level this declaration has reached before.
    ///
    /// Per new peak rather than per call, for the reason `curios-cert`'s `Spend::enter_level` states in full: a level's native frame is reclaimed when the level returns, and reduction re-enters itself once per operand and once per spine link, so charging every call would price a stack the reduction is not holding. The kernel charges the same row the same way, which is what lets the two checkers' depth limits be compared.
    pub(crate) fn enter_level(&self) -> Result<(), ReduceError> {
        let depth = self.depth.get() + 1;
        self.depth.set(depth);

        if depth > self.peak_depth.get() {
            self.peak_depth.set(depth);
            self.spend(Cost::FRAME)?;
        }

        Ok(())
    }

    /// Leave a guarded reduction level. The peak stands; only the live count falls.
    pub(crate) fn leave_level(&self) {
        self.depth.set(self.depth.get() - 1);
    }

    /// Restore the full budget for a new declaration, and with it the depth this declaration may reach before paying again.
    ///
    /// The live count is reset for the reason `curios-cert`'s `Spend::restore_budget` states in full: [`Context::enter_level`] increments before it charges and propagates the refusal, so an exhausted level is never left, and elaboration continues to the next declaration. A leaked level costs every later declaration [`Cost::FRAME`] for a frame nothing holds.
    pub(crate) fn restore_budget(&mut self) {
        self.heaviest
            .set(self.heaviest.get().heavier_of(self.consumed()));

        self.remaining.set(self.budget);
        self.depth.set(0);
        self.peak_depth.set(0);
    }

    /// What the declaration being elaborated has consumed so far.
    fn consumed(&self) -> Consumption {
        Consumption::new(self.budget - self.remaining.get(), self.peak_depth.get())
    }

    /// The heaviest declaration this context has elaborated, including the one in progress.
    ///
    /// The measurement counterpart of [`Context::retained`], and what `curios-cert`'s `Kernel::heaviest_declaration` reads on the other side of the seam — the two are deliberately the same shape, because comparing them is the point. An observation for a measurement; nothing in elaboration reads it.
    pub fn heaviest_declaration(&self) -> Consumption {
        self.heaviest.get().heavier_of(self.consumed())
    }

    /// The read half of the reduction cache. The reducer probes it wherever a term's reduction begins — at entry, and at the scrutinee stack's frame push, where a warm scrutinee dispatches in place instead of framing.
    pub(crate) fn cached_reduced(&self, term: &Term) -> Option<Term> {
        if term.has_universe_meta() {
            return None;
        }
        self.caches.reduction_get(term)
    }

    /// Record that `term` reduces to `result` — the write half of the reduction cache, hit wherever a reduction's value lands: the reducer's final return, and its scrutinee stack's frame pop. Memoize only closed terms whose WHNF names no *unsolved* metavariable — `any_metavar` bails on the first one, never building the id set. A solve is monotonic, so it can only invalidate a reduct that still names the metavariable it solved, and reduction gets stuck on (hence surfaces) an unsolved metavariable it actually depends on. Refusing to cache those is what lets `solve_metavar` skip a cache clear; an entry naming only *solved* metavariables stays valid under forward solves (re-validation's `rollback_solutions`, which *un*-solves, clears separately).
    pub(crate) fn reduce(&mut self, term: Term, result: &Term) {
        let cacheable = term.closed()
            && !term.has_universe_meta()
            && !result.has_universe_meta()
            && !result.any_metavar(&mut |id| self.metavar_solution(id).is_none());

        // The key and the reduct both have their lifetimes extended by the insertion, so both are charged; the allowance is the compilation's rather than this declaration's, and exhausting it stops the cache accepting entries instead of refusing anything.
        let cost = Cost::collection(1)
            .saturating_add(Cost::units(term.footprint()))
            .saturating_add(Cost::units(result.footprint()));

        if cacheable && self.retention.admits(cost) {
            self.caches.reduction_insert(term, result.clone());
        }
    }

    /// The read half of the canonical-key memo. [`Caches::canonical_keys`] carries why the memo exists.
    pub(crate) fn cached_canonical_key(&self, key: &Term) -> Option<Term> {
        if key.has_universe_meta() {
            return None;
        }
        self.caches.canonical_key_get(key)
    }

    /// The write half, charged as [`Context::reduce`] is.
    ///
    /// The unsolved-metavariable condition that gate takes is deliberately absent. It keeps a *reduct* from being substituted into a term after a solve invalidated it; an entry here is only ever a comparison representative, so a stale one compares unequal to the candidate a solve produced — a miss, which is the refusing direction where the reduction cache's would be the admitting one. Keeping the condition would also exclude the entries the memo exists for: a guard over a metavariable-bearing subject is exactly the case whose recomputation is unaffordable.
    pub(crate) fn record_canonical_key(&mut self, key: Term, canonical: &Term) {
        let cacheable = key.closed() && !key.has_universe_meta();

        let cost = Cost::collection(1)
            .saturating_add(Cost::units(key.footprint()))
            .saturating_add(Cost::units(canonical.footprint()));

        if cacheable && self.retention.admits(cost) {
            self.caches.canonical_key_insert(key, canonical.clone());
        }
    }

    /// Run `attempt` with at most `allowance` units of this declaration's budget in reach, answering `None` when it did not finish inside that.
    ///
    /// **For work whose result is optional and whose cost must not be a program's cost.** Canonicalizing a refinement key is the case: settling it collapses two spellings of one comparison, failing to settle it leaves the two uncollapsed, and neither outcome changes what the program means. A guard over an opaque parameter settles in a handful of steps; one over a subject built by a hundred thousand iterations does not settle at all, and without a ceiling that single attempt spends the whole declaration.
    ///
    /// **What is spent is spent.** A bail is charged the allowance rather than refunded, so this is a cap and not free work — the invariant `Context::spend` states holds through it. What bounds the total is the memo the one caller keeps: an attempt happens once per key, so a declaration pays at most its guard count times this ceiling.
    pub(crate) fn within_allowance<T>(
        &mut self,
        allowance: u64,
        attempt: impl FnOnce(&mut Self) -> Result<T, ReduceError>,
    ) -> Result<Option<T>, ReduceError> {
        let before = self.remaining.get();
        let granted = before.min(allowance);
        self.remaining.set(granted);

        let outcome = attempt(self);
        let spent = granted.saturating_sub(self.remaining.get());
        self.remaining.set(before.saturating_sub(spent));

        match outcome {
            Ok(value) => Ok(Some(value)),
            // Exhaustion is the allowance's to absorb only when the allowance was the binding constraint. When the declaration's own remainder was smaller, the attempt spent the *declaration* out — swallowing that as an ordinary bail let elaboration continue at zero budget, where every later capped attempt failed for free and an unbounded retry spun without a single unit left to charge (the map-wall coda's literal-depth reproducer). The budget is the only bound that decides, so its exhaustion must outrank the cap that happened to be live.
            Err(error) if error.is_exhausted() && before <= allowance => Err(error),
            Err(_) => Ok(None),
        }
    }

    /// How much of this compilation's retention allowance the caches have consumed.
    ///
    /// An observation for a measurement, not a control: nothing in elaboration reads it, and what it is for is setting [`DEFAULT_RETENTION_QUOTA`] against a figure rather than a guess.
    pub fn retained(&self) -> u64 {
        self.retention.spent()
    }

    /// The elaboration-level counterpart of the reduction cache ([`Context::cached_reduced`] / [`Context::reduce`]): memoize `(term, expected) → (rebuilt, type)` for subterms whose elaboration can neither read nor write anything context-dependent. Eligibility is O(1) per call (the bits are cached per `Term` node): the term — and the expected type, when checking — must contain no metavariable and no `#`-named free variable (an elaborator-minted local or witness name; `#` cannot occur in a written identifier, so every other free name is a top-level reference). Writes are detected by snapshotting `mutation_stamp` around the computation: an entry is inserted only when the run minted, solved, parked, defined, and refined nothing — a pure run whose replay would be the identity on the context. Errors are never cached. The one deliberate delta on a hit: the skipped run's `expect` no longer drains `retry_parked` at that exact point — safe deferral, since retries re-run at every later `expect` and the module drain reports whatever survives.
    ///
    /// A cached entry additionally names only *already-defined* globals: the insert refuses any result — or `Check` expected — naming a not-yet-defined global (`Context::elaboration_cacheable`), the name analogue of the unsolved-metavariable refusal above. Definedness is the one ambient fact a pure, ground elaboration reads (through `expect`'s conversions, which unfold definitions), so an entry that surfaces only settled globals cannot be invalidated by a later *fresh* `define`. That is what lets `define_entry` drop its wholesale elaboration-cache clear and keep the memo warm across the `#`-minted definitions that reduction and the frame elaborators mint within one item. (`set_island` still clears at each top-level item boundary, so the survival is within-item.)
    ///
    /// The suppression brackets need no insert refusal. Privacy: validity is directional (an entry that passed an island's strict checks is valid under suppression, but not the reverse), so `island.is_some()` is part of the key — the erasure path's privacy-suppressed re-derivations populate and hit their own partition, and `set_island` clears on every item change. Parking: `expect` can only be `Blocked` on unsolved metavariables, which the groundness gate excludes, so suppression is inert for every cacheable run. Refinements: the registrar, frame-exit, and suppression-boundary clears already remove every entry a live refinement could have influenced, on both sides of the flag.
    ///
    /// Without this cache, elaboration tree-walks DAG-shaped lowered terms: a string literal's UTF-8 derivation shares every scan-state chain by `Rc`, so re-elaborating the chain at each link cost O(N²) work and the chain's depth in native stack; with it, each shared node elaborates once, at O(1) additional depth.
    ///
    /// Probe, compute under the stamp snapshot, record — the one way in, used by every `elaborate_subterm` dispatch. The halves are split out as [`Context::probe_elaborated`] and [`Context::record_elaborated`] because this method is exactly the two of them with the `compute` call spliced in; nothing else calls them since the iterative driver that suspended between them was retired.
    pub(crate) fn get_or_init_elaborated<E>(
        &mut self,
        term: &Term,
        expected: Option<&Term>,
        compute: impl FnOnce(&mut Self) -> Result<(Term, Term), E>,
    ) -> Result<(Term, Term), E> {
        match self.probe_elaborated(term, expected) {
            ElabProbe::Hit(hit) => Ok(hit),
            ElabProbe::Uncacheable => compute(self),
            ElabProbe::Miss(stamp) => {
                let result = compute(self)?;
                self.record_elaborated(term, expected, stamp, &result);
                Ok(result)
            }
        }
    }

    /// Read half of the elaboration cache (see [`Context::get_or_init_elaborated`] for the full contract). Applies the O(1) groundness gate, then either answers from the cache (`Hit`), reports the term ineligible (`Uncacheable`), or snapshots `mutation_stamp` for the caller to thread back into [`record_elaborated`] (`Miss`). Pure: it never mutates the context, so a driver may probe speculatively at a frame push.
    pub(crate) fn probe_elaborated(&self, term: &Term, expected: Option<&Term>) -> ElabProbe {
        let ground = |t: &Term| {
            !t.has_metavar() && !self.has_unsolved_universe_meta(t) && !t.has_local_free()
        };
        if !ground(term) || !expected.is_none_or(ground) {
            return ElabProbe::Uncacheable;
        }

        // Locally-nameless discipline: every scope is opened before descent, so a term in elaboration position carries no loose bound indices — which is what makes it keyable without any binder context.
        debug_assert!(term.closed(), "elaboration-cache key has loose indices");

        match self
            .caches
            .elaboration_get(term, expected, self.island.is_some())
        {
            Some(hit) => ElabProbe::Hit(hit),
            None => ElabProbe::Miss(self.caches.stamps()),
        }
    }

    /// Write half of the elaboration cache, paired with a [`probe_elaborated`] `Miss`. Keys the same way the probe did (spans excluded from `Term` equality, so the un-restamped result the caller passes keys identically) and defers to [`insert_elaborated`]'s purity/groundness condition against the snapshotted `stamp`.
    ///
    /// [`probe_elaborated`]: Context::probe_elaborated [`insert_elaborated`]: Context::insert_elaborated
    pub(crate) fn record_elaborated(
        &mut self,
        term: &Term,
        expected: Option<&Term>,
        stamp: ElaborationStamp,
        result: &(Term, Term),
    ) {
        self.insert_elaborated(term, expected, &stamp, result);
    }

    /// Insert-side tail of [`Context::get_or_init_elaborated`], kept out of the caller's frame deliberately: `elaborate` recurses natively once per term level with `get_or_init_elaborated` on the stack, so the insert path's locals must not ride along on every level.
    #[inline(never)]
    fn insert_elaborated(
        &mut self,
        term: &Term,
        expected: Option<&Term>,
        stamp: &ElaborationStamp,
        result: &(Term, Term),
    ) {
        if self.elaboration_cacheable(stamp, expected, result) {
            self.caches
                .elaboration_insert(term, expected, self.island.is_some(), result);
        }
    }

    /// Whether a [`probe_elaborated`] `Miss` may be recorded: the purity and groundness condition, plus the *settled-globals* gate. Every global the entry names — in the result, and in the `Check` `expected` half of the key — must already be defined. Definedness is the one ambient fact a pure, ground elaboration reads (through the conversions in `expect`, which unfold definitions), so an entry that surfaces only settled globals cannot be invalidated by a later *fresh* `define` — the name analogue of the reduction cache's unsolved-metavariable refusal (`Context::reduce`), and what lets [`define_entry`] drop its wholesale elaboration-cache clear. A constructor, intrinsic, inductive, or struct is not a free `Var`, so it never trips the gate; only a `/`-qualified definition or a `rec` member does, and a `rec` member is defined (as a slot) before any sibling body elaborates, so it counts as settled here — the slot→member redefinition later clears wholesale.
    ///
    /// [`probe_elaborated`]: Context::probe_elaborated [`define_entry`]: Context::define_entry
    fn elaboration_cacheable(
        &self,
        stamp: &ElaborationStamp,
        expected: Option<&Term>,
        result: &(Term, Term),
    ) -> bool {
        let ground = |t: &Term| {
            !t.has_metavar() && !self.has_unsolved_universe_meta(t) && !t.has_local_free()
        };
        let settled = |t: &Term| t.free_vars().iter().all(|name| self.is_defined(name));
        self.caches.stamps_unchanged(stamp)
            && ground(&result.0)
            && ground(&result.1)
            && settled(&result.0)
            && settled(&result.1)
            && expected.is_none_or(settled)
    }

    fn has_unsolved_universe_meta(&self, term: &Term) -> bool {
        term.any_universe_meta(|meta| match self.universe_solver.zonk(&Level::meta(meta)) {
            Ok(level) => level.metas().next().is_some(),
            Err(_) => true,
        })
    }

    fn enter_frame(&mut self) {
        self.frames.enter();
    }

    fn leave_frame(&mut self) {
        let (dropped_refinements, dropped_definitions) = self.frames.leave();
        self.caches
            .invalidate_frame_exit(dropped_refinements, dropped_definitions);
    }

    pub(crate) fn with_frame<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.enter_frame();
        let result = f(self);
        self.leave_frame();

        result
    }

    /// [`Frames::assume`], stamping the write.
    pub(crate) fn assume(&mut self, name: &Free, type_: &Term) {
        self.caches.note_write();
        self.frames.assume(name, type_);
    }

    /// Assume `label : type_` as a `use`-plicity binder: an ordinary assumption that additionally joins the witness scope, where resolution finds it (innermost-first).
    pub(crate) fn assume_witness(&mut self, name: &Free, type_: &Term) {
        self.assume(name, type_);
        self.frames.push_witness_binder(name, type_);
    }

    pub(crate) fn witness_scope(&self) -> &[(Free, Term)] {
        self.frames.witness_scope()
    }

    /// [`Frames::reassume`], invalidating the elaboration cache — an entry elaborated between a `rec` group's lowered `assume` and this upgrade could embed the lowered signature.
    pub(crate) fn reassume(&mut self, name: &Free, type_: &Term) {
        self.caches.invalidate_for_reassumption();
        self.frames.reassume(name, type_);
    }

    pub(crate) fn assumption(&self, name: &Free) -> Option<&Term> {
        self.frames.assumption(name)
    }

    /// Collect universe metas reachable through a term and through any solved term metavariables it names. Declaration finalization runs before the final term-zonk pass, so a level occurring only in a solved hole must still join the declaration's universe closure. Recursive slots may point back to themselves; `seen` keeps this analysis finite.
    pub(crate) fn universe_metas_in(&self, term: &Term) -> BTreeSet<UniverseMetaId> {
        let mut universes = BTreeSet::new();
        let mut seen_metas = BTreeSet::new();
        let mut pending = vec![term.clone()];
        while let Some(term) = pending.pop() {
            let mut term_metas = BTreeSet::new();
            term.collect_universe_dependencies(&mut universes, &mut term_metas);
            for meta in term_metas {
                if !seen_metas.insert(meta) {
                    continue;
                }
                if let Some(entry) = self.metavar_entry(meta) {
                    match &entry.solution {
                        Some(solution) => pending.push(solution.clone()),
                        None => {
                            // An unsolved meta may survive into parked work, so keep every universe dependency needed to solve it later. A solved meta materializes only its solution through the occurrence spine; its birth result/telescope do not survive zonking.
                            pending.push(entry.result.clone());
                            pending.extend(entry.telescope.iter().map(|(_, type_)| type_.clone()));
                        }
                    }
                }
            }
        }
        universes
    }

    pub(crate) fn instantiate_assumption_universes(
        &mut self,
        name: &Free,
    ) -> Result<Option<(Term, Vec<Level>)>, UniverseError> {
        let Some(type_) = self.assumption(name).cloned() else {
            return Ok(None);
        };
        let universe_context = self
            .frames
            .assumption_universe_context(name)
            .unwrap_or_default();
        if universe_context.parameter_count == 0 {
            return Ok(Some((type_, Vec::new())));
        }
        let levels = self
            .universes_mut()
            .instantiate(&universe_context, UniverseRole::Generalizable)?;
        let type_ = instantiate_universe_levels_scoped(&type_, &levels)?;
        Ok(Some((type_, levels)))
    }

    pub(crate) fn instantiate_assumption(
        &mut self,
        name: &Free,
    ) -> Result<Option<(Term, Vec<Level>)>, Error> {
        self.instantiate_assumption_universes(name)
            .map_err(Error::from)
    }

    pub(crate) fn instantiate_assumption_at(
        &mut self,
        name: &Free,
        levels: &[Level],
    ) -> Result<Option<Term>, Error> {
        let Some(type_) = self.assumption(name).cloned() else {
            return Ok(None);
        };
        let found = self.frames.assumption_universe_context(name);
        #[cfg(feature = "profile")]
        if found
            .as_ref()
            .is_none_or(|context| context.parameter_count != levels.len())
        {
            let (frames, holders) = self.frames.assumption_universe_holders(name);
            curios_profile::tracing::debug!(
                target: "curios_elab::universe",
                %name,
                registered = found.is_some(),
                expected = found.as_ref().map_or(0, |context| context.parameter_count),
                got = levels.len(),
                frames,
                ?holders,
                "assumption instance arity mismatch",
            );
        }
        let universe_context = found.unwrap_or_default();
        self.universes_mut()
            .instantiate_at(&universe_context, levels)
            .map_err(Error::from)?;
        let type_ = instantiate_universe_levels_scoped(&type_, levels).map_err(Error::from)?;
        Ok(Some(type_))
    }

    pub(crate) fn instantiate_universe_bound<B: Bound>(
        &mut self,
        universe_context: &UniverseContext,
        value: &B,
    ) -> Result<(B, Vec<Level>), Error> {
        if universe_context.parameter_count == 0 {
            return Ok((value.clone(), Vec::new()));
        }
        let levels = self
            .universes_mut()
            .instantiate(universe_context, UniverseRole::Generalizable)
            .map_err(Error::from)?;
        let value = instantiate_universe_levels_scoped(value, &levels).map_err(Error::from)?;
        Ok((value, levels))
    }

    pub(crate) fn instantiate_universe_bound_at<B: Bound>(
        &mut self,
        universe_context: &UniverseContext,
        value: &B,
        levels: &[Level],
    ) -> Result<B, UniverseError> {
        #[cfg(feature = "profile")]
        if levels.len() != universe_context.parameter_count {
            curios_profile::tracing::debug!(
                target: "curios_elab::universe",
                expected = universe_context.parameter_count,
                got = levels.len(),
                "bound instance arity mismatch",
            );
        }
        self.universes_mut()
            .instantiate_at(universe_context, levels)?;
        instantiate_universe_levels_scoped(value, levels)
    }

    pub(crate) fn instantiate_induct_decl_at(
        &mut self,
        induct_decl: &InductDecl,
        levels: &[Level],
    ) -> Result<InductDecl, UniverseError> {
        fn rewrite<B: Bound>(value: &B, levels: &[Level]) -> Result<B, UniverseError> {
            instantiate_universe_levels_scoped(value, levels)
        }

        #[cfg(feature = "profile")]
        if levels.len() != induct_decl.universe_context.parameter_count {
            curios_profile::tracing::debug!(
                target: "curios_elab::universe",
                module = ?induct_decl.module,
                expected = induct_decl.universe_context.parameter_count,
                got = levels.len(),
                "induct instance arity mismatch",
            );
        }
        self.universes_mut()
            .instantiate_at(&induct_decl.universe_context, levels)?;
        let mut instantiated = induct_decl.clone();
        instantiated.arity = rewrite(&instantiated.arity, levels)?;
        instantiated.result_sort = rewrite(&instantiated.result_sort, levels)?;
        for constructor in instantiated.signatures_mut() {
            constructor.telescope = rewrite(&constructor.telescope, levels)?;
        }
        instantiated.universe_context = UniverseContext::empty();
        Ok(instantiated)
    }

    pub(crate) fn instantiate_struct_decl_at(
        &mut self,
        struct_decl: &StructDecl,
        levels: &[Level],
    ) -> Result<StructDecl, UniverseError> {
        fn rewrite<B: Bound>(value: &B, levels: &[Level]) -> Result<B, UniverseError> {
            instantiate_universe_levels_scoped(value, levels)
        }

        #[cfg(feature = "profile")]
        if levels.len() != struct_decl.universe_context.parameter_count {
            curios_profile::tracing::debug!(
                target: "curios_elab::universe",
                module = ?struct_decl.module,
                expected = struct_decl.universe_context.parameter_count,
                got = levels.len(),
                "struct instance arity mismatch",
            );
        }
        self.universes_mut()
            .instantiate_at(&struct_decl.universe_context, levels)?;
        let mut instantiated = struct_decl.clone();
        instantiated.arity = rewrite(&instantiated.arity, levels)?;
        instantiated.result_sort = rewrite(&instantiated.result_sort, levels)?;
        instantiated.universe_context = UniverseContext::empty();
        Ok(instantiated)
    }

    /// [`Frames::set_assumption_universe_context`], with the redefinition cache protocol — a scheme rewritten in place makes cached entries through the old scheme unsound.
    pub(crate) fn set_assumption_universe_context(
        &mut self,
        name: &Free,
        universe_context: UniverseContext,
    ) {
        self.frames
            .set_assumption_universe_context(name, universe_context);
        self.caches.invalidate_for_redefinition();
    }

    fn is_defined(&self, name: &Free) -> bool {
        self.frames.is_defined(name)
    }

    pub(crate) fn locals(&self) -> &[(Free, Term)] {
        self.frames.locals()
    }

    fn define_entry(&mut self, name: Free, entry: DefEntry) {
        // A *fresh* definition can only unstick reductions that read this name's absence, and a stuck read always leaves the name free in the WHNF (the name analogue of the unsolved-metavariable argument in `Context::reduce`) — so the reduction cache retains every entry whose result does not mention it instead of clearing wholesale. This keeps closed reducts warm across item boundaries: erasure re-derives an item right after its `define`, and a cold re-reduction of a deep closed spine (a string literal's scan-state chain) would repeat all of its work.
        //
        // The elaboration cache survives the same fresh definition with no retain at all: its insert gate (`elaboration_cacheable`) already refused every entry naming a not-yet-defined global, so the fresh name appears in no surviving entry. A minted `let` binder — which `reduce_let` leaks and the frame elaborators mint — is excluded from caching outright (`has_local_free`), and a global is only ever referenced once defined; so not clearing lets a deep spine memoize once across those definitions instead of re-elaborating its shared subterms after each.
        //
        // A *redefinition* voids both arguments — `reduce_let` and the frame elaborators define under labels that can rebind or shadow, and the old value may sit consumed inside a reduct or an elaboration result that no longer mentions the label — so there both caches clear wholesale.
        if self.frames.is_defined(&name) {
            self.caches.invalidate_for_redefinition();
        } else {
            self.caches.retain_reductions_without(&name);
        }

        self.frames.define(name, entry);
    }

    /// Define `name`. `kind` is the declaring module item's [`DefinitionKind`], or `None` for a local binding no item declared.
    pub(crate) fn define(&mut self, name: &Free, term: &Term, kind: Option<&DefinitionKind>) {
        self.define_entry(name.clone(), DefEntry::new(term.clone(), kind.cloned()));
    }

    pub(crate) fn define_assuming(
        &mut self,
        name: &Free,
        type_: &Term,
        term: &Term,
        kind: Option<&DefinitionKind>,
    ) {
        self.assume(name, type_);
        self.define(name, term, kind);
    }

    pub(crate) fn define_assuming_scheme(
        &mut self,
        name: &Free,
        type_: &Term,
        term: &Term,
        kind: Option<&DefinitionKind>,
        universe_context: UniverseContext,
    ) {
        self.define_assuming(name, type_, term, kind);
        self.set_assumption_universe_context(name, universe_context);
    }

    pub(crate) fn definition_kind(&self, name: &Free) -> Option<&DefinitionKind> {
        self.frames.definition_kind(name)
    }

    // === Refinements (see [`Frames`]) =======================================

    /// [`Frames::refine`], with the refinement cache protocol — the variable now reduces differently.
    pub(crate) fn refine(&mut self, name: &Free, term: &Term) {
        self.caches.invalidate_for_refinement();
        self.frames.refine(name, term);
    }

    /// [`Frames::refine_projection`], with the refinement cache protocol.
    pub(crate) fn refine_projection(&mut self, base: Term, index: usize, value: Term) {
        self.caches.invalidate_for_refinement();
        self.frames.refine_projection(base, index, value);
    }

    pub(crate) fn definition_body(&self, name: &Free) -> Option<&Term> {
        self.frames.definition_body(name)
    }

    pub(crate) fn var_reduct(&self, name: &Free) -> Option<&Term> {
        self.frames.var_reduct(name)
    }

    pub(crate) fn var_reduct_at(&self, name: &Free) -> Option<&Term> {
        self.frames.var_reduct_at(name)
    }

    pub(crate) fn proj_reduct(&self, base: &Term, index: usize) -> Option<&Term> {
        self.frames.proj_reduct(base, index)
    }

    /// [`Frames::refine_scrutinee`], with the refinement cache protocol.
    pub(crate) fn refine_scrutinee(&mut self, canonical: Term, original: Term, value: Term) {
        self.caches.invalidate_for_refinement();
        self.frames.refine_scrutinee(canonical, original, value);
    }

    pub(crate) fn has_scrutinee_refinements(&self) -> bool {
        self.frames.has_scrutinee_refinements()
    }

    pub(crate) fn scrutinee_head_refined(&self, head: HeadTag<'_>) -> bool {
        self.frames.scrutinee_head_refined(head)
    }

    pub(crate) fn scrutinee_reduct(&self, canonical: &Term) -> Option<&Term> {
        self.frames.scrutinee_reduct(canonical)
    }

    pub(crate) fn scrutinee_entries(&self, head: HeadTag<'_>) -> Vec<(Term, ScrutineeEntry)> {
        self.frames.scrutinee_entries(head)
    }

    pub(crate) fn is_scrutinee_key(&self, canonical: &Term) -> bool {
        self.frames.is_scrutinee_key(canonical)
    }

    pub(crate) fn refinements_suppressed(&self) -> bool {
        self.frames.refinements_suppressed()
    }

    pub(crate) fn has_refinements(&self) -> bool {
        self.frames.has_refinements()
    }

    /// Whether any refinement of any kind is registered, suppressed or not — the closed machine's gate. Suppression must not open it: a suppressed scrutinee key is *withheld* by the strategy, and the machine evaluating it would hand out the value the suppression exists to withhold.
    pub(crate) fn any_refinements_registered(&self) -> bool {
        self.frames.any_refinements_registered()
    }

    /// Run `f` with refinements suppressed (re-validation). Brackets the region with reduction-cache clears so refinement-applied and refinement-suppressed reducts never contaminate each other's cache — but only when some refinement is actually registered. With none, suppressing changes no reduct, so the flag is inert and the clears are pure waste (the common re-validation path: an oracle run outside any match arm). Each boundary is gated on the live state independently, so a refinement added and dropped *inside* `f` — which clears on its own add and exit — does not force a clear here.
    pub(crate) fn with_suppressed_refinements<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        if self.frames.any_refinements_registered() {
            self.caches.invalidate_suppression_boundary();
        }

        let previous = self.frames.set_refinements_suppressed(true);
        let result = f(self);
        self.frames.set_refinements_suppressed(previous);

        if self.frames.any_refinements_registered() {
            self.caches.invalidate_suppression_boundary();
        }

        result
    }

    // === Registries (see [`Program`]) =======================================

    pub(crate) fn register_induct(
        &mut self,
        name: &Global,
        induct_decl: InductDecl,
    ) -> Result<(), Error> {
        self.program.register_induct(name, induct_decl)
    }

    pub(crate) fn update_induct(&mut self, name: &Global, induct_decl: InductDecl) {
        self.program.update_induct(name, induct_decl);
    }

    pub(crate) fn induct_decl(&self, name: &Global) -> Option<&InductDecl> {
        self.program.induct_decl(name)
    }

    pub(crate) fn register_struct(
        &mut self,
        name: &Global,
        struct_decl: StructDecl,
    ) -> Result<(), Error> {
        self.program.register_struct(name, struct_decl)
    }

    pub(crate) fn update_struct(&mut self, name: &Global, struct_decl: StructDecl) {
        self.program.update_struct(name, struct_decl);
    }

    pub(crate) fn struct_decl(&self, name: &Global) -> Option<&StructDecl> {
        self.program.struct_decl(name)
    }

    pub(crate) fn register_concept(
        &mut self,
        name: &Global,
        concept: ConceptDecl,
    ) -> Result<(), Error> {
        self.program.register_concept(name, concept)
    }

    pub(crate) fn concept(&self, name: &Global) -> Option<&ConceptDecl> {
        self.program.concept(name)
    }

    pub(crate) fn update_concept(&mut self, name: &Global, concept: ConceptDecl) {
        self.program.update_concept(name, concept);
    }

    pub(crate) fn concepts(&self) -> &BTreeMap<Global, ConceptDecl> {
        self.program.concepts()
    }

    /// Record the prefixes one seeded module's unit claims. See [`Program::mount`].
    pub(crate) fn mount(&mut self, mounts: &[Mount]) {
        self.program.mount(mounts);
    }

    pub(crate) fn mount_of(&self, name: &Global) -> Option<&Mount> {
        self.program.mount_of(name)
    }

    pub(crate) fn mount_of_head(&self, head: &HeadKey) -> Option<&Mount> {
        self.program.mount_of_head(head)
    }

    pub(crate) fn mark_witness_declaration(&mut self, name: &Global) {
        self.program.mark_witness_declaration(name);
    }

    pub(crate) fn is_witness_declaration(&self, name: &Global) -> bool {
        self.program.is_witness_declaration(name)
    }

    pub(crate) fn witness_entries(&self) -> impl Iterator<Item = (&Global, &Witness)> {
        self.program.witness_entries()
    }

    pub(crate) fn witness_keyed_entries(
        &self,
    ) -> impl Iterator<Item = (&Global, &WitnessKey, &Witness)> {
        self.program.witness_keyed_entries()
    }

    pub(crate) fn witness(&self, concept: &Global, key: &WitnessKey) -> Option<&Witness> {
        self.program.witness(concept, key)
    }

    /// [`Program::insert_witness`], stamping the write on an actual insert — a new witness can change which pure elaborations succeed.
    pub(crate) fn insert_witness(
        &mut self,
        concept: Global,
        key: WitnessKey,
        witness: Witness,
    ) -> Option<Qualifier> {
        let existing = self.program.insert_witness(concept, key, witness);
        if existing.is_none() {
            self.caches.note_write();
        }
        existing
    }

    /// [`Program::update_witness_scheme`], stamping the write.
    pub(crate) fn update_witness_scheme(
        &mut self,
        name: &Global,
        universe_context: UniverseContext,
        signature: Term,
    ) {
        self.program
            .update_witness_scheme(name, universe_context, signature);
        self.caches.note_write();
    }

    /// Defer a witness goal ([`Solutions::defer_witness`]), stamping the write.
    pub(crate) fn defer_witness(&mut self, goal: ParkedGoal) {
        self.caches.note_write();
        self.solutions.defer_witness(goal);
    }

    pub(crate) fn take_deferred_witnesses(&mut self) -> Vec<ParkedGoal> {
        self.solutions.take_deferred_witnesses()
    }

    /// The module whose item is currently being elaborated (the qualifier prefix of its name; empty for the root), or `None` when no surface item is being elaborated — which suppresses the representation-privacy checks (see the field's invariant).
    pub(crate) fn island(&self) -> Option<&Qualifier> {
        self.island.as_ref()
    }

    /// Set the current module before elaborating an item (see `elaborate_module_suffix`).
    pub(crate) fn set_island(&mut self, island: Qualifier) {
        // Every item boundary also lands a `define_entry` clear, but this one keeps the cache's soundness independent of that ordering.
        self.caches.invalidate_for_island_change();
        self.island = Some(island);
    }

    /// Run `f` with no island — suppressing the representation-privacy checks for re-derivation of already-elaborated terms, whose machinery-built projections were never subject to surface privacy in the first place. The bracket is the only way to clear an island (mirroring the parking half of the oracle package), so no context can be left permanently altered.
    pub(crate) fn with_suppressed_privacy<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let previous = self.island.take();
        let result = f(self);
        self.island = previous;

        result
    }

    // === Metavariable store =================================================

    /// Materialize a metavariable's birth record ([`Solutions::birth`]), stamping the write.
    pub(crate) fn birth_metavar(
        &mut self,
        id: MetaId,
        telescope: impl Into<SharedTelescope>,
        result: Term,
    ) {
        self.caches.note_write();
        self.solutions.birth(id, telescope.into(), result);
    }

    /// Allocate the protected placeholder for one member of a recursive group. It has the same contextual spine as an inference metavariable so parked work can carry it across a popped local frame, but only `fill_rec_slot` may solve it.
    pub(crate) fn fresh_rec_slot(&mut self, result: Term) -> (MetaId, Term) {
        self.caches.note_write();
        let id = self.solutions.mint();
        let (telescope, spine) = self.identity_snapshot();
        self.solutions.birth_rec_slot(id, telescope, result);
        (id, Term::metavar_birthed(id, None, spine))
    }

    pub(crate) fn is_rec_slot(&self, id: MetaId) -> bool {
        self.solutions.is_rec_slot(id)
    }

    pub(crate) fn fill_rec_slot(&mut self, id: MetaId, term: Term) {
        let entry = self
            .solutions
            .entry(id)
            .expect("recursive slot has a birth entry");
        assert_eq!(entry.kind, MetaKind::RecSlot, "filled a non-rec slot");
        assert!(entry.solution.is_none(), "recursive slot filled twice");
        self.solve_metavar(id, term);
    }

    pub(crate) fn is_top_level(&self, name: &Free) -> bool {
        self.frames.is_top_level(name)
    }

    pub(crate) fn identity_snapshot(&mut self) -> (SharedTelescope, SharedSpine) {
        self.frames.identity_snapshot()
    }

    /// Raise the minting floor: every id `fresh_metavar` hands out will be `>= floor`. Called by `elaborate_module_suffix` with its `metavar_floor` argument (the count `into_core` minted) before any item is elaborated.
    pub(crate) fn seed_metavars(&mut self, floor: usize) {
        self.solutions.seed_floor(floor);
    }

    /// Mint a metavariable for an omitted implicit argument and birth it immediately — frozen local Γ, the binder's instantiated type as `result` — so the id always has a birth record. Returns the metavariable term carrying the *call site's* span and the insertion provenance (which rides on the node; see [`Metavar::origin`]).
    pub(crate) fn fresh_metavar(
        &mut self,
        result: Term,
        span: Option<Span>,
        origin: ImplicitOrigin,
    ) -> Term {
        self.fresh_metavar_with(result, span, Some(MetavarOrigin::Implicit(origin)))
            .1
    }

    /// Mint a metavariable for an omitted `use` argument — like [`Context::fresh_metavar`] but carrying witness provenance, and returning the id so the caller can register the resolution goal.
    pub(crate) fn fresh_witness_metavar(
        &mut self,
        result: Term,
        span: Option<Span>,
        origin: WitnessOrigin,
    ) -> (MetaId, Term) {
        self.fresh_metavar_with(result, span, Some(MetavarOrigin::Witness(origin)))
    }

    /// Mint an unmarked (silently spliced) metavariable — the stand-in type a written goal in synthesis position gets, so the goal survives to zonk's report instead of dying with `CannotInfer` (`elaborate_metavar`).
    pub(crate) fn fresh_unmarked_metavar(&mut self, result: Term, span: Option<Span>) -> Term {
        self.fresh_metavar_with(result, span, None).1
    }

    fn fresh_metavar_with(
        &mut self,
        result: Term,
        span: Option<Span>,
        origin: Option<MetavarOrigin>,
    ) -> (MetaId, Term) {
        let id = self.solutions.mint();
        let (telescope, spine) = self.identity_snapshot();
        self.birth_metavar(id, telescope, result);
        let metavar = Term::metavar_birthed(id, origin, spine);

        let metavar = match span {
            Some(span) => metavar.with_span(span),
            None => metavar,
        };

        (id, metavar)
    }

    pub(crate) fn metavar_entry(&self, id: MetaId) -> Option<&MetaEntry> {
        self.solutions.entry(id)
    }

    pub(crate) fn witness_hole(&self, term: &Term) -> Option<(WitnessOrigin, Term)> {
        self.solutions.witness_hole(term)
    }

    pub(crate) fn metavar_solution(&self, id: MetaId) -> Option<&Term> {
        self.solutions.solution(id)
    }

    pub(crate) fn resolve_metavar(&self, metavar: &Metavar) -> Option<Term> {
        self.solutions.resolve(metavar)
    }

    /// Commit a metavariable's solution ([`Solutions::solve`]), stamping the write. Needs no reduction-cache clear: a WHNF that still named an unsolved metavariable was never memoized (see `Context::reduce`), and a solve is monotonic, so every surviving entry stays valid. (Re-validation's [`Context::rollback_solutions`], which *un*-solves, does clear.)
    pub(crate) fn solve_metavar(&mut self, id: MetaId, term: Term) {
        self.caches.note_write();
        self.solutions.solve(id, term);
    }

    /// Close a [`Context::solution_mark`] transaction, keeping whatever it left in place. Undoing is [`Context::rollback_solutions`]; a path that undoes still ends here.
    pub(crate) fn end_solutions(&mut self, mark: SolutionMark) {
        self.universe_solver.release(mark.universe);
    }

    /// Watermark for [`Context::rollback_solutions`]: how many solutions have been committed so far. Spans both unification stores, which is why it lives here rather than on either.
    ///
    /// Opens a speculative scope on the universe solver, which [`Context::end_solutions`] closes. The two are paired by hand at every site: a closure bracket in the manner of [`Context::with_frame`] would be safer, but these sit on elaboration's recursions, where its body is a stack frame per level.
    pub(crate) fn solution_mark(&mut self) -> SolutionMark {
        SolutionMark {
            term_solution_log_len: self.solutions.solved_len(),
            universe: self.universe_solver.mark(),
        }
    }

    /// Unwind every solution committed since `mark` — the transactional bracket around re-validation. Validating a candidate runs full elaboration, which can solve *other* metavariables along the way; if the candidate is ultimately rejected, those nested solutions were derived from an equation that never held and must not survive the verdict. Removes the unwound ids from the wake signals and clears the reduction cache, which may have cached reducts through them.
    pub(crate) fn rollback_solutions(&mut self, mark: SolutionMark) {
        self.solutions.unwind_to(mark.term_solution_log_len);
        self.universe_solver.rollback(mark.universe);
        self.caches.invalidate_for_rollback();
    }

    pub(crate) fn universes(&self) -> &UniverseSolver {
        &self.universe_solver
    }

    /// Mutably borrow the universe solver and advance the authoritative [`Entropy`] stamp on guard drop only if solver state actually changed. Normalized ground/reflexive comparisons are read-equivalent and must not make an otherwise pure elaboration-cache computation look impure. Rollback performs the conservative cache clear separately.
    pub(crate) fn universes_mut(&mut self) -> UniverseMutation<'_> {
        let before = self.universe_solver.state_token();
        UniverseMutation {
            solver: &mut self.universe_solver,
            stamp: self.caches.universe_stamp(),
            before,
        }
    }

    pub(crate) fn finish_universe_transaction(&mut self) {
        let before = self.universe_solver.state_token();
        self.universes_mut().clear_constraints();
        if self.universe_solver.state_token() != before {
            self.caches.invalidate_for_universe_transaction();
        }
    }

    pub(crate) fn fresh_universe(
        &mut self,
        role: UniverseRole,
        origin: Option<UniverseConstraintOrigin>,
    ) -> Level {
        let meta = self.universes_mut().fresh(role, origin);
        Level::meta(meta)
    }

    pub(crate) fn fresh_classifier_type(&mut self, kind: &str) -> Term {
        let level = self.fresh_universe(
            UniverseRole::Flexible,
            Some(UniverseConstraintOrigin::new(
                UniverseConstraintKind::Other(kind.into()),
            )),
        );
        Term::type_at(level)
    }

    pub(crate) fn seed_universes(&mut self, seeds: &[UniverseSeed], floor: usize) {
        assert_eq!(
            seeds.len(),
            floor,
            "the universe floor must equal the lowering seed table length"
        );
        self.universe_solver.seed(seeds);
        self.caches.note_universe_write();
    }

    pub(crate) fn default_universes(&mut self, terms: &[&Term]) -> Result<Vec<Term>, Error> {
        let metas = terms
            .iter()
            .flat_map(|term| self.universe_metas_in(term))
            .collect::<BTreeSet<_>>();
        self.universes_mut().default(metas).map_err(Error::from)?;
        let solver = self.universe_solver.clone();
        let terms = terms
            .iter()
            .map(|term| super::zonk_universe_levels_scoped(*term, &solver).map_err(Error::from))
            .collect::<Result<Vec<_>, _>>()?;
        self.caches.invalidate_for_universe_rewrite();
        Ok(terms)
    }

    pub(crate) fn finalize_universe_metas(
        &mut self,
        interface: BTreeSet<UniverseMetaId>,
        internal: BTreeSet<UniverseMetaId>,
    ) -> Result<UniverseContext, Error> {
        curios_profile::profile!("ctx::finalize_universe_metas");
        let universe_context = self
            .universes_mut()
            .finalize(interface, internal)
            .map_err(Error::from)?;
        self.caches.invalidate_for_universe_rewrite();
        Ok(universe_context)
    }

    pub(crate) fn finalize_universe_metas_at_instance(
        &mut self,
        metas: BTreeSet<UniverseMetaId>,
        instance: &[Level],
        parameter_count: usize,
    ) -> Result<(), Error> {
        self.universes_mut()
            .finalize_at_instance(metas, instance, parameter_count)
            .map_err(Error::from)?;
        self.caches.invalidate_for_universe_rewrite();
        Ok(())
    }

    pub(crate) fn close_universe_instance(
        &mut self,
        minted: &[Level],
        instance: &[Level],
        determined: &[Level],
    ) -> Result<(), Error> {
        self.universes_mut()
            .close_instance(minted, instance, determined)
            .map_err(Error::from)?;
        self.caches.invalidate_for_universe_rewrite();
        Ok(())
    }

    pub(crate) fn zonk_universe_levels<B: Bound>(&self, value: &B) -> Result<B, Error> {
        super::zonk_universe_levels_scoped(value, &self.universe_solver).map_err(Error::from)
    }

    // === Parked constraints ============================================

    pub(crate) fn freeze_frame(&self) -> FrozenFrame {
        self.frames.freeze()
    }

    /// Reapply a frozen frame inside a fresh `with_frame`, restoring the equalities the parked problem's origin saw.
    ///
    /// Only what is not already live is reapplied: an intra-item retry runs while the origin's outer binders are still in scope, and re-assuming a live identity would double it in Γ — a metavariable born under the doubled telescope carries a non-linear identity spine (`?m[V, m, V, m, …]`) that pattern inversion can never invert, leaving its goals parked forever. Identities are unique mints, so a name already assumed *is* the frozen binder and skipping it loses nothing.
    pub(crate) fn restore_frame(&mut self, frame: &FrozenFrame) {
        for (name, type_) in &frame.assumptions {
            if self.assumption(name).is_none() {
                self.assume(name, type_);
            }
        }

        for (name, entry) in &frame.definitions {
            self.define_entry(name.clone(), entry.clone());
        }

        for (name, value) in &frame.refinements {
            self.refine(name, value);
        }

        for ((base, index), value) in &frame.refinement_projections {
            self.refine_projection(base.clone(), *index, value.clone());
        }

        for (canonical, entry) in &frame.refinement_scrutinees {
            self.refine_scrutinee(
                canonical.clone(),
                entry.original.clone(),
                entry.value.clone(),
            );
        }

        // The witness binders were already re-assumed by the loop above (they are a subset of `assumptions`); only the scope membership is restored here. The enclosing frame's mark truncates it on exit.
        self.frames.extend_witness_scope(&frame.witness_binders);
    }

    /// Park blocked work: freeze the live local frame around it and record which unsolved metavariables could unblock it.
    pub(crate) fn park(&mut self, work: ParkedWork, origin: Term) {
        let frame = self.freeze_frame();
        self.repark(work, origin, frame);
    }

    /// Re-park work that is still blocked after a retry, keeping its originally frozen frame ([`Solutions::park`]), stamping the write.
    pub(crate) fn repark(&mut self, work: ParkedWork, origin: Term, frame: FrozenFrame) {
        self.caches.note_write();
        self.solutions.park(work, origin, frame);
    }

    /// Mint the placeholder metavariable for a parked checking problem: birthed like any hole — frozen Γ, identity spine — with no insertion provenance. If it survives unsolved, the item drain reports the parked problem at its origin before zonk could ever meet the placeholder.
    pub(crate) fn fresh_placeholder(&mut self, result: Term, span: Option<Span>) -> (MetaId, Term) {
        let id = self.solutions.mint();
        let (telescope, spine) = self.identity_snapshot();
        self.birth_metavar(id, telescope, result);
        let term = Term::metavar_birthed(id, None, spine);

        let term = match span {
            Some(span) => term.with_span(span),
            None => term,
        };

        (id, term)
    }

    pub(crate) fn wake_parked(&mut self) -> Vec<ParkedGoal> {
        self.solutions.wake_parked()
    }

    pub(crate) fn take_parked(&mut self) -> Vec<ParkedGoal> {
        self.solutions.take_parked()
    }

    pub(crate) fn parked_len(&self) -> usize {
        self.solutions.parked_len()
    }

    pub(crate) fn has_newly_solved(&self) -> bool {
        self.solutions.has_newly_solved()
    }

    pub(crate) fn parking_suppressed(&self) -> bool {
        self.solutions.parking_suppressed()
    }

    /// Run `f` as a yes/no *oracle* around full elaboration (re-validation): parking is suppressed — `expect` treats `Blocked` as a mismatch and `retry_parked` is a no-op, so provisional success can neither leak into the verdict nor consume a parked obligation whose error the oracle would swallow — counterfactual refinements are suppressed with it, and so are the representation-privacy checks: an oracle candidate is a unification artifact that can embed machinery-built projections (eta-expansions, witness splices) whose privacy elaboration already adjudicated, and a swallowed privacy error would silently flip the verdict. The suppressions are a package: an oracle that set only some would be subtly unsound, which is why the parking half has no public setter.
    pub(crate) fn with_oracle<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.with_suppressed_parking(|context| {
            context.with_suppressed_refinements(|context| context.with_suppressed_privacy(f))
        })
    }

    fn with_suppressed_parking<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let previous = self.solutions.set_parking_suppressed(true);
        let result = f(self);
        self.solutions.set_parking_suppressed(previous);

        result
    }
}
