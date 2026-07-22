use {
    super::{
        Bound, Concept, Error, Goal, HeadKey, ImplicitOrigin, Inductive, Metavar, MetavarId,
        MetavarOrigin, Structure, Subterm, Term, Witness, WitnessKey, WitnessOrigin,
    },
    crate::Instant,
    curios_abi::RootId,
    curios_base::{Entropy, Qualifier, Span},
    std::{
        collections::{BTreeMap, BTreeSet, HashMap},
        mem,
        rc::Rc,
        time::Duration,
    },
};

/// Γ frozen in binding order, with birth-time types. `Rc`-shared: every meta
/// born under the same Γ shares one allocation (see
/// [`Context::identity_snapshot`]).
type SharedTelescope = Rc<Vec<(String, Term)>>;

/// The identity spine over a [`SharedTelescope`] — one `Var::free` per binder
/// — shared the same way.
type SharedSpine = Rc<Vec<Term>>;

/// One metavariable's record in the [`MetaStore`]. Everything here is frozen at
/// birth except `solution`, which transitions `None -> Some(_)` on solve —
/// rolled back to `None` if re-validation rejects the candidate that solved it
/// (`Context::rollback_solutions`, §7.4).
#[derive(Debug)]
pub(crate) struct MetaEntry {
    /// Γ frozen at birth: the local assumption context in binding order, with
    /// birth-time types. Drives the scope check and re-validation (§7.3–§7.4).
    pub telescope: SharedTelescope,
    /// The metavariable's type — the `expected` it was checked against at birth.
    pub result: Term,
    /// `None` while unsolved; `Some(t)` once solved. `t`'s free `Var`s are a
    /// subset of `telescope`'s names.
    pub solution: Option<Term>,
    /// Ordinary inference holes may be solved by conversion. Recursive
    /// elaboration slots are filled only by the owning `rec` elaborator;
    /// conversion treats an unfilled slot as a blocking dependency.
    pub kind: MetaKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MetaKind {
    Inference,
    RecSlot,
}

/// Flat, frame-independent store of metavariable records, indexed by
/// `Metavar::id`. Its contents are monotonic facts about the program being
/// elaborated, not lexically-scoped bindings — so `enter_frame`/`leave_frame`
/// never touch it.
#[derive(Debug, Default)]
pub(crate) struct MetaStore {
    entries: Vec<Option<MetaEntry>>,
}

/// One definition: just the definiens. Every `DefEntry` — whether a plain
/// `let`/`rec` member or mid-window rec-group registration — is treated
/// uniformly; there is no `recursive` marker distinguishing them.
#[derive(Debug, Clone)]
pub(crate) struct DefEntry {
    term: Term,
}

/// The local frame a parked problem froze at park time: assumptions (in
/// binding order), and the non-base-frame definitions, counterfactual
/// refinements, projection refinements, and scrutinee refinements (each outermost frame first, so
/// reapplying in order reproduces the shadowing). A retry must run under the
/// same equalities its origin saw — including the arm-local refinements —
/// while solution re-validation independently suppresses them, keeping
/// committed solutions refinement-free.
#[derive(Debug, Clone)]
pub(crate) struct FrozenFrame {
    assumptions: Vec<(String, Term)>,
    definitions: Vec<(String, DefEntry)>,
    refinements: Vec<(String, Term)>,
    refinement_projections: Vec<((Term, usize), Term)>,
    refinement_scrutinees: Vec<(Term, Term)>,
    /// The `use`-plicity binders in scope at park time (a subset of
    /// `assumptions`, in the same binding order). Witness resolution scans
    /// these; a retry must see the same instance scope its origin saw.
    witness_binders: Vec<(String, Term)>,
}

/// The work a parked problem will retry (§8).
#[derive(Debug)]
pub(crate) enum ParkedWork {
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
        placeholder: MetavarId,
    },
    /// A witness-resolution goal whose key type is not yet rigid: `slot` is
    /// the metavariable standing in the omitted `use`-argument's place, `goal`
    /// its (concept application) type. Woken when a watched metavariable
    /// solves; resolution then retries under the frozen frame.
    Witness {
        slot: MetavarId,
        goal: Term,
        provenance: WitnessOrigin,
    },
}

/// A problem parked by `expect` (or a blocked intro-form check) to outlive
/// its call (§8). Like a [`MetaEntry`], it freezes the local frame it was
/// born under.
#[derive(Debug)]
pub(crate) struct ParkedGoal {
    pub work: ParkedWork,
    /// The term being checked at park time; its span anchors the eventual
    /// error if the problem never resolves.
    pub origin: Term,
    pub frame: FrozenFrame,
    /// The unsolved metavariables whose solutions could unblock this —
    /// solving any of them is the wake signal.
    pub watching: BTreeSet<MetavarId>,
}

/// Key of one memoized `elaborate` call in `Context::get_or_init_elaborated`:
/// the lowered term, the `Check` expected type (`None` for `Infer`), and
/// whether an island's representation-privacy checks were live. Validity
/// under suppressed privacy is directional — an entry that passed strict
/// checks would be valid under suppression, but not the reverse — so checked
/// and suppressed runs each answer only their own partition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ElaborationKey {
    term: Term,
    expected: Option<Term>,
    privacy_checked: bool,
}

/// Outcome of [`Context::probe_elaborated`] — the read half of the elaboration
/// cache, torn out of [`Context::get_or_init_elaborated`]'s bracket so the
/// iterative `elaborate` driver can probe at a frame push and record at the
/// matching pop (the reduction cache's `cached_reduced`/`reduce` split, one
/// level up). `Hit` carries the memoized, un-span-stamped `(rebuilt, type)`;
/// `Miss` carries the `mutation_stamp` snapshot the caller threads back into
/// [`Context::record_elaborated`] as its purity witness; `Uncacheable` marks a
/// term the groundness gate excludes — the caller elaborates it but records
/// nothing.
pub(crate) enum ElabProbe {
    Hit((Term, Term)),
    Miss(usize),
    Uncacheable,
}

/// The kernel's ambient state, threaded mutably through elaboration, typing, reduction, conversion, and erasure. Two lifetimes coexist: *frame-scoped* lexical state (assumptions, local definitions, the counterfactual refinement stores, the witness scope), pushed and popped as binders and match arms are entered, and *flat monotonic facts* about the program (the `MetaStore`, inductive/struct/concept declarations, the witness table, parked and deferred goals), which frames never touch. The single deadline fixed at construction bounds *total* work across every call sharing the context — see [`Context::new`].
#[derive(Debug)]
pub struct Context {
    fresh_names: Entropy,
    deadline: Instant,
    reduction_cache: HashMap<Term, Term>,
    // Memoized `elaborate` results for ground, local-free subterms (see
    // `ElaborationKey`), holding the un-span-stamped (rebuilt, type) pair.
    // The elaboration-level analogue of `reduction_cache`: without it,
    // elaboration tree-walks the DAG-shaped terms the lowerer emits
    // (string-literal UTF-8 derivations share every state chain by `Rc`) in
    // quadratic time and linear extra stack. See
    // `Context::get_or_init_elaborated` for the exact gates and clear sites.
    elaboration_cache: HashMap<ElaborationKey, (Term, Term)>,
    assumptions: Vec<HashMap<String, Term>>,
    definitions: Vec<HashMap<String, DefEntry>>,
    // Counterfactual match-arm refinements (`refine_head`), kept parallel to
    // `definitions` but suppressible: re-validation of a metavariable
    // solution (§7.4) must keep stable definitions yet ignore these.
    refinements: Vec<HashMap<String, Term>>,
    refinement_projections: Vec<HashMap<(Term, usize), Term>>,
    // Counterfactual refinements keyed by a *stuck application* scrutinee — a
    // non-key match head (`classify(c)`, `Nat/in_range(...)`) that `refine_head`
    // could not record. Keyed by a *canonical* form (head verbatim, arguments
    // reduced to WHNF), so an occurrence that surfaces spelled differently
    // (`classify(Bin/at(cons(c,t),0,_))`, `Nat/in_range(c, lo', hi')`) still
    // matches the stored key once both are canonicalized. The term-keyed
    // analogue of the two stores above, suppressed by the same flag.
    refinement_scrutinees: Vec<HashMap<Term, Term>>,
    suppress_refinements: bool,
    // The local assumption context in binding order (a companion to
    // `assumptions`, which is keyed by name and loses order). `assume` appends;
    // frames are delimited by `local_marks`.
    local: Vec<(String, Term)>,
    local_marks: Vec<usize>,
    // Parked conversion constraints (§8) — frame-independent, like `metas`.
    parked: Vec<ParkedGoal>,
    // Ids solved since the last `wake_parked` sweep: the wake signal.
    newly_solved: Vec<MetavarId>,
    // Journal of every committed solution id, in commit order — never
    // consumed, only marked and rolled back. The watermark/rollback pair lets
    // re-validation (§7.4) unwind solutions that landed while validating a
    // candidate it then rejected.
    solved_log: Vec<MetavarId>,
    // While set, `expect` may not park: conversion is being used as a yes/no
    // oracle (re-validation) and provisional success would leak into it.
    suppress_parking: bool,
    // One tick per mutation of `local` (assume, frame exit, reassume) —
    // an `Entropy` used as a version stamp: `fresh()` bumps, `count()` reads.
    // Invalidates `identity_cache`, which shares the frozen telescope and
    // identity spine between every meta born under an unchanged Γ.
    locals_stamp: Entropy,
    identity_cache: Option<(usize, SharedTelescope, SharedSpine)>,
    // One tick per *write* to any kernel store — definitions, refinements,
    // assumptions, name/metavariable minting, solves, parked/deferred work,
    // the witness table. `get_or_init_elaborated` snapshots it around a
    // candidate sub-elaboration: an unchanged stamp certifies the run was
    // pure (replaying it would be the identity on the context), which is what
    // makes skipping the replay on a later cache hit sound.
    mutation_stamp: Entropy,
    metas: MetaStore,
    // The next metavariable id this context may mint (implicit-argument
    // insertion). Seeded by `elaborate_module` with its `metavar_floor`
    // argument so core-minted ids sit strictly above `into_core`'s.
    next_metavar: Entropy<MetavarId>,
    // Inductive declarations, keyed by the type's qualified name ("Result").
    // Like `metas`, a flat store of monotonic facts about the program, not
    // lexically-scoped bindings — `enter_frame`/`leave_frame` never touch it.
    inductives: BTreeMap<String, Inductive>,
    // Struct declarations, keyed the same way — a flat monotonic store like
    // `inductives`. Consulted by `elaborate_struct`/`elaborate_proj`/`erase`.
    structures: BTreeMap<String, Structure>,
    // Concept declarations, keyed by the concept's qualified name — a flat
    // monotonic store like `structures` (which also holds each concept's
    // record entry; this adds the resolution metadata).
    concepts: BTreeMap<String, Concept>,
    // The definition names `into_core` marked as witness declarations; each
    // registers into `witness_table` when its signature elaborates
    // (`elaborate_module_let` → `register_witness`).
    witness_declarations: BTreeSet<String>,
    // The program-wide witness table: one witness per (concept, parameter-head
    // tuple) key — global coherence, checked at registration.
    witness_table: BTreeMap<(String, WitnessKey), Witness>,
    // The `use`-plicity binders currently in scope, in binding order (a
    // subset of `local`), with frame boundaries in `witness_marks` —
    // resolution's step-1/2 search space, scanned innermost-first.
    witness_scope: Vec<(String, Term)>,
    witness_marks: Vec<usize>,
    // Witness goals whose key is rigid but has no table entry *yet*: a later
    // item may register the missing witness (the table is program-wide while
    // items elaborate in order), so these defer — retried after each item,
    // reported as errors only when the whole module has been elaborated.
    deferred_witnesses: Vec<ParkedGoal>,
    // The module whose item is currently being elaborated — the qualifier
    // prefix of that item's name (a fresh context starts at the root, the
    // empty qualifier). Set by `elaborate_module` per item; read by the
    // representation-privacy checks (§7). `None` arises only through
    // `with_suppressed_privacy` and means there is no surface use site to
    // judge from, which suppresses the checks structurally: privacy is a
    // property of *surface elaboration*, and machinery that re-derives types
    // from already-elaborated terms — erasure, the metavariable oracle —
    // walks compiler-built projections (witness splices, eta-expansions) that
    // must not be re-adjudicated. A machinery path that forgets its bracket
    // fails loudly (a spurious privacy error), never silently.
    island: Option<Qualifier>,
}

// Safety: `Term` keys contain `OnceCell` fields for caching, which triggers Clippy's
// interior mutability warning. However, the logical value is fully immutable, and the
// hash/equality check remains stable.
#[allow(clippy::mutable_key_type)]
impl Context {
    // The deadline is set once at construction and shared across every
    // `reduce`/`convert`/`infer`/`erase` call that uses this context, so the
    // timeout bounds total work, not per-call work.
    /// A fresh, empty context whose deadline is fixed at `now + timeout`. Declarations, definitions, and the metavariable floor arrive later, seeded by `elaborate_module` as it walks the lowered module.
    pub fn new(timeout: Duration) -> Self {
        Self {
            fresh_names: Entropy::<usize>::new(),
            deadline: Instant::now() + timeout,
            reduction_cache: HashMap::new(),
            elaboration_cache: HashMap::new(),
            assumptions: vec![HashMap::new()],
            definitions: vec![HashMap::new()],
            refinements: vec![HashMap::new()],
            refinement_projections: vec![HashMap::new()],
            refinement_scrutinees: vec![HashMap::new()],
            suppress_refinements: false,
            local: Vec::new(),
            local_marks: Vec::new(),
            metas: MetaStore::default(),
            next_metavar: Entropy::<MetavarId>::new(),
            inductives: BTreeMap::new(),
            structures: BTreeMap::new(),
            concepts: BTreeMap::new(),
            witness_declarations: BTreeSet::new(),
            witness_table: BTreeMap::new(),
            witness_scope: Vec::new(),
            witness_marks: Vec::new(),
            deferred_witnesses: Vec::new(),
            island: Some(Qualifier::empty()),
            parked: Vec::new(),
            newly_solved: Vec::new(),
            solved_log: Vec::new(),
            suppress_parking: false,
            locals_stamp: Entropy::new(),
            identity_cache: None,
            mutation_stamp: Entropy::new(),
        }
    }

    pub(crate) fn fresh(&mut self, hint: Option<&str>) -> String {
        self.mutation_stamp.fresh();
        let counter = self.fresh_names.fresh();

        match hint {
            Some(h) => format!("{h}#{counter}"),
            None => format!("#{counter}"),
        }
    }

    pub(crate) fn deadline(&self) -> Instant {
        self.deadline
    }

    /// The read half of the reduction cache. The reducer probes it wherever a
    /// term's reduction begins — at entry, and at the scrutinee stack's frame
    /// push, where a warm scrutinee dispatches in place instead of framing.
    pub(crate) fn cached_reduced(&self, term: &Term) -> Option<Term> {
        self.reduction_cache.get(term).cloned()
    }

    /// Record that `term` reduces to `result` — the write half of the
    /// reduction cache, hit wherever a reduction's value lands: the reducer's
    /// final return, and its scrutinee stack's frame pop.
    /// Memoize only closed terms whose WHNF names no *unsolved* metavariable
    /// — `any_metavar` bails on the first one, never building the id set. A
    /// solve is monotonic, so it can only invalidate a reduct that still
    /// names the metavariable it solved, and reduction gets stuck on (hence
    /// surfaces) an unsolved metavariable it actually depends on. Refusing to
    /// cache those is what lets `solve_metavar` skip a cache clear; an entry
    /// naming only *solved* metavariables stays valid under forward solves
    /// (re-validation's `rollback_solutions`, which *un*-solves, clears
    /// separately).
    pub(crate) fn reduce(&mut self, term: Term, result: &Term) {
        let cacheable =
            term.closed() && !result.any_metavar(&mut |id| self.metavar_solution(id).is_none());

        if cacheable {
            self.reduction_cache.insert(term, result.clone());
        }
    }

    /// The elaboration-level counterpart of the reduction cache
    /// ([`Context::cached_reduced`] / [`Context::reduce`]): memoize
    /// `(term, expected) → (rebuilt, type)` for subterms whose elaboration
    /// can neither read nor write anything context-dependent.
    /// Eligibility is O(1) per call (the bits are cached per `Term` node): the
    /// term — and the expected type, when checking — must contain no
    /// metavariable and no `#`-named free variable (an elaborator-minted local
    /// or witness name; `#` cannot occur in a written identifier, so every
    /// other free name is a top-level reference). Writes are detected by
    /// snapshotting `mutation_stamp` around the computation: an entry is
    /// inserted only when the run minted, solved, parked, defined, and refined
    /// nothing — a pure run whose replay would be the identity on the context.
    /// Errors are never cached. The one deliberate delta on a hit: the skipped
    /// run's `expect` no longer drains `retry_parked` at that exact point —
    /// safe deferral, since retries re-run at every later `expect` and the
    /// module drain reports whatever survives.
    ///
    /// A cached entry additionally names only *already-defined* globals: the
    /// insert refuses any result — or `Check` expected — naming a
    /// not-yet-defined global (`Context::elaboration_cacheable`), the name
    /// analogue of the unsolved-metavariable refusal above. Definedness is the
    /// one ambient fact a pure, ground elaboration reads (through `expect`'s
    /// conversions, which unfold definitions), so an entry that surfaces only
    /// settled globals cannot be invalidated by a later *fresh* `define`. That
    /// is what lets `define_entry` drop its wholesale elaboration-cache clear
    /// and keep the memo warm across the `#`-minted definitions that reduction
    /// and the frame elaborators mint within one item. (`set_island` still
    /// clears at each top-level item boundary, so the survival is within-item.)
    ///
    /// The suppression brackets need no insert refusal. Privacy: validity is
    /// directional (an entry that passed an island's strict checks is valid
    /// under suppression, but not the reverse), so `island.is_some()` is part
    /// of the key — the erasure path's privacy-suppressed re-derivations
    /// populate and hit their own partition, and `set_island` clears on every
    /// item change. Parking: `expect` can only be `Blocked` on unsolved
    /// metavariables, which the groundness gate excludes, so suppression is
    /// inert for every cacheable run. Refinements: the registrar,
    /// frame-exit, and suppression-boundary clears already remove every entry
    /// a live refinement could have influenced, on both sides of the flag.
    ///
    /// Without this cache, elaboration tree-walks DAG-shaped lowered terms: a
    /// string literal's UTF-8 derivation shares every scan-state chain by
    /// `Rc`, so re-elaborating the chain at each link cost O(N²) work and the
    /// chain's depth in native stack; with it, each shared node elaborates
    /// once, at O(1) additional depth.
    ///
    /// This is the recursive form — probe, compute under the stamp snapshot,
    /// record — used by every native `elaborate_subterm` dispatch. The
    /// iterative `elaborate` driver reaches the same cache through the split
    /// halves [`Context::probe_elaborated`] and [`Context::record_elaborated`]
    /// so it can suspend the computation between them; this method is those two
    /// halves with the `compute` call spliced in.
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

    /// Read half of the elaboration cache (see [`Context::get_or_init_elaborated`]
    /// for the full contract). Applies the O(1) groundness gate, then either
    /// answers from the cache (`Hit`), reports the term ineligible
    /// (`Uncacheable`), or snapshots `mutation_stamp` for the caller to thread
    /// back into [`record_elaborated`] (`Miss`). Pure: it never mutates the
    /// context, so a driver may probe speculatively at a frame push.
    pub(crate) fn probe_elaborated(&self, term: &Term, expected: Option<&Term>) -> ElabProbe {
        let ground = |t: &Term| !t.has_metavar() && !t.has_local_free();
        if !ground(term) || !expected.is_none_or(ground) {
            return ElabProbe::Uncacheable;
        }

        // Locally-nameless discipline: every scope is opened before descent,
        // so a term in elaboration position carries no loose bound indices —
        // which is what makes it keyable without any binder context.
        debug_assert!(term.closed(), "elaboration-cache key has loose indices");

        let key = ElaborationKey {
            term: term.clone(),
            expected: expected.cloned(),
            privacy_checked: self.island.is_some(),
        };
        match self.elaboration_cache.get(&key) {
            Some((rebuilt, type_)) => ElabProbe::Hit((rebuilt.clone(), type_.clone())),
            None => ElabProbe::Miss(self.mutation_stamp.count()),
        }
    }

    /// Write half of the elaboration cache, paired with a [`probe_elaborated`]
    /// `Miss`. Rebuilds the same key (spans excluded from `Term` equality, so
    /// the un-restamped result the caller passes keys identically to the probe)
    /// and defers to [`insert_elaborated`]'s purity/groundness condition against
    /// the snapshotted `stamp`.
    ///
    /// [`probe_elaborated`]: Context::probe_elaborated
    /// [`insert_elaborated`]: Context::insert_elaborated
    pub(crate) fn record_elaborated(
        &mut self,
        term: &Term,
        expected: Option<&Term>,
        stamp: usize,
        result: &(Term, Term),
    ) {
        let key = ElaborationKey {
            term: term.clone(),
            expected: expected.cloned(),
            privacy_checked: self.island.is_some(),
        };
        self.insert_elaborated(key, stamp, result);
    }

    /// Insert-side tail of [`Context::get_or_init_elaborated`], kept out of
    /// the caller's frame deliberately: `elaborate` recurses natively once per
    /// term level with `get_or_init_elaborated` on the stack, so the insert
    /// path's locals must not ride along on every level.
    #[inline(never)]
    fn insert_elaborated(&mut self, key: ElaborationKey, stamp: usize, result: &(Term, Term)) {
        if self.elaboration_cacheable(stamp, key.expected.as_ref(), result) {
            self.elaboration_cache.insert(key, result.clone());
        }
    }

    /// Whether a [`probe_elaborated`] `Miss` may be recorded: the purity and
    /// groundness condition, plus the *settled-globals* gate. Every global the
    /// entry names — in the result, and in the `Check` `expected` half of the
    /// key — must already be defined. Definedness is the one ambient fact a
    /// pure, ground elaboration reads (through the conversions in `expect`,
    /// which unfold definitions), so an entry that surfaces only settled
    /// globals cannot be invalidated by a later *fresh* `define` — the name
    /// analogue of the reduction cache's unsolved-metavariable refusal
    /// (`Context::reduce`), and what lets [`define_entry`] drop its wholesale
    /// elaboration-cache clear. A constructor, primitive, inductive, or struct
    /// is not a free `Var`, so it never trips the gate; only a `/`-qualified
    /// definition or a `rec` member does, and a `rec` member is defined (as a
    /// slot) before any sibling body elaborates, so it counts as settled here
    /// — the slot→member redefinition later clears wholesale.
    ///
    /// [`probe_elaborated`]: Context::probe_elaborated
    /// [`define_entry`]: Context::define_entry
    fn elaboration_cacheable(
        &self,
        stamp: usize,
        expected: Option<&Term>,
        result: &(Term, Term),
    ) -> bool {
        let ground = |t: &Term| !t.has_metavar() && !t.has_local_free();
        let settled = |t: &Term| t.free_vars().iter().all(|name| self.is_defined(name));
        self.mutation_stamp.count() == stamp
            && ground(&result.0)
            && ground(&result.1)
            && settled(&result.0)
            && settled(&result.1)
            && expected.is_none_or(settled)
    }

    fn enter_frame(&mut self) {
        self.assumptions.push(HashMap::new());
        self.definitions.push(HashMap::new());
        self.refinements.push(HashMap::new());
        self.refinement_projections.push(HashMap::new());
        self.refinement_scrutinees.push(HashMap::new());
        self.local_marks.push(self.local.len());
        self.witness_marks.push(self.witness_scope.len());
    }

    fn leave_frame(&mut self) {
        self.locals_stamp.fresh();
        self.assumptions.pop().unwrap();
        let definitions = self.definitions.pop().unwrap();
        let refinements = self.refinements.pop().unwrap();
        let refinement_projections = self.refinement_projections.pop().unwrap();
        let refinement_scrutinees = self.refinement_scrutinees.pop().unwrap();
        self.local.truncate(self.local_marks.pop().unwrap());
        self.witness_scope
            .truncate(self.witness_marks.pop().unwrap());

        if !refinements.is_empty()
            || !refinement_projections.is_empty()
            || !refinement_scrutinees.is_empty()
        {
            // A dropped refinement can have influenced any entry — refinement
            // keys can be `#`-free stuck applications of globals — so both
            // caches clear wholesale (refinement frames are rare).
            self.reduction_cache.clear();
            self.elaboration_cache.clear();
        } else if !definitions.is_empty() {
            // The reduction cache clears wholesale: `reduce_let` defines under
            // the written binder labels, so frame locals need not be `#`-minted,
            // and a frame value can be consumed into a reduct that no longer
            // names its definition (a chained definition folds it in), leaving
            // nothing for a selective retain to key on. The elaboration cache
            // is exempt: terms in elaboration position name only `/`-qualified
            // globals and `#`-minted locals, so no entry can reference a
            // written frame label.
            self.reduction_cache.clear();
        }
    }

    pub(crate) fn with_frame<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.enter_frame();
        let result = f(self);
        self.leave_frame();

        result
    }

    /// Assume `label : type_`. Erasure is sort-driven (a proof or a type erases),
    /// so a binder carries no runtime-multiplicity mark.
    pub(crate) fn assume<A>(&mut self, label: A, type_: &Term)
    where
        A: Into<String>,
    {
        let label = label.into();
        self.locals_stamp.fresh();
        self.mutation_stamp.fresh();
        self.local.push((label.clone(), type_.clone()));

        self.assumptions
            .last_mut()
            .unwrap()
            .insert(label, type_.clone());
    }

    /// Assume `label : type_` as a `use`-plicity binder: an ordinary
    /// assumption that additionally joins the witness scope, where resolution
    /// finds it (innermost-first).
    pub(crate) fn assume_witness<A>(&mut self, label: A, type_: &Term)
    where
        A: Into<String>,
    {
        let label = label.into();
        self.assume(label.as_str(), type_);
        self.witness_scope.push((label, type_.clone()));
    }

    /// The `use`-plicity binders in scope, in binding order (innermost last).
    pub(crate) fn witness_scope(&self) -> &[(String, Term)] {
        &self.witness_scope
    }

    /// Replace the type of an existing assumption in place — the innermost
    /// binding of `label`. Used by the `rec` elaborators: a group's signatures
    /// must be assumed (lowered) before they can be elaborated, since members
    /// reference each other, and are then upgraded here to their rebuilt forms
    /// — implicit insertion makes the two no longer interchangeable, and a
    /// lowered type must never leak into later reduction. Panics if `label`
    /// has no prior assumption — every caller is expected to have `assume`d
    /// it earlier in the same scope (a construction bug otherwise, not a
    /// user-facing case).
    pub(crate) fn reassume(&mut self, label: &str, type_: &Term) {
        self.locals_stamp.fresh();
        self.mutation_stamp.fresh();
        // A top-level `rec` group's names are qualified (`#`-free), so an
        // entry elaborated between the group's lowered `assume` and this
        // upgrade could embed the lowered signature; the upgrade makes the
        // two non-interchangeable (see below), so such entries must not
        // survive it.
        self.elaboration_cache.clear();

        let entry = self
            .local
            .iter_mut()
            .rev()
            .find(|(name, _)| name == label)
            .unwrap_or_else(|| panic!("reassume: '{label}' has no local binding to replace"));
        entry.1 = type_.clone();

        let assumptions = self
            .assumptions
            .iter_mut()
            .rev()
            .find(|assumptions| assumptions.contains_key(label))
            .unwrap_or_else(|| {
                panic!("reassume: '{label}' has no assumption-frame entry to replace")
            });
        assumptions.insert(label.to_string(), type_.clone());
    }

    pub(crate) fn assumption(&self, label: &str) -> Option<&Term> {
        self.assumptions
            .iter()
            .rev()
            .find_map(|assumptions| assumptions.get(label))
    }

    /// Whether `label` currently has a definition entry in some frame — the
    /// settled-globals gate for [`Context::elaboration_cacheable`]. A name
    /// defined here will only ever be *re*defined (which clears both caches
    /// wholesale), never freshly defined, so an elaboration entry naming it is
    /// safe to keep across a later fresh `define`.
    fn is_defined(&self, label: &str) -> bool {
        self.definitions
            .iter()
            .any(|frame| frame.contains_key(label))
    }

    /// The local assumption context in binding order (outermost first). The
    /// dependent-match generalizer (`elaborate_match`) walks this to find the
    /// hypotheses whose type depends on a scrutinee index being abstracted: they
    /// must ride into the motive as Π-binders, or the synthesized motive is
    /// ill-typed. Binding order matters — a hypothesis's type can only mention
    /// earlier binders, so the telescope it yields is already well-ordered.
    pub(crate) fn locals(&self) -> &[(String, Term)] {
        &self.local
    }

    fn define_entry(&mut self, label: String, entry: DefEntry) {
        self.mutation_stamp.fresh();

        // A *fresh* definition can only unstick reductions that read this
        // name's absence, and a stuck read always leaves the name free in the
        // WHNF (the name analogue of the unsolved-metavariable argument in
        // `Context::reduce`) — so the reduction cache retains every entry whose
        // result does not mention it instead of clearing wholesale. This keeps
        // closed reducts warm across item boundaries: erasure re-derives an
        // item right after its `define`, and a cold re-reduction of a deep
        // closed spine (a string literal's scan-state chain) would repeat all
        // of its work.
        //
        // The elaboration cache survives the same fresh definition with no
        // retain at all: its insert gate (`elaboration_cacheable`) already
        // refused every entry naming a not-yet-defined global, so the fresh
        // label appears in no surviving entry. A `#`-minted `let` binder —
        // which `reduce_let` leaks and the frame elaborators mint — is excluded
        // from caching outright (`has_local_free`), and a `/`-qualified global
        // is only ever referenced once defined; so not clearing lets a deep
        // spine memoize once across those definitions instead of re-elaborating
        // its shared subterms after each.
        //
        // A *redefinition* voids both arguments — `reduce_let` and the frame
        // elaborators define under labels that can rebind or shadow, and the
        // old value may sit consumed inside a reduct or an elaboration result
        // that no longer mentions the label — so there both caches clear
        // wholesale.
        let redefinition = self
            .definitions
            .iter()
            .any(|frame| frame.contains_key(&label));
        if redefinition {
            self.reduction_cache.clear();
            self.elaboration_cache.clear();
        } else {
            self.reduction_cache
                .retain(|_, reduct| !reduct.mentions_free(&label));
        }

        self.definitions.last_mut().unwrap().insert(label, entry);
    }

    pub(crate) fn define<A>(&mut self, label: A, term: &Term)
    where
        A: Into<String>,
    {
        self.define_entry(label.into(), DefEntry { term: term.clone() });
    }

    pub(crate) fn define_assuming<A>(&mut self, label: A, type_: &Term, term: &Term)
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
    pub(crate) fn refine<A>(&mut self, label: A, term: &Term)
    where
        A: Into<String>,
    {
        self.mutation_stamp.fresh();
        self.refinements
            .last_mut()
            .unwrap()
            .insert(label.into(), term.clone());

        self.reduction_cache.clear();
        self.elaboration_cache.clear();
    }

    /// Register a counterfactual refinement of a projection (`refine_head` on a
    /// `Proj` scrutinee).
    pub(crate) fn refine_projection(&mut self, base: Term, index: usize, value: Term) {
        self.mutation_stamp.fresh();
        self.refinement_projections
            .last_mut()
            .unwrap()
            .insert((base, index), value);

        self.reduction_cache.clear();
        self.elaboration_cache.clear();
    }

    /// The reduct of a variable: its definition, or — unless refinements are
    /// suppressed — its counterfactual refinement. Labels never appear in both
    /// stores (definitions name `let`/`rec` binders; refinements name assumed
    /// scrutinee heads), so the order between them is immaterial.
    pub(crate) fn var_reduct(&self, label: &str) -> Option<&Term> {
        if !self.suppress_refinements
            && let Some(term) = self.refinements.iter().rev().find_map(|r| r.get(label))
        {
            return Some(term);
        }

        self.definitions
            .iter()
            .rev()
            .find_map(|definitions| definitions.get(label))
            .map(|entry| &entry.term)
    }

    /// The reduct of a projection: its counterfactual match-arm refinement,
    /// unless refinements are suppressed (re-validation, §7.4).
    pub(crate) fn proj_reduct(&self, base: &Term, index: usize) -> Option<&Term> {
        if self.suppress_refinements {
            return None;
        }

        self.refinement_projections
            .iter()
            .rev()
            .find_map(|p| p.get(&(base.clone(), index)))
    }

    /// Register a counterfactual refinement of a stuck-application scrutinee
    /// (`refine_head` on a non-key head). `canonical` is the canonical
    /// form (head verbatim, arguments in WHNF); `value` is the arm's
    /// constructor. Sound for the same reason `refine` is — the arm is reached
    /// only when the scrutinee equals `value` — and non-cyclic because `value`
    /// is a constructor of the scrutinee's inductive, a normal form.
    pub(crate) fn refine_scrutinee(&mut self, canonical: Term, value: Term) {
        self.mutation_stamp.fresh();
        self.refinement_scrutinees
            .last_mut()
            .unwrap()
            .insert(canonical, value);

        self.reduction_cache.clear();
        self.elaboration_cache.clear();
    }

    /// Whether any scrutinee refinement is registered (regardless of
    /// suppression). The cheap outer gate for the reducer probe — skipped on
    /// the common refinement-free reduction without hashing anything.
    pub(crate) fn has_scrutinee_refinements(&self) -> bool {
        !self.refinement_scrutinees.iter().all(|f| f.is_empty())
    }

    /// Whether some registered scrutinee key shares `label` as its applied-head
    /// symbol. The second gate, past [`Term::head_label`]: only a head that is
    /// actually refined justifies canonicalizing the candidate's arguments.
    pub(crate) fn scrutinee_head_refined(&self, label: &str) -> bool {
        self.refinement_scrutinees
            .iter()
            .any(|f| f.keys().any(|k| k.head_label() == Some(label)))
    }

    /// The reduct of a canonical stuck scrutinee: its refinement value, unless
    /// suppressed (re-validation, §7.4).
    pub(crate) fn scrutinee_reduct(&self, canonical: &Term) -> Option<&Term> {
        if self.suppress_refinements {
            return None;
        }

        self.refinement_scrutinees
            .iter()
            .rev()
            .find_map(|f| f.get(canonical))
    }

    /// Whether `canonical` is itself a registered scrutinee key — checked *past*
    /// suppression. A `Var`/`Proj` key stays neutral under suppression for free
    /// (its reduct is withheld, so it does not unfold); an application key would
    /// otherwise unfold to its definition body and stop being a key. The reducer
    /// consults this to keep such a key neutral while suppressed, so
    /// `solve_refinement_free`'s committed (refinement-free) spelling stays a
    /// term the live refinement can still fire on.
    pub(crate) fn is_scrutinee_key(&self, canonical: &Term) -> bool {
        self.refinement_scrutinees
            .iter()
            .any(|f| f.contains_key(canonical))
    }

    pub(crate) fn refinements_suppressed(&self) -> bool {
        self.suppress_refinements
    }

    /// Whether any counterfactual refinement is currently registered (and not
    /// already suppressed) — the gate for the refinement-free candidate
    /// re-reduction in `Convert::solve_refinement_free`, so the common
    /// refinement-free path pays nothing.
    pub(crate) fn has_refinements(&self) -> bool {
        !self.suppress_refinements && self.any_refinements_registered()
    }

    /// Whether any counterfactual refinement of any kind is registered in any
    /// frame, *regardless* of suppression. The cache-contamination gate for
    /// [`Context::with_suppressed_refinements`]: only a registered refinement
    /// can make a suppressed reduct differ from the live one. (`has_refinements`
    /// is this plus "not already suppressed".)
    fn any_refinements_registered(&self) -> bool {
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

    /// Run `f` with refinements suppressed (re-validation, §7.4). Brackets the
    /// region with reduction-cache clears so refinement-applied and
    /// refinement-suppressed reducts never contaminate each other's cache — but
    /// only when some refinement is actually registered. With none, suppressing
    /// changes no reduct, so the flag is inert and the clears are pure waste
    /// (the common re-validation path: an oracle run outside any match arm).
    /// Each boundary is gated on the live state independently, so a refinement
    /// added and dropped *inside* `f` — which clears on its own add and exit —
    /// does not force a clear here.
    pub(crate) fn with_suppressed_refinements<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let previous = self.suppress_refinements;

        if self.any_refinements_registered() {
            self.reduction_cache.clear();
            self.elaboration_cache.clear();
        }

        self.suppress_refinements = true;
        let result = f(self);
        self.suppress_refinements = previous;

        if self.any_refinements_registered() {
            self.reduction_cache.clear();
            self.elaboration_cache.clear();
        }

        result
    }

    // === Inductive registry =================================================

    /// Record a new inductive declaration's metadata. Called once per
    /// `induct` declaration as a module is seeded into the context. Errs with
    /// `DuplicateInductive` (leaving the existing entry untouched) if `name`
    /// is already registered — the registry is shared across every root
    /// elaborated into this `Context`, so a collision is rejected rather than
    /// silently overwriting a prior root's declaration. Mid-elaboration
    /// rebuilds of an already-registered entry go through
    /// [`Context::update_inductive`] instead.
    pub(crate) fn register_inductive<N>(
        &mut self,
        name: N,
        inductive: Inductive,
    ) -> Result<(), Error>
    where
        N: Into<String>,
    {
        let name = name.into();
        if self.inductives.contains_key(&name) {
            return Err(Error::duplicate_inductive(name));
        }
        self.inductives.insert(name, inductive);
        Ok(())
    }

    /// Overwrite an already-registered inductive's metadata with a rebuilt
    /// telescope — called mid-elaboration by `elaborate_inductive_indices`/
    /// `elaborate_inductive_constructors` to refine the same declaration's
    /// own entry, not to register a new one, so unlike
    /// [`Context::register_inductive`] this always overwrites. Panics if
    /// `name` has no prior entry — every caller is expected to have checked
    /// `Context::inductive` first (a construction bug otherwise, not a
    /// user-facing case).
    pub(crate) fn update_inductive<N>(&mut self, name: N, inductive: Inductive)
    where
        N: Into<String>,
    {
        let name = name.into();
        assert!(
            self.inductives.contains_key(&name),
            "update_inductive: '{name}' is not already registered"
        );
        self.inductives.insert(name, inductive);
    }

    /// Look up an inductive declaration by the type's qualified name.
    pub(crate) fn inductive(&self, name: &str) -> Option<&Inductive> {
        self.inductives.get(name)
    }

    // === Struct registry ====================================================

    /// Record a new struct declaration's metadata. Called once per `struct`
    /// declaration as a module is seeded into the context (elaboration or
    /// erasure). Errs with `DuplicateStructure` (leaving the existing entry
    /// untouched) if `name` is already registered — the registry is shared
    /// across every root elaborated into this `Context`, so a collision is
    /// rejected rather than silently overwriting a prior root's declaration.
    /// Mid-elaboration rebuilds of an already-registered entry go through
    /// [`Context::update_structure`] instead.
    pub(crate) fn register_structure<N>(
        &mut self,
        name: N,
        structure: Structure,
    ) -> Result<(), Error>
    where
        N: Into<String>,
    {
        let name = name.into();
        if self.structures.contains_key(&name) {
            return Err(Error::duplicate_structure(name));
        }
        self.structures.insert(name, structure);
        Ok(())
    }

    /// Overwrite an already-registered struct's metadata with rebuilt field
    /// types — called mid-elaboration by `elaborate_structure` to refine the
    /// same declaration's own entry, not to register a new one, so unlike
    /// [`Context::register_structure`] this always overwrites. Panics if
    /// `name` has no prior entry — every caller is expected to have checked
    /// `Context::structure` first (a construction bug otherwise, not a
    /// user-facing case).
    pub(crate) fn update_structure<N>(&mut self, name: N, structure: Structure)
    where
        N: Into<String>,
    {
        let name = name.into();
        assert!(
            self.structures.contains_key(&name),
            "update_structure: '{name}' is not already registered"
        );
        self.structures.insert(name, structure);
    }

    /// Look up a struct declaration by the type's qualified name.
    pub(crate) fn structure(&self, name: &str) -> Option<&Structure> {
        self.structures.get(name)
    }

    // === Concept & witness registries =======================================

    /// Record a new concept declaration's resolution metadata (its record
    /// shape is registered separately, as an ordinary structure). Called once
    /// per `concept` declaration when a module's registries are seeded. Errs
    /// with `DuplicateConcept` (leaving the existing entry untouched) if
    /// `name` is already registered — the registry is shared across every
    /// root elaborated into this `Context`, so a collision is rejected rather
    /// than silently overwriting a prior root's declaration.
    pub(crate) fn register_concept<N>(&mut self, name: N, concept: Concept) -> Result<(), Error>
    where
        N: Into<String>,
    {
        let name = name.into();
        if self.concepts.contains_key(&name) {
            return Err(Error::duplicate_concept(name));
        }
        self.concepts.insert(name, concept);
        Ok(())
    }

    /// Look up a concept by its qualified name.
    pub(crate) fn concept(&self, name: &str) -> Option<&Concept> {
        self.concepts.get(name)
    }

    /// The registered concepts, for whole-registry validation (superclass
    /// acyclicity) at seed time.
    pub(crate) fn concepts(&self) -> &BTreeMap<String, Concept> {
        &self.concepts
    }

    /// The compilation root that declares one witness key's rigid head — a
    /// nominal head's own `root` (looked up from whichever registry has it,
    /// struct or inductive), or the fixed `RootId::Sys` for a primitive head,
    /// which is never user-declarable. Consulted by the orphan-rule check in
    /// `register_witness`.
    pub(crate) fn root_of_head(&self, head: &HeadKey) -> RootId {
        match head {
            HeadKey::Nominal(name) => self
                .structure(name)
                .map(|structure| structure.root)
                .or_else(|| self.inductive(name).map(|inductive| inductive.root))
                .expect("a nominal head names a registered structure or inductive"),
            _ => RootId::Sys,
        }
    }

    /// Mark a definition name as a witness declaration; when its signature
    /// elaborates, `elaborate_module` registers it into the witness table.
    pub(crate) fn mark_witness_declaration<N: Into<String>>(&mut self, name: N) {
        self.witness_declarations.insert(name.into());
    }

    pub(crate) fn is_witness_declaration(&self, name: &str) -> bool {
        self.witness_declarations.contains(name)
    }

    /// The witness registered under `(concept, key)`, if any.
    pub(crate) fn witness(&self, concept: &str, key: &WitnessKey) -> Option<&Witness> {
        self.witness_table.get(&(concept.to_string(), key.clone()))
    }

    /// Insert a witness under its key, returning the previous occupant's name
    /// on a collision (the caller reports `DuplicateWitness`).
    pub(crate) fn insert_witness(
        &mut self,
        concept: String,
        key: WitnessKey,
        witness: Witness,
    ) -> Option<String> {
        match self.witness_table.get(&(concept.clone(), key.clone())) {
            Some(existing) => Some(existing.name.clone()),
            None => {
                self.mutation_stamp.fresh();
                self.witness_table.insert((concept, key), witness);
                None
            }
        }
    }

    /// Defer a witness goal whose key is rigid but has no table entry yet —
    /// retried after later items register witnesses, reported only at the end
    /// of the module.
    pub(crate) fn defer_witness(&mut self, goal: ParkedGoal) {
        self.mutation_stamp.fresh();
        self.deferred_witnesses.push(goal);
    }

    /// Take every deferred witness goal for a retry sweep.
    pub(crate) fn take_deferred_witnesses(&mut self) -> Vec<ParkedGoal> {
        mem::take(&mut self.deferred_witnesses)
    }

    /// The module whose item is currently being elaborated (the qualifier
    /// prefix of its name; empty for the root), or `None` when no surface
    /// item is being elaborated — which suppresses the representation-privacy
    /// checks (see the field's invariant).
    pub(crate) fn island(&self) -> Option<&Qualifier> {
        self.island.as_ref()
    }

    /// Set the current module before elaborating an item (see
    /// `elaborate_module`).
    pub(crate) fn set_island(&mut self, island: Qualifier) {
        // Representation-privacy checks are island-relative, so an entry
        // elaborated under one item's island must not answer for another's.
        // Every item boundary also lands a `define_entry` clear, but this one
        // keeps the cache's soundness independent of that ordering.
        self.elaboration_cache.clear();
        self.island = Some(island);
    }

    /// Run `f` with no island — suppressing the representation-privacy checks
    /// for re-derivation of already-elaborated terms, whose machinery-built
    /// projections were never subject to surface privacy in the first place.
    /// The bracket is the only way to clear an island (mirroring the parking
    /// half of the oracle package), so no context can be left permanently
    /// altered.
    pub(crate) fn with_suppressed_privacy<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let previous = self.island.take();
        let result = f(self);
        self.island = previous;

        result
    }

    // === Metavariable store =================================================

    /// Materialize a metavariable's birth record (§5). The store grows to cover
    /// `id`; births happen exactly once per id (each `_` is distinct and occurs
    /// once).
    pub(crate) fn birth_metavar(
        &mut self,
        id: MetavarId,
        telescope: impl Into<SharedTelescope>,
        result: Term,
    ) {
        self.mutation_stamp.fresh();
        if id.0 >= self.metas.entries.len() {
            self.metas.entries.resize_with(id.0 + 1, || None);
        }

        self.metas.entries[id.0] = Some(MetaEntry {
            telescope: telescope.into(),
            result,
            solution: None,
            kind: MetaKind::Inference,
        });
    }

    /// Allocate the protected placeholder for one member of a recursive
    /// group. It has the same contextual spine as an inference metavariable so
    /// parked work can carry it across a popped local frame, but only
    /// `fill_rec_slot` may solve it.
    pub(crate) fn fresh_rec_slot(&mut self, result: Term) -> (MetavarId, Term) {
        self.mutation_stamp.fresh();
        let id = self.next_metavar.fresh();
        let (telescope, spine) = self.identity_snapshot();
        if id.0 >= self.metas.entries.len() {
            self.metas.entries.resize_with(id.0 + 1, || None);
        }
        self.metas.entries[id.0] = Some(MetaEntry {
            telescope,
            result,
            solution: None,
            kind: MetaKind::RecSlot,
        });
        (id, Term::metavar_birthed(id, None, spine))
    }

    pub(crate) fn is_rec_slot(&self, id: MetavarId) -> bool {
        self.metavar_entry(id)
            .is_some_and(|entry| entry.kind == MetaKind::RecSlot)
    }

    pub(crate) fn fill_rec_slot(&mut self, id: MetavarId, term: Term) {
        let entry = self
            .metas
            .entries
            .get(id.0)
            .and_then(Option::as_ref)
            .expect("recursive slot has a birth entry");
        assert_eq!(entry.kind, MetaKind::RecSlot, "filled a non-rec slot");
        assert!(entry.solution.is_none(), "recursive slot filled twice");
        self.solve_metavar(id, term);
    }

    /// The boundary between the top-level (base-frame) entries of `local` and
    /// the genuine local binders above them. Top-level definitions are
    /// `assume`d into `local` at the base level (never inside a frame), so the
    /// outermost frame mark is exactly the count of top-level entries; with no
    /// frame open, everything in `local` is top-level. A metavariable's Γ is
    /// only the binders past this point (see [`Context::identity_snapshot`]).
    fn base_locals(&self) -> usize {
        self.local_marks
            .first()
            .copied()
            .unwrap_or(self.local.len())
    }

    /// Whether `name` is bound at the top level (the persistent base frame) —
    /// a global definition, always in scope. The metavariable solver admits
    /// such names in a solution even though they are not in the metavariable's
    /// Γ/spine (which holds only local binders): a solution may freely mention
    /// a global constant without that constant being a context binder.
    pub(crate) fn is_top_level(&self, name: &str) -> bool {
        self.assumptions
            .first()
            .is_some_and(|frame| frame.contains_key(name))
    }

    /// The frozen telescope and identity spine for the *current* Γ, shared:
    /// rebuilt only when `local` has changed since the last birth, so minting
    /// a metavariable is O(1) amortized instead of O(|Γ|) per mint — the
    /// difference between linear and quadratic elaboration over a module.
    ///
    /// Γ is the *local* binders only — `local` past `Context::base_locals`.
    /// Top-level definitions are excluded so an item's elaboration is
    /// independent of how much else is in scope: a metavariable born deep in a
    /// proof carries just its enclosing binders, not the whole prelude, keeping
    /// the contextual solve's spine a small pattern (and the prelude cacheable).
    /// Globals a solution mentions are admitted by the solver's scope check via
    /// [`Context::is_top_level`] instead.
    pub(crate) fn identity_snapshot(&mut self) -> (SharedTelescope, SharedSpine) {
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
    /// argument (the count `into_core` minted) before any item is elaborated.
    pub(crate) fn seed_metavars(&mut self, floor: usize) {
        self.next_metavar.seed(floor);
    }

    /// Mint a metavariable for an omitted implicit argument and birth it
    /// immediately — frozen local Γ, the binder's instantiated type as
    /// `result` — so the id always has a birth record. Returns the
    /// metavariable term carrying the *call site's* span and the insertion
    /// provenance (which rides on the node; see [`Metavar::origin`]).
    pub(crate) fn fresh_metavar(
        &mut self,
        result: Term,
        span: Option<Span>,
        origin: ImplicitOrigin,
    ) -> Term {
        self.fresh_metavar_with(result, span, Some(MetavarOrigin::Implicit(origin)))
            .1
    }

    /// Mint a metavariable for an omitted `use` argument — like
    /// [`Context::fresh_metavar`] but carrying witness provenance, and
    /// returning the id so the caller can register the resolution goal.
    pub(crate) fn fresh_witness_metavar(
        &mut self,
        result: Term,
        span: Option<Span>,
        origin: WitnessOrigin,
    ) -> (MetavarId, Term) {
        self.fresh_metavar_with(result, span, Some(MetavarOrigin::Witness(origin)))
    }

    /// Mint an unmarked (silently spliced) metavariable — the stand-in type a
    /// written goal in synthesis position gets, so the goal survives to zonk's
    /// report instead of dying with `CannotInfer` (`elaborate_metavar`).
    pub(crate) fn fresh_unmarked_metavar(&mut self, result: Term, span: Option<Span>) -> Term {
        self.fresh_metavar_with(result, span, None).1
    }

    fn fresh_metavar_with(
        &mut self,
        result: Term,
        span: Option<Span>,
        origin: Option<MetavarOrigin>,
    ) -> (MetavarId, Term) {
        let id = self.next_metavar.fresh();
        let (telescope, spine) = self.identity_snapshot();
        self.birth_metavar(id, telescope, result);
        let metavar = Term::metavar_birthed(id, origin, spine);

        let metavar = match span {
            Some(span) => metavar.with_span(span),
            None => metavar,
        };

        (id, metavar)
    }

    pub(crate) fn metavar_entry(&self, id: MetavarId) -> Option<&MetaEntry> {
        self.metas.entries.get(id.0).and_then(Option::as_ref)
    }

    /// If `term` is a witness-resolution hole, its provenance and the concept
    /// goal it stands for (e.g. `Add(Nat)`). A conversion left stuck between two
    /// such holes is really an unresolved witness — reported as one rather than
    /// as a bare metavariable mismatch between two anonymous placeholders.
    pub(crate) fn witness_hole(&self, term: &Term) -> Option<(WitnessOrigin, Term)> {
        let Subterm::Metavar(metavar) = &**term else {
            return None;
        };
        let Some(MetavarOrigin::Witness(origin)) = &metavar.origin else {
            return None;
        };
        let goal = self.metavar_entry(metavar.id)?.result.clone();
        Some((origin.clone(), goal))
    }

    pub(crate) fn metavar_solution(&self, id: MetavarId) -> Option<&Term> {
        self.metavar_entry(id).and_then(|e| e.solution.as_ref())
    }

    /// Resolve a solved metavariable *at its occurrence*: the stored solution
    /// is spelled with the birth telescope's names, and the occurrence's spine
    /// records what each of those binders corresponds to here — so resolution
    /// is the solution with birth names rewritten through the spine. An empty
    /// spine (a never-rebuilt `into_core` hole) resolves as the identity.
    /// `None` while unsolved.
    pub(crate) fn resolve_metavar(&self, metavar: &Metavar) -> Option<Term> {
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

    /// Commit a metavariable's solution. Needs no reduction-cache clear: a WHNF
    /// that still named an unsolved metavariable was never memoized (see
    /// `Context::reduce`), and a solve is monotonic, so every
    /// surviving entry stays valid (§7.2). (Re-validation's
    /// [`Context::rollback_solutions`], which *un*-solves, does clear.) Records
    /// the id as newly solved — the wake signal for parked constraints (§8) —
    /// and journals it for [`Context::rollback_solutions`].
    pub(crate) fn solve_metavar(&mut self, id: MetavarId, term: Term) {
        self.mutation_stamp.fresh();
        if let Some(Some(entry)) = self.metas.entries.get_mut(id.0) {
            entry.solution = Some(term);
            self.newly_solved.push(id);
            self.solved_log.push(id);
        }
    }

    /// Watermark for [`Context::rollback_solutions`]: how many solutions have
    /// been committed so far.
    pub(crate) fn solution_mark(&self) -> usize {
        self.solved_log.len()
    }

    /// Unwind every solution committed since `mark` — the transactional
    /// bracket around re-validation (§7.4). Validating a candidate runs full
    /// elaboration, which can solve *other* metavariables along the way; if
    /// the candidate is ultimately rejected, those nested solutions were
    /// derived from an equation that never held and must not survive the
    /// verdict. Removes the unwound ids from the wake signals and clears the
    /// reduction cache, which may have cached reducts through them.
    pub(crate) fn rollback_solutions(&mut self, mark: usize) {
        if self.solved_log.len() <= mark {
            return;
        }

        let unwound = self.solved_log.split_off(mark);

        for id in &unwound {
            if let Some(Some(entry)) = self.metas.entries.get_mut(id.0) {
                entry.solution = None;
            }
        }

        self.mutation_stamp.fresh();
        self.newly_solved.retain(|id| !unwound.contains(id));
        self.reduction_cache.clear();
        // Entries are metavar-free on both key and value, so an un-solve
        // cannot invalidate them in principle; cleared anyway while the
        // rollback bracket is young — conservative and cheap.
        self.elaboration_cache.clear();
    }

    // === Parked constraints (§8) ============================================

    /// Freeze the live local frame (the way `fresh_metavar` freezes Γ): the
    /// base frame persists for the whole elaboration, so only the local
    /// frames — which pop before a retry can happen — are captured.
    pub(crate) fn freeze_frame(&self) -> FrozenFrame {
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
            refinement_scrutinees: flatten_frames(&self.refinement_scrutinees),
            witness_binders: self.witness_scope.clone(),
        }
    }

    /// Reapply a frozen frame inside a fresh `with_frame`, restoring the
    /// equalities the parked problem's origin saw.
    pub(crate) fn restore_frame(&mut self, frame: &FrozenFrame) {
        for (name, type_) in &frame.assumptions {
            self.assume(name, type_);
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

        for (canonical, value) in &frame.refinement_scrutinees {
            self.refine_scrutinee(canonical.clone(), value.clone());
        }

        // The witness binders were already re-assumed by the loop above (they
        // are a subset of `assumptions`); only the scope membership is
        // restored here. The enclosing frame's mark truncates it on exit.
        self.witness_scope.extend(frame.witness_binders.clone());
    }

    /// Park blocked work: freeze the live local frame around it and record
    /// which unsolved metavariables could unblock it.
    pub(crate) fn park(&mut self, work: ParkedWork, origin: Term) {
        let frame = self.freeze_frame();
        self.repark(work, origin, frame);
    }

    /// Re-park work that is still blocked after a retry, keeping its
    /// originally frozen frame. The watch set is recomputed from the work's
    /// current unsolved metavariables.
    pub(crate) fn repark(&mut self, work: ParkedWork, origin: Term, frame: FrozenFrame) {
        self.mutation_stamp.fresh();
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
            ParkedWork::Witness { goal, .. } => goal
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
    pub(crate) fn fresh_placeholder(
        &mut self,
        result: Term,
        span: Option<Span>,
    ) -> (MetavarId, Term) {
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
    pub(crate) fn wake_parked(&mut self) -> Vec<ParkedGoal> {
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
    pub(crate) fn take_parked(&mut self) -> Vec<ParkedGoal> {
        mem::take(&mut self.parked)
    }

    pub(crate) fn parked_len(&self) -> usize {
        self.parked.len()
    }

    pub(crate) fn has_newly_solved(&self) -> bool {
        !self.newly_solved.is_empty()
    }

    pub(crate) fn parking_suppressed(&self) -> bool {
        self.suppress_parking
    }

    /// Run `f` as a yes/no *oracle* around full elaboration (re-validation):
    /// parking is suppressed — `expect` treats `Blocked` as a mismatch and
    /// `retry_parked` is a no-op, so provisional success can neither leak into
    /// the verdict nor consume a parked obligation whose error the oracle
    /// would swallow — counterfactual refinements are suppressed with it, and
    /// so are the representation-privacy checks: an oracle candidate is a
    /// unification artifact that can embed machinery-built projections
    /// (eta-expansions, witness splices) whose privacy elaboration already
    /// adjudicated, and a swallowed privacy error would silently flip the
    /// verdict. The suppressions are a package: an oracle that set only some
    /// would be subtly unsound, which is why the parking half has no public
    /// setter.
    pub(crate) fn with_oracle<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.with_suppressed_parking(|context| {
            context.with_suppressed_refinements(|context| context.with_suppressed_privacy(f))
        })
    }

    fn with_suppressed_parking<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let previous = self.suppress_parking;
        self.suppress_parking = true;
        let result = f(self);
        self.suppress_parking = previous;

        result
    }
}
