use {
    super::{
        Bound, Concept, Error, Goal, HeadKey, ImplicitOrigin, Inductive, Metavar, MetavarId,
        MetavarOrigin, RecId, Structure, Subterm, Term, Witness, WitnessKey, WitnessOrigin,
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
}

/// Flat, frame-independent store of metavariable records, indexed by
/// `Metavar::id`. Its contents are monotonic facts about the program being
/// elaborated, not lexically-scoped bindings — so `enter_frame`/`leave_frame`
/// never touch it.
#[derive(Debug, Default)]
pub(crate) struct MetaStore {
    entries: Vec<Option<MetaEntry>>,
}

/// One definition: the definiens plus whether the label is a `rec`-group
/// member. `reduce_apply`'s match-guarded delta reads `recursive` to decide
/// whether a stuck unfolding refolds — a recursive call whose unfolding stays
/// a stuck match is its own normal form. The flag is set in exactly one
/// place, [`Context::define_rec_members`] — `define`/`define_assuming` never
/// take a `recursive` parameter, so there is no "plain" path a rec-group
/// registration could accidentally go through instead.
#[derive(Debug, Clone)]
pub(crate) struct DefEntry {
    term: Term,
    recursive: bool,
}

impl DefEntry {
    /// Whether this entry's substitution is withheld: a `rec`-group member
    /// still inside its raw mutual-checking window — its registered body
    /// carries elaboration-transient material (see
    /// [`Term::contains_transient`]) the rebuilt registration will replace.
    /// Substituting it would let a consumer freeze a disconnected copy of the
    /// raw body (a conversion comparison against it, a metavariable *solved*
    /// to it), and a copy is dead: no later elaboration progress ever updates
    /// it. The name itself is the one live indirection, so [`Context::var_reduct`]
    /// breaks at it instead — the value-level counterpart of the refold
    /// `reduce_apply` performs for a recursive application.
    ///
    /// `Func`-rooted bodies are exempt: an application must still open them
    /// *per call*, because whether the raw material is even reachable depends
    /// on the arguments (a type family's `T(2)` must reduce to `Nat` through
    /// a clean arm mid-window even when another arm is still raw) —
    /// `reduce_apply`'s post-hoc guard decides path-sensitively from the
    /// reduced result. A *bare* reference escaping as a raw `Func` value is
    /// caught by conversion's transient backstop instead. Derived entirely
    /// from the registered body — no separate "elaborated" flag to keep in
    /// sync, and nothing to reset: the rebuilt body contains no transients,
    /// so registration itself closes the window. Order matters for the hot
    /// path: the deep (cached) walk runs only for non-`Func` recursive
    /// entries, which exist only mid-window.
    fn withheld(&self) -> bool {
        self.recursive && !matches!(&*self.term, Subterm::Func(_)) && self.term.contains_transient()
    }
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

/// The kernel's ambient state, threaded mutably through elaboration, typing, reduction, conversion, and erasure. Two lifetimes coexist: *frame-scoped* lexical state (assumptions, local definitions, the counterfactual refinement stores, the witness scope), pushed and popped as binders and match arms are entered, and *flat monotonic facts* about the program (the `MetaStore`, inductive/struct/concept declarations, the witness table, parked and deferred goals), which frames never touch. The single deadline fixed at construction bounds *total* work across every call sharing the context — see [`Context::new`].
#[derive(Debug)]
pub struct Context {
    fresh_names: Entropy,
    deadline: Instant,
    reduction_cache: HashMap<Term, Term>,
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
    metas: MetaStore,
    // The next metavariable id this context may mint (implicit-argument
    // insertion). Seeded by `elaborate_module` with its `metavar_floor`
    // argument so core-minted ids sit strictly above `into_core`'s.
    next_metavar: Entropy<MetavarId>,
    // The next `RecId` this context may mint (one per local `rec` group, at
    // elaboration — see `Context::fresh_rec`).
    next_rec: Entropy<RecId>,
    // Memoized `rec` openings, indexed by `RecId` (see `Context::unfold_rec`
    // in `reduce.rs`): every unfolding of the same local `rec` group yields
    // the *same* opened tail, so its recursive labels stay stable across
    // repeated unfoldings — conversion's syntactic short-circuit and spine
    // comparisons rely on that to terminate. Keyed by id rather than by
    // whole-term structural equality, so it stays valid across any
    // subsequent capture/release or cloning of the `Rec` node. Flat and
    // monotonic, like `metas` — `enter_frame`/`leave_frame` never touch it.
    rec_unfolds: Vec<Option<Term>>,
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
    // The program-wide witness table: one witness per (concept, input-head
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
    // prefix of that item's name (the root module is the empty qualifier). Set
    // by `elaborate_module` per item; read by `elaborate_proj` for the struct
    // representation-privacy check (§7).
    island: Qualifier,
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
            next_rec: Entropy::<RecId>::new(),
            rec_unfolds: Vec::new(),
            inductives: BTreeMap::new(),
            structures: BTreeMap::new(),
            concepts: BTreeMap::new(),
            witness_declarations: BTreeSet::new(),
            witness_table: BTreeMap::new(),
            witness_scope: Vec::new(),
            witness_marks: Vec::new(),
            deferred_witnesses: Vec::new(),
            island: Qualifier::empty(),
            parked: Vec::new(),
            newly_solved: Vec::new(),
            solved_log: Vec::new(),
            suppress_parking: false,
            locals_stamp: Entropy::new(),
            identity_cache: None,
        }
    }

    pub(crate) fn fresh(&mut self, hint: Option<&str>) -> String {
        let counter = self.fresh_names.fresh();

        match hint {
            Some(h) => format!("{h}#{counter}"),
            None => format!("#{counter}"),
        }
    }

    pub(crate) fn deadline(&self) -> Instant {
        self.deadline
    }

    pub(crate) fn get_or_init_reduced<E>(
        &mut self,
        term: Term,
        compute: impl FnOnce(&mut Self, Term) -> Result<Term, E>,
    ) -> Result<Term, E> {
        if let Some(cached) = self.reduction_cache.get(&term) {
            return Ok(cached.clone());
        }

        let result = compute(self, term.clone())?;

        // Memoize only closed terms whose WHNF names no *unsolved* metavariable
        // — `any_metavar` bails on the first one, never building the id set. A
        // solve is monotonic, so it can only invalidate a reduct that still
        // names the metavariable it solved, and reduction gets stuck on (hence
        // surfaces) an unsolved metavariable it actually depends on. Refusing to
        // cache those is what lets `solve_metavar` skip a cache clear; an entry
        // naming only *solved* metavariables stays valid under forward solves
        // (re-validation's `rollback_solutions`, which *un*-solves, clears
        // separately).
        let cacheable =
            term.closed() && !result.any_metavar(&mut |id| self.metavar_solution(id).is_none());

        if cacheable {
            self.reduction_cache.insert(term, result.clone());
        }

        Ok(result)
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

        if !definitions.is_empty()
            || !refinements.is_empty()
            || !refinement_projections.is_empty()
            || !refinement_scrutinees.is_empty()
        {
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
        self.definitions.last_mut().unwrap().insert(label, entry);

        self.reduction_cache.clear();
    }

    pub(crate) fn define<A>(&mut self, label: A, term: &Term)
    where
        A: Into<String>,
    {
        self.define_entry(
            label.into(),
            DefEntry {
                term: term.clone(),
                recursive: false,
            },
        );
    }

    /// Register a `rec`-group's members all at once — the *only* path that
    /// marks a definition recursive (see [`DefEntry`]). Used by local `rec`
    /// (`unfold_rec`, keyed to the group's shared frame) and top-level `rec`
    /// (`elaborate_module_rec`, the prelude replay) alike: whatever collects
    /// the group's `(label, body)` pairs calls this once for the whole group,
    /// so there is no per-item loop where one member could be registered
    /// through the plain, non-recursive `define` by mistake.
    pub(crate) fn define_rec_members(&mut self, members: &[(String, Term)]) {
        for (label, term) in members {
            self.define_entry(
                label.clone(),
                DefEntry {
                    term: term.clone(),
                    recursive: true,
                },
            );
        }
    }

    /// Upgrade a just-elaborated local `rec` group's members inside every
    /// parked goal's frozen frame. A goal parked during the group's raw
    /// window froze the live local frame — raw member bodies included — and
    /// its retry runs under that frozen copy *after* the group's frame has
    /// popped, so the second `define_rec_members` call (which upgrades only
    /// the live frame) never reaches it: without this, the retry re-reduces
    /// through the raw copy forever and a resolvable goal reports as a
    /// mismatch. Member labels are entropy-fresh (minted once per group in
    /// `elaborate_rec`), so a label match is unambiguous across every frozen
    /// frame. Top-level groups need no refresh — they live in the persistent
    /// base frame, which `freeze_frame` deliberately skips.
    pub(crate) fn refresh_parked_rec_members(&mut self, members: &[(String, Term)]) {
        for goal in self
            .parked
            .iter_mut()
            .chain(self.deferred_witnesses.iter_mut())
        {
            for (label, entry) in &mut goal.frame.definitions {
                if let Some((_, rebuilt)) = members.iter().find(|(name, _)| name == label) {
                    *entry = DefEntry {
                        term: rebuilt.clone(),
                        recursive: true,
                    };
                }
            }
        }
    }

    /// Test-only convenience over [`Context::define_rec_members`] for a
    /// single name — production code always registers a whole group at once
    /// (there being no single-member `rec` group of interest to isolate), so
    /// this stays behind `cfg(test)` rather than tempting a real call site
    /// into a one-off registration that bypasses the group API.
    #[cfg(test)]
    pub(crate) fn define_recursive<A>(&mut self, label: A, term: &Term)
    where
        A: Into<String>,
    {
        self.define_rec_members(&[(label.into(), term.clone())]);
    }

    /// Whether `name` is defined as a `rec`-group member (see [`DefEntry`]).
    /// `reduce_apply`'s match-guarded delta reads this to decide whether an
    /// application's unfolding may need refolding.
    pub(crate) fn is_recursive(&self, name: &str) -> bool {
        self.definitions
            .iter()
            .rev()
            .find_map(|definitions| definitions.get(name))
            .is_some_and(|entry| entry.recursive)
    }

    /// Whether `name`'s definition is withheld from substitution (see
    /// [`DefEntry::withheld`]): a `rec`-group value member still inside its
    /// raw mutual-checking window. Conversion consults this to *park* a goal
    /// stuck on such a bare neutral — the name carries no transient marker of
    /// its own, so without this the neutral would fall through to a hard
    /// structural mismatch.
    pub(crate) fn is_withheld_rec_member(&self, name: &str) -> bool {
        self.definitions
            .iter()
            .rev()
            .find_map(|definitions| definitions.get(name))
            .is_some_and(DefEntry::withheld)
    }

    pub(crate) fn define_assuming<A>(&mut self, label: A, type_: &Term, term: &Term)
    where
        A: Into<String>,
    {
        let label = label.into();
        self.assume(label.as_str(), type_);
        self.define(label, term);
    }

    /// Mint a fresh identity for a local `rec` group, at elaboration (see
    /// [`RecId`], and `Term::rec_with_id`). Unlike a metavariable, a `RecId`
    /// carries no birth record — it exists purely as a stable memo key for
    /// [`Context::rec_unfold`].
    pub(crate) fn fresh_rec(&mut self) -> RecId {
        self.next_rec.fresh()
    }

    /// The memoized opening of a local `rec` group, if this id was unfolded
    /// before (see `unfold_rec` in `reduce.rs`). Takes the id's raw index —
    /// callers only ever reach this after matching a `RecId::Elaborated`,
    /// the only variant that names a real memo slot.
    pub(crate) fn rec_unfold(&self, id: usize) -> Option<&Term> {
        self.rec_unfolds.get(id).and_then(Option::as_ref)
    }

    pub(crate) fn remember_rec_unfold(&mut self, id: usize, tail: Term) {
        if id >= self.rec_unfolds.len() {
            self.rec_unfolds.resize_with(id + 1, || None);
        }

        self.rec_unfolds[id] = Some(tail);
    }

    // === Refinements ========================================================

    /// Register a counterfactual match-arm refinement of a variable. Unlike
    /// `define`, this lives in a suppressible store so re-validation can ignore
    /// it. Clears the reduction cache, as the variable now reduces differently.
    pub(crate) fn refine<A>(&mut self, label: A, term: &Term)
    where
        A: Into<String>,
    {
        self.refinements
            .last_mut()
            .unwrap()
            .insert(label.into(), term.clone());

        self.reduction_cache.clear();
    }

    /// Register a counterfactual refinement of a projection (`refine_head` on a
    /// `Proj` scrutinee).
    pub(crate) fn refine_projection(&mut self, base: Term, index: usize, value: Term) {
        self.refinement_projections
            .last_mut()
            .unwrap()
            .insert((base, index), value);

        self.reduction_cache.clear();
    }

    /// The reduct of a variable: its definition, or — unless refinements are
    /// suppressed — its counterfactual refinement. Labels never appear in both
    /// stores (definitions name `let`/`rec` binders; refinements name assumed
    /// scrutinee heads), so the order between them is immaterial.
    ///
    /// A raw-window rec member is withheld (see [`DefEntry::withheld`]) —
    /// `None`, as if undefined — so the name reduces to its own neutral
    /// rather than substituting-and-exposing the elaboration-transient body.
    /// Folded into this single lookup (rather than a separate pre-check in
    /// `reduce_var`) because this is the kernel's hottest path: the guard
    /// costs one flag test on a *found* definition, and nothing on a miss.
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
            .filter(|entry| !entry.withheld())
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
        self.refinement_scrutinees
            .last_mut()
            .unwrap()
            .insert(canonical, value);

        self.reduction_cache.clear();
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
        }

        self.suppress_refinements = true;
        let result = f(self);
        self.suppress_refinements = previous;

        if self.any_refinements_registered() {
            self.reduction_cache.clear();
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
                self.witness_table.insert((concept, key), witness);
                None
            }
        }
    }

    /// Defer a witness goal whose key is rigid but has no table entry yet —
    /// retried after later items register witnesses, reported only at the end
    /// of the module.
    pub(crate) fn defer_witness(&mut self, goal: ParkedGoal) {
        self.deferred_witnesses.push(goal);
    }

    /// Take every deferred witness goal for a retry sweep.
    pub(crate) fn take_deferred_witnesses(&mut self) -> Vec<ParkedGoal> {
        mem::take(&mut self.deferred_witnesses)
    }

    /// The module whose item is currently being elaborated (the qualifier
    /// prefix of its name; empty for the root). Used by the struct projection
    /// privacy check.
    pub(crate) fn island(&self) -> &Qualifier {
        &self.island
    }

    /// Set the current module before elaborating an item (see
    /// `elaborate_module`).
    pub(crate) fn set_island(&mut self, island: Qualifier) {
        self.island = island;
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
        if id.0 >= self.metas.entries.len() {
            self.metas.entries.resize_with(id.0 + 1, || None);
        }

        self.metas.entries[id.0] = Some(MetaEntry {
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
    /// `Context::get_or_init_reduced`), and a solve is monotonic, so every
    /// surviving entry stays valid (§7.2). (Re-validation's
    /// [`Context::rollback_solutions`], which *un*-solves, does clear.) Records
    /// the id as newly solved — the wake signal for parked constraints (§8) —
    /// and journals it for [`Context::rollback_solutions`].
    pub(crate) fn solve_metavar(&mut self, id: MetavarId, term: Term) {
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

        self.newly_solved.retain(|id| !unwound.contains(id));
        self.reduction_cache.clear();
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
    /// would swallow — and counterfactual refinements are suppressed with it.
    /// The two suppressions are a package: an oracle that set only one would
    /// be subtly unsound, which is why the parking half has no public setter.
    pub(crate) fn with_oracle<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
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
