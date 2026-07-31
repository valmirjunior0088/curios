//! The lexical half of the kernel's state: assumptions, local definitions, counterfactual refinements, and the witness scope, all bracket-disciplined by `enter`/`leave`.
//!
//! Everything here lives and dies with binder frames — the opposite lifetime from the flat stores in [`Program`](super::Program) and [`Solutions`](super::Solutions). Cache coordination stays with the `Context` façade: a frame write that must clear or stamp the caches does so there, so this type's methods are pure store operations.

use {
    super::{SharedSpine, SharedTelescope},
    curios_base::Entropy,
    curios_core::{DefinitionKind, Free, HeadTag, Term, UniverseContext, project_erased_universes},
    std::{collections::HashMap, rc::Rc},
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
}

/// The local frame a parked problem froze at park time: assumptions (in binding order), and the non-base-frame definitions, counterfactual refinements, projection refinements, and scrutinee refinements (each outermost frame first, so reapplying in order reproduces the shadowing). A retry must run under the same equalities its origin saw — including the arm-local refinements — while solution re-validation independently suppresses them, keeping committed solutions refinement-free.
#[derive(Debug, Clone)]
pub(crate) struct FrozenFrame {
    pub(crate) assumptions: Vec<(Free, Term)>,
    pub(crate) definitions: Vec<(Free, DefEntry)>,
    pub(crate) refinements: Vec<(Free, Term)>,
    pub(crate) refinement_projections: Vec<((Term, usize), Term)>,
    pub(crate) refinement_scrutinees: Vec<(Term, Term)>,
    /// The `use`-plicity binders in scope at park time (a subset of `assumptions`, in the same binding order). Witness resolution scans these; a retry must see the same instance scope its origin saw.
    pub(crate) witness_binders: Vec<(Free, Term)>,
}

/// The frame-scoped lexical stores. `Context` holds exactly one of these; see the module documentation for the cache-coordination contract.
#[derive(Debug)]
pub(crate) struct Frames {
    assumptions: Vec<HashMap<Free, Term>>,
    assumption_universes: Vec<HashMap<Free, UniverseContext>>,
    definitions: Vec<HashMap<Free, DefEntry>>,
    /// Counterfactual match-arm refinements (`refine_head`), kept parallel to `definitions` but suppressible: re-validation of a metavariable solution must keep stable definitions yet ignore these.
    refinements: Vec<HashMap<Free, Term>>,
    refinement_projections: Vec<HashMap<(Term, usize), Term>>,
    /// Counterfactual refinements keyed by a *stuck application* scrutinee — a non-key match head (`classify(c)`, `Nat/in_range(...)`) that `refine_head` could not record. Keyed by a *canonical* form (head verbatim, arguments reduced to WHNF), so an occurrence that surfaces spelled differently still matches the stored key once both are canonicalized. The term-keyed analogue of the two stores above, suppressed by the same flag.
    refinement_scrutinees: Vec<HashMap<Term, Term>>,
    suppress_refinements: bool,
    /// The local assumption context in binding order (a companion to `assumptions`, which is keyed by name and loses order). `assume` appends; frames are delimited by `local_marks`.
    local: Vec<(Free, Term)>,
    local_marks: Vec<usize>,
    /// The `use`-plicity binders currently in scope, in binding order (a subset of `local`), with frame boundaries in `witness_marks` — resolution's step-1/2 search space, scanned innermost-first.
    witness_scope: Vec<(Free, Term)>,
    witness_marks: Vec<usize>,
    /// One tick per mutation of `local` (assume, frame exit, reassume) — an `Entropy` used as a version stamp: `fresh()` bumps, `count()` reads. Invalidates `identity_cache`, which shares the frozen telescope and identity spine between every meta born under an unchanged Γ.
    locals_stamp: Entropy,
    identity_cache: Option<(usize, SharedTelescope, SharedSpine)>,
}

impl Frames {
    pub(crate) fn new() -> Self {
        Self {
            assumptions: vec![HashMap::new()],
            assumption_universes: vec![HashMap::new()],
            definitions: vec![HashMap::new()],
            refinements: vec![HashMap::new()],
            refinement_projections: vec![HashMap::new()],
            refinement_scrutinees: vec![HashMap::new()],
            suppress_refinements: false,
            local: Vec::new(),
            local_marks: Vec::new(),
            witness_scope: Vec::new(),
            witness_marks: Vec::new(),
            locals_stamp: Entropy::new(),
            identity_cache: None,
        }
    }

    pub(crate) fn enter(&mut self) {
        self.assumptions.push(HashMap::new());
        self.assumption_universes.push(HashMap::new());
        self.definitions.push(HashMap::new());
        self.refinements.push(HashMap::new());
        self.refinement_projections.push(HashMap::new());
        self.refinement_scrutinees.push(HashMap::new());
        self.local_marks.push(self.local.len());
        self.witness_marks.push(self.witness_scope.len());
    }

    /// Pop one frame, reporting `(dropped_refinements, dropped_definitions)` so the façade can run the matching cache protocol.
    // Safety: the popped refinement frames are keyed on `Term`, which carries interior scalar caches and so trips Clippy's interior-mutability warning. The logical value is fully immutable, and hashing and equality stay stable across those caches filling.
    #[allow(clippy::mutable_key_type)]
    pub(crate) fn leave(&mut self) -> (bool, bool) {
        self.locals_stamp.fresh();
        self.assumptions.pop().unwrap();
        self.assumption_universes.pop().unwrap();
        let definitions = self.definitions.pop().unwrap();
        let refinements = self.refinements.pop().unwrap();
        let refinement_projections = self.refinement_projections.pop().unwrap();
        let refinement_scrutinees = self.refinement_scrutinees.pop().unwrap();
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
        self.local.push((name.clone(), type_.clone()));

        self.assumptions
            .last_mut()
            .unwrap()
            .insert(name.clone(), type_.clone());
        self.assumption_universes
            .last_mut()
            .unwrap()
            .insert(name.clone(), UniverseContext::empty());
    }

    /// Join `name` to the witness scope (it must already be assumed).
    pub(crate) fn push_witness_binder(&mut self, name: &Free, type_: &Term) {
        self.witness_scope.push((name.clone(), type_.clone()));
    }

    /// The `use`-plicity binders in scope, in binding order (innermost last).
    pub(crate) fn witness_scope(&self) -> &[(Free, Term)] {
        &self.witness_scope
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
        assumptions.insert(name.clone(), type_.clone());
    }

    pub(crate) fn assumption(&self, name: &Free) -> Option<&Term> {
        self.assumptions
            .iter()
            .rev()
            .find_map(|assumptions| assumptions.get(name))
    }

    /// The innermost registered universe context for `name`, if any.
    pub(crate) fn assumption_universe_context(&self, name: &Free) -> Option<UniverseContext> {
        self.assumption_universes
            .iter()
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
        let contexts = self
            .assumption_universes
            .iter_mut()
            .rev()
            .find(|contexts| contexts.contains_key(name))
            .unwrap_or_else(|| panic!("'{name}' has no assumption universe context to replace"));
        #[cfg(feature = "profile")]
        curios_profile::tracing::debug!(
            target: "curios_elab::universe",
            %name,
            params = universe_context.parameter_count,
            was = contexts[name].parameter_count,
            "assumption scheme written",
        );
        contexts.insert(name.clone(), universe_context);
        #[cfg(feature = "profile")]
        {
            let holders = self
                .assumption_universes
                .iter()
                .enumerate()
                .filter(|(_, contexts)| contexts.contains_key(name))
                .map(|(index, contexts)| (index, contexts[name].parameter_count))
                .collect::<Vec<_>>();
            curios_profile::tracing::debug!(
                target: "curios_elab::universe",
                %name,
                frames = self.assumption_universes.len(),
                ?holders,
                "assumption scheme frames",
            );
        }
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

    /// The local assumption context in binding order (outermost first). The dependent-match generalizer (`elaborate_match`) walks this to find the hypotheses whose type depends on a scrutinee index being abstracted: they must ride into the motive as Π-binders, or the synthesized motive is ill-typed. Binding order matters — a hypothesis's type can only mention earlier binders, so the telescope it yields is already well-ordered.
    pub(crate) fn locals(&self) -> &[(Free, Term)] {
        &self.local
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

    /// The reduct of a variable: its definition, or — unless refinements are suppressed — its counterfactual refinement. A name never appears in both stores (definitions name `let`/`rec` binders; refinements name assumed scrutinee heads), so the order between them is immaterial.
    fn raw_var_reduct(&self, name: &Free) -> Option<&Term> {
        if !self.suppress_refinements
            && let Some(term) = self.refinements.iter().rev().find_map(|r| r.get(name))
        {
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
    /// A polymorphic definition's stored body is scoped by its universe context: its parameter levels are not meaningful at an occurrence until elaboration has rebuilt that occurrence as a `UniverseInst`. Letting a raw variable unfold would leak those bound parameters into the ambient solver. The explicit-instance reducer uses [`Frames::var_reduct_at`] after it has the occurrence's level arguments.
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

    /// The reduct of a projection: its counterfactual match-arm refinement, unless refinements are suppressed (re-validation).
    pub(crate) fn proj_reduct(&self, base: &Term, index: usize) -> Option<&Term> {
        if self.suppress_refinements {
            return None;
        }

        let base = project_erased_universes(base);
        self.refinement_projections
            .iter()
            .rev()
            .find_map(|p| p.get(&(base.clone(), index)))
    }

    /// Register a counterfactual match-arm refinement of a variable. Unlike a definition, this lives in a suppressible store so re-validation can ignore it. The façade clears the caches first.
    pub(crate) fn refine(&mut self, name: &Free, term: &Term) {
        self.refinements
            .last_mut()
            .unwrap()
            .insert(name.clone(), term.clone());
    }

    /// Register a counterfactual refinement of a projection (`refine_head` on a `Proj` scrutinee). The façade clears the caches first.
    pub(crate) fn refine_projection(&mut self, base: Term, index: usize, value: Term) {
        self.refinement_projections
            .last_mut()
            .unwrap()
            .insert((project_erased_universes(&base), index), value);
    }

    /// Register a counterfactual refinement of a stuck-application scrutinee (`refine_head` on a non-key head). `canonical` is the canonical form (head verbatim, arguments in WHNF); `value` is the arm's constructor. Sound for the same reason `refine` is — the arm is reached only when the scrutinee equals `value` — and non-cyclic because `value` is a constructor of the scrutinee's inductive, a normal form. The façade clears the caches first.
    pub(crate) fn refine_scrutinee(&mut self, canonical: Term, value: Term) {
        self.refinement_scrutinees
            .last_mut()
            .unwrap()
            .insert(canonical, value);
    }

    /// Whether any scrutinee refinement is registered (regardless of suppression). The cheap outer gate for the reducer probe — skipped on the common refinement-free reduction without hashing anything.
    pub(crate) fn has_scrutinee_refinements(&self) -> bool {
        !self.refinement_scrutinees.iter().all(|f| f.is_empty())
    }

    /// Whether some registered scrutinee key shares `head` as its applied-head symbol. The second gate, past `Term::head_key`: only a head that is actually refined justifies canonicalizing the candidate's arguments.
    pub(crate) fn scrutinee_head_refined(&self, head: HeadTag<'_>) -> bool {
        self.refinement_scrutinees
            .iter()
            .any(|f| f.keys().any(|k| k.head_key() == Some(head)))
    }

    /// The reduct of a canonical stuck scrutinee: its refinement value, unless suppressed (re-validation).
    pub(crate) fn scrutinee_reduct(&self, canonical: &Term) -> Option<&Term> {
        if self.suppress_refinements {
            return None;
        }

        self.refinement_scrutinees
            .iter()
            .rev()
            .find_map(|f| f.get(canonical))
    }

    /// Whether `canonical` is itself a registered scrutinee key — checked *past* suppression. A `Var`/`Proj` key stays neutral under suppression for free (its reduct is withheld, so it does not unfold); an application key would otherwise unfold to its definition body and stop being a key. The reducer consults this to keep such a key neutral while suppressed, so `solve_refinement_free`'s committed (refinement-free) spelling stays a term the live refinement can still fire on.
    pub(crate) fn is_scrutinee_key(&self, canonical: &Term) -> bool {
        self.refinement_scrutinees
            .iter()
            .any(|f| f.contains_key(canonical))
    }

    pub(crate) fn refinements_suppressed(&self) -> bool {
        self.suppress_refinements
    }

    /// Flip the refinement-suppression flag, returning the previous state — the bracket primitive for `Context::with_suppressed_refinements`.
    pub(crate) fn set_refinements_suppressed(&mut self, suppressed: bool) -> bool {
        std::mem::replace(&mut self.suppress_refinements, suppressed)
    }

    /// Whether any counterfactual refinement is currently registered (and not already suppressed) — the gate for the refinement-free candidate re-reduction in `Convert::solve_refinement_free`, so the common refinement-free path pays nothing.
    pub(crate) fn has_refinements(&self) -> bool {
        !self.suppress_refinements && self.any_refinements_registered()
    }

    /// Whether any counterfactual refinement of any kind is registered in any frame, *regardless* of suppression. The cache-contamination gate for `Context::with_suppressed_refinements`: only a registered refinement can make a suppressed reduct differ from the live one. (`has_refinements` is this plus "not already suppressed".)
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

    /// Freeze the live local frame (the way metavariable birth freezes Γ): the base frame persists for the whole elaboration, so only the local frames — which pop before a retry can happen — are captured.
    pub(crate) fn freeze(&self) -> FrozenFrame {
        fn flatten_frames<K: Clone, V: Clone>(frames: &[HashMap<K, V>]) -> Vec<(K, V)> {
            frames
                .iter()
                .skip(1)
                .flat_map(|frame| frame.iter().map(|(k, v)| (k.clone(), v.clone())))
                .collect()
        }

        FrozenFrame {
            // Past `base_locals`, exactly as `identity_snapshot` slices Γ. The whole of `local` would also carry the top-level binders, and `restore_frame` re-`assume`s whatever it is given — which stamps each restored name with an *empty* universe context in the new frame. A polymorphic global would then be shadowed by a monomorphic copy of itself, and instantiating it at its real levels fails the arity check against the wrong scheme.
            assumptions: self.local[self.base_locals()..].to_vec(),
            definitions: flatten_frames(&self.definitions),
            refinements: flatten_frames(&self.refinements),
            refinement_projections: flatten_frames(&self.refinement_projections),
            refinement_scrutinees: flatten_frames(&self.refinement_scrutinees),
            witness_binders: self.witness_scope.clone(),
        }
    }
}
