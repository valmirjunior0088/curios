//! The program-wide declaration stores: flat, monotonic facts about the module being elaborated.
//!
//! Everything here is registry state — inductive, struct, and concept declarations, the witness table, and per-definition totality verdicts. None of it is lexically scoped: `enter_frame`/`leave_frame` never touch these stores, and entries are only ever added or refined in place, never popped. The one write that must reach the cache stamps (`insert_witness`, `update_witness_scheme`) is stamped by the `Context` façade, which alone holds the [`Caches`](super::Caches).

use {
    crate::{Error, HeadKey, Witness, WitnessKey},
    curios_core::{ConceptDecl, Global, InductDecl, StructDecl, Term, Totality, UniverseContext},
    curios_utilities::{Mount, Qualifier},
    std::collections::{BTreeMap, BTreeSet},
};

/// The declaration registries and the witness table. `Context` holds exactly one of these.
#[derive(Debug, Default)]
pub(crate) struct Program {
    /// Inductive declarations, keyed by the type's qualified name ("Result").
    induct_decls: BTreeMap<Global, InductDecl>,
    /// Struct declarations, keyed the same way. Consulted by `elaborate_struct`/`elaborate_proj`/`erase`.
    struct_decls: BTreeMap<Global, StructDecl>,
    /// Concept declarations, keyed by the concept's qualified name (`struct_decls` also holds each concept's record entry; this adds the resolution metadata).
    concepts: BTreeMap<Global, ConceptDecl>,
    /// The definition names `into_core` marked as witness declarations; each registers into `witness_table` when its signature elaborates (`elaborate_module_let` → `register_witness`).
    witness_declarations: BTreeSet<Global>,
    /// The program-wide witness table: one witness per (concept, parameter-head tuple) key — global coherence, checked at registration.
    witness_table: BTreeMap<(Global, WitnessKey), Witness>,
    /// Each definition's totality, recorded as it is defined. The whole-module pass recomputes these post-zonk; this copy exists so a type position can be refused *before* it is reduced, which is the only point at which a non-productive type-level loop can still be diagnosed rather than run.
    totality: BTreeMap<Global, Totality>,
    /// Every prefix this compilation mounts — the scope's, then the unit's own. Accumulated as each module is seeded, from the `mounts` that module carries, so the elaborator answers a privilege question out of what was actually mounted rather than out of a stamp copied onto each declaration.
    mounts: Vec<Mount>,
}

impl Program {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    /// Record a new inductive declaration's metadata. Called once per `induct` declaration as a module is seeded into the context. Errs with `DuplicateInduct` (leaving the existing entry untouched) if `name` is already registered — the registry is shared across every root elaborated into this context, so a collision is rejected rather than silently overwriting a prior root's declaration. Mid-elaboration rebuilds of an already-registered entry go through [`Program::update_induct`] instead.
    pub(crate) fn register_induct(
        &mut self,
        name: &Global,
        induct_decl: InductDecl,
    ) -> Result<(), Error> {
        if self.induct_decls.contains_key(name) {
            return Err(Error::duplicate_induct(name.symbol()));
        }
        self.induct_decls.insert(name.clone(), induct_decl);
        Ok(())
    }

    /// Overwrite an already-registered inductive's metadata with a rebuilt telescope — called mid-elaboration by `elaborate_induct_indices`/ `elaborate_induct_constructors` to refine the same declaration's own entry, not to register a new one, so unlike [`Program::register_induct`] this always overwrites. Panics if `name` has no prior entry — every caller is expected to have checked [`Program::induct_decl`] first (a construction bug otherwise, not a user-facing case).
    pub(crate) fn update_induct(&mut self, name: &Global, induct_decl: InductDecl) {
        assert!(
            self.induct_decls.contains_key(name),
            "update_induct: '{name}' is not already registered"
        );
        self.induct_decls.insert(name.clone(), induct_decl);
    }

    /// Look up an inductive declaration by the type's qualified name.
    pub(crate) fn induct_decl(&self, name: &Global) -> Option<&InductDecl> {
        self.induct_decls.get(name)
    }

    /// Record a new struct declaration's metadata. Called once per `struct` declaration as a module is seeded into the context (elaboration or erasure). Errs with `DuplicateStruct` (leaving the existing entry untouched) if `name` is already registered — the registry is shared across every root elaborated into this context, so a collision is rejected rather than silently overwriting a prior root's declaration. Mid-elaboration rebuilds of an already-registered entry go through [`Program::update_struct`] instead.
    pub(crate) fn register_struct(
        &mut self,
        name: &Global,
        struct_decl: StructDecl,
    ) -> Result<(), Error> {
        if self.struct_decls.contains_key(name) {
            return Err(Error::duplicate_struct(name.symbol()));
        }
        self.struct_decls.insert(name.clone(), struct_decl);
        Ok(())
    }

    /// Overwrite an already-registered struct's metadata with rebuilt field types — called mid-elaboration by `elaborate_struct` to refine the same declaration's own entry, not to register a new one, so unlike [`Program::register_struct`] this always overwrites. Panics if `name` has no prior entry — every caller is expected to have checked [`Program::struct_decl`] first (a construction bug otherwise, not a user-facing case).
    pub(crate) fn update_struct(&mut self, name: &Global, struct_decl: StructDecl) {
        assert!(
            self.struct_decls.contains_key(name),
            "update_struct: '{name}' is not already registered"
        );
        #[cfg(feature = "profile")]
        curios_profile::tracing::debug!(
            target: "curios_elab::universe",
            %name,
            params = struct_decl.universe_context.parameter_count,
            was = self.struct_decls[name].universe_context.parameter_count,
            "struct scheme rewritten",
        );
        self.struct_decls.insert(name.clone(), struct_decl);
    }

    /// Look up a struct declaration by the type's qualified name.
    pub(crate) fn struct_decl(&self, name: &Global) -> Option<&StructDecl> {
        self.struct_decls.get(name)
    }

    /// Record a new concept declaration's resolution metadata (its record shape is registered separately, as an ordinary structure). Called once per `concept` declaration when a module's registries are seeded. Errs with `DuplicateConcept` (leaving the existing entry untouched) if `name` is already registered — the registry is shared across every root elaborated into this context, so a collision is rejected rather than silently overwriting a prior root's declaration.
    pub(crate) fn register_concept(
        &mut self,
        name: &Global,
        concept: ConceptDecl,
    ) -> Result<(), Error> {
        if self.concepts.contains_key(name) {
            return Err(Error::duplicate_concept(name.symbol()));
        }
        self.concepts.insert(name.clone(), concept);
        Ok(())
    }

    /// Look up a concept by its qualified name.
    pub(crate) fn concept(&self, name: &Global) -> Option<&ConceptDecl> {
        self.concepts.get(name)
    }

    pub(crate) fn update_concept(&mut self, name: &Global, concept: ConceptDecl) {
        assert!(
            self.concepts.contains_key(name),
            "update_concept: '{name}' is not already registered"
        );
        self.concepts.insert(name.clone(), concept);
    }

    /// The registered concepts, for whole-registry validation (superclass acyclicity) at seed time.
    pub(crate) fn concepts(&self) -> &BTreeMap<Global, ConceptDecl> {
        &self.concepts
    }

    /// Record the prefixes one seeded module's unit claims. Called once per module as it enters the context — the scope's, then the unit's own.
    pub(crate) fn mount(&mut self, mounts: &[Mount]) {
        for mount in mounts {
            if !self.mounts.contains(mount) {
                self.mounts.push(mount.clone());
            }
        }
    }

    /// The mount owning `name`, or `None` for a name no mounted prefix claims — an anonymous witness above all, whose declaring module is carried separately.
    pub(crate) fn mount_of(&self, name: &Global) -> Option<&Mount> {
        match name {
            Global::Authored(qualifier) => Mount::owning(&self.mounts, qualifier),
            Global::Witness(_) => None,
        }
    }

    /// The mount owning one witness key's rigid head, or `None` for an intrinsic head, which is never user-declarable, and for a tuple shape or a function type's plicity vector, which no module declares. Consulted by the orphan-rule check in `register_witness`.
    ///
    /// A tuple or function key therefore contributes nothing to ownership, and the orphan rule reads, for one: such a witness is declared where its concept is declared, or by a privileged root. That is the standing an intrinsic former already has, and it is what makes the standard library's arity ceiling a promise — two independent packages each declaring `Show({Nat, Nat})` would collide at link with neither in the wrong. It bites harder for a function key, whose useful key space is nearly one point (`(_) -> _` above all): a concept's owner claiming a shape claims it program-wide, which is why `/std` declines `Monad` at a function shape deliberately rather than leaving the slot open.
    ///
    /// `None` is not "unknown" — it is "claimed by no authored mount", which no declaring mount can equal. That is exactly what the previous `RootId::Sys` answer achieved: an ordinary consumer never *matched* it, it only ever failed to, so the verdict is unchanged and the answer no longer names a root it was standing in for.
    pub(crate) fn mount_of_head(&self, head: &HeadKey) -> Option<&Mount> {
        match head {
            HeadKey::Nominal(name) => self.mount_of(name),
            _ => None,
        }
    }

    /// Mark a definition name as a witness declaration; when its signature elaborates, `elaborate_module_suffix` registers it into the witness table.
    pub(crate) fn mark_witness_declaration(&mut self, name: &Global) {
        self.witness_declarations.insert(name.clone());
    }

    pub(crate) fn is_witness_declaration(&self, name: &Global) -> bool {
        self.witness_declarations.contains(name)
    }

    /// The witness registered under `(concept, key)`, if any.
    pub(crate) fn witness(&self, concept: &Global, key: &WitnessKey) -> Option<&Witness> {
        self.witness_table.get(&(concept.clone(), key.clone()))
    }

    /// Every registered witness with the concept it witnesses — the display path's raw material for folding operator projections back to infix.
    pub(crate) fn witness_entries(&self) -> impl Iterator<Item = (&Global, &Witness)> {
        self.witness_table
            .iter()
            .map(|((concept, _), witness)| (concept, witness))
    }

    /// [`Self::witness_entries`] with the keys kept — the raw material for reachability questions over one concept's edges (the missing-embedding chain report).
    pub(crate) fn witness_keyed_entries(
        &self,
    ) -> impl Iterator<Item = (&Global, &WitnessKey, &Witness)> {
        self.witness_table
            .iter()
            .map(|((concept, key), witness)| (concept, key, witness))
    }

    /// Insert a witness under its key, returning the previous occupant's declaring module on a collision (the caller reports `DuplicateWitness`, which reports modules rather than the anonymous witnesses' compiler-minted names). The façade stamps the write on an actual insert.
    pub(crate) fn insert_witness(
        &mut self,
        concept: Global,
        key: WitnessKey,
        witness: Witness,
    ) -> Option<Qualifier> {
        match self.witness_table.get(&(concept.clone(), key.clone())) {
            Some(existing) => Some(existing.module.clone()),
            None => {
                self.witness_table.insert((concept, key), witness);
                None
            }
        }
    }

    /// Rewrite a registered witness's generalized scheme once its signature finalizes. Panics if `name` was never registered. The façade stamps the write.
    pub(crate) fn update_witness_scheme(
        &mut self,
        name: &Global,
        universe_context: UniverseContext,
        signature: Term,
    ) {
        let witness = self
            .witness_table
            .values_mut()
            .find(|witness| witness.name == *name)
            .unwrap_or_else(|| panic!("witness '{name}' was not registered"));
        witness.universe_context = universe_context;
        witness.signature = signature;
    }

    /// Record a definition's totality the moment it is defined.
    ///
    /// The whole-module pass computes the same verdicts by a fixpoint, and needs one because it sees every item at once. Here the items arrive in dependency order, so everything a definition mentions is already classified and one pass per definition suffices.
    pub(crate) fn record_definition_totality(&mut self, name: &Global, totality: Totality) {
        self.totality.insert(name.clone(), totality);
    }

    /// A definition's recorded totality, or `None` for a name not yet classified — a member of the group currently elaborating, which `group_totality` settles for the group as a whole.
    pub(crate) fn definition_totality(&self, name: &Global) -> Option<Totality> {
        self.totality.get(name).copied()
    }

    /// Seed the recorded verdicts with a replayed prefix's, which its own archive settled.
    pub(crate) fn seed_totality(&mut self, inherited: &BTreeMap<Global, Totality>) {
        self.totality.extend(
            inherited
                .iter()
                .map(|(name, totality)| (name.clone(), *totality)),
        );
    }
}
