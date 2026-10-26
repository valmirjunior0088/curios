//! The context's doors to the registries: the declarations, concepts and witnesses a unit registers, what is known of each definition, and what a walk needs of a declaration that has not finished.

use {
    crate::{Answered, Context, Declared, Error, Question, Witness},
    curios_core::{
        ConceptDecl, Definition, Free, Global, HeadKey, InductDecl, ParameterField, StructDecl,
        Term, Totality, UniverseContext, Variance, WitnessKey,
    },
    curios_utilities::{Mount, Qualifier},
    std::{
        collections::{BTreeMap, BTreeSet},
        mem,
    },
};

impl<'s> Context<'s> {
    /// Offer the lemmas of the unit being elaborated to the proofs the elaborator writes from here on: its walk has elaborated the prover's vocabulary, with every declaration it reads.
    pub fn offer_lemmas(&mut self) {
        self.underway.offering = true;
    }

    /// Whether the unit's lemmas are on offer: what a trace's question about them is asked again by.
    pub fn offers_lemmas(&self) -> bool {
        self.underway.offering
    }

    /// Where `name` is written among the unit's items, where a declaration of the unit declares it.
    pub fn written_at(&self, name: &Global) -> Option<usize> {
        self.underway.declarations.item(name)
    }

    /// Whether the attempt under way has read nothing unfinished so far.
    pub fn needs_nothing(&self) -> bool {
        self.underway.needs.nothing()
    }

    /// Record that the attempt under way read the declaration declaring `name` before it finished. The attempt is void whatever it goes on to conclude, and is made again once that declaration has elaborated: the record is what voids it, so no site that turns an error into a decision can make a void attempt count.
    pub fn need(&self, name: Global) {
        self.underway.needs.need(name);
    }

    /// What the attempt under way needed, taken: empty for an attempt that read nothing unfinished, the one whose outcome stands.
    pub fn take_needs(&mut self) -> BTreeSet<Global> {
        self.underway.needs.take()
    }

    /// [`Needs::missed`](super::Needs::missed).
    pub(super) fn missed(&self, name: &Free) {
        self.underway.needs.missed(name);
    }

    /// [`Needs::reads`](super::Needs::reads).
    fn reads_entry(&self, name: &Global) {
        self.underway.needs.reads(name);
    }

    /// Whether a proof the elaborator writes for the declaration being elaborated may apply `name`. A name another unit declares is applied wherever it is in scope. A name this unit declares is one of the prover's vocabulary, which the unit's walk elaborates before anything else, with every declaration it reads, and offers only from then on: so the vocabulary and what it reads are proved without it, every other declaration finds all of it elaborated, and what a declaration's proof is built from follows what the vocabulary reads, never where either is written or which of them was asked for first. A lemma that is on offer and not in scope was refused, and is applied by nothing.
    ///
    /// **A lemma of the unit is read only where the unit's lemmas are on offer.** Before that the answer is no whatever the lemma is, so what is noted is that none was on offer and nothing of the lemma: a trace asked again then waits for no lemma its elaboration did not read.
    pub fn proof_may_apply(&self, name: &Global) -> bool {
        if self.underway.declarations.item(name).is_some() {
            self.typing
                .traced
                .note(Question::Lemmas, Answered::Whether(self.underway.offering));
            if !self.underway.offering {
                return false;
            }
        }
        self.asks(&Free::Global(*name), Question::Signature);

        self.typing
            .frames
            .assumption(&Free::Global(*name))
            .is_some()
    }

    /// Name the declarations a lowering could not read. Their dependents are withheld from elaboration before the first item is checked, so a parse failure in one declaration reports as that declaration's and nothing else's.
    pub fn set_broken(&mut self, names: BTreeSet<Global>) {
        self.underway.broken = names;
    }

    pub fn take_broken(&mut self) -> BTreeSet<Global> {
        mem::take(&mut self.underway.broken)
    }

    pub fn record_definition_totality(&mut self, name: &Global, totality: Totality) {
        self.typing
            .program
            .record_definition_totality(name, totality);
    }

    pub fn definition_totality(&self, name: &Global) -> Option<Totality> {
        self.asks(&Free::Global(*name), Question::Totality);
        self.typing.program.definition_totality(name)
    }

    /// [`Program::record_variances`](crate::Program::record_variances).
    pub(crate) fn record_definition_variances(&mut self, definition: &Definition) {
        self.typing.program.record_variances(definition);
    }

    /// The sort `name`'s value inhabits, where it is a definition whose value is a type ([`curios_core::Definition::value_sort`]): a read of that part of the definition alone.
    pub fn definition_value_sort(&self, name: &Global) -> Option<Term> {
        self.asks(&Free::Global(*name), Question::Sort);
        self.typing.program.definition_value_sort(name).cloned()
    }

    pub fn definition_variances(&self, name: &Global) -> Option<(&[Variance], usize)> {
        self.asks(&Free::Global(*name), Question::Variances);
        self.typing.program.definition_variances(name)
    }

    /// The vector of field `slot` of a record-valued definition, with its arity: a read of the definition's vectors, as [`Self::definition_variances`] is.
    pub fn field_variances(&self, name: &Global, slot: usize) -> Option<(&[Variance], usize)> {
        self.asks(&Free::Global(*name), Question::Variances);
        self.typing.program.field_variances(name, slot)
    }

    /// The field of one of its binders a definition's value is, with its arity: a read of the definition's vectors, as [`Self::definition_variances`] is.
    pub fn parameter_field(&self, name: &Global) -> Option<(ParameterField, usize)> {
        self.asks(&Free::Global(*name), Question::Variances);
        self.typing.program.parameter_field(name)
    }

    pub fn register_induct(&mut self, name: &Global, induct_decl: InductDecl) -> Result<(), Error> {
        self.typing.program.register_induct(name, induct_decl)
    }

    pub fn update_induct(&mut self, name: &Global, induct_decl: InductDecl) {
        self.typing.program.update_induct(name, induct_decl);
    }

    /// Take a refused declaration's inductive entry out of the registry, if it had one.
    pub fn remove_induct(&mut self, name: &Global) {
        self.typing.program.remove_induct(name);
        self.typing.caches.note_write();
    }

    /// Take a refused declaration's struct entry out of the registry, if it had one.
    pub fn remove_struct(&mut self, name: &Global) {
        self.typing.program.remove_struct(name);
        self.typing.caches.note_write();
    }

    /// Take a refused declaration's concept entry out of the registry, if it had one.
    pub fn remove_concept(&mut self, name: &Global) {
        self.typing.program.remove_concept(name);
        self.typing.caches.note_write();
    }

    pub fn induct_decl(&self, name: &Global) -> Option<&InductDecl> {
        self.asks(&Free::Global(*name), Question::Induct);
        self.reads_entry(name);
        self.typing.program.induct_decl(name)
    }

    pub fn register_struct(&mut self, name: &Global, struct_decl: StructDecl) -> Result<(), Error> {
        self.typing.program.register_struct(name, struct_decl)
    }

    pub fn update_struct(&mut self, name: &Global, struct_decl: StructDecl) {
        self.typing.program.update_struct(name, struct_decl);
    }

    pub fn struct_decl(&self, name: &Global) -> Option<&StructDecl> {
        self.asks(&Free::Global(*name), Question::Struct);
        self.reads_entry(name);
        self.typing.program.struct_decl(name)
    }

    pub fn register_concept(&mut self, name: &Global, concept: ConceptDecl) -> Result<(), Error> {
        self.typing.program.register_concept(name, concept)
    }

    pub fn concept(&self, name: &Global) -> Option<&ConceptDecl> {
        self.asks(&Free::Global(*name), Question::Concept);
        self.reads_entry(name);
        self.typing.program.concept(name)
    }

    /// The concept registered under `name`, the unit's own or a unit's in scope, read as [`Context::concepts`] reads the unit's table: no read is recorded. It is what the registry's validation follows a superclass edge by, which decides nothing of the declaration being elaborated and so is no need of it.
    pub fn registered_concept(&self, name: &Global) -> Option<&ConceptDecl> {
        self.typing.program.concept(name)
    }

    pub fn update_concept(&mut self, name: &Global, concept: ConceptDecl) {
        self.typing.program.update_concept(name, concept);
    }

    pub fn concepts(&self) -> &BTreeMap<Global, ConceptDecl> {
        self.typing.program.concepts()
    }

    /// Record the prefixes one seeded module's unit claims. See [`Program::mount`](crate::Program::mount).
    pub(crate) fn mount(&mut self, mounts: &[Mount]) {
        self.typing.program.mount(mounts);
    }

    pub fn mount_of(&self, name: &Global) -> Option<&Mount> {
        self.typing.program.mount_of(name)
    }

    pub fn mount_of_head(&self, head: &HeadKey) -> Option<&Mount> {
        self.typing.program.mount_of_head(head)
    }

    pub fn mark_witness_declaration(&mut self, name: &Global) {
        self.underway.witnesses.mark(name);
    }

    pub fn is_witness_declaration(&self, name: &Global) -> bool {
        self.underway.witnesses.is_marked(name)
    }

    pub fn witness(&self, concept: &Global, key: &WitnessKey) -> Option<Witness> {
        let found = self.typing.program.witness(concept, key);
        let question = Question::Witness(*concept, *key);
        // Asked after the answer is in hand, and so under a second lookup only the first time a key is asked.
        if !self.typing.traced.noted(question) {
            let name = found.as_ref().map(|witness| witness.name);
            self.typing.traced.note(question, Answered::Witness(name));
        }

        found
    }

    /// [`DeclaredWitnesses::declare`](super::DeclaredWitnesses::declare).
    pub(crate) fn declare_witness(&mut self, name: Global, declared: Declared) {
        self.underway.witnesses.declare(name, declared);
    }

    /// [`DeclaredWitnesses::spelled`](super::DeclaredWitnesses::spelled).
    pub(crate) fn spelled_witness(&self, name: &Global) -> Option<Option<&(Global, WitnessKey)>> {
        self.underway.witnesses.spelled(name)
    }

    /// Take the declared witness `name` out of what a question is answered by, poisoning the key it was spelled at: it was refused or withheld, and a goal keyed there is its dependent whether or not it had registered.
    pub fn withdraw_declared_witness(&mut self, name: &Global) {
        if let Some((concept, key)) = self.underway.witnesses.settle(name) {
            self.underway.witnesses.poison(concept, key);
        }
    }

    /// Whether a witness stands under `(concept, key)`: one registered, or one the unit declares there and has not elaborated. A question of membership, answered the same whichever witnesses have elaborated.
    pub fn witness_declared(&self, concept: &Global, key: &WitnessKey) -> bool {
        let declared = self.witness_stands(concept, key);
        self.typing.traced.note(
            Question::WitnessDeclared(*concept, *key),
            Answered::Whether(declared),
        );

        declared
    }

    /// What [`Context::witness_declared`] answers, noted in no trace: asked by the unit's walk, where it asks a trace's question again.
    pub fn witness_stands(&self, concept: &Global, key: &WitnessKey) -> bool {
        self.typing.program.witness_registered(concept, key)
            || self.underway.witnesses.at(concept, key).is_some()
    }

    /// The witness [`Context::witness`] finds at `(concept, key)`, by its name and noted in no trace: asked by the unit's walk, where it asks a trace's question again.
    pub fn witness_standing(&self, concept: &Global, key: &WitnessKey) -> Option<Global> {
        self.typing.program.witness_name(concept, key)
    }

    /// The witness the unit declares at `(concept, key)` and has not finished: the one that registers there once it elaborates ([`DeclaredWitnesses::at`](super::DeclaredWitnesses::at)).
    pub(crate) fn witness_pending(&self, concept: &Global, key: &WitnessKey) -> Option<Global> {
        self.underway.witnesses.at(concept, key)
    }

    /// Whether the table's miss at `(concept, key)` is a witness the unit declares there and has not elaborated: the attempt needs it, and is void. `false` is a miss no other declaration answers — the one being attempted registers on its signature, so a miss on its own key is its signature asking for itself.
    pub fn witness_unfinished(&self, concept: &Global, key: &WitnessKey) -> bool {
        match self.underway.witnesses.at(concept, key) {
            Some(name) if !self.underway.needs.attempts(&name) => {
                self.need(name);
                true
            }
            _ => false,
        }
    }

    /// Every key a witness of `concept` stands under, with the module declaring it: the registered ones, then the ones the unit declares and has not elaborated — the raw material for reachability questions over one concept's edges (the missing-embedding chain report), the same whichever witnesses have elaborated.
    pub fn witness_keys(&self, concept: &Global) -> Vec<(WitnessKey, Qualifier)> {
        let registered = self.typing.program.witness_keys(concept).into_iter();
        let declared = self
            .underway
            .witnesses
            .keys(concept)
            .filter(|(key, _)| !self.typing.program.witness_registered(concept, key))
            .map(|(key, module)| (*key, *module));

        registered.chain(declared).collect()
    }

    /// [`Program::insert_witness`](crate::Program::insert_witness), stamping the write on an actual insert — a new witness can change which pure elaborations succeed.
    pub(crate) fn insert_witness(
        &mut self,
        concept: Global,
        key: WitnessKey,
        witness: Witness,
    ) -> Option<(Global, Qualifier)> {
        let existing = self.typing.program.insert_witness(concept, key, witness);
        if existing.is_none() {
            self.typing.caches.note_write();
        }
        existing
    }

    /// [`Program::remove_witness`](crate::Program::remove_witness), stamping the write: a witness gone can change which pure elaborations succeed as much as one arriving.
    pub(crate) fn remove_witness(&mut self, name: &Global) -> Vec<(Global, WitnessKey)> {
        let keys = self.typing.program.remove_witness(name);
        if !keys.is_empty() {
            self.typing.caches.note_write();
        }
        keys
    }

    /// Mark `(concept, key)` as a slot a refused declaration's witness stood in — see [`DeclaredWitnesses::poison`](super::DeclaredWitnesses::poison).
    pub(crate) fn poison_witness_key(&mut self, concept: Global, key: WitnessKey) {
        self.underway.witnesses.poison(concept, key);
    }

    pub fn is_poisoned_witness(&self, concept: &Global, key: &WitnessKey) -> bool {
        self.underway.witnesses.is_poisoned(concept, key)
    }

    /// [`Program::update_witness_scheme`](crate::Program::update_witness_scheme), stamping the write.
    pub(crate) fn update_witness_scheme(
        &mut self,
        name: &Global,
        universe_context: UniverseContext,
        signature: Term,
    ) {
        self.typing
            .program
            .update_witness_scheme(name, universe_context, signature);
        self.typing.caches.note_write();
    }
}
