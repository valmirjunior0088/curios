//! The context's doors to the frames: binders entered and left, arms opened and ruled out, what is assumed and defined in scope, and the refinements an arm installs.

#[cfg(test)]
mod test_support;

use {
    super::{Bracket, ByValue, DefEntry, KeptWindow},
    crate::{
        Context, DeadArm, HeldFrames, Question, RegionOwner, ScrutineeEntry, hold_at_one_instance,
        key_spelling, shallow_scrutinee, zonk_solved_term_metas,
    },
    curios_analysis::{reads_solved, records_case_equation},
    curios_core::{
        Advance, DefinitionKind, Free, Global, HeadTag, RecGroup, Subterm, Term,
        project_erased_universes,
    },
    curios_utilities::Plicity,
    std::rc::Rc,
};

impl<'s> Context<'s> {
    /// Mint a binder nothing else can name, rendering as `hint`.
    ///
    /// The counter starts above every index the declaration's lowering minted ([`Context::seed`]), so a lowered binder and an elaborated one can never be the same identity.
    pub(crate) fn fresh(&mut self, hint: Option<&str>) -> Free {
        self.fresh_for(hint, None)
    }

    /// [`Context::fresh`] for a local opened from a scope's binder, recording where the lowering wrote that binder among its declaration's written binders: so a proof that reads the local credits the binder a lint names ([`Context::credit`]).
    pub(crate) fn fresh_for(&mut self, hint: Option<&str>, written: Option<u32>) -> Free {
        let local = Free::local(self.typing.fresh_names.fresh(), hint);
        if let Some(written) = written {
            self.underway
                .credits
                .open(local, self.underway.declaration, written);
        }
        local
    }

    /// [`Context::fresh`] for a binder a scope is closed over before anything reads it, as the binder of a `let` the matrix compiler writes ahead of a row's body is: the local remembers where the lowering wrote its binder, as a lowered one does, so the scope closed over it does, and the local opened from that scope is the one a proof credits ([`Context::fresh_for`]).
    pub(crate) fn fresh_written(&mut self, hint: Option<&str>, written: Option<u32>) -> Free {
        let index = self.typing.fresh_names.fresh();

        match written {
            Some(written) => Free::local_written(index, hint, written),
            None => Free::local(index, hint),
        }
    }

    /// What the region a `!` sequences in here belongs to: a function's body where a frame that stands opened one, and otherwise the value being elaborated.
    pub(crate) fn region_owner(&self) -> RegionOwner {
        match (self.typing.frames.in_body(), self.underway.defining) {
            (true, _) => RegionOwner::Function,
            (false, true) => RegionOwner::Definition,
            (false, false) => RegionOwner::Declaration,
        }
    }

    pub(super) fn enter_frame(&mut self) {
        self.typing.frames.enter();
    }

    pub(super) fn leave_frame(&mut self) {
        let (dropped_refinements, dropped_definitions) = self.typing.frames.leave();
        self.typing
            .caches
            .invalidate_frame_exit(dropped_refinements, dropped_definitions);
    }

    pub(crate) fn with_frame<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.enter_frame();
        let result = f(self);
        self.leave_frame();

        result
    }

    /// Say that the arm whose frame is being elaborated is never taken: its case is of type `case` and its scrutinee of type `scrutinee`, one family at indices that clash. The reason stands for that frame and every frame inside it, and an error raised there is reported with it: by the arm where the error propagates ([`Error::in_dead_arm`](crate::Error::in_dead_arm)), and by the record of an implicit minted there where one nothing solved is reported once the arm has closed.
    pub(crate) fn rule_out_arm(&mut self, case: Term, scrutinee: Term) -> Rc<DeadArm> {
        let dead = Rc::new(DeadArm::Indices { case, scrutinee });
        self.typing.frames.rule_out(Rc::clone(&dead));

        dead
    }

    /// [`Frames::open_arm`](super::Frames::open_arm): say that the frame being elaborated is the arm at `case` of a match on `guard`, so that a bound given up under it is told whether a value reaches the arm.
    pub(crate) fn open_arm(&mut self, guard: &Term, case: &Term) {
        self.typing.frames.open_arm(guard.clone(), case.clone());
    }

    /// [`Frames::open_body`](super::Frames::open_body): say that the frame being elaborated is a function's body, whose result type is the region a `!` there sequences in.
    pub(crate) fn open_body(&mut self) {
        self.typing.frames.open_body();
    }

    /// [`Frames::assume`](super::Frames::assume), stamping the write.
    pub(crate) fn assume(&mut self, name: &Free, type_: &Term) {
        self.typing.caches.note_write();
        self.typing.frames.assume(name, type_);
    }

    /// [`Context::assume`] of each of `bindings` in order, the scope restated once for all of them: an arm's retyped locals.
    pub(crate) fn assume_all(&mut self, bindings: Vec<(Free, Term)>) {
        self.typing.caches.note_write();
        self.typing.frames.assume_all(bindings);
    }

    /// Step `walk` past its next binder: mint one from the entry's hint, assume it at `domain`, and hand it back for the caller's own capture.
    pub(crate) fn advance_assumed(&mut self, walk: &mut impl Advance, domain: &Term) -> Free {
        let binder = walk.advance_fresh(|hint| self.fresh(hint));
        self.assume(&binder, domain);
        binder
    }

    /// Assume `label : type_` as a `use`-plicity binder: an ordinary assumption that additionally joins the witness scope, where resolution finds it (innermost-first).
    pub(crate) fn assume_witness(&mut self, name: &Free, type_: &Term) {
        self.assume(name, type_);
        self.typing.frames.push_witness_binder(name, type_);
    }

    /// Enter a telescope's member into scope under its mark: assumed at its type, and, where the mark is `use`, in the witness scope as well, so resolution in what follows finds it.
    ///
    /// **The one way a walk enters a member it opens**, whatever the telescope — a function type's, a lambda's, a declaration's parameters, a constructor's payload in a signature or in an arm, a structure's fields — so what a mark means where a telescope is opened is said here and at no site.
    pub(crate) fn enter(&mut self, name: &Free, type_: &Term, mark: Plicity) {
        match mark {
            Plicity::Witness => self.assume_witness(name, type_),
            Plicity::Explicit | Plicity::Implicit => self.assume(name, type_),
        }
    }

    /// [`Frames::witness_scope`](super::Frames::witness_scope).
    pub(crate) fn witness_scope(&self) -> impl Iterator<Item = &(Free, Term)> {
        self.typing.frames.witness_scope()
    }

    /// [`Frames::reassume`](super::Frames::reassume), invalidating the elaboration cache — an entry elaborated between a `rec` group's lowered `assume` and this upgrade could embed the lowered signature.
    pub(crate) fn reassume(&mut self, name: &Free, type_: &Term) {
        self.typing.caches.invalidate_for_reassumption();
        self.typing.frames.reassume(name, type_);
    }

    /// The type `name` is assumed at. A miss on a name whose declaration has not finished is recorded as what the attempt needs ([`Context::need`]).
    pub(crate) fn assumption(&self, name: &Free) -> Option<&Term> {
        self.asks(name, Question::Signature);
        let assumed = self.typing.frames.assumption(name);
        if assumed.is_none() {
            self.missed(name);
        }

        assumed
    }

    /// [`Frames::rec_definitions`](super::Frames::rec_definitions): every top-level `rec` group defined so far, with its members' names.
    pub(crate) fn rec_definitions(&self) -> Vec<(RecGroup, Vec<Global>)> {
        self.typing.frames.rec_definitions()
    }

    /// [`Frames::member_names`](super::Frames::member_names): the globals `group`'s members are defined under, in member order, by the definitions reduction opened it from in the declaration in progress ([`Caches::opened_from`](super::Caches::opened_from)), and none where it opened none. A group is its members' bodies, so a judgment told which names one goes by has read the body of each.
    pub(crate) fn member_names(&self, group: &RecGroup) -> Option<Vec<Global>> {
        let opened = self.typing.caches.opened_from(group)?;
        let names = self.typing.frames.member_names(group, opened);
        for name in names.iter().flatten() {
            self.asks(&Free::Global(*name), Question::Body);
        }

        names
    }

    pub(super) fn is_defined(&self, name: &Free) -> bool {
        self.typing.frames.is_defined(name)
    }

    /// [`Frames::local_binders`](super::Frames::local_binders).
    pub(crate) fn local_binders(&self) -> impl Iterator<Item = &(Free, Term)> {
        self.typing.frames.local_binders()
    }

    fn define_entry(&mut self, name: Free, entry: DefEntry) {
        // A *fresh* definition can only unstick reductions that read this name's absence, and a stuck read always leaves the name free in the WHNF (the name analogue of the unsolved-metavariable argument in `Context::reduce`) — so the reduction cache retains every entry whose result does not mention it instead of clearing wholesale. This keeps reducts warm across the definitions one declaration mints, and across erasure's items, which erasure walks under one budget: it re-derives an item right after its `define`, and a cold re-reduction of a deep closed spine (a string literal's scan-state chain) would repeat all of its work.
        //
        // The elaboration cache survives the same fresh definition with no retain at all: its insert gate (`elaboration_cacheable`) already refused every entry naming a not-yet-defined global, so the fresh name appears in no surviving entry. A minted `let` binder — which the frame elaborators mint — is excluded from caching outright (`has_local_free`), and a global is only ever referenced once defined; so not clearing lets a deep spine memoize once across those definitions instead of re-elaborating its shared subterms after each.
        //
        // A *redefinition* voids both arguments — the frame elaborators define under labels that can rebind or shadow, and the old value may sit consumed inside a reduct or an elaboration result that no longer mentions the label — so there both caches clear wholesale.
        if self.typing.frames.is_defined(&name) {
            self.typing.caches.invalidate_for_redefinition();
        } else {
            self.typing.caches.retain_reductions_without(&name);
            self.typing.frames.forget_spellings_naming(&name);
        }

        let entry = match name.is_local() {
            true => {
                let zonked = zonk_solved_term_metas(self, entry.body());
                let by_value = ByValue {
                    term: self.by_value(&zonked),
                    erased: self.erased_by_value(&zonked),
                };
                entry.read_by_value(by_value)
            }
            false => entry,
        };
        self.typing.frames.define(name, entry);
    }

    /// `term` by value: each local definition it names outside a metavariable's occurrence replaced by what that definition is by value, which it was read as where it was defined ([`ByValue`]), and each occurrence kept as it stands, as conversion reads a block's locals ([`read_outside_spines`](crate::read_outside_spines)): a spine is the scope its occurrence was born in, which no reading here changes, so reading into it would cost each spelling the scope. One substitution over what `term` holds, as the kernel reads a term through the values its `let`s were bound to (`curios-cert`'s `Scope::by_value`), where unfolding each definition in turn would read a chain of them again at every spelling.
    pub(crate) fn by_value(&self, term: &Term) -> Term {
        self.typing
            .frames
            .substituted_by_value(term, |by_value| &by_value.term)
    }

    /// [`Context::by_value`] with universe instances erased ([`project_erased_universes`]), over the erased values each definition keeps: what an equation's key over local definitions is spelled from, in what `term` holds.
    pub(crate) fn erased_by_value(&self, term: &Term) -> Term {
        self.typing
            .frames
            .substituted_by_value(&project_erased_universes(term), |by_value| &by_value.erased)
    }

    /// Define `name`. `kind` is the declaring module item's [`DefinitionKind`], or `None` for a local binding no item declared.
    pub(crate) fn define(&mut self, name: &Free, term: &Term, kind: Option<&DefinitionKind>) {
        self.define_entry(*name, DefEntry::new(term.clone(), kind.cloned()));
    }

    /// Define `name` as a published item binds it: [`Context::define`], and where `proof`, to a term no reduction unfolds it to ([`curios_core::Definition::proof`]). Until its item is published a proof unfolds as any definition does, which is what its own check reads it by.
    pub(super) fn define_published(
        &mut self,
        name: &Free,
        term: &Term,
        kind: &DefinitionKind,
        proof: bool,
    ) {
        self.define_entry(
            *name,
            DefEntry::published(term.clone(), kind.clone(), proof),
        );
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

    /// Undo a top-level declaration's bindings ([`Frames::forget`](super::Frames::forget)) — a refused item's, so the scope holds what elaborated and nothing of what did not. The caches clear as for a redefinition: an entry may have consumed the binding.
    pub(crate) fn forget(&mut self, name: &Free) {
        self.typing.frames.forget(name);
        self.typing.caches.invalidate_for_redefinition();
    }

    /// [`Frames::refine`](super::Frames::refine), with the refinement cache protocol — the variable now reduces differently — with every equation that names the variable restated under its value ([`Context::restate_equations`]), and with every equation the refinement closes withheld ([`Context::withhold_closed_equations`]).
    pub(crate) fn refine(&mut self, name: &Free, term: &Term) {
        self.typing.caches.invalidate_for_refinement();
        self.typing.frames.refine(name, term);
        self.restate_equations(name, term);
        self.withhold_closed_equations();
    }

    /// Restate, for as long as the current frame stands, every equation in force whose scrutinee names `name`, under the value an arm has just refined `name` to: the equation is withheld, and its instance at `value` recorded in its place.
    ///
    /// **The kernel's `Scope::restate`, in the elaborator's store.** The kernel reads the arm it checks with a case's solution substituted and assumes each equation in force again at that instance, the recorded one stepping aside. The elaborator keeps the variable spelled and refines it, so an equation recorded outside the arm still names it, and a fact written at the case value — `0 < List/len(xs)` under `match i < List/len(xs)` and then `match i | 0` — meets no key: the entry's reduced spelling is settled with the arm's frame withheld, and names the variable still. The instance is the same hypothesis read where the arm reads it, since what holds of the scrutinee at every value of the variable holds at the one the case fixes.
    ///
    /// **The recorded equation steps aside, as the kernel's does**, so an equation stands once however deep the arms go: kept beside its instance it would double at every nested match on a variable it names. A term in the arm that still names the variable meets the instance because a key and a probe are spelled as the arm spells them ([`Context::spelled_as_refined`]).
    ///
    /// **An equation whose settlement would read the refinement is restated too**, though its scrutinee does not name the variable: one naming a local whose type, or whose definition, names it, read through to the end (`curios_analysis::reads_solved`, the rule the kernel restates by). The arm re-types such a local, and a settlement reads it at its type, so a spelling settled under the arm would rest on the arm while the equation outlives it. Its instance at `value` is its own spelling, so it is registered under its own key in the arm's frame, standing in for the recorded entry there ([`ScrutineeEntry::restates`]) rather than withholding it under a key it would share.
    ///
    /// An instance that names no local is recorded by neither checker: [`Context::withhold_closed_equations`], which a refinement runs next, withholds it as it withholds any equation the refinement closes. The value an equation assumes is left as it is, a variable in it being read through its refinement.
    fn restate_equations(&mut self, name: &Free, value: &Term) {
        if !self.typing.frames.has_scrutinee_refinements() {
            return;
        }
        let solution = [(*name, value.clone())];
        // Each read with its solved metavariables materialized: a metavariable stands over a spine of every local in scope where it was born, and would read as naming the variable.
        let reads = |local: &Free| -> Vec<Term> {
            if !curios_analysis::Env::is_local(self, local) {
                return Vec::new();
            }
            self.assumption(local)
                .into_iter()
                .chain(self.definition_body(local))
                .map(|read| zonk_solved_term_metas(self, read))
                .collect()
        };
        let named = self
            .typing
            .frames
            .scrutinee_equations()
            .into_iter()
            .filter_map(|(key, entry)| {
                let original = zonk_solved_term_metas(self, &entry.original);
                reads_solved(&original, &solution, reads).then_some((key, entry, original))
            })
            .collect::<Vec<_>>();

        for (key, entry, original) in named {
            let original = original.substitute(&solution);
            let restated_key = shallow_scrutinee(self, &original);
            let restates = restated_key == key;
            let restated = ScrutineeEntry {
                original,
                value: entry.value.clone(),
                alias: entry.alias,
                withheld: false,
                restates,
            };
            if !restates {
                self.typing.frames.withhold_scrutinee(key, entry);
            }
            self.typing.frames.refine_scrutinee(restated_key, restated);
        }
    }

    /// [`Frames::spelled_as_refined`](super::Frames::spelled_as_refined).
    pub(crate) fn spelled_as_refined(&self, term: &Term) -> Term {
        self.typing.frames.spelled_as_refined(term)
    }

    /// `term` as the kernel is handed it under the arms in force: its solved metavariables materialized, its local definitions substituted by value ([`Context::by_value`]), and every variable an arm refined spelled as the value it was refined to. The kernel reads the arm it checks with a case's solution substituted, where the elaborator records it and keeps the variable spelled, so this is the reading a rule shared with the kernel is judged on.
    ///
    /// A solved metavariable is materialized first because it stands over a spine of the locals in scope where it was born, and would read as naming them: a guard written through a concept holds its witness as one, and names no local of its own once the witness is the global it resolved to. A definition's value was materialized where it was defined, and is again here for what has been solved since. An unsolved one is kept as it stands, its spine the scope it was born in: the kernel is handed none.
    pub(crate) fn kernel_spelling(&self, term: &Term) -> Term {
        let by_value = self.by_value(&zonk_solved_term_metas(self, term));
        self.spelled_as_refined(&zonk_solved_term_metas(self, &by_value))
    }

    /// Withhold, for as long as the current frame stands, every equation in force whose scrutinee names no local as the kernel spells it now ([`Context::kernel_spelling`]).
    ///
    /// **The recording rule, read where the kernel reads it.** No checker records an equation under a spelling that mentions no local (`curios_analysis::records_case_equation`). The kernel restates the equations in force under an arm's solution, so a guard over a variable an arm inside refines to a closed value is, to the kernel, an equation about a closed term, which it does not record; the elaborator's key still names the variable, and left answering it would accept in that arm a proof the kernel refuses. A closed scrutinee computes, so nothing is lost where the arm is one a value reaches: the fact holds by reduction. Where it computes to another case than its guard assumed the arm is dead, and a proof resting on the guard is refused where it is written.
    fn withhold_closed_equations(&mut self) {
        if !self.typing.frames.has_scrutinee_refinements() {
            return;
        }
        let closed = self
            .typing
            .frames
            .scrutinee_equations()
            .into_iter()
            .filter(|(_, entry)| !records_case_equation(&self.kernel_spelling(&entry.original)))
            .collect::<Vec<_>>();
        for (key, entry) in closed {
            self.typing.frames.withhold_scrutinee(key, entry);
        }
    }

    /// [`Frames::refine_projection`](super::Frames::refine_projection), with the refinement cache protocol.
    pub(crate) fn refine_projection(&mut self, base: Term, index: usize, value: Term) {
        self.typing.caches.invalidate_for_refinement();
        self.typing.frames.refine_projection(base, index, value);
    }

    /// [`Frames::definition_body`](super::Frames::definition_body), and a need where the body is missing because its declaration has not finished ([`Context::missed`]): a reduction that asks for one would otherwise go stuck on a name that only has no body yet, and the attempt would be judged on a term it could have reduced.
    pub(crate) fn definition_body(&self, name: &Free) -> Option<&Term> {
        self.asks(name, Question::Body);
        let body = self.typing.frames.definition_body(name);
        match body {
            Some(body) => self.opens(name, body),
            None => self.absent(name),
        }

        self.unless_member(body)
    }

    /// `body`, unless it is the protected slot of a member of a group being checked: a member named in a body stands for nothing there, as the kernel checks a group's bodies over members that stand for nothing, and a signature reads it through its slot instead (`elaborate_rec`).
    fn unless_member<'a>(&self, body: Option<&'a Term>) -> Option<&'a Term> {
        body.filter(|body| !matches!(&***body, Subterm::Metavar(slot) if self.is_rec_slot(slot.id)))
    }

    /// [`Frames::var_reduct`](super::Frames::var_reduct), and a need as [`Context::definition_body`] records one.
    pub(crate) fn var_reduct(&self, name: &Free) -> Option<&Term> {
        self.asks(name, Question::Body);
        let reduct = self.typing.frames.var_reduct(name);
        match reduct {
            Some(reduct) => self.opens(name, reduct),
            None => self.absent(name),
        }

        self.unless_member(reduct)
    }

    /// `name` has no definition to unfold to: a need where its declaration has not finished ([`Context::missed`]), and a name a fresh definition can unstick a reduct on ([`Caches::note_absent`](super::Caches::note_absent)).
    fn absent(&self, name: &Free) {
        self.missed(name);
        self.typing.caches.note_absent(name);
    }

    /// `name` unfolded to `reduct`: where it is a member of a top-level group, the group is filed as opened from it ([`Caches::note_opened`](super::Caches::note_opened)).
    fn opens(&self, name: &Free, reduct: &Term) {
        if let (Free::Global(global), Some((group, _))) = (name, reduct.as_rec_proj()) {
            self.typing.caches.note_opened(group, *global);
        }
    }

    /// [`Frames::var_reduct_at`](super::Frames::var_reduct_at), and a need as [`Context::definition_body`] records one.
    pub(crate) fn var_reduct_at(&self, name: &Free) -> Option<&Term> {
        self.asks(name, Question::Body);
        let reduct = self.typing.frames.var_reduct_at(name);
        match reduct {
            Some(reduct) => self.opens(name, reduct),
            None => self.absent(name),
        }

        self.unless_member(reduct)
    }

    /// [`Frames::projection_entry`](super::Frames::projection_entry)'s value, with `base` and the base the arm scrutinized held to one universe instance, and declined where they cannot be ([`hold_at_one_instance`]).
    ///
    /// The commitment sits in the accessor for the reason [`Context::scrutinee_reduct`] states, and this store needs one more thing to carry it: its key erases the base, so the unerased spelling is kept beside the value rather than recovered.
    pub(crate) fn proj_reduct(&mut self, base: &Term, index: usize) -> Option<Term> {
        let entry = self.typing.frames.projection_entry(base, index)?;
        let (original, value) = (entry.original.clone(), entry.value.clone());

        hold_at_one_instance(self, base, &original)
            .unwrap_or(false)
            .then_some(value)
    }

    /// Record one guard's equation under every spelling it is met by — each `(canonical, original, alias)` — with the refinement cache protocol run once for all of them rather than once per spelling.
    pub(crate) fn refine_scrutinee_spellings(
        &mut self,
        spellings: Vec<(Term, Term, bool)>,
        value: &Term,
    ) {
        self.typing.caches.invalidate_for_refinement();
        for (canonical, original, alias) in spellings {
            self.typing.frames.refine_scrutinee(
                canonical,
                ScrutineeEntry {
                    original,
                    value: value.clone(),
                    alias,
                    withheld: false,
                    restates: false,
                },
            );
        }
    }

    pub(crate) fn has_scrutinee_refinements(&self) -> bool {
        self.typing.frames.has_scrutinee_refinements()
    }

    pub(crate) fn scrutinee_head_refined(&self, head: HeadTag<'_>) -> bool {
        self.typing.frames.scrutinee_head_refined(head)
    }

    /// [`Frames::scrutinee_entry`](super::Frames::scrutinee_entry)'s value, with `probe` and the spelling the equation was registered on held to one universe instance, and declined where they cannot be ([`hold_at_one_instance`]). The two are read as their key spells them ([`key_spelling`]), so they differ in levels alone.
    ///
    /// **The commitment is in the accessor, so no read of the store rests on an instance it has not held.** The key cannot carry this: it is computed when an arm is entered, which is before the levels in it are solved, and it is then compared for as long as the arm stands. The kernel needs none of it because it is handed a zonked module, where every instance is already ground and keying on the scrutinee itself is exact ([Case equations and their key](../../documentation/design/soundness/case-equations-and-their-key.md)).
    ///
    /// A universe error declines. That is the direction the whole read moves in — fewer refinements fire, never more — so it can cost a reduction and never admit one.
    pub(crate) fn scrutinee_reduct(&mut self, canonical: &Term, probe: &Term) -> Option<Term> {
        let entry = self.typing.frames.scrutinee_entry(canonical)?;
        if entry.withheld {
            return None;
        }
        let (original, value) = (entry.original.clone(), entry.value.clone());
        let (probe, original) = (key_spelling(self, probe), key_spelling(self, &original));

        hold_at_one_instance(self, &probe, &original)
            .unwrap_or(false)
            .then_some(value)
    }

    /// Every equation in force with the key it is held under, aliases among them, innermost frame first.
    pub(crate) fn scrutinee_equations(&self) -> Vec<(Term, ScrutineeEntry)> {
        self.typing.frames.scrutinee_equations()
    }

    /// Every equation an arm in force withholds, innermost frame first.
    pub(crate) fn withheld_equations(&self) -> Vec<ScrutineeEntry> {
        self.typing.frames.withheld_equations()
    }

    pub(crate) fn visible_scrutinee_entries(
        &self,
    ) -> impl Iterator<Item = (usize, &Term, &ScrutineeEntry)> {
        self.typing.frames.visible_scrutinee_entries()
    }

    /// [`Frames::is_key_set_aside`](super::Frames::is_key_set_aside).
    pub(crate) fn is_key_set_aside(&self, canonical: &Term) -> bool {
        self.typing.frames.is_key_set_aside(canonical)
    }

    /// [`Frames::reads_the_refinements_of`](super::Frames::reads_the_refinements_of): whether the refinements a lookup reads now are the ones the frames `born` register, so that a spelling reduced where the solver stands rests on nothing the metavariable's birth lacks.
    pub(crate) fn reads_the_refinements_of(&self, born: &HeldFrames) -> bool {
        self.typing.frames.reads_the_refinements_of(born)
    }

    /// Run `f`, a judgment of a metavariable, where the metavariable was born: `born` are the frames its birth holds. At once where they are the ones a lookup reads already ([`Frames::stands_on`](super::Frames::stands_on)), the common case of a term judged in the arm it was born in: the live context is then the birth's with binders and definitions added, and what is judged names none of them. Standing on the frames otherwise ([`Context::standing_on`]), so a solution's check, two answers' comparison, a bound's discharge, the solver's reading of a candidate and a goal's candidates each read the birth's guards in the frames they stood in, its definitions and its witness binders, and nothing of the arm the judgment happens to be made in.
    pub(crate) fn at_birth<R>(&mut self, born: &HeldFrames, f: impl FnOnce(&mut Self) -> R) -> R {
        match self.typing.frames.stands_on(born) {
            true => f(self),
            false => self.standing_on(born, f),
        }
    }

    /// Run `f` with the refinements of `frame` and every frame inside it withheld — the bracket a scrutinee entry's reduced spelling is settled under, so it rests only on the equations that outlive it. [`Frames::withhold_refinements_from`](super::Frames::withhold_refinements_from) carries the rule, and the caches set the live window's scoped answers aside for the bracket and hand them back where it ends ([`Caches::enter_window`](super::Caches::enter_window)). What the settlement remembers is kept under the refinements it read, for the next settlement made under them, for as long as the deepest frame it read stands ([`Frames::take_settled_window`](super::Frames::take_settled_window), [`Frames::begin_reads`](super::Frames::begin_reads)).
    pub(crate) fn with_refinements_withheld_from<R>(
        &mut self,
        frame: usize,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        let plain = self.typing.asking.plain();
        let kept = self
            .typing
            .frames
            .take_settled_window(frame, plain)
            .filter(|kept| kept.stands(|footing| self.stands(footing)));
        self.typing
            .frames
            .begin_reads(kept.as_ref().map_or(0, KeptWindow::deepest_read));
        self.typing.caches.enter_window(
            Bracket::Settlement { plain },
            kept.map(KeptWindow::into_window),
        );
        let previous = self.typing.frames.withhold_refinements_from(frame);
        let result = f(self);
        self.typing.frames.restore_withheld_refinements(previous);
        let read = self.typing.frames.end_reads();
        if let Some(own) = self.typing.caches.leave_window() {
            let footing = self.footing(false, None, true, true);
            self.typing
                .frames
                .keep_settled_window(frame, plain, own, footing, read);
        }

        result
    }
}
