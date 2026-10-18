//! The lexical half of the elaborator's state: assumptions, local definitions, counterfactual refinements, and the witness scope, a stack of frames ([`Frame`]) bracket-disciplined by `enter`/`leave`.
//!
//! Everything here lives and dies with binder frames — the opposite lifetime from the flat stores in [`Program`](super::Program) and [`Solutions`](super::Solutions). Cache coordination stays with the `Context` façade: a frame write that must clear or stamp the caches does so there, so this type's methods are pure store operations.
//!
//! **A question costs what it asks, not the depth of the scope.** The innermost frame carries the scope through it ([`Frame`]), so a lookup asks it and the base frame; the gates the reducer puts at every step read counts and links the frames keep; what walks the refinements in view walks only the frames that register one; and holding the scope takes the innermost frame.
//!
//! One bracket narrows what a lookup reads: withholding hides a settlement's own frame and every frame inside it.
//!
//! **A frame is shared, and copied for a writer.** What waits takes the frames above the base as they stand ([`HeldFrames`]), a parked problem where it parks and a metavariable where it is born, and a write to a frame something holds copies the frame first, so a holder has what stood and nothing written since. A judgment made away from the live context stands on the frames held ([`Frames::stand_on`]), a retry on its problem's and what is judged of a metavariable on its birth's: the live ones above the base are set aside, the held ones stand in their place, and nothing is assumed, defined or registered again, so the frame an entry stands in is the frame it was registered in.

use {
    super::{
        DefEntry, Defined, Footing, Frame, HeldFrames, InScope, ProjectionEntry, ScrutineeEntry,
        Settled, SharedSpine, SharedTelescope, Window,
        frame::{KeptWindow, Prefix, Spelling, at, refined_through, refinements_within, stand},
    },
    crate::DeadArm,
    curios_core::{
        DefinitionKind, Free, Global, HeadTag, RecGroup, Term, UniverseContext,
        project_erased_universes,
    },
    curios_utilities::{Entropy, Keyed},
    std::{
        collections::{BTreeMap, HashMap},
        rc::Rc,
    },
};

/// The frame-scoped lexical stores. `Context` holds exactly one of these; see the module documentation for the cache-coordination contract.
#[derive(Debug)]
pub(crate) struct Frames<'s> {
    /// The units in scope, beneath the base frame: asked where no frame holds a name, and never written.
    scope: Option<&'s dyn InScope>,
    /// The base frame: it persists for the whole elaboration and holds the top-level names, which are assumed into it and never inside a frame.
    base: Frame,
    /// The innermost local frame, which holds every local frame beneath it: `None` where none stands.
    top: Option<Rc<Frame>>,
    /// The innermost local frame of the frames each judgment standing on held frames set aside, the outermost stand first ([`Frames::stand_on`]): no lookup reads them, and the questions put to every refinement are put to theirs too.
    aside: Vec<Option<Rc<Frame>>>,
    /// The number each of those stands took, the outermost first: what a [`Prefix`] names the stand by.
    stands: Vec<u64>,
    /// How many judgments have stood on held frames: the number the next stand takes. The live frames stand in none, numbered zero.
    stood: u64,
    /// Each top-level `rec` group the base frame defines a member of, beside the globals its members are defined under, in member order: its definitions read the other way, kept as each member is defined so that a reader holding a group and one of its names asks for the rest and scans nothing. Two declarations that define one group are two lists, each in the order its members were defined. A member not yet defined, or forgotten, is `None`. Asked by group and never read in order, which a hash map has none of.
    members: HashMap<Keyed<RecGroup>, Vec<Vec<Option<Global>>>>,
    /// How much of the refinement stack a settlement withholds from the inside, as the depth of the frame the entry being settled was registered in — `None` for none of it.
    ///
    /// The bracket the kernel has. A refinement key's *reduced* spelling must rest only on equations that outlive it, which are the frames outside its own: an inner arm's equation retracts first, and the entry's own frame is the equation itself, which a reduction of the key would otherwise meet at its first probe and answer with the case value it is assuming. `curios-cert`'s `Scope::hide_refinements_from` withholds the same span for the same two reasons.
    withhold_refinements_from: Option<usize>,
    /// One tick per mutation of a frame's locals (assume, frame exit, reassume) — an `Entropy` used as a version stamp: `fresh()` bumps, `count()` reads. Invalidates `identity_cache`, which shares the frozen telescope and identity spine between every meta born under an unchanged Γ.
    locals_stamp: Entropy,
    identity_cache: Option<(u64, SharedTelescope, SharedSpine)>,
    /// How many frames have been entered: the number the next one takes ([`Frame::number`]).
    entered: u64,
}

impl<'s> Frames<'s> {
    pub(crate) fn new() -> Self {
        Self {
            scope: None,
            base: Frame::base(),
            top: None,
            aside: Vec::new(),
            stands: Vec::new(),
            stood: 0,
            members: HashMap::new(),
            withhold_refinements_from: None,
            locals_stamp: Entropy::new(),
            identity_cache: None,
            entered: 0,
        }
    }

    /// Read through to `scope` where no frame holds a name.
    pub(crate) fn read_through(&mut self, scope: &'s dyn InScope) {
        self.scope = Some(scope);
    }

    /// What a unit in scope declares under `name`, which only a global names.
    fn in_scope(&self, name: &Free) -> Option<Defined<'s>> {
        match name {
            Free::Global(global) => self.scope?.definition(global),
            _ => None,
        }
    }

    /// The innermost frame, to write to: where a binding, a definition and a refinement land. Copied first where something holds it, so a holder keeps what stood.
    fn innermost(&mut self) -> &mut Frame {
        match &mut self.top {
            Some(top) => Rc::make_mut(top),
            None => &mut self.base,
        }
    }

    /// The frame holding `name`'s innermost binding, to write to: the innermost frame or the base frame, the two a write reaches. Panics where a frame between them holds it.
    fn binder_of(&mut self, name: &Free) -> Option<&mut Frame> {
        if let Some(top) = &mut self.top
            && top.assumptions.contains_key(name)
        {
            assert!(
                top.binds_itself(name),
                "'{}' is bound in a frame beneath the innermost, which no write reaches",
                name.faithful()
            );
            return Some(Rc::make_mut(top));
        }

        self.base
            .assumptions
            .contains_key(name)
            .then_some(&mut self.base)
    }

    /// The frame at `depth` among the frames that stand, the base frame at nought.
    fn frame_at(&self, depth: usize) -> Option<&Frame> {
        match depth {
            0 => Some(&self.base),
            _ => at(self.top.as_ref()?, depth).map(|frame| &**frame),
        }
    }

    pub(crate) fn enter(&mut self) {
        self.entered += 1;
        let frame = Frame::entered(self.entered, self.top.as_ref());
        self.top = Some(Rc::new(frame));
    }

    /// Pop one frame, reporting `(dropped_refinements, dropped_definitions)` so the façade can run the matching cache protocol.
    pub(crate) fn leave(&mut self) -> (bool, bool) {
        self.locals_stamp.fresh();
        let frame = self.top.take().expect("a local frame stands to leave");
        self.top = at(&frame, frame.depth - 1).cloned();

        (frame.refined(), frame.defines)
    }

    /// Say that the frame being elaborated is an arm that is never taken, for `dead`. The reason leaves with the frame.
    pub(crate) fn rule_out(&mut self, dead: Rc<DeadArm>) {
        self.innermost().dead_arm = Some(dead);
    }

    /// Say that the frame being elaborated is the arm at `case` of a match on `guard`. It leaves with the frame.
    pub(crate) fn open_arm(&mut self, guard: Term, case: Term) {
        self.innermost().arm = Some((guard, case));
    }

    /// Say that the frame being elaborated is a function's body. It leaves with the frame.
    pub(crate) fn open_body(&mut self) {
        self.innermost().body = true;
    }

    /// Whether a frame that stands is a function's body.
    pub(crate) fn in_body(&self) -> bool {
        self.base.body || self.top.as_ref().is_some_and(|top| top.body)
    }

    /// Assume `label : type_`. Erasure is sort-driven (a proof or a type erases), so a binder carries no runtime-multiplicity mark. The base frame holds one binding of a name: a top-level name assumed again replaces the binding it had.
    pub(crate) fn assume(&mut self, name: &Free, type_: &Term) {
        self.locals_stamp.fresh();
        if self.top.is_none() {
            self.base.unbind_local(name);
        }
        self.innermost().bind(*name, type_.clone());
    }

    /// Join `name` to the witness scope (it must already be assumed).
    pub(crate) fn push_witness_binder(&mut self, name: &Free, type_: &Term) {
        self.innermost().witness(*name, type_.clone());
    }

    /// The `use`-plicity binders in scope, in binding order (innermost last).
    pub(crate) fn witness_scope(&self) -> impl Iterator<Item = &(Free, Term)> {
        self.base
            .witnesses
            .iter()
            .chain(self.top.iter().flat_map(|top| top.witnesses.iter()))
            .map(|(_, witness)| witness)
    }

    /// Replace the type of an existing assumption in place — the innermost binding of `label`. Used by the `rec` elaborators: a group's signatures must be assumed (lowered) before they can be elaborated, since members reference each other, and are then upgraded here to their rebuilt forms — implicit insertion makes the two no longer interchangeable, and a lowered type must never leak into later reduction. Panics if `label` has no prior assumption — every caller is expected to have `assume`d it earlier in the same scope (a construction bug otherwise, not a user-facing case).
    pub(crate) fn reassume(&mut self, name: &Free, type_: &Term) {
        self.locals_stamp.fresh();
        self.binder_of(name)
            .unwrap_or_else(|| {
                panic!(
                    "reassume: '{}' has no local binding to replace",
                    name.faithful()
                )
            })
            .rebind(name, type_);
    }

    pub(crate) fn assumption(&self, name: &Free) -> Option<&Term> {
        self.binding(name)
            .map(|binding| &binding.type_)
            .or_else(|| self.in_scope(name).map(|defined| defined.type_))
    }

    /// The innermost binding of `name` a frame holds.
    fn binding(&self, name: &Free) -> Option<&super::frame::Binding> {
        self.top
            .as_ref()
            .and_then(|top| top.assumptions.get(name))
            .or_else(|| self.base.assumptions.get(name))
    }

    /// The frames that register a refinement, innermost first, among those that stand — the base frame last.
    fn refined_standing(&self) -> impl Iterator<Item = &Frame> {
        refined_through(self.top.as_ref())
            .map(|frame| &**frame)
            .chain(self.base.refined().then_some(&self.base))
    }

    /// The frames that register a refinement among those every judgment standing on held frames set aside.
    fn refined_aside(&self) -> impl Iterator<Item = &Frame> {
        self.aside
            .iter()
            .flat_map(|top| refined_through(top.as_ref()))
            .map(|frame| &**frame)
    }

    /// Every frame that registers a refinement, those a judgment standing on held frames set aside among them. A guard set aside still stands in the program, so the gates in front of the reducer's probe of a guard's key and of the closed machine are asked of each; beside them only [`Frames::is_key_set_aside`] reads a frame set aside.
    fn refined_everywhere(&self) -> impl Iterator<Item = &Frame> {
        self.refined_standing().chain(self.refined_aside())
    }

    /// The frames above the base as they stand, shared: what a parked problem takes where it parks and a metavariable where it is born, and a judgment standing on held frames takes them with the frame it runs in. The base frame is the elaboration's and is never held, so what waits with no local frame standing holds none. Taking them is taking the innermost.
    pub(crate) fn hold(&self) -> HeldFrames {
        HeldFrames::of(self.top.as_ref())
    }

    /// Whether `held`, the frames a metavariable was born under, are where a lookup reads now: they still stand, outermost first, the frames that stand register no refinement they do not, and no settlement withholds any. The live context is then the birth's with binders and definitions added, and what is judged of the metavariable names none of them.
    pub(crate) fn stands_on(&self, held: &HeldFrames) -> bool {
        self.withhold_refinements_from.is_none()
            && stand(self.top.as_ref(), held.innermost())
            && self.reads_the_refinements_of(held)
    }

    /// Whether the refinements a lookup reads now are the ones `held` registers, as sets: two frames may have recorded the same refinements in different orders. What the solver asks before it commits a spelling reduced where it stands: one that rests on a guard the metavariable's birth lacks is not the metavariable's.
    pub(crate) fn reads_the_refinements_of(&self, held: &HeldFrames) -> bool {
        let read = self.refined_in_view().collect::<Vec<_>>();
        let held = held.refined_frames();
        refinements_within(&read, &held) && refinements_within(&held, &read)
    }

    /// The innermost frame as it stands: the frame a block's end leaves.
    pub(crate) fn innermost_frame(&self) -> &Frame {
        self.top.as_deref().unwrap_or(&self.base)
    }

    /// Whether a local frame registers a refinement of any kind: what standing on held frames sets aside, which the caches are told of.
    pub(crate) fn locally_refined(&self) -> bool {
        refined_through(self.top.as_ref()).next().is_some()
    }

    /// The innermost definition a frame in view holds of `name`, where one does: what a held frame's definition of the name is compared with before a judgment stands on it.
    pub(crate) fn definition(&self, name: &Free) -> Option<&DefEntry> {
        self.defined(name)
    }

    /// The definitions `held` holds that the frames in view may hold otherwise or not at all, each name's innermost: every one `held` holds, less those the innermost frame in view holds as `held` does, passed over unread where the two share them. What a judgment standing on `held` compares with [`Frames::definition`] before it stands.
    pub(crate) fn held_definitions_apart(&self, held: &HeldFrames) -> Vec<(Free, DefEntry)> {
        let Some(held) = held.innermost() else {
            return Vec::new();
        };
        match &self.top {
            Some(top) => held.definitions.apart_from(&top.definitions),
            None => held
                .definitions
                .iter()
                .map(|(name, entry)| (*name, entry.clone()))
                .collect(),
        }
    }

    /// Set the local frames aside and stand on `held` in their place, with the withholding in force among them, a depth in the frames that stood where it began. Nothing is assumed, defined or registered: a held frame is the frame its entries were registered in. Answers what [`Frames::stand_back`] gives back.
    pub(crate) fn stand_on(&mut self, held: &HeldFrames) -> SetAside {
        self.locals_stamp.fresh();
        let aside = std::mem::replace(&mut self.top, held.innermost().cloned());
        self.aside.push(aside);
        self.stood += 1;
        self.stands.push(self.stood);

        SetAside {
            withheld: self.withhold_refinements_from.take(),
        }
    }

    /// Leave the frames a judgment stood on and take back the ones it set aside, with their withholding ([`Frames::stand_on`]). The frame the judgment ran in has left already.
    pub(crate) fn stand_back(&mut self, aside: SetAside) {
        self.locals_stamp.fresh();
        self.top = self.aside.pop().expect("a stand set its frames aside");
        self.stands.pop();
        self.withhold_refinements_from = aside.withheld;
    }

    /// Drop every base-frame binding of `name`: its assumption, its universe context, its definition, and its places in the local and witness scopes. Base frame only — an inner frame is popped whole by [`Frames::leave`] — because the one binding that needs forgetting singly is a top-level declaration's, undone when its item is refused.
    pub(crate) fn forget(&mut self, name: &Free) {
        assert!(
            self.top.is_none(),
            "forget: '{}' outside the base frame",
            name.faithful()
        );
        self.locals_stamp.fresh();
        self.unname_member(name);
        self.base.unbind(name);
    }
    /// The innermost registered universe context for `name`, if any.
    pub(crate) fn assumption_universe_context(&self, name: &Free) -> Option<UniverseContext> {
        self.binding(name)
            .map(|binding| &binding.universes)
            .or_else(|| self.in_scope(name).map(|defined| defined.universe_context))
            .cloned()
    }
    /// Overwrite the innermost universe context registered for `name`. Panics if none exists.
    pub(crate) fn set_assumption_universe_context(
        &mut self,
        name: &Free,
        universe_context: UniverseContext,
    ) {
        self.binder_of(name)
            .unwrap_or_else(|| {
                panic!(
                    "'{}' has no assumption universe context to replace",
                    name.faithful()
                )
            })
            .set_universes(name, universe_context);
    }

    /// The `(depth, parameter_count)` holders of `name`'s universe context, the base frame's and the innermost local binding's, beside how many frames stand — diagnostics for the instantiation mismatch traces.
    #[cfg(feature = "profile")]
    pub(crate) fn assumption_universe_holders(&self, name: &Free) -> (usize, Vec<(usize, usize)>) {
        let local = self.top.as_ref().and_then(|top| {
            let binding = top.assumptions.get(name)?;
            Some((top.depth, binding.universes.parameter_count))
        });
        let base = self
            .base
            .assumptions
            .get(name)
            .map(|binding| (0, binding.universes.parameter_count));

        (
            self.top.as_ref().map_or(1, |top| top.depth + 1),
            base.into_iter().chain(local).collect(),
        )
    }

    /// Whether `label` currently has a definition entry in some frame — the settled-globals gate for the elaboration cache. A name defined here will only ever be *re*defined (which clears both caches wholesale), never freshly defined, so an elaboration entry naming it is safe to keep across a later fresh `define`.
    pub(crate) fn is_defined(&self, name: &Free) -> bool {
        self.defined(name).is_some() || self.in_scope(name).is_some()
    }
    /// Every frame's locals in binding order (outermost first), the base frame's top-level names first: what a test reads of a declaration forgotten everywhere, where a judgment reads [`Frames::local_binders`].
    #[cfg(test)]
    pub(crate) fn locals(&self) -> impl Iterator<Item = &(Free, Term)> {
        self.base
            .locals
            .iter()
            .chain(self.top.iter().flat_map(|top| top.locals.iter()))
            .map(|(_, local)| local)
    }

    /// The locals of the frames past the base, in scope order: the binders a term stands under, without the declarations of every unit in scope the base frame holds, so a question about them costs the scope a judgment stands in and not the program's size.
    pub(crate) fn local_binders(&self) -> impl Iterator<Item = &(Free, Term)> {
        self.top
            .iter()
            .flat_map(|top| top.locals.iter())
            .map(|(_, local)| local)
    }

    /// Insert `name`'s definition into the innermost frame. The façade decides the cache protocol from [`Frames::is_defined`] first.
    pub(crate) fn define(&mut self, name: Free, entry: DefEntry) {
        if self.top.is_none() {
            self.unname_member(&name);
            if let (Free::Global(global), Some((group, index))) =
                (&name, entry.body().as_rec_proj())
            {
                let lists = self.members.entry(Keyed::new(group.clone())).or_default();
                // A member joins the first list its place is open in: the declaration defining the group's members defines them in a row.
                let open = lists
                    .iter()
                    .position(|names| names.get(index).is_some_and(Option::is_none));
                let names = match open {
                    Some(open) => &mut lists[open],
                    None => {
                        lists.push(vec![None; group.length()]);
                        lists.last_mut().expect("just pushed")
                    }
                };
                if let Some(slot) = names.get_mut(index) {
                    *slot = Some(*global);
                }
            }
        }
        let frame = self.innermost();
        frame.definitions.insert(name, entry);
        frame.defines = true;
    }

    /// Take `name` out of the group the base frame defines it as a member of, where it defines it as one: before the name is defined again, or forgotten. A group left with no named member is dropped.
    fn unname_member(&mut self, name: &Free) {
        let Some((group, index)) = self
            .base
            .definitions
            .get(name)
            .and_then(|entry| entry.body().as_rec_proj())
        else {
            return;
        };
        let Free::Global(global) = name else {
            return;
        };
        let group = Keyed::new(group.clone());
        let Some(lists) = self.members.get_mut(&group) else {
            return;
        };
        for names in lists.iter_mut() {
            if let Some(slot) = names.get_mut(index)
                && *slot == Some(*global)
            {
                *slot = None;
            }
        }
        lists.retain(|names| names.iter().any(Option::is_some));
        if lists.is_empty() {
            self.members.remove(&group);
        }
    }

    /// The globals `group`'s members are defined under, in member order, by the definitions `name` is one of: the base frame's, where it defines every one, and otherwise a unit's in scope that names `name` among them.
    pub(crate) fn member_names(&self, group: &RecGroup, name: Global) -> Option<Vec<Global>> {
        match self.members.get(&Keyed::new(group.clone())) {
            Some(lists) => lists
                .iter()
                .find(|names| names.contains(&Some(name)))?
                .iter()
                .copied()
                .collect(),
            None => self
                .scope?
                .member_names(group)
                .filter(|names| names.contains(&name)),
        }
    }

    /// The innermost definition of `name` a frame holds.
    fn defined(&self, name: &Free) -> Option<&DefEntry> {
        self.top
            .as_ref()
            .and_then(|top| top.definitions.get(name))
            .or_else(|| self.base.definitions.get(name))
    }

    /// The [`DefinitionKind`] of the module item that defined `label`, or `None` for a local binding or an undefined name.
    ///
    /// The structural replacement for splitting a definition's qualified name into a family and a case and looking the family up in a registry: the kind was known where the definition was generated, so it is read back rather than re-derived from the name's spelling.
    pub(crate) fn definition_kind(&self, name: &Free) -> Option<&DefinitionKind> {
        match self.defined(name) {
            Some(entry) => entry.kind(),
            None => self.in_scope(name).map(|defined| defined.kind),
        }
    }

    /// What `name` unfolds to through its *definition* alone — never through a refinement. The shared analyses read through this: a definitions-only lookup needs no invariant about when the refinement store happens to be empty, where [`Frames::var_reduct_at`] would silently mean something else inside a match arm.
    ///
    /// A proof a declaration published unfolds to nothing, in a frame as in a unit in scope: this is the one place a definition's body is handed out, so no reader of one is handed a proof's.
    pub(crate) fn definition_body(&self, name: &Free) -> Option<&Term> {
        match self.defined(name) {
            Some(entry) => entry.reduct(),
            None => self.in_scope(name).and_then(|defined| defined.body),
        }
    }

    /// Every top-level `rec` group defined in any frame, each with its members' global names in member order. A member's definition is the group with a projecting tail, so the groups are read off the members and their names gathered by index; a group read only partially — a member never defined — is dropped, since a spelling with a gap is no spelling.
    pub(crate) fn rec_definitions(&self) -> Vec<(RecGroup, Vec<Global>)> {
        let mut groups: Vec<(RecGroup, Vec<Option<Global>>)> = Vec::new();
        let definitions = self
            .base
            .definitions
            .iter()
            .chain(self.top.iter().flat_map(|top| top.definitions.iter()));
        for (name, entry) in definitions {
            let Free::Global(global) = name else {
                continue;
            };
            let Some((group, index)) = entry.body().as_rec_proj() else {
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
        groups
            .into_iter()
            .filter_map(|(group, names)| {
                let names = names.into_iter().collect::<Option<Vec<_>>>()?;
                Some((group, names))
            })
            .chain(self.scope.into_iter().flat_map(|scope| scope.groups()))
            .collect()
    }

    /// The reduct of a variable: its definition, or — from the frames no settlement withholds — its counterfactual refinement. A name never appears in both stores (definitions name `let`/`rec` binders; refinements name assumed scrutinee heads), so the order between them is immaterial.
    fn raw_var_reduct(&self, name: &Free) -> Option<&Term> {
        if let Some(term) = self.refinement_of(name) {
            return Some(term);
        }

        self.definition_body(name)
    }

    /// Reduce a bare variable only when its definition is monomorphic.
    ///
    /// A polymorphic definition's stored body is scoped by its universe context: its parameter levels are not meaningful at an occurrence until elaboration has rebuilt that occurrence as an `Instance`. Letting a raw variable unfold would leak those bound parameters into the ambient solver. The explicit-instance reducer uses [`Frames::var_reduct_at`] after it has the occurrence's level arguments.
    pub(crate) fn var_reduct(&self, name: &Free) -> Option<&Term> {
        let is_polymorphic = self
            .binding(name)
            .map(|binding| &binding.universes)
            .or_else(|| self.in_scope(name).map(|defined| defined.universe_context))
            .is_some_and(|context| context.parameter_count != 0);
        (!is_polymorphic)
            .then(|| self.raw_var_reduct(name))
            .flatten()
    }

    pub(crate) fn var_reduct_at(&self, name: &Free) -> Option<&Term> {
        self.raw_var_reduct(name)
    }

    /// The entry a projection's counterfactual match-arm refinement is registered under, from the frames no settlement withholds.
    ///
    /// The whole entry rather than its value, for the reason [`Frames::scrutinee_entry`] gives: the key cannot decide a universe instance, so the read above compares the unerased bases instead.
    pub(crate) fn projection_entry(&self, base: &Term, index: usize) -> Option<&ProjectionEntry> {
        let projected = Keyed::new((project_erased_universes(base), index));
        self.refined_in_view()
            .find_map(|frame| frame.refinement_projections.get(&projected))
    }

    /// Register a counterfactual match-arm refinement of a variable. Unlike a definition, this is the arm's alone: a judgment made for a metavariable born outside the arm stands on frames that hold none of it. The façade clears the caches first.
    pub(crate) fn refine(&mut self, name: &Free, term: &Term) {
        self.innermost().refinements.insert(*name, term.clone());
    }

    /// Register a counterfactual refinement of a projection (`refine_head` on a `Proj` scrutinee). The façade clears the caches first.
    pub(crate) fn refine_projection(&mut self, base: Term, index: usize, value: Term) {
        self.innermost().refinement_projections.insert(
            Keyed::new((project_erased_universes(&base), index)),
            ProjectionEntry {
                original: base,
                value,
            },
        );
    }
    /// Register a counterfactual refinement of a stuck-application scrutinee (`refine_head` on a non-key head). `canonical` is the cheap key (as written, metas and universes normalized); `original` is the unerased spelling a settlement reduces; `value` is the arm's constructor. Sound for the same reason `refine` is — the arm is reached only when the scrutinee equals `value` — and non-cyclic because `value` is a constructor of the scrutinee's inductive, a normal form. The façade clears the caches first.
    pub(crate) fn refine_scrutinee(&mut self, canonical: Term, entry: ScrutineeEntry) {
        let frame = self.innermost();
        // A key registered again is another entry, and what the one before it settled to is not its spelling.
        for spellings in frame.scrutinee_spellings.get_mut() {
            spellings.remove(&canonical);
        }
        frame.refinement_scrutinees.insert(canonical, entry);
    }
    /// What the entry `key` registered in the frame at depth `frame` settled to under the reduction `plain` names, where a probe has asked for it, beside what it rests on: the context reads whether that still stands (`Context::settled_key`).
    pub(super) fn spelling(&self, frame: usize, key: &Term, plain: bool) -> Option<Spelling> {
        self.frame_at(frame)?.scrutinee_spellings.borrow()[usize::from(plain)]
            .get(key)
            .cloned()
    }
    /// File what the entry `key` registered in the frame at depth `frame` settled to under the reduction `plain` names, on what it rests on. Filed in the frame's own memo, which every holder of the frame reads: the live context and a judgment standing on the frame settle a key once between them.
    pub(crate) fn settle_spelling(
        &mut self,
        frame: usize,
        key: Term,
        plain: bool,
        settled: Option<Settled>,
        footing: Footing,
    ) {
        if let Some(frame) = self.frame_at(frame) {
            frame.scrutinee_spellings.borrow_mut()[usize::from(plain)]
                .insert(key, Spelling { settled, footing });
        }
    }
    /// Forget the settled spellings a fresh definition of `name` can change: the ones naming it, which were stuck on its absence, and the ones that refused. Asked of the frames that stand and of those set aside, where a frame that registers a scrutinee entry is one that registers a refinement: a frame that has left the context names no binder defined since.
    pub(crate) fn forget_spellings_naming(&mut self, name: &Free) {
        for frame in self.refined_everywhere() {
            for spellings in frame.scrutinee_spellings.borrow_mut().iter_mut() {
                spellings.retain(|_, spelling| {
                    spelling
                        .settled
                        .as_ref()
                        .is_some_and(|settled| !settled.unerased.mentions_free(name))
                });
            }
            for kept in frame.settled_windows.borrow_mut().iter_mut().flatten() {
                kept.window.retain_without(name);
            }
        }
    }

    /// What the last settlement of an entry registered in the frame at depth `frame` remembered under the reduction `plain` names, taken for the next one to start from, where the frames that stood when it was filed all still stand ([`Frame::settled_windows`](super::Frame)): the context reads whether the rest of what it rests on does.
    pub(super) fn take_settled_window(&self, frame: usize, plain: bool) -> Option<KeptWindow> {
        let kept = self.frame_at(frame)?.settled_windows.borrow_mut()[usize::from(plain)].take()?;

        self.still_stand(kept.stood).then_some(kept)
    }

    /// File what a settlement of an entry registered in the frame at depth `frame` remembered under the reduction `plain` names, on what it rests on, for the next one.
    pub(crate) fn keep_settled_window(
        &self,
        frame: usize,
        plain: bool,
        window: Window,
        footing: Footing,
    ) {
        if let Some(frame) = self.frame_at(frame) {
            frame.settled_windows.borrow_mut()[usize::from(plain)] = Some(KeptWindow {
                window,
                stood: self.prefix(),
                footing,
            });
        }
    }

    /// The frames that stand now, for [`Frames::still_stand`] to be asked about later.
    fn prefix(&self) -> Prefix {
        let innermost = self.innermost_frame();

        Prefix {
            stand: self.stand_number(),
            depth: innermost.depth + 1,
            innermost: innermost.number,
        }
    }

    /// Whether every frame that stood at `prefix` still stands, in the stand it stood in.
    fn still_stand(&self, prefix: Prefix) -> bool {
        self.stand_number() == prefix.stand
            && self
                .frame_at(prefix.depth - 1)
                .is_some_and(|frame| frame.number == prefix.innermost)
    }

    /// The number of the stand the frames stand in: zero for the live ones.
    fn stand_number(&self) -> u64 {
        self.stands.last().copied().unwrap_or(0)
    }
    /// Whether any scrutinee refinement is registered, in a frame that stands or in one set aside. The cheap outer gate for the reducer probe — skipped on the common refinement-free reduction without hashing anything, and read off the counts the frames keep.
    pub(crate) fn has_scrutinee_refinements(&self) -> bool {
        !self.base.refinement_scrutinees.is_empty()
            || self
                .top
                .iter()
                .chain(self.aside.iter().flatten())
                .any(|top| top.scrutinees_through() > 0)
    }
    /// Whether some registered scrutinee key shares `head` as its applied-head symbol. The second gate, past `Term::head_key`: only a head that is actually refined justifies building the candidate's key.
    pub(crate) fn scrutinee_head_refined(&self, head: HeadTag<'_>) -> bool {
        self.refined_everywhere().any(|frame| {
            frame
                .refinement_scrutinees
                .keys()
                .any(|key| key.head_key() == Some(head))
        })
    }

    /// The entry registered under the key `canonical`, from the frames no settlement withholds — the innermost, so one an arm inside withholds is answered by the entry withholding it.
    ///
    /// The whole entry rather than its value, because the read above this one needs the `original` beside it: the key is universes-erased and cannot decide an instance, so [`Context::scrutinee_reduct`](crate::Context) holds the unerased spellings to one instance and declines where they cannot be.
    pub(crate) fn scrutinee_entry(&self, canonical: &Term) -> Option<&ScrutineeEntry> {
        self.refined_in_view()
            .find_map(|frame| frame.refinement_scrutinees.get(canonical))
    }

    /// Every scrutinee entry the window leaves visible, with the depth of the frame it was registered in, innermost frame first — what a stuck reduct is compared against once its written spelling missed, and the frame each must be settled from. Borrowed rather than collected: `reduce::refined_reduct` walks this at every stuck form under a live guard, and a copy per walk was most of what that cost.
    pub(crate) fn visible_scrutinee_entries(
        &self,
    ) -> impl Iterator<Item = (usize, &Term, &ScrutineeEntry)> {
        let ceiling = self.refinement_ceiling();
        self.refined_in_view().flat_map(move |frame| {
            frame
                .refinement_scrutinees
                .iter()
                .filter(move |(key, entry)| {
                    !entry.alias
                        && !entry.withheld
                        && !self.withheld_inside(frame.depth, ceiling, key)
                })
                .map(move |(key, entry)| (frame.depth, key, entry))
        })
    }

    /// Every equation the window leaves in force with the key it is held under, aliases among them, innermost frame first: what a variable's refinement is read against.
    pub(crate) fn scrutinee_equations(&self) -> Vec<(Term, ScrutineeEntry)> {
        let ceiling = self.refinement_ceiling();
        self.refined_in_view()
            .flat_map(|frame| {
                frame
                    .refinement_scrutinees
                    .iter()
                    .filter(move |(key, entry)| {
                        !entry.withheld && !self.withheld_inside(frame.depth, ceiling, key)
                    })
                    .map(|(key, entry)| (key.clone(), entry.clone()))
            })
            .collect()
    }

    /// Every equation an arm in the window withholds, as the entry withholding it, innermost frame first.
    pub(crate) fn withheld_equations(&self) -> Vec<ScrutineeEntry> {
        self.refined_in_view()
            .flat_map(|frame| {
                frame
                    .refinement_scrutinees
                    .iter()
                    .map(|(_, entry)| entry)
                    .filter(|entry| entry.withheld)
                    .cloned()
            })
            .collect()
    }

    /// Withhold, for as long as the current frame stands, the equation `entry` holds under `key` in a frame outside. The façade clears the caches first.
    pub(crate) fn withhold_scrutinee(&mut self, key: Term, entry: ScrutineeEntry) {
        self.innermost().refinement_scrutinees.insert(
            key,
            ScrutineeEntry {
                withheld: true,
                ..entry
            },
        );
    }

    /// Whether a frame inside the one at depth `frame`, below `ceiling`, withholds the equation `key` holds there.
    fn withheld_inside(&self, frame: usize, ceiling: usize, key: &Term) -> bool {
        self.refined_standing()
            .filter(|inner| frame < inner.depth && inner.depth < ceiling)
            .any(|inner| {
                inner
                    .refinement_scrutinees
                    .get(key)
                    .is_some_and(|entry| entry.withheld)
            })
    }

    /// The value the innermost arm in the window refines `name` to, where one refines it.
    pub(crate) fn refinement_of(&self, name: &Free) -> Option<&Term> {
        self.refined_in_view()
            .find_map(|frame| frame.refinements.get(name))
    }

    /// Whether an arm in the window refines any variable.
    pub(crate) fn refines_a_variable(&self) -> bool {
        self.refined_in_view()
            .any(|frame| !frame.refinements.is_empty())
    }

    /// The variables the arms in the window refine, each with the value its innermost arm gives it.
    pub(crate) fn refined_variables(&self) -> Vec<(Free, Term)> {
        let mut variables = BTreeMap::new();
        for frame in self.refined_in_view() {
            for (name, value) in frame.refinements.iter() {
                variables.entry(*name).or_insert_with(|| value.clone());
            }
        }
        variables.into_iter().collect()
    }

    /// Whether `canonical` is a scrutinee key registered in a frame a judgment standing on held frames set aside, and in none that stands: a guard the judgment's own frames register is its own, whether or not a settlement withholds it, and unfolds where it does not answer as it would with nothing set aside. A `Var`/`Proj` key stays neutral there for free (its refinement is not in view, so it does not unfold); an application key would otherwise unfold to its definition body and stop being a key. The reducer consults this to keep such a key neutral, so a solution committed where a guard is not in view stays a term the guard can still fire on where it is.
    pub(crate) fn is_key_set_aside(&self, canonical: &Term) -> bool {
        let registers = |frame: &Frame| frame.refinement_scrutinees.contains_key(canonical);

        self.refined_aside().any(registers) && !self.refined_standing().any(registers)
    }

    /// The depth past the frames no settlement withholds: the depth of the frame a settlement withholds from, or past the innermost. Clamped, so a frame popped past that point withholds everything rather than reaching past the frames that stand.
    fn refinement_ceiling(&self) -> usize {
        let standing = self.top.as_ref().map_or(1, |top| top.depth + 1);
        self.withhold_refinements_from
            .unwrap_or(standing)
            .min(standing)
    }

    /// The frames the window leaves visible that register a refinement, innermost first: those whose refinements a lookup reads.
    fn refined_in_view(&self) -> impl Iterator<Item = &Frame> {
        let ceiling = self.refinement_ceiling();
        self.refined_standing()
            .filter(move |frame| frame.depth < ceiling)
    }

    /// Begin withholding the refinements of the frame at depth `frame` and every frame inside it, returning the previous limit — the bracket intrinsic for `Context::with_refinements_withheld_from`.
    ///
    /// Always at or outside the current limit, since a settlement is only ever asked for an entry the window already shows, so nesting narrows the window and restoring widens it back.
    pub(crate) fn withhold_refinements_from(&mut self, frame: usize) -> Option<usize> {
        self.withhold_refinements_from.replace(frame)
    }

    /// Restore a limit taken by [`withhold_refinements_from`](Self::withhold_refinements_from).
    pub(crate) fn restore_withheld_refinements(&mut self, previous: Option<usize>) {
        self.withhold_refinements_from = previous;
    }

    /// Whether `name` is bound at the top level (the persistent base frame) — a global definition, always in scope. The metavariable solver admits such names in a solution even though they are not in the metavariable's Γ/spine (which holds only local binders): a solution may freely mention a global constant without that constant being a context binder.
    pub(crate) fn is_top_level(&self, name: &Free) -> bool {
        self.base.assumptions.contains_key(name) || self.in_scope(name).is_some()
    }

    /// The frozen telescope and identity spine for the *current* Γ, shared: rebuilt only when a frame's locals have changed since the last birth, so minting a metavariable is O(1) amortized instead of O(|Γ|) per mint — the difference between linear and quadratic elaboration over a module.
    ///
    /// Γ is the *local* binders only — the locals of the frames past the base. Top-level definitions are excluded so an item's elaboration is independent of how much else is in scope: a metavariable born deep in a proof carries just its enclosing binders, not the whole prelude, keeping the contextual solve's spine a small pattern (and the prelude cacheable). Globals a solution mentions are admitted by the solver's scope check via [`Frames::is_top_level`] instead.
    pub(crate) fn identity_snapshot(&mut self) -> (SharedTelescope, SharedSpine) {
        if let Some((stamp, telescope, spine)) = &self.identity_cache
            && *stamp == self.locals_stamp.count()
        {
            return (telescope.clone(), spine.clone());
        }

        // One entry per name, at its innermost binding. `retype_locals` re-`assume`s a generalized hypothesis under its case-specialized type and its *original* name, deliberately shadowing the ambient binder, so the locals can hold the same `Free` twice. Γ is a context rather than a stack of bindings: a shadowed entry is unreachable by construction, and leaving it in gives every metavariable born in such an arm a spine with a repeated argument — which `Convert::solve`'s inversion cannot invert, since a name reachable through two slots is not provably determined. The candidate is then refused by the scope check for mentioning a hypothesis that is plainly in scope, and the implicit surfaces as never solved.
        //
        // Keeping the *last* occurrence keeps the telescope well-scoped: everything a generalized hypothesis's type can mention was itself generalized — that is what the generalization set is — so every mentioner is re-assumed after it, and no surviving entry refers to the occurrence that was dropped.
        let locals = self.local_binders().collect::<Vec<_>>();
        let telescope = Rc::new({
            let mut innermost = HashMap::with_capacity(locals.len());
            for (index, (name, _)) in locals.iter().enumerate() {
                innermost.insert(name, index);
            }

            locals
                .iter()
                .enumerate()
                .filter(|(index, (name, _))| innermost[name] == *index)
                .map(|(_, entry)| (*entry).clone())
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
}

/// What a judgment standing on held frames set aside beside the frames themselves ([`Frames::stand_on`]): the withholding in force among them.
#[derive(Debug)]
pub(crate) struct SetAside {
    withheld: Option<usize>,
}
