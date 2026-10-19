//! One frame of the context: what a binder scope adds, and the scope through it, as one value.
//!
//! A frame is entered where a scope opens and left where it closes ([`Frames`](super::Frames)). Each holds its refinements of a variable, of a projection and of a stuck application, the reduced spelling each of the last was settled to, why its arm is dead where it is, and whether it is an arm or a function's body.
//!
//! **A frame carries the scope through it.** A frame entered over another refers to it and carries what the local frames through it hold — the assumptions and the definitions by name, the locals and the witness binders in the order they were bound — in persistent maps it shares with the frame beneath: entering a frame copies nothing, a binding costs a map's logarithm, and a lookup asks the innermost frame and the base frame, walking no stack. The base frame is the elaboration's and carries only its own, so a frame held while a declaration is published reads the base frame as it stands. A frame's refinements are its own, which few frames register, and it links the nearest frame beneath that registers any, so a question put to every refinement in view walks only the frames that register one. A second link reaches the frame at any depth beneath in a logarithm of the depth, as in Myers' applicative random-access stack.
//!
//! **A frame is shared.** The context holds the innermost behind a count, and so does what waits in it ([`HeldFrames`]): a parked problem where it parks, and a metavariable where it is born. Holding the innermost frame holds every frame beneath it. A frame written to while something else holds it is copied for the writer first ([`Frames`](super::Frames) writes through `Rc::make_mut`), which copies its own records and shares the scope's maps, so a holder has what stood and nothing written since. Only the innermost frame and the base frame are written. What a frame remembers of its guards' reduced spellings is the one thing written through a shared frame, since it is the frame's wherever the frame is held.
//!
//! **A frame is numbered where it is entered**, and a copy made for a writer keeps the number: it is the same frame with more written to it. A frame is entered at one depth over one chain of frames, so two frames at one depth that bear one number stand over the same frames, and whether the frames a metavariable was born under still stand is read off the numbers.
//!
//! **A frame may hold a name twice.** An arm assumes its own payload binder again at the type its case's solution gives it (`retype_locals`), so the locals are a list, read by the later entry, with the assumptions beside them by name.

use {
    super::{BirthTelescope, Footing, Window},
    crate::DeadArm,
    curios_core::{DefinitionKind, Free, MetavarSpine, Term, UniverseContext},
    curios_utilities::{Keyed, Ordered, Persistent},
    std::{cell::RefCell, collections::HashMap, fmt, hash::Hash, ptr, rc::Rc},
};

/// One definition: the definiens, plus the [`DefinitionKind`] of the module item that introduced it. Every `DefEntry` — whether a plain `let`/`rec` member or mid-window rec-group registration — is treated uniformly; there is no `recursive` marker distinguishing them.
///
/// `kind` is `None` for a genuine *local* binding — a `let` binder, an opened match scrutinee, a lambda parameter — which no module item declared. It is carried rather than re-derived: the kind is elaboration metadata `into_core` attached where the item was generated, and splitting the definition's name apart to recover it would misread an ordinary definition that merely happens to sit under a generated namespace (see [`DefinitionKind`]'s own docs).
#[derive(Debug, Clone)]
pub(crate) struct DefEntry {
    term: Term,
    kind: Option<DefinitionKind>,
    /// Whether the name is a proof its item published ([`curios_core::Definition::proof`]): it stays bound to `term`, which is what says which group it is a member of, and unfolds to nothing.
    proof: bool,
    /// The depth of the frame that defines the name: what a settlement reading the definition rests on ([`Frames::begin_reads`](super::Frames::begin_reads)). No part of what the definition is, so two definitions alike in all else are one.
    depth: usize,
    /// The term by value, as it was read where the name was defined ([`Context::by_value`](crate::Context::by_value)): its solved metavariables materialized and each local definition it names replaced by what that one is by value. Read in place of the term wherever the kernel's spelling of a term is asked, so a chain of definitions is read once, where each link is made, and not again at every spelling. Derived from the term and the definitions it names, so no part of what the definition is either.
    by_value: Term,
}

impl PartialEq for DefEntry {
    fn eq(&self, other: &Self) -> bool {
        self.term == other.term && self.kind == other.kind && self.proof == other.proof
    }
}

impl DefEntry {
    pub(crate) fn new(term: Term, kind: Option<DefinitionKind>) -> Self {
        Self {
            by_value: term.clone(),
            term,
            kind,
            proof: false,
            depth: 0,
        }
    }

    /// The entry a published item's name is bound by: `term`, and a proof where `proof`.
    pub(crate) fn published(term: Term, kind: DefinitionKind, proof: bool) -> Self {
        Self {
            by_value: term.clone(),
            term,
            kind: Some(kind),
            proof,
            depth: 0,
        }
    }

    /// The entry as the frame at `depth` defines it.
    pub(super) fn defined_at(self, depth: usize) -> Self {
        Self { depth, ..self }
    }

    /// The depth of the frame that defines the name.
    pub(super) fn depth(&self) -> usize {
        self.depth
    }

    /// The entry with its term read by value as `by_value`, where it is defined.
    pub(super) fn read_by_value(self, by_value: Term) -> Self {
        Self { by_value, ..self }
    }

    /// The term by value, as it was read where the name was defined.
    pub(super) fn by_value(&self) -> &Term {
        &self.by_value
    }

    pub(crate) fn body(&self) -> &Term {
        &self.term
    }

    pub(crate) fn kind(&self) -> Option<&DefinitionKind> {
        self.kind.as_ref()
    }

    /// What the name unfolds to: its term, and nothing where it is a proof.
    pub(super) fn reduct(&self) -> Option<&Term> {
        (!self.proof).then_some(&self.term)
    }
}

/// One stuck-application refinement: the scrutinee as *written* (unerased, so a settlement can still unfold its polymorphic heads — erasure strips the `Instance` a global unfolds through, so reduce-then-erase and erase-then-reduce disagree exactly there), and the arm's value.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ScrutineeEntry {
    pub(crate) original: Term,
    pub(crate) value: Term,
    /// Whether this spelling is an alias of an equation recorded under another: the kernel's spelling of a guard written over local definitions, which an occurrence reached by unfolding a definition presents exactly. The exact lookup reads it; settlement skips it, which leaves a reduct over the definitions' values unanswered — `typing`'s registration states the case.
    pub(crate) alias: bool,
    /// Whether this entry withholds the equation its key holds in a frame outside, rather than recording one: an arm that refines a variable leaves it under every equation whose scrutinee, as the kernel spells it under the arm's solution, names no local. `Context::withhold_closed_equations` states the rule. It keeps the equation's spelling and value, which a report on the arm names.
    pub(crate) withheld: bool,
}

/// A scrutinee entry's reduced spelling once settled: the form a probe is compared at — solved metavariables materialized and universe instances erased, once, when it settles — and the unerased reduct a hit reads its instance from.
#[derive(Debug, Clone)]
pub(crate) struct Settled {
    pub(crate) compared: Term,
    pub(crate) unerased: Term,
}

/// What a settlement remembered, beside the deepest frame it read and what else it rests on: it answers only while both stand.
#[derive(Debug)]
pub(super) struct KeptWindow {
    pub(super) window: Window,
    pub(super) stood: Prefix,
    pub(super) footing: Footing,
}

/// The frames through one that stands at one moment, as a later moment can ask whether they all still do: the stand they stand in, how many there are, and the number the last took.
///
/// A frame leaves only after every frame inside it, and no two frames take one number, so they all stand while the last does. A copy a stand holds keeps the number of the frame it copies and may hold less than that frame does now, so the stand is what tells the two apart.
#[derive(Debug, Clone, Copy)]
pub(super) struct Prefix {
    pub(super) stand: u64,
    pub(super) depth: usize,
    pub(super) innermost: u64,
}

/// What a settlement left for one scrutinee entry.
#[derive(Debug, Clone)]
pub(super) struct Spelling {
    /// The reduced spelling, or `None` where reducing the key refused.
    pub(super) settled: Option<Settled>,
    /// What it rests on beyond the frames outside its entry: it is what its key reduces to while that stands.
    pub(super) footing: Footing,
}

/// One projection refinement: the base as *written* (unerased, so a probe at another universe instance can be told apart from the spelling the arm actually scrutinized), and the arm's value.
///
/// The key beside it is universes-erased, which is what lets two occurrences of one polymorphic base merge while their instances are still undecided. Erasure cannot tell one instance from another, so the base is kept whole and [`Context::proj_reduct`](crate::Context) holds the two to one instance at the read.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ProjectionEntry {
    pub(crate) original: Term,
    pub(crate) value: Term,
}

/// Entries by key, read back in the order they were recorded, persistent: a clone shares every entry, and a record into either leaves the other as it was.
///
/// A hash map's own order differs from one process to the next, and a frame's entries are read in order: the guards a proof the elaborator writes is built from are admitted as they are met, so a frame holding two would yield two proofs of one bound, by the run; and a report names an unfolded `rec` group by the first definition it finds of that structure, so two groups of one structure would be spelled either way.
#[derive(Clone)]
pub(super) struct Recorded<K, V> {
    /// Each key's place and value.
    places: Persistent<K, (i64, V)>,
    /// The entries at their places, in the order they were first recorded.
    entries: Ordered<(K, V)>,
    /// The place the next key takes.
    next: i64,
}

impl<K: fmt::Debug, V: fmt::Debug> fmt::Debug for Recorded<K, V> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.entries.fmt(formatter)
    }
}

impl<K: Eq + Hash + Clone, V: Clone> Recorded<K, V> {
    fn new() -> Self {
        Self {
            places: Persistent::new(),
            entries: Ordered::new(),
            next: 0,
        }
    }

    pub(super) fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub(super) fn len(&self) -> usize {
        self.entries.len()
    }

    pub(super) fn get(&self, key: &K) -> Option<&V> {
        self.places.get(key).map(|(_, value)| value)
    }

    pub(super) fn contains_key(&self, key: &K) -> bool {
        self.places.contains_key(key)
    }

    /// Record `value` under `key`, in the place the key already holds where it was recorded before.
    pub(super) fn insert(&mut self, key: K, value: V) {
        let place = match self.places.get(&key) {
            Some((place, _)) => *place,
            None => {
                self.next += 1;
                self.next - 1
            }
        };
        self.places.insert(key.clone(), (place, value.clone()));
        self.entries.insert(place, (key, value));
    }

    pub(super) fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.entries.iter().map(|(_, (key, value))| (key, value))
    }

    pub(super) fn keys(&self) -> impl Iterator<Item = &K> {
        self.iter().map(|(key, _)| key)
    }

    /// Whether the two recorded the same entries in the same order.
    fn same_as(&self, other: &Self) -> bool
    where
        V: PartialEq,
    {
        self.len() == other.len() && self.iter().eq(other.iter())
    }

    /// Whether every entry here is one `others` recorded, in any of them.
    fn within<'a>(&self, others: impl Iterator<Item = &'a Self> + Clone) -> bool
    where
        K: 'a,
        V: PartialEq + 'a,
    {
        self.iter().all(|(key, value)| {
            others
                .clone()
                .any(|other| other.get(key).is_some_and(|held| held == value))
        })
    }

    /// The entries recorded here that `other` records otherwise or not at all, by place, read in time proportional to what the two do not share: a record and one made from it by a few writes are compared at the cost of those writes.
    pub(super) fn apart_from(&self, other: &Self) -> Vec<(K, V)>
    where
        V: PartialEq,
    {
        let mut apart = Vec::new();
        self.entries
            .diff(&other.entries, PartialEq::eq, |_, _, recorded| {
                apart.extend(recorded.cloned());
            });

        apart
    }

    /// Forget `key`, the entries recorded after it keeping their places.
    pub(super) fn remove(&mut self, key: &K) {
        if let Some((place, _)) = self.places.remove(key) {
            self.entries.remove(place);
        }
    }
}

/// The projection a refinement is registered under, its base's universe instances erased: told apart from another by its hash, so a probe compares no base but the one it asks for.
pub(super) type Projected = Keyed<(Term, usize)>;

/// One name as the scope binds it: its type, its universe context, and the place of its binding among the locals.
#[derive(Debug, Clone)]
pub(super) struct Binding {
    pub(super) type_: Term,
    pub(super) universes: UniverseContext,
    place: i64,
    /// The depth of the frame that binds the name: what a settlement reading the binding rests on.
    pub(super) depth: usize,
}

/// One frame: what the scope it was entered for adds, and the scope through it. See the module documentation.
#[derive(Clone)]
pub(crate) struct Frame {
    /// The number the frame took where it was entered, which no other frame of the elaboration takes: a copy made for a writer keeps it.
    pub(super) number: u64,
    /// Where the frame stands: the base frame at nought, and a frame entered over another one past it.
    pub(super) depth: usize,
    /// The local frame this one was entered over, as it stood: `None` for the base frame and for the outermost local frame.
    beneath: Option<Rc<Frame>>,
    /// A frame further beneath, which reaching a depth takes where it does not pass the depth: chosen as Myers' applicative random-access stack chooses it, so that reaching any depth takes a logarithm of the distance ([`at`]).
    jump: Option<Rc<Frame>>,
    /// The nearest frame beneath that registers a refinement of any kind.
    refined_beneath: Option<Rc<Frame>>,
    /// How many scrutinee entries the frames beneath register, together.
    scrutinees_beneath: usize,
    /// The nearest frame beneath that is an arm refining its scrutinee to a case.
    armed_beneath: Option<Rc<Frame>>,
    /// Every name the local frames through this one assume, at its innermost binding — or, for the base frame, the names it assumes itself.
    pub(super) assumptions: Persistent<Free, Binding>,
    /// Every name the local frames through this one define, at its innermost definition — or the base frame's own.
    pub(super) definitions: Recorded<Free, DefEntry>,
    /// Whether this frame defines a name itself, which its leaving reports.
    pub(super) defines: bool,
    /// The locals of the local frames through this one in binding order, a name bound twice at both places — or the base frame's own.
    pub(super) locals: Ordered<(Free, Term)>,
    /// The place the next local takes, and the first place this frame bound itself.
    next_local: i64,
    first_local: i64,
    /// Each name the local frames through this one bind, once, at its innermost binding and in binding order, beside the identity spine over them: what a metavariable born where this frame is innermost is born over, shared, so a birth costs nothing more than the one before it. The base frame keeps neither, its names being no metavariable's binders.
    ///
    /// One entry per name: an arm assumes a generalized hypothesis again under its case-specialized type (`retype_locals`), and a telescope holding a name twice would give every metavariable born there a spine with a repeated argument, which `Convert::solve` cannot invert. The innermost binding is kept, which keeps the telescope well-scoped: everything a generalized hypothesis's type can mention was generalized with it and bound again after it.
    pub(super) telescope: BirthTelescope,
    pub(super) spine: MetavarSpine,
    /// How many times the local frames through this one bound a name again or replaced its type: two frames over one chain that count alike bind every name the frame beneath both binds as it does ([`Frames::binds_as`](super::Frames::binds_as)).
    pub(super) rebound: usize,
    /// The `use`-plicity binders among the locals, in binding order: resolution's search space, scanned innermost first.
    pub(super) witnesses: Ordered<(Free, Term)>,
    next_witness: i64,
    /// Counterfactual match-arm refinements of a variable (`refine_head`), kept beside the definitions but suppressible: re-validation of a metavariable solution must keep stable definitions yet ignore these.
    pub(super) refinements: Recorded<Free, Term>,
    pub(super) refinement_projections: Recorded<Projected, ProjectionEntry>,
    /// Counterfactual refinements keyed by a *stuck application* scrutinee — a non-key match head (`classify(c)`, `Nat/in_range(...)`) that `refine_head` could not record. Keyed by the scrutinee as written, its metavariables and universes normalized (`reduce`'s `shallow_scrutinee`); an occurrence that surfaces spelled differently is met at the entry's reduced spelling (`scrutinee_spellings`). The term-keyed analogue of the two stores above, suppressed by the same flag.
    pub(super) refinement_scrutinees: Recorded<Term, ScrutineeEntry>,
    /// Each scrutinee entry's *reduced* spelling once a probe has asked for it, under its key — `reduce::refined_reduct`'s memo, the elaborator's copy of the kernel's per-entry reduct.
    ///
    /// **It lives as long as its entry, and is forgotten by what can change what its key reduces to.** A spelling is settled with its entry's frame and every frame inside it withheld, and read only through the window, so it rests on the refinements of the frames outside its entry — which cannot change while the entry stands: a registration lands in the innermost frame, and a frame leaves after every frame inside it — and on what reduction reads of everything else: the definitions, the solutions and the universe levels. So a registration and the exit of an inner frame leave it alone, and it is forgotten with its frame and where its key is registered again; otherwise it stands while what it read of the rest does, which it is filed with ([`Footing`]): the solutions committed when it settled, all of them while it held an unsolved metavariable, the levels where it holds a universe metavariable, the answers it was given where it asked a question, and the definitions in view. A footing is read against the stores rather than walked by them, so a frame only a parked problem holds forgets as the live ones do. A fresh definition forgets the spellings naming it, as it does the reducts.
    ///
    /// **It is the frame's, behind a cell.** The live context and every judgment that stands on the frame read one memo and settle a key once between them: a spelling rests on the frames below its own, and a frame stands on the same ones wherever it is held.
    ///
    /// **Settled once for each of the two reductions**, a table for each (`Context::plainly`): a probe is compared with the spelling reduced the way the probe was, as the kernel settles an equation's reduct once for each.
    pub(super) scrutinee_spellings: RefCell<[HashMap<Term, Spelling>; 2]>,
    /// What the last settlement made under this frame's refinements remembered, of each of the two reductions: the settlement of an entry registered in a frame this is the nearest one beneath to register a refinement, or, kept by the base frame, of one no frame beneath refines. The next settlement made under them starts from it (`Caches::enter_window`).
    ///
    /// **It rests on the refinements it read, and on the deepest frame it read.** Every settlement of such an entry withholds the frames from its entry's inwards and reads the refinements of this frame and the frames beneath, which hold still while this one registers nothing more: a registration, and an equation withheld, land in the innermost frame and forget what is kept here. But what a settlement remembers is not its key's alone: a stuck form put to an entry is the probe's, and names what the frames inside stand for, a local definition under a label another frame may define again among them. So the window is filed with the deepest frame it read ([`Prefix`], `Frames::begin_reads`) and answers only while that frame still stands, and with a footing that reads every part, since its reducts are many.
    ///
    /// **Shared by the frame's copies**, which the prefix tells apart where a stand holds one.
    pub(super) settled_windows: Rc<RefCell<[Option<KeptWindow>; 2]>>,
    /// Why the innermost arm through this frame is never taken, where it is an arm its scrutinee's indices rule out. The reason is known where the arm opens, and an implicit bound nothing discharges under it is reported once the arm has closed, so it is kept here for the metavariable born there to read ([`HeldFrames::dead_arm`]).
    pub(super) dead_arm: Option<Rc<DeadArm>>,
    /// The guard and the case of the arm the frame is, where it is an arm that refines its scrutinee to a case — `None` for every other frame. Whether a value reaches such an arm is found by reducing its guard, which nothing does where the arm opens: it is asked where an error leaves the arm, and where a bound nothing discharges under it is given up ([`HeldFrames::arms`]).
    pub(super) arm: Option<(Term, Term)>,
    /// Whether this frame or one beneath is a function's body, whose result type is the region a `!` within it sequences in. Known where the body opens, and read where a `!` there is refused, which may be a retry standing on the frame.
    pub(super) body: bool,
}

impl Frame {
    /// The base frame, which persists for the whole elaboration.
    pub(super) fn base() -> Self {
        Self::entered(0, None)
    }

    /// The frame entered as `number` over `beneath`, the innermost local frame, where one stands: it carries the scope through `beneath`.
    pub(super) fn entered(number: u64, beneath: Option<&Rc<Frame>>) -> Self {
        let Some(beneath) = beneath else {
            return Self {
                number,
                depth: usize::from(number != 0),
                beneath: None,
                jump: None,
                refined_beneath: None,
                scrutinees_beneath: 0,
                armed_beneath: None,
                assumptions: Persistent::new(),
                definitions: Recorded::new(),
                defines: false,
                locals: Ordered::new(),
                next_local: 0,
                first_local: 0,
                rebound: 0,
                telescope: BirthTelescope::default(),
                spine: MetavarSpine::new(),
                witnesses: Ordered::new(),
                next_witness: 0,
                refinements: Recorded::new(),
                refinement_projections: Recorded::new(),
                refinement_scrutinees: Recorded::new(),
                scrutinee_spellings: Default::default(),
                settled_windows: Default::default(),
                dead_arm: None,
                arm: None,
                body: false,
            };
        };

        Self {
            number,
            depth: beneath.depth + 1,
            beneath: Some(Rc::clone(beneath)),
            jump: Some(jump_over(beneath)),
            refined_beneath: match beneath.refined() {
                true => Some(Rc::clone(beneath)),
                false => beneath.refined_beneath.clone(),
            },
            scrutinees_beneath: beneath.scrutinees_through(),
            armed_beneath: match beneath.arm.is_some() {
                true => Some(Rc::clone(beneath)),
                false => beneath.armed_beneath.clone(),
            },
            assumptions: beneath.assumptions.clone(),
            definitions: beneath.definitions.clone(),
            defines: false,
            locals: beneath.locals.clone(),
            next_local: beneath.next_local,
            first_local: beneath.next_local,
            rebound: beneath.rebound,
            telescope: beneath.telescope.clone(),
            spine: beneath.spine.clone(),
            witnesses: beneath.witnesses.clone(),
            next_witness: beneath.next_witness,
            refinements: Recorded::new(),
            refinement_projections: Recorded::new(),
            refinement_scrutinees: Recorded::new(),
            scrutinee_spellings: Default::default(),
            settled_windows: Default::default(),
            dead_arm: beneath.dead_arm.clone(),
            arm: None,
            body: beneath.body,
        }
    }

    /// The nearest frame beneath that registers a refinement of any kind, where one does.
    pub(super) fn refined_beneath(&self) -> Option<&Rc<Frame>> {
        self.refined_beneath.as_ref()
    }

    /// Whether the frame registers a refinement of any kind.
    pub(super) fn refined(&self) -> bool {
        !self.refinements.is_empty()
            || !self.refinement_projections.is_empty()
            || !self.refinement_scrutinees.is_empty()
    }

    /// How many scrutinee entries this frame and the frames beneath it register.
    pub(super) fn scrutinees_through(&self) -> usize {
        self.scrutinees_beneath + self.refinement_scrutinees.len()
    }

    /// Whether the two frames register the same refinements, of every kind and in the same order.
    fn refined_as(&self, other: &Frame) -> bool {
        self.refinements.same_as(&other.refinements)
            && self
                .refinement_projections
                .same_as(&other.refinement_projections)
            && self
                .refinement_scrutinees
                .same_as(&other.refinement_scrutinees)
    }

    /// Bind `name` at `type_`, a local after every other.
    pub(super) fn bind(&mut self, name: Free, type_: Term) {
        let place = self.next_local;
        self.next_local += 1;
        let again = self.assumptions.contains_key(&name);
        self.rebound += usize::from(again);
        self.locals.insert(place, (name, type_.clone()));
        self.assumptions.insert(
            name,
            Binding {
                type_: type_.clone(),
                universes: UniverseContext::empty(),
                place,
                depth: self.depth,
            },
        );
        match (self.depth > 0, again) {
            (false, _) => {}
            (true, false) => {
                self.spine.bound(&name);
                self.telescope.bound(name, type_);
            }
            (true, true) => self.restate_scope(),
        }
    }

    /// The telescope and the spine stated again from the locals, each name at its innermost binding: where a name is bound again or its type replaced, which only an arm retyping its locals does, and which reads the scope as the retyping did.
    fn restate_scope(&mut self) {
        let locals = self
            .locals
            .iter()
            .map(|(_, local)| local)
            .collect::<Vec<_>>();
        let mut innermost = HashMap::with_capacity(locals.len());
        for (rank, (name, _)) in locals.iter().enumerate() {
            innermost.insert(name, rank);
        }

        self.telescope = BirthTelescope::default();
        self.spine = MetavarSpine::new();
        for (rank, (name, type_)) in locals.iter().enumerate() {
            if innermost[name] == rank {
                self.spine.bound(name);
                self.telescope.bound(*name, type_.clone());
            }
        }
    }

    /// Whether the innermost binding of `name` is this frame's own, rather than a frame's beneath it.
    pub(super) fn binds_itself(&self, name: &Free) -> bool {
        self.assumptions
            .get(name)
            .is_some_and(|binding| binding.place >= self.first_local)
    }

    /// Replace the type of `name`'s innermost binding, at its place among the locals.
    pub(super) fn rebind(&mut self, name: &Free, type_: &Term) {
        let Some(mut binding) = self.assumptions.get(name).cloned() else {
            return;
        };
        binding.type_ = type_.clone();
        self.rebound += 1;
        self.locals.insert(binding.place, (*name, type_.clone()));
        self.assumptions.insert(*name, binding);
        if self.depth > 0 {
            self.restate_scope();
        }
    }

    /// Replace the universe context of `name`'s innermost binding.
    pub(super) fn set_universes(&mut self, name: &Free, universes: UniverseContext) {
        if let Some(mut binding) = self.assumptions.get(name).cloned() {
            binding.universes = universes;
            self.assumptions.insert(*name, binding);
        }
    }

    /// Drop the place among the locals `name`'s innermost binding stands at, where it is bound: a binding about to be made again.
    pub(super) fn unbind_local(&mut self, name: &Free) {
        if let Some(binding) = self.assumptions.get(name) {
            let place = binding.place;
            self.locals.remove(place);
        }
    }

    /// Drop every binding of `name`: its assumption, its definition, and its places among the locals and the witness binders.
    pub(super) fn unbind(&mut self, name: &Free) {
        if let Some(binding) = self.assumptions.remove(name) {
            self.locals.remove(binding.place);
        }
        self.definitions.remove(name);
        let witnessed = self
            .witnesses
            .iter()
            .filter(|(_, (bound, _))| bound == name)
            .map(|(place, _)| place)
            .collect::<Vec<_>>();
        for place in witnessed {
            self.witnesses.remove(place);
        }
    }

    /// Join `name` to the witness binders, after every other.
    pub(super) fn witness(&mut self, name: Free, type_: Term) {
        self.witnesses.insert(self.next_witness, (name, type_));
        self.next_witness += 1;
    }
}

impl fmt::Debug for Frame {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("Frame")
            .field("number", &self.number)
            .field("depth", &self.depth)
            .field("refinements", &self.refinements)
            .field("refinement_projections", &self.refinement_projections)
            .field("refinement_scrutinees", &self.refinement_scrutinees)
            .field("arm", &self.arm.is_some())
            .field("body", &self.body)
            .finish_non_exhaustive()
    }
}

impl Drop for Frame {
    /// A frame nothing else holds drops the frame beneath it, and that one the next: each is taken apart here in turn rather than inside the drop of the one above, so a scope of any depth drops on a bounded stack. The links past the frame beneath name frames it holds, and only lower their counts.
    fn drop(&mut self) {
        self.jump = None;
        self.refined_beneath = None;
        self.armed_beneath = None;
        let mut beneath = self.beneath.take();
        while let Some(frame) = beneath {
            let Ok(mut frame) = Rc::try_unwrap(frame) else {
                break;
            };
            frame.jump = None;
            frame.refined_beneath = None;
            frame.armed_beneath = None;
            beneath = frame.beneath.take();
        }
    }
}

/// The frame a frame entered over `parent` jumps to: `parent`'s second jump where `parent`'s two jumps span equal distances, and `parent` otherwise. Myers' rule, under which reaching any depth takes a logarithm of the distance.
fn jump_over(parent: &Rc<Frame>) -> Rc<Frame> {
    if let Some(jump) = &parent.jump
        && let Some(further) = &jump.jump
        && parent.depth - jump.depth == jump.depth - further.depth
    {
        return Rc::clone(further);
    }

    Rc::clone(parent)
}

/// The frame at `depth` beneath or at `frame`, a local frame: `None` past it and at the base frame's depth.
pub(super) fn at(frame: &Rc<Frame>, depth: usize) -> Option<&Rc<Frame>> {
    if depth == 0 || depth > frame.depth {
        return None;
    }
    let mut frame = frame;
    while frame.depth > depth {
        frame = match &frame.jump {
            Some(jump) if jump.depth >= depth => jump,
            _ => frame
                .beneath
                .as_ref()
                .expect("a frame past the outermost local frame stands over one"),
        };
    }

    Some(frame)
}

/// The frames through `frame` that register a refinement, innermost first.
pub(super) fn refined_through(frame: Option<&Rc<Frame>>) -> Through<'_> {
    fn link(frame: &Frame) -> Option<&Rc<Frame>> {
        frame.refined_beneath.as_ref()
    }

    Through {
        next: frame.and_then(|frame| match frame.refined() {
            true => Some(frame),
            false => link(frame),
        }),
        link,
    }
}

/// The frames through `frame` that are arms refining a scrutinee to a case, innermost first.
fn armed_through(frame: Option<&Rc<Frame>>) -> Through<'_> {
    fn link(frame: &Frame) -> Option<&Rc<Frame>> {
        frame.armed_beneath.as_ref()
    }

    Through {
        next: frame.and_then(|frame| match frame.arm.is_some() {
            true => Some(frame),
            false => link(frame),
        }),
        link,
    }
}

/// A walk down one of a frame's links.
pub(super) struct Through<'f> {
    next: Option<&'f Rc<Frame>>,
    link: fn(&Frame) -> Option<&Rc<Frame>>,
}

impl<'f> Iterator for Through<'f> {
    type Item = &'f Rc<Frame>;

    fn next(&mut self) -> Option<Self::Item> {
        let frame = self.next?;
        self.next = (self.link)(frame);
        Some(frame)
    }
}

/// Whether every refinement `these` frames register, of every kind, is one `those` register: what containment between two contexts asks, since a solution resting on a guard is scoped to the arm the guard opens. Asked entry by entry, a frame the two share answering for itself. Each side is the frames that register a refinement.
pub(super) fn refinements_within(these: &[&Frame], those: &[&Frame]) -> bool {
    these
        .iter()
        .filter(|frame| !those.iter().any(|other| ptr::eq(**frame, *other)))
        .all(|frame| {
            frame
                .refinements
                .within(those.iter().map(|other| &other.refinements))
                && frame
                    .refinement_projections
                    .within(those.iter().map(|other| &other.refinement_projections))
                && frame
                    .refinement_scrutinees
                    .within(those.iter().map(|other| &other.refinement_scrutinees))
        })
}

/// Whether `live`, the innermost local frame that stands, stands over the frames `held` ends in: at `held`'s depth stands the same frame, by its number, with the refinements it had where it was held, and so beneath it. What is added past them, binders and definitions, is not asked about here.
pub(super) fn stand(live: Option<&Rc<Frame>>, held: Option<&Rc<Frame>>) -> bool {
    let Some(mut held) = held else {
        return true;
    };
    let Some(mut live) = live.and_then(|live| at(live, held.depth)) else {
        return false;
    };
    loop {
        if Rc::ptr_eq(live, held) {
            return true;
        }
        if live.number != held.number || !live.refined_as(held) {
            return false;
        }
        match (&live.beneath, &held.beneath) {
            (Some(live_beneath), Some(held_beneath)) => {
                live = live_beneath;
                held = held_beneath;
            }
            (None, None) => return true,
            _ => return false,
        }
    }
}

/// The frames above the base as they stood where something that waits took them: a parked problem where it parks, and a metavariable where it is born. Held as the innermost of them, which holds the rest, shared with the context and with every other holder and never copied: a frame written to since was copied for its writer, so these are what stood. A judgment made for what holds them stands on them ([`Context::standing_on`](crate::Context::standing_on)).
///
/// The base frame is the elaboration's and is never held: what waits with no local frame standing holds none.
#[derive(Debug, Clone, Default)]
pub(crate) struct HeldFrames(Option<Rc<Frame>>);

impl HeldFrames {
    pub(super) fn of(innermost: Option<&Rc<Frame>>) -> Self {
        Self(innermost.cloned())
    }

    /// The innermost frame held.
    pub(super) fn innermost(&self) -> Option<&Rc<Frame>> {
        self.0.as_ref()
    }

    /// The frames held that register a refinement, innermost first.
    pub(super) fn refined_frames(&self) -> Vec<&Frame> {
        refined_through(self.0.as_ref())
            .map(|frame| &**frame)
            .collect()
    }

    /// Whether a frame held binds `name` as a local: whether it is a binder of the telescope a metavariable born where the frames were held is born over.
    pub(crate) fn binds_locally(&self, name: &Free) -> bool {
        self.0
            .as_ref()
            .is_some_and(|frame| frame.assumptions.contains_key(name))
    }

    /// Whether no frame is held: what waits where no local frame stood.
    pub(crate) fn is_empty(&self) -> bool {
        self.0.is_none()
    }

    /// Whether a frame held registers a refinement of any kind: the residual-constraint report notes it, and the caches are told of a judgment that stands on one.
    pub(crate) fn refined(&self) -> bool {
        refined_through(self.0.as_ref()).next().is_some()
    }

    /// Whether a frame held binds a local: what a judgment standing on these reads that the base frame does not hold.
    pub(crate) fn binds(&self) -> bool {
        self.0
            .as_ref()
            .is_some_and(|frame| !frame.locals.is_empty())
    }

    /// The `use`-plicity binders the frames hold, outermost first: the reader position a report about a metavariable born under them is spelled for.
    pub(crate) fn witnesses(&self) -> impl Iterator<Item = &(Free, Term)> {
        self.0
            .iter()
            .flat_map(|frame| frame.witnesses.iter().map(|(_, witness)| witness))
    }

    /// Why the innermost arm among the frames is never taken, where its scrutinee's indices rule one out: an implicit born there and never solved is reported with it once the arm has closed.
    pub(crate) fn dead_arm(&self) -> Option<&Rc<DeadArm>> {
        self.0.as_ref()?.dead_arm.as_ref()
    }

    /// The arms among the frames that refine a scrutinee to a case, innermost first: each with its guard and its case, and the frames as far as its own, which are the ones its guard is read under.
    pub(crate) fn arms(&self) -> Vec<(HeldFrames, Term, Term)> {
        armed_through(self.0.as_ref())
            .filter_map(|frame| {
                let (guard, case) = frame.arm.as_ref()?;

                Some((Self(Some(Rc::clone(frame))), guard.clone(), case.clone()))
            })
            .collect()
    }

    /// Whether every refinement these frames register is one `other`'s register: what containment between two birth contexts asks beside their names.
    pub(crate) fn refinements_within(&self, other: &HeldFrames) -> bool {
        refinements_within(&self.refined_frames(), &other.refined_frames())
    }

    /// The frames both hold, outermost first for as far as they are the same frames, each as `self` holds it: what a metavariable is restricted to when a solution born under `other` embeds it, `self` being the earlier of the two births, whose frames are the ones nothing was written to since.
    ///
    /// Two frames at one depth that bear one number stand over the same frames, so the frames agree up to a depth and differ past it, and the depth is found by halving.
    pub(crate) fn shared_with(&self, other: &HeldFrames) -> HeldFrames {
        let (Some(mine), Some(theirs)) = (&self.0, &other.0) else {
            return Self::default();
        };
        let agree = |depth| {
            at(mine, depth)
                .zip(at(theirs, depth))
                .is_some_and(|(mine, theirs)| mine.number == theirs.number)
        };
        let (mut agreed, mut apart) = (0, mine.depth.min(theirs.depth) + 1);
        while apart - agreed > 1 {
            let middle = agreed + (apart - agreed) / 2;
            match agree(middle) {
                true => agreed = middle,
                false => apart = middle,
            }
        }

        Self(at(mine, agreed).cloned())
    }

    /// The frames held beneath the one numbered as `frame` is, the innermost that stands: what a metavariable born in a block is restricted to where the block ends. Every frame held, where none is that one.
    pub(crate) fn beneath(&self, frame: &Frame) -> HeldFrames {
        let Some(held) = &self.0 else {
            return Self::default();
        };
        match at(held, frame.depth) {
            Some(at) if at.number == frame.number => Self(at.beneath.clone()),
            _ => self.clone(),
        }
    }
}
