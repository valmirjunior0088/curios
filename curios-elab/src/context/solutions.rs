//! The unification state: metavariable birth records and solutions, the solve journal, and the parked-work store.
//!
//! Everything here is frame-independent — a metavariable's record freezes the Γ it was born under, and parked work freezes the frame it blocked in, so neither is touched by `enter_frame`/`leave_frame`. Writes that must reach the cache stamps are stamped by the `Context` façade; the transactional watermark (`SolutionMark`) also stays there, because it spans this store and the universe solver together.
//!
//! **Which term awaits a metavariable is kept here, and written at three doors.** A term that reads its parts against types holding metavariables awaits them until it has read them all ([`Solutions::enter_awaiting`]): a metavariable is awaited from its birth by the term being read, and the relation moves only where a link is made — a committed solution holds what it mentions to what it solves, a parked problem holds together what it watches ([`Solutions::birth_with_kind`], [`Solutions::solve`], [`Solutions::park`]). What links hold together is a class ([`Held`]), joined where the link is made and awaited whole by the outermost standing term that awaited any of it, so a chain of links holds its ends: a numeral under two calls waits for the outer one, which no one link says of it. A term that has completed awaits nothing, which a reader sees without a pass: the record stays and the term no longer stands. So whether a choice may still be informed by what encloses it is read off the metavariable's class, and no pass over the store answers it.

use {
    super::{FrozenFrame, SharedRefinements, SharedTelescope},
    crate::{DeadArm, Problem, Refusal},
    curios_core::{
        Bound, Free, ImplicitOrigin, Metavar, MetavarId, MetavarOrigin, NumLit, Subterm, Term,
        Transient, WitnessOrigin,
    },
    curios_utilities::Entropy,
    std::{
        cmp::Reverse,
        collections::{BTreeMap, BTreeSet},
        mem,
        rc::Rc,
    },
};

/// One metavariable's record. Everything here is frozen at birth except `solution`, which transitions `None -> Some(_)` on solve — rolled back to `None` if re-validation rejects the candidate that solved it (`Context::rollback_solutions`).
#[derive(Debug)]
pub(crate) struct MetaEntry {
    /// Γ frozen at birth: the local assumption context in binding order, with birth-time types. Drives the scope check and re-validation.
    pub telescope: SharedTelescope,
    /// The match-arm refinements in force at birth: what a solution may rest on beyond Γ, and all it may — re-validation runs under exactly these, and containment compares them as it compares Γ.
    pub refinements: SharedRefinements,
    /// The witness binders in scope at birth, outermost first, or `None` where there were none: the reader position a report about this metavariable is spelled for, since which of `telescope`'s entries resolution would search is not recorded there.
    pub witnesses: Option<Rc<[(Free, Term)]>>,
    /// The metavariable's type — the `expected` it was checked against at birth.
    pub result: Term,
    /// `None` while unsolved; `Some(t)` once solved. `t`'s free `Var`s are a subset of `telescope`'s names.
    pub solution: Option<Term>,
    /// Ordinary inference holes may be solved by conversion. A recursive elaboration slot is filled only by the owning `rec` elaborator and a placeholder only by the problem parked behind it; conversion treats either, unfilled, as a blocking dependency ([`Solutions::owner_fills`]).
    pub kind: MetaKind,
    /// Whether `result` is a proposition, decided once where an omitted implicit is minted. It is what tells a *bound* nothing discharged — `Holds(i < n)`, the whole of why a call was refused — from a type argument nothing determined, `@T: Type`, which never was an obligation; the report reads differently for the two, and zonk, which raises it, holds the context immutably and cannot ask the sort itself.
    pub proposition: bool,
    /// What `result` reduced to when the mint — or a later attempt at a parked discharge — asked whether it was decided, kept when that is an inductive type: the reduct — `False` — is what the unsolved report says beside the bound's spelling, and the report cannot reduce for itself.
    pub reduct: Option<Term>,
    /// Why the procedure that proves a bound from the facts in scope proved nothing, the last time it was asked: the facts it considered and what its search concluded, which the unsolved report states beside the bound and cannot ask for itself.
    pub refusal: Option<Rc<Refusal>>,
    /// Why the arm an omitted implicit was minted in is never taken, where its scrutinee's indices rule it out: the report an unsolved one becomes is made once the arm has closed, and says so by this.
    pub dead_arm: Option<Rc<DeadArm>>,
    /// Its place among the metavariables links hold together.
    held: Held,
}

/// A metavariable's place in its class: the metavariables a chain of links holds together, each link a committed solution, which holds what it mentions to what it solves, or a parked problem, which holds together what it watches. Whatever may still inform one of them may inform every other, so one term awaits the class whole.
#[derive(Debug, Clone, Copy)]
enum Held {
    /// At the head of its class: the term that awaits the class, as last written — it awaits only while it stands ([`Solutions::awaited`]) — and how many metavariables the class holds.
    Head {
        awaited: Option<AwaitId>,
        members: usize,
    },
    /// Under another metavariable of its class, on the way to the head.
    Under(MetavarId),
}

/// A term that awaits: one that reads its parts against types holding metavariables and takes the choices left among them once it has read them all.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct AwaitId(usize);

/// One awaiting term's record.
#[derive(Debug)]
struct Awaiting {
    /// How many awaiting terms it is read inside: the outermost of two is the one read inside fewer.
    depth: usize,
    /// Whether it is still reading its parts or completing. A term that has completed awaits nothing.
    standing: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MetaKind {
    Inference,
    RecSlot,
    /// A parked problem's stand-in for the term it will rebuild: a check's ([`ParkedWork::Checking`]). Only that problem fills it, with the term it rebuilt, so what stands in the tree is what the program wrote: conversion waits on a placeholder, and nothing re-expresses one in another context.
    Placeholder,
}

/// The work a parked problem will retry.
#[derive(Debug)]
pub(crate) enum ParkedWork {
    /// A conversion constraint that quiesced blocked on unsolved metavariables.
    Conversion(Problem),
    /// A whole checking problem: a form met an expected type whose structure is still an unsolved metavariable. The `placeholder` metavariable stands in for the rebuilt term in the tree and is solved with it once the check can run — the spine machinery then splices it everywhere the occurrence travelled. The term is elaborated once: whatever runs the check — a wake's retry, a term's completion, the drain — holds the problem out of the store while it does.
    Checking {
        term: Term,
        expected: Term,
        placeholder: MetavarId,
    },
    /// A witness-resolution goal whose key type is not yet rigid: `slot` is the metavariable standing in the omitted `use`-argument's place, `goal` its (concept application) type. Woken when a watched metavariable solves; resolution then retries under the frozen frame.
    Witness {
        slot: MetavarId,
        goal: Term,
        provenance: WitnessOrigin,
    },
    /// A decided bound nothing discharged when it was inserted, because its subject still waited on a metavariable — one a later argument or the expectation pins. `slot` is the hole standing in the bound's place, `bound` its type. Woken when a watched metavariable solves; the bound is reduced again and the hole filled if it came to truth.
    Discharge {
        slot: MetavarId,
        bound: Term,
        provenance: ImplicitOrigin,
    },
}

/// A problem parked by `expect` (or a blocked intro-form check) to outlive its call. Like a [`MetaEntry`], it freezes the local frame it was born under.
#[derive(Debug)]
pub(crate) struct ParkedProblem {
    pub work: ParkedWork,
    /// The term being checked at park time; its extent anchors the eventual error if the problem never resolves.
    pub origin: Term,
    pub frame: FrozenFrame,
    /// The unsolved metavariables whose solutions could unblock this — solving any of them is the wake signal.
    pub watching: BTreeSet<MetavarId>,
}

/// A parked conversion as a completing term reads it ([`Solutions::conversions_awaited_by`]): where it stands in the store, the problem, the term it was parked for and the frame it froze.
#[derive(Debug)]
pub(crate) struct ParkedConversion {
    pub at: usize,
    pub problem: Problem,
    pub origin: Term,
    pub frame: FrozenFrame,
}

/// What a completing term may do for a parked problem nothing else answers: the problem's own answer, one among those it admits ([`ParkedWork::choice`]). Their order is the order they are taken in, each committing less than the next and able to decide it, with one exception the store cannot tell by a problem's shape: a lambda with a domain left open is read after the spines are held, which may be what gives the domain ([`Context::complete`](crate::Context::complete)).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Choice {
    /// A `use` slot nothing solved takes the witness resolution finds for its goal, where the goal's key has come to a head resolution reads.
    Witness,
    /// A tuple literal gives the metavariable it waits on its shape: a non-dependent product of one open type a field.
    Shape,
    /// The numerals that wait on one metavariable take their default together.
    Numeral,
    /// A lambda is checked: at the type its annotations state where every domain is written, and otherwise, after the spines, at the function type its expectation has come to or at the one its annotations and body state.
    Lambda,
    /// A conversion parked apart is asked again with spines held: two applications of one definition agree by whatever that solves, one answer among those a definition that is not injective has, and an application of an unsolved metavariable is imitated against a type former's node, one function among those it may stand for. Taken where every conversion its term awaits has offered its own answers and none differs.
    Spines,
}

impl ParkedWork {
    /// The choice a completing term may take for this problem, or `None` where only a solution answers it. Stated here once, so no consumer of the store recognizes a problem by its shape.
    pub(crate) fn choice(&self) -> Option<Choice> {
        match self {
            ParkedWork::Checking { term, .. } => match &**term {
                Subterm::Tuple(tuple) if !tuple.fields.is_empty() => Some(Choice::Shape),
                Subterm::Transient(Transient::NumLit(_)) => Some(Choice::Numeral),
                Subterm::Func(_) => Some(Choice::Lambda),
                _ => None,
            },
            ParkedWork::Witness { .. } => Some(Choice::Witness),
            // Whether its sides are two applications of one definition is read where the choice is taken, through what has been solved since it parked.
            ParkedWork::Conversion(_) => Some(Choice::Spines),
            _ => None,
        }
    }
}

/// A parked check with a choice a completing term may take for it ([`ParkedWork::choice`]): the term as written, the type it waits at, the placeholder that stands for it and the frame it froze.
#[derive(Debug, Clone)]
pub(crate) struct ParkedChoice {
    pub choice: Choice,
    pub written: Term,
    pub expected: Term,
    pub placeholder: MetavarId,
    pub frame: FrozenFrame,
}

impl ParkedProblem {
    /// This problem as a check with a choice of its own, where it is one ([`ParkedWork::choice`]).
    fn as_choice(&self) -> Option<ParkedChoice> {
        match (&self.work, self.work.choice()) {
            (
                ParkedWork::Checking {
                    term,
                    expected,
                    placeholder,
                },
                Some(choice),
            ) => Some(ParkedChoice {
                choice,
                written: term.clone(),
                expected: expected.clone(),
                placeholder: *placeholder,
                frame: self.frame.clone(),
            }),
            _ => None,
        }
    }
}

/// The flat, frame-independent unification stores. `Context` holds exactly one of these; see the module documentation for the frame-independence contract.
#[derive(Debug)]
pub(crate) struct Solutions {
    /// Metavariable records indexed by `Metavar::id` — monotonic facts about the program being elaborated, not lexically-scoped bindings.
    entries: Vec<Option<MetaEntry>>,
    /// The next metavariable id this store may mint (implicit-argument insertion). Seeded by `elaborate_module_suffix` with the lowering's count, so core-minted ids sit strictly above `into_core`'s.
    next_metavar: Entropy<MetavarId>,
    /// Parked conversion constraints — frame-independent, like the entries.
    parked: Vec<ParkedProblem>,
    /// Ids solved since the last `wake_parked` sweep: the wake signal.
    newly_solved: Vec<MetavarId>,
    /// Journal of every committed solution id, in commit order — never consumed, only marked and rolled back. The watermark/rollback pair lets re-validation unwind solutions that landed while validating a candidate it then rejected.
    solved_log: Vec<MetavarId>,
    /// `Some` inside an oracle, where work may not park: elaboration is being asked a question (re-validation, a candidate's fit) and provisional success would leak into the answer. The flag is whether a site inside it wanted to park and was refused, which makes that oracle's refusal no verdict on the term it was asked about.
    oracle: Option<bool>,
    /// Whether a completing term is holding the spines of a conversion it awaits ([`Solutions::set_holding`]): conversion then takes what two spines of one definition would solve, which it otherwise leaves to that term.
    holding: bool,
    /// Whether a `!` is reading its region's monad ([`Solutions::set_imitating`]): conversion then imitates an application of an unsolved metavariable where it meets one.
    imitating: bool,
    /// Every term that has awaited, by [`AwaitId`].
    awaiting: Vec<Awaiting>,
    /// The awaiting term being read, or `None` outside one: what a metavariable born now is awaited by.
    reading: Option<AwaitId>,
    /// Journal of every change to a metavariable's place in its class, with the place it had: never consumed, only marked ([`Solutions::held_len`]) and rolled back ([`Solutions::unwind_to`]), as `solved_log` is.
    held_log: Vec<(MetavarId, Held)>,
    /// The literals that gave a metavariable its shape since the last drain. A product is sized where it is held to its metavariable, and the drain reads it again once every field is settled.
    shaped: Vec<ParkedChoice>,
}

impl Solutions {
    pub(crate) fn new() -> Self {
        Self {
            entries: Vec::new(),
            next_metavar: Entropy::<MetavarId>::new(),
            parked: Vec::new(),
            newly_solved: Vec::new(),
            solved_log: Vec::new(),
            oracle: None,
            holding: false,
            imitating: false,
            awaiting: Vec::new(),
            reading: None,
            held_log: Vec::new(),
            shaped: Vec::new(),
        }
    }

    /// Begin reading a term that awaits, inside the one being read. Answers the term and the one to return to, for [`Solutions::leave_awaiting`].
    pub(crate) fn enter_awaiting(&mut self) -> (AwaitId, Option<AwaitId>) {
        let enclosing = self.reading;
        let depth = enclosing.map_or(0, |term| self.awaiting[term.0].depth + 1);
        let term = AwaitId(self.awaiting.len());
        self.awaiting.push(Awaiting {
            depth,
            standing: true,
        });
        self.reading = Some(term);

        (term, enclosing)
    }

    /// End a term that awaits, returning to the one it was read inside. What it still awaited is awaited by none from here on.
    pub(crate) fn leave_awaiting(&mut self, term: AwaitId, enclosing: Option<AwaitId>) {
        self.awaiting[term.0].standing = false;
        self.reading = enclosing;
    }

    /// Read what follows as awaited by `term`, answering what was being read: how a retried problem runs under the term that awaits it, and how the retry returns.
    pub(crate) fn read_under(&mut self, term: Option<AwaitId>) -> Option<AwaitId> {
        mem::replace(&mut self.reading, term)
    }

    /// The head of the class `id` is held in, with the term recorded as awaiting the class and how many metavariables it holds. `None` for an id with no birth record.
    fn class(&self, id: MetavarId) -> Option<(MetavarId, Option<AwaitId>, usize)> {
        let mut id = id;
        loop {
            match self.entry(id)?.held {
                Held::Head { awaited, members } => return Some((id, awaited, members)),
                Held::Under(above) => id = above,
            }
        }
    }

    /// The standing term that awaits `id`, if one does: the one that awaits the class `id` is held in.
    pub(crate) fn awaited(&self, id: MetavarId) -> Option<AwaitId> {
        let (_, awaited, _) = self.class(id)?;

        awaited.filter(|term| self.awaiting[term.0].standing)
    }

    /// The outermost standing term among those that await `ids`.
    pub(crate) fn outermost(&self, ids: impl IntoIterator<Item = MetavarId>) -> Option<AwaitId> {
        ids.into_iter()
            .filter_map(|id| self.awaited(id))
            .min_by_key(|term| self.awaiting[term.0].depth)
    }

    /// Write `id`'s place in its class, journaling the place it had.
    fn hold(&mut self, id: MetavarId, held: Held) {
        if let Some(Some(entry)) = self.entries.get_mut(id.0) {
            self.held_log.push((id, entry.held));
            entry.held = held;
        }
    }

    /// Hold `ids` together: the classes they are held in become one, awaited by the outermost standing term that awaited any of them. The largest keeps its head, of two alike the one with the lower id, so a head is as many steps away as its class's size has binary digits, at most.
    fn link(&mut self, ids: impl IntoIterator<Item = MetavarId>) {
        let classes = ids
            .into_iter()
            .filter_map(|id| self.class(id))
            .map(|(head, _, members)| (head, members))
            .collect::<BTreeMap<_, _>>();
        let Some((&kept, _)) = classes
            .iter()
            .max_by_key(|&(head, members)| (*members, Reverse(*head)))
            .filter(|_| classes.len() > 1)
        else {
            return;
        };
        let awaited = self.outermost(classes.keys().copied());
        let members = classes.values().sum::<usize>();
        for head in classes.into_keys() {
            let held = match head == kept {
                true => Held::Head { awaited, members },
                false => Held::Under(kept),
            };
            self.hold(head, held);
        }
    }

    /// Have the term being read await each class of `ids` that no standing term awaits: the holes a call's head leaves open are the call's, as the implicits it inserts are.
    pub(crate) fn adopt(&mut self, ids: impl IntoIterator<Item = MetavarId>) {
        let Some(reading) = self.reading else {
            return;
        };
        for id in ids {
            if let Some((head, awaited, members)) = self.class(id)
                && !awaited.is_some_and(|term| self.awaiting[term.0].standing)
            {
                let awaited = Some(reading);
                self.hold(head, Held::Head { awaited, members });
            }
        }
    }

    /// The parked checks `term` awaits that have a choice of their own, in the order they were parked, and beside them what each conversion `term` awaits watches: the links by which two of those checks wait on one type. `None` asks for what no standing term awaits, which is the item's.
    pub(crate) fn choices_awaited_by(
        &self,
        term: Option<AwaitId>,
    ) -> (Vec<ParkedChoice>, Vec<BTreeSet<MetavarId>>) {
        let awaited = |parked: &&ParkedProblem| {
            let unsolved = parked
                .watching
                .iter()
                .copied()
                .filter(|id| self.solution(*id).is_none());
            self.outermost(unsolved) == term
        };
        let choices = self
            .parked
            .iter()
            .filter(awaited)
            .filter_map(ParkedProblem::as_choice)
            .collect();
        let links = self
            .parked
            .iter()
            .filter(awaited)
            .filter(|parked| matches!(parked.work, ParkedWork::Conversion(_)))
            .map(|parked| parked.watching.clone())
            .collect();

        (choices, links)
    }

    /// The conversions `term` awaits, in the order they were parked: what a completing term may hold to spines that agree ([`Choice::Spines`]). `None` asks for what no standing term awaits, which is the item's.
    pub(crate) fn conversions_awaited_by(&self, term: Option<AwaitId>) -> Vec<ParkedConversion> {
        self.parked
            .iter()
            .enumerate()
            .filter(|(_, parked)| {
                let unsolved = parked
                    .watching
                    .iter()
                    .copied()
                    .filter(|id| self.solution(*id).is_none());
                self.outermost(unsolved) == term
            })
            .filter_map(|(at, parked)| match &parked.work {
                ParkedWork::Conversion(problem) => Some(ParkedConversion {
                    at,
                    problem: problem.clone(),
                    origin: parked.origin.clone(),
                    frame: parked.frame.clone(),
                }),
                _ => None,
            })
            .collect()
    }

    /// Drop the conversions that stand at `at`, each one a completing term has decided by holding its spines. The places are those [`Solutions::conversions_awaited_by`] answered, read with nothing parked or taken since.
    pub(crate) fn drop_conversions(&mut self, at: &BTreeSet<usize>) {
        let mut place = 0;
        self.parked.retain(|parked| {
            let held = at.contains(&place) && matches!(parked.work, ParkedWork::Conversion(_));
            place += 1;

            !held
        });
    }

    /// The `use` slots nothing has solved whose goals `term` awaits, in the order the goals were parked. `None` asks for what no standing term awaits, which is the item's.
    pub(crate) fn witnesses_awaited_by(&self, term: Option<AwaitId>) -> Vec<MetavarId> {
        self.parked
            .iter()
            .filter_map(|parked| match &parked.work {
                ParkedWork::Witness { slot, .. } if self.solution(*slot).is_none() => {
                    let unsolved = parked
                        .watching
                        .iter()
                        .copied()
                        .filter(|id| self.solution(*id).is_none());
                    (self.outermost(unsolved) == term).then_some(*slot)
                }
                _ => None,
            })
            .collect()
    }

    /// Take the goal parked for the `use` slot `slot`, where it still waits: what a completing term answers itself.
    pub(crate) fn take_witness(&mut self, slot: MetavarId) -> Option<ParkedProblem> {
        let index = self.parked.iter().position(|parked| {
            matches!(&parked.work, ParkedWork::Witness { slot: held, .. } if *held == slot)
        })?;

        Some(self.parked.remove(index))
    }

    /// The check with a choice of its own parked on the bare metavariable `id`, where one is: the field a part of a shape holds while the shape is being given.
    pub(crate) fn choice_waiting_on(&self, id: MetavarId) -> Option<ParkedChoice> {
        self.parked
            .iter()
            .filter_map(ParkedProblem::as_choice)
            .find(
                |parked| matches!(&*parked.expected, Subterm::Metavar(metavar) if metavar.id == id),
            )
    }

    /// Record that the literal of `choice` shaped the metavariable it waited on: what the item's drain holds to the sort that metavariable was declared at ([`Solutions::take_shaped`]).
    pub(crate) fn note_shaped(&mut self, choice: ParkedChoice) {
        self.shaped.push(choice);
    }

    /// Take the literals that shaped a metavariable since the last drain.
    pub(crate) fn take_shaped(&mut self) -> Vec<ParkedChoice> {
        mem::take(&mut self.shaped)
    }

    /// Mint the next metavariable id.
    pub(crate) fn mint(&mut self) -> MetavarId {
        self.next_metavar.fresh()
    }

    /// Raise the minting floor: every id [`Solutions::mint`] hands out will be `>= floor`.
    pub(crate) fn seed_floor(&mut self, floor: usize) {
        self.next_metavar.seed(floor);
    }

    /// Materialize a metavariable's birth record. The store grows to cover `id`; births happen exactly once per id (each `_` is distinct and occurs once). The façade stamps the write.
    pub(crate) fn birth(
        &mut self,
        id: MetavarId,
        telescope: SharedTelescope,
        refinements: SharedRefinements,
        witnesses: Option<Rc<[(Free, Term)]>>,
        result: Term,
    ) {
        self.birth_with_kind(
            id,
            telescope,
            refinements,
            witnesses,
            result,
            MetaKind::Inference,
        );
    }

    /// [`Solutions::birth`] for a recursive-elaboration slot, which only `fill_rec_slot` may solve.
    pub(crate) fn birth_rec_slot(
        &mut self,
        id: MetavarId,
        telescope: SharedTelescope,
        refinements: SharedRefinements,
        result: Term,
    ) {
        self.birth_with_kind(id, telescope, refinements, None, result, MetaKind::RecSlot);
    }

    /// [`Solutions::birth`] for a metavariable of another kind than an inference hole's.
    pub(crate) fn birth_with_kind(
        &mut self,
        id: MetavarId,
        telescope: SharedTelescope,
        refinements: SharedRefinements,
        witnesses: Option<Rc<[(Free, Term)]>>,
        result: Term,
        kind: MetaKind,
    ) {
        if id.0 >= self.entries.len() {
            self.entries.resize_with(id.0 + 1, || None);
        }
        // An id with a birth record is never minted again: minting it would birth a second metavariable over this one's record. Elaboration keeps this without help, since lowering's written ids sit below the floor `seed_floor` raised before any minting — but a birth at an id the counter has not reached yet, which is how a fixture states its metavariables, would leave the counter free to mint it, and re-validation minting one while checking a candidate would replace a test's solved hole with its own.
        self.next_metavar.seed(id.0 + 1);
        self.entries[id.0] = Some(MetaEntry {
            telescope,
            refinements,
            witnesses,
            result,
            solution: None,
            kind,
            proposition: false,
            reduct: None,
            refusal: None,
            dead_arm: None,
            held: Held::Head {
                awaited: self.reading,
                members: 1,
            },
        });
    }

    /// Record that `id`'s `result` is a proposition. Separate from birth because only the implicit-insertion path knows, and it knows after the sort is asked, not at the mint.
    pub(crate) fn mark_proposition(&mut self, id: MetavarId) {
        if let Some(Some(entry)) = self.entries.get_mut(id.0) {
            entry.proposition = true;
        }
    }

    /// Record what `id`'s `result` reduced to, for the report an unsolved one becomes.
    pub(crate) fn note_reduct(&mut self, id: MetavarId, reduct: Term) {
        if let Some(Some(entry)) = self.entries.get_mut(id.0) {
            entry.reduct = Some(reduct);
        }
    }

    /// Record why the procedure that proves a bound from the facts in scope proved nothing, for the report the hole becomes.
    pub(crate) fn note_refusal(&mut self, id: MetavarId, refusal: Refusal) {
        if let Some(Some(entry)) = self.entries.get_mut(id.0) {
            entry.refusal = Some(Rc::new(refusal));
        }
    }

    /// Record why the arm `id` was minted in is never taken, for the report the hole becomes.
    pub(crate) fn note_dead_arm(&mut self, id: MetavarId, dead: Rc<DeadArm>) {
        if let Some(Some(entry)) = self.entries.get_mut(id.0) {
            entry.dead_arm = Some(dead);
        }
    }

    pub(crate) fn entry(&self, id: MetavarId) -> Option<&MetaEntry> {
        self.entries.get(id.0).and_then(Option::as_ref)
    }

    pub(crate) fn solution(&self, id: MetavarId) -> Option<&Term> {
        self.entry(id).and_then(|entry| entry.solution.as_ref())
    }

    pub(crate) fn is_rec_slot(&self, id: MetavarId) -> bool {
        self.entry(id)
            .is_some_and(|entry| entry.kind == MetaKind::RecSlot)
    }

    /// Whether something other than conversion fills `id`: a recursive group its slot, a parked problem its placeholder. Conversion waits on such a hole rather than solve it.
    pub(crate) fn owner_fills(&self, id: MetavarId) -> bool {
        self.entry(id)
            .is_some_and(|entry| matches!(entry.kind, MetaKind::RecSlot | MetaKind::Placeholder))
    }

    /// Commit a solution: sets the entry, records the wake signal, and journals for rollback. The façade stamps the write.
    ///
    /// An unborn id is the caller's broken contract, not a program error: every minting path births immediately, and the conversion solver's no-birth-record arm refuses before committing. A silent no-op here would surface as an unsolved metavariable far from the violation.
    pub(crate) fn solve(&mut self, id: MetavarId, term: Term) {
        let entry = self
            .entries
            .get_mut(id.0)
            .and_then(Option::as_mut)
            .expect("a solved metavariable has a birth record");
        // A solution holds what it mentions to what it solves: whatever could still inform one informs the other.
        let mentioned = match term.has_metavar() {
            true => term.metavars().into_iter().collect::<Vec<_>>(),
            false => Vec::new(),
        };
        entry.solution = Some(term);
        self.link(mentioned.into_iter().chain([id]));
        self.newly_solved.push(id);
        self.solved_log.push(id);
    }

    /// If `term` is a witness-resolution hole, its provenance and the concept goal it stands for (e.g. `Add(Nat)`). A conversion left stuck between two such holes is really an unresolved witness — reported as one rather than as a bare metavariable mismatch between two anonymous placeholders.
    pub(crate) fn witness_hole(&self, term: &Term) -> Option<(WitnessOrigin, Term)> {
        let Subterm::Metavar(metavar) = &**term else {
            return None;
        };
        let MetavarOrigin::Witness(origin) = &metavar.origin else {
            return None;
        };
        let goal = self.entry(metavar.id)?.result.clone();
        Some((origin.clone(), goal))
    }

    /// Resolve a solved metavariable *at its occurrence*: the stored solution is spelled with the birth telescope's names, and the occurrence's spine records what each of those binders corresponds to here — so resolution is the solution with birth names rewritten through the spine. An empty spine (a never-rebuilt `into_core` hole) resolves as the identity. `None` while unsolved.
    pub(crate) fn resolve(&self, metavar: &Metavar) -> Option<Term> {
        let entry = self.entry(metavar.id)?;
        let solution = entry.solution.as_ref()?;

        Some(Self::at_occurrence(entry, metavar, solution))
    }

    /// The type of a metavariable *at its occurrence*: the type it was born at, read through the occurrence's spine as [`Solutions::resolve`] reads its solution. `None` for an id with no birth record.
    pub(crate) fn type_at(&self, metavar: &Metavar) -> Option<Term> {
        let entry = self.entry(metavar.id)?;

        Some(Self::at_occurrence(entry, metavar, &entry.result))
    }

    /// `term`, spelled with the names of `entry`'s birth telescope, as it reads at the occurrence `metavar`.
    fn at_occurrence(entry: &MetaEntry, metavar: &Metavar, term: &Term) -> Term {
        if metavar.spine.is_empty() {
            return term.clone();
        }

        let binders = entry
            .telescope
            .iter()
            .map(|(name, _)| name)
            .collect::<Vec<_>>();

        let spine = metavar.spine.iter().collect::<Vec<_>>();

        term.capture(&binders).release(&spine)
    }

    /// How many solutions have been committed so far — the term half of `Context::solution_mark`'s watermark.
    pub(crate) fn solved_len(&self) -> usize {
        self.solved_log.len()
    }

    /// How many times a metavariable's place in its class has been written so far: the watermark [`Solutions::unwind_to`] restores the classes to. A count of solutions would not serve, a parked problem linking with none committed.
    pub(crate) fn held_len(&self) -> usize {
        self.held_log.len()
    }

    /// The solutions committed since `solved_len` that still stand, each with its id.
    pub(crate) fn solved_since(&self, solved_len: usize) -> Vec<(MetavarId, Term)> {
        self.solved_log[solved_len.min(self.solved_log.len())..]
            .iter()
            .filter_map(|id| self.solution(*id).map(|solution| (*id, solution.clone())))
            .collect()
    }

    /// Replace a standing solution with `term`, a term it already denotes, logging no solve: nothing parked on it wakes, and a rollback past its first commitment unwinds it as before.
    pub(crate) fn restamp(&mut self, id: MetavarId, term: Term) {
        if let Some(Some(entry)) = self.entries.get_mut(id.0)
            && entry.solution.is_some()
        {
            entry.solution = Some(term);
        }
    }

    /// Unwind every solution committed since the watermark and every link made since, removing the unwound ids from the wake signals. The façade clears the caches.
    pub(crate) fn unwind_to(&mut self, solved_len: usize, held_len: usize) {
        let unwound = self
            .solved_log
            .split_off(solved_len.min(self.solved_log.len()));

        for id in &unwound {
            curios_profile::note!(target: "curios_elab::solve", meta = id.0, "solution unwound");
            if let Some(Some(entry)) = self.entries.get_mut(id.0) {
                entry.solution = None;
            }
        }

        self.newly_solved.retain(|id| !unwound.contains(id));

        let relinked = self.held_log.split_off(held_len.min(self.held_log.len()));
        for (id, before) in relinked.into_iter().rev() {
            if let Some(Some(entry)) = self.entries.get_mut(id.0) {
                entry.held = before;
            }
        }
    }

    /// Park work under its frozen frame, recomputing the watch set from the work's currently unsolved metavariables.
    pub(crate) fn park(&mut self, work: ParkedWork, origin: Term, frame: FrozenFrame) {
        let watching = match &work {
            // Its type as well as its sides: a goal postponed because the type it is compared at is unsolved wakes when that type is.
            ParkedWork::Conversion(goal) => self.unsolved_in([&goal.this, &goal.that, &goal.type_]),
            ParkedWork::Checking { expected, .. } => self.unsolved_in([expected]),
            // Its slot as well as its goal: unification may solve the slot, which ends the goal, and the slot is what a term awaits where the goal holds no metavariable.
            ParkedWork::Witness { slot, goal, .. } => {
                let mut watching = self.unsolved_in([goal]);
                if self.solution(*slot).is_none() {
                    watching.insert(*slot);
                }
                watching
            }
            ParkedWork::Discharge { bound, .. } => self.unsolved_in([bound]),
        };

        // A parked problem holds together what it watches: a solution for one may come by another.
        self.link(watching.iter().copied());

        self.parked.push(ParkedProblem {
            work,
            origin,
            frame,
            watching,
        });
    }

    /// The unsolved metavariables `terms` wait on: the ones they mention, and the ones the solutions of the solved ones they mention come down to. A problem parked again keeps the spelling it was first parked under, and a metavariable of that spelling may since have been solved to another, re-expressed in a context that contains its solver's; read by mention alone, such a problem would watch nothing, and nothing would wake it or await it.
    fn unsolved_in<'a>(&self, terms: impl IntoIterator<Item = &'a Term>) -> BTreeSet<MetavarId> {
        let mut unsolved = BTreeSet::new();
        let mut seen = BTreeSet::new();
        let mut pending = terms
            .into_iter()
            .flat_map(|term| term.metavars())
            .collect::<Vec<_>>();
        while let Some(id) = pending.pop() {
            if !seen.insert(id) {
                continue;
            }
            match self.solution(id) {
                Some(solution) => pending.extend(solution.metavars()),
                None => {
                    unsolved.insert(id);
                }
            }
        }

        unsolved
    }

    /// The numerals parked on each bare metavariable, by the metavariable they wait on now: what a report made before their term completes shows that metavariable as ([`numeral_default`](crate::numeral_default)).
    pub(crate) fn waiting_numerals(&self) -> BTreeMap<MetavarId, Vec<NumLit>> {
        let mut waiting = BTreeMap::<MetavarId, Vec<NumLit>>::new();
        for parked in &self.parked {
            if let ParkedWork::Checking { term, expected, .. } = &parked.work
                && let Subterm::Transient(Transient::NumLit(literal)) = &**term
                && matches!(&**expected, Subterm::Metavar(_))
                && let [id] = parked.watching.iter().copied().collect::<Vec<_>>()[..]
            {
                waiting.entry(id).or_default().push(literal.clone());
            }
        }

        waiting
    }

    /// Take the parked goals woken by solutions landed since the last sweep. Consumes the wake signals; empty when nothing new has been solved.
    pub(crate) fn wake_parked(&mut self) -> Vec<ParkedProblem> {
        if self.newly_solved.is_empty() || self.parked.is_empty() {
            self.newly_solved.clear();

            return Vec::new();
        }

        let solved = self.newly_solved.drain(..).collect::<BTreeSet<_>>();

        let (woken, kept) = mem::take(&mut self.parked)
            .into_iter()
            .partition(|parked| parked.watching.iter().any(|id| solved.contains(id)));

        self.parked = kept;

        woken
    }

    /// Take every parked goal — the drain's final sweep.
    pub(crate) fn take_parked(&mut self) -> Vec<ParkedProblem> {
        mem::take(&mut self.parked)
    }

    /// Take the checking problem parked behind `placeholder`, where it still waits: what a completing term runs itself, out of reach of the wakes its own check sets off.
    pub(crate) fn take_checking(&mut self, placeholder: MetavarId) -> Option<ParkedProblem> {
        let index = self.parked.iter().position(|parked| {
            matches!(&parked.work, ParkedWork::Checking { placeholder: held, .. } if *held == placeholder)
        })?;

        Some(self.parked.remove(index))
    }

    pub(crate) fn parked_len(&self) -> usize {
        self.parked.len()
    }

    pub(crate) fn has_newly_solved(&self) -> bool {
        !self.newly_solved.is_empty()
    }

    pub(crate) fn parking_suppressed(&self) -> bool {
        self.oracle.is_some()
    }

    /// Whether conversion may take what two spines would solve: a completing term is holding them, and no oracle has been entered since, whose question is no part of that term's.
    pub(crate) fn holds_spines(&self) -> bool {
        self.holding && self.oracle.is_none()
    }

    /// Whether conversion may imitate an application of an unsolved metavariable against a type former's node: a completing term is holding spines, the imitation being one more answer they give, or a `!` is reading its region's monad, which is all the region has to say.
    pub(crate) fn imitates(&self) -> bool {
        self.imitating || self.holds_spines()
    }

    /// Begin or end reading a region's monad, answering what stood before: [`Context::imitating`](crate::Context::imitating)'s bracket.
    pub(crate) fn set_imitating(&mut self, imitating: bool) -> bool {
        mem::replace(&mut self.imitating, imitating)
    }

    /// Begin or end holding spines, answering what stood before: [`Context::holding_spines`](crate::Context::holding_spines)'s bracket.
    pub(crate) fn set_holding(&mut self, holding: bool) -> bool {
        mem::replace(&mut self.holding, holding)
    }

    /// A site wants to park. Answers whether it may, and inside an oracle, where it may not, records that it wanted to.
    pub(crate) fn may_park(&mut self) -> bool {
        match &mut self.oracle {
            Some(declined) => {
                *declined = true;
                false
            }
            None => true,
        }
    }

    /// Whether a site inside the oracle in progress wanted to park.
    pub(crate) fn declined_to_park(&self) -> bool {
        self.oracle == Some(true)
    }

    /// Enter an oracle with nothing declined yet, returning the enclosing state for [`Solutions::leave_oracle`] — `Context::with_suppressed_parking`'s bracket intrinsic.
    pub(crate) fn enter_oracle(&mut self) -> Option<bool> {
        self.oracle.replace(false)
    }

    /// Leave an oracle for the state it was entered from. What the inner one declined is its own reader's to act on, and is not the enclosing oracle's record.
    pub(crate) fn leave_oracle(&mut self, enclosing: Option<bool>) {
        self.oracle = enclosing;
    }
}
