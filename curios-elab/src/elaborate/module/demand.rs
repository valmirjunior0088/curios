//! The order a unit's declarations elaborate in: each when it is asked for, after everything it reads.
//!
//! **The declaration is the unit of work.** A declaration is an item with every item the lowering generated from it — a type former with its constructors, a concept with its method wrappers. Its items share what one written declaration shares: the universe levels of its written types, which the lowering mints once and copies, and the registry entry the former rebuilds and a constructor is finalized against. So they are attempted together, in item order, in one state, and are published together.
//!
//! **A declaration elaborates after what it reads, wherever each was written.** The walk asks for the declarations in the order they are written, which is the order the lowering hands them on in, and asking for one first asks for every declaration its items' lowered forms reach. What it turns out to read beyond that — the witness a goal resolves to, a registry entry no name it writes reaches — it meets while it elaborates: the read records the declaration it needed ([`Context::need`]), and an attempt that recorded one is void whatever it went on to conclude. A void attempt is undone as a refused item is, poisoning nothing; what it needed is elaborated; and the attempt is made again. No attempt is suspended to run another inside it, so one context serves them all.
//!
//! **The prover's vocabulary is asked for first.** A proof the elaborator writes applies lemmas no declaration names, so nothing a declaration writes says it reads them. The walk therefore begins with the declarations of the vocabulary the unit declares, which brings in every declaration they read, with no lemma of the unit on offer — and, compiled over a baseline, with the declarations the vocabulary reads through an item the baseline holds, which no walk asks for; and only then offers them ([`Context::offer_lemmas`]) and asks for the rest. A declaration the vocabulary reads is thereby proved without it, every other finds all of it elaborated, and no proof makes one declaration wait on another.
//!
//! **Over a baseline, a declaration is kept where everything it asked is answered as it was.** A unit compiled over an earlier text of itself is walked whole, and a declaration the lowering kept is not attempted at once: the questions its elaboration put to the declarations around it are asked again, in the order it asked them ([`Traces`]), each after the declaration it is about has been asked for. Where every one is answered as it was, the declaration is the baseline's, and the baseline answers for it from there on ([`Established::keep`]); at the first that is not, it is attempted as any other. A question about a declaration that was itself kept is answered as it was; one about a declaration elaborated again is answered as it was where the part asked came out equal, so an edit redoes what read the part it changed and nothing behind that.
//!
//! **A cycle is refused by its members.** A declaration asked for while it is being asked for closes a cycle, through a name it wrote or a witness a goal of its resolves to alike. Every member is refused, the report is raised once, at the first member in source order, naming them all, and what reaches a member is withheld as a dependent of any refusal is.

use {
    super::{
        Elaborated, Holding, ItemMark, ItemStamp, Poison, Survivors, elaborate_module_item,
        withhold,
    },
    crate::{
        Answered, Context, Error, Established, GoalSite, Question, Same, Traces, concepts_alike,
        entries_alike, inducts_alike, structs_alike,
    },
    curios_core::{
        DefinitionKind, Enter, Extent, Global, Item, Module, Nominal, Places, Term, Trace,
    },
    curios_utilities::recurse,
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet, HashMap, HashSet},
        ops::ControlFlow,
        rc::Rc,
    },
};

#[cfg(feature = "archive")]
use crate::Foreign;

/// Where one declaration stands in the walk.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Standing {
    Pending,
    /// Asked for and not finished: asking again closes a cycle.
    Demanded,
    /// Kept, refused or withheld.
    Finished,
}

/// The declarations a demand found on a cycle, in the order they were asked for.
pub(super) struct Cycle(Vec<usize>);

/// What one attempt at a declaration made of each of its items, held until the attempt is known to stand.
enum Outcome {
    /// The item as it was published, with the erasure obligations it breaks.
    Kept(Item, Vec<Error>),
    /// The item as it elaborated and where it holds the goals it wrote, which are reported in its place.
    Held(Item, Vec<GoalSite>),
    Refused(Error),
    Withheld,
}

/// The baseline a unit is compiled over, as its walk reads it: an earlier text of the unit as it elaborated, what each of its declarations asked, and the names the new text's lowering kept.
#[derive(Clone, Copy)]
pub(super) struct Over<'a> {
    pub(super) baseline: &'a Module,
    pub(super) places: &'a Places,
    pub(super) traces: &'a Traces,
    /// The names whose lowered item is the baseline's own (`PreparedText::kept_from`).
    pub(super) lowered: &'a BTreeSet<Global>,
    /// The scope, whose baseline layer answers for a declaration from where the walk keeps it.
    pub(super) established: &'a Established<'a>,
}

/// The walk over one unit's declarations.
pub(super) struct Demand<'a> {
    module: &'a Module,
    /// The positions of each declaration's items, the item a written declaration lowers to first and the items generated from it after, in item order.
    declarations: Vec<Vec<usize>>,
    /// The declaration declaring each name. Never iterated, and asked at every question a kept declaration's trace holds.
    owner: HashMap<Global, usize>,
    standing: Vec<Standing>,
    /// The declarations being asked for, outermost first.
    demanded: Vec<usize>,
    pub(super) poison: Poison,
    pub(super) survivors: Survivors,
    /// What each declaration's standing attempt asked of the declarations around it, and what each kept declaration's did where the baseline elaborated it.
    pub(super) traces: Traces,
    /// The baseline the unit is compiled over, where it is compiled over one.
    over: Option<Over<'a>>,
    /// The declarations kept as the baseline holds them.
    kept: Vec<bool>,
    /// What each name elaborated again over a baseline left as the baseline holds it.
    same: HashMap<Global, Same>,
    /// Whether each part a kept declaration asked of a declaration the unit does not hold, in a scope that moved, is the part the baseline read: asked again once, a part being asked by every declaration that read it.
    read: RefCell<HashMap<Global, Vec<(Question, bool)>>>,
}

/// Whether the part `question` asks, of a declaration the unit does not hold, digests now to what the baseline recorded of it. A part the baseline recorded nothing of does not stand, and neither does one that will not digest.
#[cfg(feature = "archive")]
fn read_again(over: Over<'_>, question: &Question) -> bool {
    match (
        over.traces.foreign().digest(question),
        over.established.part_digest(question),
    ) {
        (Some(then), Ok(now)) => *then == now,
        _ => false,
    }
}

/// Nothing digests where nothing is archived, so no read of a unit that moved stands.
#[cfg(not(feature = "archive"))]
fn read_again(_: Over<'_>, _: &Question) -> bool {
    false
}

/// The first position written anywhere in `term`, outermost first, each node of its graph visited once.
///
/// Where a report about a declaration is placed: the position of a declaration's label does not reach Core, and a signature the lowering synthesized around what was written, a function sugar's, has none of its own.
fn first_written(term: &Term) -> Option<Extent> {
    let found = term.try_walk(
        &mut HashSet::new(),
        |seen: &mut HashSet<Term>, term| match term.extent() {
            Some(extent) => ControlFlow::Break(extent),
            None if seen.insert(term.clone()) => ControlFlow::Continue(Enter::Descend),
            None => ControlFlow::Continue(Enter::Skip(())),
        },
        |_, _, _| (),
    );

    match found {
        ControlFlow::Break(extent) => Some(extent),
        ControlFlow::Continue(()) => None,
    }
}

/// The kind the lowering stamped an item with: a group's is its first member's, every member of one being generated alike.
fn introduced(item: &Item) -> &DefinitionKind {
    match item {
        Item::Let(definition) => &definition.kind,
        Item::Rec(rec) => &rec.definitions[0].kind,
    }
}

impl<'a> Demand<'a> {
    pub(super) fn new(
        module: &'a Module,
        poison: Poison,
        survivors: Survivors,
        over: Option<Over<'a>>,
    ) -> Self {
        curios_profile::profile!("demand::new");
        // A generated item names the declaration it was generated from, which is an item of the module wherever the two sit.
        let written = module
            .items
            .iter()
            .enumerate()
            .flat_map(|(position, item)| {
                item.declared_names()
                    .into_iter()
                    .map(move |name| (*name, position))
            })
            .collect::<BTreeMap<_, _>>();
        let mut place = BTreeMap::new();
        let mut declarations = Vec::<Vec<usize>>::new();
        let mut generated = Vec::new();
        for (position, item) in module.items.iter().enumerate() {
            match introduced(item) {
                DefinitionKind::InductiveConstructor { owner, .. }
                | DefinitionKind::ConceptMethod { owner } => {
                    generated.push((position, Global::Authored(*owner)));
                }
                _ => {
                    place.insert(position, declarations.len());
                    declarations.push(vec![position]);
                }
            }
        }
        for (position, owner) in generated {
            match written.get(&owner).and_then(|item| place.get(item)) {
                Some(&declaration) => declarations[declaration].push(position),
                // Generated from a declaration this module does not hold: a recompile's closure may take a generated item without its declaration only where it takes neither, so this is an item of its own.
                None => declarations.push(vec![position]),
            }
        }
        let owner = declarations
            .iter()
            .enumerate()
            .flat_map(|(declaration, items)| {
                items.iter().flat_map(move |item| {
                    module.items[*item]
                        .declared_names()
                        .into_iter()
                        .map(move |name| (*name, declaration))
                })
            })
            .collect();

        Self {
            module,
            standing: vec![Standing::Pending; declarations.len()],
            kept: vec![false; declarations.len()],
            declarations,
            owner,
            demanded: Vec::new(),
            poison,
            survivors,
            traces: Traces::default(),
            over,
            same: HashMap::new(),
            read: RefCell::new(HashMap::new()),
        }
    }

    /// Record what the unit's declarations read of the declarations it does not hold, each part by the digest it has in `scope`: what a later compile over this unit asks a read of another unit again by ([`Foreign`]).
    ///
    /// Taken where the walk ends, off the traces that stood. A part a kept declaration read of a unit that did not move is the baseline's, digest and all, so nothing of that unit is read to record it; and a walk that kept every declaration of its baseline in a scope that did not move holds the baseline's table.
    #[cfg(feature = "archive")]
    pub(super) fn note_foreign(&mut self, scope: &Established<'_>) {
        let earlier = self.over.map(|over| over.traces);
        if let Some(earlier) = earlier
            && scope.settled()
            && self.kept.iter().all(|kept| *kept)
            && self.traces.len() == earlier.len()
        {
            self.traces.read(Rc::clone(earlier.foreign()));
            return;
        }

        let mut read = Foreign::default();
        let mut noted = HashMap::<Global, Vec<Question>>::new();
        for trace in self.traces.each() {
            for (question, answered) in trace.asked() {
                // A witness found is read by its signature, which its own question does not ask.
                let part = match (question, answered) {
                    (Question::Witness(..), Answered::Witness(Some(witness))) => {
                        Question::Signature(*witness)
                    }
                    (part, Answered::Part) => *part,
                    _ => continue,
                };
                let Some(name) = part.subject() else {
                    continue;
                };
                if self.owner.contains_key(name) || !scope.recorded(name) {
                    continue;
                }
                let asked = noted.entry(*name).or_default();
                if asked.contains(&part) {
                    continue;
                }
                asked.push(part);

                let kept = earlier
                    .and_then(|earlier| earlier.foreign().digest(&part))
                    .filter(|_| scope.unmoved(name));
                let digest = match kept {
                    Some(digest) => digest.clone(),
                    None => match scope.part_digest(&part) {
                        Ok(digest) => digest,
                        // A part that will not digest is recorded nowhere, and a read with no record does not stand.
                        Err(_) => continue,
                    },
                };
                read.record(part, digest);
            }
        }
        self.traces.read(Rc::new(read));
    }

    /// Whether the part `question` asks of `name`, a declaration the unit does not hold, is the part the baseline read: the unit that holds it is the one the baseline was compiled after, or the part digests now to what the baseline recorded of it.
    fn read_stands(&self, over: Over<'_>, name: &Global, question: &Question) -> bool {
        if over.established.unmoved(name) {
            return true;
        }
        let asked = self
            .read
            .borrow()
            .get(name)
            .and_then(|asked| asked.iter().find(|(asked, _)| asked == question))
            .map(|(_, stands)| *stands);
        if let Some(stands) = asked {
            return stands;
        }

        let stands = read_again(over, question);
        self.read
            .borrow_mut()
            .entry(*name)
            .or_default()
            .push((*question, stands));

        stands
    }

    /// Every name of a declaration the walk kept as the baseline holds it.
    pub(super) fn kept_names(&self) -> BTreeSet<Global> {
        self.kept
            .iter()
            .enumerate()
            .filter(|(_, kept)| **kept)
            .flat_map(|(declaration, _)| self.names(declaration))
            .copied()
            .collect()
    }

    /// Elaborate every declaration, each after what it reads: the declarations of `vocabulary` and what they read first, then the rest with the unit's lemmas on offer.
    pub(super) fn all(&mut self, context: &mut Context, vocabulary: BTreeSet<Global>) {
        for declaration in self.owners(vocabulary, usize::MAX) {
            let closed = self.declaration(context, declaration);
            assert!(
                closed.is_ok(),
                "a cycle is closed by the member asked for first, which nothing here asked for"
            );
        }
        context.offer_lemmas();

        // In the order the declarations are written, which is their first items'.
        let mut order = (0..self.declarations.len()).collect::<Vec<_>>();
        order.sort_by_key(|declaration| self.declarations[*declaration][0]);
        for declaration in order {
            let closed = self.declaration(context, declaration);
            assert!(
                closed.is_ok(),
                "a cycle is closed by the member asked for first, which nothing here asked for"
            );
        }
    }

    /// The names every item of `declaration` declares.
    fn names(&self, declaration: usize) -> Vec<&'a Global> {
        let module = self.module;
        self.declarations[declaration]
            .iter()
            .flat_map(|item| module.items[*item].declared_names())
            .collect()
    }

    /// Ask for `declaration`: elaborate it unless it is finished, and hand back the cycle where it is already being asked for.
    fn declaration(&mut self, context: &mut Context, declaration: usize) -> Result<(), Cycle> {
        match self.standing[declaration] {
            Standing::Finished => return Ok(()),
            Standing::Demanded => return Err(self.cycle_through(declaration)),
            Standing::Pending => {}
        }

        self.standing[declaration] = Standing::Demanded;
        self.demanded.push(declaration);
        let settled = recurse(|| self.settle(context, declaration));
        self.demanded.pop();

        self.close(context, declaration, settled)
    }

    /// Finish `declaration`, refusing it where what it asked for led back to it.
    fn close(
        &mut self,
        context: &mut Context,
        declaration: usize,
        settled: Result<(), Cycle>,
    ) -> Result<(), Cycle> {
        let module = self.module;
        let outcome = match settled {
            Ok(()) => Ok(()),
            Err(cycle) => {
                for item in &self.declarations[declaration] {
                    let item = &module.items[*item];
                    withhold(context, item);
                    self.poison.declare(item);
                }
                let first = |member: &usize| self.declarations[*member][0];
                if cycle.0.iter().map(first).min() == Some(first(&declaration)) {
                    let members = cycle
                        .0
                        .iter()
                        .map(first)
                        .collect::<BTreeSet<_>>()
                        .into_iter()
                        .map(|member| &module.items[member])
                        .collect::<Vec<_>>();
                    let item = &module.items[first(&declaration)];
                    // Placed in the first member as written: at its signature, or in it where the lowering synthesized the signature around what was written.
                    let site = item.definitions().first().and_then(|definition| {
                        first_written(&definition.type_).or_else(|| first_written(&definition.body))
                    });
                    let error = Error::declaration_cycle(&members)
                        .at_opt(site)
                        .in_declaration(&item.describe(), item.declared_names().first().copied());
                    self.survivors.refuse(ItemStamp(first(&declaration)), error);
                }
                // The cycle is closed by the member asked for first: what asked for that one reads a refusal, as it would any other.
                match cycle.0.first() == Some(&declaration) {
                    true => Ok(()),
                    false => Err(cycle),
                }
            }
        };
        self.standing[declaration] = Standing::Finished;
        context.finish(&self.names(declaration));

        outcome
    }

    /// The declarations being asked for from `declaration`'s demand on: the cycle a second demand of it closes.
    fn cycle_through(&self, declaration: usize) -> Cycle {
        let from = self
            .demanded
            .iter()
            .position(|demanded| *demanded == declaration)
            .expect("a declaration being asked for is among those asked for");

        Cycle(self.demanded[from..].to_vec())
    }

    /// The declarations declaring `names`, but for `declaration` itself, in the order they are written.
    fn owners(&self, names: impl IntoIterator<Item = Global>, declaration: usize) -> Vec<usize> {
        let mut owners = names
            .into_iter()
            .filter_map(|name| self.owner.get(&name).copied())
            .filter(|owner| *owner != declaration)
            .collect::<BTreeSet<_>>()
            .into_iter()
            .collect::<Vec<_>>();
        owners.sort_by_key(|owner| self.declarations[*owner][0]);

        owners
    }

    /// Ask for the declaration declaring `name`, where the unit declares it in another declaration than `declaration`.
    fn ask(
        &mut self,
        context: &mut Context,
        name: &Global,
        declaration: usize,
    ) -> Result<(), Cycle> {
        match self.owner.get(name) {
            Some(&owner) if owner != declaration => self.declaration(context, owner),
            _ => Ok(()),
        }
    }

    /// Ask for every declaration the lowered forms of `declaration`'s items reach, in the order they are written.
    fn reached(&mut self, context: &mut Context, declaration: usize) -> Result<(), Cycle> {
        let module = self.module;
        let dependencies = {
            curios_profile::profile!("demand::reached");
            let reached = self.declarations[declaration]
                .iter()
                .flat_map(|item| module.reaches(&module.items[*item]))
                .collect::<Vec<_>>();

            self.owners(reached, declaration)
        };
        for dependency in dependencies {
            self.declaration(context, dependency)?;
        }

        Ok(())
    }

    /// Whether the baseline holds a trace of `declaration` under a lowering the new text kept: what [`Demand::recorded`] asks first, and all that is asked before anything the declaration reaches is asked for.
    fn traced(&self, declaration: usize) -> bool {
        self.over.is_some_and(|over| {
            let names = self.names(declaration);

            names.iter().all(|name| over.lowered.contains(*name))
                && over.traces.held(names[0]).is_some()
        })
    }

    /// What `declaration` asked where the baseline elaborated it, where the walk may keep it by that: the lowering kept every item of it, the baseline holds each under its name, nothing it reaches is refused, and a witness it declares is alone at its key.
    ///
    /// **A witness is kept where it would register.** A witness registers under its key as its signature elaborates, and the second to register at one key is refused; a kept one registers nothing, so it is asked here what registering would have: that it is the first the unit declares at its key, and that none stands there yet. One that is not is attempted, and refused where a whole compile refuses it.
    fn recorded(
        &self,
        context: &Context,
        declaration: usize,
    ) -> Option<Rc<Trace<Question, Answered>>> {
        let over = self.over?;
        let module = self.module;
        let names = self.names(declaration);
        let kept = names
            .iter()
            .all(|name| over.lowered.contains(*name) && over.places.place(name).is_some());
        let poisoned = self.declarations[declaration]
            .iter()
            .any(|item| self.poison.reaches(module, &module.items[*item]));
        let alone = names
            .iter()
            .all(|name| match context.spelled_witness(name) {
                Some(Some((concept, key))) => {
                    context.witness_pending(concept, key) == Some(**name)
                        && context.witness_standing(concept, key).is_none()
                }
                Some(None) => false,
                None => true,
            });
        if !(kept && alone) || poisoned {
            return None;
        }

        over.traces.held(names[0]).cloned()
    }

    /// Whether every question of `trace`, `declaration`'s where the baseline elaborated it, is answered as it was, each asked after the declaration it is about: the first that is not ends the asking.
    ///
    /// **Asked again as it was asked.** A read of a declaration's part waited for the declaration, and is asked again after it; one that waited for nothing — whether a witness is declared at a key, whether lemmas are on offer — is asked of the walk as it stands. So asking a trace again asks for no declaration its elaboration did not wait for, and closes no cycle that elaboration would not.
    fn stands(
        &mut self,
        context: &mut Context,
        declaration: usize,
        trace: &Trace<Question, Answered>,
    ) -> Result<bool, Cycle> {
        for (question, answered) in trace.asked() {
            match question {
                // Asked of the unit as it stands, and waiting for no declaration.
                Question::Lemmas | Question::WitnessDeclared(..) => {}
                // A goal waits for the witness the unit declares at its key, and reads nothing of the concept it is keyed under.
                Question::Witness(concept, key) => {
                    if let Some(declared) = context.witness_pending(concept, key) {
                        self.ask(context, &declared, declaration)?;
                    }
                    if let Answered::Witness(Some(found)) = answered {
                        self.ask(context, found, declaration)?;
                    }
                }
                part => {
                    if let Some(subject) = part.subject() {
                        self.ask(context, subject, declaration)?;
                    }
                }
            }
            if !self.answered(context, question, answered) {
                return Ok(false);
            }
        }

        Ok(true)
    }

    /// Whether `question` is answered `answered` now, every declaration it is about having finished.
    fn answered(&self, context: &Context, question: &Question, answered: &Answered) -> bool {
        match question {
            Question::Lemmas => *answered == Answered::Whether(context.offers_lemmas()),
            Question::WitnessDeclared(concept, key) => {
                *answered == Answered::Whether(context.witness_stands(concept, key))
            }
            // The witness found is read by its signature, which the table holds beside its name.
            Question::Witness(concept, key) => {
                let found = context.witness_standing(concept, key);

                *answered == Answered::Witness(found)
                    && found.is_none_or(|witness| self.held(&Question::Signature(witness)))
            }
            part => self.held(part),
        }
    }

    /// Whether the part of a declaration that `question` asks is as the baseline holds it: a kept declaration's is, one elaborated again where it came out equal, and one neither the unit nor the baseline declares is another unit's or no one's, which stands where the baseline read what is read now ([`Self::read_stands`]).
    fn held(&self, question: &Question) -> bool {
        let over = self.over.expect("a trace is asked again over a baseline");
        let Some(name) = question.subject() else {
            return false;
        };

        match self.owner.get(name) {
            None => over.places.place(name).is_none() && self.read_stands(over, name, question),
            Some(&owner) if self.kept[owner] => true,
            Some(_) => self
                .same
                .get(name)
                .is_some_and(|same| same.answers(question)),
        }
    }

    /// Keep `declaration` as the baseline holds it: what the unit seeded for it is taken out, so a read of it falls through to the baseline, which answers for it from here on, and its trace is the baseline's own. No item is made for it: the unit takes the baseline's where it is assembled.
    fn keep(
        &mut self,
        context: &mut Context,
        declaration: usize,
        trace: Rc<Trace<Question, Answered>>,
    ) {
        let over = self.over.expect("a declaration is kept over a baseline");
        let names = self.names(declaration);
        for name in &names {
            context.remove_induct(name);
            context.remove_struct(name);
            context.remove_concept(name);
        }
        over.established.keep(&names);
        self.traces.share(*names[0], trace);
        self.kept[declaration] = true;
    }

    /// Note which parts of `made`, an item elaborated again over a baseline, came out as the baseline holds them.
    fn compare(&mut self, context: &Context, made: &Item) {
        let Some(over) = self.over else {
            return;
        };
        for (member, name) in made.declared_names().into_iter().enumerate() {
            let Some(place) = over.places.place(name) else {
                continue;
            };
            let held = &over.baseline.items[place.item()];
            // The baseline's entry rides in the former that declares it.
            let entry = held.nominal(name);
            let same = Same::of(made, held, member).keying(
                entries_alike(
                    context.induct_decl(name),
                    entry.and_then(Nominal::inductive),
                    inducts_alike,
                ),
                entries_alike(
                    context.struct_decl(name),
                    entry.and_then(Nominal::record),
                    structs_alike,
                ),
                entries_alike(
                    context.concept(name),
                    entry.and_then(Nominal::concept),
                    concepts_alike,
                ),
            );
            self.same.insert(*name, same);
        }
    }

    /// Elaborate `declaration` after what it reads: kept as its baseline holds it where everything it asked there is answered as it was, and otherwise attempt by attempt, until one reads nothing unfinished.
    fn settle(&mut self, context: &mut Context, declaration: usize) -> Result<(), Cycle> {
        let module = self.module;
        let items = self.declarations[declaration].clone();
        let names = self.names(declaration);
        // A declaration asked again by its trace asks for each declaration its elaboration waited for as its questions come to it, so what its lowered forms reach is asked for ahead of it only where that could be told apart: ahead of the offer of the unit's lemmas, which side of the offer a declaration is first asked for on being what one of its questions is answered by.
        let traced = context.offers_lemmas() && self.traced(declaration);
        if !traced {
            self.reached(context, declaration)?;
        }

        if let Some(trace) = self.recorded(context, declaration)
            && self.stands(context, declaration, &trace)?
        {
            curios_profile::sample!("demand::kept", trace.len());
            self.keep(context, declaration, trace);
            return Ok(());
        }
        if traced {
            self.reached(context, declaration)?;
        }

        loop {
            // One attempt at every item, in one state. What it makes of each is held until it is known to stand: a void attempt refused nothing and poisoned nothing.
            let mark = ItemMark::begin(context);
            context.attempt(&names);
            let mut poison = self.poison.clone();
            let mut outcomes = Vec::with_capacity(items.len());
            for position in &items {
                let item = &module.items[*position];
                if poison.reaches(module, item) {
                    poison.declare(item);
                    outcomes.push(Outcome::Withheld);
                    continue;
                }
                let member = ItemMark::begin(context);
                match elaborate_module_item(context, item) {
                    Ok(Elaborated::Published { item, obligations }) => {
                        outcomes.push(Outcome::Kept(item, obligations));
                    }
                    Ok(Elaborated::Held {
                        item: elaborated,
                        sites,
                        published,
                    }) => {
                        // An item that could not be published by its types alone is taken out as a refused one is, and what reaches it is withheld.
                        if !published && context.needs_nothing() {
                            member.undo(context, &item.declared_names());
                            poison.declare(item);
                        }
                        outcomes.push(Outcome::Held(elaborated, sites));
                    }
                    Err(error) => {
                        if context.needs_nothing() {
                            member.undo(context, &item.declared_names());
                            poison.declare(item);
                        }
                        outcomes.push(Outcome::Refused(error));
                    }
                }
                if !context.needs_nothing() {
                    break;
                }
            }

            let needs = context.take_needs();
            if needs.is_empty() {
                // What the attempt that stands asked of the declarations around it is its declaration's.
                let trace = context.take_trace();
                curios_profile::sample!("demand::asked", trace.len());
                self.traces.file(*names[0], trace);
                self.poison = poison;
                let mut holding = Vec::new();
                for (position, outcome) in items.iter().zip(outcomes) {
                    match outcome {
                        Outcome::Kept(elaborated, obligations) => {
                            self.compare(context, &elaborated);
                            self.survivors
                                .keep(ItemStamp(*position), elaborated, obligations);
                        }
                        Outcome::Held(elaborated, sites) => holding.push((elaborated, sites)),
                        Outcome::Refused(error) => {
                            self.survivors.refuse(ItemStamp(*position), error);
                        }
                        Outcome::Withheld => withhold(context, &module.items[*position]),
                    }
                }
                // The goals are known in this attempt's state alone, which is kept for their report.
                if !holding.is_empty() {
                    self.survivors.hold(Holding {
                        state: context.set_aside(),
                        items: holding,
                    });
                }
                return Ok(());
            }

            curios_profile::sample!("demand::void", needs.len());
            mark.void(context, module, &names);
            let dependencies = self.owners(needs, declaration);
            assert!(
                !dependencies.is_empty(),
                "an attempt needs a declaration other than its own: {}",
                module.items[items[0]].describe()
            );
            for dependency in dependencies {
                self.declaration(context, dependency)?;
            }
        }
    }
}
