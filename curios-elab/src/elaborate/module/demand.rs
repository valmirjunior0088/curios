//! The order a unit's declarations elaborate in: each when it is asked for, after everything it reads.
//!
//! **The declaration is the unit of work.** A declaration is an item with every item the lowering generated from it — a type former with its constructors, a concept with its method wrappers ([`Declarations`]). Its items share what one written declaration shares: the universe levels of its written types, which the lowering mints once and copies, and the registry entry the former rebuilds and a constructor is finalized against. So they are attempted together, in item order, in one state, and are published together.
//!
//! **A declaration elaborates after what it reads, wherever each was written.** The walk asks for the declarations in the order they are written, which is the order the lowering hands them on in, and asking for one first asks for every declaration its items' lowered forms reach. What it turns out to read beyond that — the witness a goal resolves to, a registry entry no name it writes reaches — it meets while it elaborates: the read records the declaration it needed ([`Context::need`]), and an attempt that recorded one is void whatever it went on to conclude. A void attempt is undone as a refused item is, poisoning nothing; what it needed is elaborated; and the attempt is made again. No attempt is suspended to run another inside it, so one context serves them all.
//!
//! **The prover's vocabulary is asked for first.** A proof the elaborator writes applies lemmas no declaration names, so nothing a declaration writes says it reads them. The walk therefore begins with the declarations of the vocabulary the unit declares, which brings in every declaration they read, with no lemma of the unit on offer; and only then offers them ([`Context::offer_lemmas`]) and asks for the rest. A declaration the vocabulary reads is thereby proved without it, every other finds all of it elaborated, and no proof makes one declaration wait on another.
//!
//! **Over a baseline, the walk visits what the edit reached and nothing else.** A unit compiled over an earlier text of itself marks the declarations its edit could reach, through who asked what of the baseline's traces ([`Readers`]), and holds those alone: the baseline answers for every other declaration from the start ([`Established::answer_but`]), and its trace is the baseline's. A marked declaration is decided once every marked declaration it read is, kept where none of what it read moved and made otherwise; one asked for ahead of that asks again the questions its elaboration put, in the order it asked them ([`Traces`]), each after the declaration it is about has been asked for, and is kept where every one is answered as it was. A question about a declaration that was kept is answered as it was; one about a declaration elaborated again is answered as it was where the part asked came out equal, so an edit redoes what read the part it changed and nothing behind that.
//!
//! **A declaration to be made is first looked for in a store.** Where the walk is handed what a store holds of declarations elaborated before ([`Over::candidates`]), a declaration it would elaborate is taken from a record filed for what its items were lowered to, where every answer the record's elaboration was given is the walk's now and every part it read digests as it did ([`found`]); it is elaborated otherwise.
//!
//! **A cycle is refused by its members.** A declaration asked for while it is being asked for closes a cycle, through a name it wrote or a witness a goal of its resolves to alike. Every member is refused, the report is raised once, at the first member in source order, naming them all, and what reaches a member is withheld as a dependent of any refusal is.

use {
    super::{
        Elaborated, Holding, ItemMark, ItemStamp, Poison, Survivors, carrying,
        elaborate_module_item, withhold,
    },
    crate::{
        Answered, Context, Error, Established, Foreign, GoalSite, HeldTrace, Made, Parts, Question,
        Read, Readers, Same, TraceRecord, Traces, bytes_of, concepts_alike, declaration_input,
        entries_alike, found, inducts_alike, item_part_digest, items_of, lowered_digest,
        part_digest, structs_alike,
    },
    curios_core::{Declarations, Global, Item, Module, Nominal, Place, Places, Trace},
    curios_utilities::{Candidates, Facet, recurse},
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet, HashMap},
        rc::Rc,
    },
};

/// Where one declaration stands in the walk.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Standing {
    Pending,
    /// Asked for and not finished: asking again closes a cycle.
    Demanded,
    /// Kept, refused or withheld.
    Finished,
}

/// The declarations a demand found on a cycle, by their written items' places, in the order they were asked for.
pub struct Cycle(Vec<usize>);

/// What one attempt at a declaration made of each of its items, held until the attempt is known to stand.
enum Outcome {
    /// The item as it was published, with the erasure obligations it breaks.
    Kept(Item, Vec<Error>),
    /// The item as it elaborated and where it holds the goals it wrote, which are reported in its place.
    Held(Item, Vec<GoalSite>),
    Refused(Error),
    Withheld,
}

/// The baseline a unit is compiled over, as its walk reads it: an earlier text of the unit as it elaborated, what each of its declarations asked, and what the new text's lowering made again of it.
#[derive(Clone, Copy)]
pub struct Over<'a> {
    pub baseline: &'a Module,
    pub places: &'a Places,
    pub traces: &'a Traces,
    /// The names whose lowered item is not the baseline's own: new to the unit, or lowered again (`PreparedText::lowered_anew`).
    pub relowered: &'a BTreeSet<Global>,
    /// The names the baseline declared and the new text's lowering does not (`PreparedText::dropped`).
    pub dropped: &'a BTreeSet<Global>,
    /// Who asked what, of the baseline's traces: what the walk finds the declarations an edit could reach by.
    pub readers: &'a Readers,
    /// The scope, whose baseline layer answers for every declaration the walk does not hold, and for each it holds from where the walk keeps it.
    pub established: &'a Established<'a>,
    /// What a store holds of declarations elaborated before, where the walk may take a declaration from it ([`found`]).
    pub candidates: Option<&'a dyn Candidates>,
}

/// One declaration the walk holds: where it stands, whether it was kept, and over a baseline what the walk knows of what the edit reached of it.
///
/// **Marked through the baseline's readers, decided when what it read is.** A seed is a declaration that must be made, or that read an answer that moved; every declaration that read a seed is marked, and every one that read a marked one. A marked declaration counts the marked declarations it read that are undecided, and is decided once the count reaches nothing: made where it must be or a part it read moved, kept otherwise, unasked. A declaration never marked read nothing that can have moved: over a baseline the walk holds none, and the baseline answers for it.
#[derive(Clone, Copy)]
struct Held {
    standing: Standing,
    /// Kept as the baseline holds it.
    kept: bool,
    /// A seed can reach it.
    marked: bool,
    /// How many names of marked declarations not yet decided it asked about, where it is marked.
    waiting: u32,
    /// To be made: it must be, or a part it read moved.
    moved: bool,
}

impl Held {
    const PENDING: Held = Held {
        standing: Standing::Pending,
        kept: false,
        marked: false,
        waiting: 0,
        moved: false,
    };
}

/// The marked declarations a walk over a baseline is yet to take or to mark the readers of.
struct Reached {
    /// The marked declarations nothing undecided holds back, by their written items' places, the one written first taken first.
    ready: BTreeSet<usize>,
    /// The marked declarations whose readers are still to be marked.
    frontier: Vec<usize>,
}

/// The walk over one unit's declarations.
pub struct Demand<'a> {
    module: &'a Module,
    /// The declarations `module`'s items make, by which a name is asked for.
    declarations: &'a Declarations,
    /// The declarations the walk holds, by their written items' places: every one for a unit compiled whole, and for one compiled over a baseline whose edit reached the prover's vocabulary; over a baseline otherwise, the ones the edit could reach. A declaration of the unit the walk does not hold is kept as the baseline holds it, and stands finished from the start.
    held: BTreeMap<usize, Held>,
    /// The declarations being asked for, outermost first.
    demanded: Vec<usize>,
    pub poison: Poison,
    pub survivors: Survivors,
    /// What each declaration's standing attempt asked of the declarations around it, and what each kept declaration's did where the baseline elaborated it: over a baseline, the baseline's traces with each declaration made filed in place of the one it replaces, and each the unit no longer declares taken out.
    pub traces: Traces,
    /// The baseline the unit is compiled over, where it is compiled over one.
    over: Option<Over<'a>>,
    /// What each name elaborated again over a baseline left as the baseline holds it.
    same: HashMap<Global, Same>,
    /// Whether each part a kept declaration asked of a declaration the unit does not hold, in a scope that moved, is the part the baseline read: asked again once, a part being asked by every declaration that read it.
    read: RefCell<HashMap<Global, Vec<(Question, bool)>>>,
    /// The marked declarations still to be taken, where the unit is compiled over a baseline.
    reached: Option<Reached>,
    /// Whether the edit reached the prover's vocabulary, which is then asked for as a walk over no baseline asks for it, every declaration held.
    vocabulary_reached: bool,
    /// Whether a declaration is decided by asking its trace again, as a walk that knows nothing of what an edit reached does: while the prover's vocabulary is asked for where the edit reached it, so the declarations settled before the offer are the ones its new reads reach.
    asking: bool,
    /// Who asked what, of the unit's traces as the walk files them: the baseline's, with the edges of each declaration made or dropped replaced. `None` for a unit compiled whole, whose readers are made where a recompile over it first asks.
    pub readers: Option<Readers>,
    /// The names the unit declares and the baseline did not.
    gained: Vec<Global>,
    /// What each part a record read of a finished declaration, or of another unit, digests to now: asked once, however many records read it. `None` where it will not digest.
    now: RefCell<HashMap<Question, Option<Read>>>,
}

/// Whether the part `question` asks, of a declaration the unit does not hold, digests now to what the baseline recorded of it. A part the baseline recorded nothing of does not stand, and neither does one that will not digest.
fn read_again(over: Over<'_>, question: &Question) -> bool {
    match (
        over.traces.foreign().digest(question),
        over.established.part_digest(question),
    ) {
        (Some(then), Ok(now)) => *then == now,
        _ => false,
    }
}

impl<'a> Demand<'a> {
    /// The walk over `module`, whose items make `declarations`: holding every declaration where the unit is compiled whole, and none until it is told what an edit reached ([`Demand::reach`]) where it is compiled `over` a baseline.
    pub fn new(
        module: &'a Module,
        declarations: &'a Declarations,
        poison: Poison,
        survivors: Survivors,
        over: Option<Over<'a>>,
    ) -> Self {
        let held = match over {
            Some(_) => BTreeMap::new(),
            None => declarations
                .each()
                .map(|declaration| (declaration, Held::PENDING))
                .collect(),
        };

        Self {
            module,
            declarations,
            held,
            demanded: Vec::new(),
            poison,
            survivors,
            traces: over.map(|over| over.traces.clone()).unwrap_or_default(),
            over,
            same: HashMap::new(),
            read: RefCell::new(HashMap::new()),
            reached: None,
            vocabulary_reached: false,
            asking: false,
            readers: over.map(|over| over.readers.clone()),
            gained: Vec::new(),
            now: RefCell::new(HashMap::new()),
        }
    }

    /// The items of every declaration the walk holds, in the order the declarations are written: whose lowered entries the context registers and whose witnesses it declares, before anything is asked for.
    pub fn held_items(&self) -> impl Iterator<Item = &'a Item> + '_ {
        let (module, declarations) = (self.module, self.declarations);
        self.held
            .keys()
            .flat_map(move |declaration| declarations.items(*declaration).iter())
            .map(move |item| &module.items[*item])
    }

    /// Every name of a declaration the walk holds: the ones not finished when the walk begins.
    pub fn held_names(&self) -> BTreeSet<Global> {
        self.held_items()
            .flat_map(Item::declared_names)
            .copied()
            .collect()
    }

    /// Every name of a declaration elaborated again over a baseline: each one the walk holds and did not keep. Empty for a unit compiled whole.
    pub fn made_names(&self) -> BTreeSet<Global> {
        if self.over.is_none() {
            return BTreeSet::new();
        }

        self.held
            .iter()
            .filter(|(_, held)| !held.kept)
            .flat_map(|(declaration, _)| self.names(*declaration))
            .copied()
            .collect()
    }

    /// Record what the unit's declarations read of the declarations it does not hold, each part by the digest it has in `scope`: what a later compile over this unit asks a read of another unit again by ([`Foreign`]).
    ///
    /// Taken where the walk ends, off the traces that stood. A part that will not digest is recorded nowhere, and a read with no record does not stand.
    ///
    /// **Kept by its changes over a baseline.** The baseline's table records what the baseline's traces read, which the walk's traces began as and changed only at the declarations it made or dropped: a part one of those read and no declaration reads now is taken out, and so is every part of a name the unit now declares; a part a declaration made reads is recorded, digested again where the unit declaring it moved. A part a kept declaration read of a unit that moved digests to what it did, or the declaration would have been made, so its record stands; and a walk that made and dropped nothing, in a scope that did not move, holds the baseline's table. Compiled whole, every trace is read.
    pub fn note_foreign(&mut self, scope: &Established<'_>) {
        let Some(over) = self.over else {
            let mut read = Foreign::default();
            for trace in self.traces.each() {
                for part in self.foreign_parts(trace, scope) {
                    if read.digest(&part).is_none()
                        && let Ok(digest) = scope.part_digest(&part)
                    {
                        read.record(part, digest);
                    }
                }
            }
            self.traces.read(Rc::new(read));
            return;
        };
        let made = self
            .held
            .iter()
            .filter(|(_, held)| !held.kept)
            .map(|(declaration, _)| *declaration)
            .collect::<Vec<_>>();
        if scope.settled() && made.is_empty() && over.dropped.is_empty() {
            return;
        }

        let (mut read, mut dropped) = (Vec::new(), Vec::new());
        for declaration in made {
            let names = self.names(declaration);
            for name in &names {
                if let Some(earlier) = over.traces.of(name) {
                    dropped.extend(self.foreign_parts(earlier, scope));
                }
            }
            if let Some(trace) = self.traces.of(names[0]) {
                read.extend(self.foreign_parts(trace, scope));
            }
        }
        for name in over.dropped {
            if let Some(earlier) = over.traces.of(name) {
                dropped.extend(self.foreign_parts(earlier, scope));
            }
        }
        let readers = self
            .readers
            .as_ref()
            .expect("a walk over a baseline keeps who asked what");
        let revised = over.traces.foreign().revised(
            read,
            dropped,
            self.gained
                .iter()
                .flat_map(|name| Parts::ALL.questions(*name)),
            |part| part.subject().is_some_and(|name| !scope.unmoved(name)),
            |part| scope.part_digest(part),
            |part| readers.asks(part),
        );
        self.traces.read(Rc::new(revised));
    }

    /// The parts `trace` reads of declarations the unit does not hold, as the table of them records each: every part asked, and the signature of each witness found, which its own question does not ask; none of a declaration of a fixed unit.
    fn foreign_parts<'t>(
        &'t self,
        trace: &'t Trace<Question, Answered>,
        scope: &'t Established<'_>,
    ) -> impl Iterator<Item = Question> + 't {
        trace
            .asked()
            .iter()
            .filter_map(move |(question, answered)| {
                let part = match (question, answered) {
                    (Question::Witness(..), Answered::Witness(Some(witness))) => {
                        Question::Signature(*witness)
                    }
                    (part, Answered::Part) => *part,
                    _ => return None,
                };
                let name = part.subject()?;

                (self.declarations.item(name).is_none() && scope.recorded(name)).then_some(part)
            })
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

    /// Elaborate every declaration the walk holds, each after what it reads: the declarations of `vocabulary` and what they read first, then the rest with the unit's lemmas on offer.
    ///
    /// **Over a baseline, what the edit reached is decided as what it read is, and nothing else is asked.** Each marked declaration is taken once what it read is decided ([`Held`]). The vocabulary reads what it read where the edit reached none of the declarations settled before the offer, so each declaration stands on its side of the offer and none is asked for ahead of it; where the edit reached one, the vocabulary is asked for as a walk over no baseline asks for it, and a declaration that falls out of what it reads is answered otherwise about the offer, and made.
    pub fn all(&mut self, context: &mut Context, vocabulary: &BTreeSet<Global>) {
        let owners = self.owners(vocabulary.iter().copied(), usize::MAX);
        let Some(over) = self.over else {
            for declaration in owners {
                self.ask_for(context, declaration);
            }
            self.traces.settle_before(self.finished());
            context.offer_lemmas();
            for declaration in self.in_written_order() {
                self.ask_for(context, declaration);
            }
            return;
        };

        // Where the vocabulary was not reached, the declarations settled before the offer are the baseline's, which the traces began with.
        if self.vocabulary_reached {
            self.asking = true;
            for declaration in owners {
                self.ask_for(context, declaration);
            }
            self.asking = false;
            let before = self.finished();
            // Settled before the offer where the baseline elaborated it, and no longer: it is answered otherwise whether lemmas are on offer.
            for name in over.traces.before() {
                if !before.contains(name)
                    && over.readers.lemmas(name).is_some()
                    && let Some(declaration) = self.owner(name)
                {
                    self.seed(declaration);
                }
            }
            self.mark();
            self.traces.settle_before(before);
            // Every declaration no seed reaches is kept before anything is made after the offer: a read some door notes no need for — a lemma a proof applies, a method a derivation writes — finds it standing, as a walk in the order the declarations are written finds every declaration kept ahead of the ones after it. A walk that holds only what the edit reached holds no such declaration: the baseline answers for each from the start.
            for declaration in self.in_written_order() {
                if !self.is_marked(declaration) {
                    self.ask_for(context, declaration);
                }
            }
        }
        context.offer_lemmas();

        while let Some(declaration) = self
            .reached
            .as_mut()
            .and_then(|reached| reached.ready.pop_first())
        {
            self.ask_for(context, declaration);
        }
        // What the counting left: a marked declaration held back by one no counting reached, asked for as its trace asks.
        for declaration in self.in_written_order() {
            self.ask_for(context, declaration);
        }
    }

    /// Ask for `declaration`, which nothing is being asked for ahead of: a cycle it closes is closed by itself.
    fn ask_for(&mut self, context: &mut Context, declaration: usize) {
        let closed = self.declaration(context, declaration);
        assert!(
            closed.is_ok(),
            "a cycle is closed by the member asked for first, which nothing here asked for"
        );
    }

    /// Every declaration the walk holds, in the order the declarations are written, which is their written items'.
    fn in_written_order(&self) -> Vec<usize> {
        self.held.keys().copied().collect()
    }

    /// The names the finished declarations the walk holds are written as.
    fn finished(&self) -> BTreeSet<Global> {
        self.held
            .iter()
            .filter(|(_, held)| held.standing == Standing::Finished)
            .map(|(declaration, _)| *self.names(*declaration)[0])
            .collect()
    }

    /// The declaration of the unit declaring `name`, by its written item's place.
    fn owner(&self, name: &Global) -> Option<usize> {
        self.declarations.declaration(name)
    }

    /// Where `declaration` stands: one the walk does not hold is kept as the baseline holds it, and finished.
    fn standing(&self, declaration: usize) -> Standing {
        self.held
            .get(&declaration)
            .map_or(Standing::Finished, |held| held.standing)
    }

    /// What the walk knows of `declaration`, which it holds.
    fn held_mut(&mut self, declaration: usize) -> &mut Held {
        self.held
            .get_mut(&declaration)
            .expect("a declaration asked for is one the walk holds")
    }

    /// Whether a seed can reach `declaration`.
    fn is_marked(&self, declaration: usize) -> bool {
        self.reached.is_some() && self.held.get(&declaration).is_some_and(|held| held.marked)
    }

    /// Mark what the edit could reach, holding each declaration marked: every declaration the lowering made again, every reader of an answer that moved, then every declaration that read a marked one. Where that reaches the prover's vocabulary, every declaration is held. The baseline answers for every declaration not held, and every name it held that the unit no longer declares is withheld, from here on.
    ///
    /// Asked before the context holds anything of the unit: what stands at a witness key here is the scope's alone, and the baseline answers for nothing yet.
    pub fn reach(&mut self, context: &Context, vocabulary: &BTreeSet<Global>) {
        let Some(over) = self.over else {
            return;
        };
        self.reached = Some(Reached {
            ready: BTreeSet::new(),
            frontier: Vec::new(),
        });

        // A declaration the lowering made again: a name of it lowered anew, or new to the unit.
        for name in over.relowered {
            if let Some(declaration) = self.owner(name) {
                self.seed(declaration);
            }
        }
        // A name the unit declares now and the baseline did not, read where it was no one's; and one the baseline declared and the unit does not, read where it stood.
        let gained = over
            .relowered
            .iter()
            .filter(|name| over.places.place(name).is_none())
            .copied()
            .collect::<Vec<_>>();
        for name in gained.iter().chain(over.dropped) {
            self.flag_readers(name, Parts::ALL);
        }
        for name in over.dropped {
            if let (Some(readers), Some(trace)) = (&mut self.readers, over.traces.of(name)) {
                readers.replace(*name, Some(trace), None);
            }
            self.traces.forget(name);
        }
        // A key at which the unit declares other witnesses than the baseline did: a witness's name states its key, so a key moved where a witness was gained or dropped there. Every witness the unit declares there is attempted, so the one registered first is the one a whole compile registers.
        let moved = gained
            .iter()
            .filter(|name| self.module.witnesses.contains(*name))
            .chain(
                over.dropped
                    .iter()
                    .filter(|name| over.baseline.witnesses.contains(*name)),
            )
            .filter_map(Global::witness_key)
            .collect::<BTreeSet<_>>();
        for (concept, key) in &moved {
            for reader in over.readers.at_key(*concept, *key).to_vec() {
                self.flag(&reader);
            }
            let witnesses = concept.qualifier().map_or_else(Vec::new, |concept| {
                self.module
                    .witnesses_at(concept, key)
                    .copied()
                    .collect::<Vec<_>>()
            });
            for witness in witnesses {
                self.flag(&witness);
            }
        }
        self.gained = gained;
        if !over.established.settled() {
            // A part of a declaration of another unit, where that unit moved and the part digests otherwise now, or the baseline recorded nothing of it.
            let mut foreign = over
                .readers
                .names()
                .filter(|name| {
                    self.declarations.item(name).is_none() && over.places.place(name).is_none()
                })
                .copied()
                .collect::<Vec<_>>();
            foreign.sort_unstable();
            for name in foreign {
                let moved = over
                    .readers
                    .of_name(&name)
                    .iter()
                    .filter(|(_, read)| {
                        read.questions(name)
                            .any(|question| !self.read_stands(over, &name, &question))
                    })
                    .map(|(reader, _)| *reader)
                    .collect::<Vec<_>>();
                for reader in moved {
                    self.flag(&reader);
                }
            }
            // A witness the units in scope now hold at a key the unit declares one at collides with it.
            let now = self
                .module
                .witnesses
                .iter()
                .filter_map(|name| Some((name.witness_key()?, *name)))
                .collect::<Vec<_>>();
            for ((concept, key), name) in &now {
                if context.witness_standing(concept, key).is_some() {
                    self.flag(name);
                }
            }
            // A key the unit declares no witness at is answered by the units in scope, and where one of them moved, a question asked there may be answered otherwise now. Whether one is declared at a key is the scope's answer or the unit's own.
            let declared = now.iter().map(|(key, _)| *key).collect::<BTreeSet<_>>();
            let mut answered_otherwise =
                over.readers
                    .keys()
                    .filter(|((concept, key), keyed)| {
                        let found_otherwise = !declared.contains(&(*concept, *key))
                            && keyed.answered.found.is_some_and(|found| {
                                found != context.witness_standing(concept, key)
                            });
                        let declared_otherwise = keyed.answered.declared.is_some_and(|stands| {
                            stands
                                != (context.witness_stands(concept, key)
                                    || declared.contains(&(*concept, *key)))
                        });
                        found_otherwise || declared_otherwise
                    })
                    .flat_map(|(_, keyed)| keyed.readers.iter().copied())
                    .collect::<Vec<_>>();
            answered_otherwise.sort_unstable();
            for reader in answered_otherwise {
                self.flag(&reader);
            }
        }
        self.mark();

        self.vocabulary_reached = vocabulary
            .iter()
            .any(|name| self.owner(name).is_some_and(|owner| self.is_marked(owner)))
            || over.traces.before().iter().any(|name| {
                self.owner(name)
                    .is_none_or(|declaration| self.is_marked(declaration))
            });
        // Where the vocabulary was reached, every declaration is held, as a walk over no baseline holds it, and the baseline answers for each from where the walk keeps it.
        if self.vocabulary_reached {
            for declaration in self.declarations.each() {
                self.held.entry(declaration).or_insert(Held::PENDING);
            }
        } else {
            let withheld = self.held_names();
            over.established
                .answer_but(withheld.iter().chain(over.dropped));
        }
    }

    /// Hold `declaration` to be made, and mark it.
    fn seed(&mut self, declaration: usize) {
        let reached = self.reached.as_mut().expect("a seed is a recompile's");
        let held = self.held.entry(declaration).or_insert(Held::PENDING);
        held.moved = true;
        if !held.marked {
            held.marked = true;
            reached.frontier.push(declaration);
        }
    }

    /// Hold the declaration written as `reader` to be made, where the unit declares it and it is undecided: an answer it read moved. Asked before the walk begins, where every declaration is undecided, or where it holds every one.
    fn flag(&mut self, reader: &Global) {
        if let Some(declaration) = self.owner(reader)
            && self
                .held
                .get(&declaration)
                .is_none_or(|held| held.standing == Standing::Pending)
        {
            self.seed(declaration);
        }
    }

    /// Hold each declaration that read one of `parts` of `name` to be made.
    fn flag_readers(&mut self, name: &Global, parts: Parts) {
        let over = self.over.expect("a reader is a recompile's");
        for (reader, read) in over.readers.of_name(name) {
            if read.meets(parts) {
                self.flag(reader);
            }
        }
    }

    /// Mark every undecided declaration that read a marked one, holding it, counting for each the names of marked declarations it asked about, and make ready each newly marked declaration that waits on nothing.
    fn mark(&mut self) {
        let (Some(over), Some(mut reached)) = (self.over, self.reached.take()) else {
            return;
        };
        let mut marked = Vec::new();
        while let Some(declaration) = reached.frontier.pop() {
            marked.push(declaration);
            for name in self.names(declaration) {
                for (reader, _) in over.readers.of_name(name) {
                    // A declaration does not wait on itself: what a group asks of its own members is decided with it.
                    let Some(read) = self.owner(reader) else {
                        continue;
                    };
                    if read == declaration {
                        continue;
                    }
                    let held = self.held.entry(read).or_insert(Held::PENDING);
                    if held.standing != Standing::Pending {
                        continue;
                    }
                    held.waiting += 1;
                    if !held.marked {
                        held.marked = true;
                        reached.frontier.push(read);
                    }
                }
            }
        }
        for declaration in marked {
            let held = self.held[&declaration];
            if held.waiting == 0 && held.standing == Standing::Pending {
                reached.ready.insert(declaration);
            }
        }
        self.reached = Some(reached);
    }

    /// Tell the readers of `declaration`, just decided, what of it moved: each marked one that read a part that moved is to be made, and waits on one declaration fewer.
    ///
    /// **One decided outside the counting** — asked for by its trace while the prover's vocabulary was, every declaration held — marks each reader of a part of it that moved, which is to be made, as a seed would.
    fn propagated(&mut self, declaration: usize) {
        let (Some(over), Some(mut reached)) = (self.over, self.reached.take()) else {
            return;
        };
        let counted = self.held[&declaration].marked;
        for name in self.names(declaration) {
            let moved = self.moved(name, declaration);
            for (reader, read) in over.readers.of_name(name) {
                let Some(read_by) = self.owner(reader) else {
                    continue;
                };
                let Some(held) = self.held.get_mut(&read_by) else {
                    continue;
                };
                if held.standing != Standing::Pending {
                    continue;
                }
                match counted {
                    true if held.marked => {
                        if read.meets(moved) {
                            held.moved = true;
                        }
                        held.waiting = held.waiting.saturating_sub(1);
                        if held.waiting == 0 {
                            reached.ready.insert(read_by);
                        }
                    }
                    true => {}
                    false if read.meets(moved) => {
                        held.moved = true;
                        if !held.marked {
                            held.marked = true;
                            reached.frontier.push(read_by);
                        }
                    }
                    false => {}
                }
            }
        }
        let marking = !reached.frontier.is_empty();
        self.reached = Some(reached);
        if marking {
            self.mark();
        }
    }

    /// The parts of `name`, of `declaration` just decided, that are not as the baseline holds them: none of one kept, and every one of one refused, withheld, or with nothing of the baseline's to compare with.
    fn moved(&self, name: &Global, declaration: usize) -> Parts {
        if self.poison.holds(name) {
            return Parts::ALL;
        }
        if self.held[&declaration].kept {
            return Parts::default();
        }

        self.same.get(name).map_or(Parts::ALL, Same::moved)
    }

    /// Whether the walk decides `declaration` without asking its trace, and how: `Some(true)` keeps it, being one no seed reaches or one whose every marked dependency is decided and none moved, and `Some(false)` makes it, being one to be made; `None` asks, where nothing is known of what the edit reached, the vocabulary is being asked for, or the declaration is asked for ahead of what it waits on.
    fn unasked(&self, declaration: usize) -> Option<bool> {
        self.reached.as_ref().filter(|_| !self.asking)?;
        let held = self.held.get(&declaration)?;
        if !held.marked {
            return Some(true);
        }
        if held.moved {
            return Some(false);
        }

        (held.waiting == 0).then_some(true)
    }

    /// The names every item of `declaration` declares.
    fn names(&self, declaration: usize) -> Vec<&'a Global> {
        let module = self.module;
        self.declarations
            .items(declaration)
            .iter()
            .flat_map(|item| module.items[*item].declared_names())
            .collect()
    }

    /// Ask for `declaration`: elaborate it unless it is finished, and hand back the cycle where it is already being asked for.
    fn declaration(&mut self, context: &mut Context, declaration: usize) -> Result<(), Cycle> {
        match self.standing(declaration) {
            Standing::Finished => return Ok(()),
            Standing::Demanded => return Err(self.cycle_through(declaration)),
            Standing::Pending => {}
        }

        self.held_mut(declaration).standing = Standing::Demanded;
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
                for item in self.declarations.items(declaration).iter() {
                    let item = &module.items[*item];
                    withhold(context, item);
                    self.poison.declare(item);
                }
                if cycle.0.iter().min() == Some(&declaration) {
                    let members = cycle
                        .0
                        .iter()
                        .copied()
                        .collect::<BTreeSet<_>>()
                        .into_iter()
                        .map(|member| &module.items[member])
                        .collect::<Vec<_>>();
                    let item = &module.items[declaration];
                    // Placed at the first member's name, as any report with no position of its own is.
                    let error = Error::declaration_cycle(&members)
                        .in_declaration(item.declared_names().into_iter().copied());
                    self.survivors.refuse(ItemStamp(declaration), error);
                }
                // The cycle is closed by the member asked for first: what asked for that one reads a refusal, as it would any other.
                match cycle.0.first() == Some(&declaration) {
                    true => Ok(()),
                    false => Err(cycle),
                }
            }
        };
        self.held_mut(declaration).standing = Standing::Finished;
        context.finish(&self.names(declaration));
        self.propagated(declaration);

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
        names
            .into_iter()
            .filter_map(|name| self.owner(&name))
            .filter(|owner| *owner != declaration)
            .collect::<BTreeSet<_>>()
            .into_iter()
            .collect()
    }

    /// Ask for the declaration declaring `name`, where the unit declares it in another declaration than `declaration`.
    fn ask(
        &mut self,
        context: &mut Context,
        name: &Global,
        declaration: usize,
    ) -> Result<(), Cycle> {
        match self.owner(name) {
            Some(owner) if owner != declaration => self.declaration(context, owner),
            _ => Ok(()),
        }
    }

    /// Ask for every declaration the lowered forms of `declaration`'s items reach, in the order they are written.
    fn reached(&mut self, context: &mut Context, declaration: usize) -> Result<(), Cycle> {
        let module = self.module;
        let dependencies = {
            curios_profile::profile!("demand::reached");
            let reached = self
                .declarations
                .items(declaration)
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

            names.iter().all(|name| !over.relowered.contains(*name))
                && over.traces.held(names[0]).is_some()
        })
    }

    /// What `declaration` asked where the baseline elaborated it, where the walk may keep it by that: the lowering kept every item of it, the baseline holds each under its name, nothing it reaches is refused, and a witness it declares is alone at its key.
    ///
    /// **A witness is kept where it would register.** A witness registers under its key as its signature elaborates, and the second to register at one key is refused; a kept one registers nothing, so it is asked here what registering would have: that it is the first the unit declares at its key, and that none stands there yet. One that is not is attempted, and refused where a whole compile refuses it.
    fn recorded(&self, context: &Context, declaration: usize) -> Option<HeldTrace> {
        let over = self.over?;
        let module = self.module;
        let names = self.names(declaration);
        let kept = names
            .iter()
            .all(|name| !over.relowered.contains(*name) && over.places.place(name).is_some());
        let poisoned = self
            .declarations
            .items(declaration)
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

    /// Whether the part of a declaration that `question` asks is as the baseline holds it: a kept declaration's is, the walk held it or not, one elaborated again where it came out equal, and one neither the unit nor the baseline declares is another unit's or no one's, which stands where the baseline read what is read now ([`Self::read_stands`]).
    fn held(&self, question: &Question) -> bool {
        let over = self.over.expect("a trace is asked again over a baseline");
        let Some(name) = question.subject() else {
            return false;
        };

        let Some(owner) = self.owner(name) else {
            return over.places.place(name).is_none() && self.read_stands(over, name, question);
        };
        match self.held.get(&owner) {
            None => true,
            Some(held) if held.kept => true,
            Some(_) => self
                .same
                .get(name)
                .is_some_and(|same| same.answers(question)),
        }
    }

    /// Keep `declaration` as the baseline holds it: what the unit seeded for it is taken out, so a read of it falls through to the baseline, which answers for it from here on, and its trace is the baseline's own, which the walk's traces began with. No item is made for it: the unit takes the baseline's where it is assembled.
    fn keep(&mut self, context: &mut Context, declaration: usize) {
        let over = self.over.expect("a declaration is kept over a baseline");
        let names = self.names(declaration);
        for name in &names {
            context.remove_induct(name);
            context.remove_struct(name);
            context.remove_concept(name);
        }
        over.established.keep(&names);
        self.held_mut(declaration).kept = true;
    }

    /// File `record` as the declaration declaring `names`'s, under the first: in place of the baseline's record of it, and of any record the baseline held under another of its names, which was a declaration this one now holds the name of. A record made now, or one a store held, which the declaration was taken from.
    fn file(&mut self, names: &[&Global], record: HeldTrace) {
        let (first, others) = names.split_first().expect("a declaration declares a name");
        if let Some(over) = self.over {
            for name in others {
                if let (Some(readers), Some(held)) = (&mut self.readers, over.traces.of(name)) {
                    readers.replace(**name, Some(held), None);
                }
                self.traces.forget(name);
            }
            if let Some(readers) = &mut self.readers {
                readers.replace(**first, over.traces.of(first), Some(&**record));
            }
        }
        self.traces.share(**first, record);
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

    /// Take `declaration`'s items from a record a store holds for what its items were lowered to, where one stands ([`found`]): every answer its elaboration was given is the one the walk gives now, and every part it read digests as it did. Whether one was taken.
    ///
    /// **Nothing is asked for to decide it.** A record's questions are answered of declarations finished and of other units alone, so a record that read a declaration not yet decided is passed over, and the declaration elaborated as it would have been: taking one closes no cycle and decides nothing ahead of the order its readers are counted in.
    ///
    /// **Taken as elaborated.** Each item is published as its elaboration published it ([`Context::publish_adopted`]) and compared with the baseline's, the binders its proofs read are credited, and the record is filed as the declaration's, complete as the store held it, so the declarations that read it, the unit's record and a later filing find what they would have of an elaboration.
    fn adopted(&mut self, context: &mut Context, declaration: usize) -> bool {
        let Some(candidates) = self.over.and_then(|over| over.candidates) else {
            return false;
        };
        let module = self.module;
        let positions = Rc::clone(self.declarations.items(declaration));
        let names = self.names(declaration);
        if names.iter().any(|name| module.witnesses.contains(*name))
            || positions
                .iter()
                .any(|position| self.poison.reaches(module, &module.items[*position]))
        {
            return false;
        }
        curios_profile::profile!("demand::candidates");
        let Ok(inputs) = positions
            .iter()
            .map(|position| lowered_digest(module, *position))
            .collect::<Result<Vec<_>, _>>()
        else {
            return false;
        };
        let input = declaration_input(inputs);

        for record in found(candidates, Facet::Elaboration, &input, names[0]) {
            let Some(made) = record.made() else {
                continue;
            };
            if made.outputs.len() != positions.len()
                || !self.reads_stand(context, &names, made, &record)
            {
                continue;
            }
            let Some(items) = items_of(made, |identity| candidates.object(identity)) else {
                continue;
            };
            let alike = positions.iter().zip(&items).all(|(position, item)| {
                module.items[*position].declared_names() == item.declared_names()
            });
            if !alike {
                continue;
            }

            curios_profile::sample!("demand::adopted", record.len());
            for (position, item) in positions.iter().zip(items) {
                context.publish_adopted(&item);
                self.compare(context, &item);
                self.survivors.keep(ItemStamp(*position), item, Vec::new());
            }
            context.credit_adopted(made.credited.iter().copied());
            self.file(&names, Rc::clone(&record));
            return true;
        }

        false
    }

    /// Whether every question of `asked`, the declaration declaring `names` elaborated where `made` was filed, is answered now as it was, and every part it read digests as it did, each of a declaration finished or of another unit.
    fn reads_stand(
        &self,
        context: &Context,
        names: &[&Global],
        made: &Made,
        asked: &Trace<Question, Answered>,
    ) -> bool {
        asked
            .asked()
            .iter()
            .zip(&made.reads)
            .all(|((question, answered), read)| match question {
                Question::Lemmas => {
                    read.is_none() && *answered == Answered::Whether(context.offers_lemmas())
                }
                Question::WitnessDeclared(concept, key) => {
                    read.is_none()
                        && *answered == Answered::Whether(context.witness_stands(concept, key))
                }
                // A witness the unit declares at the key and has not finished may yet stand there.
                Question::Witness(concept, key) => {
                    let found = context.witness_standing(concept, key);

                    context.witness_pending(concept, key).is_none()
                        && *answered == Answered::Witness(found)
                        && match found {
                            Some(witness) => {
                                self.read_now(context, names, &Question::Signature(witness))
                                    == Some(*read)
                            }
                            None => read.is_none(),
                        }
                }
                part => self.read_now(context, names, part) == Some(*read),
            })
    }

    /// What the part `question` asks reads now, as a completed record of the declaration declaring `names` holds it ([`Read`]): `None` where it cannot be told — of a declaration not finished, refused or withheld, or a part that will not digest.
    ///
    /// A part of a declaration kept is the baseline's, one made or taken now is the item the walk keeps for it, and one of another unit is the record the baseline holds of it where that unit did not move, and digested again otherwise.
    fn read_now(&self, context: &Context, names: &[&Global], question: &Question) -> Option<Read> {
        let over = self.over?;
        let subject = question.subject()?;
        if names.contains(&subject) {
            return Some(None);
        }
        if let Some(read) = self.now.borrow().get(question) {
            return *read;
        }

        let digest = match self.owner(subject) {
            Some(owner) => {
                if self.standing(owner) != Standing::Finished || self.poison.holds(subject) {
                    return None;
                }
                match self.held.get(&owner).is_none_or(|held| held.kept) {
                    true => part_digest(over.baseline, over.places, question),
                    false => {
                        let item = carrying(context, self.survivors.item(subject)?.clone());
                        let place = place_in(&item, subject)?;
                        item_part_digest(&item, place, question)
                    }
                }
            }
            // A fixed unit's part is the compiler's, which no record holds.
            None if !over.established.recorded(subject) => {
                self.now.borrow_mut().insert(*question, Some(None));
                return Some(None);
            }
            None => match over.traces.foreign().digest(question) {
                Some(then) if over.established.unmoved(subject) => Ok(then.clone()),
                _ => over.established.part_digest(question),
            },
        };
        let read = digest
            .ok()
            .map(|digest| Some(digest.as_deref().and_then(bytes_of)));
        self.now.borrow_mut().insert(*question, read);

        read
    }

    /// Elaborate `declaration` after what it reads: kept as its baseline holds it where everything it asked there is answered as it was, and otherwise attempt by attempt, until one reads nothing unfinished.
    ///
    /// **Over a baseline, decided unasked where the walk knows how** ([`Demand::unasked`]): kept where no seed reaches it, or what it waited on is decided and none of it moved, and made where it is to be made. Only a declaration asked for ahead of what it waits on, or while the vocabulary is asked for, asks its trace again.
    fn settle(&mut self, context: &mut Context, declaration: usize) -> Result<(), Cycle> {
        let module = self.module;
        let items = Rc::clone(self.declarations.items(declaration));
        let names = self.names(declaration);
        let unasked = self.unasked(declaration);
        if unasked == Some(true) && self.recorded(context, declaration).is_some() {
            self.keep(context, declaration);
            return Ok(());
        }
        // A declaration asked again by its trace asks for each declaration its elaboration waited for as its questions come to it, so what its lowered forms reach is asked for ahead of it only where that could be told apart: ahead of the offer of the unit's lemmas, which side of the offer a declaration is first asked for on being what one of its questions is answered by.
        let traced = unasked.is_none() && context.offers_lemmas() && self.traced(declaration);
        if !traced {
            self.reached(context, declaration)?;
        }

        if unasked.is_none()
            && let Some(trace) = self.recorded(context, declaration)
            && self.stands(context, declaration, &trace)?
        {
            curios_profile::sample!("demand::kept", trace.len());
            self.keep(context, declaration);
            return Ok(());
        }
        if traced {
            self.reached(context, declaration)?;
        }
        if self.adopted(context, declaration) {
            return Ok(());
        }

        loop {
            // One attempt at every item, in one state. What it makes of each is held until it is known to stand: a void attempt refused nothing and poisoned nothing.
            let mark = ItemMark::begin(context);
            context.attempt(&names);
            let mut poison = self.poison.clone();
            let mut outcomes = Vec::with_capacity(items.len());
            for position in items.iter() {
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
                        // A refusal that stands is reported, so its report is built and settled for its reader here, before what its attempt wrote is undone; a void attempt's is thrown away unbuilt.
                        let error = match context.needs_nothing() {
                            true => {
                                let error = error.settled(context);
                                member.undo(context, &item.declared_names());
                                poison.declare(item);
                                error
                            }
                            false => error,
                        };
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
                self.file(&names, Rc::new(TraceRecord::new(trace)));
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

/// Where `item` holds `name` among its definitions, as a module holding it alone would place it.
fn place_in(item: &Item, name: &Global) -> Option<Place> {
    match item {
        Item::Let(definition) => (definition.name == *name).then_some(Place::Let(0)),
        Item::Rec(rec) => rec
            .definitions
            .iter()
            .position(|definition| definition.name == *name)
            .and_then(|member| u32::try_from(member).ok())
            .map(|member| Place::Member { item: 0, member }),
    }
}
