//! Which universe levels of an `induct` or a `struct` two instances may differ in and still be one type.
//!
//! A level is **irrelevant** when no constructor payload, index target, field or index type mentions it except through a position that is itself irrelevant: another family's irrelevant level, or an instance reduction removes. Every other level is **invariant**, and none is covariant. A parameter's type and the result sort go unread: they bound a level from below and say nothing of what an instance holds. An irrelevant level leaves two instances' constructor telescopes identical, targets included, so the two have the same inhabitants and every elimination reads the same telescopes.
//!
//! The walk reads each type's reduct, as [`positivity_vectors`](crate::positivity_vectors) does and for its reason: a type-level `let` has to be opened to see the former it names, and a `/sys` former unfolds to an intrinsic one that carries no level. A `match` the reduct is stuck on is read as conversion compares two of it ([`crate::opened`]): its head, goal, stated instance and a list's element type as written, and each arm opened against fresh binders and forced, so an instance an arm holds marks only what it reduces to. A member of a recursive group met again on its own path, or a call reduction leaves folded, holds the levels the least fixpoint of its group's unfolding holds ([`group_held`]), where the group terminates ([`crate::read_by_unfolding`]). What the walk cannot see through marks every level it carries. That is the refusing direction, an invariant level being compared for equality as every level was before a vector existed.
//!
//! The same walk over terms its caller chose answers which levels they hold ([`held_levels`], [`held_by`]): a definition's body where its levels have closed, a group's telescopes while they are open. What a held level means there is the caller's to say.
//!
//! # Shared, not duplicated
//!
//! Both checkers run this analysis, through [`Driver`], for the reason the positivity analysis gives: it is a total function of the declarations, so a second implementation would be a second run on the same input. The elaborator computes a family's vector when its group finalizes, since a later item compares two of its instances, and again over the zonked module, which is the vector a unit carries. The certifier reads the carried vector while it walks the items and recomputes its module's after: a carried irrelevance its recomputation denies refuses the module.

use {
    crate::{
        Declarations, Driver, Segment, binders, call_path, forceable, labelled, least_fixpoint,
        opened, peel, read_by_unfolding, rec_member,
    },
    curios_common::recurse,
    curios_core::{
        Advance, Bound, Func, FuncType, Global, InductType, Instance, Level, LevelHead, RecGroup,
        Sort, Struct, StructType, Subterm, Telescope, Term, TupleType, UniverseParam, Universes,
        Variance, Variant, universe_metas, universe_params,
    },
    std::{
        collections::{BTreeMap, BTreeSet, HashMap, HashSet},
        hash::{Hash, Hasher},
    },
};

/// What [`variance_vectors`] found: each analyzed declaration's vector, one entry per universe parameter, and the first reduction the driver refused with the declaration whose walk it was.
///
/// A refused reduction does not stop the walk. The term is read as written and every level it carries is marked, so a vector computed past one claims no irrelevance the reduct would have denied. It is handed back because a driver that denies a carried irrelevance on such a vector reports the budget, which may be all that denied it.
#[derive(Debug)]
pub struct VarianceVectors<E> {
    pub vectors: BTreeMap<Global, Vec<Variance>>,
    pub exhausted: Option<(Global, E)>,
}

/// Each declaration's variance vector, iterated from every level irrelevant until no vector changes.
///
/// Whole-set for the reason the positivity fixpoint is: a family mentions itself, so its own vector is an input to computing it, and in a mutual pair one learns that a level is invariant on the round after the other does. Marking only grows a vector, so the iteration ends at the least fixpoint, which is the right one: a level nothing forces invariant is one two instances cannot be told apart by.
///
/// A name outside `declarations` answers from the driver's registry: a family of a unit in scope, whose vector the same checker held to that unit's declarations when it took the unit, and which it reads from then on as it reads the rest of the entry. A name no registry holds is invariant.
pub fn variance_vectors<D: Driver>(
    driver: &mut D,
    declarations: Declarations<'_>,
) -> VarianceVectors<D::Error> {
    curios_profile::profile!("variance_vectors");
    if declarations.is_empty() {
        return VarianceVectors {
            vectors: BTreeMap::new(),
            exhausted: None,
        };
    }

    // Opening a telescope mints binders, and the fixpoint re-walks the same pieces, so every declaration is split once, up front.
    let split = declarations
        .names()
        .into_iter()
        .map(|name| {
            let entry = Split::of(driver, declarations, &name);
            (name, entry)
        })
        .collect::<BTreeMap<_, _>>();
    let mut vectors = Vectors {
        computed: split
            .iter()
            .map(|(name, entry)| (*name, vec![Variance::Irrelevant; entry.parameters]))
            .collect(),
    };
    let mut exhausted = None;

    loop {
        let mut changed = false;
        for (name, entry) in &split {
            let (marked, refused) = sweep(driver, &vectors, &entry.parts);
            if exhausted.is_none()
                && let Some(error) = refused
            {
                exhausted = Some((*name, error));
            }

            let vector = vectors.computed.get_mut(name).expect("a vector per name");
            for (parameter, variance) in vector.iter_mut().enumerate() {
                // A level past the declaration's own count is one its telescopes should not mention; nothing compares it, so nothing is recorded for it.
                if *variance == Variance::Irrelevant && marked.holds(parameter) {
                    *variance = Variance::Invariant;
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }

    VarianceVectors {
        vectors: vectors.computed,
        exhausted,
    }
}

/// A declaration opened for analysis: how many universe parameters it takes, and every type the walk reads.
#[derive(Debug)]
struct Split {
    parameters: usize,
    parts: Vec<Term>,
}

impl Split {
    fn of<D: Driver>(driver: &mut D, declarations: Declarations<'_>, name: &Global) -> Self {
        if let Some(declaration) = declarations.induct(name) {
            let params = binders(driver, declaration.arity());
            let arguments = params.iter().map(Term::free_var).collect::<Vec<_>>();

            // The index types are read here where positivity leaves them out: they store nothing, which is positivity's question, and two instances apart in a level one of them mentions are families over different index types, which is this one's.
            let mut parts = labelled(driver, &declaration.indices_at(&arguments))
                .into_iter()
                .map(|(_, type_)| type_)
                .collect::<Vec<_>>();
            for (_, constructor) in declaration.constructors() {
                let payload = constructor.open(&arguments);
                let mut cursor = payload.cursor();
                while let Some((_, type_)) = cursor.entry() {
                    parts.push(type_);
                    cursor.advance_fresh(|hint| driver.fresh(hint.filter(|hint| !hint.is_empty())));
                }
                // The targets are read because a level may reach nothing else: `mk(): (N(F))` puts `N`'s invariant level in `Fam`'s index and in no payload.
                parts.extend(cursor.body().expect("a cursor past every entry"));
            }

            return Self {
                parameters: declaration.universe_context().parameter_count,
                parts,
            };
        }

        let declaration = declarations
            .struct_(name)
            .expect("every analyzed name is an inductive or a struct");
        let params = binders(driver, &declaration.arity);
        let arguments = params.iter().map(Term::free_var).collect::<Vec<_>>();

        Self {
            parameters: declaration.universe_context.parameter_count,
            parts: labelled(driver, &declaration.fields_at(&arguments))
                .into_iter()
                .map(|(_, type_)| type_)
                .collect(),
        }
    }
}

/// Each analyzed declaration's vector as the fixpoint currently estimates it.
#[derive(Debug)]
struct Vectors {
    computed: BTreeMap<Global, Vec<Variance>>,
}

impl Vectors {
    /// `name`'s variance in its `index`th universe parameter. A name outside the analyzed set answers as its driver holds it ([`Driver::variance`]), and one the driver holds no vector for is invariant, never irrelevant.
    fn at<D: Driver>(&self, driver: &D, name: &Global, index: usize) -> Variance {
        if let Some(vector) = self.computed.get(name) {
            return vector.get(index).copied().unwrap_or(Variance::Invariant);
        }
        driver.variance(name, index)
    }
}

/// The levels `parts` fix under the current estimates, and the first reduction the driver refused while finding them.
fn sweep<D: Driver>(
    driver: &mut D,
    vectors: &Vectors,
    parts: &[Term],
) -> (Marked, Option<D::Error>) {
    let mut climbs = Climbs::default();
    let mut walk = Walk {
        driver,
        vectors,
        forcing: Vec::new(),
        entered: HashSet::new(),
        marked: Marked::default(),
        refused: None,
        climbs: &mut climbs,
        fed: None,
        groups: HashMap::new(),
    };
    for part in parts {
        walk.walk(part);
    }

    (walk.marked, walk.refused)
}

/// The levels a walk found mentioned, a parameter of the declaration or a level its elaboration has not closed: the ones it met, or every one, where it met a term that may mention any.
#[derive(Debug, Default, Clone, PartialEq)]
struct Marked {
    heads: BTreeSet<LevelHead>,
    every: bool,
}

impl Marked {
    fn holds(&self, parameter: usize) -> bool {
        self.every
            || self
                .heads
                .contains(&LevelHead::Param(UniverseParam(parameter)))
    }
}

/// What [`held_levels`] found: the level heads its terms hold where conversion compares them, whether one of them may hold any, and the first reduction the driver refused.
#[derive(Debug)]
pub struct HeldLevels<E> {
    pub heads: BTreeSet<LevelHead>,
    pub every: bool,
    pub exhausted: Option<E>,
}

impl<E> HeldLevels<E> {
    /// Whether the terms hold `head`: it was met where conversion compares it, or a term that may mention any level was.
    pub fn holds(&self, head: LevelHead) -> bool {
        self.every || self.heads.contains(&head)
    }
}

/// The levels the reducts of `parts` hold where two instances of what they are part of would be compared: the walk [`variance_vectors`] runs over a family's telescopes, over terms its caller chose.
///
/// It is the walk and nothing of what it is asked about. Which terms of a declaration stand for it is the caller's, and so is what a held level means there. A level is reported by its head, a declaration's parameter or a level still open while the declaration elaborates, so one walk answers before and after a declaration's levels close. Every family answers from the driver's registry, a name it does not hold reading invariant.
///
/// It forces what it reads, so it is for a term read once, a definition's body where its levels close. What a term holds as it is written, with nothing reduced, is `curios-core`'s `Term::level_differences` to say.
pub fn held_levels<D: Driver>(driver: &mut D, parts: &[Term]) -> HeldLevels<D::Error> {
    curios_profile::profile!("held_levels");
    let vectors = Vectors {
        computed: BTreeMap::new(),
    };
    let (marked, exhausted) = sweep(driver, &vectors, parts);

    HeldLevels {
        heads: marked.heads,
        every: marked.every,
        exhausted,
    }
}

/// The level pairs two projections of one member of a recursive group, apart in levels alone, are told apart at: those at the positions the least fixpoint of the group's unfolding holds (`group_held`), the group generalized over the positions the two differ at, a parameter above every one either names standing at each (`Term::generalized_levels`). `None` where the two are not two projections of one member apart in levels alone. A reduction the driver refuses holds every position, and so does a group not read by its unfolding (`read_by_unfolding`).
///
/// Two instances apart only at positions no unfolding holds unfold to terms apart only there, again and again, so they are one; what conversion compares of them is the rest.
pub fn held_pairs<D: Driver>(
    driver: &mut D,
    this: &Term,
    that: &Term,
) -> Option<Vec<(Level, Level)>> {
    let base = universe_params(this)
        .into_iter()
        .chain(universe_params(that))
        .map(|UniverseParam(index)| index + 1)
        .max()
        .unwrap_or(0);
    let mut next = base;
    let generalized = this.generalized_levels(that, || {
        let level = Level::param(UniverseParam(next));
        next += 1;
        level
    })?;
    let (group, _) = generalized.term.as_rec_proj()?;
    let held = group_held(driver, group);

    Some(
        generalized
            .positions
            .into_iter()
            .filter(|(placeholder, _, _)| match &held {
                Some(held) => {
                    held.exhausted.is_some()
                        || placeholder.atoms().any(|(head, _)| held.holds(head))
                }
                None => true,
            })
            .map(|(_, this_level, that_level)| (this_level, that_level))
            .collect(),
    )
}

/// The levels `declarations` hold while their own are still open: what [`variance_vectors`] reads of each, its index types, payloads, fields and targets, reported by head as [`held_levels`] reports a term's, where a vector has no parameter yet to report an open level under.
///
/// One sweep, since nothing iterates: a family whose levels are open takes no parameter, and an occurrence of it inside its own group carries no instance. Every other family answers from the driver's registry.
pub fn held_by<D: Driver>(driver: &mut D, declarations: Declarations<'_>) -> HeldLevels<D::Error> {
    curios_profile::profile!("held_by");
    let mut vectors = Vectors {
        computed: BTreeMap::new(),
    };
    let mut parts = Vec::new();
    for name in declarations.names() {
        let entry = Split::of(driver, declarations, &name);
        vectors
            .computed
            .insert(name, vec![Variance::Irrelevant; entry.parameters]);
        parts.extend(entry.parts);
    }
    let (marked, exhausted) = sweep(driver, &vectors, &parts);

    HeldLevels {
        heads: marked.heads,
        every: marked.every,
        exhausted,
    }
}

/// One declaration's traversal: which of its universe parameters something it holds mentions.
struct Walk<'a, D: Driver> {
    driver: &'a mut D,
    /// The climbs this walk is a step of, and the groups its reading has read ([`Climbs`]).
    climbs: &'a mut Climbs,
    vectors: &'a Vectors,
    /// The `rec` members this walk is already inside, innermost last: positivity's guard, for positivity's reason.
    forcing: Vec<(RecGroup, usize)>,
    /// The nodes this walk has entered. A node entered again inside the same members marks what it marked and refuses what it refused, so a node a reduct shares is walked once however many paths reach it.
    entered: HashSet<Entered>,
    marked: Marked,
    refused: Option<D::Error>,
    /// The outermost climb whose estimate what this walk marked rests on, by its depth in [`Climbs::climbing`], the walk's own climb's included.
    fed: Option<usize>,
    /// The held levels of each group this walk met whose climb read the estimate of a climb around it, which hold while those estimates do, through this walk; `None` for a group not read by its unfolding ([`read_by_unfolding`]).
    groups: HashMap<RecGroup, Option<Marked>>,
}

/// What one reading of held levels shares across every climb it makes: the climbs in progress and the groups read once and for all.
///
/// The groups a reading meets are connected through reduction where no name connects them: a group's body forces to a call of another, minted anew by the substitution that opened it, whose body forces to a call of the first. A climb of a group already being climbed further out would climb it again, and that one the other again, without end, so a group met while it is climbed reads that climb's estimate, as a walk reads its own group's. That is the least fixpoint of the groups together taken one inside the other (Bekić): the inner climb settles under the outer's estimate, and the outer's next round climbs the inner again under its next.
#[derive(Default)]
struct Climbs {
    /// The groups being climbed, outermost first, each with its estimate so far.
    climbing: Vec<(RecGroup, Marked)>,
    /// The held levels of each group whose climb read no estimate but its own and those of climbs inside it, for the rest of the reading; `None` for a group not read by its unfolding.
    settled: HashMap<RecGroup, Option<Marked>>,
}

/// What a climb of one group found: what its unfolding holds, the first reduction a walk of it refused, and the outermost climb around it whose estimate it read.
struct Climbed<E> {
    held: Option<Marked>,
    exhausted: Option<E>,
    fed: Option<usize>,
}

/// The levels two instances of `group` are told apart by: what its members' unfoldings hold where conversion compares them, the least fixpoint over its members from none, a member met again contributing what the fixpoint holds so far. Each member's body is read under its leading lambdas, opened at fresh binders ([`peel`]), and its type is not read, as no parameter's type is. A reduction the driver refuses is kept, and the caller reads it as every level held. `None` for a group not read by its unfolding ([`read_by_unfolding`]), which the caller reads as written.
pub fn group_held<D: Driver>(driver: &mut D, group: &RecGroup) -> Option<HeldLevels<D::Error>> {
    let climbed = climb(driver, &mut Climbs::default(), group);
    climbed.held.map(|held| HeldLevels {
        heads: held.heads,
        every: held.every,
        exhausted: climbed.exhausted,
    })
}

/// [`group_held`] inside a reading: the least fixpoint over `group`'s members from none, climbed with `climbs` holding the climbs around it.
fn climb<D: Driver>(driver: &mut D, climbs: &mut Climbs, group: &RecGroup) -> Climbed<D::Error> {
    curios_profile::profile!("group_held");
    match read_by_unfolding(driver, group) {
        Ok(true) => {}
        Ok(false) => {
            return Climbed {
                held: None,
                exhausted: None,
                fed: None,
            };
        }
        Err(error) => {
            return Climbed {
                held: Some(Marked {
                    heads: BTreeSet::new(),
                    every: true,
                }),
                exhausted: Some(error),
                fed: None,
            };
        }
    }
    let bodies = (0..group.length())
        .map(|index| peel(driver, group.member_body(index)).2)
        .collect::<Vec<_>>();
    let vectors = Vectors {
        computed: BTreeMap::new(),
    };

    let depth = climbs.climbing.len();
    climbs.climbing.push((group.clone(), Marked::default()));
    let mut exhausted = None;
    let mut fed: Option<usize> = None;
    let held = least_fixpoint(Marked::default(), |estimate| {
        climbs.climbing[depth].1 = estimate.clone();
        let mut walk = Walk {
            driver: &mut *driver,
            climbs: &mut *climbs,
            vectors: &vectors,
            forcing: (0..group.length())
                .map(|member| (group.clone(), member))
                .collect(),
            entered: HashSet::new(),
            marked: estimate.clone(),
            refused: None,
            fed: None,
            groups: HashMap::new(),
        };
        for body in &bodies {
            walk.walk(body);
        }
        if exhausted.is_none() {
            exhausted = walk.refused;
        }
        // An estimate read from a climb around this one makes what it holds that climb's to settle; this climb's own is what it settles.
        if let Some(read) = walk.fed.filter(|read| *read < depth) {
            fed = Some(fed.map_or(read, |fed| fed.min(read)));
        }
        walk.marked
    });
    climbs.climbing.pop();

    Climbed {
        held: Some(held),
        exhausted,
        fed,
    }
}

/// A node as a walk entered it, inside the `rec` members of its path. Told apart by the node's address, and holding the node, so the address is the node's for as long as the walk keys by it: the walk forces and opens what it reads and drops what it made, and a node met later could take a freed one's address.
struct Entered {
    node: Term,
    inside: Vec<(RecGroup, usize)>,
}

impl PartialEq for Entered {
    fn eq(&self, other: &Self) -> bool {
        self.node.identity() == other.node.identity() && self.inside == other.inside
    }
}

impl Eq for Entered {}

impl Hash for Entered {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.node.identity().hash(state);
        self.inside.hash(state);
    }
}

impl<D: Driver> Walk<'_, D> {
    /// A level standing where two instances would differ in it. The walk opens every binder it crosses and enters no universe binder, so a parameter here is the declaration's own.
    fn level(&mut self, level: &Level) {
        self.marked
            .heads
            .extend(level.atoms().map(|(head, _)| head));
    }

    /// Every level `term` mentions, read as written: what the walk does with a term it cannot see through.
    fn everything(&mut self, term: &Term) {
        self.marked
            .heads
            .extend(universe_params(term).into_iter().map(LevelHead::Param));
        self.marked
            .heads
            .extend(universe_metas(term).into_iter().map(LevelHead::Meta));
    }

    /// A call of a member of `group`, read by the levels its group's unfolding holds ([`group_held`]), and its arguments as every term is: the call's path is totality's ([`call_path`]). A group not read by its unfolding is read as written, the call with it.
    fn call(&mut self, term: &Term, group: &RecGroup) {
        let Some(held) = self.group_marks(group) else {
            self.everything(term);
            return;
        };
        self.marked.heads.extend(held.heads);
        self.marked.every |= held.every;
        let (_, steps) = call_path(term);
        for argument in steps.iter().flat_map(Segment::arguments) {
            self.walk(argument);
        }
    }

    /// The levels `group`'s unfolding holds: the estimate of the climb of it this walk is inside, and otherwise its climb, made once a reading where it reads no estimate around it and once a walk where it does, a reduction it refused holding every level. `None` for a group not read by its unfolding.
    fn group_marks(&mut self, group: &RecGroup) -> Option<Marked> {
        if let Some(depth) = self
            .climbs
            .climbing
            .iter()
            .position(|(climbing, _)| climbing == group)
        {
            self.feed(Some(depth));
            return Some(self.climbs.climbing[depth].1.clone());
        }
        if let Some(held) = self.climbs.settled.get(group) {
            return held.clone();
        }
        if let Some(held) = self.groups.get(group) {
            return held.clone();
        }
        let climbed = climb(self.driver, self.climbs, group);
        let held = climbed.held.map(|held| Marked {
            every: held.every || climbed.exhausted.is_some(),
            ..held
        });
        if self.refused.is_none() {
            self.refused = climbed.exhausted;
        }
        match climbed.fed {
            Some(_) => {
                self.feed(climbed.fed);
                self.groups.insert(group.clone(), held.clone());
            }
            None => {
                self.climbs.settled.insert(group.clone(), held.clone());
            }
        }
        held
    }

    /// What this walk marked rests on the estimate of the climb at `depth` too.
    fn feed(&mut self, depth: Option<usize>) {
        if let Some(depth) = depth {
            self.fed = Some(self.fed.map_or(depth, |fed| fed.min(depth)));
        }
    }

    /// A nominal occurrence's instance: its levels at the family's invariant positions, and nothing at an irrelevant one.
    fn nominal(&mut self, name: &Global, universes: &Universes) {
        for (index, level) in universes.levels.iter().enumerate() {
            if self.vectors.at(self.driver, name, index) == Variance::Invariant {
                self.level(level);
            }
        }
    }

    fn forced(&mut self, term: &Term) -> (Term, bool) {
        if !forceable(term) {
            return (term.clone(), false);
        }
        match self.driver.force(term) {
            Ok(reduced) => (reduced, false),
            Err(error) => {
                if self.refused.is_none() {
                    self.refused = Some(error);
                }
                (term.clone(), true)
            }
        }
    }

    /// Each entry's type under fresh binders, then the terminal, so what follows a binder holds free occurrences and can be reduced.
    fn telescope<B: Bound>(
        &mut self,
        telescope: &Telescope<B>,
        terminal: impl FnOnce(&mut Self, B),
    ) {
        let mut cursor = telescope.cursor();
        while let Some((_, type_)) = cursor.entry() {
            self.walk(&type_);
            cursor.advance_fresh(|hint| self.driver.fresh(hint.filter(|hint| !hint.is_empty())));
        }
        terminal(self, cursor.body().expect("a cursor past every entry"));
    }

    /// A form the walk reads through its children alone. A child under a binder the walk did not open is closed and is read as written: a nominal node inside it is still read by its family's vector, and an applied former, unforced there, marks every level of its instance.
    fn children(&mut self, term: &Term) {
        let subterm: &Subterm = term;
        subterm.any_child_term(&mut |child| {
            self.walk(child);
            false
        });
    }

    /// Depth is bought with stack, as every walk over data-shaped depth buys it: a part's reduct is descended into one level per arrow, nothing between the levels is a loop, and the budget is spent only through [`Driver::force`], where a `FuncType` is already a weak-head value. Without the bracket the native stack goes first and the process dies, which is no refusal ([The soundness board](../../documentation/design/soundness/the-soundness-board.md)).
    fn walk(&mut self, term: &Term) {
        recurse(|| self.walk_within(term));
    }

    fn walk_within(&mut self, term: &Term) {
        if !self.entered.insert(Entered {
            node: term.clone(),
            inside: self.forcing.clone(),
        }) {
            return;
        }
        // A `rec` member this path is already inside is not forced again, its unfolding reproducing itself one level deeper; and a call reduction leaves folded is stuck, no unfolding of it getting anywhere. Either holds the levels the least fixpoint of its group's unfolding holds ([`group_held`]), and its arguments are read as every term is.
        let member = rec_member(term);
        if let Some((group, _)) = member
            .as_ref()
            .filter(|member| self.forcing.contains(member))
        {
            self.call(term, group);
            return;
        }
        let (term, refused) = self.forced(term);
        if refused {
            self.everything(&term);
            return;
        }
        if let Some((group, _)) = rec_member(&term) {
            self.call(&term, &group);
            return;
        }
        let entered = match member {
            Some(member) => {
                self.forcing.push(member);
                true
            }
            None => false,
        };

        match &*term {
            Subterm::Sort(Sort { level, .. }) => self.level(level),
            // A parameter, a local or a monomorphic name carries no level.
            Subterm::Var(_) => {}

            Subterm::FuncType(FuncType { telescope, .. }) => {
                self.telescope(telescope, |walk, codomain| walk.walk(&codomain));
            }
            Subterm::TupleType(TupleType { telescope }) => self.telescope(telescope, |_, ()| {}),
            // A lambda standing in a type is a family passed as an argument, `(A: Type) => Result(E, A)`: its binders' types and its body are what an instance holds of it.
            Subterm::Func(Func { telescope, .. }) => {
                self.telescope(telescope, |walk, body| walk.walk(&body));
            }

            Subterm::InductType(InductType {
                name,
                universes,
                params,
                indices,
            }) => {
                self.nominal(name, universes);
                for argument in params.iter().chain(indices) {
                    self.walk(argument);
                }
            }
            Subterm::StructType(StructType {
                name,
                universes,
                params,
            }) => {
                self.nominal(name, universes);
                for argument in params {
                    self.walk(argument);
                }
            }
            // A value compares at its family's variance.
            Subterm::Variant(Variant {
                name,
                universes,
                params,
                payload,
                ..
            }) => {
                self.nominal(name, universes);
                for argument in params.iter().chain(payload) {
                    self.walk(argument);
                }
            }
            Subterm::Struct(Struct {
                name,
                universes,
                params,
                fields,
                ..
            }) => {
                self.nominal(name, universes);
                for argument in params.iter().chain(fields) {
                    self.walk(argument);
                }
            }

            // An instance that survived forcing is a bare or partly applied former, or a definition that does not unfold: its levels are compared for equality, so each is invariant. A projection head's group is closed over its own scheme, which these levels instantiate.
            Subterm::Instance(Instance { universes, .. }) => {
                for level in &universes.levels {
                    self.level(level);
                }
            }
            // A group forcing left standing — one a refused reduction stopped at, a member met again and a folded call being read by their group's fixpoint above — is read as written rather than walked into: every level it mentions is the enclosing declaration's, and marking all of them is the refusing direction.
            Subterm::Rec(_) => self.everything(&term),
            // A metavariable's solution may mention any level.
            Subterm::Metavar(_) => self.marked.every = true,

            Subterm::Intrinsic(intrinsic) => {
                for level in intrinsic
                    .result_universes()
                    .into_iter()
                    .flat_map(|universes| &universes.levels)
                {
                    self.level(level);
                }
                self.children(&term);
            }

            // A `match` the reduction left stuck, under no binder this walk has not opened, is read as conversion compares two of it: its head, goal, stated instance and a list's element type as written, and each arm opened against fresh binders and forced, a mention an arm's reduction removes being no mention.
            Subterm::Match(matched) if term.reach() == 0 => {
                let opened = opened(self.driver, matched);
                for part in opened
                    .suppliers
                    .iter()
                    .chain(&opened.annotations)
                    .chain(&opened.arms)
                {
                    self.walk(part);
                }
            }

            Subterm::Apply(_)
            | Subterm::Tuple(_)
            | Subterm::Match(_)
            | Subterm::Proj(_)
            | Subterm::Let(_)
            | Subterm::Foreign(_)
            | Subterm::Transient(_) => self.children(&term),
        }

        // Every arm above falls through but the refusal's, which entered nothing, so the path is popped however the walk leaves it.
        if entered {
            self.forcing.pop();
        }
    }
}
