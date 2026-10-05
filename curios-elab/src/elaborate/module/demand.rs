//! The order a unit's declarations elaborate in: each when it is asked for, after everything it reads.
//!
//! **The declaration is the unit of work.** A declaration is an item with every item the lowering generated from it — a type former with its constructors, a concept with its method wrappers. Its items share what one written declaration shares: the universe levels of its written types, which the lowering mints once and copies, and the registry entry the former rebuilds and a constructor is finalized against. So they are attempted together, in item order, in one state, and are published together.
//!
//! **A declaration elaborates after what it reads, wherever each was written.** The walk asks for the declarations in their lowered order, and asking for one first asks for every declaration its items' lowered forms reach. What it turns out to read beyond that — the witness a goal resolves to, a name a proof the elaborator writes applies — it meets while it elaborates: the read records the declaration it needed ([`Context::need`]), and an attempt that recorded one is void whatever it went on to conclude. A void attempt is undone as a refused item is, poisoning nothing; what it needed is elaborated; and the attempt is made again. No attempt is suspended to run another inside it, so one context serves them all.
//!
//! **A cycle is refused by its members.** A declaration asked for while it is being asked for closes a cycle, through a name it wrote or a witness a goal of its resolves to alike. Every member is refused, the report is raised once, at the first member in source order, naming them all, and what reaches a member is withheld as a dependent of any refusal is.

use {
    super::{
        Elaborated, Holding, ItemMark, ItemStamp, Poison, Survivors, elaborate_module_item,
        withhold,
    },
    crate::{Context, Error, GoalSite},
    curios_core::{DefinitionKind, Global, Item, Module},
    curios_utilities::recurse,
    std::collections::{BTreeMap, BTreeSet},
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

/// The declarations a demand found on a cycle, in the order they were asked for.
pub(super) struct Cycle(Vec<usize>);

/// What one attempt at a declaration made of each of its items, held until the attempt is known to stand.
enum Outcome {
    Kept(Item),
    /// The item as it elaborated and where it holds the goals it wrote, which are reported in its place.
    Held(Item, Vec<GoalSite>),
    Refused(Error),
    Withheld,
}

/// The walk over one unit's declarations.
pub(super) struct Demand<'a> {
    module: &'a Module,
    /// The positions of each declaration's items, the item a written declaration lowers to first and the items generated from it after, in item order.
    declarations: Vec<Vec<usize>>,
    /// The declaration declaring each name.
    owner: BTreeMap<Global, usize>,
    standing: Vec<Standing>,
    /// The declarations being asked for, outermost first.
    demanded: Vec<usize>,
    pub(super) poison: Poison,
    pub(super) survivors: Survivors,
}

/// The kind the lowering stamped an item with: a group's is its first member's, every member of one being generated alike.
fn introduced(item: &Item) -> &DefinitionKind {
    match item {
        Item::Let(definition) => &definition.kind,
        Item::Rec(rec) => &rec.definitions[0].kind,
    }
}

impl<'a> Demand<'a> {
    pub(super) fn new(module: &'a Module, poison: Poison, survivors: Survivors) -> Self {
        // A generated item names the declaration it was generated from, which is an item of the module wherever the lowering's order put the two.
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
            declarations,
            owner,
            demanded: Vec::new(),
            poison,
            survivors,
        }
    }

    /// Elaborate every declaration, each after what it reads.
    pub(super) fn all(&mut self, context: &mut Context) {
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
                    self.survivors.drop_item(item);
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
                    let site = item
                        .definitions()
                        .first()
                        .and_then(|definition| definition.type_.span());
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

    /// Elaborate `declaration` after what it reads, attempt by attempt, until one reads nothing unfinished.
    fn settle(&mut self, context: &mut Context, declaration: usize) -> Result<(), Cycle> {
        let module = self.module;
        let items = self.declarations[declaration].clone();
        let names = self.names(declaration);
        let reached = items
            .iter()
            .flat_map(|item| module.reaches(&module.items[*item]))
            .collect::<Vec<_>>();
        for dependency in self.owners(reached, declaration) {
            self.declaration(context, dependency)?;
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
                    Ok(Elaborated::Published(elaborated)) => {
                        outcomes.push(Outcome::Kept(elaborated));
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
                self.poison = poison;
                let mut holding = Vec::new();
                for (position, outcome) in items.iter().zip(outcomes) {
                    let item = &module.items[*position];
                    match outcome {
                        Outcome::Kept(elaborated) => {
                            self.survivors.keep(ItemStamp(*position), elaborated);
                        }
                        Outcome::Held(elaborated, sites) => {
                            self.survivors.drop_item(item);
                            holding.push((elaborated, sites));
                        }
                        Outcome::Refused(error) => {
                            self.survivors.drop_item(item);
                            self.survivors.refuse(ItemStamp(*position), error);
                        }
                        Outcome::Withheld => {
                            withhold(context, item);
                            self.survivors.drop_item(item);
                        }
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
