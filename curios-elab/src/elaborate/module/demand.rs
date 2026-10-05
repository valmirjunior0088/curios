//! The order a unit's items elaborate in: each when it is asked for, after everything it reads.
//!
//! **An item elaborates after what it reads, wherever each was written.** The walk asks for the items in their lowered order, and asking for one first asks for every item its lowered form reaches. What it turns out to read beyond that — the witness a goal resolves to, a name a proof the elaborator writes applies — it meets while it elaborates: the read records the declaration it needed ([`Context::need`]), and an attempt that recorded one is void whatever it went on to conclude. A void attempt is undone as a refused item is, poisoning nothing; what it needed is elaborated; and the attempt is made again. No attempt is suspended to run another inside it, so one context serves them all.
//!
//! **A cycle is refused by its members.** An item asked for while it is being asked for closes a cycle, through a name it wrote or a witness a goal of its resolves to alike. Every member is refused, the report is raised once, at the first member in source order, naming them all, and what reaches a member is withheld as a dependent of any refusal is.

use {
    super::{ItemMark, ItemStamp, Poison, Survivors, elaborate_module_item, withhold},
    crate::{Context, Error},
    curios_core::{Global, Module},
    curios_utilities::recurse,
    std::collections::{BTreeMap, BTreeSet},
};

/// Where one item stands in the walk.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Standing {
    Pending,
    /// Asked for and not finished: asking again closes a cycle.
    Demanded,
    /// Kept, refused or withheld.
    Finished,
}

/// The items a demand found on a cycle, as positions, in the order they were asked for.
pub(super) struct Cycle(Vec<usize>);

/// The walk over one unit's items.
pub(super) struct Demand<'a> {
    module: &'a Module,
    /// The item declaring each name.
    owner: BTreeMap<Global, usize>,
    standing: Vec<Standing>,
    /// The items being asked for, outermost first.
    demanded: Vec<usize>,
    pub(super) poison: Poison,
    pub(super) survivors: Survivors,
}

impl<'a> Demand<'a> {
    pub(super) fn new(module: &'a Module, poison: Poison, survivors: Survivors) -> Self {
        let owner = module
            .items
            .iter()
            .enumerate()
            .flat_map(|(position, item)| {
                item.declared_names()
                    .into_iter()
                    .map(move |name| (*name, position))
            })
            .collect();

        Self {
            module,
            owner,
            standing: vec![Standing::Pending; module.items.len()],
            demanded: Vec::new(),
            poison,
            survivors,
        }
    }

    /// Elaborate every item, each after what it reads.
    pub(super) fn all(&mut self, context: &mut Context) {
        for position in 0..self.module.items.len() {
            let closed = self.item(context, position);
            assert!(
                closed.is_ok(),
                "a cycle is closed by the member asked for first, which nothing here asked for"
            );
        }
    }

    /// Ask for the item at `position`: elaborate it unless it is finished, and hand back the cycle where it is already being asked for.
    fn item(&mut self, context: &mut Context, position: usize) -> Result<(), Cycle> {
        match self.standing[position] {
            Standing::Finished => return Ok(()),
            Standing::Demanded => return Err(self.cycle_through(position)),
            Standing::Pending => {}
        }

        self.standing[position] = Standing::Demanded;
        self.demanded.push(position);
        let settled = recurse(|| self.settle(context, position));
        self.demanded.pop();

        self.close(context, position, settled)
    }

    /// Finish the item at `position`, refusing it where what it asked for led back to it.
    fn close(
        &mut self,
        context: &mut Context,
        position: usize,
        settled: Result<(), Cycle>,
    ) -> Result<(), Cycle> {
        let module = self.module;
        let item = &module.items[position];
        let outcome = match settled {
            Ok(()) => Ok(()),
            Err(cycle) => {
                withhold(context, item);
                self.poison.declare(item);
                self.survivors.drop_item(item);
                if cycle.0.iter().min() == Some(&position) {
                    let members = cycle
                        .0
                        .iter()
                        .collect::<BTreeSet<_>>()
                        .into_iter()
                        .map(|member| &module.items[*member])
                        .collect::<Vec<_>>();
                    let site = item
                        .definitions()
                        .first()
                        .and_then(|definition| definition.type_.span());
                    let error = Error::declaration_cycle(&members)
                        .at_opt(site)
                        .in_declaration(&item.describe(), item.declared_names().first().copied());
                    self.survivors.refuse(ItemStamp(position), error);
                }
                // The cycle is closed by the member asked for first: what asked for that one reads a refusal, as it would any other.
                match cycle.0.first() == Some(&position) {
                    true => Ok(()),
                    false => Err(cycle),
                }
            }
        };
        self.standing[position] = Standing::Finished;
        context.finish(&item.declared_names());

        outcome
    }

    /// The items being asked for from `position`'s demand on: the cycle a second demand of it closes.
    fn cycle_through(&self, position: usize) -> Cycle {
        let from = self
            .demanded
            .iter()
            .position(|demanded| *demanded == position)
            .expect("an item being asked for is among those asked for");

        Cycle(self.demanded[from..].to_vec())
    }

    /// The items declaring `names`, but for the one at `position`, in item order.
    fn owners(&self, names: impl IntoIterator<Item = Global>, position: usize) -> BTreeSet<usize> {
        names
            .into_iter()
            .filter_map(|name| self.owner.get(&name).copied())
            .filter(|owner| *owner != position)
            .collect()
    }

    /// Elaborate the item at `position` after what it reads, attempt by attempt, until one reads nothing unfinished.
    fn settle(&mut self, context: &mut Context, position: usize) -> Result<(), Cycle> {
        let module = self.module;
        let item = &module.items[position];
        let names = item.declared_names();
        for dependency in self.owners(module.reaches(item), position) {
            self.item(context, dependency)?;
        }

        loop {
            if self.poison.reaches(module, item) {
                withhold(context, item);
                self.poison.declare(item);
                self.survivors.drop_item(item);
                return Ok(());
            }

            let mark = ItemMark::begin(context);
            context.attempt(&names);
            let attempt = elaborate_module_item(context, item);
            let needs = context.take_needs();
            if needs.is_empty() {
                match attempt {
                    Ok(elaborated) => self.survivors.keep(ItemStamp(position), elaborated),
                    Err(error) => {
                        mark.undo(context, &names);
                        self.poison.declare(item);
                        self.survivors.drop_item(item);
                        self.survivors.refuse(ItemStamp(position), error);
                    }
                }
                return Ok(());
            }

            curios_profile::sample!("demand::void", needs.len());
            mark.void(context, module, &names);
            let dependencies = self.owners(needs, position);
            assert!(
                !dependencies.is_empty(),
                "an attempt needs a declaration other than its own: {}",
                item.describe()
            );
            for dependency in dependencies {
                self.item(context, dependency)?;
            }
        }
    }
}
