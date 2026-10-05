//! Per-item recovery: what a refused item leaves in the context and how it is taken back out, which items its refusal withholds, and how an attempt that read something unfinished is undone to be made again.
//!
//! **A refused item is undone and poisoned; an item that reaches a poisoned name is withheld, and reports nothing of its own.** Reaching is [`Module::reaches`]: by mention, by constructing a nominal type, through a registry entry, and — the one edge no lowered term shows — through a witness a refused or withheld declaration declares, which resolution meets as a poisoned key: the one its signature is spelled at, whether or not it registered. The withheld item's report would restate the refusal it depends on, and that is the kernel's precedent: a dependent of a refused declaration says nothing in `curios-cert`'s recheck either.
//!
//! **What is undone** is everything another item could read: the item's base-frame bindings, its registry entries, its witness-table entries (each key poisoned in its place), the parked work it raised, the terms it recorded for the erasure obligations, its universe constraints, and every speculative scope its unwinding left open. **What is left** is what nothing reads: term metavariables it minted, which only its own terms mention and no module walk reaches; the universe metas beside them; its totality verdict, which is recorded on the success path alone; and the caches, cleared wholesale as for a redefinition.
//!
//! **A void attempt is undone as a refused item is, and poisons nothing.** An attempt that read a declaration not yet elaborated concluded nothing ([`Context::need`]), so it leaves no refusal, no poisoned key and no dependent to withhold: its declaration goes back to what the unit seeded — its lowered registry entries, no binding, no table entry, a witness still declared under the key it is spelled at — and is attempted again once what it needed has elaborated.
//!
//! **What a run reports is not what it dropped.** A refused item reports; a withheld one reports nothing, by the decision above. So the refusals a run collects answer whether anything was said, never whether the module still mirrors the lowering it was handed — an item can leave with nothing recorded against it. [`Survivors`] keeps both answers: the refusals, for the reader, and every name no item came out for, for a caller that reassembles the lowered order and must tell an item this run deliberately dropped from one it lost.

use {
    crate::{Attempted, Context, Error, GoalSite},
    curios_core::{Entrypoint, Free, Global, Item, Module},
    std::{collections::BTreeSet, rc::Rc},
};

/// A top-level item's position in its module's order, with the entry after the last item: what an item is kept and a refusal filed under, so both come back in item order whatever order the items elaborated in.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct ItemStamp(pub(super) usize);

/// The names whose declarations are refused or withheld: what another item must not reach.
#[derive(Clone)]
pub(super) struct Poison {
    names: BTreeSet<Global>,
}

impl Poison {
    /// Poisoned before the first item: the names a lowering reported broken.
    pub(super) fn seeded(names: BTreeSet<Global>) -> Self {
        Self { names }
    }

    /// Poison every name `item` declares.
    pub(super) fn declare(&mut self, item: &Item) {
        self.names
            .extend(item.declared_names().into_iter().cloned());
    }

    fn any(&self, names: &BTreeSet<Global>) -> bool {
        names.iter().any(|name| self.names.contains(name))
    }

    /// Whether `item`, as lowered, declares or reaches a poisoned name — the test before it is elaborated. Free when nothing is poisoned, which is every run that refuses nothing.
    pub(super) fn reaches(&self, module: &Module, item: &Item) -> bool {
        if self.names.is_empty() {
            return false;
        }

        item.declared_names()
            .into_iter()
            .any(|name| self.names.contains(name))
            || self.any(&module.reaches(item))
    }

    /// Whether `name` itself is poisoned — asked of a test the tail would schedule, by name, because a test a predecessor unit declared is no item of this module to reach.
    pub(super) fn holds(&self, name: &Global) -> bool {
        self.names.contains(name)
    }

    /// Whether the entry reaches a poisoned name, through its body or its annotation.
    pub(super) fn reaches_entry(&self, entry: &Entrypoint) -> bool {
        !self.names.is_empty() && self.any(&entry.reaches())
    }
}

/// Take a withheld item out, as [`withdraw`] takes a refused one: it never elaborates, so what is taken is what the unit seeded for it.
///
/// **A withheld witness poisons the key it is spelled at.** It registers nothing, so no table entry stands under its name to poison in its place, and every consumer would report `no witness of C(T) found`: a second record for one mistake, at a declaration with nothing wrong with it. The key is known all the same, since a witness is declared under the key its signature spells before anything elaborates, and [`withdraw`] poisons that.
pub(super) fn withhold(context: &mut Context, item: &Item) {
    withdraw(context, &item.declared_names());
}

/// Take `declared` out of every store another item could read: the witness table, poisoning each key a witness held; the witnesses the unit declares and has not elaborated, poisoning the key each was spelled at; the registries; and the base frame. Asked for a refused item and for a withheld one, whose registry entries were seeded before any item elaborated.
pub(super) fn withdraw(context: &mut Context, declared: &[&Global]) {
    for name in declared {
        for (concept, key) in context.remove_witness(name) {
            context.poison_witness_key(concept, key);
        }
        context.withdraw_declared_witness(name);
        context.remove_induct(name);
        context.remove_struct(name);
        context.remove_concept(name);
        context.forget(&Free::from(*name));
    }
}

/// Where an attempt at an item began, so what it wrote can be taken back when it is refused or void.
pub(super) struct ItemMark {
    checked: usize,
    site: Rc<str>,
}

impl ItemMark {
    /// Remember what an attempt beginning here may have to give back.
    pub(super) fn begin(context: &Context) -> Self {
        Self {
            checked: context.checked_mark(),
            site: context.checked_site(),
        }
    }

    /// What a refusal and a void attempt alike take back before their declarations: the terms recorded for the erasure obligations and the parked work.
    fn unwind(self, context: &mut Context) {
        context.truncate_checked(self.checked);
        context.restore_checked_site(self.site);
        context.take_parked();
        // The wake signals the item's solutions raised, now that nothing is parked to wake.
        context.wake_parked();
    }

    /// Close the universe stores as a successful boundary closes them, then abandon every speculative scope the unwinding left open: the brackets on the resolution cycle release by hand on their success paths, and an error unwinds past every one of them, so this is the one place a scope can be closed with no rollback pending.
    fn close(context: &mut Context) {
        context.finish_universe_transaction();
        context.abandon_universe_speculation();
    }

    /// Undo what a refused item wrote since [`ItemMark::begin`] and take its declarations out — see the module documentation for what is undone and what is left.
    pub(super) fn undo(self, context: &mut Context, declared: &[&Global]) {
        self.unwind(context);
        withdraw(context, declared);
        Self::close(context);
    }

    /// Undo an attempt that read something unfinished: what [`ItemMark::undo`] takes back, with nothing poisoned and the declarations left as `module` seeded them, for the attempt to be made again.
    pub(super) fn void(self, context: &mut Context, module: &Module, declared: &[&Global]) {
        self.unwind(context);
        for name in declared {
            context.remove_witness(name);
            context.remove_induct(name);
            context.remove_struct(name);
            context.remove_concept(name);
            context.forget(&Free::from(*name));
            if let Some(declaration) = module.induct_decls.get(*name) {
                context
                    .register_induct(name, declaration.clone())
                    .expect("the entry was just taken out");
            }
            if let Some(declaration) = module.struct_decls.get(*name) {
                context
                    .register_struct(name, declaration.clone())
                    .expect("the entry was just taken out");
            }
            if let Some(concept) = module.concepts.get(*name) {
                context
                    .register_concept(name, concept.clone())
                    .expect("the entry was just taken out");
            }
        }
        Self::close(context);
    }
}

/// A declaration that wrote goals: the state it elaborated in, which alone knows them, and each of its items that holds one, as it elaborated, with where.
pub(super) struct Holding {
    pub(super) state: Attempted,
    pub(super) items: Vec<(Item, Vec<GoalSite>)>,
}

/// What a unit's walk came to, in item order ([`Survivors::into_parts`]).
pub(super) struct Survived {
    pub(super) items: Vec<Item>,
    pub(super) refusals: Vec<Error>,
    pub(super) held: Vec<Holding>,
    pub(super) dropped: BTreeSet<Global>,
}

/// The items elaborated so far, with the refusals recorded against their positions and the names of the items that did not survive.
#[derive(Default)]
pub(super) struct Survivors {
    kept: Vec<(ItemStamp, Item)>,
    refusals: Vec<(ItemStamp, Error)>,
    held: Vec<Holding>,
    dropped: BTreeSet<Global>,
}

impl Survivors {
    pub(super) fn keep(&mut self, stamp: ItemStamp, item: Item) {
        self.kept.push((stamp, item));
    }

    /// Record that no item was produced for what `item` declares — it was withheld before elaborating, or refused.
    ///
    /// Kept beside the refusals because the two answer different questions. A refusal says something reported; this says the module no longer mirrors the lowering, which is what a caller reassembling the lowered order needs and cannot read off an absence.
    pub(super) fn drop_item(&mut self, item: &Item) {
        self.dropped
            .extend(item.declared_names().into_iter().cloned());
    }

    /// Keep a declaration that wrote goals for their report. No item is produced for one that holds a goal: a unit that holds a goal is refused for the goal, whatever else it holds.
    pub(super) fn hold(&mut self, holding: Holding) {
        self.held.push(holding);
    }

    /// Record `error` as `stamp`'s refusal — unless it says the item met a poisoned witness key, which is a dependent's silence rather than a report.
    pub(super) fn refuse(&mut self, stamp: ItemStamp, error: Error) {
        if !matches!(error.unwrapped(), Error::Poisoned) {
            self.refusals.push((stamp, error));
        }
    }

    /// Every name the kept items declare.
    pub(super) fn names(&self) -> BTreeSet<Global> {
        self.kept
            .iter()
            .flat_map(|(_, item)| item.declared_names())
            .cloned()
            .collect()
    }

    /// The kept items and the refusals, each in item order — the entry's refusal after every item's, and a whole-module pass's after the entry's — with the declarations that wrote goals, whose reports are ordered where they are made, and every name an item that did not survive declared. Items elaborate when they are asked for, so the order they were kept in is not the module's.
    pub(super) fn into_parts(mut self) -> Survived {
        self.kept.sort_by_key(|(stamp, _)| *stamp);
        self.refusals.sort_by_key(|(stamp, _)| *stamp);

        Survived {
            items: self.kept.into_iter().map(|(_, item)| item).collect(),
            refusals: self.refusals.into_iter().map(|(_, error)| error).collect(),
            held: self.held,
            dropped: self.dropped,
        }
    }
}
