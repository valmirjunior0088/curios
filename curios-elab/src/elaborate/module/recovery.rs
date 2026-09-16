//! Per-item recovery: what a refused item leaves in the context and how it is taken back out, which later items its refusal withholds, and what a refusal that surfaces late retracts.
//!
//! **A refused item is undone and poisoned; a later item that reaches a poisoned name is withheld, and reports nothing of its own.** Reaching is [`Module::reaches`]: by mention, by constructing a nominal type, through a registry entry, and — the one edge no lowered term shows — through a witness a refused declaration registered before its body failed, which resolution meets as a poisoned key. The withheld item's report would restate the refusal it depends on, and that is the kernel's precedent: a dependent of a refused declaration says nothing in `curios-cert`'s recheck either.
//!
//! **What is undone** is everything a later item could read: the item's base-frame bindings, its registry entries, its witness-table entries (each key poisoned in its place), the parked work and deferred goals it raised, the terms it recorded for the erasure obligations, its universe constraints, and every speculative scope its unwinding left open. **What is left** is what nothing reads: term metavariables it minted, which only its own terms mention and no module walk reaches; the universe metas beside them; its totality verdict, which is recorded on the success path alone; and the caches, cleared wholesale as for a redefinition.
//!
//! **What a run reports is not what it dropped.** A refused item reports; a withheld or retracted one reports nothing, by the decision above. So the refusals a run collects answer whether anything was said, never whether the module still mirrors the lowering it was handed — an item can leave with nothing recorded against it. [`Survivors`] keeps both answers: the refusals, for the reader, and every name no item came out for, for a caller that reassembles the lowered order and must tell an item this run deliberately dropped from one it lost.
//!
//! **A refusal can surface after its item elaborated**: a witness goal deferred for a table entry that never arrived is reported by the sweep after a later item, or at the end. The item that raised it is retracted then — taken out as a refused item is, and the refusal reported as its own — and so is every kept item that reaches it, since one elaborated before the refusal surfaced may hold the retracted item's witness, resolved late; the retraction runs to a fixpoint over the elaborated items rather than the lowered ones for that reason.

use {
    crate::{Context, DeferredRefusal, Error, ItemStamp, check_is_sort, read_witness_signature},
    curios_core::{Entrypoint, Free, Global, Item, Module, Term},
    std::{collections::BTreeSet, rc::Rc},
};

/// The names whose declarations are refused or withheld: what a later item must not reach.
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

    /// Whether a term reaches a poisoned name — the expected type the driver hands in, which is no part of the module.
    pub(super) fn touches(&self, term: &Term) -> bool {
        !self.names.is_empty() && self.any(&term.reaches())
    }

    /// Whether `item`, as elaborated, reaches a poisoned name: its definitions and the registry entries it declares as the context now holds them — the form that shows a witness resolved late.
    fn reaches_elaborated(&self, context: &Context, item: &Item) -> bool {
        if self.names.is_empty() {
            return false;
        }

        let mut names = item.reaches();
        for name in item.declared_names() {
            if let Some(declaration) = context.induct_decl(name) {
                names.extend(declaration.reaches());
            }
            if let Some(declaration) = context.struct_decl(name) {
                names.extend(declaration.reaches());
            }
            if let Some(concept) = context.concept(name) {
                names.extend(concept.reaches());
            }
        }

        self.any(&names)
    }
}

/// Take a withheld item out: its witness keys poisoned where it declares one, and everything else [`withdraw`] takes.
///
/// **A withheld witness declaration is the one place the silence leaked.** A *refused* witness had registered before its body failed — `elaborate_module_let` registers on the signature, so a witness can recurse through its own entry — so undoing it poisoned its key in place and a consumer met the poison and said nothing. A withheld one never elaborates at all, so there is no entry under its name to remove and no key to poison, and every consumer reported `no witness of C(T) found`: a second record for one mistake, at a declaration with nothing wrong with it.
///
/// A key is a fact about the *elaborated* signature — reduction leaves a lowered concept application an `Apply`, and only elaboration produces the `StructType` the key reads its heads off — so the signature is elaborated here, under the mark that undoes a refused item, and the mark taken straight back. What survives it is the poison, which is the point. A signature that does not elaborate poisons nothing, and needs to poison nothing: it is the signature itself that reaches the poison then, so every consumer that could have formed the goal is withheld on its own account.
pub(super) fn withhold(context: &mut Context, stamp: ItemStamp, item: &Item) {
    let declared = item.declared_names();
    let witnesses = item
        .definitions()
        .into_iter()
        .filter(|definition| context.is_witness_declaration(&definition.name))
        .collect::<Vec<_>>();
    if witnesses.is_empty() {
        withdraw(context, &declared);
        return;
    }

    let mark = ItemMark::begin(context, stamp);
    let keys = witnesses
        .iter()
        .filter_map(|definition| {
            let signature = check_is_sort(context, &definition.type_).ok()?.0;
            let read = read_witness_signature(context, &signature).ok()?;

            Some((read.concept, read.key))
        })
        .collect::<Vec<_>>();
    mark.undo(context, &declared);

    for (concept, key) in keys {
        context.poison_witness_key(concept, key);
    }
}

/// Take `declared` out of every store a later item could read: the witness table, poisoning each key a witness held; the registries; and the base frame. Asked for a refused item, a withheld one — whose registry entries were seeded before any item elaborated — and a retracted one alike.
pub(super) fn withdraw(context: &mut Context, declared: &[&Global]) {
    for name in declared {
        for (concept, key) in context.remove_witness(name) {
            context.poison_witness_key(concept, key);
        }
        context.remove_induct(name);
        context.remove_struct(name);
        context.remove_concept(name);
        context.forget(&Free::from(*name));
    }
}

/// Where an item began, so what it wrote can be undone when it is refused.
pub(super) struct ItemMark {
    stamp: ItemStamp,
    checked: usize,
    site: Rc<str>,
}

impl ItemMark {
    /// Enter `stamp`'s item, remembering what it may have to give back.
    pub(super) fn begin(context: &mut Context, stamp: ItemStamp) -> Self {
        context.begin_item(stamp);

        Self {
            stamp,
            checked: context.checked_mark(),
            site: context.checked_site(),
        }
    }

    /// Undo what the item wrote since [`ItemMark::begin`] and take its declarations out — see the module documentation for what is undone and what is left.
    ///
    /// The universe stores are closed as a successful boundary closes them, then every speculative scope the unwinding left open is abandoned: the brackets on the resolution cycle release by hand on their success paths, and an error unwinds past every one of them, so this is the one place a scope can be closed with no rollback pending.
    pub(super) fn undo(self, context: &mut Context, declared: &[&Global]) {
        context.truncate_checked(self.checked);
        context.restore_checked_site(self.site);
        context.take_parked();
        // The wake signals the item's solutions raised, now that nothing is parked to wake.
        context.wake_parked();
        context.drop_deferred_of(self.stamp);
        withdraw(context, declared);
        context.finish_universe_transaction();
        context.abandon_universe_speculation();
    }
}

/// Take a retracted item out as a refused one is taken out, and poison its names.
fn retract_one(context: &mut Context, poison: &mut Poison, stamp: ItemStamp, item: &Item) {
    withdraw(context, &item.declared_names());
    context.retract_checked(stamp);
    context.drop_deferred_of(stamp);
    poison.declare(item);
}

/// The items elaborated so far, with the refusals recorded against their positions and the names of the items that did not survive.
pub(super) struct Survivors {
    /// The entry's stamp — the position after the last item, which a late refusal of the entry's own goal carries.
    entry: ItemStamp,
    kept: Vec<(ItemStamp, Item)>,
    refusals: Vec<(ItemStamp, Error)>,
    dropped: BTreeSet<Global>,
}

impl Survivors {
    pub(super) fn new(entry: ItemStamp) -> Self {
        Self {
            entry,
            kept: Vec::new(),
            refusals: Vec::new(),
            dropped: BTreeSet::new(),
        }
    }

    pub(super) fn keep(&mut self, stamp: ItemStamp, item: Item) {
        self.kept.push((stamp, item));
    }

    /// Record that no item was produced for what `item` declares — it was withheld before elaborating, refused, or retracted after the fact.
    ///
    /// Kept beside the refusals because the two answer different questions and only one of them was ever asked. A refusal says something reported; this says the module no longer mirrors the lowering, which is what a caller reassembling the lowered order needs and cannot read off an absence.
    pub(super) fn drop_item(&mut self, item: &Item) {
        self.dropped
            .extend(item.declared_names().into_iter().cloned());
    }

    /// Record `error` as `stamp`'s refusal — unless it says the item met a poisoned witness key, which is a dependent's silence rather than a report.
    pub(super) fn refuse(&mut self, stamp: ItemStamp, error: Error) {
        if !matches!(error.unwrapped(), Error::Poisoned) {
            self.refusals.push((stamp, error));
        }
    }

    pub(super) fn items(&self) -> Vec<&Item> {
        self.kept.iter().map(|(_, item)| item).collect()
    }

    /// Every name the kept items declare.
    pub(super) fn names(&self) -> BTreeSet<Global> {
        self.kept
            .iter()
            .flat_map(|(_, item)| item.declared_names())
            .cloned()
            .collect()
    }

    /// Retract the items the late refusals name, and every kept item that reaches one: each is taken out of the context and poisoned as a refused item is, and its refusal reported as its own. `true` when the entry was among them, which the caller withholds from the module.
    pub(super) fn retract(
        &mut self,
        context: &mut Context,
        poison: &mut Poison,
        late: Vec<DeferredRefusal>,
    ) -> bool {
        let mut entry_retracted = false;
        for DeferredRefusal { item: stamp, error } in late {
            if stamp == self.entry {
                // Nothing depends on the entry, so nothing follows it out; a second goal of its reports nothing more.
                if !entry_retracted {
                    entry_retracted = true;
                    context.retract_checked(stamp);
                    self.refuse(stamp, error);
                }
                continue;
            }

            // Already retracted by an earlier refusal of the same item, or by reaching one.
            let Some(index) = self.kept.iter().position(|(kept, _)| *kept == stamp) else {
                continue;
            };
            let (_, item) = self.kept.remove(index);
            let described = item.describe();
            retract_one(context, poison, stamp, &item);
            self.drop_item(&item);
            self.refuse(stamp, error.in_declaration(&described));
            self.retract_dependents(context, poison);
        }

        entry_retracted
    }

    /// Retract every kept item that reaches a poisoned name, to a fixpoint — each retraction can expose another.
    fn retract_dependents(&mut self, context: &mut Context, poison: &mut Poison) {
        while let Some(index) = self
            .kept
            .iter()
            .position(|(_, item)| poison.reaches_elaborated(context, item))
        {
            let (stamp, item) = self.kept.remove(index);
            retract_one(context, poison, stamp, &item);
            self.drop_item(&item);
        }
    }

    /// The kept items in their order, the refusals in item order — an item's late refusal sorts where the item stood, the entry's after every item's, and a whole-module pass's after the entry's — and every name an item that did not survive declared.
    pub(super) fn into_parts(mut self) -> (Vec<Item>, Vec<Error>, BTreeSet<Global>) {
        self.refusals.sort_by_key(|(stamp, _)| *stamp);

        (
            self.kept.into_iter().map(|(_, item)| item).collect(),
            self.refusals.into_iter().map(|(_, error)| error).collect(),
            self.dropped,
        )
    }
}
