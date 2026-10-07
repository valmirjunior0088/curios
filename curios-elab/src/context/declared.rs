//! The witnesses a unit declares, as they are known before they elaborate.
//!
//! A witness is known under the concept and the key its name states (`curios_core::WitnessId`), so what a goal, the auto-lift's monad check and a report's survey each ask — whether a witness stands at a key — is answered from every witness the unit declares, whichever of them has elaborated. The witness table (`Program`) holds the ones that have; the ones still to elaborate are kept here, with the keys a refused one stood at, since an answer read off the table alone would follow the order items elaborate in.
//!
//! None of this exists where nothing is being elaborated: a context that only reads finished terms declares no witness, and every one it can be asked about is in the table.

use {
    curios_core::{Global, WitnessKey},
    curios_utilities::Qualifier,
    std::collections::{BTreeMap, BTreeSet},
};

/// A witness the unit declares, as it is known before it elaborates.
#[derive(Debug)]
pub(crate) struct Declared {
    /// The concept and the key its signature is spelled at ([`Keying`](curios_core::Keying)). `None` where the spelling decides neither: no question is answered by it, and its registration refuses it.
    pub(crate) spelled: Option<(Global, WitnessKey)>,
    /// The module declaring it, the coordinate a report names an anonymous witness by.
    pub(crate) module: Qualifier,
    /// Where its item sits in the unit's order, which says which of two witnesses spelled alike is the duplicate.
    pub(crate) position: usize,
}

/// The witnesses of the unit being elaborated: which definitions are witness declarations, the ones that have not finished, and the keys refused ones stood at.
#[derive(Debug, Default)]
pub(crate) struct DeclaredWitnesses {
    /// The definition names `into_core` marked as witness declarations; each registers into the witness table when its signature elaborates (`elaborate_module_let` → `register_witness`).
    marked: BTreeSet<Global>,
    /// The witnesses the unit declares that have not finished elaborating, each under the key its signature is spelled at. With the table this is every witness a question about a concept is answered by, whichever of them has elaborated: the table alone holds the ones that have, and an answer read off it would follow the order items elaborate in.
    unfinished: BTreeMap<Global, Declared>,
    /// The keys a refused declaration's witnesses stood under. A goal keyed here would have resolved through the refused witness, so it is the refusal's dependent and reports nothing of its own — where a plain miss would report `no witness` for a witness the reader can see.
    poisoned: BTreeSet<(Global, WitnessKey)>,
}

impl DeclaredWitnesses {
    /// Mark a definition name as a witness declaration; when its signature elaborates, `elaborate_module_suffix` registers it into the witness table.
    pub(crate) fn mark(&mut self, name: &Global) {
        self.marked.insert(*name);
    }

    /// Whether `name` is a witness declaration of the unit.
    pub(crate) fn is_marked(&self, name: &Global) -> bool {
        self.marked.contains(name)
    }

    /// Record that the unit declares the witness `name` — once per witness declaration as a module is seeded, before any item elaborates.
    pub(crate) fn declare(&mut self, name: Global, declared: Declared) {
        self.unfinished.insert(name, declared);
    }

    /// Forget the declared witness `name`, handing back the key it was spelled at: it has elaborated and the table answers for it, or it was refused and nothing does.
    pub(crate) fn settle(&mut self, name: &Global) -> Option<(Global, WitnessKey)> {
        self.unfinished.remove(name)?.spelled
    }

    /// The key the declared witness `name` is spelled at — `Some(None)` where its spelling decides none, and `None` for a name the unit does not hold as a declared witness: one a unit in scope registered, or one that has finished.
    pub(crate) fn spelled(&self, name: &Global) -> Option<Option<&(Global, WitnessKey)>> {
        self.unfinished
            .get(name)
            .map(|declared| declared.spelled.as_ref())
    }

    /// The declared witness spelled at `(concept, key)`: the one that registers there once it elaborates. Where two are spelled alike it is the first as written, so the second is the duplicate whichever of them a goal asks for first.
    pub(crate) fn at(&self, concept: &Global, key: &WitnessKey) -> Option<Global> {
        self.unfinished
            .iter()
            .filter(|(_, declared)| {
                declared
                    .spelled
                    .as_ref()
                    .is_some_and(|(spelled, at)| spelled == concept && at == key)
            })
            .min_by_key(|(_, declared)| declared.position)
            .map(|(name, _)| *name)
    }

    /// Every key a declared witness of `concept` is spelled at, with the module declaring it.
    pub(crate) fn keys(&self, concept: &Global) -> impl Iterator<Item = (&WitnessKey, &Qualifier)> {
        self.unfinished.values().filter_map(move |declared| {
            let (spelled, key) = declared.spelled.as_ref()?;
            (spelled == concept).then_some((key, &declared.module))
        })
    }

    /// Mark `(concept, key)` as a slot a refused witness stood in.
    pub(crate) fn poison(&mut self, concept: Global, key: WitnessKey) {
        self.poisoned.insert((concept, key));
    }

    /// Whether a refused witness stood at `(concept, key)`.
    pub(crate) fn is_poisoned(&self, concept: &Global, key: &WitnessKey) -> bool {
        self.poisoned.contains(&(*concept, *key))
    }
}
