//! Top-level items: the dominance-ordered item chain.
//!
//! Items become statements of the module's top-level item list, evaluated
//! eagerly in emission order. Bindings persist in the Core base frame (no
//! scoping frame), so later items and the entrypoint reduce through them —
//! top-level cross-references are already free names.

use super::{BTreeMap, BTreeSet, Bound, Context, Error, Item, Lowering, Module, Outcome};

impl Lowering {
    /// Erase every top-level item, in dominance order (see
    /// [`dominance_order`]).
    pub(super) fn erase_items(
        &mut self,
        context: &mut Context,
        module: &Module,
    ) -> Result<(), Error> {
        for index in dominance_order(module) {
            let item = &module.items[index];
            // Stamp the item's declaring module so the re-derived types run
            // the struct representation-privacy check against the right
            // use-site island (mirrors elaboration).
            match item {
                Item::Let(definition) => {
                    context.set_island(definition.island.clone());
                    let outcome = self.walk(
                        context,
                        &definition.body,
                        &definition.type_,
                        Some(&definition.name),
                    )?;
                    let Outcome::Emitted(atom) = outcome else {
                        unimplemented!("erase_ir: a top-level item initializer diverges")
                    };
                    context.define_assuming(&definition.name, &definition.type_, &definition.body);
                    self.environment.bind(&definition.name, atom);
                }
                Item::Rec(_) => {
                    unimplemented!("erase_ir: recursive items land in a later sub-step")
                }
            }
        }
        Ok(())
    }
}

/// The module's top-level items in dominance order — every item precedes the
/// items that reference it — as indices into `module.items`.
///
/// The surface-to-core lowering already sorts the items it can see, but a
/// witness reference is only spliced into its consumer during elaboration,
/// after that sort has run — so a witness definition can sit after a consumer
/// in the flat list. Eager erasure resolves every reference to an
/// already-bound operand as it threads the chain, so it needs a true dominance
/// order: the same Kahn sort, re-run over the elaborated terms whose free
/// variables now include the spliced witness references. (The legacy path
/// resolves top-level names lazily against one global environment and never
/// needed this.)
///
/// Independent items keep their flat order (lowest-index-ready tiebreak). A
/// value cycle across top-level items is unexpressible, so the stall fallback
/// that emits the lowest remaining item only guarantees termination.
fn dominance_order(module: &Module) -> Vec<usize> {
    let count = module.items.len();

    let owner = module
        .items
        .iter()
        .enumerate()
        .flat_map(|(index, item)| {
            item.declared_names()
                .into_iter()
                .map(move |name| (name, index))
        })
        .collect::<BTreeMap<&str, usize>>();

    let dependencies = module
        .items
        .iter()
        .enumerate()
        .map(|(index, item)| {
            item_reference_names(item, module)
                .iter()
                .filter_map(|name| owner.get(name.as_str()).copied())
                .filter(|&dependency| dependency != index)
                .collect::<BTreeSet<usize>>()
        })
        .collect::<Vec<_>>();

    let mut emitted = vec![false; count];
    let mut order = Vec::with_capacity(count);
    while order.len() < count {
        let ready = (0..count)
            .find(|&n| !emitted[n] && dependencies[n].iter().all(|dep| emitted[*dep]))
            .or_else(|| (0..count).find(|&n| !emitted[n]))
            .expect("an item remains while the order is incomplete");
        emitted[ready] = true;
        order.push(ready);
    }
    order
}

/// Every global name an item references: the free variables of its types and
/// bodies, plus — for an item declaring a registered inductive or struct — the
/// free variables of that registry entry's telescopes, whose field and index
/// types live nowhere in the type former's own normal-form body.
fn item_reference_names(item: &Item, module: &Module) -> BTreeSet<String> {
    let mut names = BTreeSet::new();
    match item {
        Item::Let(definition) => {
            names.extend(definition.type_.free_vars());
            names.extend(definition.body.free_vars());
        }
        Item::Rec(rec) => {
            for definition in rec.definitions() {
                names.extend(definition.type_.free_vars());
                names.extend(definition.body.free_vars());
            }
        }
    }
    for name in item.declared_names() {
        if let Some(inductive) = module.inductives.get(name) {
            names.extend(inductive.params.free_vars());
            names.extend(inductive.indices.free_vars());
            for parameter in inductive.constructors.values() {
                names.extend(parameter.telescope.free_vars());
            }
        }
        if let Some(structure) = module.structures.get(name) {
            names.extend(structure.params.free_vars());
            names.extend(structure.fields.free_vars());
        }
    }
    names
}
