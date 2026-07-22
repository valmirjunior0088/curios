//! Top-level items: the dominance-ordered item chain.
//!
//! Items become statements of the module's top-level item list, evaluated
//! eagerly in emission order. Bindings persist in the Core base frame (no
//! scoping frame), so later items and the entrypoint reduce through them —
//! top-level cross-references are already free names.

use super::{BTreeMap, BTreeSet, Bound, Context, Error, Item, Lowering, Module, Outcome};

impl Lowering {
    /// Erase every top-level item from `start` on, in dominance order among
    /// themselves (see [`dominance_order`]); items before `start` are an
    /// already-erased prefix whose bindings the environment carries.
    pub(super) fn erase_items(
        &mut self,
        context: &mut Context,
        module: &Module,
        start: usize,
    ) -> Result<(), Error> {
        for index in dominance_order(module, start) {
            let item = &module.items[index];
            match item {
                Item::Let(definition) => {
                    let outcome = self.walk(
                        context,
                        &definition.body,
                        &definition.type_,
                        Some(&definition.name),
                    )?;
                    let atom = match outcome {
                        Outcome::Emitted(atom) => atom,
                        // A diverging initializer (a vacuous elimination) has
                        // no result operand; give it the computed-member
                        // encoding — a value whose init block seals with the
                        // divergence terminator — so the program traps at
                        // initialization, matching the entry-block convention.
                        Outcome::Diverged(terminator) => {
                            let value = self.builder.value(Some(definition.name.clone()));
                            self.builder.open_block();
                            let block = self.builder.seal_block(terminator);
                            let group = self.builder.rec_group(vec![], vec![(value, block)]);
                            self.builder.let_rec(group);
                            curios_ersd::Atom::Value(value)
                        }
                    };
                    context.define_assuming(&definition.name, &definition.type_, &definition.body);
                    self.environment.bind(&definition.name, atom);
                }
                Item::Rec(rec) => {
                    self.erase_rec_item(context, rec)?;
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
fn dominance_order(module: &Module, start: usize) -> Vec<usize> {
    let items = &module.items[start..];
    let count = items.len();

    let owner = items
        .iter()
        .enumerate()
        .flat_map(|(index, item)| {
            item.declared_names()
                .into_iter()
                .map(move |name| (name, index))
        })
        .collect::<BTreeMap<&str, usize>>();

    // A reference to an item before `start` is already bound and carries no
    // edge; only references among the suffix items order the sort.
    let dependencies = items
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
    order.into_iter().map(|index| index + start).collect()
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
        if let Some(induct_decl) = module.induct_decls.get(name) {
            names.extend(induct_decl.params.free_vars());
            names.extend(induct_decl.indices.free_vars());
            for parameter in induct_decl.constructors.values() {
                names.extend(parameter.telescope.free_vars());
            }
        }
        if let Some(struct_decl) = module.struct_decls.get(name) {
            names.extend(struct_decl.params.free_vars());
            names.extend(struct_decl.fields.free_vars());
        }
    }
    names
}
