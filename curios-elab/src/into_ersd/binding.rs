//! Top-level items: the item chain, in the order the module stores them.
//!
//! Items become statements of the module's top-level item list, evaluated eagerly in emission order. Bindings persist in the Core base frame (no scoping frame), so later items and the entrypoint reduce through them — top-level cross-references are already free names.

use {
    super::{Context, Error, Lowering, Outcome, Subterm},
    curios_core::{Free, Item, Module},
};

impl Lowering {
    /// Erase every top-level item, in the order the module stores them: every item after the items it reaches ([`Module::ordered`]), which is what lets each reference resolve to an operand already bound.
    ///
    /// A module carries only its own items: what an earlier erasure established arrives as bindings the environment already holds, so there is no prefix here to start past.
    ///
    /// Each item is erased on a budget restored for it, as it was elaborated on one: whether an item erases is a fact about that item, never about what the items before it spent.
    pub(super) fn erase_items(
        &mut self,
        context: &mut Context,
        module: &Module,
    ) -> Result<(), Error> {
        for item in &module.items {
            context.restore_budget();
            self.types.clear();
            match item {
                Item::Let(definition) => {
                    let symbol = definition.name.symbol();
                    // A function's body runs where it is called, so writing one computes nothing and it is walked whatever it returns. Any other item is a value computed at initialization, which makes it a kept slot as a local binding is: a proof or a type is bound to its stand-in, and a lemma applied at the top level does not run before the program does.
                    let outcome =
                        self.with_owner(symbol.clone(), |lowering| match &*definition.body {
                            Subterm::Func(_) => lowering.walk(
                                context,
                                &definition.body,
                                &definition.type_,
                                Some(&symbol),
                            ),
                            _ => lowering.kept_operand(
                                context,
                                &definition.body,
                                &definition.type_,
                                Some(&symbol),
                            ),
                        })?;
                    let atom = match outcome {
                        Outcome::Emitted(atom) => atom,
                        // A diverging initializer (a vacuous elimination) has no result operand; give it the computed-member encoding — a value whose init block seals with the divergence terminator — so the program traps at initialization, matching the entry-block convention.
                        Outcome::Diverged(terminator) => {
                            let value = self.builder.value(Some(definition.name.symbol()));
                            self.builder.open_block();
                            let block = self.builder.seal_block(terminator);
                            let group = self.builder.rec_group(vec![], vec![(value, block)]);
                            self.builder.let_rec(group);
                            curios_ersd::Atom::Value(value)
                        }
                    };
                    let name = Free::from(&definition.name);
                    context.define_assuming_scheme(
                        &name,
                        &definition.type_,
                        &definition.body,
                        Some(&definition.kind),
                        definition.universe_context.clone(),
                    );
                    self.environment.bind(&name, atom);
                }
                Item::Rec(rec) => {
                    self.erase_rec_item(context, rec)?;
                }
            }
        }
        Ok(())
    }
}
