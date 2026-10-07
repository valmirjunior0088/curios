//! Top-level items — erasing one definition into its fragment and binding its name there.
//!
//! An item's name is bound in its fragment's environment, which is what the item hands on of itself: how each name it declares is bound is all another item's erasure reads of it. The Core context holds every item's definition before any is erased, so nothing here writes one.

use {
    super::{Error, Lowering, Outcome, Retyping, Subterm},
    curios_core::{Definition, Free},
};

impl Lowering<'_> {
    /// Erase one top-level definition as the fragment's item, and bind its name.
    pub(super) fn erase_definition(
        &mut self,
        context: &mut Retyping,
        definition: &Definition,
    ) -> Result<(), Error> {
        let symbol = definition.name.symbol();
        // A function's body runs where it is called, so writing one computes nothing and it is walked whatever it returns. Any other item is a value computed at initialization, which makes it a kept slot as a local binding is: a proof or a type is bound to its stand-in, and a lemma applied at the top level does not run before the program does.
        let outcome = self.with_owner(symbol.clone(), |lowering| match &*definition.body {
            Subterm::Func(_) => {
                lowering.walk(context, &definition.body, &definition.type_, Some(&symbol))
            }
            _ => lowering.kept_operand(context, &definition.body, &definition.type_, Some(&symbol)),
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
        self.environment.bind(&Free::from(&definition.name), atom);

        Ok(())
    }
}
