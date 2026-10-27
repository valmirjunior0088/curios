//! What a suite measures of the closed-call pass and no compile does: what each replacement holding a description would build.
//!
//! Compiled for this crate's tests and behind the `test-support` feature, so no build that ships holds it.

use super::*;

impl Module {
    /// What each replacement holding a description would build among this module's closed calls, in the nodes the dry run charges for it.
    ///
    /// The pass's own steps over the module as one round finds it: each plan that holds a description is run dry under one replacement's widest budget in place of [`DESCRIPTION_COPY_NODE_LIMIT`], so a copy the tight cap declines is weighed beside those it admits, and a reader holds each to the cap by what the evaluator charges. A plan that materializes under no budget has no charge.
    pub(crate) fn description_copy_charges(&self) -> Vec<usize> {
        let block_items = index_block_items(self);
        let item_positions = index_item_positions(self);
        let mut scope = ReifyScope::new();

        plan(self, every_statement(self).into_iter())
            .into_iter()
            .flatten()
            .filter(|plan| plan.contains_description(self))
            .filter_map(|plan| {
                let (_, _, position) = plan.splice_point(&block_items, &item_positions);
                scope.begin_replacement(position, plan.block(), plan.looping(self));
                let mut probe = ReifyBudget::new();

                plan.materializes(self, &mut probe, &mut scope)
                    .then(|| probe.spent())
            })
            .collect()
    }
}
