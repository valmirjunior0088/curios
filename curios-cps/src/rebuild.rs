//! The rebuild in place: each construction of a row the module proves may write into a value of that row dying beside it becomes a [`ValueExpr::Rebuilt`] of that value, writing the slots it changes.

use super::{Atom, Module, Node, ValueExpr, rebuilds};

/// Rewrite every construction of a row `rebuilds` admits as a rebuild of the value it admits, after the optimizer and before the representation is decided, so no pass that copies, moves or merges a computation meets one.
pub fn rebuild_in_place(module: &mut Module) {
    curios_profile::profile!("rebuild_in_place");
    for rebuild in rebuilds(module) {
        if let Some(Node::LetValue { value, .. }) = module.node_mut(rebuild.node) {
            *value = ValueExpr::Rebuilt {
                of: Atom::Value(rebuild.of),
                row: rebuild.row,
                slots: rebuild.slots,
            };
        }
    }
}
