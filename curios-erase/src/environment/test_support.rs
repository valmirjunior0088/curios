//! What a suite reads of an item's environment and no judgment does: the rows of the inductives it declares.
//!
//! Compiled for this crate's tests and behind the `test-support` feature, so no build that ships holds it.

use super::{Environment, FamilyRow};

impl Environment {
    /// The row of each inductive this item declares.
    pub fn declared_induct_rows(&self) -> impl Iterator<Item = &FamilyRow> {
        self.induct_decls.values()
    }
}
