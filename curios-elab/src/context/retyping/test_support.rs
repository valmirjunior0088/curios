//! What a suite asks of a reader of finished terms and erasure does not: a reader with nothing in scope, and what a reading spent.
//!
//! Compiled for this crate's tests and behind the `test-support` feature, so no build that ships holds it.

use {super::Retyping, crate::Context, curios_core::Consumption, curios_utilities::SyntaxRegistry};

impl Retyping<'_> {
    /// A reader with nothing in scope, for a test that reads terms naming no declaration.
    pub fn new(budget: u64, syntax: SyntaxRegistry) -> Self {
        Self {
            context: Context::new(budget, syntax),
        }
    }

    /// [`Context::consumed`], which the measurements of what a reading spends take.
    pub fn consumed(&self) -> Consumption {
        self.context.consumed()
    }
}
