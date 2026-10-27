//! What the suites read of the universe solver and no judgment does: how many constraints it holds.

use super::UniverseSolver;

impl UniverseSolver {
    /// How many constraints the store holds — none between declarations, which is what a refused item's boundary must leave.
    pub fn constraint_count(&self) -> usize {
        self.constraints.len()
    }
}
