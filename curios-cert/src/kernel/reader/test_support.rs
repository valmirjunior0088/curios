//! What a suite reading through the kernel asks and a reader does not: what a reading spent.
//!
//! Compiled for this crate's tests and behind the `test-support` feature, so no build that ships holds it.

use {super::Reader, curios_core::Consumption};

impl Reader<'_, '_> {
    /// What the declaration being read has consumed since its budget was last restored, which a measurement of what a reading spends takes.
    pub fn consumed(&self) -> Consumption {
        self.kernel.consumed()
    }
}
