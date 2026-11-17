//! A reading over a certifier for the suites that drive a walk behind its doors directly: this crate's, and a shared analysis's driven over the certifier's own reading, as `curios-analysis`'s driven suites drive one.
//!
//! Compiled for this crate's tests and behind the `test-support` feature, through which those suites reach it, so no build that ships holds it.

use super::*;

impl<'s> Certifier<'s> {
    /// Run `read` in a reading of this certifier, its terms handed as the suite wrote them.
    pub fn reading<T>(&mut self, read: impl FnOnce(&mut ByValue<'_, 's>) -> T) -> T {
        read(&mut ByValue {
            certifier: self,
            plain: false,
        })
    }
}
