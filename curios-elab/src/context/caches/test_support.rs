//! What a suite asks of the caches and no judgment does: caches that clear every table at every boundary.
//!
//! Compiled for this crate's tests and behind the `test-support` feature, so no build that ships holds it.

use super::Caches;

impl Caches {
    /// Caches under which every boundary clears every table: the protocol an answer kept for as long as what it read stands is held to, by comparing what a library elaborates to under each (`curios-pipeline`'s `std_memo_census`). An answer kept past a boundary that moves an elaborated term rests on something its life does not state.
    pub fn wholesale() -> Self {
        Self {
            wholesale: true,
            ..Self::default()
        }
    }
}
