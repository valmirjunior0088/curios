//! Every reading taken, oldest first: a reading's number is its index here.
//!
//! The bench begins with none, as the board began with no ticket. A reading is filed from what a sitting printed, never written by hand, and the ones a vanished harness took are not carried in: none was taken under a protocol this bench records, so none could be compared with a reading taken now.

#[cfg(test)]
mod tests;

mod reading_00;
use reading_00::*;

use xbench::Reading;

pub(super) const READINGS: &[&Reading] = &[&READING_00];
