//! What a reading is: one sitting of the bench, recorded with everything needed to repeat it and nothing derived from it.
//!
//! No median, span, ratio or percentage is stored. The report computes each from [`Timed::groups`] when it is asked, so no figure can disagree with the samples it is a figure of.
//!
//! A reading is a historical fact and cannot be re-run, which is what separates this crate from `xboard`, whose witnesses are put to the checkers on every run and fail where they disagree with their ticket. Nothing here can regress; what a run holds is that the record is well formed.

use crate::{Built, Date, Platform};

/// One sitting of the bench.
#[derive(Debug)]
pub struct Reading {
    pub taken: Date,
    /// The compiler the reading is about: the version it reports of itself and the commit it was built from. A reading is emitted only from a clean tree, so the commit holds what was measured.
    pub subject: &'static str,
    pub platform: Platform,
    /// The tool pins it ran under, true because a check refused to proceed otherwise. Recorded because readings outlive a pin, and nothing else would say which era one belongs to.
    pub pinned: &'static [(&'static str, &'static str)],
    /// The size each workload was measured at, once for the whole reading. Recorded rather than read from the contest, so changing a workload's size later cannot relabel what an earlier reading measured.
    pub sizes: &'static [(&'static str, u64)],
    /// What each contestant's build weighed, taken without running anything. Deterministic, so a difference between two readings is real.
    pub built: &'static [Built],
    pub timed: &'static [Timed],
}

/// One contestant's wall clock on one workload, as it was measured.
///
/// Each group is an independent batch, and within a batch the contestants ran interleaved, so a disturbance during a group reaches all three alike and the ratio between them survives it. Within a group is iteration noise; between the group medians is the span a difference must clear.
#[derive(Debug)]
pub struct Timed {
    pub workload: &'static str,
    pub contestant: &'static str,
    /// Whole-process wall clock in milliseconds, one group per batch, each group's executions in the order they ran.
    pub groups: &'static [&'static [f64]],
}

/// A contestant's samples, written on one line where a reading records them.
pub const fn took(
    workload: &'static str,
    contestant: &'static str,
    groups: &'static [&'static [f64]],
) -> Timed {
    Timed {
        workload,
        contestant,
        groups,
    }
}

impl Reading {
    /// The size `workload` was measured at, where this reading measured it.
    pub fn size(&self, workload: &str) -> Option<u64> {
        self.sizes
            .iter()
            .find_map(|&(sized, size)| (sized == workload).then_some(size))
    }

    /// What `contestant` took on `workload`, where this reading timed it.
    pub fn took(&self, workload: &str, contestant: &str) -> Option<&Timed> {
        self.timed
            .iter()
            .find(|timed| timed.workload == workload && timed.contestant == contestant)
    }

    /// What `contestant`'s build of `workload` weighed, where this reading weighed it.
    pub fn weight(&self, workload: &str, contestant: &str) -> Option<u64> {
        self.built
            .iter()
            .find(|built| built.workload == workload && built.contestant == contestant)
            .map(|built| built.bytes)
    }
}
