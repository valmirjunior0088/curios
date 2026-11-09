//! What a reading is: one sitting of the bench, recorded with everything needed to repeat it and nothing derived from it, as the TOML file under `xbench/readings/` that `collect` printed.
//!
//! No median, span, ratio or percentage is stored. The report computes each from [`Timed::milliseconds`] when it is asked, so no figure can disagree with the samples it is a figure of.
//!
//! A reading is a historical fact and cannot be re-run, which is what separates this crate from `xboard`, whose witnesses are put to the checkers on every run and fail where they disagree with their ticket. Nothing here can regress; what a run holds is that the record is well formed.

use {
    crate::Platform,
    serde::{Deserialize, Deserializer, Serialize, Serializer, de::Error as _},
    toml::value::{Date, Datetime},
};

/// One sitting of the bench.
#[derive(Debug, Serialize, Deserialize)]
pub struct Reading {
    /// The day it was taken, a TOML local date: TOML refuses a day its month does not have.
    #[serde(serialize_with = "day", deserialize_with = "dated")]
    pub taken: Date,
    /// The compiler the reading is about: the version it reports of itself and the commit it was built from.
    pub subject: String,
    pub platform: Platform,
    /// The tool pins it ran under, true because a check refused to proceed otherwise. Recorded because readings outlive a pin, and nothing else would say which era one belongs to.
    pub pinned: Vec<Pin>,
    /// The size each workload was measured at, once for the whole reading. Recorded rather than read from the contest, so changing a workload's size later cannot relabel what an earlier reading measured.
    pub sizes: Vec<Size>,
    pub timed: Vec<Timed>,
}

/// One tool and the version a reading ran it at.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Pin {
    pub tool: String,
    pub version: String,
}

/// One workload and the size a reading measured it at.
#[derive(Debug, Serialize, Deserialize)]
pub struct Size {
    pub workload: String,
    pub size: u64,
}

/// One contestant's wall clock on one workload, as it was measured.
///
/// Each group is an independent batch, and within a batch the contestants ran interleaved, so a disturbance during a group reaches all three alike and the ratio between them survives it. Within a group is iteration noise; between groups is drift.
#[derive(Debug, Serialize, Deserialize)]
pub struct Timed {
    pub workload: String,
    pub contestant: String,
    /// Whole-process wall clock in whole microseconds, one group per batch, each group's executions in the order they ran: an integer, so the record is exact.
    pub microseconds: Vec<Vec<u64>>,
}

impl Timed {
    /// The groups in milliseconds, the unit every figure is stated in.
    pub fn milliseconds(&self) -> Vec<Vec<f64>> {
        self.microseconds
            .iter()
            .map(|group| group.iter().map(|&took| took as f64 / 1000.0).collect())
            .collect()
    }
}

impl Reading {
    /// The reading as `collect` prints it and its file holds it.
    pub fn filed(&self) -> Result<String, String> {
        toml::to_string(self).map_err(|error| error.to_string())
    }

    /// A reading as its file holds it.
    pub fn read(text: &str) -> Result<Reading, String> {
        toml::from_str(text).map_err(|error| error.to_string())
    }

    /// The size `workload` was measured at, where this reading measured it.
    pub fn size(&self, workload: &str) -> Option<u64> {
        self.sizes
            .iter()
            .find_map(|sized| (sized.workload == workload).then_some(sized.size))
    }

    /// What `contestant` took on `workload`, where this reading timed it.
    pub fn took(&self, workload: &str, contestant: &str) -> Option<&Timed> {
        self.timed
            .iter()
            .find(|timed| timed.workload == workload && timed.contestant == contestant)
    }
}

/// `taken` as TOML writes a day: a local date, with no time and no offset.
fn day<S: Serializer>(date: &Date, serializer: S) -> Result<S::Ok, S::Error> {
    Datetime {
        date: Some(*date),
        time: None,
        offset: None,
    }
    .serialize(serializer)
}

/// `taken` read back: a local date and nothing else, since a reading says only on which day it was taken.
fn dated<'de, D: Deserializer<'de>>(deserializer: D) -> Result<Date, D::Error> {
    match Datetime::deserialize(deserializer)? {
        Datetime {
            date: Some(date),
            time: None,
            offset: None,
        } => Ok(date),
        other => Err(D::Error::custom(format!(
            "a reading is taken on a day, written as a local date, not `{other}`"
        ))),
    }
}
