//! What a record is: one sitting of one measurement at one commit, recorded with everything needed to repeat it and nothing derived from it, as the TOML file under `xbench/records/<measurement>/` its sitting filed.
//!
//! No median, span, ratio or percentage is stored. The report computes each from a figure's readings when it is asked, so no figure can disagree with the readings it is a figure of.
//!
//! A record is a historical fact and cannot be re-run, which is what separates this crate from `xboard`, whose witnesses are put to the checkers on every run and fail where they disagree with their ticket. Nothing here can regress; what a run holds is that the record is well formed.

use {
    crate::Platform,
    serde::{Deserialize, Deserializer, Serialize, Serializer, de::Error},
    toml::value::{Date, Datetime},
};

/// One sitting of one measurement, which its file's directory names.
#[derive(Debug, Serialize, Deserialize)]
pub struct Record {
    /// The day it was taken, a TOML local date: TOML refuses a day its month does not have.
    #[serde(serialize_with = "day", deserialize_with = "dated")]
    pub taken: Date,
    /// The compiler the record is about: the version it reports of itself and the commit it was built from.
    pub subject: String,
    pub platform: Platform,
    /// The tool pins it ran under. Recorded because records outlive a pin, and nothing else would say which era one belongs to.
    pub pinned: Vec<Pin>,
    pub figures: Vec<Figure>,
}

/// One tool and the version a record ran it at.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Pin {
    pub tool: String,
    pub version: String,
}

/// One figure of a sitting: its name within its measurement, what it is counted in, the size it was taken at, and every reading, as taken.
///
/// Each group is an independent batch, and within a batch the measurement's variants ran interleaved, so a disturbance during a group reaches them alike. Within a group is iteration noise; between groups is drift.
#[derive(Debug, Serialize, Deserialize)]
pub struct Figure {
    pub name: String,
    pub unit: Unit,
    /// The size the figure was taken at, where its measurement takes it at one. Recorded rather than read from the measurement, so changing a size later cannot relabel what an earlier record measured.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub size: Option<u64>,
    /// Every reading, one group per batch, each group's readings in the order they were taken: integers, so the record is exact.
    pub readings: Vec<Vec<u64>>,
}

/// What a figure is counted in, which decides how far two records of it compare: the platform lines a comparison of it is refused across ([`Unit::gated`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(into = "&'static str", try_from = "String")]
pub enum Unit {
    /// Wall clock in whole microseconds: compared between two records of one machine, in one state, under one pin.
    Microseconds,
    /// User-space instructions a process and the threads and processes it starts retire: compared between two records of one machine, in any state, under one pin, since the machine's arrangement moves no count and its processor's features, and the pins' builds, choose the code that retires them.
    Instructions,
    /// Bytes a thread took from the allocator: compared between two records under one pin, on any machine, since the standard library a pin builds decides what a collection takes.
    Bytes,
    /// Trips a thread made through the allocator: compared as [`Unit::Bytes`] are.
    Allocations,
    /// A count the compiler keeps of its own work — calls, looks, budget units, a magnitude it samples: compared between any two records.
    Count,
}

/// Which platform lines a comparison of a unit is refused across, where two records differ in them.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Gated {
    pub machine: bool,
    pub state: bool,
    pub pins: bool,
}

impl Unit {
    /// Every unit, in the order its name is matched.
    const ALL: [Self; 5] = [
        Self::Microseconds,
        Self::Instructions,
        Self::Bytes,
        Self::Allocations,
        Self::Count,
    ];

    /// The one spelling of the unit's name, which a record's file and a measurement's line both write.
    pub fn name(self) -> &'static str {
        match self {
            Self::Microseconds => "microseconds",
            Self::Instructions => "instructions",
            Self::Bytes => "bytes",
            Self::Allocations => "allocations",
            Self::Count => "count",
        }
    }

    /// The unit named `name`, where one is.
    pub fn named(name: &str) -> Option<Self> {
        Self::ALL.into_iter().find(|unit| unit.name() == name)
    }

    /// The platform lines a comparison of a figure counted in this unit is refused across.
    pub fn gated(self) -> Gated {
        match self {
            Self::Microseconds => Gated {
                machine: true,
                state: true,
                pins: true,
            },
            Self::Instructions => Gated {
                machine: true,
                state: false,
                pins: true,
            },
            Self::Bytes | Self::Allocations => Gated {
                machine: false,
                state: false,
                pins: true,
            },
            Self::Count => Gated {
                machine: false,
                state: false,
                pins: false,
            },
        }
    }

    /// What a reading is divided by to be stated in [`Unit::stated`].
    fn scale(self) -> f64 {
        match self {
            Self::Microseconds => 1e3,
            Self::Instructions => 1e6,
            Self::Bytes | Self::Allocations | Self::Count => 1.0,
        }
    }

    /// The unit a figure counted in this one is stated in: milliseconds for a time, millions for instructions, and every other count as it is.
    pub fn stated(self) -> &'static str {
        match self {
            Self::Microseconds => "ms",
            Self::Instructions => "M",
            Self::Bytes => "bytes",
            Self::Allocations => "allocations",
            Self::Count => "",
        }
    }
}

impl From<Unit> for &'static str {
    fn from(unit: Unit) -> Self {
        unit.name()
    }
}

impl TryFrom<String> for Unit {
    type Error = String;

    fn try_from(name: String) -> Result<Self, String> {
        Unit::named(&name).ok_or_else(|| format!("no unit is named `{name}`"))
    }
}

impl Figure {
    /// The groups in the unit its unit is stated in ([`Unit::stated`]).
    pub fn stated(&self) -> Vec<Vec<f64>> {
        let scale = self.unit.scale();
        self.readings
            .iter()
            .map(|group| group.iter().map(|&read| read as f64 / scale).collect())
            .collect()
    }
}

impl Record {
    /// The record as its sitting files it.
    pub fn filed(&self) -> Result<String, String> {
        toml::to_string(self).map_err(|error| error.to_string())
    }

    /// A record as its file holds it.
    pub fn read(text: &str) -> Result<Record, String> {
        toml::from_str(text).map_err(|error| error.to_string())
    }

    /// The figure named `name` counted in `unit`, where this record holds it: one name may be counted in several units, a contestant's time and its instructions.
    pub fn figure(&self, name: &str, unit: Unit) -> Option<&Figure> {
        self.figures
            .iter()
            .find(|figure| figure.name == name && figure.unit == unit)
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

/// `taken` read back: a local date and nothing else, since a record says only on which day it was taken.
fn dated<'de, D: Deserializer<'de>>(deserializer: D) -> Result<Date, D::Error> {
    match Datetime::deserialize(deserializer)? {
        Datetime {
            date: Some(date),
            time: None,
            offset: None,
        } => Ok(date),
        other => Err(D::Error::custom(format!(
            "a record is taken on a day, written as a local date, not `{other}`"
        ))),
    }
}
