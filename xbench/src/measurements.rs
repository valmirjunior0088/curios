//! Every measurement the bench takes but the contest, a module by subject, each named once in [`MEASUREMENTS`]: what `cargo xbench measure <MEASUREMENT>` sits in a release build of the bench, a process a group, and `xbench sit <MEASUREMENT>` runs once.
//!
//! A measurement drives the compiler through its public entries and the test support a crate keeps for another's tests, as `xboard` drives the checkers, and names each figure through the harness, which prints its readings as the lines of its record.

#[cfg(test)]
mod tests;

mod launcher;
use launcher::*;

mod runs;
use runs::*;

mod churn;
use churn::*;

mod alternation;
use alternation::*;

mod map_insert;
use map_insert::*;

mod shapes;
use shapes::*;

/// A measurement the bench takes by its name.
pub struct Measurement {
    /// The name a sitting is asked for by, and its records are filed under.
    pub name: &'static str,
    /// The measurement, which prints its readings through the harness.
    pub take: fn(),
}

/// Every measurement the bench takes but the contest.
const MEASUREMENTS: &[Measurement] = &[
    Measurement {
        name: "launcher_guard_positive_control",
        take: launcher_guard_positive_control,
    },
    Measurement {
        name: "chain_collection_decomposition",
        take: chain_collection_decomposition,
    },
    Measurement {
        name: "spines_collection_decomposition",
        take: spines_collection_decomposition,
    },
    Measurement {
        name: "spines_key_width_measurements",
        take: spines_key_width_measurements,
    },
    Measurement {
        name: "rope_alternation_collections",
        take: rope_alternation_collections,
    },
    Measurement {
        name: "spines_insert_slope",
        take: spines_insert_slope,
    },
    Measurement {
        name: "map_insert_key_share",
        take: map_insert_key_share,
    },
    Measurement {
        name: "field_shape_census",
        take: field_shape_census,
    },
    Measurement {
        name: "boxed_field_read_measurements",
        take: boxed_field_read_measurements,
    },
];

/// The measurement named `name`, refused where none is.
pub fn measurement_named(name: &str) -> Result<&'static Measurement, String> {
    MEASUREMENTS
        .iter()
        .find(|measurement| measurement.name == name)
        .ok_or_else(|| format!("no measurement of the bench is named {name}"))
}
