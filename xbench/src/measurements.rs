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

mod structural;
use structural::*;

mod ladder;
use ladder::*;

mod census;
use census::*;

mod fixpoint;
use fixpoint::*;

mod listing;
use listing::*;

mod towers;
use towers::*;

mod runaways;
use runaways::*;

mod allowance;
use allowance::*;

mod coordination;
use coordination::*;

mod host_boundary;
use host_boundary::*;

mod growth;
use growth::*;

mod reduction;
use reduction::*;

mod unfolding;
use unfolding::*;

mod nested;
use nested::*;

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
    Measurement {
        name: "closure_index_dispatch_measurements",
        take: closure_index_dispatch_measurements,
    },
    Measurement {
        name: "split_return_measurements",
        take: split_return_measurements,
    },
    Measurement {
        name: "copy_growth_measurements",
        take: copy_growth_measurements,
    },
    Measurement {
        name: "string_walk_ladder_measurements",
        take: string_walk_ladder_measurements,
    },
    Measurement {
        name: "walk_mirror_attribution_measurements",
        take: walk_mirror_attribution_measurements,
    },
    Measurement {
        name: "decoder_doubling_slope",
        take: decoder_doubling_slope,
    },
    Measurement {
        name: "aggregate_flow_census",
        take: aggregate_flow_census,
    },
    Measurement {
        name: "death_birth_census",
        take: death_birth_census,
    },
    Measurement {
        name: "fixpoint_pass_measurements",
        take: fixpoint_pass_measurements,
    },
    Measurement {
        name: "tower_measurements",
        take: tower_measurements,
    },
    Measurement {
        name: "runaway_measurements",
        take: runaway_measurements,
    },
    Measurement {
        name: "classification_allowance_measurements",
        take: classification_allowance_measurements,
    },
    Measurement {
        name: "coordination_measurements",
        take: coordination_measurements,
    },
    Measurement {
        name: "host_boundary_measurements",
        take: host_boundary_measurements,
    },
    Measurement {
        name: "growth_slopes",
        take: growth_slopes,
    },
    Measurement {
        name: "type_level_sequence_cost_measurements",
        take: type_level_sequence_cost_measurements,
    },
    Measurement {
        name: "certifier_memo_charge_measurements",
        take: certifier_memo_charge_measurements,
    },
    Measurement {
        name: "str_literal_cost_measurements",
        take: str_literal_cost_measurements,
    },
    Measurement {
        name: "combinator_web_cost_measurements",
        take: combinator_web_cost_measurements,
    },
    Measurement {
        name: "parse_primitive_measurements",
        take: parse_primitive_measurements,
    },
    Measurement {
        name: "type_level_claim_measurements",
        take: type_level_claim_measurements,
    },
    Measurement {
        name: "combinator_sharing_measurements",
        take: combinator_sharing_measurements,
    },
    Measurement {
        name: "scrutinee_refinement_measurements",
        take: scrutinee_refinement_measurements,
    },
    Measurement {
        name: "nested_operation_measurements",
        take: nested_operation_measurements,
    },
];

/// The measurement named `name`, refused where none is.
pub fn measurement_named(name: &str) -> Result<&'static Measurement, String> {
    MEASUREMENTS
        .iter()
        .find(|measurement| measurement.name == name)
        .ok_or_else(|| format!("no measurement of the bench is named {name}"))
}
