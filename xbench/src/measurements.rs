//! Every measurement the bench takes but the contest, a module by subject, each named once in [`MEASUREMENTS`]: what `cargo xbench measure <MEASUREMENT>` sits in a release build of the bench, a process a group, as many groups as its [`Spread`] asks, and `xbench sit <MEASUREMENT>` runs once.
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

mod reopen;
use reopen::*;

mod runaways;
use runaways::*;

mod allowance;
use allowance::*;

mod coordination;
use coordination::*;

mod corpus;
use corpus::*;

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

mod standard;
use standard::*;

mod prelude_trees;
use prelude_trees::*;

mod prelude_image;
use prelude_image::*;

mod machine;
use machine::*;

/// A measurement the bench takes by its name.
pub struct Measurement {
    /// The name a sitting is asked for by, and its records are filed under.
    pub name: &'static str,
    /// Whether any figure it reads moves from run to run, which decides how many groups a sitting of it takes.
    pub spread: Spread,
    /// The measurement, which prints its readings through the harness.
    pub take: fn(),
}

/// Whether a measurement's figures move from run to run, as its table entry states and every sitting of it holds.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Spread {
    /// Some figure is a time, which moves between runs on one machine.
    Moves,
    /// Every figure is a count the compiler or a pinned build decides, which repeats from run to run.
    Repeats,
}

impl Spread {
    /// The groups a sitting takes, each a process: where a figure moves, five, the fewest whose ranks separate two records entirely at about p = 0.008, as the decision reads a comparison; where every figure repeats, two, the fewest whose readings show it repeating.
    pub fn groups(self) -> usize {
        match self {
            Self::Moves => 5,
            Self::Repeats => 2,
        }
    }
}

/// Every measurement the bench takes but the contest.
const MEASUREMENTS: &[Measurement] = &[
    Measurement {
        name: "launcher_guard_positive_control",
        spread: Spread::Repeats,
        take: launcher_guard_positive_control,
    },
    Measurement {
        name: "chain_collection_decomposition",
        spread: Spread::Moves,
        take: chain_collection_decomposition,
    },
    Measurement {
        name: "spines_collection_decomposition",
        spread: Spread::Moves,
        take: spines_collection_decomposition,
    },
    Measurement {
        name: "spines_key_width_measurements",
        spread: Spread::Moves,
        take: spines_key_width_measurements,
    },
    Measurement {
        name: "rope_alternation_collections",
        spread: Spread::Moves,
        take: rope_alternation_collections,
    },
    Measurement {
        name: "spines_insert_slope",
        spread: Spread::Moves,
        take: spines_insert_slope,
    },
    Measurement {
        name: "map_insert_key_share",
        spread: Spread::Moves,
        take: map_insert_key_share,
    },
    Measurement {
        name: "field_shape_census",
        spread: Spread::Repeats,
        take: field_shape_census,
    },
    Measurement {
        name: "boxed_field_read_measurements",
        spread: Spread::Moves,
        take: boxed_field_read_measurements,
    },
    Measurement {
        name: "closure_index_dispatch_measurements",
        spread: Spread::Repeats,
        take: closure_index_dispatch_measurements,
    },
    Measurement {
        name: "split_return_measurements",
        spread: Spread::Repeats,
        take: split_return_measurements,
    },
    Measurement {
        name: "copy_growth_measurements",
        spread: Spread::Repeats,
        take: copy_growth_measurements,
    },
    Measurement {
        name: "string_walk_ladder_measurements",
        spread: Spread::Repeats,
        take: string_walk_ladder_measurements,
    },
    Measurement {
        name: "walk_mirror_attribution_measurements",
        spread: Spread::Moves,
        take: walk_mirror_attribution_measurements,
    },
    Measurement {
        name: "decoder_doubling_slope",
        spread: Spread::Moves,
        take: decoder_doubling_slope,
    },
    Measurement {
        name: "aggregate_flow_census",
        spread: Spread::Repeats,
        take: aggregate_flow_census,
    },
    Measurement {
        name: "death_birth_census",
        spread: Spread::Repeats,
        take: death_birth_census,
    },
    Measurement {
        name: "fixpoint_pass_measurements",
        spread: Spread::Moves,
        take: fixpoint_pass_measurements,
    },
    Measurement {
        name: "tower_measurements",
        spread: Spread::Moves,
        take: tower_measurements,
    },
    Measurement {
        name: "runaway_measurements",
        spread: Spread::Repeats,
        take: runaway_measurements,
    },
    Measurement {
        name: "classification_allowance_measurements",
        spread: Spread::Repeats,
        take: classification_allowance_measurements,
    },
    Measurement {
        name: "coordination_measurements",
        spread: Spread::Moves,
        take: coordination_measurements,
    },
    Measurement {
        name: "host_boundary_measurements",
        spread: Spread::Moves,
        take: host_boundary_measurements,
    },
    Measurement {
        name: "growth_slopes",
        spread: Spread::Moves,
        take: growth_slopes,
    },
    Measurement {
        name: "type_level_sequence_cost_measurements",
        spread: Spread::Moves,
        take: type_level_sequence_cost_measurements,
    },
    Measurement {
        name: "certifier_memo_charge_measurements",
        spread: Spread::Repeats,
        take: certifier_memo_charge_measurements,
    },
    Measurement {
        name: "str_literal_cost_measurements",
        spread: Spread::Repeats,
        take: str_literal_cost_measurements,
    },
    Measurement {
        name: "combinator_web_cost_measurements",
        spread: Spread::Repeats,
        take: combinator_web_cost_measurements,
    },
    Measurement {
        name: "parse_primitive_measurements",
        spread: Spread::Repeats,
        take: parse_primitive_measurements,
    },
    Measurement {
        name: "type_level_claim_measurements",
        spread: Spread::Repeats,
        take: type_level_claim_measurements,
    },
    Measurement {
        name: "combinator_sharing_measurements",
        spread: Spread::Moves,
        take: combinator_sharing_measurements,
    },
    Measurement {
        name: "scrutinee_refinement_measurements",
        spread: Spread::Moves,
        take: scrutinee_refinement_measurements,
    },
    Measurement {
        name: "nested_operation_measurements",
        spread: Spread::Moves,
        take: nested_operation_measurements,
    },
    Measurement {
        name: "std_recompile_census",
        spread: Spread::Moves,
        take: std_recompile_census,
    },
    Measurement {
        name: "std_image_division",
        spread: Spread::Moves,
        take: std_image_division,
    },
    Measurement {
        name: "std_replay_census",
        spread: Spread::Repeats,
        take: std_replay_census,
    },
    Measurement {
        name: "family_slot_probe",
        spread: Spread::Repeats,
        take: family_slot_probe,
    },
    Measurement {
        name: "printed_tree_measurements",
        spread: Spread::Repeats,
        take: printed_tree_measurements,
    },
    Measurement {
        name: "stored_prelude_measurements",
        spread: Spread::Moves,
        take: stored_prelude_measurements,
    },
    Measurement {
        name: "spend_probe",
        spread: Spread::Repeats,
        take: spend_probe,
    },
    Measurement {
        name: "corpus_measurements",
        spread: Spread::Moves,
        take: corpus_measurements,
    },
    Measurement {
        name: "reopen_measurements",
        spread: Spread::Moves,
        take: reopen_measurements,
    },
    Measurement {
        name: "keystroke_measurements",
        spread: Spread::Moves,
        take: keystroke_measurements,
    },
];

/// The measurement named `name`, refused where none is.
pub fn measurement_named(name: &str) -> Result<&'static Measurement, String> {
    MEASUREMENTS
        .iter()
        .find(|measurement| measurement.name == name)
        .ok_or_else(|| format!("no measurement of the bench is named {name}"))
}
