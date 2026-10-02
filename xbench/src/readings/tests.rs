//! What every filed reading must be: measuring what the contest names, sized for what it measured, and holding enough batches to have a span.
//!
//! The bench fails on none of this when a figure moves — a reading is a historical fact and cannot be re-run, so there is nothing for it to disagree with. What these hold is that the record is well formed.

use {
    super::*,
    xbench::{CONTESTANTS, WORKLOADS},
};

/// Two is the fewest that yields a span rather than a single difference, and the report's whole test is whether two spans are disjoint.
const LEAST_GROUPS: usize = 2;

#[test]
fn every_weighed_build_is_one_the_contest_makes() {
    for (number, reading) in READINGS.iter().enumerate() {
        for built in reading.built {
            assert!(
                WORKLOADS
                    .iter()
                    .any(|workload| workload.name == built.workload)
                    && CONTESTANTS
                        .iter()
                        .any(|contestant| contestant.name == built.contestant),
                "reading {number:02} weighs {} on {}, which the contest does not build",
                built.contestant,
                built.workload
            );
        }
    }
}

#[test]
fn every_reading_measures_what_the_contest_names() {
    for (number, reading) in READINGS.iter().enumerate() {
        for timed in reading.timed {
            assert!(
                WORKLOADS
                    .iter()
                    .any(|workload| workload.name == timed.workload),
                "reading {number:02} measures an unknown workload {}",
                timed.workload
            );
            assert!(
                CONTESTANTS
                    .iter()
                    .any(|contestant| contestant.name == timed.contestant),
                "reading {number:02} measures an unknown contestant {}",
                timed.contestant
            );
        }
    }
}

/// A figure whose size the reading does not state cannot be compared with anything, since the gate has nothing to hold the two readings to.
#[test]
fn every_figure_names_a_workload_its_reading_sized() {
    for (number, reading) in READINGS.iter().enumerate() {
        for timed in reading.timed {
            assert!(
                reading.size(timed.workload).is_some(),
                "reading {number:02} times {} without saying what size it ran at",
                timed.workload
            );
        }
    }
}

#[test]
fn every_figure_holds_enough_batches_to_have_a_span() {
    for (number, reading) in READINGS.iter().enumerate() {
        for timed in reading.timed {
            assert!(
                timed.groups.len() >= LEAST_GROUPS,
                "reading {number:02} times {} on {} in {} batches",
                timed.contestant,
                timed.workload,
                timed.groups.len()
            );
            assert!(
                timed.groups.iter().all(|group| !group.is_empty()),
                "reading {number:02} times {} on {} with an empty batch",
                timed.contestant,
                timed.workload
            );
        }
    }
}

#[test]
fn every_reading_says_what_it_is_of_and_where_it_was_taken() {
    for (number, reading) in READINGS.iter().enumerate() {
        assert!(
            !reading.subject.is_empty(),
            "reading {number:02} names no subject"
        );
        assert!(
            !reading.platform.machine.is_empty(),
            "reading {number:02} names no machine"
        );
        assert!(
            !reading.pinned.is_empty(),
            "reading {number:02} records no pin"
        );
    }
}
