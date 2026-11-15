//! What every filed timing must be: measuring what the contest names, sized for what it measured, and holding enough batches to have a span.
//!
//! The bench fails on none of this when a figure moves — a timing is a historical fact and cannot be re-run, so there is nothing for it to disagree with. What these hold is that the record is well formed.

use super::*;

/// Two is the fewest that yields a span rather than a single difference, and the report's whole test is whether two spans are disjoint.
const LEAST_GROUPS: usize = 2;

/// Every timing filed, each read as the bench reads it.
fn filed() -> Vec<Timing> {
    timings().unwrap_or_else(|why| panic!("{why}"))
}

/// A timing is filed from what a sitting printed, never written by hand: each file is what `collect` prints of the timing it holds.
#[test]
fn every_timing_is_filed_as_collect_prints_it() {
    for (number, (text, timing)) in FILED.iter().zip(filed()).enumerate() {
        assert_eq!(
            timing.filed().as_deref(),
            Ok(*text),
            "timing {number:02} is not as `collect` prints it"
        );
    }
}

#[test]
fn every_timing_measures_what_the_contest_names() {
    for (number, timing) in filed().iter().enumerate() {
        for timed in &timing.timed {
            assert!(
                WORKLOADS
                    .iter()
                    .any(|workload| workload.name == timed.workload),
                "timing {number:02} measures an unknown workload {}",
                timed.workload
            );
            assert!(
                CONTESTANTS
                    .iter()
                    .any(|contestant| contestant.name == timed.contestant),
                "timing {number:02} measures an unknown contestant {}",
                timed.contestant
            );
        }
    }
}

/// A figure whose size the timing does not state cannot be compared with anything, since the gate has nothing to hold the two timings to.
#[test]
fn every_figure_names_a_workload_its_timing_sized() {
    for (number, timing) in filed().iter().enumerate() {
        for timed in &timing.timed {
            assert!(
                timing.size(&timed.workload).is_some(),
                "timing {number:02} times {} without saying what size it ran at",
                timed.workload
            );
        }
    }
}

#[test]
fn every_figure_holds_enough_batches_to_have_a_span() {
    for (number, timing) in filed().iter().enumerate() {
        for timed in &timing.timed {
            assert!(
                timed.microseconds.len() >= LEAST_GROUPS,
                "timing {number:02} times {} on {} in {} batches",
                timed.contestant,
                timed.workload,
                timed.microseconds.len()
            );
            assert!(
                timed.microseconds.iter().all(|group| !group.is_empty()),
                "timing {number:02} times {} on {} with an empty batch",
                timed.contestant,
                timed.workload
            );
        }
    }
}

#[test]
fn every_timing_says_what_it_is_of_and_where_it_was_taken() {
    for (number, timing) in filed().iter().enumerate() {
        assert!(
            !timing.subject.is_empty(),
            "timing {number:02} names no subject"
        );
        assert!(
            !timing.platform.machine.is_empty(),
            "timing {number:02} names no machine"
        );
        assert!(
            !timing.pinned.is_empty(),
            "timing {number:02} records no pin"
        );
    }
}
