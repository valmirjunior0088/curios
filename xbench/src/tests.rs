//! What every filed record must be: filed as its sitting files it, and holding enough batches to have a span; and a record of the contest, its figures named for what the contest measures and sized.
//!
//! The bench fails on none of this when a figure moves — a record is a historical fact and cannot be re-run, so there is nothing for it to disagree with. What these hold is that the record is well formed.

use super::*;

/// Two is the fewest that yields a span rather than a single difference, and the report's whole test is whether two spans are disjoint.
const LEAST_GROUPS: usize = 2;

/// Every record of the contest filed, each read as the bench reads it.
fn filed() -> Vec<Record> {
    records(CONTEST).unwrap_or_else(|why| panic!("{why}"))
}

/// A record is filed by the sitting that took it, never written by hand: each file is what `measure` files of the record it holds.
#[test]
fn every_record_is_filed_as_its_sitting_files_it() {
    for (measurement, texts) in FILED {
        let filed = records(measurement).unwrap_or_else(|why| panic!("{why}"));
        for (number, (text, record)) in texts.iter().zip(filed).enumerate() {
            assert_eq!(
                record.filed().as_deref(),
                Ok(*text),
                "record {number:02} of {measurement} is not as `measure` files it"
            );
        }
    }
}

#[test]
fn every_figure_is_of_a_workload_and_a_contestant_the_contest_names() {
    for (number, record) in filed().iter().enumerate() {
        for figure in &record.figures {
            assert!(
                WORKLOADS.iter().any(|workload| CONTESTANTS
                    .iter()
                    .any(|contestant| figure.name == named(workload.name, contestant.name))),
                "record {number:02} holds {}, which is no contestant on a workload the contest names",
                figure.name
            );
        }
    }
}

/// A figure whose size the record does not state cannot be compared with anything, since the gate has nothing to hold the two records to.
#[test]
fn every_figure_of_the_contest_states_its_size() {
    for (number, record) in filed().iter().enumerate() {
        for figure in &record.figures {
            assert!(
                figure.size.is_some(),
                "record {number:02} times {} without saying what size it ran at",
                figure.name
            );
        }
    }
}

#[test]
fn every_figure_holds_enough_batches_to_have_a_span() {
    for (measurement, _) in FILED {
        for (number, record) in records(measurement)
            .unwrap_or_else(|why| panic!("{why}"))
            .iter()
            .enumerate()
        {
            for figure in &record.figures {
                assert!(
                    figure.readings.len() >= LEAST_GROUPS,
                    "record {number:02} of {measurement} reads {} in {} batches",
                    figure.name,
                    figure.readings.len()
                );
                assert!(
                    figure.readings.iter().all(|group| !group.is_empty()),
                    "record {number:02} of {measurement} reads {} with an empty batch",
                    figure.name
                );
            }
        }
    }
}

#[test]
fn every_record_says_what_it_is_of_and_where_it_was_taken() {
    for (number, record) in filed().iter().enumerate() {
        assert!(
            !record.subject.is_empty(),
            "record {number:02} names no subject"
        );
        assert!(
            !record.platform.machine.is_empty(),
            "record {number:02} names no machine"
        );
        assert!(
            !record.pinned.is_empty(),
            "record {number:02} records no pin"
        );
    }
}
