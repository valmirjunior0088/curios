//! The rule every probe keeps, read off a reduction's result.

use crate::{Cost, Probe, ReduceError};

fn exhausted() -> ReduceError {
    ReduceError::exhausted(3, Cost::term(8))
}

fn refused() -> ReduceError {
    ReduceError::ListGetOutOfBounds {
        len: 2,
        index: 5,
        span: None,
    }
}

#[test]
fn a_probe_keeps_the_value_a_reduction_found() {
    assert_eq!(Ok::<_, ReduceError>(7).probed(), Ok(Some(7)));
}

#[test]
fn a_probe_reads_a_term_with_no_value_as_nothing_to_offer() {
    assert_eq!(Err::<u8, _>(refused()).probed(), Ok(None));
}

#[test]
fn a_probe_propagates_the_budget_it_ran_out_of() {
    assert_eq!(Err::<u8, _>(exhausted()).probed(), Err(exhausted()));
}
