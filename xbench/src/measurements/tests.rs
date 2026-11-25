use super::*;

/// Every measurement is named once, so a name takes one measurement and its records are that measurement's alone.
#[test]
fn every_measurement_is_named_once() {
    let mut names = MEASUREMENTS
        .iter()
        .map(|measurement| measurement.name)
        .collect::<Vec<_>>();
    names.sort_unstable();
    let named = names.len();
    names.dedup();

    assert_eq!(names.len(), named, "a name is given twice among {names:?}");
}

/// A name no measurement is given is refused before anything is built.
#[test]
fn a_name_no_measurement_is_given_is_refused() {
    assert_eq!(
        measurement_named("contest").err().as_deref(),
        Some("no measurement of the bench is named contest")
    );
}
