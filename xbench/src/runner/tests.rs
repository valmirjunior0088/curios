use {
    super::*,
    xbench::{Measure, read},
};

/// What libtest lists of its ignored tests: each a path and its kind.
const LIST: &str = "tests::instrumented::fixpoint::fixpoint_pass_measurements: test\ntests::instrumented::fixpoint::fixpoint_pass_measurements_of_more: test\ntests::reduction::reduction_measurements: test\n";

/// A measurement is found by its name alone, its last segment, or by its whole path, and never by a prefix of another's.
#[test]
fn a_measurement_is_found_by_its_name_and_its_path() {
    assert_eq!(
        listed(LIST, "fixpoint_pass_measurements").as_deref(),
        Some("tests::instrumented::fixpoint::fixpoint_pass_measurements")
    );
    assert_eq!(
        listed(LIST, "tests::reduction::reduction_measurements").as_deref(),
        Some("tests::reduction::reduction_measurements")
    );
    assert_eq!(listed(LIST, "pass_measurements"), None);
    assert_eq!(listed(LIST, "growth_slopes"), None);
}

/// A reading of one figure.
fn reading(name: &str, size: Option<u64>, value: u64) -> Line {
    Line {
        name: name.to_string(),
        measure: Measure::Count,
        size,
        value,
    }
}

/// The first line a measurement prints is a reading like every other, which an invocation read behind what libtest writes before the test runs would lose.
#[test]
fn a_reading_a_test_prints_first_is_read() {
    let executable = Executable {
        program: std::env::current_exe().expect("the suite names its own executable"),
        directory: PathBuf::from(env!("CARGO_MANIFEST_DIR")),
    };

    assert_eq!(
        invoked(
            &executable,
            "runner::tests::a_test_that_prints_a_reading_first"
        ),
        Ok(vec![reading("first", None, 1)])
    );
}

/// The test [`a_reading_a_test_prints_first_is_read`] invokes, whose first output is a reading.
#[test]
#[ignore = "invoked by a_reading_a_test_prints_first_is_read"]
fn a_test_that_prints_a_reading_first() {
    read("first", Measure::Count, None, 1);
}

/// A sitting whose invocations read no figure has nothing to file, as a check taken by its name reads none: it refuses rather than filing a record with no figures.
#[test]
fn a_sitting_that_read_no_figure_files_nothing() {
    assert_eq!(
        grouped(vec![Vec::new(), Vec::new()]).err().as_deref(),
        Some("the measurement read no figure")
    );
}

/// Each invocation is a group of every figure it read, the figures in the order the first invocation read them.
#[test]
fn an_invocation_is_a_group_of_every_figure() {
    let figures = grouped(vec![
        vec![reading("looks", None, 7), reading("units", None, 3)],
        vec![reading("looks", None, 8), reading("units", None, 3)],
    ])
    .unwrap_or_else(|why| panic!("{why}"));

    let read = figures
        .iter()
        .map(|figure| (figure.name.as_str(), figure.readings.clone()))
        .collect::<Vec<_>>();
    assert_eq!(
        read,
        [
            ("looks", vec![vec![7], vec![8]]),
            ("units", vec![vec![3], vec![3]])
        ]
    );
}

/// A figure one invocation read and another did not leaves a group empty, which no span is read over: the sitting refuses rather than filing it.
#[test]
fn a_figure_missing_from_an_invocation_refuses_the_sitting() {
    let refused = grouped(vec![
        vec![reading("looks", None, 7), reading("units", None, 3)],
        vec![reading("looks", None, 8)],
    ]);

    assert!(refused.is_err_and(|why| why.contains("units")));
}

/// A figure read at two sizes is two figures under one name, which no comparison could tell apart.
#[test]
fn a_figure_read_at_two_sizes_refuses_the_sitting() {
    let refused = grouped(vec![
        vec![reading("looks", Some(12), 7)],
        vec![reading("looks", Some(16), 9)],
    ]);

    assert!(refused.is_err_and(|why| why.contains("looks")));
}
