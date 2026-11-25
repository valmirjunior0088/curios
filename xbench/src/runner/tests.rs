use {super::*, crate::Measure};

/// A reading of one figure.
fn reading(name: &str, size: Option<u64>, value: u64) -> Line {
    Line {
        name: name.to_string(),
        measure: Measure::Count,
        size,
        value,
    }
}

/// A sitting whose groups read no figure has nothing to file: it refuses rather than filing a record with no figures.
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

/// A measurement of no figure, stated so.
fn stated(spread: Spread) -> Measurement {
    Measurement {
        name: "probe",
        spread,
        take: || {},
    }
}

/// A figure counted in `measure`, read once.
fn counted_in(measure: Measure) -> Figure {
    Figure {
        name: "a figure".to_string(),
        measure,
        size: None,
        readings: vec![vec![1]],
    }
}

/// A measurement stated to repeat may read nothing that moves from run to run, since its two groups are too few to compare a time by.
#[test]
fn a_time_read_by_a_measurement_stated_to_repeat_refuses_the_sitting() {
    assert_eq!(
        spread_held(&stated(Spread::Repeats), &[counted_in(Measure::Count), counted_in(Measure::Instructions)]),
        Err("probe is stated to repeat, and reads a figure in instructions, which moves from run to run".to_string())
    );
}

/// A measurement stated to move reads something that does, or its five groups were spent on readings that repeat.
#[test]
fn a_measurement_stated_to_move_that_reads_none_that_moves_refuses_the_sitting() {
    assert_eq!(
        spread_held(&stated(Spread::Moves), &[counted_in(Measure::Bytes)]),
        Err("probe is stated to move, and every figure it reads repeats".to_string())
    );
}

/// Figures that agree with the statement leave the sitting to be filed, either way.
#[test]
fn figures_a_spread_describes_are_filed() {
    assert_eq!(
        spread_held(&stated(Spread::Repeats), &[counted_in(Measure::Count)]),
        Ok(())
    );
    assert_eq!(
        spread_held(
            &stated(Spread::Moves),
            &[
                counted_in(Measure::Count),
                counted_in(Measure::Microseconds)
            ]
        ),
        Ok(())
    );
}
