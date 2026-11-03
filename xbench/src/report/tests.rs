use {super::*, crate::*, toml::value::Date};

const MACHINE: &str = "A Processor, x86_64, 16 cpus, 31.3 GiB";
const STATE: &str = "governor performance, boost on, smt on";
const SOFTWARE: &str = "kernel 7.2.7, microcode 0xa201213";
const PINNED: &[(&str, &str)] = &[("rustc", "1.95.0"), ("wasmtime", "47.0.3")];
const SIZES: &[(&str, u64)] = &[("lcg", 100_000_000)];

/// What Rust does when nothing disturbs it: the same two batches in every reading below.
const STEADY: &[&[f64]] = &[&[218.0, 218.4], &[219.0, 219.2]];

/// The machine every reading below was taken on, in `state`, running `software`.
fn platform(state: &str, software: &str) -> Platform {
    Platform {
        machine: MACHINE.to_string(),
        state: state.to_string(),
        software: software.to_string(),
    }
}

fn here() -> Platform {
    platform(STATE, SOFTWARE)
}

/// Groups as owned values, the shape a figure is computed over.
fn grouped(groups: &[&[f64]]) -> Vec<Vec<f64>> {
    groups.iter().map(|group| group.to_vec()).collect()
}

/// A contestant's samples, given in milliseconds as a report states them and held in microseconds as a reading records them.
fn took(workload: &str, contestant: &str, groups: &[&[f64]]) -> Timed {
    Timed {
        workload: workload.to_string(),
        contestant: contestant.to_string(),
        microseconds: groups
            .iter()
            .map(|group| {
                group
                    .iter()
                    .map(|&took| (took * 1000.0).round() as u64)
                    .collect()
            })
            .collect(),
    }
}

fn was() -> Vec<Timed> {
    vec![
        took("lcg", "curios", &[&[438.0, 438.4], &[439.0, 439.2]]),
        took("lcg", "rust", STEADY),
    ]
}

fn within() -> Vec<Timed> {
    vec![
        took("lcg", "curios", &[&[438.2, 438.6], &[438.8, 439.0]]),
        took("lcg", "rust", STEADY),
    ]
}

fn under() -> Vec<Timed> {
    vec![
        took("lcg", "curios", &[&[401.0, 401.4], &[402.0, 402.2]]),
        took("lcg", "rust", STEADY),
    ]
}

/// [`was`] a tenth slower for both contestants alike, as a machine that changed between two sittings makes them.
fn shifted() -> Vec<Timed> {
    vec![
        took("lcg", "curios", &[&[481.8, 482.24], &[482.9, 483.12]]),
        took("lcg", "rust", &[&[239.8, 240.24], &[240.9, 241.12]]),
    ]
}

fn reading(day: u8, platform: Platform, timed: Vec<Timed>) -> Reading {
    sized(day, platform, SIZES, timed)
}

/// A reading sizing what `sizes` says, more than the one workload the rest of these use where it says so.
fn sized(day: u8, platform: Platform, sizes: &[(&str, u64)], timed: Vec<Timed>) -> Reading {
    Reading {
        taken: Date {
            year: 2026,
            month: 10,
            day,
        },
        subject: "curios 0.15.6 (abcdef)".to_string(),
        platform,
        pinned: PINNED
            .iter()
            .map(|&(tool, version)| Pin {
                tool: tool.to_string(),
                version: version.to_string(),
            })
            .collect(),
        sizes: sizes
            .iter()
            .map(|&(workload, size)| Size {
                workload: workload.to_string(),
                size,
            })
            .collect(),
        timed,
    }
}

fn before() -> Reading {
    reading(1, here(), was())
}

#[test]
fn the_middle_of_an_odd_and_an_even_count_of_samples() {
    assert_eq!(median(&[3.0, 1.0, 2.0]), Some(2.0));
    assert_eq!(median(&[4.0, 1.0, 3.0, 2.0]), Some(2.5));
    assert_eq!(median(&[]), None);
}

/// A contestant is read by the median of its batch medians, and the span is the least and greatest of those — not of every sample, since within a batch is noise and between them is drift.
#[test]
fn a_figure_spans_the_batch_medians_rather_than_the_samples() {
    let figure = figure(&grouped(&[&[10.0, 12.0], &[20.0, 22.0], &[30.0, 32.0]])).unwrap();

    assert_eq!(figure.middle, 21.0);
    assert_eq!(figure.least, 11.0);
    assert_eq!(figure.most, 31.0);
}

#[test]
fn a_figure_over_no_samples_is_no_figure() {
    assert_eq!(figure(&[]), None);
}

/// Stating a figure past its span claims a precision the measurement does not have.
#[test]
fn a_figure_is_stated_to_the_place_its_span_supports() {
    assert_eq!(stated(438.24, 3.0), "438");
    assert_eq!(stated(438.24, 0.4), "438.2");
    assert_eq!(stated(438.24, 0.04), "438.24");
    assert_eq!(stated(438.24, 30.0), "438");
}

#[test]
fn a_span_of_nothing_is_stated_at_full_resolution() {
    assert_eq!(stated(438.25, 0.0), "438.250");
}

#[test]
fn two_spans_that_could_have_produced_each_other_prove_nothing() {
    let was = Figure {
        middle: 10.0,
        least: 9.0,
        most: 11.0,
    };
    let overlapping = Figure {
        middle: 10.5,
        least: 10.0,
        most: 12.0,
    };
    let below = Figure {
        middle: 5.0,
        least: 4.0,
        most: 6.0,
    };
    let above = Figure {
        middle: 20.0,
        least: 19.0,
        most: 21.0,
    };

    assert!(!moved(&was, &overlapping));
    assert!(moved(&was, &below));
    assert!(moved(&was, &above));
}

#[test]
fn a_table_puts_the_fastest_first_and_reads_the_rest_against_rust() {
    let rendered = table(
        "LCG",
        "N",
        100_000_000,
        &[
            ("Curios", grouped(&[&[438.0, 438.4], &[439.0, 439.2]])),
            ("Rust (Native)", grouped(STEADY)),
        ],
    );
    let rows = rendered
        .lines()
        .filter(|line| line.starts_with('|') && !line.contains("---"))
        .collect::<Vec<_>>();

    assert!(rendered.contains("`LCG` (N = 100000000)"));
    assert!(rows[1].contains("Rust (Native)"), "{rendered}");
    assert!(rows[2].contains("Curios"), "{rendered}");
    assert!(rows[2].contains("2.0"), "{rendered}");
}

#[test]
fn with_no_reading_the_report_says_so_and_names_what_takes_one() {
    let rendered = report(&[]);

    assert!(rendered.contains("No reading recorded"));
    assert!(rendered.contains("cargo xbench collect"));
}

#[test]
fn a_reading_is_printed_with_what_it_was_taken_under() {
    let rendered = one(0, &before());

    assert!(rendered.contains("# Reading 00"));
    assert!(rendered.contains(MACHINE));
    assert!(rendered.contains(STATE));
    assert!(rendered.contains("rustc 1.95.0, wasmtime 47.0.3"));
}

#[test]
fn overlapping_spans_are_reported_as_no_difference_proven() {
    let rendered = compare(1, &before(), &reading(2, here(), within()));

    assert!(rendered.contains("no difference proven"), "{rendered}");
}

#[test]
fn disjoint_spans_are_reported_as_a_movement() {
    let rendered = compare(1, &before(), &reading(3, here(), under()));

    assert!(rendered.contains("moved, -8.4%"), "{rendered}");
}

/// Curios's own time moved past its span, and so did Rust's, alike: the ratio a comparison reads cancels what moved the machine.
#[test]
fn a_shift_that_reaches_both_contestants_alike_proves_no_difference() {
    let rendered = compare(1, &before(), &reading(4, here(), shifted()));

    assert!(rendered.contains("no difference proven"), "{rendered}");
    assert!(!rendered.contains("moved,"), "{rendered}");
}

#[test]
fn a_different_arrangement_of_the_machine_refuses_the_comparison() {
    let elsewhere = platform("governor schedutil, boost on, smt on", SOFTWARE);
    let rendered = compare(1, &before(), &reading(5, elsewhere, under()));

    assert!(rendered.contains("Not comparable"), "{rendered}");
    assert!(rendered.contains("schedutil"), "{rendered}");
    assert!(!rendered.contains("moved,"), "{rendered}");
}

/// A workload resized, or one entering the record later, must not refuse the readings' other rows — the old harness based a late workload at its own first capture, and the gate has to allow that.
#[test]
fn a_workload_resized_says_so_on_its_own_row_and_leaves_the_rest_comparable() {
    /// `trees` holds still across both readings, so only its size can speak on its row.
    const TREES: &[&[f64]] = &[&[250.0, 250.4], &[251.0, 251.2]];
    let trees = || {
        [
            took("trees", "curios", TREES),
            took("trees", "rust", STEADY),
        ]
    };
    let was_both = was().into_iter().chain(trees()).collect();
    let is_both = under().into_iter().chain(trees()).collect();
    let before_two = sized(1, here(), &[("lcg", 100_000_000), ("trees", 21)], was_both);
    let after_two = sized(2, here(), &[("lcg", 100_000_000), ("trees", 23)], is_both);
    let rendered = compare(1, &before_two, &after_two);

    assert!(!rendered.contains("Not comparable"), "{rendered}");
    assert!(
        rendered.contains("sized 21, then 23; not compared"),
        "{rendered}"
    );
    assert!(rendered.contains("moved,"), "{rendered}");
}

/// Software is recorded and not gated: a movement it caused moves Rust with Curios, and the ratio a comparison reads cancels it.
#[test]
fn a_software_difference_is_compared_anyway() {
    let patched = platform(STATE, "kernel 7.2.8, microcode 0xa201213");
    let rendered = compare(1, &before(), &reading(6, patched, under()));

    assert!(!rendered.contains("Not comparable"), "{rendered}");
    assert!(rendered.contains("moved,"), "{rendered}");
}
