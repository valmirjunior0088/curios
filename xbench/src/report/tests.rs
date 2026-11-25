use {super::*, crate::*, toml::value::Date, xbench::Platform};

const MACHINE: &str = "A Processor, x86_64, 16 cpus, 31.3 GiB";
const STATE: &str = "governor performance, boost on, smt on";
const SOFTWARE: &str = "kernel 7.2.7, microcode 0xa201213";
const PINNED: &[(&str, &str)] = &[("rustc", "1.95.0"), ("wasmtime", "47.0.3")];
const SIZES: &[(&str, u64)] = &[("lcg", 100_000_000)];

/// What Rust does when nothing disturbs it: the same two batches in every record below.
const STEADY: &[&[f64]] = &[&[218.0, 218.4], &[219.0, 219.2]];

/// The machine every record below was taken on, in `state`, running `software`.
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

/// A contestant's figure on `workload`, its samples given in milliseconds as a report states them and held in microseconds as a record holds them, with the workload its record sizes it by.
fn took(workload: &'static str, contestant: &str, groups: &[&[f64]]) -> (&'static str, Figure) {
    let figure = Figure {
        name: named(workload, contestant),
        measure: Measure::Microseconds,
        size: None,
        readings: groups
            .iter()
            .map(|group| {
                group
                    .iter()
                    .map(|&took| (took * 1000.0).round() as u64)
                    .collect()
            })
            .collect(),
    };

    (workload, figure)
}

/// A contestant's instructions on `workload`, given in millions as a report states them, with the workload its record sizes it by.
fn retired(workload: &'static str, contestant: &str, groups: &[&[u64]]) -> (&'static str, Figure) {
    let figure = Figure {
        name: named(workload, contestant),
        measure: Measure::Instructions,
        size: None,
        readings: groups
            .iter()
            .map(|group| group.iter().map(|&millions| millions * 1_000_000).collect())
            .collect(),
    };

    (workload, figure)
}

/// [`was`] with each contestant's instructions beside its time.
fn was_counted() -> Vec<(&'static str, Figure)> {
    let mut counted = was();
    counted.push(retired("lcg", "curios", &[&[900, 900], &[900, 900]]));
    counted.push(retired("lcg", "rust", &[&[500, 500], &[500, 500]]));
    counted
}

/// [`under`] with each contestant's instructions beside its time, Curios's fewer.
fn under_counted() -> Vec<(&'static str, Figure)> {
    let mut counted = under();
    counted.push(retired("lcg", "curios", &[&[800, 800], &[800, 800]]));
    counted.push(retired("lcg", "rust", &[&[500, 500], &[500, 500]]));
    counted
}

fn was() -> Vec<(&'static str, Figure)> {
    vec![
        took("lcg", "curios", &[&[438.0, 438.4], &[439.0, 439.2]]),
        took("lcg", "rust", STEADY),
    ]
}

fn within() -> Vec<(&'static str, Figure)> {
    vec![
        took("lcg", "curios", &[&[438.2, 438.6], &[438.8, 439.0]]),
        took("lcg", "rust", STEADY),
    ]
}

fn under() -> Vec<(&'static str, Figure)> {
    vec![
        took("lcg", "curios", &[&[401.0, 401.4], &[402.0, 402.2]]),
        took("lcg", "rust", STEADY),
    ]
}

/// [`was`] a tenth slower for both contestants alike, as a machine that changed between two sittings makes them.
fn shifted() -> Vec<(&'static str, Figure)> {
    vec![
        took("lcg", "curios", &[&[481.8, 482.24], &[482.9, 483.12]]),
        took("lcg", "rust", &[&[239.8, 240.24], &[240.9, 241.12]]),
    ]
}

fn record(day: u8, platform: Platform, took: Vec<(&'static str, Figure)>) -> Record {
    sized(day, platform, SIZES, took)
}

/// A record sizing what `sizes` says, more than the one workload the rest of these use where it says so: each figure at its workload's size.
fn sized(
    day: u8,
    platform: Platform,
    sizes: &[(&str, u64)],
    took: Vec<(&'static str, Figure)>,
) -> Record {
    Record {
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
        figures: took
            .into_iter()
            .map(|(workload, figure)| Figure {
                size: sizes
                    .iter()
                    .find_map(|&(sized, size)| (sized == workload).then_some(size)),
                ..figure
            })
            .collect(),
    }
}

fn before() -> Record {
    record(1, here(), was())
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
    let was = Spanned {
        middle: 10.0,
        least: 9.0,
        most: 11.0,
    };
    let overlapping = Spanned {
        middle: 10.5,
        least: 10.0,
        most: 12.0,
    };
    let below = Spanned {
        middle: 5.0,
        least: 4.0,
        most: 6.0,
    };
    let above = Spanned {
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
        Measure::Microseconds,
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
fn with_no_record_the_report_says_so_and_names_what_takes_one() {
    let rendered = report(&[]);

    assert!(rendered.contains("No record filed"));
    assert!(rendered.contains("cargo xbench measure contest"));
}

#[test]
fn a_record_is_printed_with_what_it_was_taken_under() {
    let rendered = one(0, &before());

    assert!(rendered.contains("# Record 00"));
    assert!(rendered.contains(MACHINE));
    assert!(rendered.contains(STATE));
    assert!(rendered.contains("rustc 1.95.0, wasmtime 47.0.3"));
}

#[test]
fn overlapping_spans_are_reported_as_no_difference_proven() {
    let rendered = compare(1, &before(), &record(2, here(), within()));

    assert!(rendered.contains("no difference proven"), "{rendered}");
}

#[test]
fn disjoint_spans_are_reported_as_a_movement() {
    let rendered = compare(1, &before(), &record(3, here(), under()));

    assert!(rendered.contains("moved, -8.4%"), "{rendered}");
}

/// Curios's own time moved past its span, and so did Rust's, alike: the ratio a comparison reads cancels what moved the machine.
#[test]
fn a_shift_that_reaches_both_contestants_alike_proves_no_difference() {
    let rendered = compare(1, &before(), &record(4, here(), shifted()));

    assert!(rendered.contains("no difference proven"), "{rendered}");
    assert!(!rendered.contains("moved,"), "{rendered}");
}

#[test]
fn a_different_arrangement_of_the_machine_refuses_the_comparison() {
    let elsewhere = platform("governor schedutil, boost on, smt on", SOFTWARE);
    let rendered = compare(1, &before(), &record(5, elsewhere, under()));

    assert!(rendered.contains("Not comparable"), "{rendered}");
    assert!(rendered.contains("schedutil"), "{rendered}");
    assert!(!rendered.contains("moved,"), "{rendered}");
}

/// A workload resized, or one entering the record later, must not refuse the records' other rows — the old harness based a late workload at its own first capture, and the gate has to allow that.
#[test]
fn a_workload_resized_says_so_on_its_own_row_and_leaves_the_rest_comparable() {
    /// `trees` holds still across both records, so only its size can speak on its row.
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
    let rendered = compare(1, &before(), &record(6, patched, under()));

    assert!(!rendered.contains("Not comparable"), "{rendered}");
    assert!(rendered.contains("moved,"), "{rendered}");
}

/// A machine's arrangement moves a time and no count: two records of one machine in two states refuse their times and compare their instructions.
#[test]
fn a_different_arrangement_refuses_the_times_and_compares_the_instructions() {
    let elsewhere = platform("governor schedutil, boost on, smt on", SOFTWARE);
    let rendered = compare(
        1,
        &record(1, here(), was_counted()),
        &record(7, elsewhere, under_counted()),
    );
    let (times, counts) = rendered
        .split_once("### Instructions")
        .unwrap_or_else(|| panic!("both records counted instructions: {rendered}"));

    assert!(times.contains("Not comparable"), "{rendered}");
    assert!(times.contains("schedutil"), "{rendered}");
    assert!(!counts.contains("Not comparable"), "{rendered}");
    assert!(counts.contains("moved, -11.1"), "{rendered}");
}

/// Another machine moves both: a time is the machine's, and the instructions a build retires are its processor's choice.
#[test]
fn another_machine_refuses_the_times_and_the_instructions() {
    let other = Platform {
        machine: "Another Processor, x86_64, 8 cpus, 15.6 GiB".to_string(),
        ..here()
    };
    let rendered = compare(
        1,
        &record(1, here(), was_counted()),
        &record(8, other, under_counted()),
    );
    let (times, counts) = rendered
        .split_once("### Instructions")
        .unwrap_or_else(|| panic!("both records counted instructions: {rendered}"));

    assert!(times.contains("Not comparable"), "{rendered}");
    assert!(counts.contains("Not comparable"), "{rendered}");
    assert!(counts.contains("Another Processor"), "{rendered}");
}

/// A figure of one measure, readings given as they are held.
fn counted(name: &str, measure: Measure, groups: &[&[u64]]) -> Figure {
    Figure {
        name: name.to_string(),
        measure,
        size: None,
        readings: groups.iter().map(|group| group.to_vec()).collect(),
    }
}

/// A measurement's figures are compared one by one: a figure whose measure is gated by a line two records differ in is refused on its own row, naming the line, and every other figure is compared, its movement proven or not.
#[test]
fn a_measurement_s_figure_is_refused_only_across_what_its_unit_is_gated_by() {
    let figures = |time: &[&[u64]], looks: &[&[u64]]| {
        vec![
            ("", counted("cps_optimize", Measure::Microseconds, time)),
            ("", counted("cps_optimize/calls", Measure::Count, looks)),
        ]
    };
    let elsewhere = platform("governor schedutil, boost on, smt on", SOFTWARE);
    let rendered = figures_compare(
        1,
        &record(1, here(), figures(&[&[100], &[101]], &[&[11], &[11]])),
        &record(2, elsewhere, figures(&[&[90], &[91]], &[&[12], &[12]])),
    );
    let row = |name: &str| {
        rendered
            .lines()
            .find(|line| line.contains(&format!("`{name}`")))
            .unwrap_or_else(|| panic!("{name} has a row: {rendered}"))
            .to_string()
    };

    assert!(
        row("cps_optimize").contains("not compared: the state differs"),
        "{rendered}"
    );
    assert!(row("cps_optimize/calls").contains("moved,"), "{rendered}");
}

/// An engine's collections are its pin's: a figure of them is refused across two records whose pins differ, where the compiler's own count beside it is compared.
#[test]
fn an_engine_s_collections_are_refused_across_a_pin_the_compiler_s_counts_are_not() {
    let figures = |collections: &[&[u64]]| {
        vec![
            (
                "",
                counted(
                    "stock at 400/collections",
                    Measure::Collections,
                    collections,
                ),
            ),
            (
                "",
                counted("stock at 400/looks", Measure::Count, &[&[11], &[11]]),
            ),
        ]
    };
    let mut bumped = record(2, here(), figures(&[&[9], &[9]]));
    bumped.pinned[1].version = "48.0.0".to_string();
    let rendered = figures_compare(1, &record(1, here(), figures(&[&[7], &[7]])), &bumped);
    let row = |name: &str| {
        rendered
            .lines()
            .find(|line| line.contains(&format!("`{name}`")))
            .unwrap_or_else(|| panic!("{name} has a row: {rendered}"))
            .to_string()
    };

    assert!(
        row("stock at 400/collections").contains("not compared: the pinned differs"),
        "{rendered}"
    );
    assert!(
        !row("stock at 400/looks").contains("not compared"),
        "{rendered}"
    );
}

/// A count of whole things — calls, bytes, allocations — is stated as a whole number even where it repeats exactly, which leaves no span to set a resolution by.
#[test]
fn a_whole_count_is_stated_without_a_fraction() {
    let rendered = figures_one(
        0,
        &record(
            1,
            here(),
            vec![(
                "",
                counted("cps_optimize/calls", Measure::Count, &[&[11], &[11]]),
            )],
        ),
    );
    let row = rendered
        .lines()
        .find(|line| line.contains("`cps_optimize/calls`"))
        .unwrap_or_else(|| panic!("the figure has a row: {rendered}"));

    assert!(row.contains("| 11 "), "{row}");
    assert!(row.contains("11–11"), "{row}");
    assert!(!row.contains('.'), "{row}");
}
