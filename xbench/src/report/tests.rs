use {
    super::*,
    crate::{Built, Date, Platform, Timed, took, weighed},
};

const MACHINE: &str = "A Processor, x86_64, 16 cpus, 31.3 GiB";
const STATE: &str = "governor performance, boost on, smt on";
const SOFTWARE: &str = "kernel 7.2.7, microcode 0xa201213";
const PINNED: &[(&str, &str)] = &[("rustc", "1.95.0"), ("wasmtime", "47.0.3")];
const SIZES: &[(&str, u64)] = &[("lcg", 100_000_000)];

/// Most of these fixtures are about the timed half, and a reading that weighed nothing prints no build table.
const NOTHING_WEIGHED: &[Built] = &[];

const HERE: Platform = Platform {
    machine: MACHINE,
    state: STATE,
    software: SOFTWARE,
};

/// What the control does when nothing disturbs it: the same two batches in every reading below.
const STEADY: &[&[f64]] = &[&[218.0, 218.4], &[219.0, 219.2]];

const WAS: &[Timed] = &[
    took("lcg", "curios", &[&[438.0, 438.4], &[439.0, 439.2]]),
    took("lcg", "rust", STEADY),
];
const WITHIN: &[Timed] = &[
    took("lcg", "curios", &[&[438.2, 438.6], &[438.8, 439.0]]),
    took("lcg", "rust", STEADY),
];
const UNDER: &[Timed] = &[
    took("lcg", "curios", &[&[401.0, 401.4], &[402.0, 402.2]]),
    took("lcg", "rust", STEADY),
];
/// The subject moved and so did the control, which is what disqualifies a comparison.
const ADRIFT: &[Timed] = &[
    took("lcg", "curios", &[&[401.0, 401.4], &[402.0, 402.2]]),
    took("lcg", "rust", &[&[260.0, 260.4], &[261.0, 261.2]]),
];

const fn reading(day: u8, platform: Platform, timed: &'static [Timed]) -> Reading {
    Reading {
        taken: Date::new(2026, 10, day),
        subject: "curios 0.15.6 (abcdef)",
        platform,
        pinned: PINNED,
        sizes: SIZES,
        built: NOTHING_WEIGHED,
        timed,
    }
}

/// A reading sizing more than the one workload the rest of these use.
const fn sized(day: u8, sizes: &'static [(&'static str, u64)], timed: &'static [Timed]) -> Reading {
    Reading {
        taken: Date::new(2026, 10, day),
        subject: "curios 0.15.6 (abcdef)",
        platform: HERE,
        pinned: PINNED,
        sizes,
        built: NOTHING_WEIGHED,
        timed,
    }
}

const BEFORE: Reading = reading(1, HERE, WAS);
const UNMOVED: Reading = reading(2, HERE, WITHIN);
const FASTER: Reading = reading(3, HERE, UNDER);

#[test]
fn the_middle_of_an_odd_and_an_even_count_of_samples() {
    assert_eq!(median(&[3.0, 1.0, 2.0]), Some(2.0));
    assert_eq!(median(&[4.0, 1.0, 3.0, 2.0]), Some(2.5));
    assert_eq!(median(&[]), None);
}

/// A contestant is read by the median of its batch medians, and the span is the least and greatest of those — not of every sample, since within a batch is noise and between them is drift.
#[test]
fn a_figure_spans_the_batch_medians_rather_than_the_samples() {
    const SPREAD: &[&[f64]] = &[&[10.0, 12.0], &[20.0, 22.0], &[30.0, 32.0]];
    let figure = figure(SPREAD).unwrap();

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
    const CURIOS: &[&[f64]] = &[&[438.0, 438.4], &[439.0, 439.2]];
    let rendered = table(
        "LCG",
        "N",
        100_000_000,
        &[("Curios", CURIOS), ("Rust", STEADY)],
    );
    let rows = rendered
        .lines()
        .filter(|line| line.starts_with('|') && !line.contains(":---"))
        .collect::<Vec<_>>();

    assert!(rendered.contains("`LCG` (N = 100000000)"));
    assert!(rows[1].contains("Rust"), "{rendered}");
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
    let rendered = one(0, &BEFORE);

    assert!(rendered.contains("# Reading 00"));
    assert!(rendered.contains(MACHINE));
    assert!(rendered.contains(STATE));
    assert!(rendered.contains("rustc 1.95.0, wasmtime 47.0.3"));
}

#[test]
fn overlapping_spans_are_reported_as_no_difference_proven() {
    let rendered = compare(1, &BEFORE, &UNMOVED);

    assert!(rendered.contains("no difference proven"), "{rendered}");
}

#[test]
fn disjoint_spans_are_reported_as_a_movement() {
    let rendered = compare(1, &BEFORE, &FASTER);

    assert!(rendered.contains("moved, -8.4%"), "{rendered}");
}

/// Where the control moved, the machine was not the same machine twice whatever its platform lines say.
#[test]
fn a_control_that_moved_disqualifies_the_comparison() {
    let rendered = compare(1, &BEFORE, &reading(4, HERE, ADRIFT));

    assert!(rendered.contains("control did not hold"), "{rendered}");
    assert!(!rendered.contains("moved,"), "{rendered}");
}

#[test]
fn a_different_arrangement_of_the_machine_refuses_the_comparison() {
    const ELSEWHERE: Platform = Platform {
        machine: MACHINE,
        state: "governor schedutil, boost on, smt on",
        software: SOFTWARE,
    };
    let rendered = compare(1, &BEFORE, &reading(5, ELSEWHERE, UNDER));

    assert!(rendered.contains("Not comparable"), "{rendered}");
    assert!(rendered.contains("schedutil"), "{rendered}");
    assert!(!rendered.contains("moved,"), "{rendered}");
}

/// A workload resized, or one entering the record later, must not refuse the readings' other rows — the old harness based a late workload at its own first capture, and the gate has to allow that.
#[test]
fn a_workload_resized_says_so_on_its_own_row_and_leaves_the_rest_comparable() {
    /// `trees` holds still across both readings, so only its size can speak on its row.
    const TREES: &[&[f64]] = &[&[250.0, 250.4], &[251.0, 251.2]];
    const WAS_BOTH: &[Timed] = &[
        took("lcg", "curios", &[&[438.0, 438.4], &[439.0, 439.2]]),
        took("lcg", "rust", STEADY),
        took("trees", "curios", TREES),
        took("trees", "rust", STEADY),
    ];
    const IS_BOTH: &[Timed] = &[
        took("lcg", "curios", &[&[401.0, 401.4], &[402.0, 402.2]]),
        took("lcg", "rust", STEADY),
        took("trees", "curios", TREES),
        took("trees", "rust", STEADY),
    ];
    const BEFORE_TWO: Reading = sized(1, &[("lcg", 100_000_000), ("trees", 21)], WAS_BOTH);
    const AFTER_TWO: Reading = sized(2, &[("lcg", 100_000_000), ("trees", 23)], IS_BOTH);
    let rendered = compare(1, &BEFORE_TWO, &AFTER_TWO);

    assert!(!rendered.contains("Not comparable"), "{rendered}");
    assert!(
        rendered.contains("sized 21, then 23; not compared"),
        "{rendered}"
    );
    assert!(rendered.contains("moved,"), "{rendered}");
}

/// A build's weight is deterministic under the pins, so a difference is real and is stated as itself — no span, no control.
#[test]
fn a_build_that_changed_weight_is_reported_exactly() {
    const WAS_WEIGHED: &[Built] = &[
        weighed("lcg", "curios", 12_000_000),
        weighed("lcg", "rust", 310_000),
    ];
    const IS_WEIGHED: &[Built] = &[
        weighed("lcg", "curios", 12_000_512),
        weighed("lcg", "rust", 310_000),
    ];
    const BEFORE_WEIGHED: Reading = Reading {
        taken: Date::new(2026, 10, 7),
        subject: "curios 0.15.6 (abcdef)",
        platform: HERE,
        pinned: PINNED,
        sizes: SIZES,
        built: WAS_WEIGHED,
        timed: WAS,
    };
    const AFTER_WEIGHED: Reading = Reading {
        taken: Date::new(2026, 10, 8),
        subject: "curios 0.15.6 (abcdef)",
        platform: HERE,
        pinned: PINNED,
        sizes: SIZES,
        built: IS_WEIGHED,
        timed: UNDER,
    };
    let rendered = compare(1, &BEFORE_WEIGHED, &AFTER_WEIGHED);

    assert!(rendered.contains("Builds that changed"), "{rendered}");
    assert!(
        rendered.contains("12000000 | 12000512 | +512"),
        "{rendered}"
    );
    // Rust's build did not move, so it is not a row.
    assert!(!rendered.contains("310000"), "{rendered}");
}

/// Two contestants' weights are never put side by side in a comparison: the Curios executable carries the engine, so the figures are read down a column.
#[test]
fn a_reading_that_weighed_nothing_prints_no_build_table() {
    assert!(!one(0, &BEFORE).contains("Builds"));
}

/// Software is recorded and not gated: a movement it caused moves the control, which is what refuses the comparison.
#[test]
fn a_software_difference_is_noted_and_compared_anyway() {
    const PATCHED: Platform = Platform {
        machine: MACHINE,
        state: STATE,
        software: "kernel 7.2.8, microcode 0xa201213",
    };
    let rendered = compare(1, &BEFORE, &reading(6, PATCHED, UNDER));

    assert!(rendered.contains("software differs"), "{rendered}");
    assert!(rendered.contains("moved,"), "{rendered}");
}
