use {
    super::*,
    curios_profile::{Memory, Tallied},
    std::{cell::RefCell, time::Duration},
};

/// A span's figures are its fold's: its time to the microsecond, its calls, what its thread took, and what its work added to each tally.
#[test]
fn a_span_reads_as_the_figures_its_fold_states() {
    let summary = Summary {
        target: "curios_cps".to_string(),
        name: "cps_optimize".to_string(),
        group: None,
        calls: 11,
        total: Duration::from_micros(1_909_334),
        self_total: Duration::from_micros(40),
        min: Duration::from_micros(1),
        max: Duration::from_micros(2_000),
        memory: Some(Memory {
            retained: 0,
            allocated: 1_311_244_288,
            self_allocated: 12,
            allocations: 16_722_058,
        }),
        tallies: vec![Tallied {
            name: "looks".to_string(),
            added: 7_340_032,
            self_added: 512,
        }],
    };
    let read = span_lines("cps_optimize", &summary, Some(4))
        .into_iter()
        .map(|line| (line.name, line.measure, line.size, line.value))
        .collect::<Vec<_>>();

    assert_eq!(
        read,
        [
            (
                "cps_optimize".to_string(),
                Measure::Microseconds,
                Some(4),
                1_909_334
            ),
            (
                "cps_optimize/calls".to_string(),
                Measure::Count,
                Some(4),
                11
            ),
            (
                "cps_optimize/allocated".to_string(),
                Measure::Bytes,
                Some(4),
                1_311_244_288
            ),
            (
                "cps_optimize/allocations".to_string(),
                Measure::Allocations,
                Some(4),
                16_722_058
            ),
            (
                "cps_optimize/looks".to_string(),
                Measure::Count,
                Some(4),
                7_340_032
            ),
        ]
    );
}

/// A time reads in whole microseconds, a part of one dropped, and one past what `u64` holds as its largest.
#[test]
fn a_time_reads_as_its_whole_microseconds() {
    assert_eq!(microseconds(Duration::from_nanos(1_999)), 1);
    assert_eq!(microseconds(Duration::from_millis(1_909)), 1_909_000);
    assert_eq!(microseconds(Duration::MAX), u64::MAX);
}

/// A sample's figures are what it observed in total and how many times.
#[test]
fn a_sample_reads_as_its_total_and_its_observations() {
    let sample = SampleSummary {
        target: "curios_cps".to_string(),
        name: "cps::rounds".to_string(),
        count: 11,
        total: 40,
        min: 1,
        max: 9,
    };
    let read = sample_lines("cps::rounds", &sample, None)
        .into_iter()
        .map(|line| (line.name, line.value))
        .collect::<Vec<_>>();

    assert_eq!(
        read,
        [
            ("cps::rounds".to_string(), 40),
            ("cps::rounds/observations".to_string(), 11),
        ]
    );
}

/// Every variant runs once untimed, its readings printed by no one, and then a round at a time, each round starting one later: no variant always runs first.
#[test]
fn variants_turn_round_robin_after_an_untimed_round() {
    let ran = RefCell::new(Vec::new());
    let mut first = || ran.borrow_mut().push(("first", UNTIMED.with(Cell::get)));
    let mut second = || ran.borrow_mut().push(("second", UNTIMED.with(Cell::get)));
    let mut third = || ran.borrow_mut().push(("third", UNTIMED.with(Cell::get)));
    turn(3, &mut [&mut first, &mut second, &mut third]);

    assert_eq!(
        ran.into_inner(),
        [
            ("first", true),
            ("second", true),
            ("third", true),
            ("first", false),
            ("second", false),
            ("third", false),
            ("second", false),
            ("third", false),
            ("first", false),
            ("third", false),
            ("first", false),
            ("second", false),
        ]
    );
}

/// A phase hands back what its work answered with the fold of what it recorded, and counts what it retired where the kernel counts at all.
#[test]
fn a_phase_answers_with_its_fold_and_its_instructions() {
    let ran = phase(&[], || {
        (0..10_000_u64).map(std::hint::black_box).sum::<u64>()
    });

    assert_eq!(ran.answer, 49_995_000);
    assert!(ran.profile.open.is_empty());
    if let Some(instructions) = ran.instructions {
        assert!(instructions > 10_000, "{instructions}");
    }
}

/// A phase reads as the span it opened around its work, under the name its caller gives it, and as the instructions the work retired where the kernel counts: entered once, and what its work added to each tally the phase was handed.
#[test]
fn a_phase_reads_as_its_own_span_under_its_callers_name() {
    static COUNTED: Tally = Tally {
        name: "steps",
        read: steps,
    };
    thread_local! {
        static STEPS: Cell<u64> = const { Cell::new(0) };
    }
    fn steps() -> u64 {
        STEPS.with(Cell::get)
    }

    let ran = phase(std::slice::from_ref(&COUNTED), || {
        STEPS.with(|steps| steps.set(steps.get() + 7));
    });
    let read = ran
        .lines("work", Some(3))
        .into_iter()
        .map(|line| (line.name, line.measure, line.size))
        .collect::<Vec<_>>();

    assert_eq!(
        read[0],
        ("work".to_string(), Measure::Microseconds, Some(3))
    );
    let lines = ran.lines("work", Some(3));
    let value = |name: &str, measure: Measure| {
        lines
            .iter()
            .find(|line| line.name == name && line.measure == measure)
            .map(|line| line.value)
    };
    assert_eq!(value("work/calls", Measure::Count), Some(1));
    assert_eq!(value("work/steps", Measure::Count), Some(7));
    assert_eq!(
        value("work", Measure::Instructions).is_some(),
        ran.instructions.is_some()
    );
}
