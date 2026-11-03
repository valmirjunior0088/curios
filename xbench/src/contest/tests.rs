use super::*;

#[test]
fn every_contestant_agreeing_on_the_expected_answer_passes() {
    let answers = vec![
        ("curios", "9345".to_string()),
        ("rust", "9345".to_string()),
        ("rust-wasm", "9345".to_string()),
    ];

    assert!(agreement(&answers, Some("9345")).is_ok());
}

#[test]
fn one_contestant_disagreeing_names_every_answer() {
    let answers = vec![("curios", "9344".to_string()), ("rust", "9345".to_string())];
    let reported = agreement(&answers, Some("9345")).unwrap_err();

    assert!(reported.contains("curios"));
    assert!(reported.contains("9344"));
    assert!(reported.contains("rust"));
}

/// Every contestant agreeing on the wrong answer is still wrong, which is the whole reason the corpus records an anchor.
#[test]
fn every_contestant_agreeing_on_the_wrong_answer_fails() {
    let answers = vec![("curios", "9344".to_string()), ("rust", "9344".to_string())];

    assert!(agreement(&answers, Some("9345")).is_err());
}

/// Where a size has no recorded answer the contestants are held to each other, which still catches a mistranslation in one of them.
#[test]
fn with_nothing_expected_the_contestants_are_held_to_each_other() {
    let agreeing = vec![("curios", "7".to_string()), ("rust", "7".to_string())];
    let differing = vec![("curios", "7".to_string()), ("rust", "8".to_string())];

    assert!(agreement(&agreeing, None).is_ok());
    assert!(agreement(&differing, None).is_err());
}

/// Every contestant reads its size on the command line, and the same spelling is what the agreement check runs and what is timed.
#[test]
fn each_contestant_is_given_its_size_as_its_argument() {
    let root = Path::new("/w");
    let workload = &WORKLOADS[0];

    let curios = invocation("curios", workload, 8, root);
    assert_eq!(curios.arguments, ["8"]);

    let rust = invocation("rust", workload, 8, root);
    assert_eq!(rust.arguments, ["8"]);

    let wasm = invocation("rust-wasm", workload, 8, root);
    assert_eq!(wasm.program.display().to_string(), "wasmtime");
    assert_eq!(wasm.arguments[0], "run");
    assert_eq!(wasm.arguments[2], "8");
}

#[test]
fn every_contestant_runs_a_build_the_bench_makes() {
    let root = Path::new("/w");

    for contestant in CONTESTANTS {
        let invocation = invocation(contestant.name, &WORKLOADS[0], 8, root);
        let named = format!(
            "{} {}",
            invocation.program.display(),
            invocation.arguments.join(" ")
        );

        assert!(
            named.contains(ARTIFACTS),
            "{} runs nothing the bench built: {named}",
            contestant.name
        );
    }
}

#[test]
fn a_workload_is_checked_at_a_smaller_size_than_it_is_timed_at() {
    for workload in WORKLOADS {
        assert!(
            workload.check < workload.size,
            "{} is checked at {} and timed at {}",
            workload.name,
            workload.check,
            workload.size
        );
        assert_ne!(workload.answer, workload.anchor, "{}", workload.name);
    }
}

/// `date -I` answers a zero-padded day, and a reading records it as TOML's local date, which refuses a day its month does not have.
#[test]
fn the_day_date_answers_is_the_day_a_reading_records() {
    assert_eq!(
        today("2026-10-07").map(|day| day.to_string()),
        Ok("2026-10-07".to_string())
    );
    assert!(today("2026-02-30").is_err());
    assert!(today("yesterday").is_err());
}
