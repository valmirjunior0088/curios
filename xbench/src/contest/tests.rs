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

/// Curios reads its size on standard input and Rust on the command line, and the same spelling is what the agreement check runs and what is timed.
#[test]
fn each_contestant_is_given_its_size_the_way_it_reads_one() {
    let root = Path::new("/w");
    let workload = &WORKLOADS[0];

    let curios = invocation("curios", workload, 8, root);
    assert_eq!(curios.input.as_deref(), Some("8\n"));
    assert!(curios.arguments.is_empty());

    let rust = invocation("rust", workload, 8, root);
    assert_eq!(rust.input, None);
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

#[test]
fn a_recorded_reading_is_the_module_that_files_it() {
    let platform = Captured {
        machine: "A Processor, x86_64, 16 cpus, 31.3 GiB".to_string(),
        state: "governor performance, boost on, smt on".to_string(),
        software: "kernel 7.2.7, microcode 0xa201213".to_string(),
    };
    let module = recorded(
        7,
        &Sitting {
            taken: "2026-10-07".to_string(),
            subject: "curios 0.15.6 (51232992ce88)".to_string(),
            platform,
            pinned: vec![("rustc", "1.95.0".to_string())],
            sizes: vec![("lcg", 100_000_000)],
            built: vec![("lcg", "curios", 12_000_000)],
            timed: vec![("lcg", "curios", vec![vec![1.5, 2.5], vec![3.0]])],
        },
    );

    assert!(module.contains("READING_07: Reading"));
    // `date -I` zero-pads the day, and `07` is a zero-prefixed decimal literal that does not compile.
    assert!(module.contains("Date::new(2026, 10, 7)"));
    assert!(module.contains("subject: \"curios 0.15.6 (51232992ce88)\""));
    assert!(module.contains("machine: \"A Processor, x86_64, 16 cpus, 31.3 GiB\""));
    assert!(module.contains("(\"rustc\", \"1.95.0\")"));
    assert!(module.contains("(\"lcg\", 100000000)"));
    assert!(module.contains("weighed(\"lcg\", \"curios\", 12000000)"));
    assert!(module.contains("took(\"lcg\", \"curios\", &["));
    assert!(module.contains("&[1.500, 2.500]"));
    assert!(module.contains("&[3.000]"));
}
