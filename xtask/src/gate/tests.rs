//! What a run holds before it starts anything: which steps it takes on, and what each waits for.

use {super::*, crate::Subject};

#[test]
fn a_step_brings_what_it_is_run_with_first() {
    assert_eq!(
        closed(&[Step::WorkspaceTest]),
        [Step::LauncherBuild, Step::WorkspaceTest]
    );
    assert_eq!(
        closed(&[Step::BundleTest]),
        [Step::BundleBuild, Step::BundleTest]
    );
}

/// What a step waits for is an order among the steps running, never a request: the Zed extension's test follows the grammar's where both run, and asking for the extension alone runs nothing of the grammar.
#[test]
fn a_step_brings_nothing_it_only_waits_for() {
    let zed = closed(&Step::about(Subject::Zed));

    assert!(zed.contains(&Step::ZedTest));
    assert!(!zed.contains(&Step::GrammarTest));
}

#[test]
fn a_step_waits_for_one_that_has_not_ended_and_is_held_by_one_that_failed() {
    let running = [Step::BundleBuild, Step::BundleTest];
    let mut outcomes = BTreeMap::new();

    assert!(matches!(
        standing(Step::BundleBuild, &running, &outcomes),
        Standing::Ready
    ));
    assert!(matches!(
        standing(Step::BundleTest, &running, &outcomes),
        Standing::Waiting
    ));

    outcomes.insert(
        place(Step::BundleBuild),
        Outcome::Failed(Duration::default()),
    );
    assert!(matches!(
        standing(Step::BundleTest, &running, &outcomes),
        Standing::Held(Step::BundleBuild)
    ));

    outcomes.insert(
        place(Step::BundleBuild),
        Outcome::Passed(Duration::default()),
    );
    assert!(matches!(
        standing(Step::BundleTest, &running, &outcomes),
        Standing::Ready
    ));
}

/// A step a path condition spared clears what waits for it: nothing it would have written is missing.
#[test]
fn a_spared_step_clears_what_waits_for_it() {
    assert!(Outcome::Skipped(String::new()).clears());
    assert!(!Outcome::Skipped(String::new()).fails());
    assert!(Outcome::Held(Step::LauncherBuild).fails());
}
