//! What a run takes on before it starts anything, and what the runner makes of a task that must leave a file as it found it.

use {
    super::*,
    crate::Subject,
    std::{
        env,
        path::PathBuf,
        process,
        time::{Duration, SystemTime},
    },
};

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

/// A step whose prerequisite failed is not run, and neither is one behind it: what each reads is not there.
#[test]
fn a_step_behind_one_that_did_not_pass_is_not_run() {
    assert_eq!(
        blocked(Step::ZedTest, &[Step::GrammarBuild], &[]),
        Some(Step::GrammarBuild)
    );
    assert_eq!(
        blocked(Step::BundleTest, &[], &[Step::BundleBuild]),
        Some(Step::BundleBuild)
    );
    assert_eq!(blocked(Step::ZedTest, &[Step::BundleBuild], &[]), None);
}

/// A bare subject is its steps in the table's order, behind what they are run with first and nothing else.
#[test]
fn a_bare_subject_brings_no_step_of_another_but_its_prerequisites() {
    assert_eq!(
        closed(&Step::about(Subject::Compiler)),
        [Step::LauncherBuild, Step::CompilerBuild, Step::CompilerTest]
    );
}

/// A file of this test's own in the system's temporary directory, last written long ago, so a write during the test moves its time whatever the file system's resolution.
fn aged(name: &str) -> PathBuf {
    let path = env::temp_dir().join(format!("xtask-{name}-{}", process::id()));
    fs::write(&path, "before").unwrap();
    fs::File::options()
        .write(true)
        .open(&path)
        .and_then(|file| file.set_modified(SystemTime::UNIX_EPOCH + Duration::from_secs(1)))
        .unwrap();

    path
}

#[test]
fn a_task_that_writes_what_it_must_keep_fails_saying_what_that_means() {
    let path = aged("rewritten");
    let kept = Task::Keeps {
        path: path.clone(),
        during: Box::new(Task::Write {
            path: path.clone(),
            text: "after",
        }),
        rewritten: "the write came back",
    };

    let outcome = act(&kept);
    fs::remove_file(&path).unwrap();

    let refusal = outcome.unwrap_err();
    assert!(refusal.starts_with("the write came back: "), "{refusal}");
}

#[test]
fn a_task_that_leaves_what_it_must_keep_passes() {
    let path = aged("left");
    let other = env::temp_dir().join(format!("xtask-other-{}", process::id()));
    let kept = Task::Keeps {
        path: path.clone(),
        during: Box::new(Task::Write {
            path: other.clone(),
            text: "elsewhere",
        }),
        rewritten: "the write came back",
    };

    let outcome = act(&kept);
    fs::remove_file(&path).unwrap();
    fs::remove_file(&other).unwrap();

    assert_eq!(outcome, Ok(()));
}
