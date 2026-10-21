//! What a run reads off the plans before it starts anything, and what the runner makes of a task that must leave a file as it found it.

use {
    super::*,
    crate::{Action, Subject, Tree},
    std::{
        env,
        path::PathBuf,
        process,
        time::{Duration, SystemTime},
    },
};

/// The steps a run of `asked` performs under `request`, in the order it performs them.
fn scheduled(asked: &[Step], request: &Request) -> Vec<Step> {
    schedule(asked, request, &Checkout::default())
        .unwrap()
        .into_iter()
        .map(|planned| planned.step)
        .collect()
}

#[test]
fn every_product_is_left_by_one_step_that_builds_or_installs_it() {
    let producers = producers(&Checkout::default()).unwrap();

    assert_eq!(
        producers.keys().copied().collect::<Vec<_>>(),
        [
            Product::Launcher,
            Product::Compiler,
            Product::Bundle,
            Product::Parser,
            Product::Installed(Tree::Grammar),
            Product::Installed(Tree::Vscode),
        ]
    );
    for step in producers.values() {
        assert!(
            matches!(step.row().action, Action::Build | Action::Install),
            "{}",
            spelled(*step)
        );
    }
}

/// The gate walks the rows as they stand: the table orders every step that leaves a product before each step that reads it.
#[test]
fn the_table_orders_every_producer_before_what_reads_it() {
    assert_eq!(
        scheduled(Step::value_variants(), &Request::whole()),
        Step::value_variants()
    );
}

/// The native host's lint builds no `curios`, so it reads no launcher, and runs without one being built.
#[test]
fn a_lint_of_the_native_host_builds_no_launcher() {
    let request = Request::narrowed(Some("curios-native".to_string()), Vec::new(), Vec::new());

    assert_eq!(
        scheduled(&Step::of(Subject::Workspace, Action::Lint), &request),
        [Step::WorkspaceLint, Step::WorkspaceLintWeb]
    );
}

#[test]
fn a_lint_of_the_workspace_builds_the_launcher_first() {
    assert_eq!(
        scheduled(
            &Step::of(Subject::Workspace, Action::Lint),
            &Request::whole()
        ),
        [
            Step::LauncherBuild,
            Step::WorkspaceLint,
            Step::WorkspaceLintWeb
        ]
    );
}

#[test]
fn an_npm_script_runs_after_its_tree_s_install() {
    assert_eq!(
        scheduled(&[Step::VscodeTest], &Request::whole()),
        [Step::VscodeInstall, Step::VscodeTest]
    );
}

/// `zed test` hashes the parser `grammar build` generates, which the grammar's own tools generate.
#[test]
fn the_extension_s_test_runs_after_the_grammar_it_hashes() {
    assert_eq!(
        scheduled(&[Step::ZedTest], &Request::whole()),
        [Step::GrammarInstall, Step::GrammarBuild, Step::ZedTest]
    );
}

/// A bare subject is its steps in the table's order, behind the steps that leave what they read and nothing else.
#[test]
fn a_bare_subject_brings_no_step_of_another_but_what_it_reads() {
    assert_eq!(
        scheduled(&Step::about(Subject::Compiler), &Request::whole()),
        [Step::LauncherBuild, Step::CompilerBuild, Step::CompilerTest]
    );
}

/// A step behind one that did not pass is not run, nor is one behind a step not run: what each reads is not there.
#[test]
fn a_step_behind_one_that_did_not_pass_is_not_run() {
    let schedule = schedule(&[Step::ZedTest], &Request::whole(), &Checkout::default()).unwrap();
    let zed = schedule
        .iter()
        .find(|planned| planned.step == Step::ZedTest)
        .unwrap();

    assert_eq!(
        blocked(zed, &[Step::GrammarBuild], &[]),
        Some(Step::GrammarBuild)
    );
    assert_eq!(
        blocked(zed, &[], &[Step::GrammarBuild]),
        Some(Step::GrammarBuild)
    );
    assert_eq!(blocked(zed, &[Step::BundleBuild], &[]), None);
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
