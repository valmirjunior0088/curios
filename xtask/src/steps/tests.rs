//! What the table holds of itself, and what the command line and the check workflow hold of the table.

use {
    super::*,
    crate::{Asked, Cli, closed},
    clap::{CommandFactory, Parser},
    std::collections::BTreeSet,
};

/// The check workflow, whose jobs are the machines the steps run on.
const WORKFLOW: &str = include_str!("../../../.github/workflows/check.yml");

/// The pairs the command line can spell: each subject's subcommand with each action under it.
fn spellable() -> BTreeSet<(String, String)> {
    Cli::command()
        .get_subcommands()
        .flat_map(|subject| {
            subject.get_subcommands().map(|action| {
                (
                    subject.get_name().to_string(),
                    action.get_name().to_string(),
                )
            })
        })
        .collect()
}

/// The pairs the table's rows name.
fn tabled() -> BTreeSet<(String, String)> {
    Step::value_variants()
        .iter()
        .map(|step| {
            let row = step.row();

            (spelled(row.subject), spelled(row.action))
        })
        .collect()
}

#[test]
fn every_action_has_a_step_and_every_step_an_action() {
    assert_eq!(spellable(), tabled());
}

#[test]
fn every_subject_is_a_subcommand() {
    let subcommands = Cli::command()
        .get_subcommands()
        .map(|subcommand| subcommand.get_name().to_string())
        .collect::<BTreeSet<_>>();

    for subject in Subject::value_variants() {
        assert!(
            subcommands.contains(&spelled(*subject)),
            "{}",
            spelled(*subject)
        );
    }
}

/// What a step is run with first stands before it in the table, so the table's order is one a run can follow.
#[test]
fn a_step_is_run_with_first_what_the_table_holds_before_it() {
    for (place, step) in Step::value_variants().iter().enumerate() {
        let row = step.row();
        for before in row.ensures {
            let earlier = Step::value_variants()
                .iter()
                .position(|other| other == before)
                .expect("a step of the table");
            assert!(
                earlier < place,
                "{} before {}",
                spelled(*before),
                spelled(*step)
            );
        }
    }
}

/// What a step is run with first is something built, never a check: asking for one action never runs another subject's tests.
#[test]
fn a_step_ensures_only_builds() {
    for step in Step::value_variants() {
        for ensured in step.row().ensures {
            assert_eq!(ensured.row().action, Action::Build, "{}", spelled(*step));
        }
    }
}

/// Every line of the check workflow that runs this program is a command the parser reads, and together, with what each is run with first, they run every step: a step added to the table and to no job fails here, and so does a job left spelling a step that is gone.
#[test]
fn every_step_has_a_job() {
    let mut run = Vec::new();

    let lines = WORKFLOW
        .lines()
        .map(str::trim)
        .filter(|line| !line.starts_with('#') && line.contains("cargo xtask"));

    for line in lines {
        let words = line
            .strip_prefix("run: cargo xtask ")
            .unwrap_or_else(|| panic!("not one command on a line of its own: {line}"));
        let asked = Cli::try_parse_from(["cargo xtask"].into_iter().chain(words.split(' ')))
            .unwrap_or_else(|error| panic!("{line}\n{error}"))
            .asked;
        let Asked::About(about) = asked else {
            panic!("no step of the table: {line}");
        };

        run.extend(closed(&about.steps().0));
    }

    for step in Step::value_variants() {
        assert!(run.contains(step), "{}", spelled(*step));
    }
}

#[test]
fn a_bare_subject_is_its_steps_in_the_table_s_order() {
    assert_eq!(
        Step::about(Subject::Vscode),
        [
            Step::VscodeInstall,
            Step::VscodeFormat,
            Step::VscodeLint,
            Step::VscodeTest,
            Step::VscodeBuild
        ]
    );
    assert_eq!(
        Step::about(Subject::Instrumented),
        [Step::InstrumentedLint, Step::InstrumentedTest]
    );
}
