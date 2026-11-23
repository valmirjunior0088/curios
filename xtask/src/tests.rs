//! What the repository's own task files hold of the rules `README.md` states, and what the workflows hold of them.

use {crate::*, std::collections::BTreeSet};

/// The check workflow, whose jobs are the machines the steps run on.
const WORKFLOW: &str = include_str!("../../.github/workflows/check.yml");

/// The lock file, which says what version of `wasm-bindgen` the bundle builds against.
const LOCK: &str = include_str!("../../Cargo.lock");

/// `curios-profile`'s build script, which names the Perfetto release it takes the trace schema at.
const PROFILE_BUILD: &str = include_str!("../../curios-profile/build.rs");

fn tree() -> Tree {
    Tree::read().unwrap()
}

/// Every word of every command of the steps `group` holds.
fn commands_of<'a>(tree: &'a Tree, group: &str) -> Vec<(&'a Step, Vec<String>)> {
    tree.steps_of(&[group.to_string()])
        .into_iter()
        .flat_map(|step| {
            let step = &tree.steps[step];
            step.run
                .iter()
                .filter_map(move |instruction| match instruction {
                    Instruction::Command(command) => Some(command),
                    Instruction::Write { .. } | Instruction::Check(_) => None,
                })
                .map(move |command| {
                    (
                        step,
                        command
                            .iter()
                            .map(|word| {
                                word.0
                                    .iter()
                                    .map(|piece| match piece {
                                        Piece::Text(text) => text.clone(),
                                        Piece::Value(_) => "{}".to_string(),
                                    })
                                    .collect::<String>()
                            })
                            .collect(),
                    )
                })
        })
        .collect()
}

/// Every line of the check workflow that runs this program is a command the parser reads, and together they run every step the handoff runs: a step added to a subject and to no job fails here, and so does a job left spelling a task that is gone.
#[test]
fn every_step_the_handoff_runs_has_a_job() {
    let tree = tree();
    let mut run = BTreeSet::new();

    let lines = WORKFLOW
        .lines()
        .map(str::trim)
        .filter(|line| !line.starts_with('#') && line.contains("cargo xtask"));
    for line in lines {
        let words = line
            .strip_prefix("run: cargo xtask ")
            .unwrap_or_else(|| panic!("not one command on a line of its own: {line}"));
        let matches = command(&tree)
            .try_get_matches_from(["cargo xtask"].into_iter().chain(words.split(' ')))
            .unwrap_or_else(|error| panic!("{line}\n{error}"));
        let Asked::Task { name, given } = asked(&matches) else {
            panic!("no task of the tree: {line}");
        };
        run.extend(schedule(&tree, &name, &given));
    }

    let handoff = vec!["handoff".to_string()];
    for step in schedule(&tree, &handoff, &Given::new()) {
        if tree.steps[step].name != handoff {
            assert!(run.contains(&step), "{}", spelled(&tree.steps[step].name));
        }
    }
}

/// The compiler is checked as it ships: no step of it turns a feature on.
#[test]
fn a_compiler_step_names_no_feature() {
    let tree = tree();

    for (step, words) in commands_of(&tree, "compiler") {
        assert!(
            words.iter().all(|word| !word.contains("feature")),
            "{}: {words:?}",
            dotted(&step.name)
        );
    }
}

/// A lint goes on past a crate that fails, so one run reports every crate's errors rather than the first's.
#[test]
fn a_lint_goes_on_past_a_crate_that_fails() {
    let tree = tree();

    for group in ["compiler", "native", "jig"] {
        for (step, words) in commands_of(&tree, group) {
            if words.contains(&"clippy".to_string()) {
                assert!(
                    words.contains(&"--keep-going".to_string()),
                    "{}: {words:?}",
                    dotted(&step.name)
                );
            }
        }
    }
}

/// The version the tool `name`, written from the top without its dot, is fetched at.
fn version_of(tree: &Tree, name: &str) -> String {
    match &tree.tools[&name.split('.').map(str::to_string).collect::<Name>()] {
        Tool::Fetched(fetched) => fetched.version.clone(),
        Tool::Shipped { .. } => panic!("`{name}` is fetched itself"),
    }
}

/// The generator must match the `wasm-bindgen` crate the bundle is built against exactly, so the task files fetch it at the lock's version, and a bump of the crate fails here until the tool follows.
#[test]
fn the_bindings_generator_is_fetched_at_the_lock_s_version() {
    let lock = LOCK.parse::<toml_edit::DocumentMut>().unwrap();
    let locked = lock["package"]
        .as_array_of_tables()
        .unwrap()
        .iter()
        .find(|package| package["name"].as_str() == Some("wasm-bindgen"))
        .and_then(|package| package["version"].as_str())
        .unwrap();

    assert_eq!(version_of(&tree(), "tools.wasm-bindgen"), locked);
}

/// `profile query` asks Perfetto's own reader what a stream holds, so the task files fetch it at the release whose schema `curios-profile` generates its messages from, and a bump of the schema fails here until the reader follows.
#[test]
fn the_trace_reader_is_fetched_at_the_schema_s_release() {
    let release = PROFILE_BUILD
        .lines()
        .find_map(|line| {
            line.strip_prefix("const REVISION: &str = \"v")?
                .strip_suffix("\";")
        })
        .expect("curios-profile's build script names the release it takes the schema at");

    assert_eq!(version_of(&tree(), "tools.trace_processor_shell"), release);
}
