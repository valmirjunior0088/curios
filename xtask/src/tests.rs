//! What the repository's own task files hold of the rules `README.md` states, and what the workflows hold of them.

use {crate::*, std::collections::BTreeSet};

/// The check workflow, whose jobs are the machines the steps run on.
const CHECK_WORKFLOW: &str = include_str!("../../.github/workflows/check.yml");

/// The release workflow, whose jobs build what a release publishes and publish it.
const RELEASE_WORKFLOW: &str = include_str!("../../.github/workflows/release.yml");

/// The lock file, which says what version of `wasm-bindgen` the bundle builds against.
const LOCK: &str = include_str!("../../Cargo.lock");

/// `curios-profile`'s build script, which names the Perfetto release it takes the trace schema at.
const PROFILE_BUILD: &str = include_str!("../../curios-profile/build.rs");
const UNICODE_BUILD: &str = include_str!("../../curios-unicode/build.rs");
const BINARYEN_BUILD: &str = include_str!("../../curios-binaryen/build.rs");

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
                    Instruction::Command { words: command, .. } => Some(command),
                    Instruction::Write { .. } => None,
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

/// The words after `cargo xtask` of every line of `workflow` that runs this program, each a command on a line of its own: a step's `run:`, or a line of a `run: |` block.
fn tasks_of(workflow: &str) -> Vec<&str> {
    workflow
        .lines()
        .map(str::trim)
        .filter(|line| !line.starts_with('#') && line.contains("cargo xtask"))
        .map(|line| {
            line.strip_prefix("run: ")
                .unwrap_or(line)
                .strip_prefix("cargo xtask ")
                .unwrap_or_else(|| panic!("not one command on a line of its own: {line}"))
        })
        .collect()
}

/// The steps the task `words` names, as the parser reads it from the command line.
fn scheduled(tree: &Tree, words: &str) -> Vec<usize> {
    let matches = command(tree)
        .try_get_matches_from(["cargo xtask"].into_iter().chain(words.split(' ')))
        .unwrap_or_else(|error| panic!("cargo xtask {words}\n{error}"));
    let Asked { name, given } = asked(&matches);

    schedule(tree, &name, &given)
}

/// Every line of the check workflow that runs this program is a command the parser reads, and together they run every step the handoff runs: a step added to a subject and to no job fails here, and so does a job left spelling a task that is gone.
#[test]
fn every_step_the_handoff_runs_has_a_job() {
    let tree = tree();
    let mut run = BTreeSet::new();

    for words in tasks_of(CHECK_WORKFLOW) {
        run.extend(scheduled(&tree, words));
    }

    let handoff = vec!["handoff".to_string()];
    for step in schedule(&tree, &handoff, &GivenValues::new()) {
        if tree.steps[step].name != handoff {
            assert!(run.contains(&step), "{}", spelled(&tree.steps[step].name));
        }
    }
}

/// Every line of the release workflow that runs this program is a command the parser reads, so a step renamed or taken out fails here rather than at the release that runs it.
#[test]
fn every_task_the_release_runs_is_one_the_parser_reads() {
    let tree = tree();
    let tasks = tasks_of(RELEASE_WORKFLOW);

    assert!(!tasks.is_empty(), "the release runs this program");
    for words in tasks {
        scheduled(&tree, words);
    }
}

/// Each job of `workflow` by its name, with the lines that state it: those after `jobs:`, a job opening on its name two columns in.
fn jobs_of(workflow: &str) -> Vec<(&str, Vec<&str>)> {
    let mut jobs: Vec<(&str, Vec<&str>)> = Vec::new();
    for line in workflow.lines().skip_while(|line| *line != "jobs:").skip(1) {
        let opening = line
            .strip_prefix("  ")
            .filter(|rest| !rest.starts_with(' ') && !rest.starts_with('#'))
            .and_then(|rest| rest.strip_suffix(':'));
        match (opening, jobs.last_mut()) {
            (Some(name), _) => jobs.push((name, Vec::new())),
            (None, Some((_, lines))) => lines.push(line),
            (None, None) => {}
        }
    }

    jobs
}

/// Every job of either workflow that runs `cargo` installs the toolchain `rust-toolchain.toml` pins through the shared setup step before it, so no job leaves the toolchain to rustup's first `cargo`, which installs it with a warning and caches nothing. Mutation-checked: with the release job's setup step taken out, the job is named here.
#[test]
fn every_job_that_runs_cargo_installs_the_pinned_toolchain_first() {
    for (workflow, text) in [("check", CHECK_WORKFLOW), ("release", RELEASE_WORKFLOW)] {
        for (job, lines) in jobs_of(text) {
            let runs = |line: &&str| {
                let line = line.trim();
                !line.starts_with('#')
                    && line
                        .strip_prefix("run: ")
                        .unwrap_or(line)
                        .starts_with("cargo ")
            };
            let Some(cargo) = lines.iter().position(runs) else {
                continue;
            };
            let setup = lines
                .iter()
                .position(|line| line.trim() == "- uses: ./.github/actions/setup");

            assert!(
                setup.is_some_and(|setup| setup < cargo),
                "the {workflow} workflow's job `{job}` runs cargo before it installs the pinned toolchain"
            );
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

/// A crate's build names the version of the data it reads, which names the directory it finds it in, so the version the task files pin data at is the one that crate's build script names: a bump of either fails here until the other follows, rather than at a build that finds nothing kept.
#[test]
fn data_a_crate_s_build_reads_is_pinned_at_the_version_the_build_names() {
    let tree = tree();
    for (data, script, constant) in [
        ("unicode", UNICODE_BUILD, "VERSION"),
        ("perfetto", PROFILE_BUILD, "REVISION"),
        ("binaryen", BINARYEN_BUILD, "BINARYEN_VERSION"),
    ] {
        let named = script
            .lines()
            .find_map(|line| {
                line.strip_prefix(&format!("const {constant}: &str = \""))?
                    .strip_suffix("\";")
            })
            .unwrap_or_else(|| panic!("the build script of `{data}`'s reader names `{constant}`"));
        let pinned = &tree.data[&vec!["data".to_string(), data.to_string()]];

        assert_eq!(pinned.version, named, "`data.{data}`");
        assert!(
            pinned.beside.is_some(),
            "`data.{data}` is kept beside its reader"
        );
    }
}
