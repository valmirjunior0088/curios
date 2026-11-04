//! What the repository's own task files hold of the rules `README.md` states, and what the workflows hold of them.

use {crate::*, std::collections::BTreeSet};

/// The check workflow, whose jobs are the machines the steps run on.
const WORKFLOW: &str = include_str!("../../.github/workflows/check.yml");

/// The release workflow, which builds the bundle a release ships.
const RELEASE_WORKFLOW: &str = include_str!("../../.github/workflows/release.yml");

/// The lock file, which says what version of `wasm-bindgen` the bundle builds against.
const LOCK: &str = include_str!("../../Cargo.lock");

const MANIFEST: &str = include_str!("../../Cargo.toml");

const BIOME: &str = include_str!("../../biome.json");

fn tree() -> Tree {
    Tree::read().unwrap()
}

/// Every word of every command of the steps `group` holds.
fn commands_of<'a>(tree: &'a Tree, group: &str) -> Vec<(&'a Step, Vec<String>)> {
    tree.steps_of(&[group.to_string()])
        .into_iter()
        .flat_map(|step| {
            let step = &tree.steps[step];
            step.run.iter().map(move |command| {
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

/// The value the variable `name` writes, where it writes one word.
fn written(tree: &Tree, name: &str) -> String {
    match &tree.variables[&name.split('.').map(str::to_string).collect::<Name>()] {
        Variable::Words(words) => words.concat(),
        other => panic!("{name} is {other:?}"),
    }
}

/// Every line of the check workflow that runs this program is a command the parser reads, and together they run every step the gate runs: a step added to a subject and to no job fails here, and so does a job left spelling a task that is gone.
#[test]
fn every_step_the_gate_runs_has_a_job() {
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

    let gate = vec!["gate".to_string()];
    for step in schedule(&tree, &gate, &Given::new()) {
        if tree.steps[step].name != gate {
            assert!(run.contains(&step), "{}", spelled(&tree.steps[step].name));
        }
    }
}

/// The workspace is checked as it ships: no step of it turns a feature on, which is `instrumented`'s.
#[test]
fn a_workspace_step_names_no_feature() {
    let tree = tree();

    for (step, words) in commands_of(&tree, "workspace") {
        assert!(
            words.iter().all(|word| !word.contains("feature")),
            "{}: {words:?}",
            dotted(&step.name)
        );
    }
}

/// A lint goes on past a crate that fails, so one run reports every crate's errors rather than the first's.
#[test]
fn a_workspace_lint_goes_on_past_a_crate_that_fails() {
    let tree = tree();

    for group in ["workspace", "instrumented", "native", "tools"] {
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

/// `rustfmt` is handed the edition `cargo fmt` reads from the manifest, for the Rust no package holds.
#[test]
fn the_edition_handed_to_rustfmt_is_the_workspace_s() {
    let manifest = MANIFEST.parse::<toml_edit::DocumentMut>().unwrap();

    assert_eq!(
        written(&tree(), "workspace.edition"),
        manifest["workspace"]["package"]["edition"]
            .as_str()
            .unwrap()
    );
}

/// Every directory `biome.json` reads outside the editors' own trees is one the workspace's Biome steps hand it, so no file Biome reads is left to no step.
#[test]
fn the_workspace_s_biome_steps_reach_every_file_biome_reads_of_it() {
    let tree = tree();
    let Variable::Words(handed) =
        &tree.variables[&vec!["workspace".to_string(), "web".to_string()]]
    else {
        panic!("the workspace's web paths are words");
    };
    // The one list `biome.json` holds, read as text: the file is JSON, which nothing else here parses.
    let includes = BIOME
        .split("\"includes\": [")
        .nth(1)
        .and_then(|rest| rest.split(']').next())
        .expect("biome.json lists the files it reads");
    for include in includes
        .split(',')
        .map(|item| item.trim().trim_matches('"'))
    {
        if include.is_empty() || include.starts_with('!') || include.starts_with("editors/") {
            continue;
        }
        assert!(
            handed
                .iter()
                .any(|path| include == path || include.starts_with(&format!("{path}/"))),
            "{include}"
        );
    }
}

/// The generator must match the `wasm-bindgen` crate the bundle is built against exactly, so each workflow installs it at the lock's version, and a bump of the crate fails here until the pins follow.
#[test]
fn the_bindings_generator_is_pinned_at_the_lock_s_version() {
    let pin = |workflow: &'static str| {
        workflow
            .lines()
            .map(str::trim)
            .find_map(|line| line.strip_prefix("WASM_BINDGEN: "))
            .expect("the workflow pins the bindings generator")
    };
    let lock = LOCK.parse::<toml_edit::DocumentMut>().unwrap();
    let locked = lock["package"]
        .as_array_of_tables()
        .unwrap()
        .iter()
        .find(|package| package["name"].as_str() == Some("wasm-bindgen"))
        .and_then(|package| package["version"].as_str())
        .unwrap();

    assert_eq!(pin(WORKFLOW), locked);
    assert_eq!(pin(RELEASE_WORKFLOW), locked);
}
