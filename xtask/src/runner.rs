//! How a run is performed, and the one place a step's commands are launched.
//!
//! **A run's tools are kept before its first step.** Every tool the scheduled steps name is fetched where it is not kept yet, and a run any of them cannot be had for runs nothing, naming each ([`fetch`]). A step's commands find first on their `PATH` the directory of each tool it names that is kept as a directory.
//!
//! **A run reports every failure rather than the first.** [`perform`] takes the schedule and runs each step to its end whatever the one before it did: a step's commands run in order, the first that fails failing the step, and a step whose produced files are not there afterwards fails too. A step behind one that did not pass is not run, since what it reads is not there; the steps that read nothing of the failure go on, and the end names what failed and what was not run.

use {
    crate::{
        Given, Instruction, Name, Tree, Values, Word, directories, fetch, launch, launch_in, root,
        say, schedule, spelled,
    },
    std::{env, fs, time::Instant},
};

#[cfg(test)]
mod tests;

/// The mark of a step that passed, on the line that closes it: the one `curios test` prints for a test that passed (`curios/src/report/tree.rs`), so the two transcripts read alike.
const PASSED: &str = "✓";

/// The mark of a step that did not pass, as `curios test` marks a test that did not.
const UNPASSED: &str = "✗";

/// Run `asked` and what it runs after, as `given` narrows it, and close each step with a line naming it, `»»» <step> «««`, how it ended and in how long.
pub fn perform(tree: &Tree, asked: &Name, given: &Given) -> Result<(), String> {
    let values = Values::new(tree, given);
    let scheduled = schedule(tree, asked, given);
    fetch(tree, &values, &scheduled)?;
    let mut failed = Vec::new();
    let mut unrun = Vec::new();

    for step in scheduled {
        let name = spelled(&tree.steps[step].name);
        let blocked = tree
            .prerequisites(step)
            .into_iter()
            .find(|other| failed.contains(other) || unrun.contains(other));
        if let Some(other) = blocked {
            eprintln!(
                "{name} not run: {} did not pass",
                spelled(&tree.steps[other].name)
            );
            unrun.push(step);
            continue;
        }

        let started = Instant::now();
        let outcome = run(tree, &values, step);
        let took = started.elapsed().as_secs_f64();

        match outcome {
            Ok(()) => eprintln!("»»» {name} ««« {PASSED} {took:.1}s"),
            Err(message) => {
                eprintln!("{message}");
                eprintln!("»»» {name} ««« {UNPASSED} {took:.1}s");
                failed.push(step);
            }
        }
    }

    let named = |steps: &[usize]| {
        steps
            .iter()
            .map(|step| spelled(&tree.steps[*step].name))
            .collect::<Vec<_>>()
            .join(", ")
    };

    match (failed.is_empty(), unrun.is_empty()) {
        (true, true) => Ok(()),
        (false, true) => Err(format!("failed: {}", named(&failed))),
        (_, false) => Err(format!(
            "failed: {}; not run: {}",
            named(&failed),
            named(&unrun)
        )),
    }
}

/// Run one step's instructions in order — each command launched, each write's file written whole, its directory made where it is not, each check judged — then hold the step to what it produces.
fn run(tree: &Tree, values: &Values<'_>, step: usize) -> Result<(), String> {
    // Each tool the step names that ships beside others, found first by every launch the step makes, its own nested ones included.
    let found = directories(tree, step);
    let path = match found.is_empty() {
        true => None,
        false => Some(
            env::join_paths(
                found
                    .into_iter()
                    .chain(env::split_paths(&env::var_os("PATH").unwrap_or_default())),
            )
            .map_err(|error| format!("a tool's directory cannot stand on `PATH`: {error}"))?,
        ),
    };
    let step = &tree.steps[step];
    let directory = root().join(step.dir.as_deref().unwrap_or(""));

    for instruction in &step.run {
        let command = match instruction {
            Instruction::Command(command) => command,
            Instruction::Write { path, contents } => {
                let path = directory.join(values.text(path)?);
                let failed = |error: std::io::Error| format!("{}: {error}", path.display());
                if let Some(parent) = path.parent() {
                    fs::create_dir_all(parent).map_err(failed)?;
                }
                fs::write(&path, values.text(contents)?).map_err(failed)?;
                continue;
            }
            Instruction::Check(check) => {
                // Said as a command's line is, so the terminal reads as a transcript of the work.
                say(&format!("check {}", check.name));
                (check.judge)()?;
                continue;
            }
        };
        let Some(words) = values.command(command)? else {
            continue;
        };
        let mut launched = launch(&words)?;
        if let Some(path) = &path {
            launched.command.env("PATH", path);
        }
        let written = command
            .iter()
            .map(Word::written)
            .collect::<Vec<_>>()
            .join(" ");
        launch_in(&directory, launched, &written)?;
    }

    for produced in &step.produces {
        let path = values.path(produced)?;
        if !path.exists() {
            return Err(format!("{} was not left", path.display()));
        }
    }

    Ok(())
}
