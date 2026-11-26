//! How a run is performed, and the one place a step's commands are launched.
//!
//! **A run's tools are kept before its first step.** Every tool the scheduled steps name is fetched where it is not kept yet, and a run any of them cannot be had for runs nothing, naming each ([`fetch`]). A step's commands find first on their `PATH` the directory of each tool it names that is kept as a directory.
//!
//! **A run reports every failure rather than the first.** [`perform`] takes the schedule and runs each step to its end whatever the one before it did: a step's commands run in order, the first that fails failing the step, and a step whose produced files are not there afterwards fails too. A step behind one that did not pass is not run, since what it reads is not there, and says so on a line of its own; the steps that read nothing of the failure go on. A failure is said once, on the line that closes its step, so the run hands back only whether every step passed, which the exit code says.

use {
    crate::{
        GivenValues, Instruction, Name, Tree, Values, Word, concluded, directories, ending, fetch,
        launch, launch_in, root, say_apart, schedule, spelled,
    },
    std::{env, fs, path::PathBuf, time::Instant},
};

#[cfg(test)]
mod tests;

/// Run `asked` and what it runs after, as `given` narrows it, and close each step with a line naming it, `` === `<step>`: ✓ 58.7s ``, how it ended and in how long, or that it was not run and which step before it did not pass: whether every step passed, and a refusal where a tool the steps name could not be had and nothing ran.
pub fn perform(tree: &Tree, asked: &Name, given: &GivenValues) -> Result<bool, String> {
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
            say_apart(&concluded(
                &name,
                &format!(
                    "not run, `{}` did not pass",
                    spelled(&tree.steps[other].name)
                ),
            ));
            unrun.push(step);
            continue;
        }

        let started = Instant::now();
        let outcome = run(tree, &values, step);
        let took = started.elapsed();

        if let Err(message) = &outcome {
            eprintln!("{message}");
            failed.push(step);
        }
        say_apart(&concluded(&name, &ending(outcome.is_ok(), took)));
    }

    Ok(failed.is_empty() && unrun.is_empty())
}

/// The directory an instruction stands in: its `in`, read from the repository's root, or the root where it states none.
fn placed(values: &Values<'_>, directory: Option<&Word>) -> Result<PathBuf, String> {
    Ok(match directory {
        Some(directory) => root().join(values.text(directory)?),
        None => root().to_path_buf(),
    })
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

    for instruction in &step.run {
        let (command, directory) = match instruction {
            Instruction::Command { words, directory } => (words, directory),
            Instruction::Write {
                path,
                contents,
                directory,
            } => {
                let path = placed(values, directory.as_ref())?.join(values.text(path)?);
                let failed = |error: std::io::Error| format!("{}: {error}", path.display());
                if let Some(parent) = path.parent() {
                    fs::create_dir_all(parent).map_err(failed)?;
                }
                fs::write(&path, values.text(contents)?).map_err(failed)?;
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
        // A command run elsewhere than the root says where, so its echo reads as what a person would type there.
        let written = match directory {
            Some(directory) => format!("in {}: {written}", directory.written()),
            None => written,
        };
        launch_in(&placed(values, directory.as_ref())?, launched, &written)?;
    }

    for produced in &step.produces {
        let path = values.path(produced)?;
        if !path.exists() {
            return Err(format!("{} was not left", path.display()));
        }
    }

    Ok(())
}
