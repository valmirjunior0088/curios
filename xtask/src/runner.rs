//! How a run is performed, and the one place a step's commands are launched.
//!
//! **A pinned program is asked its version once a run, before its first step.** Every program the scheduled steps launch — a command's own, read off its words as written, and a variable's command a step reads — that the task files pin has its `--version` held to the pin before anything runs, and a run any of them misses runs nothing, naming each and the version its pin needs. A program no scheduled step launches is never asked.
//!
//! **A run reports every failure rather than the first.** [`perform`] takes the schedule and runs each step to its end whatever the one before it did: a step's commands run in order, the first that fails failing the step, and a step whose produced files are not there afterwards fails too. A step behind one that did not pass is not run, since what it reads is not there; the steps that read nothing of the failure go on, and the end names what failed and what was not run.

use {
    crate::{
        Fill, Given, Instruction, Name, Tree, Values, Variable, Word, check_version, directories,
        fetch, launch, launch_in, launched, root, schedule, spelled,
    },
    std::{collections::BTreeSet, env, fs, time::Instant},
};

#[cfg(test)]
mod tests;

/// Run `asked` and what it runs after, as `given` narrows it, and say of each step how it ended and in how long.
pub fn perform(tree: &Tree, asked: &Name, given: &Given) -> Result<(), String> {
    let values = Values::new(tree, given);
    let scheduled = schedule(tree, asked, given);
    check_pins(tree, &scheduled)?;
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
            Ok(()) => eprintln!("{name} passed in {took:.1}s"),
            Err(message) => {
                eprintln!("{message}");
                eprintln!("{name} failed in {took:.1}s");
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

/// Hold every pinned program the `scheduled` steps launch to its pin, a command's own and a variable's command a step reads, refusing with every program that misses.
fn check_pins(tree: &Tree, scheduled: &[usize]) -> Result<(), String> {
    let mut programs = BTreeSet::new();
    for &step in scheduled {
        for instruction in &tree.steps[step].run {
            let Instruction::Command(words) = instruction else {
                continue;
            };
            programs.extend(launched(words).map(|(_, program)| program));
            for fill in words.iter().flat_map(Word::fills) {
                if let Fill::Variable(name) = fill
                    && let Variable::Output(command) = &tree.variables[name]
                    && let Ok(launch) = launch(command)
                {
                    programs.insert(launch.program);
                }
            }
        }
    }

    let refused = programs
        .iter()
        .filter_map(|program| Some((program, tree.programs.get(program)?)))
        .filter_map(|(program, pin)| check_version(program, pin).err())
        .collect::<Vec<_>>();
    match refused.is_empty() {
        true => Ok(()),
        false => Err(format!("{}\nnothing run", refused.join("\n"))),
    }
}

/// Run one step's instructions in order — each command launched, each write's file written whole, its directory made where it is not — then hold the step to what it produces.
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
        };
        let Some(words) = values.command(command)? else {
            continue;
        };
        let mut launched = launch(&words)?;
        if let Some(path) = &path {
            launched.command.env("PATH", path);
        }
        let pin = tree.programs.get(&launched.program);
        launch_in(&directory, launched, pin)?;
    }

    for produced in &step.produces {
        let path = values.path(produced)?;
        if !path.exists() {
            return Err(format!("{} was not left", path.display()));
        }
    }

    Ok(())
}
