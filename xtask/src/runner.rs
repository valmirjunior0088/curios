//! How a run is performed, and the one place a step's commands are launched.
//!
//! **A pinned program is asked its version once a run.** The first time a step launches a program the task files pin, its `--version` is held to the pin before it runs, and a program that does not meet it fails that step, saying what installs the one the files name.
//!
//! **A run reports every failure rather than the first.** [`perform`] takes the schedule and runs each step to its end whatever the one before it did: a step's commands run in order, the first that fails failing the step, and a step whose produced files are not there afterwards fails too. A step behind one that did not pass is not run, since what it reads is not there; the steps that read nothing of the failure go on, and the end names what failed and what was not run.

use {
    crate::{
        Given, Instruction, Name, Tree, Values, check_version, launch, launch_in, root, schedule,
        spelled,
    },
    std::{collections::BTreeSet, fs, time::Instant},
};

#[cfg(test)]
mod tests;

/// Run `asked` and what it runs after, as `given` narrows it, and say of each step how it ended and in how long.
pub(crate) fn perform(tree: &Tree, asked: &Name, given: &Given) -> Result<(), String> {
    let values = Values::new(tree, given);
    let mut checked = BTreeSet::new();
    let mut failed = Vec::new();
    let mut unrun = Vec::new();

    for step in schedule(tree, asked, given) {
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
        let outcome = run(tree, &values, &mut checked, step);
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

/// Run one step's instructions in order — each command's pinned program held to its pin the first time the run launches it, each write's file written whole, its directory made where it is not — then hold the step to what it produces.
fn run(
    tree: &Tree,
    values: &Values<'_>,
    checked: &mut BTreeSet<String>,
    step: usize,
) -> Result<(), String> {
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
        let launched = launch(&words)?;
        let declared = tree.programs.get(&launched.program);
        if let Some(program) = declared
            && !checked.contains(&launched.program)
        {
            check_version(&launched.program, program)?;
            checked.insert(launched.program.clone());
        }
        launch_in(
            &directory,
            launched,
            declared.and_then(|program| program.install.as_deref()),
        )?;
    }

    for produced in &step.produces {
        let path = values.path(produced)?;
        if !path.exists() {
            return Err(format!("{} was not left", path.display()));
        }
    }

    Ok(())
}
