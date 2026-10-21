//! How the steps asked for are run, and the one place a plan's tasks are performed.
//!
//! [`perform`] runs what the command line selected: one step after another in the table's order, each after what it is run with first, on the terminal's own streams, and each to its end whatever the one before it did, so one run reports every failure rather than the first. A step's work is its plan's tasks, performed in order by [`act`] and by nothing else: a recipe only says what is to be done.

use {
    crate::{
        Checkout, Request, Step, Task, bindgen_web, file_with_inputs, launch_in, modified, spelled,
    },
    clap::ValueEnum,
    std::{ffi::OsString, fs, time::Instant},
};

#[cfg(test)]
mod tests;

/// `asked` with every step they are run with first, in the table's order and each once.
pub(crate) fn closed(asked: &[Step]) -> Vec<Step> {
    let mut wanted = asked.to_vec();
    let mut next = 0;
    while next < wanted.len() {
        for ensured in wanted[next].row().ensures {
            if !wanted.contains(ensured) {
                wanted.push(*ensured);
            }
        }
        next += 1;
    }

    Step::value_variants()
        .iter()
        .copied()
        .filter(|step| wanted.contains(step))
        .collect()
}

/// The first step `step` ensures that failed or was not run, where there is one: what `step` reads is then not there.
fn blocked(step: Step, failed: &[Step], unrun: &[Step]) -> Option<Step> {
    step.row()
        .ensures
        .iter()
        .copied()
        .find(|ensured| failed.contains(ensured) || unrun.contains(ensured))
}

/// Run `asked` and what they are run with first, as `request` narrows them, each to its end whatever the one before it did, and say of each how it ended and in how long. A step lacking a tool it launches is refused with what installs it, and a step whose prerequisite did not pass is not run, since what it reads is not there: the steps that read nothing of the failure go on, and the end names both lists.
pub(crate) fn perform(asked: &[Step], request: &Request) -> Result<(), String> {
    let checkout = Checkout::default();
    let mut failed = Vec::new();
    let mut unrun = Vec::new();

    for step in closed(asked) {
        if let Some(missing) = blocked(step, &failed, &unrun) {
            eprintln!(
                "{} not run: {} did not pass",
                spelled(step),
                spelled(missing)
            );
            unrun.push(step);
            continue;
        }

        let started = Instant::now();
        let outcome = step
            .plan(request, &checkout)
            .and_then(|plan| plan.iter().try_for_each(act));
        let took = started.elapsed().as_secs_f64();

        match outcome {
            Ok(()) => eprintln!("{} passed in {took:.1}s", spelled(step)),
            Err(message) => {
                eprintln!("{message}");
                eprintln!("{} failed in {took:.1}s", spelled(step));
                failed.push(step);
            }
        }
    }

    let named = |steps: &[Step]| {
        steps
            .iter()
            .map(|step| spelled(*step))
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

/// Perform `task`, saying what it does as it does it.
fn act(task: &Task) -> Result<(), String> {
    match task {
        Task::Run(run) => {
            let arguments = run.spelled();
            let environment = run
                .environment
                .iter()
                .map(|(name, value)| (*name, OsString::from(value.spelled().concat())))
                .collect::<Vec<_>>();

            launch_in(
                &run.tree.directory(),
                run.tool,
                &environment
                    .iter()
                    .map(|(name, value)| (*name, value.as_os_str()))
                    .collect::<Vec<_>>(),
                &arguments.iter().map(String::as_str).collect::<Vec<_>>(),
            )
        }
        Task::File {
            built,
            filed,
            listed,
        } => file_with_inputs(built, filed, listed),
        Task::Bindgen { module, bundle } => bindgen_web(module, bundle),
        Task::Write { path, text } => {
            fs::write(path, text).map_err(|error| format!("{}: {error}", path.display()))?;
            eprintln!("wrote {}", path.display());

            Ok(())
        }
        Task::Keeps {
            path,
            during,
            rewritten,
        } => {
            let before =
                modified(path).ok_or_else(|| format!("{} is not there to keep", path.display()))?;
            act(during)?;

            match modified(path) == Some(before) {
                true => Ok(()),
                false => Err(format!("{rewritten}: {} was written again", path.display())),
            }
        }
    }
}
