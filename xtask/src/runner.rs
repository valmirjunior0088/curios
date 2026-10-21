//! How the steps asked for are run, and the one place a plan's tasks are performed.
//!
//! **What a step is run after is read off its plan.** [`schedule`] plans the steps asked for, finds the one step that leaves each product their tasks read, plans that step too, and so on until nothing new is read; then it orders them in the table's order, each after the steps that leave what it reads. Every product is left by one step and the table orders every such step before what reads it, so a run walks the rows as they stand, and a step two others read from runs once.
//!
//! **A run reports every failure rather than the first.** [`perform`] runs the schedule on the terminal's own streams, each step to its end whatever the one before it did: a step's work is its plan's tasks, performed in order by [`act`] and by nothing else, and a step behind one that did not pass is not run, since what it reads is not there.

use {
    crate::{
        Checkout, Product, Request, Step, Task, bindgen_web, file_with_inputs, launch_in, modified,
        spelled,
    },
    clap::ValueEnum,
    std::{
        collections::{BTreeMap, VecDeque},
        ffi::OsString,
        fs,
        time::Instant,
    },
};

#[cfg(test)]
mod tests;

/// A step a run performs: its plan, and the steps that leave what the plan reads.
#[derive(Debug)]
pub(crate) struct Planned {
    pub(crate) step: Step,
    plan: Vec<Task>,
    after: Vec<Step>,
}

/// The step that leaves each product, read off every step's plan.
fn producers(checkout: &Checkout) -> Result<BTreeMap<Product, Step>, String> {
    let mut producers = BTreeMap::new();
    for step in Step::value_variants() {
        for task in step.plan(&Request::whole(), checkout)? {
            if let Some(product) = task.leaves()
                && let Some(other) = producers.insert(product, *step)
                && other != *step
            {
                return Err(format!(
                    "{} and {} both leave {}",
                    spelled(other),
                    spelled(*step),
                    product.path().display()
                ));
            }
        }
    }

    Ok(producers)
}

/// The steps a run of `asked` performs under `request`, each with its plan: those asked for, planned as `request` narrows them, and the steps that leave what any of them reads, planned whole. They stand in the table's order, each after the steps that leave what it reads.
pub(crate) fn schedule(
    asked: &[Step],
    request: &Request,
    checkout: &Checkout,
) -> Result<Vec<Planned>, String> {
    let producers = producers(checkout)?;

    let mut planned = BTreeMap::new();
    let mut pending = asked
        .iter()
        .map(|step| (*step, request.clone()))
        .collect::<VecDeque<_>>();
    while let Some((step, request)) = pending.pop_front() {
        if planned.contains_key(&(step as usize)) {
            continue;
        }

        let plan = step.plan(&request, checkout)?;
        let mut after = Vec::new();
        for task in &plan {
            for product in task.reads(checkout)? {
                let producer = *producers.get(&product).ok_or_else(|| {
                    format!(
                        "{} reads {}, which no step leaves",
                        spelled(step),
                        product.path().display()
                    )
                })?;
                if producer != step && !after.contains(&producer) {
                    after.push(producer);
                    pending.push_back((producer, Request::whole()));
                }
            }
        }

        planned.insert(step as usize, Planned { step, plan, after });
    }

    let mut ordered = Vec::<Planned>::new();
    while !planned.is_empty() {
        let ready = planned
            .iter()
            .find(|(_, candidate)| {
                candidate
                    .after
                    .iter()
                    .all(|producer| ordered.iter().any(|done| done.step == *producer))
            })
            .map(|(place, _)| *place);

        match ready.and_then(|place| planned.remove(&place)) {
            Some(next) => ordered.push(next),
            None => {
                return Err(format!(
                    "these steps read what each other leaves: {}",
                    planned
                        .values()
                        .map(|stuck| spelled(stuck.step))
                        .collect::<Vec<_>>()
                        .join(", ")
                ));
            }
        }
    }

    Ok(ordered)
}

/// The first step `planned` is run after that failed or was not run, where there is one: what `planned` reads is then not there.
fn blocked(planned: &Planned, failed: &[Step], unrun: &[Step]) -> Option<Step> {
    planned
        .after
        .iter()
        .copied()
        .find(|producer| failed.contains(producer) || unrun.contains(producer))
}

/// Run `asked` and what they read from, as `request` narrows them, each to its end whatever the one before it did, and say of each how it ended and in how long. A step lacking a tool it launches is refused with what installs it, and a step behind one that did not pass is not run, since what it reads is not there: the steps that read nothing of the failure go on, and the end names both lists.
pub(crate) fn perform(asked: &[Step], request: &Request) -> Result<(), String> {
    let checkout = Checkout::default();
    let mut failed = Vec::new();
    let mut unrun = Vec::new();

    for planned in schedule(asked, request, &checkout)? {
        let step = planned.step;
        if let Some(missing) = blocked(&planned, &failed, &unrun) {
            eprintln!(
                "{} not run: {} did not pass",
                spelled(step),
                spelled(missing)
            );
            unrun.push(step);
            continue;
        }

        let started = Instant::now();
        let outcome = planned.plan.iter().try_for_each(act);
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

/// Perform `task`, saying what it does as it does it. A task that leaves a product is held to it: the product must be there afterwards, as the release reads back what it wrote.
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
            )?;
        }
        Task::File {
            built,
            product,
            listed,
        } => file_with_inputs(built, &product.path(), listed)?,
        Task::Bindgen { module, product } => bindgen_web(module, &product.path())?,
        Task::Write { path, text } => {
            fs::write(path, text).map_err(|error| format!("{}: {error}", path.display()))?;
            eprintln!("wrote {}", path.display());
        }
        Task::Keeps {
            path,
            during,
            rewritten,
        } => {
            let before =
                modified(path).ok_or_else(|| format!("{} is not there to keep", path.display()))?;
            act(during)?;

            if modified(path) != Some(before) {
                return Err(format!("{rewritten}: {} was written again", path.display()));
            }
        }
    }

    match task.leaves() {
        Some(product) if !product.path().exists() => {
            Err(format!("{} was not left", product.path().display()))
        }
        _ => Ok(()),
    }
}
