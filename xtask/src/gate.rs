//! Running steps: one action streamed to the terminal, or many steps each as a process of its own writing a log of its own.
//!
//! **An action is streamed; a subject and the gate are logged.** One action is something a person watches, so [`perform`] runs its steps here, one after another, on the terminal's own streams. A bare subject and the gate are many steps at once, so [`gate`] starts each as a process of its own — this program, asked for the one step by the name its row carries — with both of its streams in `target/gate/<step>.log`, and says on the terminal only how each ended. Nothing a step does differs between the two: both run [`Step::run`].
//!
//! **The order is the rows'.** A step starts once every step it is run after, among those running, has passed; steps that wait for nothing start at once, with no cap, since cargo already serializes the builds that share a directory. A step whose prerequisite did not pass is not run, and is reported as held by it. Every other step still runs: a gate reports every failure rather than the first.
//!
//! **A step a path condition holds back is skipped, and said to be.** The base is the revision the gate was given, local `main` otherwise, compared with the working tree.

use {
    crate::{Request, Step, ask, spelled, target_directory},
    clap::ValueEnum,
    std::{
        collections::BTreeMap,
        env,
        fs::{self, File},
        mem,
        path::Path,
        process::{Command, Stdio},
        sync::mpsc,
        thread,
        time::{Duration, Instant},
    },
};

#[cfg(test)]
mod tests;

/// How one step of a run ended.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Outcome {
    Passed(Duration),
    Failed(Duration),
    /// A tool it needs is missing: what to install.
    Refused(String),
    /// Nothing its condition reads changed since this base.
    Skipped(String),
    /// A step it is run after did not pass.
    Held(Step),
}

impl Outcome {
    /// Whether a step waiting for this one may run.
    fn clears(&self) -> bool {
        matches!(self, Outcome::Passed(_) | Outcome::Skipped(_))
    }

    /// Whether the run failed for it.
    fn fails(&self) -> bool {
        matches!(
            self,
            Outcome::Failed(_) | Outcome::Refused(_) | Outcome::Held(_)
        )
    }
}

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

/// The paths that differ between `base` and the working tree, one a line as git names them.
fn changed_since(base: &str) -> Result<Vec<String>, String> {
    let mut git = Command::new("git");
    git.args(["diff", "--name-only", base]);

    Ok(ask(git, &[])?.lines().map(str::to_string).collect())
}

/// Why `step` is not to run against `base`, where its row's condition says so. A base that is no revision here spares it too, and says so: a shallow checkout holds nothing to compare the working tree with, and the step such a condition guards is one too costly to run on a guess.
fn spared(step: Step, base: &str) -> Option<String> {
    let read = step.row().when?;

    match changed_since(base) {
        Ok(changed) if changed.iter().any(|path| read.contains(&path.as_str())) => None,
        Ok(_) => Some(format!("nothing it reads changed since {base}")),
        Err(_) => Some(format!(
            "`{base}` is no revision here to compare the working tree with"
        )),
    }
}

/// The first tool `step` needs that is not ready, as the sentence saying what installs it.
fn unready(step: Step) -> Option<String> {
    step.row().needs.iter().find_map(|tool| tool.ready().err())
}

/// Run `asked` and what they are run with first, here and on the terminal's streams, each to its end whatever the one before it did.
pub(crate) fn perform(asked: &[Step], request: &Request, base: &str) -> Result<(), String> {
    let mut failed = Vec::new();

    for step in closed(asked) {
        if let Some(reason) = spared(step, base) {
            eprintln!("skipped {}: {reason}", spelled(step));
            continue;
        }

        let outcome = match unready(step) {
            Some(refusal) => Err(refusal),
            None => step.run(request),
        };
        if let Err(message) = outcome {
            eprintln!("{message}");
            failed.push(spelled(step));
        }
    }

    match failed.is_empty() {
        true => Ok(()),
        false => Err(format!("failed: {}", failed.join(", "))),
    }
}

/// What `step` waits on among `running`: nothing, a step still to finish, or one that did not pass.
enum Standing {
    Ready,
    Waiting,
    Held(Step),
}

fn standing(step: Step, running: &[Step], outcomes: &BTreeMap<usize, Outcome>) -> Standing {
    let row = step.row();
    let mut waiting = false;

    for before in row.ensures.iter().chain(row.after) {
        if !running.contains(before) {
            continue;
        }

        match outcomes.get(&place(*before)) {
            None => waiting = true,
            Some(outcome) if outcome.clears() => {}
            Some(_) => return Standing::Held(*before),
        }
    }

    match waiting {
        true => Standing::Waiting,
        false => Standing::Ready,
    }
}

/// Where `step` stands in the table, which orders a report.
fn place(step: Step) -> usize {
    Step::value_variants()
        .iter()
        .position(|other| *other == step)
        .expect("every step is in the table")
}

/// Run `step` as a process of its own with its streams in `log`, and how it ended.
fn spawned(step: Step, log: &Path) -> Outcome {
    let started = Instant::now();
    let passed = File::create(log)
        .and_then(|file| Ok((file.try_clone()?, file)))
        .and_then(|(out, err)| {
            Command::new(env::current_exe()?)
                .args(["step", &spelled(step)])
                .stdin(Stdio::null())
                .stdout(out)
                .stderr(err)
                .status()
        })
        .is_ok_and(|status| status.success());

    match passed {
        true => Outcome::Passed(started.elapsed()),
        false => Outcome::Failed(started.elapsed()),
    }
}

/// One line saying how `step` ended.
fn told(step: Step, outcome: &Outcome) -> String {
    let name = spelled(step);

    match outcome {
        Outcome::Passed(time) => format!("passed   {name} ({}s)", time.as_secs()),
        Outcome::Failed(time) => format!(
            "FAILED   {name} ({}s): target/gate/{name}.log",
            time.as_secs()
        ),
        Outcome::Refused(refusal) => format!("REFUSED  {name}: {refusal}"),
        Outcome::Skipped(reason) => format!("skipped  {name}: {reason}"),
        Outcome::Held(by) => format!("HELD     {name}: {} did not pass", spelled(*by)),
    }
}

/// Run `asked` and what they are run with first, each a process of its own writing `target/gate/<step>.log`, in the order their rows derive, and report how each ended.
pub(crate) fn gate(asked: &[Step], base: &str) -> Result<(), String> {
    let running = closed(asked);
    let logs = target_directory().join("gate");
    fs::create_dir_all(&logs).map_err(|error| format!("{}: {error}", logs.display()))?;

    let mut outcomes = BTreeMap::<usize, Outcome>::new();
    let mut pending = running.clone();
    let mut started = 0;
    let (sender, receiver) = mpsc::channel::<(Step, Outcome)>();

    thread::scope(|scope| {
        loop {
            // Start or settle everything that can be, until a pass changes nothing: a step settled without running may clear, or hold, one that waited for it.
            let mut settled = true;
            while settled {
                settled = false;
                for step in mem::take(&mut pending) {
                    let outcome = match standing(step, &running, &outcomes) {
                        Standing::Waiting => {
                            pending.push(step);
                            continue;
                        }
                        Standing::Held(by) => Some(Outcome::Held(by)),
                        Standing::Ready => spared(step, base)
                            .map(Outcome::Skipped)
                            .or_else(|| unready(step).map(Outcome::Refused)),
                    };

                    match outcome {
                        Some(outcome) => {
                            eprintln!("{}", told(step, &outcome));
                            outcomes.insert(place(step), outcome);
                            settled = true;
                        }
                        None => {
                            let log = logs.join(format!("{}.log", spelled(step)));
                            let sender = sender.clone();
                            eprintln!("started  {}", spelled(step));
                            started += 1;
                            scope.spawn(move || {
                                // The receiver outlives every sender, so a send cannot fail.
                                let _ = sender.send((step, spawned(step, &log)));
                            });
                        }
                    }
                }
            }

            // Nothing runs, so nothing more can finish: the table's order is one a run can follow, so nothing is left waiting.
            if started == 0 {
                break;
            }

            let (step, outcome) = receiver.recv().expect("a started step reports");
            started -= 1;
            eprintln!("{}", told(step, &outcome));
            outcomes.insert(place(step), outcome);
        }
    });

    eprintln!();
    for step in &running {
        if let Some(outcome) = outcomes.get(&place(*step)) {
            eprintln!("{}", told(*step, outcome));
        }
    }

    let failed = running
        .iter()
        .filter(|step| outcomes.get(&place(**step)).is_none_or(Outcome::fails))
        .map(|step| spelled(*step))
        .collect::<Vec<_>>();

    match failed.is_empty() {
        true => Ok(()),
        false => Err(format!("failed: {}", failed.join(", "))),
    }
}
