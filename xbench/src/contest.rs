//! The contest: every workload of the corpus built three ways, held to the answers it is known to have, and timed.
//!
//! **Interleaved, not batched.** A group runs the contestants round-robin rather than one contestant to exhaustion, so a disturbance during a group — a compile elsewhere on the machine, a thermal ramp, a page cache warming — reaches all three alike and the ratio between them survives it. Between the groups is the span a difference must clear; inside one is iteration noise.
//!
//! **No shell, and a cleared environment.** Each contestant is spawned directly, so a figure is one process rather than a shell's and the contestant's, and the environment is the same small thing every time: the size of an inherited environment shifts a stack and moves a measurement without anything looking wrong.
//!
//! **A sitting is a record of the contest.** Each contestant on each workload is one of its figures, named by [`named`], in microseconds, at the size the workload was timed at.

#[cfg(test)]
mod tests;

use {
    crate::{
        CONTEST, Figure, Measure, Record, asked, capture, edition, file, held, root, subject,
        table, today,
    },
    curios_profile::Instructions,
    std::{
        fs, io,
        path::{Path, PathBuf},
        process::{Command, Stdio},
        time::Instant,
    },
};

/// The name of `contestant`'s figure on `workload` in a record of the contest: the one spelling of it, which the contest files and the report reads.
pub fn named(workload: &str, contestant: &str) -> String {
    format!("{workload}/{contestant}")
}

/// One workload of the measurement corpus, `programs/<name>/`, which carries the same program in Curios and in Rust. What each computes is `programs/README.md`'s.
#[derive(Debug)]
pub struct Workload {
    pub name: &'static str,
    /// How a table names it.
    pub title: &'static str,
    /// The letter its size is reported under.
    pub letter: &'static str,
    /// The size it is timed at.
    pub size: u64,
    /// The small size every contestant is first held to one answer at.
    pub check: u64,
    /// What every contestant prints at `check`.
    pub answer: &'static str,
    /// What every contestant prints at `size`: the corpus's anchor.
    pub anchor: &'static str,
}

/// The corpus's workloads, in the order their tables are printed.
pub const WORKLOADS: &[Workload] = &[
    Workload {
        name: "lcg",
        title: "LCG",
        letter: "N",
        size: 200_000_000,
        check: 8,
        answer: "9345",
        anchor: "55661",
    },
    Workload {
        name: "trees",
        title: "trees",
        letter: "D",
        size: 21,
        check: 10,
        answer: "96122",
        anchor: "536864",
    },
    Workload {
        name: "chain",
        title: "chain",
        letter: "K",
        size: 4800,
        check: 8,
        answer: "819185",
        anchor: "378420",
    },
    Workload {
        name: "churn",
        title: "churn",
        letter: "N",
        size: 75_000_000,
        check: 8,
        answer: "897441",
        anchor: "762495",
    },
    Workload {
        name: "walk",
        title: "walk",
        letter: "N",
        size: 150_000,
        check: 8,
        answer: "464630",
        anchor: "318377",
    },
    Workload {
        name: "spines",
        title: "spines",
        letter: "N",
        size: 500_000,
        check: 8,
        answer: "28",
        anchor: "391724",
    },
];

/// One contestant: a compiler's build of a workload, run as its own process.
#[derive(Debug)]
pub struct Contestant {
    pub name: &'static str,
    /// How a table names it.
    pub title: &'static str,
}

/// Curios, and the one anchor, compiled two ways.
///
/// Rust native is the ceiling, and Rust native against Rust to WebAssembly is what WebAssembly itself costs, so what stands between Curios and `rust-wasm` is Curios's. On a workload that allocates, `rust-wasm` manages linear memory and is a *bound* rather than a peer: the gap holds Curios's codegen and the whole cost of delegating collection, and no contestant separates them.
pub const CONTESTANTS: &[Contestant] = &[
    Contestant {
        name: "curios",
        title: "Curios",
    },
    Contestant {
        name: "rust",
        title: "Rust (Native)",
    },
    Contestant {
        name: "rust-wasm",
        title: "Rust (WebAssembly)",
    },
];

/// Where the built contestants go: under the workspace's own build directory, so a sitting leaves nothing in the corpus.
const ARTIFACTS: &str = "target/xbench";

/// The WebAssembly target the Rust contestant is built for. The workspace's own toolchain already carries it, so the bench installs nothing.
const TARGET: &str = "wasm32-wasip2";

/// The runner `rust-wasm` runs under, the bench's own binary over the engine `Cargo.lock` resolves for the compiler's own, built where the compiler is.
const TWIN: &str = "target/release/twin";

/// How many independent batches a contestant is measured in. Between their medians is the span; two would make that span a single difference rather than an estimate of one.
const GROUPS: usize = 5;

/// How many timed rounds a group runs.
const EXECUTIONS: usize = 5;

/// How many untimed rounds open a group, enough to page the binary in and warm the file cache. Every contestant is compiled ahead of time, so there is no engine to settle.
const WARMUP: usize = 1;

/// How a contestant is run: a program and its arguments, the size among them.
#[derive(Debug)]
struct Invocation {
    program: PathBuf,
    arguments: Vec<String>,
}

/// Where the bench's build of `contestant` on `workload` lands. The one place the layout is spelled, so what is built and what is run cannot name different files.
fn artifact(contestant: &str, workload: &Workload, root: &Path) -> PathBuf {
    let built = root
        .join(ARTIFACTS)
        .join(workload.name)
        .display()
        .to_string();

    PathBuf::from(match contestant {
        "curios" => format!("{built}_curios"),
        "rust" => format!("{built}_rust"),
        // The twin's precompile, which its runner loads; the component it was compiled from is beside it ([`component`]).
        _ => format!("{built}_rust.cwasm"),
    })
}

/// The component `rust-wasm`'s twin is built as, beside the precompile its runner loads.
fn component(workload: &Workload, root: &Path) -> PathBuf {
    artifact("rust-wasm", workload, root).with_extension("wasm")
}

/// How `contestant` runs `workload` at `size`.
///
/// Every contestant reads its size from its arguments. The same spelling is what the agreement check runs and what is timed, so the two cannot run different things.
fn invocation(contestant: &str, workload: &Workload, size: u64, root: &Path) -> Invocation {
    let artifact = artifact(contestant, workload, root);

    match contestant {
        "curios" | "rust" => Invocation {
            program: artifact,
            arguments: vec![size.to_string()],
        },
        // The twin is no program: the bench's runner loads its precompile, on the engine the compiler embeds.
        _ => Invocation {
            program: root.join(TWIN),
            arguments: vec![
                "run".to_string(),
                artifact.display().to_string(),
                size.to_string(),
            ],
        },
    }
}

/// What one execution of a contestant gave: what it printed, how long the whole process took, and the instructions it retired.
#[derive(Debug)]
struct Execution {
    answer: String,
    /// Whole microseconds of wall clock, from before the process started to after it was waited for.
    microseconds: u64,
    /// The user-space instructions the process and everything it started retired, with the few the bench's own thread retires starting and waiting for it; `None` where the kernel opens no count, which is absent evidence, never nought.
    instructions: Option<u64>,
}

/// Run one contestant once: what it printed, how long the whole process took, and what it retired.
///
/// Every contestant is started by its path, so none needs a variable of the environment, and the environment is cleared whole: a contestant starts under the same empty one in every execution of every sitting. The instruction count is opened before the process starts, so the process inherits it, and read once it has been waited for, when the kernel has added what the process retired.
fn once(invocation: &Invocation) -> Result<Execution, String> {
    let named = invocation.program.display().to_string();

    let counted = Instructions::open();
    let started = Instant::now();
    let child = Command::new(&invocation.program)
        .args(&invocation.arguments)
        .env_clear()
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .spawn()
        .map_err(|error| format!("{named} did not start: {error}"))?;

    let produced = child
        .wait_with_output()
        .map_err(|error| format!("{named} could not be waited for: {error}"))?;
    let microseconds = u64::try_from(started.elapsed().as_micros()).unwrap_or(u64::MAX);
    let instructions = counted.as_ref().map(Instructions::read);

    match produced.status.success() {
        true => Ok(Execution {
            answer: String::from_utf8_lossy(&produced.stdout).trim().to_string(),
            microseconds,
            instructions,
        }),
        false => Err(format!("{named} failed: {}", produced.status)),
    }
}

/// Run one program to completion, its output the terminal's, failing with its status.
fn run(program: &str, arguments: &[&str], directory: &Path) -> Result<(), String> {
    let status = Command::new(program)
        .args(arguments)
        .current_dir(directory)
        .stdout(Stdio::from(io::stderr()))
        .status()
        .map_err(|error| format!("{program} did not start: {error}"))?;

    match status.success() {
        true => Ok(()),
        false => Err(format!(
            "{program} {} failed: {status}",
            arguments.join(" ")
        )),
    }
}

/// Build `workload` in both languages: Rust natively and to WebAssembly from one source, and Curios as a self-contained executable.
fn build(workload: &Workload, root: &Path) -> Result<(), String> {
    let name = workload.name;
    let source = format!("programs/{name}/{name}");
    let output = |contestant| artifact(contestant, workload, root).display().to_string();
    eprintln!(">> building {name}");

    let rust = root.join(format!("{source}.rs")).display().to_string();
    let edition = edition(root)?;
    let twin = component(workload, root).display().to_string();
    run(
        "rustc",
        &["-O", "--edition", &edition, &rust, "-o", &output("rust")],
        root,
    )?;
    run(
        "rustc",
        &[
            "-O",
            "--edition",
            &edition,
            "--target",
            TARGET,
            &rust,
            "-o",
            &twin,
        ],
        root,
    )?;
    // Compiled here, once, by the runner that loads it at every execution: what an execution spends is a load and a run.
    run(TWIN, &["precompile", &twin, &output("rust-wasm")], root)?;

    run(
        "target/release/curios",
        &["compile", &format!("{source}.crs"), "-o", &output("curios")],
        root,
    )
}

/// Whether every contestant printed `expected`, or, where nothing is expected, the same answer as the others; and which did not where they differ.
pub fn agreement(answers: &[(&str, String)], expected: Option<&str>) -> Result<(), String> {
    let Some(expected) = expected.or_else(|| answers.first().map(|(_, first)| first.as_str()))
    else {
        return Ok(());
    };

    if answers.iter().all(|(_, answer)| answer == expected) {
        return Ok(());
    }

    Err(answers
        .iter()
        .map(|(name, answer)| format!("  {name:<12} {answer}"))
        .collect::<Vec<_>>()
        .join("\n"))
}

/// Hold every contestant of `workload` to the answer it should print at `size`, before anything is timed.
fn agree(workload: &Workload, size: u64, root: &Path) -> Result<(), String> {
    eprintln!(">> checking {} at {size}", workload.name);

    let expected = match size {
        size if size == workload.check => Some(workload.answer),
        size if size == workload.size => Some(workload.anchor),
        _ => None,
    };

    let mut answers = Vec::new();
    for contestant in CONTESTANTS {
        let execution = once(&invocation(contestant.name, workload, size, root))?;
        answers.push((contestant.name, execution.answer));
    }

    agreement(&answers, expected).map_err(|answers| {
        let wanted = expected.map_or(String::new(), |expected| {
            format!(", where {expected} is right")
        });
        format!(
            "the contestants answer {} at {size} wrongly{wanted}:\n{answers}",
            workload.name
        )
    })
}

/// Time every contestant on `workload`, a group at a time and the contestants interleaved inside each, and hand back each one's executions by group.
fn time(workload: &Workload, size: u64, root: &Path) -> Result<Vec<Vec<Vec<Execution>>>, String> {
    eprintln!(">> timing {} ({}={size})", workload.name, workload.letter);

    let invocations = CONTESTANTS
        .iter()
        .map(|contestant| invocation(contestant.name, workload, size, root))
        .collect::<Vec<_>>();
    let mut samples = CONTESTANTS.iter().map(|_| Vec::new()).collect::<Vec<_>>();

    for _ in 0..GROUPS {
        let mut batch = CONTESTANTS.iter().map(|_| Vec::new()).collect::<Vec<_>>();

        for _ in 0..WARMUP {
            for invocation in &invocations {
                once(invocation)?;
            }
        }
        for _ in 0..EXECUTIONS {
            // One round over the contestants, so the three sit adjacent in time and a disturbance reaches them alike.
            for (taken, invocation) in batch.iter_mut().zip(&invocations) {
                taken.push(once(invocation)?);
            }
        }

        for (groups, taken) in samples.iter_mut().zip(batch) {
            groups.push(taken);
        }
    }

    Ok(samples)
}

/// A contestant's figures on one workload: its time, and its instructions where the kernel counted every execution of it.
struct Contested {
    time: Figure,
    instructions: Option<Figure>,
}

/// `contestant`'s figures on `workload`, read off its executions by group.
fn contested(workload: &Workload, contestant: &str, groups: &[Vec<Execution>]) -> Contested {
    let figure = |measure, readings| Figure {
        name: named(workload.name, contestant),
        measure,
        size: Some(workload.size),
        readings,
    };
    let times = groups
        .iter()
        .map(|group| {
            group
                .iter()
                .map(|execution| execution.microseconds)
                .collect()
        })
        .collect();
    let counts = groups
        .iter()
        .map(|group| {
            group
                .iter()
                .map(|execution| execution.instructions)
                .collect::<Option<Vec<_>>>()
        })
        .collect::<Option<Vec<_>>>();

    Contested {
        time: figure(Measure::Microseconds, times),
        instructions: counts.map(|counts| figure(Measure::Instructions, counts)),
    }
}

/// Build the contestants, hold them to the answers their workloads are known to have, time them, and file the sitting as the contest's next record.
///
/// The record is numbered after `filed`, the contest's records already filed, which the binary keeps, and written only once the sitting has ended, and never over a file that stands: a sitting that fails files nothing. Everything a person watches goes to standard error.
pub fn contest_sitting(filed: usize) -> Result<(), String> {
    let root = root();

    // Before anything is built: building with the wrong compiler and timing it afterwards spends the run to produce the wrong artefact.
    let subject = subject(&root)?;
    let pinned = held(&root)?;
    let platform = capture();

    fs::create_dir_all(root.join(ARTIFACTS)).map_err(|error| error.to_string())?;
    run("cargo", &["xtask", "compiler", "build"], &root)?;
    run(
        "cargo",
        &["build", "--release", "--package", "xbench", "--bin", "twin"],
        &root,
    )?;

    for workload in WORKLOADS {
        build(workload, &root)?;
    }
    for workload in WORKLOADS {
        agree(workload, workload.check, &root)?;
        agree(workload, workload.size, &root)?;
    }

    let mut figures = Vec::new();
    for workload in WORKLOADS {
        let measured = CONTESTANTS
            .iter()
            .zip(time(workload, workload.size, &root)?)
            .map(|(contestant, groups)| contested(workload, contestant.name, &groups))
            .collect::<Vec<_>>();

        let times = CONTESTANTS
            .iter()
            .zip(&measured)
            .map(|(contestant, figures)| (contestant.title, figures.time.stated()))
            .collect::<Vec<_>>();
        let counts = CONTESTANTS
            .iter()
            .zip(&measured)
            .filter_map(|(contestant, figures)| {
                Some((contestant.title, figures.instructions.as_ref()?.stated()))
            })
            .collect::<Vec<_>>();
        for (measure, rows) in [
            (Measure::Microseconds, times),
            (Measure::Instructions, counts),
        ] {
            if !rows.is_empty() {
                eprint!(
                    "\n{}",
                    table(
                        workload.title,
                        workload.letter,
                        workload.size,
                        measure,
                        &rows
                    )
                );
            }
        }

        figures.extend(
            measured
                .into_iter()
                .flat_map(|figures| [Some(figures.time), figures.instructions])
                .flatten(),
        );
    }

    let record = Record {
        taken: today(&asked("date", &["-I"], &root)?)?,
        subject,
        platform,
        pinned,
        figures,
    };

    file(&root, CONTEST, filed, &record)
}
