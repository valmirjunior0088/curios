//! The contest: every workload of the corpus built three ways, held to the answers it is known to have, and timed.
//!
//! **Interleaved, not batched.** A group runs the contestants round-robin rather than one contestant to exhaustion, so a disturbance during a group — a compile elsewhere on the machine, a thermal ramp, a page cache warming — reaches all three alike and the ratio between them survives it. Between the groups is the span a difference must clear; inside one is iteration noise.
//!
//! **No shell, and a cleared environment.** Each contestant is spawned directly, so a figure is one process rather than a shell's and the contestant's, and the environment is the same small thing every time: the size of an inherited environment shifts a stack and moves a measurement without anything looking wrong.

#[cfg(test)]
mod tests;

use {
    crate::{Pin, Reading, Size, Timed, capture, held, table},
    std::{
        env, fs, io,
        path::{Path, PathBuf},
        process::{Command, Stdio},
        time::Instant,
    },
    toml::value::Datetime,
};

/// One workload of the measurement corpus, `programs/<name>/`, which carries the same program in Curios and in Rust. What each computes is `programs/README.md`'s.
#[derive(Debug)]
pub(crate) struct Workload {
    pub(crate) name: &'static str,
    /// How a table names it.
    pub(crate) title: &'static str,
    /// The letter its size is reported under.
    pub(crate) letter: &'static str,
    /// The size it is timed at.
    pub(crate) size: u64,
    /// The small size every contestant is first held to one answer at.
    pub(crate) check: u64,
    /// What every contestant prints at `check`.
    pub(crate) answer: &'static str,
    /// What every contestant prints at `size`: the corpus's anchor.
    pub(crate) anchor: &'static str,
}

/// The corpus's workloads, in the order their tables are printed.
pub(crate) const WORKLOADS: &[Workload] = &[
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
pub(crate) struct Contestant {
    pub(crate) name: &'static str,
    /// How a table names it.
    pub(crate) title: &'static str,
}

/// Curios, and the one anchor, compiled two ways.
///
/// Rust native is the ceiling, and Rust native against Rust to WebAssembly is what WebAssembly itself costs, so what stands between Curios and `rust-wasm` is Curios's. On a workload that allocates, `rust-wasm` manages linear memory and is a *bound* rather than a peer: the gap holds Curios's codegen and the whole cost of delegating collection, and no contestant separates them.
pub(crate) const CONTESTANTS: &[Contestant] = &[
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

/// Where the built contestants go: under the workspace's own build directory, so a reading leaves nothing in the corpus.
const ARTIFACTS: &str = "target/xbench";

/// The WebAssembly target the Rust contestant is built for. The workspace's own toolchain already carries it, so the bench installs nothing.
const TARGET: &str = "wasm32-wasip2";

/// How many independent batches a contestant is measured in. Between their medians is the span; two would make that span a single difference rather than an estimate of one.
const GROUPS: usize = 5;

/// How many timed rounds a group runs.
const EXECUTIONS: usize = 5;

/// How many untimed rounds open a group, enough to page the binary in and warm the file cache. Both contestants are compiled ahead of time, so there is no engine to settle.
const WARMUP: usize = 1;

/// The workspace root, which is the crate's parent.
fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("the crate sits in the workspace's root")
        .to_path_buf()
}

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
        _ => format!("{built}_rust.wasm"),
    })
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
        // The module is not the program: the engine on the path runs it, and the pin holds it to the one Curios embeds.
        _ => Invocation {
            program: PathBuf::from("wasmtime"),
            arguments: vec![
                "run".to_string(),
                artifact.display().to_string(),
                size.to_string(),
            ],
        },
    }
}

/// Run one contestant once: what it printed, and how long the whole process took, in whole microseconds.
///
/// `PATH` is the one variable that survives the clearing, because the engine is found on it; everything else is dropped so that the environment a contestant starts under is the same small thing in every execution of every reading.
fn once(invocation: &Invocation) -> Result<(String, u64), String> {
    let path = env::var("PATH").unwrap_or_default();
    let named = invocation.program.display().to_string();

    let started = Instant::now();
    let child = Command::new(&invocation.program)
        .args(&invocation.arguments)
        .env_clear()
        .env("PATH", path)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .spawn()
        .map_err(|error| format!("{named} did not start: {error}"))?;

    let produced = child
        .wait_with_output()
        .map_err(|error| format!("{named} could not be waited for: {error}"))?;
    let took = u64::try_from(started.elapsed().as_micros()).unwrap_or(u64::MAX);

    match produced.status.success() {
        true => Ok((
            String::from_utf8_lossy(&produced.stdout).trim().to_string(),
            took,
        )),
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

/// What a program says of itself, read rather than shown.
fn asked(program: &str, arguments: &[&str], directory: &Path) -> Result<String, String> {
    let produced = Command::new(program)
        .args(arguments)
        .current_dir(directory)
        .output()
        .map_err(|error| format!("{program} did not start: {error}"))?;

    match produced.status.success() {
        true => Ok(String::from_utf8_lossy(&produced.stdout).trim().to_string()),
        false => Err(format!("{program} {} failed", arguments.join(" "))),
    }
}

/// Build `workload` in both languages: Rust natively and to WebAssembly from one source, and Curios as a self-contained executable.
fn build(workload: &Workload, root: &Path) -> Result<(), String> {
    let name = workload.name;
    let source = format!("programs/{name}/{name}");
    let output = |contestant| artifact(contestant, workload, root).display().to_string();
    eprintln!(">> building {name}");

    let rust = root.join(format!("{source}.rs")).display().to_string();
    run("rustc", &["-O", &rust, "-o", &output("rust")], root)?;
    run(
        "rustc",
        &["-O", "--target", TARGET, &rust, "-o", &output("rust-wasm")],
        root,
    )?;

    run(
        "target/release/curios",
        &["compile", &format!("{source}.crs"), "-o", &output("curios")],
        root,
    )
}

/// Whether every contestant printed `expected`, or, where nothing is expected, the same answer as the others; and which did not where they differ.
pub(crate) fn agreement(answers: &[(&str, String)], expected: Option<&str>) -> Result<(), String> {
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
        let (answer, _) = once(&invocation(contestant.name, workload, size, root))?;
        answers.push((contestant.name, answer));
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

/// Time every contestant on `workload`, a group at a time and the contestants interleaved inside each, and hand back each one's samples by group.
fn time(workload: &Workload, size: u64, root: &Path) -> Result<Vec<Vec<Vec<u64>>>, String> {
    eprintln!(">> timing {} ({}={size})", workload.name, workload.letter);

    let invocations = CONTESTANTS
        .iter()
        .map(|contestant| invocation(contestant.name, workload, size, root))
        .collect::<Vec<_>>();
    let mut samples = vec![Vec::new(); CONTESTANTS.len()];

    for _ in 0..GROUPS {
        let mut batch = vec![Vec::new(); CONTESTANTS.len()];

        for _ in 0..WARMUP {
            for invocation in &invocations {
                once(invocation)?;
            }
        }
        for _ in 0..EXECUTIONS {
            // One round over the contestants, so the three sit adjacent in time and a disturbance reaches them alike.
            for (taken, invocation) in batch.iter_mut().zip(&invocations) {
                let (_, took) = once(invocation)?;
                taken.push(took);
            }
        }

        for (groups, taken) in samples.iter_mut().zip(batch) {
            groups.push(taken);
        }
    }

    Ok(samples)
}

/// Build the contestants, hold them to the answers their workloads are known to have, time them, and print the reading as the TOML file that records it.
///
/// The reading is numbered after `filed`, the readings already filed, which the binary keeps. Standard output carries the reading alone, so a redirect files the capture and nothing else; everything a person watches goes to standard error, the name to file it under among it.
pub(crate) fn collect(filed: usize) -> Result<(), String> {
    let root = root();

    // Before anything is built: building with the wrong compiler and timing it afterwards spends the run to produce the wrong artefact.
    let pinned = held(&root)?;
    let platform = capture();
    let commit = asked("git", &["rev-parse", "--short", "HEAD"], &root)?;

    fs::create_dir_all(root.join(ARTIFACTS)).map_err(|error| error.to_string())?;
    run("cargo", &["xtask", "compiler", "build"], &root)?;

    let subject = format!(
        "{} ({commit})",
        asked("target/release/curios", &["--version"], &root)?
    );

    for workload in WORKLOADS {
        build(workload, &root)?;
    }
    for workload in WORKLOADS {
        agree(workload, workload.check, &root)?;
        agree(workload, workload.size, &root)?;
    }

    let mut timed = Vec::new();
    for workload in WORKLOADS {
        let measured = CONTESTANTS
            .iter()
            .zip(time(workload, workload.size, &root)?)
            .map(|(contestant, microseconds)| Timed {
                workload: workload.name.to_string(),
                contestant: contestant.name.to_string(),
                microseconds,
            })
            .collect::<Vec<_>>();

        let rows = CONTESTANTS
            .iter()
            .zip(&measured)
            .map(|(contestant, timed)| (contestant.title, timed.milliseconds()))
            .collect::<Vec<_>>();
        eprint!(
            "\n{}",
            table(workload.title, workload.letter, workload.size, &rows)
        );

        timed.extend(measured);
    }

    let reading = Reading {
        taken: today(&asked("date", &["-I"], &root)?)?,
        subject,
        platform,
        pinned: pinned
            .into_iter()
            .map(|(tool, version)| Pin {
                tool: tool.to_string(),
                version,
            })
            .collect(),
        sizes: WORKLOADS
            .iter()
            .map(|workload| Size {
                workload: workload.name.to_string(),
                size: workload.size,
            })
            .collect(),
        timed,
    };

    print!("{}", reading.filed()?);
    eprintln!(">> file it as xbench/readings/{filed:02}.toml");

    Ok(())
}

/// The day `date -I` answers, as a reading records it.
fn today(said: &str) -> Result<toml::value::Date, String> {
    match said.parse::<Datetime>() {
        Ok(Datetime {
            date: Some(date),
            time: None,
            offset: None,
        }) => Ok(date),
        _ => Err(format!("`date -I` answered {said:?}, which is no day")),
    }
}
