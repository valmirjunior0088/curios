//! The contest: every workload of the corpus built three ways, held to the answers it is known to have, and timed.
//!
//! **Interleaved, not batched.** A group runs the contestants round-robin rather than one contestant to exhaustion, so a disturbance during a group — a compile elsewhere on the machine, a thermal ramp, a page cache warming — reaches all three alike and the ratio between them survives it. Between the groups is the span a difference must clear; inside one is iteration noise.
//!
//! **No shell, and a cleared environment.** Each contestant is spawned directly, so a figure is one process rather than a shell's and the contestant's, and the environment is the same small thing every time: the size of an inherited environment shifts a stack and moves a measurement without anything looking wrong.

#[cfg(test)]
mod tests;

use {
    crate::{Captured, capture, held, table},
    std::{
        env, fs,
        io::{self, Write},
        path::{Path, PathBuf},
        process::{Command, Stdio},
        time::Instant,
    },
};

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
        size: 100_000_000,
        check: 8,
        answer: "9345",
        anchor: "17662",
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
        size: 1600,
        check: 8,
        answer: "819185",
        anchor: "457407",
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
        size: 75_000,
        check: 8,
        answer: "28",
        anchor: "675283",
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
        title: "Rust",
    },
    Contestant {
        name: "rust-wasm",
        title: "Rust → wasm",
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

/// How a contestant is run: a program, its arguments, and the size where it is read from standard input rather than the command line.
#[derive(Debug)]
struct Invocation {
    program: PathBuf,
    arguments: Vec<String>,
    input: Option<String>,
}

/// How `contestant` runs `workload` at `size`.
///
/// Curios reads its size on standard input, which is its proven input path; Rust reads argv. The same spelling is what the agreement check runs and what is timed, so the two cannot run different things.
/// Where the bench's build of `contestant` on `workload` lands. The one place the layout is spelled, so what is run and what is weighed cannot name different files.
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

fn invocation(contestant: &str, workload: &Workload, size: u64, root: &Path) -> Invocation {
    let artifact = artifact(contestant, workload, root);

    match contestant {
        "curios" => Invocation {
            program: artifact,
            arguments: Vec::new(),
            input: Some(format!("{size}\n")),
        },
        "rust" => Invocation {
            program: artifact,
            arguments: vec![size.to_string()],
            input: None,
        },
        // The module is not the program: the engine on the path runs it, and the pin holds it to the one Curios embeds.
        _ => Invocation {
            program: PathBuf::from("wasmtime"),
            arguments: vec![
                "run".to_string(),
                artifact.display().to_string(),
                size.to_string(),
            ],
            input: None,
        },
    }
}

/// What each contestant's build of `workload` weighs.
fn weigh(
    workload: &Workload,
    root: &Path,
) -> Result<Vec<(&'static str, &'static str, u64)>, String> {
    CONTESTANTS
        .iter()
        .map(|contestant| {
            let artifact = artifact(contestant.name, workload, root);
            let weighed = fs::metadata(&artifact)
                .map_err(|error| format!("{} cannot be weighed: {error}", artifact.display()))?;

            Ok((workload.name, contestant.name, weighed.len()))
        })
        .collect()
}

/// Run one contestant once: what it printed, and how long the whole process took, in milliseconds.
///
/// `PATH` is the one variable that survives the clearing, because the engine is found on it; everything else is dropped so that the environment a contestant starts under is the same small thing in every execution of every reading.
fn once(invocation: &Invocation) -> Result<(String, f64), String> {
    let path = env::var("PATH").unwrap_or_default();
    let named = invocation.program.display().to_string();

    let started = Instant::now();
    let mut child = Command::new(&invocation.program)
        .args(&invocation.arguments)
        .env_clear()
        .env("PATH", path)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .spawn()
        .map_err(|error| format!("{named} did not start: {error}"))?;

    {
        let mut standard_input = child
            .stdin
            .take()
            .ok_or_else(|| format!("{named} took no standard input"))?;

        if let Some(input) = &invocation.input {
            standard_input
                .write_all(input.as_bytes())
                .map_err(|error| format!("{named} would not read its size: {error}"))?;
        }
    }

    let produced = child
        .wait_with_output()
        .map_err(|error| format!("{named} could not be waited for: {error}"))?;
    let took = started.elapsed().as_secs_f64() * 1000.0;

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
    let built = format!("{ARTIFACTS}/{name}");
    eprintln!(">> building {name}");

    let rust = root.join(format!("{source}.rs")).display().to_string();
    let rust_built = root.join(format!("{built}_rust")).display().to_string();
    run("rustc", &["-O", &rust, "-o", &rust_built], root)?;
    run(
        "rustc",
        &[
            "-O",
            "--target",
            TARGET,
            &rust,
            "-o",
            &format!("{rust_built}.wasm"),
        ],
        root,
    )?;

    run(
        "target/release/curios",
        &[
            "compile",
            &format!("{source}.crs"),
            "-o",
            &format!("{built}_curios"),
        ],
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
fn time(workload: &Workload, size: u64, root: &Path) -> Result<Vec<Vec<Vec<f64>>>, String> {
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

/// A reading as the module that records it, ready to be filed under `src/readings/` and listed in `readings.rs`.
/// Everything one sitting captured, which is the subject `recorded` writes out. It travels as one value because it is one thing: what a reading will say.
#[derive(Debug)]
struct Sitting {
    taken: String,
    subject: String,
    platform: Captured,
    pinned: Vec<(&'static str, String)>,
    sizes: Vec<(&'static str, u64)>,
    built: Vec<(&'static str, &'static str, u64)>,
    timed: Vec<(&'static str, &'static str, Vec<Vec<f64>>)>,
}

fn recorded(number: usize, sitting: &Sitting) -> String {
    let Sitting {
        taken,
        subject,
        platform,
        ..
    } = sitting;
    // `date -I` zero-pads, and `03` is a zero-prefixed decimal literal, which does not compile.
    let unpadded = |field: &str| field.trim_start_matches('0').to_string();
    let (year, month, day) = (
        &taken[0..4],
        unpadded(&taken[5..7]),
        unpadded(&taken[8..10]),
    );
    let mut module = format!(
        "//! Reading {number:02}, taken {taken}.\n\nuse super::*;\n\npub(super) const READING_{number:02}: Reading = Reading {{\n    taken: Date::new({year}, {month}, {day}),\n    subject: {subject:?},\n    platform: Platform {{\n        machine: {:?},\n        state: {:?},\n        software: {:?},\n    }},\n    pinned: &[\n",
        platform.machine, platform.state, platform.software
    );

    for (tool, version) in &sitting.pinned {
        module.push_str(&format!("        ({tool:?}, {version:?}),\n"));
    }
    module.push_str("    ],\n    sizes: &[\n");
    for (workload, size) in &sitting.sizes {
        module.push_str(&format!("        ({workload:?}, {size}),\n"));
    }
    module.push_str("    ],\n    built: &[\n");
    for (workload, contestant, bytes) in &sitting.built {
        module.push_str(&format!(
            "        weighed({workload:?}, {contestant:?}, {bytes}),\n"
        ));
    }
    module.push_str("    ],\n    timed: &[\n");

    for (workload, contestant, groups) in &sitting.timed {
        module.push_str(&format!("        took({workload:?}, {contestant:?}, &[\n"));
        for group in groups {
            let group = group
                .iter()
                .map(|took| format!("{took:.3}"))
                .collect::<Vec<_>>()
                .join(", ");
            module.push_str(&format!("            &[{group}],\n"));
        }
        module.push_str("        ]),\n");
    }

    module.push_str("    ],\n};\n");
    module
}

/// Build the contestants, hold them to the answers their workloads are known to have, time them, and print the reading as the module that records it.
///
/// The reading is numbered after `taken`, the readings already filed, which the binary keeps. Standard output carries the module alone, so a redirect files the capture and nothing else; everything a person watches goes to standard error.
pub fn collect(taken: usize) -> Result<(), String> {
    let root = root();

    // Before anything is built: building with the wrong compiler and timing it afterwards spends the run to produce the wrong artefact.
    let pinned = held(&root)?;
    let platform = capture();
    let commit = asked("git", &["rev-parse", "--short", "HEAD"], &root)?;

    fs::create_dir_all(root.join(ARTIFACTS)).map_err(|error| error.to_string())?;
    run("cargo", &["xtask", "build"], &root)?;

    let subject = format!(
        "{} ({commit})",
        asked("target/release/curios", &["--version"], &root)?
    );

    let mut built = Vec::new();
    for workload in WORKLOADS {
        build(workload, &root)?;
        built.extend(weigh(workload, &root)?);
    }
    for workload in WORKLOADS {
        agree(workload, workload.check, &root)?;
        agree(workload, workload.size, &root)?;
    }

    let mut timed = Vec::new();
    for workload in WORKLOADS {
        let samples = time(workload, workload.size, &root)?;

        let borrowed = samples
            .iter()
            .map(|groups| groups.iter().map(Vec::as_slice).collect::<Vec<_>>())
            .collect::<Vec<_>>();
        let rows = CONTESTANTS
            .iter()
            .zip(&borrowed)
            .map(|(contestant, groups)| (contestant.title, groups.as_slice()))
            .collect::<Vec<_>>();
        eprint!(
            "\n{}",
            table(workload.title, workload.letter, workload.size, &rows)
        );

        for (contestant, groups) in CONTESTANTS.iter().zip(samples) {
            timed.push((workload.name, contestant.name, groups));
        }
    }

    let sizes = WORKLOADS
        .iter()
        .map(|workload| (workload.name, workload.size))
        .collect::<Vec<_>>();

    let sitting = Sitting {
        taken: asked("date", &["-I"], &root)?,
        subject,
        platform,
        pinned,
        sizes,
        built,
        timed,
    };

    print!("{}", recorded(taken, &sitting));

    Ok(())
}
