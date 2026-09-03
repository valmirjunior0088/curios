//! The `curios` CLI. `run` obtains an entrypoint's precompiled payload and executes it in-process, forwarding the trailing arguments as the program's argv (the entry path, or `-`, as argv[0]) and its exit code as the process's; `compile` appends that same payload to the embedded launcher stub and writes a self-contained native executable. Both take the same target four ways — no argument for the governing package's default executable, an identifier for one it declares, a path for a bare `.crs` file, and `-` for the program on standard input — because what a bare invocation means inside a package should not depend on which subcommand asked.
//!
//! **Neither subcommand compiles unconditionally.** A manifest target's payload is filed in the project's store, so an invocation whose entry, whose entry's modules and whose dependencies are all unchanged is served from it — one slot for both subcommands, which is what makes `compile` after `run` a file write. Neither standalone form has a project, so neither consults anything. `pipeline` owns that decision and everything downstream of it.
//!
//! Argument parsing lives in `cli`, target resolution, compilation and payload reuse in `pipeline`, executable emission in `bundle` — this file only dispatches, mapping any error to stderr and a failure exit.

mod bundle;
use bundle::*;

mod cli;
use cli::*;

mod pipeline;
use pipeline::*;

mod report;
use report::*;

mod test_runner;
use test_runner::*;

use {
    clap::Parser,
    curios::{serve, wonder_diagnostics, wonder_stage, wonder_tests},
    curios_package::{Governing, Target, curate, scaffold},
    curios_pipeline::CompileError,
    curios_runtime::{ForeignBindings, OsHost, run_bytes},
    curios_text::Formatted,
    std::{
        ffi::OsString,
        fs, iter,
        path::Path,
        process::{self, ExitCode},
        time::Instant,
    },
};

#[cfg(feature = "profile")]
use curios_profile::capture;

// Only the `profile` build installs it, so the ordinary CLI keeps the system allocator untouched and pays nothing for counters no mode would read.
#[cfg(feature = "profile")]
#[global_allocator]
static ALLOCATOR: curios_profile::CountingAllocator = curios_profile::CountingAllocator;

/// The process-level failure split: a written-goal batch is incomplete development state (exit 2), everything else a hard error (exit 1). A running program's own exit code passes through untouched, so 0 always means "compiled, ran, and exited 0".
enum Failure {
    Incomplete(String),
    Error(String),
}

impl From<String> for Failure {
    fn from(message: String) -> Self {
        Failure::Error(message)
    }
}

impl From<CompileError> for Failure {
    fn from(error: CompileError) -> Self {
        match error {
            CompileError::Incomplete(_) => Failure::Incomplete(error.to_string()),
            CompileError::Failure(_) => Failure::Error(error.to_string()),
        }
    }
}

fn dispatch() -> Result<(), Failure> {
    let Cli {
        budget,
        units,
        manifest,
        mode,
    } = Cli::parse();

    match mode {
        Mode::Run { target, args } => {
            let target = Target::here(target.as_deref(), manifest.as_deref())?;
            // argv[0] is how the program was invoked, so a program on standard input passes on the `-` that invoked it rather than the name the compiler reports it by. Every argument crosses as the bytes the OS holds, since `/std/proc/args` promises opaque byte strings and a path or an argument need not be UTF-8.
            let entry = target.entry().map_or_else(
                || Target::STDIN.as_bytes().to_vec(),
                |path| path.as_os_str().as_encoded_bytes().to_vec(),
            );
            let subject = subject_of(&target);
            let cwasm = payload_of(budget, &units, target)?;

            step(Heading::Running, &subject);

            let code = run_bytes(
                &cwasm,
                OsHost::with_args(
                    iter::once(entry)
                        .chain(args.into_iter().map(OsString::into_encoded_bytes))
                        .collect(),
                ),
                ForeignBindings::empty(),
            )?;

            if code != 0 {
                process::exit(code);
            }
        }
        // Exit 1 on any failing, trapping or exiting test, exactly as a failing compile exits 1 and a goal batch exits 2 — 0 means every selected test passed or proved.
        Mode::Test { filter } => {
            if !run_tests(budget, &units, manifest.as_deref(), filter.as_deref())? {
                process::exit(1);
            }
        }
        Mode::Compile {
            target,
            output_path,
        } => {
            let target = Target::here(target.as_deref(), manifest.as_deref())?;
            let entry = target.entry().map(Path::to_path_buf);

            // An anonymous program has no stem to name its executable after, and inventing one would silently claim a path the invocation never mentioned. Refuse instead, naming the flag that answers it.
            let Some(output) = output_path.or_else(|| target.output()) else {
                return Err(Failure::Error(format!(
                    "compiling {} produces an executable with no name to take; pass `-o PATH`",
                    Subject::Stdin
                )));
            };

            // Nothing enforces a `.crs` extension, so an extensionless input's default output is the input itself — and `-o` can name it explicitly. Refuse before compiling rather than destroy the source.
            if let Some(entry) = &entry
                && let (Ok(input), Ok(written)) = (entry.canonicalize(), output.canonicalize())
                && input == written
            {
                return Err(Failure::Error(format!(
                    "refusing to overwrite the input {}",
                    entry.display()
                )));
            }

            let started = Instant::now();
            let cwasm = payload_of(budget, &units, target)?;

            emit_exe(&cwasm, &output)?;

            // Where it landed rather than what it was called: that is the one fact a finished build is read for, and the group above already named the target twice. The time is the whole invocation's, payload and emission both, so it is measured here rather than by the line.
            let mut line = Line::open(Heading::Finished, &Subject::File(output));
            line.outcome(&format!("{:.1}s", started.elapsed().as_secs_f64()));
            eprintln!();
        }
        Mode::New { directory } => {
            for written in scaffold(&directory)? {
                // A trailing separator is what tells a reader the first line is the directory the other two landed in.
                match written.is_dir() {
                    true => fact(Heading::Created, format!("{}/", written.display())),
                    false => fact(Heading::Created, written.display()),
                }
            }

            fact(
                Heading::Try,
                format!("cd {} && curios run", directory.display()),
            );
        }
        Mode::Curate => {
            let governing = Governing::here(manifest.as_deref())?;

            // Past tense because it is: every round has fetched before the acquisitions come back to be reported.
            for acquisition in curate(&governing)? {
                fact(Heading::Fetched, Subject::package(&acquisition.name));
            }
        }
        Mode::Format { paths, check } => {
            // The formatter is pure and reports changedness in its result; whether a `Changed` verdict fails the run (`--check`) or rewrites the file is this loop's policy. The formatter refuses internally when its output would not reparse to the same program, so nothing corrupt is ever written.
            let mut dirty = Vec::new();
            for path in &paths {
                match Formatted::from_path(path)? {
                    Formatted::Unchanged(_) => {}
                    Formatted::Changed(text) => match check {
                        true => dirty.push(path.display().to_string()),
                        false => fs::write(path, text)
                            .map_err(|error| format!("{}: {error}", path.display()))?,
                    },
                }
            }
            if !dirty.is_empty() {
                return Err(Failure::Error(format!(
                    "would reformat: {}",
                    dirty.join(", ")
                )));
            }
        }
        Mode::Wonder { query } => match query {
            Query::Diagnostics { target } => {
                wonder_diagnostics(budget, &units, manifest.as_deref(), target.as_deref())?
            }
            Query::Tests { target } => {
                wonder_tests(budget, &units, manifest.as_deref(), target.as_deref())?
            }
            Query::Stage { name, target } => wonder_stage(
                budget,
                &units,
                manifest.as_deref(),
                &name,
                target.as_deref(),
            )?,
            Query::Server => serve(budget, &units, manifest.as_deref())?,
        },
        #[cfg(feature = "profile")]
        Mode::Profile { input_path } => {
            let scope = load_units(&units)?;
            let subject = Subject::File(input_path.clone());
            let (compilation, report) =
                capture(|| compile_file(budget, scope, &input_path, &subject));

            println!(
                "total_ms\tcalls\tmin_ms\tmax_ms\tretained_mb\tallocated_mb\tallocs\ttarget\tname\t(peak {:.1} MiB)",
                report.peak as f64 / (1024.0 * 1024.0),
            );
            for summary in &report.summaries {
                println!(
                    "{:.3}\t{}\t{:.3}\t{:.3}\t{:.1}\t{:.1}\t{}\t{}\t{}\t{}",
                    summary.total.as_secs_f64() * 1_000.0,
                    summary.calls,
                    summary.min.as_secs_f64() * 1_000.0,
                    summary.max.as_secs_f64() * 1_000.0,
                    summary.retained as f64 / (1024.0 * 1024.0),
                    summary.allocated as f64 / (1024.0 * 1024.0),
                    summary.allocations,
                    summary.target,
                    summary.name,
                    summary.group.as_deref().unwrap_or(""),
                );
            }

            if !report.samples.is_empty() {
                println!();
                println!("count\ttotal\tmin\tmean\tmax\ttarget\tname");
                for sample in &report.samples {
                    println!(
                        "{}\t{}\t{}\t{:.1}\t{}\t{}\t{}",
                        sample.count,
                        sample.total,
                        sample.min,
                        sample.mean(),
                        sample.max,
                        sample.target,
                        sample.name,
                    );
                }
            }

            compilation.map(|_| ())?;
        }
    }

    Ok(())
}

fn main() -> ExitCode {
    match dispatch() {
        Ok(()) => ExitCode::SUCCESS,
        Err(Failure::Incomplete(report)) => {
            eprintln!("{report}");

            ExitCode::from(2)
        }
        Err(Failure::Error(error)) => {
            eprintln!("{error}");

            ExitCode::FAILURE
        }
    }
}
