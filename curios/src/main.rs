//! The `curios` CLI. `run` compiles an entrypoint and executes it in-process, forwarding the trailing arguments as the program's argv (the entry path as argv[0]) and its exit code as the process's; `compile` emits a self-contained native executable (the embedded launcher stub with the `.cwasm` payload appended). Both take the same target three ways — no argument for the governing package's default executable, an identifier for one it declares, and a path for a bare `.crs` file — because what a bare invocation means inside a package should not depend on which subcommand asked. Argument parsing lives in `cli`, target resolution and stage printing in `pipeline`, executable emission in `bundle` — this file only dispatches, mapping any error to stderr and a failure exit.

mod bundle;
use bundle::*;

mod cli;
use cli::*;

mod pipeline;
use pipeline::*;

mod report;
use report::*;

use {
    clap::Parser,
    curios::{to_cwasm, to_cwasm_dumped},
    curios_package::{Governing, Target, curate, scaffold},
    curios_pipeline::CompileError,
    curios_runtime::{ForeignBindings, OsHost, run_bytes},
    curios_text::Formatted,
    std::{
        fs, iter,
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
            CompileError::Incomplete(message) => Failure::Incomplete(message),
            CompileError::Failure(message) => Failure::Error(message),
        }
    }
}

fn dispatch() -> Result<(), Failure> {
    let Cli {
        budget,
        print,
        units,
        manifest,
        mode,
    } = Cli::parse();

    let print = print.unwrap_or_default();

    match mode {
        Mode::Run { target, args } => {
            let target = Target::here(target.as_deref(), manifest.as_deref())?;
            let entry = target.entry().to_path_buf();
            let subject = subject_of(&target);
            let module = compile_target(budget, &print, &units, target)?;

            // Chosen before optimizing rather than filtered after, unlike the driver's stages: this one's payload costs — Binaryen's text capture, and the name section riding into the artifact.
            let cwasm = match print.split(',').any(|name| name == "wasm-optm") {
                true => to_cwasm_dumped(&module, stage_printer(&print)?)?,
                false => to_cwasm(&module)?,
            };

            fact(Heading::Running, &subject);

            let code = run_bytes(
                &cwasm,
                OsHost::with_args(
                    iter::once(entry.to_string_lossy().into_owned().into_bytes())
                        .chain(args.into_iter().map(String::into_bytes))
                        .collect(),
                ),
                ForeignBindings::empty(),
            )?;

            if code != 0 {
                process::exit(code);
            }
        }
        Mode::Compile {
            target,
            output_path,
        } => {
            let target = Target::here(target.as_deref(), manifest.as_deref())?;
            let entry = target.entry().to_path_buf();
            let output = output_path.unwrap_or_else(|| target.output());

            // Nothing enforces a `.crs` extension, so an extensionless input's default output is the input itself — and `-o` can name it explicitly. Refuse before compiling rather than destroy the source.
            if let (Ok(input), Ok(written)) = (entry.canonicalize(), output.canonicalize())
                && input == written
            {
                return Err(Failure::Error(format!(
                    "refusing to overwrite the input {}",
                    entry.display()
                )));
            }

            let started = Instant::now();
            let module = compile_target(budget, &print, &units, target)?;

            // Chosen before optimizing rather than filtered after, unlike the driver's stages: this one's payload costs — Binaryen's text capture, and the name section riding into the artifact.
            let cwasm = match print.split(',').any(|name| name == "wasm-optm") {
                true => to_cwasm_dumped(&module, stage_printer(&print)?)?,
                false => to_cwasm(&module)?,
            };

            emit_exe(&cwasm, &output)?;

            // Where it landed rather than what it was called: that is the one fact a finished build is read for, and the group above already named the target twice.
            let mut line = Line::open(Heading::Finished, &Subject::File(output));
            line.outcome(&format!("done {:.1}s", started.elapsed().as_secs_f64()));
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

            for acquisition in curate(&governing)? {
                fact(Heading::Fetching, Subject::package(&acquisition.name));
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
        #[cfg(feature = "profile")]
        Mode::Profile { input_path } => {
            let scope = load_units(&units)?;
            let subject = Subject::File(input_path.clone());
            let (compilation, report) =
                capture(|| compile_entry(budget, &print, scope, &input_path, &subject, None));

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
