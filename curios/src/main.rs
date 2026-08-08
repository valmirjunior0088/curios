//! The `curios` CLI. Two modes: `run` compiles a `.crs` entrypoint and executes it in-process, forwarding the trailing arguments as the program's argv (input path as argv[0]) and its exit code as the process's; `compile` emits a self-contained native executable (the embedded launcher stub with the `.cwasm` payload appended). Argument parsing lives in `cli`, stage printing and file loading in `pipeline`, executable emission in `bundle` — this file only dispatches, mapping any error to stderr and a failure exit.

mod bundle;
use bundle::*;

mod cli;
use cli::*;

mod pipeline;
use pipeline::*;

use {
    clap::Parser,
    curios::{run_wasm, to_cwasm},
    curios_pipeline::CompileError,
    curios_runtime::{ForeignBindings, OsHost},
    curios_text::Formatted,
    std::{
        fs, iter,
        process::{self, ExitCode},
    },
};

#[cfg(feature = "profile")]
use {curios::load, curios_pipeline::compile_entrypoint, curios_profile::capture};

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
        mode,
    } = Cli::parse();

    let print = print.unwrap_or_default();

    match mode {
        Mode::Run { input_path, args } => {
            let module = compile_file(budget, &print, &input_path)?;

            let code = run_wasm(
                &module,
                OsHost::with_args(
                    iter::once(input_path.to_string_lossy().into_owned().into_bytes())
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
            input_path,
            output_path,
        } => {
            let output = output_path.unwrap_or_else(|| exe_output_path(&input_path));

            // Nothing enforces a `.crs` extension, so an extensionless input's default output is the input itself — and `-o` can name it explicitly. Refuse before compiling rather than destroy the source.
            if let (Ok(input), Ok(target)) = (input_path.canonicalize(), output.canonicalize())
                && input == target
            {
                return Err(Failure::Error(format!(
                    "refusing to overwrite the input {}",
                    input_path.display()
                )));
            }

            let module = compile_file(budget, &print, &input_path)?;
            let cwasm = to_cwasm(&module)?;

            emit_exe(&cwasm, &output)?;
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
            let (entrypoint, loader) = load(&input_path)?;
            let (compilation, report) =
                capture(|| compile_entrypoint(budget, &entrypoint, loader, |_| {}));

            println!("total_ms\tcalls\tmin_ms\tmax_ms\ttarget\tname");
            for summary in &report.summaries {
                println!(
                    "{:.3}\t{}\t{:.3}\t{:.3}\t{}\t{}",
                    summary.total.as_secs_f64() * 1_000.0,
                    summary.calls,
                    summary.min.as_secs_f64() * 1_000.0,
                    summary.max.as_secs_f64() * 1_000.0,
                    summary.target,
                    summary.name,
                );
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
