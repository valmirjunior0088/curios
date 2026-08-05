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
    std::{
        iter,
        process::{self, ExitCode},
    },
};

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
            let module = compile_file(budget, &print, &input_path)?;
            let cwasm = to_cwasm(&module)?;
            let output = output_path.unwrap_or_else(|| exe_output_path(&input_path));

            emit_exe(&cwasm, &output)?;
        }
        #[cfg(feature = "profile")]
        Mode::Profile { input_path } => {
            let (entrypoint, loader) = curios::load(&input_path)?;
            let (compilation, report) = curios_profile::capture(|| {
                curios_pipeline::compile_entrypoint(budget, &entrypoint, loader, |_| {})
            });

            println!("total_ms\tcalls\tmin_ms\tmax_ms\ttarget\tname");
            for summary in report.summaries() {
                println!(
                    "{:.3}\t{}\t{:.3}\t{:.3}\t{}\t{}",
                    summary.total().as_secs_f64() * 1_000.0,
                    summary.calls(),
                    summary.min().as_secs_f64() * 1_000.0,
                    summary.max().as_secs_f64() * 1_000.0,
                    summary.target(),
                    summary.name(),
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
