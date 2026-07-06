//! The `curios` CLI. Three modes: `run` compiles a `.crs` entrypoint and executes it in-process, forwarding the trailing arguments as the program's argv (input path as argv[0]) and its exit code as the process's; `check` type-checks only, falling through to the full pipeline just when `--print` requests a post-core stage that type-checking never produces; `compile` emits a self-contained native executable (the embedded launcher stub with the `.cwasm` payload appended). Argument parsing lives in `cli`, stage printing and file loading in `pipeline`, executable emission in `bundle` — this file only dispatches, mapping any error to stderr and a failure exit.

mod bundle;
use bundle::*;

mod cli;
use cli::*;

mod pipeline;
use pipeline::*;

use {
    clap::Parser,
    curios::{run_wasm, to_cwasm},
    curios_rt::{ForeignBindings, OsHost},
    std::{
        iter,
        process::{self, ExitCode},
    },
};

fn dispatch() -> Result<(), String> {
    let Cli {
        timeout,
        print,
        mode,
    } = Cli::parse();

    let print = print.unwrap_or_default();

    match mode {
        Mode::Run { input_path, args } => {
            let module = compile_file(timeout, &print, &input_path)?;

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
        Mode::Check { input_path } => {
            // Run the fast type-check-only path; fall through to the full pipeline
            // only when a post-core stage is requested for printing, since those
            // stages do not exist until lowering runs.
            if print
                .split(',')
                .any(|stage| curios_pipeline::NAMES[2..].contains(&stage))
            {
                compile_file(timeout, &print, &input_path)?;
            } else {
                typecheck_file(timeout, &print, &input_path)?;
            }
        }
        Mode::Compile {
            input_path,
            output_path,
        } => {
            let module = compile_file(timeout, &print, &input_path)?;
            let cwasm = to_cwasm(&module)?;
            let output = output_path.unwrap_or_else(|| exe_output_path(&input_path));

            emit_exe(&cwasm, &output)?;
        }
    }

    Ok(())
}

fn main() -> ExitCode {
    if let Err(error) = dispatch() {
        eprintln!("{error}");

        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}
