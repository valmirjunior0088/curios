mod bundle;
mod cli;
mod pipeline;

use {
    clap::Parser,
    cli::{Cli, Mode},
    curios_compiler::{run_wasm, to_cwasm},
    curios_runtime::OsHost,
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
            let module = pipeline::compile_file(timeout, &print, &input_path)?;

            let code = run_wasm(
                &module,
                OsHost::with_args(
                    iter::once(input_path.to_string_lossy().into_owned().into_bytes())
                        .chain(args.into_iter().map(String::into_bytes))
                        .collect(),
                ),
            )?;

            if code != 0 {
                process::exit(code);
            }
        }
        Mode::Check { input_path } => {
            // Run the fast type-check-only path; fall through to the full pipeline
            // only when a post-core stage is requested for printing, since those
            // stages do not exist until lowering runs.
            let post_core = ["ersd", "ersd-optm", "cont", "cont-optm", "wasm"];

            if print.split(',').any(|stage| post_core.contains(&stage)) {
                pipeline::compile_file(timeout, &print, &input_path)?;
            } else {
                pipeline::typecheck_file(timeout, &print, &input_path)?;
            }
        }
        Mode::Compile {
            input_path,
            output_path,
        } => {
            let module = pipeline::compile_file(timeout, &print, &input_path)?;
            let cwasm = to_cwasm(&module)?;
            let output = output_path.unwrap_or_else(|| bundle::exe_output_path(&input_path));

            bundle::emit_exe(&cwasm, &output)?;
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
