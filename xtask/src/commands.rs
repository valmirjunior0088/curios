//! How a program is spawned and echoed: a [`Tool`] a plan runs, cargo for the commands that are not steps, any other program one of those names, and the one generator called as the library it wraps rather than spawned.
//!
//! **A verb names how a program is reached.** [`launch_in`] runs a [`Tool`] from a directory, refused with what installs it where its program is not there, and is how the runner performs a plan's runs; [`cargo`] is the toolchain's cargo at the workspace root, for the commands outside the table; anything else is a [`Command`] handed to [`run`], which defaults to the root. Every one of them echoes its command line before it runs, so the terminal reads as a transcript of the work.

use {
    crate::{Tool, root},
    std::{
        ffi::OsStr,
        io::ErrorKind,
        path::Path,
        process::{Command, Stdio},
    },
    wasm_bindgen_cli_support::Bindgen,
};

#[cfg(test)]
mod tests;

/// Where a command's output goes: to the terminal, because it is the user's to watch, or to the caller, because it is the program's to read.
enum Output {
    Streamed,
    Captured,
}

/// Why a spawn did not succeed: its program is not there to run, or it ran and failed, as the message says.
enum Failure {
    Missing(String),
    Failed(String),
}

impl Failure {
    /// The failure as a sentence, a missing program named with what the system said of it.
    fn message(self) -> String {
        match self {
            Failure::Missing(message) | Failure::Failed(message) => message,
        }
    }
}

/// The one spawn. A streamed command echoes its command line and inherits both of the terminal's streams; a captured one says nothing and pipes stdout back, leaving stderr inherited so a failure explains itself in the tool's own words rather than through this function. Either way a non-zero status is the error, and a program the system cannot find is told apart from one that ran.
fn execute(
    directory: &Path,
    mut command: Command,
    arguments: &[&str],
    output: Output,
) -> Result<String, Failure> {
    command.args(arguments).current_dir(directory);

    let program = command.get_program().to_string_lossy().into_owned();
    let unspawned = |error: std::io::Error| {
        let message = format!("cannot run {program}: {error}");
        match error.kind() {
            ErrorKind::NotFound => Failure::Missing(message),
            _ => Failure::Failed(message),
        }
    };

    let (status, printed) = match output {
        Output::Streamed => {
            eprintln!(
                "{program} {}",
                command
                    .get_args()
                    .map(|argument| argument.to_string_lossy())
                    .collect::<Vec<_>>()
                    .join(" ")
            );

            let status = command.status().map_err(unspawned)?;

            (status, String::new())
        }
        Output::Captured => {
            let produced = command
                .stdout(Stdio::piped())
                .spawn()
                .map_err(unspawned)?
                .wait_with_output()
                .map_err(|error| Failure::Failed(format!("cannot read from {program}: {error}")))?;

            (
                produced.status,
                String::from_utf8_lossy(&produced.stdout).into_owned(),
            )
        }
    };

    match status.success() {
        true => Ok(printed),
        false => Err(Failure::Failed(format!("{program} exited with {status}"))),
    }
}

/// The toolchain's cargo at the workspace root, for a command that is not a step.
pub(crate) fn cargo(arguments: &[&str]) -> Result<(), String> {
    launch_in(root(), Tool::Cargo, &[], arguments)
}

/// Run one command from the workspace root, echoing it first as a recipe would, and fail with its status.
pub(crate) fn run(command: Command, arguments: &[&str]) -> Result<(), String> {
    run_in(root(), command, arguments)
}

/// [`run`] from `directory` instead of the workspace root.
pub(crate) fn run_in(directory: &Path, command: Command, arguments: &[&str]) -> Result<(), String> {
    execute(directory, command, arguments, Output::Streamed)
        .map(drop)
        .map_err(Failure::message)
}

/// Run `tool` from `directory` as [`run_in`] does, with `environment` set beside what it inherits. A program the system cannot find is refused with what installs the tool, since that is the only way it is ever looked for.
pub(crate) fn launch_in(
    directory: &Path,
    tool: Tool,
    environment: &[(&str, &OsStr)],
    arguments: &[&str],
) -> Result<(), String> {
    let mut command = tool.command();
    command.envs(environment.iter().copied());

    execute(directory, command, arguments, Output::Streamed)
        .map(drop)
        .map_err(|failure| match failure {
            Failure::Missing(_) => tool.missing(),
            Failure::Failed(message) => message,
        })
}

/// What a command printed, for a recipe that is asking rather than doing.
///
/// A question's answer is the program's to read, so nothing is echoed and nothing reaches the terminal unless the command writes to stderr — which a failing one does, in its own words. The step verbs above are the other half: their output is the user's, and streaming it live is the whole point of them.
pub(crate) fn ask(command: Command, arguments: &[&str]) -> Result<String, String> {
    execute(root(), command, arguments, Output::Captured).map_err(Failure::message)
}

/// What `wasm-bindgen --target web --out-dir` does, called as the library it wraps and echoed as the command line it stands for. The command line emits the TypeScript declarations unless told not to, where the library does not unless told to; asking for them keeps the bundle's file set what the command line produced.
pub(crate) fn bindgen_web(module: &Path, bundle: &Path) -> Result<(), String> {
    eprintln!(
        "wasm-bindgen --target web --out-dir {} {}",
        bundle.display(),
        module.display()
    );

    let mut bindgen = Bindgen::new();

    bindgen
        .input_path(module)
        .web(true)
        .map_err(|error| format!("{error:#}"))?
        .typescript(true)
        .generate(bundle)
        .map_err(|error| format!("{error:#}"))?;

    Ok(())
}
