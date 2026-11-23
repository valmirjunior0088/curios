//! How a program is spawned and echoed: a step's command, cargo for the commands outside the task files, and any other program one of those names.
//!
//! **A verb names how a program is reached.** [`launch_in`] runs a command's words from a directory, and is how the runner performs a step's commands; [`cargo`] is the toolchain's cargo at the workspace root, for the commands outside the task files; anything else is a [`Command`] handed to [`run`], which defaults to the root. Every one of them echoes its command line before it runs, so the terminal reads as a transcript of the work.

use {
    crate::{Launch, launch, root},
    std::{
        path::Path,
        process::{Command, Stdio},
    },
};

#[cfg(test)]
mod tests;

/// Where a command's output goes: to the terminal, because it is the user's to watch, or to the caller, because it is the program's to read.
enum Output {
    Streamed,
    Captured,
}

/// The one spawn. A streamed command echoes its command line and inherits both of the terminal's streams; a captured one says nothing and pipes stdout back, leaving stderr inherited so a failure explains itself in the tool's own words rather than through this function. Either way a non-zero status is the error, as is a program the system cannot find, named with what the system said of it.
fn execute(
    directory: &Path,
    mut command: Command,
    arguments: &[&str],
    output: Output,
) -> Result<String, String> {
    command.args(arguments).current_dir(directory);

    let program = command.get_program().to_string_lossy().into_owned();
    let unspawned = |error: std::io::Error| format!("cannot run {program}: {error}");

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
                .map_err(|error| format!("cannot read from {program}: {error}"))?;

            (
                produced.status,
                String::from_utf8_lossy(&produced.stdout).into_owned(),
            )
        }
    };

    match status.success() {
        true => Ok(printed),
        false => Err(format!("{program} exited with {status}")),
    }
}

/// The toolchain's cargo at the workspace root, for a command outside the task files.
pub fn cargo(arguments: &[&str]) -> Result<(), String> {
    let mut words = vec!["cargo".to_string()];
    words.extend(arguments.iter().map(|argument| argument.to_string()));

    launch_in(root(), launch(&words)?)
}

/// Run one command from the workspace root, echoing it first as a step's would be, and fail with its status.
pub fn run(command: Command, arguments: &[&str]) -> Result<(), String> {
    run_in(root(), command, arguments)
}

/// [`run`] from `directory` instead of the workspace root.
pub fn run_in(directory: &Path, command: Command, arguments: &[&str]) -> Result<(), String> {
    execute(directory, command, arguments, Output::Streamed).map(drop)
}

/// Run what `launched` names from `directory` as [`run_in`] does, what its leading assignments set beside what it inherits.
pub fn launch_in(directory: &Path, launched: Launch) -> Result<(), String> {
    let Launch { command, arguments } = launched;
    let arguments = arguments.iter().map(String::as_str).collect::<Vec<_>>();

    execute(directory, command, &arguments, Output::Streamed).map(drop)
}

/// What `launched` prints, asked from the root as [`ask`] asks.
pub fn ask_launched(launched: Launch) -> Result<String, String> {
    let Launch { command, arguments } = launched;
    let arguments = arguments.iter().map(String::as_str).collect::<Vec<_>>();

    execute(root(), command, &arguments, Output::Captured)
}

/// What a command printed, for a caller that is asking rather than doing.
///
/// A question's answer is the program's to read, so nothing is echoed and nothing reaches the terminal unless the command writes to stderr — which a failing one does, in its own words. The step verbs above are the other half: their output is the user's, and streaming it live is the whole point of them.
pub fn ask(command: Command, arguments: &[&str]) -> Result<String, String> {
    execute(root(), command, arguments, Output::Captured)
}
