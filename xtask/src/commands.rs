//! How a program is spawned and echoed: a step's command, and a program the runner asks for what it prints.
//!
//! **A verb names how a program is reached.** [`launch_in`] runs a command's words from a directory, and is how the runner performs a step's commands, echoing its command line before it runs, so the terminal reads as a transcript of the work; [`ask`] and [`ask_launched`] run a program for what it prints, and echo nothing.

use {
    crate::{Launch, root},
    std::{
        path::Path,
        process::{Command, Stdio},
        sync::atomic::{AtomicBool, Ordering},
    },
};

#[cfg(test)]
mod tests;

/// Where a command's output goes: to the terminal, because it is the user's to watch, or to the caller, because it is the program's to read.
enum Output {
    Streamed,
    Captured,
}

/// Whether a row has been said in this run, so every row after the first stands apart from the output above it.
static SAID: AtomicBool = AtomicBool::new(false);

/// Say what is about to run, between guillemets, so a reader of the terminal finds where each piece of work begins: a step's command as its task file writes it, a check by its name, or a command outside the task files by its words.
pub fn say(line: &str) {
    if SAID.swap(true, Ordering::Relaxed) {
        eprintln!();
    }
    eprintln!("««« {line} »»»");
}

/// The one spawn. A streamed command inherits both of the terminal's streams, its caller having said it ([`say`]); a captured one says nothing and pipes stdout back, leaving stderr inherited so a failure explains itself in the tool's own words rather than through this function. Either way a non-zero status is the error, as is a program the system cannot find, named with what the system said of it.
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

/// Run what `launched` names from `directory`, what its leading assignments set beside what it inherits, said first as `said`, a step's command as its task file writes it, and fail with its status.
pub fn launch_in(directory: &Path, launched: Launch, said: &str) -> Result<(), String> {
    let Launch { command, arguments } = launched;
    let arguments = arguments.iter().map(String::as_str).collect::<Vec<_>>();

    say(said);
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
