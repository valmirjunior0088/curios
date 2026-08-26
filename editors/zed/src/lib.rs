//! What the extension adds beyond its grammar: which process answers as the language server, and where to find it.
//!
//! The server is the `curios` binary itself — `curios wonder server` — so there is nothing to download and no version to pin here: the compiler on the user's `PATH` is the one whose diagnostics they see, and installing Curios installs the server. The extension therefore never fetches, and when it can find no binary at all it hands back the one-line installer rather than a path it made up.
//!
//! Finding one is not the same as reading `PATH`, though. Zed inherits the `PATH` the desktop session launched it with, not the one an interactive shell assembles, so `~/.local/bin` — where the installer puts the binary — is routinely invisible here while every terminal on the machine can see it. Resolution therefore has three steps: an explicit setting, then `PATH`, then the installer's own directory.

use zed_extension_api::{self as zed, settings::LspSettings, LanguageServerId, Result};

struct Curios;

/// Where the installer puts the binary, relative to `$HOME`.
///
/// This is the one location the extension may assume, because it is the one the installer writes; anywhere else is the setting's business.
const INSTALLED_PATH: &str = ".local/bin/curios";

/// The one-line installer, named once because every message below ends in it.
///
/// An extension cannot run it — it has no shell — and cannot offer a button either: the extension API is headless, so a language server command fails with a string and nothing more. Zed puts that string in a read-only buffer, reached from the status bar, which is why each message isolates the command on its own line: in an editor a whole line is one gesture to select and copy.
const INSTALL_COMMAND: &str =
    "curl -fsSL https://github.com/valmirjunior0088/curios/releases/latest/download/install.sh | sh";

/// What to say when no step found anything.
fn not_found() -> String {
    format!(
        "Could not find the `curios` binary on PATH or in ~/.local/bin.\n\nInstall it with:\n\n{INSTALL_COMMAND}\n\nThen restart the language server. To use a binary from somewhere else, set `lsp.curios.binary.path` to it."
    )
}

/// What to say when the installer's directory holds a binary that will not start.
///
/// Absence and refusal are different reports and the user can act on each, so they are not merged: telling someone their compiler is missing while it sits where they installed it sends them to reinstall a file that is already there — which is, as it happens, the fix, but for a reason they were not told.
fn does_not_run(path: &str) -> String {
    format!(
        "Found `{path}`, but it does not run, so it cannot answer as the language server. A binary built for another architecture, or left half-written by an interrupted download, fails this way.\n\nReplace it with:\n\n{INSTALL_COMMAND}\n\nOr set `lsp.curios.binary.path` to a working binary."
    )
}

impl Curios {
    /// The binary to spawn, sought in the order that costs the least to be wrong about: what the user said, then what the environment says, then where the installer would have put it.
    fn server_path(id: &LanguageServerId, worktree: &zed::Worktree) -> Result<String> {
        let configured = LspSettings::for_worktree(id.as_ref(), worktree)
            .ok()
            .and_then(|settings| settings.binary)
            .and_then(|binary| binary.path);

        // A configured path, and one found on `PATH`, are handed on as written. The user named the first and the environment the second, so a binary that turns out to be wrong should fail as Zed's spawn error for that exact path — or, if it starts but is too old for `wonder server`, as the compiler's own report of an unknown subcommand. Neither is this extension's to paraphrase.
        if let Some(path) = configured.or_else(|| worktree.which("curios")) {
            return Ok(path);
        }

        Self::installed_path(worktree)
    }

    /// The installer's directory, checked before it is offered, because unlike the two steps above it is a guess: nobody named this path, so a wrong one would reach Zed as a spawn failure for a location the user never mentioned.
    ///
    /// `$HOME` is read from the shell environment rather than the process's, for the same reason the `PATH` lookup ahead of this one fails: the environment Zed was launched with is not the one the user's shell builds.
    fn installed_path(worktree: &zed::Worktree) -> Result<String> {
        let Some(home) = worktree
            .shell_env()
            .into_iter()
            .find_map(|(name, value)| (name == "HOME").then_some(value))
        else {
            return Err(not_found());
        };

        let candidate = format!("{home}/{INSTALLED_PATH}");

        // Spawning is what fails when there is no file, so the error case is the one that means absence; a process that ran and refused is a binary that exists and cannot serve.
        match zed::process::Command::new(&candidate)
            .arg("--version")
            .output()
        {
            Err(_) => Err(not_found()),
            Ok(output) if output.status == Some(0) => Ok(candidate),
            Ok(_) => Err(does_not_run(&candidate)),
        }
    }
}

impl zed::Extension for Curios {
    fn new() -> Self {
        Self
    }

    fn language_server_command(
        &mut self,
        id: &LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        let command = Self::server_path(id, worktree)?;

        Ok(zed::Command {
            command,
            args: vec!["wonder".to_string(), "server".to_string()],
            env: Vec::new(),
        })
    }
}

zed::register_extension!(Curios);
