//! What the extension adds beyond its grammar: which process answers as the language server, and where to find it.
//!
//! The server is the `curios` binary itself — `curios wonder server` — so there is nothing to download and no version to pin here: the compiler on the user's `PATH` is the one whose diagnostics they see, and installing Curios installs the server. The extension therefore never fetches, and says so when the binary is absent rather than guessing at one.

use zed_extension_api::{self as zed, LanguageServerId, Result};

struct Curios;

impl zed::Extension for Curios {
    fn new() -> Self {
        Self
    }

    fn language_server_command(
        &mut self,
        _id: &LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        // The one-line installer, spelled out: an extension cannot run it — it has no shell — but it can make the fix a paste.
        let command = worktree.which("curios").ok_or_else(|| {
            "`curios` is not on PATH. Install it with `curl -fsSL https://github.com/valmirjunior0088/curios/releases/latest/download/install.sh | sh`, then restart the language server.".to_string()
        })?;

        Ok(zed::Command {
            command,
            args: vec!["wonder".to_string(), "server".to_string()],
            env: Vec::new(),
        })
    }
}

zed::register_extension!(Curios);
