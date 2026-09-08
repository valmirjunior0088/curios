//! Whether the grammar this extension publishes is the grammar this tree holds.

use std::{path::Path, process::Command};

/// The object `revision` names, or `None` when this checkout cannot resolve it.
fn tree(root: &Path, revision: &str) -> Option<String> {
    let output = Command::new("git")
        .current_dir(root)
        .args(["rev-parse", revision])
        .output()
        .ok()?;

    output
        .status
        .success()
        .then(|| String::from_utf8_lossy(&output.stdout).trim().to_string())
}

/// Zed installs the grammar from a pushed commit rather than from the checkout, so the rev in `extension.toml` is the only thing tying this extension to a grammar, and nothing else in the tree reads it. A regenerated grammar whose rev was not moved leaves every install building the previous one, silently.
///
/// **The fetch is a fallback rather than the first move.** A working checkout already holds the pinned commit whenever it is an ancestor, so the common case resolves offline and this stays runnable without a network; CI checks out one commit deep and has to be told to go and get it.
#[test]
fn the_pinned_rev_publishes_this_grammar() {
    let extension = Path::new(env!("CARGO_MANIFEST_DIR"));
    let root = extension.join("../..");
    let manifest =
        std::fs::read_to_string(extension.join("extension.toml")).expect("extension.toml is read");
    let rev = manifest
        .lines()
        .find_map(|line| line.strip_prefix("rev = \""))
        .and_then(|rest| rest.strip_suffix('"'))
        .expect("extension.toml states a grammar rev");

    let published = tree(&root, &format!("{rev}:editors/grammar")).unwrap_or_else(|| {
        Command::new("git")
            .current_dir(&root)
            .args(["fetch", "--depth=1", "origin", rev])
            .status()
            .expect("git fetch runs");

        tree(&root, &format!("{rev}:editors/grammar")).expect("the pinned rev is reachable")
    });
    let held = tree(&root, "HEAD:editors/grammar").expect("HEAD holds editors/grammar");

    assert_eq!(
        published, held,
        "extension.toml pins {rev}, publishing grammar tree {published}, but this tree holds {held} — commit and push editors/grammar, then move the rev"
    );
}
