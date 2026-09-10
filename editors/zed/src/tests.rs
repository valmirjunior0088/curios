//! Whether the grammar this extension publishes is the grammar this tree holds.

use std::{path::Path, process::Command};

/// The directory Zed installs the grammar from, relative to the repository root.
const GRAMMAR: &str = "editors/grammar";

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

/// The tree [`GRAMMAR`] holds **on disk**, as a commit of this checkout would publish it.
///
/// **A temporary index, because a working tree has no tree object until something writes one.** Seeding it from `HEAD` and staging the directory over it is what turns the files on disk into a hash comparable with a commit's: `git add` honours `.gitignore`, so `node_modules/` is excluded exactly as a commit excludes it, and a file deleted on disk registers as a deletion against the seed. Neither the real index nor any file is touched, and the index is removed before anything can assert.
fn working(root: &Path) -> Option<String> {
    let index = std::env::temp_dir().join(format!("curios-zed-index-{}", std::process::id()));
    let git = |args: &[&str]| {
        Command::new("git")
            .current_dir(root)
            .env("GIT_INDEX_FILE", &index)
            .args(args)
            .output()
            .ok()
            .filter(|output| output.status.success())
    };

    let written = git(&["read-tree", "HEAD"])
        .and_then(|_| git(&["add", "-A", GRAMMAR]))
        .and_then(|_| git(&["write-tree", &format!("--prefix={GRAMMAR}")]))
        .map(|output| String::from_utf8_lossy(&output.stdout).trim().to_string());

    // Before the caller asserts, so a mismatch cannot leak the index it was computed through.
    let _ = std::fs::remove_file(&index);

    written
}

/// Zed installs the grammar from a pushed commit rather than from the checkout, so the rev in `extension.toml` is the only thing tying this extension to a grammar, and nothing else in the tree reads it. A regenerated grammar whose rev was not moved leaves every install building the previous one, silently.
///
/// **The held side is the working tree, and it has to be.** It was `HEAD` once, which made this check unable to fail until the grammar change was already committed — so the gate, which runs before a hand-off, could never be the thing that caught one. It was CI, every time, after the push. Hashing what is on disk moves the refusal to where the edit is, and costs nothing when the tree is clean: a clean checkout hashes to exactly what `HEAD` holds.
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

    let published = tree(&root, &format!("{rev}:{GRAMMAR}")).unwrap_or_else(|| {
        Command::new("git")
            .current_dir(&root)
            .args(["fetch", "--depth=1", "origin", rev])
            .status()
            .expect("git fetch runs");

        tree(&root, &format!("{rev}:{GRAMMAR}")).expect("the pinned rev is reachable")
    });
    let held = working(&root).expect("the working tree's grammar hashes");

    assert_eq!(
        published, held,
        "extension.toml pins {rev}, publishing grammar tree {published}, but this tree holds {held} — commit and push {GRAMMAR}, then move the rev"
    );
}
