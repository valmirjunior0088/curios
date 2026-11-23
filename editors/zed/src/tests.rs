//! What ties this extension to the tree around it: the grammars it publishes are the grammars the tree holds, Core text's highlight query is the program's and its own, and the lints it is held to are the workspace's.

use std::{
    collections::{BTreeMap, BTreeSet},
    env, fs,
    path::Path,
    process::{self, Command},
};

/// The directory the grammars stand in, relative to the repository root: the program's in `base/` and Core text's in `core/`, each a directory Zed installs one from.
const GRAMMARS: &str = "editors/tree-sitter";

/// Each grammar `extension.toml` pins, by the revision Zed installs it from and the directory at that revision it installs.
fn pins(manifest: &str) -> Vec<(String, String)> {
    let mut tables = Vec::<(Option<String>, Option<String>)>::new();
    let mut in_grammar = false;

    for line in manifest.lines().map(str::trim) {
        let value = |key: &str| {
            line.strip_prefix(key)?
                .strip_prefix(" = \"")?
                .strip_suffix('"')
                .map(str::to_string)
        };
        if line.starts_with('[') {
            in_grammar = line.starts_with("[grammars.");
            if in_grammar {
                tables.push((None, None));
            }
        } else if let (true, Some((rev, path))) = (in_grammar, tables.last_mut()) {
            if let Some(value) = value("rev") {
                *rev = Some(value);
            } else if let Some(value) = value("path") {
                *path = Some(value);
            }
        }
    }

    tables
        .into_iter()
        .map(|table| match table {
            (Some(rev), Some(path)) => (rev, path),
            _ => panic!("each grammar extension.toml pins states its rev and its path"),
        })
        .collect()
}

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

/// The tree `directory` holds **on disk**, as a commit of this checkout would publish it.
///
/// **A temporary index, because a working tree has no tree object until something writes one.** Seeding it from `HEAD` and staging the directory over it is what turns the files on disk into a hash comparable with a commit's: `git add` honours `.gitignore`, so `node_modules/` is excluded exactly as a commit excludes it, and a file deleted on disk registers as a deletion against the seed. Neither the real index nor any file is touched, and the index is removed before anything can assert.
fn working(root: &Path, directory: &str) -> Option<String> {
    let index = env::temp_dir().join(format!("curios-zed-index-{}", process::id()));
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
        .and_then(|_| git(&["add", "-A", directory]))
        .and_then(|_| git(&["write-tree", &format!("--prefix={directory}")]))
        .map(|output| String::from_utf8_lossy(&output.stdout).trim().to_string());

    // Before the caller asserts, so a mismatch cannot leak the index it was computed through.
    let _ = fs::remove_file(&index);

    written
}

/// Zed installs each grammar from a pushed commit rather than from the checkout, so the pins in `extension.toml` — one for the program's grammar and one for Core text's, each a revision and a directory at it — are the only thing tying this extension to a grammar, and nothing else in the tree reads them. A regenerated grammar whose rev was not moved leaves every install building the previous one, silently.
///
/// **A pin is held to the grammar of its name, the directory Zed installs and nothing beside it**: the published `{rev}:{path}` to the working tree's directory of [`GRAMMARS`] named as `path` ends, so a change beside the two grammars — `tree-sitter.json` and `common.js`, which only generating a parser reads — asks for no pin, and a pin made before the grammars' directory moved stays good while the grammar it publishes is the one the tree holds.
///
/// **The held side is the working tree, and it has to be.** It was `HEAD` once, which made this check unable to fail until the grammar change was already committed — so the gate, which runs before a hand-off, could never be the thing that caught one. It was CI, every time, after the push. Hashing what is on disk moves the refusal to where the edit is, and costs nothing when the tree is clean: a clean checkout hashes to exactly what `HEAD` holds.
///
/// **The fetch is a fallback rather than the first move.** A working checkout already holds the pinned commit whenever it is an ancestor, so the common case resolves offline and this stays runnable without a network; CI checks out one commit deep and has to be told to go and get it.
#[test]
fn every_pinned_rev_publishes_this_grammar() {
    let extension = Path::new(env!("CARGO_MANIFEST_DIR"));
    let root = extension.join("../..");
    let manifest =
        fs::read_to_string(extension.join("extension.toml")).expect("extension.toml is read");
    let pins = pins(&manifest);
    assert!(!pins.is_empty(), "extension.toml pins a grammar");

    for (rev, path) in pins {
        let name = path.rsplit('/').next().unwrap_or(&path);
        let grammar = format!("{GRAMMARS}/{name}");
        let held = working(&root, &grammar).expect("the working tree's grammar hashes");
        let published = tree(&root, &format!("{rev}:{path}")).unwrap_or_else(|| {
            Command::new("git")
                .current_dir(&root)
                .args(["fetch", "--depth=1", "origin", &rev])
                .status()
                .expect("git fetch runs");

            tree(&root, &format!("{rev}:{path}")).expect("the pinned rev is reachable")
        });

        assert_eq!(
            published, held,
            "extension.toml pins {rev}:{path}, publishing grammar tree {published}, but this tree holds {held} at {grammar} — commit and push {grammar}, then move the rev and the path"
        );
    }
}

/// Zed reads a language's queries from its own directory alone, so Core text's highlight query restates the program's — every node of the program's grammar is one of Core text's — and adds its own beneath it, where a later pattern refines an earlier one.
#[test]
fn the_core_text_query_opens_with_the_programs() {
    let languages = Path::new(env!("CARGO_MANIFEST_DIR")).join("languages");
    let read = |language: &str| {
        fs::read_to_string(languages.join(language).join("highlights.scm"))
            .expect("a highlight query is read")
    };

    assert!(read("curios_core").starts_with(&read("curios")));
}

/// The lints a manifest states, by tool: each line of every table whose header is `prefix` and the tool's name.
fn lints(manifest: &str, prefix: &str) -> BTreeMap<String, BTreeSet<String>> {
    let mut tables = BTreeMap::<String, BTreeSet<String>>::new();
    let mut tool = None;

    for line in manifest.lines().map(str::trim) {
        if let Some(header) = line.strip_prefix('[') {
            tool = header
                .strip_suffix(']')
                .and_then(|header| header.strip_prefix(prefix))
                .map(str::to_string);
        } else if let Some(tool) = &tool
            && !line.is_empty()
            && !line.starts_with('#')
        {
            tables
                .entry(tool.clone())
                .or_default()
                .insert(line.to_string());
        }
    }

    tables
}

/// The extension is a workspace of its own, since Zed builds it from this directory, so it cannot inherit the compiler's lint table and states a copy. This holds the copy to its original: a lint the workspace gains, drops or moves fails here until the extension follows.
#[test]
fn the_lint_table_is_the_workspace_s() {
    let extension = Path::new(env!("CARGO_MANIFEST_DIR"));
    let workspace = fs::read_to_string(extension.join("../../Cargo.toml"))
        .expect("the workspace's manifest is read");
    let own = fs::read_to_string(extension.join("Cargo.toml")).expect("the manifest is read");

    let held = lints(&workspace, "workspace.lints.");
    assert!(!held.is_empty(), "the workspace states a lint table");
    assert_eq!(lints(&own, "lints."), held);
}
