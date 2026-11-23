//! `zed check`: the Zed extension held to the tree around it — each grammar `extension.toml` pins is the grammar the tree holds, Core text's highlight query opens with the program's, and the lint table the extension restates is the workspace's.
//!
//! **Each is a copy the tree cannot spell once.** Zed installs a grammar from a pushed commit rather than from the checkout, reads a language's queries from that language's own directory alone, and builds the extension from its own directory as a workspace of its own, which inherits no lint table. So the pins, the program's query restated at the head of Core text's and the lint table are copies, and nothing else in the tree reads them.

use {
    crate::{ask, root, run},
    std::{
        collections::{BTreeMap, BTreeSet},
        env, fs,
        process::{self, Command},
    },
    toml_edit::{DocumentMut, Item},
};

#[cfg(test)]
mod tests;

/// The extension's directory, from the repository's root.
const EXTENSION: &str = "editors/zed";

/// The directory the grammars stand in: the program's in `base/` and Core text's in `core/`, each a directory Zed installs one from.
const GRAMMARS: &str = "editors/tree-sitter";

/// Every way the extension departs from the tree, a line each, or nothing.
pub fn zed() -> Result<(), String> {
    let mut wrong = Vec::new();

    for pin in pins(&read(&format!("{EXTENSION}/extension.toml"))?)? {
        wrong.extend(pin.unpublished()?);
    }

    // Core text's grammar extends the program's, so every node the program's query captures is one of Core text's, and its own patterns beneath refine the program's, a later pattern refining an earlier one.
    let program = read(&format!("{EXTENSION}/languages/curios/highlights.scm"))?;
    let core = read(&format!("{EXTENSION}/languages/curios_core/highlights.scm"))?;
    if !core.starts_with(&program) {
        wrong.push(format!("{EXTENSION}/languages/curios_core/highlights.scm does not open with {EXTENSION}/languages/curios/highlights.scm: Zed reads Core text's queries from its own directory alone, so it restates the program's at its head"));
    }

    let held = lints(&read("Cargo.toml")?, &["workspace", "lints"])?;
    let own = lints(&read(&format!("{EXTENSION}/Cargo.toml"))?, &["lints"])?;
    wrong.extend(departures(&held, &own));

    match wrong.is_empty() {
        true => Ok(()),
        false => Err(wrong.join("\n")),
    }
}

/// The text of the file at `path`, from the repository's root.
fn read(path: &str) -> Result<String, String> {
    fs::read_to_string(root().join(path)).map_err(|error| format!("{path}: {error}"))
}

/// A grammar `extension.toml` pins: the revision Zed installs it from, and the directory at that revision it installs.
#[derive(Debug, PartialEq, Eq)]
struct Pin {
    rev: String,
    path: String,
}

/// Each grammar `manifest`, an `extension.toml`, pins, in the order it writes them.
fn pins(manifest: &str) -> Result<Vec<Pin>, String> {
    let document = manifest
        .parse::<DocumentMut>()
        .map_err(|error| format!("extension.toml: {error}"))?;
    let Some(grammars) = document.get("grammars").and_then(Item::as_table_like) else {
        return Err("extension.toml pins no grammar".to_string());
    };

    grammars
        .iter()
        .map(|(name, grammar)| {
            let key = |key: &str| {
                grammar
                    .get(key)
                    .and_then(Item::as_str)
                    .map(str::to_string)
                    .ok_or_else(|| format!("extension.toml's grammar `{name}` states no `{key}`"))
            };

            Ok(Pin {
                rev: key("rev")?,
                path: key("path")?,
            })
        })
        .collect()
}

impl Pin {
    /// How the pin departs from the tree, where it does: the grammar of its name, the directory Zed installs and nothing beside it, published at `{rev}:{path}` and held in the working tree.
    ///
    /// **Held to the working tree, not `HEAD`.** Held to `HEAD`, a grammar change was caught only once committed, so the handoff, which runs before one, never caught it and the push did; the files on disk refuse it where the edit is, and a clean checkout hashes to exactly what `HEAD` holds. **Named by its directory's last part**, so a change beside the two grammars — `tree-sitter.json` and `common.js`, which only generating a parser reads — asks for no pin, and a pin made before the grammars' directory moved stays good while the grammar it publishes is the one the tree holds.
    ///
    /// **Fetched only where it is not here.** A working checkout holds the pinned commit whenever it is an ancestor, so the common case resolves offline; a checkout one commit deep, as CI's is, fetches it.
    fn unpublished(&self) -> Result<Option<String>, String> {
        let name = self.path.rsplit('/').next().unwrap_or(&self.path);
        let grammar = format!("{GRAMMARS}/{name}");
        let held = working(&grammar)?;
        let pinned = format!("{}:{}", self.rev, self.path);
        let published = match object(&pinned) {
            Some(published) => published,
            None => {
                run(
                    Command::new("git"),
                    &["fetch", "--depth=1", "origin", &self.rev],
                )?;
                object(&pinned).ok_or_else(|| {
                    format!("extension.toml pins {pinned}, which origin does not hold")
                })?
            }
        };

        Ok((published != held).then(|| {
            format!("extension.toml pins {pinned}, publishing grammar tree {published}, but this tree holds {held} at {grammar}: commit and push {grammar}, then move the rev and the path")
        }))
    }
}

/// The object `revision` names, or `None` where this checkout does not hold it.
fn object(revision: &str) -> Option<String> {
    ask(
        Command::new("git"),
        &["rev-parse", "--verify", "--quiet", revision],
    )
    .ok()
    .map(|printed| printed.trim().to_string())
}

/// The tree `directory` holds on disk, as a commit of this checkout would publish it.
///
/// **Through an index of its own, since a working tree has no tree object until something writes one.** Seeded from `HEAD`, with the directory staged over it, it turns the files on disk into a hash comparable with a commit's: `git add` honours `.gitignore`, so `node_modules/` is left out as a commit leaves it, and a file deleted on disk is a deletion against the seed. Neither the real index nor any file is touched, and the index is removed before the hash is handed back.
fn working(directory: &str) -> Result<String, String> {
    let index = env::temp_dir().join(format!("curios-xtask-index-{}", process::id()));
    let git = |arguments: &[&str]| {
        let mut command = Command::new("git");
        command.env("GIT_INDEX_FILE", &index);
        ask(command, arguments)
    };

    let written = git(&["read-tree", "HEAD"])
        .and_then(|_| git(&["add", "-A", directory]))
        .and_then(|_| git(&["write-tree", &format!("--prefix={directory}")]));
    let removed = fs::remove_file(&index);

    let tree = written?;
    removed.map_err(|error| format!("{}: {error}", index.display()))?;

    Ok(tree.trim().to_string())
}

/// The lints a manifest's table at `path` states, each by its tool and its name, `clippy.unused_self`, with the value it is set to, as written.
fn lints(manifest: &str, path: &[&str]) -> Result<BTreeMap<String, String>, String> {
    let document = manifest
        .parse::<DocumentMut>()
        .map_err(|error| format!("a manifest: {error}"))?;
    let table = path
        .iter()
        .try_fold(document.as_item(), |item, key| item.get(key))
        .and_then(Item::as_table_like)
        .ok_or_else(|| format!("a manifest states no `{}`", path.join(".")))?;

    let mut stated = BTreeMap::new();
    for (tool, lints) in table.iter() {
        let lints = lints
            .as_table_like()
            .ok_or_else(|| format!("`{}.{tool}` is no table of lints", path.join(".")))?;
        for (lint, level) in lints.iter() {
            stated.insert(
                format!("{tool}.{lint}"),
                level.to_string().trim().to_string(),
            );
        }
    }

    Ok(stated)
}

/// Each lint the extension's table, `own`, states otherwise than the workspace's, `held`: set to another value, or stated by one alone.
fn departures(held: &BTreeMap<String, String>, own: &BTreeMap<String, String>) -> Vec<String> {
    let stated = |lints: &BTreeMap<String, String>, lint: &str| {
        lints
            .get(lint)
            .map_or_else(|| "unstated".to_string(), |level| format!("`{level}`"))
    };

    held.keys()
        .chain(own.keys())
        .collect::<BTreeSet<_>>()
        .into_iter()
        .filter(|lint| held.get(*lint) != own.get(*lint))
        .map(|lint| {
            format!(
                "the lint `{lint}` is {} in Cargo.toml and {} in {EXTENSION}/Cargo.toml, a copy of the workspace's table",
                stated(held, lint),
                stated(own, lint)
            )
        })
        .collect()
}
