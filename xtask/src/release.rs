//! The release dance: the version in `Cargo.toml`, the lock file beside it, one commit, one tag, and the push that publishes them — along with whatever else `main` had not published yet.
//!
//! **Invoking this recipe is the intent to publish.** `.github/workflows/release.yml` fires on a `release/*` tag and creates the public GitHub release from it, so once the preflight passes the dance runs to the end without asking again. Every precondition is therefore a refusal taken before anything is written: a dirty tree, a branch that is not `main`, a `main` that origin has moved past, a tag that already exists, or a version that is not above the one in the manifest stops the recipe with nothing done.
//!
//! **Nothing here builds, and nothing here undoes.** The check workflow runs on every push to `main` and has already had its say on the commits being released; the tag's own workflow builds and publishes without running it again. And removing a local commit or tag is destructive, so a failure past the commit prints what exists and what would undo it rather than doing it — that is the user's to ask for.
//!
//! **The write is checked by reading it back.** `toml_edit` preserves every byte it did not set and `cargo update --workspace --offline` rewrites one version per workspace member, so a bump is exactly one line in `Cargo.toml` and one per member in `Cargo.lock`. [`verify`] asserts that against the diff, which is what makes the mutation a fact rather than a hope.

use {
    crate::{
        commands::{ask, cargo, run},
        places::root,
    },
    std::{fmt, fs, process::Command},
    toml_edit::{DocumentMut, value},
};

#[cfg(test)]
mod tests;

/// A release version, as every tag in this repository's history spells one: three numbers and nothing else.
///
/// The field order is the comparison order, so `target > current` is the whole monotonicity rule.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Version {
    major: u64,
    minor: u64,
    patch: u64,
}

impl fmt::Display for Version {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Version {
            major,
            minor,
            patch,
        } = self;

        write!(formatter, "{major}.{minor}.{patch}")
    }
}

impl Version {
    /// The version `text` spells, or why it is not one.
    ///
    /// The round trip is the whole rule: three parts that parse as numbers and print back exactly as they arrived. That one check is what refuses a prerelease, a sign, surrounding space, and a leading zero that would otherwise be normalized away between the argument and the tag it becomes.
    fn parse(text: &str) -> Result<Version, String> {
        let refuse =
            || format!("not a release version: {text:?}; a version is three numbers, as 0.14.6 is");

        let parts = text.split('.').collect::<Vec<_>>();
        let [major, minor, patch] = parts.as_slice() else {
            return Err(refuse());
        };

        let version = Version {
            major: major.parse().map_err(|_| refuse())?,
            minor: minor.parse().map_err(|_| refuse())?,
            patch: patch.parse().map_err(|_| refuse())?,
        };

        match version.to_string() == text {
            true => Ok(version),
            false => Err(refuse()),
        }
    }

    /// The three versions a bump word names, in the order `patch`, `minor`, `major`.
    ///
    /// A minor bump zeroes the patch and a major bump zeroes both, so the rest of the range being left is abandoned rather than resumed — which is why a version below the current one can be free of a tag and must still be refused.
    fn bumps(self) -> [Version; 3] {
        let Version {
            major,
            minor,
            patch,
        } = self;

        [
            Version {
                major,
                minor,
                patch: patch + 1,
            },
            Version {
                major,
                minor: minor + 1,
                patch: 0,
            },
            Version {
                major: major + 1,
                minor: 0,
                patch: 0,
            },
        ]
    }
}

/// git at the workspace root, for a step of the dance.
fn git(arguments: &[&str]) -> Result<(), String> {
    run(Command::new("git"), arguments)
}

/// git at the workspace root, for a question the dance asks.
fn git_ask(arguments: &[&str]) -> Result<String, String> {
    ask(Command::new("git"), arguments)
}

/// The workspace version in the root manifest — the only place a release version is written, which every crate inherits and `Cargo.lock` records once per member.
fn declared(document: &DocumentMut) -> Result<Version, String> {
    let version = document
        .get("workspace")
        .and_then(|workspace| workspace.get("package"))
        .and_then(|package| package.get("version"))
        .and_then(|version| version.as_str())
        .ok_or_else(|| "Cargo.toml has no [workspace.package] version".to_string())?;

    Version::parse(version)
}

/// The version the argument names, refused unless it is above `current`.
///
/// With no argument the refusal *is* the answer: a recipe cannot ask which bump was meant, so it names the version in the manifest and the three a word would reach, and stops. A release is not cut from a guess.
fn resolve(bump: Option<&str>, current: Version) -> Result<Version, String> {
    let [patch, minor, major] = current.bumps();

    let target = match bump {
        None => {
            return Err(format!(
                "which release? {current} is current — patch {patch}, minor {minor}, major {major}"
            ));
        }
        Some("patch") => patch,
        Some("minor") => minor,
        Some("major") => major,
        Some(text) => Version::parse(text)?,
    };

    match target > current {
        true => Ok(target),
        false => Err(format!(
            "{target} is not above {current}, the version in the manifest"
        )),
    }
}

/// What the bump was allowed to do: the two manifests changed, and every changed line in them the version and nothing else.
///
/// Neither assertion needs to know which file a hunk came from. The file list already says both were touched, and a line that is neither version is refused wherever it appeared — which is what catches a `toml_edit` write that reached a dependency's version row, or a `cargo update` that moved something under the release.
fn verify(names: &str, diff: &str, current: Version, target: Version) -> Result<(), String> {
    let mut changed = names.split_whitespace().collect::<Vec<_>>();
    changed.sort_unstable();

    if changed != ["Cargo.lock", "Cargo.toml"] {
        return Err(format!(
            "the bump changed {changed:?}, not Cargo.toml and Cargo.lock alone"
        ));
    }

    let added = format!("version = \"{target}\"");
    let removed = format!("version = \"{current}\"");

    for line in diff.lines() {
        // The file headers carry the same signs as the bodies and say nothing about content.
        if line.starts_with("+++") || line.starts_with("---") {
            continue;
        }

        let offending = match (line.strip_prefix('+'), line.strip_prefix('-')) {
            (Some(body), _) => body != added,
            (_, Some(body)) => body != removed,
            _ => false,
        };

        if offending {
            return Err(format!(
                "the bump changed a line that is not the version: {line}"
            ));
        }
    }

    Ok(())
}

/// Everything that must hold before a release is cut, asked before anything is written.
fn preflight(tag: &str) -> Result<(), String> {
    let dirty = git_ask(&["status", "--porcelain"])?;
    if !dirty.trim().is_empty() {
        return Err(format!(
            "the tree is not clean, and a release must not carry uncommitted work:\n{}",
            dirty.trim_end()
        ));
    }

    let branch = git_ask(&["rev-parse", "--abbrev-ref", "HEAD"])?;
    if branch.trim() != "main" {
        return Err(format!("a release is cut from main, not {}", branch.trim()));
    }

    git(&["fetch", "origin", "main"])?;

    // **Commits `main` has not published are pushed by the dance, not refused by it.** The push below carries them with the bump, so cutting a release from a main that is merely ahead of origin is the ordinary case rather than a mistake to report. What is still refused is a main origin has moved *past*: that push would be rejected as a non-fast-forward, and it would be rejected after the commit and the tag were already written — leaving behind exactly the local state this preflight exists to make impossible.
    let unpulled = git_ask(&["rev-list", "--count", "HEAD..origin/main"])?;
    if unpulled.trim() != "0" {
        return Err(format!(
            "origin/main holds {} commit(s) this tree does not, so the push would be refused after the tag was written; pull before cutting a release",
            unpulled.trim()
        ));
    }

    if !git_ask(&["tag", "--list", tag])?.trim().is_empty() {
        return Err(format!("{tag} is already a tag, so that version was cut"));
    }

    Ok(())
}

/// Cut the release `bump` names.
pub(crate) fn release(bump: Option<&str>) -> Result<(), String> {
    let manifest = root().join("Cargo.toml");

    let mut document = fs::read_to_string(&manifest)
        .map_err(|error| format!("cannot read {}: {error}", manifest.display()))?
        .parse::<DocumentMut>()
        .map_err(|error| format!("cannot read {}: {error}", manifest.display()))?;

    let current = declared(&document)?;
    let target = resolve(bump, current)?;
    let tag = format!("release/{target}");

    preflight(&tag)?;

    document["workspace"]["package"]["version"] = value(target.to_string());
    fs::write(&manifest, document.to_string())
        .map_err(|error| format!("cannot write {}: {error}", manifest.display()))?;

    cargo(&["update", "--workspace", "--offline"])?;

    verify(
        &git_ask(&["diff", "--name-only"])?,
        &git_ask(&["diff"])?,
        current,
        target,
    )?;

    git(&[
        "commit",
        "Cargo.toml",
        "Cargo.lock",
        "-m",
        &format!("Bump the version to {target}"),
    ])?;

    git(&["tag", &tag]).map_err(|error| {
        format!(
            "{error}\n\nthe bump is committed and not tagged; a reset over a commit whose only content is the bump is what would undo it"
        )
    })?;

    git(&["push", "origin", "main"]).map_err(|error| {
        format!(
            "{error}\n\nnothing is published, and the commit and the tag are local; `git tag -d {tag}` and a reset over the bump are what would undo them"
        )
    })?;

    git(&["push", "origin", &tag]).map_err(|error| {
        format!(
            "{error}\n\nthe version is on main and the release is not cut; `git push origin {tag}` is what completes it"
        )
    })?;

    println!("\ncut {tag} at {}", git_ask(&["rev-parse", "HEAD"])?.trim());

    // The release is already cut by the time this runs, so a missing `gh` must not exit non-zero and say otherwise.
    if run(
        Command::new("gh"),
        &["run", "list", "--workflow", "release.yml", "--limit", "1"],
    )
    .is_err()
    {
        println!("release.yml is building it; `gh run list --workflow release.yml` follows it");
    }

    Ok(())
}
