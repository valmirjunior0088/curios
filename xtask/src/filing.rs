//! The copy discipline every filed artifact shares: a build product reaches `<crate>/.artifacts/` only when its bytes changed, beside the list of the sources it was built from.
//!
//! **This is a contract with another crate, not a step of one recipe.** `curios/build.rs` embeds what is filed here, reads the sidecar written beside it, and warns when a listed input is newer than the filed bytes — so what this module writes and what that build script reads have to agree.

use {
    crate::places::{modified, root},
    std::{
        fs,
        path::{Path, PathBuf},
        time::SystemTime,
    },
};

/// File `built` at `filed`, skipping the copy when the filed bytes are already the built ones, and say whether it copied. A recipe that embeds the launcher runs `runtime` first unconditionally, so filing must cost nothing when nothing changed: cargo already answers that for the build, and the skip is what keeps a repeated run from touching the file `curios/build.rs` watches.
///
/// The artifacts directory is the caller's to create — see [`file_with_inputs`], which writes there before this runs.
fn file(built: &Path, filed: &Path) -> Result<bool, String> {
    let bytes =
        fs::read(built).map_err(|error| format!("cannot read {}: {error}", built.display()))?;

    if fs::read(filed).is_ok_and(|current| current == bytes) {
        eprintln!("up to date {}", filed.display());
        return Ok(false);
    }

    fs::copy(built, filed).map_err(|error| {
        format!(
            "cannot copy {} to {}: {error}",
            built.display(),
            filed.display()
        )
    })?;

    eprintln!("filed {}", filed.display());

    Ok(true)
}

/// File a built binary as [`file`](file()) does, beside the list of what it was built from, and keep the filed timestamp honest when the bytes did not change.
///
/// `curios/build.rs` cannot rebuild the launcher, so it warns when a listed input is newer than the filed file. The list is cargo's own dep-info for the binary — every source rustc read, so a test file it never read is not in it — plus the workspace lock file, for a dependency bump the dep-info does not see; it is rewritten only when it changed, since the build script watches it too. The same comparison decides here whether a byte-identical rebuild refreshes the timestamp: an edit that changed no launcher byte — a comment, say — would otherwise leave that warning standing for a command with nothing left to do. A run in which nothing is newer touches nothing, which is what keeps a repeated `cargo x build` from recompiling the compiler that embeds the launcher.
pub(crate) fn file_with_inputs(built: &Path, filed: &Path, listed: &Path) -> Result<(), String> {
    // Created before the first write rather than before the copy, because the sidecar is what reaches the directory first. `.artifacts/` is a build product and is not committed, so on a clean checkout — CI's, and any fresh clone's — nothing has created it; the copy used to be its only creator, and a run got as far as the sidecar and failed there every time.
    let directory = filed.parent().expect("a filed artifact has a parent");
    fs::create_dir_all(directory)
        .map_err(|error| format!("cannot create {}: {error}", directory.display()))?;

    let mut sources = dep_info_sources(&built.with_extension("d"))?;
    sources.push(PathBuf::from("Cargo.lock"));

    let list = sources
        .iter()
        .map(|source| format!("{}\n", source.display()))
        .collect::<String>();
    if fs::read_to_string(listed).is_ok_and(|current| current == list) {
        eprintln!("up to date {}", listed.display());
    } else {
        fs::write(listed, list)
            .map_err(|error| format!("cannot write {}: {error}", listed.display()))?;
        eprintln!("filed {}", listed.display());
    }

    if file(built, filed)? {
        return Ok(());
    }

    let newest = sources
        .iter()
        .filter_map(|source| modified(&root().join(source)))
        .max();
    if let (Some(filed_at), Some(newest)) = (modified(filed), newest)
        && filed_at < newest
    {
        fs::File::options()
            .write(true)
            .open(filed)
            .and_then(|file| file.set_modified(SystemTime::now()))
            .map_err(|error| format!("cannot refresh {}: {error}", filed.display()))?;
        eprintln!("refreshed {}", filed.display());
    }

    Ok(())
}

/// The sources listed in the dep-info cargo wrote beside a built binary, relative to the workspace root where they lie under it.
///
/// The format is Makefile syntax — `<binary>: <source> <source> …` on one line, a space inside a path escaped as `\ ` — and it is a documented interface: cargo writes the file for exactly this kind of tool.
fn dep_info_sources(dep_info: &Path) -> Result<Vec<PathBuf>, String> {
    let text = fs::read_to_string(dep_info)
        .map_err(|error| format!("cannot read {}: {error}", dep_info.display()))?;
    let (_, listed) = text
        .lines()
        .next()
        .and_then(|line| line.split_once(": "))
        .ok_or_else(|| format!("{} is not a dep-info file", dep_info.display()))?;

    let mut sources = Vec::new();
    let mut current = String::new();
    let mut characters = listed.chars();
    while let Some(character) = characters.next() {
        match character {
            '\\' => match characters.next() {
                Some(' ') => current.push(' '),
                Some(other) => {
                    current.push('\\');
                    current.push(other);
                }
                None => current.push('\\'),
            },
            ' ' if !current.is_empty() => sources.push(std::mem::take(&mut current)),
            ' ' => {}
            _ => current.push(character),
        }
    }
    if !current.is_empty() {
        sources.push(current);
    }

    let mut sources = sources
        .into_iter()
        .map(|source| {
            let source = PathBuf::from(source);
            source
                .strip_prefix(root())
                .map_or_else(|_| source.clone(), Path::to_path_buf)
        })
        .collect::<Vec<_>>();
    sources.sort();
    sources.dedup();
    Ok(sources)
}
