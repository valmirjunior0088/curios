//! How a package on disk becomes the unit the compiler lowers.
//!
//! [`curios_text::RootSource`] states the layout rule every header obeys — a header's namespace directory is its stem directory. **This module owns its one exception:** a package's library header sits beside the manifest, and its namespace directory is the manifest's, not `lib/`. That is forced by law 2, because directory names are semantics-free and so no stem on disk could mark the package root; the manifest is what names that namespace, so the manifest is what anchors it. `lib` therefore enters no qualified name, and `/json/parse/lexer` is `parse/lexer.crs` inside the package directory — a qualified name's tail and its path are the same string, and only the head maps to the manifest instead of to a spelled directory.
//!
//! The consequence enforced below is that **the package root has one stem space**: the library header holds a stem in it, so does every module that header enumerates, and so does every executable compiled from a file directly inside it. A stem claimed twice is a refusal naming both claimants (law 4).
//!
//! **A package has a library when `lib.crs` sits beside its manifest, and its own executable when `exe.crs` does.** These are the two places location decides rather than declaration, and both are deliberate: neither is an artifact a package opts into, they are the two things a package *is*, and a manifest entry admitting to either would be ceremony. `[[executables]]` remains for everything else — a second program, or one compiled from a path of its own — and a row always wins over the file.
//!
//! This crate used to argue the executable half the other way, on the grounds that a vanished default "fails by silently not being there". That does not survive contact: delete `exe.crs` and `curios run` refuses, naming the package and saying it declares no executable. Both failures are loud; they are merely loud in different places.

#[cfg(test)]
mod tests;

use {
    crate::{LIBRARY, MANIFEST, Manifest, Package},
    curios_text::{Module, RootSource, TopItem},
    curios_utilities::RootKind,
    std::{
        collections::BTreeMap,
        path::{Path, PathBuf},
    },
};

/// The package whose manifest sits in `directory`, and the resolver its library is lowered from — `None` when it has no library at all.
pub fn package_at(directory: &Path) -> Result<(Package, Option<RootSource>), String> {
    let manifest = Manifest::from_path(&directory.join(MANIFEST))?;

    let Manifest::Package(package) = manifest else {
        return Err(format!(
            "{} declares an umbrella, and an umbrella compiles nothing of its own: name one of its members instead",
            directory.join(MANIFEST).display()
        ));
    };

    let source = package_source(&package, directory)?;

    Ok((package, source))
}

/// The resolver `package`'s library is lowered from, its header beside the manifest in `directory`, or `None` when there is no header there.
///
/// The header is read here and read again when discovery asks for it. That is deliberate: the stem refusal below has to fire before elaboration, and a header is one small file — paying for it twice is cheaper than a refusal arriving as an unbound name. Read from disk both times; a source overlay reaches discovery's read and not this one, so a stem collision is judged on what is saved, which is the only thing a collision can be between.
pub fn package_source(package: &Package, directory: &Path) -> Result<Option<RootSource>, String> {
    let header = directory.join(LIBRARY);
    let present = header.is_file();

    // A package of nothing but programs has no body, and no vestigial file to write saying so. Only absence is an answer: a header that fails to *parse* is still a refusal, but discovery's — it reads the same file through the unit's own loader and reports the failure as a located diagnostic, where a refusal raised here would be text with a snippet in it. What the header contributes to the stem space simply has nothing to contribute until it parses.
    let library = present.then(|| Module::from_path(&header).ok()).flatten();

    // Asked whether or not there is a header, because the stem space is the package root's and not the library's: executables claim stems in it with nothing else there to collide with, and two that collide are the same refusal either way. Gating this on the header would mean adding or deleting one silently turned the rule on and off for a manifest that did not change.
    stems(package, library.as_ref())?;

    match present {
        false => Ok(None),
        true => Ok(Some(RootSource::mounted(
            &package.name,
            RootKind::Ordinary,
            header,
            directory,
        ))),
    }
}

/// The resolvers `directories` are lowered from, in the order given — which is the order they are compiled in.
///
/// The hand-written stand-in for a manifest's dependency edges, so nothing here resolves or sorts. A directory with no library is refused rather than skipped: mounting it would mount nothing, and a caller that named it meant something by it.
pub fn mounted(directories: &[PathBuf]) -> Result<Vec<RootSource>, String> {
    directories
        .iter()
        .map(|directory| {
            let (_, source) = package_at(directory)?;

            source.ok_or_else(|| {
                format!(
                    "{} has no library, so mounting it would mount nothing",
                    directory.display()
                )
            })
        })
        .collect()
}

/// Refuse a stem claimed twice in the package root: the library header when there is one, every module that header enumerates, and every executable compiled from a file directly inside it.
///
/// `library` is `None` for a package with no header, and for one whose header does not parse — neither has modules to enumerate, and neither holds the `lib` stem in a way worth naming a claimant for. The executables are checked in both cases, because what they claim does not depend on any of that.
fn stems(package: &Package, library: Option<&Module>) -> Result<(), String> {
    let modules = library
        .into_iter()
        .flat_map(|library| library.items.iter())
        .filter_map(|item| match item {
            TopItem::Mod(declaration) => Some((
                declaration.label.to_string(),
                format!("`mod {}` in `{LIBRARY}`", declaration.label),
            )),
            _ => None,
        });

    // An executable whose path leaves the package root claims a stem somewhere else, and somewhere else is not this stem space.
    let executables = package
        .executables
        .iter()
        .filter(|executable| executable.path.parent() == Some(Path::new("")))
        .map(|executable| {
            (
                stem(&executable.path),
                format!("the executable {:?}", executable.name),
            )
        });

    let mut claimed = BTreeMap::new();
    if library.is_some() {
        claimed.insert(
            stem(Path::new(LIBRARY)),
            format!("the library header `{LIBRARY}`"),
        );
    }

    for (stem, claimant) in modules.chain(executables) {
        if let Some(earlier) = claimed.insert(stem.clone(), claimant.clone()) {
            return Err(format!(
                "the package root claims the stem `{stem}` twice: {earlier} and {claimant}"
            ));
        }
    }

    Ok(())
}

/// The stem `path` spells, which is what it claims in the directory it sits in.
fn stem(path: &Path) -> String {
    path.file_stem()
        .unwrap_or_default()
        .to_string_lossy()
        .into_owned()
}
