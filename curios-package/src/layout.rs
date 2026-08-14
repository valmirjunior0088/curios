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
/// The header is read here and read again when discovery asks for it. That is deliberate: the stem refusal below has to fire before elaboration, and a header is one small file — paying for it twice is cheaper than a refusal arriving as an unbound name.
pub fn package_source(package: &Package, directory: &Path) -> Result<Option<RootSource>, String> {
    let header = directory.join(LIBRARY);

    // A package of nothing but programs has no body, and no vestigial file to write saying so. A header that fails to *parse* is still a refusal — only its absence is an answer.
    if !header.is_file() {
        return Ok(None);
    }

    let library = Module::from_path(&header)
        .map_err(|error| format!("{}: {}", header.display(), error.format()))?;

    stems(package, &library)?;

    Ok(Some(RootSource::mounted(
        &package.name,
        RootKind::Ordinary,
        header,
        directory,
    )))
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

/// Refuse a stem claimed twice in the package root.
fn stems(package: &Package, library: &Module) -> Result<(), String> {
    let modules = library.items.iter().filter_map(|item| match item {
        TopItem::Mod(declaration) => Some((
            declaration.label.clone(),
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

    let mut claimed = BTreeMap::from([(
        stem(Path::new(LIBRARY)),
        format!("the library header `{LIBRARY}`"),
    )]);

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
