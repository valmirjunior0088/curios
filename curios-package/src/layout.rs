//! How a package on disk becomes the unit the compiler lowers.
//!
//! [`curios_text::RootSource`] states the layout rule every header obeys — a header's namespace directory is its stem directory. **This module owns its one exception:** a package's library header sits beside the manifest, and its namespace directory is the manifest's, not `lib/`. That is forced by law 2, because directory names are semantics-free and so no stem on disk could mark the package root; the manifest is what names that namespace, so the manifest is what anchors it. `lib` therefore enters no qualified name, and `/myorg/json/parse/lexer` is `parse/lexer.crs` inside the package directory — a qualified name's tail and its path are the same string, and only the head maps to the manifest instead of to a spelled directory.
//!
//! The consequence enforced below is that **the package root has one stem space**: the library header holds a stem in it, so does every module that header enumerates, and so does every executable compiled from a file directly inside it. A stem claimed twice is a refusal naming both claimants (law 4).
//!
//! **A package has a library when `lib.crs` sits beside its manifest, and none when it does not.** This is the one place location decides rather than declaration, and it is deliberate: a library is not an artifact a package opts into, it is the package's own body, and a package of nothing but programs has none to declare. What makes the exception safe is that the failure is loud — a library that stops existing takes its whole mount with it, so every reference in every dependent stops resolving at once. The same reasoning does not extend to executables: a default executable that stops existing fails by silently not being there, which is why those stay enumerated.

#[cfg(test)]
mod tests;

use {
    crate::{LIBRARY, MANIFEST, Manifest, Package},
    curios_base::RootKind,
    curios_text::{Module, RootSource, TopItem},
    std::{collections::BTreeMap, path::Path},
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
        package.name.clone(),
        RootKind::Ordinary,
        header,
        directory,
    )))
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
    let executables = package.executables.iter().filter_map(|executable| {
        (executable.path.parent() == Some(Path::new(""))).then(|| {
            (
                stem(&executable.path),
                format!("the executable {:?}", executable.name),
            )
        })
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
