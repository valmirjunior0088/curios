//! How a package on disk becomes the unit the compiler lowers.
//!
//! [`curios_text::RootSource`] states the layout rule every header obeys — a header's namespace directory is its stem directory. **This module owns its one exception:** a package's library header sits beside the manifest, and its namespace directory is the manifest's, not `lib/`. That is forced by law 2, because directory names are semantics-free and so no stem on disk could mark the package root; the manifest is what names that namespace, so the manifest is what anchors it. `lib` therefore enters no qualified name, and `/myorg/json/parse/lexer` is `parse/lexer.crs` inside the package directory — a qualified name's tail and its path are the same string, and only the head maps to the manifest instead of to a spelled directory.
//!
//! The consequence enforced below is that **the package root has one stem space**: the library header holds a stem in it, so does every module that header enumerates, and so does every executable compiled from a file directly inside it. A stem claimed twice is a refusal naming both claimants (law 4).

#[cfg(test)]
mod tests;

use {
    crate::{LIBRARY, MANIFEST, Manifest, Package},
    curios_base::RootKind,
    curios_text::{Module, RootSource, TopItem},
    std::{collections::BTreeMap, path::Path},
};

/// The package whose manifest sits in `directory`, and the resolver its unit is lowered from.
pub fn package_at(directory: &Path) -> Result<(Package, RootSource), String> {
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

/// The resolver `package`'s unit is lowered from, its library header beside the manifest in `directory`.
///
/// The header is read here and read again when discovery asks for it. That is deliberate: the stem refusal below has to fire before elaboration, and a header is one small file — paying for it twice is cheaper than a refusal arriving as an unbound name.
pub fn package_source(package: &Package, directory: &Path) -> Result<RootSource, String> {
    let header = directory.join(LIBRARY);

    // `name` obligates a library, so its absence is a refusal rather than a "no library then".
    let library = Module::from_path(&header).map_err(|error| {
        format!(
            "{}: {}. A package's `name` obligates a library header beside its manifest.",
            header.display(),
            error.format()
        )
    })?;

    stems(package, &library)?;

    Ok(RootSource::mounted(
        package.name.clone(),
        RootKind::Ordinary,
        header,
        directory,
    ))
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
