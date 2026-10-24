//! How a package on disk becomes the unit the compiler lowers.
//!
//! [`curios_text::RootSource`] states the layout rule every header obeys: a unit's root reads its modules from the directory it sits in, and a module below it from its stem directory. A package's library header sits beside the manifest, so its namespace is the manifest's directory: `lib` enters no qualified name, and `/json/parse/lexer` is `parse/lexer.crs` inside the package directory, a qualified name's tail and its path the same string and only the head mapped to the manifest, which is what names that namespace (law 2).
//!
//! The consequence enforced below is that **a file is one unit's**: the library and every executable beside the manifest read their modules from the package's directory alike, and a row may point anywhere below it, so a file two units' `mod` chains reach is a refusal naming both (law 4).
//!
//! **A package has a library when `lib.crs` sits beside its manifest, and its own executable when `exe.crs` does.** These are the two places location decides rather than declaration, and both are deliberate: neither is an artifact a package opts into, they are the two things a package *is*, and a manifest entry admitting to either would be ceremony. `[[executables]]` remains for everything else — a second program, or one compiled from a path of its own — and a row always wins over the file.
//!
//! The objection that a vanished default "fails by silently not being there" does not hold: delete `exe.crs` and `curios run` refuses, naming the package and saying it declares no executable. Both failures are loud; they are merely loud in different places.

#[cfg(test)]
mod tests;

use {
    crate::{EXTENSION, LIBRARY, MANIFEST, Manifest, Package, require_compiler, umbrella_over},
    curios_text::{Entrypoint, RootSource},
    curios_utilities::{Qualifier, RootKind, is_identifier},
    std::{
        collections::BTreeMap,
        iter,
        path::{Component, Path, PathBuf},
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

/// The prefixes `package`'s library may name: every dependency it declares, plus the standard library.
///
/// **Direct dependencies only.** A dependency of a dependency is in the fold — it had to be, for the one in between to compile — and is unspellable here, which is what makes a manifest's rows the whole statement of what a unit reaches. This is law 3: membership organizes and dependency compiles.
///
/// `/std` is implicit because it is the one unit every program is entitled to and no manifest declares: a row for it would be ceremony every package in existence repeats. It is named here rather than derived, which is the one place in this crate that knows the standard library by name; the root beneath it, `/sys`, is named by nothing here at all — it has no path, so no manifest can reach it and `curios-pipeline`'s `standard` module makes the one grant of it.
pub(crate) fn declared(package: &Package) -> Vec<Qualifier> {
    package
        .dependencies
        .keys()
        .map(|name| Qualifier::from([name.as_str()]))
        .chain([Qualifier::from(["std"])])
        .collect()
}

/// The prefixes one of `package`'s executables may name: everything its library may, and the library itself.
///
/// An executable is part of its package, so it reaches the package's own library without declaring it — a program does not depend on itself. Spelled once, here, because every way of selecting a declared program asks it — by name, as the package's default, or through a file a question placed — and an executable that resolves a prefix under one and not another is a disagreement no test crosses.
pub(crate) fn reachable(package: &Package) -> Vec<Qualifier> {
    declared(package)
        .into_iter()
        .chain([Qualifier::from([package.name.as_str()])])
        .collect()
}

/// The module `file` would be in `package`'s library at `directory`, by the layout rule and its one exception: `lib.crs` beside the manifest is the root, and every other `.crs` under the directory is the qualifier its path spells, `parse/lexer.crs` being `/json/parse/lexer`. `None` when the spelling is no module's — another extension, or a segment no identifier — since no `mod` could declare it.
///
/// The spelling is what is asked, not the disk: whether a `mod` reaches the module is `RootSource::declares_module`'s question, and this is what to ask it about. Both paths are compared as given, so hand them one spelling.
pub(crate) fn module_of(package: &Package, directory: &Path, file: &Path) -> Option<Qualifier> {
    if file == directory.join(LIBRARY) {
        return Some(Qualifier::from([package.name.as_str()]));
    }

    let mut segments = vec![package.name.clone()];
    segments.extend(spelled_segments(file.strip_prefix(directory).ok()?)?);

    segments
        .iter()
        .all(|segment| is_identifier(segment))
        .then(|| Qualifier::from(segments))
}

/// The module `file` would be under a root whose namespace directory is `namespace`, by the layout rule every header obeys: `helper/deep.crs` is `/helper/deep` below the root's own mount. `None` when the spelling is no module's.
pub(crate) fn module_below(namespace: &Path, file: &Path) -> Option<Qualifier> {
    spelled_segments(file.strip_prefix(namespace).ok()?).map(Qualifier::from)
}

/// The segments `relative` spells as a module path — `parse/lexer.crs` is `parse`, `lexer` — or `None` for another extension or a segment no identifier.
fn spelled_segments(relative: &Path) -> Option<Vec<String>> {
    if relative.extension()? != EXTENSION {
        return None;
    }

    let mut segments = Vec::new();
    for component in relative.parent()?.components() {
        let Component::Normal(segment) = component else {
            return None;
        };
        segments.push(segment.to_str()?.to_string());
    }
    segments.push(relative.file_stem()?.to_str()?.to_string());

    segments
        .iter()
        .all(|segment| is_identifier(segment))
        .then_some(segments)
}

/// The resolver `package`'s library is lowered from, its header beside the manifest in `directory`, or `None` when there is no header there.
///
/// Every unit's `mod` chain is read here and read again when discovery asks for it. That is deliberate: the refusal of a file two units reach has to fire before elaboration, and one arriving as a module compiled twice, or as a program parsed as a module, would say nothing of the two claims. It is parsed once all the same where the two read one text, both reads going through the thread's parse memo (`curios_text::Module::from_path`). Read from disk here; a source overlay reaches discovery's read and not this one, so a claim is judged on what is saved, which is the only thing two claims can be between.
pub fn package_source(package: &Package, directory: &Path) -> Result<Option<RootSource>, String> {
    let umbrella = umbrella_over(directory)?;
    require_compiler(
        package,
        directory,
        umbrella
            .as_ref()
            .map(|(umbrella, root)| (umbrella, root.as_path())),
    )?;
    let header = directory.join(LIBRARY);

    // A package of nothing but programs has no body, and no vestigial file to write saying so. Only absence is an answer: a header that fails to *parse* is still mounted, and its refusal is discovery's — it reads the same file through the unit's own loader and reports the failure as a located diagnostic, where a refusal raised here would be text with a snippet in it. The library is what a package documents, described as the manifest describes it, and what its modules answer: each claim travels with the unit, which is where it is held to a declaration.
    let source = header.is_file().then(|| {
        RootSource::mounted(
            &package.name,
            RootKind::Ordinary,
            &header,
            RootSource::namespace_of(&header),
        )
        .declaring(declared(package))
        .documented(&package.name, package.description.as_deref())
        .answering(package.foreign.iter().flat_map(|foreign| {
            foreign
                .answers
                .keys()
                .map(|declaration| (declaration.clone(), foreign.name.clone()))
        }))
    });

    // Asked whether or not there is a header, because the units sharing the package's directory are its programs as well as its library: two programs compiled from one file are the same refusal either way, and gating this on the header would turn the rule on and off for a manifest that did not change.
    one_unit_each(package, directory, source.as_ref())?;

    // A module answers declarations of its package's library, so with no library there is none for a row's exports to answer — and no unit to hold the claim to.
    if source.is_none()
        && let Some(foreign) = package
            .foreign
            .iter()
            .find(|foreign| !foreign.answers.is_empty())
    {
        return Err(format!(
            "the foreign row {:?} of {} names declarations its exports answer, and the package has no `{LIBRARY}` to declare them in: a module answers its own package's declarations",
            foreign.name, package.name
        ));
    }

    Ok(source)
}

/// One unit's claim on a file: who claims it, and whether as the program compiled from it or as a module its `mod` chain reaches.
struct Claim {
    claimant: String,
    entry: bool,
}

/// Who claims the library's files.
const LIBRARY_CLAIMANT: &str = "the library";

/// Refuse a file two units reach: the library through the `mod` chain from its header, and each executable through its entry, which is a file of its own unit, and the chain from it.
///
/// Roots share the directory they sit in, so the library and every executable beside the manifest read their modules from one directory, and a row may point below it, into a module's namespace directory. A file both reach would be compiled into two units as two unrelated modules, or be a program and a module at once, where the module loader would report a parse error at the program's first token naming neither claim. A file that does not read is still claimed, and ends its branch: the compilation reports it on its own account, and a program declared as a module is one such file. A package with no executable is one unit, and its chain is not walked here.
fn one_unit_each(
    package: &Package,
    directory: &Path,
    library: Option<&RootSource>,
) -> Result<(), String> {
    if package.executables.is_empty() {
        return Ok(());
    }

    let library_files = library
        .map(RootSource::reached_files)
        .unwrap_or_default()
        .into_iter()
        .map(|file| {
            let claim = Claim {
                claimant: LIBRARY_CLAIMANT.to_string(),
                entry: false,
            };
            (file, claim)
        });
    let executable_files = package.executables.iter().flat_map(|executable| {
        let entry = directory.join(&executable.path);
        let modules = Entrypoint::opened(&entry)
            .map(|(entrypoint, loader, _)| loader.entry_reached_files(&entrypoint.module.items))
            .unwrap_or_default();

        iter::once((entry, true))
            .chain(modules.into_iter().map(|file| (file, false)))
            .map(|(file, entry)| {
                let claim = Claim {
                    claimant: package.describe(executable),
                    entry,
                };
                (file, claim)
            })
    });

    let mut claimed = BTreeMap::<PathBuf, Claim>::new();
    for (file, claim) in library_files.chain(executable_files) {
        match claimed.get(&file) {
            None => {
                claimed.insert(file, claim);
            }
            Some(earlier) => return Err(refusal(package, directory, &file, earlier, &claim)),
        }
    }

    Ok(())
}

/// Why `file` cannot be in the units of `earlier` and `later` both, and what would put it in one.
fn refusal(
    package: &Package,
    directory: &Path,
    file: &Path,
    earlier: &Claim,
    later: &Claim,
) -> String {
    let spelled = file.strip_prefix(directory).unwrap_or(file).display();

    match (earlier.entry, later.entry) {
        (true, true) => format!(
            "{} and {} are both compiled from `{spelled}`: a file is one program's",
            earlier.claimant, later.claimant
        ),
        (true, false) | (false, true) => {
            let (program, module) = match earlier.entry {
                true => (earlier, later),
                false => (later, earlier),
            };
            // The library's modules are named by the package, so its claim can say which module the file is.
            let named = match module.claimant == LIBRARY_CLAIMANT {
                true => module_of(package, directory, file)
                    .map(|module| format!("the module `{}`", module.join())),
                false => None,
            }
            .unwrap_or_else(|| "a module".to_string());
            format!(
                "{} is compiled from `{spelled}`, which {} also declares as {named}: a file is a program or a module, never both",
                program.claimant, module.claimant
            )
        }
        (false, false) => {
            // An executable reaches its own package's library, never another executable, so the library is where a module two units want lives.
            let reach = module_of(package, directory, file)
                .map(|module| format!(" through `{}`", module.join()))
                .unwrap_or_default();
            let remedy = match earlier.claimant == LIBRARY_CLAIMANT {
                true => {
                    format!("declare it in the library alone, and reach it from the other{reach}")
                }
                false => format!("declare it in the library, and reach it from both{reach}"),
            };
            format!(
                "`{spelled}` is a module of {} and of {}: a file is one unit's, so {remedy}",
                earlier.claimant, later.claimant
            )
        }
    }
}
