//! What answers the `foreign` rows a program reaches, for each target: the `[[foreign]]` rows of its package graph that answer one of them, compiled natively into the modules a run links and an executable carries, or read for a browser into the gateways and files its page holds — and the refusal of a reached row nothing answers, by what would answer it.
//!
//! **Nothing is read where the host answers every row the program reaches**, which is very nearly every program, since each reaches the standard library's rows and few any other: no manifest is walked and no file opened, so the common case pays one pass over its rows. A row answers a declaration of its own package alone, so none could answer for such a program.
//!
//! **The graph walked is the one of the package whose manifest declares the program**, an umbrella's member as a package standing alone: an umbrella's directory, where the store sits, declares no package to walk from. A loose program has no manifest and so no row: what it reaches of the standard library's rows the host answers, and a row of its own is refused, saying it has nowhere to name what would answer it.

#[cfg(test)]
mod tests;

use {
    curios_abi::{ForeignStore, host_answers},
    curios_browser::{Linked, Placed},
    curios_common::{Report, Said},
    curios_native::{DeclaredModule, ForeignBindings, Unlinked, plugin_bindings},
    curios_package::{
        Foreign, Governing, MANIFEST, Refused, Row, Step, Store, Target, declared_modules,
    },
    std::{
        fs,
        path::{Path, PathBuf},
    },
};

/// The `[[foreign]]` rows of a program's package graph that answer a declaration it reaches, split by whether they state one target's table.
#[derive(Default)]
struct Reached {
    /// Each row stating the target's table, with the package that declares it and the directory its paths are read from.
    stating: Vec<(String, PathBuf, Foreign)>,
    /// Each row stating none: the package that declares it, the directory its manifest sits in, its name, and what it answers.
    elsewhere: Vec<(String, PathBuf, String, Vec<String>)>,
    /// The store a fetched delivery is read from, where a graph was walked.
    store: Option<Store>,
}

/// The rows of the graph of the package `manifest` declares that answer a declaration among `foreigns`, split by whether they state `target`'s table.
fn reached(
    manifest: Option<&Path>,
    foreigns: &ForeignStore,
    target: Target,
) -> Result<Reached, Box<Report>> {
    if foreigns
        .iter()
        .all(|function| host_answers(function.name()))
    {
        return Ok(Reached::default());
    }

    let Some(manifest) = manifest else {
        return Ok(Reached::default());
    };

    let governing = Governing::at(manifest)?;
    let mut reached = Reached {
        store: Some(governing.store()),
        ..Reached::default()
    };

    // Only a row that answers a declaration the program reaches is read: the rest of the graph's are neither linked nor carried.
    for (package, directory, foreign) in declared_modules(&governing)? {
        if !foreign
            .answers
            .keys()
            .any(|declaration| foreigns.get(declaration).is_some())
        {
            continue;
        }

        match foreign.delivery(target) {
            Some(_) => reached.stating.push((package, directory, foreign)),
            None => reached.elsewhere.push((
                package,
                directory,
                foreign.name,
                foreign.answers.into_keys().collect(),
            )),
        }
    }

    Ok(reached)
}

/// Refuse the first declaration among `foreigns` that nothing `answered` names and the host does not answer, by what would answer it: a row that answers it and states no table for `target`, which is the author's word that the target links nothing for it, or a `[[foreign]]` row of a manifest, which a loose program has none to write in.
fn unanswered(
    manifest: Option<&Path>,
    foreigns: &ForeignStore,
    elsewhere: &[(String, PathBuf, String, Vec<String>)],
    target: Target,
    answered: impl Fn(&str) -> bool,
) -> Result<(), Box<Report>> {
    let Some(name) = foreigns
        .iter()
        .map(|function| function.name())
        .find(|name| !answered(name) && !host_answers(name))
    else {
        return Ok(());
    };

    let refused = Refused::of("no implementation");
    let spelled = target.spelled();

    Err(
        match elsewhere
            .iter()
            .find(|(_, _, _, answers)| answers.iter().any(|answer| answer == name))
        {
            Some((package, directory, row, _)) => {
                let refused = refused
                    .stating(format_args!(
                        "`{name}` is answered by the foreign row `{row}` of `/{package}`"
                    ))
                    .stating(format_args!("that row states no `[foreign.{spelled}]`"))
                    .stating(format_args!(
                        "the {spelled} target has nothing to link for it"
                    ));
                // A gateway is tested natively through a module standing in for it, and against the gateway itself by a test serve, so the native refusal names both doors.
                let refused = match target {
                    Target::Native => refused
                        .stating("a `.wasm` standing in for its gateway, stated as `[foreign.native]`, is what `run` and `test` link")
                        .stating("`test --target browser` runs its tests against the gateway"),
                    Target::Browser => refused,
                };

                written(directory, row).refuse(refused)
            }
            None => {
                let refused = refused.stating(format_args!("`{name}` is declared `foreign`"));

                match manifest {
                    Some(manifest) => refused
                        .stating("no `[[foreign]]` row of the package's manifests names an export that answers it")
                        .about_path(manifest),
                    None => refused
                        .stating("a `foreign` is answered by a module that a `[[foreign]]` row of a manifest names")
                        .stating("a loose program has no manifest to write one in")
                        .about_named(name),
                }
            }
        },
    )
}

/// The `[[foreign]]` row `row` of the package whose manifest sits in `directory`: where a refusal of what the row declares stands.
fn written(directory: &Path, row: &str) -> Row {
    Row::of(
        directory.join(MANIFEST),
        [Step::key("foreign"), Step::Named(row.to_string())],
    )
}

/// The report of what linking refused of the program the package at `manifest` declares, for whoever links it past [`bindings_of`]: at the row that declares the module it is said of, read off the package's graph again, since a refusal is the path that can afford the walk.
pub fn unlinked(unlinked: Unlinked, manifest: Option<&Path>) -> Box<Report> {
    let rows = manifest
        .and_then(|manifest| Governing::at(manifest).ok())
        .and_then(|governing| declared_modules(&governing).ok())
        .unwrap_or_default()
        .into_iter()
        .map(|(package, directory, foreign)| (package, directory, foreign.name))
        .collect::<Vec<_>>();

    refused(unlinked, &rows, manifest)
}

/// The report of what linking refused: at the row that declares the module it is said of, found among `rows` by its package and its name, and about the manifest, or the program where it has none, otherwise.
fn refused(
    unlinked: Unlinked,
    rows: &[(String, PathBuf, String)],
    manifest: Option<&Path>,
) -> Box<Report> {
    let Unlinked { tag, facts, module } = unlinked;
    let mut said = Said::line(tag);
    for fact in facts {
        said.stating(fact);
    }
    let refused = Refused::saying(said);
    let declared = module.and_then(|(package, name)| {
        rows.iter()
            .find(|(of, _, row)| *of == package && *row == name)
            .map(|(_, directory, row)| written(directory, row))
    });

    match (declared, manifest) {
        (Some(row), _) => row.refuse(refused),
        (None, Some(manifest)) => refused.about_path(manifest),
        (None, None) => refused.about_named("the program"),
    }
}

/// What answers a program's rows natively ahead of the host: each module its packages declare for the rows it reaches, accepted against the hash its row pins and compiled here, where Cranelift is, to the machine code `run`, `test` and `compile` alike resolve it from. Shared by the three, so what a bundled executable carries and what a run links are one resolution, and a reached row nothing answers is refused before anything runs or is written.
pub fn bindings_of(
    manifest: Option<&Path>,
    foreigns: &ForeignStore,
) -> Result<ForeignBindings, Box<Report>> {
    let reached = reached(manifest, foreigns, Target::Native)?;
    let rows = reached
        .stating
        .iter()
        .map(|(package, directory, foreign)| {
            (package.clone(), directory.clone(), foreign.name.clone())
        })
        .collect::<Vec<_>>();

    let mut modules = Vec::new();
    for (package, directory, foreign) in reached.stating {
        let native = foreign
            .native
            .as_ref()
            .expect("a row stating the native table delivers a module");
        let store = reached.store.as_ref().expect("a walked graph has a store");

        let code = curios_cranelift::compile(&native.bytes(
            &foreign.name,
            Target::Native,
            &directory,
            store,
        )?)
        .map_err(|error| {
            written(&directory, &foreign.name).refuse(
                Refused::of("invalid module")
                    .stating(format_args!(
                        "the module `{}` of `/{package}` is no WebAssembly module this engine accepts",
                        foreign.name
                    ))
                    .stating(error),
            )
        })?;

        modules.push(DeclaredModule {
            package,
            code,
            name: foreign.name,
            answers: foreign.answers,
        });
    }

    let bindings = plugin_bindings(foreigns, modules)
        .map_err(|unlinked| refused(unlinked, &rows, manifest))?;
    unanswered(
        manifest,
        foreigns,
        &reached.elsewhere,
        Target::Native,
        |name| bindings.answers(name),
    )?;

    Ok(bindings)
}

/// What a browser links for a program's rows ahead of the host: each gateway its packages declare for the rows it reaches, accepted against the hash its row pins, with the declarations of the program it answers and every file its `static` table serves, read here once. A reached row nothing answers is refused before anything is written.
pub fn linked_of(
    manifest: Option<&Path>,
    foreigns: &ForeignStore,
) -> Result<Vec<Linked>, Box<Report>> {
    let reached = reached(manifest, foreigns, Target::Browser)?;

    let mut linked = Vec::new();
    for (package, directory, foreign) in reached.stating {
        let gateway = foreign
            .browser
            .as_ref()
            .expect("a row stating the browser table delivers a gateway");
        let store = reached.store.as_ref().expect("a walked graph has a store");

        let mut statics = Vec::new();
        for served in gateway.served(&foreign.name, &directory)? {
            let bytes = fs::read(&served.file).map_err(|error| {
                written(&directory, &foreign.name).refuse(
                    Refused::of("unreadable file")
                        .stating(format_args!(
                            "the static entry `{}` of the foreign row `{}` reads {}",
                            served.entry,
                            foreign.name,
                            served.file.display()
                        ))
                        .stating(error),
                )
            })?;

            statics.push((
                served.entry,
                Placed {
                    path: served.page,
                    bytes,
                },
            ));
        }

        linked.push(Linked {
            gateway: Placed {
                path: gateway.stands(&foreign.name),
                bytes: gateway
                    .delivery
                    .bytes(&foreign.name, Target::Browser, &directory, store)?,
            },
            // A declaration the program never reaches is no hook a page could hold to a row.
            answers: foreign
                .answers
                .into_iter()
                .filter(|(declaration, _)| foreigns.get(declaration).is_some())
                .collect(),
            statics,
            package,
            row: foreign.name,
        });
    }

    unanswered(
        manifest,
        foreigns,
        &reached.elsewhere,
        Target::Browser,
        |name| {
            linked.iter().any(|row| {
                row.answers
                    .iter()
                    .any(|(declaration, _)| declaration == name)
            })
        },
    )?;

    Ok(linked)
}
