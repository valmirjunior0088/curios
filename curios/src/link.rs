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
    curios_native::{DeclaredModule, ForeignBindings, plugin_bindings},
    curios_package::{Foreign, Governing, Store, Target, declared_modules},
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
    /// Each row stating none: the package that declares it, its name, and what it answers.
    elsewhere: Vec<(String, String, Vec<String>)>,
    /// The store a fetched delivery is read from, where a graph was walked.
    store: Option<Store>,
}

/// The rows of the graph of the package `manifest` declares that answer a declaration among `foreigns`, split by whether they state `target`'s table.
fn reached(
    manifest: Option<&Path>,
    foreigns: &ForeignStore,
    target: Target,
) -> Result<Reached, String> {
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
    elsewhere: &[(String, String, Vec<String>)],
    target: Target,
    answered: impl Fn(&str) -> bool,
) -> Result<(), String> {
    let Some(name) = foreigns
        .iter()
        .map(|function| function.name())
        .find(|name| !answered(name) && !host_answers(name))
    else {
        return Ok(());
    };

    let target = target.spelled();

    Err(
        match elsewhere
            .iter()
            .find(|(_, _, answers)| answers.iter().any(|answer| answer == name))
        {
            Some((package, row, _)) => format!(
                "`{name}` is answered by the foreign row {row:?} of {package}, which states no `[foreign.{target}]`: the {target} target has nothing to link for it"
            ),
            None => match manifest {
                Some(_) => format!(
                    "`{name}` has no implementation: no `[[foreign]]` row of the package's manifests names an export that answers it"
                ),
                None => format!(
                    "`{name}` has no implementation: a `foreign` is answered by a module that a `[[foreign]]` row of a manifest names, and a loose program has no manifest to write one in"
                ),
            },
        },
    )
}

/// What answers a program's rows natively ahead of the host: each module its packages declare for the rows it reaches, accepted against the hash its row pins and compiled here, where Cranelift is, to the machine code `run`, `test` and `compile` alike resolve it from. Shared by the three, so what a bundled executable carries and what a run links are one resolution, and a reached row nothing answers is refused before anything runs or is written.
pub(crate) fn bindings_of(
    manifest: Option<&Path>,
    foreigns: &ForeignStore,
) -> Result<ForeignBindings, String> {
    let reached = reached(manifest, foreigns, Target::Native)?;

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
            format!(
                "the module {:?} of {package} is not a WebAssembly module this engine accepts: {error}",
                foreign.name
            )
        })?;

        modules.push(DeclaredModule {
            package,
            code,
            name: foreign.name,
            answers: foreign.answers,
        });
    }

    let bindings = plugin_bindings(foreigns, modules)?;
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
pub(crate) fn linked_of(
    manifest: Option<&Path>,
    foreigns: &ForeignStore,
) -> Result<Vec<Linked>, String> {
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
                format!(
                    "the static entry {:?} of the foreign row {:?} reads {}: {error}",
                    served.entry,
                    foreign.name,
                    served.file.display()
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
