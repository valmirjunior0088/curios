//! What answers the `foreign` rows a program reaches, for each target: the foreign providers of its package graph that answer one of them, compiled natively into the modules a run links and an executable carries, or read for a browser into the gateways and files its page holds — and the refusal of a reached row nothing answers, by what would answer it.
//!
//! **Nothing is read where the run answers itself every row the program reaches** — the host's, and a test run's own ([`RunAs`]) — which is very nearly every program, since each reaches the standard library's rows and few any other: no manifest is walked and no file opened, so the common case pays one pass over its rows. A row answers a declaration of its own package alone, so none could answer for such a program.
//!
//! **The graph walked is the one of the package whose manifest declares the program**, an umbrella's member as a package standing alone: an umbrella's directory, where `.curios/` sits, declares no package to walk from. A loose program has no manifest and so no row: what it reaches of the standard library's rows the host answers, and a row of its own is refused, saying it has nowhere to name what would answer it.

#[cfg(test)]
mod tests;

use {
    curios_abi::{ForeignStore, RunAs, host_answers},
    curios_browser::{Linked, Placed},
    curios_common::{OsError, Report, Said},
    curios_native::{DeclaredModule, ForeignBindings, Unlinked, plugin_bindings},
    curios_package::{
        Cabinet, Governing, MANIFEST, Provider, Refused, Row, Target, declared_modules,
    },
    std::{fs, path::Path},
};

/// The foreign providers of a program's package graph that answer a declaration it reaches, split by whether they state one target's table.
#[derive(Default)]
struct Reached {
    /// Each provider stating the target's table.
    stating: Vec<Provider>,
    /// Each provider stating none.
    elsewhere: Vec<Provider>,
    /// The cabinet a fetched delivery is read from, where a graph was walked.
    cabinet: Option<Cabinet>,
}

/// The rows of the graph of the package `manifest` declares that answer a declaration among `foreigns`, split by whether they state `target`'s table, the graph read out of the cabinet `cabinet` names or the governing root's own — none walked where a run of `run`'s kind answers every one itself.
fn reached(
    manifest: Option<&Path>,
    foreigns: &ForeignStore,
    (target, run): (Target, RunAs),
    cabinet: Option<&Path>,
) -> Result<Reached, Box<Report>> {
    if foreigns
        .iter()
        .all(|function| host_answers(function.name()) || run.answers(function.name()))
    {
        return Ok(Reached::default());
    }

    let Some(manifest) = manifest else {
        return Ok(Reached::default());
    };

    let governing = Governing::at(manifest, cabinet)?;
    let mut reached = Reached {
        cabinet: Some(governing.cabinet.clone()),
        ..Reached::default()
    };

    // Only a row that answers a declaration the program reaches is read: the rest of the graph's are neither linked nor carried.
    for provider in declared_modules(&governing)? {
        if !provider
            .foreign
            .answers
            .keys()
            .any(|declaration| foreigns.get(declaration).is_some())
        {
            continue;
        }

        match provider.foreign.delivery(target).is_some() {
            true => reached.stating.push(provider),
            false => reached.elsewhere.push(provider),
        }
    }

    Ok(reached)
}

/// The first declaration among `foreigns` that nothing `answered` names and the host does not answer: what a page, beside which no engine stands to ask, is refused for before anything is written.
fn first_unanswered(foreigns: &ForeignStore, answered: impl Fn(&str) -> bool) -> Option<&str> {
    foreigns
        .iter()
        .map(|function| function.name())
        .find(|name| !answered(name) && !host_answers(name))
}

/// The refusal of `name`, a declaration nothing answers, by what would answer it among `providers`: a provider that answers it and states no table for `target`, which is the author's word that the target links nothing for it, or a provider in a manifest's `[foreign]`, which a loose program has none to write in.
fn unanswered(
    name: &str,
    providers: &[&Provider],
    manifest: Option<&Path>,
    target: Target,
) -> Box<Report> {
    let refused = Refused::of("no implementation");
    let spelled = target.spelled();

    match providers
        .iter()
        .find(|provider| provider.foreign.answers.contains_key(name))
    {
        Some(Provider {
            package,
            directory,
            name: row,
            ..
        }) => {
            let refused = refused
                .stating(format_args!(
                    "`{name}` is answered by the foreign row `{row}` of `/{package}`"
                ))
                .stating(format_args!(
                    "that row states no `[foreign.{row}.{spelled}]`"
                ))
                .stating(format_args!(
                    "the {spelled} target has nothing to link for it"
                ));
            // A gateway is tested natively through a module standing in for it, and against the gateway itself by a test serve, so the native refusal names both doors.
            let refused = match target {
                Target::Native => refused
                    .stating(format_args!("a `.wasm` standing in for its gateway, stated as `[foreign.{row}.native]`, is what `run` and `test` link"))
                    .stating("`test --target browser` runs its tests against the gateway"),
                Target::Browser => refused,
            };

            written(directory, row).refuse(refused)
        }
        None => {
            let refused = refused.stating(format_args!("`{name}` is declared `foreign`"));

            match manifest {
                Some(manifest) => refused
                    .stating("no `[foreign]` row of the package's manifests names an export that answers it")
                    .about_path(manifest),
                None => refused
                    .stating("a `foreign` is answered by a module that a `[foreign]` row of a manifest names")
                    .stating("a loose program has no manifest to write one in")
                    .about_named(name),
            }
        }
    }
}

/// The foreign row `row` of the package whose manifest sits in `directory`: where a refusal of what the row declares stands.
fn written(directory: &Path, row: &str) -> Row {
    Row::of(directory.join(MANIFEST), ["foreign", row])
}

/// The report of what linking refused of the program the package at `manifest` declares, for whoever links it past [`bindings_of`]: at the row that declares the module it is said of, read off the package's graph again, since a refusal is the path that can afford the walk.
pub fn unlinked(
    unlinked: Unlinked,
    manifest: Option<&Path>,
    cabinet: Option<&Path>,
) -> Box<Report> {
    let providers = manifest
        .and_then(|manifest| Governing::at(manifest, cabinet).ok())
        .and_then(|governing| declared_modules(&governing).ok())
        .unwrap_or_default();

    refused(
        unlinked,
        &providers.iter().collect::<Vec<_>>(),
        manifest,
        Target::Native,
    )
}

/// The report of what linking refused for `target`: of a declaration nothing answers, by what among `providers` would have ([`unanswered`]); otherwise at the row that declares the module it is said of, found among `providers` by its package and its name, and about the manifest, or the program where it has none.
fn refused(
    unlinked: Unlinked,
    providers: &[&Provider],
    manifest: Option<&Path>,
    target: Target,
) -> Box<Report> {
    let Unlinked {
        tag,
        facts,
        module,
        unanswered: declaration,
    } = unlinked;
    if let Some(declaration) = declaration {
        return unanswered(&declaration, providers, manifest, target);
    }
    let mut said = Said::line(tag);
    for fact in facts {
        said.stating(fact);
    }
    let refused = Refused::saying(said);
    let declared = module.and_then(|(package, name)| {
        providers
            .iter()
            .find(|provider| provider.package == package && provider.name == name)
            .map(|provider| written(&provider.directory, &provider.name))
    });

    match (declared, manifest) {
        (Some(row), _) => row.refuse(refused),
        (None, Some(manifest)) => refused.about_path(manifest),
        (None, None) => refused.about_named("the program"),
    }
}

/// What answers a program's rows natively ahead of the host, for a run of `run`'s kind: each module its packages declare for the rows it reaches, accepted against the hash its row pins and compiled here, where Cranelift is, to the machine code `run`, `test` and `compile` alike resolve it from. Shared by the three, so what a bundled executable carries and what a run links are one resolution, and a reached row nothing answers — neither a module nor what the run answers itself — is refused before anything runs or is written.
pub fn bindings_of(
    manifest: Option<&Path>,
    foreigns: &ForeignStore,
    cabinet: Option<&Path>,
    run: RunAs,
) -> Result<ForeignBindings, Box<Report>> {
    let reached = reached(manifest, foreigns, (Target::Native, run), cabinet)?;

    let mut modules = Vec::new();
    for Provider {
        package,
        directory,
        name,
        foreign,
    } in &reached.stating
    {
        let native = foreign
            .native
            .as_ref()
            .expect("a row stating the native table delivers a module");
        let cabinet = reached
            .cabinet
            .as_ref()
            .expect("a walked graph has a cabinet");

        let code = curios_cranelift::compile(&native.bytes(
            name,
            Target::Native,
            directory,
            cabinet,
        )?)
        .map_err(|error| {
            written(directory, name).refuse(
                Refused::of("invalid module")
                    .stating(format_args!(
                        "the module `{name}` of `/{package}` is no WebAssembly module this engine accepts"
                    ))
                    .stating(error),
            )
        })?;

        modules.push(DeclaredModule {
            package: package.clone(),
            code,
            name: name.clone(),
            answers: foreign.answers.clone(),
        });
    }

    let providers = reached
        .stating
        .iter()
        .chain(&reached.elsewhere)
        .collect::<Vec<_>>();
    let bindings = plugin_bindings(foreigns, modules)
        .map_err(|unlinked| refused(unlinked, &providers, manifest, Target::Native))?;
    // Linked as the run links, entering nothing: whether each reached row is answered, and at its declaration's types, is the engine's one judgment, and what it refuses is worded here from what the package graph says of the row.
    bindings
        .check(foreigns, run)
        .map_err(|unlinked| refused(unlinked, &providers, manifest, Target::Native))?;

    Ok(bindings)
}

/// What a browser links for a program's rows ahead of the host, for a page of `run`'s kind: each gateway its packages declare for the rows it reaches, accepted against the hash its row pins, with the declarations of the program it answers and every file its `static` table serves, read here once. A reached row nothing answers — neither a gateway, the harness, nor the page's loader, which answers a test page's own row — is refused before anything is written.
pub fn linked_of(
    manifest: Option<&Path>,
    foreigns: &ForeignStore,
    cabinet: Option<&Path>,
    run: RunAs,
) -> Result<Vec<Linked>, Box<Report>> {
    let reached = reached(manifest, foreigns, (Target::Browser, run), cabinet)?;

    let mut linked = Vec::new();
    for Provider {
        package,
        directory,
        name,
        foreign,
    } in reached.stating
    {
        let gateway = foreign
            .browser
            .as_ref()
            .expect("a row stating the browser table delivers a gateway");
        let cabinet = reached
            .cabinet
            .as_ref()
            .expect("a walked graph has a cabinet");

        let mut statics = Vec::new();
        for served in gateway.served(&name, &directory)? {
            let bytes = fs::read(&served.file).map_err(|error| {
                written(&directory, &name).refuse(
                    Refused::of("unreadable file")
                        .stating(format_args!(
                            "the static entry `{}` of the foreign row `{name}` reads {}",
                            served.entry,
                            served.file.display()
                        ))
                        .stating(OsError(&error)),
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
                path: gateway.stands(&name),
                bytes: gateway
                    .delivery
                    .bytes(&name, Target::Browser, &directory, cabinet)?,
            },
            // A declaration the program never reaches is no hook a page could hold to a row.
            answers: foreign
                .answers
                .into_iter()
                .filter(|(declaration, _)| foreigns.get(declaration).is_some())
                .collect(),
            statics,
            package,
            row: name,
        });
    }

    let answered = |name: &str| {
        run.answers(name)
            || linked.iter().any(|row| {
                row.answers
                    .iter()
                    .any(|(declaration, _)| declaration == name)
            })
    };
    if let Some(name) = first_unanswered(foreigns, answered) {
        return Err(unanswered(
            name,
            &reached.elsewhere.iter().collect::<Vec<_>>(),
            manifest,
            Target::Browser,
        ));
    }

    Ok(linked)
}
