//! A project's own cabinet, collected to what the project reaches ([`Reach`]): an object for every unit its packages' graphs fold and for every executable's own declarations, built and tested; a payload for every executable and every test subject, on every target; every tree and foreign module its manifests pin; and nothing else, of the cabinet or of `.curios/`. A filing command collects it as it ends, and the `wonder` server after a check that filed.
//!
//! **A project's own alone.** A cabinet named for every project a run sees (`CURIOS_CABINET`) is filed into by projects no run knows of, so it is collected within each address alone (`Packs::collect`), and no caller hands one here.
//!
//! **Nothing where the reach is not whole.** A manifest refused, a member missing or a tree not yet curated leaves the cabinet as it stands, since a reach read in part would take away what the part not read reaches.
//!
//! **What no family holds goes too.** An entry of `.curios/` or of the cabinet that is none of the running compiler's families — what a layout moved away from left behind, a family an older compiler kept — is taken away with the rest, so a move of the layout needs no migration. A name holding a dot is a staging, the extension or the prefix of what a filer, a fetch or a pin has yet to rename into place, and stays until it is [`STALE`](crate::STALE): every key a family files under is a digest, an address, a scheme's or a target's name or a family's own, none of which holds a dot.

#[cfg(test)]
mod tests;

use {
    crate::{entry_address, keep, mounted_address, payload_address},
    curios_module::{Compiled, UnitSource},
    curios_package::{Cabinet, Products, Reach, Reached, Target, compiler},
    curios_pipeline::fold_sources,
    std::{
        collections::BTreeSet,
        path::{Path, PathBuf},
        time::SystemTime,
    },
};

/// Take away from the own cabinet of the project at `root`, and from its `.curios/`, everything the project does not reach, a staging only once it is a day old at `now` (`STALE`): nothing where what it reaches cannot be read whole ([`Reach::of`]).
pub fn collect_project(root: &Path, now: SystemTime) {
    let Ok(reach) = Reach::of(root) else {
        return;
    };
    let cabinet = Cabinet::of(root, None);
    let products = Products::at(root.to_path_buf());
    let families = cabinet.families();

    keep(
        &products.directory(),
        products
            .families()
            .into_iter()
            .chain([cabinet.directory().to_path_buf()]),
        now,
    );
    keep(cabinet.directory(), cabinet.families().all(), now);
    // A compiler that cannot name itself filed no object, and has none of its own to tell from the rest.
    if let Some(compiler) = compiler(&cabinet) {
        let payloads = reach.packages.iter().flat_map(payloads).collect::<Vec<_>>();
        keep_nested(
            &families.objects,
            reach
                .packages
                .iter()
                .flat_map(addresses)
                .chain(payloads.iter().cloned())
                .map(|address| cabinet.object(&compiler, &address)),
            now,
        );
        // An executable's object holds a payload for each target, and nothing a target does not name.
        for address in &payloads {
            keep(
                &cabinet.object(&compiler, address),
                Target::ALL.map(|target| cabinet.payload_for(&compiler, target, address)),
                now,
            );
        }
    }
    keep_nested(
        &families.sources,
        reach.trees.iter().map(|tree| cabinet.source(tree)),
        now,
    );
    keep_nested(
        &families.foreign,
        reach.modules.iter().map(|module| cabinet.foreign(module)),
        now,
    );
}

/// Every address a compilation of `package` files a unit at: each unit of its graph, and each of its executables' own declarations, built and tested, as its fold addresses them.
fn addresses(package: &Reached) -> Vec<String> {
    let sources = fold_sources(&package.units);
    let stand_in = sources.first().and_then(UnitSource::stand_in);
    let units = sources
        .iter()
        .flat_map(|source| [mounted_address(source), mounted_address(&source.other())]);
    let entries = package.executables.iter().flat_map(|executable| {
        [Compiled::Built, Compiled::Tested].map(|compiled| {
            entry_address(
                &package.name,
                executable,
                Some(package.declares.as_slice()),
                compiled,
                stand_in,
            )
        })
    });

    units.chain(entries).collect()
}

/// The address each payload of `package` is filed at, whatever its target.
fn payloads(package: &Reached) -> Vec<String> {
    package
        .payloads()
        .iter()
        .map(|name| payload_address(&package.name, name))
        .collect()
}

/// Take away every entry of the family at `family` but `kept`, each two levels down, under its hash scheme's directory or its compiler's, and every such directory holding none of them, a staging only once it is [`STALE`](crate::STALE) at `now`.
fn keep_nested(family: &Path, kept: impl IntoIterator<Item = PathBuf>, now: SystemTime) {
    let kept = kept.into_iter().collect::<BTreeSet<_>>();
    let holding = kept
        .iter()
        .filter_map(|path| path.parent())
        .map(Path::to_path_buf)
        .collect::<BTreeSet<_>>();

    keep(family, holding.iter().cloned(), now);
    for directory in &holding {
        keep(
            directory,
            kept.iter()
                .filter(|path| path.parent() == Some(directory.as_path()))
                .cloned(),
            now,
        );
    }
}
