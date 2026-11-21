//! What a project reaches, read off its manifests: every package its governing root holds, each member an umbrella enumerates or the one package, with the graph each folds, its executables and its tests, and every tree and foreign module those graphs pin. Which of what a filing keeps that is, and the taking away of the rest, are `curios-verdict`'s.

#[cfg(test)]
mod tests;

use {
    crate::{FileHash, Governing, MANIFEST, Manifest, TreeHash, order, pins, reachable},
    curios_common::{Qualifier, Report},
    curios_module::RootSource,
    std::{collections::BTreeSet, path::Path},
};

/// What the project at a governing root reaches: every compilation of it is one of a package the root holds, over that package's graph, and every tree and module a compilation reads of the cabinet is one those graphs pin.
pub struct Reach {
    /// Each package the root holds: every member an umbrella enumerates, or the one package.
    pub packages: Vec<Reached>,
    /// Every tree the packages' graphs pin, by the hash it was accepted against.
    pub trees: BTreeSet<TreeHash>,
    /// Every foreign module the packages' graphs fetch, by the hash it was accepted against.
    pub modules: BTreeSet<FileHash>,
}

/// One package a project's root holds, as its compilations are filed.
pub struct Reached {
    /// Its name, which its executables and its tests file their payloads under.
    pub name: String,
    /// Its graph in fold order, its own library last where it has one ([`order`]): what every compilation of it folds.
    pub units: Vec<RootSource>,
    /// What each of its executables' entries may name (`reachable`).
    pub declares: Vec<Qualifier>,
    /// Its executables, by the name each is run by.
    pub executables: Vec<String>,
}

impl Reach {
    /// What the project at `root` reaches. Refused wherever anything it reads is — a manifest, a member, a graph [`order`] refuses, a tree not yet curated among them — so what is in hand is the whole of what the project reaches, or nothing.
    pub fn of(root: &Path) -> Result<Self, Box<Report>> {
        let directories = match Manifest::from_path(&root.join(MANIFEST))? {
            Manifest::Package(_) => vec![root.to_path_buf()],
            Manifest::Umbrella(umbrella) => umbrella
                .members
                .iter()
                .map(|member| root.join(member))
                .collect(),
        };
        let mut reach = Self {
            packages: Vec::new(),
            trees: BTreeSet::new(),
            modules: BTreeSet::new(),
        };
        for directory in directories {
            let governing = Governing::at(&directory.join(MANIFEST), None)?;
            let pinned = pins(&governing)?;
            reach.trees.extend(pinned.trees);
            reach.modules.extend(pinned.modules);
            reach.packages.push(Reached {
                units: order(&governing)?,
                declares: reachable(&governing.package),
                executables: governing.package.executables.keys().cloned().collect(),
                name: governing.package.name,
            });
        }

        Ok(reach)
    }
}

impl Reached {
    /// Every name a payload of the package is filed under: each executable's own, and its tests', its library's and each executable's ([`tests_filed_as`]).
    pub fn payloads(&self) -> Vec<String> {
        self.executables
            .iter()
            .cloned()
            .chain([tests_filed_as(None)])
            .chain(
                self.executables
                    .iter()
                    .map(|executable| tests_filed_as(Some(executable))),
            )
            .collect()
    }
}

/// The name a package's tests are filed under, as an executable's payload is under its own: `tests/` for its library's, and `tests/<executable>` for its `executable`'s. An executable is named by an identifier, so none is filed under one of these.
pub fn tests_filed_as(executable: Option<&str>) -> String {
    format!("tests/{}", executable.unwrap_or_default())
}
