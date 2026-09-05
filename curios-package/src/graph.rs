//! The order a compilation folds its units in.
//!
//! **Membership organizes; dependency compiles** (law 3). The umbrella's tree decides where the store goes and what a marker may resolve to; only declared dependencies order compilation, and enumerating a member creates no edge. What comes out of here is a topological sort over the edges packages declared for themselves, with the governing package last.
//!
//! Exact pins mean there is nothing to solve, only to realize: a name resolves to one place or the resolution is refused. Every refusal names both parties — both dependents and both pins for a conflict, the whole chain for a cycle — and all of them fire before any package involved elaborates (law 4).

#[cfg(test)]
mod tests;

use {
    crate::{Dependency, Governing, MANIFEST, Manifest, Package, Snapshot, package_source},
    curios_text::RootSource,
    std::{
        collections::BTreeMap,
        path::{Path, PathBuf},
    },
};

/// Every unit the governing package reaches, in the order a compilation folds them: each package after everything it depends on, and the governing package itself last.
pub fn order(governing: &Governing) -> Result<Vec<RootSource>, String> {
    let mut walk = Walk {
        members: members(governing)?,
        governing,
        placed: BTreeMap::new(),
        open: Vec::new(),
        order: Vec::new(),
    };

    if let Some(source) = walk.enter(&governing.package, &governing.directory)? {
        walk.order.push(source);
    }

    Ok(walk.order)
}

/// Each member's declared name, and the directory its manifest sits in.
///
/// Read once, because every marker in the graph is answered against it: a `member` row asks whether this name is enumerated, and a direct pin asks the same question in order to be told it may not answer it itself.
fn members(governing: &Governing) -> Result<BTreeMap<String, PathBuf>, String> {
    let Some(umbrella) = &governing.umbrella else {
        return Ok(BTreeMap::new());
    };

    let mut members = BTreeMap::new();

    for member in &umbrella.members {
        let directory = governing.root.join(member);

        let Manifest::Package(package) = Manifest::from_path(&directory.join(MANIFEST))? else {
            return Err(format!(
                "the member {} declares an umbrella, and umbrellas do not nest",
                member.display()
            ));
        };

        if let Some(earlier) = members.insert(package.name.clone(), directory.clone()) {
            return Err(format!(
                "two members declare the name {:?}: {} and {}",
                package.name,
                earlier.display(),
                directory.display()
            ));
        }
    }

    Ok(members)
}

/// A depth-first walk over declared dependencies, accumulating the fold order behind it.
struct Walk<'a> {
    governing: &'a Governing,
    /// The umbrella's members by declared name — empty when no umbrella governs, which is what makes every marker a mismatch there.
    members: BTreeMap<String, PathBuf>,
    /// Where each canonical name resolved, and who said so. Both halves are the conflict refusal.
    placed: BTreeMap<String, Placed>,
    /// The names on the walk's own stack, which is the cycle.
    open: Vec<String>,
    /// The fold order, predecessors first.
    order: Vec<RootSource>,
}

/// Where one canonical name resolved, and what resolved it.
struct Placed {
    directory: PathBuf,
    /// The snapshot that fixed it, for a fetchable source. Live code pins nothing, so two live rows agree only by resolving to one place.
    snapshot: Option<Snapshot>,
    /// The package whose manifest said so.
    dependent: String,
}

impl Walk<'_> {
    /// Resolve everything `package` depends on into the order behind it, and hand back its own resolver — `None` when it has no library, since then it contributes no unit and only its programs exist.
    fn enter(&mut self, package: &Package, directory: &Path) -> Result<Option<RootSource>, String> {
        self.open.push(package.name.clone());

        for (name, row) in &package.dependencies {
            self.edge(package, directory, name, row)?;
        }

        self.open.pop();

        package_source(package, directory)
    }

    /// Follow one declared edge: check it against every other claim on that name, locate what it names, and walk it.
    fn edge(
        &mut self,
        dependent: &Package,
        from: &Path,
        name: &str,
        row: &Dependency,
    ) -> Result<(), String> {
        // The snapshot is read off the row, before anything is located: once the first dependent's pin is placed — which locates it, so its tree has to be there — a second dependent disagreeing about the name is refused whether or not its own tree has been materialized, which is what "before any of the three elaborates" has to mean.
        let snapshot = self.pinned(name, row);

        // Asked before anything else, because a name on the walk's own stack is a cycle *whether or not* it has already been placed. Checking it second let `b → c → b` past: `b` was placed on the way in, so the agreement branch below returned early and the walk emitted `c` before the `b` it depends on — a wrong fold order rather than a refusal, which is the shape a cycle takes when nobody looks for it.
        if self.open.iter().any(|open| open == name) {
            return Err(format!(
                "the dependencies cycle: {} depends on {:?} again",
                self.open
                    .iter()
                    .map(|open| format!("{open:?}"))
                    .collect::<Vec<_>>()
                    .join(" depends on "),
                name
            ));
        }

        if let Some(earlier) = self.placed.get(name) {
            if let (Some(earlier_snapshot), Some(snapshot)) = (&earlier.snapshot, &snapshot) {
                return match earlier_snapshot == snapshot {
                    // A diamond shares its point rather than duplicating it, which is the whole reason a package names itself.
                    true => Ok(()),
                    false => Err(format!(
                        "the dependency {:?} is pinned two ways: {} by {:?}, and {} by {:?}",
                        name,
                        describe(earlier_snapshot),
                        earlier.dependent,
                        describe(snapshot),
                        dependent.name
                    )),
                };
            }

            let directory = self.locate(dependent, from, name, row)?;

            return match earlier.directory == directory {
                true => Ok(()),
                false => Err(format!(
                    "the dependency {:?} resolves two ways: {} by {:?}, and {} by {:?}",
                    name,
                    earlier.directory.display(),
                    earlier.dependent,
                    directory.display(),
                    dependent.name
                )),
            };
        }

        let directory = self.locate(dependent, from, name, row)?;

        self.placed.insert(
            name.to_string(),
            Placed {
                directory: directory.clone(),
                snapshot,
                dependent: dependent.name.clone(),
            },
        );

        let Manifest::Package(package) = Manifest::from_path(&directory.join(MANIFEST))? else {
            return Err(format!(
                "the dependency {:?} resolves to {}, which declares an umbrella; a dependency is a package",
                name,
                directory.display()
            ));
        };

        // A package names itself and every consumer refers to it by that name (law 2). A row keyed by anything else would be the consumer choosing the prefix, which is exactly what makes a diamond duplicate instead of share.
        if package.name != name {
            return Err(format!(
                "the dependency {:?} resolves to {}, which declares itself {:?}; a package is referred to by the name it declares",
                name,
                directory.display(),
                package.name
            ));
        }

        // A dependency is something to import from, so a package with no library is nothing to depend *on* — refused here rather than surfacing as every one of its names being unbound.
        let Some(source) = self.enter(&package, &directory)? else {
            return Err(format!(
                "the dependency {:?} resolves to {}, which has no library; there is nothing there to import",
                name,
                directory.display()
            ));
        };

        self.order.push(source);

        Ok(())
    }

    /// The snapshot `row` pins — read through a `catalog` marker to the umbrella's row it names, so a catalogued pin and a direct one disagreeing about a name are refused as the two pins they are, before either is located, rather than as two store paths once both are. A marker nothing answers pins nothing here; [`Walk::point`] is where it is refused, naming what is missing.
    fn pinned(&self, name: &str, row: &Dependency) -> Option<Snapshot> {
        match row {
            Dependency::Catalog => self
                .governing
                .umbrella
                .as_ref()?
                .catalog
                .get(name)?
                .snapshot(),
            row => row.snapshot(),
        }
    }

    /// Where `row` points, as the filesystem names it.
    ///
    /// Canonical rather than as written, because a location is compared: `../left/../base` and `../right/../base` are one package, and a diamond that read them as two would compile its point twice and hand out two nominally distinct families spelled the same.
    fn locate(
        &self,
        dependent: &Package,
        from: &Path,
        name: &str,
        row: &Dependency,
    ) -> Result<PathBuf, String> {
        let subject = format!("the dependency {:?} of {:?}", name, dependent.name);

        let directory = self.point(dependent, from, name, row, &subject)?;

        directory
            .canonicalize()
            .map_err(|error| format!("{subject} resolves to {}: {error}", directory.display()))
    }

    /// The place `row` names, before the filesystem has been asked to agree it is one.
    fn point(
        &self,
        dependent: &Package,
        from: &Path,
        name: &str,
        row: &Dependency,
        subject: &str,
    ) -> Result<PathBuf, String> {
        match row {
            Dependency::Member => match self.members.get(name) {
                Some(directory) => Ok(directory.clone()),
                None => Err(format!(
                    "{subject} is `source = \"member\"`, and no governing umbrella enumerates a member declaring that name"
                )),
            },

            Dependency::Catalog => {
                if self.members.contains_key(name) {
                    return Err(format!(
                        "{subject} is `source = \"catalog\"`, but that name is a live member of the governing umbrella; a member is the only answer for its own name inside its tree"
                    ));
                }

                let row = self
                    .governing
                    .umbrella
                    .as_ref()
                    .and_then(|umbrella| umbrella.catalog.get(name))
                    .ok_or_else(|| {
                        format!(
                            "{subject} is `source = \"catalog\"`, and no governing umbrella's `[catalog]` holds that name"
                        )
                    })?;

                self.locate(dependent, &self.governing.root, name, row)
            }

            // A direct row is the consumer answering for a name its own umbrella already answers for. Refused in both directions, because a promotion that only half happened is the shape that compiles two of one package.
            _ if self.members.contains_key(name) => Err(format!(
                "{subject} is pinned directly, but that name is a live member of the governing umbrella; a member is the only answer for its own name inside its tree"
            )),

            Dependency::Path { path } => Ok(from.join(path)),

            // The store is where a fetchable dependency lives, and being *in* it is what "materialized" means — the hash it is filed under is the hash it was accepted against, so finding it there is the verification, already done.
            Dependency::Git { url, rev, hash } => {
                let tree = self.governing.store().source(hash);

                match tree.is_dir() {
                    true => Ok(tree),
                    false => Err(format!(
                        "{subject} is fetched from {url} at {rev}, and nothing has materialized it; run `curios curate`\n  the delivered tree must hash to {hash}"
                    )),
                }
            }
        }
    }
}

/// A snapshot as a refusal spells it.
fn describe(snapshot: &Snapshot) -> String {
    format!("`{}` ({})", snapshot.rev, snapshot.hash)
}
