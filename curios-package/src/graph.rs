//! The order a compilation folds its units in.
//!
//! **Membership organizes; dependency compiles** (law 3). The umbrella's tree decides where the store goes and what a marker may resolve to; only declared dependencies order compilation, and enumerating a member creates no edge. What comes out of here is a topological sort over the edges packages declared for themselves, with the governing package last.
//!
//! Exact pins mean there is nothing to solve, only to realize: a name resolves to one place or the resolution is refused. Every refusal names both parties — both dependents and both pins for a conflict, the whole chain for a cycle — and all of them fire before any package involved elaborates (law 4).

#[cfg(test)]
mod tests;

use {
    crate::{
        Dependency, Governing, MANIFEST, Manifest, Package, Refused, Snapshot, package_source,
    },
    curios_common::Report,
    curios_module::RootSource,
    std::{
        collections::BTreeMap,
        path::{Path, PathBuf},
    },
};

/// Every unit the governing package reaches, in the order a compilation folds them: each package after everything it depends on, and the governing package itself last.
pub fn order(governing: &Governing) -> Result<Vec<RootSource>, Box<Report>> {
    let mut walk = Walk {
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

/// A depth-first walk over declared dependencies, accumulating the fold order behind it.
struct Walk<'a> {
    governing: &'a Governing,
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
    fn enter(
        &mut self,
        package: &Package,
        directory: &Path,
    ) -> Result<Option<RootSource>, Box<Report>> {
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
    ) -> Result<(), Box<Report>> {
        let written = self.written(from, name);

        // The snapshot is read off the row, before anything is located: once the first dependent's pin is placed — which locates it, so its tree has to be there — a second dependent disagreeing about the name is refused whether or not its own tree has been materialized, which is what "before any of the three elaborates" has to mean.
        let snapshot = self.pinned(name, row);

        // Asked before anything else, because a name on the walk's own stack is a cycle *whether or not* it has already been placed. Checked second, `b → c → b` would get past: `b` is placed on the way in, so the agreement branch below would return early and the walk emit `c` before the `b` it depends on — a wrong fold order rather than a refusal, which is the shape a cycle takes when nobody looks for it.
        if self.open.iter().any(|open| open == name) {
            return Err(written.refuse(
                self.open
                    .windows(2)
                    .fold(Refused::of("dependency cycle"), |refused, pair| {
                        refused.stating(format_args!("`{}` depends on `{}`", pair[0], pair[1]))
                    })
                    .stating(format_args!(
                        "`{}` depends on `{name}` again",
                        dependent.name
                    )),
            ));
        }

        if let Some(earlier) = self.placed.get(name) {
            match (&earlier.snapshot, &snapshot) {
                (Some(earlier_snapshot), Some(snapshot)) => {
                    return match earlier_snapshot == snapshot {
                        // A diamond shares its point rather than duplicating it, which is the whole reason a package names itself.
                        true => Ok(()),
                        false => Err(written.refuse(
                            Refused::of("conflicting pins")
                                .stating(format_args!("`{name}` is pinned two ways"))
                                .stating(format_args!(
                                    "to {} by `{}`",
                                    describe(earlier_snapshot),
                                    earlier.dependent
                                ))
                                .stating(format_args!(
                                    "to {} by `{}`",
                                    describe(snapshot),
                                    dependent.name
                                )),
                        )),
                    };
                }

                // A pin and a live row disagree about what the name *is*, and that is the fact to state. Comparing their locations instead would find two directories, one of them the store's — the right directory and the wrong fact, sending the reader after a path nobody wrote. The live side is named by where its row points, not by locating it: the disagreement stands whether or not that place exists.
                (Some(pinned), None) => {
                    let live = self.point(dependent, from, name, row, &written)?;

                    return Err(written.refuse(
                        Refused::of("conflicting pins")
                            .stating(format_args!(
                                "`{name}` is pinned to {} by `{}`",
                                describe(pinned),
                                earlier.dependent
                            ))
                            .stating(format_args!(
                                "and taken live by `{}` from {}",
                                dependent.name,
                                live.display()
                            ))
                            .stating("a name is pinned or live, never both"),
                    ));
                }
                (None, Some(pinned)) => {
                    return Err(written.refuse(
                        Refused::of("conflicting pins")
                            .stating(format_args!(
                                "`{name}` is pinned to {} by `{}`",
                                describe(pinned),
                                dependent.name
                            ))
                            .stating(format_args!(
                                "and taken live by `{}` from {}",
                                earlier.dependent,
                                earlier.directory.display()
                            ))
                            .stating("a name is pinned or live, never both"),
                    ));
                }

                (None, None) => {}
            }

            let directory = self.locate(dependent, from, name, row, &written)?;

            return match earlier.directory == directory {
                true => Ok(()),
                false => Err(written.refuse(
                    Refused::of("ambiguous dependency")
                        .stating(format_args!("`{name}` resolves two ways"))
                        .stating(format_args!(
                            "to {} by `{}`",
                            earlier.directory.display(),
                            earlier.dependent
                        ))
                        .stating(format_args!(
                            "to {} by `{}`",
                            directory.display(),
                            dependent.name
                        )),
                )),
            };
        }

        let directory = self.locate(dependent, from, name, row, &written)?;

        self.placed.insert(
            name.to_string(),
            Placed {
                directory: directory.clone(),
                snapshot,
                dependent: dependent.name.clone(),
            },
        );

        // Asked before the read, for the reason `members` asks: the read's refusal names the file it could not open and nothing else, and the reader has to be sent to the row that pointed there. A fetched tree with no manifest lands here too — this is the refusal `curate`'s doc promises for it.
        let manifest = directory.join(MANIFEST);
        if !manifest.is_file() {
            return Err(written.refuse(
                Refused::of("not a package")
                    .stating(format_args!("`{name}` resolves to {}", directory.display()))
                    .stating(format_args!("no `{MANIFEST}` sits there"))
                    .stating("a dependency is a package"),
            ));
        }

        let Manifest::Package(package) = Manifest::from_path(&manifest)? else {
            return Err(written.refuse(
                Refused::of("not a package")
                    .stating(format_args!("`{name}` resolves to {}", directory.display()))
                    .stating("the manifest there declares an umbrella")
                    .stating("a dependency is a package"),
            ));
        };

        // A package names itself and every consumer refers to it by that name (law 2). A row keyed by anything else would be the consumer choosing the prefix, which is exactly what makes a diamond duplicate instead of share.
        if package.name != name {
            return Err(written.refuse(
                Refused::of("name mismatch")
                    .stating(format_args!("`{name}` resolves to {}", directory.display()))
                    .stating(format_args!(
                        "that package declares itself `{}`",
                        package.name
                    ))
                    .stating("a package is referred to by the name it declares"),
            ));
        }

        // A dependency is something to import from, so a package with no library is nothing to depend *on* — refused here rather than surfacing as every one of its names being unbound.
        let Some(source) = self.enter(&package, &directory)? else {
            return Err(written.refuse(
                Refused::of("no library")
                    .stating(format_args!("`{name}` resolves to {}", directory.display()))
                    .stating("it has no library, so there is nothing there to import"),
            ));
        };

        self.order.push(source);

        Ok(())
    }

    /// Where the row `name` of the package in `from` is written: its `[dependencies]`, in the manifest that was read for the governing package and in the one beside every other.
    fn written<'n>(&self, from: &Path, name: &'n str) -> Written<'n> {
        Written {
            manifest: match from == self.governing.directory {
                true => self.governing.manifest.clone(),
                false => from.join(MANIFEST),
            },
            table: "dependencies",
            name,
        }
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

    /// Where `row` points, as the filesystem names it. `written` is where the row stands, for a refusal, and it is the caller's to say because the caller knows which file holds the row: a dependency row sits in the dependent's manifest, and the catalog row it may name sits in the umbrella's.
    ///
    /// Canonical rather than as written, because a location is compared: `../left/../base` and `../right/../base` are one package, and a diamond that read them as two would compile its point twice and hand out two nominally distinct families spelled the same.
    fn locate(
        &self,
        dependent: &Package,
        from: &Path,
        name: &str,
        row: &Dependency,
        written: &Written<'_>,
    ) -> Result<PathBuf, Box<Report>> {
        let directory = self.point(dependent, from, name, row, written)?;

        directory.canonicalize().map_err(|error| {
            written.refuse(
                Refused::of("unreadable directory")
                    .stating(format_args!("`{name}` resolves to {}", directory.display()))
                    .stating(error),
            )
        })
    }

    /// The place `row` names, before the filesystem has been asked to agree it is one.
    fn point(
        &self,
        dependent: &Package,
        from: &Path,
        name: &str,
        row: &Dependency,
        written: &Written<'_>,
    ) -> Result<PathBuf, Box<Report>> {
        // A marker nothing answers is one of two faults, and the refusal has to say which: an umbrella governs and its list lacks the name, or no umbrella governs — one above may well list the name, and what it omits is the package asking. Told the first when the second is true, a reader checks the entry that is there and misses the one that is not.
        let ungoverned = |marker: &str| {
            written.refuse(
                Refused::of("no umbrella")
                    .stating(format_args!("this row is `source = \"{marker}\"`"))
                    .stating(format_args!(
                        "no umbrella governs `{}`",
                        dependent.name
                    ))
                    .stating("a marker is answered by the umbrella whose `members` list the package that wrote it"),
            )
        };

        match row {
            Dependency::Member => {
                match (self.governing.members.get(name), &self.governing.umbrella) {
                    (Some(directory), _) => Ok(directory.clone()),
                    (None, None) => Err(ungoverned("member")),
                    (None, Some(_)) => Err(written.refuse(
                        Refused::of("unknown member")
                            .stating("this row is `source = \"member\"`")
                            .stating("the governing umbrella lists no member declaring that name"),
                    )),
                }
            }

            Dependency::Catalog => {
                if self.governing.members.contains_key(name) {
                    return Err(written.refuse(
                        Refused::of("shadowed by a member")
                            .stating("this row is `source = \"catalog\"`")
                            .stating(format_args!(
                                "`{name}` is a live member of the governing umbrella"
                            ))
                            .stating(
                                "a member is the only answer for its own name inside its tree",
                            ),
                    ));
                }

                let Some(umbrella) = &self.governing.umbrella else {
                    return Err(ungoverned("catalog"));
                };

                let row = umbrella.catalog.get(name).ok_or_else(|| {
                    written.refuse(
                        Refused::of("unknown catalog row")
                            .stating("this row is `source = \"catalog\"`")
                            .stating(
                                "the governing umbrella's `[catalog]` holds no row of that name",
                            ),
                    )
                })?;

                // The row being located is the umbrella's now, so the refusal stands in the umbrella's manifest: the member wrote a marker and no path, and a refusal at the member's row sends the reader to a file that holds nothing to fix.
                let catalogued = Written {
                    manifest: self.governing.root.join(MANIFEST),
                    table: "catalog",
                    name,
                };

                self.locate(dependent, &self.governing.root, name, row, &catalogued)
            }

            // A direct row is the consumer answering for a name its own umbrella already answers for. Refused in both directions, because a promotion that only half happened is the shape that compiles two of one package.
            _ if self.governing.members.contains_key(name) => Err(written.refuse(
                Refused::of("shadowed by a member")
                    .stating("this row states its own source")
                    .stating(format_args!(
                        "`{name}` is a live member of the governing umbrella"
                    ))
                    .stating("a member is the only answer for its own name inside its tree"),
            )),

            Dependency::Path { path } => Ok(from.join(path)),

            // The store is where a fetchable dependency lives, and being *in* it is what "materialized" means — the hash it is filed under is the hash it was accepted against, so finding it there is the verification, already done.
            Dependency::Git { url, rev, hash } => {
                let tree = self.governing.store().source(hash);

                match tree.is_dir() {
                    true => Ok(tree),
                    false => Err(written.refuse(
                        Refused::of("not fetched")
                            .stating(format_args!("`{name}` is fetched from {url} at {rev}"))
                            .stating("nothing has materialized it")
                            .stating(format_args!("the delivered tree must hash to {hash}"))
                            .trying("curios curate"),
                    )),
                }
            }
        }
    }
}

/// Where a dependency's row is written: the manifest that holds it, the table it sits in, and the name it is filed under. A refusal of the row stands there.
struct Written<'a> {
    manifest: PathBuf,
    table: &'static str,
    name: &'a str,
}

impl Written<'_> {
    /// The report of `refused`, at this row.
    fn refuse(&self, refused: Refused) -> Box<Report> {
        refused
            .entry([self.table, self.name])
            .located_at(&self.manifest)
    }
}

/// A snapshot as a refusal spells it.
fn describe(snapshot: &Snapshot) -> String {
    format!("`{}` ({})", snapshot.rev, snapshot.hash)
}
