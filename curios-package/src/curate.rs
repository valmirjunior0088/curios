//! The store's tool, and the toolchain's only network actor.
//!
//! **Why fetching lives here and nowhere else.** Opacity is a compiler property — it compares a `rev` and never interprets one — and interpretation is exactly what turning a revision into bytes requires. Putting it here is a decision rather than a concession, because acceptance is by hash: *any* transport may deliver the bytes, an untrusted one included, and a delivery that fails its hash is refused regardless of who fetched it. A separate fetcher layered above this would double the tooling for zero integrity gain. The compiler itself never fetches.
//!
//! That same argument picks the transport: `git` on the developer's own machine, shelled out to. A second implementation of the git protocol vendored into this workspace would buy nothing the hash does not already guarantee, while owning an authentication story — ssh keys, credential helpers, private remotes — that the one already installed handles.
//!
//! **A delivered tree is source, and `.git` is not source.** It is removed before the tree is hashed or placed: a fresh clone's object store differs run to run, so leaving it in would make the criterion unreproducible and the store key meaningless. This is not something the scheme could state, because the scheme hashes whatever it is handed; it is a fact about what `curate` hands it.

#[cfg(test)]
mod tests;

use {
    crate::{Dependency, Governing, MANIFEST, Manifest, Package, Store, TreeHash},
    std::{
        collections::{BTreeMap, BTreeSet},
        fmt, fs,
        path::{Path, PathBuf},
        process::Command,
    },
};

/// One pin to realize: where to get it, and what to accept.
#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct Pin {
    pub name: String,
    pub url: String,
    pub rev: String,
    pub hash: TreeHash,
}

impl fmt::Display for Pin {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{} from {} at {}", self.name, self.url, self.rev)
    }
}

/// Bring every pin the governing package reaches into the store.
///
/// Iterated to a fixed point rather than walked once, because a dependency's own dependencies cannot be read until it is there: each round materializes what the manifests currently reachable declare, and the next round sees further. A round that fetches nothing new is the end of it — and a round that fetches without revealing anything is impossible, since a fetched tree either has a manifest or is refused for having none.
pub fn materialize(governing: &Governing) -> Result<Vec<Pin>, String> {
    let store = governing.store();
    let mut fetched = Vec::new();

    loop {
        let wanted = pins(governing)?;
        let absent = wanted
            .into_iter()
            .filter(|pin| !store.src(&pin.hash).is_dir())
            .collect::<Vec<_>>();

        if absent.is_empty() {
            return Ok(fetched);
        }

        for pin in absent {
            fetch(&store, &pin)?;
            fetched.push(pin);
        }
    }
}

/// Every pin reachable through manifests that are readable *now*.
///
/// Unreadable ones are not an error here: a dependency that has not been materialized is precisely what this walk exists to discover, and refusing on it would make the first round the only round.
fn pins(governing: &Governing) -> Result<BTreeSet<Pin>, String> {
    let mut pins = BTreeSet::new();
    let mut seen = BTreeSet::new();
    let mut frontier = vec![(governing.package.clone(), governing.directory.clone())];

    while let Some((package, directory)) = frontier.pop() {
        if !seen.insert(package.name.clone()) {
            continue;
        }

        for (name, row) in &package.dependencies {
            let resolved = match row {
                Dependency::Git { url, rev, hash } => {
                    pins.insert(Pin {
                        name: name.clone(),
                        url: url.clone(),
                        rev: rev.clone(),
                        hash: hash.clone(),
                    });

                    governing.store().src(hash)
                }
                Dependency::Path { path } => directory.join(path),
                // A marker resolves through the umbrella, which is on disk already: `order` is what refuses a mismatched one, and this walk only needs somewhere further to look.
                Dependency::Member | Dependency::Catalog => match reachable(governing, name, row) {
                    Some(directory) => directory,
                    None => continue,
                },
            };

            if let Ok(Manifest::Package(package)) = Manifest::from_path(&resolved.join(MANIFEST)) {
                frontier.push((package, resolved));
            }
        }
    }

    Ok(pins)
}

/// Where a marker points, when the umbrella that answers it says so.
fn reachable(governing: &Governing, name: &str, row: &Dependency) -> Option<PathBuf> {
    let umbrella = governing.umbrella.as_ref()?;

    match row {
        Dependency::Catalog => match umbrella.catalog.get(name)? {
            Dependency::Git { hash, .. } => Some(governing.store().src(hash)),
            Dependency::Path { path } => Some(governing.root.join(path)),
            _ => None,
        },
        _ => umbrella
            .members
            .iter()
            .map(|member| governing.root.join(member))
            .find(|directory| {
                matches!(
                    Manifest::from_path(&directory.join(MANIFEST)),
                    Ok(Manifest::Package(package)) if package.name == name
                )
            }),
    }
}

/// Fetch `pin`, verify it, and place it — in that order, and only in that order.
///
/// The tree is hashed where it lands temporarily and moved into the store only once it has been accepted, so a failed or interrupted fetch cannot leave a directory the store would later read as a verified delivery.
fn fetch(store: &Store, pin: &Pin) -> Result<(), String> {
    let scratch = store.src(&pin.hash).with_extension("fetching");
    let _ = fs::remove_dir_all(&scratch);
    fs::create_dir_all(&scratch)
        .map_err(|error| format!("failed to create {}: {error}", scratch.display()))?;

    let outcome = deliver(&scratch, pin).and_then(|()| accept(&scratch, store, pin));

    if outcome.is_err() {
        let _ = fs::remove_dir_all(&scratch);
    }

    outcome
}

/// Ask `git` for the revision, into `scratch`.
fn deliver(scratch: &Path, pin: &Pin) -> Result<(), String> {
    git(scratch, &["init", "--quiet"])?;
    git(scratch, &["remote", "add", "origin", &pin.url])?;

    // A shallow fetch of one revision is what this wants, and a server may decline to serve one by object name; the deep fetch is the fallback rather than the default because the whole history is the thing worth not transferring.
    if git(
        scratch,
        &["fetch", "--quiet", "--depth", "1", "origin", &pin.rev],
    )
    .is_err()
    {
        git(scratch, &["fetch", "--quiet", "origin"])?;
    }

    git(scratch, &["checkout", "--quiet", "--detach", &pin.rev])
        .or_else(|_| git(scratch, &["checkout", "--quiet", "--detach", "FETCH_HEAD"]))?;

    // Source is what was delivered; the object store is how it arrived. A fresh clone's differs run to run, so leaving it in would make the hash unreproducible and the store key meaningless.
    fs::remove_dir_all(scratch.join(".git"))
        .map_err(|error| format!("failed to drop the fetched repository's metadata: {error}"))
}

/// Verify the delivery and move it into the store.
fn accept(scratch: &Path, store: &Store, pin: &Pin) -> Result<(), String> {
    let delivered = TreeHash::of(scratch)?;

    if delivered != pin.hash {
        return Err(format!(
            "the dependency {:?} was fetched from {} at {}, and what arrived is not what it is pinned to\n  expected {}\n  delivered {delivered}",
            pin.name, pin.url, pin.rev, pin.hash
        ));
    }

    let placed = store.src(&pin.hash);
    if let Some(parent) = placed.parent() {
        fs::create_dir_all(parent)
            .map_err(|error| format!("failed to create {}: {error}", parent.display()))?;
    }

    // Another process may have placed the identical tree while this one was fetching. Content-addressed means the two are the same tree, so losing the race costs nothing but the work.
    match fs::rename(scratch, &placed) {
        Ok(()) => Ok(()),
        Err(_) if placed.is_dir() => {
            let _ = fs::remove_dir_all(scratch);

            Ok(())
        }
        Err(error) => Err(format!("failed to place {}: {error}", placed.display())),
    }
}

/// Run `git` in `directory`, with its own diagnostics on failure.
fn git(directory: &Path, arguments: &[&str]) -> Result<(), String> {
    let output = Command::new("git")
        .current_dir(directory)
        .args(arguments)
        .output()
        .map_err(|error| {
            format!(
                "`git {}` could not be run: {error}. `curate` is the only thing in this toolchain that fetches, and it fetches by asking the `git` already on this machine.",
                arguments.join(" ")
            )
        })?;

    match output.status.success() {
        true => Ok(()),
        false => Err(format!(
            "`git {}` failed: {}",
            arguments.join(" "),
            String::from_utf8_lossy(&output.stderr).trim()
        )),
    }
}

/// What the project declares and does not use, and what sits in it that nothing names.
///
/// Reports rather than refuses: none of it is wrong, and all of it is worth knowing. What is *not* here yet is the half that needs the compiler's own answer — a dependency declared but never named by any module, and a name resolved against no declaration — because the mount table knows precisely which prefixes a unit resolved against and nothing hands that over yet. Those two are the reason this returns a list rather than being folded into a check.
pub fn reconcile(governing: &Governing) -> Result<Vec<String>, String> {
    let mut reports = Vec::new();

    if let Some(umbrella) = &governing.umbrella {
        let mut referenced = BTreeSet::new();
        for member in &umbrella.members {
            let directory = governing.root.join(member);
            if let Ok(Manifest::Package(package)) = Manifest::from_path(&directory.join(MANIFEST)) {
                for (name, row) in &package.dependencies {
                    if matches!(row, Dependency::Catalog) {
                        referenced.insert(name.clone());
                    }
                }
            }
        }

        for name in umbrella.catalog.keys() {
            if !referenced.contains(name) {
                reports.push(format!(
                    "the catalog entry {:?} is referenced by no member, so nothing materializes it — activation lives in the package that names it",
                    name
                ));
            }
        }
    }

    reports.extend(inert(&governing.package, &governing.directory)?);

    Ok(reports)
}

/// The `.crs` files in a package that nothing enumerates.
///
/// A file nothing names is inert wherever it sits (law 1), which is the scratch-file freedom this design preserves by construction — so this reports rather than guesses, and the compiler never sees the file at all.
fn inert(package: &Package, directory: &Path) -> Result<Vec<String>, String> {
    let mut claimed = BTreeMap::new();
    claimed.insert(crate::LIBRARY.to_string(), ());

    for executable in &package.executables {
        claimed.insert(executable.path.to_string_lossy().into_owned(), ());
    }

    let header = directory.join(crate::LIBRARY);
    if header.is_file() {
        let library = curios_text::Module::from_path(&header)
            .map_err(|error| format!("{}: {}", header.display(), error.format()))?;

        for item in &library.items {
            if let curios_text::TopItem::Mod(declaration) = item {
                claimed.insert(format!("{}.crs", declaration.label), ());
            }
        }
    }

    let mut reports = Vec::new();
    let entries =
        fs::read_dir(directory).map_err(|error| format!("{}: {error}", directory.display()))?;

    for entry in entries {
        let entry = entry.map_err(|error| format!("{}: {error}", directory.display()))?;
        let name = entry.file_name().to_string_lossy().into_owned();

        if name.ends_with(".crs") && !claimed.contains_key(&name) {
            reports.push(format!(
                "{name} sits in the package and nothing names it, so nothing compiles it"
            ));
        }
    }

    reports.sort();

    Ok(reports)
}
