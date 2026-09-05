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
    crate::{Dependency, Governing, MANIFEST, Manifest, Snapshot, Store, TreeHash},
    std::{
        collections::BTreeSet,
        fmt, fs,
        path::{Path, PathBuf},
        process::Command,
    },
};

/// One package to bring into the store: which one, where from, and the snapshot that decides what to accept.
///
/// The transport sits here rather than in [`Snapshot`] because only a fetch needs it. What two dependents must agree on is the snapshot alone, so a mirror and its origin are one acquisition's worth of difference and no conflict at all.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Acquisition {
    pub name: String,
    pub url: String,
    pub snapshot: Snapshot,
}

impl Acquisition {
    /// What the delivered tree must hash to, which is also where it is filed.
    fn hash(&self) -> &TreeHash {
        &self.snapshot.hash
    }
}

impl fmt::Display for Acquisition {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "{} from {} at {}",
            self.name, self.url, self.snapshot.rev
        )
    }
}

/// Bring every snapshot the governing package reaches into the store.
///
/// Iterated to a fixed point rather than walked once, because a dependency's own dependencies cannot be read until it is there: each round materializes what the manifests currently reachable declare, and the next round sees further. A round that fetches nothing new is the end of it — and a round that fetches without revealing anything is impossible, since a fetched tree either has a manifest or is refused for having none.
pub fn curate(governing: &Governing) -> Result<Vec<Acquisition>, String> {
    let store = governing.store();
    let mut fetched = Vec::new();

    loop {
        let wanted = acquisitions(governing)?;
        let absent = wanted
            .into_iter()
            .filter(|acquisition| !store.source(acquisition.hash()).is_dir())
            .collect::<Vec<_>>();

        if absent.is_empty() {
            return Ok(fetched);
        }

        // Two dependents pinning one tree through two mirrors are one acquisition's worth of difference, as `Acquisition` states: the tree is fetched once, from the first transport named for it, and reported once. The set above tells them apart by `url`, and the filter ran before anything landed, so this is where the second one learns the first already placed it.
        let mut placed = BTreeSet::new();
        for acquisition in absent {
            if !placed.insert(acquisition.hash().clone()) {
                continue;
            }

            fetch(&store, &acquisition)?;
            fetched.push(acquisition);
        }
    }
}

/// Every acquisition reachable through manifests that are readable *now*.
///
/// Unreadable ones are not an error here: a dependency that has not been materialized is precisely what this walk exists to discover, and refusing on it would make the first round the only round.
fn acquisitions(governing: &Governing) -> Result<BTreeSet<Acquisition>, String> {
    let mut acquisitions = BTreeSet::new();
    let mut seen = BTreeSet::new();
    let mut frontier = vec![(governing.package.clone(), governing.directory.clone())];

    while let Some((package, directory)) = frontier.pop() {
        if !seen.insert(package.name.clone()) {
            continue;
        }

        for (name, row) in &package.dependencies {
            // A `catalog` marker names a row the umbrella holds, and *that* row is what this walk must realize — so it is resolved before the dispatch below, leaving the fetchable arm the only place a fetch is decided. Resolving it after instead is what left a fetchable catalog row acquired by nobody: the marker landed in the store at a hash nothing had put there, `order` refused it naming `curate`, and `curate` had just declined to fetch it. A member marker names a directory rather than a row, so only this one indirects.
            //
            // The base travels with the row because a relative `path` is relative to whoever wrote it: the umbrella's root for a catalog row, the depending package's own directory otherwise. `order` reads it the same way, at `Walk::point`'s catalog arm.
            let (row, base) = match row {
                Dependency::Catalog => match catalogued(governing, name) {
                    Some(row) => (row, governing.root.as_path()),
                    None => continue,
                },
                row => (row, directory.as_path()),
            };

            let resolved = match row {
                Dependency::Git { url, rev, hash } => {
                    acquisitions.insert(Acquisition {
                        name: name.clone(),
                        url: url.clone(),
                        snapshot: Snapshot {
                            rev: rev.clone(),
                            hash: hash.clone(),
                        },
                    });

                    governing.store().source(hash)
                }
                Dependency::Path { path } => base.join(path),
                // A member is on disk already: `order` is what refuses a mismatched one, and this walk only needs somewhere further to look.
                Dependency::Member => match member(governing, name) {
                    Some(directory) => directory,
                    None => continue,
                },
                // Unreachable: a catalog row may name no marker (`Document::umbrella` refuses one), so the resolution above lands on a fetchable or `path` row or on nothing.
                Dependency::Catalog => continue,
            };

            if let Ok(Manifest::Package(package)) = Manifest::from_path(&resolved.join(MANIFEST)) {
                frontier.push((package, resolved));
            }
        }
    }

    Ok(acquisitions)
}

/// The row the governing umbrella's `[catalog]` files under `name`, when one governs and it holds that name.
///
/// A row rather than a directory, because where it points is the caller's question and *what it is* decides how: a fetchable one has to be acquired before it points anywhere at all.
fn catalogued<'a>(governing: &'a Governing, name: &str) -> Option<&'a Dependency> {
    governing.umbrella.as_ref()?.catalog.get(name)
}

/// Where the member declaring `name` sits, when the governing umbrella enumerates one.
fn member(governing: &Governing, name: &str) -> Option<PathBuf> {
    governing
        .umbrella
        .as_ref()?
        .members
        .iter()
        .map(|member| governing.root.join(member))
        .find(|directory| {
            matches!(
                Manifest::from_path(&directory.join(MANIFEST)),
                Ok(Manifest::Package(package)) if package.name == name
            )
        })
}

/// Fetch `acquisition`, verify it, and place it — in that order, and only in that order.
///
/// The tree is hashed where it lands temporarily and moved into the store only once it has been accepted, so a failed or interrupted fetch cannot leave a directory the store would later read as a verified delivery.
fn fetch(store: &Store, acquisition: &Acquisition) -> Result<(), String> {
    let scratch = store.source(acquisition.hash()).with_extension("fetching");
    let _ = fs::remove_dir_all(&scratch);
    fs::create_dir_all(&scratch)
        .map_err(|error| format!("failed to create {}: {error}", scratch.display()))?;

    let outcome =
        deliver(&scratch, acquisition).and_then(|()| accept(&scratch, store, acquisition));

    if outcome.is_err() {
        let _ = fs::remove_dir_all(&scratch);
    }

    outcome
}

/// Ask `git` for the revision, into `scratch`.
fn deliver(scratch: &Path, acquisition: &Acquisition) -> Result<(), String> {
    let rev = &acquisition.snapshot.rev;

    git(scratch, &["init", "--quiet"])?;
    git(scratch, &["remote", "add", "origin", &acquisition.url])?;

    // A shallow fetch of the one revision is what this wants, and what it brought is `FETCH_HEAD` — which is the pin whether it named an object, a branch or a tag, and the only spelling that is. A fetched branch leaves no local ref of its own, so checking one out by the name it was pinned under is what does not work here.
    match git(
        scratch,
        &["fetch", "--quiet", "--depth", "1", "origin", rev],
    ) {
        Ok(()) => git(scratch, &["checkout", "--quiet", "--detach", "FETCH_HEAD"])?,

        // A server may decline to serve one revision by object name, and the whole history is the fallback rather than the default because it is the thing worth not transferring. The revision is then resolved against that history *by name*, never through `FETCH_HEAD`: a refspec-less fetch points it at the remote's default branch, so reaching for it here would deliver whatever that branch holds for any pin the shallow fetch could not serve — including one naming a revision that does not exist, which is how a wrong `rev` came to be reported as a delivery disagreeing with its `hash`.
        Err(_) => {
            git(scratch, &["fetch", "--quiet", "origin"])?;
            git(scratch, &["checkout", "--quiet", "--detach", rev])?;
        }
    }

    // Source is what was delivered; the object store is how it arrived. A fresh clone's differs run to run, so leaving it in would make the hash unreproducible and the store key meaningless.
    fs::remove_dir_all(scratch.join(".git"))
        .map_err(|error| format!("failed to drop the fetched repository's metadata: {error}"))
}

/// Verify the delivery and move it into the store.
fn accept(scratch: &Path, store: &Store, acquisition: &Acquisition) -> Result<(), String> {
    let delivered = TreeHash::of(scratch)?;

    if &delivered != acquisition.hash() {
        return Err(format!(
            "the dependency {:?} was fetched from {} at {}, and what arrived is not what it is pinned to\n  expected {}\n  delivered {delivered}",
            acquisition.name, acquisition.url, acquisition.snapshot.rev, acquisition.snapshot.hash
        ));
    }

    let placed = store.source(acquisition.hash());
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
