//! The store's tool, and the toolchain's only network actor.
//!
//! **Why fetching lives here and nowhere else.** Opacity is a compiler property — it compares a `rev` and never interprets one — and interpretation is exactly what turning a revision into bytes requires. Putting it here is a decision rather than a concession, because acceptance is by hash: *any* transport may deliver the bytes, an untrusted one included, and a delivery that fails its hash is refused regardless of who fetched it. A separate fetcher layered above this would double the tooling for zero integrity gain. The compiler itself never fetches.
//!
//! That same argument picks the transports: programs already on the developer's machine, shelled out to — `git` for a package tree, `curl` or `wget` for a `[[foreign]]` module. A second implementation of the git protocol vendored into this workspace would buy nothing the hash does not already guarantee, while owning an authentication story — ssh keys, credential helpers, private remotes — that the one already installed handles; an HTTP client would buy no more, against proxy and certificate configuration the installed one already reads. Two fetchers rather than one because neither is universally present and choosing between them would be arbitrary.
//!
//! **A delivered tree is source, and `.git` is not source.** It is removed before the tree is hashed or placed: a fresh clone's object store differs run to run, so leaving it in would make the criterion unreproducible and the store key meaningless. This is not something the scheme could state, because the scheme hashes whatever it is handed; it is a fact about what `curate` hands it.

#[cfg(test)]
mod tests;

use {
    crate::{
        Dependency, FileHash, Foreign, Governing, MANIFEST, Manifest, Module, Snapshot, Store,
        TreeHash,
    },
    std::{
        collections::BTreeSet,
        fmt, fs, io,
        path::{Path, PathBuf},
        process::{Command, Output},
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

/// One foreign module to bring into the store: the row that named it, the package that declared that row, and what the delivery must hash to.
///
/// A module reveals nothing further — it is a file, not a tree with a manifest — so these are gathered and fetched after the package fixed point rather than inside it.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ModuleAcquisition {
    /// The package whose manifest declared the row, for a refusal to name.
    pub package: String,
    /// The row's own name, which is what a report and `curios add --refresh` call it.
    pub name: String,
    pub url: String,
    pub hash: FileHash,
}

impl fmt::Display for ModuleAcquisition {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "{} of {} from {}",
            self.name, self.package, self.url
        )
    }
}

/// Everything one `curate` brought in: the trees, then the modules those trees' manifests named.
#[derive(Debug, Default)]
pub struct Curated {
    pub packages: Vec<Acquisition>,
    pub modules: Vec<ModuleAcquisition>,
}

impl Curated {
    /// Whether the store already held everything the manifests reference, which is what a second `curate` over an unchanged project finds.
    pub fn is_empty(&self) -> bool {
        self.packages.is_empty() && self.modules.is_empty()
    }
}

/// Bring every snapshot the governing package reaches into the store, and then every foreign module the manifests now readable name.
///
/// Iterated to a fixed point rather than walked once, because a dependency's own dependencies cannot be read until it is there: each round materializes what the manifests currently reachable declare, and the next round sees further. A round that fetches nothing new is the end of it — and a round that fetches without revealing anything is impossible, since a fetched tree either has a manifest or is refused for having none.
///
/// Modules come after, in one pass. A fetched module is a file and declares nothing, so it can reveal no further rows — which is what lets this be a pass rather than another fixed point.
pub fn curate(governing: &Governing) -> Result<Curated, String> {
    let store = governing.store();
    let mut curated = Curated::default();

    // The module set converges with the trees: each round reads further manifests, so the round that fetches nothing new is also the one whose module set is complete.
    let named = loop {
        let reachable = acquisitions(governing)?;
        let absent = reachable
            .packages
            .into_iter()
            .filter(|acquisition| !store.source(acquisition.hash()).is_dir())
            .collect::<Vec<_>>();

        if absent.is_empty() {
            break reachable.fetches;
        }

        // Two dependents pinning one tree through two mirrors are one acquisition's worth of difference, as `Acquisition` states: the tree is fetched once, from the first transport named for it, and reported once. The set above tells them apart by `url`, and the filter ran before anything landed, so this is where the second one learns the first already placed it.
        let mut placed = BTreeSet::new();
        for acquisition in absent {
            if !placed.insert(acquisition.hash().clone()) {
                continue;
            }

            fetch(&store, &acquisition)?;
            curated.packages.push(acquisition);
        }
    };

    // Content-keyed like a tree, so two packages naming one module fetch it once and every project on the machine shares the entry.
    for acquisition in named {
        if store.foreign(&acquisition.hash).is_file() {
            continue;
        }

        fetch_module(&store, &acquisition)?;
        curated.modules.push(acquisition);
    }

    Ok(curated)
}

/// Every acquisition reachable through manifests that are readable *now*, and every foreign module those manifests name.
///
/// Unreadable ones are not an error here: a dependency that has not been materialized is precisely what this walk exists to discover, and refusing on it would make the first round the only round.
///
/// Both are collected in the one walk because both are read off the same manifests. A second traversal would have to resolve each dependency row a second time to find them, and resolution is where a store path comes from — so the two walks could disagree about which tree a row meant.
#[derive(Default)]
struct Reachable {
    /// The trees to bring in.
    packages: BTreeSet<Acquisition>,
    /// The modules to fetch, which is the `url` rows alone.
    fetches: BTreeSet<ModuleAcquisition>,
    /// Every `[[foreign]]` row, however delivered, with the package and directory it belongs to — what a link needs, where the two above are what a fetch needs.
    rows: Vec<(String, PathBuf, Foreign)>,
}

/// Every `[[foreign]]` row the governing package's graph declares, with the package that declared it and the directory a carried module's path is relative to.
///
/// One walk with the fetch's, for the reason [`acquisitions`] gives: resolution is where a directory comes from, and a second traversal could resolve a row differently.
pub fn declared_modules(governing: &Governing) -> Result<Vec<(String, PathBuf, Foreign)>, String> {
    Ok(acquisitions(governing)?.rows)
}

fn acquisitions(governing: &Governing) -> Result<Reachable, String> {
    let mut reachable = Reachable::default();
    let mut seen = BTreeSet::new();
    let mut frontier = vec![(governing.package.clone(), governing.directory.clone())];

    while let Some((package, directory)) = frontier.pop() {
        if !seen.insert(package.name.clone()) {
            continue;
        }

        for foreign in &package.foreign {
            // A carried module needs no fetch: it is in the tree already, and what it must be is checked where it is read rather than here.
            if let Module::Fetched(url) = &foreign.module {
                reachable.fetches.insert(ModuleAcquisition {
                    package: package.name.clone(),
                    name: foreign.name.clone(),
                    url: url.clone(),
                    hash: foreign.hash.clone(),
                });
            }

            reachable
                .rows
                .push((package.name.clone(), directory.clone(), foreign.clone()));
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
                // A pin of a name the umbrella enumerates is `order`'s to refuse, and it does, naming the member; what this walk declines is to fetch on its behalf first, since a fetch is the one network action in the toolchain and a refused row earns none.
                Dependency::Git { .. } if governing.members.contains_key(name) => continue,
                Dependency::Git { url, rev, hash } => {
                    reachable.packages.insert(Acquisition {
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
                // A member is on disk already, and governance refused every entry no manifest answers before this walk began; a name the roster lacks is `order`'s to refuse, and this walk only needs somewhere further to look.
                Dependency::Member => match governing.members.get(name) {
                    Some(directory) => directory.clone(),
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

    Ok(reachable)
}

/// Fetch one foreign module, accept it against its pin, and file it in the shared store.
///
/// **Shelled out, as `git` is, and for the reason the module doc gives.** Acceptance is by hash, so any transport may deliver the bytes — which makes the one already installed the right one to use rather than an HTTP client vendored into this crate. What `curate` owns is the criterion, not the download.
///
/// The scratch path is keyed by the digest the row states rather than by what arrives, so a delivery that fails its hash is removed and nothing is ever filed under a key it does not have.
fn fetch_module(store: &Store, acquisition: &ModuleAcquisition) -> Result<(), String> {
    let placed = store.foreign(&acquisition.hash);
    let scratch = placed.with_extension("fetching");

    if let Some(parent) = placed.parent() {
        fs::create_dir_all(parent)
            .map_err(|error| format!("failed to create {}: {error}", parent.display()))?;
    }
    let _ = fs::remove_file(&scratch);

    let outcome = deliver_module(&scratch, acquisition)
        .and_then(|()| accept_module(&scratch, &placed, acquisition));

    if outcome.is_err() {
        let _ = fs::remove_file(&scratch);
    }

    outcome
}

/// What one fetcher's invocation settles: `None` where it is not on `PATH` and the next may be tried, `Some` where it ran and its outcome is the answer.
///
/// **Absent and failed are different failures, and only the first is a reason to look further.** A 404 or a refused connection is what this invocation found; asking a second program would either find the same thing or find something else and hide the disagreement.
fn attempted(
    program: &str,
    attempt: io::Result<Output>,
    acquisition: &ModuleAcquisition,
) -> Option<Result<(), String>> {
    match attempt {
        Err(error) if error.kind() == io::ErrorKind::NotFound => None,
        Err(error) => Some(Err(format!("failed to run `{program}`: {error}"))),
        Ok(output) if output.status.success() => Some(Ok(())),
        Ok(output) => Some(Err(format!(
            "failed to fetch the foreign module {} of {} from {} with `{program}`: {}",
            acquisition.name,
            acquisition.package,
            acquisition.url,
            String::from_utf8_lossy(&output.stderr).trim()
        ))),
    }
}

/// Ask the first fetcher on `PATH` for the URL, into `scratch`.
///
/// Two of them because neither is universally installed and both are ubiquitous enough that demanding a particular one would be arbitrary — the same reasoning that shells out at all rather than vendoring a client.
fn deliver_module(scratch: &Path, acquisition: &ModuleAcquisition) -> Result<(), String> {
    let curl = Command::new("curl")
        .args([
            "--location",
            "--fail",
            "--silent",
            "--show-error",
            "--output",
        ])
        .arg(scratch)
        .arg(&acquisition.url)
        .output();

    if let Some(outcome) = attempted("curl", curl, acquisition) {
        return outcome;
    }

    // `wget` follows redirects and fails on an HTTP error rather than saving the error page without being asked, so it needs neither of curl's first two flags.
    let wget = Command::new("wget")
        .args(["--quiet", "--output-document"])
        .arg(scratch)
        .arg(&acquisition.url)
        .output();

    if let Some(outcome) = attempted("wget", wget, acquisition) {
        return outcome;
    }

    Err(format!(
        "the foreign module {} of {} is fetched from {}, and neither `curl` nor `wget` is on PATH; `curios curate` reaches the network through whichever of them is installed",
        acquisition.name, acquisition.package, acquisition.url
    ))
}

/// Accept the delivery against its pin and place it, or refuse stating both digests.
fn accept_module(
    scratch: &Path,
    placed: &Path,
    acquisition: &ModuleAcquisition,
) -> Result<(), String> {
    let bytes = fs::read(scratch)
        .map_err(|error| format!("failed to read {}: {error}", scratch.display()))?;
    let delivered = FileHash::of_bytes(&bytes);

    if delivered != acquisition.hash {
        return Err(format!(
            "the foreign module {} of {} was fetched from {}, and what arrived is not what it is pinned to\n  expected {}\n  delivered {delivered}\n  a module that changed at a fixed URL is not a stale pin; re-pin it only if you know why it changed",
            acquisition.name, acquisition.package, acquisition.url, acquisition.hash
        ));
    }

    // Another process may have placed the identical bytes while this one was fetching. Content-addressed means they are the same bytes, so losing the race costs nothing but the work.
    match fs::rename(scratch, placed) {
        Ok(()) => Ok(()),
        Err(_) if placed.is_file() => {
            let _ = fs::remove_file(scratch);

            Ok(())
        }
        Err(error) => Err(format!("failed to place {}: {error}", placed.display())),
    }
}

/// The row the governing umbrella's `[catalog]` files under `name`, when one governs and it holds that name.
///
/// A row rather than a directory, because where it points is the caller's question and *what it is* decides how: a fetchable one has to be acquired before it points anywhere at all.
fn catalogued<'a>(governing: &'a Governing, name: &str) -> Option<&'a Dependency> {
    governing.umbrella.as_ref()?.catalog.get(name)
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
