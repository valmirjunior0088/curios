//! The prelude's images the checkout keeps, and the key a state of the sources the build script links is known by.
//!
//! Cargo gives each build-script unit an `OUT_DIR` of its own and runs the script once a unit, and each configuration of the workspace is a unit: Clippy against a build, one selection against another, every feature, each profile. Every one of them makes the same bytes of one state of the sources, so the first run of a state files its images here, and a run under another configuration copies them into its own `OUT_DIR` where it would make them again. The key holds what the script links and nothing of the configuration that runs it.
//!
//! `curios-prelude`'s build script opens the images under its own crate's `.artifacts/images/` and keys it by the crates it links, this one among them.

#[cfg(test)]
mod tests;

use {
    curios_common::{Fingerprint, digest},
    std::{
        collections::{BTreeMap, BTreeSet},
        fs::{self, File, OpenOptions},
        path::{Path, PathBuf},
    },
    toml::{Table, Value},
};

/// The files at the workspace's root that every crate's build reads: the manifest, the lock, and the toolchain file naming the compiler.
const ROOT_FILES: [&str; 3] = ["Cargo.toml", "Cargo.lock", "rust-toolchain.toml"];

/// The file that names the images' key, each image and the files the lowering read in, written last.
const MARKER: &str = "marker";

/// What the build script links, as one digest: `rustc`, the compiler's own account of itself, the files at the workspace's root that every build reads, and every file of `prelude`'s crate and of each workspace crate it links that a build of the script may read, as `files` walks them, each by its path under the root and then its bytes.
///
/// Nothing of the configuration that runs the script is in it — no profile, feature, target or `OUT_DIR` — so every unit of one state of the sources reads one key. A path is read under the root, so a checkout elsewhere holding the same files reads the same key, and images copied into it are right there.
pub fn key(workspace: &Path, prelude: &Path, rustc: &str) -> String {
    let workspace = canonical(workspace);
    let mut fingerprint = Fingerprint::new();
    fingerprint.feed(rustc);
    for name in ROOT_FILES {
        feed(&mut fingerprint, &workspace, &workspace.join(name));
    }
    for directory in linked(&workspace, &canonical(prelude)) {
        for file in files(&directory) {
            feed(&mut fingerprint, &workspace, &file);
        }
    }

    fingerprint.hex()
}

/// One file into `fingerprint`, as two parts: its path under `workspace`, then its bytes.
fn feed(fingerprint: &mut Fingerprint, workspace: &Path, file: &Path) {
    let name = file
        .strip_prefix(workspace)
        .unwrap_or_else(|_| panic!("{} is not under the workspace's root", file.display()));
    fingerprint.feed(name.as_os_str().as_encoded_bytes());
    fingerprint
        .feed(fs::read(file).unwrap_or_else(|error| panic!("read {}: {error}", file.display())));
}

/// The directory of `prelude`'s crate and of every workspace crate its build-dependencies reach, read off the manifests: from each crate reached, its dependency and build-dependency tables, a `target` table's among them, whether a feature turns a row on or not, so the set errs toward more. A row with no path is a registry crate's, which the lock holds.
fn linked(workspace: &Path, prelude: &Path) -> BTreeSet<PathBuf> {
    let root = manifest(workspace);
    let shared = root
        .get("workspace")
        .and_then(|table| table.get("dependencies"))
        .and_then(Value::as_table);

    let mut linked = BTreeSet::from([prelude.to_path_buf()]);
    let mut pending = named(prelude, &["build-dependencies"], workspace, shared);
    while let Some(directory) = pending.pop() {
        if linked.insert(directory.clone()) {
            pending.extend(named(
                &directory,
                &["dependencies", "build-dependencies"],
                workspace,
                shared,
            ));
        }
    }

    linked
}

/// The directory of each workspace crate the manifest at `directory` names in `tables`, at its top or under a `target` table: a row's own `path`, read from `directory`, or, for a row taking the workspace's row of its name, that row's, read from the root.
fn named(
    directory: &Path,
    tables: &[&str],
    workspace: &Path,
    shared: Option<&Table>,
) -> Vec<PathBuf> {
    let read = manifest(directory);
    let targets = read
        .get("target")
        .and_then(Value::as_table)
        .into_iter()
        .flat_map(|targets| targets.values())
        .filter_map(Value::as_table);

    std::iter::once(&read)
        .chain(targets)
        .flat_map(|table| {
            tables
                .iter()
                .filter_map(|name| table.get(*name)?.as_table())
        })
        .flat_map(|rows| rows.iter())
        .filter_map(|(name, row)| {
            let row = row.as_table()?;
            if row.get("workspace").and_then(Value::as_bool) == Some(true) {
                Some(workspace.join(shared?.get(name)?.get("path")?.as_str()?))
            } else {
                Some(directory.join(row.get("path")?.as_str()?))
            }
        })
        .map(|directory| canonical(&directory))
        .collect()
}

/// The manifest of the crate at `directory`, or of the workspace at its root.
fn manifest(directory: &Path) -> Table {
    let path = directory.join("Cargo.toml");
    let text = fs::read_to_string(&path)
        .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));

    toml::from_str(&text).unwrap_or_else(|error| panic!("{}: {error}", path.display()))
}

/// `path` with every link and `..` resolved, so a crate that two manifests spell two ways is one directory.
fn canonical(path: &Path) -> PathBuf {
    path.canonicalize()
        .unwrap_or_else(|error| panic!("{}: {error}", path.display()))
}

/// Every file under `crate_root` a build of the script may read, in the order of their paths. Left out are what lies under an entry whose name begins with a dot — what a tool keeps beside a crate, its `.artifacts/`, or beside a package, its `.curios/` cabinet, is no source of either — and what no build the script links compiles ([`compiled_by_no_link`]).
fn files(crate_root: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    let mut pending = vec![crate_root.to_path_buf()];
    while let Some(directory) = pending.pop() {
        let found = fs::read_dir(&directory)
            .unwrap_or_else(|error| panic!("read {}: {error}", directory.display()));
        for found in found {
            let found =
                found.unwrap_or_else(|error| panic!("read {}: {error}", directory.display()));
            if found.file_name().as_encoded_bytes().starts_with(b".") {
                continue;
            }
            let kind = found
                .file_type()
                .unwrap_or_else(|error| panic!("read {}: {error}", found.path().display()));
            if kind.is_dir() {
                pending.push(found.path());
            } else if !compiled_by_no_link(crate_root, &found.path()) {
                files.push(found.path());
            }
        }
    }
    files.sort();

    files
}

/// Whether `file`, under the crate at `crate_root`, is one no build the script links compiles: a crate's `README.md`, which none includes, and a Rust file at a test's place — `tests.rs`, a name ending `_tests.rs`, `test_support.rs`, or one under a `tests` or `test_support` directory — which `.claude/rules/rust.md`'s layout gives no production module and which reaches a build only through `#[cfg(test)]` or `test-support`, on nowhere the script's graph reaches. A `.crs` file under a `tests` directory is `/std`'s, a source its lowering reads, and stays. What holds the place to its premise is `every_rust_file_at_a_tests_place_is_reached_only_through_a_test_gate`, and an image a key gone stale would keep is refused by `compiler caches prelude`'s `compare`, which the handoff runs.
fn compiled_by_no_link(crate_root: &Path, file: &Path) -> bool {
    file.ends_with("README.md")
        || (file.extension().is_some_and(|extension| extension == "rs")
            && tests_place(crate_root, file))
}

/// Whether the Rust file `file`, under `crate_root`, stands at a test's place: what [`compiled_by_no_link`] reads, and what the premise's test holds to its gates.
fn tests_place(crate_root: &Path, file: &Path) -> bool {
    let name = file
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("");
    let beneath = file
        .strip_prefix(crate_root)
        .ok()
        .and_then(Path::parent)
        .into_iter()
        .flat_map(Path::components)
        .any(|component| {
            matches!(
                component.as_os_str().to_str(),
                Some("tests" | "test_support")
            )
        });

    beneath || matches!(name, "tests.rs" | "test_support.rs") || name.ends_with("_tests.rs")
}

/// The checkout's images, held: those one state of the sources made, beside the marker that names the state's key, each image's digest and the files the lowering read. The marker is written last, so images cut short anywhere hold none and are no hit. The lock is taken where the images are opened and let go where it is dropped, so no two units of the checkout fill or read it at once.
pub struct Images {
    directory: PathBuf,
    /// Held, never read: dropping it lets the lock go.
    _lock: File,
}

/// What a run that made its images did with them ([`Images::settle`]).
#[derive(Debug, PartialEq, Eq)]
pub enum Settled {
    /// Compared with the kept images of their state, and the same.
    Same,
    /// Filed as their state's images.
    Filed,
    /// Neither compared nor filed: a source the build links changed while it ran.
    Moved,
}

/// What the images' marker names.
#[derive(Default)]
struct Marker {
    pub key: String,
    /// Each image by its name, with its digest.
    pub images: BTreeMap<String, String>,
    /// The files the lowering read, named under `std/`.
    pub reads: Vec<PathBuf>,
}

impl Images {
    /// The images in `directory`, made where there are none, once no other unit holds it.
    pub fn open(directory: PathBuf) -> Self {
        fs::create_dir_all(&directory)
            .unwrap_or_else(|error| panic!("create {}: {error}", directory.display()));
        let path = directory.join("lock");
        let lock = OpenOptions::new()
            .create(true)
            .truncate(false)
            .read(true)
            .write(true)
            .open(&path)
            .unwrap_or_else(|error| panic!("open {}: {error}", path.display()));
        lock.lock()
            .unwrap_or_else(|error| panic!("lock {}: {error}", path.display()));

        Self {
            directory,
            _lock: lock,
        }
    }

    /// The files the lowering read, named under `std/`, where they are `key`'s images whole, each then copied into `out`. `None` where it holds another key or none, and where an image is not the one its marker names, which a warning says, since the run that misses files them again.
    pub fn reuse(&self, key: &str, out: &Path) -> Option<Vec<PathBuf>> {
        let marker = self.marker().filter(|marker| marker.key == key)?;
        for (name, held) in &marker.images {
            let path = self.directory.join(name);
            if fs::read(&path).ok().map(|bytes| digest(&bytes)).as_ref() != Some(held) {
                println!(
                    "cargo:warning={} is not the image its marker names, so the prelude is made again",
                    path.display()
                );
                return None;
            }
        }
        for name in marker.images.keys() {
            copy(&self.directory.join(name), &out.join(name));
        }

        Some(marker.reads)
    }

    /// What a run does with the images it made in `out` of the state `key` names, `again` being the key read once the work was done and `reads` the files the lowering read: nothing, where `again` names another state, since a source read after the work began may be in the images or not; else, asked to `compare`, the kept images held to the ones made where they are this state's, and a refusal where an image differs; and otherwise the ones made filed. A difference is laid to the key or the build only where the sources held still, so an edit made while the build ran is never read as either.
    pub fn settle(
        &self,
        key: &str,
        again: &str,
        out: &Path,
        reads: &[PathBuf],
        compare: bool,
    ) -> Result<Settled, String> {
        if again != key {
            return Ok(Settled::Moved);
        }
        if compare && self.compare(key, out)? {
            return Ok(Settled::Same);
        }
        self.fill(key, out, reads);

        Ok(Settled::Filed)
    }

    /// File every file of `out` as `key`'s images, with `reads`, the files the lowering read, named under `std/`: the marker is removed first and written last.
    fn fill(&self, key: &str, out: &Path, reads: &[PathBuf]) {
        let marker = self.directory.join(MARKER);
        if marker.exists() {
            fs::remove_file(&marker)
                .unwrap_or_else(|error| panic!("remove {}: {error}", marker.display()));
        }

        let mut text = format!("key {key}\n");
        for image in files(out) {
            let name = image
                .file_name()
                .and_then(|name| name.to_str())
                .unwrap_or_else(|| panic!("{} is named in no text", image.display()));
            copy(&image, &self.directory.join(name));
            let bytes = fs::read(&image)
                .unwrap_or_else(|error| panic!("read {}: {error}", image.display()));
            text += &format!("image {name} {}\n", digest(&bytes));
        }
        for read in reads {
            text += &format!("read {}\n", read.display());
        }
        fs::write(&marker, text)
            .unwrap_or_else(|error| panic!("write {}: {error}", marker.display()));
    }

    /// Whether `out` holds the images these hold for `key`, by the digests its marker names: `false` where it names another key or none, and a refusal where an image differs. A run asked to compare, which makes its images again, answers so of the run that filed them, since the same sources made both.
    fn compare(&self, key: &str, out: &Path) -> Result<bool, String> {
        let Some(marker) = self.marker().filter(|marker| marker.key == key) else {
            return Ok(false);
        };
        for (name, held) in &marker.images {
            let path = out.join(name);
            let made = digest(
                &fs::read(&path).unwrap_or_else(|error| panic!("read {}: {error}", path.display())),
            );
            if made != *held {
                return Err(format!(
                    "{} differs from the image {} holds of the same sources: the prelude's key leaves out an input its build reads, or its build is not deterministic",
                    path.display(),
                    self.directory.display()
                ));
            }
        }

        Ok(true)
    }

    /// The marker the images were filed under, or `None` where there is none or it says what no fill writes.
    fn marker(&self) -> Option<Marker> {
        let text = fs::read_to_string(self.directory.join(MARKER)).ok()?;
        let mut marker = Marker::default();
        for line in text.lines() {
            match line.split_once(' ')? {
                ("key", key) => marker.key = key.to_owned(),
                ("image", image) => {
                    let (name, digest) = image.split_once(' ')?;
                    marker.images.insert(name.to_owned(), digest.to_owned());
                }
                ("read", read) => marker.reads.push(PathBuf::from(read)),
                _ => return None,
            }
        }

        Some(marker)
    }
}

fn copy(from: &Path, to: &Path) {
    fs::copy(from, to)
        .unwrap_or_else(|error| panic!("copy {} to {}: {error}", from.display(), to.display()));
}
