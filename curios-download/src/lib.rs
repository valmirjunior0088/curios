//! A file a build pins by its SHA-256: read where a hand placed it or an earlier run kept it, else downloaded, and checked against its digest either way, a failure saying how to place it by hand where it cannot be had; a directory pinned by the digest of its files, one of an archive a forge generates or one of files a site publishes one by one; and the pieces that keep what it holds, unpacked or as an executable.
//!
//! What takes something from the network takes it through this crate — `curios-binaryen`'s build script, for its source archive, and `xtask`, each tool a step runs and the data its steps and the builds read, `curios-unicode`'s database and `curios-profile`'s trace schema among it — so how a pinned file is obtained, checked and kept, and what an offline build is told, is written once. Why this is a crate of its own is `README.md`'s decision.
//!
//! **Whole or not at all.** Whatever is kept is written beside its place under a name of this process's own and renamed onto it, so a run that stops midway leaves nothing another reads as the whole, and two runs keeping one thing at once each finish with it whole.

use {
    flate2::read::GzDecoder,
    sha2::{Digest, Sha256},
    std::{
        fmt::{self, Write},
        fs,
        io::{self, ErrorKind, Read},
        os::unix,
        path::{Path, PathBuf},
        process,
    },
    tar::Archive,
};

/// A file a build pins: what it is, where it is published, the digest it must have, and where a build looks for a copy before it downloads one.
pub struct Download<'a> {
    /// What the file is, as the instructions a failure prints name it: "the Binaryen source needed to build curios-binaryen".
    pub what: &'a str,
    /// Where the file is published, at the version the build pins.
    pub url: &'a str,
    /// The file's SHA-256, in lowercase hexadecimal.
    pub sha256: &'a str,
    /// Where a copy placed by hand, or kept by an earlier build, stands.
    pub place: &'a Path,
}

/// Why a pinned file could not be had, and how to fetch and place it by hand: what a build script panics with and `xtask` refuses a run with.
#[derive(Debug)]
pub struct Unobtained {
    what: String,
    cause: String,
    /// What to download by hand, what is checked of it and where it goes, said as the instructions say it.
    by_hand: String,
}

impl fmt::Display for Unobtained {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "could not obtain {}:\n  {}\n\nTo build offline, {}\nthen run again.",
            self.what, self.cause, self.by_hand,
        )
    }
}

impl Download<'_> {
    /// The file's bytes, checked against its digest: the copy at [`place`](Self::place), or else a download, which is not stored.
    pub fn bytes(&self) -> Result<Vec<u8>, Unobtained> {
        self.obtained().map(|(bytes, _)| bytes)
    }

    /// The file's bytes as [`bytes`](Self::bytes) has them, and a download kept at [`place`](Self::place) once it checks, so the next build reads it there.
    pub fn kept(&self) -> Result<Vec<u8>, Unobtained> {
        let (bytes, downloaded) = self.obtained()?;
        if downloaded {
            keep(&bytes, self.place).map_err(|error| {
                self.unobtained(format!("keep {}: {error}", self.place.display()))
            })?;
        }

        Ok(bytes)
    }

    /// The file's bytes, checked, and whether they were downloaded rather than read at the place.
    fn obtained(&self) -> Result<(Vec<u8>, bool), Unobtained> {
        let (bytes, downloaded) = match fs::read(self.place) {
            Ok(bytes) => (bytes, false),
            Err(error) if error.kind() == ErrorKind::NotFound => (
                download(self.url).map_err(|cause| self.unobtained(cause))?,
                true,
            ),
            Err(error) => {
                return Err(self.unobtained(format!("read {}: {error}", self.place.display())));
            }
        };

        let actual = sha256_hex(&bytes);
        if actual != self.sha256 {
            return Err(self.unobtained(format!(
                "sha256 mismatch: expected {}, got {actual}",
                self.sha256
            )));
        }

        Ok((bytes, downloaded))
    }

    /// `cause`, with how to build offline: the file to fetch by hand, the digest it must have and where to place it.
    fn unobtained(&self, cause: String) -> Unobtained {
        Unobtained {
            what: self.what.to_string(),
            cause,
            by_hand: format!(
                "download this file by hand:\n  {}\nverify it has sha256:\n  {}\nand place it at:\n  {}",
                self.url,
                self.sha256,
                self.place.display()
            ),
        }
    }
}

/// A directory a run pins by the digest of the files it holds: one directory of a gzipped tarball a forge generates at a commit, whose bytes the forge does not hold stable where its contents are — the reason Go's module hash and Nix's `fetchFromGitHub` pin what they unpack.
pub struct Unpacked<'a> {
    /// What the directory is, as the instructions a failure prints name it: "JSONTestSuite's cases".
    pub what: &'a str,
    /// Where the archive is published, at the commit the run pins.
    pub url: &'a str,
    /// The directory kept, from the archive's top, its sole top directory taken as the contents as [`unpack`] takes it: the whole of it where this is empty.
    pub directory: &'a str,
    /// The digest of the files the directory holds, as [`tree_sha256_hex`] writes it.
    pub digest: &'a str,
    /// Where a copy of the archive placed by hand stands, read before the network.
    pub placed: &'a Path,
    /// Where the directory is kept.
    pub place: &'a Path,
}

impl Unpacked<'_> {
    /// Keep the directory at [`place`](Self::place), where it is not kept already: the archive placed by hand, or else a download, unpacked beside the place, the directory's files checked against the digest, and the directory renamed onto the place, whole or not at all. A kept directory is trusted by its presence, since only one that checked is renamed there; the rest of the archive, and the archive itself, are not kept.
    pub fn kept(&self) -> Result<(), Unobtained> {
        if self.place.is_dir() {
            return Ok(());
        }
        let archive = match fs::read(self.placed) {
            Ok(bytes) => bytes,
            Err(error) if error.kind() == ErrorKind::NotFound => {
                download(self.url).map_err(|cause| self.unobtained(cause))?
            }
            Err(error) => {
                return Err(self.unobtained(format!("read {}: {error}", self.placed.display())));
            }
        };

        let unpacked = partial(self.place);
        let kept = self.checked(&archive, &unpacked);
        let removed = match unpacked.exists() {
            true => fs::remove_dir_all(&unpacked),
            false => Ok(()),
        };

        kept?;
        removed.map_err(|error| self.unobtained(format!("remove {}: {error}", unpacked.display())))
    }

    /// Unpack `archive` at `unpacked`, check the directory's files, and rename the directory onto the place.
    fn checked(&self, archive: &[u8], unpacked: &Path) -> Result<(), Unobtained> {
        let failed = |error: io::Error| self.unobtained(error.to_string());
        unpack(archive, unpacked).map_err(failed)?;
        let directory = unpacked.join(self.directory);
        let actual = tree_sha256_hex(&directory)
            .map_err(|error| self.unobtained(format!("read {}: {error}", directory.display())))?;
        if actual != self.digest {
            let over = match self.directory.is_empty() {
                true => "the archive",
                false => self.directory,
            };
            return Err(self.unobtained(format!(
                "digest mismatch over {over}: expected {}, got {actual}",
                self.digest
            )));
        }
        if let Some(parent) = self.place.parent() {
            fs::create_dir_all(parent).map_err(failed)?;
        }

        match fs::rename(&directory, self.place) {
            Ok(()) => Ok(()),
            Err(_) if self.place.is_dir() => Ok(()),
            Err(error) => Err(failed(error)),
        }
    }

    /// `cause`, with how to fetch the archive by hand and what is checked of it.
    fn unobtained(&self, cause: String) -> Unobtained {
        Unobtained {
            what: self.what.to_string(),
            cause,
            by_hand: format!(
                "download this file by hand:\n  {}\n{} are checked to have the digest:\n  {}\nand place it at:\n  {}",
                self.url,
                match self.directory.is_empty() {
                    true => "its files".to_string(),
                    false => format!("the files of its directory `{}`", self.directory),
                },
                self.digest,
                self.placed.display()
            ),
        }
    }
}

/// A directory a run pins by the digest of the files it holds, each published by itself under one URL, as a standard's own site publishes its data: every file read where a hand placed it or else downloaded, the files checked together against the digest, and the directory kept.
pub struct Gathered<'a> {
    /// What the directory is, as the instructions a failure prints name it: "the Unicode Character Database".
    pub what: &'a str,
    /// The directory the files are published under.
    pub url: &'a str,
    /// Each file's path under the URL, which is its path in the directory kept.
    pub files: &'a [String],
    /// The digest of the files, as [`tree_sha256_hex`] writes it.
    pub digest: &'a str,
    /// Where files placed by hand stand, each at its path, read before the network.
    pub placed: &'a Path,
    /// Where the directory is kept.
    pub place: &'a Path,
}

impl Gathered<'_> {
    /// Keep the directory at [`place`](Self::place), where it is not kept already: each file a hand placed, or else a download, gathered beside the place, the files checked against the digest, and the directory renamed onto the place, whole or not at all.
    pub fn kept(&self) -> Result<(), Unobtained> {
        if self.place.is_dir() {
            return Ok(());
        }

        let gathered = partial(self.place);
        let kept = self.checked(&gathered);
        let removed = match gathered.exists() {
            true => fs::remove_dir_all(&gathered),
            false => Ok(()),
        };

        kept?;
        removed.map_err(|error| self.unobtained(format!("remove {}: {error}", gathered.display())))
    }

    /// Gather the files at `gathered`, check them, and rename the directory onto the place.
    fn checked(&self, gathered: &Path) -> Result<(), Unobtained> {
        let failed = |error: io::Error| self.unobtained(error.to_string());
        if gathered.exists() {
            fs::remove_dir_all(gathered).map_err(failed)?;
        }
        for file in self.files {
            let placed = self.placed.join(file);
            let bytes = match fs::read(&placed) {
                Ok(bytes) => bytes,
                Err(error) if error.kind() == ErrorKind::NotFound => {
                    download(&format!("{}/{file}", self.url))
                        .map_err(|cause| self.unobtained(cause))?
                }
                Err(error) => {
                    return Err(self.unobtained(format!("read {}: {error}", placed.display())));
                }
            };
            let at = gathered.join(file);
            if let Some(parent) = at.parent() {
                fs::create_dir_all(parent).map_err(failed)?;
            }
            fs::write(&at, bytes).map_err(failed)?;
        }

        let actual = tree_sha256_hex(gathered)
            .map_err(|error| self.unobtained(format!("read {}: {error}", gathered.display())))?;
        if actual != self.digest {
            return Err(self.unobtained(format!(
                "digest mismatch over the files: expected {}, got {actual}",
                self.digest
            )));
        }
        if let Some(parent) = self.place.parent() {
            fs::create_dir_all(parent).map_err(failed)?;
        }

        match fs::rename(gathered, self.place) {
            Ok(()) => Ok(()),
            Err(_) if self.place.is_dir() => Ok(()),
            Err(error) => Err(failed(error)),
        }
    }

    /// `cause`, with how to fetch the files by hand and what is checked of them.
    fn unobtained(&self, cause: String) -> Unobtained {
        let urls = self
            .files
            .iter()
            .map(|file| format!("  {}/{file}", self.url))
            .collect::<Vec<_>>()
            .join("\n");

        Unobtained {
            what: self.what.to_string(),
            cause,
            by_hand: format!(
                "download these files by hand:\n{urls}\nthey are checked together to have the digest:\n  {}\nand place each, at its path under that address, in:\n  {}",
                self.digest,
                self.placed.display()
            ),
        }
    }
}

/// The SHA-256 of the files under `directory`, in lowercase hexadecimal: of the lines `<sha256>  <path>` of every file, its path from `directory` with `/` between its parts, sorted by path, each line ending in a newline — what `find . -type f -printf '%P\0' | LC_ALL=C sort -z | xargs -0 sha256sum | sha256sum` prints there, as Go's `h1:` hash is defined over a module's files.
pub fn tree_sha256_hex(directory: &Path) -> io::Result<String> {
    let mut files = Vec::new();
    files_under(directory, "", &mut files)?;
    files.sort();

    let mut lines = String::new();
    for path in files {
        let bytes = fs::read(directory.join(&path))?;
        let _ = writeln!(lines, "{}  {path}", sha256_hex(&bytes));
    }

    Ok(sha256_hex(lines.as_bytes()))
}

/// Every file under `directory`, by its path from the directory the walk began at, `prefix` the path to this one.
fn files_under(directory: &Path, prefix: &str, files: &mut Vec<String>) -> io::Result<()> {
    for entry in fs::read_dir(directory)? {
        let entry = entry?;
        let name = entry.file_name().to_string_lossy().into_owned();
        let path = format!("{prefix}{name}");
        match entry.file_type()?.is_dir() {
            true => files_under(&entry.path(), &format!("{path}/"), files)?,
            false => files.push(path),
        }
    }

    Ok(())
}

/// The SHA-256 of `bytes` in lowercase hexadecimal, as a [`Download`]'s digest is written.
pub fn sha256_hex(bytes: &[u8]) -> String {
    Sha256::digest(bytes)
        .iter()
        .fold(String::new(), |mut hex, byte| {
            let _ = write!(hex, "{byte:02x}");
            hex
        })
}

/// What the gzipped `compressed` decodes to.
pub fn gunzipped(compressed: &[u8]) -> io::Result<Vec<u8>> {
    let mut decoded = Vec::new();
    GzDecoder::new(compressed).read_to_end(&mut decoded)?;

    Ok(decoded)
}

/// Keep `bytes` at `at` as an executable, whole or not at all.
pub fn keep_executable(bytes: &[u8], at: &Path) -> io::Result<()> {
    let partial = partial(at);
    fs::write(&partial, bytes)?;
    fs::set_permissions(&partial, unix::fs::PermissionsExt::from_mode(0o755))?;

    fs::rename(&partial, at)
}

/// Unpack the gzipped tarball `archive` into the directory `into`, whole or not at all: a sole top directory, as a release's `name-version/` is, is taken as the contents, so a path inside holds whatever the archive's top is called. An entry keeps the permissions it was archived with. Where another run kept `into` first, its copy stands.
pub fn unpack(archive: &[u8], into: &Path) -> io::Result<()> {
    let partial = partial(into);
    if partial.exists() {
        fs::remove_dir_all(&partial)?;
    }
    fs::create_dir_all(&partial)?;
    if let Err(error) = Archive::new(GzDecoder::new(archive)).unpack(&partial) {
        fs::remove_dir_all(&partial)?;
        return Err(error);
    }

    let entries = fs::read_dir(&partial)?.collect::<Result<Vec<_>, _>>()?;
    let contents = match entries.as_slice() {
        [sole] if sole.file_type()?.is_dir() => sole.path(),
        _ => partial.clone(),
    };
    let kept = match fs::rename(&contents, into) {
        Ok(()) => Ok(()),
        Err(_) if into.is_dir() => Ok(()),
        Err(error) => Err(error),
    };
    // What is left beside the place: the emptied top directory, or the whole copy another run's kept one made unneeded.
    if partial.exists() {
        fs::remove_dir_all(&partial)?;
    }

    kept
}

/// Write `bytes` at `place`, whole or not at all.
fn keep(bytes: &[u8], place: &Path) -> io::Result<()> {
    if let Some(directory) = place.parent() {
        fs::create_dir_all(directory)?;
    }
    let partial = partial(place);
    fs::write(&partial, bytes)?;

    fs::rename(&partial, place)
}

/// Where this process writes what it keeps at `place` before renaming it there: beside it, under a name no other process writes.
fn partial(place: &Path) -> PathBuf {
    let mut name = place.file_name().unwrap_or_default().to_os_string();
    name.push(format!(".partial-{}", process::id()));

    place.with_file_name(name)
}

fn download(url: &str) -> Result<Vec<u8>, String> {
    let response = ureq::get(url)
        .call()
        .map_err(|error| format!("GET {url} failed: {error}"))?;
    let mut bytes = Vec::new();
    response
        .into_body()
        .into_reader()
        .read_to_end(&mut bytes)
        .map_err(|error| format!("reading the response from {url} failed: {error}"))?;

    Ok(bytes)
}

#[cfg(test)]
mod tests;
