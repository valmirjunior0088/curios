//! A file a build pins by its SHA-256: read where a hand placed it or an earlier run kept it, else downloaded, and checked against its digest either way, a failure saying how to place it by hand where it cannot be had; and the pieces that keep what it holds, unpacked or as an executable.
//!
//! What takes something from the network takes it through [`Download`] — the build scripts, `curios-unicode`'s database, `curios-binaryen`'s source archive and `curios-profile`'s trace schema, and `xtask`, each tool a step runs — so how a pinned file is obtained, checked and kept, and what an offline build is told, is written once. Why this is a crate of its own is `README.md`'s decision.
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
    url: String,
    sha256: String,
    place: PathBuf,
    cause: String,
}

impl fmt::Display for Unobtained {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "could not obtain {}:\n  {}\n\nTo build offline, download this file by hand:\n  {}\nverify it has sha256:\n  {}\nand place it at:\n  {}\nthen run again.",
            self.what,
            self.cause,
            self.url,
            self.sha256,
            self.place.display(),
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
            url: self.url.to_string(),
            sha256: self.sha256.to_string(),
            place: self.place.to_path_buf(),
            cause,
        }
    }
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
