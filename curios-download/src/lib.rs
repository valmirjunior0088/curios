//! A file a build script pins by its SHA-256: read where a hand placed it or an earlier build kept it, else downloaded, and checked against its digest either way, the build failing with how to place it by hand where it cannot be had.
//!
//! The build scripts that take something from the network — `curios-unicode`'s database, `curios-binaryen`'s source archive and `curios-profile`'s trace schema — take it through [`Download`], so how a pinned file is obtained, checked and kept, and what an offline build is told, is written once. Why this is a crate of its own is `README.md`'s decision.

use {
    sha2::{Digest, Sha256},
    std::{
        fmt::Write,
        fs,
        io::{ErrorKind, Read},
        path::Path,
    },
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

impl Download<'_> {
    /// The file's bytes, checked against its digest: the copy at [`place`](Self::place), or else a download, which is not stored.
    pub fn bytes(&self) -> Vec<u8> {
        self.obtained().0
    }

    /// The file's bytes as [`bytes`](Self::bytes) has them, and a download kept at [`place`](Self::place) once it checks, so the next build reads it there.
    ///
    /// The copy is written beside its place and renamed onto it, so a build that stops midway leaves no partial file another would read as the whole.
    pub fn kept(&self) -> Vec<u8> {
        let (bytes, downloaded) = self.obtained();
        if downloaded {
            let directory = self.place.parent().expect("a kept file has a directory");
            fs::create_dir_all(directory)
                .unwrap_or_else(|error| panic!("create {}: {error}", directory.display()));
            let partial = self.place.with_extension("partial");
            fs::write(&partial, &bytes)
                .unwrap_or_else(|error| panic!("write {}: {error}", partial.display()));
            fs::rename(&partial, self.place)
                .unwrap_or_else(|error| panic!("keep {}: {error}", self.place.display()));
        }

        bytes
    }

    /// The file's bytes, checked, and whether they were downloaded rather than read at the place.
    fn obtained(&self) -> (Vec<u8>, bool) {
        let (bytes, downloaded) = match fs::read(self.place) {
            Ok(bytes) => (bytes, false),
            Err(error) if error.kind() == ErrorKind::NotFound => (
                download(self.url).unwrap_or_else(|cause| self.refused(&cause)),
                true,
            ),
            Err(error) => panic!("read {}: {error}", self.place.display()),
        };

        let actual = sha256_hex(&bytes);
        if actual != self.sha256 {
            self.refused(&format!(
                "sha256 mismatch: expected {}, got {actual}",
                self.sha256
            ));
        }

        (bytes, downloaded)
    }

    /// End the build with `cause` and how to build offline: the file to fetch by hand, the digest it must have and where to place it.
    fn refused(&self, cause: &str) -> ! {
        panic!(
            "\n\ncould not obtain {}:\n  {cause}\n\n\
            To build offline, download this file by hand:\n  {}\n\
            verify it has sha256:\n  {}\n\
            and place it at:\n  {}\n\
            then re-run the build.\n\n",
            self.what,
            self.url,
            self.sha256,
            self.place.display(),
        );
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
