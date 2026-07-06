//! Downloads Binaryen's source for tag `version_130` and statically links
//! the optimizer built from it: upstream only publishes a prebuilt
//! `libbinaryen.a` for Linux (macOS and Windows releases ship a
//! dylib/import-lib instead), so a from-source build is the only route to
//! a static lib on every platform curios supports. Building from a fetched
//! tarball (rather than a vendored copy under version control) keeps the
//! ~3k-file C++ tree out of the repo.
//!
//! Flow, in order:
//! 1. `$OUT_DIR/binaryen-version_130/CMakeLists.txt` already present (a
//!    prior build already extracted the source) -> skip straight to CMake.
//! 2. `$OUT_DIR/version_130.tar.gz` already present (fetched by a prior
//!    run, or placed there by hand for an offline build) -> verify its
//!    sha256 against `SOURCE_SHA256` below, then extract.
//! 3. Otherwise download the tag's source archive from GitHub, verify,
//!    and extract.
//!
//! A checksum mismatch is always fatal — a corrupted download or a
//! tampered/placed file is never silently accepted. A failed download
//! prints the exact URL, checksum, and destination path so the file can
//! be fetched by hand and dropped into `$OUT_DIR` for a fully offline
//! build.
//!
//! To bump the Binaryen version: update `VERSION` below, then re-derive
//! `SOURCE_SHA256` from the new tag's source archive
//! (`https://github.com/WebAssembly/binaryen/archive/refs/tags/<VERSION>.tar.gz`).

use {
    flate2::read::GzDecoder,
    sha2::{Digest, Sha256},
    std::{
        env, fs,
        io::Read,
        path::{Path, PathBuf},
    },
    tar::Archive,
};

const VERSION: &str = "version_130";

const SOURCE_SHA256: &str = "20d727e7f3011cfe604b8ebdc873edbb4831c6b148209cb15bc2bedcded036ee";

fn source_url() -> String {
    format!("https://github.com/WebAssembly/binaryen/archive/refs/tags/{VERSION}.tar.gz")
}

fn sha256_hex(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    hasher
        .finalize()
        .iter()
        .map(|byte| format!("{byte:02x}"))
        .collect()
}

/// Download `url` into memory. The only failure mode this crate needs to
/// distinguish from "succeeded" is "didn't succeed" — the caller's error
/// message already tells the user exactly how to route around it by hand.
fn download(url: &str) -> Result<Vec<u8>, String> {
    let response = ureq::get(url)
        .call()
        .map_err(|error| format!("GET {url} failed: {error}"))?;

    let mut bytes = Vec::new();

    response
        .into_body()
        .into_reader()
        .read_to_end(&mut bytes)
        .map_err(|error| format!("reading response body from {url} failed: {error}"))?;

    Ok(bytes)
}

/// Extract the full source tarball into `out_dir`, landing at
/// `out_dir/binaryen-<VERSION>/` (the tarball's own top-level directory
/// name) so CMake can be pointed straight at it.
fn extract(archive_bytes: &[u8], out_dir: &Path) {
    let tar = GzDecoder::new(archive_bytes);
    let mut archive = Archive::new(tar);
    archive.unpack(out_dir).expect("valid gzip/tar stream");
}

fn instructions_on_failure(archive_path: &Path, cause: &str) -> ! {
    panic!(
        "\n\ncould not obtain the Binaryen source needed to build curios-binaryen:\n  {cause}\n\n\
        To build offline, download this file by hand:\n  {}\n\
        verify it has sha256:\n  {SOURCE_SHA256}\n\
        and place it at:\n  {}\n\
        then re-run the build.\n\n",
        source_url(),
        archive_path.display(),
    );
}

fn main() {
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let source_dir = out_dir.join(format!("binaryen-{VERSION}"));

    if !source_dir.join("CMakeLists.txt").exists() {
        let archive_path = out_dir.join(format!("{VERSION}.tar.gz"));

        let archive_bytes = if archive_path.exists() {
            fs::read(&archive_path).expect("read cached archive")
        } else {
            let bytes = download(&source_url())
                .unwrap_or_else(|error| instructions_on_failure(&archive_path, &error));

            fs::write(&archive_path, &bytes).expect("write downloaded archive");

            bytes
        };

        let actual = sha256_hex(&archive_bytes);

        if actual != SOURCE_SHA256 {
            instructions_on_failure(
                &archive_path,
                &format!("sha256 mismatch: expected {SOURCE_SHA256}, got {actual}"),
            );
        }

        extract(&archive_bytes, &out_dir);
    }

    // Always build Binaryen as Release: a Debug-profile optimizer is an
    // order of magnitude slower at runtime for no benefit to us.
    let destination = cmake::Config::new(&source_dir)
        .profile("Release")
        .define("BUILD_SHARED_LIBS", "OFF")
        .define("BUILD_TOOLS", "OFF")
        .define("BUILD_TESTS", "OFF")
        .define("ENABLE_WERROR", "OFF")
        .build();

    // GNUInstallDirs installs to lib64 on some Linux distributions.
    println!(
        "cargo:rustc-link-search=native={}/lib",
        destination.display()
    );

    println!(
        "cargo:rustc-link-search=native={}/lib64",
        destination.display()
    );

    println!("cargo:rustc-link-lib=static=binaryen");

    let target = env::var("TARGET").unwrap();

    // The MSVC toolchain links its C++ runtime automatically.
    if target.contains("apple") {
        println!("cargo:rustc-link-lib=c++");
    } else if target.contains("linux") {
        println!("cargo:rustc-link-lib=stdc++");
    }
}
