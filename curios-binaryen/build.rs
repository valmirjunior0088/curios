//! Fetches a prebuilt Binaryen static library instead of building from
//! source: Binaryen's own release process already publishes
//! `libbinaryen.a` + `binaryen-c.h` per platform, and the FFI boundary in
//! `src/lib.rs` only ever crosses that pure-C header, so there is nothing
//! for a from-source build to buy us here.
//!
//! Flow, in order:
//! 1. `$OUT_DIR/binaryen-version_130/lib/libbinaryen.a` already present
//!    (a prior build already extracted it) -> just emit link directives.
//! 2. `$OUT_DIR/<asset>.tar.gz` already present (fetched by a prior run,
//!    or placed there by hand for an offline build) -> verify its sha256
//!    against the pinned table below, then extract.
//! 3. Otherwise download the asset from the GitHub release, verify, and
//!    extract.
//!
//! A checksum mismatch is always fatal — a corrupted download or a
//! tampered/placed file is never silently accepted. A failed download
//! prints the exact URL, checksum, and destination path so the file can
//! be fetched by hand and dropped into `$OUT_DIR` for a fully offline
//! build.
//!
//! To bump the Binaryen version: update `VERSION` below, then re-derive
//! `ASSETS`' sha256 values from the new release's own checksum files
//! (https://github.com/WebAssembly/binaryen/releases/tag/<VERSION>).

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

/// (Rust target triple substring, release asset target suffix, sha256 of the
/// asset). Checked in order; the first substring match wins.
const ASSETS: &[(&str, &str, &str)] = &[
    (
        "x86_64-unknown-linux",
        "x86_64-linux",
        "0a18362361ad05465118cd8eeb72edaeec89de6894bc283576ef4e07aa3babcc",
    ),
    (
        "aarch64-unknown-linux",
        "aarch64-linux",
        "e6ae6e09ac40f4e14bc5be6f687c58e2995c84170013975fa641809dd3b480a0",
    ),
    (
        "x86_64-apple-darwin",
        "x86_64-macos",
        "d3e2d1235b70c93c54b52eabc1625ea960965152218754f1f4eeb0f873c48e03",
    ),
    (
        "aarch64-apple-darwin",
        "arm64-macos",
        "79d3ab9f417d9e215f15f598f523d001a7d9ac1e59367e5c869fbdabd1cba72e",
    ),
    (
        "x86_64-pc-windows",
        "x86_64-windows",
        "cc09c874f4332d00aa32ab72745a9b98c9a172f795762f21d03e70638a3f7f4c",
    ),
    (
        "aarch64-pc-windows",
        "arm64-windows",
        "b18c9cbe000562b1ee5d9cb60146616a949aca504903ad63f27fd9fd679898a7",
    ),
];

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

/// Extract `lib/` and `include/` from a Binaryen release tarball into
/// `out_dir`, so the result lands at `out_dir/binaryen-<VERSION>/{lib,include}`
/// (the tarball's own top-level directory name).
fn extract(archive_bytes: &[u8], out_dir: &Path) {
    let tar = GzDecoder::new(archive_bytes);
    let mut archive = Archive::new(tar);

    for entry in archive.entries().expect("valid gzip/tar stream") {
        let mut entry = entry.expect("valid tar entry");
        let path = entry.path().expect("valid entry path").into_owned();

        let Some(top) = path.components().next() else {
            continue;
        };
        let rest = path.strip_prefix(top).unwrap();
        let Some(second) = rest.components().next() else {
            continue;
        };

        if second.as_os_str() == "lib" || second.as_os_str() == "include" {
            entry.unpack_in(out_dir).expect("unpack tar entry");
        }
    }
}

fn instructions_on_failure(asset_url: &str, sha256: &str, archive_path: &Path, cause: &str) -> ! {
    panic!(
        "\n\ncould not obtain the Binaryen release needed to build curios-binaryen:\n  {cause}\n\n\
        To build offline, download this file by hand:\n  {asset_url}\n\
        verify it has sha256:\n  {sha256}\n\
        and place it at:\n  {}\n\
        then re-run the build.\n\n",
        archive_path.display(),
    );
}

fn main() {
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let target = env::var("TARGET").unwrap();

    let (_, asset_target, sha256) = ASSETS
        .iter()
        .find(|(triple, _, _)| target.contains(triple))
        .unwrap_or_else(|| {
            let supported = ASSETS
                .iter()
                .map(|(triple, _, _)| *triple)
                .collect::<Vec<_>>()
                .join(", ");
            panic!("no prebuilt Binaryen release for target `{target}`; supported: {supported}")
        });

    let install_dir = out_dir.join(format!("binaryen-{VERSION}"));
    let lib_dir = install_dir.join("lib");

    if !lib_dir.join("libbinaryen.a").exists() {
        let asset_name = format!("binaryen-{VERSION}-{asset_target}.tar.gz");
        let asset_url = format!(
            "https://github.com/WebAssembly/binaryen/releases/download/{VERSION}/{asset_name}"
        );
        let archive_path = out_dir.join(&asset_name);

        let archive_bytes = if archive_path.exists() {
            fs::read(&archive_path).expect("read cached archive")
        } else {
            let bytes = download(&asset_url).unwrap_or_else(|error| {
                instructions_on_failure(&asset_url, sha256, &archive_path, &error)
            });

            fs::write(&archive_path, &bytes).expect("write downloaded archive");

            bytes
        };

        let actual = sha256_hex(&archive_bytes);

        if &actual != sha256 {
            instructions_on_failure(
                &asset_url,
                sha256,
                &archive_path,
                &format!("sha256 mismatch: expected {sha256}, got {actual}"),
            );
        }

        extract(&archive_bytes, &out_dir);
    }

    println!("cargo:rustc-link-search=native={}", lib_dir.display());
    println!("cargo:rustc-link-lib=static=binaryen");

    // The MSVC toolchain links its C++ runtime automatically.
    if target.contains("apple") {
        println!("cargo:rustc-link-lib=c++");
    } else if target.contains("linux") {
        println!("cargo:rustc-link-lib=stdc++");
    }
}
