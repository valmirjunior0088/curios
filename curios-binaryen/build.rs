//! Downloads, verifies, and builds the pinned Binaryen source release.
//!
//! Cargo gives distinct build-script fingerprints their own `OUT_DIR`, so that directory cannot cache an expensive C++ build shared by ordinary builds, tests, and Clippy. This script builds in `OUT_DIR`, then keeps only the static library in one locked cache per compilation target in `.artifacts/` beside this crate — outside Cargo's target tree, so `cargo clean` does not take a build measured in minutes with it — and empties `OUT_DIR` again. A cache entry is complete only once its library is in place and the script writes its versioned `done` marker.

use {
    flate2::read::GzDecoder,
    sha2::{Digest, Sha256},
    std::{
        env,
        fs::{self, File, OpenOptions},
        io::Read,
        path::{Path, PathBuf},
        process::Command,
    },
    tar::Archive,
};

const BINARYEN_VERSION: &str = "version_130";
const BINARYEN_SOURCE_SHA256: &str =
    "20d727e7f3011cfe604b8ebdc873edbb4831c6b148209cb15bc2bedcded036ee";

fn source_url() -> String {
    format!("https://github.com/WebAssembly/binaryen/archive/refs/tags/{BINARYEN_VERSION}.tar.gz")
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

/// This script's own bytes, hashed — the build *recipe*, as distinct from the source it builds.
///
/// This replaces a hand-bumped `BUILD_SCHEMA` constant. The constant was correct and forgettable: nothing but a contributor's memory connected a changed CMake flag to the bump that would invalidate every warm cache, so the same commit could link a library built with the old flags here and the new flags on a cold machine. Deriving it removes the step instead of documenting it.
///
/// `include_bytes!` rather than a read at run time, because the path is then resolved by the compiler relative to this file and cannot go looking in the wrong directory. Hashing the file that computes the hash is not circular: the hash is never written into the file.
///
/// The cost is that any edit here — a comment included — discards the cache. This file changes rarely, and the error is in the direction of rebuilding.
fn build_schema() -> String {
    sha256_hex(include_bytes!("build.rs"))
}

/// How the C++ toolchain identifies itself, which is what the cached library is actually compatible with.
///
/// The entry's *path* carries the target triple, so an architecture cannot be confused. Nothing carried the platform underneath it, and two machines of one triple on different distributions produce incompatible static libraries under identical paths and identical markers. That was nearly unreachable while this cache sat under `target/`; beside the crate it is one `rsync`, shared checkout, or restored CI cache away.
///
/// A probe that fails answers with the compiler's name rather than a fixed string, so an unidentifiable toolchain differs from an identified one and forces a rebuild rather than silently inheriting someone else's.
fn toolchain() -> String {
    let compiler = env::var("CXX").unwrap_or_else(|_| "c++".to_owned());

    Command::new(&compiler)
        .arg("--version")
        .output()
        .ok()
        .filter(|probe| probe.status.success())
        .and_then(|probe| String::from_utf8(probe.stdout).ok())
        .and_then(|version| version.lines().next().map(str::to_owned))
        .unwrap_or_else(|| format!("unidentified {compiler}"))
}

fn build_marker() -> String {
    let schema = build_schema();
    let target = env::var("TARGET").unwrap();
    let toolchain = toolchain();

    format!(
        "version={BINARYEN_VERSION}\nsource={BINARYEN_SOURCE_SHA256}\nschema={schema}\ntarget={target}\ntoolchain={toolchain}\n"
    )
}

/// Where a cache entry records that it is done: written last by [`build`], so its presence is what says the entry is usable.
///
/// Spelled here rather than at each use because [`main`] declares it to Cargo as a rerun input and [`build`] reads and writes it, and the two must be the same file.
fn done_marker(entry: &Path) -> PathBuf {
    entry.join("done")
}

fn lock(path: &Path) -> File {
    let file = OpenOptions::new()
        .create(true)
        .truncate(false)
        .read(true)
        .write(true)
        .open(path)
        .unwrap_or_else(|error| panic!("open cache lock {}: {error}", path.display()));
    file.lock()
        .unwrap_or_else(|error| panic!("lock cache {}: {error}", path.display()));
    file
}

/// The names the static library takes: `libbinaryen.a`, or `binaryen.lib` on MSVC.
const LIBRARY_NAMES: [&str; 2] = ["libbinaryen.a", "binaryen.lib"];

/// The static library in `directory`, under whichever name the platform gives it.
fn library_in(directory: &Path) -> Option<PathBuf> {
    LIBRARY_NAMES
        .iter()
        .map(|name| directory.join(name))
        .find(|path| path.is_file())
}

/// Remove `directory` and make it again, empty. This script is the only writer in its `OUT_DIR`.
fn fresh(directory: &Path) {
    if directory.exists() {
        fs::remove_dir_all(directory)
            .unwrap_or_else(|error| panic!("empty {}: {error}", directory.display()));
    }
    fs::create_dir_all(directory)
        .unwrap_or_else(|error| panic!("create {}: {error}", directory.display()));
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
        .map_err(|error| format!("reading response body from {url} failed: {error}"))?;
    Ok(bytes)
}

fn instructions_on_failure(archive_path: &Path, cause: &str) -> ! {
    panic!(
        "\n\ncould not obtain the Binaryen source needed to build curios-binaryen:\n  {cause}\n\n\
        To build offline, download this file by hand:\n  {}\n\
        verify it has sha256:\n  {BINARYEN_SOURCE_SHA256}\n\
        and place it at:\n  {}\n\
        then re-run the build.\n\n",
        source_url(),
        archive_path.display(),
    );
}

/// The source archive, checked against its pinned hash in memory and never stored: one placed in the cache entry by hand, for an offline build, or else a download.
fn source(entry: &Path) -> Vec<u8> {
    let placed = entry.join(format!("{BINARYEN_VERSION}.tar.gz"));
    let bytes = match fs::read(&placed) {
        Ok(bytes) => bytes,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            download(&source_url()).unwrap_or_else(|error| instructions_on_failure(&placed, &error))
        }
        Err(error) => panic!("read Binaryen archive {}: {error}", placed.display()),
    };

    let actual = sha256_hex(&bytes);
    if actual != BINARYEN_SOURCE_SHA256 {
        instructions_on_failure(
            &placed,
            &format!("sha256 mismatch: expected {BINARYEN_SOURCE_SHA256}, got {actual}"),
        );
    }

    bytes
}

/// Fill the cache `entry` unless its marker already names this build: unpack the source into `out`, build and install it there, keep the static library alone in the entry's `lib/`, and empty `out` again.
fn build(entry: &Path, out: &Path) {
    let done = done_marker(entry);
    let cached = entry.join("lib");
    let marker = build_marker();

    if fs::read_to_string(&done).is_ok_and(|contents| contents == marker)
        && library_in(&cached).is_some()
    {
        return;
    }

    let _ = fs::remove_file(&done);
    if cached.exists() {
        fs::remove_dir_all(&cached).expect("remove the cached Binaryen library");
    }
    fresh(out);

    let archive = source(entry);
    Archive::new(GzDecoder::new(archive.as_slice()))
        .unpack(out)
        .expect("extract Binaryen source archive");

    let prefix = cmake::Config::new(out.join(format!("binaryen-{BINARYEN_VERSION}")))
        .profile("Release")
        .define("BUILD_SHARED_LIBS", "OFF")
        .define("BUILD_TOOLS", "OFF")
        .define("BUILD_TESTS", "OFF")
        .define("ENABLE_WERROR", "OFF")
        .build();

    // CMake installs into `lib` or `lib64` as the platform prefers; the entry keeps one `lib/` either way.
    let installed = ["lib", "lib64"]
        .iter()
        .find_map(|directory| library_in(&prefix.join(directory)))
        .unwrap_or_else(|| {
            panic!(
                "Binaryen did not install its static library under {}",
                prefix.display()
            )
        });
    fs::create_dir_all(&cached).expect("create the cached Binaryen library's directory");
    fs::copy(&installed, cached.join(installed.file_name().unwrap()))
        .expect("keep the Binaryen library in the cache");
    fs::write(done, marker).expect("mark Binaryen cache done");

    fresh(out);
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    // The cache marker carries the C++ toolchain's own identity, and `toolchain()` reads `CXX` to find it — but a build script only re-runs on an environment change it declares. Without this, switching `CXX` replays the cached link directives and the marker is never consulted, which is precisely the case the probe exists for.
    println!("cargo:rerun-if-env-changed=CXX");

    let target_triple = env::var("TARGET").unwrap();
    let binaryen_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap())
        .join(".artifacts")
        .join(&target_triple);
    // Every directive this script emits is an absolute path into the entry below, and the entry sits outside Cargo's target tree — so nothing else tells Cargo when it goes away. Without this, deleting `.artifacts` by hand replays stale `-L` paths into a directory that no longer exists instead of rebuilding it. `curios/build.rs` declares its launcher for the same reason, and states it.
    println!(
        "cargo:rerun-if-changed={}",
        done_marker(&binaryen_dir).display()
    );

    fs::create_dir_all(&binaryen_dir).expect("create Binaryen cache entry");
    let _lock = lock(&binaryen_dir.join("lock"));

    build(&binaryen_dir, &PathBuf::from(env::var("OUT_DIR").unwrap()));

    println!(
        "cargo:rustc-link-search=native={}",
        binaryen_dir.join("lib").display()
    );
    println!("cargo:rustc-link-lib=static=binaryen");

    if target_triple.contains("apple") {
        println!("cargo:rustc-link-lib=c++");
    } else if target_triple.contains("linux") {
        println!("cargo:rustc-link-lib=stdc++");
    }
}
