//! Downloads, verifies, and builds the pinned Binaryen source release.
//!
//! Cargo gives distinct build-script fingerprints their own `OUT_DIR`, so that directory cannot cache an expensive C++ build shared by ordinary builds, tests, and Clippy. This script instead keeps one locked cache per compilation target in `.artifacts/` beside this crate — outside Cargo's target tree, so `cargo clean` does not take a build measured in minutes with it. A cache entry is complete only after CMake installs the static library and the script writes its versioned completion marker.

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

fn static_library_exists(destination: &Path) -> bool {
    ["lib", "lib64"].iter().any(|directory| {
        ["libbinaryen.a", "binaryen.lib"]
            .iter()
            .any(|library| destination.join(directory).join(library).is_file())
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

fn archive(archive_path: &Path) -> Vec<u8> {
    match fs::read(archive_path) {
        Ok(bytes) => {
            if sha256_hex(&bytes) == BINARYEN_SOURCE_SHA256 {
                return bytes;
            }
            // A corrupted cache entry (e.g. a truncated download persisted by an earlier failure) must not wedge every subsequent build: drop it and fall through to a fresh download.
            fs::remove_file(archive_path).unwrap_or_else(|error| {
                panic!(
                    "remove corrupted Binaryen archive {}: {error}",
                    archive_path.display()
                )
            });
        }
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
        Err(error) => panic!("read Binaryen archive {}: {error}", archive_path.display()),
    }

    let bytes = download(&source_url())
        .unwrap_or_else(|error| instructions_on_failure(archive_path, &error));
    let actual = sha256_hex(&bytes);
    if actual != BINARYEN_SOURCE_SHA256 {
        instructions_on_failure(
            archive_path,
            &format!("sha256 mismatch: expected {BINARYEN_SOURCE_SHA256}, got {actual}"),
        );
    }
    fs::write(archive_path, &bytes).expect("write downloaded Binaryen archive");

    bytes
}

fn build(entry: &Path) {
    let work = entry.join("work");
    let complete = entry.join("complete");
    let archive_path = entry.join(format!("{BINARYEN_VERSION}.tar.gz"));
    let marker = build_marker();

    if fs::read_to_string(&complete).is_ok_and(|contents| contents == marker)
        && static_library_exists(&work)
    {
        return;
    }

    if work.exists() {
        fs::remove_dir_all(&work).expect("remove incomplete Binaryen build");
    }
    fs::create_dir_all(&work).expect("create Binaryen work directory");
    let _ = fs::remove_file(&complete);

    let archive_bytes = archive(&archive_path);
    let tar = GzDecoder::new(archive_bytes.as_slice());
    Archive::new(tar)
        .unpack(&work)
        .expect("extract Binaryen source archive");

    let source = work.join(format!("binaryen-{BINARYEN_VERSION}"));
    cmake::Config::new(source)
        .out_dir(&work)
        .profile("Release")
        .define("BUILD_SHARED_LIBS", "OFF")
        .define("BUILD_TOOLS", "OFF")
        .define("BUILD_TESTS", "OFF")
        .define("ENABLE_WERROR", "OFF")
        .build();

    assert!(
        static_library_exists(&work),
        "Binaryen did not install its static library under {}",
        work.display()
    );
    fs::write(complete, marker).expect("mark Binaryen cache complete");
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    // The cache marker carries the C++ toolchain's own identity, and `toolchain()` reads `CXX` to find it — but a build script only re-runs on an environment change it declares. Without this, switching `CXX` replays the cached link directives and the marker is never consulted, which is precisely the case the probe exists for.
    println!("cargo:rerun-if-env-changed=CXX");

    let target_triple = env::var("TARGET").unwrap();
    let binaryen_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap())
        .join(".artifacts")
        .join(&target_triple);
    fs::create_dir_all(&binaryen_dir).expect("create Binaryen cache entry");
    let _lock = lock(&binaryen_dir.join("lock"));

    build(&binaryen_dir);

    let binaryen_install_dir = binaryen_dir.join("work");
    println!(
        "cargo:rustc-link-search=native={}/lib",
        binaryen_install_dir.display()
    );
    println!(
        "cargo:rustc-link-search=native={}/lib64",
        binaryen_install_dir.display()
    );
    println!("cargo:rustc-link-lib=static=binaryen");

    if target_triple.contains("apple") {
        println!("cargo:rustc-link-lib=c++");
    } else if target_triple.contains("linux") {
        println!("cargo:rustc-link-lib=stdc++");
    }
}
