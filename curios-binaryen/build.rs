//! Downloads, verifies, and builds the pinned Binaryen source release.
//!
//! Cargo gives distinct build-script fingerprints their own `OUT_DIR`, so that directory cannot cache an expensive C++ build shared by ordinary builds, tests, and Clippy. This script instead keeps one locked cache per compilation target beneath Cargo's target tree. A cache entry is complete only after CMake installs the static library and the script writes its versioned completion marker.

use {
    flate2::read::GzDecoder,
    sha2::{Digest, Sha256},
    std::{
        env,
        fs::{self, File, OpenOptions},
        io::Read,
        path::{Path, PathBuf},
    },
    tar::Archive,
};

const BINARYEN_VERSION: &str = "version_130";
const BINARYEN_SOURCE_SHA256: &str =
    "20d727e7f3011cfe604b8ebdc873edbb4831c6b148209cb15bc2bedcded036ee";
const BINARYEN_BUILD_SCHEMA: &str = "1";

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

fn build_marker() -> String {
    format!(
        "version={BINARYEN_VERSION}\nsource={BINARYEN_SOURCE_SHA256}\nschema={BINARYEN_BUILD_SCHEMA}\n"
    )
}

/// Find Cargo's target directory from its `<target-dir>[/<target>]/<profile>/build/<package-hash>/out` layout.
fn cargo_target_dir(out_dir: &Path, target_triple: &str) -> PathBuf {
    let package_dir = out_dir
        .parent()
        .unwrap_or_else(|| panic!("unexpected OUT_DIR: {}", out_dir.display()));
    let build_dir = package_dir
        .parent()
        .unwrap_or_else(|| panic!("unexpected OUT_DIR: {}", out_dir.display()));
    assert_eq!(
        build_dir.file_name().and_then(|name| name.to_str()),
        Some("build"),
        "unexpected OUT_DIR: {}",
        out_dir.display()
    );
    let profile_dir = build_dir
        .parent()
        .unwrap_or_else(|| panic!("unexpected OUT_DIR: {}", out_dir.display()));
    let target_scope = profile_dir
        .parent()
        .unwrap_or_else(|| panic!("unexpected OUT_DIR: {}", out_dir.display()));

    if target_scope.file_name().and_then(|name| name.to_str()) == Some(target_triple) {
        target_scope
            .parent()
            .unwrap_or_else(|| panic!("unexpected OUT_DIR: {}", out_dir.display()))
            .to_path_buf()
    } else {
        target_scope.to_path_buf()
    }
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

    let cargo_out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let target_triple = env::var("TARGET").unwrap();
    let binaryen_dir = cargo_target_dir(&cargo_out_dir, &target_triple)
        .join("binaryen")
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
