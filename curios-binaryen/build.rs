//! Downloads, verifies, and builds the pinned Binaryen source release.
//!
//! Cargo gives distinct build-script fingerprints their own `OUT_DIR`, so that
//! directory cannot cache an expensive C++ build shared by ordinary builds,
//! tests, and Clippy. This script instead keeps one locked cache beneath
//! Cargo's target tree. A cache entry is complete only after CMake installs the
//! static library and the script writes its completion marker.

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

const VERSION: &str = "version_130";
const SOURCE_SHA256: &str = "20d727e7f3011cfe604b8ebdc873edbb4831c6b148209cb15bc2bedcded036ee";
const BUILD_SCHEMA: &str = "1";

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

fn cache_key(target: &str, host: &str) -> String {
    let material = format!(
        "version={VERSION}\nsource={SOURCE_SHA256}\nschema={BUILD_SCHEMA}\ntarget={target}\nhost={host}"
    );
    sha256_hex(material.as_bytes())[..24].to_string()
}

/// Find the target scope from Cargo's
/// `<scope>/<profile>/build/<package-hash>/out` layout. For host builds the
/// scope is the target directory; for explicit targets it is that target's
/// triple directory. Either way, different profiles and fingerprints share it.
fn cache_root(out_dir: &Path) -> PathBuf {
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

    target_scope.join("binaryen")
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
        verify it has sha256:\n  {SOURCE_SHA256}\n\
        and place it at:\n  {}\n\
        then re-run the build.\n\n",
        source_url(),
        archive_path.display(),
    );
}

fn archive(archive_path: &Path) -> Vec<u8> {
    let bytes = match fs::read(archive_path) {
        Ok(bytes) => bytes,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            let bytes = download(&source_url())
                .unwrap_or_else(|error| instructions_on_failure(archive_path, &error));
            fs::write(archive_path, &bytes).expect("write downloaded Binaryen archive");
            bytes
        }
        Err(error) => panic!("read Binaryen archive {}: {error}", archive_path.display()),
    };

    let actual = sha256_hex(&bytes);
    if actual != SOURCE_SHA256 {
        instructions_on_failure(
            archive_path,
            &format!("sha256 mismatch: expected {SOURCE_SHA256}, got {actual}"),
        );
    }

    bytes
}

fn build(entry: &Path) {
    let work = entry.join("work");
    let complete = entry.join("complete");
    let archive_path = entry.join(format!("{VERSION}.tar.gz"));

    if complete.is_file() && static_library_exists(&work) {
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

    let source = work.join(format!("binaryen-{VERSION}"));
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
    fs::write(complete, b"complete").expect("mark Binaryen cache complete");
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let target = env::var("TARGET").unwrap();
    let host = env::var("HOST").unwrap();
    let root = cache_root(&out_dir);
    fs::create_dir_all(&root).expect("create Binaryen cache root");

    let key = cache_key(&target, &host);
    let entry = root.join(&key);
    fs::create_dir_all(&entry).expect("create Binaryen cache entry");
    let _lock = lock(&root.join(format!("{key}.lock")));

    build(&entry);

    let destination = entry.join("work");
    println!(
        "cargo:rustc-link-search=native={}/lib",
        destination.display()
    );
    println!(
        "cargo:rustc-link-search=native={}/lib64",
        destination.display()
    );
    println!("cargo:rustc-link-lib=static=binaryen");

    if target.contains("apple") {
        println!("cargo:rustc-link-lib=c++");
    } else if target.contains("linux") {
        println!("cargo:rustc-link-lib=stdc++");
    }
}
