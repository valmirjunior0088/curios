//! Supplies the target-scoped runtime launcher path to the `curios` binary.

use std::{env, path::PathBuf};

/// Find Cargo's target directory from its
/// `<target-dir>[/<target>]/<profile>/build/<package-hash>/out` layout.
fn cargo_target_dir(out_dir: &std::path::Path, target_triple: &str) -> PathBuf {
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

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let target_triple = env::var("TARGET").unwrap();
    let curios_runtime_bin = cargo_target_dir(&out_dir, &target_triple)
        .join("curios")
        .join(&target_triple)
        .join("runtime");

    println!("cargo:rerun-if-changed={}", curios_runtime_bin.display());
    println!(
        "cargo:rustc-env=CURIOS_RUNTIME_BIN={}",
        curios_runtime_bin.display()
    );
}
