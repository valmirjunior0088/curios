//! Builds and statically links the optimizer vendored in `binaryen/`:
//! https://github.com/WebAssembly/binaryen at tag `version_130`, trimmed
//! to the files the static-library CMake build needs:
//!
//! - `src/`, `CMakeLists.txt`, `config.h.in`, `LICENSE`
//! - `third_party/CMakeLists.txt`, `third_party/FP16/`, and
//!   `third_party/llvm-project/` (the Outlining pass includes LLVM
//!   suffix-tree headers unconditionally on native builds, so DWARF
//!   support stays enabled)
//! - `scripts/binaryen-lit.in`, `test/lit/{CMakeLists.txt,lit.site.cfg.py.in}`
//!   (referenced unconditionally by the root CMakeLists; configure-time only)
//!
//! Dropped: `test/` (34M), `third_party/googletest`, `fuzz/`, `scripts/`,
//! `media/`, docs.
//!
//! To update: clone the new tag, copy the same file set over `binaryen/`,
//! and bump the tag in this comment. No vendored file is patched.

#[cfg(feature = "binaryen")]
fn main() {
    use {cmake::Config, std::env};

    println!("cargo:rerun-if-changed=binaryen");

    // Always build Binaryen as Release: a Debug-profile optimizer is an
    // order of magnitude slower at runtime for no benefit to us.
    let destination = Config::new("binaryen")
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

#[cfg(not(feature = "binaryen"))]
fn main() {}
