//! The fixed prelude's build, as Cargo runs it: the work is `compile.rs`'s, filed where the library that embeds the images reads them.
//!
//! The compiled crate is the verdict: the build panics on the first refusal of any stage, the kernel's among them, so the crate compiles only if the kernel accepted the whole prelude, and nothing can reach the prelude except through a crate that compiled.

mod compile;
use compile::*;

// The library's module too, which a build script cannot reach as a dependency of its own package.
#[path = "src/library.rs"]
mod library;
use library::*;

use {
    curios_package::MANIFEST,
    std::{env, path::PathBuf},
};

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=compile.rs");
    println!("cargo:rerun-if-changed=src/library.rs");

    // Filed under `OUT_DIR`, where Cargo keeps a build script's outputs, because the library that includes them is their one reader: nothing outside the build opens an image.
    let out = PathBuf::from(
        env::var_os("OUT_DIR").expect("Cargo runs a build script with `OUT_DIR` set"),
    );

    // Each file the lowering read, rather than the directory: the read set is closed — a module joins `/std` only through a `mod` in a header that is itself a read, so the edit that adds a file changes a file already watched, and the rerun then records the new one. Watching the directory would also rerun this build for anything else written under it, and a question asked about a `/std` module writes its store's memo there.
    for path in compile(&out) {
        println!("cargo:rerun-if-changed={}", path.display());
    }

    // The library's manifest is read beside its files, as a package's is: what it names, describes and says its modules answer is part of what the unit is made from.
    println!(
        "cargo:rerun-if-changed={}",
        std_directory().join(MANIFEST).display()
    );
}
