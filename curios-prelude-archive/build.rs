//! The elaborating half of the fixed prelude's build, as Cargo runs it: the work is `elaborate.rs`'s, filed where the crate that embeds the images reads them.

mod elaborate;

use {
    elaborate::elaborate,
    std::{env, path::PathBuf},
};

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=elaborate.rs");

    // Filed under `OUT_DIR`, where Cargo keeps a build script's outputs, because the crate that includes them is their one reader: nothing outside the build opens an image.
    let out = PathBuf::from(
        env::var_os("OUT_DIR").expect("Cargo runs a build script with `OUT_DIR` set"),
    );

    // Each file the lowering read, rather than the directory: the read set is closed — a module joins `/std` only through a `mod` in a header that is itself a read, so the edit that adds a file changes a file already watched, and the rerun then records the new one. Watching the directory would also rerun this build for anything else written under it, and a question asked about a `/std` module writes its store's memo there.
    for path in elaborate(&out) {
        println!("cargo:rerun-if-changed={}", path.display());
    }
}
