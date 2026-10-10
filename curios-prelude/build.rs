//! The certifying half of the fixed prelude's build, as Cargo runs it.
//!
//! `curios-prelude-archive` produced one image per root. `certify.rs` restores them, walks every item with the independent kernel, each root against the roots before it, and panics on the first refusal — so this crate compiles only if the kernel accepted the whole prelude, and nothing can reach the prelude except through a crate that compiled.
//!
//! The compiled crate is the verdict. What the script files is the one thing a later walk reads of it: the certifier's record of each root's definitions — each one's totality and what judging it read — at `certification.rkyv` under `OUT_DIR`, with which the crate certifies the units it restores.

mod certify;

use {
    certify::certify,
    std::{env, path::PathBuf},
};

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=certify.rs");

    // Under `OUT_DIR`, as the images are, since the crate is the one reader of what is filed.
    let out = PathBuf::from(
        env::var_os("OUT_DIR").expect("Cargo runs a build script with `OUT_DIR` set"),
    );

    certify(&out);
}
