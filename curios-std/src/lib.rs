//! The `/std` root as a source: the package authored beside this file, read as every package is. What `curios-prelude-archive`'s build script lowers into the archive after `/sys`, and where every tool that reads the library asks for its place.

#[cfg(test)]
mod tests;

use {
    curios_package::package_at,
    curios_text::RootSource,
    curios_utilities::Qualifier,
    std::path::{Path, PathBuf},
};

/// Where `/std` is authored: this crate's `src`, where the package sits beside this file. One statement of the place, so the archive's build, its tests and every tool that reads the library ask here rather than spell a path from where they stand.
pub fn std_directory() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("src")
}

/// The `/std` root, read from the package at [`std_directory`] — the second unit of the prelude fold, compiled against `/sys`.
///
/// Two sources rather than one because they are two units: `/std` references `/sys` and `/sys` references nothing above it, so the fold has an order and each half is lowered against what precedes it.
///
/// **Read as the package it is, by the reader every package has.** Its `curios.toml` names it, describes it and says what its modules answer, and `curios-package` makes the unit's source of that here exactly as it does for a package a command compiles. So the library the compiler is built with and the one a command compiles from this tree are one reading of one manifest, and nothing the manifest states is restated in Rust. Its header is `lib.crs` beside the manifest and its namespace *is* that directory; nothing here enumerates its modules, because a module enters a unit by being declared `mod` in a header and the resolver reads each one when discovery asks for it.
pub fn std_source() -> RootSource {
    let (_, source) = package_at(&std_directory())
        .unwrap_or_else(|refusal| panic!("the standard library's package is refused: {refusal}"));

    source
        .expect("the standard library's header sits beside its manifest")
        // The one declaration of `/sys` anywhere. A closed root is in no unit's default set — it has no path for a manifest to name — so this is what lets `/std` wrap the intrinsics, and its absence everywhere else is what keeps them wrapped.
        .declaring([Qualifier::from(["sys"])])
}
