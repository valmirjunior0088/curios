//! The `/std` root as a source: the package authored beside this file, mounted the way a package is. What `curios-prelude-archive`'s build script lowers into the archive after `/sys`, and where every tool that reads the library asks for its place.

#[cfg(test)]
mod tests;

use {
    curios_text::RootSource,
    curios_utilities::{Qualifier, RootKind},
    std::path::{Path, PathBuf},
};

/// What `/std`'s manifest declares it is. Spelled here and in `curios.toml`, which `curios-prelude-archive`'s tests hold to agreement: reading the manifest here would make `curios-package` a prerequisite of this crate, and so of every crate that reaches the prelude, which would re-elaborate the standard library on every manifest edit.
pub const STD_NAME: &str = "std";

/// See [`STD_NAME`].
pub const STD_DESCRIPTION: &str = "The standard library: what every Curios program gets for free, compiled into the fixed prelude beside the syntax forms and the host's operations.";

/// Where `/std` is authored: this crate's `src`, where the package sits beside this file. One statement of the place, so the archive's build, its tests and every tool that reads the library ask here rather than spell a path from where they stand.
pub fn std_directory() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("src")
}

/// The `/std` root, read from the package at [`std_directory`] — the second unit of the prelude fold, compiled against `/sys`.
///
/// Two sources rather than one because they are two units: `/std` references `/sys` and `/sys` references nothing above it, so the fold has an order and each half is lowered against what precedes it.
///
/// **Read the way a package is read, because `/std` is one.** Its header is `lib.crs` beside its own `curios.toml` and its namespace *is* that directory, which is the exception `curios-package`'s layout states for a library; nothing here enumerates its modules, because a module enters a unit by being declared `mod` in a header and the resolver reads each one when discovery asks for it.
pub fn std_source() -> RootSource {
    let directory = std_directory();

    RootSource::mounted(
        STD_NAME,
        RootKind::Ordinary,
        directory.join("lib.crs"),
        directory,
    )
    // The one declaration of `/sys` anywhere. A closed root is in no unit's default set — it has no path for a manifest to name — so this is what lets `/std` wrap the intrinsics, and its absence everywhere else is what keeps them wrapped.
    .declaring([Qualifier::from(["sys"])])
    .documented(STD_NAME, Some(STD_DESCRIPTION))
}
