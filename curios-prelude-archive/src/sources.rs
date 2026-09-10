//! The prelude's two roots as sources: `/sys` synthesized from the host table and the intrinsic roster, `/std` read from the package this crate holds. Shared by the build script, which lowers them into the archive, and by the tests, which hold the same sources to what the archive cannot check — that they lint clean.

use {
    curios_abi::host_ops,
    curios_text::{RootSource, sys_module},
    curios_utilities::{Qualifier, RootKind},
    std::path::Path,
};

use crate::syntax::SYNTAX;

/// What `/std`'s manifest declares it is. Spelled here and in `std/curios.toml`, which `src/tests.rs` holds to agreement: reading the manifest here would make `curios-package` a build prerequisite of this crate, and so of every crate that reaches the prelude, which would re-elaborate the standard library on every manifest edit.
pub(crate) const STD_NAME: &str = "std";

/// See [`STD_NAME`].
pub(crate) const STD_DESCRIPTION: &str = "The standard library: what every Curios program gets for free, compiled into the fixed prelude beside the syntax forms and the host's operations.";

/// What `/sys` is, for the record it carries. No consumer reads this page — `curios document` is pointed at `std.rkyv` — but the record is what `/std` adopts its declarations out of, and a record states what it is about.
pub(crate) const SYS_NAME: &str = "sys";

/// See [`SYS_NAME`].
pub(crate) const SYS_DESCRIPTION: &str = "The compiler's own root: every intrinsic type former with the operations over it, the host's rows placed by the subject each names, and the propositions those operations state their preconditions in.";

/// The `/sys` root, supplied whole by `sys_module` — the first unit of the prelude fold, which nothing precedes.
///
/// **It documents itself, and that is what makes `/std`'s pages possible.** A declaration `/std` re-exports out of `/sys` is rendered from `/sys`'s *surface tree*, which stops existing the moment `/sys` is a unit of its own — a `PreparedText` carries resolution tables and an elaborated module, never the items they were read from. So `/sys` builds a record while its own tree is in hand, and `/std` adopts declarations out of that record rather than re-deriving them from items it cannot reach.
pub(crate) fn sys_source() -> RootSource {
    let mut modules = RootSource::supplied();
    modules.insert_root(
        SYS_NAME,
        RootKind::Internal,
        sys_module(&host_ops(), &SYNTAX),
    );

    modules.documented(SYS_NAME, Some(SYS_DESCRIPTION))
}

/// The `/std` root, read from the package at `manifest/std` — the second unit, compiled against `/sys`.
///
/// Two sources rather than one because they are two units: `/std` references `/sys` and `/sys` references nothing above it, so the fold has an order and each half is lowered against what precedes it. What `/syn` once made impossible was exactly this — it sat between them and referenced both.
///
/// **Read the way a package is read, because `/std` is one.** Its header is `lib.crs` beside its own `curios.toml` and its namespace *is* that directory, which is the exception `curios-package`'s layout states for a library; nothing here enumerates its modules, because a module enters a unit by being declared `mod` in a header and the resolver reads each one when discovery asks for it.
pub(crate) fn std_source(manifest: &Path) -> RootSource {
    let directory = manifest.join(STD_NAME);

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
