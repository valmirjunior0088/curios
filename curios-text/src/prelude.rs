//! The prelude's first root as a source: `/sys`, synthesized from the host table and the intrinsic roster. What `curios-prelude-archive`'s build script lowers into the archive ahead of `/std`, which is `curios-std`'s.

use {
    crate::{RootSource, SYNTAX, sys_module},
    curios_abi::host_ops,
    curios_utilities::RootKind,
};

/// What `/sys` is, for the record it carries. No consumer reads this page — `curios document --std` renders `/std`'s — but the record is what `/std` adopts its declarations out of, and a record states what it is about.
const SYS_NAME: &str = "sys";

/// See [`SYS_NAME`].
const SYS_DESCRIPTION: &str = "The compiler's own root: every intrinsic type former with the operations over it, the host's rows placed by the subject each names, and the propositions those operations state their preconditions in.";

/// The `/sys` root, supplied whole by `sys_module` — the first unit of the prelude fold, which nothing precedes.
///
/// **It documents itself, and that is what makes `/std`'s pages possible.** A declaration `/std` re-exports out of `/sys` is rendered from `/sys`'s *surface tree*, which stops existing the moment `/sys` is a unit of its own — a `PreparedText` carries resolution tables and an elaborated module, never the items they were read from. So `/sys` builds a record while its own tree is in hand, and `/std` adopts declarations out of that record rather than re-deriving them from items it cannot reach.
pub fn sys_source() -> RootSource {
    let mut modules = RootSource::supplied();
    modules.insert_root(
        SYS_NAME,
        RootKind::Internal,
        sys_module(&host_ops(), &SYNTAX),
    );

    modules.documented(SYS_NAME, Some(SYS_DESCRIPTION))
}
