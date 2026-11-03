//! The module system of the `.crs` language — the compiler pipeline's first stage (curios-module → curios-elab → curios-anf → curios-cps → curios-emit → curios-wasm), over the tree `curios-surface` reads a file into: loading, discovery, `use` resolution and visibility, the lints, the registry of compiler-known names, and the `into_core` lowering that hands the rest of the pipeline a flat `curios_core::Definitions`.
//!
//! Lowering resolves one unit's names *against* what earlier units established: `Scoped` layers what the unit declares over a slice of [`PreparedText`]s, one base per unit in scope, where reads span every half and writes only ever touch the unit's own. The output `curios_core::Definitions` carries the unit's own items alone. The public-exposure audit is the one pass that crosses the boundary, since an alias may land on a type from scope, and it asks a scope rather than a merged map. Why a unit is lowered over its scope and never over a copy of it is `documentation/design/compilation/a-unit-holds-its-own-definitions-and-the-prelude-is-an-environment.md`'s.
//!
//! A [`Mount`](curios_common::Mount) binds a logical prefix to a unit, looked up longest-match since the entry mounts the empty prefix, and a [`RootSource`] binds each of those prefixes to a base — qualifier `a/b/c` under the empty prefix reads `base/a/b/c.crs` — which is a directory or a tree already in memory, as `curios-js` supplies every body inline. The resolver contract is a qualifier in and a module out; why the two halves stay apart is `README.md`'s decision.
//!
//! `curios-surface`'s terms keep sugar as written, which its `README.md` decides, and lowering undoes it. Every lowering is [`into_core_unit`], over a [`UnitSource`] pairing a resolver with the entrypoint the one unit that has one carries; [`into_core`](into_core()), [`prepare_prelude`] and [`into_core_with_prelude`] are the three spellings its callers want, differing only in what they hand back. All four take an explicit [`SyntaxRegistry`](curios_common::SyntaxRegistry), whose slots this stage reads but never fills.
//!
//! Surface `Type` is nullary, as `documentation/design/theory/a-universe-level-is-implicit-cumulative-and-settles-by-where-it-stands.md` decides. Lowering owns a lexical universe allocator and assigns exactly one `curios_core::UniverseMetaId` plus a role-and-origin seed to each written `Type`; it never remints that identity during elaboration. [`PreparedText`] archives the seed table, and the allocator starts at zero for every unit, since no term in scope carries a universe metavariable for a fresh one to collide with. Elaboration solves, generalizes, validates and erases the resulting levels.

mod error;
pub use error::*;

mod archive;
// A deliberate narrowing of the crate-root `pub use` convention: the module's items are archive plumbing with no consumer outside the crate.
pub(crate) use archive::*;

mod lint;
pub use lint::*;

mod root_source;
pub use root_source::*;

mod into_core;
pub use into_core::*;

mod registry;
pub use registry::*;

mod load;
pub use load::*;
