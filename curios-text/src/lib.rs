//! Surface syntax of the `.crs` language — the compiler pipeline's first stage (curios-text → curios-elab → curios-ersd → curios-cont → curios-wasm): the parser, the printer that round-trips what was written, the module system (discovery, `use` resolution, visibility), and the `into_core` lowering that hands the rest of the pipeline a flat `curios_core::Module`.
//!
//! Lowering resolves one unit's names *against* what earlier units established, rather than over a copy of them. `Scoped` layers what the unit declares over a slice of [`PreparedText`]s — one base per unit in scope, never merged, since merging would copy every predecessor's table into every compilation — where reads span every half and writes only ever touch the unit's own. The output `Module` carries the unit's own items alone. The public-exposure audit is the one pass that genuinely crosses the boundary, since an alias may land on a type from scope, and it asks a scope rather than a map somebody merged for it. See `documentation/design/toolchain/a-module-is-a-compilation-unit-and-the-prelude-is-an-environment.md`.
//!
//! **This stage owns the logical-to-physical mapping, and the two halves are separate.** A [`Mount`](curios_utilities::Mount) binds a logical prefix to a unit and carries the privilege tier that decides what may reference an internal root; lookup is longest-match, because the entry mounts the empty prefix and every qualifier lies within it. A [`RootSource`] binds each of those prefixes to a base — qualifier `a/b/c` under the empty prefix reads `base/a/b/c.crs` — and a base is a directory or a tree already in memory, because `curios-js` supplies every body inline and compiles with no file system at all. That is the whole resolver contract: a qualifier in, a module out, with nothing about where the bytes came from. Nothing derives a root from a name's spelling: the mount table is asked, and the `RootId` stamp that used to be carried beside each declaration is gone.
//!
//! The tree is deliberately literal: sugar — function-definition signatures, infix operators, destructuring patterns, postfix `!`, string and spread literals — is kept verbatim so [`Term`]s print back as written, and is undone only during lowering. Every lowering is [`into_core_unit`], over a [`UnitSource`] pairing a resolver with the entrypoint the one unit that has one carries; [`into_core`], [`prepare_prelude`] and [`into_core_with_prelude`] are the three spellings its callers want, differing only in what they hand back. All four take an explicit [`SyntaxRegistry`](curios_utilities::SyntaxRegistry), whose slots this stage reads but never fills.
//!
//! Universe syntax is deliberately absent. Lowering owns a lexical universe allocator and assigns exactly one `curios_core::UniverseMetaId` plus a role-and-origin seed to each written nullary `Type`; it never remints that identity during elaboration. [`PreparedText`] archives the seed table and allocator floor alongside the binder floor, and entrypoint lowering resumes above both so restored prelude and user nodes cannot collide. Core alone solves, generalizes, validates, and erases the resulting levels.

mod error;
pub use error::*;

#[cfg(feature = "archive")]
mod archive;
// A deliberate narrowing of the crate-root `pub use` convention: the module's items are archive plumbing with no consumer outside the crate.
#[cfg(feature = "archive")]
pub(crate) use archive::*;

mod names;
pub use names::*;

mod root_source;
pub use root_source::*;

mod nat;
pub use nat::*;

mod intrinsic;
pub use intrinsic::*;

mod term;
pub use term::*;

mod into_core;
pub use into_core::*;

mod format;
pub use format::*;

mod print;
use print::*;

mod prelude;
pub use prelude::*;

mod parse;

mod module;
pub use module::*;
