//! Cont → Cont optimization. `optm.rs` is a façade over its submodules:
//!
//! - [`walk`] — the traversal engine: a closed walker over the region tree with
//!   read-only (`Sink`) and rewriting (`SinkMut`) variants; the one place the
//!   structural recursion and the `Code` operand match live.
//! - [`harvest`] — metadata-harvesting functions (uses, references) built on the
//!   read-only walker.
//! - [`copy_propagation`] — eliminates `let x = y` renames.
//! - [`constant_folding`] — evaluates primitive ops on literal operands.
//! - [`closure_lifting`] — turns known closures into functions and devirtualizes
//!   their call sites.
//! - [`inlining`] — splices single-call-site functions into their call site.
//! - [`dead_code_elimination`] — drops unused bindings and unreachable
//!   functions, closures, and consts.

mod walk;
pub use walk::*;

mod harvest;
pub use harvest::*;

mod copy_propagation;
pub use copy_propagation::*;

mod constant_folding;
pub use constant_folding::*;

mod closure_lifting;
pub use closure_lifting::*;

mod inlining;
pub use inlining::*;

mod dead_code_elimination;
pub use dead_code_elimination::*;

use super::cont::*;

/// Run the optimization pipeline and return the rewritten module.
///
/// Copy propagation runs first so that constant folding and closure lifting see
/// real value identities. Closure lifting then turns known closures into direct
/// calls, and inlining splices those callees into their one call site — which
/// finally brings literal arguments next to the primitive ops the prelude wraps,
/// so a second copy-propagation and folding pass can collapse them. Dead-code
/// elimination runs last to sweep the alias, literal, and closure bindings the
/// earlier passes leave behind, plus anything they kept alive.
pub fn optimize(mut module: Module) -> Module {
    propagate_copies(&mut module);
    fold_constants(&mut module);
    lift_closures(&mut module);
    inline_calls(&mut module);
    propagate_copies(&mut module);
    fold_constants(&mut module);
    eliminate_dead_code(&mut module);
    module
}
