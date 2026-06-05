//! Cont → Cont optimization. `optm.rs` is a façade over its submodules:
//!
//! - [`walk`] — the traversal engine: a closed walker over the region tree with
//!   read-only (`Sink`) and rewriting (`SinkMut`) variants; the one place the
//!   structural recursion and the `Code` operand match live.
//! - [`harvest`] — metadata-harvesting functions (uses, references) built on the
//!   read-only walker.
//! - [`copy_propagation`] — eliminates `let x = y` renames.
//! - [`constant_folding`] — evaluates primitive ops on literal operands.
//! - [`specialize_calls`] — clones a function per closure shape passed into a
//!   candidate parameter, so closure lifting can devirtualize through it.
//! - [`closure_lifting`] — turns known closures into functions and devirtualizes
//!   their call sites.
//! - [`function_inlining`] — splices single-call-site functions into their call site.
//! - [`jump_threading`] — merges single-predecessor blocks into their predecessor.
//! - [`dead_argument_elimination`] — drops unused function parameters and closure
//!   captures, finishing type erasure.
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

mod specialize_calls;
pub use specialize_calls::*;

mod closure_lifting;
pub use closure_lifting::*;

mod function_inlining;
pub use function_inlining::*;

mod jump_threading;
pub use jump_threading::*;

mod dead_argument_elimination;
pub use dead_argument_elimination::*;

mod dead_code_elimination;
pub use dead_code_elimination::*;

use super::cont::*;

/// Run the optimization pipeline and return the rewritten module.
///
/// Copy propagation runs first so that constant folding and closure lifting see
/// real value identities. Closure lifting then turns known closures into direct
/// calls — which exposes higher-order callees as direct calls carrying known
/// closures in their candidate parameters, so specialization can clone them per
/// closure shape; a second lift devirtualizes the calls those clones expose. With
/// the higher-order layer flattened, inlining splices the resulting callees into
/// their one call site — which,
/// together with jump threading dissolving the leftover continuation blocks,
/// finally brings literal arguments next to the primitive ops the prelude wraps,
/// so a second copy-propagation and folding pass can collapse them. Folding also
/// forwards aggregate projections and decides matches on known tags, which leaves
/// alias bindings and freshly single-predecessor arms behind; a second jump
/// threading and a final copy propagation collapse those, so dead-code
/// elimination — running last to sweep the alias, literal, and closure bindings
/// the earlier passes leave behind — can reclaim the now-unreferenced aggregates
/// and untaken arms along with everything else dead.
pub fn optimize(mut module: Module) -> Module {
    propagate_copies(&mut module);
    fold_constants(&mut module);
    lift_closures(&mut module);
    specialize_calls(&mut module);
    lift_closures(&mut module);
    inline_calls(&mut module);
    thread_jumps(&mut module);
    propagate_copies(&mut module);
    fold_constants(&mut module);
    thread_jumps(&mut module);
    propagate_copies(&mut module);
    eliminate_dead_arguments(&mut module);
    eliminate_dead_code(&mut module);
    module
}
