//! Cont → Cont optimization. `optm.rs` is a façade over its submodules:
//!
//! - [`mangle`] — the one place pass-minted names are constructed; documents
//!   the shared `base@tag#item` grammar.
//! - [`walk`] — the traversal engine: a closed walker over the region tree with
//!   read-only (`Sink`) and rewriting (`SinkMut`) variants, so no pass spells
//!   out the structural recursion or the `Code` operand match itself.
//! - [`harvest`] — metadata-harvesting functions (uses, references) built on the
//!   read-only walker.
//! - [`eval_env`] — the [`EvalEnv`] abstraction evaluation reads operands through,
//!   and its literal-map instance ([`Lits`]).
//! - [`scalar_eval`] — the wasm-faithful `Code` evaluator both folding and
//!   interpretation share, generic over an [`EvalEnv`].
//! - [`copy_propagation`] — eliminates `let x = y` renames.
//! - [`constant_folding`] — evaluates primitive ops on literal operands.
//! - [`evaluate_pure_calls`] — interprets pure-callee direct/indirect calls
//!   whose arguments are all literal, replacing them with the materialised
//!   result plus a `Jump` to the original resume; its purity classifier and
//!   interpreter live beside it in [`purity`] and [`interp`].
//! - [`hoist_literals`] — lifts bytestrings and closed aggregates into shared
//!   module consts.
//! - [`specialize_calls`] — clones a function per closure shape passed into a
//!   candidate parameter, so closure lifting can devirtualize through it.
//! - [`closure_lifting`] — turns known closures into functions and devirtualizes
//!   their call sites.
//! - [`function_inlining`] — splices a callee's body into its call sites; a
//!   single-call-site rule unfolds any-sized callees once, and a size-bounded
//!   multi-site rule dissolves small callees (e.g. the primitive wrappers)
//!   at every site.
//! - [`jump_threading`] — merges single-predecessor blocks into their predecessor.
//! - [`tag_threading`] — threads a jump through the match its known-tag
//!   argument already decides, specializing the join block per edge.
//! - [`dead_argument_elimination`] — drops unused function parameters and closure
//!   captures, finishing type erasure.
//! - [`dead_code_elimination`] — drops unused bindings and unreachable
//!   functions, closures, and consts.

mod mangle;

mod walk;
pub use walk::*;

mod harvest;
pub use harvest::*;

mod eval_env;
pub use eval_env::*;

mod scalar_eval;
pub use scalar_eval::*;

mod copy_propagation;
pub use copy_propagation::*;

mod constant_folding;
pub use constant_folding::*;

mod purity;
pub use purity::*;

mod interp;
pub use interp::*;

mod evaluate_pure_calls;
pub use evaluate_pure_calls::*;

mod hoist_literals;
pub use hoist_literals::*;

mod specialize_calls;
pub use specialize_calls::*;

mod closure_lifting;
pub use closure_lifting::*;

mod function_inlining;
pub use function_inlining::*;

mod jump_threading;
pub use jump_threading::*;

mod tag_threading;
pub use tag_threading::*;

mod dead_argument_elimination;
pub use dead_argument_elimination::*;

mod dead_code_elimination;
pub use dead_code_elimination::*;

use super::cont::*;

/// Run the optimization pipeline and return the rewritten module.
///
/// The pipeline is staged: each stage exposes the work the next one acts on.
///
/// 1. **Settle identities.** Copy propagation eliminates renames so folding and
///    closure lifting see a value's real identity; folding collapses what is
///    already literal.
/// 2. **Flatten the higher-order layer.** Lifting turns known closures into
///    direct calls, which exposes higher-order callees as direct calls carrying
///    known closures in their candidate parameters; specialization clones them
///    per closure shape; a second lift devirtualizes the calls those clones
///    expose. An interim dead-code elimination then sweeps the specialization
///    residue — the orphaned original closures still carry direct calls that
///    would inflate `inline_calls`' call-site counts.
/// 3. **Dissolve call boundaries.** Single-site inlining splices each remaining
///    callee into its one call site; jump threading dissolves the leftover
///    continuation blocks; a settle round (copy propagation + folding) collapses
///    the literal arguments now sitting next to the primitive ops they feed.
/// 4. **Thread decided matches.** Inlining left constructor-then-eliminate
///    chains joined at multi-predecessor match blocks folding cannot decide
///    (the `Result` re-wrap in every parser combinator): known-tag threading
///    retargets each deciding edge straight to its arm, then a jump threading
///    and settle round collapse the spliced clones — exposing the intermediate
///    constructors as dead.
/// 5. **Partially evaluate pure calls.** The gap inlining cannot close: a
///    statically-pure `Direct` callee with all-literal arguments is interpreted
///    at compile time and replaced by its materialised result — dissolving
///    recursive callees (the parser combinator in `examples/crs_printf.rs`)
///    single-site inlining can never reach. A settle round follows.
/// 6. **Dissolve residual wrappers.** A second inlining round: the size-bounded
///    multi-site rule splices the tiny primitive wrappers called from several
///    specialized closures at every site. A settle round follows, and a second
///    known-tag threading round catches the joins the new splices exposed.
/// 7. **Hoist constants.** Every bytestring and closed aggregate becomes a
///    shared module const, built once at startup instead of per execution.
/// 8. **Final cleanup.** Folding's decided matches and forwarded projections
///    left alias bindings and single-predecessor arms behind: a second jump
///    threading and a final copy propagation collapse them, dead-argument
///    elimination finishes type erasure, and dead-code elimination — last, so
///    it sees everything — reclaims the unreferenced bindings, aggregates,
///    closures, and untaken arms.
pub fn optimize(mut module: Module) -> Module {
    // 1. Settle identities.
    propagate_copies(&mut module);
    fold_constants(&mut module);

    // 2. Flatten the higher-order layer.
    lift_closures(&mut module);
    specialize_calls(&mut module);
    lift_closures(&mut module);
    eliminate_dead_code(&mut module);

    // 3. Dissolve call boundaries.
    inline_calls(&mut module);
    thread_jumps(&mut module);
    propagate_copies(&mut module);
    fold_constants(&mut module);

    // 4. Thread decided matches.
    thread_known_tags(&mut module);
    thread_jumps(&mut module);
    propagate_copies(&mut module);
    fold_constants(&mut module);

    // 5. Partially evaluate pure calls.
    evaluate_pure_calls(&mut module);
    propagate_copies(&mut module);
    fold_constants(&mut module);

    // 6. Dissolve residual wrappers.
    inline_calls(&mut module);
    propagate_copies(&mut module);
    fold_constants(&mut module);
    thread_known_tags(&mut module);
    thread_jumps(&mut module);
    propagate_copies(&mut module);
    fold_constants(&mut module);

    // 7. Hoist constants.
    hoist_literals(&mut module);

    // 8. Final cleanup.
    thread_jumps(&mut module);
    propagate_copies(&mut module);
    eliminate_dead_arguments(&mut module);
    eliminate_dead_code(&mut module);

    module
}
