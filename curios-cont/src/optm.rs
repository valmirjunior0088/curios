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
//! - [`common_subexpressions`] — rebinds a duplicate pure computation (or
//!   aggregate construction) as an alias of its first occurrence on the same
//!   region path, for copy propagation to collapse.
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
//! - [`tail_recursion`] — rewrites a function whose every direct self-call is
//!   a tail call into a loop (a header block plus backward jumps), taking it
//!   out of the direct-call cycles that exclude it from inlining.
//! - [`loops`] — the shared loop-shape recognizer over the headers
//!   `tail_recursion` mints (and inlining splices intact).
//! - [`loop_invariant_motion`] — moves a loop header's invariant computations
//!   and allocations out to the entering region, once per entry instead of
//!   per iteration.
//! - [`function_inlining`] — splices a callee's body into its call sites; a
//!   single-call-site rule unfolds any-sized callees once, and a size-bounded
//!   multi-site rule dissolves small callees (e.g. the primitive wrappers)
//!   at every site.
//! - [`jump_threading`] — merges single-predecessor blocks into their predecessor.
//! - [`jump_argument_propagation`] — substitutes a block parameter that every
//!   edge feeds the same single value (`p = φ(v, p, …)` is `p = v`), exposing
//!   the closures loops thread as parameters to lifting and inlining.
//! - [`tag_threading`] — threads a jump through the tail its known argument
//!   already decides, specializing the join block per edge: a `Match` whose arm
//!   the edge picks, or an `Indirect` call whose callee the edge fixes to a
//!   known closure (devirtualized by the lift round that follows).
//! - [`dead_argument_elimination`] — drops unused function parameters and closure
//!   captures, finishing type erasure.
//! - [`dead_code_elimination`] — drops unused bindings and unreachable
//!   functions, closures, and consts.
//! - [`map_simplification`] — collapses an `Arr.map` by the identity closure to
//!   an alias of its source, letting copy propagation and dead-code elimination
//!   see through the otherwise-opaque map primitive.
//! - [`slice_forwarding`] — re-bases `len`/`get`/`slice` of a slice onto the
//!   sliced buffer, so a recursor's per-step tail slice goes unmaterialised and
//!   the quadratic free-monoid fold turns linear.

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

mod common_subexpressions;
pub use common_subexpressions::*;

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

mod tail_recursion;
pub use tail_recursion::*;

mod loops;
pub use loops::*;

mod loop_invariant_motion;
pub use loop_invariant_motion::*;

mod function_inlining;
pub use function_inlining::*;

mod jump_threading;
pub use jump_threading::*;

mod jump_argument_propagation;
pub use jump_argument_propagation::*;

mod tag_threading;
pub use tag_threading::*;

mod dead_argument_elimination;
pub use dead_argument_elimination::*;

mod dead_code_elimination;
pub use dead_code_elimination::*;

mod map_simplification;
pub use map_simplification::*;

mod slice_forwarding;
pub use slice_forwarding::*;

use {super::*, curios_base::Entropy};

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
/// 3. **Dissolve call boundaries.** Tail-recursion conversion first rewrites
///    each function whose self-calls (made direct by the self-capture seed in
///    lifting) are all tail calls into a loop, removing it from the cycles
///    inlining refuses. Then single-site inlining splices each remaining
///    callee into its one call site; jump threading dissolves the leftover
///    continuation blocks; common-subexpression elimination rebinds the
///    duplicate computations the splices juxtaposed as aliases; a settle round
///    (copy propagation + folding) collapses those aliases and the literal
///    arguments now sitting next to the primitive ops they feed.
/// 4. **Thread decided matches.** Inlining left constructor-then-eliminate
///    chains joined at multi-predecessor match blocks folding cannot decide
///    (the `Result` re-wrap in every parser combinator): known-tag threading
///    retargets each deciding edge straight to its arm, then a jump threading
///    and settle round collapse the spliced clones — exposing the intermediate
///    constructors as dead.
/// 5. **Partially evaluate pure calls.** The gap inlining cannot close: a
///    statically-pure `Direct` callee with all-literal arguments is interpreted
///    at compile time and replaced by its materialised result — dissolving
///    recursive callees (`std/Fmt`'s format-string parser combinator)
///    single-site inlining can never reach. A settle round follows.
/// 6. **Shed recursion residue, then dissolve residual wrappers.** A
///    dead-argument round first drops the self-fed arguments the converted
///    loops no longer use (chiefly the devirtualized self-closure each loop
///    still threads around), and the dead-code sweep that follows reclaims the
///    orphaned closure twins *and the call sites inside them* — often leaving a
///    loop with a single caller for the next step. Then a second inlining
///    round: the single-site rule fuses those loops, and the size-bounded
///    multi-site rule splices the tiny primitive wrappers called from several
///    specialized closures at every site. A settle round follows, and only then
///    does map simplification run: by here a field-projection closure like the
///    newtype `to_bin` has been forwarded down to the bare identity, so an
///    `Arr.map` by it collapses to an alias its consumers see through (fusing
///    `Bin.flatten(Arr.map(to_bin, xs))` to `Bin.flatten(xs)`). A second
///    known-tag threading round then catches the joins the new splices exposed.
/// 7. **Hoist constants.** Every bytestring and closed aggregate becomes a
///    shared module const, built once at startup instead of per execution.
/// 8. **Final cleanup.** Folding's decided matches and forwarded projections
///    left alias bindings and single-predecessor arms behind: a second jump
///    threading, a last common-subexpression round over the merged straight
///    lines, and a final copy propagation collapse them, dead-argument
///    elimination finishes type erasure, and dead-code elimination — last, so
///    it sees everything — reclaims the unreferenced bindings, aggregates,
///    closures, and untaken arms.
pub fn optimize(module: &mut Module) {
    // One gensym shared across every inlining pass below, so the `@{callee}#{n}`
    // freshening suffix stays unique even when a re-lifted closure is inlined by
    // more than one pass (see `inline_calls_with`).
    let entropy = Entropy::new();

    // 0. Collapse the prelude. The erased module materializes *every* builtin
    //    closure (there is no source-level reachability prune — the whole prelude
    //    is type-checked on each compile, by design), but a program reaches only a
    //    handful. An up-front dead-code sweep drops the unreachable rest so the
    //    super-linear passes below — lifting, specialization, inlining — only ever
    //    walk the reachable subset. The final sweep (§8) still reclaims whatever
    //    those passes newly expose.
    eliminate_dead_code(module);

    // 1. Settle identities.
    propagate_copies(module);
    fold_constants(module);

    // 2. Flatten the higher-order layer.
    lift_closures(module);
    specialize_calls(module);
    lift_closures(module);
    eliminate_dead_code(module);

    // 3. Dissolve call boundaries.
    convert_tail_recursion(module);
    inline_calls_with(module, &entropy);
    thread_jumps(module);
    eliminate_common_subexpressions(module);
    propagate_copies(module);
    fold_constants(module);

    // 4. Thread decided dispatch.
    thread_decided_dispatch(module);
    thread_jumps(module);
    propagate_copies(module);
    fold_constants(module);

    // 5. Partially evaluate pure calls.
    evaluate_pure_calls(module);
    propagate_copies(module);
    fold_constants(module);

    // 6. Shed recursion residue, then dissolve residual wrappers.
    eliminate_dead_arguments(module);
    eliminate_dead_code(module);
    inline_calls_with(module, &entropy);
    propagate_copies(module);
    fold_constants(module);
    // With the loop bodies inlined and settled, a recursor's per-step tail slice
    // now sits in the same region as its consumers: forwarding re-bases those
    // reads onto the original buffer, and the copy propagation and dead-code
    // sweeps below reclaim the slice that is left with no uses.
    forward_slices(module);
    simplify_maps(module);
    thread_decided_dispatch(module);
    thread_jumps(module);
    propagate_copies(module);
    // A converted loop threads its case closures around as header parameters
    // — entry passes a known closure, back edges pass the parameter through —
    // hiding them from lifting. Argument propagation substitutes them out, so
    // the lift round below devirtualizes the loop's indirect calls too.
    propagate_jump_arguments(module);
    // Threading monomorphized any closure-returning-match join (the erased
    // proof-convoy residue): the callee is now a single known closure per edge,
    // so a lift round rewrites the indirect call to a direct one, a dead-code
    // sweep reclaims the orphaned closure twins it leaves, and the inline fuses
    // the now-direct-called arm.
    lift_closures(module);
    eliminate_dead_code(module);
    inline_calls_with(module, &entropy);
    eliminate_common_subexpressions(module);
    propagate_copies(module);
    fold_constants(module);

    // 6.5. Optimize the settled loops. Inlining left the converted loops in
    //      their final shape: invariant motion pulls per-iteration work (and
    //      the invariant closure rebuilds) out to the loop entries, and the
    //      settle round collapses what the rewrites exposed. Runs before
    //      literal hoisting so hoisted *closed* data continues on to a module
    //      const. The jump threading first: the freshly inlined loop steps
    //      leave their results behind single-predecessor resume blocks.
    thread_jumps(module);
    propagate_copies(module);
    hoist_loop_invariants(module);
    propagate_copies(module);
    fold_constants(module);
    eliminate_dead_code(module);

    // 7. Hoist constants.
    hoist_literals(module);

    // 8. Final cleanup.
    thread_jumps(module);
    eliminate_common_subexpressions(module);
    propagate_copies(module);
    eliminate_dead_arguments(module);
    eliminate_dead_code(module);
}
