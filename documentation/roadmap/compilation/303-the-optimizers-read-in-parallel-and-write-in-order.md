# The optimizers read in parallel and write in order

**Not refined yet.** This specification reserves running the ANF and CPS optimizers, and the lowering between them, on several workers, the stages [item tasks](301-item-tasks.md) leaves out. What it gathers is the state of the art and the shape that fits these stages; what it waits on is a reading of what share of a compile they take, below under *What it waits on*, and its refinement is the session that takes it.

## What is known

- **The stages are whole-module.** Erasure files a fragment a declaration, which `Module::link` (`curios-anf/src/link.rs`) joins into one program module; `optimize` (`curios-anf/src/optimize.rs`) runs the ANF passes over it whole; `lower_to_cps` (`curios-anf/src/into_cps.rs`) lowers it whole, its top level, the item chain and the entry block, becoming one parameterless entry `main`; and `optimize` (`curios-cps/src/optimize.rs`) runs rounds of the CPS passes over that module whole, every write through one door, `Written::write` (`curios-cps/src/module/written.rs`).
- **The CPS optimizer already parts reading from writing.** A pass discovers its candidates against one snapshot, discovery mints nothing, and the independent candidates are applied in a deterministic order ([A pass applies the independent plans its snapshot admits](../../../curios-cps/README.md#a-pass-applies-the-independent-plans-its-snapshot-admits)); what a pass reads off the module whole is the module's at the generation it was read at ([A fact lives as long as the module's generation it was read at](../../../curios-cps/README.md#a-fact-lives-as-long-as-the-modules-generation-it-was-read-at)).
- **Its budgets are spent in an order that is part of the output.** Specialization spends one module-wide, non-refundable allowance of copied weight, contexts taken by priority, ties broken by identity (`CopyBudget`, `CopyPriority`); a copier's allowance is one figure for the module (`optimize.rs`).
- **Some facts are the whole module's and some a function's.** The value flow is a 0CFA over the whole module on the shared solver ([A closure call reaches only what flows to it](../../../curios-cps/README.md#a-closure-call-reaches-only-what-flows-to-it)), and so are the call analysis and the closed recursion; liveness and the sharing reads walk one function's blocks (`curios-cps/src/sharing.rs`), and representation is decided at each boundary whose parties the module sees ([Representation is decided wherever every party is visible](../../../curios-cps/README.md#representation-is-decided-wherever-every-party-is-visible)).
- **The ANF passes are few** ([The ANF optimizer is thin](../../../curios-anf/README.md#the-anf-optimizer-is-thin)): pruning, compile-time evaluation of closed terms, reconstruction over a graph of construction sites, and the two recursions made loops, each over one recursion.
- **Neither crate holds a Core term.** `curios-anf` and `curios-cps` depend on no `curios-core`, so making their state cross a thread waits on nothing of item tasks' term representation, only on their own: the facts are `Rc`s behind a `RefCell` (`Readings`, `curios-cps/src/module/readings.rs`), and the solver and the sharing reads keep `RefCell`s (`curios-cps/src/dataflow.rs`, `sharing.rs`).

## Peers

**What worked.**

- **Binaryen**, the closest peer, a WebAssembly optimizer: a pass opts in as function-parallel (`isFunctionParallel`), is instantiated once per function, and a run of such passes is batched over all functions on a pool, every other pass running over the module serially ([`pass.h`](https://chromium.googlesource.com/external/github.com/WebAssembly/binaryen/+/version_33/src/pass.h), [`pass.cpp`](https://chromium.googlesource.com/external/github.com/WebAssembly/binaryen/+/version_17/src/passes/pass.cpp)).
- **MLIR** runs a pass in parallel over operations isolated from above, whose use-def chains cross no region boundary, which makes every pipeline implicitly multithreaded ([the MLIR paper](https://arxiv.org/pdf/2002.11054)).
- **GCC's WHOPR** makes every interprocedural decision once, serially, over the whole call graph (WPA), then compiles balanced partitions in parallel (LTRANS) ([GCC's LTO overview](https://gcc.gnu.org/onlinedocs/gccint/LTO-Overview.html)); Cranelift under Wasmtime compiles each function apart and joins them in order, which Curios already runs.
- **Whole-program fixpoints parallelize, at a cost in engineering.** Inclusion-based points-to analysis reaches up to 3× on eight cores as graph rewriting (Méndez-Lojo, Mathew and Pingali, [OOPSLA 2010](https://www.doi.org/10.1145/1869459.1869495)), the value flow's own algorithm family.

**What went wrong.**

- **A partition is an inlining boundary.** rustc's codegen units split a crate so LLVM compiles the parts in parallel, and inlining across a part is lost: `std` inlining "goes quite wrong" with several units, one unit buying large runtime speedups, and ThinLTO was taken up to win it back ([Let's talk about parallel codegen](https://internals.rust-lang.org/t/lets-talk-about-parallel-codegen/2759)). An interprocedural decision here is the module's, read off the whole module, and stays so.
- **The shared tables become the bottleneck.** MLIR's uniquer is sharded with a cache per thread to cut lock contention, and an atomic in that cache was a bottleneck removed in 2024 ([PR #93270](https://lists.llvm.org/pipermail/mlir-commits/2024-May/057609.html)); item tasks names Curios's interners for the same reason.
- **Isolation is what a function-parallel pass needs, and these functions are not isolated.** A CPS function is lexically nested and reads its parent's bindings, so it is not isolated from above in MLIR's sense, and a rewrite of one may move what another reads; GHC's simplifier runs serially within a module, and MLton's whole-program passes, the shape of this value flow, run on one thread.

## The shape that fits

These are directions for the refining session, each to be settled there.

- **One writer, readers in parallel.** Every write stays the module's one door, made in the order it is made today. What reads a snapshot runs on the workers: a pass's discovery of its candidates, function by function, merged in identity order, so the plans, their order and the bytes are those of one worker, and every budget is spent by the writer as it is spent now. Discovery minting nothing is what makes this hold already.
- **A function's facts are built on the workers, the module's on one.** Liveness and the sharing reads are a function's, built in parallel and joined in identity order; the value flow, the call analysis and the closed recursion stay serial until their share is measured, and a parallel solver for the value flow is a question of its own.
- **A closed term is evaluated on a worker.** The ANF optimizer's compile-time evaluation of a closed term is a function of the term, so each candidate's evaluation runs apart and its result is applied in order.
- **The lowering is an item's.** Lowering an item into the CPS module is a function of the item, as erasure's fragment is of its declaration, so items lower apart and join in item order where `main` binds them; whether the lowering composes so is the refinement's first question.
- **A write confined to one function is last.** A local pass rewriting one function on a worker needs identities minted in an order no schedule moves, a range of the function's own or a renumbering at the join, and a function whose rewrite reads nothing a sibling's rewrite writes, which the scope tree and not a flat list of functions decides.
- **The determinism contract is item tasks'.** The same bytes for every number of workers, held by its differential, through the executor seam it lands.

## What it waits on

- **A reading of the share.** No figure yet says what part of a compile these stages take, on `/std`'s executables and on the corpus in `programs/`: the `curios profile` summary of one compile, by stage, at a commit, counted and timed, is the first step, and these stages are worth parallelizing only where that share is.
- **The growths ahead of it.** The uncertain growth entry of [the roadmap's root findings](../000-findings.md) holds growths past their bound in the ANF optimizer and the CPS lowering; a superlinear pass on many workers stays superlinear, so its cause is settled first.
- **Item tasks' executor seam**, its stage 2, and the facts made shareable, which is this crate pair's own change and not item tasks' term representation.

## Non-goals

Partitioning the module for an interprocedural decision; the emitter, which item tasks keeps out beside Cranelift; a second representation of either module for the workers.
