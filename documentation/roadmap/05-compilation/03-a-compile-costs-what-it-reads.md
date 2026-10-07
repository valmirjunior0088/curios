# A compile costs what it reads

Working specification for making what a compilation does follow what its declarations read: an edit redoes the declarations that read what it changed and no other, and a unit pays for the declarations of its scope it reads, not for the scope. One environment every read goes through, and a record of those reads, are how: the record says what an edit redoes, and the environment is what a unit's scope becomes.

It needs [A declaration is a function of what it reads](02-a-declaration-is-a-function-of-what-it-reads.md) whole: a record of reads decides nothing while anything but reads reaches a declaration. [Item tasks](04-item-tasks.md) needs this, for the environment its workers share.

## What this builds on

- **The `Cache` seam and the stored-unit format** (`curios-pipeline/src/compile.rs`, `curios-unit`): a product-supplied policy the fold consults, and a serialized unit that already crosses processes.
- **A stored unit is a baseline** ([A stored unit is a baseline for an item-level recompile](../../design/compilation/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md)): a unit compiled over one is stored as the bytes it is compiled whole, and the closure it re-elaborates is `invalidated`'s (`curios-pipeline/src/recompile.rs`).
- **The certifier records what it read.** `curios_core::Reads` holds, per definition, the items whose type, universe scheme or registry entry a judgment read and the bodies it asked for, taken where the kernel consults its environment, and `curios-pipeline`'s `the_prelude_reads_along_the_graph_its_definitions_reach` holds the reads inside the elaborated graph.
- **A declaration's cells are its own**, each written once as it finishes, and a read of one not yet finished is a need the elaborator's accessors record ([A declaration is a function of what it reads](02-a-declaration-is-a-function-of-what-it-reads.md)). The accessors that record a need are where a read is recorded.

## The gap

**An edit redoes everything that reaches it.** An edit to one item of `/std` reaches 39.2 items on average through the reverse closure `invalidated` takes, 5 at the median and 1,449 for `/std/ops/Cmp`, where the kernel's recorded body reads name 5.8 with the item itself, 1 at the median. `/std/Bool/not`'s body, the census's hub, reaches 1,123 items and is read by 50. The closure is over names an item reaches, whatever it read of them, and transitive, since nothing says where a re-elaborated item came out the same. Counted at `87189c97d` over the restored prelude.

**Nothing records what the elaborator read.** The kernel's reads are its own judgment's. What elaboration read and left no name for — a body unfolded while a candidate was tried, a key at which no witness stands, which lemmas a proof was offered — is in no term, and elaboration adds 3,018 edges to the 6,488 the lowered forms of `/std`'s declarations name, 1,881 of them to a witness, so the lowered graph is not the record either. Counted at `0283c7242`, as [A declaration is a function of what it reads](02-a-declaration-is-a-function-of-what-it-reads.md) states. One question a recompile answers from its baseline instead: whether the prover's vocabulary reads a declaration is read off the baseline's elaborated graph (`vocabulary_reach`, `curios-elab/src/elaborate/module.rs`), so an edit that changes what the vocabulary reads leaves a reused declaration elaborated under the other answer, and a read that left no name is not in that graph. Only a unit that declares the vocabulary, `/std`, can meet either.

**Every compile re-seeds all its predecessors.** Erasure projects every predecessor's Core and elaboration re-seeds every definition, registering each witness again by reducing its signature to a key the unit that declared it had in hand; `Globals::of` copies each registry and maps each definition once per predecessor unit, and the kernel copies the result again (`Kernel::seed`). Compiling `programs/hello_world.crs`, `compile_entrypoint` takes 533 ms (528 to 545): elaboration 22, the kernel 31, erasure 81, and the back half 385, which is [item tasks](04-item-tasks.md)'s. Timed, the median of nine compiles under a release `profile` build at `87189c97d`. An item-level recompile pays the same floor for each question: `std_recompile_closure_census`'s leaf edit, whose closure is one item, recompiles in 1.3 s, the median of five readings that all read 1.3 s.

**The fold is a line.** `Predecessors` are every unit before this one rather than what this one depends on, and `curios-package` has the dependency graph and flattens it into that line.

**A void attempt is forgotten.** An attempt that read a declaration not yet elaborated is made again, and the next compile of the same unit finds the same order again, attempt by attempt. How many attempts a compile of `/std` voids is not yet counted.

## Prior art

- **A trace of what was read decides what is rebuilt, and an unchanged result stops there.** Mokhov, Mitchell and Peyton Jones: a verifying trace is "to record the values/hashes of dependencies used last time, and if something has changed, the key is dirty and must be rebuilt", and under early cutoff, "When it executes a task and the result is unchanged from the previous build, it is unnecessary to execute the dependent tasks" ([Build Systems à la Carte](https://www.microsoft.com/en-us/research/wp-content/uploads/2018/03/build-systems.pdf), sections 4.2.2 and 2.3). The tree today is the paper's Make for what an edit dirties: a static over-approximation, read before anything runs.
- **Reads are revalidated in the order they were made.** rustc's try-mark-green: "it is important that we visit each node in `reads(Q)` in same order as they occurred in the original compilation", since a later read may exist only because of what an earlier one answered, and visited first it would run a query the new program never asks ([Incremental compilation](https://rustc-dev-guide.rust-lang.org/queries/incremental-compilation.html)). A result is compared by fingerprint, and a small query over a large one is a firewall: "even if the result of the monolithic query changes (which it is very likely to do) the small projections can still mostly be marked as green" ([Incremental compilation in detail](https://rustc-dev-guide.rust-lang.org/queries/incremental-compilation-in-detail.html)).
- **The peers cut off the same way.** Skyframe: "If a node is invalidated, but upon rebuild, it is discovered that its new value is the same as its old value, the nodes that were invalidated due to a change in this node are 'resurrected'" ([Skyframe](https://bazel.build/reference/skyframe)). salsa backdates: "even though the inputs changed, the output didn't" ([The red-green algorithm](https://salsa-rs.github.io/salsa/reference/algorithm.html)). rust-analyzer names the price, "fine-grained tracking of dependencies takes time and memory" ([Three Architectures for a Responsive IDE](https://rust-analyzer.github.io/blog/2020/07/20/three-architectures-for-responsive-ide.html)).
- **A declaration is the grain a compiler records at, and an instance is the read no name records.** GHC "calculates a fingerprint (in fact an MD5 hash) of each interface file, and of each declaration within the interface file", and "compares the fingerprints on the things it needs this time with the fingerprints on the things it needed last time"; for an instance no name leads to, it "identifies orphan modules, and visits the interface file of every orphan module below the module being compiled" ([Separate compilation](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/separate_compilation.html)). Curios's orphan rule and its keys spelled before elaboration are what let a goal's read be one key, found or absent.
- **A dependently typed compiler built this way exists.** Sixty's build library "keeps track of what dependencies a task used when it was executed (much like Shake) in a table, i.e. what keys it fetched and what the values were" ([Query-based compiler architectures](https://ollef.github.io/blog/posts/query-based-compilers.html)).
- **Verdicts are cached at the same grain.** Leino and Wüstholz cache a verifier's results per procedure, using "the program's call graph and control-flow graph to focus the verification effort on just the parts of the program that were affected by the user's most recent modifications" ([Fine-grained Caching of Verification Results](https://www.microsoft.com/en-us/research/publication/fine-grained-caching-verification-results/), CAV 2015).
- **The order a restarting build found is worth keeping.** "To reduce the number of aborts (often to zero) Excel records the discovered task order in its calc chain, and uses it as the starting point for the next build" (Build Systems à la Carte, section 4.1.2).

## Decisions

1. **One environment, and every read goes through it.** A declaration is a record of cells: its key where it is a witness, its signature, its body, its registry entries, their vectors, its totality, the kernel's verdict. Every read of another declaration is a request for one of those cells, or a question — whether a witness stands at a key, whether a declaration is one the prover's vocabulary reaches — and is recorded in the order it was made: a signature read apart from a body unfolded, a key found apart from a key absent.
2. **The record is the declaration's, stored with its unit.** The elaborator's for what the declaration elaborated to, and the certifier's own for its verdict, as `Certification` holds it.
3. **An edit redoes what read what it changed.** A declaration is reused where its lowered form is unchanged and every read its record holds finds a cell equal to the one it read. Reads are revalidated in the order they were made, and a declaration re-elaborated to equal cells dirties nothing.
4. **A reused verdict stands on the record.** [Cached verdicts](../../design/soundness/cached-verdicts.md)' per-item argument is restated over recorded reads in the change that makes invalidation read them.
5. **A unit's scope is a view.** `Established`, `Globals`, `Resumed` and `Predecessors` are views of the environment, a unit carries the key each of its witnesses registered under, and nothing of a scope is copied, replayed or keyed again, so a unit's prologue costs what the unit reads.
6. **Units are not scheduled.** A declaration in one unit reads the declarations of another, so the package graph is honoured without a fold over one order of units.

## Open questions

- **Whether a compile over a baseline asks for its declarations in the order the baseline's records give**, as a calc chain does, so that an attempt reads nothing unfinished. It waits on the count of void attempts over `/std`, which [A declaration is a function of what it reads](02-a-declaration-is-a-function-of-what-it-reads.md)'s fourth stage reads.
- **What makes two registry cells equal**: the entry's terms with the vectors the certifier filed, or with elaboration's stamps beside them.

## The components

| Component | Treatment | Stage |
| --- | --- | --- |
| Reads | Recorded where the environment is asked, per cell and per key, in order, and stored with the unit | The record |
| Invalidation | By recorded reads, revalidated in the order they were made, stopping where a re-elaborated cell comes out equal; `invalidated`'s reverse closure is deleted | Invalidation |
| Units | `Established`, `Globals`, `Resumed` and `Predecessors` are views of the environment, and a witness's key is stored | Across units |

## Stages

Each lands alone, on its own check, and says what it expects to move; a move outside that is a finding.

1. **The record.** Decisions 1 and 2. Expected to move: a stored unit's bytes, by its records, and what elaboration costs, by the recording.
2. **Invalidation.** Decisions 3 and 4. Expected to move: which declarations a recompile redoes, and the closure census's figures for its hub.
3. **Across units.** Decisions 5 and 6. Expected to move: what a compile costs before its first declaration.

## Verification

- **The record is complete.** An ignored measurement beside `std_replay_census`: each declaration of `/std` elaborated against the declarations its record names and no other gives the declaration the library holds. Counted.
- **Invalidation.** A recompile over a baseline redoes exactly the declarations whose recorded reads changed, held by fixtures over a signature change, a body change, a witness added at a key a goal found empty, and a declaration moved into what the prover's vocabulary reaches; a unit compiled over a baseline is still the bytes it is compiled whole; and `std_recompile_closure_census` is retaken for its hub.
- **Across units.** The compile of `programs/hello_world.crs` is retaken as *The gap* states it.
- **The closure's figures.** The nodes are the restored prelude's items and an edge is a name `Module::reaches`; the reverse closure is `invalidated`'s; body readers are the items whose `Certification::reads` hold the name among their bodies. Counted.

## Design decisions this overturns or corrects

- [A stored unit is a baseline for an item-level recompile](../../design/compilation/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md): invalidation by recorded reads. Invalidation.
- [Cached verdicts](../../design/soundness/cached-verdicts.md): the per-item argument restated over recorded reads. Invalidation.
- [`curios-unit`'s README](../../../curios-unit/README.md): *Predecessors are borrowed, per stage*, with `Predecessors`' own documentation. Across units.
- [A module is a compilation unit, and the prelude is an environment](../../design/compilation/a-module-is-a-compilation-unit-and-the-prelude-is-an-environment.md): a compilation stops being units folded over one dependency order. Across units.

## Rejected

- **salsa.** At 0.28 with no 1.0, it removed its experimental parallel feature in 0.25.0; its database model would own the term representation, and its invalidation would sit beside the store's trust argument rather than under it. The graph here has thousands of nodes and obligations — budgets, deterministic identities, charged caches — that a small component states directly.
- **The certifier's reads as the record.** They are what its judgment of the finished terms read, and a key found absent, a candidate tried and dropped and a lemma offered and not applied are in no finished term.
- **The names an elaborated declaration reaches as the record.** It is what a unit's items are ordered by, and it holds no read that left no name.

## Completion and retirement

Done when every read of one declaration by another goes through the environment and is recorded, a recompile redoes the declarations whose recorded reads changed and no other, and a unit's prologue copies, replays and keys nothing of its scope. The rule, its rationale and what is rejected go to a design decision under `documentation/design/compilation/`; the environment's contract is in the owning crates' READMEs and rustdoc, and `.claude/rules/`' area routing names it. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
