# An optimizer query costs what its answer uses

The continuation optimizer asks its module questions whole: a pass that reads a handful of facts builds an analysis of every function, every value or every node, built again wherever a pass has written the module since ([`curios-cps`'s README](../../../curios-cps/README.md#a-fact-lives-as-long-as-the-modules-generation-it-was-read-at)). Counted over a scratch copy of `76064910b` holding a fact's members by number as they now are, the facts built whole cost a little over half of the optimizer over the corpus and two thirds of it over the whole `/std` test program, where the value flow the row tests read is a fifth and nearly half of it (*What is known*). The rule this specification holds the optimizer to is the one [Every stage but reduction costs the graph it is handed](../../design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md) already holds the checkers to: a declaration is read a fact at a time, and a judgment is remembered for as long as what it read stands. Here, a pass builds the facts it reads and no other. One question a pass asks of a place in the module, placement's, costs the chain before the place at every ask, and breaks that rule outright: [Placement reads a reader's place in its graph](100-placement-reads-a-readers-place-in-its-graph.md) takes it first.

The next step is stage 1, each narrowing prototyped and counted; stage 2 waits on a consumer. None waits on [item tasks](300-item-tasks.md), which this serial work precedes.

## What is missing

**A fact is built for readers that never read it.** `analyze_calls` (`curios-cps/src/analysis.rs`) builds, wherever a pass asks it of a module written since, the call sites, the body-only call graph and its components, the owner of each known call and `LetFun`, the escapes, and recursion closed under definition. The components are read by known-value propagation and component specialization alone; recursion by those two, the inliner and the shrinker; and the shrinker, which reads recursion alone, builds all of it.

**A bounded copy enumerates before it declines.** `CopyPlan::limited` (`curios-cps/src/optimize/clone.rs`) checks the distinct-node allowance when a pending body is popped, but a visited `LetFun` first reads every defined member, inserts its identity in `nested` and queues its body, and a `LetCont` likewise its complete group, so a request allowed sixteen nodes can enumerate thousands of members before its seventeenth node refuses it. Synthetic wide-group controls establish the shape; no program measured supplies one (*What is known*). It is the documented qualification of [bounded copy admission](../../../curios-cps/README.md#a-copy-is-planned-within-its-allowance), not a violation of its copied-node ceiling.

## What is known

- **The protocol.** Counted, every figure below over a scratch copy of `76064910b` holding a fact's members by number as they now are, but the refused copies', at `d8f53c21c`: a thread-local `curios_profile::Instructions::open_alone` count sampled as the user-space instructions retired by the optimizer whole, by each build of a fact the module files, by the shrinker's census where a pass takes it, and by each part of the call analysis, which also samples the owners it files and those a known call or a `LetFun` is; a refusing `CopyPlan::limited`'s queue sampled at its refusal. Built `--release --features profile`, run as `curios --profile <stream> wonder --stage wasm <program>` over `programs/`, the TOML driver of `fixpoint_pass_measurements` and a JSON driver that decodes its first argument and encodes it back — the corpus — and as `curios --profile <stream> test --manifest <copy>/curios.toml` over a fresh copy of `curios-prelude/std` and a fresh `CURIOS_CACHE` — the whole `/std` test program, its 320 tests one program; each sample's values summed. The instrumented build emits the Wasm the uninstrumented one does, byte for byte, for every program of the corpus.
- **What each question costs.** Of the optimizer's retired instructions:

  | Question | Corpus | Whole `/std` tests |
  | --- | ---: | ---: |
  | optimizer | 48.8 G | 181.2 G |
  | the row tests' value flow | 8.7 G | 81.8 G |
  | call analysis | 5.0 G | 12.2 G |
  | `demands` | 3.7 G | 8.8 G |
  | shrinker's census | 2.9 G | 6.2 G |
  | scopes | 2.8 G | 5.8 G |
  | `origins` | 1.8 G | 3.2 G |

- **What the call analysis spends.** Over the corpus, in 1,347 builds: bodies collected into a set before the walk, 1.13 G; the rest of the walk, 2.22 G, filing 3.23 million owners where 0.34 million are of a known call or a `LetFun`; recursion closed, 1.21 G; the body graph's components, 0.33 G. Over the whole `/std` test program, in 83: 2.05 G, then 5.29 G filing 6.04 million owners for 0.99 million, then 3.46 G and 1.01 G. `Scopes::of` costs 2.84 G over the corpus's 528 builds and 5.78 G over the `/std` test program's 28.
- **What a refused copy queues.** Over the corpus and the whole `/std` test program, 170,000 refusals, none with more than 60 bodies queued or 45 members recorded.
- **Peers.** Each analysis is its own object, asked for by the pass that reads it: MLIR computes one "lazily on-demand" ([Pass Infrastructure](https://mlir.llvm.org/docs/PassManagement/)), LLVM hands a pass each it names by `getResult` ([Using the New Pass Manager](https://llvm.org/docs/NewPassManager.html)), GCC computes dominance where a pass calls for it and frees it whole (`calculate_dominance_info`, `free_dominance_info`, [`dominance.cc`](https://github.com/gcc-mirror/gcc/blob/master/gcc/dominance.cc)). A plan is stopped where its allowance runs out, not after: Waddell and Dybvig's effort counter aborts an inlining attempt the moment it is spent (*Fast and Effective Procedure Inlining*, SAS 1997), as Guile's `peval` does ("If peval exceeds the effort counter while attempting to inline a call site, it aborts the inlining attempt").

## Decided

- **No figure moves an answer.** Each stage is held to the optimized CPS and both Wasm artifacts unchanged, byte for byte, over the corpus, and to the whole `/std` test program passing: a question answered for less is answered the same.
- **A narrowing pays a percent.** One is taken where its counted saving is at least 1% of the optimizer over the corpus or over the whole `/std` test program, and dropped from this file with its figure otherwise.

## Stages

1. **The narrowings worth taking.** Each prototyped over the module's filed facts and counted, and taken where it pays a percent: recursion closed only for its readers. Check: the optimized CPS and both Wasm artifacts byte-equal over the corpus; the counted saving.
2. **A bounded copy stops at its allowance.** Waits for a consumer: attribution naming a program whose compilation pays for group enumeration. Then group bodies enumerated incrementally in the shared planner, counted against the allowance as an effort counter is, keeping complete successful extents, deduplication and fresh-plan execution; no second fuel limit, and no change to which copies fit. Check: counted group-member enumeration and pending work over doubling groups, identical exact-limit plans and nested captures, unchanged consumer artifacts and scheduling.

## Verification

`cargo xtask workspace lint` and each stage's tests by name; `curios-prelude`'s corpus tests, which reach `curios-cps`; the counted figures above retaken by the protocol at each stage, each with its commit.

## Retirement

Once stage 1 has landed and stage 2's has landed or the work is restated without it, `curios-cps`'s README states that a pass builds the facts it reads, beside the decision that a fact lives as long as the module's generation it was read at, and this file is deleted.
