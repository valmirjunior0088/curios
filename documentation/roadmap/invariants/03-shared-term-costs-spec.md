# Standard-library invariants, part 3: a shared term costs its size

Working specification for the places where checking a term still costs what its tree would, where the term is a graph. A reduct shares its subterms: `Str/trim(" x ")` in a type reduces to a value whose fields name the same nodes many times over, and a pass that walks it once per path rather than once per node pays exponentially for a value built in linear time. The invariants work made the elaborator's walks graph-aware wherever the walk was pure, and made re-validation and rollbacks stop redoing work they had already done; what remains is here, with the instruments that find the next one and a harness that catches it.

Its first stage needs nothing and serves part 1 and [part 2](02-unrecorded-universes-spec.md) as well, as their budget. The rest lands stage by stage; the settlement stage follows part 1's change to solving, which has landed, since both touch what solving and re-validation walk.

## What this builds on

- **The graph-aware walks.** Term equality and hashing, `free_vars`, `capture` (memoized per node and depth), the reduction cache, level identification, the zonks (`Visit::rewriting_shared`, and `NodeMemo` for the strict one), `abstract_occurrences`, `metavar_origins`, `replace_term`, `mentions_term` and the scoped level rewrite. A memoized walk hands each occurrence its own span. Each crate that owns walks runs them over one doubling term in `every_walk_answers_a_doubling_term_in_its_own_size` — `curios-core`'s in `term/sharing_tests.rs`, `curios-elab`'s and `curios-cert`'s in `walk_tests.rs` — and a walk private to its module keeps its fixture beside it.
- **The kernel's memos.** Term-keyed tables live one declaration; the reduct tables keep a local-bearing term for as long as the equations in force stand; the `infer` memo keeps local-free terms and hands back a remembered type without replaying its mints, the freshness counter being monotone.
- **Re-validation and rollbacks.** Inside an oracle bracket each memoizable node is elaborated once, whether or not the global cache keeps it; a rollback clears the reducts and elaborations only when it unwound a term solution, and the elaborations and the universe stamp when only the universe solver moved.
- **Settlement.** A term-keyed refinement's reduced spelling is settled at most once per entry and cached as `settled_keys` (`curios-elab/src/context/caches.rs`); `reduce::settle` computes it with the entry's own frame and every frame inside it withheld, so it rests only on the frames outside.
- **The profile.** `curios-profile`'s spans, samples and notes, written through as each row is made, so a run that aborts leaves its open spans on disk. Kept for hunts: the spans `convert::outcome`, `convert::drain`, `typing::expect`, `typing::retry_parked`, `typing::drain_parked`, `ctx::park`, `ctx::wake_parked`, `reduce::settle` carrying the key and frame it settles, and the kernel's `convert::unfolded_retry`, and the samples `caches::rollback_dropped` and `caches::suppression_dropped`. On the `/std` build they read `typing::retry_parked` at 40.9 s inclusive over 63,862 calls and `convert::drain` at 19.9 s over 70,096.

## The gap

**Settlement is a quarter of elaboration.** `reduce::settle` takes 12.1 s of 51.5 s of profiled `/std` elaboration. `/std/http/Client/target_of` alone settles five times, once for 3.708 s — about 30% of all settlement — on the key `Url/of_str(Str/flatten([...]))`, whose parse settles in 214 ms. Two causes are candidates, and neither is measured. Settled spellings are cleared wholesale by every refinement registration, suppression boundary, frame exit, universe rewrite and rollback (`caches.rs`), though each rests only on the frames outside its own entry, so an entry is settled again after invalidations that could not have changed it. And erasure registers the same arms again — `into_ersd/eliminate.rs` calls `refine_head` at seven sites — so every equation is settled a second time after elaboration. Whether a type-based filter, `could_reduce_to` asked of a key's type, would skip the expensive one is not known either; that filter is sound by subject reduction. `could_reduce_to` is written twice, identically (`curios-elab/src/reduce.rs`, `curios-cert/src/kernel/scope.rs`).

**`capture` loses sharing, and `shift` and `release` walk an open term per path.** `capture` rewrites free names, so it cannot prune by `reach`, and it rebuilds even a subterm that mentions none of its binders, at each depth anew. `shift` and `release` prune by `reach`, which answers a closed subterm at once, and remember nothing: over an open shared term every node's reach is above zero, so each walks every path — releasing a captured sixty-level doubling term does not finish, and neither is yet a row of `curios-core`'s harness. The kernel types a tuple by inferring each field and building the Σ with `Term::tuple_type`, whose `Telescope::build` captures each field type at its own depth — capturing nothing, since the Σ is non-dependent — so two equal field types stop being one node, and the inferred type of `(t, t)` nested `k` deep grows as a tree: 39 ms, 331 ms, 5.3 s and 21 s at depths 10, 14, 18 and 20, with the `infer` memo on.

**The kernel's conversion infers a scrutinee's type.** Comparing a stuck match whose result is a family against one whose result is an ambient goal, `family_at_head` (`curios-cert/src/kernel/convert.rs`) calls the full `infer` on the scrutinee to read its indices, against `kernel/sort.rs`'s rule that conversion only looks a type up. A stuck scrutinee is a neutral spine, and `synth_neutral` is the lookup for one. It was not hot in the last profile.

**Walks not yet graph-aware.** The kernel types a local-bearing term once per path: its `infer` memo keeps local-free terms alone, so inferring or checking a sixty-level doubling sum over a local exhausts a million-unit budget, where reducing it and converting it answer at once; neither is yet a row of `curios-cert`'s harness. The kernel's conversion keeps no completed-pair memo: `History` drops a goal once it is decided. The totality analysis's `walk_term` (`curios-analysis/src/totality.rs`) carries each arm's effects, so it is not a pure walk. Erasure was not surveyed. Printing's output is itself a tree, so a shared term needs its shared nodes named, or its print truncated.

**Type-level claims at their budget.** The six in `curios/src/tests/corpus/strings/decomposition.crs` pass, `split_once` and `trim` near half a minute each on a debug build. `Option/map(Str/index_of("abcdefgh", 'h'), Str/At/to_offset)` in a type does not finish in 300 s; the time sat inside one `convert::solve`, in 115,892 closed-machine reductions under re-validation's per-node checks.

## Stages

Each lands alone, with a doubling-term fixture where it makes a walk graph-aware, mutation-checked.

1. **Instruments, the benchmark set and the harness.** Landed: the spans under "What this builds on", the budgets below, and the three harnesses, each mutation-checked — `capture` unmemoized, the solved-metavariable zonk unmemoized, and a kernel with its memos off each fail their table.
2. **`capture`, `shift` and `release` keep what they do not touch.** A subterm that mentions none of `capture`'s binders and has nothing to shift is returned unchanged, decided from the node's cached scalars; `shift` and `release` remember what they rebuild per node and depth, as `capture` does, where the term is open. In `curios-core`, for both checkers, with `shift` and `release` joining the harness.
3. **Settlement.** The instruments say which cause dominates. A settled spelling is kept for as long as the frames it rests on stand, so only an invalidation that can have changed it clears it, and erasure reuses what elaboration settled or settles nothing it does not probe. The type-based filter is tried, and `could_reduce_to` moves to `curios-analysis`, one copy for both checkers.
4. **The kernel looks the scrutinee's type up.** `family_at_head` reads the scrutinee's type through `synth_neutral` rather than inferring it.
5. **The remaining walks.** The kernel's `infer` keeps a local-bearing term's type for as long as the equations in force stand, as its reduct tables do, and inference and checking join `curios-cert`'s harness; a completed-pair memo for the kernel's conversion, keyed so a hit cannot change a verdict; `walk_term` over a graph, its effects accounted per node; erasure surveyed; printing's shared nodes named. Printing and erasure need a design first, presented before either is built.

## Budgets

Taken on the tree stage 1 landed on, with `target/debug/curios` built by `cargo build --package curios --all-features`, a program's wall time the time `wonder diagnostics -` takes on standard input from a warm filesystem, the program holding `use /std/{Str, Option, Eq, Nat, Flt, List};` and the one claim, `Eq()(<left>, <right>) = Eq/refl()`, as the decomposition corpus spells it.

| Claim or build | Budget |
| --- | --- |
| `Str/split("a€€b€c", "€")` | 2.5 s |
| `Str/split_once("key=value=x", "=")` | 28.2 s |
| `Str/lines("a\r\nb\n")` | 2.2 s |
| `Str/replace("a€b€", "€", "-")` | 2.2 s |
| `Str/trim(" \té \n")` | 27.9 s |
| `Str/strip_suffix("héllo", "llo")` | 2.2 s |
| `Flt/of_str("-Infinity")`, from `curios/src/tests/corpus/flt.crs` | 1.9 s |
| `Option/map(Str/index_of("abcdefgh", 'h'), Str/At/to_offset)` against `Option/some(7)` | does not finish in 300 s |
| `/std` elaboration, `elaborate_and_zonk_with_prelude` | 100.0 s, peak 441.9 MiB |
| `/std` certification, `with_prelude` | 38.9 s, peak 140.2 MiB |

The two `/std` rows are read from the profiles `cargo x clippy` files, folded by `target/debug/curios profile curios-prelude-archive/.artifacts/profile.tsv` and `target/debug/curios profile curios-prelude/.artifacts/profile.tsv`, whose first data row is the total. A figure is taken once per stage on an otherwise idle machine; a claim past its budget by more than a fifth is a regression to explain before the stage lands.

## Verification

- Each walk made graph-aware joins its crate's harness, and the harness is mutation-checked: restoring a per-path walk fails its row.
- No verdict changes, except a claim that exhausted its budget and now completes, named as such.
- The benchmark set is re-taken after each stage and stays within its budgets; `index_of` in a type completes by the last stage.
- The prelude build is measured before and after each stage, naming the stage.

## Retirement

Record the graph-walk rule and each memo's key in the owning crates' documentation, the benchmark set beside the tests that hold it, and the settlement protocol in `reduce.rs`'s documentation. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
