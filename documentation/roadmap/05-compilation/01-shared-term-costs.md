# A shared term costs its size

Working specification for making the compiler principled about what a stage may cost. [A reduction step costs what it builds](../../design/soundness/a-reduction-step-costs-what-it-builds.md) rests on one premise: "Reduction is the only stage a well-typed program can drive to arbitrary cost, because a type can call it; every other stage is bounded by what elaboration produced." What elaboration produces is a graph — a reduct names one node many times over, and a `let` the kernel substitutes puts one node at every use — and a stage that walks it once per path pays its tree, which doubles with every line that names the line before it twice. The goal is the guarantee the budget was written for: a compilation ends, accepted or refused for its budget, in time proportional to what was written plus what the budget counted. The rule that delivers it is that every stage but reduction costs the graph it is handed.

It needs [the rule that no memo outlives its declaration](../../design/soundness/a-reduction-step-costs-what-it-builds.md) and [the evaluation memo](../../design/soundness/conversion/the-evaluation-memo.md)'s argument, which every kernel table here extends.

## What this builds on

- **The graph-aware walks.** Term equality and hashing, `free_vars`, `capture` (memoized per node and depth), the reduction cache, level identification, the zonks (`Visit::rewriting_shared`, and `NodeMemo` for the strict one), `abstract_occurrences`, `metavar_origins`, `replace_term`, `mentions_term` and the scoped level rewrite. A memoized walk hands each occurrence its own span, and a rebuild that changes nothing hands back the node it was given (`Term::rebuilt`), so a capture over a subterm naming none of its binders keeps the node and its sharing. Each crate that owns walks runs them over one doubling term in `every_walk_answers_a_doubling_term_in_its_own_size` — `curios-core`'s in `term/sharing_tests.rs`, `curios-elab`'s and `curios-cert`'s in `walk_tests.rs` — each mutation-checked, and a walk private to its module keeps its fixture beside it.
- **The kernel's memos.** Term-keyed tables live one declaration; the reduct tables keep a local-bearing term for as long as the equations in force stand; the `infer` memo keeps local-free terms outside every arm and hands back a remembered type without replaying its mints, the freshness counter being monotone. Sort-hood is remembered per distinct type for one item (`Positions`), at the type a position is recorded at and no deeper.
- **Re-validation and rollbacks.** Inside an oracle bracket each memoizable node is elaborated once, whether or not the global cache keeps it; a rollback clears the reducts and elaborations only when it unwound a term solution, and the elaborations and the universe stamp when only the universe solver moved.
- **Settlement.** A term-keyed refinement's reduced spelling is settled at most once per entry between two clears and cached as `settled_keys` (`curios-elab/src/context/caches.rs`); `reduce::settle` computes it with the entry's own frame and every frame inside it withheld, so it rests only on the frames outside. The kernel keeps the same spelling in the equation's own entry (`Scope::settle_refinement`), where it lives exactly as long as the equation.
- **The hold.** The towers are generated programs in `curios/src/tests/towers.rs`, each value shape in a function's body and in a recursive member's; `a_sixty_line_chain_compiles` is the control's fixture, and `tower_measurements` prints each shape's verdict and each checker's units and looks at 12 and 16 lines. A look is counted where `Term` hands out its node (`curios_core::take_looks`, under `profile`) and sampled per declaration as `term::looks` beside `budget::consumed`, in both checkers.
- **The profile.** `curios-profile`'s spans, samples and notes, written through as each row is made, so a run that aborts leaves its open spans on disk. Kept for hunts: the spans `convert::outcome`, `convert::drain`, `typing::expect`, `typing::retry_parked`, `typing::drain_parked`, `ctx::park`, `ctx::wake_parked`, `reduce::settle` carrying the key and frame it settles, and the kernel's `Sort::of`, `convert` and `convert::unfolded_retry`; the samples `caches::rollback_dropped`, `caches::suppression_dropped` and, in both checkers, `budget::consumed` where a declaration's budget is restored.
- **Peers.** Lean 4's kernel keeps a table per judgment — inferred types, both weak-head forms, and plain sets of the pairs conversion accepted and refused, plain because "taking a transitive closure of successful pairs would make its result depend on evaluation order" ([`type_checker.h`](https://github.com/leanprover/lean4/blob/master/src/kernel/type_checker.h)) — and its `replace`, which substitution and lifting go through, remembers a shared node per binder depth unless a caller opts out ([`replace_fn.cpp`](https://github.com/leanprover/lean4/blob/master/src/kernel/replace_fn.cpp)). rustc's type walker visits each type once ([`walk.rs`](https://doc.rust-lang.org/nightly/nightly-rustc/src/rustc_type_ir/walk.rs.html)) and its suite keeps a thirty-line tower ([`issue-72408-nested-closures-exponential.rs`](https://rust.googlesource.com/rust/+/HEAD/tests/ui/closures/issue-72408-nested-closures-exponential.rs)); the shape was reported walker by walker ([#54540](https://github.com/rust-lang/rust/issues/54540), [#72412](https://github.com/rust-lang/rust/pull/72412), [#83031](https://github.com/rust-lang/rust/issues/83031), [#140004](https://github.com/rust-lang/rust/issues/140004)), and a walker that skips repeats stopped the type-length limit that counted them ([#125507](https://github.com/rust-lang/rust/pull/125507)). [Selsam, Hudon and de Moura](https://arxiv.org/abs/2003.01685) state the problem — "traversing a term requires time proportional to the tree size of the term as opposed to its graph size" — and their remedy, a stored hash, pointer-accelerated equality and a remembered traversal, is the representation `Term` already has. [Accattoli and Dal Lago](https://arxiv.org/abs/1601.01233) show a count of reduction steps is a reasonable cost model only over shared terms, and [Condoluci, Accattoli and Sacerdoti Coen](https://arxiv.org/abs/1907.06101) that equality of shared terms is linear. GHC's Core inlines types and pays their tree ([type lets](https://www.tweag.io/blog/2024-08-15-type-lets)).

## The gap

**A tower is refused or does not finish.** A tower of `n` lines is a first line and `n` lines after it, each naming the line before it twice: a graph of `n + 1` nodes whose tree has `2ⁿ`. Taken at `b900e392f`, verdicts and looks counted, "no answer" a run stopped after a minute:

| Each line is | 20 lines | 60 lines | Kernel looks at 12 and 16 lines | Elaborator looks at 12 and 16 lines |
| --- | --- | --- | --- | --- |
| `g(x)`, the chain every tower is read against | accepted | accepted | 142,884 and 143,808 | 54,185 and 55,573 |
| `g(x, x)` | refused by the kernel | | 583,848 and 7,219,464 | 55,326 and 57,114 |
| `(x, x)` | accepted | no answer | 681,575 and 8,791,327 | 220,301 and 2,681,841 |
| `[x, x]` | refused by the kernel | | 3,316,313 and 50,876,129 | 83,679 and 106,743 |
| `match b \| true => x \| false => x end` | accepted | refused by the kernel | 1,396,545 and 20,198,793 | 67,494 and 78,110 |
| `{T, T}`, a type alias | accepted | refused by the elaborator | 487,630 and 5,651,386 | 218,069 and 2,677,197 |
| `(T) -> T`, a type alias | accepted | refused by the elaborator | 365,976 and 3,687,316 | 218,468 and 2,677,732 |
| `(x, x)` twice, the two towers claimed equal | no answer | | 2,296,932 and 34,614,392 | 1,632,345 and 24,994,503 |

The value shapes are the body of one function — `let tower(g: (Nat) -> Nat, n: Nat) -> Nat` from `g(n)`, `let tower(g: (Nat, Nat) -> Nat, n: Nat) -> Nat` from `g(n, n)`, `let tower(n: Nat) -> Nat` from `(n, n)` and from `[n, n]` ending in `match xₙ | _ => n end`, `let tower(b: Bool, n: Nat) -> Nat` from `n` — each line a `let xᵢ = …;` and the last name the result; the type shapes are top-level aliases from `let T0 = Nat;` read by `let keep(x: Tₙ) -> Tₙ = x;`; the two towers are two top-level chains from `(1, 2)` under `let _same: Eq()(xₙ, yₙ) = Eq/refl();`, which answers at 16 lines. Every program imports what it names from `/std` and ends in `/std/print("ok\n")`. A verdict is the release compiler's, `curios run -` at the default budget; the looks — how many times a term handed out its node while a checker ran — are `tower_measurements`' (`curios/src/tests/towers/measurement_tests.rs`), which builds these programs and prints each checker's units beside them. Every refusal is for the budget — "the kernel's reduction budget ran out", "reduction ran out of steps" — on a program that reduces nothing: the kernel refuses three towers the elaborator accepts, which [two checkers given one budget must cost alike](../../design/soundness/a-reduction-step-costs-what-it-builds.md) forbids.

**Work over a term is one of four kinds, and one of them holds.**

| Kind | Rule | Today |
| --- | --- | --- |
| A read — free variables, a needle sought | A node is visited once | Holds in the harnesses' walks; the truth table and the second door's level alignment are [findings](00-findings.md) |
| A rebuild — a substitution, a shift, a zonk | A node is rebuilt once per depth and the graph is kept | `capture` and the zonks hold; `shift` and `release` do not |
| A judgment — a reduct, a sort, a type, a conversion, a settlement | An answer is remembered per node for as long as what it rests on stands | Reducts alone, in full |
| An emission — a print, a report | A node is printed once, or the print is bounded | The Core printer prints the tree |

**The sort of a type is computed per path, in three places.** The kernel's `Sort::of` (`curios-cert/src/kernel/sort.rs`) classifies each field of a Σ and each domain of a Π by calling itself, remembering nothing, and is asked for every distinct type `record_checked` records a position at and at every conversion's irrelevance test; a hit on a remembered reduct is free, so the walk spends nothing and the budget does not see it. The elaborator's `Sort::of_in` (`curios-elab/src/convert/sort.rs`) has the same shape and "runs on every conversion problem", and it is what `is_prop`, `sort_term`, totality's proof positions and erasure's `is_erasable_in` ask. Over `/std` at `b900e392f`, counted, the kernel's is 142,085 calls, 86,854 of them outermost.

**The kernel types a term per path where it names a local or stands under an arm.** The `infer` memo keeps a term with no local free and no loose index — a recursive call is an application of a local, and each call is recorded where it is typed — and stands aside while a case equation is in force. A `let` is substituted, so the lines of a function's body are one graph over its parameters and none of it is remembered.

**The kernel's conversion remembers no verdict.** `History` (`curios-cert/src/kernel/convert.rs`) holds the goals in progress and drops each as it is decided, so two terms that are equal graphs built apart are compared once per path. The elaborator's history keeps every problem a run has met.

**A settled spelling is cleared by what cannot have changed it.** Over `/std` at `b900e392f`, counted, `reduce::settle` runs 7,128 times, 1,741 of them inside another settlement, which the fold's total counts twice: 479 under erasure, and 6,649 in elaboration, of which 2,545 are the first of their declaration, frame and key and 4,104 repeat one — 1,304 after a suppression bracket, 166 after a rollback, 2,634 after neither. A spelling rests on the frames outside its entry and on what reduction reads globally, and every refinement registration, suppression boundary, frame exit, universe rewrite and rollback clears every spelling.

**Erasure's walk over a shared term was not surveyed.** It reads an elimination head's type once per item and keeps it.

**`shift` and `release` walk an open shared term per path, and hand back its tree.** They prune by `reach`, which answers a closed subterm at once, and remember nothing: over a doubling term with a loose index at its base each doubles its time per level and the result's operands are no longer one node. No tower above reaches them.

**The Core printer prints the tree.** `wonder stage core-elab` over the pairs tower prints 24,977 bytes at 8 lines and 393,741 at 12, each `let`'s type written out, and a report that shows a tower's type has the same print.

**In a recursive member's body the elaborator walks more towers per path.** `tower_measurements` puts each value shape in the successor arm of a function that calls itself: there the elaborator's looks at 12 and 16 lines are 173,738 and 1,896,474 for the calls and 360,404 and 4,673,508 for the arms, which outside a member it walks in their size. What does it is not established; the totality analysis's `walk_term` (`curios-analysis/src/totality.rs`) carries each arm's effects, so it is not a pure walk, and it runs over a member's body alone.

## Decisions settled

- **One table module per checker, each table stating what its answers rest on.** The kernel's `Memos` already gives a reduct two lives — the declaration for a local-free term outside every arm, the equations in force otherwise — and a sort, a type and a conversion's verdict are that question with those answers; a judgment that reads a local's type ends its second life where a local is re-typed as well. The elaborator's `Caches` holds its sort beside its reducts. A hit is free and nothing is charged at a recorded price, as the budget decision has it. Rejected: a memo argued per walk, which is how one shape is reported four times.
- **A judgment's hit replays what is per occurrence, and is refused where it cannot.** A remembered type still records its position and recalls a partial group. A term that names a member of a group whose body is being checked is typed at every occurrence, so every call is recorded and graded where it stands. Rejected: remembering a local-bearing type only outside every group body, which leaves a tower in a recursive function per path.
- **A settled spelling lives in its entry**, as the kernel's does: it dies with the entry, is cleared by a rollback, a universe rewrite and a redefinition, and is settled again where it holds an unsolved metavariable and a solution has landed since. Rejected: keeping `settled_keys` and arguing each clear, whose key is a frame's index and not the frame.
- **The kernel's conversion remembers a clean pair**: a verdict reached with no goal in progress assumed, kept in plain sets. Rejected: a goal that assumed only goals entered inside it, which needs the depth of every assumption argued in the trusted base, and a closure over accepted pairs, for Lean's reason.
- **The print is bounded.** A print visits at most a constant multiple of the term's distinct nodes, never fewer than 5,000, and past that a compound subterm prints as `…`. Rejected: naming a shared node with a `let`, which stays pasteable and moves every printed rung and report where a term shares a node; a flat bound, which elides a large declaration that shares nothing.
- **The hold is the towers and a counted sample.** Each tower is a fixture at 60 lines once its stage lands, and a per-declaration count of node looks, sampled beside `budget::consumed`, finds the next walk in `/std` on any machine. Rejected: a time bound in a test.
- **A node is not interned where it is built.** `Sharing` stays at the archive's boundary: a table keyed on `Term` already hashes and compares a shared graph in its size, and interning does nothing for what reduction builds ([smalltt](https://github.com/AndrasKovacs/smalltt): "hash consing alone is inadequate for eliminating size explosions"). **A walk is not charged to the budget**: a walk that costs its graph is bounded by the nodes the budget charged when they were built, and a charge would move verdicts.

## Stages

Each lands alone, with its fixture mutation-checked: with its table off, the row does not answer.

1. **The kernel remembers a type's sort**, under both lives, in `Memos`; `Positions`' own memo goes, whose one life spans every arm of an item whatever equations a type's sort rests on. `Sort::of` joins `curios-cert`'s harness.
2. **The kernel remembers a type under both lives**, for a term naming no member of an open group. Inference and checking join the harness. Unblocks the kernel's side of the call, list, arm and pair towers.
3. **The elaborator remembers a type's sort**, for as long as nothing is written and no reduct is cleared. `Sort::of_in` joins `curios-elab`'s harness. Unblocks the alias, arrow and pair towers and the elaborator's side of the two towers.
4. **The kernel's conversion remembers a clean pair.** Unblocks the two towers.
5. **A settled spelling lives in its entry.** The settlements over `/std` are counted before and after.
6. **Rebuilds keep sharing.** `shift` and `release` remember a shared node per depth, `capture` returns a node it has nothing to do to, and both join `curios-core`'s harness.
7. **The print is bounded**, its constant set from the largest ratio of tree to graph a declaration of `/std` prints at, taken first.

Each tower's fixture lands with the stage that unblocks it. A recursive-member tower no stage unblocks is designed before it is built.

## Budgets

Timed, at `8bf70ba8b`, with `target/debug/curios` built by `cargo build --package curios --all-features`, a program's wall time the time `wonder diagnostics -` takes on standard input from a warm filesystem, the program holding `use /std/{Str, Option, Eq, Nat, Flt, List};` and the one claim, `Eq()(<left>, <right>) = Eq/refl()`, as `curios/src/tests/corpus/strings/decomposition.crs` spells its cases. At `b900e392f` the eight claims read within a twentieth of their budgets, each the median of five.

| Claim or build | Budget |
| --- | --- |
| `Str/split("a€€b€c", "€")` | 2.7 s |
| `Str/split_once("key=value=x", "=")` | 2.5 s |
| `Str/lines("a\r\nb\n")` | 2.4 s |
| `Str/replace("a€b€", "€", "-")` | 2.2 s |
| `Str/trim(" \té \n")` | 2.4 s |
| `Str/strip_suffix("héllo", "llo")` | 2.2 s |
| `Flt/of_str("-Infinity")`, from `curios/src/tests/corpus/flt.crs` | 2.0 s |
| `Option/map(Str/index_of("abcdefgh", 'h'), Str/At/to_offset)` against `Option/some(7)` | 2.5 s |
| `/std` elaboration, `elaborate_and_zonk_with_prelude` | 85.9 s, peak 366.1 MiB |
| `/std` certification, `with_prelude` | 31.4 s, peak 111.2 MiB |

The two `/std` rows are read from the profiles `cargo xtask clippy` files, folded by `target/debug/curios profile curios-prelude-archive/.artifacts/profile.tsv` and `target/debug/curios profile curios-prelude/.artifacts/profile.tsv`, whose first data row is the total. A figure is taken once per stage on an otherwise idle machine; a claim past its budget by more than a fifth is a regression to explain before the stage lands.

## Verification

- Each tower compiles at 60 lines in the time the chain does, and `tower_measurements` reads each shape's units and looks growing with its lines.
- Each walk made graph-aware joins its crate's harness, and the harness is mutation-checked: restoring a per-path walk fails its row.
- No verdict changes over `/std` and the corpus, and `kernel_memo_parity` agrees with the memos off.
- The benchmark set is re-taken after each stage and stays within its budgets, and the prelude build is measured before and after each stage, naming the stage.

## Retirement

Every tower is a fixture that passes. Record the rule that a stage costs its graph, with what was rejected, as a decision under `documentation/design/compilation/`; each table's key and what it rests on in the owning module's documentation and, for the kernel's, in [the evaluation memo](../../design/soundness/conversion/the-evaluation-memo.md); the benchmark set beside the tests that hold it; and the settlement protocol in `reduce.rs`'s documentation. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
