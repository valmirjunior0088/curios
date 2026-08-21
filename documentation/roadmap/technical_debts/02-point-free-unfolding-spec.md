# A reified closure is bound once, not copied per use

## Status

Deliberately unrefined. The cure is narrowed to one mechanism and is not built. Nothing is started.

**Which figures are retaken and which are inherited.** The trigger table, the growth laws, the before-and-after on the landed memo, and every count and wall clock in [The trigger, isolated](#the-trigger-isolated) and [What was refuted](#what-was-refuted) are reproduced by `curios`' `combinator_sharing_measurements`, which carries the command, the date, the host and what it last printed; the profiler attributions and the pre-memo worktree method come from the commands that probe's documentation names. Four figures under [Known for certain](#known-for-certain) are marked *inherited* — carried from the predecessor and **not** retaken. They are kept because they refute cures rather than propose one, so a stale figure there costs a rejected alternative rather than a design; but nothing should be sized against them without retaking them — and the per-callee census under [What was refuted](#what-was-refuted) explicitly must not be, because the fix changes the distribution it measures.

This document replaces an earlier one that located the same symptom in the same crate and got the cause wrong. What that one claimed and what re-measuring found is recorded under [What was refuted](#what-was-refuted) rather than deleted, because each refuted claim was a stated reason not to attempt the cure proposed here.

## Why it exists

Rewriting `/std/Parse` and its consumers from recursive scanners over positions into combinator constants made compilation stop finishing: a single `Toml/decode` program still had not compiled after fifteen minutes, where the scanner spelling compiles in seconds. The two spellings denote the same grammar.

The cure has to be a compiler change rather than a style rule, because the rule a user would otherwise have to know is not one anybody could derive from the language. Below is the whole of it, and it is a rule about *where a call is written*, not about what it computes.

## The trigger, isolated

A grammar of `n` rules in the `/std/Json/decode` idiom — each rule a top-level `Parse` definition built from a `!` chain over the two before it — with one thing varied: whether the rule's inner combinator applications (`Parse/many0(prev)`, `Parse/sep_by0(prev, eq)`) are written where they are used, or named as items first. Same grammar, same combinators, same parsers.

| spelling at 16 rules | `Parse/bind` copies | emitted functions | compile | growth of copies |
| --- | --- | --- | --- | --- |
| no application inside a continuation | 18 | 166 | 0.52 s | `n + 2` |
| the applications hoisted to items — *the cure, emulated in source* | 18 | 262 | 1.19 s | `n + 2` |
| every rule eta-expanded, applications left in place | 18 | 312 | 4.39 s | `n + 2` |
| **as written** — applications inside the continuation | **258** | 566 | **23.21 s** | **`n² + 2`** |

The quadratic is exact over `n` ∈ {2, 4, 8, 12, 16}: 6, 18, 66, 146, 258. The other three are exactly `n + 2` — hoisting the applications costs precisely what never writing them inside a continuation costs.

So the rule is: *a combinator application written inside a closure body is reified without sharing, and every definition that reaches it re-materializes the whole chain below it. Writing the same application as a top-level definition makes it linear.* A `!` continuation is a closure body; so is a match arm, and so is a lambda.

## Where it comes from

`curios-ersd`'s closed-term planner folds an application when its callee and every argument are closed (`optimize/evaluate/closed.rs`), and `is_closed_atom` answers `true` for every `Let`-bound value module-wide. Reification then materializes the folded result, deep-copying each closure's region under the substitution its captures resolve to.

The memo that shares those copies has two halves, and only one of them is scoped by splice position. `ReifyScope::local` is cleared per replacement and always applies; `ReifyScope::shared` is written by `record` and read by `reusable` only when the candidate carries a position, and `apply` gives a position to `Owner::Items` alone (`optimize/evaluate/closed.rs`, `optimize/evaluate/reify.rs`). So a candidate inside a block still shares within itself — which is what fixes duplication *within* one fold — and contributes nothing to, and takes nothing from, any other replacement. That second half is the whole of the quadratic.

The reason is scope rather than soundness of sharing as such: the copy is spliced immediately before its own candidate, inside a block that need not dominate anything else, so an atom carried out of it would name a function bound where the next candidate cannot see it.

The scoping is a real constraint and the current answer to it is the conservative one. What the measurements say is that the conservative answer costs a factor of fourteen on the idiom the standard library is written in: `/std/Json/decode.crs` spells `Parse/sep_by0(decode, symbol(0x2C))` inside a `!` continuation, and that is the shape.

## Known for certain

- **A block-owned candidate costs fourteen times the copies of the identical program with the same applications at item level**, and twenty times the wall clock — 258 against 18 copies, 23.21 s against 1.19 s, at sixteen rules. Under `--features profile` the difference is entirely `curios_cont::optimize`: 21 998 ms against 602 ms, and 223.8 M allocations against 6.5 M.

- **The landed memo carries the ordinary case, and it is what made position matter at all.** Taken against a worktree at the commit before it, on the spelling with *no* application inside a continuation, `Parse/bind` copies at sixteen rules went from 138 to 18 — a quadratic series to `n + 2` — with the module from 302 emitted functions to 166 and the compile from 3.23 s to 0.52 s. On the pathological spelling it buys 378 → 258, which is real and modest. Two consequences the predecessor's framing misses. **Before the memo, where the application was written made no difference whatever**: hoisted and in-a-continuation measured identically at every size, so the asymmetry this document is about is one the memo *created* by reaching item-level candidates and not block-level ones — the cure widens its reach rather than replacing it, and without it there would be nothing to widen. And the eta spelling is untouched by the memo to the copy, which is the check that eta declines the folds rather than sharing them. `combinator_sharing_measurements` carries the whole table and the worktree method that retakes it.

- **The reification pass itself is not the cost and never was.** `evaluate_closed_terms` is **19 ms of a 22 193 ms compile** on the worst case measured here, and 13 ms of a real `Toml/decode` compile. It runs four rounds, not the eight the driver allows. What it *produces* is the cost, because [the fixpoint below it](03-cont-fixpoint-cost-spec.md) is super-quadratic in module size — 97.6% of an ordinary `Toml/decode` compile today, with no point-free code in it anywhere. The cliff is the product of the two, and neither alone explains it.

- **Refusing a fold because it produces code is wrong** *(inherited, not retaken)*. Declining every fold whose result holds a closure ("fold to data, never to code") breaks eleven cross-stage tests and regresses the scanner `/std`'s fold from 401 to 3 605 lines. The tests it breaks are precisely the folds reification exists for: `Fmt` collapse, devirtualisation, fallback-shell removal, string-walk closure elimination.

- **Refusing a fold because its callee has many call sites is also wrong** *(inherited, not retaken)*. It would decline `/syn/Monad/bind` — 630 folds — which is monadic specialisation, worth 5.9× on `monad_io` and standing against a 300–1000× measured gap between monadic and manual carriers. Those 630 folds cost 630 units of 169 088. The frequent callees are the cheap ones.

- **A locally-measured growth test refuses everything** *(inherited; an argument rather than a figure)*. Every reification replaces one statement with a materialized region, so all of them "grow" locally. The payoff of a good fold is a later `prune` removing the now-dead callee, which is invisible at reification time.

- **A weight cap already exists and is set above the damage.** `MAX_REIFY_NODES` is 2 048 — a code fact — and every fold in the predecessor's census is under it *(that census is inherited and not retaken)*. The mechanism to refuse is present and its threshold never fires on this shape.

## What was refuted

Three claims in the predecessor were measured again and did not survive. They are kept because each was the stated reason not to attempt the cure this document proposes.

- **"Sharing cannot fix this, and the ceiling is measured — a perfect position-blind memo would avoid 27%."** Refuted. Making the same applications item-level removes 93% of the copies and 95% of the compile time. The 27% ceiling was computed from a counter at the position guard, which can only observe keys that were *recorded* — and a block-owned candidate never records, so the dominant case was invisible to the instrument by construction. The same reading explains the companion claim that "only 132 reuses were blocked by the item-position guard, so the guard is not the constraint": the guard is the constraint, and 132 is what a counter placed after it can see.

- **"The reification memo fixes the depth axis completely and the fan-in axis not at all."** Both halves understate it, in opposite directions. The memo's *larger* contribution is not the within-fold sharing the predecessor credits but the cross-replacement sharing it dismisses, which is what took ordinary item-level code from quadratic to linear — see the counterfactual above. And what it leaves is not "the fan-in axis" but one ownership class. With the memo landed, fan-in at item level costs `+0 functions / +3 lines` per referencing definition; at block level it costs `+5 / +65`. Those are the predecessor's own two figures for "with module-wide sharing" and "without", and both are the code as it stands — the difference between them is not a change anybody has to make, it is where the definition was written.

- **"The cliff disappears; the opaque spelling does not become fast."** Refuted, and inverted. The predecessor's cure was a refusal policy, and refusal is the *worse* of the two available cures. Eta-expanding every rule declines the folds and costs 312 functions and 4.39 s; hoisting the applications performs the folds and shares the result, and costs 262 functions and **1.19 s**. Sharing done properly is both smaller and faster than refusing — for the same grammar, at the same size, in the same table above.

Two further claims are not refuted but rest on a distribution the fix changes. The predecessor's per-callee census — `/std/Toml/strings/ml_literal_body` at 172 folds × 333 weight, 34% of all materialized weight — was taken with block-owned sharing absent. How much of those 172 is duplication a memo would remove is not known, so the census has to be retaken before it can size anything.

## The shape the measurements point at

**Let-insertion.** The residual code a fold produces should be bound once, at a point every consumer can see, rather than copied into each candidate's own block. That is the mechanism partial evaluators for call-by-value languages have used since Similix, where it exists for exactly this reason — to prevent duplication of residual computations — and which MetaOCaml spells `genlet`. The domain-specific version is the same idea: Parsley finds let-bound parsers by observable sharing and compiles each to a `Call`/`Ret` pair rather than inlining it, because inlining a staged combinator grammar is otherwise exponential in the number of conditionals.

Concretely, and this is the whole of the proposal: **every statement in a replacement's spliced group is closed by construction** — built only from interned constants, item-bound functions (which `outward_ok` already enforces), and earlier statements of the same group — so the group can be spliced at item level, ahead of the item enclosing the candidate, instead of ahead of the candidate. `Module::verify` binds an item's names ambiently for everything after it, so the result is in scope at the candidate and reusable by every later one. Every candidate then has a position, and the memo that exists today does the rest.

The hoisted row of the table above is that cure emulated in source, so it is a ceiling rather than a verification. Three obligations it does not model, and which are where a fourteen-fold could become a two-fold:

- The block-to-enclosing-item map has to be built. `outward_functions_item_bound` already walks regions and is the place to take it from.
- Plans are visited in `module.statements()` arena order while positions are `module.items()` order (`optimize/evaluate/closed.rs`). Where those two disagree, `reusable`'s `position >= defined` test refuses a reuse that is in scope. Whether they can disagree in a real module is not established.
- Hoisting changes what `prune` sees. A group whose candidate later dies must still be collectable.
- **Nothing tests the memo.** There is no test module under `curios-ersd/src/optimize/evaluate/`, and its correctness rests on the scope argument in its own documentation plus `Module::verify` catching a mistake at the end of the pass. Widening its reach is the moment to pin that argument rather than re-argue it.
- **The dry run gains a second dependent.** `apply`'s probe is what guarantees `reify` cannot fail after it passed; if it ever could, the partial statements are orphaned *and* `ReifyScope::shared` holds atoms naming them, so a later replacement could reuse a function whose binding was never spliced. The failure is a loud `verify` panic rather than a silent miscompile, and the orphan predates the memo — but the memo is why that invariant now has two dependents, and let-insertion touches exactly this code.

**A per-callee budget is the backstop, not the cure, and its constant must be derived after sharing is fixed.** Bounding how many times one callee may be specialised within a pass is not a new idea — it is GHC's `-fspec-constr-count`, which ships at **3**, beside `-fspec-constr-threshold=2000`. Chez Scheme's `cp0` pairs a size counter with an *effort* counter and residualises the call when either trips, which is the half Curios does not have. What none of them do is decide by size alone, and the predecessor's own measurement says why: frequency and size each fail to separate the folds that pay, and only the product discriminates.

**The benefit signal exists and is not consulted.** The predecessor states that a fold's payoff is invisible at reification time. It is not: the interpreter counted the steps the fold replaced, in `Budget`, and throws the number away. Flambda decides at the call site by weighing code growth against the operations the inlining removes; the same weighing is available here for the cost of returning a counter.

## Prerequisites

**`/std/Parse` needs a `delay`.** `Json/decode.crs` ties its recursive grammar's knot with `Parse { run(input, pos) = … }`, and `Toml/decode.crs` records the same technique as a deliberate mitigation. Sealing the representation removes the only way to write that, and a recursive grammar written point-free instead is not merely slow — it reaches an `assert!`:

```
thread 'main' panicked at curios-ersd/src/into_cont.rs:360:13:
unsupported eager self-recursive value: its initializer evaluates the member it defines
```

A `delay(f: ({}) -> Parse(A)) -> Parse(A)`, definable inside the sealed module because a private representation is transparent throughout its own subtree, compiles the same grammar in 0.30 s — a one-off reading on the program above, same date and host, not covered by any probe. So the refactor needs one before it needs anything here. Eta-expansion is also the classical binding-time control — *Eta-expansion does The Trick* — and GHC reaches the same place from the other side by designating a loop breaker it will never unfold. What the table says is that it should be reached for because a grammar recurses, not because a grammar is large: eta-expanding every rule costs 312 functions and 4.39 s where hoisting the same grammar's applications costs 262 and 1.19 s.

**The panic is its own defect.** A self-referential value whose initializer forces itself is a real error, and the right answer is a diagnostic naming the cause, not an assertion. It is reachable from ordinary source and documented nowhere.

## Deliberately not specified

Whether the residual cost is acceptable for `/std` or whether the scanner spelling remains the right way to write a grammar regardless. The budget's shape and constant, which cannot be sized until the census is retaken. Whether a diagnostic should exist at all — `curios-ersd`'s `optimize` returns `()` and there is no warning channel from it through `curios-pipeline` to the CLI, so surfacing "this definition was expanded into N functions" is real plumbing, and a cure that removes the cliff may make it unnecessary. Post-optimization function dedup, which is a different pass in a different crate. And the `/std/Parse` opacity rewrite itself, which provoked all of the above and was discarded unmerged.

## Out of scope

[The fixpoint costs more than everything it optimizes](03-cont-fixpoint-cost-spec.md) owns `curios_cont::optimize`'s super-quadratic cost, which is the amplifier here and is already the dominant cost of compiles that have nothing to do with this. [A case refinement is keyed at the cheap spelling first](01-kernel-scrutinee-key-spec.md) owns a second, unrelated cliff on combinator-shaped code, in the kernel.
