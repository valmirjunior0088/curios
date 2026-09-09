# A combinator specialized on a known function argument stops calling through it

## Status

Refined, and not started. The measurement this file used to ask for has been taken, and it settles more than it was asked to: the population is one function, the specializer route is closed by a structural fact rather than a missing pass, and the inlining route reaches the goal while costing more than it buys. What is open is a way past the obstacle in the last section, and there may not be one worth its price.

## Why it exists

A higher-order combinator is compiled once, with its function parameter as an ordinary runtime value, so a call site that knows the function still reaches it through a closure. `curios`' `tests::codegen::ladder` counts what an idiomatic UTF-8 walk costs per character in the emitted `/std/Str/fold` and lists the indirect call through `f` as one of its per-character sites.

What specialization buys is not the one dispatch. It is that a substituted callee is visible to every pass downstream — inlining, folding, contification, the four passes that refuse an escaping function because its signature is frozen. That claim survives the measurement; what does not survive is the assumption that making the callee visible is free.

## What the measurement found

Taken over `programs/*.crs` through `wonder stage cont-optm` and `wonder stage wasm`.

- **The population is one function.** `/std/Str/fold` is the only combinator in the corpus that both dispatches through a function parameter and is too large to inline: extent **53** by `Census::extent` and by `copied_extent` alike, at two call sites in most programs and three in `parse_multibyte`. `/std/Async/bind_raw` receives function arguments at six sites but is 2 nodes and already inlines. Every other indirect call belongs to an `io/bind` returned as a value, which is the landed monadic-step item.
- **A capturing lambda is still a `CpsAtom::Fun`.** This file used to say a closure that captured anything is a `CpsAtom::Value`, and defer capturing lambdas to another item. That is wrong at the level of `curios-cont`'s defining property: it is *pre-closure* CPS, conversion is delayed to `into_wasm`, and `curios-ersd`'s emitter maps `Atom::Function` to `CpsAtom::Fun` without consulting captures. `CpsAtom::Fun` is a syntactic reference to a named function; `CpsAtom::Value` is a function-typed value of unknown identity — a parameter, a continuation parameter, or a runtime selection. **39 of the 44 function arguments passed as `Fun` across the corpus capture something**, so capturing lambdas are the population rather than an extension of it.
- **The extent figure recorded beside `step_specialization_extent` is stale.** It reads `fold 63` from 2026-08-17; the instrument now reports 53. `step` and `classify` are unchanged at 26 and 52.

## Why the specializer cannot reach it

Three gates, and the third is the one that matters.

1. `eligible_sccs` requires a recursive SCC. `/std/Str/fold`'s loop is contified into a continuation, so it is a non-recursive singleton and never enters the analysis.
2. Widening that gate does not help: the callers pass *different* `Fun` atoms, so `invariant_fixpoint` joins to `Conflict`.
3. **Nor does cloning per context, and this is structural rather than a missing placer.** `/std/Str/trim_bounds/1` captures a parameter of a continuation bound inside `Str/trim_bounds`, so it is usable only within that continuation's region. A clone of `Str/fold` specialized on it must name that function, so its only legal home is inside that region — and the only sensible place there is the call site. **A specialized copy's only legal home is the call site, which is inlining.** Separately, `copy_bodies` emits no binding at all and all three existing placers append a clone to the *original's* `LetFun`, where the reference is not nameable.

## The inlining route, and why it is not the answer either

`inline_call` already devirtualizes: it binds parameters to argument atoms and rewrites a `CpsCallee::Closure` on a substituted parameter into `CpsCallee::Known`. Inlining is also the crate's only copy that legally lands in a different lexical scope, because a site naming `Fun(f)` has already proved `f` in scope at that node. So the change is a profitability gate, not a pass: price a copy that devirtualizes against what it removes rather than against its size.

That was built and measured. It works, and it is a net loss.

- **It reaches the goal.** `/std/Str/fold` disappears as a function in all 18 programs, indirect calls fall 26 to 24 wherever it is used and 92 to 90 in `monad_async`, the two programs that never call it are byte-identical, module growth is +4.7% to +11%, and every program converges.
- **It costs four rope slices per character.** In-loop `call $bytes/slice`, counted by the ladder's own algorithm, goes from **0 to 4** in `parse_digits`, `parse_multibyte` and `walk_mirror_baseline`. That is the suffix view a previous campaign removed, re-introduced. Trading one indirect call per character for four rope slices is worse than doing nothing.
- **The cause is `split_windows`, and it is not containable by location.** Inlining the fold into `Nat/of_str`, `of_str` into the caller's counting loop, and that loop into the entry puts the walk inside a nested loop where the window region is no longer the one chosen; `split_windows` picks the widest admissible region on the converged graph and cannot revisit, and its own comment names `walk_mirror_held_scan` as a prior instance of exactly this. Blocking every inline into the entry was tried: one more function survives, the node count moves, and the four slices stay.

The two optimizations are in genuine tension rather than accidentally colliding. Relocating a combinator's body into its caller is what puts its walk in the caller's loop, and the route that would not relocate it — specialization — is closed by the section above.

## What a future attempt would need

Not a fourth specializer, and not a wider inline budget. One of:

- **A revisitable window split.** The obstacle is that `split_windows` is irrevocable and decides on a graph that inlining then changes. A split that could be reconsidered, or a region choice that does not depend on when the fixpoint settled, would remove the tension for this and for anything else that moves a body.
- **Devirtualization without relocation.** A rewrite that turns the closure call into a known call *in place*, without copying the body to the site. The scope obstacle above is what makes this hard: the reference is nameable at the site and not at the callee.

## What the implementation would look like, if the obstacle falls

Recorded so it need not be re-derived; it was built and measured, then discarded.

- **The profitability predicate is `Demand`'s, not a syntactic walk.** `Demand::Applied(n)` is exactly "the callee applies this parameter", carries the arity so a parameter reached at two arities is already `Opaque`, and distinguishes a parameter that is applied from one that is applied *and also* stored — which matters, because the stored use reproduces the substituted reference, the callee keeps escaping, and the copy buys one dispatch instead of a signature. Hoisting `demands` beside `analyze_calls` inverts the staleness argument written above `inline_known_calls`: a stale fact there tightens the budget, and a stale `Applied` loosens it.
- **Two arity checks belong in the admission, before minting.** A `Known` call must hand its callee exactly its parameters, and its return continuation must accept the callee's return arity where a closure's accepts one. Pass ordering keeps both true today and nothing states it; `map_callee` cannot decline after the copy has minted.
- **Price the duplication, not the callee** — extent times sites — and read it as an *alternative* to the size rule rather than a replacement, so widening cannot refuse a call the module already inlines.

## Deliberately not specified

The pass's position in the fixpoint. Whether `split_windows` should become revisitable, which is its own subject and larger than this one. Whether any of this changes what the ladder measures, which is the instrument's own to report.
