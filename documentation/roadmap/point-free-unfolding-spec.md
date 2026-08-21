# A grammar written point-free is unfolded, not called

## Status

Deliberately unrefined. This specification records what the `/std/Parse` opacity attempt established for certain — every figure below was measured, and the refuted designs were refuted by running them, not by argument. The cure is not built and its shape is only narrowed, not chosen. Nothing is started.

## Why it exists

Rewriting `/std/Parse` and its consumers from recursive scanners over positions into combinator constants made compilation stop finishing: a single `Toml/decode` program still had not compiled after fifteen minutes, where the scanner spelling compiles in seconds. The two spellings denote the same grammar and differ only in whether a definition names its input.

The cause is not specific to parsing, which is what makes this a specification rather than a bug report. `curios-ersd`'s closed-term planner folds an application when its callee *and every argument* are closed (`optimize/evaluate/closed.rs`, the `is_closed_atom` guard in `plan`), and `is_closed_atom` answers `true` for every top-level definition. A grammar assembled from top-level combinator constants is therefore one closed expression: partial evaluation runs the whole parser tree at compile time and reification materializes it as code. A scanner written `rec f(input: Bytes, cur: Nat, ...)` is never a candidate, because `input` and `cur` are symbolic. `curios-prelude-archive/std/Toml/decode.crs` carries a comment recording exactly this, written as a deliberate mitigation; the rewrite deleted it.

So the surface rule a user would have to know is: *a top-level definition whose value is built by applying combinators to other top-level definitions is fully unfolded at every use, and adding a parameter that varies at runtime is what stops it.* That is a compiler-internal fact with no diagnostic, no error, and no way to derive it from the language. The failure mode is a silent superlinear compile.

## The minimal pair

The mechanism reproduces in a generated program with no parser, no monad, and no `/std/Parse`. Both programs below build the same tree with the same combinator body and compute the same answer; the only difference is whether `cur` appears in the argument list.

```crs
let Scan: Type = (cur: Nat) -> Nat;

-- A: every argument closed, so the planner folds it
let alt(p0: Scan, p1: Scan) -> Scan =
    (cur) =>
        let x0 = p0(cur);
        match x0: (_) => Nat | 0 => p1(cur) | _ => x0 end;
let a0: Scan = (cur) => (cur + 1) % 1000003;
let a1: Scan = alt(a0, a0);
let a2: Scan = alt(a1, a1);

-- B: `cur` is symbolic, so it is never a candidate
let alt_at(p0: Scan, p1: Scan, cur: Nat) -> Nat =
    let x0 = p0(cur);
    match x0: (_) => Nat | 0 => p1(cur) | _ => x0 end;
let b0(cur: Nat) -> Nat = (cur + 1) % 1000003;
let b1(cur: Nat) -> Nat = alt_at(b0, b0, cur);
let b2(cur: Nat) -> Nat = alt_at(b1, b1, cur);
```

Generate a family by varying the nesting depth, the width of each `alt`, and the number of separate definitions naming one shared sub-parser, then read `--print=ersd-optm` line and `^function ` counts. The program must consume a runtime-tainted input (`/std/read()!`, as `programs/parse_digits.crs` does) or the whole thing constant-folds away.

## Known for certain

- **Two independent axes multiply the cost, and they need different cures.** *Depth* is duplication within one fold — `alt(a, a)` reaches the same closure twice — and cost `2^depth` copies before the reification memo, exactly `2^d − 2` functions above baseline through depth 6 at width 2. *Fan-in* is duplication across folds — many definitions naming one shared sub-parser — and costs a flat `+5` functions and `+73` lines per referencing definition, perfectly linear to fan-in 16.

- **The reification memo (landed) fixes the depth axis completely and the fan-in axis not at all.** With it, style A drops to `+1` function per level, matching B's slope and beating B's absolute count. Fan-in is untouched by it, because the memo is scoped to one replacement plus item-level candidates only.

- **Module-wide sharing flattens fan-in on generated programs and is nearly worthless on `/std`.** Extending the memo across replacements takes fan-in from `+5 funcs / +73 lines` per referencing definition to `+0 / +3` — flat to fan-in 16, with style A then strictly cheaper than style B on both axes. On the real opaque `/std` the same change moved 144,300 lines / 8,818 functions to 140,503 / 8,616: **2.6%**. The compile still did not finish.

- **Sharing cannot fix this, and the ceiling is measured.** Instrumenting the opaque `Parse` compile: 3,846 copies, of which a *perfect* position-blind memo would avoid 1,025 (**27%**), leaving 2,821 genuinely distinct materializations. Only 132 reuses were blocked by the item-position guard, so the guard is not the constraint. The 462 identical copies of one function counted in the emitted module are identical only *after* later passes fold their differences away; at reification time their capture atoms differ, so no memo keyed on `(function, captures)` can match them. Post-optimization dedup would reach them; sharing during reification cannot.

- **The cost is concentrated in a few callees cloned many times at high weight.** Over 3,988 folds totalling 169,088 materialized weight, the median fold weighs **2** and p90 weighs 103; the top 200 folds hold 50% of all weight and the top 500 hold 76%. Per callee: `/std/Toml/strings/ml_literal_body` is 172 folds × 333 weight = **34% of all weight in the compile**; `/std/Toml/strings/trim_run` 19 × 717; `/std/Toml/values/inline_items` 8 × 930. Against those, `/syn/Monad/bind` is 630 folds at weight **1**, `/std/Result/Result/failure` 1,030 at weight 2, `/std/Parse/fail` 302 at weight 8.

- **A weight cap already exists and is set above the damage.** `MAX_REIFY_NODES` is 2,048; every damaging fold measured here is under it (333, 717, 930, max 1,302). The mechanism to refuse is present, its threshold simply never fires on this shape, and lowering it uniformly would also refuse the large single folds that pay.

- **Refuting a fold because it produces code is wrong.** Declining every fold whose result holds a closure ("fold to data, never to code") breaks eleven cross-stage tests and regresses the scanner `/std`'s fold from 401 to 3,605 lines. The tests it breaks are precisely the folds reification exists for: `Fmt` collapse, devirtualisation, fallback-shell removal, string-walk closure elimination.

- **Refuting a fold because its callee has many call sites is also wrong.** It would decline `/syn/Monad/bind` — 630 folds — which is monadic specialisation, worth 5.9× on `monad_io` and standing against a 300–1000× measured gap between monadic and manual carriers. Those 630 folds cost 630 units of 169,088. The frequent callees are the cheap ones.

- **A locally-measured growth test refuses everything.** Every reification replaces one statement with a materialized region, so all of them "grow" locally. The payoff of a good fold is a later `prune` removing the now-dead callee, which is invisible at reification time.

## The shape the measurements point at

Neither frequency nor size alone separates the folds that pay from the ones that do not; the product does. One 333-weight fold is a specialisation, and 172 of them is cloning. A **per-callee cumulative materialisation budget within a pass** — a given function may be specialised a few times, not 172 — leaves `bind` (630 × 1) and a single large `Fmt` collapse untouched while stopping `ml_literal_body` after a few copies, removing 34% of total weight by one rule. It bounds duplication of one callee, which is structural, and the distribution it discriminates is bimodal across three orders of magnitude, so the constant is not load-bearing in the way a tuned cap would be.

What such a gate does to the opaque `Parse` is leave the refused definitions as residual calls — which is exactly what the threaded spelling already compiles to. **That convergence is the point**: the cliff exists because two equivalent spellings compile differently, and the two spellings meeting removes it. Note what convergence costs: the opaque module then stays near the ~38k lines it enters partial evaluation with, rather than the 401 the scanner `/std` folds down to. The cliff disappears; the opaque spelling does not become fast.

## Deliberately not specified

The budget's shape and constant, and whether it is charged per callee, per callee per round, or against a decaying pool. Whether the residual cost is acceptable for `/std` or whether the scanner spelling remains the right way to write a grammar regardless. Whether a diagnostic should exist at all — `curios-ersd`'s `optimize` returns `()` and there is no warning channel from it through `curios-pipeline` to the CLI, so surfacing "this definition was expanded into N functions" is real plumbing, and a cure that removes the cliff may make it unnecessary. Post-optimization function dedup, which is the only instrument that reaches the 462 twins, and which is a different pass in a different crate. And the `/std/Parse` opacity rewrite itself: sealing `Parse`'s representation so nothing constructs it outside the module and every consumer goes through combinators, which is what provoked all of the above and was discarded unmerged.
