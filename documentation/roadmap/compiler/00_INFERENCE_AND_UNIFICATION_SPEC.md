# Inference and unification

Working implementation specification for the remaining inference and unification work in `curios-elab`: the metavariable solver, the postponement scheduler, and the diagnostics that report what they could not decide. Durable user-facing semantics belong in `SYNTAX.md`, elaborator invariants in `curios-elab` module documentation and tests, and cross-cutting rationale in `DESIGN.md`.

This document was rebuilt on 2026-08-08 around a demand measurement of the `/std` and `/syn` corpus rather than inherited item lists. Two items survive from the prior version (residual-constraint diagnostics, pruning), two were added because the measurement showed them to be the largest actual blockers (metavariable-blocked conversion postponement, packed-literal unification views), one was dropped for having no demonstrated consumer (η-equating metavariable heads — see Non-goals), and monomorphic lambda inference moved to [`06_ANONYMOUS_MATCH_FUNCTION_SPEC.md`](06_ANONYMOUS_MATCH_FUNCTION_SPEC.md), reframed as that form's inference-position machinery.

## Covered roadmap items

- Surfacing residual unification constraints (distinguishing postponed from rigid-mismatch diagnostics)
- Metavariable-blocked conversions postpone instead of mismatching
- Pruning of out-of-scope metavariables
- Packed-literal views in unification decomposition

## The corpus measurement

Every explicitly supplied implicit argument in `curios-prelude/std/` and `curios-prelude/syn/` was cataloged and re-tested with the implicit omitted, by replicating each site's exact typing context — callee signature, argument order, live refinements, and expected-type flow — in standalone programs compiled at 2026-08-08 HEAD. `/syn` has none; `/std` has 24 call sites carrying 50 explicit implicit arguments. The corpus is a demand floor, not a ceiling: it shows only where authors pushed through with `@` rather than designing around a gap (`Async/Future.crs:13`'s comment documents one designed-around case).

| Sites | Args | Blocked by | Unlocked by |
| --- | --- | --- | --- |
| `Char.crs:11`, `:16`, `:72` | 3 | eager mismatch checking `True/qed()` against `Below(?c, …)`/`InRange(?c, …)` stuck on the unsolved index | Item 2 |
| `Str.crs:68`, `:78`, `:84`, `:144`, `:152`, `:159` | 12 | same shape: `Nat/Lt(0, Bytes/len(?b))` checked before the witness argument that would solve `?b` | Item 2 |
| `Str/utf8.crs:216` | 2 | same | Item 2 |
| `BigNat/add.crs:1270`, `:1286` | 4 | comparison of two matches stuck on the same scrutinee with a metavariable in an arm position | Item 2, pending its investigation bullet |
| `BigNat/succ.crs:56`, `:61`, `:94`, `:100` | 16 | packed constant folding versus cons/append/concat decomposition | Item 4 |
| `Async.crs:399`, `Async/Future.crs:15` | 2 | embedded-metavariable postponement chains that never commit | Item 3 |
| `Async.crs:615` | 1 | list-literal elaboration refuses a ground solution eagerly | defect (a) below, not an item |
| `Str/utf8.crs:180` | 1 | refinement-suppressed solving postpones unrecoverably | none; Item 1 makes its report honest |
| `Async.crs:454`; `Str/utf8.crs:9`, `:205`, `:211-213` | 9 | nothing — already inferable | droppable now |

The corpus sites double as acceptance tests: each item's retirement includes dropping the explicit arguments it unlocks, so the prelude stays calibrated to the solver instead of drifting stale again — the condition the measurement found it in, with nine arguments of accumulated slack.

## Status ledger

Verified against the tree on 2026-08-08. Every citation is a file and line to re-read before starting, not a frozen API.

| Item | Landed | Remaining |
| --- | --- | --- |
| Residual constraints | The checking-shaped half: a parked `Checking` obligation surviving every retry reports `Error::postponed_check` (`typing.rs:338`, `error.rs:459`) at the expression's own span, naming the expected type it waited on | The conversion-shaped residue. A parked `Conversion` goal surviving the drain reports as a plain mismatch at its origin (`typing.rs:328`), distinguished only when it stands between witness holes |
| Conversion postponement | Nothing. A conversion whose verdict hinges on a term stuck on an unsolved metavariable — `True ≡ Below(?c, 0xD800)` reducing to a match stuck on `?c` — returns `Mismatch` eagerly, aborting elaboration before later arguments or the result unification could solve the blocker | The item |
| Pruning | The degenerate forms only: the embedded-metavariable guard postpones any solve whose candidate mentions an unsolved metavariable (`convert.rs:1446`, "the stand-in for pruning"), and spine inversion refuses dependence on non-pattern slots (`convert.rs:1504`, "pruning in its simplest form") | Real pruning |
| Packed views | Nothing. `b[h, ..t]` lowers to `Bits.concat(Bits.append(b[], h), t)`, and the reducer folds constant spines into literals — `append(b[], true)` becomes `b[\1]` — so unification meets `append(b[], ?h) ≡ b[\0]` with no rule to answer it | The item |

## Item 1 — surfacing residual unification constraints

Half landed, and the landed half is the template. A `ParkedWork::Checking` obligation that survives the final drain reports at its own span, naming the expected type it was waiting on.

The remaining half is `ParkedWork::Conversion`. A parked conversion goal that never wakes surfaces as an ordinary type mismatch at its origin, which is honest but misleading: a rigid mismatch means the program is wrong; a postponed conversion means the program may be right and inference never gained the structure to decide. Every non-eager corpus failure in the measurement surfaced exactly this way — `inferred: ?2459` against a fully concrete expected type — so the item now has direct evidence, not just principle.

The diagnostic should distinguish the two, name the metavariables the goal was watching, and anchor at the origin term the `ParkedGoal` already carries (`context/solutions.rs:51`). The existing witness-hole special case (`typing.rs:313`) is a third state and remains reachable rather than being folded into either.

Two additions to the original scope:

- A solve postponed by `solve_refinement_free` (`convert.rs:1842`) — the guard that refuses to commit solutions derived under counterfactual match-arm refinements — should also report as a postponement, naming the refinement dependence. `Str/utf8.crs:180` is the corpus reproduction; the guard itself is sound and stays.
- The wake-cascade investigation from the defect ledger belongs to this item: residual reporting is only trustworthy once it is known whether surviving goals genuinely lacked solutions or were stranded by wake bookkeeping.

This item remains the prerequisite for everything below in practice: Items 2 and 3 both add ways for work to park, and both are much harder to evaluate against a diagnostic that cannot say whether the solver stalled or the program disagreed.

## Item 2 — metavariable-blocked conversions postpone instead of mismatching

The largest measured item: 21 of the corpus's 50 explicit arguments across 12 sites exist only because a conversion whose verdict depends on an unsolved metavariable fails hard instead of parking.

The rule: when either side's weak-head normal form is stuck on a term containing an unsolved metavariable — a match whose scrutinee mentions one, an intrinsic application over one — the goal is undecided, not unequal. It parks as `ParkedWork::Conversion` watching the blocking metavariables, exactly as flex-headed goals already do. A stuck comparison whose blocking positions contain no metavariable stays a hard mismatch: the watch set would be empty, nothing could ever wake it, and failing fast preserves early, well-located errors for genuinely wrong programs.

Reproduction — the minimal pair, distilled from the `peel_byte` call sites. `after` compiles today; `before` fails at `True/qed()` with `expected: Lt(0, Bytes/len(?b))` because the proof argument is checked before the witness argument that solves `?b`:

```crs
use /syn/Str/{Scan, Utf8};
use /std/{Nat, Byte, Bytes, True};

let witness_first(@b: Bytes, w: Utf8(Scan/lead(), b), nz: Nat/Lt(0, Bytes/len(b))) -> {} = ();
let proof_first(@b: Bytes, nz: Nat/Lt(0, Bytes/len(b)), w: Utf8(Scan/lead(), b)) -> {} = ();

let after(h: Byte, t: Bytes, valid: Utf8(Scan/lead(), x[h, ..t])) -> {} = witness_first(valid, True/qed());
let before(h: Byte, t: Bytes, valid: Utf8(Scan/lead(), x[h, ..t])) -> {} = proof_first(True/qed(), valid);
```

The same failure blocks the `Char.crs` sites even at a closed index — `Char { code = 0x30, scalar = Scalar/below(True/qed()) }` fails because the proof is checked before the constructor's result-index unification would solve `?code` — which is why the alternative fix of unifying constructor target indices before payload arguments was considered and rejected: it repairs only the constructor sites, while the postponement rule repairs those, the argument-order sites, and every future shape in the class with one verdict change.

Constraints:

- Committing nothing, the rule cannot be unsound; the risks are diagnostic quality (met by Item 1 landing first) and lost eagerness (met by the empty-watch-set fast-fail).
- The parked goal must watch the blockers transitively — the metavariables in stuck positions, not merely a flex head — or the wake never comes.
- Retry under the frozen frame must reproduce the refinements live at park time, since the woken goal may only discharge under them (the `Char.crs` sites reduce `Below(code, 0xD800)` to `True` only inside the `code < 0xD800` arm).

Investigation bullet, gating the `BigNat/add.crs` pair: `add.crs:1239` omits the same implicits that `add.crs:1270` must spell, and the probe shows the difference is a comparison of two matches stuck on the same scrutinee with a metavariable in an arm slot (`xor3(xh, ?b1, c)` against its reduced concrete form). Some match-match conversion already exists — `step(c, Scan/lead()) ≡ scan_of_class(classify(c))` converts as stuck matches today — so the failure needs pinpointing (definition-unfolding staging versus missing congruence with flexible arms) before deciding whether this item's rule or an arm-wise congruence extension covers it.

## Item 3 — pruning of out-of-scope metavariables

When solving `?m[σ] ≡ t`, a candidate `t` mentioning another unsolved metavariable is refused wholesale by the embedded-metavariable guard (`convert.rs:1446`), because that metavariable may carry a wider context than `?m`'s. Postponement is correct and incomplete: the goal wakes only if something else solves the offending metavariable, and if nothing does, an equation with a legitimate restricted solution surfaces as a mismatch.

Pruning replaces the refusal with a restriction. Where a candidate mentions `?n` whose context exceeds `?m`'s, mint `?n'` at the intersection of the two contexts, solve `?n := ?n'` restricted to that intersection, and re-attempt the original equation against the pruned candidate. Where the intersection cannot support `?n`'s type, the equation is genuinely unsolvable and should fail rather than postpone. For same-context metavariables — the whole measured class — the intersection is the full context and pruning degenerates to committing immediately, which is what unblocks the chains.

Named consumers: `Async.crs:399` and `Async/Future.crs:15`, both `let x = action!` continuations whose element type only the continuation pins; and `Async/Future.crs:13`, whose comment records that `Future/new` takes its type parameter explicitly because this gap made the implicit unusable — the item retires that API constraint, not just two `@`s.

Constraints, unchanged from the prior version of this document:

- The existing scope check stays. Pruning narrows what a candidate mentions; it does not license a solution to mention what its birth frame never had.
- Solutions remain monotonic. A pruned metavariable is solved, not retracted, so no cache may assume an unsolved metavariable stays unsolved.
- Pruning runs inside the same transaction as the solve it serves, so a rolled-back speculative branch unwinds the pruning with it.
- `convert.rs:1446`'s comment is the marker for the site to change, and its wording should stop advertising a stand-in once the real thing exists.

One dependency to respect: even with eager commits, the measured chains still take one wake hop through a flex-flex postponement, so this item's acceptance depends on the wake-cascade investigation in Item 1 having landed or exonerated the machinery.

## Item 4 — packed-literal views in unification decomposition

`b[h, ..t]` lowers to `Bits.concat(Bits.append(b[], h), t)`, and the reducer folds constant spines into packed literals. The folding is correct for evaluation and fatal for inversion: once `append(b[], true)` becomes the literal `b[\1]`, an equation like `append(b[], ?h) ≡ b[\1]` or `concat(b[\1], ?t) ≡ b[\1]` has a unique obvious solution and no rule that produces it.

The corpus contains the minimal pair in adjacent lines: `BigNat/succ.crs:95` omits the implicits that `:94` must spell, because line 95's spine keeps open variable heads (nothing folds, rigid decomposition works) while line 94's constant heads fold into literals. All 16 arguments across the four `succ.crs` sites are this one gap, and any future proof work over packed values with constant bits — more `BigNat` arithmetic, `utf8` lemmas — reproduces it.

The rule: during rigid-rigid decomposition, a nonempty packed literal opposite a cons/append/concat spine contributes its own cons view — `b[\1]` decomposes as `append(b[], true)`, equivalently `concat(append(b[], true), b[])` when a concat tail must be answered — and decomposition proceeds pairwise as it already does for unfolded spines. `Bytes` gets the same treatment as `Bits`.

Constraints:

- This is an elaboration-side solving rule; the kernel rechecks whatever it commits, so the soundness exposure is a wrong guess failing recheck, not an unsound acceptance. It must still only fire where the view is forced — a nonempty literal is uniquely a cons of its first atom onto its tail, so the view is deterministic, not a search.
- The reduction laws themselves do not change. The fold laws in `reduce/prim.rs` are shared by both checkers and have prior non-confluence history (the deleted slice-reassociation rule); this item reads them, never edits them.
- The view applies during solving and decomposition only, never in normalization output, so printed terms and erased code are unaffected.

## Defect ledger

Two behaviors observed during the measurement look like defects rather than designed incompleteness. Neither is an item; both carry reproductions because Items 1-3 will trip over them.

**(a) List-literal elaboration refuses a ground solution eagerly.** `Async/map(body, (a) => Option/some(a))` compiles alone, but the same call as a list-literal element fails eagerly at the lambda body with `inferred: Option(A), expected: ?B` — a flex-rigid equation with a ground, in-scope candidate that should commit. Annotating the list type restores it. The suspicion is metavariable state minted under the element-agreement path losing its birth record, making `solve` return `Failed` where `Done` is available. `Async.crs:615` is the corpus consumer; fix as a bug on its own schedule.

```crs
use /std/{Async, Option, Nat};

let probe(@A: Type, body: Async(A)) -> Async({Nat, Option(A)}) =
    Async/select([Async/map(body, (a) => Option/some(a))]);
```

**(b) Wake-cascade fragility.** In the reproduction below, every parked goal's blockers eventually receive solutions — `Option/some(1)` pins the element type, and the chain to the `Cell/new` argument is three rigid decompositions — yet the first goal survives to the drain and reports. `drain_parked` (`typing.rs:292`) documents retry-to-fixpoint with a final sweep, which should have resolved it; either the watch sets miss transitive blockers, or a retry re-parks watching a stale set, or the cascade genuinely runs and something narrower blocks. Resolving which is part of Item 1, and Item 3's acceptance depends on the answer.

```crs
use /std/{Cell, Option, Io, Nat};

let probe: Io({}) =
    let c = Cell/new(Option/none())!;
    let _ = Cell/set(c, Option/some(1))!;
    Io/pure(());

probe
```

## Ordering

1. **Item 1** first: cheap, half-done, and every later item adds ways for work to park. The wake-cascade investigation rides with it.
2. **Item 2** second: the largest measured win, and the lowest-risk verdict change — it commits nothing, converting hard failures into parked goals the Item 1 diagnostics can now explain.
3. **Item 3** third: it commits solutions where the solver currently refuses, is the item with the most soundness exposure, and its chains still need the wake machinery Item 1 vetted.
4. **Item 4** last: self-contained, theory-specific, and independent of the scheduler entirely.

## Implementation map

The likely surface, to be re-read rather than trusted:

- `curios-elab/src/convert.rs` — the postponement rule at the stuck-comparison verdicts (Item 2), pruning at the solve site (`solve`, `convert.rs:1427`; the guard at `:1446`) (Item 3), packed views in the decomposition arms (Item 4), `solve_refinement_free` (`convert.rs:1842`) reporting (Item 1).
- `curios-elab/src/typing.rs` — the drain's conversion-residue diagnostic (`drain_parked`, `typing.rs:292`), retry and wake behavior (`retry_parked`, `typing.rs:264`) (Items 1-3).
- `curios-elab/src/context/solutions.rs` — watch-set computation (`park`, `solutions.rs:209`) for transitive blockers (Items 1, 2).
- `curios-elab/src/error.rs` — the postponed-conversion diagnostic beside `postponed_check` (`error.rs:459`) (Item 1).
- Test modules beside each, plus `curios/src/tests/` for cross-stage programs proving accepted terms compile and run.
- `curios-prelude/std/` — dropping the corpus explicits each item unlocks, as part of that item's retirement.
- `documentation/ROADMAP.md`, `documentation/SOUNDNESS.md`, and affected module rustdocs once items land.

No core representation change is expected, no `HeadKey` or `WitnessKey` variant is added, and the prelude's rkyv archive format is unchanged. No erased IR, continuation IR, wasm, ABI, or runtime change is expected. Nothing here crosses the `curios-cert` seam: the kernel rechecks committed solutions either way, which is precisely why Items 3 and 4 are elaboration-scoped.

## Acceptance tests

**Item 1** — a never-woken `Conversion` goal reports a postponement diagnostic naming its watched metavariables, distinct from a rigid mismatch; a genuine rigid mismatch still reports as a mismatch; the witness-hole case remains its own third state; the landed `postponed_check` behavior is unchanged; a refinement-suppressed postponement names its refinement dependence; defect (b)'s reproduction is explained — resolved or reclassified as designed with the watch sets corrected.

**Item 2** — the `witness_first`/`proof_first` pair both compile; the six `Str.crs` `peel_byte` sites, `Str/utf8.crs:216`, and the three `Char.crs` sites compile with their implicits omitted; a stuck comparison with no metavariable in any blocking position still fails eagerly at the original span; the `add.crs` investigation has a verdict, and if congruence is in scope, `add.crs:1270`/`:1286` compile with implicits omitted.

**Item 3** — an equation whose candidate mentions a same-context metavariable commits by restriction rather than postponing; an equation whose intersection cannot support the pruned metavariable's type fails rather than postponing; a rolled-back speculative branch unwinds its pruning; the degenerate scope check at `convert.rs:1504` still holds; both defect-ledger reproductions' `Cell`/`Future` shapes compile with implicits omitted, and `Future/new` can be re-declared with an implicit parameter.

**Item 4** — `append(b[], ?h) ≡ b[\0]` and `concat(b[\1], ?t) ≡ b[\1]` solve; the four `BigNat/succ.crs` sites compile with all sixteen implicits omitted; the `succ.crs:94`/`:95` pair becomes uniform; `Bytes` literals decompose the same way; a literal opposite a non-spine rigid term still mismatches.

Where practical, tests should assert obligation counts, so a superficially successful program cannot hide duplicated or stranded parked work.

## Non-goals

- Monomorphic lambda inference, in any form — moved to [`06_ANONYMOUS_MATCH_FUNCTION_SPEC.md`](06_ANONYMOUS_MATCH_FUNCTION_SPEC.md) as the `match =>` form's inference-position machinery, with general unannotated-lambda inference dropped outright there.
- η-equating metavariable heads (`?m ≡ λx. ?m(x)` and flex-side contraction). Dropped from scope: the corpus produces no equation of this shape, and the item predates any concrete consumer. Reinstate only when a real program blocks on it; `reduce_func_eta`'s rigid-side contraction is unaffected.
- Hindley–Milner generalization, cross-item inference, or any inference boundary change.
- Full higher-order unification. Imitation stays a guess that blocks rather than a complete procedure, and Item 2 parks undecided goals rather than deciding them.
- Theory-specific inversion beyond the packed cons views of Item 4 — no general "invert this intrinsic" mechanism, and no change to the shared reduction laws.
- Relaxing `solve_refinement_free`'s refusal to commit refinement-derived solutions; Item 1 reports it, nothing licenses it.
- Effect rows, or any second constraint domain beside the universe solver.
- A new core term, or a change to any downstream IR.

## Effort estimate

Item 1 is a focused diagnostics change plus the wake investigation, which is open-ended but bounded by two small reproductions. Item 2 is a small verdict change with a large test surface — the corpus sites are the tests. Item 3 is the largest and carries the most soundness exposure, since it commits solutions in the file carrying the most perimeter weight. Item 4 is a contained decomposition extension with mechanical tests.

## Verification

After each item, run the repository's full done bar in order:

```sh
make curios/runtime
cargo fmt --all -- --check
cargo check --workspace --all-targets --all-features
RUSTFLAGS="-Dwarnings" cargo clippy --workspace --all-targets --all-features
cargo test --workspace --all-targets --all-features
```

Because `curios-elab` is in the browser compiler's dependency graph, also run `make curios/web` with the exactly version-matched `wasm-bindgen-cli`.

Items touching `convert.rs` should additionally be weighed against `SOUNDNESS.md`: the conversion checker carries several probed rows, and changes to solving verdicts are the class of change whose evidence those rows record.

## Retirement criteria

Each item is retired individually; the file is deleted when all are. (Right-biased partial imitation and partially-applied witness keying retired 2026-08-08: semantics in `SYNTAX.md`, rules on `imitate_flex_apply` and `HeadKey::of_whnf`, obligations as `curios/src/tests/concepts.rs`'s partial-family tests.)

- Solver, parking, and retry invariants are recorded in the owning `curios-elab` module documentation and tests.
- The corpus explicits an item unlocks are dropped from `curios-prelude/std/` as part of that item's landing. The nine already-stale arguments — `Async.crs:454`, `Str/utf8.crs:9`, `:205`, `:211-213` — need no item and may be dropped as standalone cleanup at any time.
- Cross-cutting rationale — notably that imitation is a deliberate guess, that packed views are solving-only, and that η-metavariable heads were dropped for lack of demand — is recorded in `DESIGN.md` or `curios-elab/README.md` as appropriate.
- Both defect-ledger entries are resolved or reclassified with their reproductions pinned as tests.
- Each roadmap entry is a checked, unlinked summary, and no reference to this filename remains.
