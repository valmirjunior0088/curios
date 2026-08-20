# The map's remaining distance falls by a walk, a cast and a branch

## Status

**Partly landed, and being re-scoped as it lands — read this section before the steps below, which are the original plan and no longer all of it.** Step 0 landed on 2026-08-20 as a soundness fix in its own right (`ImmediateGet`, plus an emitter guard that turns the whole class into a build failure), and step 1 landed the same day at **−27% on the insert slope, measured like-for-like on one box** — its figure now lives in `map_wall_spines_slope`. Step 2 is still to land. **Step 3 is under review and is expected to be declined**: a read-only session found that Binaryen already runs closed-world and its `TypeRefining` narrows *nothing* in `$tuple/*` — every field is still `anyref` after `-O2`, because one type serves every constructor of that arity module-wide — so the width-keyed tower withholds the information a pass already being paid for would use, and investing in it entrenches that. The measured population is 338 `anyref` box/unbox sites against 26 tuple casts in optimized `spines`. A decomposition probe on the post-step-1 insert decides it, and this file is rewritten around the result.

Originally: refined working specification, not started. Produced by the read-only investigation of 2026-08-20 over run 08 of the benchmark harness ([08_RESULTS.md](../../benchmarks/08_RESULTS.md)), the emitted code of `programs/spines/spines.crs` at commit `418309d0`, the pinned engine's source (wasmtime 47.0.3), and throwaway probes whose figures and methods are recorded inline below. It sequences four steps; the first is a soundness fix found in passing and lands alone, the other three are independently landable and independently measured. Every figure here carries its method; when a step lands, its figure moves into an ignored probe beside the code per the measurement discipline, and this file records only what the figure decided.

## Why it exists

[The map wall falls by classes, not by symptom](../design/toolchain/the-map-wall-falls-by-classes-not-by-symptom.md) closed its campaign with `spines` at 54.4 ms natively — 22.6× off Rust's hash map, 2.9× off Perceus-compiled Lean — and the harness's run 08 confirmed the landing with every other column held still. That is a distance of the same kind the other four workloads show, but still the largest of them, and the decision record names what it left on the table: the insert walks twice, every fork descent pays a cast and a dispatch, and the rebuild keeps real frames.

This specification decomposes what remains and finds that most of it is not a map cost at all. One part is library-local and was a step the previous campaign consciously skipped because a later step would supersede it — and that later step was declined. The rest is two compiler classes every program with a heap variant pays: a `match` on a two-heap-constructor family is a **host call** on the engine this pipeline ships, and a two-way `match` is a jump table where a conditional branch would do. Beside them sits a miscompile the investigation's own probes tripped over, which lands first because it is a soundness bug.

## The deficit, decomposed

Measured 2026-08-20 on an Apple M4 Pro (native macOS, not the harness's Docker VM), release `curios` at `418309d0`, each program compiled once with `curios compile` and the executable timed whole-process, min of 7, at N = 0, 25 000 and 75 000 so the fixed cost and a slope both read. Every variant printed `spines(8) = 28` and `spines(75000) = 675283`, the corpus's anchors.

- **Stock `spines` reads 55.1 ms at N = 75 000, against the harness's 54.4.** The fixed cost is 2.5 ms, so the remaining ~700 ns per insert is work, and the machine reproduces the harness closely enough to price changes on.
- **A verbatim copy of `/std/Map`'s trie inside the program reads within 1.5% of stock** (55.8 ms), so variants that differ from that copy by one change price that change alone.
- **The single-walk insert reads 38.2 ms — −32% — with the insert slope falling from 778 to 527 ns.** One descent that computes the crit at the leaf and wedges on the way back up replaces the `lookup` descent plus the `insert_node` descent.
- **Mask forks read +10% slower on top of that** (41.9 ms): storing a byte index and mask beside the crit widens every fork, and the extra field costs more to rebuild and read than the `div 9`/`rem 9` it deletes. A power-of-two position encoding (`g·16 + o`, so the split is a shift and a mask) reads within noise (−1%). The record's claim that per-level arithmetic became cheap once the key rode the i31 holds; the arithmetic is not the wall.
- **What a `match` on a heap variant costs, isolated on a resting `List(F)` of 65 536 elements folded 100 and 300 times, per element:** a family whose constructors are both tag-only (`a()`/`b()`, every object exactly `$tuple/1`) reads 9.1 ns; the same family with a `Nat` payload on each (`a(Nat)`/`b(Nat)`, every object a `$tuple/2` read through `$tuple/1`) reads **12.5 ns**; a family with one immediate constructor (`a(Nat)`/`b()`, dispatched by `ref.test (ref i31)` and an `if`) reads 8.5 ns. Two sequences agreed to ±0.1 ns. So the supertype cast costs ~3.3 ns and the `br_table` ~0.5 ns over a conditional branch. The same pair measured on a linked list — `nil()`/`cons` against `stop(Nat)`/`cons` — read 5.25 against 3.17 ns per cell, which is ~40% of `chain`'s 8.3 ns per cell.

**Why the cast is a host call.** The emitter reads every variant's tag as `TupleGet(0)`, which `curios-cont/src/into_wasm/code_emitter.rs` casts to `$tuple/1` — the smallest prefix type holding the field — and the `$tuple/N` family is declared as a subtype chain, `$tuple/4 <: $tuple/3 <: $tuple/2 <: $tuple/1`, none final (`module_emitter.rs`, `emit_tuple_types`). In wasmtime 47.0.3, `ref.cast`/`ref.test` to a concrete non-final type compares the object's type index against the target and, on inequality, calls the `is_subtype` libcall — a host call through a per-store `HashMap<u64, bool, NopHasher>` cache (`wasmtime-internal-cranelift/src/func_environ/gc.rs`, `is_subtype`; `wasmtime/src/runtime/store/gc.rs`, `is_subtype_cached`; the engine's own FIXME names issue #13484). Only `b_is_final` short-circuits the slow path, and only for the positive answer's absence — so a *failed* test on a non-final type is a libcall too. Every real node is a `$tuple/3` or `$tuple/4`, never a `$tuple/1`, so every tag read misses the fast path. `trees` escapes because its leaf collapses onto the i31 and the one boxed constructor's cast is exact; `Node`, `Option`, `Result` and `chain`'s `nil`/`cons` do not.

**Why the branch is a jump table.** `Context::match_instrs` (`curios-cont/src/into_wasm/context.rs`) emits an `if` only for one case plus a default; two sequential cases — every `Bool` match, which `Rhs::SwitchBool` lowers as cases `{0, 1}` with no default, and every two-constructor tag — take the `br_table` path, and Binaryen leaves it. Cranelift lowers `br_table` on aarch64 as `cmp, b.hs, csel, adr, ldrsw, add, br` — a dependent load feeding an indirect branch — against a single `cbz` for an `if`. There is no small-table special case in the translator, the mid-end or the ISA lowering.

Per insert after the single walk, one descent of ~17 levels reads a tag through the libcall at every level and dispatches two `Bool` matches and the tag through jump tables — roughly 60 ns of libcalls and 25 ns of jump tables against ~470. The two classes are worth more elsewhere: `chain` pays one libcall per cell.

## Step 0 — the door stops lying about an immediate payload

**The bug.** The program below prints `0` for input `0` and traps for every input `r ≥ 1`: the inlined loop builds the cons cell with `struct.new $tuple/3` and immediately casts it with `ref.cast (ref i31)`. It becomes this step's regression test. Ersd-optm and cont-optm are correct; the fault is below them. Seeding with `stop()` instead of `stop(r)`, or matching the list once instead of in a loop, does not trip it.

```crs
use /std/{Str, Nat, Option, Io};

induct L: Type
| stop(Nat)
| cons(Nat, L)
end

rec build(n: Nat, acc: L) -> L =
    match n: (_) => L
    | 0 => acc
    | m + 1; ih => build(m, L/cons(m, acc))
    end;

rec total(c: L, acc: Nat) -> Nat =
    match c: (_) => Nat | stop(z) => acc + z | cons(v, tail) => total(tail, acc + v) end;

let input = /std/read()!;
match input: (_) => Io({})
| some(bytes) =>
    match Str/of_bytes(bytes): (_) => Io({})
    | some(s) =>
        match Nat/of_str(Str/trim(s)): (_) => Io({})
        | some(r) => /std/print(Str/concat(Nat/to_str(total(build(r, L/stop(r)), 0)), "\n"))
        | none() => /std/print("bad input\n")
        end
    | none() => /std/print("invalid utf-8\n")
    end
| none() => /std/print("no input\n")
end
```

**The cause.** `lower_collapsed_arm` (`curios-ersd/src/into_cont.rs`) aliases a one-binder arm's payload to the scrutinee itself, which is right for the `Collapsed` encoding, where the value is always the scalar — and `lower_immediate_match` reuses it for the tested `Immediate` encoding, where the scrutinee is a scalar on one path and a tuple on the other. The representation analysis then does exactly what its header promises: `NatAdd` demands the raw carrier of its operand, the operand is the scrutinee, the scrutinee is a continuation parameter (`Offer::Open`) so the demand is admitted, the edge rule carries it back to `build`'s accumulator, and the `cons` construction arriving on that parameter is coerced to raw at the edge — `ref.cast i31` on a struct. The analysis's premise — *a value's carrier is fixed by whatever produced it* — is violated because the payload has no producer of its own.

**The fix.** A new intrinsic, `CpsIntrinsicOp::ImmediateGet`: one operand read at `Repr::Ref`, result `Repr::Ref`, `Total`; the emitter passes the reference through unchanged; `evaluate.rs` folds it to its literal. `lower_immediate_match` binds the immediate arm's binder through it instead of through `lower_collapsed_arm`. The payload now has a definition of its own whose offer is `Never`, like a function parameter's, so a raw demand lands on it and coerces at its use, and the scrutinee is never demanded raw. For correct programs the emitted code is what it was: the arm already unboxed at each use.

**Rejected — withdrawing `IsImmediate`'s operand in `represent::offers`.** Three lines, and probably sufficient, but it adds an exception to the analysis to compensate for a door that lies; the premise should be made true rather than worked around. The rule is worth keeping as an *assertion* rather than a fix, which the next paragraph does one level lower.

**Defense in depth.** `Context::load_value_instrs` and `jump_instrs` know when the value they load is a `Tuple` or `List` construction. Loading such a value at a raw carrier is never correct, so it becomes a compile-time panic instead of a runtime `ref.cast` trap: the whole class becomes a build failure, with a positive control pinning that the guard fires.

**Optional refinement, deliberately not part of the fix.** `FieldShape::Immediate` could carry the payload's carrier — `curios-elab/src/into_ersd/classify.rs` already distinguishes `Nat`/`Bool`/`Byte` from `Int` where it decides the shape — so `ImmediateGet` could answer `Fixed(Nat)` or `Fixed(Int)` and the payload could ride raw through a loop. A schema change for a performance question, to be priced on its own probe if a kernel ever shows the coercion.

**Tests.** The reproducer as a cross-stage test in `curios/src/tests/matching.rs`; a door test in `curios-ersd/src/into_cont/tests.rs` asserting the immediate arm's payload is a distinct `ImmediateGet` value rather than the scrutinee; the emitter guard's positive control. `structural.rs`'s T6 and `packed_unary_payload_declines_the_immediate_encoding` hold unchanged — `sum` still discriminates with `ref.test (ref i31)` and no `$tuple/1` cast.

## Step 1 — the insert and the remove walk once

**Change, in `curios-prelude-archive/std/Map.crs`.** `insert1(t, k, v) -> {Node(V), Nat, Nat}` descends by `bit(k, tc)` and rebuilds on the way back up. At the leaf it compares keys: equal returns `(leaf(k, v), 0, replaced)`; otherwise it computes `crit(k, k0)` and returns `(leaf(k0, v0), c, pending)`. A fork whose child came back `pending` wedges the new leaf between itself and that child when `tc < c`, answering `placed`; otherwise it returns its *original* node, still `pending`, and the decision moves up. The root wedges if still pending. The third component — placed, pending, replaced — is what keeps `size` right. The invariant that makes the return-side decision correct, to be stated in the module comment: the leaf found by following `k`'s bits agrees with `k` at every crit on its path, so `c` never equals a path crit, and the wedge point is the shallowest fork whose crit exceeds `c`, which the return reaches first from below exactly when its parent's crit is below `c`.

`remove1(t, k) -> {Node(V), Nat}` is the same shape with states unchanged, removed and emptied: a fork whose child emptied returns its sibling as removed; the root maps emptied to `none`. `get` already walks once.

**What it preserves.** The shape is a function of the key set, so canonicity — and with it `Toml/encode`'s document order and structural map equality — is untouched. `curios/src/tests/map.rs` already pins entries agreeing across insertion orders, prefix-related keys, and delete-and-collapse; those are the check.

**History.** This was the first half of the retired campaign specification's step 3, named there as a hedge to be landed only if the reshape slipped, because the reshape would have replaced the crit descent outright. The reshape was implemented, measured and declined on its own figure, so nothing supersedes the hedge any more; the decision record gets one sentence saying so.

**Predicted band and probe.** −30% on the harness's `spines` columns, slope to ~530 ns per insert; `map_wall_spines_slope` in `curios/src/tests/codegen/map_wall.rs` gets a dated section with the retaken figure on its own box.

## Step 2 — a two-way match is a branch

**Change, in `Context::match_instrs`.** A switch whose cases are exactly `{0, 1}` with no default — every `Bool` match, every two-constructor tag, the exhaustive shape — emits `If { then: jump(case 1), else: jump(case 0) }` on the loaded operand, which is in `{0, 1}` by construction. Cases `{0, 1}` *with* a default route through the existing `binary_search_instrs`, which is already the compare chain that shape wants. Three or more cases keep the jump table; whether a three-way compare chain beats it is a separate probe question and is not decided here.

**Tests.** `structural.rs`'s header and G4 describe a constructor-tag match as a `br_table` over `$case$N` labels and identify a genuine dispatcher by the `loop $$dispatch/` name alone, so G4's logic holds and its prose is updated; a small kernel pins that a `Bool` match emits no `br_table`.

**Predicted band and probe.** ~0.5 ns per dispatch on the fold probe; on `spines` roughly three dispatches per level. Small and universal — it lands for what it deletes from every program, and the probe of step 3 carries its figure.

## Step 3 — a tuple is read at its own type

**The constraint that shapes it.** `cps/fields.rs` rebuilds a narrow constructor at its region's width: a surviving whole-value use of a `leaf` inside a split region can be a `$tuple/4` with filler, by the variant-width decision's own account. So no reader may assume an object's arity is its constructor's — only that it is *at least* that, because widening never narrows (`takeable` refuses a multi-width source rather than reading it at the narrowest). Any design that casts to a single exact arity is unsound; the cascade below exists because of this.

**IR.** `TupleGet(index)` becomes `TupleGet { index, likely }`, where `likely` is the widths the producer expects, in preference order. Every producer knows one: a tag read lists the family's constructor arities, widest first; an arm's payload reads list `[constructor arity, family max]`; `Rhs::Project` lists its `ProductId`'s arity; the optimizer's own projections (`fields.rs`, `simplify.rs`) list the settled width they were minted at. The operand is read at `Repr::Ref`; `Repr::Tuple` loses its only producer and is deleted from the roster, `LoadAs::of`, `box_instrs` and the table. The verifier checks `index < min(likely)`; the literal folds are unchanged.

**Emitter.** `emit_tuple_types` declares every `$tuple/N` final with no supertype. `TupleGet` emits a `ref.test` cascade: each width in `likely`, then every roster arity *greater* than `max(likely)` — the only widths widening can produce — the last as a `ref.cast`. Every test is an exact-type equality, one load and a compare, and no negative ever reaches a libcall because nothing is a subtype of anything. For `Node` in `spines`' roster of `$tuple/0..4` the fallback is empty; for `chain`'s `cons` the cascade is one test. The table's maximum arity reads from the op.

**Rejected — keeping the chain under the cascade.** It needs no fallback, since the prefix cast still catches a widened object, but a failed `ref.test` on a non-final type is itself the libcall, so it only moves the cost from `fork`, `cons` and `some` to `leaf`, `nil` and `none`. Finality is what makes a negative free, and the roster fallback is its price.

**Rejected — dispatching on arity alone, without a tag.** Distinct-arity families would need no tag read, but a widened narrow constructor has its sibling's arity, so the tag stays the discriminant and the cascade only decides which exact type to read it through.

**Rejected — waiting for an inline supertype check in the engine.** Issue #13484's "true solution" would delete the host call but still walk supertype arrays — several dependent loads against one compare — and it would arrive on the engine's schedule, under an invariant this pipeline does not need once its types are final.

**Interactions.** [A variant travels as the fields of its widest constructor](../design/toolchain/a-variant-travels-as-the-fields-of-its-widest-constructor.md) keeps its rationale untouched — fields stay `(ref null any)` — and gains the reader-side rule above. [A monomorphic field carries its own type](typed-heap-fields-spec.md) re-keys tuple types by shape vector; the cascade keys on `likely` per site, so it survives that change, but the roster fallback grows with the roster and must be re-priced when that census runs — a note to add there when this lands. Binaryen's closed-world type refinement may prune tests it can prove impossible; Cranelift needs nothing.

**Tests and probe.** Structural assertions that every `$tuple/N` is `sub final` with no supertype, and that a `Node`-shaped match's hot path carries `ref.test (ref $tuple/4)` and no `ref.cast (ref $tuple/1)`. The three-way fold probe above becomes an ignored measurement in `curios/src/tests/codegen/`, carrying today's 12.5 / 9.1 / 8.5 ns per element and the expectation that the first two converge on the third. `map_wall_spines_slope` is retaken, and the harness's `chain` column is the cross-language check: about 2 of its 8.3 ns per cell is this class.

**Documentation.** One design decision under `documentation/design/toolchain/` — the decision, the widening fact that forced the cascade, and the rejections above — and the roadmap line this file already holds.

## Order and gating

Step 0 lands alone: a soundness fix, its own commit, the full gate. Steps 1, 2 and 3 land in that order as one task — the largest measured win first, the trivial one next, the representation change last — with Clippy between steps and the full gate once at the end, per the contributor guide. Each step is independently landable and judged by the probe named in its section; a step whose figure reads inside its probe's noise is recorded as such rather than kept on faith.

## Measurement

The harness's `spines` and `chain` columns are the end-to-end check. The honest target remains the Lean/OCaml cluster: Rust's row is an imperative hash map no canonical persistent structure will meet. Against Lean's 18.7 ms the steps predict, in order, roughly 37 ms, then the mid-thirties, leaving a distance of the kind the other columns show; what remains after them is the record's parked classes — a fork descent's `ref.as_non_null`s and the rebuild's real frames — each its own measured decision.

## Appendix — what this specification does not touch

- **Mask forks and position encodings.** Measured above and declined: the arithmetic per level is not the wall.
- **Typed heap fields and rebuild frames.** The record's parked classes, unchanged by this sequence; typed fields gain a note on the roster fallback when step 3 lands.
- **A three-way compare chain**, and **carrier-aware `ImmediateGet`**. Each is a probe question that this file names and leaves open.
