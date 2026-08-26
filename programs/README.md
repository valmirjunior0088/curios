# programs — the corpus every measurement is taken over

What Curios compiles when someone wants a number. Two kinds of entry live here, and the layout says which is which: **a bare `.crs` is a Curios-only instrument; a directory is a cross-language workload carrying the same program in all eight spellings.**

Nothing here is a test fixture. Fixtures are written inline in the probes that assert on them — `curios/src/tests/codegen/structural.rs` and `parity.rs` — precisely so they can be shaped to the question. These are real programs, because what they measure is what idiomatic code costs, and a program written to be measured tends to answer a question nobody asked.

## Who reads it

- `curios/src/tests/codegen/` — `census.rs` surveys the fourteen Curios-only programs and three of the workloads, `ladder.rs` and `structural.rs` name individual programs, and `churn.rs` measures three under the collector. Figures live beside the probe that reproduces them, never here.
- [`benchmarks/`](../benchmarks/README.md) — the Docker harness times the five workloads against seven other languages. That README owns the results, the toolchains, and the caveats that belong beside a number.
- `cargo xtask profile programs/<file>.crs` — one compilation under the tracing profiler.

Run one directly:

```sh
cargo run --package curios -- run programs/hello_world.crs
echo 1000000 | cargo run --package curios -- run programs/parse_digits.crs
```

Every program except `hello_world.crs` and `dependent_vectors.crs` reads its workload size from stdin. That is not a convenience: a closed program is constant-folded away, so an instrument that does not read its input measures nothing.

## The Curios-only instruments

**The string-walk ladder.** `parse_digits.crs`, `parse_bindless.crs` and `parse_manual.crs` decode the same digit string the same number of times and differ only in what they pay for it — the UTF-8 scan, the closure per character, the bind per character — so each difference isolates one cost. `parse_multibyte.crs` folds mixed-width text through the same walk. `curios/src/tests/codegen/ladder.rs` owns the rung table and the timings.

**The walk mirrors.** `walk_mirror_baseline.crs` is a faithful user-level mirror of `/std/Str/fold`'s walk, and one program per removed obligation follows it: `flat_acc` (the accumulator tuple), `held_scan` (the scan-argument reconstruction), `inline_step` (the returned scan state), `indexed` (the suffix view). They are bounds rather than equivalents — each removal reshapes the arms around it, and they carry no validity witness — which is why they sit outside the census corpus.

**Subject and control pairs.** `state_monad.crs`/`state_manual.crs` and `rng_state.crs`/`rng_manual.crs` run the same loop through a monad and by hand, with identical arithmetic and identical output. `monad_io.crs`, `monad_throw.crs` and `monad_async.crs` run one loop in three carriers, to separate the cost of `bind` from the cost of what `bind` builds.

**Samples.** `hello_world.crs` — also `cargo xtask profile`'s default subject — and `dependent_vectors.crs`, which show the language rather than measure it.

## The cross-language workloads

All five are (a) expressible in a total, structurally-recursive language, (b) immune to constant-folding and closed-form shortcuts — the input arrives at runtime — and (c) bit-identical in output across every implementation, so a mismatch flags a mistranslation before any timing is trusted. [The harness](../benchmarks/README.md) enforces that last property on every run, and it — not co-location — is what keeps the eight spellings agreeing.

- **`lcg`** — iterate `x = (75 · x) mod 65537` N times from `x = 1`. One multiply + one modulo per iteration; the max intermediate is 75·65536 ≈ 4.9M, far under i31. Measures integer ALU + loop/call overhead. Default `N = 100_000_000` (≈ 0.45s of Curios compute; below ~10⁷ it is startup-dominated). Anchor: `lcg(10⁸) = 17662`.
- **`trees`** (the classic binary-trees allocation stress) — build a perfect tree of depth D whose nodes carry unique heap-numbered payloads (root `1`, children `2v` / `2v+1`), then reduce to `sum mod 1000003`. The unique payloads make every node distinct, defeating any structural subtree-sharing and forcing 2^(D+1)−1 real allocations; the modulus keeps the checksum inside i31. Measures allocation + GC and heap traversal. Default `D = 21` (≈ 4.2M nodes, ≈ 0.25s; D=23 ≈ 1s). Anchor: `trees(21) = 536864`.
- **`chain`** — build a cons list of 10 000 cells once, then transform it K times, each round rebuilding every cell from a predecessor that dies with the operation. Measures *death-birth churn*: unlike `trees`, where every allocation survives to be traversed, nothing here outlives the step that replaces it. That is the pairing [Perceus](https://dl.acm.org/doi/10.1145/3453483.3454032) reference counting turns into an in-place write, so the Lean column prices dynamic reuse and the ratio to it is the number this workload exists for. The seed is derived from K so nothing is a closed term, each round reverses the order (which the sum ignores), and every walk is tail-recursive or a loop so no contestant's stack depends on the 10 000. Default `K = 1600` (≈ 16M cells reborn, ≈ 0.33s). Anchors: `chain(8) = 819185`, `chain(1600) = 457407`.

  Two asymmetries belong beside its ratio rather than inside it. A Curios cons cell is **three** slots to Lean's two, since a tagged constructor carries its discriminant — real, and not what the workload is about. And the chain's live set is small by design, so the collector's marking half is barely exercised: what is measured is the allocation rate and the young-collection frequency that churn drives, which is the half reuse removes.

- **`churn`** — thread a six-field record through N LCG-fed steps, two fields updated per step, the written pair rotating over three phases so every field keeps circulating; print one field at the end. The purest record-update signal: the imperative contestants mutate a struct in place and allocate nothing, OCaml's and Grain's functional updates allocate a fresh record per step, and Lean's structure update is the shape Perceus's reset-and-reuse rewrites in place. Curios spells the update as a spread — and its optimizer erases the record entirely: the threaded record travels as fields, the loop allocates nothing (pinned by `churn_threaded_record_allocates_nothing` in `curios/src/tests/codegen/churn.rs`), so its column prices dispatch and checked i31 arithmetic against the mutation floor rather than allocation. The record-update tax this workload was specified to price therefore lives only where a record *rests*, which is the census's and `spines`' territory. Default `N = 75_000_000` (≈ 0.33s of Curios compute). Anchors: `churn(8) = 897441`, `churn(75000000) = 762495`.

- **`spines`** — N LCG-keyed inserts into a map, then fold the values; the keys revisit a 65 536-value orbit, so the live set plateaus while every insert keeps rebuilding a root-to-leaf spine that dies with the operation — the live-set-under-churn dimension `chain` deliberately lacks. Two confounds are part of the design and the reason it orients rather than proves: the table compares map algorithms as much as memory management (Curios's crit-bit trie against imperative hash maps and the functional contestants' balanced trees), and `/std/Map` deliberately has no `Key(Nat)` — the injectivity a `Key` witness owes is unprovable for a division-based encoding under unary elimination, as the module records — so Curios keys enter through `Bytes/of_nat`: minimal big-endian, a division and a table index per byte, a boundary cost of a few arithmetic operations no int-keyed hash map quite mirrors. Lean's `Std.TreeMap` is the reuse-on-spines column: a persistent tree whose dying path Perceus rewrites in place. Default `N = 75_000` (≈ 0.3s of Curios compute). Anchors: `spines(8) = 28`, `spines(75000) = 675283`.

### Why the constants are small

Curios's `Nat` and `Int` are unbounded in the type checker but ride an **i31** — the unboxed WebAssembly-GC 31-bit integer — at runtime, and arithmetic is _checked_: a result that leaves i31 traps. Why it traps rather than wrapping is [Numeric carriers narrow by refusing, never by changing a value](../documentation/design/toolchain/numeric-carriers-narrow-by-refusing-never-by-changing-a-value.md). (`Flt`/`f64` has the range but heap-allocates per value — the wrong tool for a tight integer loop.) So every workload is deliberately sized to keep **every intermediate, including products,** within i31, and every other language uses its native integer to compute the identical values. The upshot: the integer comparison is like-for-like on values, and it honestly folds Curios's per-op overflow check into the measured cost rather than hiding it.

### Editing a spelling

A workload directory holds `<name>.{crs,rs,ml,js,ts,gr}`, `<Name>.lean` and a `lakefile.toml` — Lake is package-oriented, which is why these five are directories and the instruments above are not. Change any spelling and the next run's cross-check is what catches a mistranslation.

One spelling is not "edit and rerun": a changed `.gr` needs an image rebuild, for the reason the [harness's toolchain notes](../benchmarks/README.md) give.
