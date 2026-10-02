# programs — the corpus every measurement is taken over

What Curios compiles when someone wants a number. Two kinds of entry live here, and the layout says which is which: **a bare `.crs` is a Curios-only instrument; a directory is a cross-language workload carrying the same program in Curios and in Rust.**

Nothing here is a test fixture. Fixtures are written inline in the probes that assert on them — `curios/src/tests/codegen/structural.rs` and `parity.rs` — precisely so they can be shaped to the question. These are real programs, because what they measure is what idiomatic code costs, and a program written to be measured tends to answer a question nobody asked.

## Who reads it

- `curios/src/tests/codegen/` — `census.rs` surveys thirteen of the Curios-only programs and four of the workloads — every one but `lcg` —, `ladder.rs` and `structural.rs` name individual programs, and `churn.rs` measures three under the collector. Figures live beside the probe that reproduces them, never here.
- [`xbench/`](../xbench/README.md) — the bench times the five workloads against Rust, compiled natively and to WebAssembly. That README owns the readings, how a figure is read from them, and when a comparison is refused.
- `cargo xtask profile programs/<file>.crs` — one run under a profiling build, folded into its summaries; debug unless `--profile release` asks for the shipped compiler. What that build measures and where it files it is [`curios-profile`](../curios-profile/README.md)'s.

Run one directly:

```sh
cargo run --package curios -- run programs/hello_world.crs
echo 1000000 | cargo run --package curios -- run programs/parse_digits.crs
```

Every program except `hello_world.crs` and `dependent_vectors.crs` reads its workload size from stdin. That is not a convenience: a closed program is constant-folded away, so an instrument that does not read its input measures nothing.

## The Curios-only instruments

**The string-walk ladder.** `parse_digits.crs`, `parse_bindless.crs` and `parse_manual.crs` decode the same digit string the same number of times and differ only in what they pay for it — the UTF-8 scan, the closure per character, the bind per character — so each difference isolates one cost. `parse_multibyte.crs` folds mixed-width text through the same walk. `curios/src/tests/codegen/ladder.rs` owns the rung table and the timings.

**The walk mirrors.** `walk_mirror_baseline.crs` is a faithful user-level mirror of `/std/Str/fold`'s walk, and one program per removed obligation follows it: `flat_acc` (the accumulator tuple), `held_scan` (the scan-argument reconstruction), `inline_step` (the returned scan state), `indexed` (the suffix view). They are bounds rather than equivalents — each removal reshapes the arms around it, and they carry no validity witness — which is why they sit outside the census corpus.

**Subject and control pairs.** `state_monad.crs`/`state_manual.crs` and `rng_state.crs`/`rng_manual.crs` run the same loop through a monad and by hand, with identical arithmetic and identical output. `monad_io.crs`, `monad_result.crs` and `monad_async.crs` run one loop in three carriers, to separate the cost of `bind` from the cost of what `bind` builds.

**The rope's hazard.** `rope_push_peek.crs` alternates one append with one indexed read over a list growing to N, the alternation `curios-emit`'s rope cost model names as quadratic, with `rng_manual.crs` as its control: the same arithmetic and the same output with no list. It stands outside the census corpus, whose roster is fixed, and it exists so that a change to how a rope answers a read has a program to be measured on.

**The NaN check's cost.** `flt_hot_loop.crs` runs N rounds of ties-to-even float arithmetic — a multiply, two adds, a square root and a divide — on a value that stays positive, so every NaN check the emitter places after an instruction is taken and none fires. `curios-emit`'s README owns the figure and how to retake it.

**Samples.** `hello_world.crs` — also `cargo xtask profile`'s default subject — and `dependent_vectors.crs`, which show the language rather than measure it.

## The cross-language workloads

All five are (a) expressible in a total, structurally-recursive language, (b) immune to constant-folding and closed-form shortcuts — the input arrives at runtime — and (c) bit-identical in output across every implementation, so a mismatch flags a mistranslation before any timing is trusted. [The bench](../xbench/README.md) enforces that last property before it times anything, and it — not co-location — is what keeps the two spellings agreeing.

- **`lcg`** — iterate `x = (75 · x) mod 65537` N times from `x = 1`. One multiply + one modulo per iteration; the max intermediate is 75·65536 ≈ 4.9M, far under i31. Measures integer ALU + loop/call overhead. Default `N = 100_000_000` (≈ 0.45s of Curios compute; below ~10⁷ it is startup-dominated). Anchor: `lcg(10⁸) = 17662`.
- **`trees`** (the classic binary-trees allocation stress) — build a perfect tree of depth D whose nodes carry unique heap-numbered payloads (root `1`, children `2v` / `2v+1`), then reduce to `sum mod 1000003`. The unique payloads make every node distinct, defeating any structural subtree-sharing and forcing 2^(D+1)−1 real allocations; the modulus keeps the checksum inside i31. Measures allocation + GC and heap traversal. Default `D = 21` (≈ 4.2M nodes, ≈ 0.25s; D=23 ≈ 1s). Anchor: `trees(21) = 536864`.
- **`chain`** — build a cons list of 10 000 cells once, then transform it K times, each round rebuilding every cell from a predecessor that dies with the operation. Measures *death-birth churn*: unlike `trees`, where every allocation survives to be traversed, nothing here outlives the step that replaces it. That is the pairing [Perceus](https://dl.acm.org/doi/10.1145/3453483.3454032) reference counting turns into an in-place write, and it is the shape a collector pays most for, which is why the workload is sized to drive it. The seed is derived from K so nothing is a closed term, each round reverses the order (which the sum ignores), and every walk is tail-recursive or a loop so no contestant's stack depends on the 10 000. Default `K = 1600` (≈ 16M cells reborn, ≈ 0.33s). Anchors: `chain(8) = 819185`, `chain(1600) = 457407`.

  One thing belongs beside its figure rather than inside it: the chain's live set is small by design, so the collector's marking half is barely exercised. What is measured is the allocation rate and the young-collection frequency that churn drives, and what that costs Curios against a contestant managing linear memory is bounded rather than attributed — `chain_collection_decomposition` in `curios/src/tests/codegen/churn.rs` is what separates the collector's share from the codegen's.

- **`churn`** — thread a six-field record through N LCG-fed steps, two fields updated per step, the written pair rotating over three phases so every field keeps circulating; print one field at the end. The purest record-update signal: Rust mutates a struct in place and allocates nothing, which is the floor a functional update is read against. Curios spells the update as a spread — and its optimizer erases the record entirely: the threaded record travels as fields, the loop allocates nothing (pinned by `threaded_record_allocates_nothing` in `curios/src/tests/codegen/churn.rs`), so its column prices dispatch and checked i31 arithmetic against the mutation floor rather than allocation. The record-update tax this workload was specified to price therefore lives only where a record *rests*, which is the census's and `spines`' territory. Default `N = 75_000_000` (≈ 0.33s of Curios compute). Anchors: `churn(8) = 897441`, `churn(75000000) = 762495`.

- **`walk`** — fold a mixed-width UTF-8 text N times into a rolling hash, each fold seeded by the accumulator the fold before it produced. The text carries 1-, 2-, 3- and 4-byte characters, so the walk leaves the ASCII arm of `/std/Str/fold` and its continuation obligations run once per continuation byte rather than never. The seeding is what makes it a workload rather than an instrument: a fold over an unchanging text is loop-invariant, and an implementation free to hoist it walks the text once while one that cannot walks it N times, so the figure would price the optimizer rather than the walk. Default `N = 150_000`, chosen where both spellings are measurable — Curios is far enough off Rust here that sizing for one leaves the other inside its own startup. Anchors: `walk(8) = 464630`, `walk(150000) = 318377`.

- **`spines`** — N LCG-keyed inserts into a map, then fold the values; the keys revisit a 65 536-value orbit, so the live set plateaus while every insert keeps rebuilding a root-to-leaf spine that dies with the operation — the live-set-under-churn dimension `chain` deliberately lacks. The table compares map algorithms as much as memory management — Curios's crit-bit trie against Rust's hash map — which is why it orients rather than proves. Keys enter as a `Nat` in Curios and as a machine integer in Rust. Default `N = 75_000` (≈ 0.3s of Curios compute). Anchors: `spines(8) = 28`, `spines(75000) = 675283`.

### Why the constants are small

Curios's `Nat` and `Int` are unbounded at every layer, and at runtime each is an **i31** — the unboxed WebAssembly-GC 31-bit integer — while it is small and a boxed magnitude past it, every operation keeping a fast path for two i31s and a range check on its result: [Nat and Int are an i31 until they outgrow it](../documentation/design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md). (`Flt`/`f64` has the range but heap-allocates per value — the wrong tool for a tight integer loop.) So every workload is deliberately sized to keep **every intermediate, including products,** within i31 — on the fast path — and every other language uses its native integer to compute the identical values. The upshot: the integer comparison is like-for-like on values, and it honestly folds Curios's per-op test and range check into the measured cost rather than hiding it.

### Editing a spelling

A workload directory holds `<name>.crs` and `<name>.rs` — the one program in two spellings, which is why these five are directories and the instruments above are single files. The Rust spelling is compiled twice, natively and to WebAssembly, from that one source. Change either and the next reading's agreement check is what catches a mistranslation, before anything is timed.
