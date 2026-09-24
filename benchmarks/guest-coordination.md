# Guest coordination measurements

This capture compares compilation and execution around the migration from rewritable cells and notification-based coordination to write-once cells, bounded channels and level waiting. The baseline is recorded before that migration; the comparison remains pending. It is a local native measurement, separate from the containerized cross-language series.

## Workloads and reproduction

```sh
cargo test --release --package curios --lib --all-features -- --ignored --nocapture coordination_measurements --test-threads=1
```

The instrument is [coordination.rs](../curios/src/tests/coordination.rs). It runs one warmup (sample 0) and five measured samples per workload, in order: `monad_async`, then `tui`. Each sample compiles the source afresh against the embedded prelude and executes it once with a fresh scripted host. There is no package-cache lookup. Rust compilation and prelude construction finish before the instrument starts and are excluded.

- **`monad_async`:** [the existing corpus program](../programs/monad_async.crs), with stdin `10000\n`, requiring stdout `30000\n` and no stderr. This measures the Async carrier's recursive bind path, not contention among fibers.
- **`tui`:** the existing [bordered listing fixture](../curios/src/tests/tui.rs), driven by 100 down/up pairs followed by `q`. The first down escape sequence arrives in two chunks; each remaining key has its own chunk. Terminal-size queries answer 12×5, 16×6, then 12×5 forever. Monotonic-clock queries advance by 1 ms, starting at zero, with 100,000 scripted readings. The mock host releases the next input chunk when polled and does not sleep in real time. Assertions require terminal entry and restoration, raw mode on then off, selection `one`, a titled border, exactly three screen clears (initial frame and two resizes), no stderr, and a successful exit.

`curios_profile::trace` listens during compilation and execution, writing one unrotated stream per sample into a timestamped directory under `curios/.artifacts/coordination/`. Each stream has a folded `.tsv` beside it. The `coordination_compile` span includes parsing, all compiler stages, Binaryen and Cranelift precompilation. The `coordination_run` span includes deserialization, instantiation and execution. Host setup, output assertions and folding are outside those spans. All span boundaries must close. The test binary has no counting allocator; memory columns in the folded reports are zero and are not allocation evidence.

These are wall-clock timings under built-in tracing, with its overhead included. The process is not CPU-pinned and frequency scaling remains enabled. One warmup does not eliminate machine noise; retain the individual samples, use the same workload and profiling configuration for the comparison, and investigate changes before drawing conclusions. The Tui workload has synthetic time and no physical terminal latency.

## Baseline

Captured on 2026-09-24, on `main` at `3a80b39ae1f657ca7a0871349897fa1d1d6ee036` plus the measurement harness in this checkpoint: `/sys/Option` and the shared string registry entry are present; cells, channels, Async and Tui still use the previous coordination implementation.

Host: AMD Ryzen 7 5800X3D (8 cores, 16 logical CPUs), `x86_64-unknown-linux-gnu`, Linux `7.2.4-ogc3.1.fc44.x86_64`; Rust `1.95.0 (59807616e 2026-04-14)`. Cargo release profile, all features, one test thread, default runtime engine configuration.

All times below are milliseconds. Sample 0 is the warmup and is excluded from the medians.

| Workload | Sample | Compile | Run | Output bytes |
| --- | ---: | ---: | ---: | ---: |
| `monad_async` | 0 (warmup) | 1717.323 | 2.354 | 6 |
| `monad_async` | 1 | 1572.409 | 2.071 | 6 |
| `monad_async` | 2 | 1567.169 | 1.996 | 6 |
| `monad_async` | 3 | 1572.799 | 2.028 | 6 |
| `monad_async` | 4 | 1563.540 | 2.056 | 6 |
| `monad_async` | 5 | 1556.929 | 2.040 | 6 |
| `tui` | 0 (warmup) | 4751.845 | 53.916 | 11368 |
| `tui` | 1 | 4743.140 | 43.359 | 11368 |
| `tui` | 2 | 4721.612 | 41.798 | 11368 |
| `tui` | 3 | 4747.226 | 45.056 | 11368 |
| `tui` | 4 | 4731.631 | 42.499 | 11368 |
| `tui` | 5 | 4759.048 | 60.506 | 11368 |

Measured medians: `monad_async` compile **1567.169 ms**, run **2.040 ms**; `tui` compile **4743.140 ms**, run **43.359 ms**. Tui execution ranges from 41.798 to 60.506 ms, so a later comparison must account for that spread. All twelve samples passed their behavior and closed-span assertions; each workload produced the same output length in every sample.

Local raw and folded captures: `curios/.artifacts/coordination/1790267061024372648/`, with files named `<workload>-<sample>.trace` and `<workload>-<sample>.tsv`. These generated artifacts are not committed; the protocol and individual timings above are the durable baseline.
