# Binaryen and Cranelift compile a function in its size

[Every stage but reduction costs the graph it is handed](../../design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md) states that "a compilation ends — accepted, or refused for its budget — in time proportional to what was written plus what the budget counted". Binaryen's optimizer and Cranelift's code generation are stages of a compile, and each pays more than that for a long function. Every stage before them is held to the rule by `growth_slopes` (`curios/src/tests/instrumented/growth.rs`), and the emitter hands them each function with its locals allocated by liveness ([`curios-emit`'s decision](../../../curios-emit/README.md#locals-are-allocated-by-liveness)).

## What is wrong

### Binaryen pays superlinearly for a long function

A replay at `28bcb9aba` with Binaryen `version_130` attributes a dependent addition chain to `precompute-propagate` after `simplify-locals`, and an open list literal to `simplify-locals-nostructure`. This is an attribution to investigate, not a finding against the version `curios-binaryen/build.rs` now pins: the pass attribution must be repeated with that version, the current emitter and the current settings before any pass is skipped.

[Precompute-propagate's introduction](https://github.com/WebAssembly/binaryen/pull/1179) already kept it to higher levels for its compile time. The [LLVM WebAssembly backend](https://android.googlesource.com/platform/external/opencl/llvm-project/+/cd46354dbd10) counts excess locals among what makes it "easy to hit engine limitations of 50K locals" and code "slower to compile to native code".

### The counted slopes reach both

`growth_slopes` (`curios/src/tests/instrumented/growth.rs`) owns the counted reading and the growths held before Binaryen alone (`HELD_FRONT`). These include open and nested lists, chains of operators, match arms and nested matches, and chains of functions. The current reading is taken before attributing any of them to a pass. Its pooled instruction counts can vary between runs, so a pass attribution is held by repeated readings and by the allocations, bytes and pass-local work beside them.

## Goal

Doubling a program along any axis an author can grow it at most doubles what Binaryen and Cranelift count, as it does every stage before them.

## Settled

- **`curios-binaryen` holds Binaryen to the rule.** It is Binaryen's authority ([One crate is the authority for one external concern](../../design/one-crate-is-the-authority-for-one-external-concern.md)): a pass the growth suite refuses is skipped by name with its reason stated, and reported upstream with its reduced module. Constant propagation is the continuation optimizer's in any case ([`curios-binaryen`'s README](../../../curios-binaryen/README.md)).
- **Cranelift's cost is attributed before it is taken**, and taken where `curios-cranelift` configures Cranelift or where the emitted function shapes what it is handed.

## Stages

1. **Binaryen.** Record the pinned version and settings, retake the long-function shapes over the current emitter, and attribute the remaining growth to individual passes before changing the default pipeline. Skip only a pass the current attribution still refuses, state the reason in `curios-binaryen`'s README, and prepare its reduced module and upstream report. Check: the affected rows held in `growth_slopes`, pass-local work over at least five doubling sizes beginning at the affected row's base, and `wasm-optm` sizes over `programs/` before and after. A pass no longer responsible is left enabled.
2. **Cranelift.** What it pays for a long function attributed, and taken where `curios-cranelift` configures it or the emitted function shapes it. Check: their rows held.

## Protocol

The counted slopes are `growth_slopes`' (`curios/src/tests/instrumented/growth.rs`), whose `binaryen` and `cranelift` columns are these stages. Its `HELD_SLOPE` is a regression threshold, not a proof of linearity; each fix additionally states the bound of the responsible pass or emitted shape and counts the work exposing its cause across the extended grid. Timed pass comparisons use release builds, warm-ups and interleaved baseline and candidate runs, reporting medians and spans with the commit, dependency version and settings. A Binaryen reading replays the bytes `curios` hands `BinaryenModuleReadWithFeatures`, dumped by a debugger at that call, through `BinaryenModuleOptimize` or `BinaryenModuleRunPasses`, linked against the cached `libbinaryen.a`, with `BinaryenAddPassToSkip` for a skip.

## Rejected

- **A pass list of Curios's own in place of Binaryen's default**: the list would have to follow every version's, where a skip names only what the suite refuses.

## Retirement

- `curios-binaryen`'s README states its skips, and `curios-cranelift`'s what it configures for a long function.
- What cites this specification is updated, and it is deleted.
