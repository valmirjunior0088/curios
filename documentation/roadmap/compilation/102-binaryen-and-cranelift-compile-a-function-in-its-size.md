# Binaryen and Cranelift compile a function in its size

[Every stage but reduction costs the graph it is handed](../../design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md) states that "a compilation ends — accepted, or refused for its budget — in time proportional to what was written plus what the budget counted". Binaryen's optimizer and Cranelift's code generation are stages of a compile, and each pays more than that for a long function. Every stage before them is held to the rule by `growth_slopes` (`curios/src/tests/instrumented/growth.rs`), and the emitter hands them each function with its locals allocated by liveness ([`curios-emit`'s decision](../../../curios-emit/README.md#locals-are-allocated-by-liveness)).

## What is wrong

### Binaryen pays superlinearly for a long function

A Binaryen `version_130` replay of the module `curios` hands `BinaryenModuleReadWithFeatures`, at `curios-binaryen`'s settings, taken at `28bcb9aba` and timed, one reading each:

- **`precompute-propagate`.** After `simplify-locals` has sunk a dependent chain of n `+` into one expression, it takes 10.0 s at 150 and 62.9 s at 300, while either pass alone takes milliseconds. The whole default pipeline with that one pass skipped (`BinaryenAddPassToSkip`) takes 2.25 s and 4.74 s, on a module whose baseline is 0.34 s.
- **`simplify-locals-nostructure`.** On a 2 000-element list built from a run-time value, it takes 6.8 s of a 10 s pipeline the skip above does not shorten.

[Precompute-propagate's introduction](https://github.com/WebAssembly/binaryen/pull/1179) already kept it to higher levels for its compile time. The [LLVM WebAssembly backend](https://android.googlesource.com/platform/external/opencl/llvm-project/+/cd46354dbd10) counts excess locals among what makes it "easy to hit engine limitations of 50K locals" and code "slower to compile to native code".

### The counted slopes reach both

`growth_slopes`' reading over the compiler of `be93acf08` holds eleven growths linear in every stage before Binaryen and not after it. Past three: a match's arms (11.95 instructions in `binaryen`, which read 5.12 and 7.75 on other runs of one compiler, and 3.21 bytes in `cranelift`), a list built from a run-time value (3.82 in `binaryen`), nested list literals (3.72), a chain of operators nested to the right (3.56 instructions and 3.60 bytes in `cranelift`), nested matches (3.45 in `binaryen`), and n functions each calling the last and a left-leaning chain of operators (3.03 and 3.05 in `binaryen`). A `let` chain over `+` and over `Nat/add`, a telescope's parameters summed and a structure's fields read under three at the grid's sizes, and are held before Binaryen alone until their stages are taken up here.

## Goal

Doubling a program along any axis an author can grow it at most doubles what Binaryen and Cranelift count, as it does every stage before them.

## Settled

- **`curios-binaryen` holds Binaryen to the rule.** It is Binaryen's authority ([One crate is the authority for one external concern](../../design/one-crate-is-the-authority-for-one-external-concern.md)): a pass the growth suite refuses is skipped by name with its reason stated, and reported upstream with its reduced module. Constant propagation is the continuation optimizer's in any case ([`curios-binaryen`'s README](../../../curios-binaryen/README.md)).
- **Cranelift's cost is attributed before it is taken**, and taken where `curios-cranelift` configures Cranelift or where the emitted function shapes what it is handed.

## Stages

1. **Binaryen.** The long-function shapes retaken over the emitter's locals by liveness; each pass the growth suite still refuses skipped, stated in `curios-binaryen`'s README, and its reduced module and report prepared. Check: their rows held in `growth_slopes`; `wasm-optm` sizes over `programs/` before and after.
2. **Cranelift.** What it pays for a long function attributed, and taken where `curios-cranelift` configures it or the emitted function shapes it. Check: their rows held.

## Protocol

The counted slopes are `growth_slopes`' (`curios/src/tests/instrumented/growth.rs`), whose `binaryen` and `cranelift` columns are these stages. A Binaryen reading replays the bytes `curios` hands `BinaryenModuleReadWithFeatures`, dumped by a debugger at that call, through `BinaryenModuleOptimize` or `BinaryenModuleRunPasses`, linked against the cached `libbinaryen.a`, with `BinaryenAddPassToSkip` for a skip.

## Rejected

- **A pass list of Curios's own in place of Binaryen's default**: the list would have to follow every version's, where a skip names only what the suite refuses.

## Retirement

- `curios-binaryen`'s README states its skips, and `curios-cranelift`'s what it configures for a long function.
- What cites this specification is updated, and it is deleted.
