# CLAUDE.md

Operational guide for working on Curios. Read this before investigating or changing the repository.

## Working with the user

- **Mutation requires explicit authorization, narrowly scoped.** Investigation, explanation, and proposals are read-only activities. Do not edit, format, generate, delete, stage, commit, or otherwise mutate the repository unless the user has authorized that specific change. Authorization for one change covers neither adjacent refactors, cleanup, dependency upgrades, and unrelated fixes, nor a materially broader scope that would make implementation easier. When the boundary of an authorization is ambiguous, stop and ask.
- **Report, don't fix, problems you were not asked to solve.** A discovered bug, inefficiency, inconsistency, or cleanup opportunity is a finding to surface; the user decides whether it becomes work.
- **Run every decision through the user.** Where there is more than one reasonable design, present the alternatives and their trade-offs, recommend one plainly, and wait for the user to choose.
- **Preserve existing work.** Assume every uncommitted change belongs to the user. Do not overwrite, revert, reformat, stage, or incorporate it unless the user explicitly includes it in the task.
- **Do not commit or push unless explicitly asked.** When asked to commit, include only the authorized changes and follow the requested attribution and message constraints.
- **Do not spiral into self-doubt.** State findings, uncertainties, and recommendations plainly. Do not hedge, repeatedly reopen settled decisions, or defer to the user without first presenting the available evidence.
- **Do not spawn subagents or delegate work unless explicitly asked.** Investigate and implement directly unless the user requests parallel or delegated work.

## Before starting

- Read [ROADMAP.md](documentation/ROADMAP.md) before proposing or implementing a capability. Confirm whether the work is new, pending, or already represented differently.
- Inspect the worktree before editing. Preserve unrelated changes and avoid files outside the authorized scope.
- Identify the subsystem that owns the behavior, then read its crate-level and relevant module-level `//!` documentation before changing Rust.
- Read [SYNTAX.md](documentation/SYNTAX.md) in full immediately before writing or modifying Curios source. Do not rely on remembered syntax.
- Trace public contracts to their consumers before changing them. Pipeline stages, the host ABI, the runtime, the JavaScript harness, and embedded standard-library modules often impose downstream obligations.
- Prefer focused investigation first. Search with `rg` or `rg --files`, read the narrowest authoritative source, and widen only when the evidence requires it.

## System at a glance

Curios is a functional, dependently typed language implemented in Rust 2024. It compiles `.crs` source through several intermediate representations to WebAssembly and executes precompiled modules with Wasmtime.

```text
.crs source
  → curios-text       parse surface syntax and lower it to core
  → curios-elab       elaborate, typecheck, normalize, and erase types
  → curios-ersd       optimize erased terms and lower them to continuations
  → curios-cont       optimize continuation IR and emit WebAssembly
  → curios-wasm       model, parse, and encode WebAssembly modules
  → curios-unit       name what one unit hands its successors, below the kernel
  → curios-pipeline   drive the compiler pipeline, and supply the fixed prelude

Native compiler path:
  → curios-package    read the manifest, resolve dependencies, and file units in the store
  → curios-binaryen   optimize emitted WebAssembly
  → curios            precompile with Wasmtime, run, or bundle
  → curios-runtime         deserialize and execute the precompiled module

Browser path:
  → curios-web         expose curios-pipeline through wasm-bindgen
```

Data flows downward through the diagram, while Rust dependencies between compiler stages point in the opposite direction: lowering code depends on the representation it constructs. `curios-text` depends on `curios-elab`, which depends on `curios-ersd`, which depends on `curios-cont`, which depends on `curios-wasm`. Beside that chain, `curios-core` owns the term representation, `curios-analysis` the rules both checkers run over it, and `curios-cert` the kernel that only one of them does: `curios-elab` depends on `curios-core` and `curios-analysis`, `curios-cert` on both, and none of those reverse. `curios-elab` takes `curios-cert` as a *dev*-dependency only, so nothing whose build script reaches elaboration reaches the kernel through it — which is what keeps a kernel edit from re-elaborating the fixed prelude.

### Ownership map

| Area | Owner | Responsibility |
| --- | --- | --- |
| Zero-copy archiving | `curios-archive` | The workspace's only rkyv dependency: the pin, the feature set, the re-exported derives, and the `archived` attribute macro (in the `curios-archive-derive` companion, since a proc-macro crate can export nothing else) |
| Shared foundations | `curios-base` | Spans, names, entropy, parser/printer utilities, packed values, the `SyntaxRegistry` shape the `/syn`-emitting stages read, and other stage-independent intrinsics — plus, as the workspace's only `stacker` dependency, the `recurse` guard every walk over data-shaped depth runs inside, which is the one place its figures are written |
| Host/guest contract | `curios-abi` | Wire constants and self-describing foreign-function rows shared by compiler and runtime |
| Compilation unit | `curios-unit` | `Unit` — what one unit hands its successors, one opaque artifact per stage — and the `Prefix` of borrowed predecessors each stage is compiled against. No certifier dependency, for the reason `curios-prelude-archive` states for itself: a unit is produced here and judged by the driver above |
| Surface language | `curios-text` | Lexer, parser, surface AST, printer, module resolution, generated `/sys`, and lowering to core |
| Prelude image | `curios-prelude-archive` | Authored `/syn` and `/std` sources, canonical syntax names, and the compiler-build-scoped Text/Core/Ersd archive. No certifier dependency: it elaborates, it does not judge |
| Certified prelude | `curios-prelude` | The image above, plus a build script that walks it with the kernel and fails the build on a refusal. Its successful build *is* the verdict; every consumer depends on this crate, never on the image |
| Term representation | `curios-core` | `Term` and its binder discipline, the intrinsic roster and folds, universe levels, registry entries, the finished-program `Module` both checkers walk, names, and the printer |
| Shared analyses | `curios-analysis` | The `Env`/`Judge` seam and the rules both checkers run behind it: index inversion, strict positivity, size-change totality, universe satisfiability |
| Trusted certifier | `curios-cert` | The independent kernel, the whole-module walk that applies it, the erasure obligations, and level entailment |
| Type theory | `curios-elab` | Elaboration, typing, conversion, reduction, inductives, structures, concepts, zonking, and erasure |
| Erased optimization | `curios-ersd` | Post-erasure IR, compile-time evaluation and specialization, worker/wrapper transforms, and lowering to CPS |
| Continuation IR | `curios-cont` | CPS optimization and WebAssembly emission |
| WebAssembly model | `curios-wasm` | Wasm AST, parser, encoder, and binary writer |
| Compiler driver | `curios-pipeline` | `compile_entrypoint`, `Stage`, and orchestration without runtime, Binaryen, or CLI dependencies — plus, in `standard.rs`, the same fold with the fixed prelude supplied |
| Packages and projects | `curios-package` | The workspace's only TOML dependency: the `curios.toml` manifest, the governance walk, the dependency resolver, and the store. Beside the pipeline, never under it |
| Binaryen integration | `curios-binaryen` | Binaryen source build, static FFI, and Wasm optimization |
| Runtime | `curios-runtime` | Wasmtime engine, host bindings, `.cwasm` deserialization, bundle payload format, and slim launcher |
| Native product | `curios` | The native back end — Binaryen optimization, Wasmtime precompilation, in-process running — plus the CLI, the unit cache, executable bundling, and the cross-stage test corpus. Compiling itself is `curios-pipeline`'s |
| Browser product | `curios-web` | wasm-bindgen compiler exports and JavaScript execution harness |
| Profiling | `curios-profile` | The workspace's only `tracing` dependency: `profile!`/`profile_span!` span macros and the `capture` aggregate-timing subscriber, gated per-crate on a `profile` feature |

## Change routing

| If changing… | Start in… | Also inspect… |
| --- | --- | --- |
| Surface grammar, syntax tree, or printing | `curios-text/src/parse*`, `module.rs`, `print.rs` | `into_core/`, parser tests, `documentation/SYNTAX.md` |
| Surface-to-core lowering | `curios-text/src/into_core/` | Core constructors and cross-stage integration tests |
| Elaboration, typing, or conversion | `curios-elab/src/` | Text lowering, erasure, diagnostics, and integration tests |
| Kernel judgments | `curios-cert/src/` | `curios-core`'s representation, `curios-cert/src/recheck.rs`, and `documentation/DESIGN.md`'s perimeter |
| A shared analysis | `curios-analysis/src/` | Both drivers — `curios-cert`'s `Kernel` and `curios-elab`'s `Context` — plus `curios-analysis/tests/driven.rs`, where the checker-driven probes live |
| Concepts or witness resolution | `curios-elab/src/concept.rs`, `resolve.rs` | Surface declarations, standard-library witnesses, and syntax documentation |
| Type erasure | `curios-elab/src/into_ersd*` | `curios-ersd` representation and downstream tests |
| Erased optimization | `curios-ersd/src/optimize/` | `into_cont.rs`, derived analyses, deep-input and specialization tests |
| CPS optimization or Wasm emission | `curios-cont/src/` | `curios-wasm`, codegen tests, and runtime behavior |
| Wasm representation or encoding | `curios-wasm/src/` | Continuation emission and parser/round-trip tests |
| Host operations or foreign calls | `curios-abi/src/` | Core validation, Wasm imports, runtime bindings, and the JavaScript harness |
| What a unit hands its successors | `curios-unit/src/` | Every stage whose artifact `Unit` holds, `curios-pipeline`'s fold, and the store's stored-unit format |
| Pipeline orchestration | `curios-pipeline/src/compile.rs`, `stage.rs`, `standard.rs` | Native and browser callers |
| Manifests, dependency resolution, or the store | `curios-package/src/` | The CLI subcommands that wrap it, `curios-base`'s `Qualifier`/`Mount`, and `documentation/SOUNDNESS.md`'s *Cached verdicts* when the store's keys are involved |
| Runtime or bundle format | `curios-runtime/src/`, `curios/src/bundle.rs` | Slim-launcher dependency boundary and bundle integration tests |
| CLI or native compile behavior | `curios/src/` | `README.md`, public helpers, and integration tests |
| Standard or syntax library | `curios-prelude-archive/std/`, `curios-prelude-archive/syn/` | Module indices, canonical syntax registry, `SYNTAX.md`, and Curios integration tests |
| Prelude archive or replay | `curios-prelude-archive/build.rs`, `curios-prelude-archive/src/` | Text preparation, Core elaboration/erasure replay APIs, pipeline integration, and archive validation tests |
| Browser compiler or harness | `curios-web/` | Host ABI, wasm32 build, wasm-bindgen version, and CI release steps |
| Profiling instrumentation | `curios-profile/src/lib.rs` | Each consumer crate's `profile` feature fan-out, and `make curios/profile` |
| Binaryen version, build, or FFI | `curios-binaryen/` | Shared cache behavior, native compiler linkage, and optimize round-trip tests |

## Architectural invariants

- Compiler stages own their representations. A lowering belongs to the crate holding the source representation and depends on the crate holding the destination representation.
- `curios-pipeline` is the compiler boundary. It must not depend on Binaryen, Wasmtime, the runtime, or the CLI. It *may* name the fixed prelude, and does so in `standard.rs` alone: `compile_entrypoint` still takes a scope and still cannot tell which unit is `/std`, and nothing in the fold calls the layer above it. That layer exists because the native product, the browser product and this crate's own fixtures each wrote the same prelude wiring by hand — three callers agreeing is a missing function, not a policy, and the third was not a product at all.
- `curios-package` sits beside that boundary, never under it. The driver folds its stages over whatever scope it is handed; *deciding* that scope is a product's job, so `curios-pipeline` must not depend on `curios-package` and `curios-web` must not touch it. It is also the workspace's only TOML dependency.
- `curios-unit` sits below the kernel and must stay there: `cargo tree -p curios-unit --edges normal` must not contain `curios-cert`. A unit is *produced* by stages that do not judge and *judged* by the driver above, because `curios-prelude-archive`'s build script constructs a `Unit`, and a build script that reaches the certifier re-elaborates the whole standard library on every kernel edit.
- `curios-runtime` is the runtime-only boundary. It must not depend on `curios`, Binaryen, or Wasmtime's Cranelift compiler.
- `curios` is the only workspace crate that combines Binaryen with Cranelift-enabled Wasmtime.
- The workspace uses crate boundaries, not Cargo features, to separate the compiler, runtime, and browser products.
- `curios` and `curios-runtime` use the same workspace-pinned Wasmtime version so compiler-produced `.cwasm` modules match the runtime that deserializes them.
- `curios-abi` is the source of truth for the host/guest wire contract. A host operation is incomplete until its ABI row, compiler use, native runtime implementation, and applicable JavaScript implementation agree.
- `/std` and `/syn` are owned by `curios-prelude-archive` and compiled into an rkyv image in that crate's `OUT_DIR`. Every source module must be registered in its Curios index; the build script discovers every `.crs` input, fingerprints it, and emits the matching Cargo rebuild directives.
- Production compilation has no fixed-prelude source fallback or cache-miss branch. Archive construction or restoration failure is a compiler invariant and fails loudly. The image is scoped to one compiler build and is not a stable interchange format.
- `/syn` ownership — which names it holds, and why — is `curios-prelude-archive/README.md`'s decision to state. The registry contract belongs to `curios-base`, below both stages that read it, and the erased runtime carriers for compiler-emitted literals remain `Nat` and packed `Bytes`. No crate below `curios-prelude-archive` may spell a `/syn` name: `curios-base` states slots, `curios-prelude-archive/src/syntax.rs` states spellings, and the prelude build checks every slot against the sources.
- Binaryen is built from a verified source release. Its expensive C++ build is shared through the locked, target-specific cache under `target/binaryen`, not a Cargo fingerprint-specific `OUT_DIR`.
- Recursive lowering and packed-value interpretation must work on the default test-thread stack. Do not use `RUST_MIN_STACK` to hide a regression.
- Generated `.wasm` files and other build products are not source. Do not commit them. `Cargo.lock` is source and must remain synchronized with dependency changes.

## Writing Rust

- Re-read the pipeline and ownership map above, then open the `//!` documentation for every stage being changed.
- Follow the established module layout: no `mod.rs` anywhere; `foo.rs` declares its `foo/` submodules and re-exports them with `mod x; pub use x::*;` (the default — a narrower re-export visibility is a deliberate deviation, not a default), and crate roots do the same, so every crate stays a flat namespace. One module keeps a documented, deliberate namespace instead: `curios-base`'s `pub mod monads` (its `parser`/`printer` submodules both name their unit `pure`, so flattening would make it ambiguous). The kernel's judgments — which once justified a second exception as `curios-core`'s `pub mod kernel` — now live flattened on `curios-cert`'s root, the crate name doing that disambiguation.
- Import names everywhere except at the four lowering seams (`curios-text`→`curios-elab`, `curios-elab`→`curios-ersd`, `curios-ersd`→`curios-cont`, `curios-cont`→`curios-wasm`), where the downstream crate's names stay qualified by exactly one level — the crate name only, e.g. `curios_ersd::Foo`, never a module path. `curios_base` and `curios_abi` are never qualified anywhere, even in seam files. A name arriving from two or more crates in the same file stays qualified rather than aliased. Traits are imported by name like any other item, never anonymously (`use Bound;`, not `use Bound as _;`).
- Place unit tests beside their implementation: `foo.rs` declares `#[cfg(test)] mod tests;` and the tests live in `foo/tests.rs`. A small test module may stay inline as `#[cfg(test)] mod tests { … }` in the file it covers. Put programs that cross compiler stages in `curios/src/tests/`; codegen tests live in `curios/src/tests/codegen/`.
- Name per-carrier helpers, fields, and emitted functions type-first, operation-last (`bin_force`, `list_slice`), never operation-first (`force_bin`).
- When changing one stage, check the next representation or consumer explicitly. Parsing changes usually affect printing and lowering; core changes usually affect erasure; IR changes usually affect the next lowering and its tests.
- Use `//!` for module purpose and invariants, and `///` for public API contracts. Do not duplicate detailed subsystem documentation in this file. Write every comment — `//`, `///`, `//!` — as one line per paragraph or list item, matching the Markdown rule below; do not hardwrap. Add a comment only where the WHY is non-obvious — an invariant, a rejected alternative, a measured tradeoff — and never write one that only restates what the code already says.
- Use stock rustfmt and Clippy settings. There is no repository-specific `rustfmt.toml` or `clippy.toml`.

## Writing Curios

- Read [SYNTAX.md](documentation/SYNTAX.md) in full before editing any `.crs` file. It is the normative surface-language reference; `curios-text/src/parse.rs` implements the contract.
- The surface grammar's syntax forms are closed: a new type never gets its own operator or keyword. It opts into an existing form (`+`, `==`, postfix `!`, …) by writing a `satisfy` witness against the form's `/syn` concept. See "Syntax forms are closed, semantics extend by witness" in `documentation/DESIGN.md` before proposing hardcoded syntax for a type.
- Use `curios-prelude-archive/std/` as the reference for idiomatic code.
- Register a new `curios-prelude-archive/std/Foo.crs` module in `curios-prelude-archive/std.crs`. Apply the corresponding rule to `curios-prelude-archive/syn/` and `curios-prelude-archive/syn.crs`; update `curios-prelude-archive/src/syntax.rs` only when Rust directly emits the new `/syn` name — from lowering, or from a type-directed feature in `curios-elab`.
- Remember that names use `/` qualification, `{}` is the unit type, `()` is the unit value, and visibility of a nominal name is independent from visibility of its representation. Consult `SYNTAX.md` for the full rules rather than extending this reminder list.
- Run Curios programs through the native CLI: `cargo run --package curios -- run <file.crs>`.

## Build and validation

The native compiler embeds the slim `curios-runtime` launcher with `include_bytes!`. Build that launcher in isolation before building `curios`:

```sh
make curios/runtime
cargo build --package curios
cargo run --package curios -- <args>
```

`make curios/runtime` is load-bearing: it builds `curios-runtime` without workspace feature unification, keeping Cranelift and Binaryen out, and copies the resulting launcher to the target-scoped `target/curios/<target>/runtime` path for embedding. A `curios-runtime` binary produced by a workspace build is not evidence that the isolated launcher remains slim.

### While iterating

- Run the smallest check or test that exercises the changed behavior.
- Prefer stage-local crate checks while developing. Do not repeatedly build the whole workspace merely to obtain feedback.
- The full workspace test suite can take more than five minutes. Run it deliberately in the background, redirect output to a file, and inspect the file after completion instead of piping or continuously scrolling it.

```sh
cargo test --workspace --all-targets --all-features > /tmp/curios-tests.txt 2>&1
```

### What a workspace check already exercises

`cargo clippy --workspace` (or any workspace build) is not only a compile. It builds `curios-prelude-archive`, whose build script elaborates every `/std` and `/syn` module, erases them through `erase_unit` — which runs `curios-ersd`'s verifier over the result — and then `curios-prelude`'s script certifies the whole module with the kernel, which decides universe satisfiability for every context before assuming it. So a change on the Text, Core, Ersd or certification path is exercised over the entire standard library by a step already in the gate, and needs only its own crate's tests beside it.

A change *below* Ersd is not. The archive stops there, so nothing in a workspace check reaches `curios-cont` or `curios-wasm`, and their detector is the cross-stage corpus in `curios`. That line, rather than the size of a diff, is what decides whether a change can be verified cheaply.

### Before handing off code changes

Run this gate, in order. All commands must pass, and Clippy warnings are errors in CI.

```sh
make curios/runtime
cargo fmt --all -- --check
cargo clippy --workspace --all-targets --all-features -- -Dwarnings
cargo test --workspace --all-targets --all-features
```

This is deliberately *not* CI's list of jobs. CI runs `check`, `clippy` and `test` as three separate jobs that never share a target directory, so it pays nothing for the overlap between them; a local sequential run pays for all of it. Two consequences.

`cargo check` is dropped because `cargo clippy` is the same compilation with more lints, and `cargo test` compiles for real — so `check` establishes nothing either of them misses, while costing a full pass of its own. What it buys back is only that a type error surfaces a little earlier. CI keeps its own `check` job, where it is free.

The Clippy denial is passed *after* `--` rather than as `RUSTFLAGS` for the same class of reason. `RUSTFLAGS` is a global fingerprint input, so setting it for one step forks every unit in the graph — including `curios-prelude`'s build script, which then re-elaborates and re-certifies the whole fixed prelude for that step alone. With the denial after the separator, the `clippy` step rebuilds no prelude at all, reusing what ran before it.

Do not quote a whole-gate rebuild total. One was stated here twice and was wrong twice, because a run measured with one test invocation was compared against a run measured with another — the suite step's own count depends on `--all-targets`, not on either change above. Measure a step, name the step.

Expect the gate to be latency-bound rather than throughput-bound: the crate graph is a deep near-linear chain, so most cores sit idle. Wall clock is the critical path, and more parallelism in the build does not shorten it.

For the same reason, keep the feature set constant while iterating. `--all-features` enables `profile`, and a plain `cargo build --package curios` does not, so alternating between them maintains two archives that evict each other. Pick one for a work session.

Documentation-only changes do not require rebuilding the compiler unless they alter documented commands or make claims that need executable verification. Check their diff, links, filenames, and hardwrapping directly.

### Additional gates

- Changes to `curios-web` or its dependencies must also pass `make curios/web` with the exactly version-matched `wasm-bindgen-cli` installed.
- Changes to `curios-binaryen/build.rs` must verify an empty-cache build and a cache hit from a different Cargo mode or build-script fingerprint. Bump `BUILD_SCHEMA` when the CMake configuration or installed-library contract changes.
- Changes to runtime dependencies must rebuild `curios-runtime` in isolation through `make curios/runtime` and confirm that neither Cranelift nor Binaryen entered its dependency graph.
- Changes to the bundle format must run the ignored end-to-end test in `curios/tests/bundle.rs` explicitly.

### Profiling

Profile the compiler through the built-in `tracing` mechanism, not an external sampler. `perf`-based tools (`samply`, `perf`) may be unavailable under a restricted `perf_event_paranoid`, and the built-in path measures the exact stages under a deterministic subscriber.

Run one compilation under the profiler with the `profile` recipe:

```sh
make curios/profile CURIOS_PROFILE_SOURCE=programs/hello_world.crs
```

It builds the `curios` binary with `--features profile` — which is the only build in which the `profile` subcommand exists — and prints per-span aggregate timings sorted by total time descending. The instrumentation mechanics — `profile!` and `profile_span!`, the per-crate `profile` feature fan-out, the `capture` collector, and the temporary-instrumentation norm — are documented in the `curios-profile` crate, the one place in the workspace that names `tracing`.

**Keep a figure beside the probe that reproduces it, or do not record it.** A number in prose with no method decays quietly and is then designed against — which has happened here twice, once by an order of magnitude and once by claiming a saving larger than the whole operation costs. Prose cannot check a figure, so prose is the wrong place for one. `curios-prelude-archive`'s `stored_prelude_measurements` is the pattern: an ignored test carrying the command, the date, the profile, and what it last printed, so a number cannot drift from the thing that would check it.

This generalizes "measure a step, name the step" above — that rule says what to compare, this one says where the comparison belongs. Documentation states what a measurement *decided*; the measurement itself lives with the code.

## Documentation ownership

Document each fact at the narrowest authoritative level and link to it elsewhere. Do not maintain parallel explanations that can drift.

| Location | Owns |
| --- | --- |
| `README.md` | Public introduction: what Curios is, the happy path to running one, and where to go next. Reference detail belongs in `documentation/USAGE.md`, not here |
| `documentation/USAGE.md` | Complete command-line and package reference — every subcommand, exit codes, dependencies, umbrellas, and the global flags |
| `CLAUDE.md` | Contributor behavior, ownership boundaries, durable invariants, and validation |
| `documentation/SYNTAX.md` | Complete Curios surface-language reference |
| `documentation/ROADMAP.md` | Implemented capabilities and pending specifications |
| `documentation/DESIGN.md` | Cross-cutting design decisions — those spanning the language or several crates — their rationale, and rejected alternatives |
| `documentation/SOUNDNESS.md` | The soundness perimeter: every rule that can admit a term, its grade, and the evidence behind it |
| Crate `README.md` files | The crate's mission and its crate-scoped design decisions, rationale, and rejected alternatives |
| Crate and module rustdoc | Local architecture, algorithms, invariants, and public APIs |
| `Cargo.toml` descriptions | One-line crate purposes for Cargo tooling |
| `benchmarks/README.md` | Benchmark harness mechanics |

Do not hardwrap Markdown prose. Write one source line per paragraph or list item and let the renderer soft-wrap it. Fenced code blocks and tables retain their deliberate line structure.

## Repository conventions

- Use imperative, capitalized, descriptive commit subjects. Do not add co-authors unless the user requests them.
- Do not mix vendored changes, generated files, or unrelated formatting into a feature commit.
- Use non-interactive Git commands and never discard work with destructive reset or checkout operations unless the user explicitly requests that exact action.
- Keep source files focused. Prefer extending an existing ownership boundary over creating a parallel abstraction for the same responsibility.

## Known build constraints

- `curios-binaryen` downloads, verifies, and builds a pinned Binaryen source release with CMake. The first cache population takes minutes and requires a C++ toolchain; subsequent Cargo modes must reuse `target/binaryen`. A full `cargo clean` removes the cache.
- `target/` is pruned by hand and never with `cargo clean`, which takes `target/binaryen` with it. `target/debug/incremental` is pure rustc cache and the one directory safe to delete outright, provided no build is running — measured at 21G of `target/debug`'s 39G, since Cargo enables incremental compilation for `dev` and `test` while this workspace's edit pattern rebuilds every downstream crate in full anyway; `CARGO_INCREMENTAL=0` suppresses it per invocation. Everything else there holds real artifacts — above all `target/binaryen` and the target-triple-scoped `target/<triple>/release/curios-runtime` that `make curios/runtime` copies the embedded launcher out of.
- `curios-web` deliberately uses plain `cargo build` plus `wasm-bindgen-cli`; do not introduce `wasm-pack` or `wasm-opt` without an explicit design decision. Binaryen optimization belongs only to the native `curios` product.
- The wasm32 build requires the installed `wasm-bindgen-cli` version to match the `wasm-bindgen` crate version in `Cargo.lock` exactly.
