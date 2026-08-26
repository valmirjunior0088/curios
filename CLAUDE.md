# CLAUDE.md

Operational guide for working on Curios. Read it before investigating or changing the repository.

## Working with the user

- **Mutation requires explicit authorization, narrowly scoped.** Investigation, explanation and proposals are read-only. Do not edit, format, generate, delete, stage, commit or otherwise mutate the repository unless the user has authorized that specific change. Authorization for one change covers neither adjacent refactors, cleanup, dependency upgrades or unrelated fixes, nor a broader scope that would make the change easier. When the boundary of an authorization is ambiguous, stop and ask. Once a change is authorized, keeping formatters and linters passing on the touched code is in scope.
- **Report, don't fix, problems you were not asked to solve.** A discovered bug, inefficiency, inconsistency or cleanup opportunity is a finding to surface; the user decides whether it becomes work.
- **Run every decision through the user.** Where more than one design is reasonable, present the alternatives and their trade-offs, recommend one plainly, and wait for the choice.
- **Preserve existing work.** Every uncommitted change belongs to the user. Do not overwrite, revert, reformat, stage or incorporate it unless the user includes it in the task.
- **Commit only when asked, as a single line.** A commit message is one imperative, capitalized, descriptive subject — no body, no bullets, no trailer, and never a co-author. Include only the authorized changes; commit to `main` directly.
- **State findings plainly.** Name uncertainties once, with the evidence, and let the user resolve them. Do not reopen settled decisions or hedge in loops.
- **Do not spawn subagents or delegate work unless explicitly asked.**

## Interacting with a Curios codebase

The compiler is the interface. A claim about what a Curios program means is a hypothesis until the compiler has answered it, and the two tools below are how it is asked.

**Test a theory on standard input.** Every command that takes a program takes `-` for one on standard input, so a probe is a heredoc, never a file left in the tree:

```sh
cargo run --package curios -- run - <<'CRS'
/std/print("hello\n")
CRS
```

**Derive facts with `wonder`.** `curios wonder <query> [target]` answers a question from the compilation that would build the target and executes nothing. The answer goes to stdout; exit 0 means it was answered, including when the answer is a list of errors.

- `wonder diagnostics -` is the loop: every error and goal, rendered as `run` reports it. A snapshot stops at the first failure, so iterate one error at a time.
- `?` is the type oracle, addressed by text rather than by coordinate. `let y: ? = e` reports `? = ` the type of `e`; a bare `?` reports the local scope, the expected type, the obligations holding it up and candidate fits. Several `?` in one program report in one compile. This is how the type of a binder, an expression or a hole is learned — never by guessing from the surrounding code.
- `wonder stage <stage> -` reprints the program at one pipeline rung (`text`, `core`, `core-elab`, `ersd`, `ersd-optm`, `cont`, `cont-optm`, `wasm`, `wasm-optm`): the way to see what a lowering or an optimization actually produced.
- A file target is placed in the unit that declares it: a module under a package's directory is analysed as that package's library, an executable's entry as that executable, anything else standalone.

**Read the sources for the rest.** Every fact `wonder` does not answer is on disk: `documentation/syntax.md` is the normative surface reference, `curios-prelude-archive/std/` is the idiom reference and where a standard-library signature is read, and a dependency's sources are materialized under `.curios/src/`. Search with `rg`, read the narrowest authoritative source, and widen only when the evidence requires it. Do not reconstruct from memory what a file states.

**The binary is the tree's.** `cargo run --package curios --` runs the compiler this checkout builds, which is the one a change is being made to; it needs `cargo xtask runtime` once per checkout for the launcher it embeds. An installed `curios` on `PATH` is a different build and answers for it, not for the tree.

## Before changing anything

- Read [roadmap.md](documentation/roadmap.md) before proposing or implementing a capability, and confirm whether the work is new, pending, or already represented differently.
- Inspect the worktree. Preserve unrelated changes and stay inside the authorized files.
- Identify the subsystem that owns the behavior and read its crate-level and module-level `//!` documentation before changing Rust.
- Read [syntax.md](documentation/syntax.md) in full immediately before writing or modifying Curios source.
- Trace a public contract to its consumers before changing it. Pipeline stages, the host ABI, the runtime, the JavaScript harness and the embedded standard library impose downstream obligations.

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
  → curios            precompile with Wasmtime, run, bundle, and answer questions
  → curios-runtime    deserialize and execute the precompiled module

Browser path:
  → curios-js         expose curios-pipeline through wasm-bindgen
```

Data flows downward; Rust dependencies between stages point upward, because a lowering depends on the representation it constructs: `curios-text` depends on `curios-elab`, which depends on `curios-ersd`, which depends on `curios-cont`, which depends on `curios-wasm`. Beside that chain, `curios-core` owns the term representation, `curios-analysis` the rules both checkers run over it, and `curios-cert` the kernel that only one of them does. `curios-elab` depends on `curios-core` and `curios-analysis`, `curios-cert` on both, and `curios-elab` takes `curios-cert` as a *dev*-dependency only, so nothing whose build script reaches elaboration reaches the kernel through it.

### Ownership map

| Area | Owner | Responsibility |
| --- | --- | --- |
| Zero-copy archiving | `curios-archive` | The workspace's only rkyv dependency and the only place it is spelled: the pin, the feature set, the `archived` attribute macro (in the `curios-archive-derive` companion), the `Proxy`/`Via` adapter, and the serialization entry points. Nothing above it names an rkyv item |
| Shared foundations | `curios-utilities` | Spans, names, entropy, the typed identity-addressed `Arena`, packed values, the `SyntaxRegistry` shape the `/syn`-emitting stages read, and — as the workspace's only `stacker` dependency — the `recurse` guard every walk over data-shaped depth runs inside |
| Parser combinators | `curios-parse` | The `FnOnce` parser DSL: ordered choice under progress-based commitment, packrat memoization, caret-snippet errors. Behind both the `.crs` grammar and the WAT parser |
| Printer combinators | `curios-print` | The Wadler document algebra the IR crates write their `Display` impls in. Depends on nothing, so every IR crate can use it without depending on each other |
| Numeric tower | `curios-num` | The workspace's only `num-bigint`/`num-traits` dependency: the sealed unbounded `Natural` and `Integer`, the bitwise-identity `Floating`, and the `scalar` semantics of the erased carriers every constant folder shares. Adding an operation here is adding to the trusted base |
| Host/guest contract | `curios-abi` | Wire constants and self-describing foreign-function rows shared by compiler and runtime |
| Compilation unit | `curios-unit` | `Unit` — what one unit hands its successors, one opaque artifact per stage — and the `Prefix` of borrowed predecessors each stage is compiled against. No certifier dependency |
| Surface language | `curios-text` | Lexer, parser, surface AST, printer, module resolution, generated `/sys`, and lowering to core |
| Prelude image | `curios-prelude-archive` | Authored `/syn` and `/std` sources, canonical syntax names, and the compiler-build-scoped Text/Core/Ersd archive. It elaborates; it does not judge |
| Certified prelude | `curios-prelude` | The image above plus a build script that walks it with the kernel and fails the build on a refusal. Every consumer depends on this crate, never on the image |
| Term representation | `curios-core` | `Term` and its binder discipline, the intrinsic roster and folds, universe levels, registry entries, the finished-program `Module` both checkers walk, names, and the printer |
| Shared analyses | `curios-analysis` | The `Env`/`Judge` seam and the rules both checkers run behind it: index inversion, strict positivity, size-change totality, universe satisfiability |
| Trusted certifier | `curios-cert` | The independent kernel, the whole-module walk that applies it, the erasure obligations, and level entailment |
| Type theory | `curios-elab` | Elaboration, typing, conversion, reduction, inductives, structures, concepts, zonking, and erasure |
| Erased optimization | `curios-ersd` | Post-erasure IR, compile-time evaluation and specialization, worker/wrapper transforms, and lowering to CPS |
| Continuation IR | `curios-cont` | CPS optimization and WebAssembly emission |
| WebAssembly model | `curios-wasm` | Wasm AST, parser, encoder, and binary writer |
| Compiler driver | `curios-pipeline` | `compile_entrypoint`, `Stage`, and orchestration without runtime, Binaryen or CLI dependencies — plus, in `standard.rs`, the same fold with the fixed prelude supplied |
| Packages and projects | `curios-package` | The workspace's only TOML dependency: the `curios.toml` manifest, the governance walk, the dependency resolver, and the store. Beside the pipeline, never under it |
| Binaryen integration | `curios-binaryen` | Binaryen source build, static FFI, and Wasm optimization |
| Runtime | `curios-runtime` | The workspace's only `wasmtime` dependency — the pin, the feature set, and the `cranelift` opt-in — plus the engine, host bindings, `validate`/`precompile`, `.cwasm` deserialization, the bundle payload format, and the slim launcher |
| Native product | `curios` | The native back end — Binaryen optimization, Wasmtime precompilation, in-process running — plus the CLI, the unit cache, executable bundling, the cross-stage test corpus, and `wonder` in `src/wonder/`: the engine that answers a question from the compilation that would build the program, with its two transports, the one-shot `ask` and the language `server` (the workspace's only `lsp-server`/`lsp-types` rows) |
| Browser product | `curios-js` | wasm-bindgen compiler exports and JavaScript execution harness |
| Profiling | `curios-profile` | The workspace's only `tracing` dependency: `profile!`/`profile_span!` and the `capture` aggregate-timing subscriber, gated per crate on a `profile` feature |
| Build recipes | `xtask` | The workspace recipes as `cargo xtask`: the isolated launcher, the compiler, the browser bundle, a profile run, the benchmarks, and a bridge onto each editor tree under `editors/`. Reached only through the alias in `.cargo/config.toml`, and a dependency of nothing |

### Change routing

| If changing… | Start in… | Also inspect… |
| --- | --- | --- |
| Surface grammar, syntax tree, or printing | `curios-text/src/parse*`, `module.rs`, `print.rs` | `into_core/`, parser tests, `documentation/syntax.md` |
| Surface-to-core lowering | `curios-text/src/into_core/` | Core constructors and cross-stage integration tests |
| Elaboration, typing, or conversion | `curios-elab/src/` | Text lowering, erasure, diagnostics, and integration tests |
| Kernel judgments | `curios-cert/src/` | `curios-core`'s representation, `curios-cert/src/recheck.rs`, and `documentation/design/language/the-soundness-perimeter.md` |
| A shared analysis | `curios-analysis/src/` | Both drivers — `curios-cert`'s `Kernel` and `curios-elab`'s `Context` — and `curios-analysis/tests/driven.rs` |
| A numeric carrier or its arithmetic | `curios-num/src/` | Every constant folder sharing `scalar` (`curios-core`, `curios-ersd`, `curios-cont`), and `documentation/design/toolchain/numeric-carriers-narrow-by-refusing-never-by-changing-a-value.md` |
| Concepts or witness resolution | `curios-elab/src/concept.rs`, `resolve.rs` | Surface declarations, standard-library witnesses, and syntax documentation |
| Type erasure | `curios-elab/src/into_ersd*` | `curios-ersd` representation and downstream tests |
| Erased optimization | `curios-ersd/src/optimize/` | `into_cont.rs`, derived analyses, deep-input and specialization tests |
| CPS optimization or Wasm emission | `curios-cont/src/` | `curios-wasm`, codegen tests, and runtime behavior |
| Wasm representation or encoding | `curios-wasm/src/` | Continuation emission and parser/round-trip tests |
| Host operations or foreign calls | `curios-abi/src/` | Core validation, Wasm imports, runtime bindings, and the JavaScript harness |
| What a unit hands its successors | `curios-unit/src/` | Every stage whose artifact `Unit` holds, `curios-pipeline`'s fold, and the store's stored-unit format |
| Pipeline orchestration | `curios-pipeline/src/compile.rs`, `stage.rs`, `standard.rs` | Native and browser callers |
| A `wonder` query, a record, or what a diagnostic carries | `curios/src/wonder/` | `curios-utilities`'s `Report` and every stage's `report`/`reports_with_hints`, `curios-pipeline`'s `CompileError` and `check_with_units`, the transports `curios/src/wonder/ask.rs` and `server.rs`, `curios-package`'s `Membership`, and `documentation/roadmap/wonder-spec.md` |
| Manifests, dependency resolution, or the store | `curios-package/src/` | The CLI subcommands that wrap it, `curios-utilities`'s `Qualifier`/`Mount`, and `documentation/soundness/admission-without-judgment/cached-verdicts.md` when the store's keys are involved |
| Runtime or bundle format | `curios-runtime/src/`, `curios/src/bundle.rs` | Slim-launcher dependency boundary and bundle integration tests |
| CLI or native compile behavior | `curios/src/` | `README.md`, `documentation/usage.md`, public helpers, and integration tests |
| Standard or syntax library | `curios-prelude-archive/std/`, `curios-prelude-archive/syn/` | Module indices, canonical syntax registry, `syntax.md`, and Curios integration tests |
| Prelude archive or replay | `curios-prelude-archive/build.rs`, `curios-prelude-archive/src/` | Text preparation, Core elaboration/erasure replay APIs, pipeline integration, and archive validation tests |
| Browser compiler or harness | `curios-js/` | Host ABI, wasm32 build, `xtask`'s `js` recipe, and CI release steps |
| Profiling instrumentation | `curios-profile/src/lib.rs` | Each consumer crate's `profile` feature fan-out, and `cargo xtask profile` |
| Binaryen version, build, or FFI | `curios-binaryen/` | Shared cache behavior, native compiler linkage, and optimize round-trip tests |
| A build recipe | `xtask/src/main.rs` | `curios/build.rs`, which expects what `runtime` files; the CI workflows that call the recipe; `README.md`'s build steps |

## Architectural invariants

- Compiler stages own their representations. A lowering belongs to the crate holding the source representation and depends on the crate holding the destination representation.
- `curios-pipeline` is the compiler boundary. It must not depend on Binaryen, Wasmtime, the runtime or the CLI. It may name the fixed prelude, in `standard.rs` alone; `compile_entrypoint` takes a scope and cannot tell which unit is `/std`.
- `curios-package` sits beside that boundary, never under it: the driver folds its stages over whatever scope it is handed, and deciding that scope is a product's job. `curios-pipeline` must not depend on `curios-package`, and `curios-js` must not touch it.
- `curios-unit` sits below the kernel: `cargo tree -p curios-unit --edges normal` must not contain `curios-cert`. A unit is produced by stages that do not judge and judged by the driver above, because `curios-prelude-archive`'s build script constructs a `Unit`, and a build script that reached the certifier would re-elaborate the whole standard library on every kernel edit.
- `curios-runtime` is the runtime-only boundary in its default feature set, the set `cargo xtask runtime`'s isolated build uses. It must not depend on `curios` or Binaryen, and must not reach Cranelift by default: its `cranelift` feature exists for `curios` and never enters `default`. `curios/src/bundle.rs` enforces this on the launcher image that ships, refusing a backend marker or a size over the ceiling.
- `curios` is the only crate that combines Binaryen with Cranelift-enabled Wasmtime. It enables the latter through `curios-runtime`'s feature, names no wasmtime type, and reaches the runtime through `curios_runtime::validate` and `curios_runtime::precompile`. The Wasmtime pin lives in `curios-runtime/Cargo.toml` and nowhere else, so the precompiler and the launcher cannot disagree about the version.
- Crate boundaries, not Cargo features, separate the compiler, runtime and browser products.
- `curios-abi` is the source of truth for the host/guest wire contract. A host operation is complete only when its ABI row, compiler use, native runtime implementation and JavaScript implementation agree.
- `Intrinsic::signature` is the source of truth for what an intrinsic demands and produces. `curios-cert`'s typing, `curios-elab`'s elaboration and both congruences walk it rather than restate it; `/sys`'s declarations state the same types a second time, deliberately, and the prelude build checks them against the table. A new operation is typed by adding a row.
- `/std` and `/syn` are owned by `curios-prelude-archive` and compiled into an rkyv image in that crate's `OUT_DIR`. Every source module is registered in its Curios index; the build script discovers every `.crs` input and emits the matching rebuild directives.
- Production compilation has no fixed-prelude source fallback or cache-miss branch. Archive construction or restoration failure is a compiler invariant and fails loudly. The image is scoped to one compiler build and is not an interchange format.
- No crate below `curios-prelude-archive` may spell a `/syn` name: `curios-utilities` states slots, `curios-prelude-archive/src/syntax.rs` states spellings, and the prelude build checks every slot against the sources. The erased runtime carriers for compiler-emitted literals are `Nat` and packed `Bytes`.
- Binaryen is built from a verified source release, shared through the locked, target-specific cache under `curios-binaryen/.artifacts/<triple>`, never a fingerprint-specific `OUT_DIR`.
- Recursive lowering and packed-value interpretation must work on the default test-thread stack. Do not use `RUST_MIN_STACK` to hide a regression.
- Generated `.wasm` files and other build products are not source and are not committed. `Cargo.lock` is source and stays synchronized with dependency changes. `editors/grammar/src/` is the one committed generated artifact, because git is Zed's distribution channel for it: it is committed with the `grammar.js` it was generated from, and `editors/grammar`'s `npm test` refuses any drift between them.

## Writing Rust

- Open the `//!` documentation for every stage being changed, and check the next representation or consumer explicitly: parsing changes usually affect printing and lowering, core changes erasure, IR changes the next lowering and its tests.
- Module layout: no `mod.rs`; `foo.rs` declares its `foo/` submodules and re-exports them with `mod x; pub use x::*;`, crate roots do the same, and every crate stays a flat namespace. A namespace never survives as a way to disambiguate a name — reach for the crate name instead. The two kept namespaces, `curios-runtime`'s and `curios-ersd`'s `pub mod test_support`, exist to mark a use site as reaching for scaffolding rather than product API.
- Import names everywhere except at the four lowering seams (`curios-text`→`curios-elab`, `curios-elab`→`curios-ersd`, `curios-ersd`→`curios-cont`, `curios-cont`→`curios-wasm`), where the downstream crate's names stay qualified by the crate name only, e.g. `curios_ersd::Foo`. The shared foundations — `curios_utilities`, `curios_abi`, `curios_num` — are never qualified anywhere. A name arriving from two crates in one file stays qualified rather than aliased. Traits are imported by name, never anonymously.
- Unit tests live beside their implementation: `foo.rs` declares `#[cfg(test)] mod tests;` and the tests live in `foo/tests.rs`; a small module may stay inline. Programs that cross compiler stages go in `curios/src/tests/`, codegen tests in `curios/src/tests/codegen/`.
- Name per-carrier helpers, fields and emitted functions type-first, operation-last (`bin_force`, `list_slice`).
- Use `//!` for module purpose and invariants, `///` for public API contracts. Write every comment as one line per paragraph; do not hardwrap. Add a comment only where the WHY is non-obvious — an invariant, a rejected alternative, a measured trade-off — never one that restates the code.
- Stock rustfmt. Clippy runs with two workspace lints raised to `warn` in `Cargo.toml`, `unreachable_pub` and `unused_self`, inherited by every crate through `[lints] workspace = true`; `clippy.toml` holds exactly one entry, `ignore-interior-mutability` naming `Term`, which configures a lint rather than enabling one. Do not raise, lower or configure a lint beyond these without a design decision.

## Writing Curios

- Read [syntax.md](documentation/syntax.md) in full before editing any `.crs` file; `curios-text/src/parse.rs` implements the contract.
- The surface grammar's syntax forms are closed: a new type never gets its own operator or keyword. It opts into an existing form by writing a `satisfy` witness against the form's `/syn` concept. See `documentation/design/language/syntax-forms-are-closed-semantics-extend-by-witness.md`.
- Use `curios-prelude-archive/std/` as the reference for idiomatic code.
- Register a new `curios-prelude-archive/std/Foo.crs` in `curios-prelude-archive/std.crs`, and likewise under `syn/`; update `curios-prelude-archive/src/syntax.rs` only when Rust directly emits the new `/syn` name.
- Names use `/` qualification, `{}` is the unit type, `()` the unit value, and visibility of a nominal name is independent from visibility of its representation. `syntax.md` has the full rules.
- Probe a program on standard input and read the compiler's answer through `wonder`, as described above, before asserting what it does.

## Build and validation

The build recipes are `cargo xtask <recipe>`: the `xtask` crate reached through the alias in `.cargo/config.toml`, and the only build tool a clone needs beside cargo is none. The native compiler embeds the slim `curios-runtime` launcher with `include_bytes!`, and that launcher must be built in its own Cargo invocation: `cargo xtask build` does both stages in order, and `cargo xtask runtime` is the first stage alone, building `curios-runtime` in a cargo process of its own so workspace feature unification keeps Cranelift and Binaryen out, and filing the launcher at `curios/.artifacts/<triple>`. A `curios-runtime` binary from a workspace build is not evidence the isolated launcher is slim. Building `curios` without that stage fails naming the recipe to run.

### While iterating

- Run the smallest check or test that exercises the changed behavior, and prefer stage-local crate checks. Never run two Cargo builds concurrently.
- **In a multi-step task, run the full gate once, after the last step.** Between steps, `cargo clippy --workspace --all-targets --all-features -- -Dwarnings` plus `cargo fmt --all` is the check, even when each step is its own commit. Do not add `cargo check` beside it.
- Keep the feature set constant within a work session: `--all-features` enables `profile`, a plain `cargo build` does not, and alternating maintains two prelude archives that evict each other.
- The full suite can take more than five minutes. Run it in the background with output redirected to a file, and read the file after completion.

### What a workspace check already exercises

`cargo clippy --workspace` builds `curios-prelude-archive`, whose build script elaborates every `/std` and `/syn` module and erases them through `erase_unit` — whose prefix hand-off walks the result with `Module::verify_prefix` — and then `curios-prelude`'s script certifies the whole module with the kernel. A change on the Text, Core, Ersd or certification path is therefore exercised over the entire standard library by a step already in the gate, and needs only its own crate's tests beside it. Nothing below Ersd is reached: the archive stops there, so `curios-cont` and `curios-wasm` are detected only by the cross-stage corpus in `curios`.

### Before handing off code changes

Run this gate, in order. All commands must pass; Clippy warnings are errors in CI.

```sh
cargo xtask runtime
cargo fmt --all -- --check
cargo clippy --workspace --all-targets --all-features -- -Dwarnings
cargo test --workspace --all-targets --all-features
```

`cargo check` is deliberately absent: `clippy` is the same compilation with more lints. The denial is passed after `--` rather than as `RUSTFLAGS`, because `RUSTFLAGS` is a global fingerprint input that would rebuild and re-certify the prelude for that step alone. The gate is latency-bound: the crate graph is a deep near-linear chain, so wall clock is the critical path and more parallelism does not shorten it.

Measure a step and name the step; never quote a whole-gate total. Documentation-only changes need no rebuild unless they alter documented commands or make claims that need executable verification.

### Additional gates

- Changes to `curios-js` or its dependencies must also pass `cargo xtask js`.
- Changes to `curios-binaryen/build.rs` must verify an empty-cache build and a cache hit from a different Cargo mode or build-script fingerprint. The cache marker carries a hash of the build script, the target and the C++ toolchain's version string, so any change to the recipe invalidates every entry.
- Changes to runtime dependencies must rebuild `curios-runtime` through `cargo xtask runtime` and confirm that neither `cranelift-codegen` nor `curios-binaryen` entered its graph — name those crates, since Wasmtime's runtime legitimately pulls the `cranelift-bitset`, `cranelift-bforest` and `cranelift-entity` utility crates. The ordinary suite checks the artifact through `curios/src/bundle.rs`'s guards; this manual step diagnoses a failure rather than detecting one.
- Changes to the bundle format must run the ignored end-to-end test in `curios/tests/bundle.rs` explicitly.

### Profiling

Profile through the built-in `tracing` mechanism, not an external sampler: `cargo xtask profile programs/hello_world.crs` builds `curios` with `--features profile` — the only build in which the `profile` subcommand exists — and prints per-span aggregate timings sorted by total time. The instrumentation mechanics are documented in `curios-profile`.

Keep a figure beside the probe that reproduces it, or do not record it: a number belongs in an ignored test carrying the command, the date, the profile and what it last printed (`curios-prelude-archive`'s `stored_prelude_measurements` is the pattern), never in prose alone. Documentation states what a measurement decided; the measurement lives with the code.

## Documentation ownership

Document each fact at the narrowest authoritative level and link to it elsewhere; do not maintain parallel explanations that can drift. Do not hardwrap Markdown prose: one source line per paragraph or list item.

| Location | Owns |
| --- | --- |
| `README.md` | Public introduction: what Curios is, the happy path to running one, and where to go next |
| `documentation/usage.md` | Complete command-line and package reference — every subcommand, exit codes, dependencies, umbrellas, and the global flags |
| `CLAUDE.md` | Contributor behavior, ownership boundaries, durable invariants, and validation |
| `documentation/syntax.md` | Complete Curios surface-language reference |
| `documentation/roadmap.md` | Implemented capabilities and pending specifications |
| `documentation/design.md` and `documentation/design/**` | The objectives, and one cross-cutting design decision per file, cited by path |
| `documentation/soundness.md` and `documentation/soundness/**` | The perimeter, how to read a grade, and one perimeter rule per file with its evidence |
| Crate `README.md` files | The crate's mission and its crate-scoped design decisions |
| Crate and module rustdoc | Local architecture, algorithms, invariants, and public APIs |
| `Cargo.toml` descriptions | One-line crate purposes for Cargo tooling |
| `programs/README.md` | The measurement corpus: the layout rule, the instrument families, and the cross-language workloads |
| `benchmarks/README.md` | Benchmark harness mechanics, results, and the caveats that belong beside a number |

## Repository conventions

- Do not mix vendored changes, generated files or unrelated formatting into a feature commit.
- Use non-interactive Git, and never discard work with a destructive reset or checkout unless the user requests that exact action. Never rewrite history.
- Keep source files focused. Prefer extending an existing ownership boundary over creating a parallel abstraction for the same responsibility.

## Known build constraints

- `rust-toolchain.toml` pins the toolchain so a fresh clone builds with the one this checkout does; the floor is Wasmtime's, so bump the file when the pin in `curios-runtime/Cargo.toml` moves past it.
- `curios-binaryen` downloads, verifies and builds a pinned Binaryen source release with CMake, which needs a C++ toolchain. Every Cargo mode reuses `curios-binaryen/.artifacts/<triple>`.
- A build product that outlives the build that made it lives in `.artifacts/` beside its owner, never under `target/`: `curios-binaryen/.artifacts/<triple>`, `curios/.artifacts/<triple>`, `curios-js/.artifacts/<triple>`, and `benchmarks/.artifacts`. One `**/.artifacts` line in `.gitignore` covers them, and `cargo clean` never removes them; delete one by hand to force a rebuild.
- `target/debug/incremental` is pure rustc cache and safe to delete when no build is running; `CARGO_INCREMENTAL=0` suppresses it per invocation.
- `curios-js` is built by `cargo xtask js`; do not introduce `wasm-pack` or `wasm-opt` without a design decision. Binaryen optimization belongs to the native product only.
