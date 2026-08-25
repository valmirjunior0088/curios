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

- Read [roadmap.md](documentation/roadmap.md) before proposing or implementing a capability. Confirm whether the work is new, pending, or already represented differently.
- Inspect the worktree before editing. Preserve unrelated changes and avoid files outside the authorized scope.
- Identify the subsystem that owns the behavior, then read its crate-level and relevant module-level `//!` documentation before changing Rust.
- Read [syntax.md](documentation/syntax.md) in full immediately before writing or modifying Curios source. Do not rely on remembered syntax.
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
  → curios-js         expose curios-pipeline through wasm-bindgen
```

Data flows downward through the diagram, while Rust dependencies between compiler stages point in the opposite direction: lowering code depends on the representation it constructs. `curios-text` depends on `curios-elab`, which depends on `curios-ersd`, which depends on `curios-cont`, which depends on `curios-wasm`. Beside that chain, `curios-core` owns the term representation, `curios-analysis` the rules both checkers run over it, and `curios-cert` the kernel that only one of them does: `curios-elab` depends on `curios-core` and `curios-analysis`, `curios-cert` on both, and none of those reverse. `curios-elab` takes `curios-cert` as a *dev*-dependency only, so nothing whose build script reaches elaboration reaches the kernel through it — which is what keeps a kernel edit from re-elaborating the fixed prelude.

### Ownership map

| Area | Owner | Responsibility |
| --- | --- | --- |
| Zero-copy archiving | `curios-archive` | The workspace's only rkyv dependency, and the only place it is *spelled*: the pin, the feature set, the `archived` attribute macro with its `always`/`recursive` keywords and inert field markers (in the `curios-archive-derive` companion, since a proc-macro crate can export nothing else), the `Proxy`/`Via` adapter for types rkyv cannot archive directly, and the four serialization entry points with their bounds taken on. Nothing above it names an rkyv type, trait, function, or helper attribute |
| Shared foundations | `curios-utilities` | Spans, names, entropy, the typed identity-addressed `Arena`, packed values, the `SyntaxRegistry` shape the `/syn`-emitting stages read, and other stage-independent intrinsics — plus, as the workspace's only `stacker` dependency, the `recurse` guard every walk over data-shaped depth runs inside, which is the one place its figures are written |
| Parser combinators | `curios-parse` | The `FnOnce` parser DSL: ordered choice under progress-based commitment, packrat memoization, caret-snippet errors. Behind both the `.crs` grammar and the WAT parser |
| Printer combinators | `curios-print` | The Wadler document algebra `curios-text`, `curios-core` and `curios-wasm` write their `Display` impls in. Depends on nothing — it is defined over `std::fmt` alone, which is what lets every IR crate use it without depending on each other |
| Numeric tower | `curios-num` | The workspace's only `num-bigint` and `num-traits` dependency: the sealed unbounded `Natural` and `Integer`, the bitwise-identity `Floating`, and the `scalar` semantics of the erased `u32`/`i32`/binary32 carriers that every stage's constant folder shares. Sealed rather than re-exported — the magnitudes are private, so no crate above it names a bignum, which is what keeps `num-traits` out of the workspace's code entirely |
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
| Runtime | `curios-runtime` | The workspace's only `wasmtime` dependency — the pin, the feature set, and the `cranelift` opt-in — plus the engine, host bindings, `validate`/`precompile`, `.cwasm` deserialization, bundle payload format, and slim launcher |
| Native product | `curios` | The native back end — Binaryen optimization, Wasmtime precompilation, in-process running — plus the CLI, the unit cache, executable bundling, the cross-stage test corpus, and `wonder`: the engine that answers a question about a program from the compilation that would build it, in `src/wonder/`, and its two transports, the one-shot `ask` and the language `server`, which hold the workspace's only `lsp-server`/`lsp-types` rows. Compiling itself is `curios-pipeline`'s |
| Browser product | `curios-js` | wasm-bindgen compiler exports and JavaScript execution harness |
| Profiling | `curios-profile` | The workspace's only `tracing` dependency: `profile!`/`profile_span!` span macros and the `capture` aggregate-timing subscriber, gated per-crate on a `profile` feature |

## Change routing

| If changing… | Start in… | Also inspect… |
| --- | --- | --- |
| Surface grammar, syntax tree, or printing | `curios-text/src/parse*`, `module.rs`, `print.rs` | `into_core/`, parser tests, `documentation/syntax.md` |
| Surface-to-core lowering | `curios-text/src/into_core/` | Core constructors and cross-stage integration tests |
| Elaboration, typing, or conversion | `curios-elab/src/` | Text lowering, erasure, diagnostics, and integration tests |
| Kernel judgments | `curios-cert/src/` | `curios-core`'s representation, `curios-cert/src/recheck.rs`, and `documentation/design/language/the-soundness-perimeter.md` |
| A shared analysis | `curios-analysis/src/` | Both drivers — `curios-cert`'s `Kernel` and `curios-elab`'s `Context` — plus `curios-analysis/tests/driven.rs`, where the checker-driven probes live |
| A numeric carrier or its arithmetic | `curios-num/src/` | Every constant folder that shares `scalar` (`curios-core`, `curios-ersd`, `curios-cont`), and `documentation/design/toolchain/numeric-carriers-narrow-by-refusing-never-by-changing-a-value.md`. Adding an operation to `Natural`/`Integer` is adding to the trusted base — see the kernel decision's enumeration |
| Concepts or witness resolution | `curios-elab/src/concept.rs`, `resolve.rs` | Surface declarations, standard-library witnesses, and syntax documentation |
| Type erasure | `curios-elab/src/into_ersd*` | `curios-ersd` representation and downstream tests |
| Erased optimization | `curios-ersd/src/optimize/` | `into_cont.rs`, derived analyses, deep-input and specialization tests |
| CPS optimization or Wasm emission | `curios-cont/src/` | `curios-wasm`, codegen tests, and runtime behavior |
| Wasm representation or encoding | `curios-wasm/src/` | Continuation emission and parser/round-trip tests |
| Host operations or foreign calls | `curios-abi/src/` | Core validation, Wasm imports, runtime bindings, and the JavaScript harness |
| What a unit hands its successors | `curios-unit/src/` | Every stage whose artifact `Unit` holds, `curios-pipeline`'s fold, and the store's stored-unit format |
| Pipeline orchestration | `curios-pipeline/src/compile.rs`, `stage.rs`, `standard.rs` | Native and browser callers |
| A query, a record, or what a diagnostic carries | `curios/src/wonder/` | `curios-utilities`'s `Report` and every stage's `report`/`reports_with_hints` that produces one, `curios-pipeline`'s `CompileError` and `check_with_units`, the transports `curios/src/ask.rs` and `server.rs`, `curios-package`'s `Membership`, and `documentation/roadmap/wonder-spec.md` for what is not yet here |
| Manifests, dependency resolution, or the store | `curios-package/src/` | The CLI subcommands that wrap it, `curios-utilities`'s `Qualifier`/`Mount`, and `documentation/soundness/admission-without-judgment/cached-verdicts.md` when the store's keys are involved |
| Runtime or bundle format | `curios-runtime/src/`, `curios/src/bundle.rs` | Slim-launcher dependency boundary and bundle integration tests |
| CLI or native compile behavior | `curios/src/` | `README.md`, public helpers, and integration tests |
| Standard or syntax library | `curios-prelude-archive/std/`, `curios-prelude-archive/syn/` | Module indices, canonical syntax registry, `syntax.md`, and Curios integration tests |
| Prelude archive or replay | `curios-prelude-archive/build.rs`, `curios-prelude-archive/src/` | Text preparation, Core elaboration/erasure replay APIs, pipeline integration, and archive validation tests |
| Browser compiler or harness | `curios-js/` | Host ABI, wasm32 build, wasm-bindgen version, and CI release steps |
| Profiling instrumentation | `curios-profile/src/lib.rs` | Each consumer crate's `profile` feature fan-out, and `make curios/profile` |
| Binaryen version, build, or FFI | `curios-binaryen/` | Shared cache behavior, native compiler linkage, and optimize round-trip tests |

## Architectural invariants

- Compiler stages own their representations. A lowering belongs to the crate holding the source representation and depends on the crate holding the destination representation.
- `curios-pipeline` is the compiler boundary. It must not depend on Binaryen, Wasmtime, the runtime, or the CLI. It *may* name the fixed prelude, and does so in `standard.rs` alone: `compile_entrypoint` still takes a scope and still cannot tell which unit is `/std`, and nothing in the fold calls the layer above it. That layer exists because the native product, the browser product and this crate's own fixtures each wrote the same prelude wiring by hand — three callers agreeing is a missing function, not a policy, and the third was not a product at all.
- `curios-package` sits beside that boundary, never under it. The driver folds its stages over whatever scope it is handed; *deciding* that scope is a product's job, so `curios-pipeline` must not depend on `curios-package` and `curios-js` must not touch it. It is also the workspace's only TOML dependency.
- `curios-unit` sits below the kernel and must stay there: `cargo tree -p curios-unit --edges normal` must not contain `curios-cert`. A unit is *produced* by stages that do not judge and *judged* by the driver above, because `curios-prelude-archive`'s build script constructs a `Unit`, and a build script that reaches the certifier re-elaborates the whole standard library on every kernel edit.
- `curios-runtime` is the runtime-only boundary **in its default feature set**, which is the set `make curios/runtime`'s isolated build uses. It must not depend on `curios` or Binaryen at all, and must not reach Cranelift by default: its `cranelift` feature exists for `curios` and must never enter `default`. This is the one invariant here that does not rest on a reader noticing it — `curios/src/bundle.rs` scans the launcher image that actually ships and refuses a backend marker or a size over the ceiling.
- `curios` is the only workspace crate that combines Binaryen with Cranelift-enabled Wasmtime. It enables the latter through `curios-runtime`'s feature rather than a wasmtime row of its own, and names no wasmtime type: `curios_runtime::validate` and `curios_runtime::precompile` are the seam.
- The workspace uses crate boundaries, not Cargo features, to separate the compiler, runtime, and browser products.
- `curios` and `curios-runtime` cannot disagree about the Wasmtime version, because there is only one row: the pin lives in `curios-runtime/Cargo.toml` and nothing else names wasmtime. That matters because `curios` precompiles a `.cwasm` and the launcher deserializes it — wasmtime records its own version in the artifact and refuses a mismatch at load time, so a drift that used to be a loud runtime failure is now unrepresentable.
- `curios-abi` is the source of truth for the host/guest wire contract. A host operation is incomplete until its ABI row, compiler use, native runtime implementation, and applicable JavaScript implementation agree.
- `Intrinsic::signature` is the source of truth for what an intrinsic demands of its operands and what it produces. `curios-cert`'s typing, `curios-elab`'s elaboration and both congruences *walk* it rather than restating it, so an operand's type is not something the two checkers can disagree about — that property is the point, not the line count it saved. The same types are stated a second time, deliberately, by `/sys`'s declarations, which are the surface a user actually calls; those are *checked* against the table by the prelude build, since a declaration disagreeing with the operation its body constructs does not elaborate. A new operation is typed by adding a row, and a walker asserts the table agrees in length with `Intrinsic::operands` rather than trusting that it does.
- `/std` and `/syn` are owned by `curios-prelude-archive` and compiled into an rkyv image in that crate's `OUT_DIR`. Every source module must be registered in its Curios index; the build script discovers every `.crs` input and emits the matching Cargo rebuild directives.
- Production compilation has no fixed-prelude source fallback or cache-miss branch. Archive construction or restoration failure is a compiler invariant and fails loudly. The image is scoped to one compiler build and is not a stable interchange format.
- `/syn` ownership — which names it holds, and why — is `curios-prelude-archive/README.md`'s decision to state. The registry contract belongs to `curios-utilities`, below both stages that read it, and the erased runtime carriers for compiler-emitted literals remain `Nat` and packed `Bytes`. No crate below `curios-prelude-archive` may spell a `/syn` name: `curios-utilities` states slots, `curios-prelude-archive/src/syntax.rs` states spellings, and the prelude build checks every slot against the sources.
- Binaryen is built from a verified source release. Its C++ build is shared through the locked, target-specific cache under `curios-binaryen/.artifacts/<triple>`, not a Cargo fingerprint-specific `OUT_DIR`.
- Recursive lowering and packed-value interpretation must work on the default test-thread stack. Do not use `RUST_MIN_STACK` to hide a regression.
- Generated `.wasm` files and other build products are not source. Do not commit them. `Cargo.lock` is source and must remain synchronized with dependency changes. `editors/grammar/src/` is the one exception, and it is one because git is the *distribution channel* for that artifact rather than a cache of it: Zed resolves a grammar by cloning this repository at a pinned revision and compiling `parser.c` to WebAssembly, and never runs `tree-sitter generate` itself, so a parser absent from the commit is a parser Zed cannot build. It is committed in the same commit as the `grammar.js` it was generated from, and `editors/grammar`'s `npm test` regenerates and refuses any change under `src/`, untracked files included, so the two cannot drift apart unnoticed.

## Writing Rust

- Re-read the pipeline and ownership map above, then open the `//!` documentation for every stage being changed.
- Follow the established module layout: no `mod.rs` anywhere; `foo.rs` declares its `foo/` submodules and re-exports them with `mod x; pub use x::*;` (the default — a narrower re-export visibility is a deliberate deviation, not a default), and crate roots do the same, so every crate stays a flat namespace. **No namespace survives as a way to disambiguate a name.** Both that once did were the same problem — two vocabularies colliding on one name — and both were resolved by a crate boundary instead: the kernel's judgments live flattened on `curios-cert`'s root rather than under `curios-core`'s `pub mod kernel`, and the combinator DSLs are `curios-parse` and `curios-print` rather than `curios-utilities`'s `pub mod monads`, which existed only so that `parser::pure` and `printer::pure` stayed apart. When a name needs disambiguating, reach for the crate name.

Two namespaces are kept for a different reason: `curios-runtime`'s and `curios-ersd`'s `pub mod test_support`. Nothing in either collides with anything — the path exists so that `curios_runtime::test_support::GuestInstance` or `curios_ersd::test_support::census_settles_constructor_field` announces at its use site that the caller reached for scaffolding rather than product API. A namespace earns its place by *marking* code, never by separating names.
- Import names everywhere except at the four lowering seams (`curios-text`→`curios-elab`, `curios-elab`→`curios-ersd`, `curios-ersd`→`curios-cont`, `curios-cont`→`curios-wasm`), where the downstream crate's names stay qualified by exactly one level — the crate name only, e.g. `curios_ersd::Foo`, never a module path. The shared foundations — `curios_utilities`, `curios_abi`, `curios_num` — are never qualified anywhere, even in seam files: they are vocabulary both sides of a seam already speak, not the downstream representation a lowering is constructing. A name arriving from two or more crates in the same file stays qualified rather than aliased. Traits are imported by name like any other item, never anonymously (`use Bound;`, not `use Bound as _;`).
- Place unit tests beside their implementation: `foo.rs` declares `#[cfg(test)] mod tests;` and the tests live in `foo/tests.rs`. A small test module may stay inline as `#[cfg(test)] mod tests { … }` in the file it covers. Put programs that cross compiler stages in `curios/src/tests/`; codegen tests live in `curios/src/tests/codegen/`.
- Name per-carrier helpers, fields, and emitted functions type-first, operation-last (`bin_force`, `list_slice`), never operation-first (`force_bin`).
- When changing one stage, check the next representation or consumer explicitly. Parsing changes usually affect printing and lowering; core changes usually affect erasure; IR changes usually affect the next lowering and its tests.
- Use `//!` for module purpose and invariants, and `///` for public API contracts. Do not duplicate detailed subsystem documentation in this file. Write every comment — `//`, `///`, `//!` — as one line per paragraph or list item, matching the Markdown rule below; do not hardwrap. Add a comment only where the WHY is non-obvious — an invariant, a rejected alternative, a measured tradeoff — and never write one that only restates what the code already says.
- Use stock rustfmt and Clippy settings. There is no repository-specific `rustfmt.toml`, and `clippy.toml` holds exactly one entry: `ignore-interior-mutability` names `Term`, whose cache fills never move hash or equality — the fact stated once there instead of as an `#[allow(clippy::mutable_key_type)]` at every `Term`-keyed map. Do not add entries that change a lint's strictness.

## Writing Curios

- Read [syntax.md](documentation/syntax.md) in full before editing any `.crs` file. It is the normative surface-language reference; `curios-text/src/parse.rs` implements the contract.
- The surface grammar's syntax forms are closed: a new type never gets its own operator or keyword. It opts into an existing form (`+`, `==`, postfix `!`, …) by writing a `satisfy` witness against the form's `/syn` concept. See `documentation/design/language/syntax-forms-are-closed-semantics-extend-by-witness.md` before proposing hardcoded syntax for a type.
- Use `curios-prelude-archive/std/` as the reference for idiomatic code.
- Register a new `curios-prelude-archive/std/Foo.crs` module in `curios-prelude-archive/std.crs`. Apply the corresponding rule to `curios-prelude-archive/syn/` and `curios-prelude-archive/syn.crs`; update `curios-prelude-archive/src/syntax.rs` only when Rust directly emits the new `/syn` name — from lowering, or from a type-directed feature in `curios-elab`.
- Remember that names use `/` qualification, `{}` is the unit type, `()` is the unit value, and visibility of a nominal name is independent from visibility of its representation. Consult `syntax.md` for the full rules rather than extending this reminder list.
- Run Curios programs through the native CLI: `cargo run --package curios -- run <file.crs>`.

## Build and validation

The native compiler embeds the slim `curios-runtime` launcher with `include_bytes!`, and that launcher must be built in its own Cargo invocation. `make curios` does both stages in order:

```sh
make curios
cargo run --package curios -- <args>
```

`make curios/runtime` is the first stage on its own, and it is load-bearing: it builds `curios-runtime` without workspace feature unification, keeping Cranelift and Binaryen out, and copies the resulting launcher to `curios/.artifacts/<triple>` for embedding. A `curios-runtime` binary produced by a workspace build is not evidence that the isolated launcher remains slim. Building `curios` without that stage fails with a message naming the command to run, and a launcher older than `curios-runtime`'s sources warns rather than being embedded silently.

### While iterating

- Run the smallest check or test that exercises the changed behavior.
- Prefer stage-local crate checks while developing. Do not repeatedly build the whole workspace merely to obtain feedback.
- **In a multi-step task, run the full gate once — after the last step, not after each one.** Between steps, `cargo clippy --workspace --all-targets --all-features -- -Dwarnings` is the check, plus `cargo fmt --all`. This holds even when each step is its own commit: the unit that earns a suite run is the task, not the commit. Clippy is sufficient in between for the reason the next section gives — it builds `curios-prelude`, so a change on the Text, Core, Ersd or certification path is already exercised over the whole standard library. Do not add `cargo check` beside it, for the reason the gate section gives.
- The full workspace test suite can take more than five minutes. Run it deliberately in the background, redirect output to a file, and inspect the file after completion instead of piping or continuously scrolling it.

```sh
cargo test --workspace --all-targets --all-features > /tmp/curios-tests.txt 2>&1
```

### What a workspace check already exercises

`cargo clippy --workspace` (or any workspace build) is not only a compile. It builds `curios-prelude-archive`, whose build script elaborates every `/std` and `/syn` module, erases them through `erase_unit` — whose prefix hand-off walks the result with `Module::verify_prefix`, every rule but the entry block a prefix does not have — and then `curios-prelude`'s script certifies the whole module with the kernel, which decides universe satisfiability for every context before assuming it. So a change on the Text, Core, Ersd or certification path is exercised over the entire standard library by a step already in the gate, and needs only its own crate's tests beside it.

That the *prefix* is what gets walked is the load-bearing part, and it was not always so: a unit with no entrypoint leaves erasure through `ErsdBuilder::into_module`, which used to hand the image over unverified because `Module::verify` requires an entry block a prefix has not got. The archive therefore reached disk unwalked, and a fault in erasure or compaction was first reported against whichever program later compiled on top of it.

Nothing *below* Ersd is reached at all: the archive stops there, so a workspace check never enters `curios-cont` or `curios-wasm`, and their detector is the same cross-stage corpus in `curios`. Where a change sits against that line, rather than the size of its diff, is what decides whether it can be verified cheaply.

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

- Changes to `curios-js` or its dependencies must also pass `make curios/js` with the exactly version-matched `wasm-bindgen-cli` installed.
- Changes to `curios-binaryen/build.rs` must verify an empty-cache build and a cache hit from a different Cargo mode or build-script fingerprint. There is no schema constant to bump: the cache marker carries a hash of the build script itself, so any change to the recipe invalidates every entry, and it carries the target and the C++ toolchain's own version string beside it, so a cache is never reused across a toolchain it was not built with.
- Changes to runtime dependencies must rebuild `curios-runtime` in isolation through `make curios/runtime` and confirm that neither `cranelift-codegen` nor `curios-binaryen` entered its dependency graph. Name those crates when checking: Wasmtime's runtime pulls the `cranelift-bitset`, `cranelift-bforest` and `cranelift-entity` utility crates, so a search for "cranelift" reports a boundary that holds. The ordinary suite now checks the *artifact* rather than the graph — `curios/src/bundle.rs`'s guards scan the embedded launcher image for backend markers and hold it under a size ceiling — so this manual step is for diagnosing a failure, not for detecting one. Both guards have been run against a Cranelift-linked launcher and observed to fail; `launcher_guard_positive_control` records that measurement and how to reproduce it.
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
| `README.md` | Public introduction: what Curios is, the happy path to running one, and where to go next. Reference detail belongs in `documentation/usage.md`, not here |
| `documentation/usage.md` | Complete command-line and package reference — every subcommand, exit codes, dependencies, umbrellas, and the global flags |
| `CLAUDE.md` | Contributor behavior, ownership boundaries, durable invariants, and validation |
| `documentation/syntax.md` | Complete Curios surface-language reference |
| `documentation/roadmap.md` | Implemented capabilities and pending specifications |
| `documentation/design.md` | The objectives, and what a design decision is; one decision per file under `documentation/design/`, cited by path so a rename fails loudly |
| `documentation/design/**` | One cross-cutting design decision each — those spanning the language or several crates — with its rationale and rejected alternatives |
| `documentation/soundness.md` | What the perimeter is, how to read a grade, and the index of every entry's grade; one entry per file under `documentation/soundness/`, cited by path |
| `documentation/soundness/**` | One perimeter rule each — what it assumes, how far that has been checked, and the fixtures that are its evidence |
| Crate `README.md` files | The crate's mission and its crate-scoped design decisions, rationale, and rejected alternatives |
| Crate and module rustdoc | Local architecture, algorithms, invariants, and public APIs |
| `Cargo.toml` descriptions | One-line crate purposes for Cargo tooling |
| `programs/README.md` | The measurement corpus: the layout rule, the instrument families, and the cross-language workloads |
| `benchmarks/README.md` | Benchmark harness mechanics, results, and the caveats that belong beside a number |

Do not hardwrap Markdown prose. Write one source line per paragraph or list item and let the renderer soft-wrap it. Fenced code blocks and tables retain their deliberate line structure.

## Repository conventions

- Use imperative, capitalized, descriptive commit subjects. Do not add co-authors unless the user requests them.
- Do not mix vendored changes, generated files, or unrelated formatting into a feature commit.
- Use non-interactive Git commands and never discard work with destructive reset or checkout operations unless the user explicitly requests that exact action.
- Keep source files focused. Prefer extending an existing ownership boundary over creating a parallel abstraction for the same responsibility.

## Known build constraints

- `rust-toolchain.toml` pins the Rust toolchain, so a fresh clone's `rustup` fetches the one this checkout builds with instead of failing on a dependency's `rust-version` floor. The floor is Wasmtime's; when its pin in `curios-runtime/Cargo.toml` moves past the toolchain, bump the file.
- `curios-binaryen` downloads, verifies, and builds a pinned Binaryen source release with CMake, which requires a C++ toolchain. Subsequent Cargo modes must reuse `curios-binaryen/.artifacts/<triple>`, which sits beside the crate rather than under `target/` precisely so `cargo clean` cannot remove a build measured in minutes.
- **A build product that outlives the build that made it lives in `.artifacts/` beside its owner**, never under `target/`. Four do: `curios-binaryen/.artifacts/<triple>` holds the Binaryen build, `curios/.artifacts/<triple>` the embedded launcher, `curios-js/.artifacts/<triple>` the wasm-bindgen bundle, and `benchmarks/.artifacts` every contestant the harness builds. The first two are read by build scripts and are found through `CARGO_MANIFEST_DIR`, so nothing reconstructs Cargo's internal directory layout; the other two are written by `make` and by the harness. One `**/.artifacts` line in `.gitignore` covers all four, and `cargo clean` is an ordinary command again. Delete any of them by hand when you want it rebuilt.
- `target/debug/incremental` is pure rustc cache and safe to delete outright, provided no build is running — and it holds a large share of `target/debug`, since Cargo enables incremental compilation for `dev` and `test` while this workspace's edit pattern rebuilds every downstream crate in full anyway; `CARGO_INCREMENTAL=0` suppresses it per invocation.
- `curios-js` deliberately uses plain `cargo build` plus `wasm-bindgen-cli`; do not introduce `wasm-pack` or `wasm-opt` without an explicit design decision. Binaryen optimization belongs only to the native `curios` product.
- The wasm32 build requires the installed `wasm-bindgen-cli` version to match the `wasm-bindgen` crate version in `Cargo.lock` exactly.
