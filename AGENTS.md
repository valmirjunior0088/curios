# AGENTS.md

Operational guide for working on Curios. Read this before investigating or changing the repository.

## Working with the user

- **Do not change anything without explicit instruction to do so.** Investigation, explanation, and proposals are read-only activities. Do not edit, format, generate, delete, stage, commit, or otherwise mutate the repository unless the user has authorized that action.
- **Treat authorization as narrowly scoped.** A request to implement one change does not authorize adjacent refactors, cleanup, dependency upgrades, documentation rewrites, or fixes for unrelated problems.
- **Do not proactively solve problems you were not asked to solve.** If you discover an unrelated bug, inefficiency, inconsistency, or cleanup opportunity, report it and ask whether the user wants it addressed.
- **Run every decision through the user.** Where there is more than one reasonable design, present the alternatives and their trade-offs, recommend one plainly, and wait for the user to choose.
- **Do not silently broaden the task to make implementation easier.** If the requested result requires a material change in scope, explain why and request permission.
- **Preserve existing work.** Assume every uncommitted change belongs to the user. Do not overwrite, revert, reformat, stage, or incorporate it unless the user explicitly includes it in the task.
- **Do not commit or push unless explicitly asked.** When asked to commit, include only the authorized changes and follow the requested attribution and message constraints.
- **Do not spiral into self-doubt.** State findings, uncertainties, and recommendations plainly. Do not hedge, repeatedly reopen settled decisions, or defer to the user without first presenting the available evidence.
- **Do not spawn subagents or delegate work unless explicitly asked.** Investigate and implement directly unless the user requests parallel or delegated work.
- **Stop when authorization is ambiguous.** Ask before taking an action whose effects extend beyond the clear scope of the request.

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
  → curios-core       elaborate, typecheck, normalize, and erase types
  → curios-ersd       optimize erased terms and lower them to continuations
  → curios-cont       optimize continuation IR and emit WebAssembly
  → curios-wasm       model, parse, and encode WebAssembly modules
  → curios-pipeline   drive the pure compiler pipeline

Native compiler path:
  → curios-binaryen   optimize emitted WebAssembly
  → curios            precompile with Wasmtime, run, or bundle
  → curios-rt         deserialize and execute the precompiled module

Browser path:
  → curios-js         expose curios-pipeline through wasm-bindgen
```

Data flows downward through the diagram, while Rust dependencies between compiler stages point in the opposite direction: lowering code depends on the representation it constructs. `curios-text` depends on `curios-core`, which depends on `curios-ersd`, which depends on `curios-cont`, which depends on `curios-wasm`.

### Ownership map

| Area | Owner | Responsibility |
| --- | --- | --- |
| Shared foundations | `curios-base` | Spans, names, entropy, parser/printer utilities, packed values, and other stage-independent primitives |
| Host/guest contract | `curios-abi` | Wire constants and self-describing foreign-function rows shared by compiler and runtime |
| Surface language | `curios-text` | Lexer, parser, surface AST, printer, lowering to core, and embedded `/std` and `/syn` libraries |
| Type theory | `curios-core` | Elaboration, typing, conversion, reduction, inductives, structures, concepts, zonking, and erasure |
| Erased optimization | `curios-ersd` | Post-erasure IR, compile-time evaluation and specialization, worker/wrapper transforms, and lowering to CPS |
| Continuation IR | `curios-cont` | CPS optimization and WebAssembly emission |
| WebAssembly model | `curios-wasm` | Wasm AST, parser, encoder, and binary writer |
| Pure compiler driver | `curios-pipeline` | `compile_entrypoint`, `Stage`, and orchestration without runtime, Binaryen, or CLI dependencies |
| Binaryen integration | `curios-binaryen` | Binaryen source build, static FFI, and Wasm optimization |
| Runtime | `curios-rt` | Wasmtime engine, host bindings, `.cwasm` deserialization, bundle payload format, and slim launcher |
| Native product | `curios` | Public compile/run helpers, CLI, Binaryen optimization, Wasmtime precompilation, and executable bundling |
| Browser product | `curios-js` | wasm-bindgen compiler exports and JavaScript execution harness |

## Change routing

| If changing… | Start in… | Also inspect… |
| --- | --- | --- |
| Surface grammar, syntax tree, or printing | `curios-text/src/parse*`, `module.rs`, `print.rs` | `into_core/`, parser tests, `documentation/SYNTAX.md` |
| Surface-to-core lowering | `curios-text/src/into_core/` | Core constructors and cross-stage integration tests |
| Elaboration, typing, or conversion | `curios-core/src/` | Text lowering, erasure, diagnostics, and integration tests |
| Concepts or witness resolution | `curios-core/src/concept.rs`, `resolve.rs` | Surface declarations, standard-library witnesses, and syntax documentation |
| Type erasure | `curios-core/src/erase*` | `curios-ersd` representation and downstream tests |
| Erased optimization | `curios-ersd/src/optimize/` | `into_cont/`, call-graph utilities, deep-input and specialization tests |
| CPS optimization or Wasm emission | `curios-cont/src/` | `curios-wasm`, codegen tests, and runtime behavior |
| Wasm representation or encoding | `curios-wasm/src/` | Continuation emission and parser/round-trip tests |
| Host operations or foreign calls | `curios-abi/src/` | Core validation, Wasm imports, runtime bindings, and the JavaScript harness |
| Pipeline orchestration | `curios-pipeline/src/lib.rs` | Native and browser callers |
| Runtime or bundle format | `curios-rt/src/`, `curios/src/bundle.rs` | Slim-launcher dependency boundary and bundle integration tests |
| CLI or native compile behavior | `curios/src/` | `README.md`, public helpers, and integration tests |
| Standard or syntax library | `curios-text/std/`, `curios-text/syn/` | Module indices, `prelude.rs`, `SYNTAX.md`, and Curios integration tests |
| Browser compiler or harness | `curios-js/` | Host ABI, wasm32 build, wasm-bindgen version, and CI release steps |
| Binaryen version, build, or FFI | `curios-binaryen/` | Shared cache behavior, native compiler linkage, and optimize round-trip tests |

## Architectural invariants

- Compiler stages own their representations. A lowering belongs to the crate holding the source representation and depends on the crate holding the destination representation.
- `curios-pipeline` is the pure compiler boundary. It must not depend on Binaryen, Wasmtime, the runtime, or the CLI.
- `curios-rt` is the runtime-only boundary. It must not depend on `curios`, Binaryen, or Wasmtime's Cranelift compiler.
- `curios` is the only workspace crate that combines Binaryen with Cranelift-enabled Wasmtime.
- The workspace uses crate boundaries, not Cargo features, to separate the compiler, runtime, and browser products.
- `curios` and `curios-rt` use the same workspace-pinned Wasmtime version so compiler-produced `.cwasm` modules match the runtime that deserializes them.
- `curios-abi` is the source of truth for the host/guest wire contract. A host operation is incomplete until its ABI row, compiler use, native runtime implementation, and applicable JavaScript implementation agree.
- `/std` and `/syn` modules are embedded at build time. Every module must be registered in its Curios index and in the `include_str!` table in `curios-text/src/prelude.rs`.
- Compiler-emitted proof-certified literals are owned by `/syn`: character literals construct transparent `/syn/Char` values and string literals construct `/syn/Str` values. Rust spellings of those hidden lowering targets belong in `curios-base/src/syn.rs`; the erased runtime carriers remain `Nat` and packed `Bytes`, respectively.
- Binaryen is built from a verified source release. Its expensive C++ build is shared through the locked, target-specific cache under `target/binaryen`, not a Cargo fingerprint-specific `OUT_DIR`.
- Recursive lowering and packed-value interpretation must work on the default test-thread stack. Do not use `RUST_MIN_STACK` to hide a regression.
- Generated `.wasm` files and other build products are not source. Do not commit them. `Cargo.lock` is source and must remain synchronized with dependency changes.

## Writing Rust

- Re-read the pipeline and ownership map above, then open the `//!` documentation for every stage being changed.
- Follow the established module layout: `foo.rs` declares and usually re-exports focused submodules from a sibling `foo/` directory.
- Place unit tests beside their implementation in a `*_tests.rs` module gated by `#[cfg(test)]`. Put programs that cross compiler stages in `curios/src/tests/`; codegen tests live in `curios/src/tests/codegen/`.
- When changing one stage, check the next representation or consumer explicitly. Parsing changes usually affect printing and lowering; core changes usually affect erasure; IR changes usually affect the next lowering and its tests.
- Use `//!` for module purpose and invariants, and `///` for public API contracts. Do not duplicate detailed subsystem documentation in this file.
- Use stock rustfmt and Clippy settings. There is no repository-specific `rustfmt.toml` or `clippy.toml`.

## Writing Curios

- Read [SYNTAX.md](documentation/SYNTAX.md) in full before editing any `.crs` file. It is the normative surface-language reference; `curios-text/src/parse.rs` implements the contract.
- Use `curios-text/std/` as the reference for idiomatic code.
- Register a new `curios-text/std/Foo.crs` module in both `curios-text/std.crs` and `curios-text/src/prelude.rs`. Apply the corresponding rule to `curios-text/syn/` and `curios-text/syn.crs`.
- Remember that names use `/` qualification, `{}` is the unit type, `()` is the unit value, and visibility of a nominal name is independent from visibility of its representation. Consult `SYNTAX.md` for the full rules rather than extending this reminder list.
- Run Curios programs through the native CLI: `cargo run --package curios -- run <file.crs>`.

## Build and validation

The native compiler embeds the slim `curios-rt` launcher with `include_bytes!`. Build that launcher in isolation before building `curios`:

```sh
make curios/runtime
cargo build --package curios
cargo run --package curios -- <args>
```

`make curios/runtime` is load-bearing: it builds `curios-rt` without workspace feature unification, keeping Cranelift and Binaryen out, and copies the resulting launcher to `curios/runtime` for embedding. A `curios-rt` binary produced by a workspace build is not evidence that the isolated launcher remains slim.

### While iterating

- Run the smallest check or test that exercises the changed behavior.
- Prefer stage-local crate checks while developing. Do not repeatedly build the whole workspace merely to obtain feedback.
- The full workspace test suite can take more than five minutes. Run it deliberately in the background, redirect output to a file, and inspect the file after completion instead of piping or continuously scrolling it.

```sh
cargo test --workspace --all-targets --all-features > /tmp/curios-tests.txt 2>&1
```

### Before handing off code changes

Run the same gate as CI, in order. All commands must pass, and Clippy warnings are errors in CI.

```sh
make curios/runtime
cargo fmt --all -- --check
cargo check --workspace --all-targets --all-features
RUSTFLAGS="-Dwarnings" cargo clippy --workspace --all-targets --all-features
cargo test --workspace --all-targets --all-features
```

Documentation-only changes do not require rebuilding the compiler unless they alter documented commands or make claims that need executable verification. Check their diff, links, filenames, and hardwrapping directly.

### Additional gates

- Changes to `curios-js` or its dependencies must also pass the wasm32 build used by CI: build `curios-js` for `wasm32-unknown-unknown`, then run the exactly version-matched `wasm-bindgen-cli --target web` step from the workflow.
- Changes to `curios-binaryen/build.rs` must verify an empty-cache build and a cache hit from a different Cargo mode or build-script fingerprint. Bump `BUILD_SCHEMA` when the CMake configuration or installed-library contract changes.
- Changes to runtime dependencies must rebuild `curios-rt` in isolation through `make curios/runtime` and confirm that neither Cranelift nor Binaryen entered its dependency graph.
- Changes to the bundle format must run the ignored end-to-end test in `curios/tests/bundle.rs` explicitly.

## Documentation ownership

Document each fact at the narrowest authoritative level and link to it elsewhere. Do not maintain parallel explanations that can drift.

| Location | Owns |
| --- | --- |
| `README.md` | Public introduction, setup, CLI usage, and repository overview |
| `AGENTS.md` | Contributor behavior, ownership boundaries, durable invariants, and validation |
| `documentation/SYNTAX.md` | Complete Curios surface-language reference |
| `documentation/ROADMAP.md` | Implemented capabilities and pending specifications |
| Crate and module rustdoc | Local architecture, algorithms, invariants, and public APIs |
| `Cargo.toml` descriptions | One-line crate purposes for Cargo tooling |
| `benchmarks/README.md` | Benchmark harness mechanics |
| `benchmarks/RESULTS.md` | Results from a specific benchmark run |

Do not hardwrap Markdown prose. Write one source line per paragraph or list item and let the renderer soft-wrap it. Fenced code blocks and tables retain their deliberate line structure.

## Repository conventions

- Use imperative, capitalized, descriptive commit subjects. Do not add co-authors unless the user requests them.
- Do not mix vendored changes, generated files, or unrelated formatting into a feature commit.
- Use non-interactive Git commands and never discard work with destructive reset or checkout operations unless the user explicitly requests that exact action.
- Keep source files focused. Prefer extending an existing ownership boundary over creating a parallel abstraction for the same responsibility.

## Known build constraints

- `curios-binaryen` downloads, verifies, and builds a pinned Binaryen source release with CMake. The first cache population takes minutes and requires a C++ toolchain; subsequent Cargo modes must reuse `target/binaryen`. A full `cargo clean` removes the cache.
- `curios-js` deliberately uses plain `cargo build` plus `wasm-bindgen-cli`; do not introduce `wasm-pack` or `wasm-opt` without an explicit design decision. Binaryen optimization belongs only to the native `curios` product.
- The wasm32 build requires the installed `wasm-bindgen-cli` version to match the `wasm-bindgen` crate version in `Cargo.lock` exactly.
