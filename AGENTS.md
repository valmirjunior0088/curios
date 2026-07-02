# AGENTS.md

Agent guide to `curios`. Operational reference plus an orientation map. Read this before touching the code.

## Overview

`curios` is a work-in-progress functional, dependently-typed programming language, implemented in Rust (edition 2024, ~75k lines). It compiles `.crs` source through a series of intermediate representations down to WebAssembly, and runs the result on an embedded `wasmtime` engine.

The repo is a **Cargo workspace** (virtual manifest at the root) of twelve crates, layered along the pipeline:

- **`curios-abi`** — the host/guest contract: wire ABI constants (`/sys/Io`'s status, poll-event, and open-mode codes) plus the `ForeignStore` of self-describing `ForeignFunction` rows (import name, `WireSignature`, type-level `Reduction`) describing every host operation. The `sys_io()` seed is the single source the `/sys/Io` prelude declarations, elaboration checks, wasm `env.*` imports, and runtime linker types all derive from — the IR nodes carry the row itself (an `Arc<ForeignFunction>`), so adding a host op is one `sys_io` row, one `ForeignImpls::define` closure in `curios-rt`, and the `Host` trait method/impls. A pure leaf, shared by the compiler stages and the runtime.
- **`curios-base`** — foundational utilities shared by every stage: source spans, the fresh-name `Entropy`/`Mint` supply, the `name!` newtype macro, the parser/printer monad combinators, and the slice `suffix_view` re-base laws (shared by `ersd`'s `worker_wrapper` and `cont`'s `slice_forwarding`).
- **`curios-wasm`** — the wasm module model, parser, and binary writer/encoder. A pristine leaf on top of `curios-base`.
- **`curios-cont`** — the continuation-passing IR: cont→cont optimization (`optm/`) and wasm emission (`to_wasm/`).
- **`curios-ersd`** — the erased IR (post type-erasure): ersd→ersd optimization (`optm/`) and lowering to CPS (`to_cont/`).
- **`curios-core`** — the core language: elaboration, typing, reduction, conversion, inductives, erasure, zonking.
- **`curios-text`** — the surface syntax: lexer/parser, lowering to core (`to_core/`), plus the embedded standard library (`curios-text/std/`, `curios-text/syn/`).
- **`curios-binaryen`** — FFI to the vendored Binaryen optimizer (`curios-binaryen/binaryen/`).
- **`curios-pipeline`** — the pure pipeline driver: `compile_entrypoint`/`typecheck_entrypoint`/`Stage`, chaining `text` → `core` → `ersd` → `cont` → `wasm` with no runtime/Binaryen/CLI dependencies. Extracted from `curios` so a wasm32 build of the compiler doesn't have to drag those in.
- **`curios-js`** — the curios ↔ JavaScript boundary: `wasm-bindgen` exports (`compile`, `typecheck`) of `curios-pipeline`, built with `wasm-pack --target web` for a browser playground. No host imports are satisfied here.
- **`curios-rt`** — runtime-only engine (lib) + the launcher stub (bin `curios-rt`). Deserializes a precompiled module and runs it on wasmtime; **never** links Cranelift or Binaryen. Depends only on `curios-abi` (for the wire constants), not on `curios` — that's what keeps it slim and lets `curios` depend back on it without a cycle.
- **`curios`** — the facade + driver + CLI: re-exports the five pipeline-stage crates under their historical module names (`text`, `core`, `ersd`, `cont`, `wasm`), plus `curios-pipeline` (`compile_entrypoint`/`typecheck_entrypoint`/`Stage`), the compile/precompile/run-from-source helpers, and the clap-based CLI (bin `curios`). The **only** crate that links Cranelift (via `wasmtime`'s `cranelift` feature) and Binaryen.

Code dependencies between the pipeline-stage crates run the *opposite* direction of data flow: `curios-text` depends on `curios-core` (its `to_core` lowering constructs core terms), `curios-core` depends on `curios-ersd` (`erase` constructs ersd terms), `curios-ersd` depends on `curios-cont` (`to_cont` constructs cont terms), and `curios-cont` depends on `curios-wasm` (`to_wasm` constructs a wasm module). `curios-wasm` is a leaf.

The JIT-vs-deserialize split is a _crate boundary_, not a feature flag — see [Crates, features, and the slim launcher](#crates-features-and-the-slim-launcher) for the full mechanism. `curios` and `curios-rt` share one `wasmtime`, version-pinned once in `[workspace.dependencies]`, so the `.cwasm` `curios` produces matches what `curios-rt` deserializes.

Two languages live in this repo: **Rust** (the compiler) and **curios** itself (the object language, with a standard library under `curios-text/std/`). Work touches one or both.

For what's already built vs. still planned, see [ROADMAP.md](ROADMAP.md) — check it before starting work on a new capability, both to confirm it's genuinely unstarted and to see how finished, related features are described.

## Before you write code

Refresh the relevant reference into working memory _before_ writing, every time — do not rely on a stale recollection from earlier in the session or from training.

- **Writing curios (`.crs`)?** Read [SYNTAX.md](SYNTAX.md) in full first. The surface language has many specialized forms (per-scrutinee `match` shapes, motives, glued literal signs, postfix-`!` do-notation) that are easy to get subtly wrong from memory.
- **Writing Rust (the compiler)?** Re-read [The pipeline](#the-pipeline) and [Where things live](#where-things-live) below, then open the `//!` module docs of the stage(s) you are touching, so the full architecture and the stage's local invariants are fresh. A change in one stage usually has obligations in the next.

This is cheap insurance: both languages reward precision and punish half-remembered syntax or architecture.

## The pipeline

The compiler is a chain of stages, each its own crate (module root `src/`). This chain is the backbone — when orienting yourself in unfamiliar code, find the stage first.

```
.crs source
  → text/   (crate: curios-text)   parse surface syntax; lower to core (text/to_core)
  → core/   (crate: curios-core)   elaborate, typecheck, reduce/convert, then erase types
  → ersd/   (crate: curios-ersd)   "erased" IR (types gone); ersd→ersd optimization (ersd/optm), then lower to continuations (ersd/to_cont)
  → cont/   (crate: curios-cont)   continuation-passing-style IR; cont→cont optimization (cont/optm), then emit wasm (cont/to_wasm)
  → wasm/   (crate: curios-wasm)   wasm module model, encoder/writer, parser
  → lib.rs  (crate: curios-pipeline)   the pipeline driver: compile_entrypoint / typecheck_entrypoint / Stage

then, in curios itself:
  → optimize (curios-binaryen) + precompile to .cwasm (to_cwasm)
  → run on wasmtime via curios-rt::run_bytes  (or bundle into an executable)
```

`curios-binaryen/binaryen/` is vendored C++ (the Binaryen wasm optimizer), linked via FFI from `curios-binaryen/src/` to optimize emitted wasm. The wasmtime engine + host stack live in `curios-rt/src/` (`run_bytes` deserializes a `.cwasm` and runs it; `instantiate` wires the `env.io_*` host imports).

## Where things live

| Path                                              | Role                                                                                                                                             |
| --------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------ |
| `curios-text/src/`                               | Lexer/parser, surface AST, lowering to core (`to_core/`)                                                                                         |
| `curios-core/src/`                               | Core language: elaboration, typing, reduction, conversion, inductives, erasure, zonking                                                          |
| `curios-ersd/src/`                               | Erased IR (post type-erasure); ersd→ersd optimization (`optm/`: prune, the `worker_wrapper` engine — monoid accumulator + suffix cursor — over a shared `call_graph`/`curios_base::suffix_view`); lowering to CPS (`to_cont/`) |
| `curios-cont/src/`                               | Continuation-passing IR; cont→cont optimization (`optm/`: inlining, DCE, copy/tag/jump threading, tail recursion, …); wasm emission (`to_wasm/`) |
| `curios-wasm/src/`                               | Wasm module model, parser, binary writer/encoder                                                                                                 |
| `curios-pipeline/src/lib.rs`                     | Pipeline driver: `compile_entrypoint`, `typecheck_entrypoint`, `Stage`                                                                           |
| `curios-js/src/lib.rs`                           | `wasm-bindgen` exports of `curios-pipeline` for a browser build (`compile`, `typecheck`)                                                         |
| `curios-base/src/monads/`                        | Parser/printer monad combinators                                                                                                                 |
| `curios-base/src/{span,entropy,macros,suffix_view}.rs` | Foundational utilities shared by every stage                                                                                               |
| `curios-abi/src/{lib,host}.rs`                   | Host↔guest contract: wire ABI constants and the `ForeignFunction`/`ForeignStore` host-op rows (`sys_io()` seed)                                  |
| `curios-text/{std,syn}/`                         | curios standard / support libraries (`*.crs`), indexed by `curios-text/{std,syn}.crs`                                                            |
| `curios/examples/`                               | Runnable examples that execute (`parse_*`, `inline_*`, `crs_*`, `bench_parse`, `bench_check`)                                                    |
| `curios-binaryen/src/`                           | FFI bindings to vendored Binaryen (`sys.rs`); `optimize(bytes)`; `build.rs` links it                                                             |
| `curios-binaryen/binaryen/`                      | Vendored Binaryen C++ source (do not edit — see Gotchas)                                                                                         |
| `curios-rt/src/`                                 | wasmtime engine + host stack (`run_bytes`, `instantiate`, `shared_engine`); OS + mock hosts                                                      |
| `curios-rt/src/bundle.rs`                        | Bundled-executable footer format (`append_payload`/`extract_payload`), shared by bundler + launcher                                              |
| `curios-rt/src/main.rs`                          | The launcher stub (bin `curios-rt`): reads its appended `.cwasm` tail and runs it                                                                |
| `curios/src/compile.rs`                          | `to_cwasm`, `run_wasm`, `load`, run-from-source helpers (public `curios` API; host types come from `curios_rt` directly)                         |
| `curios/src/main.rs`                             | clap-based CLI (bin `curios`): module wiring + subcommand dispatch                                                                               |
| `curios/src/{cli,pipeline,bundle}.rs`            | CLI surface (clap), pipeline driving + `--print` stage dumps, and the `compile`→executable bundler                                               |
| `Makefile`                                       | Builds the slim `curios-rt` and copies it to `curios/runtime` for `bundle.rs` to `include_bytes!`                                                |
| `curios/src/tests/`                              | Cross-stage integration tests (incl. `codegen/` and the relocated Binaryen optimize-roundtrip test)                                              |
| `curios/tests/bundle.rs`                         | Gated (`#[ignore]`) end-to-end test of the `compile`→executable path                                                                             |

## Build & test

**Run `make curios/runtime` first, then build with `cargo` as usual.** `curios` embeds the slim launcher via `include_bytes!`, so it must sit at `curios/runtime` before the compiler is built; `make` builds it in isolation and copies it there. If the file is missing, the build **fails** at the `include_bytes!` with a clear "couldn't read" error — run `make curios/runtime` and rebuild. The isolated build is also what keeps the launcher slim ([Crates, features, and the slim launcher](#crates-features-and-the-slim-launcher)).

```sh
make curios/runtime                  # build the slim launcher, place it for embedding
cargo build --package curios         # the CLI (Binaryen C++ build on first run)
cargo run   --package curios -- <args> # invoke the CLI
cargo test --workspace --all-targets --all-features
```

`curios compile foo.crs` produces a native executable `foo` (the embedded launcher with the program's `.cwasm` appended — no launcher file is consulted at compile time); `curios run foo.crs` compiles and runs in-process; `curios check foo.crs` type-checks only.

### Crates, features, and the slim launcher

There are **no Cargo features** on the workspace crates. The JIT/Cranelift split is a crate boundary instead:

- `curios-rt` declares only a runtime-only `wasmtime` (no `cranelift`) and never depends on `curios-binaryen`. **`cargo build --package curios-rt` is a slim, Cranelift/Binaryen-free launcher** — that is the build embedded into the compiler as the stub.
- `curios` adds the `cranelift` feature to its own `wasmtime` dependency and depends on `curios-binaryen`. Feature unification makes Cranelift available throughout a `curios` build (and a `--workspace` build), so the `curios-rt` _bin_ produced by a `--workspace` build is **not** the slim one. This is why `make curios/runtime` builds the launcher with an isolated `cargo build --release --package curios-rt` _before_ the compiler and copies it to `curios/runtime`: building it alone keeps Cranelift out. Do not hand-build the launcher via `--workspace` and expect it to be slim — it will be the fat (Cranelift-linked) one.
- The done bar lists no separate `cargo check --package curios-rt`: the isolated `cargo build --release --package curios-rt` that `make curios/runtime` runs already proves the runtime-only configuration compiles — something `--workspace --all-features` cannot do, since feature unification pulls Cranelift in.

Building `curios-binaryen` compiles a large C++ project via CMake on first build — expect minutes, not seconds, and a C++ toolchain + CMake on the machine. Anything depending on it (`curios-binaryen`, `curios`, the whole `--workspace`) pays that cost once; the pipeline-stage crates (`curios-text`, `curios-core`, `curios-ersd`, `curios-cont`, `curios-wasm`) and `curios-rt` on their own do not.

## The done bar

Before considering any change complete, run the same gate CI enforces, in order. All five must pass; `clippy` runs with warnings denied.

```sh
make curios/runtime                                        # provide the launcher curios embeds
cargo fmt --all -- --check
cargo check  --workspace --all-targets --all-features
cargo clippy --workspace --all-targets --all-features      # CI sets RUSTFLAGS="-Dwarnings"
cargo test   --workspace --all-targets --all-features
```

The first line is load-bearing twice over: it provides the launcher binary `bundle.rs` embeds, and its isolated build doubles as the slim-configuration check (see [Crates, features, and the slim launcher](#crates-features-and-the-slim-launcher)).

There is no `rustfmt.toml` or `clippy.toml` — stock toolchain defaults apply. Run `cargo fmt --all` to fix formatting rather than hand-aligning.

## Rust conventions

- **Module pattern.** A module is `foo.rs` (declaring submodules and re-exporting them, usually `mod x; pub use x::*;`) alongside a sibling `foo/` directory. Match this when adding modules; keep files focused (the history favors splitting large files into thematic submodules).
- **Tests.** Unit tests live beside the code as a `*_tests.rs` submodule gated with `#[cfg(test)]` (e.g. `core/typing.rs` ↔ `core/typing_tests.rs`). Cross-stage integration tests (which compile and run programs) live in `curios/src/tests/`.
- **Docs.** Use `//!` module-level doc comments to explain a module's purpose; `curios-binaryen/build.rs` is a good model.
- **Commits.** Imperative mood, capitalized, descriptive — e.g. "Split term.rs and elaborate.rs into focused submodules". Do not patch vendored files in a feature commit.

## Writing curios (`.crs`)

The standard library under `curios-text/std/` is the reference for idiomatic curios. Each module is one file (`curios-text/std/Foo.crs`) and must be registered in two places: `curios-text/std.crs` (`pub mod Foo; pub use Foo/{let Foo};`) **and** the `include_str!` table in `curios-text/src/prelude.rs` (the modules are embedded into the compiler at build time, not read from disk). The same applies to `curios-text/syn/` via `curios-text/syn.crs`.

[SYNTAX.md](SYNTAX.md) covers every construct with examples (and `curios-text/src/parse.rs` is the ultimate source of truth). A few essentials that are easy to trip on from memory:

- Names are path-qualified with `/`: `Option/none`, `/std/Lst`, `/syn/Lst`; a leading `/` is absolute.
- `@x : T` is an implicit (type-erased) parameter; ordinary `x : T` is explicit; `use T` (or `use x : T`) is an **instance argument** filled by witness resolution.
- **Concepts / witnesses / instance arguments** are the ad-hoc-polymorphism layer (see [SYNTAX.md](SYNTAX.md#concepts-witnesses-and-instance-arguments)). A `concept` lowers to a `record` plus a `Concept` registry entry (`curios-core/src/concept.rs`) and per-field method wrappers; a `witness` is anonymous (no name, no `pub` — a second instance of a key is an ordinary concept-typed `let` passed via `use <term>`) and lowers to a compiler-named definition (`witness#N`) registered in a program-wide table keyed by `(concept, tuple of the rigid heads of the concept's input parameters)`. A concept's `use`-marked (superclass) fields leave the positional sequence in every concept literal: omitted → resolved as a witness goal, explicitly filled with a `use <term>` entry (`elaborate_struct` in `curios-core/src/elaborate.rs`). The `Plicity::Witness` binder is filled by the resolution engine in `curios-core/src/resolve.rs`; conversion and erasure stay plicity-blind. Registries mirror the `inductives`/`structures` pattern: carried on `Module`, seeded into each `Context`. `Show`/`Ord` live in `curios-text/std/`; `Eql` is homed in `/sys` (operator dispatch) and `Monad` in `/syn` (the postfix `!` desugars each site to `/syn/Monad/bind(action, continuation)` in `to_core/lower.rs` — every value body is a region root, there is no `let !` header, and the action's type resolves the witness), each with a `/std` facade. Higher-kinded concepts (e.g. `Monad(M : (Type) -> Type)`) are supported via the flex-apply imitation rule in `convert.rs` (`?M(?A) ≡ Option(Nat)` commits `?M := (A) => Option(A)`; exact arity, imitation-only, nominal rigid heads). Every infix operator except `&&`/`||` dispatches through the `/sys` operator concepts (`Add`/`Sub`/`Mul`/`Div`/`Rem`/`Eql`/`Cmp`, built as AST in `curios-text/src/prelude.rs`; the operator→concept table lives in `curios-core/src/sys_names.rs`, the rewrite in `elaborate_infix`) — there is no separate primitive-operator path, and codegen parity with the bare prims is pinned by `curios/src/tests/codegen/parity.rs`. The function-field sugar (`name(params) -> T` / `name(args) = body`, legal in every field list) is kept verbatim in the text AST (`func_params` on the field nodes) and undone in `to_core` — the parser never desugars it.
- `{}` is the unit type; `()` is the unit value.
- `record` exposes its representation (construct/project directly); `struct` keeps it private to the module — touch it only via exported helpers, or you hit a `PrivateRepresentation` error.

To run or test `.crs` code, use the CLI (`cargo run --package curios -- run …`) or one of the runnable examples in `curios/examples/` (`cargo run --package curios --example crs_eq`; the `crs_*` and `parse_*` examples drive `.crs` programs through the pipeline).

## Gotchas

- **Never edit `curios-binaryen/binaryen/`.** It is vendored upstream source, used unpatched. The re-vendoring procedure (which files to copy, how to bump the tag) is documented at the top of `curios-binaryen/build.rs`. Changes belong in `curios-binaryen/src/`, not the vendored tree.
- **Keep the slim launcher slim.** `cargo build --package curios-rt` must stay free of Cranelift and Binaryen; keep any new runtime dependency out of that crate's graph. A change can pass `--workspace --all-features` and still break `--package curios-rt` (or vice versa) — run both. For the full mechanism see [Crates, features, and the slim launcher](#crates-features-and-the-slim-launcher).
- **The codegen tests live in `curios/src/tests/codegen/`.** They execute emitted wasm, which needs the runtime (`curios-rt`); `curios-rt` depends only on `curios-abi`, not on `curios`, so `curios` depending on `curios-rt` is not a cycle — that's what lets these tests live alongside the rest of the integration suite instead of in a separate crate.
- **Generated `.wasm` files** are gitignored (`/*.wasm`); don't commit build output.
- **`Cargo.lock` is committed** — keep it in sync when changing dependencies.
