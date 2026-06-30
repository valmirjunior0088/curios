# AGENTS.md

Agent guide to `curios`. Operational reference plus an orientation map. Read this before touching the code.

## Overview

`curios` is a work-in-progress functional, dependently-typed programming language, implemented in Rust (edition 2024, ~70k lines). It compiles `.crs` source through a series of intermediate representations down to WebAssembly, and runs the result on an embedded `wasmtime` engine.

The repo is a **Cargo workspace** (virtual manifest at the root) of four crates:

- **`curios`** — the compiler library: the whole `.crs` → wasm pipeline, plus the embedded standard library (`curios/std/`, `curios/syn/`). Pure Rust; no wasmtime.
- **`curios-binaryen`** — FFI to the vendored Binaryen optimizer (`curios-binaryen/binaryen/`).
- **`curios-runtime`** — runtime-only engine (lib) + the launcher stub (bin `curios-runtime`). Deserializes a precompiled module and runs it on wasmtime; **never** links Cranelift or Binaryen.
- **`curios-compiler`** — the CLI (bin `curios-compiler`) + a lib with the compile / precompile (`to_cwasm`) / run-from-source helpers. The **only** crate that links Cranelift (via `wasmtime`'s `cranelift` feature) and Binaryen.

The JIT-vs-deserialize split is a *crate boundary*, not a feature flag: `cargo build --package curios-runtime` yields a slim launcher with no Cranelift/Binaryen. The CLI's `wasmtime` dependency adds `cranelift`, which feature-unification makes available in compiler builds; both crates share one `wasmtime` (version pinned once in `[workspace.dependencies]`) so the `.cwasm` a `curios-compiler` produces matches what `curios-runtime` deserializes.

Two languages live in this repo: **Rust** (the compiler) and **curios** itself (the object language, with a standard library under `curios/std/`). Work touches one or both.

## Before you write code

Refresh the relevant reference into working memory _before_ writing, every time — do not rely on a stale recollection from earlier in the session or from training.

- **Writing curios (`.crs`)?** Read [SYNTAX.md](SYNTAX.md) in full first. The surface language has many specialized forms (per-scrutinee `match` shapes, motives, glued literal signs, `let !` do-notation) that are easy to get subtly wrong from memory.
- **Writing Rust (the compiler)?** Re-read [The pipeline](#the-pipeline) and [Where things live](#where-things-live) below, then open the `//!` module docs of the stage(s) you are touching, so the full architecture and the stage's local invariants are fresh. A change in one stage usually has obligations in the next.

This is cheap insurance: both languages reward precision and punish half-remembered syntax or architecture.

## The pipeline

The compiler is a chain of stages, each its own module under `curios/src/`. This chain is the backbone — when orienting yourself in unfamiliar code, find the stage first.

```
.crs source                                         (crate: curios)
  → text/   parse surface syntax; lower to core (text/to_core)
  → core/   elaborate, typecheck, reduce/convert, then erase types
  → ersd/   "erased" IR (types gone); ersd→ersd optimization (ersd/optm), then lower to continuations (ersd/to_cont)
  → cont/   continuation-passing-style IR; cont→cont optimization (cont/optm), then emit wasm (cont/to_wasm)
  → wasm/   wasm module model, encoder/writer, parser
  → driver.rs   the pipeline driver: compile_entrypoint / typecheck_entrypoint / Stage

then, in curios-compiler:
  → optimize (curios-binaryen) + precompile to .cwasm (to_cwasm)
  → run on wasmtime via curios-runtime::run_bytes  (or bundle into an executable)
```

`curios-binaryen/binaryen/` is vendored C++ (the Binaryen wasm optimizer), linked via FFI from `curios-binaryen/src/` to optimize emitted wasm. The wasmtime engine + host stack live in `curios-runtime/src/` (`run_bytes` deserializes a `.cwasm` and runs it; `instantiate` wires the `env.io_*` host imports).

## Where things live

| Path                                          | Role                                                                                         |
| --------------------------------------------- | -------------------------------------------------------------------------------------------- |
| `curios/src/text/`                            | Lexer/parser, surface AST, lowering to core (`to_core/`)                                     |
| `curios/src/core/`                            | Core language: elaboration, typing, reduction, conversion, inductives, erasure, zonking      |
| `curios/src/ersd/`                            | Erased IR (post type-erasure); ersd→ersd optimization (`optm/`: prune, accumulators, offsets); lowering to CPS (`to_cont/`) |
| `curios/src/cont/`                            | Continuation-passing IR; cont→cont optimization (`optm/`: inlining, DCE, copy/tag/jump threading, tail recursion, …); wasm emission (`to_wasm/`) |
| `curios/src/wasm/`                            | Wasm module model, parser, binary writer/encoder                                             |
| `curios/src/driver.rs`                        | Pipeline driver: `compile_entrypoint`, `typecheck_entrypoint`, `Stage`                       |
| `curios/src/monads/`                          | Parser/printer monad combinators                                                             |
| `curios/src/{span,entropy,wire,macros}.rs`    | Shared utilities (`wire` = host↔guest ABI constants)                                         |
| `curios/{std,syn}/`                           | curios standard / support libraries (`*.crs`), indexed by `curios/{std,syn}.crs`            |
| `curios/examples/`                            | Pipeline-only Rust examples (`bench_check`, `inline_wasm`)                                    |
| `curios-binaryen/src/`                        | FFI bindings to vendored Binaryen (`sys.rs`); `optimize(bytes)`; `build.rs` links it         |
| `curios-binaryen/binaryen/`                   | Vendored Binaryen C++ source (do not edit — see Gotchas)                                      |
| `curios-runtime/src/`                         | wasmtime engine + host stack (`run_bytes`, `instantiate`, `shared_engine`); OS + mock hosts  |
| `curios-runtime/src/main.rs`                  | The launcher stub (bin `curios-runtime`): reads its appended `.cwasm` tail and runs it        |
| `curios-compiler/src/lib.rs`                  | `to_cwasm`, `run_wasm`, run-from-source helpers; re-exports `Stage`/`MockHost`/`OsHost`       |
| `curios-compiler/src/main.rs`                 | clap-based CLI (bin `curios-compiler`) incl. the `compile`→executable bundler                 |
| `Makefile`                                    | Builds the slim `curios-runtime` and copies it to `curios-compiler/runtime` for `main.rs` to `include_bytes!` |
| `curios-compiler/src/tests/`                  | Cross-stage integration tests (incl. relocated `codegen/` tests)                             |
| `curios-compiler/examples/`                   | Runnable examples that execute (`parse_*`, `inline_*`, `crs_*`, `bench_parse`)               |
| `curios-compiler/tests/bundle.rs`             | Gated (`#[ignore]`) end-to-end test of the `compile`→executable path                          |

## Build & test

**Run `make curios-compiler/runtime` first, then build with `cargo` as usual.** `curios-compiler` embeds the slim launcher via `include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/runtime"))`, so the launcher must sit at `curios-compiler/runtime` before the compiler is built. `make curios-compiler/runtime` builds the runtime in isolation (which keeps it slim, see below) and copies it there; everything else is plain `cargo`. If `curios-compiler/runtime` is missing, the build **fails** at the `include_bytes!` with a clear "couldn't read" error — run `make curios-compiler/runtime` and rebuild.

```sh
make curios-compiler/runtime                  # build the slim launcher, place it for embedding
cargo build --package curios-compiler         # the CLI (Binaryen C++ build on first run)
cargo run   --package curios-compiler -- <args> # invoke the CLI
cargo test --workspace --all-targets --all-features
```

`curios-compiler compile foo.crs` produces a native executable `foo` (the embedded launcher with the program's `.cwasm` appended — no launcher file is consulted at compile time); `curios-compiler run foo.crs` compiles and runs in-process; `curios-compiler check foo.crs` type-checks only.

### Crates, features, and the slim launcher

There are **no Cargo features** on the workspace crates. The JIT/Cranelift split is a crate boundary instead:

- `curios-runtime` declares only a runtime-only `wasmtime` (no `cranelift`) and never depends on `curios-binaryen`. **`cargo build --package curios-runtime` is a slim, Cranelift/Binaryen-free launcher** — that is the build embedded into the compiler as the stub.
- `curios-compiler` adds the `cranelift` feature to its own `wasmtime` dependency and depends on `curios-binaryen`. Feature unification makes Cranelift available throughout a `curios-compiler` build (and a `--workspace` build), so the `curios-runtime` *bin* produced by a `--workspace` build is **not** the slim one. This is why `make curios-compiler/runtime` builds the launcher with an isolated `cargo build --release --package curios-runtime` *before* the compiler and copies it to `curios-compiler/runtime`: building it alone keeps Cranelift out. Do not hand-build the launcher via `--workspace` and expect it to be slim — it will be the fat (Cranelift-linked) one.
- `cargo check --package curios-runtime` is part of the done bar precisely because `--workspace --all-features` cannot prove the runtime-only configuration still compiles.

Building `curios-binaryen` compiles a large C++ project via CMake on first build — expect minutes, not seconds, and a C++ toolchain + CMake on the machine. Anything depending on it (`curios-binaryen`, `curios-compiler`, the whole `--workspace`) pays that cost once; `curios` and `curios-runtime` on their own do not.

## The done bar

Before considering any change complete, run the same gate CI enforces, in order. All five must pass. `clippy` runs with warnings denied. Run `make curios-compiler/runtime` first if you have not already this session — the `check`/`clippy`/`test` steps compile `curios-compiler`, which `include_bytes!`s the launcher from `curios-compiler/runtime` (this isolated runtime build also proves the slim runtime-only config still compiles, so no separate isolated check is needed).

```sh
make curios-compiler/runtime                               # provide the launcher curios-compiler embeds
cargo fmt --all -- --check
cargo check  --workspace --all-targets --all-features
cargo clippy --workspace --all-targets --all-features      # CI sets RUSTFLAGS="-Dwarnings"
cargo test   --workspace --all-targets --all-features
```

The first line is load-bearing twice over: it provides the launcher binary `main.rs` embeds (at `curios-compiler/runtime`), and — because `--workspace --all-features` unifies Cranelift into the shared `wasmtime` — the isolated `--package curios-runtime` build `make curios-compiler/runtime` runs is also the only thing that proves the slim launcher configuration still compiles.

There is no `rustfmt.toml` or `clippy.toml` — stock toolchain defaults apply. Run `cargo fmt --all` to fix formatting rather than hand-aligning.

## Rust conventions

- **Module pattern.** A module is `foo.rs` (declaring submodules and re-exporting them, usually `mod x; pub use x::*;`) alongside a sibling `foo/` directory. Match this when adding modules; keep files focused (the history favors splitting large files into thematic submodules).
- **Tests.** Unit tests live beside the code as a `*_tests.rs` submodule gated with `#[cfg(test)]` (e.g. `core/typing.rs` ↔ `core/typing_tests.rs`). Cross-stage integration tests (which compile and run programs) live in `curios-compiler/src/tests/`.
- **Docs.** Use `//!` module-level doc comments to explain a module's purpose; `curios-binaryen/build.rs` is a good model.
- **Commits.** Imperative mood, capitalized, descriptive — e.g. "Split term.rs and elaborate.rs into focused submodules". Do not patch vendored files in a feature commit.

## Writing curios (`.crs`)

The standard library under `curios/std/` is the reference for idiomatic curios. Each module is one file (`curios/std/Foo.crs`) and must be registered in two places: `curios/std.crs` (`pub mod Foo; pub use Foo/{let Foo};`) **and** the `include_str!` table in `curios/src/text/prelude.rs` (the modules are embedded into the compiler at build time, not read from disk). The same applies to `curios/syn/` via `curios/syn.crs`.

For the full surface language — every construct, with examples — see [SYNTAX.md](SYNTAX.md). The cheat sheet below is just enough to get oriented; `curios/src/text/parse.rs` is the ultimate source of truth.

Syntax cheat-sheet:

```
use /std/{Bln, Nat};            -- import names from a module (paths use `/`)

pub induct Option(A : Type)     -- inductive type declaration
| some(A)
| none()
end

pub use Option/*;               -- re-export constructors

pub record Pair(A : Type, B : Type) {   -- transparent struct: representation is public
    fst : A,
    snd : B
}

pub record Meters { Nat }       -- single unnamed field; newtype, project with `.0`

pub struct Token { Bin }        -- opaque struct: representation private to this module

pub let mk_pair(@A : Type, @B : Type, a : A, b : B) -> Pair(A, B) =
    Pair {                      -- construct by field; project with `p.fst`, `p.snd`
        fst = a,
        snd = b
    };

pub let map(@A : Type, @B : Type, f : (A) -> B, m : Option(A)) -> Option(B) =
    match m                     -- `@` marks an implicit/erased argument
    | some(a) => Option/some(f(a))
    | none()  => Option/none()
    end;

pub rec len(@A : Type, l : Lst(A)) -> Nat =   -- `rec` for recursive defs
    match l
    | nil()        => 0
    | cons(_, t)   => len(t) + 1
    end;
```

- `record` declares a struct with a **public** representation (callers can construct and project it directly); `struct` declares one whose representation is **private** to its module — construct/project it only via exported helpers, or you hit a `PrivateRepresentation` error. A single unnamed field (`record Meters { Nat }`) is a newtype, projected with `.0`, and erases to the bare field at runtime.
- `@x : T` is an implicit (type-erased) parameter; ordinary `x : T` is explicit.
- `{}` is the unit type; `()` is the unit value.
- Names are path-qualified with `/`: `Option/none`, `/std/Lst`, `/syn/Lst`.
- A `match` may carry a motive annotation: `match l : Lst(B) | … end`.
- Local recursion uses `rec go(...) -> T = ...;` then a call to `go(...)`.

To run or test `.crs` code, use the CLI (`cargo run --package curios-compiler -- run …`) or follow an existing example in `curios-compiler/examples/` (the `crs_*` and `parse_*` examples drive `.crs` programs through the pipeline).

## Gotchas

- **Never edit `curios-binaryen/binaryen/`.** It is vendored upstream source, used unpatched. The re-vendoring procedure (which files to copy, how to bump the tag) is documented at the top of `curios-binaryen/build.rs`. Changes belong in `curios-binaryen/src/`, not the vendored tree.
- **Keep the slim launcher slim.** `cargo build --package curios-runtime` must stay free of Cranelift and Binaryen; if you add a runtime dependency, keep it out of that crate's graph or you defeat the embedded launcher. A change can pass `--workspace --all-features` and still break `--package curios-runtime` (or vice versa) — run both. For the full mechanism (why it's a separate build, how `make curios-compiler/runtime` embeds it) see [Crates, features, and the slim launcher](#crates-features-and-the-slim-launcher).
- **The codegen tests live in `curios-compiler`, not `curios`.** They execute emitted wasm, which needs the runtime; `curios` cannot depend on the runtime (that would be a cycle), so `curios-compiler/src/tests/codegen/` is their home even though they test `curios`'s `cont::to_wasm`.
- **Generated `.wasm` files** are gitignored (`/*.wasm`); don't commit build output.
- **`Cargo.lock` is committed** — keep it in sync when changing dependencies.
