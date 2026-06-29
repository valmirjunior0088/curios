# AGENTS.md

Agent guide to `curios`. Operational reference plus an orientation map. Read this before touching the code.

## Overview

`curios` is a work-in-progress functional, dependently-typed programming language, implemented in Rust (edition 2024, ~70k lines). It compiles `.crs` source through a series of intermediate representations down to WebAssembly, and runs the result on an embedded `wasmtime` engine.

Two languages live in this repo: **Rust** (the compiler) and **curios** itself (the object language, with a standard library under `std/`). Work touches one or both.

## Before you write code

Refresh the relevant reference into working memory _before_ writing, every time — do not rely on a stale recollection from earlier in the session or from training.

- **Writing curios (`.crs`)?** Read [SYNTAX.md](SYNTAX.md) in full first. The surface language has many specialized forms (per-scrutinee `match` shapes, motives, glued literal signs, `let !` do-notation) that are easy to get subtly wrong from memory.
- **Writing Rust (the compiler)?** Re-read [The pipeline](#the-pipeline) and [Where things live](#where-things-live) below, then open the `//!` module docs of the stage(s) you are touching, so the full architecture and the stage's local invariants are fresh. A change in one stage usually has obligations in the next.

This is cheap insurance: both languages reward precision and punish half-remembered syntax or architecture.

## The pipeline

The compiler is a chain of stages, each its own module under `src/`. This chain is the backbone — when orienting yourself in unfamiliar code, find the stage first.

```
.crs source
  → text/   parse surface syntax; lower to core (text/to_core)
  → core/   elaborate, typecheck, reduce/convert, then erase types
  → ersd/   "erased" IR (types gone); lower to continuations (ersd/to_cont)
  → cont/   continuation-passing-style IR; emit wasm (cont/to_wasm)
  → optm/   optimization passes over the cont IR
  → wasm/   wasm module model, encoder/writer, parser
  → run/    execute the module on wasmtime (feature "run")
```

`binaryen/` is vendored C++ (the Binaryen wasm optimizer), linked via FFI from `src/binaryen/` to optimize emitted wasm.

## Where things live

| Path                                | Role                                                                                         |
| ----------------------------------- | -------------------------------------------------------------------------------------------- |
| `src/text/`                         | Lexer/parser, surface AST, lowering to core (`to_core/`)                                     |
| `src/core/`                         | Core language: elaboration, typing, reduction, conversion, inductives, erasure, zonking      |
| `src/ersd/`                         | Erased IR (post type-erasure); lowering to CPS (`to_cont/`)                                  |
| `src/cont/`                         | Continuation-passing IR; wasm emission (`to_wasm/`)                                          |
| `src/optm/`                         | Optimization passes over cont IR (inlining, DCE, copy/tag/jump threading, tail recursion, …) |
| `src/wasm/`                         | Wasm module model, parser, binary writer/encoder                                             |
| `src/binaryen/`                     | FFI bindings to vendored Binaryen (`sys.rs`)                                                 |
| `src/run/`                          | wasmtime engine, host functions, OS + mock hosts (feature `run`)                             |
| `src/monads/`                       | Parser/printer monad combinators                                                             |
| `src/cli.rs`                        | clap-based CLI (feature `cli`)                                                               |
| `src/{span,entropy,wire,macros}.rs` | Shared utilities                                                                             |
| `src/tests/`                        | Cross-stage integration tests                                                                |
| `std/`                              | curios standard library (`*.crs`), indexed by `std.crs`                                      |
| `syn/`                              | Smaller curios support library, indexed by `syn.crs`                                         |
| `examples/`                         | Runnable Rust examples driving the compiler (`parse_*`, `inline_*`, `crs_*`, `bench_*`)      |
| `binaryen/`                         | Vendored Binaryen C++ source (do not edit — see Gotchas)                                     |
| `build.rs`                          | Builds and statically links Binaryen                                                         |

## Build & test

```sh
cargo build                       # default features: binaryen + cli (+ run)
cargo test --all-targets --all-features
cargo run -- <args>               # invoke the CLI
```

### Feature flags

| Feature    | Enables                                                               |
| ---------- | --------------------------------------------------------------------- |
| `run`      | wasmtime execution + the networking/IO host stack                     |
| `binaryen` | Builds & links the vendored Binaryen optimizer (slow C++/CMake build) |
| `cli`      | clap CLI (implies `run`)                                              |
| `default`  | `binaryen` + `cli`                                                    |

The `binaryen` feature compiles a large C++ project via CMake on first build — expect minutes, not seconds, and a C++ toolchain + CMake on the machine. To skip it during fast iteration, build with `--no-default-features` and select only the features you need (e.g. `--features run`). Note CI runs `--all-features`, so the final check must include `binaryen`.

## The done bar

Before considering any change complete, run the same gate CI enforces, in order. All four must pass. `clippy` runs with warnings denied.

```sh
cargo fmt --all -- --check
cargo check  --all-targets --all-features
cargo clippy --all-targets --all-features      # CI sets RUSTFLAGS="-Dwarnings"
cargo test   --all-targets --all-features
```

There is no `rustfmt.toml` or `clippy.toml` — stock toolchain defaults apply. Run `cargo fmt --all` to fix formatting rather than hand-aligning.

## Rust conventions

- **Module pattern.** A module is `foo.rs` (declaring submodules and re-exporting them, usually `mod x; pub use x::*;`) alongside a sibling `foo/` directory. Match this when adding modules; keep files focused (the history favors splitting large files into thematic submodules).
- **Tests.** Unit tests live beside the code as a `*_tests.rs` submodule gated with `#[cfg(test)]` (e.g. `core/typing.rs` ↔ `core/typing_tests.rs`). Cross-stage integration tests live in `src/tests/`.
- **Docs.** Use `//!` module-level doc comments to explain a module's purpose; `build.rs` is a good model.
- **Commits.** Imperative mood, capitalized, descriptive — e.g. "Split term.rs and elaborate.rs into focused submodules". Do not patch vendored files in a feature commit.

## Writing curios (`.crs`)

The standard library under `std/` is the reference for idiomatic curios. Each module is one file (`std/Foo.crs`) and must be registered in `std.crs` (`pub mod Foo; pub use Foo/{let Foo};`). The same applies to `syn/` via `syn.crs`.

For the full surface language — every construct, with examples — see [SYNTAX.md](SYNTAX.md). The cheat sheet below is just enough to get oriented; `src/text/parse.rs` is the ultimate source of truth.

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

To run or test `.crs` code, use the CLI (`cargo run -- …`) or follow an existing example in `examples/` (the `crs_*` and `parse_*` examples drive `.crs` programs through the pipeline).

## Gotchas

- **Never edit `binaryen/`.** It is vendored upstream source, used unpatched. The re-vendoring procedure (which files to copy, how to bump the tag) is documented at the top of `build.rs`. Changes belong in `src/binaryen/`, not the vendored tree.
- **Feature-gated code.** Much of `run/`, `cli`, and `binaryen` is behind cfg flags. A change can compile under default features and still break `--all-features --all-targets` (or vice versa) — always run the done-bar with `--all-features --all-targets`.
- **Generated `.wasm` files** are gitignored (`/*.wasm`); don't commit build output.
- **`Cargo.lock` is committed** — keep it in sync when changing dependencies.
