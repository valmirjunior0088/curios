# curios

A work-in-progress functional, **dependently-typed** programming language that compiles to WebAssembly.

curios is a small language with a full dependent type system: types can depend on values, propositions are first-class, and you can write machine-checked proofs alongside ordinary code. Programs compile through a series of typed intermediate representations down to WebAssembly and run on an embedded [wasmtime](https://wasmtime.dev/) engine, with an optimizing backend built on [Binaryen](https://github.com/WebAssembly/binaryen).

> **Status: early and experimental.** This is a research language under active development. The syntax, standard library, and compiler internals all change frequently, and there is no stability guarantee yet. If you enjoy poking at dependently-typed languages and compilers in progress, you're in the right place; if you need something production-ready, this isn't it (yet).

**[Try curios in your browser →](https://valmirjunior0088.github.io/curios/)** — no install needed, it typechecks and compiles to WebAssembly right there in the page.

## What's interesting about it

- **Full dependent types** — function and tuple types are Π/Σ types, so a value's type can mention earlier values (e.g. a length-indexed vector `Vec(T, n)`).
- **Propositions and proofs** — a separate `Prop` sort for proof-irrelevant propositions, with propositional equality (`Eq`) and the usual `refl`/`sym`/`trans`/`cong`/`subst` toolkit in the standard library.
- **Indexed inductive types** — declare your own data and proof families with index telescopes and per-constructor targets.
- **Type erasure** — implicit/erased arguments (marked `@`) and zero-cost newtypes carry type-level information that vanishes at runtime.
- **Compiles to WebAssembly** — a real lowering pipeline (parse → elaborate/typecheck → erase → CPS → optimize → emit), not an interpreter.
- **A practical standard library** — numbers (including `BigNat`), strings, lists, vectors, options/results, plus IO, files, TCP, HTTP, tasks, time, and JSON.

## A taste

A length-indexed vector whose `append` is checked to produce the right length:

```
pub induct Vec(T : Type) : (n : Nat) -> Type
| nil() : (0)
| cons(@m : Nat, x : T, xs : Vec(T, m)) : (m + 1)
end

pub rec append(@T : Type, @n : Nat, @m : Nat, v : Vec(T, n), w : Vec(T, m)) -> Vec(T, n + m) =
    match v : (v : Vec(T, k)) => Vec(T, k + m)
    | nil()          => w
    | cons(j, x, xs) => Vec/cons(x, append(xs, w))
    end;
```

And a proof — symmetry of equality, by matching on the single `refl` constructor:

```
pub let sym(@A : Type, @x : A, @y : A, p : Eq(x, y)) -> Eq(y, x) =
    match p : (q : Eq(A, s, t)) => Eq(t, s)
    | refl(z) => Eq/refl()
    end;
```

A minimal program (an entrypoint is a sequence of declarations followed by a final term):

```
use /std/{Io};

Io/print("Hello, world!\n")
```

See [SYNTAX.md](SYNTAX.md) for the full language reference.

## Getting started

### Prerequisites

- A recent Rust toolchain (the project uses edition 2024).
- A C++ toolchain and **CMake** — the default build compiles the vendored Binaryen optimizer. The first build takes a few minutes as a result.

Binaryen is built only by the crates that need it. For fast iteration on a single pipeline stage you can build e.g. `cargo build --package curios-core` — pure Rust, no Binaryen/CMake.

### Download a pre-built binary

If you'd rather not build from source, a pre-built `curios` for your platform is published on the [GitHub releases page](https://github.com/valmirjunior0088/curios/releases). It's a single self-contained binary with the launcher embedded. Download it and skip straight to [Run a program](#run-a-program).

### Build

The compiler embeds the slim runtime launcher at build time, so generate it with `make` first, then build with `cargo` as usual:

```sh
git clone https://github.com/valmirjunior0088/curios
cd curios
make curios/runtime                     # build the slim launcher the compiler embeds
cargo build --release --package curios
```

This produces `target/release/curios` — a single self-contained CLI with the launcher embedded. (If you build the compiler before the launcher, the build fails with a clear "couldn't read" error — run `make curios/runtime` and rebuild.)

### Run a program

Save the hello-world snippet above as `hello.crs`, then:

```sh
cargo run --package curios --release -- run hello.crs
```

## Using the CLI

The `curios` binary exposes three subcommands:

| Command                              | What it does                                                                         |
| -------------------------------------- | ------------------------------------------------------------------------------------ |
| `curios run <file.crs> [args...]`    | Compile and execute the program. Extra arguments are readable from `/std/Proc/args`. |
| `curios check <file.crs>`            | Type-check the entrypoint without running it.                                        |
| `curios compile <file.crs> [-o out]` | Compile to a self-contained native executable (default name: the input file stem).   |

Two global flags are useful while exploring:

- `--print[=STAGES]` dumps intermediate representations to stderr. With no value it prints all stages; or pass a comma-separated subset, e.g. `--print=core,wasm` (stages: `text,core,ersd,ersd-optm,cont,cont-optm,wasm`).
- `--timeout MILLIS` bounds the type-checker's reduction time (default `1000`).

```sh
cargo run --package curios --release -- check hello.crs --print=core
cargo run --package curios --release -- compile hello.crs -o hello
./hello
```

A compiled executable is the slim launcher stub — embedded inside `curios` itself — with the program's precompiled module appended to it, so it runs standalone. The launcher is baked in at compile time, so `curios` needs no companion files to build executables.

## Repository layout

A Cargo workspace of twelve crates, layered along the pipeline: **`curios-abi`** (host/guest wire ABI constants) and **`curios-base`** (spans, entropy, the `name!` macro, parser/printer monads) are the shared foundations; **`curios-wasm`** → **`curios-cont`** → **`curios-ersd`** → **`curios-core`** → **`curios-text`** are the pipeline stages (`text` → `core` → `ersd` → `cont` → `wasm`, each its own crate, code dependencies running the opposite direction of data flow); **`curios-pipeline`** is the pure driver chaining those stages (`compile_entrypoint`/`typecheck_entrypoint`); **`curios-js`** is a `wasm-bindgen` build of `curios-pipeline` for a browser playground; **`curios-binaryen`** is the vendored Binaryen optimizer; **`curios-rt`** is the runtime-only engine + launcher stub; and **`curios`** is the facade + driver + CLI, the only crate that links Cranelift and Binaryen. The standard library (`std`/`syn`) lives under **`curios-text`**, not `curios`.

For a full tour of the architecture, build/test workflow, and conventions, see [AGENTS.md](AGENTS.md). For the language itself, see [SYNTAX.md](SYNTAX.md). For what's built vs. planned, see [ROADMAP.md](ROADMAP.md).

## Contributing & feedback

curios is exploratory and moving fast, so the most useful contributions right now are bug reports, small example programs that break things, and feedback on the language design. Please open an issue at <https://github.com/valmirjunior0088/curios>.

curios is licensed under [Apache-2.0](LICENSE).
