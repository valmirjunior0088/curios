<div align="center">

# [Curios](https://valmirjunior0088.github.io/curios/)

**A small language, fully dependent.**

Curios is a dependently typed programming language that compiles to WebAssembly. Types can depend on values, proofs live beside ordinary code, and the compiler is happy to double-check your math homework.

[Playground](https://valmirjunior0088.github.io/curios/playground) · [Documentation](https://valmirjunior0088.github.io/curios/docs/curios/index.html) · [Language reference](documentation/SYNTAX.md) · [Usage](documentation/USAGE.md) · [Releases](https://github.com/valmirjunior0088/curios/releases) · [Roadmap](documentation/ROADMAP.md)

[![Build](https://github.com/valmirjunior0088/curios/actions/workflows/check.yml/badge.svg)](https://github.com/valmirjunior0088/curios/actions/workflows/check.yml)
[![Release](https://img.shields.io/github/v/release/valmirjunior0088/curios)](https://github.com/valmirjunior0088/curios/releases)
[![License](https://img.shields.io/badge/license-Apache--2.0-blue.svg)](LICENSE)

</div>

> **Status:** Curios is early, experimental, and under active development — syntax, standard library, and compiler may all change without notice. An independent kernel re-checks every compilation from the finished terms alone, but it is still being refined, and it is a second opinion rather than a proof of soundness.

## A taste

Here is the standard library's vector, which carries its own length around in its type:

```crs
pub induct Vec(T: Type): (Nat) -> pub Type
| nil(): (0)
| cons(@m: Nat, x: T, xs: Vec(T, m)): (m + 1)
end
```

That length is not a comment, and nobody has to remember to check it. Try telling the compiler that an empty vector holds one element:

```crs
use /std/{Nat, Vec};

let empty : Vec(Nat, 0) = Vec/nil();
let single : Vec(Nat, 1) = empty;
```

```text
while elaborating /single:
type mismatch
  inferred: Vec(Nat, 0)
  expected: Vec(Nat, 1)

   --> vector.crs:4:28
    4 | let single : Vec(Nat, 1) = empty;
      |                            ^^^^^
```

`Vec(Nat, 0)` and `Vec(Nat, 1)` are simply different types, so the off-by-one never reaches the generated program — there is nothing to test for, because there is nothing to run. And the `@m` that made that work does its thinking at compile time and then goes home: none of it survives into the WebAssembly.

## What you get

- Dependent function and tuple types, indexed inductive families, and pattern matching that works out exhaustiveness for you
- A cumulative universe hierarchy, with levels inferred rather than written by hand
- A proof-irrelevant `Prop`, so proofs weigh nothing at runtime
- Erased arguments — type-level information that guides checking and then vanishes from the output
- Concepts and witnesses for ad-hoc polymorphism
- A standard library for collections, formatting, IO, networking, tasks, and JSON
- One lowering pipeline from source to WebAssembly, whether you run it natively or in a browser tab

## Try it

The [browser playground](https://valmirjunior0088.github.io/curios/playground) runs the same compiler pipeline as the native CLI, entirely in your browser. No install, no account, nothing to uninstall afterwards.

For the CLI, this drops it into `~/.local/bin`:

```sh
curl -fsSL https://github.com/valmirjunior0088/curios/releases/latest/download/install.sh | sh
```

The installer takes no options and installs the release it shipped with, verified against that release's `checksums.txt` before it lands. Prebuilt binaries exist for Linux x86-64, Linux aarch64, and Apple Silicon; anywhere else, [build from source](#build-from-source). Every binary is on the [releases page](https://github.com/valmirjunior0088/curios/releases) too, if you would rather place one yourself.

Then save this as `hello.crs`:

```crs
/std/print("Hello, world!\n")
```

Run it:

```sh
curios run hello.crs
```

Or fold it into a standalone native executable, runtime and all:

```sh
curios compile hello.crs -o hello
./hello
```

Exit codes, the formatter, and the rest of the command line are in [Usage](documentation/USAGE.md).

## When one file is not enough

A `.crs` file is standalone wherever it sits: it needs no manifest, and no manifest above it captures it. A package is what you write once a program outgrows that.

```sh
curios new hello
cd hello
curios run
```

`curios new` names the package after its directory — checked before anything is written — and writes the smallest thing that runs: a `curios.toml`, and a `hello.crs` beside it. With `--lib` you get a `lib.crs` and no executable instead; a package is a program or a library until it says otherwise.

```toml
name = "hello"

[[executables]]
name = "hello"
```

From there, `run` and `compile` mean the same three things: with no argument, the package's sole executable; with an identifier, the one declared under that name; with a path, that file, standalone. The governing package is whichever one's `curios.toml` sits in the working directory — there is no searching around above you.

Dependencies are pinned exactly and accepted by hash, `curios curate` is the only part of the toolchain that touches the network, and packages developed together sit under an umbrella. All of that lives in [Usage](documentation/USAGE.md).

## Build from source

Building requires Rust, a C++ toolchain, and CMake. The first build compiles Binaryen from a verified source release and takes several minutes — a good moment to go read the [language reference](documentation/SYNTAX.md).

```sh
git clone https://github.com/valmirjunior0088/curios
cd curios
make curios/runtime
cargo build --release --package curios
```

The resulting CLI is `target/release/curios`.

## Go deeper

- [Language reference](documentation/SYNTAX.md) — the complete surface language
- [Usage](documentation/USAGE.md) — the complete command line and package reference
- [Development roadmap](documentation/ROADMAP.md)
- [Cross-cutting design decisions](documentation/DESIGN.md) — decisions scoped to one crate live in that crate's `README.md`
- [Benchmark methodology and results](benchmarks/README.md)

## Contributing

Curios is still finding its shape, which is the fun part. Bug reports, small programs that expose surprising behavior, and focused language-design feedback are especially useful. Open an issue on [GitHub](https://github.com/valmirjunior0088/curios/issues).

Licensed under [Apache 2.0](LICENSE).
