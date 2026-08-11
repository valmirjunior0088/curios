<div align="center">

# [Curios](https://valmirjunior0088.github.io/curios/)

**A small language, fully dependent.**

Curios is a dependently typed programming language that compiles to WebAssembly. Types can depend on values, proofs live beside ordinary code, and the compiler is happy to double-check your math homework.

[Playground](https://valmirjunior0088.github.io/curios/playground) · [Documentation](https://valmirjunior0088.github.io/curios/docs/curios/index.html) · [Language reference](documentation/SYNTAX.md) · [Releases](https://github.com/valmirjunior0088/curios/releases) · [Roadmap](documentation/ROADMAP.md)

[![Build](https://github.com/valmirjunior0088/curios/actions/workflows/check.yml/badge.svg)](https://github.com/valmirjunior0088/curios/actions/workflows/check.yml)
[![Release](https://img.shields.io/github/v/release/valmirjunior0088/curios)](https://github.com/valmirjunior0088/curios/releases)
[![License](https://img.shields.io/badge/license-Apache--2.0-blue.svg)](LICENSE)

</div>

> **Status:** Curios is early, experimental, and under active development. Its syntax, standard library, and compiler may change without notice.

## A taste

Here is a vector whose length is part of its type:

```crs
use /std/{Nat};

-- A vector indexed by its own length.
pub induct Vec(T : Type) : (n : Nat) -> pub Type
| nil() : (0)
| cons(@m : Nat, x : T, xs : Vec(T, m)) : (m + 1)
end
```

The compiler checks the relationship between each constructor and the resulting length. The erased parameter `@m` guides type checking, then disappears from the generated program.

Curios includes:

- Dependent function and tuple types
- Indexed inductive types
- A cumulative universe hierarchy with levels inferred rather than written
- A proof-irrelevant `Prop` universe
- Erased arguments and zero-cost type-level information
- Concepts and witnesses for ad-hoc polymorphism
- Pattern matching with exhaustiveness checking
- An independent kernel that re-checks every compilation from the finished terms alone <br> *(still being refined, and a second opinion rather than a proof of soundness)*
- A standard library for collections, formatting, IO, networking, tasks, and JSON
- A complete lowering pipeline from source to WebAssembly

## Try it

The [browser playground](https://valmirjunior0088.github.io/curios/playground) runs the same compiler pipeline as the native CLI, entirely in your browser.

To use the CLI, install it into `~/.local/bin`:

```sh
curl -fsSL https://github.com/valmirjunior0088/curios/releases/latest/download/install.sh | sh
```

The installer takes no options and installs the release it shipped with, verifying the download against that release's `checksums.txt`. Prebuilt binaries exist for Linux x86-64, Linux aarch64, and Apple Silicon; anywhere else, [build from source](#build-from-source). Every binary is also on the [releases page](https://github.com/valmirjunior0088/curios/releases) if you would rather place one yourself.

Then save this as `hello.crs`:

```crs
/std/print("Hello, world!\n")
```

Run it directly:

```sh
curios run hello.crs
```

Or compile it into a standalone native executable:

```sh
curios compile hello.crs -o hello
./hello
```

Exit codes are a tri-state: `0` means the program compiled and (for `run`) exited 0 itself — a running program's own exit code passes through untouched — `2` means the program contains written goals (`?`) and their report was printed to stderr, and `1` means a hard error. Tooling can therefore distinguish "here is your goal batch" from "something is wrong" without parsing stderr.

`curios format <files…>` rewrites sources into the one canonical style, in place; `--check` writes nothing and exits nonzero when any file would change. Formatting is verified before anything is written — the output must reparse to exactly the same program, with every comment preserved — so a formatter defect refuses rather than corrupts.

## Packages

A `.crs` file is standalone wherever it sits: it needs no manifest, and no manifest above it captures it. A package is what you write once a program outgrows that.

```sh
curios new hello
cd hello
curios run
```

`curios new` names the package after its directory — checked before anything is written — and writes the smallest thing that runs: a `curios.toml`, and a `hello.crs` beside it. With `--lib` it writes a `lib.crs` and declares no executable instead; a package is a program or a library until it says otherwise.

```toml
name = "hello"

[[executables]]
name = "hello"
```

`run` and `compile` take the same three forms, so what a bare invocation means never depends on which one you asked. With no argument, the governing package's sole executable — or the one `default` names, when it declares several. With an identifier, the executable declared under that name. With anything ending in `.crs` or holding a path separator, that file, standalone.

A dependency is pinned exactly, and its name is how every consumer refers to it: a package named `json` mounts at `/json`, and no consumer may rename it, which is what lets two dependents on one package share it instead of duplicating it. A `git` row requires all three of `url`, `rev` and `hash`; a `path` row requires only `path`.

```toml
name = "app"

[dependencies]
json = { source = "git", url = "https://github.com/you/json", rev = "…", hash = "c1:…" }
shape = { source = "path", path = "../shape" }
```

`curios curate` materializes what the manifests reference. It is the only part of the toolchain that reaches the network — the compiler itself never fetches — and a delivered tree is accepted against its `hash` whatever transport produced it, so a mirror is no weaker than the origin.

Everything generated goes under `.curios/`, beside the governing manifest: built executables, materialized sources, and compiled units. It is the only directory the toolchain writes into.

Packages developed together sit under an umbrella, which declares `members` rather than a `name`, and may declare a `catalog` of pins its members draw on. An umbrella governs a package only if it enumerates it, so a directory nothing enumerates is governed by nothing above it, however deep it sits.

The governing package is the one whose `curios.toml` sits in the working directory — there is no search above it, so what a command compiles is whatever an `ls` shows. Only the umbrella is looked for further up, and only an umbrella that enumerates you governs you.

Two flags apply to every subcommand: `--manifest <PATH>` names the governing package's manifest instead of using the working directory's, and `--unit <DIR>` mounts a package ahead of the entry program without a manifest edge, repeated in dependency order.

## Build from source

Building requires Rust, a C++ toolchain, and CMake. The first build compiles Binaryen from a verified source release and may take several minutes.

```sh
git clone https://github.com/valmirjunior0088/curios
cd curios
make curios/runtime
cargo build --release --package curios
```

The resulting CLI is `target/release/curios`.

## Learn more

- [Language reference](documentation/SYNTAX.md)
- [Development roadmap](documentation/ROADMAP.md)
- [Cross-cutting design decisions](documentation/DESIGN.md) — decisions scoped to one crate live in that crate's `README.md`
- [Benchmark methodology and results](benchmarks/README.md)

## Contributing

Curios is still finding its shape. Bug reports, small programs that expose surprising behavior, and focused language-design feedback are especially useful. Open an issue on [GitHub](https://github.com/valmirjunior0088/curios/issues).

Licensed under [Apache 2.0](LICENSE).
