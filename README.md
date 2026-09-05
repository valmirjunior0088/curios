<div align="center">

# [Curios](https://valmirjunior0088.github.io/curios/)

**A small language, fully dependent.**

Curios is a dependently typed programming language that compiles to WebAssembly. Types can depend on values, proofs live beside ordinary code, and the compiler is happy to double-check your math homework.

[Playground](https://valmirjunior0088.github.io/curios/playground) · [Documentation](https://valmirjunior0088.github.io/curios/docs/curios/index.html) · [Language reference](documentation/syntax.md) · [Usage](documentation/usage.md) · [Releases](https://github.com/valmirjunior0088/curios/releases) · [Roadmap](documentation/roadmap.md)

[![Build](https://github.com/valmirjunior0088/curios/actions/workflows/check.yml/badge.svg)](https://github.com/valmirjunior0088/curios/actions/workflows/check.yml)
[![Release](https://img.shields.io/github/v/release/valmirjunior0088/curios)](https://github.com/valmirjunior0088/curios/releases)
[![License](https://img.shields.io/badge/license-Apache--2.0-blue.svg)](LICENSE)

</div>

> **Status:** Curios is early, experimental, and under active development — syntax, standard library, and compiler may all change without notice. An independent kernel re-checks every compilation from the finished terms alone; what it covers, and what each rule it applies rests on, is tracked in the [soundness perimeter](documentation/soundness).

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

let empty: Vec(Nat, 0) = Vec/nil();
let single: Vec(Nat, 1) = empty;
```

```text
while elaborating /single:
type mismatch
  inferred: Vec(Nat, 0)
  expected: Vec(Nat, 1)

   --> vector.crs:4:27
    4 | let single: Vec(Nat, 1) = empty;
      |                           ^^^^^
```

`Vec(Nat, 0)` and `Vec(Nat, 1)` are simply different types, so the off-by-one never reaches the generated program — there is nothing to test for, because there is nothing to run. And the `@m` that made that work does its thinking at compile time and then goes home: none of it survives into the WebAssembly.

## What you get

- Dependent function and tuple types, indexed inductive families, and pattern matching that works out exhaustiveness for you
- A cumulative universe hierarchy, with levels inferred rather than written by hand
- A proof-irrelevant `Prop`, so proofs weigh nothing at runtime
- Erased arguments — type-level information that guides checking and then vanishes from the output
- Concepts and witnesses for ad-hoc polymorphism
- A standard library for collections, formatting, IO, networking, tasks, time, randomness, arbitrary-precision integers, JSON, and TOML
- One lowering pipeline from source to WebAssembly, whether you run it natively or in a browser tab

## Where it is going

Curios aims to be an ergonomic proof assistant and an ergonomic functional programming language at once, and the totality obligations are what let one language be both: general recursion stays unrestricted wherever a program uses it, and is removed from exactly the positions where erasure would turn it into a logical hole. Reduction is not strongly normalizing and values are not canonical — a program may diverge, while a proof may not. The long-term objective is a self-hosting compiler — every language-specific stage from source text to raw Wasm bytes written in Curios — running on the retained Rust host, which already serves the native product and the browser product from one backend.

## Try it

The [browser playground](https://valmirjunior0088.github.io/curios/playground) runs the same compiler pipeline as the native CLI, entirely in your browser. No install, no account, nothing to uninstall afterwards.

For the CLI, this drops it into `~/.local/bin`:

```sh
curl -fsSL https://github.com/valmirjunior0088/curios/releases/latest/download/install.sh | sh
```

No options, and it checks what it downloaded against the release's `checksums.txt` before installing it. Linux x86-64, Linux aarch64, and Apple Silicon have prebuilt binaries — anywhere else, [build from source](#build-from-source).

Then save this as `hello.crs`:

```crs
/std/print("Hello, world!\n")
```

Run it:

```sh
curios run hello.crs
```

## When one file is not enough

A `.crs` file is standalone wherever it sits: it needs no manifest, and no manifest above it captures it. A package is what you write once a program outgrows that.

```sh
curios new hello
cd hello
curios run
```

That gives you a whole package, and the manifest is one line — `name = "hello"`. Everything else is found rather than declared: `lib.crs` is the library, `exe.crs` is the program.

```crs
-- lib.crs
use /std/{Str};

pub let message: Str =
    "Hello, world!";
```
```crs
-- exe.crs
use /std/{Fmt};
use /hello/{message};

Fmt/print("%\n")(message)
```

Note what the program imports. `lib` and `exe` are spellings nothing can refer to, so `/hello` is unambiguously the *mount* — a package's own name never has to mean two things at once. Delete either file and the package is happily just the other one.

A package is also what gets folded into a standalone native executable, runtime and all:

```sh
curios compile -o hello
./hello
```

Dependencies, umbrellas, extra executables, and the rest of the command line live in [Usage](documentation/usage.md).

## Build from source

Building requires Rust (`rustup` installs the pinned toolchain from `rust-toolchain.toml` on first use), a C++ toolchain, and CMake. The first build compiles Binaryen from a verified source release and takes several minutes — a good moment to go read the [language reference](documentation/syntax.md).

```sh
git clone https://github.com/valmirjunior0088/curios
cd curios
cargo x build
```

The resulting CLI is `target/release/curios`. The build has two stages — a slim runtime launcher, then the compiler that embeds it — and `cargo x build` runs both in order. The documentation a release ships is built the same way: `cargo x rust-docs` renders the crates' own under `target/doc`, and `cargo x std-docs` renders the standard library's pages under `curios-prelude-archive/.artifacts/documentation`, read off the prelude image the compiler was built with.

## Go deeper

- [Language reference](documentation/syntax.md) — the complete surface language, when you want to know what something means or how to spell it
- [Usage](documentation/usage.md) — every subcommand, flag, and package concept the command line offers
- [Design decisions](documentation/design) — one file per decision, `language/` for what Curios is and `toolchain/` for how it is built and run, when you want to know *why* Curios is the way it is; a decision scoped to one crate lives in that crate's `README.md`
- [Soundness perimeter](documentation/soundness) — every rule that can admit a term, what it assumes, and how far it has actually been checked; [the claim it stands for](documentation/design/language/the-soundness-perimeter.md), and how to read a grade
- [Development roadmap](documentation/roadmap.md) — what exists, what is pending, and the specifications for the pending half
- [Benchmark methodology and results](benchmarks/README.md)

## Contributing

Curios is still finding its shape, which is the fun part. Bug reports, small programs that expose surprising behavior, and focused language-design feedback are especially useful. Open an issue on [GitHub](https://github.com/valmirjunior0088/curios/issues).

Licensed under [Apache 2.0](LICENSE).
