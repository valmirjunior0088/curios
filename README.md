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
pub induct Vec(T : Type) : (n : Nat) -> Type
| nil() : (0)
| cons(@m : Nat, x : T, xs : Vec(T, m)) : (m + 1)
end
```

The compiler checks the relationship between each constructor and the resulting length. The erased parameter `@m` guides type checking, then disappears from the generated program.

Curios includes:

- Dependent function and tuple types
- Indexed inductive types
- A proof-irrelevant `Prop` universe
- Erased arguments and zero-cost type-level information
- Concepts and witnesses for ad-hoc polymorphism
- Pattern matching with exhaustiveness checking
- A standard library for collections, formatting, IO, networking, tasks, and JSON
- A complete lowering pipeline from source to WebAssembly

## Try it

The [browser playground](https://valmirjunior0088.github.io/curios/playground) runs the same compiler pipeline as the native CLI, entirely in your browser.

To use the CLI, download a binary from the [releases page](https://github.com/valmirjunior0088/curios/releases), then save this as `hello.crs`:

```crs
use /std/{Io};

Io/print("Hello, world!\n")
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
- [Compiler architecture and contributor guide](AGENTS.md)
- [Benchmark methodology and results](benchmarks/README.md)

## Contributing

Curios is still finding its shape. Bug reports, small programs that expose surprising behavior, and focused language-design feedback are especially useful. Open an issue on [GitHub](https://github.com/valmirjunior0088/curios/issues).

Licensed under [Apache 2.0](LICENSE).
