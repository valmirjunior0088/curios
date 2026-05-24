# Curios

Curios is a functional language with dependent types that compiles to WebAssembly. Most languages with dependent types evolved from proof assistants, where non-determinism is a property to be excluded rather than embraced - Curios inverts this, aiming to bring dependent function types (Π-types, λ-abstractions), dependent tuple types (Σ-types, dependent pairs), and dependent enumeration types (disjoint sets of atoms with dependent elimination semantics) to a programming context where non-determinism is simply part of daily life.

Dependent types pay off most in a handful of recurring patterns. Length-indexed collections rule out bounds errors by construction, replacing runtime panics with type-level guarantees. Typed format strings derive their argument list directly from the format value, eliminating a whole class of variadic bugs. Dependent records encode protocol state in the type itself, turning invalid transitions into compile-time errors rather than runtime failures.

Curios is an impure language, like OCaml. Side effects — terminal IO, file access, and other operations — are ordinary expressions that can appear anywhere in a program. The type system accommodates this by treating effectful operations as opaque at the type level: when the type checker encounters a term that performs IO during reduction, it raises a type error rather than attempting to evaluate the side effect.

## Installation

Download a pre-built binary for your platform from the [releases page](https://github.com/valmirjunior0088/curios/releases), or install directly from the repository:

```
cargo install --git https://github.com/valmirjunior0088/curios
```

Or, if you have a local clone:

```
cargo install --path .
```

## Usage

```
curios [--timeout MILLIS] [--check] [--print] <file>
```

- `--timeout` sets the type-checker's reduction timeout in milliseconds (default: 1000)
- `--check` runs the full compilation pipeline without executing the result, exiting with a non-zero status on failure (default: off)
- `--print` prints every intermediate representation — core, ersd, cont, and wasm — before executing (default: off)
- `<file>` is the path to an entrypoint file; a Curios source file whose last expression is the program's result

A minimal example:

```
-- hello.crs
let msg : Bin = "hello, world";
msg
```

```
curios hello.crs
```

## Examples

The `examples/` directory contains end-to-end Rust programs that drive the full compiler pipeline. Two are particularly instructive:

**Typed format strings** (`examples/crs_printf.rs`) — calls `fmt/printf` with a format string whose argument list is derived from the string's content at compile time:

```
pub mod fmt;
fmt/printf "%s is %d" "Alice" 30
-- output: "Alice is 30"
```

Passing the wrong type is a compile-time error, not a runtime failure:

```
pub mod fmt;
fmt/printf "%d" "Alice"
-- TypeMismatch: the format specifier %d expects Nat, but "Alice" has type Bin
```

**JSON codec** (`examples/crs_json_codec.rs`) — constructs a `json/Value` tree using the dependent sum type idiom, encodes it to a `Bin` string with `json/encode`, parses it back with `json/decode`, and asserts the output is byte-identical to the original. It exercises file-backed modules (`std`, `parser`, `json`), arrays, and nested dependent sum types together.

## Documentation

- [ARCHITECTURE.md](ARCHITECTURE.md) — full architectural overview of the compiler pipeline, from parsing through type checking, erasure, CPS lowering, and WebAssembly code generation, including a "Start Here" guide for newcomers.
- [SYNTAX.md](SYNTAX.md) — language syntax reference covering lexical basics, all term and type forms, primitive operations, and idioms for sum types and recursive types.
- [CRASH_COURSE.md](CRASH_COURSE.md) — guided introduction for Rust programmers, building from familiar constructs up to dependent function types and length-indexed vectors.
