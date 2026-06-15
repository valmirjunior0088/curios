# Curios

Curios is a functional language with dependent types that compiles to WebAssembly. Most languages with dependent types evolved from proof assistants, where non-determinism is a property to be excluded rather than embraced - Curios inverts this, aiming to bring dependent function types (Π-types, λ-abstractions), dependent tuple types (Σ-types, dependent pairs), and nominal sum types (inductive unions with dependent elimination semantics) to a programming context where non-determinism is simply part of daily life.

Dependent types pay off most in a handful of recurring patterns. Length-indexed collections rule out bounds errors by construction, replacing runtime panics with type-level guarantees. Typed format strings derive their argument list directly from the format value, eliminating a whole class of variadic bugs. Dependent records encode protocol state in the type itself, turning invalid transitions into compile-time errors rather than runtime failures.

Curios is an impure language, like OCaml. Side effects — currently terminal IO through `/sys/Io` — are ordinary expressions that can appear anywhere in a program. The type system accommodates this by treating effectful operations as opaque at the type level: when the type checker encounters a term that performs IO during reduction, it raises a type error rather than attempting to evaluate the side effect.

## Installation

Download a pre-built binary for your platform from the [releases page](https://github.com/valmirjunior0088/curios/releases), or install directly from the repository:

```
cargo install --git https://github.com/valmirjunior0088/curios
```

Or, if you have a local clone:

```
cargo install --path .
```

Building from source compiles the vendored [Binaryen](https://github.com/WebAssembly/binaryen) optimizer, which requires CMake and a C++17 compiler. To build without them, disable the `binaryen` feature (compiled modules are then emitted unoptimized): `cargo install --path . --no-default-features --features cli`.

## Usage

```
curios [--timeout MILLIS] [--print [STAGES]] run <input-path>
curios [--timeout MILLIS] [--print [STAGES]] check <input-path>
curios [--timeout MILLIS] [--print [STAGES]] compile <input-path> [--output-path PATH]
```

- `--timeout` sets the type-checker's reduction timeout in milliseconds (default: 1000)
- `--print [STAGES]` prints selected intermediate representations to stderr; `STAGES` is a comma-separated subset of `text,core,ersd,cont,optm,wasm`. Bare `--print` selects all; omitting the flag prints none.
- `run` compiles and executes the entrypoint
- `check` type-checks the entrypoint without executing it, exiting with a non-zero status on failure; if `--print` requests a post-core stage (`ersd`, `cont`, `optm`, or `wasm`), it runs the full lowering pipeline so that stage exists to print
- `compile` emits the compiled WebAssembly module; pass `--output-path PATH` to write the binary to that path, otherwise it writes `<input-stem>.wasm`
- `<input-path>` is the path to an entrypoint file; a Curios source file whose last expression is the program's result

A minimal example:

```
-- hello.crs
let msg : /sys/Bin = "hello, world";
/sys/Io/write(/sys/Io/stdout, msg)
```

```
curios run hello.crs
```

Programs can read and write files through `/std/File` (and the raw `/sys/Io/open`); they run with the invoking user's filesystem access — there is no sandbox.

## Examples

The `examples/` directory contains end-to-end Rust programs that drive the full compiler pipeline. Two are particularly instructive:

**Typed format strings** (`examples/crs_printf.rs`) — reads a name from stdin via `Io/read`, trims it, then calls `/std/Fmt/printf` with a format string whose argument list is derived from the string's content at compile time:

```
let name_bytes = Str/trim(Io/read(Io/stdin, 1024).bytes);
match Str/of_bin(name_bytes) : {}
| some(name) => Fmt/printf("%s is %d")(name)(30)
| none() => Io/print("invalid input")
end
-- with input "Alice": "Alice is 30"
```

Passing the wrong type is a compile-time error, not a runtime failure:

```
/std/Fmt/printf("%d")("Alice")
-- TypeMismatch: the format specifier %d expects Nat, but "Alice" has type Str
```

**JSON codec** (`examples/crs_json_codec.rs`) — constructs a `Json` tree using `union` constructors such as `Json/obj` and `Json/str`, encodes it to a `Bin` string with `Json/encode`, parses it back with `Json/decode`, and asserts the output is byte-identical to the original. It exercises the prepended standard library (`std/Json`, `std/Parse`), arrays, and nested union values together.

## Documentation

- [ARCHITECTURE.md](ARCHITECTURE.md) — full architectural overview of the compiler pipeline, from parsing through type checking, erasure, CPS lowering, and WebAssembly code generation, including a "Reading order" guide for newcomers.
- [SYNTAX.md](SYNTAX.md) — language syntax reference covering lexical basics, all term and type forms, primitive operations, and idioms for sum types and recursive types.
- [CRASH_COURSE.md](CRASH_COURSE.md) — guided introduction for Rust programmers, building from familiar constructs up to dependent function types and length-indexed vectors.
- [PROOFS_101.md](PROOFS_101.md) — follow-up to the crash course on proving: propositions as types, equality via `/std/Eq`, induction with `match`, negation, and proofs that re-type data; every snippet is pinned by `examples/crs_proofs.rs`.
