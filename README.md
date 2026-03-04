# curios

`curios` is an MVP compiler/runtime experiment for a small dependently typed core language. Source terms are checked and erased into a runtime term language, lowered into a continuation-style IR, emitted as Wasm using GC/reference-types/tail-call features, and executed with Wasmtime.

Most of the complexity sits in language semantics, binder handling, erasure and checking, continuation-style lowering, and the handwritten Wasm surface/backend.

## Status

This project is still an MVP.

That matters in two ways:

- The feature set is intentionally narrow.
- The implementation already includes multiple compiler stages and a custom Wasm backend.

The current backend assumes modern Wasm features:

- reference types
- function references
- GC
- tail calls

## End-to-end pipeline

The executable path is:

1. Parse source text into `core::Term`
2. Infer the term's type
3. Erase the term against that type into `core::ErasedTerm`
4. Lower the erased term into `cont::Module`
5. Emit a `wasm::Module`
6. Serialize Wasm to bytes
7. Run the generated module with Wasmtime
8. Pretty-print the resulting heap reference / `anyref`

Conceptually, inference precedes erasure. In the current implementation, however, checking and erasure are also used internally while validating terms, so those phases are distinct but not completely isolated.

The CLI in `src/main.rs` is a direct implementation of that pipeline via `execute`.

## Architecture

### 1. Core language

The `src/core/` tree defines the source language and most of its semantics.

Main responsibilities:

- `term.rs`
  Defines the typed AST, names, binders, scopes, and substitution-style open/close operations.
- `parse.rs`
  Parses surface syntax into `core::Term`.
- `print.rs`
  Pretty-prints core terms and round-trips with the parser for closed terms.
- `context.rs`
  Tracks assumptions, definitions, fresh names, and timeout configuration.
- `infer.rs`
  Infers types for terms that can synthesize them.
- `reduce.rs`
  Normalizes terms with timeout-based preemption.
- `convert.rs`
  Implements definitional equality / conversion checking with cycle and timeout protection.
- `erase.rs`
  Checks terms against expected types, removes type-level structure, and produces the runtime-facing erased term.
- `erased_term.rs`
  Defines the runtime term language after erasure.

The parser and printer layers here are shared infrastructure rather than one-off utilities: the project also carries handwritten parser/printer combinators in `src/monads/` that are reused by both the core language and Wasm layers.

### 2. Continuation IR

The `src/cont/` tree defines a continuation-oriented intermediate representation.

This IR makes control flow explicit:

- values are named
- blocks have parameters
- regions contain local values and nested blocks
- tails end in `jump`, `case`, or `call`

The key types live in `src/cont/module.rs`:

- `Value`
- `Block`
- `Region`
- `Tail`
- `Clsr`
- `Func`
- `Module`

This is the bridge between expression-oriented source terms and explicit control-flow code generation.

### 3. Core to cont lowering

`src/core/to_cont/` lowers erased core terms into the continuation IR.

This stage is where:

- closures become explicit closure values
- tail calls become explicit call tails
- non-tail computations become join blocks
- recursive bindings are turned into backpatchable IR values

Important MVP boundary:

- Recursive `let rec` lowering currently supports only recursive RHSs that can be represented directly as first-order `cont::Value`s.
- More general recursive knot tying is not implemented yet; unsupported recursive shapes currently fail during lowering rather than lowering through a more general fixpoint/cell mechanism.

### 4. Wasm IR

The `src/wasm/` tree defines a handwritten Wasm representation.

It includes:

- a Wasm AST (`expr.rs`, `types.rs`, `module.rs`)
- a text printer (`print.rs`)
- a text parser (`parse.rs`)
- a binary encoder (`writer.rs`)

The project owns its own Wasm surface and serializer.

### 5. Cont to Wasm lowering

`src/cont/to_wasm/` emits Wasm GC code from the continuation IR.

Main responsibilities:

- `table.rs`
  Builds the naming/type lookup tables used during emission.
- `context.rs`
  Tracks locals, frames, blocks, captured fields, and loading strategies.
- `module_emitter.rs`
  Defines synthetic Wasm types and emits globals/functions.
- `expr_emitter.rs`
  Emits Wasm instructions for values, control flow, closure layout, tuple projection, and backpatching.

The backend models runtime values as heap references:

- unit is represented as an empty struct
- integers and atom tags are represented with `i31ref`
- floats are boxed in a single-field struct
- pairs are boxed in a 2-field struct
- closures are represented as environment structs carrying a function reference and captured fields

### 6. Runtime inspection

`src/execute.rs` inspects Wasmtime `anyref` results and pretty-prints:

- `i31`
- structs
- arrays
- recursive/back-referenced object graphs

Generated programs return heap references rather than plain Rust-native values.

## Language snapshot

The current core language includes:

- `Type`
- `Int` and `Flt`
- integer and float primitives
- dependent function types and functions
- dependent pair types and pairs
- atom enums
- atom-enum case analysis via `match`
- `let`
- `let rec`

The erased runtime language includes:

- unit, primitives, and names
- functions and applications
- pairs and projection via split lowering
- atom tags and matches
- `let` and `let rec`

## Examples

The examples are the current executable documentation for the project.

- `examples/core.rs`
  Builds a typed core program directly with the Rust API.
- `examples/core_to_wasm.rs`
  Parses source text, erases it, lowers it, and prints the resulting Wasm.
- `examples/end_to_end.rs`
  Runs the full pipeline and asserts a concrete runtime result.
- `examples/execute.rs`
  Demonstrates a richer source program and prints the runtime heap result.
- `examples/core_erased_to_cont.rs`
  Shows the erased term language lowering into continuation IR.
- `examples/core_erased_to_wasm.rs`
  Shows erased terms going all the way to Wasm.
- `examples/cont.rs`
  Builds continuation IR directly.
- `examples/cont_to_wasm.rs`
  Emits Wasm from hand-written continuation IR.
- `examples/wasm.rs`
  Builds and prints the handwritten Wasm AST directly.

If you want to understand the project quickly, start with:

1. `examples/end_to_end.rs`
2. `examples/execute.rs`
3. `examples/core_erased_to_cont.rs`
4. `examples/cont_to_wasm.rs`

## Tests

The test suite is concentrated in `src/` modules rather than a separate `tests/` directory.

It covers:

- core parsing
- core print/parse round-tripping
- reduction
- conversion / alpha-equivalence
- erasure
- core-to-cont lowering
- cont-to-wasm execution, including direct calls, recursive tuples, recursive closures, unit results, and float results
- Wasm parse/print round-tripping

Run the test suite with:

```bash
cargo test
```

## Complexity assessment

### High-level verdict

This is a high-complexity MVP.

Why:

- The repository is not large, but it spans several non-trivial compiler phases.
- The source language already includes dependent typing machinery.
- The backend targets advanced Wasm GC features directly.
- The Wasm representation, text parser/printer, and binary writer are all handwritten.

### Where the complexity sits

Conceptual complexity is highest in:

- binder handling and scope opening/closing
- type inference for dependent constructs
- normalization and conversion
- erasure and checking
- CPS-style lowering into explicit control flow

Implementation complexity is highest in:

- `src/core/erase.rs`
- `src/core/convert.rs`
- `src/core/to_cont/lowerer.rs`
- `src/cont/to_wasm/expr_emitter.rs`
- `src/cont/to_wasm/module_emitter.rs`
- `src/wasm/writer.rs`
- `src/wasm/parse.rs`

### Practical interpretation

This project is:

- small in surface area
- medium in total size
- high in semantic density
- high in maintenance risk per line of code

In other words, the feature set is still narrow, but the implementation already contains most of the major pieces of a compiler pipeline.

## Current constraints and MVP limits

The current design is intentionally constrained.

Notable limits:

- very little top-level documentation outside examples
- no optimizer
- no package/module system
- no separate external test corpus
- recursive lowering only supports recursive RHSs that can become direct `cont::Value`s; unsupported recursive shapes currently fail during lowering
- runtime representation is tightly coupled to Wasm GC features and Wasmtime behavior

These are reasonable MVP tradeoffs, but they are real boundaries rather than accidental omissions.

## Running the CLI

The binary expects a file containing a core-language program.

Example:

```bash
cargo run -- path/to/program.curios
```

Optional timeout:

```bash
cargo run -- --timeout 1000 path/to/program.curios
```

The timeout is in milliseconds and is used to preempt normalization/conversion work that may diverge.

## Summary

`curios` is best understood as a compact compiler stack:

- a typed core language
- an erasure/checking pass into a runtime term language
- a continuation IR
- a Wasm GC backend
- a Wasmtime execution path

The feature set is still narrow, but the project already includes most major stages of a compiler pipeline.
