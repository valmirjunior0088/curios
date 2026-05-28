**README.md** introduces Curios — a dependently-typed functional language that compiles to WebAssembly. It explains the language's motivation (bringing Π-types, Σ-types, and dependent enumerations to an impure programming context), lists installation methods via `cargo install`, and documents the CLI flags (`--timeout`, `--check`, `--print`) plus a minimal `/sys/Io/print` entrypoint.

**ARCHITECTURE.md** provides a full technical overview of the compiler pipeline across six stages: parsing (`text`), elaboration (`text/to_core`), type checking and erasure (`core/typing.rs`), CPS lowering (`ersd/to_cont`), WebAssembly codegen (`cont/to_wasm`), and binary serialization (`wasm/writer.rs`). It also covers the module layout conventions, value representation in WASM, the `Provider` trait for IO, the test suite, and a recommended reading order for newcomers.

**CRASH_COURSE.md** is a guided introduction aimed at Rust programmers. It walks through bindings, lambdas, `match` over `Nat`, primitive types, tuples, atoms, the sum-type idiom via dependent tuples, dependent function types, and length-indexed vectors — always pairing Rust and Curios code side by side.

**SYNTAX.md** is the complete language reference. It covers lexical basics, top-level declarations (`let`, `rec`, `mod`, `use`), all term and type forms, literals, the `/sys` prelude module for primitive types and operations (`Nat`, `Int`, `Flt`, `Bin`, `Arr`, `Bln`, `Io`), and the canonical idioms for sum types and recursive types.

The `examples/` directory contains runnable Rust programs that drive the full compiler pipeline end-to-end — parsing Curios source, type-checking, erasing, lowering to CPS, generating WASM, and executing via Wasmtime. Two examples are particularly instructive: `crs_json_codec.rs` encodes a `json/Value` tree to a `Bin` string and round-trips it back through a parser, asserting the decoded output is byte-identical to the original — it exercises file-backed modules (`std`, `parser`, `json`), dependent sum types, and arrays all together; `crs_printf.rs` calls `fmt/printf("%s is %d")("Alice")(30)` and additionally demonstrates the type-safety guarantee by showing that passing a `Bin` where `%d` expects a `Nat` produces a `TypeMismatch` error at compile time.

When questions arise about the Curios language — its syntax, type system, primitives, idioms, or compiler architecture — the documents and examples mentioned above are the authoritative source and should be consulted first.

This repository has a dedicated Github Projects board named Curios, available through the `gh project` command, which you must invoke any time an instruction involves the board.

The agent must avoid changing code unless explicitly instructed to do so; this means that the agent must not implicitly infer whether to change code and that the agent must wait for a direct go-ahead instruction before changing code.

The agent must never infer workarounds or silently adopt fallback strategies when facing ambiguity or obstacles; architectural and design decisions must always be surfaced to the user and approved before any action is taken. The agent must also never stall in a state of doubt — the user is fully capable of resolving any uncertainty, so the agent must ask directly and immediately rather than speculating, hedging, or deferring to assumptions.
