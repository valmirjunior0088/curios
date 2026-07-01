# Roadmap

Tracks `curios` development by feature area. Checkboxes reflect current codebase state, not chronological history — items whose description was later superseded by a rework are folded into the item that replaced them.

## Core Pipeline

- [x] Π-types, λ-abstractions, and application
- [x] Σ-types and dependent pairs
- [x] `let` and `let-rec` bindings
- [x] Cyclic module dependency resolution
- [x] Bidirectional dependent type checking with full definitional equality (β/ι/δ reduction, primitive computation, indexed-inductive inversion)
- [x] Closure capture analysis and atom-to-index erasure
- [x] CPS lowering with join blocks and tail instructions
- [x] Value-level mutual recursion in `rec`
- [x] Anyref-based uniform value representation with GC closures and tail calls
- [x] Binary WebAssembly serialization
- [x] WebAssembly execution via a shared, GC-enabled wasmtime engine
- [x] AOT `.cwasm` precompilation (deserialized and run without re-JITting)
- [x] Crate-boundary split isolating the Cranelift/Binaryen-free launcher (`curios-rt`) from the JIT-capable compiler
- [x] Pure pipeline driver crate (`curios-pipeline`) decoupled from runtime/Binaryen/CLI, enabling a wasm32 (browser) build
- [ ] Bootstrap the compiler in Curios itself (self-hosting)

## Primitive Types

- [x] Primitives as orthogonal builtins _(uniform `/sys` builtin declarations, not parser-special-cased)_
  - [x] `Nat`
  - [x] `Int`
  - [x] `Flt`
  - [x] `Bin`
  - [x] `Arr`

## Type System

- [x] Implicit arguments (`@`-marked binders)
- [x] `struct`/`record` declarations (nominal types with representation-visibility control)
- [x] Inductive types (`induct` declarations)
  - [x] Constructor registry & dependent eliminators
  - [x] Indexed families (e.g. `Vec`)
  - [x] Variant arity checking
  - [x] Exhaustiveness/coverage checking (index inversion)
  - [x] Large-elimination guard (restricts `Prop` → `Type` elimination)
- [x] Eta-reduction for Π-types and Σ-types
- [x] Named tuple fields
- [x] `Prop` universe with definitional proof irrelevance
- [x] Unification solver
  - [x] Pattern unification for higher-order metavariable spines
  - [x] Re-validate solutions in checking mode
  - [ ] Pruning of out-of-scope metavariables
  - [ ] η-equate metavariable heads
  - [ ] Surface residual unification constraints (distinguish postponed vs. rigid-mismatch diagnostics)

## Syntax Sugar

- [x] Multi-parameter function syntax sugar
- [x] Monadic sequencing syntax
- [x] Field projection sugar (`.0`/`.label`; no destructuring-pattern binders)
- [ ] Struct spread/update syntax (`T { ..base, f = x }`)

## Optimizations

- [x] Core calculus machinery (reduction & conversion performance)
- [x] Ersd (ersd→ersd) optimization passes
  - [x] Dead-item pruning via call-graph reachability
  - [x] Worker/wrapper transform for non-tail self-recursion
- [x] CPS (cont→cont) optimization passes
  - [x] Constant folding
  - [x] Dead code elimination
  - [x] Tag threading (known-argument case/callee specialization)
  - [x] Closure lifting and call-site specialization
  - [x] Tail-recursion-to-loop conversion
  - [x] Copy propagation, pure-call evaluation, literal hoisting, jump threading, dead-argument elimination, and slice forwarding
- [x] Wasm-emission optimizations
  - [x] `struct.new` construction with immutable fields
  - [x] Direct `br` for single-target regions
- [x] Binaryen closed-world post-optimization pass

## IO

- [x] `Io` unified byte-stream handle abstraction
  - [x] Terminal
  - [x] File
  - [x] Client network (TCP)
  - [x] Server network (TCP)
  - [x] TLS (https) for client and server sockets
  - [x] Non-blocking IO & concurrent connection handling
  - [x] Never-reused fd handle tokens (monotonic mint counter, use-after-close hardening)
  - [x] Clock & randomness
  - [x] Process IO
- [x] Runtime-driven IO (program output flows through the host bridge, not compile-time introspection)

## Error Messages

- [x] Diagnostics
  - [x] Span-based error quality across all stages
  - [x] Diagnostic terms printed with names in scope

## Testing & Documentation

- [x] Documentation
  - [x] Syntax overview, examples, and tutorial
  - [x] Full language reference
- [x] Benchmarks
  - [x] Internal benchmarks
  - [x] Cross-language benchmarks (Docker harness vs. Rust/OCaml/Node/Lean 4 native and Rust/Grain/AssemblyScript on wasmtime)

## Tooling & Ecosystem

- [x] CLI (`run`, `check`, `compile` subcommands — `compile` bundles a native executable: launcher + appended `.cwasm`)
- [x] Standard library canonicalization
- [x] CI pipeline (fmt/check/clippy/test)
- [x] Multi-platform release automation (Linux x86_64/aarch64, macOS aarch64 native binaries + wasm playground bundle, via tag-triggered GitHub Releases)
- [x] Module system
- [x] Browser playground
- [ ] Developer tooling
  - [ ] Code formatter
  - [ ] Terminal REPL
  - [ ] Language server (hover, go-to-definition, highlighting)
  - [ ] Package manager
  - [ ] Project manifest & discovery
  - [ ] `curios new` scaffolding
  - [ ] Linter
  - [ ] Test runner
  - [ ] Documentation generator

## Standard Library

- [ ] Async combinators for `/std/Task`
  - [x] `map`
  - [x] concurrent `both`/`race`/`select`
  - [x] result cell (`Cell`)
  - [ ] `sleep`/`timeout`
- [x] JSON codec (`std/Json`)
- [x] HTTP client (`std/Http`, built on `Tcp` + `Task`)
- [x] Typed format strings (`std/Fmt`)
- [x] Arbitrary-precision naturals (`std/BigNat`)
- [x] Reader combinator (`std/Reader`)
- [x] Proof-carrying UTF-8 string decoding (`std/Str`, `std/Char`)
- [x] Parser-combinator library (`std/Parse`)
- [x] Core collections (`std/Lst`, length-indexed `std/Vec`, `std/Arr` helpers)
- [x] Equality and ordering (`std/Eq`, `std/Order`)
- [x] Foundational proof/logic types (`std/True`, `std/False`)
