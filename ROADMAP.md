# Roadmap

Tracks Curios development by feature area. Checkboxes reflect current codebase state, not chronological history — items whose description was later superseded by a rework are folded into the item that replaced them.

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
  - [x] `Lst`

## Type System

- [x] Implicit arguments (`@`-marked binders)
- [x] Instance arguments (ad-hoc polymorphism: `concept` declarations, anonymous `satisfy` (witness) declarations, the `use` binder plicity, deterministic witness resolution with local-scope, superclass-projection, and global-table steps; `use`-marked concept fields resolve by omission and fill explicitly with `use <term>` entries)
  - [x] `Show`/`Eql`/`Ord`/`Monad` in the standard library
  - [x] Higher-kinded concepts (`Monad(M : (Type) -> Type)`, via the flex-apply imitation rule in `convert.rs`)
  - [x] Multi-parameter keying (tuple of input heads) and functional dependencies (`out` parameters)
  - [x] Orphan rule (a witness must be declared where its concept, or a type in its key, is already declared; the standard library's three roots — `sys`/`syn`/`std` — are exempt from the check against each other, one coordinated implementation rather than independent packages)
  - [x] Concept-based operators (every infix except `&&`/`||` dispatches through `Add`/`Sub`/`Mul`/`Div`/`Rem`/`Eql`/`Cmp` with `/sys` witnesses; primitive codegen unchanged)
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
- [x] Monadic sequencing syntax (postfix `!`, dispatched through the `/syn/Monad` concept — no `let !` header; every value body is a region)
- [x] Field projection sugar (`.0`/`.label`)
- [x] Destructuring patterns at `let`/lambda-parameter/function-sugar-parameter position (tuple/struct only, irrefutable; desugars to projections)
- [x] Function-field sugar in every field list (`name(params) -> T` in tuple types and `struct`/`record` declarations, `name(args) = body` in tuple and struct literals — the forms concept/witness bodies always had) and trailing commas in field lists
- [x] Struct spread/update syntax (`T { ..base, f = x }` — one leading spread; labeled, declaration-ordered overrides; unwritten fields copied from the base, concept superclass fields included, overridable with `use <term>`; no tuple spread)
- [x] List/bytestring spread syntax (`[a, ..xs, b]`, `\00\..bytes\01` — positional splices, any position/count, desugared to the n-ary concat prims; `Bin` literals stay whitespace-free with glued atomic operands; no tuple/string spread)

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

## Host Interface (FFI)

- [x] Self-describing foreign-function store (`curios-abi`'s `ForeignFunction`/`WireSignature`/`ForeignStore`): the `/sys/Io` prelude declarations, elaboration, wasm `sys.*` imports, and runtime linking all derive from one per-compilation store of named signature rows — the generic `Foreign` IR nodes carry the row itself, and the runtime links by pulling the module's imports from a name-keyed registry
- [x] Surface `foreign` declarations (user-visible FFI syntax): a program's own declarations accumulate a second, `ffi`-tier store `compile_entrypoint` hands back
- [x] Embedder-extensible host-function registry (`curios-rt::ForeignBindings`, threaded through `run_bytes`/`instantiate`/`run_wasm`)

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
  - [x] Private-item-in-public-interface check (signature-only; hidden `struct` fields exempt)
- [x] Browser playground
  - [x] Run harness owned by `curios-js` (`run`/`bridge_bytes`/`abi`, derived from `curios-abi`/`curios-cont`)
- [ ] Developer tooling
  - [ ] Code formatter
  - [ ] Terminal REPL
  - [ ] Language server (hover, go-to-definition, highlighting)
  - [ ] Package manager (needs the orphan rule under Type System first)
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
- [x] Proof-carrying UTF-8 string decoding (`std/Str`, `std/Char`)
- [x] Parser-combinator library (`std/Parse`)
- [x] Core collections (`std/Lst` and its helpers, length-indexed `std/Vec`)
- [x] Key-value map (`std/Map`: a canonical crit-bit trie over `Bin` keys — same entries, same shape — with injective key encodings via its `Key` concept)
- [x] Equality and ordering (`std/Eq`, `std/Order`)
- [x] Foundational proof/logic types (`std/True`, `std/False`)
