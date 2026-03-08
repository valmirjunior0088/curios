# Curios

Curios is a functional language with dependent types and algebraic effects that compiles to WebAssembly. Most languages with dependent types evolved from proof assistants, where non-determinism is a property to be excluded rather than embraced - Curios inverts this, aiming to bring dependent function types (Π-types, λ-abstractions), dependent tuple types (Σ-types, dependent pairs), and dependent enumeration types (disjoint sets of atoms with dependent elimination semantics) to a programming context where non-determinism is simply part of daily life.

Dependent types pay off most in a handful of recurring patterns. Length-indexed collections rule out bounds errors by construction, replacing runtime panics with type-level guarantees. Typed format strings derive their argument list directly from the format value, eliminating a whole class of variadic bugs. Dependent records encode protocol state in the type itself, turning invalid transitions into compile-time errors rather than runtime failures.

Algebraic effects bring composability to side effects: I/O, exceptions, and async/await all layer naturally without monad transformer stacks, and handlers can be swapped out - mocking I/O in tests, for instance, without touching call sites. Dependent effects extend this further, allowing an effect's return type to depend on the request value. This enables typed interaction protocols where each response's shape is statically determined by what was asked, catching mismatches that untyped effect systems leave to runtime.

## Prototype

The prototype establishes end-to-end correctness across the full compilation pipeline, with each pass implemented to the minimum extent needed. Tooling, error quality, standard library, and documentation are deferred to later stages.

- [x] CLI (`curios <file>`)

### `String` -> `Core`

The front-end of the pipeline. Elaborates user-written source text into the internal typed representation, resolving names, desugaring constructs, and filling in implicit arguments.

- [x] Dependent function types (Π-types), functions (λ-abstractions) and applications
- [x] Dependent tuple types (Σ-types), dependent pairs and let-split
- [x] Dependent enumeration types, atoms and match
- [x] Let, let-rec
- [-] Int type, int literals, operations and comparisons
- [-] Float type, float literals, operations and comparisons
- [ ] String type and operations
- [ ] Vector type and operations
- [ ] Multi-param function syntactic sugar
- [ ] Expand dependent tuple types (Σ-types, dependent pairs) to full dependent struct types
- [ ] Enum syntax
- [ ] Algebraic effects (syntax for effects and handlers)
- [ ] FFI (foreign import declarations with host function types)
- [ ] Non-dependent type inference (Hindley-Milner style)
- [ ] Non-dependent implicit arguments
- [ ] Error messages (parse errors)
- [ ] Source maps (attach source spans to nodes)

### `Core` -> `Erased`

Traverses typed Core terms against expected types, type-checking them and producing a runtime-only representation where types erase to unit and are discarded, and values are lowered to their runtime forms guided by type information.

- [x] Bidirectional dependent type checking
- [x] Beta-reduction during type checking
- [x] Closure capture collection via free-variable analysis
- [x] Atom-to-index conversion
- [x] Timeout enforcement
- [ ] Eta-equality for dependent function types (Π-types, λ-abstractions) and dependent tuple types (Σ-types, dependent pairs)
- [ ] Algebraic effects (effect typing and handler elaboration)
- [ ] FFI (type-check imported functions against declared Core types)
- [ ] Error messages (type errors)
- [ ] Source maps (preserve spans through erasure)

### `Erased` -> `Cont`

Lowers erased terms into a region-based intermediate representation that makes control flow and tail positions fully explicit. Each function body becomes a flat list of SSA-like value bindings, a set of labeled join blocks, and a single tail instruction.

- [x] Flat SSA-like value bindings
- [x] Labeled join blocks
- [x] Tail instructions: Jump, Case, Call
- [x] Non-tail Apply, Match, Split via join blocks
- [x] Fresh name generation
- [ ] Algebraic effects (lower effects to continuation passing; unwind stack with tag and arguments to handler)
- [ ] FFI (represent imported functions as opaque call targets in Cont IR)
- [ ] Source maps (preserve spans through lowering)

### `Cont` -> `Cont`

Applies optimization passes over the continuation IR between lowering and code generation, improving program structure without changing its semantics.

- [ ] Constant folding
- [ ] Dead value/block elimination
- [ ] Known-atom case collapsing (statically resolve case on known atom to matching branch)
- [ ] Known-closure inlining (inline statically known call targets)
- [ ] Arity raising (uncurry known multi-arg calls)
- [ ] Source maps (preserve spans through optimization)

### `Cont` -> `Wasm`

Generates WebAssembly from the continuation IR using the GC, function references, and tail call proposals.

- [x] GC struct closures with typed function references
- [x] Uniform arity dispatch via abstract struct/funcref supertypes
- [x] i31ref for integers, single-field struct for floats
- [x] Uniform (ref any) value representation
- [x] Three-phase value emission (preallocate, initialize, backpatch)
- [x] Tail calls via return_call_ref
- [x] Binary WASM serialization
- [ ] Algebraic effects (emit handler dispatch)
- [ ] FFI (WASM imports for host functions)
- [ ] Source maps (emit bytecode-to-source mappings)

### `Wasm` -> `()`

Loads the serialized WASM module into Wasmtime and executes it, producing output through side effects.

- [x] Wasmtime with GC, function references, and tail calls
- [x] Stopgap output via anyref introspection with cycle detection
- [ ] Proper output via algebraic effects (replaces anyref introspection stopgap)
- [ ] FFI (register host function implementations at instantiation)

## Pre-release

Pre-release targets feature-completeness, adding the tooling, error reporting, and documentation needed for external use. Ecosystem infrastructure, including the module system, package manager, and language tooling, is deferred to Release.

- [ ] CLI (`curios run`, `curios fmt`, `curios check`)
- [ ] Standard library (primitives via FFI, higher-level in Curios)
- [ ] Error messages (usable quality across all stages)
- [ ] Source maps (end-to-end, from source to bytecode)
- [ ] Formatter
- [ ] Documentation (syntax overview, examples, short tutorial)
- [ ] Test suite (golden-file or snapshot tests covering end-to-end pipeline correctness)

## Release

Release marks the delivery of a production-ready distribution with full ecosystem infrastructure.

- [ ] CLI (build, project management, package commands)
- [ ] Module system
- [ ] Error recovery (multiple errors, continue after failure)
- [ ] REPL / browser playground
- [ ] LSP (hover types, go-to-definition, syntax highlighting)
- [ ] Package manager
- [ ] Documentation (full language reference)
- [ ] Test suite (comprehensive coverage of language features, edge cases, and error messages)
- [ ] Benchmarks (vs. C, Rust, OCaml, Haskell, Lean 4, Agda, Python, JavaScript/TypeScript)
- [ ] Self-bootstrapping (compiler written in Curios, compiled by itself)

## Post-release

Post-release opens the language to advanced type-theoretic research and features beyond the initial release.

- [ ] Universe hierarchy
- [ ] Dependent type inference and goal solving (higher-order pattern unification)
- [ ] Dependent implicit arguments
- [ ] Termination checking for a sound subset
