# Roadmap

Tracks Curios development by feature area. Checkboxes reflect current codebase state, not chronological history — items whose description was later superseded by a rework are folded into the item that replaced them.

Unchecked items may link to working implementation specifications. When an item lands completely, transfer every durable contract and invariant to its owning source, module or crate documentation and tests; update remaining specifications to depend on the landed API rather than the working document; replace the linked checkbox with a checked plain-text summary; verify that nothing still references the specification filename; and delete the specification.

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
- [x] Crate-boundary split isolating the Cranelift/Binaryen-free launcher (`curios-runtime`) from the JIT-capable compiler
- [x] Pure pipeline driver crate (`curios-pipeline`) decoupled from runtime/Binaryen/CLI, enabling a wasm32 (browser) build
- [x] Build-scoped archived prelude and replay (`curios-prelude` compiles and validates fixed Text/Core/Ersd state in `OUT_DIR`; production compilations restore it with no source fallback and lower/elaborate/erase only the user suffix)
- [ ] [Bootstrap the compiler in Curios itself](05_BOOTSTRAP_SPEC.md) (self-host every language-specific stage through raw WebAssembly generation while retaining Rust as the native host and stage-zero seed)

## Primitive Types

- [x] Primitives as orthogonal builtins _(uniform `/sys` builtin declarations, not parser-special-cased)_
  - [x] `Nat`
  - [x] `Byte` (i31 scalar; contextual literals `0..=255`; `Byte/to_nat` and wrapping `Nat/to_byte`)
  - [x] `Int`
  - [x] `Flt` (bit-preserving binary32 identity, including `to_le_bytes`/`of_le_bytes` reinterpretation across every compiler stage)
  - [x] Packed `Bits` and `Bytes` (grain-specialized operations over shared immutable windows; O(1) slices and tails)
  - [x] `Lst`

## Type System

- [x] Implicit arguments (`@`-marked binders)
- [x] Instance arguments (ad-hoc polymorphism: `concept` declarations, anonymous `satisfy` (witness) declarations, the `use` binder plicity, deterministic witness resolution with local-scope, superclass-projection, and global-table steps; `use`-marked concept fields resolve by omission and fill explicitly with `use <term>` entries)
  - [x] `Show`/`Eql`/`Ord`/`Monad` in the standard library
  - [x] Higher-kinded concepts (`Monad(M : (Type) -> Type)`, via the flex-apply imitation rule in `convert.rs`)
  - [x] Multi-parameter keying (tuple of every parameter head)
  - [x] Orphan rule (a witness must be declared where its concept, or a type in its key, is already declared; the standard library's three roots — `sys`/`syn`/`std` — are exempt from the check against each other, one coordinated implementation rather than independent packages)
  - [x] Concept-based operators (every infix, `&&`/`||` included, dispatches through `Add`/`Sub`/`Mul`/`Div`/`Rem`/`And`/`Or`/`Eql`/`Cmp` with `/sys` witnesses; primitive codegen unchanged)
- [x] Unified `struct` declarations (independent nominal and declaration-local representation visibility)
- [x] Inductive types (`induct` declarations)
  - [x] Independent nominal/representation visibility with opaque construction and elimination
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
- [ ] [Monomorphic, use-driven inference for unannotated lambda parameters](10_LAMBDA_INFERENCE_SPEC.md) (park structurally blocked inference within one enclosing item)

## Syntax Sugar

- [x] Multi-parameter function syntax sugar
- [x] Monadic sequencing syntax (postfix `!`, dispatched through the `/syn/Monad` concept — no `let !` header; every value body is a region)
- [x] Field projection sugar (`.0`/`.label`)
- [x] Destructuring patterns at `let`/lambda-parameter/function-sugar-parameter position (tuple/struct only, irrefutable; desugars to projections)
- [x] Function-field sugar in every field list (`name(params) -> T` in tuple types and `struct` declarations, `name(args) = body` in tuple and struct literals — the forms concept/witness bodies always had) and trailing commas in field lists
- [x] Struct spread/update syntax (`T { ..base, f = x }` — one leading spread; labeled, declaration-ordered overrides; unwritten fields copied from the base, concept superclass fields included, overridable with `use <term>`; no tuple spread)
- [x] List/Bits/Bytes spread syntax (`[a, ..xs, b]`, `b\1\..bits\0`, `x\00\..bytes\01` — positional splices, any position/count, desugared to n-ary concat prims; packed literals stay whitespace-free with glued atomic operands and require their grain prefix; no tuple/string spread)
- [x] Nested/tuple/struct match-arm patterns (the pattern-matrix compiler — full enumeration, no row priority)
- [x] Headless match (a headless `Bool` condition ladder `match | cond => … | _ => … end` with a mandatory `_` default; arms inherit their condition's definitional refinement)
- [x] Bind-arms (`| pattern = value =>`, Rust `if let`, in the headless ladder; refutable LHS, nested patterns, fallthrough shared through a nullary thunk)
- [x] Final `| _ =>` catch-all in headed inductive matches (bare/final/top-level only; lowers to the core `Cases::Inductive` default)
- [ ] [Anonymous match functions](11_ANONYMOUS_MATCH_FUNCTION_SPEC.md) (`match =>`, lowering to an ordinary one-argument lambda and headed match)

## Optimizations

- [x] Continuation IR v2 (a pre-closure CPS graph replacing the region-based post-closure optimizer: arena-backed high CPS, delayed closure conversion, an interprocedural optimizer with literal folding, effect-aware dead-binding and dead-parameter elimination, bounded inlining, contification, and recursive-SCC known-argument propagation, specialization, and branch specialization, and structured Wasm control flow by SCC condensation into blocks, loops, and one localized dispatcher for irreducible scopes; the region optimizer's separate late passes — common-subexpression elimination, pure-call evaluation, literal hoisting, tag/callee threading, loop-invariant motion, list-map simplification, and slice forwarding — were measured redundant in the new pipeline and removed)
- [x] Ersd v2 (the semantic half of the backend, symmetric to Cont v2: a flat, verified, first-order erased representation replacing the legacy erased-term representation outright — module-owned arenas of single-operation statements with distinct scalar shapes, schema-carrying products and variants, and first-class switch and fold forms; erasure as pure transcription under the once-per-expression operand law, with the fixed prelude erased once at compiler build time and replayed from the archive; the three transformations — behavior-summary pruning, partial evaluation with closed-term folding and literal-spine specialization, and the monoid worker/wrapper — over one behavior oracle; and a single normative lowering into the landed continuation interface, where every encoding decision is made)
- [x] Core calculus machinery (reduction & conversion performance)
- [x] Ersd (ersd→ersd) optimization passes
  - [x] Dead-item pruning via call-graph reachability
  - [x] Closed-term evaluation and recursive literal-spine specialization
  - [x] Worker/wrapper transform for non-tail self-recursion
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
- [x] Embedder-extensible host-function registry (`curios-runtime::ForeignBindings`, threaded through `run_bytes`/`instantiate`/`run_wasm`)

## Error Messages

- [x] Diagnostics
  - [x] Span-based error quality across all stages
  - [x] Diagnostic terms printed with names in scope
  - [x] Bare written goals (`?`) report their local scope, expected type, and optional inferred solution
  - [ ] [Required labeled written goals (`?label`), complete goal batches, and a typed incomplete checking outcome](12_WRITTEN_GOALS_SPEC.md)

## Testing & Documentation

- [x] Documentation
  - [x] Syntax overview, examples, and tutorial
  - [x] Full language reference
- [x] Benchmarks
  - [x] Internal benchmarks
  - [x] Cross-language benchmarks (Docker harness vs. Rust/OCaml/Node/Lean 4 native and Rust/Grain/AssemblyScript on wasmtime)

## Tooling & Ecosystem

- [x] CLI (`run` and `compile` subcommands — `compile` bundles a native executable: launcher + appended `.cwasm`)
- [x] Standard library canonicalization
- [x] CI pipeline (fmt/check/clippy/test)
- [x] Multi-platform release automation (Linux x86_64/aarch64, macOS aarch64 native binaries + wasm playground bundle, via tag-triggered GitHub Releases)
- [x] Module system
  - [x] Exact private-item-in-public-interface audit (signature-only; representation signatures checked only where exposed, through re-exports, identity aliases, and structurally direct-headed type-family aliases; opaque constructor namespaces cannot be re-exported)
- [x] Browser playground
  - [x] Run harness owned by `curios-web` (`compile`/`run`, with wire codes from `curios-abi` and a local bridge for the compiler's wire-ABI `Bin` payload shape)
- [ ] Developer tooling
  - [ ] [`curios wonder` structured program-analysis interface](13_WONDER_SPEC.md) (source and semantic indexes, diagnostics, references, dependencies, witnesses, and snapshot queries)
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
- [x] HTTP client (`std/http`, built on `tcp` + `Task`)
- [x] Typed format strings (`std/Fmt`)
- [x] Arbitrary-precision naturals (`std/BigNat`, canonical and packed over `Bits`)
  - [x] Machine-checked additive and multiplicative laws, additive cancellation, order reflection/transitivity, and power-of-two interaction lemmas
- [x] Certified strictly-positive arbitrary-precision naturals (`std/NonZero`)
- [x] Arbitrary-precision integers (`std/BigInt` over the strictly-positive `std/NonZero`)
- [ ] [`std/Toml`: native-`Int`/binary32 TOML codec](02_TOML_SPEC.md) _(next implementation effort; useful but explicitly not fully TOML-conforming because numeric storage is native-width)_
- [ ] [Dyadic `BigFlt` canonical representation, exact core operations, comparison, and witnesses](03_BIG_FLT_DYADIC_CORE_SPEC.md)
- [ ] [Dyadic `BigFlt` exact binary32 conversion and correctly rounded output](04_BIG_FLT_DYADIC_BINARY32_SPEC.md)
- [ ] [`BigInt` certified algebra, order, and binary-scale laws](06_BIG_INT_LAWS_SPEC.md) _(first effort immediately after bootstrap; the dyadic core lands only its required normalization and uniqueness subset)_
- [ ] Post-bootstrap dyadic `BigFlt` proof and quotient-boundary completion
  - [ ] [Algebra and order theorem corpus](07_BIG_FLT_DYADIC_LAWS_SPEC.md)
  - [ ] [Correctly rounded exact quotient conversion to binary32](08_BIG_FLT_DYADIC_RATIO_NARROWING_SPEC.md)
  - [ ] [Binary32 round-trip and correct-rounding proofs](09_BIG_FLT_DYADIC_BOUNDARY_PROOFS_SPEC.md)
- [x] Proof-carrying UTF-8 string storage and decoding (`std/Str`; decoded scalar values are exposed as certified `Char` values while storage remains packed `Bytes`)
- [x] Certified Unicode-scalar `Char` type and `Str` migration (`'…' : Char`, typed character APIs, explicit Byte/Char/Nat boundaries, and ASCII-explicit classification and casing policy)
- [x] Parser-combinator library (`std/Parse`)
- [x] Core collections (`std/Lst` and its helpers, length-indexed `std/Vec`)
- [x] Key-value map (`std/Map`: a canonical crit-bit trie over `Bytes` keys — same entries, same shape — with injective key encodings via its `Key` concept)
- [x] Equality and ordering (`std/Eq`, `std/Order`)
- [x] Foundational proof/logic types (`std/True`, `std/False`)
- [ ] General rational `BigFlt` sequence _(explicitly after `wonder`; no umbrella-only implementation step)_
  - [ ] [`BigNat` certified Euclidean division, GCD, divisibility, and coprimality](14_BIG_NAT_EUCLIDEAN_SPEC.md)
  - [ ] [General `BigFlt` reduced rational representation and exact operations](15_BIG_FLT_GENERAL_CORE_SPEC.md)
  - [ ] [General canonical uniqueness, ring, and order laws](16_BIG_FLT_GENERAL_LAWS_SPEC.md)
  - [ ] [General division and field laws](17_BIG_FLT_GENERAL_FIELD_LAWS_SPEC.md)
  - [ ] [General rational binary32 boundaries](18_BIG_FLT_GENERAL_BINARY32_SPEC.md)
  - [ ] [Exact decimal parsing and presentation](19_BIG_FLT_GENERAL_DECIMAL_SPEC.md)
