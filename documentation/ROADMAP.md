# Roadmap

Tracks Curios development by feature area. Checkboxes reflect current codebase state, not chronological history — items whose description was later superseded by a rework are folded into the item that replaced them.

Unchecked items may link to working implementation specifications. Unchecked items whose design is not refined yet instead link a placeholder specification marked "Not refined yet", possibly an umbrella covering several related items; the placeholder only reserves the specification location until refinement replaces it. When an item lands completely, transfer every durable contract and invariant to its owning source, module or crate documentation and tests; record its design rationale and rejected alternatives in [DESIGN.md](DESIGN.md); update remaining specifications to depend on the landed API rather than the working document; replace the linked checkbox with a checked plain-text summary; verify that nothing still references the specification filename; and delete the specification.

## Type System

- [x] Π-types, λ-abstractions, and application
- [x] Σ-types and dependent pairs
- [x] `let` and `let-rec` bindings
- [x] Value-level mutual recursion in `rec`
- [x] Bidirectional dependent type checking with full definitional equality (β/ι/δ reduction, primitive computation, indexed-inductive inversion)
- [x] Eta-reduction for Π-types and Σ-types
- [x] Named tuple fields
- [x] `Prop` universe with definitional proof irrelevance
- [x] Implicit cumulative `Type` hierarchy (nullary surface `Type`, algebraic inferred levels, declaration-local universe polymorphism, fresh external instantiation, monomorphic recursive groups, cumulative checking, finalized nominal registry instances, archive replay, and complete erasure before Ersd)
- [x] Implicit arguments (`@`-marked binders)
  - [x] Plicity as a coherent contract (part of function-type identity — plicity-sensitive conversion — with every written function binder and constructor-pattern argument checked against its slot, and omitted implicit/witness *lambda* binders inserted automatically from the expected type, mirroring application-side insertion)
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
  - [x] Strict positivity modulo polarity (per-parameter polarity vectors on `induct`/`struct`, computed by a whole-declaration-set fixpoint over zonked Core, accepted on the diagonal of the transitively closed occurrence relation, and carried into the prelude archive)
- [x] Unification solver
  - [x] Pattern unification for higher-order metavariable spines
  - [x] Re-validate solutions in checking mode
  - [ ] [Pruning of out-of-scope metavariables](compiler/06_UNIFICATION_REFINEMENTS_SPEC.md)
  - [ ] [η-equate metavariable heads](compiler/06_UNIFICATION_REFINEMENTS_SPEC.md)
  - [ ] [Surface residual unification constraints](compiler/06_UNIFICATION_REFINEMENTS_SPEC.md) (distinguish postponed vs. rigid-mismatch diagnostics)
- [ ] [Monomorphic, use-driven inference for unannotated lambda parameters](match_ergonomics/01_LAMBDA_INFERENCE_SPEC.md) (park structurally blocked inference within one enclosing item)

## Pattern Matching

- [x] Nested/tuple/struct match-arm patterns (the pattern-matrix compiler — full enumeration, no row priority)
- [x] Multi-scrutinee matrix matching (a tuple scrutinee matched column by column with grouped rows; a binder may occupy a later column once earlier columns distinguish its row)
- [x] Explicit match motives (a term checked against the eliminator's motive type — `match v : (k, v) => Vec(T, k + m)`)
- [x] Primitive match families (Boolean arms, natural-number induction and literal dispatch, list fold and case split, and packed `Bits`/`Bytes` folds)
- [x] `choose` (an ordered guarded `Bool` condition ladder `choose | cond => … | _ => … end` with a mandatory `_` default; arms inherit their condition's definitional refinement)
- [x] Bind-arms (`| pattern = value =>`, Rust `if let`, in `choose`; refutable LHS, nested patterns, fallthrough shared through a nullary thunk)
- [x] Final `| _ =>` catch-all in headed inductive matches (bare/final/top-level only; lowers to the core `Cases::Induct` default)
- [x] Destructuring patterns at `let`/lambda-parameter/function-sugar-parameter position (tuple/struct only, irrefutable; desugars to projections)
- [ ] [Anonymous match functions](match_ergonomics/02_ANONYMOUS_MATCH_FUNCTION_SPEC.md) (`match =>`, lowering to an ordinary one-argument lambda and headed match) _(follows lambda inference on the same Phase 0-or-post-cutover schedule)_

## Syntax Sugar

- [x] Multi-parameter function syntax sugar
- [x] Monadic sequencing syntax (postfix `!`, dispatched through the `/syn/Monad` concept — no `let !` header; every value body is a region)
- [x] Field projection sugar (`.0`/`.label`)
- [x] Function-field sugar in every field list (`name(params) -> T` in tuple types and `struct` declarations, `name(args) = body` in tuple and struct literals — the forms concept/witness bodies always had) and trailing commas in every comma-separated list
- [x] Struct spread/update syntax (`T { ..base, f = x }` — one leading spread; labeled, declaration-ordered overrides; unwritten fields copied from the base, concept superclass fields included, overridable with `use <term>`; no tuple spread)
- [x] List/Bits/Bytes spread syntax (`[a, ..xs, b]`, `b\1\..bits\0`, `x\00\..bytes\01` — positional splices, any position/count, desugared to n-ary concat prims; packed literals stay whitespace-free with glued atomic operands and require their grain prefix; no tuple/string spread)

## Primitive Types

- [x] Primitives as orthogonal builtins _(uniform `/sys` builtin declarations, not parser-special-cased)_
  - [x] `Nat`
  - [x] `Byte` (i31 scalar; contextual literals `0..=255`; `Byte/to_nat` and wrapping `Nat/to_byte`)
  - [x] `Int`
  - [x] `Flt` (bit-preserving binary32 identity, including `to_le_bytes`/`of_le_bytes` reinterpretation across every compiler stage, plus the full native arithmetic and comparison family — `add`/`sub`/`mul`/`div`/`rem`/`min`/`max`/`neg`/`abs`/`sqrt`/`floor`/`ceil`/`trunc`/`nearest`/`copysign`)
  - [x] Packed `Bits` and `Bytes` (grain-specialized operations over shared immutable windows; O(1) slices and tails)
  - [x] `Lst`

## Module System

- [x] Cyclic module dependency resolution
- [x] Subtree-scoped privacy: a declaration without `pub` is visible within its declaring module's subtree, in both namespaces and for the declaration-local representation marker, so an abstraction can span several files without exporting how it is built
- [x] Exact private-item-in-public-interface audit (signature-only; audiences rather than declaration paths, so re-exports widen and a subtree-scoped item may name its own subtree; representation signatures checked only where exposed, through re-exports, identity aliases, and structurally direct-headed type-family aliases; opaque constructor namespaces cannot be re-exported)

## Compiler Pipeline

- [x] Closure capture analysis and atom-to-index erasure
- [x] CPS lowering with join blocks and tail instructions
- [x] Anyref-based uniform value representation with GC closures and tail calls
- [x] Binary WebAssembly serialization
- [x] WebAssembly text-format parsing and printing for the modeled feature set (round-tripped against the binary writer in `curios-wasm`)
- [x] WebAssembly execution via a shared, GC-enabled wasmtime engine
- [x] AOT `.cwasm` precompilation (deserialized and run without re-JITting)
- [x] Crate-boundary split isolating the Cranelift/Binaryen-free launcher (`curios-runtime`) from the JIT-capable compiler
- [x] Pure pipeline driver crate (`curios-pipeline`) decoupled from runtime/Binaryen/CLI, enabling a wasm32 (browser) build
- [x] Build-scoped archived prelude and replay (`curios-prelude` compiles and validates fixed Text/Core/Ersd state in `OUT_DIR`; production compilations restore it with no source fallback and lower/elaborate/erase only the user suffix)
- [x] Configurable type-checker reduction budget (the CLI's `--budget`, default 1,000,000 steps, restored per declaration; counting steps rather than elapsed time makes acceptance reproducible across machines, so the browser build needs no clock shim)
- [x] Elaboration and per-node memoization bounded by written binder nesting, never data length (the `elaborate → elaborate_apply → check` cycle defunctionalized onto a frame stack for ground, all-explicit applications; each term's cached derivations carried on the shared `Rc` node and filled by an iterative post-order walk — so a literal or generated spine of any size compiles on a default 2MB stack, the ceiling now being the reduction deadline and memory)
- [x] Names as identity only (a compiler name distinguishes bindings and renders for a human, and nothing branches on its spelling: `Free`/`Global`/`Mint`/`WitnessId` replace the five facts that were flattened into one `String`, constructor runtime tags are declaration order rather than alphabetical rank, anonymous witnesses carry an identity rather than a manufactured name, and no accessor reaches a spelling from a `Free` outside the printer)
- [x] Crate-boundary split separating the term representation from the elaborator (`curios-core` holds `Term`, its binder discipline, the primitive roster and folds, universes, and the nominal registry; `curios-elab` holds elaboration, unification, zonking, the universe solver, witness resolution, and erasure — with `Reducer` as the seam that shares primitive folding while leaving reduction strategy to each side)
- [ ] Independent kernel in `curios-core` re-checking what the elaborator accepts, so a rule the elaborator gets wrong is caught unless the kernel gets it wrong the same way (see [DESIGN.md](DESIGN.md), "An independent kernel re-checks what the elaborator accepts"). Written: reduction (agreeing with the elaborator's reducer under test), the sort query, conversion with proof irrelevance and a coinductive recurrence rule, the typing judgment with its primitive table, and nominal elimination — each arm checked at its own constructor's index targets, with the large-elimination guard deciding its singleton side condition by whether the index targets *pin* a payload component rather than by whether they mention it. Also written: the module entry point (`recheck_module`), which walks a zonked module and checks every item in order. Blocked on levels reaching the kernel ground: a zonked module still carries unsolved universe metavariables, which the kernel has no solver to reconcile and must not guess at, so real programs are refused today. Closing that means defaulting the surviving metas at declaration finalization and extending `validate_bound_universes` to reject a term-level meta. Also not written: coverage (an absent arm is not asked to justify its absence) and the free-monoid carriers' arms
- [ ] [Full data section support in `curios-wasm`](compiler/07_WASM_FULL_CONFORMANCE_SPEC.md) (active data segments, `memory.init`/`data.drop`, and the complete linear-memory load/store instruction family; today the section is minimum-fitted to its one consumer — passive-only segments reached through `array.new_data`)
- [ ] [Full element section support in `curios-wasm`](compiler/07_WASM_FULL_CONFORMANCE_SPEC.md) (every element-segment mode with table declarations and table instructions; today the section is minimum-fitted to its one consumer — a single declarative segment making functions `ref.func`-eligible)

## Optimizations

- [x] Core calculus machinery (reduction & conversion performance)
- [x] Ersd v2 (the semantic half of the backend, symmetric to Cont v2: a flat, verified, first-order erased representation replacing the legacy erased-term representation outright)
  - [x] Module-owned arenas of single-operation statements with distinct scalar shapes, schema-carrying products and variants, first-class switch and fold forms, and deterministic printing
  - [x] Derived arena analyses (free values, uses, and recursive components)
  - [x] Erasure as pure transcription under the once-per-expression operand law (scalars, functions, applications, aggregates, eliminations to first-class switches/folds/variant matches, and recursive groups with verifier-owned rejection)
  - [x] Fixed prelude erased once at compiler build time and replayed from the archive
  - [x] Behavior-summary pruning of unreachable pure items over the behavior oracle
  - [x] Partial evaluation (closed-term folding and recursive literal-spine specialization)
  - [x] Monoid worker/wrapper (deferred recursion rebased onto tail accumulators through forwarders)
  - [x] Single normative lowering into the landed continuation interface, where every encoding decision is made
  - [x] Production cutover to the arena replay path and deletion of the legacy erasure and erased representation
- [x] Continuation IR v2 (a pre-closure CPS graph replacing the region-based post-closure optimizer)
  - [x] Arena-backed high CPS with delayed closure conversion
  - [x] Interprocedural optimizer (literal folding, effect-aware dead-binding and dead-parameter elimination, bounded inlining, and contification)
  - [x] Recursive-SCC known-argument propagation, specialization, and branch specialization
  - [x] Structured Wasm control flow by SCC condensation (blocks, loops, and one localized dispatcher for irreducible scopes)
  - [x] Region-optimizer late passes measured redundant in the new pipeline and removed (common-subexpression elimination, pure-call evaluation, literal hoisting, tag/callee threading, loop-invariant motion, list-map simplification, and slice forwarding)
- [x] Wasm-emission optimizations
  - [x] `struct.new` construction with immutable fields
  - [x] Direct `br` for single-target regions
- [x] Binaryen closed-world post-optimization pass

## IO

- [x] `Handle` unified byte-stream abstraction
- [x] Terminal
- [x] File
- [x] Client network (TCP)
- [x] Server network (TCP)
- [x] TLS (https) for client and server sockets
- [x] Non-blocking IO & concurrent connection handling
- [x] Never-reused fd handle tokens (monotonic mint counter, use-after-close hardening)
- [x] Clock & randomness
- [x] Process IO

## Host Interface (FFI)

- [x] Self-describing foreign-function store (`curios-abi`'s `ForeignFunction`/`WireSignature`/`ForeignStore`): the `/sys/Handle` prelude declarations, elaboration, wasm `sys.*` imports, and runtime linking all derive from one per-compilation store of named signature rows — the generic `Foreign` IR nodes carry the row itself, and the runtime links by pulling the module's imports from a name-keyed registry
- [x] Surface `foreign` declarations (user-visible FFI syntax): a program's own declarations accumulate a second, `ffi`-tier store `compile_entrypoint` hands back
- [x] Embedder-extensible host-function registry (`curios-runtime::ForeignBindings`, threaded through `run_bytes`/`instantiate`/`run_wasm`)

## Diagnostics

- [x] Span-based error quality across all stages
- [x] Diagnostic terms printed with names in scope
- [x] Bare written goals (`?`) report their local scope, expected type, and optional inferred solution
- [ ] [Required labeled written goals (`?label`), complete goal batches, and a typed incomplete checking outcome](program_analysis/01_WRITTEN_GOALS_SPEC.md) _(after the Curios elaborator is authoritative, so the feature has one production implementation)_

## Standard Library

- [x] Canonicalized module layout and registration
- [x] Foundational proof/logic types (`std/True`, `std/False`)
- [x] Equality and ordering (`std/Eq`, `std/Order`)
- [x] Foundational sum types (`std/Option`, `std/Result`)
- [x] Core collections (`std/Lst` and its helpers, length-indexed `std/Vec`)
- [x] Key-value map (`std/Map`: a canonical crit-bit trie over `Bytes` keys — same entries, same shape — with injective key encodings via its `Key` concept)
- [x] Proof-carrying UTF-8 string storage and decoding (`std/Str`; decoded scalar values are exposed as certified `Char` values while storage remains packed `Bytes`)
- [x] Certified Unicode-scalar `Char` type and `Str` migration (`'…' : Char`, typed character APIs, explicit Byte/Char/Nat boundaries, and ASCII-explicit classification and casing policy)
- [x] Parser-combinator library (`std/Parse`)
- [x] Typed format strings (`std/Fmt`)
- [x] Decimal numeric conversions (`of_str`/`to_str` for `Nat`, `Int`, and `Flt`; `Flt/to_str` renders the shortest round-trip binary32 decimal through exact `BigNat` digit generation)
- [x] JSON codec (`std/Json`)
- [x] TOML 1.0.0 codec over native `Int` and binary32 `Flt` (`std/Toml`; explicitly not fully TOML-conforming because numeric storage is native-width)
  - [ ] [Full TOML conformance over exact numerics](compiler/08_TOML_FULL_CONFORMANCE_SPEC.md) _(not refined; after the general rational `BigFlt` sequence)_
- [x] Async combinators for `/std/Async`
  - [x] `map`
  - [x] concurrent `both`/`race`/`select`
  - [x] result cell (`Cell`)
  - [x] `sleep`/`timeout`
- [x] HTTP client (`std/http`, built on `tcp` + `Async`)
- [x] Host-service modules (`std/time` `Instant`/`Duration`, `std/proc` `args`/`env`/`exit`, `std/rand`)
- [x] Arbitrary-precision naturals (`std/BigNat`, canonical and packed over `Bits`)
  - [x] Machine-checked additive and multiplicative laws, additive cancellation, order reflection/transitivity, and power-of-two interaction lemmas
- [x] Certified strictly-positive arbitrary-precision naturals (`std/NonZero`)
- [x] Arbitrary-precision integers (`std/BigInt` over the strictly-positive `std/NonZero`)
- [ ] Dyadic `BigFlt` exact core
  - [ ] [Canonical representation, exact operations, comparison, and witnesses](big_flt_dyadic/01_CORE_SPEC.md)
  - [ ] [Exact binary32 conversion and correctly rounded output](big_flt_dyadic/02_BINARY32_SPEC.md)
- [ ] [`BigInt` certified algebra, order, and binary-scale laws](big_flt_dyadic_proofs/01_BIG_INT_LAWS_SPEC.md)
- [ ] Dyadic `BigFlt` proof and quotient-boundary completion
  - [ ] [Algebra and order theorem corpus](big_flt_dyadic_proofs/02_LAWS_SPEC.md)
  - [ ] [Correctly rounded exact quotient conversion to binary32](big_flt_dyadic_proofs/03_RATIO_NARROWING_SPEC.md)
  - [ ] [Binary32 round-trip and correct-rounding proofs](big_flt_dyadic_proofs/04_BOUNDARY_PROOFS_SPEC.md)
- [ ] General rational `BigFlt` sequence _(explicitly after `wonder`; no umbrella-only implementation step)_
  - [ ] [`BigNat` certified Euclidean division, GCD, divisibility, and coprimality](big_flt_general/01_BIG_NAT_EUCLIDEAN_SPEC.md)
  - [ ] [General `BigFlt` reduced rational representation and exact operations](big_flt_general/02_CORE_SPEC.md)
  - [ ] [General canonical uniqueness, ring, and order laws](big_flt_general/03_LAWS_SPEC.md)
  - [ ] [General division and field laws](big_flt_general/04_FIELD_LAWS_SPEC.md)
  - [ ] [General rational binary32 boundaries](big_flt_general/05_BINARY32_SPEC.md)
  - [ ] [Exact decimal parsing and presentation](big_flt_general/06_DECIMAL_SPEC.md)

## Tooling & Ecosystem

- [x] CLI (`run` and `compile` subcommands — `compile` bundles a native executable: launcher + appended `.cwasm`)
- [x] Staged IR debugging (`--print`, comma-selected pipeline stages dumped to stderr)
- [x] Built-in tracing-based profiling harness (`make curios/profile`; per-span aggregation through the workspace-wide `profile` feature)
- [x] CI pipeline (fmt/check/clippy/test)
- [x] Multi-platform release automation (Linux x86_64/aarch64, macOS aarch64 native binaries + wasm playground bundle, via tag-triggered GitHub Releases)
- [x] Browser playground
  - [x] Run harness owned by `curios-web` (`compile`/`run`, with wire codes from `curios-abi` and a local bridge for the compiler's wire-ABI `Bin` payload shape)
- [x] Documentation
  - [x] Syntax overview, examples, and tutorial
  - [x] Full language reference
- [x] Benchmarks
  - [x] Internal benchmarks
  - [x] Cross-language benchmarks (Docker harness vs. Rust/OCaml/Node/Lean 4 native and Rust/Grain/AssemblyScript on wasmtime)
- [ ] Developer tooling
  - [ ] [`curios wonder` structured program-analysis interface](program_analysis/02_WONDER_SPEC.md) (source and semantic indexes, diagnostics, references, dependencies, witnesses, and snapshot queries)
  - [ ] [Code formatter](compiler/03_FRONTEND_TOOLING_SPEC.md)
  - [ ] [Terminal REPL](compiler/05_EXECUTION_TOOLING_SPEC.md)
  - [ ] [Language server](compiler/03_FRONTEND_TOOLING_SPEC.md) (hover, go-to-definition, highlighting)
  - [ ] [Package manager](compiler/04_PACKAGING_SPEC.md)
  - [ ] [Project manifest & discovery](compiler/04_PACKAGING_SPEC.md)
  - [ ] [`curios new` scaffolding](compiler/04_PACKAGING_SPEC.md)
  - [ ] [Linter](compiler/03_FRONTEND_TOOLING_SPEC.md)
  - [ ] [Test runner](compiler/05_EXECUTION_TOOLING_SPEC.md)
  - [ ] [Documentation generator](compiler/03_FRONTEND_TOOLING_SPEC.md)
