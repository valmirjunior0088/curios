# Roadmap

Tracks Curios development by feature area. Checkboxes reflect current codebase state, not chronological history — items whose description was later superseded by a rework are folded into the item that replaced them.

Unchecked items may link to working implementation specifications. Unchecked items whose design is not refined yet instead link a placeholder specification marked "Not refined yet", possibly an umbrella covering several related items; the placeholder only reserves the specification location until refinement replaces it. When an item lands completely, transfer every durable contract and invariant to its owning source, module or crate documentation and tests; record its design rationale and rejected alternatives in [DESIGN.md](DESIGN.md) when cross-cutting or in the owning crate's `README.md` when crate-scoped; update remaining specifications to depend on the landed API rather than the working document; replace the linked checkbox with a checked plain-text summary; verify that nothing still references the specification filename; and delete the specification.

## Type System

- [x] Π-types, λ-abstractions, and application
- [x] Σ-types and dependent pairs
- [x] `let` and `let-rec` bindings
- [x] Value-level mutual recursion in `rec`
- [x] Bidirectional dependent type checking with full definitional equality (β/ι/δ reduction, intrinsic computation, indexed-inductive inversion)
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
  - [x] `Lift` embeddings (`/syn/Lift(M, N)` with `Monad` superclasses; one witness per ordered pair, never chained; `/std/Async` declares `Lift(Io, Async)`; missing-edge reports name the sequencing, monadhood, and any declared chain)
  - [x] Orphan rule (a witness must be declared where its concept, or a type in its key, is already declared; the standard library's three roots — `sys`/`syn`/`std` — are exempt from the check against each other, one coordinated implementation rather than independent packages)
  - [x] Concept-based operators (every infix, `&&`/`||` included, dispatches through `Add`/`Sub`/`Mul`/`Div`/`Rem`/`And`/`Or`/`Eql`/`Cmp` with `/sys` witnesses; intrinsic codegen unchanged)
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
  - [x] Surface residual unification constraints (a drain survivor reports as a postponement naming its still-unsolved watched metavariables, noting live match-arm refinements, distinct from a rigid mismatch; the witness-hole case stays its own third state)
  - [x] Metavariable-blocked conversions postpone instead of mismatching (a structural mismatch stuck on an unsolved metavariable parks watching it, empty watch sets still fail fast; flex-scrutinee match pairs decompose only with identical motives and case tables)
  - [x] Solving reads materialized candidates and parks blocked goals under raw spellings (committed solutions splice before the occurs/scope analyses, so a solved metavariable's spine cannot strand a cascade; retries re-reduce raw goals in their restored frames)
  - [x] Packed-literal views in unification decomposition (a nonempty `Bits`/`Bytes` literal decomposes against `append`/`concat` spines length-directedly, solving-side only — the shared reduction and peel laws are untouched)
  - [x] Right-biased partial imitation for flex-apply (`?M(?A) ≟ T(b̄, x)` under-applied commits `λx. T(b̄, x)` with the suffix equated pairwise and the split re-validated against the birth type — what pins a two-parameter monad's `?M` from its region)
  - [x] Witness keying through a partially applied type constructor (`satisfy (@S) => Monad((A) => State(S, A))` keys on the stuck application's head; registration and goal lookup share the one `of_whnf` arm)

## Pattern Matching

- [x] Nested/tuple/struct match-arm patterns (the pattern-matrix compiler — full enumeration, no row priority)
- [x] Multi-scrutinee matrix matching (a tuple scrutinee matched column by column with grouped rows; a binder may occupy a later column once earlier columns distinguish its row)
- [x] Explicit match motives (a term checked against the eliminator's motive type — `match v : (k, v) => Vec(T, k + m)`)
- [x] Intrinsic match families (Boolean arms, natural-number induction and literal dispatch, list fold and case split, and packed `Bits`/`Bytes` folds)
- [x] `choose` (an ordered guarded `Bool` condition ladder `choose | cond => … | _ => … end` with a mandatory `_` default; arms inherit their condition's definitional refinement)
- [x] Bind-arms (`| pattern = value =>`, Rust `if let`, in `choose`; refutable LHS, nested patterns, fallthrough shared through a nullary thunk)
- [x] Final `| _ =>` catch-all in headed inductive matches (bare/final/top-level only; lowers to the core `Cases::Induct` default)
- [x] Destructuring patterns at `let`/lambda-parameter/function-sugar-parameter position (tuple/struct only, irrefutable; desugars to projections)
- [x] Irrefutable patterns at the `;` fold-hypothesis binder (`| pred + 1; (count, live) =>` — the hypothesis binds the fold result rather than scrutinee shape, so it takes the same patterns a `let` binder does, desugared to projections)
- [ ] [Anonymous match functions](roadmap/compiler/06_ANONYMOUS_MATCH_FUNCTION_SPEC.md) (`match =>`, lowering to an ordinary one-argument lambda and headed match; staged — the syntax ships checking-position first, then the form's own parked inference-position machinery)

## Syntax Sugar

- [x] Multi-parameter function syntax sugar
- [x] Monadic sequencing syntax (postfix `!`, dispatched through the `/syn/Monad` concept — no `let !` header; every value body is a region; the region's monad is read strictly from its type, and a cross-monad action lifts through the declared `/syn/Lift` edge)
- [x] Field projection sugar (`.0`/`.label`)
- [x] Function-field sugar in every field list (`name(params) -> T` in tuple types and `struct` declarations, `name(args) = body` in tuple and struct literals — the forms concept/witness bodies always had) and trailing commas in every comma-separated list
- [x] Struct spread/update syntax (`T { ..base, f = x }` — one leading spread; labeled, declaration-ordered overrides; unwritten fields copied from the base, concept superclass fields included, overridable with `use <term>`; no tuple spread)
- [x] List/Bits/Bytes spread syntax (`[a, ..xs, b]`, `b[\1, ..bits, \0]`, `x[\00, ..bytes, \01]` — positional splices, any position/count, desugared to n-ary concat intrinsics; packed literals are bracketed like lists behind a glued grain letter; no tuple/string spread)
- [x] Packed single-atom entry (`b[head, ..tail]`, `x[..acc, b]` — one `Bool`/`Byte` generator where `..` takes a whole value, desugared to `append` over what precedes it; the cons and append forms are literal syntax rather than named `/std` functions)

## Intrinsic Types

- [x] Intrinsics as orthogonal builtins _(uniform `/sys` builtin declarations, not parser-special-cased)_
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
- [x] Elaboration transients grouped under one core variant (`Transient`: `Infix`, `NumLit`, and `Bang` — postfix `!` carried into core unresolved and desugared by `elaborate_bang`, where the type-directed lift decision lives; refused wholesale at the kernel boundary)
- [x] Names as identity only (a compiler name distinguishes bindings and renders for a human, and nothing branches on its spelling: `Free`/`Global`/`Mint`/`WitnessId` replace the five facts that were flattened into one `String`, constructor runtime tags are declaration order rather than alphabetical rank, anonymous witnesses carry an identity rather than a manufactured name, and no accessor reaches a spelling from a `Free` outside the printer)
- [x] Totality of everything erasure deletes, so no closed term inhabits `/syn/False` by a divergent type or a divergent proof (size-change termination per `rec` group, classifying rather than rejecting; obligation **(T)** over type positions and **(V)** over `Prop`-sorted terms, both seeded from what elaboration settled; partiality persisted on `Definition` and inherited across the prelude archive — see [DESIGN.md](DESIGN.md), "Totality of the erased program", and [SOUNDNESS.md](SOUNDNESS.md), which also records the one route still open)
- [x] Crate-boundary split separating the term representation from the elaborator (`curios-core` holds `Term`, its binder discipline, the intrinsic roster and folds, universes, and the nominal registry; `curios-elab` holds elaboration, unification, zonking, the universe solver, witness resolution, and erasure — with `Reducer` as the seam that shares intrinsic folding while leaving reduction strategy to each side)
- [x] Independent kernel in `curios-cert` re-checking what the elaborator accepts, from the finished terms alone — reduction, sort, conversion, the typing judgment, nominal elimination (the large-elimination guard's singleton condition decided rather than approximated), subsumption as its own cumulative relation, universe constraint entailment in both directions, and declaration acceptance (constructor/field sizing, strict positivity, size-change totality), with index inversion, positivity, totality, and universe entailment shared rather than duplicated behind the `Env`/`Judge` seam (see [DESIGN.md](DESIGN.md), "An independent kernel re-checks what the elaborator accepts"). On the compile path in production: `recheck_module_suffix` runs inside `compile_entrypoint` and a refusal fails the compile, with the fixed prelude validated the same way at archive-build time and no source fallback. The whole-prelude disagreement count closed at **0 of 1052** across all three profiles; two positions remain named rather than closed — an elimination's motive/arms and `rec`-group/spine-argument comparison at `Type` — both recorded conversion incompleteness that does not count against the gate.
- [ ] [Full data section support in `curios-wasm`](roadmap/compiler/07_WASM_FULL_CONFORMANCE_SPEC.md) (active data segments, `memory.init`/`data.drop`, and the complete linear-memory load/store instruction family; today the section is minimum-fitted to its one consumer — passive-only segments reached through `array.new_data`)
- [ ] [Full element section support in `curios-wasm`](roadmap/compiler/07_WASM_FULL_CONFORMANCE_SPEC.md) (every element-segment mode with table declarations and table instructions; today the section is minimum-fitted to its one consumer — a single declarative segment making functions `ref.func`-eligible)
- [ ] [`Stage::WasmOptm`: observe the Binaryen-optimized module through a binary reader](roadmap/compiler/09_WASM_OPTM_STAGE_SPEC.md) (`reader.rs`/`from_bytes` as the binary writer's inverse over the pinned feature envelope; sequenced after full conformance, since bulk-memory optimization can rewrite into the forms the representation cannot yet hold)
- [ ] Self-hosting bootstrap of the language-specific stages _(deferred until further notice, deliberately unspecified; the objective and the Curios/Rust ownership split are recorded in [DESIGN.md](DESIGN.md))_

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
- [x] Complete written-goal batches: one elaboration reports every reached goal, located by file, line, and column, tolerantly materialized, with operator witness projections folded back to infix and terms rendered within a fixed width through the printer's width-aware document layer _(labels and a typed incomplete checking outcome remain possible extensions)_
- [x] Goal suggestions (`? ≈`): sandboxed local and application candidate fits in goal reports — complete candidates verified to compile when pasted, capped at three, with goal-bearing programs exiting 2 so tooling distinguishes incomplete from erroneous

## Standard Library

- [x] Canonicalized module layout and registration
- [x] Foundational proof/logic types (`std/True`, `std/False`)
- [x] Equality and ordering (`std/Eq`, `std/Order`)
- [x] Foundational sum types (`std/Option`, `std/Result`)
- [x] Pure state threading (`std/State`: the context-first carrier `State(S, A)` with `state`/`get`/`put`/`modify`/`run` and the parametric `Monad` witness; deliberately no `Lift(Io, State(S))` edge, so a `State` region provably performs nothing)
- [x] Short-circuiting failure (`std/Throw` over `Result`: `raise`/`rescue`/`of`/`run` with the parametric `Monad` witness — `!` as checked early return)
- [x] Core collections (`std/Lst` and its helpers, length-indexed `std/Vec`)
- [x] Key-value map (`std/Map`: a canonical crit-bit trie over `Bytes` keys — same entries, same shape — with injective key encodings via its `Key` concept)
- [x] Proof-carrying UTF-8 string storage and decoding (`std/Str`; decoded scalar values are exposed as certified `Char` values while storage remains packed `Bytes`)
- [x] Certified Unicode-scalar `Char` type and `Str` migration (`'…' : Char`, typed character APIs, explicit Byte/Char/Nat boundaries, and ASCII-explicit classification and casing policy)
- [x] Parser-combinator library (`std/Parse`)
- [x] Typed format strings (`std/Fmt`)
- [x] Decimal numeric conversions (`of_str`/`to_str` for `Nat`, `Int`, and `Flt`; `Flt/to_str` renders the shortest round-trip binary32 decimal through exact `BigNat` digit generation)
- [x] JSON codec (`std/Json`)
- [x] TOML 1.0.0 codec over native `Int` and binary32 `Flt` (`std/Toml`; explicitly not fully TOML-conforming because numeric storage is native-width)
  - [ ] [Full TOML conformance over exact numerics](roadmap/compiler/08_TOML_FULL_CONFORMANCE_SPEC.md) _(not refined; after the general rational `BigFlt` sequence)_
- [x] Async combinators for `/std/Async`
  - [x] `map`
  - [x] concurrent `both`/`race`/`select`
  - [x] result cell (`Cell`)
  - [x] `sleep`/`timeout`
- [x] Purity through an opaque Io monad (three intrinsics — `Io(T)`, `pure`, `bind` — every host operation retyped to return `Io`, the entrypoint tail an `Io({})` the emitted boundary forces once, and `curios-cert`'s purity analysis deleted in favor of the typing invariant; supersedes the reverted algebraic-effects design)
  - [x] Stage 1: the `Io` vocabulary (`/sys/Io`, `/std/Io`, the `Monad` witness; no behavior change)
  - [x] Stage 2: the flip — the host surface and `/std` retyped, the `Io({})` entrypoint contract, `curios-cert/src/purity.rs` deleted _(breaking: every program's tail becomes an `Io`)_
- [x] HTTP client (`std/http`, built on `tcp` + `Async`)
- [x] Host-service modules (`std/time` `Instant`/`Duration`, `std/proc` `args`/`env`/`exit`, `std/rand`)
- [x] Arbitrary-precision naturals (`std/BigNat`, canonical and packed over `Bits`)
  - [x] Machine-checked additive and multiplicative laws, additive cancellation, order reflection/transitivity, and power-of-two interaction lemmas
- [x] Certified strictly-positive arbitrary-precision naturals (`std/NonZero`)
- [x] Arbitrary-precision integers (`std/BigInt` over the strictly-positive `std/NonZero`)
- [ ] Dyadic `BigFlt` exact core
  - [ ] [Canonical representation, exact operations, comparison, and witnesses](roadmap/big_flt_dyadic/01_CORE_SPEC.md)
  - [ ] [Exact binary32 conversion and correctly rounded output](roadmap/big_flt_dyadic/02_BINARY32_SPEC.md)
- [ ] [`BigInt` certified algebra, order, and binary-scale laws](roadmap/big_flt_dyadic_proofs/01_BIG_INT_LAWS_SPEC.md)
- [ ] Dyadic `BigFlt` proof and quotient-boundary completion
  - [ ] [Algebra and order theorem corpus](roadmap/big_flt_dyadic_proofs/02_LAWS_SPEC.md)
  - [ ] [Correctly rounded exact quotient conversion to binary32](roadmap/big_flt_dyadic_proofs/03_RATIO_NARROWING_SPEC.md)
  - [ ] [Binary32 round-trip and correct-rounding proofs](roadmap/big_flt_dyadic_proofs/04_BOUNDARY_PROOFS_SPEC.md)
- [ ] General rational `BigFlt` sequence _(explicitly after the program-analysis interface; no umbrella-only implementation step)_
  - [ ] [`BigNat` certified Euclidean division, GCD, divisibility, and coprimality](roadmap/big_flt_general/01_BIG_NAT_EUCLIDEAN_SPEC.md)
  - [ ] [General `BigFlt` reduced rational representation and exact operations](roadmap/big_flt_general/02_CORE_SPEC.md)
  - [ ] [General canonical uniqueness, ring, and order laws](roadmap/big_flt_general/03_LAWS_SPEC.md)
  - [ ] [General division and field laws](roadmap/big_flt_general/04_FIELD_LAWS_SPEC.md)
  - [ ] [General rational binary32 boundaries](roadmap/big_flt_general/05_BINARY32_SPEC.md)
  - [ ] [Exact decimal parsing and presentation](roadmap/big_flt_general/06_DECIMAL_SPEC.md)

## Tooling & Ecosystem

- [x] CLI (`run` and `compile` subcommands — `compile` bundles a native executable: launcher + appended `.cwasm`)
- [x] Staged IR debugging (`--print`, comma-selected pipeline stages dumped to stderr)
- [x] Built-in tracing-based profiling harness (`make curios/profile`; per-span aggregation through the workspace-wide `profile` feature)
- [x] CI pipeline (fmt/check/clippy/test)
- [x] Multi-platform release automation (Linux x86_64/aarch64, macOS aarch64 native binaries + wasm playground bundle, via tag-triggered GitHub Releases)
- [x] Browser playground
  - [x] Run harness owned by `curios-web` (`compile`/`run`, with wire codes from `curios-abi` and a local bridge for the compiler's wire-ABI `Bytes` payload shape)
- [x] Documentation
  - [x] Syntax overview, examples, and tutorial
  - [x] Full language reference
- [x] Benchmarks
  - [x] Internal benchmarks
  - [x] Cross-language benchmarks (Docker harness vs. Rust/OCaml/Node/Lean 4 native and Rust/Grain/AssemblyScript on wasmtime)
- [ ] Developer tooling
  - [ ] Structured program-analysis interface: symbols, references, dependencies, witnesses, and exact source ranges over a reusable analysis crate — respec fresh when scheduled (the retired `wonder` draft lives in git history; its durable insights are source-versus-semantic tagging and snapshot-bound identities). Goal reports already cover the diagnostics slice, and `let _ : ? = f;` answers ad-hoc type queries through the solved-goal report.
  - [x] Code formatter (`curios format`, in-place with `--check`: canonical width-100/indent-4 style over the comment-capturing parse product and the width-aware printing algebra, verified by reparse before anything is written; the prelude stands formatted as the style corpus)
  - [ ] Terminal REPL
  - [ ] Language server (hover, go-to-definition, highlighting)
  - [ ] Package manager
  - [ ] Project manifest & discovery
  - [ ] `curios new` scaffolding
  - [ ] Linter
  - [ ] Test runner
  - [ ] Documentation generator
