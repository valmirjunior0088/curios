# Roadmap

Tracks Curios development by feature area. Checkboxes reflect current codebase state, not chronological history — items whose description was later superseded by a rework are folded into the item that replaced them.

Specifications live under [roadmap/](roadmap). A campaign large enough to sequence gets its own directory, and so does a kind that has collected more than one — a shortcut that costs something today and was recorded when it was taken, a capability that does not exist yet and costs nothing until a consumer asks for it, or a cost the compiler could remove from code that is already correct. A specification with no siblings sits directly in `roadmap/` under its own name, since a directory holding one file says less than that file's name does. Those directories come and go as campaigns land, so listing `roadmap/` is how you see what is open — naming them here would go stale silently, and a directory cannot. Within a directory the numeric prefix is a reading order, and it is append-only: a landed specification leaves its number behind rather than renumbering its siblings.

An item's entry here is a summary and a link, never the specification in miniature. Name the capability and, for an unchecked item, what is wrong or missing today; leave rationale, mechanism, findings, and rejected alternatives to the owners named below.

Unchecked items may link to working implementation specifications. Unchecked items whose design is not refined yet instead link a placeholder specification marked "Not refined yet", possibly an umbrella covering several related items; the placeholder only reserves the specification location until refinement replaces it. When an item lands completely, transfer every durable contract and invariant to its owning source, module or crate documentation and tests; record its design rationale and rejected alternatives in [design.md](design.md) when cross-cutting or in the owning crate's `README.md` when crate-scoped; update remaining specifications to depend on the landed API rather than the working document; replace the linked checkbox with a checked plain-text summary; verify that nothing still references the specification filename; and delete the specification.

## Type System

- [x] Π-types, λ-abstractions, and application
- [x] Σ-types and dependent pairs
- [x] `let` bindings, recursive by their body, and `let … and …;` groups
- [x] Value-level mutual recursion in a `let` group, forced by need and guarded by the erased verifier
- [x] Bidirectional dependent type checking with full definitional equality
- [x] Eta-reduction for Π-types and Σ-types
- [x] Named tuple fields
- [x] `Prop` universe with definitional proof irrelevance
- [x] Implicit cumulative `Type` hierarchy with declaration-local universe polymorphism
- [x] Implicit arguments (`@`-marked binders)
  - [x] Plicity as part of function-type identity, with lambda-binder insertion
- [x] Instance arguments (`concept` and `satisfy` declarations, deterministic resolution)
  - [x] `Show`/`Eql`/`Ord`/`Monad` in the standard library
  - [x] Higher-kinded concepts (`Monad(M : (Type) -> Type)`, via flex-apply imitation)
  - [x] Multi-parameter keying (tuple of every parameter head)
  - [x] `Lift` embeddings (`/syn/Lift(M, N)`; one witness per ordered pair, never chained)
  - [x] Orphan rule (a witness is declared where its concept, or a type in its key, is)
  - [x] Witness groups (`satisfy C(A) { … } and D(B) { … }`, members resolving through one another)
  - [x] Concept-based operators (every infix, `&&`/`||` included, dispatches through a concept)
- [ ] [Auto-derive: a witness whose body the compiler writes](roadmap/auto-derive-spec.md)
- [x] Unified `struct` declarations (independent nominal and representation visibility)
- [x] Inductive types (`induct` declarations)
  - [x] Independent nominal/representation visibility, with opaque construction
  - [x] Constructor registry & dependent eliminators
  - [x] Indexed families (e.g. `Vec`)
  - [x] Variant arity checking
  - [x] Exhaustiveness/coverage checking (index inversion)
  - [x] Large-elimination guard (restricts `Prop` → `Type` elimination)
  - [x] [Strict positivity modulo polarity](design/language/strict-positivity-modulo-polarity.md) (per-parameter polarity vectors)
- [x] Unification solver
  - [x] Pattern unification for higher-order metavariable spines
  - [x] Re-validate solutions in checking mode
  - [x] Surface residual unification constraints (a postponement, not a rigid mismatch)
  - [x] Metavariable-blocked conversions postpone instead of mismatching
  - [x] Solving reads materialized candidates and parks blocked goals under raw spellings
  - [x] Packed-literal views in unification decomposition (solving-side only)
  - [x] Right-biased partial imitation for flex-apply (what pins a two-parameter monad's `?M`)
  - [x] Witness keying through a partially applied type constructor (keyed on the stuck head)
- [ ] [Equality is defined at the type, not assumed over all of them](roadmap/observational-equality-spec.md) (not refined yet)

## Pattern Matching

- [x] Nested/tuple/struct match-arm patterns (the pattern-matrix compiler)
- [x] Multi-scrutinee matrix matching (a tuple scrutinee matched column by column)
- [x] Explicit match motives (a term checked against the eliminator's motive type)
- [x] Intrinsic match families (Boolean, `Nat`, list, and packed `Bits`/`Bytes` arms)
- [x] `choose` (an ordered guarded `Bool` ladder with a mandatory `_` default)
- [x] Bind-arms (`| pattern = value =>` in `choose`, Rust's `if let`; refutable LHS)
- [x] Final `| _ =>` catch-all in dispatching matches (bare, final, and top-level only)
- [x] Destructuring patterns at `let` and parameter positions (tuple/struct, irrefutable)
- [x] Irrefutable patterns at the `;` fold-hypothesis binder, as a `let` binder takes them

## Syntax Sugar

- [x] Multi-parameter function syntax sugar
- [x] Monadic sequencing syntax (postfix `!`, dispatched through the `/syn/Monad` concept)
- [x] Field projection sugar (`.0`/`.label`)
- [x] Function-field sugar in every field list, and trailing commas in every list
- [x] Struct spread/update syntax (`T { ..base, f = x }` — one leading spread, no tuple spread)
- [x] List/Bits/Bytes spread syntax (`[a, ..xs, b]`, `b[1, ..bits, 0]` — any position or count)
- [x] Packed single-atom entry (`b[head, ..tail]`, `x[..acc, b]` — one `Bool`/`Byte` generator)

## Intrinsic Types

- [x] Intrinsics as orthogonal builtins _(uniform `/sys` builtin declarations)_
  - [x] `Nat`
  - [x] `Byte` (i31 scalar; contextual literals `0..=255`; `Byte/to_nat` and `Nat/to_byte`)
  - [x] `Int`
  - [x] `Flt` (bit-preserving binary32 identity, with the full arithmetic and comparison family)
  - [x] Packed `Bits` and `Bytes` (shared immutable windows; O(1) slices and tails)
  - [x] `Flt` specified by a hardware-independent model, stated in this repository
  - [ ] Width-relative bit operations on `Bits`, where a width exists to relate them to
  - [x] `List`
- [x] [Total `/sys` primitives](design/language/a-partial-primitive-is-totalized-by-a-canonical-extension-or-it-states-its-domain.md) — an operation whose reduction could fail states its precondition
  - [x] The bound reaches Core and the kernel re-checks it, for every one of the twelve

## Module System

- [x] Cyclic module dependency resolution
- [x] Subtree-scoped privacy: a declaration without `pub` is visible within its subtree
- [x] Exact private-item-in-public-interface audit (signature-only, keyed on audiences)

## Compiler Pipeline

- [x] Closure capture analysis and atom-to-index erasure
- [x] CPS lowering with join blocks and tail instructions
- [x] Anyref-based uniform value representation with GC closures and tail calls
- [x] Binary WebAssembly serialization
- [x] WebAssembly text-format parsing and printing, round-tripped against the binary writer
- [x] WebAssembly execution via a shared, GC-enabled wasmtime engine
- [x] AOT `.cwasm` precompilation (deserialized and run without re-JITting)
- [x] Crate-boundary split isolating the Cranelift/Binaryen-free launcher (`curios-runtime`)
- [x] Pure pipeline driver crate (`curios-pipeline`), decoupled from runtime, Binaryen and CLI
- [x] Build-scoped archived prelude and replay (`curios-prelude-archive`, certified by `curios-prelude`)
- [x] [A compilation is units folded over a dependency order](design/toolchain/a-module-is-a-compilation-unit-and-the-prelude-is-an-environment.md), with `--unit <DIR>`
- [x] Configurable type-checker reduction budget (the CLI's `--budget`, restored per declaration)
- [x] Elaboration and per-node memoization bounded by written binder nesting, never data length
- [x] Elaboration transients grouped under one core variant (`Transient`), refused at the kernel
- [x] [Names as identity only](design/toolchain/one-naming-scheme-for-compiler-identities.md) — nothing branches on a name's spelling
- [x] [Totality of everything erasure deletes](design/language/totality-of-the-erased-program.md), so nothing inhabits `/syn/False`
- [x] Crate-boundary split separating the term representation from the elaborator (`curios-core`)
- [x] [Independent kernel in `curios-cert` re-checking what the elaborator accepts](design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md)
- [x] Crate-boundary split separating the rules both checkers run (`curios-analysis`)
- [x] Full memory and data section support in `curios-wasm` (plural memories, 32- and 64-bit)
- [x] Full table and element section support in `curios-wasm` (plural tables, every segment mode)
- [x] `Stage::WasmOptm`: the Binaryen-optimized module observable through `wonder stage`
- [ ] Self-hosting bootstrap of the language-specific stages _(deferred, unspecified)_

## Optimizations

- [x] Core calculus machinery (reduction & conversion performance)
- [x] Ersd v2 (a flat, verified, first-order erased representation)
  - [x] Module-owned arenas of single-operation statements, schemas, switches and folds
  - [x] Derived arena analyses (free values, uses, and recursive components)
  - [x] Erasure as pure transcription under the once-per-expression operand law
  - [x] Fixed prelude erased once at compiler build time and replayed from the archive
  - [x] Behavior-summary pruning of unreachable pure items over the behavior oracle
  - [x] Partial evaluation (closed-term folding and recursive literal-spine specialization)
  - [x] Monoid worker/wrapper (deferred recursion rebased onto tail accumulators)
  - [x] One normative lowering into the continuation interface, where encoding is decided
  - [x] Production cutover to the arena replay path, and the legacy erasure deleted
- [x] Continuation IR v2 (a pre-closure CPS graph replacing the region-based optimizer)
  - [x] Arena-backed high CPS with delayed closure conversion
  - [x] Interprocedural optimizer (folding, dead-code elimination, inlining, contification)
  - [x] Recursive-SCC known-argument propagation, specialization, and branch specialization
  - [x] Structured Wasm control flow by SCC condensation (a dispatcher per irreducible scope)
  - [x] Region-optimizer late passes retired, and the CPS pipeline grew its own where earned
- [x] A dataflow substrate for `curios-cont`, with unboxed scalar locals as its first payoff
- [x] Returning through several continuations, so a tagged union becomes control flow
- [x] A copied body reproduces the definitions nested inside it
- [x] Moving an application into the function that returns it, so a monadic step stops allocating
- [x] An idiomatic string walk stops building a suffix and a closure per character
- [ ] Specializing on a known function argument, so a combinator stops calling through it
- [x] The unfolding discard decides on progress
- [x] [A reduction step costs what it builds](design/toolchain/a-reduction-step-costs-what-it-builds.md)
- [x] [A type-level concatenation no longer copies what it joins](soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md)
- [x] [A value costs when it is kept, not when it is named](design/toolchain/a-value-costs-when-it-is-kept-not-when-it-is-named.md)
- [x] A string literal is checked once per use
- [x] [A closed fold no longer costs what its data is long](design/toolchain/evaluating-a-closed-term-is-representation-not-judgment.md)
- [x] [A variant collapses when nothing needs to distinguish it](design/toolchain/a-variant-collapses-when-nothing-needs-to-distinguish-it.md)
- [x] [A variant travels as the fields of its widest constructor](design/toolchain/a-variant-travels-as-the-fields-of-its-widest-constructor.md)
- [x] [A pure program rebuilds what an impure one would mutate](design/toolchain/the-heap-is-sized-ahead-of-its-churn.md)
- [ ] [The survivors are what cost](roadmap/collector-economics-spec.md) (the mechanism is settled; the route is not)
- [x] [The map wall falls by classes, not by symptom](design/toolchain/the-map-wall-falls-by-classes-not-by-symptom.md)
- [x] [A monomorphic field carries its own type](design/toolchain/a-field-is-declared-at-the-carrier-its-shape-names.md)
- [x] The map's remaining distance is decomposed before it is spent
- [x] [A closure carries its code as a table index](design/toolchain/a-closure-carries-its-code-as-a-table-index.md)
- [x] [Recursion restored to the defunctionalized walks](design/toolchain/depth-is-bought-with-stack-not-with-hand-rolled-frames.md)
- [x] Wasm-emission optimizations
  - [x] `struct.new` construction with immutable fields
  - [x] Direct `br` for single-target regions
- [x] Binaryen closed-world post-optimization pass
- [x] Five measured cliffs where an ordinary spelling cost superlinearly, or refused
- [x] [A product of two symbolic sums is its own weak-head form](design/toolchain/a-sum-is-merged-when-it-is-forced-not-when-it-is-built.md)
- [x] [A stuck comparison is spelled one way](design/toolchain/a-comparison-is-spelled-one-way-when-it-is-stuck.md)

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

- [x] Self-describing foreign-function store (`curios-abi`'s `ForeignFunction`/`ForeignStore`)
- [x] Surface `foreign` declarations, accumulating a second `ffi`-tier store
- [x] Embedder-extensible host-function registry (`curios-runtime::ForeignBindings`)

## Diagnostics

- [x] A bound whose subject does not terminate is refused by name, as a declared type is
- [x] Span-based error quality across all stages
- [x] A self-referential value reports rather than asserts, naming the path
- [x] Diagnostic terms printed with names in scope
- [x] Bare written goals (`?`) report their local scope, expected type, and any solution
- [x] Complete written-goal batches: one elaboration reports every reached goal, located
- [x] [Goal suggestions (`? ≈`)](design/toolchain/goal-suggestions-are-depth-one-fits-not-proof-search.md): sandboxed candidate fits, verified to compile
- [x] Goal suggestions reach what a program has not already mentioned

## Standard Library

- [x] Canonicalized module layout and registration
- [x] Foundational proof/logic types (`std/True`, `std/False`)
- [x] Equality and ordering (`std/Eq`, `std/Order`)
- [x] Foundational sum types (`std/Option`, `std/Result`)
- [x] Pure state threading (`std/State`; no `Lift(Io, State(S))` edge, so a region performs nothing)
- [x] Short-circuiting failure (`std/Throw` over `Result`; `!` as checked early return)
- [x] Core collections (`std/List` and its helpers, length-indexed `std/Vec`)
- [x] Key-value map (`std/Map`: a canonical crit-bit trie over `Bytes` keys)
- [x] Proof-carrying UTF-8 string storage and decoding (`std/Str`, over packed `Bytes`)
- [x] Certified Unicode-scalar `Char` type and `Str` migration (`'…' : Char`, typed APIs)
- [x] Parser-combinator library (`std/Parse`)
- [x] Typed format strings (`std/Fmt`)
- [x] Decimal numeric conversions (`of_str`/`to_str` for `Nat`, `Int` and `Flt`; they round-trip)
- [x] JSON codec (`std/Json`)
- [x] TOML 1.0.0 codec over native `Int` and binary32 `Flt` (`std/Toml`; not fully conforming)
  - [ ] [TOML's numbers are wider than the carriers under them](roadmap/toml-full-conformance-spec.md) (the gap is named; the float carrier is not chosen)
- [x] Async combinators for `/std/Async`
  - [x] `map`
  - [x] concurrent `both`/`race`/`select`
  - [x] result cell (`Cell`)
  - [x] `sleep`/`timeout`
- [x] Purity through an opaque `Io` monad (three intrinsics: `Io(T)`, `pure`, `bind`)
  - [x] Stage 1: the `Io` vocabulary (`/sys/Io`, `/std/Io`, the `Monad` witness)
  - [x] Stage 2: the flip — `/std` retyped and the certifier's purity analysis deleted
- [x] HTTP client (`std/http`, built on `tcp` + `Async`)
- [x] Host-service modules (`std/time`, `std/proc`, `std/rand`)
- [x] Arbitrary-precision naturals (`std/BigNat`, canonical and packed over `Bits`)
  - [x] Machine-checked additive, multiplicative, cancellation and order laws
- [x] Certified strictly-positive arbitrary-precision naturals (`std/BigPos`)
- [x] Arbitrary-precision integers (`std/BigInt` over the strictly-positive `std/BigPos`)
- [ ] Dyadic `BigFlt` exact core
  - [ ] [Canonical representation, exact operations, comparison, and witnesses](roadmap/big-flt-dyadic/01-core-spec.md)
  - [ ] [Exact binary32 conversion and correctly rounded output](roadmap/big-flt-dyadic/02-binary32-spec.md)
- [ ] [`BigInt` certified algebra, order, and binary-scale laws](roadmap/big-flt-dyadic/03-big-int-laws-spec.md)
- [ ] Dyadic `BigFlt` proof and quotient-boundary completion
  - [ ] [Algebra and order theorem corpus](roadmap/big-flt-dyadic/04-laws-spec.md)
  - [ ] [Correctly rounded exact quotient conversion to binary32](roadmap/big-flt-dyadic/05-ratio-narrowing-spec.md)
  - [ ] [Binary32 round-trip and correct-rounding proofs](roadmap/big-flt-dyadic/06-boundary-proofs-spec.md)
- [ ] General rational `BigFlt` sequence _(after `curios wonder`)_
  - [ ] [`BigNat` certified Euclidean division, GCD, divisibility, and coprimality](roadmap/big-flt-general/01-big-nat-euclidean-spec.md)
  - [ ] [General `BigFlt` reduced rational representation and exact operations](roadmap/big-flt-general/02-core-spec.md)
  - [ ] [General canonical uniqueness, ring, and order laws](roadmap/big-flt-general/03-laws-spec.md)
  - [ ] [General division and field laws](roadmap/big-flt-general/04-field-laws-spec.md)
  - [ ] [General rational binary32 boundaries](roadmap/big-flt-general/05-binary32-spec.md)
  - [ ] [Exact decimal parsing and presentation](roadmap/big-flt-general/06-decimal-spec.md)

## Tooling & Ecosystem

- [x] CLI (`run` and `compile` subcommands; `compile` bundles a native executable)
- [x] Staged IR debugging (`wonder stage <name>`, one pipeline rung reprinted to stdout)
- [x] Built-in tracing-based profiling harness (`cargo xtask profile`, per-span aggregation)
- [x] CI pipeline (fmt/check/clippy/test)
- [x] Multi-platform release automation (Linux and macOS binaries, via tag-triggered releases)
- [x] Browser playground
  - [x] Run harness owned by `curios-js` (`compile`/`run`, with wire codes from `curios-abi`)
- [x] Documentation
  - [x] Syntax overview, examples, and tutorial
  - [x] Full language reference
- [x] Benchmarks
  - [x] Internal benchmarks
  - [x] Cross-language benchmarks (a Docker harness against seven other languages)
- [ ] Developer tooling
  - [x] `curios wonder` — questions answered by the compilation, over a CLI and a language server
  - [x] Editor support — a tree-sitter grammar, and Zed and VS Code extensions on `wonder server`
  - [x] Code formatter (`curios format`, in-place with `--check`; verified by reparse)
  - [ ] Terminal REPL
  - [x] Package manager (exactly pinned dependencies, a content-addressed store, and a unit cache)
  - [x] [Payload reuse](soundness/admission-without-judgment/reused-payloads.md) (an unchanged target re-executes without recompiling)
  - [x] Project manifest & discovery (identity declared once; scope reached through artifacts)
  - [ ] Project reconciliation: what a project declares and does not use, and what nothing names
  - [x] `curios new` scaffolding (a package named after its directory, with both halves written)
  - [x] One-line installer (`install.sh`, versioned by its URL and checksum-verified)
  - [ ] Linter
  - [ ] [Test runner](roadmap/test-harness-spec.md) (`curios test`; no way to state or run a test today)
  - [ ] Documentation generator
