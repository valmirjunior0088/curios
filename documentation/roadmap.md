# Roadmap

Tracks Curios development by feature area. Checkboxes reflect current codebase state, not chronological history — items whose description was later superseded by a rework are folded into the item that replaced them.

Specifications live under [roadmap/](roadmap). A campaign large enough to sequence gets its own directory, and so does a kind that has collected more than one — a shortcut that costs something today and was recorded when it was taken, a capability that does not exist yet and costs nothing until a consumer asks for it, or a cost the compiler could remove from code that is already correct. A specification with no siblings sits directly in `roadmap/` under its own name, since a directory holding one file says less than that file's name does. Those directories come and go as campaigns land, so listing `roadmap/` is how you see what is open — naming them here would go stale silently, and a directory cannot. Within a directory the numeric prefix is a reading order, and it is append-only: a landed specification leaves its number behind rather than renumbering its siblings.

An item's entry here is a summary and a link, never the specification in miniature. Name the capability and, for an unchecked item, what is wrong or missing today; leave rationale, mechanism, findings, and rejected alternatives to the owners named below.

Unchecked items may link to working implementation specifications. Unchecked items whose design is not refined yet instead link a placeholder specification marked "Not refined yet", possibly an umbrella covering several related items; the placeholder only reserves the specification location until refinement replaces it. When an item lands completely, transfer every durable contract and invariant to its owning source, module or crate documentation and tests; record its design rationale and rejected alternatives under [`design/`](design) when cross-cutting or in the owning crate's `README.md` when crate-scoped; update remaining specifications to depend on the landed API rather than the working document; replace the linked checkbox with a checked plain-text summary; verify that nothing still references the specification filename; and delete the specification.

## Language

### Core calculus

- [x] Π-types and Σ-types, λ-abstraction, application and dependent pairs, with eta-reduction for both
- [x] Named tuple fields
- [x] `let` bindings, recursive by their body, and `let … and …;` groups, whose value-level mutual recursion is forced by need and guarded by the erased verifier
- [x] Implicit cumulative `Type` hierarchy with declaration-local universe polymorphism
- [x] `Prop` universe with definitional proof irrelevance, asked before either side is reduced in both checkers ([A proof is never reduced to decide what irrelevance decides](design/language/a-proof-is-never-reduced-to-decide-what-irrelevance-decides.md))
- [x] Implicit arguments (`@`-marked binders), with plicity as part of function-type identity and lambda-binder insertion
- [x] [Totality of everything erasure deletes](design/language/totality-of-the-erased-program.md), so nothing inhabits `/std/Bool/False`

### Data types

- [x] Unified `struct` declarations (independent nominal and representation visibility)
- [x] Inductive types (`induct` declarations), with independent nominal/representation visibility and opaque construction
  - [x] Constructor registry & dependent eliminators
  - [x] Indexed families (e.g. `std/Tui/Layout/Sizes`)
  - [x] Variant arity checking
  - [x] Exhaustiveness/coverage checking (index inversion)
  - [x] Large-elimination guard (restricts `Prop` → `Type` elimination; erasure reads a payload the guard admits as pinned back from the scrutinee's index)
  - [x] [Strict positivity modulo polarity](design/language/strict-positivity-modulo-polarity.md) (per-parameter polarity vectors)
- [x] Structure and concept groups (`struct A … and B …`, `concept A … and B …`, members naming one another)

### Concepts and witnesses

- [x] Instance arguments (`concept` and `satisfy` declarations, deterministic resolution)
- [x] The concept roster (`Add`, `Sub`, `Mul`, `Div`, `Rem`, `And`, `Or`, `Cmp`, `Eql`, `Monad`, `Lift` and `Spell`, beside `Show`, `Ord` and `/std/Map`'s `Key`, all in `/std`)
- [x] Concept-based operators (every infix, `&&`/`||` included, dispatches through a concept)
- [x] Witness keys: a multi-parameter concept keys on the tuple of every parameter head, a tuple on the shape `{_, _}` with its labels, a function on the plicity vector `(_) -> _`, and a partially applied type constructor on its stuck head
- [x] Higher-kinded concepts (`Monad(M : (Type) -> Type)`, via flex-apply imitation)
- [x] `Lift` embeddings (`/std/Lift(M, N)`; one witness per ordered pair, never chained)
- [x] Orphan rule (a witness is declared where its concept, or a type in its key, is)
- [x] Witness groups (`satisfy C(A) { … } and D(B) { … }`, members resolving through one another)
- [x] [Sealed concept representations](design/language/concept-representations-may-be-sealed.md) (`concept C(A): Type` — witness declarations, dictionary literals and raw projections confined to the declaring subtree)
- [x] Concept laws (a field whose type is a proposition about earlier fields, discharged by `satisfy` at the implementations it supplies)
- [x] Associated types (a field whose result is a sort — what lets `Div` state each carrier's own division precondition)
- [x] Superclass edges (a `use`-prefixed field; `use value` fills a slot in a concept literal, never in a `satisfy`, and an `Ord(A)` witness answers an `Eql(A)` goal by projection)
- [x] [Derived witnesses](design/language/a-witness-body-may-be-written-by-the-compiler.md) (`satisfy C(T);` writes the body from the key's declaration)

### Pattern matching

- [x] Nested/tuple/struct match-arm patterns (the pattern-matrix compiler), and multi-scrutinee matrix matching (a tuple scrutinee matched column by column)
- [x] Explicit match motives (a term checked against the eliminator's motive type)
- [x] Dependent elimination at the ambient goal (an omitted motive over a variable scrutinee is the expected type, specialized per arm; no convoy)
- [x] Intrinsic match families (Boolean, `Nat`, list, and packed `Bits`/`Bytes` arms)
- [x] `choose` (an ordered guarded `Bool` ladder with a mandatory `_` default), with bind-arms (`| pattern = value =>`, Rust's `if let`; refutable LHS)
- [x] Final `| _ =>` catch-all in dispatching matches (bare, final, and top-level only)
- [x] Irrefutable destructuring patterns (tuple/struct) at `let`, parameter and `;` fold-hypothesis binders
- [ ] [Typed patterns: a wildcard beside a concrete pattern in any column, coverage against the scrutinee's constructors, and redundant arms reported](roadmap/typed-patterns-spec.md)

### Surface syntax

- [x] Multi-parameter function syntax sugar
- [x] Monadic sequencing syntax (postfix `!`, dispatched through the `/std/Monad` concept)
- [x] Field projection sugar (`.0`/`.label`)
- [x] Function-field sugar in every field list, and trailing commas in every list
- [x] Struct spread/update syntax (`T { ..base, f = x }` — one leading spread, no tuple spread)
- [x] List/Bits/Bytes spread syntax (`[a, ..xs, b]`, `b[1, ..bits, 0]` — any position or count), and packed single-atom entry (`b[head, ..tail]`, `x[..acc, b]` — one `Bool`/`Byte` generator)

## Checking

### Elaboration

- [x] Bidirectional dependent type checking with full definitional equality
- [x] Unification solver
  - [x] Pattern unification for higher-order metavariable spines
  - [x] Re-validate solutions in checking mode
  - [x] Surface residual unification constraints (a postponement, not a rigid mismatch)
  - [x] Metavariable-blocked conversions postpone instead of mismatching
  - [x] Solving reads materialized candidates and parks blocked goals under raw spellings
  - [x] Packed-literal views in unification decomposition (solving-side only)
  - [x] Right-biased partial imitation for flex-apply (what pins a two-parameter monad's `?M`)
  - [x] A lambda whose expectation never gains structure settles by synthesizing its type (unannotated domains stand as named metavariables for the body to pin)
- [x] Elaboration transients grouped under one core variant (`Transient`), refused at the kernel

### The certifier

- [x] [Independent kernel in `curios-cert` re-checking what the elaborator accepts](design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md)
- [ ] [Certifier, part 1: independent certification records and call-site discovery](roadmap/certifier-pt1-spec.md) — totality stamps still come from elaboration and call-site discovery is shared
  - [ ] The certifier profiled
  - [ ] Its own verdict record in place of the carried totality stamps
  - [ ] Call sites recorded during the certifier's own typing walk
- [ ] [Certifier, part 2: checked evidence and trusted reasoning](roadmap/certifier-pt2-spec.md) — not refined yet; certificate transport and stronger restrictions on trusted implementations
  - [ ] Evidence checked, beginning with linear integer arithmetic's certificates
  - [ ] Trusted-code requirements refined and enforced by the dependency graph
  - [ ] The remaining soundness-perimeter changes, including the evaluator and shared reasoning

### What conversion decides about the carriers

- [x] [A law is decided where it neither respells nor invents](design/toolchain/a-law-is-decided-where-it-neither-respells-nor-invents.md) — a left shift by a literal count, parity, a position inside a window, a map by a function convertible to the identity, and De Morgan with absorption, each moved from the law grid's refused rows to its held ones
- [x] [Euclid's identity, a comparison split by sign, `Nat/to_int` as an ordered-semiring embedding, and a shift by a symbolic count](soundness/per-term-rules/open-fold-laws-and-the-sum-normal-form.md), each decided by both checkers and moved from the law grid's refused rows to its held ones
- [x] [A product of two symbolic sums is its own weak-head form](design/toolchain/a-sum-is-merged-when-it-is-forced-not-when-it-is-built.md)
- [x] [A stuck comparison is spelled one way](design/toolchain/a-comparison-is-spelled-one-way-when-it-is-stuck.md)
- [ ] [Algebra, part 1: one owner for existing behavior](roadmap/algebra-pt1-spec.md) — today's mathematics is interleaved with Core terms and its comparison strategy is repeated in both checkers
  - [ ] Existing intrinsic algebra consolidated into `curios-algebra`, with term adapters in Core
  - [ ] One shared comparison strategy in Analysis and an inversion interface restricted to admissible deductions
  - [ ] Baseline behavior and costs verified, replaced implementations removed, and durable contracts documented
- [ ] [Algebra, part 2: broader reasoning and representations](roadmap/algebra-pt2-spec.md) — not refined yet; capabilities beyond the existing algebra
  - [ ] Canonical algebraic forms and refinement keys
  - [ ] Stronger Boolean and bitwise reasoning
  - [ ] Linear integer arithmetic with search outside the certifier and checked certificates
  - [ ] Expanded polynomial reasoning and unification
  - [ ] Additional operation and morphism declarations, including `pow`
  - [ ] One internal sequence carrier, preserving the guest carriers
  - [ ] Bounds discharged from hypotheses by ordinary proofs
  - [ ] Declaration-generated law grids and a theory audit

### Compile-time cost

- [x] Configurable type-checker reduction budget (the CLI's `--budget`, restored per declaration), in which [a reduction step costs what it builds](design/toolchain/a-reduction-step-costs-what-it-builds.md)
- [x] Core calculus machinery (reduction & conversion performance)
- [x] Elaboration and per-node memoization bounded by written binder nesting, never data length, and a function type elaborated in its size, not its size times its binders (measured)
- [x] [A closed fold no longer costs what its data is long](design/toolchain/evaluating-a-closed-term-is-representation-not-judgment.md)
- [x] [A type-level concatenation no longer copies what it joins](soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md)
- [x] A string literal is checked once per use
- [x] The unfolding discard decides on progress
- [x] Five measured cliffs where an ordinary spelling cost superlinearly, or refused

## Numbers and intrinsic carriers

### Carriers

- [x] Intrinsics as orthogonal builtins _(uniform `/sys` builtin declarations)_
- [x] `Bool` (conjunction, disjunction, exclusive or, and both equalities)
- [x] `Byte` (i31 scalar; contextual literals `0..=255`; `Byte/to_nat` and `Nat/to_byte`)
- [x] `Nat`, unbounded at run time as in the theory — an i31 while small and a boxed magnitude past it, with no arbitrary-precision library beside it ([Nat and Int are an i31 until they outgrow it](design/toolchain/nat-and-int-are-an-i31-until-they-outgrow-it.md)) — with certified division with remainder and divisibility (`/std/Nat/div_mod` and `/std/Nat/Divides`)
- [x] `Int`, unbounded at run time as `Nat` is, with its order carried from `Nat` along the embedding (a sign view, trichotomy, and the laws of `/std/Int/Lt` and `/std/Int/Le`)
- [x] `Flt`, IEEE 754-2019 binary64 specified by a hardware-independent model stated in this repository: every bit pattern a value under one symmetric NaN rule, the five rounding directions and `fma`, exceptions as values and the environment as a monad, decimal and hexadecimal text in every direction, `/std/Dyadic` as a finite value's exact form, and §9.4's reductions and §9.5's augmented operations
- [x] Packed `Bits` and `Bytes` (shared immutable windows; O(1) slices and tails; pointwise `and`/`or`/`xor` under a decided equal-length bound, `replicate`, and the reinterpretation between grains under a decided alignment bound)
- [x] Bitwise vocabulary on the packed carriers (`not`, `shl`, `shr`, `rotl`, `rotr` at both grains over `/sys`'s `replicate`, length-preserving and positional; `Bits` and `Bytes` read least-significant-first without exception, and `Bits` is level with `Bytes` on the surface they share)
- [x] `List`
- [x] `Cell` (a mutable reference cell over any carrier, with `set` and `get`)
- [ ] [Host and guest boundary, part 1: guest coordination](roadmap/host-and-guest-boundary-pt1-spec.md) — write-once cells, bounded channels, level waiting and threaded session state
  - [x] `Option` declared in `/sys`, preserving explicit `/std` re-exports
  - [ ] Knot and program cells share write-once semantics; initialized construction, `Cell/set` and `Cell/get` removed
  - [ ] The `Channel` intrinsic with a positive-capacity obligation and atomic outcomes
  - [ ] Scheduler state threaded through its loop; opaque waits replace wakers and notification lists
  - [ ] `Tui/Session` state threaded through reading, size tracking and drawing
- [ ] [Host and guest boundary, part 2: host operations and outcomes](roadmap/host-and-guest-boundary-pt2-spec.md) — canonical operation contracts and checked adapters; write progress, flushing and poll-wide failure remain design decisions
  - [ ] `Byte` on the wire and exit as a diverging row
  - [ ] `Result` declared in `/sys`, preserving explicit `/std` re-exports and reusing part 1's `Option`
  - [ ] Checked host outcomes, guest reply validation, buffer ownership and resource transitions
  - [ ] Ordinary `/sys` outcome wrappers over wire-shaped Core calls; domain vocabulary retained in `/std`
  - [ ] Native, mock, plugin and browser conformance, with independent protocol fixtures
- [x] [Total `/sys` primitives](design/language/a-partial-primitive-is-totalized-by-a-canonical-extension-or-it-states-its-domain.md) — an operation whose reduction could fail states its precondition
  - [x] The bound reaches Core and the kernel re-checks it, for every one of the thirteen
  - [x] A bound is read off the node that carries it, and the oracle closed on a criterion
  - [x] A fact is stated once, or the copies are checked — the fold arms' grain twins, the decomposition's carriers and its two measures, and a key's encoding

### Numeric library

- [ ] [`Nat`: the Euclidean remainder past certified division, the unsigned binary scale, and `min` and `max` as declared operations](roadmap/nat-laws-spec.md)
- [ ] [`Int`: multiplicative cancellation, `abs`, `sign`, `min` and `max` as declared operations, and the signed scale](roadmap/int-laws-spec.md)
- [ ] [`Flt`: its decided laws and the theorems over its bits](roadmap/flt-laws-spec.md)
- [ ] [Correctly rounded Flt elementary functions](roadmap/flt-elementary-spec.md) — the §9.2 functions are absent; fuel and exhaustion still need a contract
- [ ] [Alternate floating-point exception handling](roadmap/flt-exception-handling-spec.md) — the remaining §8 policies and per-operation substitution and recording
- [ ] [Rat, part 1: exact rationals and their laws](roadmap/rat-pt1-spec.md) — a canonical rational library, executable binary64 conversions, exact decimals and library proofs
- [ ] [Rat, part 2: proofs of rational–binary64 conversion](roadmap/rat-pt2-spec.md) — not refined yet; the formal boundary theorems need a specified connection to the primitive floating-point model

## Modules and packages

- [x] Cyclic module dependency resolution
- [x] Subtree-scoped privacy: a declaration without `pub` is visible within its subtree
- [x] Exact private-item-in-public-interface audit (signature-only, keyed on audiences)
- [x] [A compilation is units folded over a dependency order](design/toolchain/a-module-is-a-compilation-unit-and-the-prelude-is-an-environment.md), every edge declared in a manifest
- [x] Project manifest & discovery (identity declared once; scope reached through artifacts)
- [x] Package manager (exactly pinned dependencies, a content-addressed store, and a unit cache; `curios pin` derives a row's hash from the delivery and writes it, so a pin is never computed by hand)
- [x] [Payload reuse](soundness/admission-without-judgment/reused-payloads.md) (an unchanged target re-executes without recompiling)
- [x] [A unit the store holds is a baseline, not a hit or a miss](design/toolchain/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md) (a question compiles an edited unit over the stored one, reusing every item the edit did not reach; elaboration and parsing recover per item)
- [x] `curios new` scaffolding (a package named after its directory, with both halves written)

## Compiler architecture

- [x] Pure pipeline driver crate (`curios-pipeline`), decoupled from runtime, Binaryen and CLI
- [x] Crate boundaries
  - [x] `curios-runtime`, isolating the Cranelift/Binaryen-free launcher
  - [x] `curios-core`, separating the term representation from the elaborator
  - [x] `curios-analysis`, separating the rules both checkers run
  - [x] `curios-verdicts` and `curios-wonder` kept off the native back end
  - [x] WebAssembly emission (`curios-emit`) and `curios-wasm` kept out of the prelude build
  - [x] `curios-text` off the elaborator, with the term builders in `curios-core`
- [x] Build-scoped archived prelude and replay (`curios-prelude-archive`, certified by `curios-prelude`), erased once at compiler build time and replayed from the archive
- [x] [Names as identity only](design/toolchain/one-naming-scheme-for-compiler-identities.md) — nothing branches on a name's spelling
- [x] [Recursion restored to the defunctionalized walks](design/toolchain/depth-is-bought-with-stack-not-with-hand-rolled-frames.md)
- [ ] [Multithreaded compilation](roadmap/multithreaded-compilation-spec.md) — a compilation walks one unit and one item at a time through state shared across items, so it occupies one core, and whether an item is accepted can depend on which items were elaborated before it
  - [ ] Cranelift compiles in parallel
  - [ ] One caching rule for both checkers
  - [ ] Interned names and a source map
  - [ ] Artifacts that carry no minted identity
  - [ ] Elaboration local to an item, against a complete witness index
  - [ ] One declaration environment, its reads recorded
  - [ ] Erasure per item, linked by name
  - [ ] A term representation that crosses threads, its cost measured
  - [ ] The executor, and the gate holding one worker and many to the same bytes

## Code generation

### Erasure and the erased IR

- [x] Closure capture analysis and atom-to-index erasure
- [x] Ersd v2 (a flat, verified, first-order erased representation)
  - [x] Module-owned arenas of single-operation statements, schemas, switches and folds
  - [x] Derived arena analyses (free values, uses, and recursive components)
  - [x] Erasure as pure transcription under the once-per-expression operand law
  - [x] Behavior-summary pruning of unreachable pure items over the behavior oracle
  - [x] Partial evaluation (closed-term folding and recursive literal-spine specialization)
  - [x] Monoid worker/wrapper (deferred recursion rebased onto tail accumulators)
  - [x] One normative lowering into the continuation interface, where encoding is decided
  - [x] Production cutover to the arena replay path, and the legacy erasure deleted

### The continuation IR

- [x] CPS lowering with join blocks and tail instructions
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

### Value representation

- [x] Anyref-based uniform value representation with GC closures and tail calls
- [x] [A variant collapses when nothing needs to distinguish it](design/toolchain/a-variant-collapses-when-nothing-needs-to-distinguish-it.md)
- [x] [A variant travels as the fields of its widest constructor](design/toolchain/a-variant-travels-as-the-fields-of-its-widest-constructor.md)
- [x] [A value costs when it is kept, not when it is named](design/toolchain/a-value-costs-when-it-is-kept-not-when-it-is-named.md)
- [x] [A monomorphic field carries its own type](design/toolchain/a-field-is-declared-at-the-carrier-its-shape-names.md)
- [x] [A closure carries its code as a table index](../curios-emit/README.md)
- [x] [A pure program rebuilds what an impure one would mutate](design/toolchain/a-pure-program-rebuilds-what-an-impure-one-would-mutate.md)

### WebAssembly

- [x] Binary WebAssembly serialization, and text-format parsing and printing round-tripped against the binary writer
- [x] Full memory and data section support in `curios-wasm` (plural memories, 32- and 64-bit), and full table and element section support (plural tables, every segment mode)
- [x] Wasm-emission optimizations: `struct.new` construction with immutable fields, and direct `br` for single-target regions
- [x] Binaryen closed-world post-optimization pass, observable as `Stage::WasmOptm` through `wonder stage`

### Measured workloads

- [x] [The map wall falls by classes, not by symptom](design/toolchain/the-map-wall-falls-by-classes-not-by-symptom.md), and the map's remaining distance is decomposed before it is spent
- [x] An idiomatic string walk stops building a suffix and a closure per character

## Runtime and host

### Execution

- [x] WebAssembly execution via a shared, GC-enabled wasmtime engine
- [x] AOT `.cwasm` precompilation (deserialized and run without re-JITting)

### Effects and IO

- [x] Purity through an opaque `Io` monad (three intrinsics: `Io(T)`, `pure`, `bind`)
  - [x] Stage 1: the `Io` vocabulary (`/sys/Io`, `/std/Io`, the `Monad` witness)
  - [x] Stage 2: the flip — `/std` retyped and the certifier's purity analysis deleted
- [x] The effect tier retyped: `Io` where a module suspends, `Try` where it can fail
- [x] Streams: `Async/Read` and `Async/Write` over every host handle, never a raw one
- [x] Non-blocking IO: every peer-facing handle is non-blocking and never waits on a peer
- [x] Never-reused fd handle tokens (monotonic mint counter, use-after-close hardening)
- [x] Terminal, with raw mode and window size (`/sys/tty`, wrapped by `std/Tty` with a restoring bracket; the terminal rows in `std/Io`)
- [x] File, and the filesystem over `Path` (`std/fs` in `Try` over `Io`; the browser denies every row)
- [x] Clock & randomness (`std/time`, `std/rand`)
- [x] Process IO (`std/proc`) and subprocesses (`std/Command`: `spawn`, `run` and `status`, a child's pipes as streams)
- [x] Client and server network (TCP), with TLS (https) for both
- [x] Serial ports (`/sys/serial`, wrapped by `std/Serial`: opened raw at a required speed and frame, DTR, RTS and the input discard, Linux enumeration)

### Concurrency

- [x] Structured concurrency in `/std/Async`
  - [x] `map`, and `sleep`/`timeout`
  - [x] Concurrent `race`/`first` over spawned tasks, `select` over offers, and `join_all` over a list of tasks
  - [x] Fibers (`go`) and tasks (`spawn`/`join`/`cancel`), over `Future`/`await`
  - [x] One park over a list of waits — a handle's readability, an elapsed duration, or a waker registration — resumed by whichever fires first and claimed once, with `yield_now` beside it, driven by the poll-based run loop (`block_on`/`run`); the park is the library's own, reached by a program through the stream types, `sleep`, `join` and `select`
  - [x] Scoped resource ownership (`using`), a finalizer run exactly once on both exits
  - [x] Deadlock detection (no runnable job, nothing blocked on a handle, no sleeper — reported with how many fibers wait on a waker nothing will fire, rather than hung)
- [x] A channel owns its state, and a fiber parks with none (`/std/Async/Channel`: a bounded queue with `Sender` and `Receiver` ends, every park one `park` over a list of offers claimed once)

### Foreign functions

- [x] Self-describing foreign-function store (`curios-abi`'s `ForeignFunction`/`ForeignStore`)
- [x] Surface `foreign` declarations, accumulating a second `ffi`-tier store
- [x] Embedder-extensible host-function registry (`curios-runtime::ForeignBindings`), filled from the manifest: a package names the WebAssembly module answering its declarations and which export answers which, `curate` fetches it against a file hash, `run` and `test` link it in process, and `compile` carries it inside the executable — so a program declaring `foreign` runs where there is no manifest, no sources and no compiler
- [ ] A plugin that speaks more than scalars and byte strings (a `Handle`, a `List` and several results at once are each refused where the signature is read, because marshalling is the host copying between a GC array and a linear memory)

## Standard library

### Foundations

- [x] Canonicalized module layout and registration
- [x] Foundational proof/logic types (`std/Bool/True`, `std/Bool/False`), and equality and ordering (`std/Eq`, `std/Ordering`)
- [x] Foundational sum types (`std/Option`, `std/Result`), with `std/Result` its own monad, error first, and `!` as checked early return
- [x] Pure state threading (`std/State`; no `Lift(Io, State(S))` edge, so a region performs nothing)
- [x] The error channel over any monad (`std/Try`: `raise`, `rescue`, `attempt` and `run`)
- [x] The host's failure vocabulary (`std/Io/Error`) and paths as host bytes (`std/Path`)

### Collections — the tier every one of nine surveyed peers ships

- [x] Core collections (`std/List` and its helpers, and `std/Vec`, which counts a list in its type)
- [x] Key-value map (`std/Map`: a canonical crit-bit trie over `Bytes` keys), with `Key(Nat)`, `Key(Byte)` and `Key(Bool)` over the encodings `/std/Hash` already gave them
- [ ] The certified sort, deferred to a consumer
- [ ] The `Ord`-keyed tree, deferred to a consumer

### Text and formats

- [x] Proof-carrying UTF-8 string storage and decoding (`std/Str`, over packed `Bytes`)
- [x] Certified Unicode-scalar `Char` type and `Str` migration (`'…' : Char`, typed APIs)
- [x] Character literals realize as numerals (`Char` by default, `Nat`/`Byte`/`Int` from context)
- [x] Parser-combinator library (`std/Parse`) and typed format strings (`std/Fmt`)
- [x] Decimal numeric conversions (`of_str`/`to_str` for `Nat`, `Int` and `Flt`; they round-trip)
- [x] JSON codec (`std/Json`; numbers are binary64 `Flt`, which is RFC 8259's interoperability recommendation, so only integers past 2⁵³ fail to round-trip)
- [x] TOML 1.0.0 codec over native `Int` and binary64 `Flt` (`std/Toml`; conforming on floats and on the full 64-bit integer range)
- [x] HTML as a tree (`/std/Html`, rendered escaped and read back as a browser reads it)

### Applications

- [x] HTTP client and server (`std/http` over `tcp` + `Async`; a handler answers each connection)
- [x] Command-line interfaces (`/std/Cli`: a specification computes the record a line parses into)
- [x] A terminal program draws a screen and reads keys (`/std/Tui`, with five widgets)

### Library-wide

- [ ] [Explicit invariants in the standard library](roadmap/standard-library-invariants-spec.md) — where a branch is unreachable for a reason the types do not state, `/std` answers a made-up value: a decoded character becomes `'?'`, re-encoded text `""`, and every text parser re-checks the `Str` it was handed

## Diagnostics

- [x] Span-based error quality across all stages
- [x] Diagnostic terms printed with names in scope
- [x] A self-referential value reports rather than asserts, naming the path
- [x] A bound whose subject does not terminate is refused by name, as a declared type is
- [x] [A failing program names what failed](design/toolchain/a-refusal-is-a-panic-the-emitter-renders.md) (one sentence per class; no program can spell it)
- [x] Written goals (`?`)
  - [x] A bare goal reports its local scope, expected type, and any solution
  - [x] Complete batches: one elaboration reports every reached goal, located
  - [x] [Goal suggestions (`? ≈`)](design/toolchain/goal-suggestions-are-depth-one-fits-not-proof-search.md): sandboxed candidate fits, verified to compile, reaching what a program has not already mentioned

## Tooling

### Commands and editors

- [x] CLI (`run`, `compile`, `test`, `document`, `curate`, `new`, `format`, `lint` and `wonder`)
- [x] `curios wonder` — questions answered by the compilation, over a CLI and a language server
  - [x] Staged IR debugging (`wonder stage <name>`, one pipeline rung reprinted to stdout)
  - [x] `wonder cost` — what became of each declaration by the time the optimizer settled, `survived`, `specialized <n>` or `absorbed`, read off the continuation graph either side of the optimizer with no pass instrumented
- [x] Editor support — a tree-sitter grammar, and Zed and VS Code extensions on `wonder server`
- [x] Code formatter (`curios format`, in-place with `--check`; verified by reparse)
- [x] [Linter](design/toolchain/a-lint-is-an-exact-finding-read-off-the-compilation.md) (`curios lint` — four exact, always-on lints read off name resolution: an unused import, binder, private declaration or dependency; reported beside diagnostics by `wonder` and the server, turned into an exit code by `lint` alone)
- [x] [Test runner](design/toolchain/a-test-is-a-declared-description-run-by-a-synthesized-tail.md) (`test name = body;` declarations run by `curios test`, listed by `wonder tests`)
- [x] [Documentation generator](design/toolchain/a-library-is-documented-for-its-consumers-from-the-compilation-that-builds-it.md) (`curios document` writes a package's library interface as static pages under `.curios/documentation/`, read off the compilation that builds it; `---` documentation comments are syntax attached to the declaration below them)

### Profiling

- [x] Built-in tracing-based profiling harness (`cargo x profile`, per-span aggregation)
- [x] Profiling is a property of the build, not a subcommand (`curios profile` retired; a `profile` build files every span and event of whatever it ran to `.artifacts/profile.tsv` beside the crate that wrote it, and `cargo x profile` folds it)
- [ ] What checking a declaration cost, in the budget's own machine-independent units (not refined yet)
- [ ] How often each priced site ran, from one execution (not refined yet)

### Distribution

- [x] CI pipeline (formatting, lints, tests, documentation, the browser bundle and the grammar)
- [x] Multi-platform release automation (Linux and macOS binaries, via tag-triggered releases)
- [x] One-line installer (`install.sh`, versioned by its URL and checksum-verified)
- [x] Browser playground, with its run harness owned by `curios-js` (`compile`/`run`, with wire codes from `curios-abi`)

### Documentation and benchmarks

- [x] Documentation: syntax overview and examples, and the full language reference
- [x] Benchmarks: internal, and cross-language (a Docker harness against six other languages in seven columns, Rust compiled both natively and to WebAssembly)
