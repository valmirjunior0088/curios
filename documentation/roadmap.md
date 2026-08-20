# Roadmap

Tracks Curios development by feature area. Checkboxes reflect current codebase state, not chronological history — items whose description was later superseded by a rework are folded into the item that replaced them.

Specifications live under [roadmap/](roadmap). A campaign large enough to sequence gets its own directory, and so does a kind that has collected more than one — a shortcut that costs something today and was recorded when it was taken, a capability that does not exist yet and costs nothing until a consumer asks for it, or a cost the compiler could remove from code that is already correct. A specification with no siblings sits directly in `roadmap/` under its own name, since a directory holding one file says less than that file's name does. Those directories come and go as campaigns land, so listing `roadmap/` is how you see what is open — naming them here would go stale silently, and a directory cannot. Within a directory the numeric prefix is a reading order, and it is append-only: a landed specification leaves its number behind rather than renumbering its siblings.

An item's entry here is a summary and a link, never the specification in miniature. Name the capability and, for an unchecked item, what is wrong or missing today; leave rationale, mechanism, findings, rejected alternatives, and every measured figure to the owners named below. A figure in particular belongs beside the probe that reproduces it and appears here in no form at all.

Unchecked items may link to working implementation specifications. Unchecked items whose design is not refined yet instead link a placeholder specification marked "Not refined yet", possibly an umbrella covering several related items; the placeholder only reserves the specification location until refinement replaces it. When an item lands completely, transfer every durable contract and invariant to its owning source, module or crate documentation and tests; record its design rationale and rejected alternatives in [design.md](design.md) when cross-cutting or in the owning crate's `README.md` when crate-scoped; update remaining specifications to depend on the landed API rather than the working document; replace the linked checkbox with a checked plain-text summary; verify that nothing still references the specification filename; and delete the specification.

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
  - [x] Higher-kinded concepts (`Monad(M : (Type) -> Type)`, via the flex-apply imitation rule in `curios-elab`'s `convert.rs`)
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
- [x] Final `| _ =>` catch-all in dispatching matches (bare/final/top-level only, after any run of dispatching arms — inductive constructors, `Bool`, `Nat` shapes, list and packed cases — and not after tuple or struct arms)
- [x] Destructuring patterns at `let`/lambda-parameter/function-sugar-parameter position (tuple/struct only, irrefutable; desugars to projections)
- [x] Irrefutable patterns at the `;` fold-hypothesis binder (`| pred + 1; (count, live) =>` — the hypothesis binds the fold result rather than scrutinee shape, so it takes the same patterns a `let` binder does, desugared to projections)

## Syntax Sugar

- [x] Multi-parameter function syntax sugar
- [x] Monadic sequencing syntax (postfix `!`, dispatched through the `/syn/Monad` concept — no `let !` header; every value body is a region; the region's monad is read strictly from its type, and a cross-monad action lifts through the declared `/syn/Lift` edge)
- [x] Field projection sugar (`.0`/`.label`)
- [x] Function-field sugar in every field list (`name(params) -> T` in tuple types and `struct` declarations, `name(args) = body` in tuple and struct literals — the forms concept/witness bodies always had) and trailing commas in every comma-separated list
- [x] Struct spread/update syntax (`T { ..base, f = x }` — one leading spread; labeled, declaration-ordered overrides; unwritten fields copied from the base, concept superclass fields included, overridable with `use <term>`; no tuple spread)
- [x] List/Bits/Bytes spread syntax (`[a, ..xs, b]`, `b[1, ..bits, 0]`, `x[0x00, ..bytes, 0x01]` — positional splices, any position/count, desugared to n-ary concat intrinsics; packed literals are bracketed like lists behind a glued grain letter; no tuple/string spread)
- [x] Packed single-atom entry (`b[head, ..tail]`, `x[..acc, b]` — one `Bool`/`Byte` generator where `..` takes a whole value, desugared to `append` over what precedes it; the cons and append forms are literal syntax rather than named `/std` functions)

## Intrinsic Types

- [x] Intrinsics as orthogonal builtins _(uniform `/sys` builtin declarations, not parser-special-cased)_
  - [x] `Nat`
  - [x] `Byte` (i31 scalar; contextual literals `0..=255`; `Byte/to_nat` and wrapping `Nat/to_byte`)
  - [x] `Int`
  - [x] `Flt` (bit-preserving binary32 identity, including `to_le_bytes`/`of_le_bytes` reinterpretation across every compiler stage, plus the full native arithmetic and comparison family — `add`/`sub`/`mul`/`div`/`rem`/`min`/`max`/`neg`/`abs`/`sqrt`/`floor`/`ceil`/`trunc`/`nearest`/`copysign`)
  - [x] Packed `Bits` and `Bytes` (grain-specialized operations over shared immutable windows; O(1) slices and tails)
  - [x] `List`
- [x] Total `/sys` primitives — every operation whose reduction could fail carries its precondition in its type, in the decided style (see [A partial primitive is totalized by a canonical extension, or it states its domain](design/language/a-partial-primitive-is-totalized-by-a-canonical-extension-or-it-states-its-domain.md) and [A bound is stated in a decided proposition and discharged by reduction](design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md))
  - [x] The bound reaches Core and the kernel re-checks it, for every one of the nine — the five whose reduction preserves its operands (`Nat/div`, `Nat/rem`, `Int/div`, `Int/rem`, `Flt/of_le_bytes`) and the four sequence accessors (`Bytes/get`, `Bytes/slice`, `List/get`, `List/slice`), which reduction rebuilds. A window is a start and a *count*, so an invalid one cannot be written and the surviving bound is *carried* from one window to the next rather than composed — reduction moves a proof it was handed and derives nothing

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
- [x] Build-scoped archived prelude and replay (`curios-prelude-archive` compiles fixed Text/Core/Ersd state into an image in its `OUT_DIR` and `curios-prelude` certifies it, the split keeping a kernel edit from re-elaborating the standard library; production compilations restore the image with no source fallback and lower/elaborate/erase only the user suffix)
- [x] A compilation is units folded over a dependency order, with `--unit <DIR>` mounting a package beside the entry program (see [A module is a compilation unit, and the prelude is an environment](design/toolchain/a-module-is-a-compilation-unit-and-the-prelude-is-an-environment.md))
- [x] Configurable type-checker reduction budget (the CLI's `--budget`, restored per declaration; counting units of reduction work rather than elapsed time makes acceptance reproducible across machines, so the browser build needs no clock shim — what a unit buys is [A reduction step costs what it builds](design/toolchain/a-reduction-step-costs-what-it-builds.md), and the default is `curios-elab`'s `DEFAULT_STEP_BUDGET`, which states what it was calibrated against)
- [x] Elaboration and per-node memoization bounded by written binder nesting, never data length (the `elaborate → elaborate_apply → check` cycle defunctionalized onto a frame stack for ground, all-explicit applications; each term's cached derivations carried on the shared `Rc` node and filled by an iterative post-order walk — so a literal or generated spine of any size compiles on a default 2MB stack, the ceiling now being the reduction deadline and memory)
- [x] Elaboration transients grouped under one core variant (`Transient`: `Infix`, `NumLit`, and `Bang` — postfix `!` carried into core unresolved and desugared by `elaborate_bang`, where the type-directed lift decision lives; refused wholesale at the kernel boundary)
- [x] Names as identity only — a compiler name distinguishes bindings and renders for a human, and nothing branches on its spelling (see [One naming scheme for compiler identities](design/toolchain/one-naming-scheme-for-compiler-identities.md))
- [x] Totality of everything erasure deletes, so no closed term inhabits `/syn/False` by a divergent type or a divergent proof (see [Totality of the erased program](design/language/totality-of-the-erased-program.md), and [soundness.md](soundness.md), which grades every rule that can admit a term)
- [x] Crate-boundary split separating the term representation from the elaborator (`curios-core` holds `Term`, its binder discipline, the intrinsic roster and folds, universes, and the nominal registry; `curios-elab` holds elaboration, unification, zonking, the universe solver, witness resolution, and erasure — with `Reducer` as the seam that shares intrinsic folding while leaving reduction strategy to each side)
- [x] Independent kernel in `curios-cert` re-checking what the elaborator accepts from the finished terms alone, on the compile path in production and at prelude-archive build time (see [An independent kernel re-checks what the elaborator accepts](design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md); what it covers is [`curios-cert/README.md`](../curios-cert/README.md), and its disagreement inventory is `curios-prelude-archive`'s `kernel_disagreements`)
- [x] Crate-boundary split separating the rules both checkers run from the kernel only one runs (`curios-analysis` holds the `Env`/`Judge` seam, index inversion, strict positivity, size-change totality, and universe satisfiability; `curios-elab` and `curios-cert` both depend on it and neither reverses, so a shared analysis is a place the two checkers cannot disagree, and `curios-elab` takes `curios-cert` as a dev-dependency alone — see [An independent kernel re-checks what the elaborator accepts](design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md))
- [x] Full memory and data section support in `curios-wasm` (plural declared and imported memories, each 32- or 64-bit addressed with its own limits; the complete load/store family under real memargs; the bulk-memory instructions; passive and active segments; and no memory a module did not declare)
- [x] Full table and element section support in `curios-wasm` (plural declared and imported tables, each 32- or 64-bit addressed and optionally carrying an initializer; the table instruction family with `call_indirect`/`return_call_indirect`; every element-segment mode and both list forms; and the segment-consuming GC array operations)
- [x] `Stage::WasmOptm`: the Binaryen-optimized module is observable through `--print wasm-optm`, rendered by Binaryen's own text writer in the session that optimized it (the one stage the pure pipeline never emits — the native product constructs it after `optimize`, which is what keeps Binaryen out of `curios-pipeline`)
- [ ] Self-hosting bootstrap of the language-specific stages _(deferred until further notice, deliberately unspecified; the objective and the Curios/Rust ownership split are recorded in [design.md](design.md))_

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
  - [x] Region-optimizer late passes retired with the region optimizer, and the CPS pipeline grew its own where they earned a place — scoped common-subexpression elimination over deterministic intrinsics, constant folding, and identity folding; the rest measured redundant here and were not reproduced
- [x] A dataflow substrate for `curios-cont`, and unboxed scalar locals as its first payoff — the lattice and SCC fixpoint welded to the specializer extracted into a shared solver (`cps/dataflow.rs`), the per-operand representation demand lifted onto the intrinsic roster as `Repr`, and scalars held in machine registers across a function body (`cps/represent.rs`). The emitted module is validated before Binaryen sees it. The scope is locals only, recorded with its enforcement in `curios-cont/README.md`
- [x] Returning through several continuations, so a tagged union becomes control flow — a class of functions decided together over the undirected tail-call graph hands back its construction's leading fields instead of a heap tuple the caller projects and switches on, and the caller rebuilds nothing. Demand analysis was the prerequisite and subsumed `eliminate_dead_parameters` on the way. `curios/src/tests/codegen/structural.rs` holds what it is worth
- [x] A copied body reproduces the definitions nested inside it — `clone_scc`, `clone_continuation` and `inline_known_calls` were three near-verbatim subtree copiers that each declined outright when the body nested a `LetFun` or `RecInit`, and a nested lambda is what higher-order code is made of. The walk is now written once, closed under lexical nesting, and the inliner counts the members it would duplicate rather than the outer body alone
- [x] Moving an application into the function that returns it, so a monadic step stops allocating — a `State(S, A)` is a `(S) -> {A, S}`, so an action *is* a closure and every `!` allocated one and called it indirectly; the application it always receives moves into the callee, needing no knowledge of *which* closure comes back. A class is decided per tail-call component with the width propagated across it, and width zero is excluded: with no argument to absorb, the rewrite would decide when a description *runs*, which for an `Io` is the whole of its meaning
- [x] An idiomatic string walk costs about a sixth of what it did, and none of the three changes that bought it was the mechanism proposed for it — every sequence fold in the language was materializing a suffix nothing reads, `/std/Str/fold`'s motive was a function so a walk built one closure per character, and `/std/Nat/of_str` decided emptiness by walking the whole string. `curios/src/tests/codegen/ladder.rs` carries the figures beside the probes that reproduce them, and `structural.rs` asserts the walk captures nothing per character
- [ ] Specializing on a known function argument, so a shared combinator stops calling through a parameter — `/std/Str/fold` is called from two places with two different step functions, so the parameter joins to a conflict and every call through it stays indirect, once per character. Every existing pass declines structurally, and nothing downstream is missing: `rewrite_atoms` devirtualizes the moment a callee value is known, so this is two existing passes recognizing one more atom
- [x] The unfolding discard decides on progress — a folded recursive call's one definitional unfolding is kept when it exposed a head constructor **or** carries no member of the group it recursed on, and discarded only when it is still neutral and still names that group; the head-shape test it replaced could not tell a stuck form from an answer that happens to be a variable. Both reducers carry the rule, written separately per the two-checker discipline
- [x] A reduction step costs what it builds — a transition costs one unit, a construction costs its logical size, a level of guarded recursion costs the native frame it takes, and a term-keyed memo hit costs nothing; a compilation-scoped quota bounds what the caches retain, exhausting into a cold cache rather than a refusal, and a refusal names the row it was refused on. See [A reduction step costs what it builds](design/toolchain/a-reduction-step-costs-what-it-builds.md), `curios-core`'s `cost` and `retention`, and `curios`'s `tests::reduction` for the figures
- [x] A type-level concatenation no longer copies what it joins — reduction declines to fuse an all-literal concatenation past a documented operand size, and the free monoid gained a **measure** beside the peel: one iterative fold over an already-reduced spine, taking no `Reducer` so that computing a length physically cannot re-enter reduction, with `len`, `get` and `slice` as its three consumers. `curios-core`'s `free_monoid` owns the mechanism, `curios`'s `tests::reduction` the figures, and [Intrinsic fold laws and the free-monoid peel](soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md) the law and its probes
- [x] A value costs when it is kept, not when it is named — the walk's per-character heap traffic is gone where its identity was never observed: continuation scalar replacement threads exact-arity products through join parameters, loop backedges the central case, and window virtualization carries a rope suffix as base, offset and length behind an extent guard that keeps the eager trap in place. The emitted `/std/Str/fold` body carries no accumulator tuple, no suffix view, and no slice-helper call. See [A value costs when it is kept, not when it is named](design/toolchain/a-value-costs-when-it-is-kept-not-when-it-is-named.md), `curios-cont`'s `fields` and `origin` modules, and `curios`'s `tests::codegen` ladder, census and mirror probes for the figures
- [x] A string literal is checked once per use — the kernel consulted its evaluation memo only at its crate boundary where the elaborator consults one at every reduction level, so it re-derived what the table already held; `whnf_within` now probes and stores at every level and the two checkers report the same cost for the same program. `curios`'s `str_literal_cost_measurements` carries what a literal costs and `a_str_literal_costs_transitions_rather_than_frames` holds it
- [x] A closed fold no longer costs what its data is long — closed, metavariable-free, refinement-free terms evaluate on one shared explicit-stack machine, `curios-core`'s `reduce_closed`, entered from both checkers' reduction paths, with every charge landing on the host's own counter through the same price list. A user's own fold over a packed carrier gets the same treatment by the same closedness gate, which is what distinguishes this from blessing `Str` the way Lean blesses its strings. **Two pre-existing defects it surfaced are still open:** a struct declaration whose proof field applies a fold to an earlier field overflows the default test-thread stack in debug with the machine on or off (`a_struct_refinement_field_overflows_the_test_thread_stack` is the repro), and the elaborator retains quadratically in a literal's length. See [Evaluating a closed term is representation, not judgment](design/toolchain/evaluating-a-closed-term-is-representation-not-judgment.md), [The closed machine](soundness/per-term-rules/the-closed-machine.md), `curios-core`'s `machine` module, and `curios`'s `tests::reduction` probes for the figures
- [x] A variant collapses when nothing needs to distinguish it — the tag and its tuple are paid only where discrimination can happen: a single-constructor family encodes as the struct with the same relevant row would, and a family with exactly one immediate-unary constructor rides it as the bare payload behind an `IsImmediate` test, which removes the leaf allocations of tree-shaped data outright. Eligibility is a `FieldShape` recorded per payload field on the ersd constructor schema at erasure. See [A variant collapses when nothing needs to distinguish it](design/toolchain/a-variant-collapses-when-nothing-needs-to-distinguish-it.md), `curios-ersd`'s `into_cont` encoding tests, and `curios`'s `trees_leaf_rides_its_payload` and collapsed-wrapper probes
- [x] A variant travels as the fields of its widest constructor — a parameter every flow reaches as a construction travels as fields even when the constructions disagree about width, the region taking the widest and each narrower edge carrying its own fields followed by filler, at continuation join parameters and at a non-escaping known function's parameters alike. The per-character path of an idiomatic UTF-8 walk now allocates nothing. See [A variant travels as the fields of its widest constructor](design/toolchain/a-variant-travels-as-the-fields-of-its-widest-constructor.md), `curios-cont`'s `fields` and `origin` modules, and `curios`'s `tests::codegen` ladder and census for the figures
- [x] A pure program rebuilds what an impure one would mutate — the death-birth churn campaign: two workloads (`churn`, `spines`) joined `chain` in the harness, the death-birth census located the population, the lever gate admitted the engine and refused reuse and coalescing, and the admitted campaign landed as [The heap is sized ahead of its churn](design/toolchain/the-heap-is-sized-ahead-of-its-churn.md). See that decision, the decomposition probes and `death_birth_census` in `curios/src/tests/codegen/`, and the harness's next capture for the cross-language delta; the successor is [A young value dies free](roadmap/generational-nursery-spec.md)
- [ ] [A young value dies free](roadmap/generational-nursery-spec.md) (deliberately unrefined: only the churn campaign's certain facts — the engine's collector inventory at the 47 pin, the three-site write-barrier surface, the measured recopying and cache economics, the one-row integration seam — recorded for whoever picks it up; the brainstorming is not started)
- [x] The map wall falls by classes, not by symptom — the campaign closed 2026-08-19 at ~1353 ns/insert against the 8320 baseline (6.1×): sequence reads split on the leaf, packed appends fuse to flat chunks, indexed list constructions flatten by census and demand (an internal `ListSettle`, never a surface name), and small `Bytes` and `Bits` ride the i31 small-canonically, with `Repr::Bin(Grain)` keeping the two layouts unconfusable by construction. **What the work found that its specification did not predict:** halving the read protocol's call count did not move the slope, so the decomposition's ~60% read share was the serial fork loads beside the calls, not the calls — the wall fell where the centerpiece framing put it, on the immediate key; the qp reshape, implemented in full, measured 1802 against the crit-bit's 1353 ns/insert, because the immediate key had inverted its pricing premise — seventeen near-free levels beat four or five that each copy an O(width) child array on an all-insert workload — so it was declined on its own probe, keeping the census's store-safety fixpoint; canonicity needs every materialization site, which the hoisted-constant mixed-pair bug demonstrated; and two pre-existing defects surfaced — the constant interner conflating same-byte different-length bit literals (fixed by keying logical length) and the literal-depth elaboration runaway (since cured by the metavariable walk's visited set). See [The map wall falls by classes, not by symptom](design/toolchain/the-map-wall-falls-by-classes-not-by-symptom.md), `map_wall_spines_slope` for every figure with its method, and the census assertion surface in `curios-ersd`'s `test_support`
- [ ] [A monomorphic field carries its own type](roadmap/typed-heap-fields-spec.md) (deliberately unrefined: the map-wall campaign's successor for field representation, recording only the shape of the work, the boundaries its evidence already drew, and the census that gates a go/no-go; no schedule claims until that census runs)
- [ ] [The map's remaining distance falls by a walk, a cast and a branch](roadmap/map-distance-spec.md) (all three steps landed 2026-08-20, plus the immediate-payload miscompile they surfaced: −41% cumulative on the insert slope, and the cast step alone −61% on `chain`, which a static census had priced as negligible and was wrong about. Re-scoped around what is now open — the ~744 ns per insert that no session has decomposed, and the field-representation successor whose census this paid part of)
- [x] A closure carries its code as a table index rather than a funcref — the environment's code field is an `i32` slot in one module-level funcref table filled by one active element segment (slot 0 null, so an unfilled recursive shell's zeroed field still traps), dispatched with `call_indirect`/`return_call_indirect`; construction writes a constant instead of paying wasmtime's per-store funcref-to-GC-heap intern. The annex it enabled — a closure whose captures are all interned constants is a constant aggregate like any tuple — retired the constant hoister's one exclusion. See [A closure carries its code as a table index](design/toolchain/a-closure-carries-its-code-as-a-table-index.md) and `curios`'s codegen probes for the figures
- [x] Recursion restored to the defunctionalized walks — the walks recurse inside `curios-utilities`'s `recurse` instead of driving explicit frame machines (see [Depth is bought with stack, not with hand-rolled frames](design/toolchain/depth-is-bought-with-stack-not-with-hand-rolled-frames.md))
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
- [ ] Goal suggestions reach what a program has not already mentioned — candidate pools are bounded by *reachability* today, so a standard-library or mounted-unit definition is offered only where the program already references it somewhere else and nothing unused is ever suggested. Ranking by type shape, indexing the scope's definitions by instantiated result head, is what turns the pool from reachability-bounded into relevance-bounded against the attempt cap

## Standard Library

- [x] Canonicalized module layout and registration
- [x] Foundational proof/logic types (`std/True`, `std/False`)
- [x] Equality and ordering (`std/Eq`, `std/Order`)
- [x] Foundational sum types (`std/Option`, `std/Result`)
- [x] Pure state threading (`std/State`: the context-first carrier `State(S, A)` with `state`/`get`/`put`/`modify`/`run` and the parametric `Monad` witness; deliberately no `Lift(Io, State(S))` edge, so a `State` region provably performs nothing)
- [x] Short-circuiting failure (`std/Throw` over `Result`: `raise`/`rescue`/`of`/`run` with the parametric `Monad` witness — `!` as checked early return)
- [x] Core collections (`std/List` and its helpers, length-indexed `std/Vec`)
- [x] Key-value map (`std/Map`: a canonical crit-bit trie over `Bytes` keys — same entries, same shape — with injective key encodings via its `Key` concept)
- [x] Proof-carrying UTF-8 string storage and decoding (`std/Str`; decoded scalar values are exposed as certified `Char` values while storage remains packed `Bytes`)
- [x] Certified Unicode-scalar `Char` type and `Str` migration (`'…' : Char`, typed character APIs, explicit Byte/Char/Nat boundaries, and ASCII-explicit classification and casing policy)
- [x] Parser-combinator library (`std/Parse`)
- [x] Typed format strings (`std/Fmt`)
- [x] Decimal numeric conversions (`of_str`/`to_str` for `Nat`, `Int`, and `Flt`; `Flt/to_str` renders the shortest round-trip binary32 decimal through exact `BigNat` digit generation)
- [x] JSON codec (`std/Json`)
- [x] TOML 1.0.0 codec over native `Int` and binary32 `Flt` (`std/Toml`; explicitly not fully TOML-conforming because numeric storage is native-width)
  - [ ] [Full TOML conformance over exact numerics](roadmap/toml-full-conformance-spec.md) _(not refined; after the general rational `BigFlt` sequence)_
- [x] Async combinators for `/std/Async`
  - [x] `map`
  - [x] concurrent `both`/`race`/`select`
  - [x] result cell (`Cell`)
  - [x] `sleep`/`timeout`
- [x] Purity through an opaque Io monad (three intrinsics — `Io(T)`, `pure`, `bind` — every host operation retyped to return `Io`, the entrypoint tail an `Io({})` the emitted boundary forces once, and `curios-cert`'s purity analysis deleted in favor of the typing invariant; supersedes the reverted algebraic-effects design)
  - [x] Stage 1: the `Io` vocabulary (`/sys/Io`, `/std/Io`, the `Monad` witness; no behavior change)
  - [x] Stage 2: the flip — the host surface and `/std` retyped, the `Io({})` entrypoint contract, and the certifier's purity analysis deleted _(breaking: every program's tail becomes an `Io`)_
- [x] HTTP client (`std/http`, built on `tcp` + `Async`)
- [x] Host-service modules (`std/time` `Instant`/`Duration`, `std/proc` `args`/`env`/`exit`, `std/rand`)
- [x] Arbitrary-precision naturals (`std/BigNat`, canonical and packed over `Bits`)
  - [x] Machine-checked additive and multiplicative laws, additive cancellation, order reflection/transitivity, and power-of-two interaction lemmas
- [x] Certified strictly-positive arbitrary-precision naturals (`std/BigPos`)
- [x] Arbitrary-precision integers (`std/BigInt` over the strictly-positive `std/BigPos`)
- [ ] Dyadic `BigFlt` exact core
  - [ ] [Canonical representation, exact operations, comparison, and witnesses](roadmap/big-flt-dyadic/01-core-spec.md)
  - [ ] [Exact binary32 conversion and correctly rounded output](roadmap/big-flt-dyadic/02-binary32-spec.md)
- [ ] [`BigInt` certified algebra, order, and binary-scale laws](roadmap/big-flt-dyadic-proofs/01-big-int-laws-spec.md)
- [ ] Dyadic `BigFlt` proof and quotient-boundary completion
  - [ ] [Algebra and order theorem corpus](roadmap/big-flt-dyadic-proofs/02-laws-spec.md)
  - [ ] [Correctly rounded exact quotient conversion to binary32](roadmap/big-flt-dyadic-proofs/03-ratio-narrowing-spec.md)
  - [ ] [Binary32 round-trip and correct-rounding proofs](roadmap/big-flt-dyadic-proofs/04-boundary-proofs-spec.md)
- [ ] General rational `BigFlt` sequence _(explicitly after the program-analysis interface; no umbrella-only implementation step)_
  - [ ] [`BigNat` certified Euclidean division, GCD, divisibility, and coprimality](roadmap/big-flt-general/01-big-nat-euclidean-spec.md)
  - [ ] [General `BigFlt` reduced rational representation and exact operations](roadmap/big-flt-general/02-core-spec.md)
  - [ ] [General canonical uniqueness, ring, and order laws](roadmap/big-flt-general/03-laws-spec.md)
  - [ ] [General division and field laws](roadmap/big-flt-general/04-field-laws-spec.md)
  - [ ] [General rational binary32 boundaries](roadmap/big-flt-general/05-binary32-spec.md)
  - [ ] [Exact decimal parsing and presentation](roadmap/big-flt-general/06-decimal-spec.md)

## Tooling & Ecosystem

- [x] CLI (`run` and `compile` subcommands — `compile` bundles a native executable: launcher + appended `.cwasm`)
- [x] Staged IR debugging (`--print`, comma-selected pipeline stages dumped to stderr)
- [x] Built-in tracing-based profiling harness (`make curios/profile`; per-span aggregation through the workspace-wide `profile` feature)
- [x] CI pipeline (fmt/check/clippy/test)
- [x] Multi-platform release automation (Linux x86_64/aarch64, macOS aarch64 native binaries + wasm playground bundle, via tag-triggered GitHub Releases)
- [x] Browser playground
  - [x] Run harness owned by `curios-js` (`compile`/`run`, with wire codes from `curios-abi` and a local bridge for the compiler's wire-ABI `Bytes` payload shape)
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
  - [x] Package manager (a manifest of exactly pinned dependencies — an opaque `rev` to fetch, a `c1:` hash to accept — resolved to bytes by a resolver rather than to a path, topologically ordered, with a conflict refused naming both dependents and `curate` as the toolchain's only network actor over a two-layer content-addressed store; and the unit cache that makes depending on a package affordable, addressed by its mounts and its certifier and never keyed on a path)
  - [x] Payload reuse (an unchanged manifest target re-executes without recompiling: the precompiled `.cwasm` filed beside the units under the same address/record split, with one slot serving `run` and `compile` alike. The payload is the store's first machine-dependent artifact, so `curios-runtime` folds wasmtime's compatibility stamp into a caller-supplied hasher — keeping `sha2` out of the runtime and wasmtime out of the crate that builds the address. See [Reused payloads](soundness/admission-without-judgment/reused-payloads.md))
  - [x] Project manifest & discovery (identity is declared once: a package names itself with a single-word name and every consumer refers to it by that name, which is what makes a diamond share instead of duplicate; a manifest is exclusively a package or an umbrella; and discovery is decided — project scope is reachable only through declared artifacts, so a bare `.crs` file stays standalone everywhere, as does the `-` that compiles standard input)
  - [ ] Project reconciliation: what a project declares and does not use, and what sits in it that nothing names — reporting rather than refusing, since none of it is wrong and all of it is worth knowing. Two of the four are decidable from the manifests alone and were carried by `curate` until they earned their keep elsewhere; the other two — a dependency declared but never named by any module, and a name resolved against no declaration — wait on the mount table, which knows which prefixes a unit resolved against and hands that to nothing today
  - [x] `curios new` scaffolding (a package named after its directory, with both halves written — a manifest, a library, and one executable — and deliberately no flag to ask for one of them, since deleting a file already says so and the question is answerable only by somebody who knows the answer; the name checked before anything is written, and what it writes is what the rest of the toolchain already reads)
  - [x] One-line installer (`install.sh`, rendered from `install.sh.in` and shipped as a release asset with its version baked in, so the URL is the only version selector and a pinned script installs the binary it was cut for; verified against that release's `checksums.txt`, run once before it is installed rather than after, and reporting the version and path it landed at)
  - [ ] Linter
  - [ ] Test runner
  - [ ] Documentation generator
