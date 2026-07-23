# Design

This document owns Curios design decisions: what was decided, why, and what was rejected. An entry is amended only when its decision is superseded. What the language *is* belongs to [SYNTAX.md](SYNTAX.md), what exists or is pending belongs to [ROADMAP.md](ROADMAP.md), local architecture belongs to crate and module rustdoc, and contributor rules belong to [AGENTS.md](../AGENTS.md) — link there, do not restate here.

## Objectives

Curios is a small, fully dependent language: types depend on values, proofs live beside ordinary code, and one pure pipeline compiles everything to WebAssembly-GC, serving a native product and a browser product from the same backend. The long-term objective is a self-hosting compiler — every language-specific stage from source text to raw Wasm bytes written in Curios — running on the retained Rust host.

Non-goals: Curios is not a foundational proof assistant and makes no logical-soundness claim; it does not target multiple backends; it does not pursue self-hosting below the language-specific stages.

## The language

### One universe, general recursion

**Decision.** `Type : Type` is a kernel rule, `rec` provides unrestricted general recursion uniformly across the type and value layers, and there is no termination or positivity checking. A safe subset carrying those checks is planned for later — not soon.

**Rationale.** For the time being, Curios is a programming language first. The logical-soundness bullet is bitten once and explicitly rather than layering conservative checkers over a system whose universe is already inconsistent: `rec` is the honest marker of where general recursion enters, and type-level computation is bounded operationally by the reduction deadline, not logically by a termination proof. Until the safe subset exists, proofs are best-effort certificates, not foundational guarantees.

**Rejected.** Gating the whole language on termination and positivity checking. When soundness arrives, it arrives as an opt-in checked subset, not as a second language of accepted-versus-rejected recursion imposed on all code.

### Strict Prop under Type

**Decision.** There are exactly two universes: `Type`, and a strict `Prop` with definitional proof irrelevance. `Prop : Type` holds, `Prop ⊑ Type` is the sole subsumption, and large elimination out of `Prop` is guarded.

**Rationale.** Proof irrelevance is what makes proofs erasable by construction: any two proofs of a proposition are definitionally equal, so no program can depend on which proof it received, and erasure drops them wholesale. The large-elimination guard is what keeps that erasure sound.

### Erased positions are non-strict

**Decision.** An expression in an erased value position — an argument to an erased parameter, an erased structure or constructor field, an operand slot instantiated at a proposition or a type — is not evaluated: its effects, traps, and divergence do not occur. Statement positions evaluate under call-by-value regardless of their result's sort: a top-level item, a local `let` binding, and a direct call of a never-returning function (`/std/proc/exit : (Nat) -> False`) all run even though their results erase. Type-level positions are guarded rather than non-strict: forcing an effectful primitive — a host call, `IoExit`, or a `Cell` operation — during type-level reduction is a compile error (`EffectAtTypeLevel`); an effectful subterm a type never forces is erased with the type.

**Rationale.** Not evaluating erased value positions is what erasure is for: proofs are certificates whose computation must cost nothing at runtime, which is what lets the `/std` lemma corpus decorate data structures without runtime traversals. Host effects make the gap observable, because an effectful never-returning expression can inhabit a proposition, and no type-level rule can separate it from pure proof content. The boundary is therefore drawn by position rather than by type: what the program sequences as a statement runs; what erasure removes does not; what type checking would have to perform itself is refused outright.

**Rejected.** A transitive host-effect analysis that evaluates an erased position when its expression may reach a host operation: real machinery that still cannot preserve divergence, so it buys only a partial strictness guarantee. Also rejected: retyping never-returning operations at an empty `Type`-sorted carrier, which narrows the observable gap without closing it — an elimination immediately launders the effect back into an erased position.

### Concepts resolve with global coherence

**Decision.** Ad-hoc polymorphism is concepts and witnesses. Witness resolution consults one program-wide table under global coherence checks, and anonymous witnesses fill structure the goal already determines.

**Rationale.** Coherence makes the chosen witness a fact about the program rather than about the scope of the call site, so moving code or reorganizing imports cannot silently change which witness runs.

### Concept representations may be sealed

**Decision.** Concepts carry the same declaration-local representation visibility as structs and inductives: `: pub Type` is transparent, `: Type` is sealed. A sealed concept's representation is private to its declaring module — witness declarations, dictionary literals, structure updates, and raw field projections are permitted only there — while resolution, `use` parameters, and the generated method wrappers work identically for both. A sealed `pub` concept's fields are not interface, so they may reference private names: a private superclass is a hidden obligation resolution discharges without the consumer naming it.

**Rationale.** Sealing lets an owner control the full instance set of a concept — a guarantee otherwise inexpressible, since the auto-generated public wrappers plus explicit implicit arguments leak any field value, defeating private-token workarounds. It reuses the enforcement that representation privacy already provides, adding no new checks, and it is honest about mechanism: a concept is a record, so its visibility story is the record's. Enforcement is scoped to surface elaboration (an island in `Context`); machinery that re-derives types from already-elaborated terms — erasure, the metavariable oracle — suppresses it through the one bracket that clears the island, because compiler-built projections (witness splices, eta-expansions) were never subject to surface privacy.

**Rejected.** Coherence-only opacity (blocking dictionary literals while exempting `satisfy`): its motivating hazard dissolved once `Map` was recognized as byte-keyed by design, leaving no coherence-sensitive concept. Sealing through orphan-rule tightening: representation privacy already provides the gate, at the construction site where it belongs.

### Plicity is part of function identity

**Decision.** A binder's plicity — explicit, implicit (`@`), or witness (`use`) — is part of a function type's identity and calling convention. Function types that differ only in plicity are not convertible, every written function binder and constructor-pattern argument is checked against the plicity of the slot it claims, and an omitted implicit or witness *lambda* binder is inserted automatically from the expected function type, mirroring application-side hidden-argument insertion. Reduction stays positional and erasure stays sort-driven, so plicity has no runtime effect.

**Rationale.** Plicity controls elaboration-visible calling behaviour, so it must be stable under conversion: without it a convertible annotation or alias could reinterpret which binders get inserted. Lambda-side insertion lets a polymorphic definition omit the hidden binders it does not name while still writing exactly the ones it does, and keeping the rule positional-by-plicity (never by binder label) avoids named-argument selection. Constructor patterns are exempt from insertion because the type-blind match matrix lays out payload columns before elaboration knows the constructor signature; that omission is deferred rather than approximated.

**Rejected.** Plicity-blind function-type conversion; silently binding an implicit or witness slot with a plain binder; inserting omitted hidden constructor-pattern arguments by analogy with lambdas.

### Matching is total by enumeration

**Decision.** Every match must cover its scrutinee. Arms enumerate constructors without row priority, nested patterns compile through the pattern matrix by full enumeration, and an omitted arm is legal only when index inversion proves it impossible.

**Rationale.** Arm order never changes meaning, coverage is a checked property rather than a runtime default, and impossibility is discharged by the type system instead of an unreachable-arm convention.

**Rejected.** First-match-wins row priority with catch-all defaults.

### Literals are library values

**Decision.** Character and string literals lower to transparent `/syn/Char` and `/syn/Str` values — proof-certified library structures — while the erased runtime carriers remain `Nat` and packed `Bytes`.

**Rationale.** The kernel stays free of ad-hoc literal types, literals arrive already carrying the structure and certificates library code wants to consume, and erasure guarantees those certificates cost nothing at runtime.

**Rejected.** Kernel-primitive character and string types.

### No expression-level ascription

**Decision.** `term : type` is not an expression form and is not planned; `:` appears only in binder, signature, and motive positions.

**Rationale.** The whole-term forms — `let`, `rec`, `match`, lambdas, function types — already extend to the end of the enclosing term, so a postfix ascription would compete with them for the same tail. Where a type must be forced mid-expression, a `let` binding is the idiom.

## The toolchain

### WebAssembly-GC is the only target

**Decision.** The pipeline emits Wasm-GC exclusively. Program values live in GC references, never linear memory, and the same backend serves the native and browser products.

**Rationale.** A functional dependently typed language needs a garbage collector, and targeting Wasm-GC inherits a production collector instead of hand-rolling a runtime system. One backend yields both products, and portability comes with the ecosystem. The mechanism — the symbolic module builder and the GC-only, memory-less instruction roster — belongs to `curios-wasm`'s rustdoc.

**Rejected.** Native code generation, and Wasm over linear memory with a shipped garbage collector.

### Curios owns the language, Rust owns the host

**Decision.** Rust owns the native host — Binaryen optimization, Wasmtime precompilation and execution, bundling, the CLI, and operating-system services — and the self-hosting objective claims only the language-specific stages. The frozen Rust baseline compiler remains as bootstrap seed and differential oracle.

**Rationale.** Self-hosting pays off exactly where the language defines itself — parsing through Wasm generation. Reimplementing Wasmtime, Binaryen, or host integration in Curios would add risk without teaching the language anything.

**Rejected.** Making the bootstrap objective reach past the Curios toolchain. Going further stays open for later — with a more robust FFI story, host components could link directly through WebAssembly imports and exports compiled from other languages — but for now, the main toolchain in Curios itself is the objective.

### Distribution is ahead-of-time

**Decision.** The native product precompiles modules to `.cwasm` with Cranelift at build time and bundles them with the slim runtime launcher into standalone executables. The launcher deserializes and runs; it cannot compile. Other native distribution modes are not foreclosed; none is planned yet.

**Rationale.** User startup does no compilation work, the launcher stays slim precisely because Cranelift and Binaryen are excluded from it, and pinning the compiler and runtime to one Wasmtime version guarantees every `.cwasm` matches the engine that deserializes it.

### Deep shared terms are cached, not special-cased

**Decision.** The lowerer emits `Rc`-shared DAGs — a string literal's certified UTF-8 derivation shares every per-byte scan-state chain — and the compiler scales to them through generic mechanisms only, none of which recurses on input depth: an elaboration cache for metavariable-free, minted-name-free subterms whose result names only already-defined globals — so it survives, within one item, the `#`-minted definitions reduction and the frame elaborators mint (`Context::get_or_init_elaborated` / `elaboration_cacheable`, split into probe/record halves for the driver); the reduction cache's probe/record pair (`Context::cached_reduced` / `Context::reduce`); eliminator scrutinees reduced on an explicit frame stack inside `reduce`'s trampoline; elaboration's `elaborate → elaborate_apply → check` cycle defunctionalized onto its own frame stack for ground, all-explicit applications, so a data-shaped constructor spine elaborates at native depth bounded by the written program's binder nesting (`elaborate`'s driver and `ElabFrame`); and each term's memoized derivations (`hash`/`reach`/`free_vars`/`has_local_free`/`has_metavar`) carried on the shared `Rc` node (`Node`) and filled by an iterative post-order walk, so a subtree memoizes once across every occurrence, on a bounded native stack. No stage carries `Str`- or literal-specific machinery; eligibility gates and invalidation discipline belong to the `curios-core` rustdoc of those methods.

**Rationale.** Sharing-oblivious elaboration cost O(N²) work and O(N) native stack in a literal's byte length, and the depth surfaced wherever the checker ran on a bounded thread. The defect was the compiler's — recursive tree walks over DAG-shaped input — so the cure had to be compiler-general: the next proof-carrying literal or generated spine benefits identically, and `/syn`, `/std`, and the emitter stay ordinary library code.

**Rejected.** Emitter-side pre-reduced scan states (duplicates `/std/Str` semantics inside the compiler); a canonical-spine special case in conversion (a nominal `Str` exception to definitional equality); redesigning the `/syn/Str` evidence to sidestep depth (the language may write deep certified data; the compiler must carry it).

**Deferred.** The elaboration cache now survives a fresh `define` — the insert refuses any result naming a not-yet-defined global (the name analogue of the reduction cache's unsolved-metavariable rule, which lets reduction entries survive `define` selectively), so `define_entry` no longer clears it wholesale — but `set_island` still clears it at every top-level item boundary, so the survival is within-item. Extending it across items would key the cache on the item's actual island rather than merely whether privacy checks were live, so `set_island` need not clear; the reduction cache, island-independent, already spans items.
