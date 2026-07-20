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

### Concepts resolve with global coherence

**Decision.** Ad-hoc polymorphism is concepts and witnesses. Witness resolution consults one program-wide table under global coherence checks, and anonymous witnesses fill structure the goal already determines.

**Rationale.** Coherence makes the chosen witness a fact about the program rather than about the scope of the call site, so moving code or reorganizing imports cannot silently change which witness runs.

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
