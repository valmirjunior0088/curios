# Bootstrapping the Curios compiler — contracts and baseline

Working implementation specification for moving the language-specific compiler pipeline into Curios while retaining Rust as the native host, bootstrap seed, runtime, optimizer driver, and packaging environment. This document owns the durable bootstrap contracts and the Phase 0 baseline; each later implementation phase owns one of the numbered specifications that follow it in this directory. When the bootstrap lands, fold the durable architecture into `AGENTS.md`, update `ROADMAP.md`, and retain these documents only for implementation history that does not belong in those references.

This specification assumes the complete planned representation series has landed and been validated before Phase 0 begins. It also assumes the current pipeline and host split described by `AGENTS.md`: `text → core → ersd → cont → wasm` lives in the pure compiler crates, while Binaryen, Wasmtime compilation and execution, AOT precompilation, bundling, CLI integration, and operating-system services live downstream in Rust.

## Objective

Curios becomes self-hosting by making the Curios implementation authoritative for every language-specific stage from source text through raw WebAssembly generation. The resulting compiler must compile its own source, and a compiler built by that compiler must reproduce the same compiler artifact.

Rust deliberately remains part of the system. It owns the native driver, Wasmtime, Binaryen, AOT `.cwasm` production, execution, bundling, release packaging, and host integration. A frozen Rust implementation of the baseline compiler may remain as the source bootstrap seed and as a differential oracle, but it does not continue to receive new language features after the ownership cutover.

The architectural end state is:

```text
Curios source graph
  → self-hosted Curios compiler running as Wasm
  → raw Wasm bytes plus foreign manifest and diagnostics
  → Rust host
      → Binaryen optimization
      → Wasmtime precompilation or execution
      → bundling, CLI, and OS integration
```

Rust does not need to understand the current surface AST, type system, Core language, Ersd, continuation IR, or Wasm construction rules in order to compile an ordinary program after the cutover.

## Definition of self-hosting

Curios is self-hosting when all of the following are true:

- The authoritative lexer, parser, module resolver, surface AST, lowering, elaborator, conversion and reduction engine, zonker, eraser, Ersd pipeline, continuation pipeline, Wasm model, and Wasm encoder are written in Curios.
- The Curios compiler accepts the complete supported language rather than a bootstrap-only source subset when compiling user programs.
- The Curios compiler can compile the complete source of the Curios compiler itself.
- A compiler built by the self-hosted compiler reaches a deterministic fixed point as specified under [generation stability](08_STABILITY_AND_PRODUCTION_SPEC.md).
- The production Rust CLI invokes the self-hosted compiler rather than the Rust compiler pipeline for ordinary compilation.
- The production compiler artifact imports no temporary whole-compiler or Rust-backend bootstrap service.
- Binaryen, Wasmtime, AOT, execution, bundling, host IO, and packaging may remain implemented in Rust without weakening the self-hosting claim.

Self-hosting does not require deleting the frozen Rust stage-zero compiler, eliminating Rust from the repository, binding Wasmtime directly from Curios, or replacing the native host with C.

## Goals

- Stop duplicating future language, AST, elaboration, analysis, and code-generation machinery across actively maintained Rust and Curios implementations.
- Port one coherent, validated representation baseline instead of migrating Rust, partial Curios stages, bridge formats, and compiler source through representation changes concurrently.
- Reach the ownership cutover incrementally rather than requiring one untestable rewrite.
- Keep every temporary cross-language boundary byte-oriented, deterministic, and coarse-grained.
- Preserve the current Rust compiler as a correctness oracle throughout the port.
- Make the first meaningful cutover the complete `text → core → erase` frontend, so future source and type-system work belongs only to Curios.
- Retain the current reliable Rust runtime, Binaryen, Wasmtime, bundling, and release infrastructure.
- Make compiler determinism an implementation invariant rather than relying on semantic equivalence to excuse unstable output.
- Build enough compiler-oriented library substrate in Curios that later analysis and tooling work does not recreate ad hoc data structures.

## Non-goals

- A Rust-free toolchain.
- A C host or a binding to Wasmtime's C API.
- Exposing the Wasmtime engine, stores, modules, instances, GC references, callbacks, or lifetimes directly to Curios.
- Fine-grained FFI access to Rust AST nodes, contexts, terms, or IR objects.
- A stable public serialization format for every internal compiler IR.
- Exact textual equality between Rust and Curios diagnostics during the early port.
- Porting every optimizer before the first self-compilation proof.
- Implementing general incremental compilation, an editor service, a package manager, or a language server as part of the bootstrap.
- Making the temporary Ersd bridge a permanent public embedding API.

## Representation-series prerequisite

The representation series is a hard prerequisite to the bootstrap baseline rather than ordinary feature work that may land on either side of the ownership cutover. Representation projects have unusually broad consequences: they can change source spelling, the compiler's own Curios source, surface and Core AST shapes, elaboration invariants, erasure, optimizer assumptions, runtime value layouts, Wasm types, ABI rows, differential normalizers, and serialized artifacts.

For this gate, the representation series means every already-planned project whose primary purpose changes one or more of:

- Surface declarations or annotations that select visibility, storage, calling, erasure, or runtime representation.
- Core term, binder, recursive-definition, literal, registry, or identity representation.
- Erased values, closures, environments, variants, primitive payloads, or foreign-call representation.
- Continuation, block, calling-convention, allocation, tag, or control-flow representation.
- Emitted Wasm GC types, references, arrays, closures, imports, exports, tail calls, or value layouts.
- Host/guest wire types, foreign manifests, or other ABI representation consumed by both compiler and runtime.
- Prelude or compiler artifacts whose semantic contents would be invalidated by one of the preceding changes.

The set is enumerated and agreed before the bootstrap begins. The gate does not claim that Curios representations can never change again; it says that the known representation series is completed in Rust and becomes the coherent S0 baseline, while representation projects proposed after the ownership cutover are implemented in the authoritative Curios stages.

For the current numeric sequence, the representation gate includes the landed private dyadic `BigFlt` layout and its executable binary32 boundary. The native-width `Toml` codec is scheduled before them but does not alter compiler representations. The complete standalone `BigInt` law corpus begins immediately after bootstrap; the dyadic core lands only the focused normalization and uniqueness facts it actually requires.

The dyadic theorem corpus, exact quotient rounding, and boundary proofs in the `big_flt_dyadic_proofs` specification sequence are explicitly post-bootstrap proof and library work. The general odd-denominator extension in the `big_flt_general` specification sequence is explicitly post-Wonder private standard-library evolution. Those deferred projects are known but excluded from the S0 representation gate because they change no language syntax, compiler IR, host ABI, or compiler-owned runtime representation; prelude artifacts are rebuilt and versioned when the private library layout later changes.

The representation-series gate is complete only when:

- Every included specification has landed across all affected stages, libraries, tests, and durable documentation.
- Source-breaking migrations have updated the embedded libraries, compiler fixtures, examples, and any compiler source already written in Curios.
- Cross-stage tests demonstrate that Core, Ersd, Cont, raw Wasm, runtime behavior, and the host ABI agree with the intended final representation.
- The repository's full done bar and applicable browser build pass on the resulting baseline.
- No known representation project remains scheduled for implementation during the corresponding Curios stage port.
- The completed representation baseline and the compiler-source subset it supports are identified for Phase 0.

Before this gate closes, work may explore the byte-oriented FFI topology, deterministic collections, builders, deep-traversal techniques, shared-node caching, and small isolated Wasm probes. That work is bootstrap-neutral feasibility research and must be cheap to revise.

Before this gate closes, do not freeze the Curios surface AST, Core representation, Ersd or Cont models, live Ersd interchange envelope, differential normalization schemas, compiler artifact format, or permanent prelude artifact. Those values would otherwise encode a representation baseline that is already scheduled to change.

## Ownership model

There are three distinct forms of ownership during the migration.

**Authoritative** means the implementation is used by the production path and defines accepted programs and emitted behavior. An authoritative Curios stage must report its own errors; it must never silently fall back to Rust after encountering unsupported input.

**Shadow** means the Curios implementation runs against fixtures or alongside the production compiler, but its result is used only for comparison. Shadow implementations may be incomplete while their coverage is explicit.

**Oracle** means the frozen Rust implementation is invoked by tests to establish the pre-bootstrap behavior. Oracle use is never part of the production compilation path after a stage becomes authoritative.

Ownership moves only toward Curios. Once a stage has crossed the done bar and becomes authoritative, new behavior is implemented in Curios first and the Rust oracle is not extended to match it. This ratchet is what prevents the migration from becoming permanent dual maintenance.

## Why the temporary boundary is post-erasure

The first live mixed pipeline crosses from Curios to Rust after erasure:

```text
Curios: source → text → core → elaborate → zonk → erase
                                              │
                                              ▼
                                      versioned Ersd bytes
                                              │
                                              ▼
Rust:                         Ersd optimize → Cont → Wasm
```

Crossing earlier would turn the boundary into a serialization contract for the hardest compiler state: source origins, module interfaces, name identities, metavariables, shared terms, inductive and structure registries, concepts, witnesses, visibility islands, refinements, and elaboration diagnostics. Crossing after erasure keeps that state entirely inside the Curios frontend.

Ersd is still a real IR and therefore requires an exhaustive codec, but it is first-order, type-erased, and much less coupled to elaborator internals. It is the earliest practical seam that transfers ownership of the AST and type-system machinery that motivated the bootstrap.

No production Core interchange format is introduced. Core may have test-only normalizations or snapshots, but Rust and Curios never exchange live Core object graphs.

## Temporary FFI services

The existing Curios foreign-function mechanism and `curios-runtime::ForeignBindings` are sufficient for the bootstrap services. Both temporary services exchange `Bin` payloads and ordinary scalar status values; neither requires a general Wasmtime FFI layer.

The names below are conceptual. The implementation may choose repository-consistent qualified names, but the two responsibilities must remain separate.

### Whole-compiler fallback

The first compiler shell delegates to the Rust compiler through a service equivalent to:

```text
bootstrap_compile(request : Bin) -> {
    status: Nat,
    artifact: Bin,
    diagnostics: Bin,
}
```

The request identifies the entrypoint, compilation options, source environment, requested output stage, and protocol version. The response contains either the requested artifact or structured diagnostics. Host faults and malformed protocol data are returned as explicit failures rather than panics crossing the guest boundary.

This service proves the host topology, request and response framing, foreign binding, source access, and compiler driver before any language stage is replaced. It does not count as a partial bootstrap: at this point Rust still owns all compiler semantics.

### Rust backend bridge

After the frontend cutover, the compiler calls a service equivalent to:

```text
bootstrap_backend(ersd_envelope : Bin) -> {
    status: Nat,
    wasm: Bin,
    diagnostics: Bin,
}
```

The Ersd envelope contains the erased module, entrypoint, foreign manifest, protocol version, and any backend options that affect semantics. The Rust implementation validates the complete envelope before constructing Rust Ersd values and running the existing Ersd, Cont, and Wasm pipeline.

This is the one live IR seam permitted by the plan. It disappears when the Curios backend becomes authoritative.

### Boundary rules

- Every request and response begins with a magic value and explicit format version.
- Integers, packed bytes, names, indices, tags, collections, and optional values have one canonical encoding.
- Collection order is semantic or explicitly sorted; hash-table iteration never determines encoded output.
- The decoder rejects unknown required fields, invalid tags, duplicate identities, out-of-range references, malformed lengths, and trailing data unless the format explicitly permits extensions.
- The boundary carries values, never Rust pointers, Wasmtime references, object handles, callbacks, or borrowed memory.
- The protocol is release-internal and may change atomically with the host and compiler artifact, but every change increments its version and receives compatibility-failure tests.
- Diagnostics crossing the boundary use a transport-neutral schema rather than Rust error strings as the machine-readable representation.
- A temporary service is absent from the compiler's Wasm import table after its replacement milestone.

## Ersd interchange envelope

The Ersd bridge needs one purpose-built, versioned schema. It must be specified from Ersd semantics rather than derived automatically from Rust enum layout or serialized with an unversioned Rust implementation format.

At minimum, the envelope records:

- Format magic and version.
- Compiler semantic-version or compatibility identifier.
- Deterministic tables for names and other repeated atoms when interning materially reduces size.
- Every Ersd item and term constructor, with explicit numeric tags.
- Binder, item, variant, closure, and primitive identities in canonical integer form.
- Arbitrary-precision numeric and packed binary literals in canonical byte encodings.
- The entrypoint term and any entrypoint metadata required by lowering.
- Every used foreign function's namespace, import name, label, parameter names and wire types, and result names and wire types.
- Backend options whose values can alter generated Wasm.

Sharing that is not semantically observable may be reconstructed by the decoder. Cycles or identity-sensitive references must be encoded through explicit IDs and validated before use. The schema must not expose `Rc`, `Arc`, allocation addresses, Rust discriminants, hash implementation details, or source-only Core state.

The initial implementation should use a compact custom binary format or another format that Curios can encode and Rust can validate without importing Rust's internal serialization model into the guest. JSON remains appropriate for diagnostics and analysis responses, but it is not the preferred representation for the large, frequently exchanged Ersd module.

The bridge is temporary, but its round-trip and corruption tests must be production quality because it becomes the live compiler boundary during the longest hybrid phase.

## Permanent compiler/host boundary

The final compiler produces a response equivalent to:

```text
CompileResponse {
    status,
    raw_wasm,
    diagnostics,
    foreign_manifest,
    compiler_metadata,
}
```

The first implementation may run the compiler as an ordinary Curios program using arguments and `/sys/Io` for source and output files. This is sufficient for the bootstrap generations and requires no new runtime calling convention.

A later in-memory service transport may carry the same logical request and response through byte buffers for the native CLI, browser, or embedders. Transport changes must not move module resolution, parsing, or language semantics back into Rust. A host may provide a filesystem or virtual filesystem, but the Curios compiler decides which modules to request and how their logical paths resolve.

After raw Wasm is returned, the Rust driver may optimize it with Binaryen, precompile it with Wasmtime, execute it, or append it to the slim launcher. These are downstream host operations and are not guest FFI calls into the Wasmtime object model.

## Frozen stage-zero compiler

The existing Rust compiler becomes stage zero, abbreviated **S0**. S0 is retained to build the first self-hosted compiler from source and to serve as a differential oracle.

After the frontend ownership cutover, S0 is frozen to the bootstrap language baseline. It receives only correctness fixes necessary to compile the bootstrap compiler source, security fixes, and maintenance required by unavoidable dependency or platform changes. It does not receive new syntax, type-system features, analysis indexes, or optimization work merely to stay feature-equivalent with the self-hosted compiler.

The Curios compiler source must remain within the language subset accepted by S0 for as long as source bootstrapping relies on S0. New language features may be implemented and exposed to user programs without immediately rewriting the compiler source to use them.

If preserving that source discipline becomes materially restrictive, a later project may replace normal S0 builds with a reviewed compiler Wasm seed from a prior release. That seed policy, provenance, reproducibility, and update ceremony are not chosen by this specification; the initial bootstrap keeps S0 because Rust remains an accepted part of the toolchain.

Production binaries must not link the full S0 compiler pipeline merely to run already-built compiler Wasm. As with the slim launcher, compiler-artifact construction and production hosting should be separated so the native driver carries only the host functionality it uses at runtime.

## Compiler substrate required in Curios

The language can express the compiler, but the standard library does not yet provide all of the representations and mutation patterns needed to port the current Rust implementation directly. The bootstrap begins by building compiler-specific substrate rather than reproducing incidental Rust APIs one at a time.

### Identity and shared terms

Core terms require cheap sharing, structural equality, deterministic hashing, free-variable and reachability queries, and memoized derived facts. Curios has no required generic pointer-identity primitive, so compiler values must not depend on allocation address identity.

The initial design should use explicit minted identities for shared compiler nodes together with cached structural hashes and structural equality fallback. Identity accelerates caches within one compiler invocation; structural content defines semantic equality and deterministic artifacts.

The design must support recursive definitions and graph-shaped terms without making ordinary traversals recursively consume the native or Wasm stack.

### Mutable compiler state

The elaborator and optimizers require maps, sets, queues, worklists, union-like stores, counters, and append-heavy builders. Curios's persistent `Map` remains useful for snapshots and immutable environments, while `Cell`-owned persistent structures or purpose-built mutable compiler containers provide local mutation.

The substrate must include:

- Deterministic maps and sets over compiler keys.
- Collision-safe hash buckets with structural key comparison.
- FIFO queues, stacks, blocker watch lists, and retry worklists.
- Append-efficient byte, text, item, and instruction builders.
- Name interning and explicit fresh-name or fresh-ID supplies.
- Snapshot or rollback support where solver transactions require it.
- Iterative traversal utilities for deep terms, patterns, ropes, and IR graphs.

The implementation is free to use specialized structures where one general abstraction would impose unacceptable allocation or proof overhead. Their observable iteration order must nevertheless be deterministic.

### Parsing

The self-hosted lexer and parser should operate over a purpose-built byte cursor with explicit offsets and commitment state. The general `/std/Parse` combinator library is useful for applications but is not required to reproduce the current typed memoization, error commitment, and source-span behavior of the compiler parser.

The parser substrate must support exact byte spans, bounded lookahead, committed errors, iterative handling of deep or long sequences, and diagnostic expectations without retaining unnecessary source copies.

### Diagnostics and source ownership

Spans, logical source identities, source storage, compiler phases, diagnostic categories, and rendered messages need Curios-native representations before the frontend port. Machine-readable diagnostics must remain separate from presentation strings so the CLI, tests, and future analysis tools consume one semantic result.

The bootstrap does not implement the complete `wonder` index, but it must avoid representations that would prevent universal source origins, reference capture, or structured goal diagnostics from being added after the ownership cutover.

### Determinism

Every identity supply begins from explicit input state. Hash seeds are fixed or encoded as part of nonsemantic runtime state and never influence output order. Worklist scheduling, module order, witness order, optimizer traversal, serialization tables, and diagnostic ordering are deterministic for one request.

Determinism is tested throughout the port rather than postponed until self-compilation.

## Implementation phases

The port proceeds through nine strictly ordered phases. Phase 0 is specified here because its deliverable is this document's contracts and the recorded baseline; each later phase or phase pair owns one numbered specification in this directory:

1. [Phase 1 — hybrid shell and feasibility probes](02_FEASIBILITY_SPEC.md)
2. [Phase 2 — Curios base and Wasm leaf](03_BASE_AND_WASM_LEAF_SPEC.md)
3. [Phase 3 — surface frontend in shadow mode](04_SURFACE_FRONTEND_SPEC.md)
4. [Phase 4 — Core, elaboration, and erasure in shadow mode](05_CORE_ELABORATION_SPEC.md)
5. [Phase 5 — frontend ownership and the Ersd cutover](06_FRONTEND_CUTOVER_SPEC.md)
6. [Phase 6 — Ersd and continuation backend](07_BACKEND_SPEC.md)
7. [Phases 7 and 8 — generation stability and production integration](08_STABILITY_AND_PRODUCTION_SPEC.md)

### Phase 0 — baseline and contracts

Phase 0 begins only after the [representation-series prerequisite](#representation-series-prerequisite) is complete. If an included representation specification remains unimplemented or its cross-stage migration remains unvalidated, the bootstrap has not started.

Parked lambda inference and anonymous match functions are not bootstrap prerequisites. By default they wait until the corresponding Curios stages are authoritative. If early availability is explicitly chosen, parked inference and then anonymous match functions land during Phase 0 before the baseline is frozen and before their Curios ports begin; they must not be introduced halfway through a stage port.

After that optional language batch is decided and, if selected, completed, record the exact S0 revision as the bootstrap language and representation baseline, together with the permanent Rust/Curios ownership boundary, the temporary FFI services, the initial source discipline for S0, and the self-hosting done bar.

Choose and pin the request and response envelopes, error model, protocol versioning, artifact identities, and the exact command used to build and run a compiler generation only against the completed representation baseline. Add no fine-grained AST FFI while resolving these contracts.

## Prelude strategy

The frozen Rust stage-zero compiler uses a build-scoped prelude image generated automatically in `curios-prelude`'s `OUT_DIR`. It archives prepared Text resolver state, elaborated and zonked Core state, and the erased item prefix. Production S0 compilation always restores that image and has no runtime source-compilation fallback or cache-miss path.

This rkyv image is an internal implementation detail of one compiler build, not a stable interchange format and not the permanent self-hosted cache design. Its explicit schema and source fingerprint detect stale or incompatible build products; Cargo regenerates it together with the compiler. It is never committed, distributed independently, or consumed by a different compiler build.

The bootstrap uses this sequence:

1. Preserve the build-scoped archived S0 behavior for the frozen Rust seed.
2. Compile the prelude from source in the early Curios frontend to establish correctness.
3. Measure parsing, lowering, elaboration, erasure, artifact size, and restoration independently.
4. Add a Curios-owned, versioned prelude artifact when measurements show it is required for usable compiler iteration.
5. Let Rust store, load, and integrity-check that artifact as opaque bytes; Curios defines and decodes its semantic contents.

The Curios artifact may contain separate elaborated and erased sections, but its identities, tables, schema, validation, and replay semantics belong to the self-hosted compiler. Its cache key includes the compiler artifact identity, embedded prelude source hashes, format version, target-independent semantic options, and any other input that can affect the restored state.

The S0 cleanup expresses prelude replay as ordinary context preparation and cached-prefix replay. Its Rust-layout serialization is explicitly confined to stage zero and is not promoted into the permanent architecture.

## Relationship to planned language and tooling work

The complete representation series is the hard prerequisite described above. The gate includes planned representation work across the surface language, Core, Ersd, Cont, Wasm, and the host ABI even when a project introduces no new surface syntax.

Parked lambda inference and anonymous match functions are optional before the baseline freeze. The bootstrap-first default defers them until Curios owns Core and text respectively. If their near-term user value outweighs the duplication, they land in Rust in their specified order before Phase 3 begins and become part of the frozen oracle baseline.

Labeled written goals wait until the Curios elaborator is authoritative. Their metavariable origins, birth-time registry, tolerant reification, incomplete outcome, and structured diagnostics then have one production implementation.

`wonder` keeps its public, transport-neutral schema and conceptual source-versus-semantic model, but its resolved source graph, universal origins, reference capture, semantic indexes, witness snapshots, and query engine are implemented over the Curios frontend after Phase 5. The bootstrap substrate deliberately preserves the information needed by that work without building the full index early.

Formatter, language-server, linter, and documentation tooling should likewise consume the Curios-owned parser and source index after the frontend cutover. Host-only work such as Wasmtime integration, Binaryen upgrades, bundling, release automation, and OS services may proceed independently because it stays in Rust.

Any non-representation source-breaking or Core-semantic project proposed during Phases 3 and 4 must choose one of two explicit schedules: land completely before the affected Curios port begins and join the frozen baseline, or wait until the affected Curios stage becomes authoritative. It must not be developed concurrently in both active implementations without a separate decision accepting that cost.

Any newly proposed representation project after Phase 0 begins waits until every affected Curios stage is authoritative unless a separate decision explicitly reopens the prerequisite, pauses the port, and accepts migration of every already-built Rust, Curios, bridge, test-normalization, and artifact surface. Reopening the prerequisite is an exceptional recovery path, not ordinary sequencing.

## Testing strategy

### Component tests

Curios compiler modules receive focused tests for representations, substitution, maps, parser commitment, reduction, conversion, inference, coverage, erasure, IR lowering, optimization, and encoding. Compiler test programs run through the existing Rust host until the self-hosted test runner exists; using Rust to execute tests does not make Rust authoritative for compiler semantics.

### Differential tests

During shadow phases, the same fixture runs through S0 and the Curios component. Comparisons use a stage-appropriate semantic normalization:

- Lexer and parser: token kinds, AST constructors, names, visibility, literals, and exact source spans.
- Resolution and lowering: module graph, introduced symbols, dependencies, normalized binders, and Core constructors.
- Elaboration and zonking: acceptance, inferred types, solved term structure, registries, and residual-state absence.
- Erasure and backend: normalized IR, foreign signatures, raw Wasm validation, and runtime behavior.
- Diagnostics: phase, category, principal span, relevant symbol or term, and structured payload; exact prose is required only for deliberately stable messages.

Generated names and identities are compared after deterministic rebasing when their concrete numbers are not semantic. Normalization must never erase information whose difference could affect downstream behavior.

### Metamorphic and property tests

Use parse-print-parse round trips, encode-decode-encode stability, alpha-renaming, repeated deterministic builds, cache-hit versus cache-miss equivalence, optimized versus unoptimized behavior, and S2/S3 generation equality. Malformed protocol and Wasm fixtures receive bounded-decoder and clean-error tests.

### Cross-stage corpus

The existing integration suite remains the primary semantic oracle. Add compiler-sized programs, the complete embedded standard library, modules with user foreign declarations, private and public representations, indexed inductives, concepts and witnesses, deep recursive syntax, large packed literals, and optimizer-sensitive workloads.

### Performance observations

Record compile wall time, peak resident memory, guest allocations where measurable, raw and optimized compiler size, prelude phase costs, protocol payload size, and generation times at every phase boundary. These measurements identify regressions and decide optimizer and cache priority; they are not permitted to weaken correctness gates.

## Stage done bars

A Curios stage may move from shadow to authoritative only when:

- Its data representation and invariants are documented in the Curios module.
- Its focused positive, negative, deep-input, and determinism tests pass.
- The applicable S0 differential corpus passes or every intentional semantic difference is recorded as a post-cutover Curios change.
- It introduces no fine-grained Rust-object FFI.
- Its errors do not trigger production fallback to Rust.
- Its downstream artifacts validate and execute through the existing pipeline.
- Its performance has no known asymptotic blocker on the standard library or compiler-sized fixtures.

The frontend cutover additionally requires complete source-to-Ersd coverage, Ersd envelope validation, foreign-manifest parity, prelude correctness, and the absence of a `bootstrap_compile` import.

The backend cutover additionally requires valid raw Wasm for the full corpus, successful execution under the existing runtime, sufficient viability optimization to compile the compiler itself, and the absence of a `bootstrap_backend` import.

## Bootstrap completion criteria

The bootstrap is complete when:

- The completed representation series is present in the recorded S0 baseline and in the authoritative Curios compiler.
- The production compiler path runs the Curios compiler artifact.
- Curios owns every language-specific stage through raw Wasm encoding.
- The compiler accepts the full current language and compiles its own complete source.
- S2 and S3 raw compiler Wasm are byte-identical.
- S2 and S3 agree on the complete semantic and diagnostic corpus.
- The compiler artifact imports no temporary bootstrap compiler or backend service.
- Rust still supplies only the documented permanent host responsibilities on the ordinary path.
- S0 is frozen and no longer receives ordinary language features.
- The prelude path is correct from source, with any cache defined by the Curios compiler rather than Rust object layout.
- CI builds the native host, builds the self-hosted compiler, checks the generation fixed point, runs the language suite through the self-hosted compiler, and preserves the existing slim-launcher and browser obligations where applicable.
- `AGENTS.md`, `ROADMAP.md`, build instructions, release automation, and public CLI documentation describe the self-hosted architecture.
- Before the last of these bootstrap specifications is deleted, permanent ownership boundaries and architectural invariants are recorded in `AGENTS.md` and the owning crate or module documentation, operational procedures are recorded in build and release documentation, remaining plans refer to the authoritative self-hosted pipeline rather than these files, the roadmap entry is a checked unlinked summary, and no reference to their filenames remains.

## Effort estimate

For one engineer already familiar with the Rust compiler, including differential tests, diagnostics, performance work, and hardening:

| Work | Estimated effort |
| --- | ---: |
| Compiler substrate, hybrid shell, protocols, and feasibility work | 2–4 engineer-months |
| Surface frontend, loader, resolution, and lowering | 3–5 engineer-months |
| Core representation, elaboration, conversion, reduction, zonking, and erasure | 7–11 engineer-months |
| Ersd, continuation IR, Wasm model, encoder, and correctness lowering | 5–8 engineer-months |
| Viability optimizers, self-compilation, generation stability, and production hardening | 4–7 engineer-months |
| Remaining optimizer and diagnostic parity after self-hosting | 6–10 additional engineer-months |

The expected range to full-language repeated self-compilation is therefore approximately 21–35 engineer-months. Production parity with the important behavior and optimizer quality of the Rust compiler is approximately 27–45 engineer-months. These estimates begin after the representation-series prerequisite; they do not include implementing or validating that series. A restricted proof that compiles a bootstrap subset to raw Wasm can arrive earlier, but it does not satisfy this specification's self-hosting definition.

The hybrid FFI plan does not substantially reduce the amount of compiler logic to port. Its value is risk reduction, observable ownership milestones, and the ability to keep a working production backend while the frontend becomes authoritative. A proliferation of additional IR seams would add months of codec and compatibility work without reducing the semantic port.

## Principal risks

### Compile-time performance and allocation

The current Rust compiler relies heavily on mutable vectors, hash maps and sets, shared terms, cached structural facts, and work queues. Curios's immutable data defaults and Wasm GC representation can make a mechanically translated compiler allocate excessively even when asymptotic complexity appears acceptable. The Phase 1 probes and the staged optimizer/cache work address this risk before the full port depends on the representation.

### Deep traversals

Compiler inputs can create deeply nested syntax, terms, substitutions, and IR graphs. Every unbounded traversal must be audited for iterative execution under the ordinary Wasm and test-thread stack. Raising a stack limit is not a completion strategy.

### Nondeterminism

Mint order, hash iteration, worklist scheduling, module traversal, optimizer order, and serialization tables can prevent a bootstrap fixed point without changing program behavior. Determinism is therefore a representation and algorithm requirement from Phase 1 onward.

### Semantic drift during a long port

Actively extending both implementations can make the target move faster than it is ported. The baseline freeze, ownership ratchet, and explicit pre-or-post schedule for every affected feature contain that risk.

### Incomplete representation baseline

Starting a representation-dependent port before the known series lands can force coordinated migrations across S0, partial Curios stages, Ersd envelopes, differential normalizers, compiler source, and cached artifacts. The representation-series gate prevents that work from being mistaken for harmless feature drift.

### Temporary bridge permanence

The Ersd bridge could become comfortable enough to survive indefinitely. Its import-absence done bar, the prohibition on further live IR seams, and the self-hosting definition make its removal a required milestone rather than optional cleanup.

### Bootstrap seed evolution

A frozen S0 can compile future compiler sources only while those sources remain within its accepted subset. The initial source-discipline policy is simple and Rust-compatible, but a future source-breaking language change may justify a reviewed prior-release Wasm seed. That transition requires a separate supply-chain and reproducibility design.

### Prelude cost

Re-elaborating the full prelude inside the early self-hosted compiler may dominate iteration time. A premature Rust-layout cache creates duplicate machinery, while waiting too long can make development impractical. Phase measurements decide the timing, and the permanent artifact remains Curios-owned.

## Verification

Documentation-only changes to this specification require Markdown and link review rather than the compiler done bar. Each implementation milestone runs the repository checks required by `AGENTS.md` for the crates and Curios sources it changes, including the browser build whenever the self-hosted compiler or its host path affects `curios-web`.

The final bootstrap gate includes, in order:

```sh
make curios/runtime
cargo fmt --all -- --check
cargo check --workspace --all-targets --all-features
cargo clippy --workspace --all-targets --all-features
cargo test --workspace --all-targets --all-features
```

It additionally builds S1, S2, and S3 in a clean environment, compares S2 and S3 raw Wasm byte-for-byte, runs the full language suite through S2 and S3, verifies the production compiler's import set, and runs the `curios-web` wasm32 and matching `wasm-bindgen` build when that path consumes the self-hosted artifact.

## Open decisions to resolve at their phase gates

- The exact binary encoding used by the temporary request, response, and Ersd envelopes.
- Whether the first compiler shell uses only command-style file IO or also introduces the in-memory service transport immediately.
- The concrete stage-zero artifact build layout that keeps S0 out of production runtime binaries.
- Which Ersd and continuation optimizations constitute the minimum viability tier for self-compilation.
- The measured point at which the Curios-owned prelude artifact becomes mandatory.
- Whether a prior-release Wasm seed eventually replaces S0 in ordinary clean builds.

None of these decisions changes the permanent ownership boundary, permits fine-grained Rust AST FFI, or weakens the generation fixed-point requirement.
