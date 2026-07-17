# Ersd v2

Working implementation specification for replacing the current recursive Ersd term representation and optimizer with a verified, arena-backed, direct-style ANF representation whose semantic operations, observable behavior, static facts, and optimization laws are expressed through one coherent interface.

This project is part of the representation series that must settle before the bootstrap baseline described in `05_BOOTSTRAP_SPEC.md`. It starts only after Continuation IR v2 has landed and its actual input contract has been frozen. While this specification is active, it owns the intended Ersd v2 architecture, migration order, compatibility obligations, and acceptance criteria. When the project lands completely, move durable local contracts into `curios-ersd`, `curios-core`, and `curios-prelude` crate and module documentation, update `AGENTS.md` and `ROADMAP.md`, verify that no document still depends on this filename, and delete this working specification.

## Status and scope

Ersd v2 is a replacement of the `curios-ersd` representation, Core-to-Ersd erasure boundary, archived Ersd prelude prefix, Ersd optimizer, and Ersd-owned lowering into the landed Cont v2 representation. Reorganizing those components from scratch is permitted. The project must preserve language behavior, the public pipeline stages, the host ABI, the runtime ABI, the supported recursive initialization contract, and the downstream Cont contract unless this specification explicitly says otherwise.

The project deliberately follows Cont v2 rather than changing both representations concurrently. Phase 0 records the Cont types and behaviors that actually landed, reconciles any stale statements in this document or `00_CONT_V2_SPEC.md`, and establishes a frozen lowering target before production Ersd migration begins.

The intended pipeline becomes:

```text
elaborated and zonked Core
  → Core-owned erasure through ErsdBuilder
  → verified scoped direct-style ANF          public Ersd stage
  → semantic analysis and optimization
  → verified optimized scoped ANF             public ErsdOptm stage
  → Ersd-owned iterative lowering
  → landed high CPS Cont v2
  → Cont optimization and Wasm lowering
```

The pipeline's existing `Stage::Ersd` and `Stage::ErsdOptm` observation points remain. Their printed representation changes, but their position and meaning do not.

## Objective

Ersd v2 must give erasure, analysis, optimization, prelude replay, and Cont lowering a single principled contract. A behavior that affects correctness or optimizer legality must be represented once in the Ersd semantic interface and consumed exhaustively, rather than being inferred independently from term variants in multiple passes.

The concrete objectives are:

- Replace pointer-shaped recursive terms with stable IDs, explicit lexical ownership, and arena-backed storage suitable for deterministic mutation and a later Curios implementation.
- Make sequencing and scope explicit without turning Ersd into CPS or SSA.
- Preserve semantic distinctions such as Bool versus Nat, Byte versus Nat, sequence roles, product fields, and variant constructors until Ersd optimization is complete.
- Centralize operation contracts, constant folding, algebraic laws, trap behavior, effects, allocation behavior, and callback invocation.
- Replace `candidate` with derived known-value facts and an explicit deterministic benefit and budget policy.
- Derive function free values, call relationships, effects, and value shapes rather than storing facts that become stale under rewriting.
- Preserve the current eager-global and recursive-initialization behavior while making their constraints verifier-checkable.
- Restore the archived fixed prelude as a complete, finalized Ersd prefix that can be safely extended with a user suffix.
- Lower directly and exhaustively into the landed Cont v2 contract while keeping all i31 constraints exclusively in the Cont-to-Wasm boundary.
- Retain deep-input safety and make traversal, analysis, printing, archive construction, and optimization deterministic.

## Non-goals

- Changing Core type theory, elaboration, conversion, normalization, surface syntax, or source-language semantics.
- Changing the landed Cont v2 representation merely to simplify Ersd v2.
- Changing the Wasm value layout, closure ABI, host ABI, runtime ABI, or browser host contract.
- Moving i31 representability, boxing, tagging, or overflow constraints into Ersd or Core.
- Introducing a full runtime erased-type system or mandatory serialized type annotation on every binder.
- Turning Ersd into CPS, SSA, a generic compiler dialect framework, or a general-purpose graph IR.
- Changing the source semantics of recursive values or accepting recursive computed-value cycles that are currently rejected.
- Introducing a new allocation strategy, linear-memory object model, reference counting scheme, or ownership calculus.
- Treating the build-scoped rkyv prelude image as a stable interchange format.
- Porting every current Ersd optimization mechanically before its legality has been restated through the new semantic contracts.
- Broad cleanup of Core, Cont, the prelude, or runtime code unrelated to the representation migration.

## Numeric boundary and representation invariant

Core may use unbounded values while reducing type-level and compile-time terms, but erased runtime Nat and Int values have the same width assumed by Ersd and Cont: Nat is `u32` and Int is `i32`. Ersd constants, primitive contracts, constant folding, specialization keys, tests, and lowering all use the full 32-bit domains.

i31 is not an Ersd type, literal domain, optimizer assumption, algebraic precondition, trap condition, or serialization rule. Ersd lowering preserves u32 and i32 facts into Cont. Only Cont's final Wasm boundary may decide whether a value is immediately representable, requires boxing, or participates in another physical encoding constraint.

The following layering is mandatory:

- Core owns unbounded type-level reduction and the decision to erase a runtime Nat or Int into its 32-bit carrier.
- `curios-base` may own shared, stage-independent mechanics for the runtime `u32`, `i32`, bit-preserving binary32, and packed binary carriers.
- Ersd owns semantic operation identities, 32-bit constant evaluation, observable behavior, and optimizer laws.
- Cont owns CPS and physical-operation representation while preserving Ersd's runtime scalar meaning.
- Cont-to-Wasm owns i31 representability, immediate versus boxed layout, and exact Wasm-GC encoding.

Phase 0 must explicitly freeze the existing 32-bit arithmetic contract, including overflow, division, remainder, conversion, and shift behavior, against runtime execution. Whether a particular operation wraps, traps, saturates, or is otherwise constrained is a 32-bit semantic decision; i31 cannot be used as its explanation. Until that audit is complete, an algebraic law whose validity depends on overflow behavior remains unavailable to optimization.

Floating-point constants and specialization keys use the bit-preserving `curios_base::Flt` carrier rather than raw `f32` equality or hashing. NaN payloads and signed zero must not be silently canonicalized by Ersd.

## Existing behavior that constrains the design

### Ersd is direct style

Ersd sits after type erasure and before CPS conversion. Its representation must expose lexical sequencing, local functions, recursive groups, matches, and induction in direct style. Continuations, return continuations, join-point conversion, closure conversion, and machine CFG concerns remain owned by Cont.

### Global initialization is eager

Top-level computed items are not equivalent to dormant function bodies. Removing an unreferenced global initializer is legal only when evaluating it is unobservable: it cannot trap, diverge, exit, invoke the host, access mutable state, or create an identity-sensitive resource whose disappearance is observable under the retained semantics.

By contrast, creating an unused function does not evaluate its body. A function whose body performs host or cell effects may be removed when the function value itself is unreachable and its creation has no observable allocation behavior.

### Recursive groups contain functions and computed values

The current recursive form is simultaneous and can contain functions as well as computed items. Function-only recursion is ordinary lexical recursion. Mixed groups may contain a bidirectional initialization dependency in which a computed member needs a function value and the function captures that member.

Ersd v2 preserves the source behavior accepted when Cont v2 lands. Computed-only initialization cycles continue to produce `CyclicRecComputed` unless a separate language decision changes that rule. Any restrictions represented by `UnsupportedSyncRecItem` are frozen from the landed pipeline during Phase 0 rather than silently weakened or strengthened.

### Sequence elimination has semantic structure

Core currently exposes list, bit-string, and byte-string eliminations whose erased implementation can be described using length, indexing, slicing, Nat induction, and products. Ersd v2 must retain the sequence-level fold meaning instead of forcing all such eliminations immediately into low-level length/get/slice terms.

The retained operation is a right fold in the order defined by current erasure. It exposes the current element, the suffix view, and the accumulated result, evaluates the empty branch with the same frequency as current behavior, and preserves all bounds and trap behavior. A suffix view is materialized only if used, but eliminating it must not eliminate a required bounds guard.

### Semantic identities survive erasure

Several operations share a physical implementation but do not share optimizer laws or source meaning. Ersd must preserve at least Bool versus Nat operations, Byte versus Nat carriers, Io equality versus packed-binary equality, distinct packed-binary grains, list versus binary sequence roles, product field identity, variant family identity, and constructor identity.

Many-to-one lowering into physical Cont operations is allowed only in `into_cont`, after all Ersd transformations that rely on the semantic distinction have completed.

### List map is call-like

`LstMap` invokes its mapper zero or more times in sequence. Its behavior includes the mapper's transitive traps, divergence, host effects, state access, and allocation behavior. It is a dedicated intrinsic, not a total primitive that may be freely reordered or deleted.

### Product and variant layouts are declaration contracts

Product construction and projection are governed by product and field identities, not by an undocumented positional tuple convention. Variant construction and matching are governed by family and constructor identities, not by manually reading a tag from field zero and projecting a payload by convention.

Erasure fixes each runtime schema using the declaration and signature's existing opaque/open discipline. Construction, projection, archive replay, match checking, and Cont lowering must agree on the same schema. Retained runtime positions whose instantiated proof or type value erases to Unit remain present when required by the established representation contract.

## Representation model

### Semantic tree, arena storage

Ersd v2 is a structured, direct-style, scoped ANF tree stored in module-owned arenas. Blocks and functions have lexical owners; references use stable IDs. This separates semantic nesting from physical storage without introducing arbitrary control-flow edges.

The conceptual core is:

```text
Atom =
    Value(ValueId)
  | Global(GlobalId)
  | Function(FunctionId)
  | Constant(ConstantId)

Block {
    statements: [StatementId],
    terminator: Terminator,
}

Statement =
    Let { result: ValueId, rhs: Rhs }
  | LetFunctions { functions: [FunctionId] }
  | LetRec { group: RecGroupId }

Terminator =
    Return(Atom)
  | Exit(Atom)
  | Unreachable
```

This shape is normative; exact Rust enum names and field packing may vary if the resulting invariants and verifier coverage are equivalent.

Every non-atomic computation is bound by `Let`, introduced as a function or recursive group, or appears as a block terminator. Operand evaluation order is therefore the statement order within a block. Nested branch and fold bodies are referenced by `BlockId` and have exactly one structural lexical owner.

Structural block references must be acyclic. Recursion occurs only through explicit function and recursive-group identities, never by making a block graph cyclic.

### Module structure

An Ersd module owns:

- top-level global declarations and initialization groups;
- the entry `BlockId` or equivalent exported entry contract;
- arenas for globals, blocks, statements, values, functions, recursive groups, constants, product schemas, variant families, constructors, and foreign rows;
- deterministic symbol and exported-name maps;
- optional nonsemantic name and origin side tables;
- no mandatory stored use lists, capture lists, effect summaries, or optimizer hints.

Top-level order remains explicit because initialization order is observable. Function declarations may be grouped according to their recursive scope, while computed globals retain the evaluation order established by erasure and prelude replay.

### Identity and arenas

Each semantic entity has a distinct typed ID backed by a `u32` index or an equivalent compact 32-bit identifier. IDs are minted monotonically within one module construction epoch and are never derived from addresses, names, printed text, or hash iteration order.

Rewriting does not reuse deleted IDs. Removed entries become tombstones until an explicit deterministic compaction step remaps the complete module and all side tables. Compaction is allowed only at a documented pass boundary, never as an incidental consequence of insertion or deletion.

The representation must remain bootstrap-friendly:

- use plain vectors and deterministic ordered maps by default;
- avoid `Rc<RefCell<_>>`, raw-pointer graphs, intrusive lists, and output-dependent hash iteration;
- keep traversal and worklist order deterministic;
- implement potentially deep walks iteratively;
- make all ID remapping explicit and verifier-checkable.

### Binder namespaces and scope

Globals, local values, functions, blocks, schemas, constructors, and foreign rows occupy distinct namespaces. The verifier rejects using one ID kind as another even if their numeric indices coincide.

A local `ValueId` has one definition and is usable only where that definition lexically dominates its use. Function parameters and fold or branch binders are ordinary value definitions whose scopes are determined by their owned blocks. Debug names do not affect identity or scope.

All function IDs in a recursive function group are in scope in every member body and in the enclosing continuation block. A nonrecursive local function form may be represented as a one-way group when profitable, but scope and recursion remain explicit rather than inferred from declaration order.

### Atoms and aliases

Atoms are computation-free. Reading an atom cannot trap, allocate, invoke a callback, access state, or call the host. An alias is represented by `Rhs::Alias(Atom)` or eliminated by rewiring; it is not an implicit recursive term.

Known code identity uses `Atom::Function(FunctionId)`. A function is not a constant and must not be interned in the constant arena. The distinction allows static function identity to participate in analysis without pretending that a runtime closure and an immutable scalar constant have the same semantics.

### Right-hand sides

The required right-hand-side families are conceptually:

```text
Rhs =
    Alias(Atom)
  | Apply { callee: Atom, arguments: [Atom] }
  | Product { schema: ProductId, fields: [Atom] }
  | Construct { constructor: ConstructorId, fields: [Atom] }
  | Project { product: Atom, field: FieldId }
  | MatchVariant { scrutinee: Atom, arms: [VariantArm], default: BlockId? }
  | SwitchBool { scrutinee: Atom, false_block: BlockId, true_block: BlockId }
  | SwitchNat { scrutinee: Atom, cases: [NatCase], default: BlockId }
  | FoldNat { index: Atom, zero: BlockId, step: BlockId }
  | FoldSequence { grain: SequenceGrain, sequence: Atom, empty: BlockId, step: BlockId }
  | Operation { operation: Operation, operands: [Atom] }
  | Foreign { foreign: ForeignId, operands: [Atom] }
  | Cell { operation: CellOperation, operands: [Atom] }
  | Intrinsic { intrinsic: Intrinsic, operands: [Atom] }
```

An implementation may place control-producing forms in a sibling enum rather than `Rhs`, but it must preserve ANF sequencing, owned branch blocks, one result binder, and exhaustive verifier and behavior queries.

`Apply` remains saturated according to the erased function contract. Known and unknown callees share the source form because known-function analysis may improve after construction; `Atom::Function` and the derived static-value domain preserve direct identity.

`SwitchBool` and `SwitchNat` remain distinct. A Bool switch cannot be widened into a Nat dispatch merely because both eventually use an i32 carrier.

`FoldNat` preserves Nat induction as a semantic construct. `FoldSequence` preserves list, bit, and byte sequence elimination with a grain or sequence-kind descriptor sufficient to recover exact element, suffix, and runtime carrier behavior.

### Functions and derived free values

A function stores its ID, ordered parameters, body block, recursive-group membership where applicable, and optional metadata such as a debug name or origin. It does not store an authoritative capture vector.

Free-value analysis derives lexical captures from ownership and use. Cont lowering consumes the current derived result, and Cont v2 remains responsible for delayed closure conversion. Any cached free-value result is invalidated when relevant functions or blocks change and rebuilt at a declared analysis boundary.

This removes capture-list maintenance from every Ersd transformation and prevents stale environments after inlining, specialization, projection folding, or branch reduction.

### Recursive groups

`RecGroup` explicitly records its simultaneously scoped functions and computed values. It preserves source order for deterministic diagnostics and uses a separately derived dependency graph for validation and lowering.

The verifier and recursive-group analysis classify dependencies among members, identify function-only SCCs, topologically order acyclic computed initialization with source-order tie-breaking, reject computed-only cycles, and identify any residual mixed knot supported by the landed Cont contract.

The analysis must be iterative. A long acyclic recursive declaration group must not consume native stack proportional to its size.

### Constants

The constant arena holds immutable, acyclic values with exact runtime semantic identity. The baseline domain includes:

```text
Constant =
    Unit
  | Bool(bool)
  | Nat(u32)
  | Byte(u8)
  | Int(i32)
  | Flt(Flt)
  | Bin { grain: Grain, value: PackedBin }
  | Io(IoToken)
  | List([ConstantId])
  | Product { schema: ProductId, fields: [ConstantId] }
  | Construct { constructor: ConstructorId, fields: [ConstantId] }
```

Exact support for Io tokens and aggregate interning may follow the existing carrier constraints, but the representation must not erase semantic distinctions merely to maximize interning.

Constants are interned deterministically by exact structural and bitwise identity. Cyclic constants are forbidden. Function identity, cells, host resources, and allocation identities are not constants.

### Static values

Optimizer knowledge is broader than the constant arena and is represented by a derived abstract domain such as:

```text
StaticValue =
    Unknown
  | Constant(ConstantId)
  | Function(FunctionId)
  | Product { schema: ProductId, fields: [StaticValue] }
  | Construct { constructor: ConstructorId, fields: [StaticValue] }
```

The implementation may use bounded references into an analysis arena to avoid recursive host-stack traversal and excessive copying. Static values are analysis results, not serialized annotations on terms.

Specialization keys use canonical constant, function, schema, constructor, and structural static-value identities. Printed terms, debug names, source spans, pointer identity, and hash-map iteration never participate in the key.

### Value shapes

Ersd v2 does not require a full erased type on every value. Instead, analysis derives the minimum shape facts needed for verification and optimization:

```text
ValueShape =
    Unknown
  | Unit
  | Bool
  | Nat
  | Byte
  | Int
  | Flt
  | Bin(Grain)
  | Io
  | List
  | Product(ProductId)
  | Variant(FamilyId)
  | Function
  | Cell
```

The domain may be refined when an implemented optimization demonstrates a concrete need. Adding full per-binder erased types is deferred until measurements and verifier requirements justify their archive size, erasure complexity, and polymorphic `Any` cases.

Shape facts are derived from definitions, schemas, constants, parameters with justified signatures, and fixed-point propagation. They are not profitability hints and must not become `candidate` under another name.

### Schemas and semantic layout

Product schemas own ordered `FieldId` rows and any semantic names required for printing or diagnostics. Variant families own their constructors; constructors own their payload field schema. IDs remain stable within the module and archived prefix.

`Project` identifies a field, and the verifier confirms that the field belongs to the product schema of the operand when that schema is known. A projection may fold from a constant or known product only through matching schema identity.

`MatchVariant` identifies constructors directly. Each arm binds its payload fields without manually projecting a numeric tag. The verifier checks family membership, duplicate arms, payload arity, and exhaustiveness or the presence of a default.

Core erasure must maintain a canonical map from elaborated declaration identities to Ersd product, family, and constructor IDs. Restoring a prelude prefix restores that map so user erasure reuses prelude schemas rather than recreating lookalikes.

### Foreign rows

An Ersd foreign node stores a `ForeignId`, not a cloned `Arc` or embedded ABI row. The module's foreign table contains canonical rows derived from `curios-abi`, including operand arity, host result arity, and the language-level result reconstruction needed by verification and lowering. A foreign statement still binds one erased language value; zero or multiple host machine results are reconstructed explicitly when lowering to Cont according to the canonical row.

All foreign operations are host-observable unless the ABI explicitly gains a stronger contract in a separate cross-layer decision. Ersd does not invent purity based on a foreign operation's name or apparent implementation.

### Metadata side tables

Debug names, source spans, and transformation origins are optional nonsemantic side tables keyed by stable IDs. Missing metadata never changes behavior or verifier success. Compaction remaps these tables together with semantic arenas.

The exact origin schema is deferred. It must remain bounded and must not create ownership cycles or force every optimizer rewrite to retain an unbounded provenance graph.

## Construction and mutation APIs

### Checked builder

The representation's fields are crate-private. Core erasure constructs a module through `ErsdBuilder` or an equivalent checked API that owns ID allocation, lexical context, schema registration, constant interning, and finalization.

The builder must make the common valid path straightforward:

- allocate parameters before entering a function body;
- allocate branch and fold binders with explicit lexical ownership;
- append statements only to their owning block;
- create schemas before values that reference them;
- register recursion before constructing mutually recursive bodies;
- finalize blocks and functions exactly once;
- reject dangling reservations at module finalization.

Builder checks complement rather than replace the whole-module verifier. Construction may temporarily contain reserved IDs, but only within the builder and never in a finalized or printable module.

### Batch editor

Optimization uses a checked batch editor or rewrite transaction rather than exposing arena vectors for arbitrary mutation. A batch records replacements, insertions, removals, block rewrites, function clones, and schema-preserving substitutions, then applies them in deterministic order.

Applying a batch invalidates the declared derived analyses. The editor does not rebuild global use lists after every insertion. Passes either maintain a narrowly documented local fact or request a bulk deterministic analysis rebuild at the next boundary.

Failed verification after a debug rewrite identifies the pass and batch. Production builds may choose a coarser verification cadence after the implementation is stable, but every public stage observation and every Cont lowering input must be verified.

### Deterministic compaction

Compaction constructs explicit old-to-new maps for every arena, rewrites all semantic and metadata references, emits live entries in their deterministic semantic order, and finishes by running the verifier. It cannot be combined with a semantic optimization whose effects would make failures ambiguous.

Archived prelude construction requires a finalized representation with no tombstones or unresolved reservations. Ordinary optimization need not compact merely to improve ID density.

## Central semantic interface

### Closed operation alphabets

Ersd behavior is described by closed enums and exhaustive queries, not open traits, generic dialect registration, or per-pass matches with fallback arms. This keeps the contract auditable, bootstrap-friendly, and compiler-enforced when an operation is added.

The principal interface is conceptually:

```text
Semantics::contract(operation) -> OperationContract
Semantics::fold(operation, constants) -> FoldOutcome
Semantics::laws(operation) -> [AlgebraicLaw]
Semantics::sequence_role(operation) -> SequenceRole?
Semantics::local_behavior(rhs) -> LocalBehavior

FoldOutcome = Value(ConstantId) | WouldTrap(TrapKind) | Unknown
```

Exact ownership may be a module of total functions rather than a runtime `Semantics` value. The essential requirements are one authoritative implementation, exhaustive matching, and use by the verifier, constant evaluator, simplifier, effect analysis, and Cont lowering tests.

Operation identity is separate from operands. A pass cannot classify behavior by inspecting a whole term variant and silently overlooking a new operation.

### Operation contracts

`OperationContract` records the facts intrinsic to one operation:

- operand arity and accepted shape rules;
- result arity and result shape rule;
- whether evaluation is total or may trap, including the trap category where useful;
- host interaction;
- mutable-state reads and writes;
- process exit or nonreturning behavior;
- allocation kind and possible identity sensitivity;
- callback invocation behavior;
- algebraic laws and their explicit preconditions;
- sequence role where applicable.

The contract reports semantic facts, not an optimizer decision. Whether to inline, clone, reorder, delete, specialize, or worker-wrap remains policy governed by context, derived summaries, and budgets.

### Semantic and operational behavior

Behavior distinguishes observable semantic events from operational actions. A suitable domain includes:

```text
ObservableBehavior {
    may_trap,
    may_diverge,
    may_exit,
    host_effect,
    state_read,
    state_write,
}

OperationalBehavior {
    allocation: None | Immutable | IdentitySensitive | Mutable,
    callback: None | Known | Unknown,
}
```

Allocation does not automatically mean a language-visible effect, but identity-sensitive or mutable allocation cannot be discarded or duplicated without an explicit proof. Keeping the dimensions separate prevents a three-valued `Pure/Host/Cell` classification from conflating traps, allocation, callback invocation, and evaluation timing.

Divergence is generally contextual and interprocedural. Most primitive operation contracts are terminating; calls and recursive folds acquire `may_diverge` through analysis rather than a hard-coded primitive flag.

### Cells

Cell creation, reading, and writing are distinct `CellOperation` identities. Creation allocates mutable identity, reading observes mutable state, and writing mutates it. Their result shapes and sequencing rules are explicit.

No optimizer may replace two cell creations with one, duplicate a cell creation, move a read across a possible write, or remove a write merely because the returned language value is unused.

### Intrinsics

Call-like helpers such as `LstMap` use a dedicated `Intrinsic` alphabet. An intrinsic contract states which operands are callbacks, how often and in what order they may be invoked, and how local behavior composes with callback summaries.

For `LstMap`, the mapper can be invoked zero or more times in element order. The result inherits mapper traps, divergence, host behavior, state access, and relevant allocation behavior. An unknown mapper yields a conservative summary.

### Algebraic laws

Laws are data with named preconditions, not unconditional enum tags. The baseline vocabulary may include identity, annihilator, idempotence, commutativity, associativity, inverse, projection/construction, and sequence decomposition, but an optimization consumes a law only after proving all preconditions for the exact carrier and behavior context.

Boolean and Nat bitwise operations remain distinct so their laws cannot be confused. `IoEq` and packed-binary equality remain distinct. Float addition and multiplication are not associative under binary32 semantics, and transformations must preserve NaNs and signed zero.

`BinAppend` and `LstAppend` append an element to a sequence; they are not homogeneous binary monoids and must not be registered as associative monoid operations. Concatenation, append-element, length, indexing, and slicing have separate sequence roles.

Any Nat or Int associativity or reassociation law that depends on overflow behavior remains disabled until Phase 0 freezes and tests the exact u32 or i32 contract. No law refers to i31.

Reordering operands or statements additionally requires that the moved computations are total and reorder-safe in context. The absence of host or cell effects alone is insufficient because a pure computation may trap, diverge, allocate an observable identity, or invoke a callback.

Bounds-sensitive sequence transformations must retain the original guard or prove the access in bounds. O(1) slicing does not justify deleting a trap that occurred before the slice result was consumed.

### Constant folding

All primitive constant evaluation goes through `Semantics::fold`. The result distinguishes a value from a known trap and from lack of knowledge. The optimizer must not turn a known trap into dead code, execute it at compile time as a compiler panic, or silently treat it as unknown when control-flow simplification depends on the distinction.

The folding implementation uses the exact u32, i32, binary32, packed-binary, list, product, and constructor semantics shared with or checked against Cont and runtime execution. A second ad hoc folder in the partial evaluator or simplifier is forbidden.

## Derived analyses

### Analysis snapshots

Analyses are immutable snapshots tied to a module revision. A pass declares which snapshots it consumes and invalidates. Rebuilding is bulk, deterministic, and measured.

The baseline analysis set includes:

- definition, ownership, and use indexing;
- lexical free values;
- global and local call graph;
- recursive SCCs and initialization dependencies;
- value shapes and static values;
- local behavior and interprocedural function summaries;
- reachability from exports, entry, and observable eager initialization;
- specialization opportunities and estimated benefit.

### Use and ownership analysis

The use index records semantic use kinds such as operand, callee, branch scrutinee, projection source, function escape, global initializer dependency, and returned value. Profitability can therefore distinguish a known function used as a callee from the same function stored in an aggregate.

Ownership analysis maps blocks and statements to their lexical function or global initializer and maps values to definitions. It is the source of dominance checks and free-value derivation.

### Call graph

Direct edges arise from `Atom::Function` and from derived static function identity. Unknown calls are recorded explicitly rather than omitted. Intrinsic callback edges are included when the callback is known and conservatively classified when unknown.

SCC construction and traversal are iterative and deterministic. Source order or stable ID order breaks ties; hash iteration does not affect summaries or diagnostics.

### Effect and termination summaries

Function summaries are computed to a fixed point over the call graph. They distinguish evaluating a definition now, invoking a function body later, and evaluating an eager global initializer.

The analysis follows these rules:

- constructing a function does not inherit its body's effects;
- a known direct call composes the callee summary;
- an unknown indirect call is conservative;
- a recursive SCC may diverge unless a stronger termination fact is proved;
- `LstMap` and other callback intrinsics compose their callback summary;
- a global initializer summary includes every call and intrinsic it evaluates;
- trap, exit, host, state, allocation, and divergence dimensions remain distinct.

The first implementation is conservative about immutable allocation and resource identity. Relaxing deletion or duplication rules requires a focused alias and observability analysis, not a blanket `pure` label.

### Reachability and global pruning

Reachability roots include the entrypoint, exported symbols, ABI-required rows, and any eager initializer whose evaluation may be observable even when its bound value has no later use.

An unreachable function may be removed even when its dormant body contains effects. An unused total eager initializer may be removed when its evaluation, allocation, and termination are proven unobservable. An unused initializer that may trap, diverge, exit, call the host, access state, or allocate observable identity must remain in evaluation order.

### Known values and specialization benefit

`candidate` is removed completely. Known constants, function identities, products, and constructors are derived from definitions and propagation. Profitability is a separate policy based on actual uses and deterministic budgets.

High-value static positions include a callee used for direct invocation, a constructor used as a match discriminator, a product used only through known projections, and a literal that unlocks bounded evaluation or branch removal. Merely having originated from a particular Core type or binder is not evidence of benefit.

Specialization keys are `(FunctionId, static argument positions and canonical static values)` or the corresponding SCC key. Keys do not contain source names, spans, printed terms, pointer addresses, or `candidate` bits.

## Hard deprecation of `candidate`

The current `candidate` annotation was a stopgap for a specialization problem that Ersd v2 solves through derived facts and explicit policy. It has no compatibility role in the new representation.

Migration removes:

- Core erasure's `is_candidate` analysis and all calls that compute it;
- the Ersd argument field and any propagation, cloning, printing, archive, or serialization support for it;
- any transitional Cont field copied from Ersd solely for candidate profitability;
- tests that assert candidate markings instead of optimizer outcomes.

There is no always-false compatibility field, deprecated accessor, archive default, or replacement annotation with a different name. If the landed Cont v2 still carries a candidate compatibility field, Phase 0 records its removal as part of switching the Ersd input contract; it must not influence correctness or remain in the final production path.

This deprecation applies to the optimizer annotation only. Generic local variables or prose using the ordinary word “candidate,” such as an elaborator unification candidate, are unrelated and must not be renamed mechanically.

Acceptance tests focus on the capability that remains after deletion: known concept methods, witness projections, higher-order map and fold functions, and recursive higher-order functions must still specialize or become direct calls when derived evidence and budgets justify it.

## Core erasure into Ersd v2

### Ownership

Core owns erasure because it holds the source representation and depends on Ersd. Erasure uses the checked builder and emits semantic Ersd constructs directly. It must not construct an intermediate copy of the legacy recursive Ersd term merely to translate it afterward in production.

### Binder and schema environment

The erasure environment maps Core variables and declarations to typed Ersd IDs. It separately tracks values, functions, globals, products, variant families, constructors, and foreign rows. Shadowing is lexical and does not depend on debug strings.

Prelude restoration seeds the declaration-to-schema and declaration-to-global maps. User suffix erasure extends those maps and never duplicates a canonical prelude identity.

### Semantic operation selection

Erasure selects the most precise Ersd semantic identity available. Bool operations remain Bool operations; Nat operations remain Nat operations; Byte values remain Byte-shaped; sequence eliminators become `FoldSequence`; product and variant operations carry schemas; foreign rows become canonical `ForeignId`s.

Erasure does not select an operation based on eventual Wasm representation. It never asks whether a Nat or Int fits i31.

### ANF construction

Erasure emits operands before their consuming statement in source evaluation order. Nested Core computations become statements and values; branch bodies become owned blocks. Atomic Core values may become atoms directly when doing so preserves identity and scope.

Deep Core terms are lowered with explicit frames rather than host recursion proportional to term depth. The default test-thread stack is the supported environment.

### Proof and type erasure

Type and proof terms erase according to current language semantics. When a retained runtime schema position represents an erased proof or type value, construction uses Unit or the established erased carrier rather than changing field numbering between instantiations.

No mandatory full erased type is attached to the resulting binder. Schema and operation identities preserve the semantic information required downstream, and `ValueShape` derives the remainder.

## Prelude archive and replay

### Archived prefix

The current archive of a vector of independent Ersd items is insufficient for a module with stable arenas and cross-item identities. Ersd v2 archives a finalized `ErsdPrefix` or equivalent containing:

- every live semantic arena entry needed by the fixed prelude;
- the constant arena and interning order;
- product, field, family, and constructor schemas;
- canonical foreign rows used by the prefix;
- top-level initialization order and source item boundaries;
- exported and canonical symbol maps;
- the Core declaration-to-Ersd identity maps needed to erase the user suffix;
- bounded debug-name metadata required for deterministic stage output.

The prefix contains no tombstones, pending reservations, transient use index, cached effect summary, specialization cache, or optimizer worklist.

### Restore and append

Restoring a prefix creates a fresh mutable builder state whose arena lengths determine the next IDs. The user suffix appends to that state. No counter is stored independently from the arena whose length defines it.

Each compilation receives an isolated restoration. Optimizing one restored module cannot mutate the archived image or another compilation's module through shared interior mutability.

Archive construction and restoration are deterministic. Repeating the same compiler build with the same sources produces equivalent prefix bytes under the build-scoped format, and repeated restoration followed by the same suffix yields identical Ersd stage output and semantic IDs.

### Format ownership

The rkyv image remains compiler-build-scoped and internal. Ersd v2 bumps the prelude archive `SCHEMA` and fails loudly on incompatible or invalid restoration; it does not add a production source fallback.

A future bootstrap boundary may need an explicitly versioned Ersd envelope and codec. That stable or cross-implementation codec is separate from the rkyv prefix and must not be approximated by declaring the build artifact stable.

## Verifier

Every finalized module and every public Ersd stage must satisfy the verifier. The verifier reports deterministic errors with relevant IDs and origins where available.

It checks at least:

- every referenced ID exists, is live, and has the expected kind;
- every local value, function, global, schema, field, family, constructor, and block is defined exactly as required;
- value uses are within lexical scope and dominated by their definitions;
- each structural block has exactly one lexical owner;
- structural block ownership is acyclic;
- statement order and block termination are complete;
- function and known-call arities agree;
- operation operand and result arities satisfy `Semantics::contract`;
- product construction arity matches its schema;
- constructor payload arity matches its constructor schema;
- projections reference a field belonging to the applicable product schema;
- variant arms belong to one family, contain no duplicates, bind the correct payload, and are exhaustive or have a default;
- Bool and Nat switches use compatible scrutinee shapes when known;
- Nat and sequence fold binder scopes and result shapes agree;
- recursive group scope, dependency classification, supported mixed knots, and deterministic order are valid;
- computed-only recursive cycles are rejected deterministically;
- foreign rows and operand and result arities agree with `curios-abi`;
- constants are acyclic, schema-correct, and exactly typed by their semantic carriers;
- tombstones are never referenced;
- archived prefixes have no tombstones or pending builder state;
- explicit candidate annotations and stored capture lists are absent by construction.

Verification and cycle detection use iterative traversal. Malformed deeply nested input must produce a diagnostic rather than overflow the native stack.

## Optimizer architecture

### Required pipeline

The initial production optimizer runs deterministic pass groups in this order:

1. Verify the finalized input.
2. Build ownership, uses, free values, call graph, recursive SCCs, effect summaries, value shapes, and static values.
3. Prune unreachable globals and functions under eager-initialization behavior.
4. Perform local alias elimination, constant folding, projection reduction, constructor-match reduction, and branch simplification.
5. Run fueled partial evaluation and residualization.
6. Perform budgeted static-value specialization where measured benefit justifies cloning.
7. Prune again and rebuild invalidated analyses.
8. Apply worker/wrapper transformations only after their laws and behavior preconditions are implemented and enabled.
9. Simplify, prune, and deterministically compact only if the selected boundary requires it.
10. Rebuild required lowering analyses and verify the output.

Passes may iterate a bounded subset to a fixed point, but the schedule, fuel, worklist order, and stopping condition must be deterministic. No phase relies on pointer identity or nondeterministic hash order.

### Local simplification

Local simplification consumes `Semantics::fold`, schemas, static values, and behavior contracts. It includes alias rewiring, known projection, known constructor match, known Bool and Nat switch reduction, and removal of unused total unobservable statements.

Removing a binding never removes an observable computation. Folding a branch must preserve the evaluation of the scrutinee and any preceding statements. Known traps remain explicit residual computations unless the containing control path is proven unreachable.

### Partial evaluation

The evaluator interprets verified Ersd blocks over the constant and static-value domains. It uses the same semantic folding interface as local simplification and never reimplements operation behavior.

Evaluation is bounded by deterministic limits for steps, call depth represented through an explicit machine stack, residual output size, recursive unfolding, and aggregate construction. Hitting a limit produces a residual term and a measured bailout reason, not a compiler error or host-stack overflow.

Residualization may move or omit a tail only when the behavior contract proves the transformation sound. If an operation, call, callback, allocation, or trap cannot be safely separated, the evaluator stops before it rather than guessing.

### Specialization

Specialization clones a function or supported function SCC for a canonical static argument pattern. It substitutes known constants, function identities, products, and constructors, then runs local simplification under a deterministic code-growth budget.

The policy accounts for:

- call-site count and whether all relevant entries agree;
- direct-call and indirect-call elimination;
- branch, match, and projection elimination potential;
- evaluator progress unlocked by the static values;
- recursion and SCC clone cost;
- current and projected live node count;
- clone count per function and per module.

A specialization that does not repay a minimum structural benefit is discarded or never committed. Budgets are stable for a fixed module and do not use wall-clock time.

Ersd specialization and Cont optimization have different responsibilities. Ersd specializes semantic data and performs direct-style partial evaluation before CPS. Cont retains late SCC-wide known-argument propagation, contification, continuation simplification, and known-function optimization exposed by the final control graph. Ersd must not duplicate Cont merely to remove every invariant parameter early.

### Worker/wrapper

The current worker/wrapper pass is not ported mechanically. It may be reintroduced only after effect summaries and law preconditions exist and after a retained workload demonstrates value beyond partial evaluation and Cont optimization.

The previous slice-cursor rationale must be revalidated because the backend now treats slices as O(1) views. A cursor transform that removes an eager bounds check changes observable trap behavior even if it improves allocation. Any future cursor form must preserve an explicit guard or prove the original access safe.

Accumulator conversion is enabled only for an operation with a valid law under the exact u32, i32, binary32, or sequence semantics and only when reordering the recursive computation is behavior-safe. The old monoid table is not an oracle and is not copied wholesale.

The `Str/count_w` workload remains an acceptance case for stack safety and behavior. Passing it does not by itself justify a generalized transform.

## Lowering to Cont v2

### Ownership and prerequisite

`curios-ersd` owns `into_cont` because it owns the source representation and depends on `curios-cont`. Implementation begins against the Cont v2 API that actually landed, not an anticipated or concurrently changing shape.

Phase 0 records:

- Cont value, function, continuation, operation, and recursive-initialization forms;
- exact handling of known versus unknown callees;
- supported mixed recursive initialization and its diagnostics;
- u32 and i32 scalar meaning before Wasm encoding;
- foreign result arity and intrinsic lowering;
- required verifier and construction APIs.

If the landed Cont behavior differs from this specification, reconcile the documents and obtain a focused design decision before broad Ersd implementation. Do not adapt by embedding Cont concepts into Ersd without review.

### Exhaustive mapping

Lowering is exhaustive over Ersd semantic constructs:

- atoms become Cont atoms or bindings while preserving known function identity;
- products and constructors lower through their schemas to the retained physical aggregate representation;
- projections and matches use verified field and constructor layout;
- Bool and Nat switches lower to appropriate Cont control without losing their prior semantic checks;
- `FoldNat` and `FoldSequence` lower to explicit Cont functions, continuations, loops, or intrinsics according to the landed contract;
- foreign and cell nodes become dedicated sequenced Cont nodes;
- callback intrinsics retain their call-like behavior;
- ordinary recursive functions become Cont recursive function groups;
- supported residual mixed initialization becomes the landed `RecInit` or equivalent form;
- `Exit` becomes a nonreturning Cont transfer;
- unreachable Ersd becomes unreachable Cont.

The mapping from semantic operations to shared physical Cont operations is centralized and exhaustively tested. It is the first point at which Bool and Nat operations or other semantically distinct operations may intentionally converge.

### Width preservation

Nat constants and operations enter Cont with u32 semantics. Int constants and operations enter Cont with i32 semantics. Lowering neither range-checks them against i31 nor inserts i31 boxing decisions.

Tests that exercise i31 immediate limits, boxing thresholds, or tagged Wasm-GC layout belong to Cont's Wasm lowering. Ersd-to-Cont tests cover the full u32 and i32 domains and confirm semantic preservation across the boundary.

### Iterative lowering

Lowering uses explicit frames and worklists for blocks, nested branches, folds, and recursive groups. It must handle the existing deep-input corpus on the default test-thread stack.

Derived free values and call facts may guide construction, but Cont's verifier is the final authority for the produced high CPS module. No stale Ersd analysis cache is serialized into Cont.

## Migration plan

The rewrite may use a short-lived side-by-side v2 path for vertical and differential testing. It must not leave two permanent Ersd abstractions or require every future optimization to support both. Production switches atomically once the unoptimized v2 path reaches Cont and passes the behavior gate; the legacy path is then retained only as a temporary oracle until later phases replace its tested capability.

### Phase 0 — Freeze the landed Cont and semantic baseline

- Finish and land Cont v2 first.
- Inventory the actual Ersd-to-Cont API, recursive initialization behavior, scalar semantics, operation alphabet, and diagnostics.
- Freeze exact u32 and i32 arithmetic, conversion, division, remainder, shift, trap, and overflow behavior with boundary tests.
- Record baseline stage output, runtime behavior, archive behavior, deep-input behavior, structural metrics, and representative optimization results.
- Inventory every producer and consumer of `candidate`, explicit captures, legacy Ersd terms, and archived Ersd items.

Exit criterion: Cont is a stable lowering target, numeric behavior is described without i31, migration consumers are enumerated, and differential or behavior tests cover the current supported pipeline.

### Phase 1 — Representation infrastructure

- Introduce private arenas, typed IDs, blocks, statements, atoms, schemas, constants, functions, recursive groups, and metadata side tables.
- Implement the checked builder, deterministic printer, verifier, iterative traversal, and deterministic compaction.
- Implement bulk ownership, definition, use, free-value, call-graph, and shape analyses on hand-built modules.
- Add a test-only construction surface that does not expose unchecked production mutation.

Exit criterion: representative modules can be built, printed, verified, analyzed, rejected deterministically when malformed, and traversed deeply without entering Core or Cont.

### Phase 2 — Core erasure

- Add Core-to-v2 erasure through the checked builder.
- Emit semantic product, variant, Bool, Nat, sequence fold, operation, foreign, cell, and intrinsic identities.
- Derive lexical free values instead of writing captures.
- Delete Core's optimizer-candidate analysis and Ersd candidate propagation from the v2 path.
- Verify and print the complete Ersd stage for source programs.

Exit criterion: the fixed prelude and representative user programs erase into verified v2 modules with deterministic stage output, no candidate annotation, no stored capture lists, and no native-stack regression.

### Phase 3 — Prelude prefix archive

- Replace archived independent Ersd items with the finalized prefix.
- Restore schemas, declaration maps, globals, constants, and IDs into a fresh builder.
- Append and erase only the user suffix.
- Bump and validate the build-scoped archive schema.
- Test deterministic construction, restoration isolation, and duplicate-schema prevention.

Exit criterion: production compilation restores the v2 prefix without source fallback, user erasure appends safely, and replay is deterministic and isolated.

### Phase 4 — Direct unoptimized lowering to Cont

- Implement exhaustive iterative v2-to-landed-Cont lowering.
- Preserve recursive groups, eager initialization, foreign results, callbacks, schemas, traps, and full u32 and i32 semantics.
- Run the existing Cont optimizer and backend after lowering.
- Compare legacy and v2 runtime behavior and normalized structural facts.

This is the critical vertical milestone. Exit criterion: the production pipeline can switch to v2 with optimization disabled, the full supported behavior corpus passes, deep inputs remain safe, and every differential discrepancy is explained or fixed.

### Phase 5 — Behavior analysis and pruning

- Implement the central semantic contract and exhaustive contract tests.
- Add fixed-point effect, divergence, callback, allocation, and call summaries.
- Add reachability and eager-global pruning.
- Add behavior-aware dead binding elimination and local simplification.
- Remove any remaining production dependency on legacy effect classifications.

Exit criterion: pruning preserves all eager traps and effects, removes dormant unreachable functions correctly, treats unknown calls and callbacks conservatively, and produces deterministic summaries and metrics.

### Phase 6 — Constant evaluation and partial evaluation

- Implement exact constant interning and `Semantics::fold` for the complete operation alphabet needed by Ersd.
- Add local constant, projection, constructor-match, and switch reductions.
- Add the fueled iterative evaluator and residualizer.
- Differentially test every foldable operation against Cont/runtime behavior at numeric and sequence boundaries.

Exit criterion: the current closed-term evaluation capability is recovered without duplicate semantics, compile-time traps are handled soundly, and evaluator budgets contain pathological inputs deterministically.

### Phase 7 — Static-value specialization

- Implement the derived static-value domain and canonical keys.
- Add use-sensitive benefit estimation and deterministic clone and node-growth budgets.
- Recover important higher-order, witness, product, constructor, literal-spine, and recursive specialization outcomes without `candidate`.
- Measure interaction with Cont's SCC-wide known-argument propagation and avoid redundant cloning.

Exit criterion: candidate-dependent capability has an evidence-based replacement, representative higher-order indirect calls disappear where profitable, and code growth remains within declared budgets.

### Phase 8 — Measured worker/wrapper recovery

- Measure residual recursion, stack behavior, slice-view allocation, bounds guards, and Cont loop formation.
- Reintroduce only transformations justified by those measurements.
- Express every transformation through semantic laws and behavior summaries.
- Preserve `Str/count_w` and other established worker/wrapper behavior tests.

Exit criterion: each enabled transform has a named profitable workload, sound law preconditions, preserved trap and effect behavior, and deterministic metrics. If no transform meets the bar, omission is a valid outcome.

### Phase 9 — Retirement and documentation

- Remove the legacy Ersd representation, optimizer, adapter, candidate field, capture lists, archive shape, and oracle path.
- Remove compatibility tests that assert obsolete structure while retaining their behavioral coverage.
- Move durable invariants into crate and module documentation.
- Update `AGENTS.md`, `ROADMAP.md`, and the bootstrap representation baseline to describe the landed architecture.
- Reconcile or remove references in `00_CONT_V2_SPEC.md`, then delete this working specification when no active document depends on it.

Exit criterion: one production Ersd representation remains, all done-bar gates pass, and repository documentation describes actual code rather than the migration.

## Testing strategy

### Representation and verifier tests

- Typed-ID kind confusion, dangling IDs, duplicate definitions, tombstoned uses, and unfinished reservations.
- Lexical dominance, sibling-block leakage, branch and fold binder scope, and unique block ownership.
- Structural block cycles and deep acyclic ownership.
- Function, operation, product, constructor, match, foreign, and intrinsic arity.
- Variant-family membership, duplicate arms, exhaustiveness, and defaults.
- Recursive scope, function-only SCCs, acyclic computed dependencies, supported mixed groups, computed-only cycles, and deterministic diagnostics.
- Constant acyclicity, exact float-bit identity, deterministic interning, and compaction remapping.
- Deterministic printing and traversal before and after rewrites.

### Core erasure tests

- Deep let spines on the default test stack.
- Runtime Nat and Int boundary values across the full u32 and i32 domains.
- Bool versus Nat operation and switch identity.
- Byte versus Nat and packed-binary grain identity.
- Products with erased proof or type fields and projections by `FieldId`.
- Variant construction, payload binding, exhaustive and default matching.
- Nat induction and list, Bits, and Bytes sequence folds.
- Local, recursive, and higher-order functions with derived free values.
- Foreign operations, cells, exits, and list map.

### Candidate-removal tests

- Concept method selection retains direct known function identity.
- Witness tuples and products fold known projections.
- Higher-order list map and fold specialize or become direct where benefit is established.
- Recursive higher-order functions propagate invariant known functions through Ersd or Cont without an annotation.
- No stage printer, archive row, serialized form, Core erasure result, or Cont input contains candidate metadata.

### Semantic contract tests

Every operation and intrinsic has an exhaustive contract test covering operand arity, result shape, traps, host and state behavior, allocation, callbacks, sequence role, and advertised laws.

Constant folding is checked against runtime execution for:

- u32 zero, one, high-bit, maximum, overflow, division, remainder, conversion, and shift boundaries;
- i32 minimum, maximum, negative, overflow, division, remainder, conversion, and shift boundaries;
- binary32 NaNs, payloads where preserved, infinities, subnormals, signed zero, and non-associative examples;
- list and packed-binary empty, singleton, boundary index, out-of-bounds, slice, append-element, and concatenation behavior;
- product, constructor, equality, and Io distinctions.

i31 boundary tests do not appear in Ersd semantic tests. They remain in Cont-to-Wasm representation and runtime tests.

### Effect and initialization tests

- An unused eager initializer that calls the host is retained.
- An unused eager initializer that reads or writes a cell is retained.
- An unused eager initializer that may trap, exit, or diverge is retained.
- An unused proven-total and unobservable initializer is removed.
- An unused function whose body contains effects is removed when the function value and creation are unobservable.
- A known call inherits the callee summary.
- An unknown call is conservative.
- `LstMap` inherits a known or unknown mapper summary.
- Recursive SCCs reach deterministic fixed points.
- Pruning preserves global evaluation order.

### Recursion tests

- Nonrecursive functions, self recursion, mutual function recursion, and deep function groups.
- Acyclic computed recursive groups with source-order tie-breaking.
- Every mixed function/computed pattern accepted by the landed Cont contract.
- Deterministic `CyclicRecComputed` and retained `UnsupportedSyncRecItem` cases.
- Residual mixed knots lower to the exact landed Cont recursive-initialization form.
- Derived free values agree before and after specialization and pruning.

### Archive tests

- Repeated archive construction yields deterministic bytes within one compiler build.
- Restored prefixes reproduce IDs, schemas, stage output, and symbol mappings.
- User suffix append begins at arena lengths and cannot collide with prefix IDs.
- Prelude product and constructor identities are reused rather than duplicated.
- Two restorations are mutation-isolated.
- Invalid schema, tombstones, pending reservations, or dangling IDs fail restoration loudly.
- Production retains no source fallback.

### Optimizer tests

- Alias, constant, projection, constructor-match, Bool switch, and Nat switch simplification.
- Partial evaluation of formatter parsing, literal spines, known function arguments, known products, and known constructors.
- Deterministic bailout for step, depth, unfolding, aggregate, and residual-size budgets.
- Specialization key stability across printing and metadata changes.
- Clone-count and node-growth enforcement.
- Preservation of traps, host order, state order, callback order, and allocation identity.
- Algebraic transformations with satisfied and deliberately unsatisfied preconditions.
- Bounds guards retained by sequence transformations.
- `Str/count_w` stack safety and result behavior.

### Cont and end-to-end tests

- Unoptimized Ersd v2 lowers to verified Cont v2.
- Known functions remain known calls where expected; unknown closures remain indirect.
- Products, variants, sequence folds, cells, foreign calls, callbacks, exits, and recursive initialization retain behavior.
- Full u32 and i32 values survive Ersd-to-Cont lowering.
- Raw Wasm validation and runtime behavior agree with the legacy baseline.
- Existing compiler-stage, codegen, standard-library, browser-applicable, and deep-input suites retain coverage.
- Raw and post-Binaryen behavior are both checked where optimization might otherwise hide poor lowering.

### Differential tests

During migration, compile representative Core modules through both the legacy and v2 Ersd paths. Compare runtime results, ordered host observations, exact expected traps, and normalized structural facts rather than requiring identical internal IDs or Wasm bytes.

Differential tests are removed only after the legacy path is deleted and their behavioral obligations have permanent owners. A discrepancy is never accepted solely because the v2 output appears more optimized.

## Metrics and quality gates

Debug or benchmark instrumentation records at least:

- live and tombstoned counts for each Ersd arena;
- builder, verifier, compaction, analysis, optimization, archive restore, and Cont lowering time;
- analysis rebuild counts and invalidation reasons;
- functions, globals, blocks, statements, values, schemas, constructors, and constants;
- direct, indirect, and intrinsic callback call edges;
- recursive SCCs and residual mixed initialization groups;
- effect-summary populations by trap, divergence, host, state, exit, and allocation dimensions;
- known constants, functions, products, constructors, and shape precision;
- evaluator steps, residual nodes, and bailout reasons;
- specialization opportunities, committed clones, rejected clones, and attributed node growth;
- worker/wrapper opportunities, applied transforms, guards retained, and measured benefit;
- prefix archive bytes, restoration counts, and suffix arena growth;
- Ersd and Cont stage text size plus raw and optimized Wasm size for representative programs.

Metrics are deterministic for a fixed compiler build and input except for elapsed time. They are diagnostic facts, not a stable public output format.

The structural acceptance cases include:

- higher-order arithmetic or concept functions become direct calls where static identity is available and profitable;
- known constructor and product arguments eliminate corresponding matches and projections;
- eager observable initializers remain present despite unused results;
- ordinary recursive functions do not gain Ersd-stored captures;
- full-width Nat and Int semantics are visible through Ersd and Cont while i31 decisions appear only in Wasm lowering;
- deep erasure, analysis, optimization, printing, archive replay, and lowering succeed on the default test stack;
- code growth stays within declared deterministic budgets.

Performance acceptance compares compiler time, peak memory where measurable, generated raw Wasm, final native Wasm, and runtime on the retained representative workloads. A transform is not required merely because the legacy optimizer had it; it is required when removing it creates a demonstrated behavioral, stack-safety, compiler-quality, or material performance regression that the new pipeline does not otherwise recover.

## Failure containment and rollback points

Each migration phase keeps a working production boundary or an isolated, test-only v2 path until its exit criterion passes.

- Arena representation can be tested without Core erasure.
- Core erasure can print and verify v2 before it becomes production.
- Prefix replay can be validated against source-built v2 in tests while production still uses the last working boundary.
- Unoptimized v2 can lower into landed Cont before any new optimizer is enabled.
- Behavior analysis, evaluation, specialization, and worker/wrapper are enabled independently.
- The legacy path remains only as a temporary behavior oracle and is never extended with new architecture after the production switch.

If a phase fails, fix or redesign that phase. Do not compensate by weakening verification, retaining candidate metadata, storing manual captures, moving i31 into Ersd, changing source recursive semantics, or broadening Cont and runtime scope without a new decision.

## Deferred decisions

The following remain deferred until the specified baseline and measurements exist:

- mandatory full erased-type annotations for every binder;
- the exact bounded transformation-origin metadata schema;
- a stable cross-implementation Ersd codec for bootstrap;
- richer alias, escape, allocation-identity, or resource analyses;
- profile-guided or nondeterministic optimization budgets;
- generalized cursor or slice-view worker representations;
- generic operation traits or extensible compiler dialects;
- additional static-value shapes beyond demonstrated optimizer needs;
- changing the accepted language of recursive computed values;
- changing the Cont, closure, aggregate, or Wasm-GC runtime representation.

Adopting a deferred option requires a focused proposal describing its owner, consumers, verifier consequences, archive consequences, migration cost, measurements, and bootstrap impact. None is silently included in Ersd v2.

## Done bar

Ersd v2 is complete only when:

- Core erases directly into a verified arena-backed scoped ANF module;
- the production representation has stable typed IDs, explicit lexical ownership, deterministic traversal, and iterative deep-input handling;
- product, variant, Bool, Nat, Byte, sequence, foreign, cell, and intrinsic semantics remain explicit through Ersd optimization;
- all operation contracts, constant folding, effects, callbacks, traps, allocations, and algebraic laws are centralized and exhaustive;
- Nat and Int use full u32 and i32 semantics throughout Ersd and Cont, with every i31 constraint confined to Cont-to-Wasm lowering;
- `candidate`, `is_candidate`, compatibility fields, serialized annotations, and candidate-based tests are absent;
- captures are derived from lexical ownership and uses rather than stored on Ersd functions;
- eager global pruning, recursive initialization, unknown calls, and callback intrinsics preserve observable behavior;
- the constant evaluator, residualizer, and specialization system are deterministic, bounded, and use canonical semantic keys;
- specialization recovers the important higher-order and witness-driven capability without type-origin hints;
- worker/wrapper transforms are either reintroduced under explicit sound laws and measured benefit or deliberately omitted with their acceptance workloads satisfied elsewhere;
- the fixed prelude restores as a finalized, deterministic, isolated Ersd prefix and user erasure appends without identity duplication;
- optimized Ersd lowers directly and iteratively into the verified landed Cont v2 representation;
- public `Ersd` and `ErsdOptm` stage observation is deterministic;
- the specified unit, differential, archive, semantic, recursion, deep-input, raw-Wasm, runtime, browser-applicable, performance, and repository-wide gates pass;
- no production or active test path depends on the legacy Ersd representation or optimizer;
- durable contracts have moved to their owning crate and module documentation;
- the roadmap and bootstrap representation-series gate identify the landed Ersd architecture rather than this working specification.
