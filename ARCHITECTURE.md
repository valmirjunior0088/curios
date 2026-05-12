# Curios — Architectural Overview

Curios is a compiler for an impure, dependently typed functional programming language targeting WebAssembly. It combines full dependent types (Π, Σ, atoms) with first-class functions, algebraic data via labeled unions, and compiles through a CPS intermediate representation down to WebAssembly bytecode executed by Wasmtime.

**Codebase size:** ~18,700 lines of Rust.

---

## Compilation Pipeline

Source text flows through five distinct phases, each with a clean handoff:

```
Source Text
   │
   ▼
Parsing          → core::Term (full AST with types)
   │
   ▼
Type Inference    → core::Term (annotated, type-checked)
   │
   ▼
Type Erasure      → ersd::Term (runtime-only structure)
   │
   ▼
CPS Lowering      → cont::Module (blocks, closures, jumps)
   │
   ▼
WASM Codegen      → wasm::Module (structs, functions, instructions)
   │
   ▼
Binary Writer     → raw WebAssembly bytes
   │
   ▼
Wasmtime          → execution and result printing
```

---

## 1. Parsing

**Files:** `src/core/parse.rs`, `src/monads/parser.rs`

A custom monadic parser combinator library. `Parser<'a, A>` supports `or`, `and`, `flat_map`, and `lazy` combinators, with position-aware error reporting via `ParserState`.

The grammar covers:

- Dependent function types `(x: A) -> B`, lambdas `x => body`
- Dependent pair types `(x: A, B)`, pair values `(a, b)`
- Atom types `{:left, :right}`, atom values `:left`
- Pair elimination `let (x, y) with m => motive = pair; tail`
- Pattern matching `match x with k => Type; case :tag => body;`
- Let bindings and recursive groups `let { f : T = body; }; tail`
- Primitive types (`Nat`, `Int`, `Flt`) and built-in operations (arithmetic, comparisons, and conversions for all three — e.g. `Int.add`, `Nat.div`, `Flt.sqrt`, `Int.to-flt`)

---

## 2. Core Type System

**Files:** `src/core/term.rs`, `src/core/typing.rs`, `src/core/reduce.rs`, `src/core/convert.rs`, `src/core/context.rs`

The central `Term` enum represents the full surface language:

| Variant                       | Role                                             |
| ----------------------------- | ------------------------------------------------ |
| `Type`                        | The sort (type of types — no universe hierarchy) |
| `FuncType` / `Func` / `Apply` | Π-types, λ-abstraction, application              |
| `PairType` / `Pair` / `Split` | Σ-types, pair construction, elimination          |
| `AtomType` / `Atom` / `Match` | Labeled unions, tags, pattern matching           |
| `Let` / `LetRec`              | Bindings, mutual recursion                       |
| `Prim`                        | Built-in values and operations                   |
| `Name`                        | Bound variables                                  |

**Key techniques employed:**

### De Bruijn Indices with Bidirectional Conversion

Variables are stored as labels (`String`) during parsing and printing but converted to de Bruijn indices after binding. The `Scope` type's `close(labels, body)` captures free labels as indices, and `open(terms)` substitutes indices back with concrete terms. `shift(amount)` adjusts indices when moving under binders. This solves alpha-equivalence without rename passes.

### Generic Binder Abstraction (`Scope<A: Arity>`)

A single `Scope` type handles 1-ary (functions), 2-ary (pair elimination), and n-ary (recursive groups) binders through an `Arity` trait with associated type `Params<'a, T>`. This provides compile-time arity safety across all binding forms.

### Bidirectional Type Checking

Type inference synthesizes types upward (`infer` mode) and checks them downward (`erase` with `expect`). Dependent types mean the codomain of a function type depends on the argument value — so after checking the argument, the compiler reduces the codomain with the argument substituted before proceeding.

### Timeout-Controlled Reduction

Every reduction operation receives an `Instant` deadline. This prevents infinite loops during type checking (which requires normalization of terms). If the deadline expires, a `Preempted` error propagates up. Each inference invocation gets an independent deadline.

### BFS-Based Type Equality

`src/core/convert.rs` checks type equality by reducing both sides to normal form and comparing structurally. The comparison uses a BFS queue with memoization rather than recursive descent, which avoids stack overflow on deeply nested types.

### Two-Level Context

`src/core/context.rs` maintains separate stacks for **assumptions** (name → type) and **definitions** (name → value). Scoped frames via `with_frame(f)` handle nested contexts. Fresh name generation uses an entropy counter.

### Impure Terms and Type-Level Reduction

Curios is an impure language: effectful operations (IO, etc.) are ordinary expressions at the term level. When the type checker needs to normalize a term — for instance, to check type equality or to compute a dependent return type — and that term contains an effectful operation, reduction raises a type error. This keeps the type checker pure and predictable without restricting what programs can do at runtime.

---

## 3. Type Erasure

**Files:** `src/ersd/term.rs`, `src/ersd/prim.rs`

Transforms `core::Term` into `ersd::Term`, stripping everything that exists only at the type level:

| Erased (removed)                           | Preserved                                 |
| ------------------------------------------ | ----------------------------------------- |
| `Type`, `FuncType`, `PairType`, `AtomType` | `Func`, `Apply`, `Pair`, `Split`, `Match` |
| Type annotations on bindings               | Function bodies, captures, parameters     |
|                                            | `Let`, `LetRec`, all control flow         |
|                                            | Primitives (except type constructors)     |

**Atom index translation:** During erasure, atom labels (`:left`, `:right`) are replaced with numeric indices matching case order in `Match`. This enables efficient dispatch without string comparison at runtime.

**Explicit closure captures:** `ersd::Func` carries a `captures: Vec<String>` listing exactly which free variables the function closes over, resolved to concrete values during CPS lowering.

---

## 4. CPS Lowering

**Files:** `src/cont/module.rs`, `src/ersd/to_cont/lowerer.rs`, `src/ersd/to_cont/entropy.rs`, `src/ersd/to_cont/frame.rs`

The CPS IR is the heart of the compiler's control flow representation:

```
Module
  ├── consts: global constant values
  ├── clsrs:  closure definitions (environment shape + body)
  └── funcs:  top-level functions
        └── Region
              ├── values: local value definitions (SSA-like)
              ├── blocks: join points (labeled, with parameters)
              └── tail:   terminator (Jump | Case | Call)
```

**Values** include: `Pure` (constants), `Eval` (primitive ops), `Clsr` (closure allocation with captures), `Tpl2` (pairs), `Proj` (projection), `Name`.

**Tails** (terminators) include: `Jump` (unconditional branch to block), `Case` (dispatch on atom index via `br_table`), `Call` (direct or indirect function call with resume target).

### Second-Class Continuations

This is the defining characteristic of the CPS IR. Continuations are **block labels** — named join points within a region — not callable closures. A `Call` specifies a `resume` target (a block name), and when the callee returns, control jumps to that block. You can declare continuations and jump to them, but you cannot store them in data structures, pass them as arguments, or return them. They are purely structural, scoped to their enclosing region.

This means the CPS IR maps directly to WASM's structured control flow (`block`/`br`/`br_table`) without needing to reify continuations as heap objects.

### Join Blocks for Value-Position Calls

When a function call appears in value position (not tail position), the lowerer creates a **join block** that receives the result as a parameter. This normalizes the control flow graph into an SSA-like form and avoids diamond-shaped CFGs.

### Mutual Recursion via Preallocated Stubs

`LetRec` groups preallocate stub values for all names before any bodies are lowered. This allows mutual references within the group. The stubs are then assigned within the same region.

### Frame-Based Variable Scoping

Each scope (function, block, closure) extends a `Frame` (HashMap of name → `ValueName`). Lookups walk the implicit scope chain. Fresh names are generated by `Entropy<T>` streams (separate streams for values, blocks, closures, functions).

---

## 5. WebAssembly Code Generation

**Files:** `src/cont/to_wasm/`, `src/wasm/expr.rs`, `src/wasm/module.rs`, `src/wasm/types.rs`, `src/wasm/writer.rs`

### Value Representation

| Curios Value | WASM Representation                                |
| ------------ | -------------------------------------------------- |
| Integer      | `i31ref` (31-bit signed, packed in a single i32)   |
| Float        | Boxed in a GC struct with a single `f32` field     |
| Unit         | Empty GC struct                                    |
| Pair/Tuple   | GC struct with two `anyref` fields                 |
| Closure      | GC struct with funcref + captured values as fields |
| Atom         | `i31ref` (the index)                               |

### Closure Calling Convention

A closure is a GC struct whose first implicit member is a typed function reference and whose remaining fields are the captured values. Calling a closure means: load the funcref from the struct, pass the struct itself as the environment parameter along with the actual argument, and `call_ref`. The callee loads its captures from the struct via `struct.get`.

### Tail Calls

The codegen uses WASM's tail call extension (`return_call` for direct calls, `return_call_ref` for indirect). This eliminates stack growth for recursive patterns — critical for a functional language.

### Codegen Submodules

- **`src/cont/to_wasm/table.rs`:** Builds symbol tables mapping CPS names to WASM type/function names. Pre-allocates struct types for closures, tuples, and floats.
- **`src/cont/to_wasm/context.rs`:** Tracks locals, frames, and value classifications (`Raw`, `NonNull`, `Concrete`, `Int`, `Flt`) for correct loading and casting.
- **`src/cont/to_wasm/frame.rs`:** Represents nested WASM blocks, accumulates instructions, manages label-based branching.
- **`src/cont/to_wasm/expr_emitter.rs`:** Emits instructions for CPS values — closure allocation, tuple construction/projection, arithmetic with type conversions, constant promotion.
- **`src/cont/to_wasm/module_emitter.rs`:** Emits the top-level WASM module: imports, type definitions, function bodies, exports.

### Binary Serialization

`src/wasm/writer.rs` serializes the WASM AST to the binary format. The compiler writes its own WASM binary rather than depending on a library like `wasm-encoder`.

---

## 6. Execution

**File:** `src/execute.rs`

Wasmtime is configured with:

- Reference types (anyref)
- Function references (typed funcref)
- GC (struct/array types)
- Tail calls

The pipeline runs parse → infer → erase → CPS lower → WASM codegen → serialize → load into Wasmtime → call `func/main` → print result. Result printing uses a `RefIds` table to track already-seen references (cycle detection) and recursively formats structs, arrays, and i31 values.

---

## 7. Testing

Tests exist at each layer:

- **Term operations:** scope open/close symmetry, capture/release substitution
- **Parsing:** round-trip tests for let-rec, atoms, pairs, function types
- **Reduction:** beta-reduction, let inlining, timeout enforcement
- **CPS lowering:** recursive pairs, tail application, join block creation
- **WASM codegen + execution:** end-to-end tests that compile and run through Wasmtime, checking final output values

---

## 8. Utility Layer

**`src/monads/`:** Provides both the parser combinator library (`Parser<'a, A>`) and a printer combinator library used for pretty-printing terms. The printer uses `Printer` combinators that mirror the parser structure, giving a degree of symmetry between parsing and printing.

**`src/macros.rs`:** Helper macros used across the codebase.

---

## Start Here

For anyone wanting to understand this project:

1. **Browse `examples/`** — the example files (`examples/core.rs`, `examples/execute.rs`, etc.) show the language in action at each compilation stage. They are the fastest way to see what Curios programs look like and how the pipeline behaves. For a full pipeline execution test, see `tests/end_to_end.rs`.

2. **Read `src/core/term.rs`** — the `Term` enum is the central data structure. Everything else transforms it or consumes it. Understanding the variants (especially `Scope`, `FuncType`, `PairType`, `AtomType`) is prerequisite to everything.

3. **Read `src/core/parse.rs`** — see what the surface syntax looks like. The test cases at the bottom are concrete examples.

4. **Read `src/core/typing.rs`** — follow how each `Term` variant gets its type. Notice the bidirectional flow and where reduction is invoked.

5. **Read `src/ersd/term.rs`** — see exactly what disappears and what survives into runtime.

6. **Read `src/cont/module.rs`** — the CPS IR types. Understand `Region`, `Value`, `Tail`, and especially how `Call` specifies a `resume` block.

7. **Read `src/ersd/to_cont/lowerer.rs`** — follow how `ersd::Term` becomes CPS. The `lower_tail` and `lower_value` distinction (tail position vs. value position) is the key insight.

8. **Read `src/cont/to_wasm/expr_emitter.rs`** and **`src/cont/to_wasm/module_emitter.rs`** — see how CPS maps to WASM instructions.

9. **Read `src/execute.rs`** — the top-level pipeline that ties everything together. Run `cargo test` to see the end-to-end tests execute.
