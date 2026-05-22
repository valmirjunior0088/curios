# Curios — Architectural Overview

Curios is a compiler for an impure, dependently typed functional programming language targeting WebAssembly. It combines full dependent types (Π, Σ, atoms) with first-class functions, algebraic data via labeled unions, and compiles through a CPS intermediate representation down to WebAssembly bytecode executed by Wasmtime.

**Codebase size:** ~28,200 lines, including examples, tests, and docs.

---

## Compilation Pipeline

Source text flows through six distinct phases, each with a clean handoff:

```
Source Text
   │
   ▼
Parsing           → text::Entrypoint (surface AST: module items + tail expression)
   │
   ▼
Elaboration       → core::Term (full AST with de Bruijn indices)
   │
   ▼
Type Inference    → core::Term type result (checked source term)
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

**Files:** `src/text/parse.rs`, `src/monads/parser.rs`

A custom monadic parser combinator library. `Parser<'a, A>` supports `or`, `and`, `flat_map`, and `lazy` combinators, with position-aware error reporting via `ParserState`.

Parsing produces a `text::Entrypoint` — a surface AST consisting of a list of top-level `TopItem`s followed by a `tail: Term`. Each `TopItem` is one of `Mod` (a nested module block), `Use` (an import), `Let` (a top-level binding), `Rec` (a mutually recursive group), or `Def` (an opaque-type block). The tail expression and all inner term bodies are `text::Term` nodes where every variable is a plain string label. There are no de Bruijn indices at this stage; all binding and scoping is resolved during elaboration.

The grammar covers:

- Dependent function types `(x: A) -> B` or bare `A -> B`, lambdas `x => body`
- Tuple types `{x: A, B, z: C}` (curly braces, fields optionally labeled), tuple values `(a, b)` or `(a, b, c)`
- Atom types `'[left, right]`, atom values `'left`
- Tuple elimination `split tuple : m => motive; | (x, y) => tail`
- Natural-number induction `Nat.fold n : k => motive; | 0 => zero; | pred ih => succ;`
- Sparse natural-number dispatch `Nat.match n : k => motive; | 34 => body; | _ => default;`
- Char literals as nat values: single-character string with `n` suffix (`"["n`, `"\""n`) parses to the Unicode codepoint
- Pattern matching `match x : k => Type; | 'tag => body;`
- Non-recursive let bindings `let x : T = body; tail`
- Recursive groups `rec f : T = body; tail` or `rec f : T = v and g : T2 = v2; tail` for mutual recursion
- Primitive types (`Nat`, `Int`, `Flt`) and built-in operations (arithmetic, comparisons, and conversions — e.g. `Int.add`, `Nat.div`, `Flt.sqrt`, `Int.to_flt`)
- Binary values via string literals, hex byte literals, and `Bin.len`/`Bin.eql`/`Bin.get`/`Bin.slice`/`Bin.append`/`Bin.concat`
- Array type `Arr T` and array literals, with operations: `Arr.len`, `Arr.get`, `Arr.slice`, `Arr.append`, `Arr.concat`
- Module blocks `mod Label ... end` / `pub mod Label ... end` — group bindings under a namespace; only `pub` items are accessible from outside
- Import declarations `use Path/qualifier` (relative) / `use /abs/Path` (absolute) / `pub use ...` to re-export a qualifier; single-segment relative `use` is forbidden
- Qualified names `Namespace/name` (slash-separated) used to reference items inside modules
- Opaque-type blocks `def Label(witness) ... end` — introduces an opaque type named `Label` backed by `witness`; `Label.from value` coerces into the opaque type, `Label.into value` coerces out; coercions are only valid inside the `def` block

---

## 2. Elaboration

**Files:** `src/text/to_core.rs`, `src/text/to_core/elaborate.rs`, `src/text/to_core/context.rs`

Converts a `text::Entrypoint` into `core::Term` via the public `text::to_core()` function. This phase has two concerns:

**Module processing** (`src/text/to_core.rs`): `process_items()` walks the `TopItem` list recursively, building a name-resolution scope. It:
- Resolves `use` declarations, registering qualifiers and enforcing `pub`/private visibility.
- Translates `mod` blocks into nested scopes, qualifying each binding name (e.g. `Foo/bar`).
- Translates `def Label(witness) ... end` into `core::Sealed` nodes, binding the opaque-type label over the elaborated body.
- Collects `let`/`rec` groups as flat items, then folds them right-to-left into the tail expression.

**Term elaboration** (`src/text/to_core/elaborate.rs`): `Elaborate` converts each `text::Term` into the corresponding `core::Term` constructor via structural recursion, calling `Scope::close()` to bind free variable labels as de Bruijn indices.

The key distinctions between the two representations:

- **`text::Term`** — all variables are `String` labels; binders (lambdas, let, rec, split) carry their binding labels as plain strings; `FuncType.label` is `Option<String>` to allow anonymous `A -> B` types; `TupleType` and `Split` carry `Vec<String>` field labels.
- **`core::Term`** — variables are de Bruijn indices stored in `Scope<A: Arity>` wrappers; free variables (not yet bound by any enclosing scope) remain as labeled `Var::free(label)` until they are captured by a `Scope::close()` call.

`Elaborate` does no type-directed work — it is a pure syntactic translation. Type checking happens after, in `src/core/typing.rs`.

---

## 3. Core Type System

**Files:** `src/core/term.rs`, `src/core/typing.rs`, `src/core/reduce.rs`, `src/core/convert.rs`, `src/core/context.rs`

The central `Term` enum represents the full typed language after elaboration:

| Variant                           | Role                                             |
| --------------------------------- | ------------------------------------------------ |
| `Type`                            | The sort (type of types — no universe hierarchy) |
| `FuncType` / `Func` / `Apply`     | Π-types, λ-abstraction, application              |
| `TupleType` / `Tuple` / `Split`   | Σ-types (n-ary), tuple construction, elimination |
| `NatFold`                         | Natural-number structural induction (zero case + pred/IH case) |
| `NatMatch`                        | Sparse dispatch on specific nat values (explicit cases + default) |
| `AtomType` / `Atom` / `Match`     | Labeled unions, tags, pattern matching           |
| `Let` / `Rec`                     | Bindings, mutual recursion                       |
| `Sealed` / `Seal` / `Unseal`      | Opaque-type abstraction (from `def`): `Sealed` binds a label to a witness type over a continuation; `Seal` / `Unseal` coerce values in and out of the opaque type |
| `Prim`                            | Built-in values and operations                   |
| `Var`                             | Bound variables                                  |

**Key techniques employed:**

### De Bruijn Indices with Bidirectional Conversion

Variables arrive from elaboration as free labels (`Var::free(label)`). Each binding construct (`Func`, `FuncType`, `Split`, `Rec`, etc.) calls `Scope::close(labels, body)` to capture those free labels as de Bruijn indices, and `open(terms)` to substitute indices back with concrete terms during reduction. `shift(amount)` adjusts indices when moving under binders. This solves alpha-equivalence without rename passes.

### Generic Binder Abstraction (`Scope<A: Arity>`)

A single `Scope` type handles 1-ary (functions), n-ary (tuple elimination, recursive groups) binders through an `Arity` trait with associated type `Params<'a, T>`. This provides compile-time arity safety across all binding forms.

### Bidirectional Type Checking

Type inference synthesizes types upward (`infer` mode) and checks them downward (`erase` with `expect`). Dependent types mean the codomain of a function type depends on the argument value — so after checking the argument, the compiler reduces the codomain with the argument substituted before proceeding.

### Timeout-Controlled Reduction

Every reduction operation receives an `Instant` deadline. This prevents infinite loops during type checking (which requires normalization of terms). If the deadline expires, a `Preempted` error propagates up. Each inference invocation gets an independent deadline.

### BFS-Based Type Equality

`src/core/convert.rs` checks type equality by reducing both sides to normal form and comparing structurally. The comparison uses a BFS queue with memoization rather than recursive descent, which avoids stack overflow on deeply nested types.

### Two-Level Context

`src/core/context.rs` maintains separate stacks for **assumptions** (name → type) and **definitions** (name → value). Scoped frames via `with_frame(f)` handle nested contexts. Fresh name generation uses an entropy counter.

### Opaque Types (`def`)

`Sealed` binds a label to a witness type (the underlying representation) and scopes it over a continuation. Inside that scope, `Seal` (`Label.from`) and `Unseal` (`Label.into`) coerce values between the opaque type and the witness. The type checker enforces that the label and the representation are interchangeable only within the `def` block; outside, the label is opaque. All three constructs are transparent at runtime — erasure drops `Sealed` and replaces `Seal`/`Unseal` with a direct pass-through of the wrapped value.

### Runtime Effects and Type-Level Reduction

Curios is intended to support impure term-level computation, but type-level normalization must remain pure and predictable. The current core implements total reduction for the existing primitive term forms (`Nat`, `Int`, `Flt`, `Bin`, `Arr`, functions, tuples, atoms, and eliminators) with timeout protection; future effectful primitives should be treated as opaque or rejected during type-level reduction rather than executed by the checker.

---

## 4. Type Erasure

**Files:** `src/ersd/term.rs`, `src/ersd/prim.rs`

Transforms `core::Term` into `ersd::Term`, stripping everything that exists only at the type level:

| Erased (removed)                              | Preserved                                            |
| --------------------------------------------- | ---------------------------------------------------- |
| `Type`, `FuncType`, `TupleType`, `AtomType`   | `Func`, `Apply`, `Tuple`, `Split`, `NatMatch`, `Match` |
| `Sealed`, `Seal`, `Unseal`                    | Function bodies, captures, parameters                |
| Type annotations on bindings                  | `Let`, `Rec`, all control flow                       |
|                                               | Primitives (except type constructors)                |
|                                               | `Bin`, `Arr`, and their operations                   |
|                                               | `Name` references                                    |

**Erased placeholder:** Removed type-level terms (`Type`, `FuncType`, `TupleType`, `AtomType`) are not dropped outright — they are replaced by the `ersd::Term::Erased` variant, which serves as a runtime placeholder for any position that was occupied purely by type information.

**Atom index translation:** During erasure, atom labels (`'left`, `'right`) are replaced with numeric indices matching case order in `Case`. This enables efficient dispatch without string comparison at runtime.

**Explicit closure captures:** `ersd::Func` carries a `captures: Vec<String>` listing exactly which free variables the function closes over, resolved to concrete values during CPS lowering.

---

## 5. CPS Lowering

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

**Values** use a three-level structure:

- `Value` has three variants: `Pure(Data)`, `Eval(Code)`, `Alias(ValueName)`
- `Data` (constant/aggregate): `Unit`, `Nat(u32)`, `Int(i32)`, `Flt(f32)`, `Bin(Vec<u8>)`, `Arr(Vec<ValueName>)`, `Tpl(Vec<ValueName>)`, `Clsr(ClsrName, Vec<ValueName>)`
- `Code` (computed): arithmetic, comparison, conversion, selected bitwise/counting ops, `TplGet(ValueName, usize)`, `BinLen`/`BinEql`/`BinGet`/`BinSlice`/`BinAppend`/`BinConcat`, and `ArrLen`/`ArrGet`/`ArrSlice`/`ArrAppend`/`ArrConcat`

**Tails** (terminators) include: `Jump` (unconditional branch to block), `Match` (sparse dispatch on a `u32` key — atom index or nat value — with a `BTreeMap` of explicit cases and an optional default), `Call` (direct or indirect function call with resume target).

### Second-Class Continuations

This is the defining characteristic of the CPS IR. Continuations are **block labels** — named join points within a region — not callable closures. A `Call` specifies a `resume` target (a block name), and when the callee returns, control jumps to that block. You can declare continuations and jump to them, but you cannot store them in data structures, pass them as arguments, or return them. They are purely structural, scoped to their enclosing region.

This means the CPS IR maps directly to WASM's structured control flow (`block`/`br`/`br_table`) without needing to reify continuations as heap objects.

### Join Blocks for Value-Position Calls

When a function call appears in value position (not tail position), the lowerer creates a **join block** that receives the result as a parameter. This normalizes the control flow graph into an SSA-like form and avoids diamond-shaped CFGs.

### Mutual Recursion via Preallocated Stubs

`Rec` groups pre-reserve value names before any bodies are lowered. This allows mutual references within the group when each recursive right-hand side can lower directly to a `cont::Value` in the current region. The MVP lowerer intentionally rejects more general recursive RHSs that would need value-level knot tying such as aliases, cells, or fixpoint support.

### Frame-Based Variable Scoping

Each scope (function, block, closure) extends a `Frame` (HashMap of name → `ValueName`). Lookups walk the implicit scope chain. Fresh names are generated by `Entropy<T>` streams (separate streams for values, blocks, closures, functions).

---

## 6. WebAssembly Code Generation

**Files:** `src/cont/to_wasm/`, `src/wasm/expr.rs`, `src/wasm/module.rs`, `src/wasm/types.rs`, `src/wasm/writer.rs`

### Value Representation

| Curios Value | WASM Representation                                                                  |
| ------------ | ------------------------------------------------------------------------------------ |
| Natural      | `i31ref` (packed in a single i32)                                                    |
| Integer      | `i31ref` (packed in a single i32)                                                    |
| Float        | Boxed in a GC struct with a single `f32` field                                       |
| Unit         | Empty GC struct                                                                      |
| Tuple        | GC struct with N `anyref` fields (subtype hierarchy: `tpl/1` ← `tpl/2` ← `tpl/3` …)|
| Closure      | GC struct with funcref + captured values as fields                                   |
| Atom         | `i31ref` (the index)                                                                 |
| Binary       | GC array of packed `i8`                                                              |
| Array        | GC array of nullable top references                                                  |

### Closure Calling Convention

A closure is a GC struct whose first implicit member is a typed function reference and whose remaining fields are the captured values. Calling a closure means: load the funcref from the struct, pass the struct itself as the environment parameter along with the actual argument, and `call_ref`. The callee loads its captures from the struct via `struct.get`.

### Tail Calls

The codegen uses WASM's tail call extension (`return_call` for direct calls, `return_call_ref` for indirect). This eliminates stack growth for recursive patterns — critical for a functional language.

### Codegen Submodules

- **`src/cont/to_wasm/table.rs`:** Builds symbol tables mapping CPS names to WASM type/function names. Pre-allocates struct types for closures, tuples, and floats.
- **`src/cont/to_wasm/context.rs`:** Tracks locals, frames, and value classifications (`Null`, `NonNull`, `Concrete`, `Int`, `Flt`, `Bin`, `Arr`) for correct loading and casting.
- **`src/cont/to_wasm/frame.rs`:** Represents nested WASM blocks, accumulates instructions, manages label-based branching.
- **`src/cont/to_wasm/expr_emitter.rs`:** Emits instructions for CPS values — closure allocation, tuple construction/projection, arithmetic with type conversions, constant promotion.
- **`src/cont/to_wasm/module_emitter.rs`:** Emits the top-level WASM module: imports, type definitions, function bodies, exports.

### Binary Serialization

`src/wasm/writer.rs` serializes the WASM AST to the binary format. The compiler writes its own WASM binary rather than depending on a library like `wasm-encoder`.

### WAT Parser

`src/wasm/parse.rs` implements a `FromStr` parser for `wasm::Module` covering the WebAssembly Text (WAT) format. It uses the same monadic parser combinator library as the Curios surface parser. `wasm::Module` supports a text round-trip: a parsed module can be printed and re-parsed to an identical result, which is verified by the round-trip test in `src/wasm/module.rs`.

---

## 7. Execution

**Files:** `src/lib.rs`, `src/print.rs`

`src/lib.rs` exposes the public `run(timeout: Duration, source: &str) -> Result<String, String>` function that drives the full pipeline: parse → elaborate → infer → erase → CPS lower → WASM codegen → serialize → load into Wasmtime → call `func/main` → print result.

Wasmtime is configured with:

- Reference types (anyref)
- Function references (typed funcref)
- GC (struct/array types)
- Tail calls

`src/print.rs` contains the result printer. `RefIds` tracks already-seen GC references by raw identity (cycle detection), and `print_ref` recursively formats `i31ref`, struct, and array values.

---

## 8. CLI

**File:** `src/main.rs`

The binary entry point is a thin Clap wrapper around `run`. It accepts:

- `--timeout <MILLIS>` — deadline for type-checking reduction (default: 1000 ms)
- `<path>` — path to a Curios source file

It reads the file, calls `curios::run(cli.timeout, &source)`, and prints the result or a formatted error message.

---

## 9. Testing

Tests exist at each layer:

- **Term operations:** scope open/close symmetry, capture/release substitution
- **Parsing:** round-trip tests for rec groups, atoms, tuples, function types, primitives, and split/case syntax
- **Reduction:** beta-reduction, let inlining, natural elimination, arrays, binaries, and timeout enforcement
- **Type checking / erasure:** dependent tuples over atom cases, Nat elimination, recursive definitions, primitive operand validation, arrays, and binaries
- **CPS lowering:** recursive tuples, tail application, arrays/binaries, and join block creation
- **WASM codegen + execution:** primitives, arrays, binaries, tuples, recursive closures, data segments, and end-to-end execution through Wasmtime
- **`tests/triangular_sum.rs`:** standalone Wasmtime test that verifies `Nat.match` computes the triangular sum `sum(5) = 10` end-to-end

---

## 10. Utility Layer

**`src/monads/`:** Provides both the parser combinator library (`Parser<'a, A>`) and a printer combinator library used for pretty-printing terms. The printer uses `Printer` combinators that mirror the parser structure, giving a degree of symmetry between parsing and printing.

**`src/print.rs`:** Wasmtime result printer. `RefIds` maps raw GC reference identities to display indices for cycle detection; `print_ref` recursively formats `i31ref`, struct, and array references returned from a Wasmtime `Store`.

**`src/macros.rs`:** Helper macros used across the codebase, including the `name!` macro that generates the newtype name wrappers (`ValueName`, `BlockName`, `ClsrName`, `FuncName`, etc.) used throughout the CPS and WASM layers.

---

## Start Here

For anyone wanting to understand this project:

1. **Browse `examples/`** — the example files show the language in action at each compilation stage. They are the fastest way to see what Curios programs look like and how the pipeline behaves. Two naming conventions:
   - `inline_*` examples build terms directly in Rust (e.g. `inline_core.rs`, `inline_cont.rs`, `inline_wasm.rs`, `inline_core_ersd.rs`, `inline_ersd_arr.rs`, `inline_cont_to_wasm.rs`, `inline_binary_search.rs`)
   - `parse_*` examples parse Curios source text (e.g. `parse_execute.rs`, `parse_fibonacci.rs`, `parse_even_odd.rs`, `parse_binary_tree.rs`, `parse_recursive_sum_type.rs`, `parse_triple.rs`, `parse_core_arr.rs`, `parse_core_to_wasm.rs`, `parse_nat_match.rs`)

   For a full pipeline execution test that asserts on the result, see `tests/end_to_end.rs`.

2. **Read `src/text/term.rs`** — the `text::Term` enum is the surface AST. Its variants mirror the language syntax directly, with all variables as plain string labels. This is where to learn the surface grammar in type form.

3. **Read `src/text/parse.rs`** — see what the surface syntax looks like. The test cases at the bottom are concrete examples.

4. **Read `src/text/to_core.rs`** and **`src/text/to_core/elaborate.rs`** — see how `text::Entrypoint` becomes `core::Term`. `to_core.rs` handles module processing and name qualification; `elaborate.rs` handles term-level structural translation. Understanding this translation is key to understanding what de Bruijn scoping means in practice.

5. **Read `src/core/term.rs`** — the `core::Term` enum is the central data structure for type checking. Understanding the `Scope<A: Arity>` wrapper (especially `FuncType`, `TupleType`, `AtomType`) is prerequisite to everything downstream.

6. **Read `src/core/typing.rs`** — follow how each `Term` variant gets its type. Notice the bidirectional flow and where reduction is invoked.

7. **Read `src/ersd/term.rs`** — see exactly what disappears and what survives into runtime.

8. **Read `src/cont/module.rs`** — the CPS IR types. Understand `Region`, `Value`, `Tail`, and especially how `Call` specifies a `resume` block.

9. **Read `src/ersd/to_cont/lowerer.rs`** — follow how `ersd::Term` becomes CPS. The `lower_tail` and `lower_value` distinction (tail position vs. value position) is the key insight.

10. **Read `src/cont/to_wasm/expr_emitter.rs`** and **`src/cont/to_wasm/module_emitter.rs`** — see how CPS maps to WASM instructions.

11. **Read `src/lib.rs`** — the top-level `run()` function that ties the entire pipeline together. `src/print.rs` shows how Wasmtime results are formatted. The integration test in `tests/end_to_end.rs` shows the same path running through Wasmtime and asserting on the result.
