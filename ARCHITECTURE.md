# Curios — Architecture

Curios is a from-scratch compiler for a dependently-typed functional language targeting WebAssembly, implemented in Rust with two external dependencies (`clap`, `wasmtime`). It implements its own type checker, CPS lowering, WASM binary serializer, and parser combinator library.

**Codebase size:** ~28,400 lines in `src/`, ~1,500 in `examples/`.

---

## Pipeline

Source text flows through six stages, each represented by its own module with a clean handoff:

```
Source Text
    │
    ▼  src/text/parse.rs
text::Entrypoint          surface AST; all variables are plain String labels
    │
    ▼  src/text/to_core/
core::Term                de Bruijn AST; Scope<A: Arity> binders
    │
    ▼  src/core/typing.rs (infer + erase)
ersd::Term                type-erased; closures carry explicit capture lists
    │
    ▼  src/ersd/to_cont/
cont::Module              CPS IR; second-class continuations as block labels
    │
    ▼  src/cont/to_wasm/
wasm::Module              WebAssembly AST; GC structs, typed funcrefs, tail calls
    │
    ▼  src/wasm/writer.rs
Vec<u8>                   raw WASM binary
    │
    ▼  Wasmtime
result                    printed by src/run.rs
```

| Stage                   | Key file(s)                                    | Lines  |
| ----------------------- | ---------------------------------------------- | ------ |
| Parsing                 | `text/parse.rs`                                | 1,406  |
| Elaboration             | `text/to_core.rs`, `text/to_core/elaborate.rs` | ~1,000 |
| Type checking + erasure | `core/typing.rs`                               | 2,539  |
| Normalization           | `core/reduce.rs`, `core/convert.rs`            | ~2,200 |
| CPS lowering            | `ersd/to_cont/lowerer.rs`                      | 3,101  |
| WASM codegen            | `cont/to_wasm/` (5 files)                      | ~3,300 |
| Binary serialization    | `wasm/writer.rs`                               | 2,018  |

---

## Module layout

Every stage (`text`, `core`, `ersd`, `cont`, `wasm`) follows an identical layout:

```
src/<stage>.rs              facade — mod X; pub use X::*;
src/<stage>/names.rs        newtype name wrappers via name! macro
src/<stage>/prim.rs         primitive types and operations enum
src/<stage>/term.rs         central AST enum + supporting structs
src/<stage>/print.rs        Display impl via printer combinators (not re-exported)
src/<stage>/to_<next>.rs    transformation to next stage (may be a folder)
```

Transformation entry points (`to_cont.rs`, `to_wasm.rs`) declare submodules privately — callers see only the public transformation function.

Two top-level modules fall outside this pattern:

| Module          | Role                                                                                      |
| --------------- | ----------------------------------------------------------------------------------------- |
| `src/run.rs`    | Wasmtime execution and result printing; gated behind the `run` Cargo feature              |
| `src/cli.rs`    | Clap argument parsing and CLI entry point; gated behind the `cli` Cargo feature           |

The `cli` feature depends on `run`; `default = ["cli"]`. Dev builds activate `run` via a self-referential dev-dependency (`curios = { path = ".", features = ["run"] }`), giving tests access to `run_file` without enabling `cli`.

---

## Stage 1 — Parsing (`src/text/`)

**Key files:** `parse.rs`, `module.rs`, `term.rs`, `prim.rs`

Uses a custom monadic parser combinator library (`src/monads/parser.rs`). `Parser<'a, A>` supports `.or()`, `.and()`, `.flat_map()`, `.map()`, and `lazy` for recursive grammars. Position-aware error reporting via `ParserState`.

Line comments (`-- text`) are stripped inside `parse_whitespace`, which is called after every terminal token. Comments are discarded at parse time and do not appear in the AST.

Parsing produces a `text::Entrypoint`: a list of `TopItem`s followed by a `tail: Term`. Top-level items are `Let`, `Rec` (mutual recursion), `Mod` (inline or file-backed module), `Use` (import), and `Def` (opaque type block).

`text::Term` has no de Bruijn indices — all variables are `String` labels. The grammar covers:

- Π-types `(x: A) -> B`, lambdas `x => body`
- Σ-types `{x: A, B, z: C}`, tuples `(a, b)`
- Atoms `'[left, right]`, `'left`; pattern matching `match x : k => T; | 'tag => body;`
- `e.0`, `e.1` (field access / Σ-elimination), `Nat.fold` (structural induction), `Nat.match` (sparse dispatch)
- `Nat`, `Int`, `Flt`, `Bin`, `Arr` primitives with all built-in operations
- Module system: `mod Label ... end`, `mod Label;` (file-backed), `use Path/name;`, `pub use ...;`
- Opaque types: `def Label(witness) ... end`
- Char literals as nat codepoints: `'a'`

`text::Prim` has richer surface forms than later stages: `Nat(Number(u32) | Char(char))`, `Bin(Bytes(Vec<u8>) | String(String))`.

---

## Stage 2 — Elaboration (`src/text/to_core/`)

**Key files:** `to_core.rs`, `to_core/elaborate.rs`, `to_core/context.rs`, `text/loader.rs`

Two concerns, handled separately:

**Module processing** (`to_core.rs`): walks `TopItem` list, resolves `use` declarations (enforcing visibility), qualifies names under `mod` blocks (e.g. `Foo/bar`), resolves file-backed `mod Label;` via the `Loader` trait, translates `def` blocks into `core::Sealed` nodes, and folds `let`/`rec` items right-to-left into the tail.

The `Loader` trait has two implementations: `FileLoader` (resolves `Label.crs` relative to a base directory) and `PanicLoader` (used for inline programs and tests).

**Term elaboration** (`to_core/elaborate.rs`): pure syntactic translation from `text::Term` to `core::Term`. The only binding work is calling `Scope::close()` to convert free string labels into de Bruijn indices. No type-directed work — that happens in `core/typing.rs`.

---

## Stage 3 — Core type system (`src/core/`)

**Key files:** `term.rs`, `typing.rs`, `reduce.rs`, `convert.rs`, `context.rs`, `arity.rs`

The central `core::Term` enum:

| Variant                         | Role                                                 |
| ------------------------------- | ---------------------------------------------------- |
| `Type`                          | The sort (no universe hierarchy)                     |
| `FuncType` / `Func` / `Apply`   | Π-types, λ-abstraction, application                  |
| `TupleType` / `Tuple` / `Proj`  | Σ-types (n-ary), construction, field access          |
| `BlnMatch`                      | Dependent elimination of `Bln` (false + true cases)  |
| `NatFold`                       | Structural induction on `Nat` (zero + pred/IH cases) |
| `NatMatch`                      | Sparse dispatch on specific `Nat` values             |
| `AtomType` / `Atom` / `Match`   | Labeled unions, tags, pattern matching               |
| `Let` / `Rec`                   | Bindings and mutual recursion                        |
| `Sealed` / `Seal` / `Unseal`    | Opaque type abstraction from `def`                   |
| `Prim`                          | Built-in values and operations                       |
| `Var`                           | Variables (free or bound)                            |

### De Bruijn indices and `Scope<A: Arity>`

Variables arrive from elaboration as free labels (`Var::free("x")`). Each binding construct calls `Scope::close(arity, labels, body)` to capture them as de Bruijn indices; `scope.open(terms)` substitutes indices back during reduction.

`Scope<A: Arity>` handles all binder arities via a single generic type:

| `A`       | Used by                     |
| --------- | --------------------------- |
| `One`     | `Func`, `FuncType`, `NatFold` (motive), `NatMatch` (motive), `Match` (motive), `BlnMatch` (motive), `Let` (tail), `Sealed` (tail) |
| `Two`     | `NatFold` (succ_case — binds `pred` and `ih`)                                                                                     |
| `Many(n)` | `TupleType` (fields), `Rec` (items and tail)                                                                                      |

A private `Visit<F>` struct drives all variable traversals (`shift`, `capture`, `release`, `free_vars`). The closure `F: FnMut(depth, &Var) -> Option<Term>` can return a replacement or `None` to leave the variable unchanged.

### Bidirectional type checking (`typing.rs`)

`infer(context, term)` synthesizes a type upward. `erase(context, term, expected_type)` checks downward and simultaneously produces the `ersd::Term`. For dependent function types, after checking the argument the codomain is reduced with the argument substituted before checking the body.

`typing.rs` also produces the `ersd::Term` output directly as a side effect of type checking — erasure is not a separate pass.

### Normalization (`reduce.rs`, `convert.rs`)

`reduce(context, term)` performs full beta-normalization: applies functions, eliminates lets, runs all primitive operations on concrete values. Every call site supplies an `Instant` deadline; if it expires, `Preempted` propagates up and surfaces as a `Error::ReducePreempted`.

`convert(context, this, that)` checks definitional equality by reducing both sides and comparing structurally. Uses a BFS queue with a `HashSet`-based visited set to avoid stack overflow on deeply nested terms.

### Two-level context (`context.rs`)

Maintains separate stacks for **assumptions** (name → type) and **definitions** (name → value). `with_frame(f)` handles nested scopes. A monotonically increasing entropy counter generates fresh names during type checking.

### Opaque types

`Sealed` binds a label to a witness type and scopes it over a continuation. Inside the `def` block, `Seal` (`Label.from`) and `Unseal` (`Label.into`) coerce between the opaque type and the witness. All three are transparent at runtime — erasure drops `Sealed` and replaces `Seal`/`Unseal` with a pass-through of the wrapped value.

---

## Stage 4 — Type erasure (`src/ersd/`)

**Key files:** `term.rs`, `prim.rs`

Erasure is performed inside `core::typing.rs` (the `erase` function), not as a standalone pass. The output is `ersd::Term`.

| Removed                                              | Preserved                                                         |
| ---------------------------------------------------- | ----------------------------------------------------------------- |
| `Type`, `FuncType`, `TupleType`, `AtomType`, `BlnType` | `Func`, `Apply`, `Tuple`, `Proj`, `NatFold`, `NatMatch`, `Match`  |
| `Sealed`, `Seal`, `Unseal`                           | `Let`, `Rec`, all control flow                                    |
| Type annotations on binders                          | `Prim`, `Bin`, `Arr`, `Name`                                      |

`Bln(false/true)` erase to `ersd::Prim::Nat` (false → 0, true → 1). `BlnMatch` erases to `ersd::NatMatch` with the false branch keyed at 0 and the true branch as the default case.

Type-level positions are replaced with `ersd::Term::Erased` (not dropped), so the tree shape is preserved for later phases.

Key differences from `core`:

- No `Scope` — variables are plain `String` labels
- `ersd::Func` carries `captures: Vec<String>` explicitly
- Atom labels → numeric indices (`ersd::Atom { index: usize }`)
- `ersd::Match` cases are `Vec<Subterm>` indexed by atom order (no label keys)
- `ersd::NatMatch` cases are `Vec<(u32, Subterm)>` (not a `BTreeMap`)

---

## Stage 5 — CPS lowering (`src/ersd/to_cont/`)

**Key files:** `lowerer.rs`, `frame.rs`, `entropy.rs`, `to_cont.rs`

This is the most complex transformation in the pipeline (3,101 lines).

### CPS IR structure

```
Module
  ├── consts: Vec<(ValueName, Data)>
  ├── clsrs:  Vec<(ClsrName, Clsr)>
  └── funcs:  Vec<(FuncName, Func)>
                └── Region
                      ├── values: Vec<(ValueName, Value)>
                      ├── blocks: Vec<(BlockName, Block)>
                      └── tail:   Tail
```

**Values** (`cont/module.rs`) use a three-tier hierarchy:

| Tier               | Variants                                                                                                                                   |
| ------------------ | ------------------------------------------------------------------------------------------------------------------------------------------ |
| `Pure(Data)`       | `Nat(u32)`, `Int(i32)`, `Flt(f32)`, `Bin(Vec<u8>)`, `Arr(Vec<ValueName>)`, `Tpl(Vec<ValueName>)`, `Clsr(ClsrName, Vec<ValueName>)` |
| `Eval(Code)`       | arithmetic, comparisons, conversions, bitwise/counting ops, `TplGet`, `BinLen`/`BinGet`/etc., `ArrLen`/`ArrGet`/etc.                       |
| `Alias(ValueName)` | forward reference within a region                                                                                                          |

**Tails** (terminators):

| Variant                                 | Meaning                                                             |
| --------------------------------------- | ------------------------------------------------------------------- |
| `Jump(target, params)`                  | unconditional branch to a block                                     |
| `Match(operand, cases, default)`        | sparse dispatch on a `u32` (atom index or nat)                      |
| `Call(Direct/Indirect, params, resume)` | function call; `resume` is the block that receives the return value |

### Second-class continuations

The defining property of this IR: continuations are **block labels** scoped to their enclosing `Region`, not heap-allocated closures. A `Call` specifies a `resume: BlockName`; when the callee returns, control jumps to that block. Continuations cannot be stored in data structures, passed as arguments, or returned. This maps directly to WASM structured control flow without reification.

### Lowering strategy

`lower_tail(term, frame, resume, ...)` — lowers `term` in tail position (the result goes to `resume`).
`lower_value(term, frame, ...)` — lowers `term` in value position (returns a `ValueName`).

When a call appears in value position, the lowerer creates a **join block** that receives the result as a block parameter, normalizing the CFG into SSA-like form.

`Rec` groups pre-reserve value names before any bodies are lowered (via `lower_letrec_bindings`), enabling mutual references within the group.

### Frame and entropy

`Frame` is a `HashMap<String, ValueName>` representing the current scope. `Entropy<T>` is a counter-based stream of fresh names, with separate streams for values, blocks, closures, and functions.

---

## Stage 6 — WebAssembly codegen (`src/cont/to_wasm/`, `src/wasm/`)

**Key files:** `cont/to_wasm.rs`, `cont/to_wasm/table.rs`, `cont/to_wasm/context.rs`, `cont/to_wasm/frame.rs`, `cont/to_wasm/expr_emitter.rs`, `cont/to_wasm/module_emitter.rs`

### Value representation

| Curios value | WASM representation                                                        |
| ------------ | -------------------------------------------------------------------------- |
| `Nat`        | `i31ref` (packed i32)                                                      |
| `Int`        | `i31ref` (packed i32)                                                      |
| `Bln`        | `i31ref` (erases to `Nat`; false → 0, true → 1)                           |
| `Flt`        | GC struct with single `f32` field                                          |
| `Tuple(n)`   | GC struct with N `anyref` fields; subtype chain `tpl/1 ← tpl/2 ← tpl/3 …` |
| `Closure`    | GC struct: funcref field + captured values as fields                       |
| `Atom`       | `i31ref` (the index)                                                       |
| `Bin`        | GC array of packed `i8`                                                    |
| `Arr`        | GC array of nullable `anyref`                                              |

### Closure calling convention

A closure struct's first implicit member is a typed funcref. Calling a closure: load the funcref, pass the struct itself as the environment parameter plus the actual argument, then `call_ref`. The callee loads captures from the struct via `struct.get`.

### Tail calls

Direct calls use `return_call`; indirect calls use `return_call_ref`. This eliminates stack growth for recursive patterns.

### Codegen submodules

| File                | Responsibility                                                                                 |
| ------------------- | ---------------------------------------------------------------------------------------------- |
| `table.rs`          | Builds symbol tables; pre-allocates GC struct types for closures, tuples, floats               |
| `context.rs`        | Tracks locals, frames, and value classification (`LoadAs` enum) for correct casting            |
| `frame.rs`          | Represents nested WASM blocks; accumulates instructions; manages label-based branching         |
| `expr_emitter.rs`   | Emits instructions for CPS values: closure allocation, tuple projection, arithmetic, constants |
| `module_emitter.rs` | Emits the top-level WASM module: type definitions, function bodies, exports, and host imports when the corresponding operations are used |

The `LoadAs` enum (`Null`, `NonNull`, `Concrete(TypeName)`, `Int`, `Flt`, `Bin`, `Arr`) drives which cast or unboxing sequence the emitter generates for each value.

### Binary serialization (`src/wasm/writer.rs`)

The compiler writes WASM binary directly — no `wasm-encoder` or similar library. Implements LEB128 (signed and unsigned), IEEE 754 single/double, and all WASM section encodings. 2,018 lines.

### WAT parser (`src/wasm/parse.rs`)

A full WebAssembly Text format parser implemented with the same monadic combinator library as the surface parser. `wasm::Module` supports a text round-trip: parse → print → parse produces an identical result, verified by a round-trip test in `src/wasm/module.rs`.

---

## Execution (`src/run.rs`)

Four public entry points, all accepting a `provider: P where P: Provider + Send + Sync + 'static`:

- `run_text(timeout, source, provider)` — inline source with `PanicLoader`
- `run_file(timeout, path, provider)` — reads a `.crs` file; constructs `FileLoader` rooted at the file's directory
- `run(timeout, source, loader, provider)` — shared core: full pipeline → `run_wasm`
- `run_wasm(wasm_module, provider)` — executes a `wasm::Module` directly via Wasmtime

The `Provider` trait (`src/run/provider.rs`) abstracts all program IO:

```rust
pub trait Provider {
    fn print(&self, bytes: &[u8]);
    fn read(&self) -> Vec<u8>;
}
```

Two implementations ship: `StdioProvider` writes to stdout and reads a line from stdin; `ChannelProvider` routes `print` output through an `mpsc` channel and serves `read` calls from a pre-loaded `VecDeque`. `ChannelProvider::out()` constructs an output-only instance; `ChannelProvider::io(lines)` pre-loads input lines for full IO simulation in tests.

Five operations are wired as Wasmtime host imports under `"env"`: `nat_to_str`, `int_to_str`, and `flt_to_str` are pure Rust functions that convert primitive values to `Bin`; `sys_print` unpacks the `Bin` argument and calls `provider.print()`; `sys_read` calls `provider.read()` and returns the result as a `Bin`.

Wasmtime is configured with reference types, function references, GC, and tail calls. `run_wasm` returns `Result<(), String>`; all IO is performed via `Sys.print` and `Sys.read` through the `Provider`.

---

## Utility layer

| Module                  | Purpose                                                                                                                                                                                                               |
| ----------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `src/monads/parser.rs`  | Monadic parser combinators: `Parser<'a, A>`, `.or()`, `.and()`, `.flat_map()`, `lazy`, `many0/1`, `sep_by0/1`, `take_while`, etc. Used for both surface syntax and WAT parsing.                                       |
| `src/monads/printer.rs` | Mirror of the parser: `Printer<'a>` combinators (`pure`, `flat`, `indent`, `sep_flat`) driven by `run_printer`. Used in all `print.rs` modules.                                                                       |
| `src/macros.rs`         | `name!(Foo)` — generates a newtype `pub struct Foo { pub string: String }` with `From<A: Into<String>>`, `Debug`, `Clone`, `PartialEq`, `Eq`, `PartialOrd`, `Ord`, `Hash`. Used for all name types across all stages. |

---

## CLI (`src/cli.rs`)

A Clap wrapper that runs the full compilation pipeline with optional flags:

```
curios [--timeout <MILLIS>] [--check] [--print] <path>
```

- `--timeout` sets the type-checker's reduction timeout in milliseconds (default: 1000)
- `--check` runs the full compilation pipeline without executing the result, exiting with a non-zero status on failure (default: off)
- `--print` prints every intermediate representation — core, ersd, cont, and wasm — before executing (default: off)
- `<file>` is the path to an entrypoint file; a Curios source file whose last expression is the program's result

---

## Testing

181 tests across 13 files, covering every layer:

| Layer           | What is tested                                                                          |
| --------------- | --------------------------------------------------------------------------------------- |
| Term operations | `Scope` open/close symmetry, shift, capture, release                                    |
| Parsing         | Round-trips: rec groups, atoms, tuples, function types, primitives, field access         |
| Reduction       | Beta reduction, let inlining, nat elimination, array/binary ops, timeout enforcement    |
| Type checking   | Dependent tuples, `Nat.fold`, recursion, primitive operand validation, arrays, binaries |
| Erasure         | Sealed/Unseal non-recursive, opaque type boundary enforcement                           |
| CPS lowering    | Recursive tuples, tail application, arrays/binaries, join block creation                |
| WASM codegen    | Primitives, arrays, binaries, tuples, recursive closures, end-to-end Wasmtime execution |
| Integration     | `src/tests/triangular_sum.rs` — `Nat.fold` computes `sum(5) = 10` end-to-end            |
| Integration     | `src/tests/anonymous_module.rs` — file-backed `mod Foo;` resolved through `FileLoader`  |
| End-to-end      | `src/tests/end_to_end.rs` — full pipeline from source text through Wasmtime assertion   |

---

## Reading order

1. **`examples/`** — fastest way to see the language and pipeline in action. `inline_*` examples build terms in Rust directly; `parse_*` examples parse Curios source text.
2. **`src/text/term.rs`** — the surface AST; variants mirror the language syntax with all variables as plain strings.
3. **`src/text/parse.rs`** — the surface grammar; test cases at the bottom are concrete examples.
4. **`src/text/to_core.rs`** + **`src/text/to_core/elaborate.rs`** — how `text::Entrypoint` becomes `core::Term`; how `Scope::close` turns string labels into de Bruijn indices.
5. **`src/core/term.rs`** — the typed AST; understanding `Scope<A: Arity>` is prerequisite for everything downstream.
6. **`src/core/typing.rs`** — bidirectional type checking; note where reduction is invoked and how erasure is interleaved.
7. **`src/ersd/term.rs`** — what disappears at erasure and what survives into runtime.
8. **`src/cont/module.rs`** — the CPS IR types; pay attention to how `Call` specifies a `resume` block.
9. **`src/ersd/to_cont/lowerer.rs`** — how `ersd::Term` becomes CPS; the `lower_tail` vs `lower_value` distinction is the key insight.
10. **`src/cont/to_wasm/expr_emitter.rs`** + **`src/cont/to_wasm/module_emitter.rs`** — how CPS maps to WASM instructions.
11. **`src/run.rs`** — `run`, `run_text`, `run_file`, `run_wasm` tie the whole pipeline together.
