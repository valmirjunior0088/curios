# Curios — Architecture

Curios is a from-scratch compiler for a dependently-typed functional language targeting WebAssembly, implemented in Rust with required numeric support from `num-bigint` and `num-traits`, plus optional CLI/runtime dependencies (`clap`, `wasmtime`). It implements its own type checker, CPS lowering, WASM binary serializer, and parser combinator library.

- [Pipeline](#pipeline)
- [Design invariants](#design-invariants)
- [Module layout](#module-layout)
- [Stage 1 — Parsing](#stage-1--parsing-srctext)
- [Stage 2 — Resolution & elaboration](#stage-2--resolution--elaboration-srctextto_core)
- [Stage 3 — Core type system](#stage-3--core-type-system-srccore)
- [Stage 4 — Type erasure](#stage-4--type-erasure-srcersd)
- [Stage 5 — CPS lowering](#stage-5--cps-lowering-srcersdto_cont)
- [Stage 6 — CPS optimization](#stage-6--cps-optimization-srcoptm)
- [Stage 7 — WebAssembly codegen](#stage-7--webassembly-codegen-srccontto_wasm)
- [Stage 8 — Serialization & Binaryen](#stage-8--serialization--binaryen-srcwasm-srcbinaryenrs)
- [Execution](#execution-srcrunrs-and-srcrun)
- [Utility layer](#utility-layer)
- [Error reporting](#error-reporting)
- [CLI](#cli-srcclirs)
- [Testing](#testing)
- [Reading order](#reading-order)

---

## Pipeline

Source text flows through eight stages, each represented by its own module with a clean handoff:

```
Source Text
    │
    ▼  src/text/parse.rs
text::Entrypoint          surface AST; all variables are plain String labels
    │
    ▼  src/text/to_core/
core::Module              de Bruijn AST; names/modules resolved, unions registered as inductives
    │
    ▼  src/core/elaborate.rs + src/core/zonk.rs + src/core/erase.rs
ersd::Module              elaborated, meta-free, type-erased; closures carry explicit capture lists
    │
    ▼  src/ersd/to_cont/
cont::Module              CPS IR; second-class continuations as block labels
    │
    ▼  src/optm/
cont::Module              optimized CPS IR; monomorphized, devirtualized, DCE'd
    │
    ▼  src/cont/to_wasm/
wasm::Module              WebAssembly AST; GC structs, typed funcrefs, tail calls
    │
    ▼  src/wasm/writer.rs
Vec<u8>                   raw WASM binary
    │
    ▼  src/binaryen.rs
Vec<u8>                   Binaryen-optimized WASM binary
    │
    ▼  Wasmtime
result                    printed by src/run.rs
```

The de Bruijn machinery (`Scope`, `Telescope`, `Var`, the `Bound` traversal trait) lives in `src/core/scope.rs` — see [Stage 3](#stage-3--core-type-system-srccore).

Key files by area (not 1:1 with the stages — normalization serves stage 3 throughout):

| Area                    | Key file(s)                                                            |
| ----------------------- | ---------------------------------------------------------------------- |
| Parsing                 | `text/parse.rs`                                                        |
| Resolution + desugaring | `text/to_core.rs`, `text/to_core/`                                     |
| Type checking + erasure | `core/elaborate.rs`, `core/zonk.rs`, `core/erase.rs`, `core/typing.rs` |
| Normalization           | `core/reduce.rs`, `core/convert.rs`, primitive helpers                 |
| CPS lowering            | `ersd/to_cont/lowerer.rs`                                              |
| CPS optimization        | `optm.rs` + `optm/` (one file per pass)                                |
| WASM codegen            | `cont/to_wasm/`                                                        |
| Binary serialization    | `wasm/writer.rs`                                                       |
| WASM optimization       | `binaryen.rs` + `binaryen/sys.rs` (vendored Binaryen in `binaryen/`)   |

---

## Design invariants

Cross-cutting decisions the code depends on but cannot state in any single place:

- **Union `match` reduction is call-by-name.** A selected arm's binders are bound to _projections of the original head term_ (`head.(i + 1)`), never to the reduced payload values — substituting reduced payloads would inline evaluated definition internals into types that flow on to `zonk` (`src/core/reduce.rs`).
- **Only rebuilt terms flow downstream.** Elaboration returns _rebuilt_ terms; implicit insertion saturates applications, so a lowered (pre-insertion) type or body is no longer interchangeable with its rebuilt form and must never leak into later reduction. `rec` groups assume lowered signatures to break the cycle, then upgrade them in place via `Context::reassume` (`src/core/context.rs`).
- **Continuations are second-class.** Block labels scoped to a region, never values — this is what lets CPS map onto WASM structured control flow without reification (see [Stage 5](#stage-5--cps-lowering-srcersdto_cont)).
- **Binaryen runs with an exact feature set.** `src/binaryen.rs` enables exactly the features the pipeline targets and Wasmtime's engine enables — never `BinaryenFeatureAll`, which lets the optimizer emit post-GC proposals (e.g. exact reference types) the runtime rejects. Binaryen's settings are process-global and its optimizer is not thread-safe across modules, so the whole sequence runs under a lock. The vendored tree keeps `third_party/llvm-project` (DWARF support) because the Outlining pass includes LLVM suffix-tree headers unconditionally (`build.rs`).

---

## Module layout

The stages share a common pattern of a small facade module that re-exports the stage's public surface, but the exact internal files differ by stage.

```
src/text.rs          facade; re-exports error, names, loader, nat, bin, prim, term, to_core, prelude, module
src/core.rs          facade; re-exports int, flt, nat, prim, names, term, module, inductive, reduce, context, convert, error, typing, invert, elaborate, erase, zonk
src/ersd.rs          facade; re-exports prim, names, term, module, to_cont
src/cont.rs          facade; re-exports names, module, to_wasm
src/optm.rs          facade; re-exports walk, harvest, and each optimization pass
src/wasm.rs          facade; re-exports names, types, expr, module, writer; exposes parse and print
```

Transformation entry points (`src/text/to_core.rs`, `src/ersd/to_cont.rs`, `src/cont/to_wasm.rs`) declare submodules privately, so callers see only the public transformation function.

Several top-level modules fall outside this pattern:

| Module        | Role                                                                                                                                                       |
| ------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `src/span.rs` | `Source` (text + optional path) and `Span` byte ranges with the `render_snippet` method; the foundation of [error reporting](#error-reporting)             |
| `src/run.rs`  | Public entry points for running a program; the implementation lives in `src/run/{host,engine,compile,lift,lower}.rs`. Gated behind the `run` Cargo feature |
| `src/cli.rs`  | Clap argument parsing and CLI entry point; gated behind the `cli` Cargo feature                                                                            |

The `cli` feature depends on `run`; `default = ["cli"]`. Dev builds activate `run` via a self-referential dev-dependency (`curios = { path = ".", features = ["run"] }`), giving tests access to `run_file` without enabling `cli`.

---

## Stage 1 — Parsing (`src/text/`)

**Key files:** `parse.rs`, `module.rs`, `term.rs`, `prim.rs`

Uses a custom monadic parser combinator library (`src/monads/parser.rs`). `Parser<'a, A>` supports `.or()`, `.and()`, `.flat_map()`, `.map()`, and `lazy` for recursive grammars. `ParserState` tracks the current byte offset and source; on failure `ParserError::format` renders the offending line via `Span::render_snippet` (see [Error reporting](#error-reporting)).

Line comments (`-- text`) are stripped inside `parse_whitespace`, which is called after every terminal token. Comments are discarded at parse time and do not appear in the AST.

Parsing produces a `text::Entrypoint`: a list of `TopItem`s followed by a `tail: Term`. Top-level items are `Let`, `Rec` (mutual recursion), `Union` (sum type sugar), `Mod` (inline or file-backed module), and `Use` (import).

`text::Term` has no de Bruijn indices — all variables are `String` labels. The grammar covers:

- Π-types `(x : A, y : B) -> C`, lambdas `(x, y) => body`, and the `let`/`rec` function shorthand `f(x : A) -> B = body` (desugared in the parser to a Π-type plus lambda)
- Application `f(a, b)`
- Σ-types `{x: A, B, z: C}`, tuples `(a, b)`
- The unified `match x : motive | … end` eliminator covering unions (`| case(payload, ...)`), booleans (`| true`/`| false`), structural `Nat` induction (`| 0`/`| pred + 1, ih`), and sparse `Nat` dispatch (`| n`/`| _`); the motive ladder is `: T`, `: (x) => T`, or — union scrutinees — the index-binding `: (x : Vec(T, k)) => T`
- `e.0`, `e.1` (field access / Σ-elimination)
- Holes `?`, which elaborate to fresh metavariables solved by bidirectional type checking
- Monadic sequencing sugar: `with bind body` plus postfix `!`, desugared before core elaboration by re-elaborating the bind at each bang site
- Primitive literals plus the prelude-backed `/sys` module, which exposes `Nat`, `Int`, `Flt`, `Bin`, `Arr(T)`, `Bln`, and their operations as ordinary paths
- Module system: `mod Label ... end`, `mod Label;` (file-backed), `union Label ... end`, `use Path/{name, ...};`, `use Path/*;`, `pub use ...;`
- Char literals as nat codepoints: `'a'`

`text::Prim` has richer surface forms than later stages: `Nat(Zero | Succ(NatLiteral, Subterm))` where `NatLiteral` is `Number(BigUint) | Char(char)`, and `Bin(BinLiteral)` where `BinLiteral` is `Bytes(Vec<u8>) | String(String)`. Numeric literals desugar in the parser: `0` → `Nat::Zero`; any `n > 0` → `Nat::Succ(n, Zero)`.

---

## Stage 2 — Resolution & elaboration (`src/text/to_core/`)

`text → core` does the name/module resolution and the desugaring that makes terms fully explicit, in a single pass producing a flat `core::Module`.

**Key files:** `to_core.rs`, `to_core/elaborate.rs`, `to_core/context.rs`, `to_core/interface.rs`, `text/loader.rs`

**Discovery** (`to_core.rs`): a single pass (`Resolved::for_entrypoint`) walks the `mod` tree once. Because `mod` declarations only name children, the module graph is a tree — every qualifier is reached exactly once, so the walk needs no visited-set: it loads each file-backed module through the `Loader` into a cache and records its `ModuleInfo` in the same traversal.

**Interface fixed point** (`to_core/interface.rs`): before any body is elaborated, the public export view of every module is computed to a fixed point (`PublicInterface`), resolving `pub use` re-exports (including chains) and rejecting `ExportConflict` / `CyclicReExport`. This separates a module's _interface_ (its exports) from the _lexical_ import effect of `use`, which is applied per-body in source order.

**Module processing** (`to_core.rs`): walks the `TopItem` list, applies `use`/`pub use` scoping, qualifies names under `mod` blocks, and lowers `union` declarations to two parts — a `rec` group of type bindings (each producing a primitive `UnionType` normal form, wrapped in a `Func` over any type parameters and indices) and one constructor function per variant whose body produces a primitive `Variant` normal form. Each union is also recorded in the inductive registry with its parameter telescope, index telescope, and constructor signatures. All generated `let`/`rec` items are flattened, **topologically reordered** (`order_flat_items`, a stable Kahn pass) so each declaration's value dependencies precede it, then folded right-to-left into the tail. A genuine value cycle is left unorderable and surfaces downstream as an unbound name — cross-declaration value recursion is unexpressible by construction.

The `Loader` trait (`src/text/loader.rs`) has two base implementations: `FileLoader` (resolves `Label.crs` relative to a base directory) and `NullLoader` (for inline programs and tests, which have no file-backed modules — any `load` is a `ModuleNotFound`). Because the whole module-info table exists before elaboration, cross-module name references may be cyclic (value-level recursion still needs `rec`).

**Prelude and embedded standard library** (`src/text/prelude.rs`): `prelude(inner)` wraps any base loader in two layers — `SysLoader` serves the built-in `/sys` modules (`Nat`, `Int`, `Flt`, `Bin`, `Arr`, `Bln`, `Io`, …), constructed directly as `text` AST and never parsed; `StdLoader` serves the `/std` standard library, whose sources are real Curios authored alongside the compiler in `std/*.crs` (plus the `std.crs` manifest of `pub mod`/`pub use` declarations) and embedded into the binary with `include_str!`. Both layers also add `sys`/`std` to `Loader::roots`, so `to_core` declares them at the entrypoint root automatically — every program sees `/sys` and `/std` without an explicit import. Anything not under those roots falls through to `inner`.

**Term elaboration** (`to_core/elaborate.rs`): syntactic translation from `text::Term` to `core::Term`. The binding work is calling `Scope::close()` to convert free string labels into de Bruijn indices; a `union` match lowers to a primitive `core::UnionMatch` whose arms carry their binders as scopes. No type-directed work.

`to_core` returns `Result<core::Module, text::Error>`, where `core::Module { items, inductives, metavars, type_, body }` carries flat top-level definitions, the inductive registry (each `union` declaration's parameter telescope, index telescope, and per-constructor signatures, consulted by elaboration and erasure), the metavariable floor, the optional entrypoint type annotation, and the entrypoint body. `src/text/error.rs` enumerates the failure modes — `UnresolvedQualifier`, `ModuleNotFound`, `ChildModuleNotFound`, `PrivateChildModule`, `BindingNotFound`, `PrivateBinding`, plus the conflict/interface modes (`QualifierConflict`, `BindingConflict`, `NotAModule`, `NotABinding`, `NoSuchUseTarget`, `DuplicatePublicDeclaration`, `ExportConflict`, `CyclicReExport`, `ModuleLoadFailed`) — each attachable to a source `Span` via `.at(span)` (see [Error reporting](#error-reporting)).

The three union lowerings are:

- a union **declaration** → a type-constructor function whose body is the primitive `UnionType` normal form, plus a registry entry recording the parameter telescope, the index telescope, and per-constructor signatures (for an indexed union each signature terminates in its _per-case_ `UnionType`, indices stated by that case's target)
- a **constructor function** → a function whose body is the primitive `Variant` normal form
- a union **match** → a primitive `Match` with `Cases::Union` (arm binders typed from the registry telescopes during core elaboration, with static arity checking; an annotated motive's type-pattern rides along for positional validation, and index information flows through refinement and the restricted inverter in `core/invert.rs`)

---

## Stage 3 — Core type system (`src/core/`)

**Key files:** `term.rs`, `elaborate.rs`, `zonk.rs`, `erase.rs`, `typing.rs`, `error.rs`, `reduce.rs`, `convert.rs`, `context.rs`, `scope.rs`

The central `core::Term` enum:

| Variant                        | Role                                                                                                                                                   |
| ------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `Type`                         | The sort (no universe hierarchy)                                                                                                                       |
| `FuncType` / `Func` / `Apply`  | Π-types (as a `Telescope<Term>`), λ-abstraction, application                                                                                           |
| `TupleType` / `Tuple` / `Proj` | Σ-types (as a `Telescope<()>`), construction, field access                                                                                             |
| `Match`                        | The unified eliminator: one scrutinee + motive (`Scope<Many>` — arity 1 except an index-binding union motive), with `Cases::{Bln, Nat, Switch, Union}` |
| `UnionType` / `Variant`        | Nominal (inductive) unions: the type and constructor values                                                                                            |
| `Let` / `Rec`                  | Bindings and mutual recursion                                                                                                                          |
| `Prim`                         | Built-in values and operations                                                                                                                         |
| `Var`                          | Variables (free or bound)                                                                                                                              |

### De Bruijn indices and `Scope<A: Arity, B: Bound>`

The de Bruijn machinery lives in **`src/core/scope.rs`**. `core`'s `Subterm::traverse` (the big structural match, including its primitives) plugs into it by implementing `Bound`. `Scope`, `Telescope`, `Var`, the `Bound` trait, the `Visit` driver, and the `Arity` types (`One`, `Two`, `Many`) all live here.

Variables arrive from elaboration as free labels (`Var::free("x")`). Each binding construct calls `Scope::close(arity, labels, body)` to capture them as de Bruijn indices; `scope.open(terms)` substitutes indices back during reduction.

`Scope<A: Arity, B: Bound = Term>` handles all binder arities and is generic over its body type. The body parameter `B` is what makes the cons-style telescope possible — see below.

| `A`       | Used by                                                                   |
| --------- | ------------------------------------------------------------------------- |
| `One`     | `Let` (tail), `Telescope` links                                           |
| `Two`     | `Cases::Nat` (succ_case — binds `pred` and `ih`)                          |
| `Many(n)` | `Func` (parameters), `Rec` (items and tail), `Match` (motive), union arms |

The `Bound` trait describes types that can sit under a `Scope` — its required method is `traverse(&self, visit: &mut Visit<F>) -> Self`, and it provides `shift`, `capture`, `release`, `free_vars` as default methods on top. `Term`, `()`, and `Telescope<B>` all implement `Bound`. A `Visit<F>` struct threads de Bruijn depth and a per-variable rewrite closure (`F: FnMut(depth, &Var) -> Option<Term>`, returning a replacement or `None`) through the whole tree.

### Telescopes (`Telescope<B: Bound>`)

`FuncType` and `TupleType` are encoded as structural cons-style telescopes rather than a flat `Vec<Scope<Many>>`:

```rust
pub enum Telescope<B: Bound> {
    Done(Box<B>),
    Cons(Subterm, Scope<One, Telescope<B>>),
}
```

Each `Cons` carries one parameter type and a `Scope<One, …>` that binds exactly one variable into the rest. The "i-th entry binds over the previous i" invariant is structural, not conventional — the type system enforces it. `FuncType` wraps `Telescope<Term>` where `Done` carries the output type; `TupleType` wraps `Telescope<()>` since Σ-telescopes have no body.

### Bidirectional elaboration (`elaborate.rs`, `zonk.rs`, `erase.rs`)

`elaborate_module(context, module, mode)` (in `elaborate.rs`) performs bidirectional type checking and returns a rebuilt `core::Module` plus the entrypoint type. `Mode::Infer` synthesizes a type upward; `Mode::Check(expected)` drives a term against a known type. Elaboration is authoritative: it solves omitted lambda domains and surface holes by creating and unifying metavariables, inserts omitted implicit arguments from `@` plicity marks, validates union constructor and match arities against the inductive registry, then re-closes binders in the rebuilt term.

Metavariables are **contextual**: every occurrence carries a spine — a delayed substitution, one term per binder of the frozen birth telescope, identity at birth. The spine is ordinary term content (`traverse` walks it), so `close` captures it and `open` substitutes it, and a solution — stored once, spelled with the birth telescope's names — resolves correctly at every occurrence by rewriting through that occurrence's spine, no matter how many times the surrounding binders were re-closed and reopened under fresh names. Unification (`convert.rs`) solves through the spine's *pattern* entries (distinct variables, inverted), abstracts the candidate's syntactic occurrences of meta-free non-pattern entries to their birth binders (with a round-trip verification guarding the choice), and postpones what it cannot invert. Note the abstraction match is syntactic: candidates arrive reduced while spine entries stay unreduced, so an entry the reducer rewrites does not match and conservatively postpones.

Conversion distinguishes **provably unequal from not-yet-decidable** (`Outcome::{Converts, Mismatch, Blocked}`). The strict boolean `convert` — used wherever a yes/no oracle is needed (solution re-validation, the inverter) — treats `Blocked` as false; the elaboration turnaround `expect` instead **parks** blocked goals on the `Context` (`ParkedGoal`, freezing the local assumptions, definitions, and counterfactual refinements like a `MetaEntry` freezes Γ) and succeeds provisionally. Solving a metavariable wakes the goals watching it; `elaborate_module` drains the store after every item, reporting a survivor as a mismatch at its origin span. This is what lets `sym(Eq/refl())` typecheck: the argument's flex–flex constraints outlive their own `expect` call and resolve once the result type pins the metavariables. Oracles run under `Context::with_oracle`, which suppresses parking and refinements as a package, so provisional success can never leak into a verdict.

The store also parks whole **checking problems**: a checked-only introduction form (tuple, lambda) meeting an expected type whose structure is still an unsolved metavariable leaves a placeholder metavariable in the rebuilt tree and re-checks when the type's metas solve — the placeholder's solution is the rebuilt term, spliced by the ordinary spine machinery. Same-head flex–flex pairs are discharged by an entrywise congruence probe when both spines are meta-free; distinct heads are never intersected — they park and either resolve through later pins or drain as errors. Parked problems drain **per item** — top-level items carry explicit types, so a later item never determines an earlier item's elaboration, and an unresolvable problem is always attributed to its own definition.

`zonk_module(context, module)` (in `zonk.rs`) substitutes every solved metavariable into the elaborated module. Any remaining unsolved metavariable is reported as an inference failure at its source span, so downstream passes receive a meta-free module.

`erase_module(context, module, expected_type)` (in `erase.rs`) checks the meta-free module against the elaborated entrypoint type while producing the `ersd::Module`. For dependent function types, after checking an argument the codomain is reduced with that argument substituted before checking the body.

`typing.rs` holds the shared infrastructure — the `Error` enum, the `expect`/`refine_head` helpers, and the timeout-aware `reduce_with`/`convert_with` wrappers.

### Normalization (`reduce.rs`, `convert.rs`)

`reduce(context, term)` performs full beta-normalization: applies functions, eliminates lets, runs all primitive operations on concrete values. Every call site supplies an `Instant` deadline; if it expires, `Preempted` propagates up and surfaces as a `Error::ReducePreempted`.

`convert(context, this, that)` checks definitional equality by reducing both sides and comparing structurally. Uses a BFS queue with a `HashSet`-based visited set to avoid stack overflow on deeply nested terms.

### Two-level context (`context.rs`)

Maintains separate stacks for **assumptions** (name → type) and **definitions** (name → value). `with_frame(f)` handles nested scopes. A shared `Entropy` counter (see [Utility layer](#utility-layer)) generates fresh names during type checking.

---

## Stage 4 — Type erasure (`src/ersd/`)

**Key files:** `term.rs`, `prim.rs`

Erasure is performed by `core::erase_module` after elaboration and zonking. The output is `ersd::Module`.

| Removed                                                 | Preserved                                             |
| ------------------------------------------------------- | ----------------------------------------------------- |
| `Type`, `FuncType`, `TupleType`, `UnionType`, `BlnType` | `Func`, `Apply`, `Tuple`, `Proj`, `NatMatch`, `Match` |
| Type annotations on binders                             | `Let`, `Rec`, `Prim`, `Bin`, `Arr`, `Name`            |

`Bln(false/true)` erase to `ersd::Prim::Nat` (false → 0, true → 1). `Cases::Bln` erases to `ersd::NatMatch` with the false branch keyed at 0 and the true branch as the default case.

Type-level positions are replaced with `ersd::Term::Erased` (not dropped), so the tree shape is preserved for later phases.

Key differences from `core`:

- No `Scope` — variables are plain `String` labels
- `ersd::Func` carries `captures: Vec<String>` explicitly
- Union constructor tags → numeric indices (`ersd::Atom { index: usize }`); a constructor value lowers to one flat record `(tag, payload...)` and a union match to an `ersd::Match` on the tag
- `ersd::Match` cases are `Vec<Subterm>` indexed by tag order (no label keys)
- `ersd::NatMatch` dispatch cases are stored as `BTreeMap<u32, Subterm>`

---

## Stage 5 — CPS lowering (`src/ersd/to_cont/`)

**Key files:** `lowerer.rs`, `builder.rs`, `conts.rs`, `rec.rs`, `lower_prim.rs`, `frame.rs`, `to_cont.rs`

This is one of the more complex transformations in the pipeline; `lowerer.rs` is its largest single file.

### CPS IR structure

```
Module
  ├── consts: Vec<(ValueName, Data)>
  ├── clsrs:  Vec<(ClsrName, Clsr)>
  └── funcs:  Vec<(FuncName, Func)>
                └── Region
                      ├── preallocs: Vec<(ValueName, Prealloc)>
                      ├── values:    Vec<(ValueName, Value)>
                      ├── blocks:    Vec<(BlockName, Block)>
                      └── tail:      Tail
```

**Values** (`cont/module.rs`) use a three-tier hierarchy:

| Tier               | Variants                                                                                                                           |
| ------------------ | ---------------------------------------------------------------------------------------------------------------------------------- |
| `Pure(Data)`       | `Nat(u32)`, `Int(i32)`, `Flt(f32)`, `Bin(Vec<u8>)`, `Arr(Vec<ValueName>)`, `Tpl(Vec<ValueName>)`, `Clsr(ClsrName, Vec<ValueName>)` |
| `Eval(Code)`       | arithmetic, comparisons, conversions, bitwise/counting ops, `TplGet`, `BinLen`/`BinGet`/etc., `ArrLen`/`ArrGet`/etc.               |
| `Alias(ValueName)` | forward reference within a region                                                                                                  |

**Tails** (terminators):

| Variant                                 | Meaning                                                                                        |
| --------------------------------------- | ---------------------------------------------------------------------------------------------- |
| `Jump(target, params)`                  | unconditional branch to a block                                                                |
| `Match(operand, cases, default)`        | sparse dispatch on a `u32` (tag index or nat)                                                  |
| `Call(Direct/Indirect, params, resume)` | function call; `resume` is the block that receives the return value                            |
| `Host(IoRead/IoWrite, resume)`          | host-provided IO primitive in tail position; the impure boundary that purity analysis stops at |

### Second-class continuations

The defining property of this IR: continuations are **block labels** scoped to their enclosing `Region`, not heap-allocated closures. A `Call` specifies a `resume: BlockName`; when the callee returns, control jumps to that block. Continuations cannot be stored in data structures, passed as arguments, or returned. This maps directly to WASM structured control flow without reification.

### Lowering strategy

`lower_tail(term, frame, resume, ...)` — lowers `term` in tail position (the result goes to `resume`). `lower_to_name(term, frame, ..., cont)` — lowers `term` in value position, passing the resulting `ValueName` to a continuation.

When a call appears in value position, the lowerer creates a **join block** that receives the result as a block parameter, normalizing the CFG into SSA-like form.

`Rec` groups support value-level mutual recursion, including through arbitrary calls (e.g. point-free parser combinators that reference one another). The lowerer reserves every binding name up front, then declares a **prealloc** — an empty shell with a stable heap identity, recorded in `Region::preallocs` — for each aggregate binding (`Func`/`Tuple`/`Arr`), so its identity exists before its fields are known. Call- and match-valued bindings are lowered in dependency order through resume blocks, and the shells are filled once the values they capture exist — possibly in a descendant region (the _cross-region_ case), so a closure produced by a runtime call can still join the recursive knot. Two call-valued bindings that each need the other's _value_ form a cycle that would require a runtime fixpoint cell, and are rejected. The fill is an emission-time detail (`struct.set`/`array.set` over the prealloc'd shell); no first-class mutation op enters the IR.

### Frame and entropy

`Frame` is a `HashMap<String, ValueName>` representing the current scope. Fresh names come from the shared `Entropy<T>` counter (`src/entropy.rs` — see [Utility layer](#utility-layer)): `frame.rs` bundles per-function value and block streams as `FrameEntropy`, and the lowerer keeps one module-wide stream for closure names.

---

## Stage 6 — CPS optimization (`src/optm/`)

**Key files:** `optm.rs` (façade + pass pipeline), `walk.rs`, `harvest.rs`, plus one file per pass.

A `cont::Module` → `cont::Module` transform: `optm::optimize` (`src/optm.rs`) runs a fixed sequence of passes over the CPS IR before codegen. Its central goal is **monomorphization and devirtualization** — turning indirect closure dispatch into direct calls where the closure shape is statically known, then cleaning up the fallout. Wired into the pipeline at `src/run/compile.rs` between `ersd::to_cont` and `cont::to_wasm`, and surfaced to the observer as `Stage::Optm`.

### Shared infrastructure

- **`walk.rs`** — the traversal engine: a closed walker over the region tree with read-only (`Sink`) and rewriting (`SinkMut`) variants. The single place the structural recursion and the `Code` operand match live, so passes describe _what_ to do per node, not _how_ to recurse.
- **`harvest.rs`** — metadata-harvesting helpers (use counts, references) built on the read-only walker; passes consult these to decide what is safe to rewrite.
- **`scalar_eval.rs`** — wasm-faithful leaf semantics for `Code` operations against a literal environment (arithmetic, bitwise, conversions, aggregate builders, and the value-dependent trap conditions). Consumed by both `constant_folding` and `evaluate_pure_calls`, so compile-time folding and compile-time interpretation share the same trap and host-boundary set.

### Passes (in pipeline order)

| Pass                       | File                           | Effect                                                                                                                                                                                                                                                                                                                        |
| -------------------------- | ------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `propagate_copies`         | `copy_propagation.rs`          | Eliminates `let x = y` renames (`Alias` values)                                                                                                                                                                                                                                                                               |
| `fold_constants`           | `constant_folding.rs`          | Evaluates primitive ops on literal operands                                                                                                                                                                                                                                                                                   |
| `lift_closures`            | `closure_lifting.rs`           | Turns known closures into functions and devirtualizes their call sites                                                                                                                                                                                                                                                        |
| `specialize_calls`         | `specialize_calls.rs`          | Clones a function per closure shape passed into a candidate parameter, so closure lifting can devirtualize through it (monomorphization)                                                                                                                                                                                      |
| `evaluate_pure_calls`      | `evaluate_pure_calls.rs`       | CPS-level partial evaluator: interprets pure-callee `Direct`/`Indirect` calls with all-literal arguments at compile time, replacing the call with the materialised result plus a `Jump` to the original resume — reaches recursive callees (e.g. the parser combinator in `crs_printf`) that single-call-site inlining cannot |
| `inline_calls`             | `function_inlining.rs`         | Splices a `Func` body into its `Direct` call sites. Two tiers: **Tier 1** (single call site, any size) and **Tier 2** (multi-site, body size ≤ 8) — the latter dissolves the tiny primitive wrappers (e.g. `Nat.add`, `Bin.concat`) at every site. Direct-call cycles are excluded                                            |
| `thread_jumps`             | `jump_threading.rs`            | Merges single-predecessor blocks into their predecessor                                                                                                                                                                                                                                                                       |
| `thread_known_tags`        | `tag_threading.rs`             | Threads a `Jump` through the `Match` its known-tag argument already decides: when an edge's literal arguments determine the target block's match (a constructor built on one side of a join, eliminated on the other), the edge gets a per-edge clone of the target specialized to the taken arm. Loop blocks are excluded    |
| `hoist_literals`           | `hoist_literals.rs`            | Lifts bytestrings and closed aggregates into shared module consts                                                                                                                                                                                                                                                             |
| `eliminate_dead_arguments` | `dead_argument_elimination.rs` | Drops unused function parameters and closure captures, finishing type erasure                                                                                                                                                                                                                                                 |
| `eliminate_dead_code`      | `dead_code_elimination.rs`     | Drops unused bindings and unreachable functions, closures, and consts                                                                                                                                                                                                                                                         |

`optimize` (`src/optm.rs`) interleaves and repeats these passes in a fixed sequence — the code is the source of truth for the exact order; what matters architecturally is _why_ the orderings hold:

- `lift_closures` runs both **before and after** `specialize_calls`: specialization exposes fresh known-closure shapes to lift.
- An **interim `eliminate_dead_code`** sweeps the specialization residue so the single-call-site rule in `inline_calls` sees accurate counts.
- `inline_calls` runs **twice**: the first round brings literal arguments next to the primitive ops the prelude wraps (then `thread_jumps`/`propagate_copies`/`fold_constants` collapse them); the second picks up residual primitive wrappers via the Tier 2 size-bounded rule.
- `thread_known_tags` follows each inlining round, because inlining is what exposes the constructor-then-eliminate joins — multi-predecessor match blocks folding cannot decide.
- `evaluate_pure_calls` sits between the two inlining rounds and closes the gap inlining cannot: recursive pure callees dissolve by interpretation, not splicing.
- `hoist_literals` waits until every bytestring and closed aggregate has reached its final shape; `eliminate_dead_arguments` and `eliminate_dead_code` run last to reclaim everything dead.

---

## Stage 7 — WebAssembly codegen (`src/cont/to_wasm/`)

**Key files:** `cont/to_wasm.rs`, `cont/to_wasm/table.rs`, `cont/to_wasm/context.rs`, `cont/to_wasm/frame.rs`, `cont/to_wasm/expr_emitter.rs`, `cont/to_wasm/code_emitter.rs`, `cont/to_wasm/module_emitter.rs`

### Value representation

| Curios value | WASM representation                                                       |
| ------------ | ------------------------------------------------------------------------- |
| `Nat`        | `i31ref` (packed i32)                                                     |
| `Int`        | `i31ref` (packed i32)                                                     |
| `Bln`        | `i31ref` (erases to `Nat`; false → 0, true → 1)                           |
| `Flt`        | GC struct with single `f32` field                                         |
| `Tuple(n)`   | GC struct with N `anyref` fields; subtype chain `tpl/1 ← tpl/2 ← tpl/3 …` |
| `Closure`    | GC struct: funcref field + captured values as fields                      |
| `Atom`       | `i31ref` (the union constructor's tag index)                              |
| `Bin`        | GC array of packed `i8`                                                   |
| `Arr`        | GC array of nullable `anyref`                                             |

### Closure calling convention

A closure struct's first implicit member is a typed funcref. Calling a closure: load the funcref, pass the struct itself as the environment parameter plus the actual argument, then `call_ref`. The callee loads captures from the struct via `struct.get`.

### Tail calls

Direct calls use `return_call`; indirect calls use `return_call_ref`. This eliminates stack growth for recursive patterns.

### Codegen submodules

| File                | Responsibility                                                                                                                              |
| ------------------- | ------------------------------------------------------------------------------------------------------------------------------------------- |
| `table.rs`          | Builds symbol tables; pre-allocates GC struct types for closures, tuples, floats                                                            |
| `context.rs`        | Tracks locals, frames, and value classification (`LoadAs` enum) for correct casting                                                         |
| `frame.rs`          | Represents nested WASM blocks; accumulates instructions; manages label-based branching                                                      |
| `code_emitter.rs`   | Emits a single CPS `Code` operation (arithmetic, comparisons, conversions, projections, array/binary ops) with the right boxing (`WrapAs`)  |
| `expr_emitter.rs`   | Emits instructions for CPS values and tails: closure allocation, tuple projection, constants, jumps, matches, calls (drives `code_emitter`) |
| `module_emitter.rs` | Emits the top-level WASM module: type definitions, function bodies, exports, and host imports when the corresponding operations are used    |

The `LoadAs` enum (`Null`, `NonNull`, `Concrete(TypeName)`, `Int`, `Flt`, `Bin`, `Arr`) drives which cast or unboxing sequence the emitter generates for each value.

---

## Stage 8 — Serialization & Binaryen (`src/wasm/`, `src/binaryen.rs`)

### Binary serialization (`src/wasm/writer.rs`)

The compiler writes WASM binary directly — no `wasm-encoder` or similar library. Implements LEB128 (signed and unsigned), IEEE 754 single/double, and all WASM section encodings, with helper modules under `wasm/writer/`.

### Binaryen optimization (`src/binaryen.rs`)

Deliberately the last stage: `binaryen::optimize` consumes and produces serialized module bytes after the writer and knows nothing about any Curios IR — semantic optimization belongs in `optm`. The vendored Binaryen 130 (`binaryen/` at the repo root) is built and statically linked by `build.rs` via CMake, behind the default-on `binaryen` Cargo feature; building without it (`--no-default-features --features cli`) emits unoptimized modules. The module is read with the exact feature set the pipeline targets, optimized closed-world at optimize level 2 / shrink level 1, validated, and re-serialized. See [Design invariants](#design-invariants) for why the feature set is exact and the sequence is serialized under a lock.

### WAT parser (`src/wasm/parse.rs`)

A full WebAssembly Text format parser implemented with the same monadic combinator library as the surface parser. `wasm::Module` supports a text round-trip: parse → print → parse produces an identical result, verified by a round-trip test in `src/wasm/module_tests.rs`.

---

## Execution (`src/run.rs` and `src/run/`)

`src/run.rs` re-exports everything from `src/run/{host,engine,compile,lift,lower}.rs` and defines the top-level entry points. The execution entry points are generic over a host `H: Host + Send + Sync + 'static`:

- `run_text(timeout, source, host)` — inline source with `NullLoader`
- `run_file(timeout, path, host)` — reads a `.crs` file; constructs `FileLoader` rooted at the file's directory
- `run(timeout, source, host)` — inline source with `NullLoader`; `run_text` is a compatibility wrapper around it
- `run_entrypoint(timeout, entrypoint, loader, host)` — shared core: full pipeline → `run_wasm`
- `run_wasm(wasm_module, host)` — executes a `wasm::Module` directly via Wasmtime (`src/run/engine.rs`)
- `compile_entrypoint(timeout, entrypoint, loader, observe)` — runs the full pipeline from text to `wasm::Module` without execution. An explicit expected type is carried by the `text::Entrypoint` when present, otherwise the type is inferred. The `observe: FnMut(Stage<'_>)` callback receives each intermediate representation (`Stage::Text`/`Core`/`Ersd`/`Cont`/`Optm`/`Wasm`) and is what the CLI's `--print` flag drives (`src/run/compile.rs`)
- `typecheck_entrypoint(timeout, entrypoint, loader, observe)` — runs the fast check path (`to_core → elaborate → zonk`) without erasure, lowering, optimization, or codegen. The CLI's `check` subcommand uses this unless `--print` asks for a post-core stage.

The `Host` trait (`src/run/host.rs`) abstracts the IO side of the program:

```rust
pub trait Host {
    fn open(&self, path: &[u8], mode: u32) -> (u32, u32);   // (status, handle)
    fn close(&self, handle: u32);
    fn read(&self, handle: u32, count: u32) -> (u32, Vec<u8>);
    fn write(&self, handle: u32, bytes: &[u8]) -> u32;
}
```

The `handle` is the i32 token a `/sys/Io` value lowers to; `STDIN`/`STDOUT`/`STDERR` constants (0/1/2) name the well-known entries and `open` mints fresh tokens from 3 up. Failable ops report through `STATUS_*` codes (0 ok, 1 eof, 2 not-found, 3 permission, 4 exists, 5 other — mirrored by `/std/File`'s `decode`); `read` blocks until at least one byte is available and returns up to `count` bytes, with eof as status 1 and empty bytes. There is no path sandbox: programs have the invoking user's filesystem access.

The number-to-`Bin` conversions live alongside the trait as free functions — `nat_to_str(u32)`, `int_to_str(i32)`, `flt_to_str(f32)`, `flt_to_le_bin(f32)` — not trait methods. The runtime wasm imports and the compile-time `scalar_eval` folder both call the same free functions, so the two paths cannot diverge.

Two trait implementations ship:

- **`StdioHost`** maps the stdin/stdout/stderr handles onto the real process streams (raw `Read::read` for stdin, so short reads behave POSIX-style) and backs file handles with `std::fs::File`.
- **`ChannelHost`** routes writes (stdout and stderr alike) through an `mpsc::Sender<Vec<u8>>` and serves stdin reads from an `mpsc::Receiver<Vec<u8>>` pre-loaded with input lines: each message is one line, a `\n` is appended on refill, and an internal leftover buffer serves `count`-byte slices so short reads never drop bytes. File handles are backed by an in-memory filesystem. Constructors: `ChannelHost::in_out(lines)` returns the host plus the matching output `Receiver`; `ChannelHost::out()` is the input-empty shorthand; `ChannelHost::with_fs(lines, files)` additionally pre-seeds the in-memory filesystem and returns it for post-run inspection.

Up to eight operations are wired as Wasmtime host imports under `"env"` by `run_wasm`: the four conversions (each emitted only if the codegen `Table` records the corresponding `_used()` flag) route directly to the free functions; `io_open`/`io_close`/`io_read`/`io_write` route to the trait. Scalar *results* of the io imports cross the boundary pre-boxed as i31 refs (so generated code can land them directly in anyref block params); a two-result import's resume block packs `(status, payload)` into the `{ status, … }` record the prim's type promises. The `io_*` imports correspond to the `Tail::Host` variants introduced in [Stage 5](#stage-5--cps-lowering-srcersdto_cont).

Wasmtime is configured with reference types, function references, GC, and tail calls. `run_wasm` returns `Result<(), String>`; all IO is performed via the `/sys/Io` handle operations through the `Host`.

---

## Utility layer

| Module                  | Purpose                                                                                                                                                                                                                                                                                                                                                                                                        |
| ----------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `src/monads/parser.rs`  | Monadic parser combinators: `Parser<'a, A>`, `.or()`, `.and()`, `.flat_map()`, `lazy`, `many0/1`, `sep_by0/1`, `take_while`, etc. Used for both surface syntax and WAT parsing.                                                                                                                                                                                                                                |
| `src/monads/printer.rs` | Mirror of the parser: `Printer<'a>` combinators (`pure`, `flat`, `indent`, `sep_flat`) driven by `run_printer`. Used in all `print.rs` modules.                                                                                                                                                                                                                                                                |
| `src/macros.rs`         | `name!(Foo)` — generates a newtype `pub struct Foo { pub string: String }` with `From<A: Into<String>>`, `Debug`, `Clone`, `PartialEq`, `Eq`, `PartialOrd`, `Ord`, `Hash`. Used for all name types across all stages. The `name!(Foo, "prefix")` form additionally implements `Mint`, so the name can be generated by `Entropy`.                                                                               |
| `src/entropy.rs`        | `Entropy<T>` — the shared gensym source: a `Cell`-backed monotonic counter whose `fresh()` mints a `T: Mint` (raw `usize` ids by default). Every fresh-name need in the pipeline draws from one: metavariables and binder labels in `text/to_core`, fresh names in the core `Context`, value/block/closure names in CPS lowering, synthesized bindings in `optm/evaluate_pure_calls`, and WASM codegen locals. |

---

## Error reporting

Every fallible stage reports through a uniform pattern built on two primitives in `src/span.rs`:

- `Source { path: Option<PathBuf>, text }` — the source text plus an optional file path (`None` for inline/REPL sources). Reference-counted (`Rc`) and shared by every span cut from it.
- `Span { source, start, end }` — a byte range into a specific source; carries its `Rc<Source>`, so the originating file travels with the span even after modules are merged into one core term.
- `Span::render_snippet(&self)` — formats the offending line with a line number and a `^` caret underline, prefixed with a `--> path:line` header when the source has a path.

Each stage owns an `Error` enum (`src/text/error.rs`, `src/core/error.rs`) whose variants carry the specifics of each failure. A `Located { span, error }` wrapper attaches a span to any variant via `.at(span)` (idempotent — re-wrapping an already-located error is a no-op). Calling `.format()` prints the message followed by the rendered snippet; an unlocated error prints the message alone. Because the source rides on the span, type-checking and erasure errors in file-backed modules name their file too. The parser's `ParserError::format` renders a zero-width span at the failure offset. Reduction timeouts surface here too, as `core::Error::ReducePreempted`.

Sources are built at the parse entry points: `FromStr` (`"...".parse()`) builds a pathless `Source`, while `Entrypoint::from_path`/`Module::from_path` read a file and build a path-bearing one (`src/text/parse.rs`, error type `LoadError`). `main` returns `ExitCode`: on `Err` it prints the formatted message to stderr and exits `FAILURE`, otherwise `SUCCESS`.

---

## CLI (`src/cli.rs`)

A Clap wrapper around the run, check, and compile entry points. The usage synopsis lives in `README.md`; top-level options precede a subcommand, and the semantics are:

- `--timeout` sets the type-checker's reduction timeout in milliseconds (default: 1000)
- `--print [STAGES]` prints selected intermediate representations to stderr; `STAGES` is a comma-separated subset of `text,core,ersd,cont,optm,wasm`. Bare `--print` selects all; omitting the flag prints none.
- `run` compiles and executes the entrypoint
- `check` type-checks the entrypoint without executing it, exiting with a non-zero status on failure; if `--print` requests a post-core stage (`ersd`, `cont`, `optm`, or `wasm`), it runs the full lowering pipeline so that stage exists to print
- `compile` emits the compiled WebAssembly module; pass `--output-path PATH` to write the binary to that path, otherwise it writes `<input-stem>.wasm`
- `<input-path>` is the path to an entrypoint file; a Curios source file whose last expression is the program's result

---

## Testing

The library-crate test suite covers every layer:

| Layer            | What is tested                                                                                                                                                                                                                                                                   |
| ---------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Term operations  | `Scope` open/close symmetry, shift, capture, release                                                                                                                                                                                                                             |
| Parsing          | Round-trips: rec groups, unions, tuples, function types, primitives, field access                                                                                                                                                                                                |
| Reduction        | Beta reduction, let inlining, nat elimination, array/binary ops, timeout enforcement                                                                                                                                                                                             |
| Type checking    | Dependent tuples, structural `Nat` induction, recursion, primitive operand validation, arrays, binaries                                                                                                                                                                          |
| Erasure          | Primitive, tuple, array, binary type erasure                                                                                                                                                                                                                                     |
| CPS lowering     | Recursive tuples, tail application, arrays/binaries, join block creation, prealloc'd shells, cross-region mutual recursion, call-cycle rejection                                                                                                                                 |
| CPS optimization | `src/optm/` — per-pass tests: constant folding (the bulk), partial evaluation of pure calls, literal hoisting, call specialization, closure lifting, copy propagation, jump threading, single-site and size-bounded multi-site inlining, dead-argument and dead-code elimination |
| WASM codegen     | Primitives, arrays, binaries, tuples, recursive closures, end-to-end Wasmtime execution                                                                                                                                                                                          |
| Module system    | `src/text/to_core/tests.rs` — qualifier resolution, visibility, `use`/`pub use`, absolute paths, interface fixed point                                                                                                                                                           |
| Integration      | `src/tests.rs` — `triangular_sum` (structural `Nat` induction, `sum(5) = 10`), `multi_arg_function` / `curried_function` (multi-argument and curried calls)                                                                                                                      |
| End-to-end       | `src/tests.rs` — `end_to_end` runs the full pipeline from source text through a Wasmtime output assertion                                                                                                                                                                        |

---

## Reading order

1. **`examples/`** — fastest way to see the language and pipeline in action. Start with `crs_printf.rs` (typed format strings end-to-end, minimal pipeline setup) and `crs_json_codec.rs` (standard-library `Json` encode/decode round-trip, full pipeline with output assertions); `crs_proofs.rs` is the entry point for indexed unions and proofs (`/std/Eq`, `/std/Void`, induction, checked rejections). The `inline_*` examples build terms in Rust directly; `parse_*` examples parse Curios source text.
2. **`src/text/term.rs`** — the surface AST; variants mirror the language syntax with all variables as plain strings.
3. **`src/text/parse.rs`** — the surface grammar; test cases at the bottom are concrete examples.
4. **`src/text/to_core.rs`** + **`src/text/to_core/elaborate.rs`** — how `text::Entrypoint` becomes a flat `core::Module`: how `Scope::close` turns string labels into de Bruijn indices, how `union` lowers to type + constructor bindings, and how the inductive registry records parameters, indices, and constructor signatures for later elaboration and erasure.
5. **`src/core/scope.rs`** + **`src/core/term.rs`** — the de Bruijn machinery (`Scope<A: Arity, B: Bound>`, the `Bound` trait, `Telescope<B>`) and the typed AST built on it; prerequisite for everything downstream.
6. **`src/core/elaborate.rs`** + **`src/core/zonk.rs`** + **`src/core/erase.rs`** — bidirectional elaboration, metavariable substitution, and erasure; note where reduction is invoked, how holes are solved, and how the meta-free module is checked while producing `ersd`. Shared helpers live in `src/core/typing.rs`.
7. **`src/ersd/term.rs`** — what disappears at erasure and what survives into runtime.
8. **`src/cont/module.rs`** — the CPS IR types; pay attention to how `Call` specifies a `resume` block.
9. **`src/ersd/to_cont/lowerer.rs`** — how `ersd::Term` becomes CPS; the `lower_tail` vs `lower_to_name` distinction is the key insight.
10. **`src/optm.rs`** — the optimizer pass pipeline; read `optm/walk.rs` for the shared traversal, then `optm/specialize_calls.rs` + `optm/closure_lifting.rs` for the monomorphization/devirtualization core.
11. **`src/cont/to_wasm/expr_emitter.rs`** + **`src/cont/to_wasm/module_emitter.rs`** — how CPS maps to WASM instructions.
12. **`src/run.rs`** — `run`, `run_text`, `run_file`, `run_entrypoint`, `run_wasm`, `typecheck_entrypoint`, and `compile_entrypoint` (with the `Stage` observer) tie the whole pipeline together; the implementation lives under `src/run/`.
