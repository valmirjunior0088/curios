# Purity through an opaque Io monad

Working implementation specification for confining every host effect to an opaque primitive `Io` type, hard-deprecating untracked effects, and deleting `curios-cert`'s purity analysis in favor of a typing invariant.

This supersedes the algebraic-effects specification. That design — user-declared signatures, a transparent free-monad carrier, clause-based handlers — was implemented end to end on the `effects-system` branch and reverted whole by its author's judgment. This design keeps the one conclusion that survived the experiment: the pivot that pays is retyping the host surface so effects become values, and everything above that pivot (signatures, handlers, a host-side interpreter) was weight. What the old specification proved and refused is preserved here in [Non-goals](#non-goals) so its analysis outlives the revert.

Durable user-facing semantics belong in `SYNTAX.md`, the library contract in `curios-prelude`'s module documentation, and the cross-cutting rationale in `DESIGN.md` and `PERIMETER.md` once the stages land.

## Objective

Curios today enforces effect soundness with an analysis: `curios-cert/src/purity.rs` walks a scrutinee, every definition it reaches, and the head of every application it makes, deciding whether its spelling fixes a value. The walk is shared by both checkers, memoized through an `effect_memo` seam on `Judge`, `Kernel`, and the elaborator's `Context`, and measured at a third of the fixed prelude's build when cold. It is also structurally incomplete in one direction it documents itself: `f(b)` for a binder `f` must be assumed effectful, because the function space admits `Cell/get` and no property of `(Bool) -> Bool` distinguishes an effectful inhabitant from a pure one.

The objective is to make that sentence false. Every host operation returns `Io(T)`, an opaque primitive type with no eliminator; a closure that performs an effect can only have an `Io`-returning type; therefore every term of non-`Io` type is pure by typing, and the analysis is deleted rather than improved. Untracked effects are hard-deprecated: the entrypoint tail is always an `Io(T)`, which the emitted boundary forces once.

This is a language-identity change — from direct-style ambient effects to effects as inert first-class descriptions — taken deliberately minimally: three primitives, no user-declarable effects, no handlers, and a backend that does not change at all.

## What is already true

Structural facts of this tree that the design leans on, each verified by reading the named source.

| Fact | Where |
| --- | --- |
| Operation primitives reach source only through `/sys` bodies; the parser produces literal variants only, so the surface retype is confined to the generated prelude | `curios-text/src/prim.rs` (enum doc), `curios-text/src/prelude.rs` |
| All 23 builtin host rows flow through the single `Foreign` variant and one `host_fn` output shape, so their retype is one site | `curios-abi/src/host/ops.rs`, `curios-text/src/prelude.rs` (`host_fn`) |
| A higher-order primitive with two type parameters already exists and is typed procedurally in a few lines per checker — `LstMap : (@A, @B, Lst(A), (A) -> B) -> Lst(B)` | `curios-elab/src/elaborate/prim.rs`, `curios-cert/src/kernel/infer/prim.rs` |
| A prim-bodied unary type former already keys a `Monad` witness, and postfix `!` desugars to `/syn/Monad/bind` in lowering | `curios-prelude/std/Lst.crs` (`satisfy Monad(Lst)`), `curios-text/src/term.rs` (`Bang`) |
| The runtime invokes `func/main` and discards its result; the browser harness likewise; nothing downstream reads the tail's value | `curios-runtime/src/engine.rs`, `curios-web/src/harness.js` |
| Integration tests assert on `MockHost` captured output, and the dominant program tail is `/std/print(…)` — retyping `print` makes those tails conform without edits | `curios/src/tests.rs` (`run`), e.g. `curios/src/tests/numeric.rs` |
| `clock_wall`, `clock_mono`, and `args` are 0-arity *functions* only because a value binding would force-reduce an effectful body at definition time — a workaround `Io` obsoletes | `curios-text/src/prelude.rs` (`host_operations` comment) |
| `exit`'s unit result is a settled soundness decision: a non-returning term is unsound exactly when it inhabits a type nothing total inhabits, and `{}` has nothing to forge | `curios-core/src/prim.rs` (`Prim::Exit`) |

## Architecture

### Three primitives

```text
IoType(T)            Io(T) : Type            — the opaque carrier's type former
IoPure(T, v)         pure  : (@T: Type, x: T) -> Io(T)
IoBind(A, B, m, f)   bind  : (@A: Type, @B: Type, m: Io(A), f: (A) -> Io(B)) -> Io(B)
```

Like every primitive, these are saturated nodes — operands are enum fields, never an application spine — and their typing rules are procedural per-variant arms in both checkers. No primitive's Π-type is ever represented as a core term; the polymorphic signatures above are spelled once, as ordinary surface syntax in the generated `/sys/Io` bindings. `IoBind`'s arm is `LstMap`'s with the carrier positions wrapped in `Io`: check `A` and `B` are sorts, check `m` against `Io(A)`, construct `(x: A) -> Io(B)` with a fresh binder and check `f` against it, return `Io(B)`. The universe policy is inherited from `LstType`/`LstMap` unchanged.

None of the three reduce. An `Io` value is an inert description: conversion compares congruently, and there are deliberately no monad laws in definitional equality — `bind(pure(x), f)` is not definitionally `f(x)`, which costs programs nothing since nothing can be proven about an `Io` anyway. `Io` is opaque end to end: no `PrimHead` entry, no match cases, no projection, and above all **no eliminator from `Io(T)` to `T`**. `bind` is non-dependent — `f : (A) -> Io(B)` with `B` fixed — matching the `/syn/Monad` field it satisfies and avoiding a motive-carrying primitive nothing needs.

### The `/sys` surface

`/sys/Io` follows the existing unary-former pattern (`Lst`, `Cell`) verbatim: the module holds the type binding first, then `pure` and `bind`, and `pub use Io/{let Io}` hoists the type so `/sys/Io` names the monad itself. The module holds nothing else.

The placement law: **`Io` owns the monad, never the operations.** An operation belongs with its subject; `Io` is the type its result wears. The `/sys` root remains the flat 1:1 mirror of the `ForeignStore` — row order, import names, binding labels — and taxonomy stays `/std`'s job.

The retype is uniform — anything crossing the wire returns `Io`:

- Every store-described row wraps its result shape in `Io(…)` at the single `host_fn` site: `read : (h: Handle, n: Nat) -> Io({status: Nat, bytes: Bytes})`, and so on for all 23 rows. Record shapes stay inside the wrapper.
- `exit : (n: Nat) -> Io({})`, the unit-result rationale carried over unchanged.
- `Cell/new : (@T, T) -> Io(Cell(T))`, `Cell/set : (@T, Cell(T), T) -> Io({})`, `Cell/get : (@T, Cell(T)) -> Io(T)`.
- The 0-arity operations become constants — `clock_wall : Io({secs_hi: Nat, secs_lo: Nat, nanos: Nat})`, `clock_mono : Io({secs: Nat, nanos: Nat})`, `args : Io(Lst(Bytes))` — because an `Io` primitive is a value and the definition-time force-reduction workaround retires with the guard that motivated it.

`curios-abi`'s wire contract does not move: only the guest-facing types change.

### The entrypoint contract

The tail term of every program is an `Io(T)`, unconditionally — elaboration checks it against `Io(?T)` instead of the current optional-annotation path, so an untracked-effect program is a type error rather than a policy violation. Emission is uniform: `func/main` evaluates the tail to its erased description and forces it once, returning the inner `T`. The runtime keeps calling `func/main` and mapping `ExitTrap` exactly as today; the JavaScript harness, the bundle format, and the ABI are untouched. The interpretation of the monad is that single boundary force, and the compiler plants it.

### Erasure: descriptions are thunks

`Io(T)` erases to a zero-argument closure. `IoPure(v)` becomes a closure returning `v`; `IoBind(m, f)` becomes a closure that forces `m`, applies `f`, and forces the result; each effectful primitive wraps its existing lowering — `Terminator::Exit`, `Rhs::Foreign`, `Rhs::Cell` — inside a closure instead of emitting it inline. Host operations remain direct Wasm imports inside those thunk bodies.

This is the choice that makes an `Io` value a genuine description: bound once and forced twice, it performs twice; substituting a definition for its name never changes behavior. And it is the choice that leaves `curios-ersd`, `curios-cont`, `curios-wasm`, the runtime, and the browser product entirely unchanged — by `ersd` there is no `Io`, only closures the existing optimizer already handles. The backend's own effect discipline (`ersd`'s effect-aware dead-binding, `cont`'s `EmissionTail::Host` boundary) is unchanged and still load-bearing: thunk bodies contain real host calls that must not be duplicated or dropped, and that is the operational layer's question, not the type theory's.

### `purity.rs` is deleted, not replaced

The invariant that replaces the analysis: **no term of non-`Io` type performs an effect.** It rests on three construction facts, each enforced independently by both checkers' per-variant arms — a signature table, not a search:

1. Every effectful primitive's result type is `Io(…)`.
2. The only consumer of `Io(A)` is `IoBind`, which returns `Io(B)` — nothing lowers `Io` to its content.
3. `Io` is opaque: no cases, no projections, no `PrimHead` entry.

Both of the walk's questions become vacuous. A host operation is an inert description denoting one value, and the scrutinee that motivated the module — `match Cell/get(c)` — is ill-typed before refinement is considered. The binder case, the one the walk could never close, is closed by the arrow discipline itself: no inhabitant of `(Bool) -> Bool` effects, so `f(true)` is pure for every possible caller binding, and the refinement a pure opaque head licenses — which `fixes_no_value` had to withhold — is restored. The unsolved-metavariable concession closes the same way: a metavariable of non-`Io` type can only be solved by a pure term, and the `PERIMETER.md` row recording that gap retires.

Deleted with the file: `purity/tests.rs`; the `effect_memo` seams on `Judge`, `Kernel`, and `Context`; the `fixes_no_value` gates in `curios-elab/src/typing.rs` and `curios-cert/src/kernel/infer/eliminate.rs` (the surrounding refinement machinery stays); `ReduceError::EffectAtTypeLevel` and its elaborator rendering, since a description at the type level is a value, not an error. The perimeter derivations (`an_effect_behind_a_*`) are restated as type errors — the demotion they earn when the unsoundness they pin becomes unrepresentable.

What guards the invariant afterwards is a one-line review obligation on any future primitive: if it effects, it returns `Io`.

### The `/std` layer

`/std/Io.crs` is the one new module: the facade re-export of `/sys/Io`, the `satisfy Monad(Io)` witness — a direct delegation, since the primitive signatures match the concept's field order — and whatever derived conveniences implementation decides to want (`map` and `then` being the obvious candidates). The witness is what makes postfix `!` sequence `Io` with no further machinery.

No operation moves. `Handle/read` stays in `Handle`, retyped to `(Handle, Nat) -> Io(Read)` with a body that binds `/sys/read(h, n)!` and keeps its pure status decode; `write`'s resend-`rec` works because `!` hoists inside `rec` bodies. `Cell.crs` stays a re-export. `proc`, `rand`, `time`, `File`, `tcp/*`, `http` keep their homes and gain `Io` in their outputs; `/std/print` becomes `(Str) -> Io({})`, which is what makes most test tails conform automatically. `Async` is the risk concentration: its scheduler drives cell-mutation loops that all become `Io` chains, and it converts last, after the new typing has a corpus of simpler consumers.

## Constraints

- **No eliminator from `Io(T)` to `T` may ever be added.** Every hole `purity.rs` guarded reopens through one. This is the whole soundness story, and it must be stated where a future primitive author will read it.
- No monad laws in conversion; descriptions compare congruently.
- `exit` keeps its unit result inside the wrapper. The `Io(A)`-polymorphic bottom is arguably sound now — there is no eliminator to extract the `A` — but it reopens a settled decision for no consumer, and is not taken.
- The prelude archive builds as one corpus, so the `/sys` retype and the whole `/std` retype are one atomic change. There is no incremental path through a half-retyped prelude.
- Recursive lowering and thunk interpretation must work on the default test-thread stack, per the repository invariant.

## Staging

### Stage 1 — the `Io` vocabulary

The three primitives through `curios-text`, `curios-core`, `curios-elab`, `curios-cert`, both printers, and erasure; the `/sys/Io` module; `/std/Io.crs` with the `Monad` witness, registered in `std.crs`. Nothing is retyped: `Io` values are constructible and sequenceable via `!`, and no existing program changes behavior. Independently landable, and erasure lands here so `Io` programs run end to end.

### Gate — the fusion benchmark

Before Stage 2: `Handle/write` in a loop, current tree against a prototype retype. Each host operation gains a closure allocation plus an indirect call; `ersd` inlining and contification are expected to collapse statically-known bind chains, but that is a claim to measure, not assume. Prelude build time is watched here too, since the retype churns the archive.

### Stage 2 — the flip

One campaign, inherently atomic per the constraint above, and deliberately breaking: every existing program's tail becomes an `Io`.

- Retype the `/sys` surface: `host_fn`'s wrapper, `exit`, the `Cell` operations, the 0-arity constants.
- Retype all of `/std`: `Cell` first as the smallest seam probe, then `Handle`, `File`, `proc`, `rand`, `time`, `tcp/*`, `http`, `print`; `Async` last.
- The entrypoint contract: the tail checks against `Io(?T)`; emission forces the tail's description in `func/main`.
- Delete `purity.rs` and every seam listed above; restate the perimeter derivations as type errors.
- Migrate `programs/`, the benchmarks, and the integration corpus: `!` at effect sites, `Io/pure` on bare-value tails, `with_type` sites spelling `Io(…)`.

### Stage 3 — record

`SYNTAX.md` states the program contract and the `Io` vocabulary; `DESIGN.md` records the referential-transparency decision, the thunk semantics, the placement law, and the no-eliminator invariant; `PERIMETER.md` retires the purity rows, naming the typing invariant that replaced them; `ROADMAP.md` is checked.

## Acceptance tests

**Stage 1.** `pure`/`bind` construct and sequence an `Io` program; `!` over `Io` produces the same term as the explicit `bind` chain; a second `Monad(Io)` witness is refused as a duplicate; matching an `Io` scrutinee is refused; the three primitives print and round-trip through the surface printer; an `Io` value bound once and forced twice performs twice — the description semantics pinned, not merely tolerated.

**Stage 2.** A non-`Io` tail is refused; an effectful program runs through the forced tail and its `MockHost` output is unchanged from before the migration; a closure performing an effect cannot inhabit `(Bool) -> Bool` — the restatement of `an_effect_behind_a_function_parameter_does_not_refine` as a type error; a binder-headed application scrutinee of pure type refines its arm — the refinement `fixes_no_value` withheld, now licensed; `Cell/get` in scrutinee position is ill-typed; the kernel re-checks the retyped prelude with zero disagreements.

## Non-goals

- **Algebraic effects, signatures, handlers, or a host-side step interpreter.** Implemented on the `effects-system` branch and reverted whole. The reusable finding: reifying continuations and interpreting descriptions host-side multiplies allocation and boundary crossings for a generality the standard library never cashed in.
- **Effect rows, effect annotations on function types, or any effect information in types beyond the `Io` wrapper.** Rows need a row sort in `curios-core`, row equality re-derived in the kernel, and a second constraint domain in the elaborator comparable to `universe_solver.rs`, inside a conversion checker that already needs a step budget. Precondition for reopening, carried over from the superseded specification: a written program this design accepts whose author wanted it rejected. None was found then and none has appeared.
- **`unsafe_perform` or any `Io(T) -> T` eliminator**, per the constraint above.
- **Monad laws in definitional equality.**
- **A dependent `bind`.** If a motive-carrying sequencing ever finds a consumer, it is a new decision, not a forced generalization.
- **Moving operations into `/sys/Io`, or regrouping the flat `/sys` root by subject.** The root is the store mirror; `/std` owns taxonomy.
- **Matching on `Io`, a `PrimHead` entry, or any elimination form.**

## Implementation map

- `curios-text/src/prim.rs`, `prelude.rs`, `into_core/lowerer.rs`, `print.rs` — the three variants, the `/sys/Io` module, the retype, the 0-arity constants.
- `curios-core/src/prim.rs`, `print.rs`, `reduce/prim.rs` — the three variants through `for_each_operand`/`traverse`, printing, normal-form treatment, `EffectAtTypeLevel` deletion.
- `curios-elab/src/elaborate/prim.rs`, `convert/prim.rs`, `zonk.rs`, `concept.rs`, `into_ersd/prim.rs`, `typing.rs`, `error.rs` — typing arms and the five retyped results, congruence, the `HeadKey` arm, thunk erasure, guard deletion.
- `curios-cert/src/kernel/infer/prim.rs`, `kernel/convert/prim.rs`, `positivity.rs`, `kernel/infer/eliminate.rs`; `purity.rs` and `purity/tests.rs` deleted with the `effect_memo` seams in `judge.rs`, `kernel.rs`, and `kernel/globals.rs`.
- `curios-elab/src/context.rs`, `context/caches.rs`, `reduce.rs` — the elaborator's memo seam.
- `curios-pipeline/src/compile.rs` — the unconditional `Io(?T)` tail expectation.
- `curios-prelude/std/Io.crs`, `std.crs`, and the whole `/std` retype.
- `curios/src/tests/` and `programs/` — migration and perimeter restatement.
- `documentation/SYNTAX.md`, `DESIGN.md`, `PERIMETER.md`, `ROADMAP.md`.
- Untouched: `curios-abi`, `curios-ersd`, `curios-cont`, `curios-wasm`, `curios-runtime`, `curios-web`, `curios-binaryen`.

## Verification

The ordinary gate, per stage:

```sh
make curios/runtime
cargo fmt --all -- --check
cargo check --workspace --all-targets --all-features
RUSTFLAGS="-Dwarnings" cargo clippy --workspace --all-targets --all-features
cargo test --workspace --all-targets --all-features
```

Stage 2 additionally requires the fusion benchmark's verdict recorded, and `make curios/web` with the version-matched `wasm-bindgen-cli`, since the retyped prelude and pipeline are `curios-web` dependencies even though the harness contract does not change.

## Retirement criteria

The file is deleted when Stage 3 lands.

- The `Io` vocabulary and thunk semantics are recorded in `curios-prelude`'s module documentation and the core primitives' rustdoc.
- The no-eliminator invariant and the placement law are recorded in `DESIGN.md`, where a future primitive author will find them.
- `purity.rs`'s deletion is recorded in `PERIMETER.md` as retired rows naming the typing invariant that replaced them.
- The rows and handlers deferral rationale survives in `DESIGN.md`, so the superseded specification's analysis outlives both files.
- Roadmap entries are checked, and no reference to this filename remains.
