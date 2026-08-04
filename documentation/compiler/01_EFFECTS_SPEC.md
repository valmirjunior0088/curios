# Algebraic effects and handlers

Working implementation specification for an effect system that carries user-declared effects and host effects alike, replacing both the ad-hoc `Monad` witnesses and `curios-cert`'s separate purity analysis with one mechanism.

Durable user-facing semantics belong in `SYNTAX.md`, the library's contract in `curios-prelude`'s module documentation, and the cross-cutting rationale — in particular why effect rows are refused and why resumption discipline is a visibility question — in `DESIGN.md` once the feature lands.

## Objective

Curios today has three unrelated notions of "effect":

- `curios-cert`'s `carries_effect`, a reachability walk over host primitives that gates scrutinee refinement in both checkers.
- `/syn/Monad`, a concept with four witnesses, through which the postfix `!` sequences anything monad-shaped.
- `/std/Async`, a hand-written free monad with a hand-written scheduler, which is an effect system in all but name.

The objective is **one**: effect operations are declared as signatures, computations are values of a free-monad carrier, and handlers are interpreters. Host operations are ordinary operations of a privileged signature. `carries_effect` is deleted rather than reimplemented, and `/std`'s two parallel IO stacks become two handlers of one signature.

This is a language-identity change, not a refactor: it moves Curios from direct-style with ambient effects to effects-as-values. It is staged below so each step is independently landable and independently valuable, but the destination is what shapes the design, and a reader should not treat the early stages as the whole feature.

## What is already true

Every row below was run against a compiler built from this tree, not reasoned about. Several are surprising, and each closes a question that would otherwise be relitigated.

| Question | Answer |
| --- | --- |
| May a constructor bind an implicit appearing in no parameter and no index target — a value existential? | **Yes.** `wrap(@n : Nat, Vec(Nat, n))` constructs and eliminates |
| The same at `Type`, which the general `step` needs? | **Yes.** `step(@R : Type, Sig(R), (R) -> Free(A))` elaborates, erases, compiles, runs; the arm binds `R` opaquely and applies the continuation at each operation's own return type |
| May a declaration take a type-former parameter? | **Yes.** `Free(S : (Type) -> Type, A : Type)` is accepted |
| Is that a real positivity accept, or an unchecked admission? | **Real, verified both ways.** `Bad(S) \| node(S(Bad(S)))` and `Mu(F) \| fix(F(Mu(F)))` are *refused*; `Free` is accepted because the recursive occurrence sits in the positive codomain of `(R) -> …` and never under `S` |
| Does a unary carrier get `!` through a `Monad` witness? | **Yes**, end to end |
| Does a two-parameter carrier? | **No.** Both `Monad(Free(S))` and `Monad((A) => Free(S, A))` are refused — see [Dependencies](#dependencies) |
| Can a handler resume a continuation more than once? | **Yes.** A nondeterminism handler invoking `k` per alternative printed the full cross product. No stack capture is involved: the free-monad encoding has already reified the continuation as an ordinary closure |
| Can that be prevented where it is wrong? | **Yes**, by sealing the carrier's representation — verified in both directions below |
| Does a `rec` naming a concrete signature at several positions typecheck? | **No.** Each mention takes a fresh universe instance and they fail to unify. See [Universe instantiation](#universe-instantiation) |

## Architecture

### Signatures

An effect signature is an indexed inductive family whose index is each operation's **return type**:

```crs
pub induct State : (r : Type) -> pub Type
| get() : (Nat)
| put(Nat) : ({})
end
```

The index is what lets one handler serve operations of differing result types while each continuation stays correctly typed. Signatures are ordinary declarations needing no keyword, no registry entry, and no compiler knowledge.

**A signature's representation is public.** That is what lets a consumer match on an operation and write a handler clause — including a test double for host IO, which is the main reason to want effects at all.

### The carrier, and why the continuation is reified

```crs
induct Free(S : (Type) -> Type, A : Type) : Type
| done(A)
| step(@R : Type, S(R), (R) -> Free(S, A))
end
```

The textbook `Free f a = Pure a | Impure (f (Free f a))` places the recursive occurrence *inside* `f`, which is exactly the `Mu` shape positivity refuses — verified above, and correctly, since `Mu` at a negative functor yields a closed inhabitant of `False` in ten lines. Reifying the continuation moves the recursion into the positive codomain of an arrow, where the checker sees it.

This is not a workaround for a checker limitation. It is the decision that makes the whole feature expressible today, and it is also what makes multi-shot resumption free: a reified continuation is a closure, and calling a closure twice needs no stack capture, no `resume` instruction, and no backend support.

### Continuation ownership is the spine

Everything about resumption discipline follows from one fact: **`k` is reachable exactly by whoever can match `step`.** Resumption discipline is therefore a *visibility* question, decided per carrier, not a separate feature needing linear types or a runtime check.

| Carrier representation | Who can match `step` | Discipline |
| --- | --- | --- |
| Public | anyone | multi-shot available |
| Sealed to a subtree | that subtree only | the subtree's exported combinators decide; user code structurally cannot resume at all |

Both directions are verified. With the carrier sealed and the signature public, a consumer outside the declaring module wrote a clause mocking an operation and it ran; the same consumer attempting to match the carrier to reach `k` was refused with *"the representation of type `/Eff/Prog` is private to its declaring module and its descendants"*.

This is stronger than the state of the art. OCaml 5 enforces one-shot *dynamically*, raising `Continuation_already_resumed` at run time. Sealing has no run-time cost and no run-time failure mode.

**The design therefore ships two carriers, and the difference between them is exactly this table.**

### `Free` — the transparent carrier, for user effects

Public representation. Users own their own effects completely: they may write driver loops, store continuations, and resume more than once. Nondeterminism is a legitimate program under this carrier and is verified to work.

Multi-shot is *not* a deferred item and must not be documented as unsupported. It is a property this carrier has, deliberately.

### `Io` — the sealed carrier, for host effects

Representation private to `/std`. User code performs host operations and writes handler clauses, but never obtains a continuation, so replaying a socket read is not expressible rather than merely discouraged.

The exported handler interface is clause-based. The driver owns `k` and applies it at most once:

```crs
pub induct Reply(R : Type, B : Type) : pub Type
| with(R)
| stop(B)
end

pub rec handle(@A : Type, @B : Type, m : Io(A), ret : (A) -> B,
               clause : (@R : Type, Host(R)) -> Reply(R, B)) -> B
```

A clause is affine by construction: it has no continuation to abuse. This is the interface a test double uses.

**Two limits, stated rather than discovered later.** `Reply` cannot express resume-then-post-process, so at least one bracket-shaped combinator is also needed for finalization. And `Reply` cannot express a scheduler, which must *store* `k` and resume it later from elsewhere — `/std/Async` does exactly that. The scheduler therefore uses the raw `step` form from inside the seal, and its one-shot obligation is an audited `/std` invariant rather than a theorem. What sealing buys is that this obligation shrinks from "all code" to "a fixed handful of functions in one module".

### The `Monad` witness, and its endgame

`Free` is itself a monad, so the library declares:

```crs
satisfy (@S : (Type) -> Type) => Monad((A : Type) => Free(S, A))
```

One parametric witness keyed on `Nominal(Free)`, covering every signature — the same discipline the corpus already uses for `Show(Lst(A))` and `Show(Result(A, E))`. This is why [00's item 1.5](00_INFERENCE_AND_UNIFICATION_SPEC.md) must not relax witness uniqueness to admit ground instances: there should never be more than one witness here.

**On replacing `Monad` wholesale.** The concept cannot simply be deleted, because `!` resolves through it and `Free` needs it as much as anything else does. What *can* happen is that its witnesses drain away: `Async` and `Parse` become signatures, host IO becomes `Io`, and `Monad` is left with `Free`, `Option`, and `Lst`.

At that point `Option` and `Lst` are the question, and they are data types whose monads are conveniences — `Option`'s is short-circuiting, `Lst`'s is nondeterminism. Keeping their witnesses costs nothing and keeps `x!` working on an `Option` value. Removing them buys a smaller concept and forces every such site to lift into `Free`.

If they are removed, `Monad` has one witness, is doing no dispatch work, and `!` could be hardcoded to `Free/bind` — genuine wholesale replacement. **That last step is a decision this specification does not take**, because it trades away `DESIGN.md`'s "syntax forms are closed, semantics extend by witness": a hardcoded `!` is precisely the rejected alternative that decision names. Stage 5 below records it as a decision point with its cost, not as planned work.

## Constraints the implementation must respect

### Universe instantiation

A `rec` whose signature names a **concrete** effect signature at several positions does not typecheck. Each mention takes a fresh universe instance and they fail to unify:

```text
the kernel refused /bind: expected `Free(Choice.{x1,w,x}, B)`, found `Free(Choice.{x1,y,z}, B)`
```

The generic form does not have this problem, and a handler naming its signature once does not either — both verified. So the rule is structural rather than incidental:

- `bind`, `map`, `perform`, and every other combinator are **generic over `@S`** and live in `/std/Effect`.
- Only handlers name a concrete signature, and they name it once.
- A consumer must never need to write a signature-specialized `bind`. If the library forces one, the library is wrong.

### Positivity

The carrier is accepted; the `Mu` shape is refused. An implementation that "simplifies" `step` by dropping the reified continuation will be refused by the positivity checker, and correctly so. Signature *coproducts* (`Free(f :+: g)`, the route to handlers polymorphic in remaining effects) are blocked for the same reason and are out of scope — see [Non-goals](#non-goals).

### Totality, and the proof boundary

**A handler classifies `Partial`, and this is a boundary rather than a limitation to route around.**

A deep handler recurses through a continuation it receives as an opaque function value. `curios-cert`'s size-change engine reads arguments against parameters; an opaque function value is unread, the call matrix grades `Matrix::unknown`, and the group is not accepted as descending. That is the fail-closed direction.

The consequence: **no handler, and nothing whose type mentions a handler's result, may appear in a proof or type position.** This is exactly `block_on`'s status today. It must be pinned by a negative test, because it is the one promise here that would rot silently.

## Dependencies

Two items in [`00_INFERENCE_AND_UNIFICATION_SPEC.md`](00_INFERENCE_AND_UNIFICATION_SPEC.md) are hard prerequisites for `!` over a generic carrier, and nothing else here depends on anything else there.

- **1.5, witness keying through a partially applied type constructor.** Without it the parametric `Monad` witness cannot be *declared*: the lambda body is a stuck application that `HeadKey::of_whnf` does not read.
- **1.4, right-biased partial imitation.** Without it the witness may exist but `x!` on a `Free(Sig, Nat)` never pins `?M`. The bias matters — `Free(S, A)` needs `S` fixed and `A` abstracted, which is the right-biased split.

00's item 1.3 is not a prerequisite but should land first: under 1.4 the pin is a guess that blocks on refutation, so the characteristic failure of generic do-notation is a witness goal that parked and never woke — today indistinguishable from the program being wrong.

## Staging

### Stage 1 — `/std/Effect`

The transparent carrier and its generic combinators: `Free`, `done`, `step`, `perform`, `bind`, `map`, a fold-shaped `handle`, and the parametric `Monad` witness. Registered in `curios-prelude/std.crs` per the two-touch-point rule.

Lands **before** 00's items, used with explicit `bind`. Sequencing it first is deliberate: it produces the working corpus against which 1.4 and 1.5 are validated.

No Rust changes. `curios-core`, `curios-cert`, `curios-elab`, `curios-text`, erasure, the continuation IR, wasm emission, the ABI, and the runtime are all untouched.

### Stage 2 — `!` over the generic carrier

Nothing to implement here; the stage exists to name the dependency. When 00's 1.4 and 1.5 land, `!` sequences `Free(Sig, _)` and Stage 1's explicit `bind` chains become sugar sites.

### Stage 3 — `/std/Io` and the host signature

The pivot, and the stage that pays for the feature.

- Declare `Host : (r : Type) -> pub Type` covering every host operation, and the sealed carrier `Io(A)`.
- Retype the generated `/sys` surface so host operations return `Io(_)` — `read`, `write`, `exit`, the `Foreign` rows, and the `Cell` operations. `curios-abi`'s wire contract is unchanged; only the surface types move.
- Retype every `/std` module that touches them: `Handle`, `File`, `tcp`, `http`, `proc`, `rand`, `time`.
- Change the entrypoint: a program is an `Io({})` the runtime handles, not a bare expression. **This breaks every existing program**, including `programs/hello_curios.crs`.
- Implement the base handler in the runtime. It cannot be written in Curios: `block_on` uses `Cell` for its own queues, so something primitive must sit underneath. The privileged handler is relocated, not eliminated.
- **Delete `curios-cert/src/purity.rs`.** With host operations returning `Io(_)`, `Cell/get(c)` is a `step` node — it denotes one value, so scrutinee refinement is sound and the premise is vacuous. This removes the analysis, its `Env::effect_memo` cache (a third of the prelude build when cold), and a `PERIMETER.md` row.

Two further gaps close as a side effect, and both should be recorded rather than discovered: `Prim::Exit` can no longer inhabit a proposition, and `DESIGN.md`'s erased-position decision loses its motivating case, since erasing a *description* of an effect is unremarkable.

### Stage 4 — `Handle` and `Async` as two handlers

`/std` currently has two hand-written interpreters of the same syscalls: `Handle/read` blocking, and `Async/read` non-blocking with park-and-retry. After Stage 3 they are two handlers of one `Host` signature, and the duplication goes away.

`Async`'s scheduler keeps the raw `step` form from inside the seal — guards, cancellation tokens, sleeper queues, and the deadlock verdict all need to store `k`. Do not attempt to express it through `Reply`.

This stage is where the objective's "one central place" actually becomes visible in the standard library rather than only in the compiler, and it is also the riskiest: it puts the most load-bearing module in the corpus onto a new abstraction. It should not be attempted until Stage 3 has a second consumer.

### Stage 5 — the `Monad` decision

A decision point, not planned work. Once Stages 1–4 land, `Monad` retains witnesses for `Free`, `Option`, and `Lst`. Whether to drop the last two and hardcode `!` to `Free/bind` is a trade against `DESIGN.md`'s closed-forms principle, and should be argued on evidence from Stages 1–4 rather than settled here.

## Open questions

- **Fusion.** Every host call becomes an allocation plus an interpreter step where there is now a direct Wasm import. `curios-ersd`'s partial evaluation and `curios-cont`'s inlining and contification could in principle fuse a known handler with a known program, but this is a sufficiently-smart-compiler argument. **Measure before committing to Stage 3**; a benchmark of `Handle/write` in a loop, before and after, is the minimum.
- **Bracket combinator.** `Reply` cannot express resume-then-post-process. Design the finalization-safe shape before Stage 3, because `block_on`'s guard draining depends on it.
- **Prelude build cost.** A new `/std` module and a retyped IO surface both change the archive. Prelude build time is already a known sore point; watch it per stage rather than at the end.
- **Sealing across file-backed modules.** The sealing behavior was verified with an inline module in a single file. `/std` uses file-backed modules across a subtree, under the same documented visibility rule, but that exact shape has not been run.

## Acceptance tests

**Stage 1.** A signature with operations of differing return types declares and elaborates; `perform` produces a `Free(S, R)` whose continuation is applied at the operation's own return type; a handler interprets a program built from several operations; the parametric `Monad` witness registers and a second ground witness at the same head is refused as `DuplicateWitness`; two distinct signatures each get a handler with no interference; a handler that ignores its continuation terminates the computation; **a handler that resumes twice enumerates both branches** — this is a supported property of the transparent carrier and must be pinned, not merely tolerated; positivity refuses the `Mu`-shaped variant, pinning why the reified continuation is required; a signature-specialized `bind` is refused, pinning the universe constraint; a handler used in a proof or type position is refused, naming the partial definition.

**Stage 2.** `!` sequences a generic `Free(Sig, _)`, and the result matches the explicit `bind` chain from Stage 1.

**Stage 3.** A user outside `/std` writes a clause mocking a host operation and it runs; the same user attempting to match `Io` to reach `k` is refused; a program is an `Io({})` and the runtime's base handler executes it; `Cell/get` in a scrutinee position refines soundly, with the test that motivated `carries_effect` — `an_effect_behind_a_stuck_head_does_not_refine` — restated against the new typing and still passing; `Prim::Exit` cannot inhabit a proposition.

**Stage 4.** A blocking and a non-blocking handler over one `Host` signature produce the same results for a sequential program; the scheduler's guard draining runs on every exit path including deadlock; the existing `/std/Async` and `curios/src/tests/scheduler.rs` behavior is preserved.

## Non-goals

- Effect rows, effect annotations on function types, or any effect information in types. Rows would need a row sort in `curios-core`, row equality re-derived in the kernel, polarity and size-change support, and a second constraint domain in the elaborator roughly the size of `universe_solver.rs` — running inside a conversion checker that already needs a step budget to stay terminating. 00's non-goals already exclude a second constraint domain. **Precondition for reopening:** a written program this design accepts and whose author wanted it rejected. None was found.
- Signature coproducts, effect subtyping, or handlers polymorphic in the remaining effect set. Blocked on the deferred per-binder polarity obligation in `curios-cert`, a different crate and perimeter. The ceiling is explicit: **one signature per computation type, chosen where the carrier is written.**
- Linear or affine types. Resumption discipline is a visibility question here, deliberately.
- A dynamic one-shot check in the style of OCaml 5.
- Surface `effect` or `handle` grammar. If taken up it must be pure sugar over this encoding, introducing no effect information into types, and specified separately.
- Restating `/std/Lst`'s nondeterminism monad as an effect. It is expressible, but `Lst` is a data structure whose monad is a convenience.
- Effects usable inside proofs or type-level computation.

## Implementation map

- `curios-prelude/std/Effect.crs`, `curios-prelude/std/Io.crs`, and `curios-prelude/std.crs` — Stages 1 and 3.
- `curios-text/src/prelude.rs` — the generated `/sys` surface types (Stage 3 only).
- `curios-runtime/` — the base handler and the entrypoint contract (Stage 3 only).
- `curios-cert/src/purity.rs` — deleted at Stage 3, with its `Env::effect_memo` seam and `PERIMETER.md` row.
- `curios/src/tests/` — cross-stage programs per stage.
- `documentation/SYNTAX.md`, `DESIGN.md`, `PERIMETER.md`, `ROADMAP.md` — durable records as each stage lands.

Stages 1 and 2 need no Rust change at all. Every Rust change in this document belongs to Stage 3 or later, which is the honest boundary between "a library" and "a language change".

## Verification

The ordinary gate, per stage:

```sh
make curios/runtime
cargo fmt --all -- --check
cargo check --workspace --all-targets --all-features
RUSTFLAGS="-Dwarnings" cargo clippy --workspace --all-targets --all-features
cargo test --workspace --all-targets --all-features
```

Stage 3 additionally requires the fusion benchmark, a `curios-runtime` isolation check through `make curios/runtime` confirming neither Cranelift nor Binaryen entered its graph, and `make curios/web` since the entrypoint contract reaches the JavaScript harness.

## Retirement criteria

Each stage retires individually; the file is deleted when Stage 4 lands and Stage 5 is decided either way.

- The library vocabulary, the two carriers, and the clause-based handler interface are recorded in `curios-prelude`'s module documentation.
- **The continuation-ownership principle** — that resumption discipline follows from carrier visibility — is recorded in `DESIGN.md`, since it is the reasoning a later reader is most likely to lose and most likely to undo.
- The totality boundary is recorded beside the library and pinned by its negative test.
- `carries_effect`'s deletion is recorded in `PERIMETER.md` as a retired row with the typing invariant that replaced it.
- The deferral rationale for rows and for coproducts survives in `DESIGN.md`, so this file's analysis outlives it.
- Roadmap entries are checked, unlinked summaries, and no reference to this filename remains.
