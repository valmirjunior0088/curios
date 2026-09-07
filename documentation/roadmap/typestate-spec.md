# What a value may do next is part of its type

## Status

Not refined. Two consumers were found — one removed, one declined — and the three routes are surveyed, with the least expected of them traced against the compiler this checkout builds. [channels/03](channels/03-session-spec.md) is this same question asked about a two-party channel, and its survey of what linearity costs is not repeated here.

## Why it exists

`/std/http` indexed a reply by the stage of the protocol it had reached: `Reply: (Stage) -> Type` over `start`, `headers`, `fixed(n)`, `chunked` and `done`, with each operation typed from one stage to the next and `Server/Handler` demanding `(Request, Reply(start)) -> Try(Async, Io/Error, Reply(done))`. It was removed. `/std/Tty` was going to index a terminal by whether this program had put it in raw mode. It was declined.

Both promise more than they can keep. The index guarantees a caller cannot *start* an operation in the wrong state; it does not guarantee the caller left the previous one, because Curios has no multiplicities and the old value stays in scope. A `Reply(Stage/headers())` survives `body`, so a header can still be written after the body began — which is precisely what the type was introduced to refuse, and what its documentation claimed it did.

The name for this is **typestate**, after Strom and Yemini (1986): the operations legal on a value change with its state, and the type tracks which. Session types are the specialization of typestate to a channel with two ends and a duality between them, which is why this specification is not `channels/03` and why that one is an instance of this.

## What was removed

`Stage`, the indexed `Reply` family, and the eight staged operations `status`, `header`, `body`, `write`, `finish`, `stream`, `chunk` and `last`, none of which had a caller anywhere in the tree. `Server/accept` went with them, since its whole contract was to hand back a reply the caller then owed a response and a close — the unenforceable obligation in miniature.

Two things of value went with it. `write` carried `@_fits: Nat/Le(Bytes/len(b), n)`, a real proof obligation that a fixed body could not be overrun, discharged by reduction against the remaining count in the index; nothing replaces it. And chunked transfer encoding for responses went, since `stream`, `chunk` and `last` were the only way to write one. Both return when this specification is designed.

What replaced it is the shape [channels/03](channels/03-session-spec.md) already recommends: the library owns the socket, and a handler is `(Request) -> Try(Async, Io/Error, Response)`. The user cannot step a connection twice because the user never holds one.

## What was declined, and why it is the easy case

`Tty(Mode)` over `cooked` and `raw`, so that an operation needing a raw terminal could demand one. It is recorded here because its shape differs instructively from `Reply`'s and would cost nothing to add later.

`Reply`'s stages are a sequence, and going backwards corrupts the protocol. `Tty`'s index is a one-way capability: the only harmful direction is using a cooked terminal where a raw one is required, `Tty(Mode/raw())` is unforgeable because the representation is private and `raw` and `with_raw` are its only producers, and a stale `Tty(Mode/cooked())` cannot manufacture one. Cooked is not scarce either — `Tty/stdin` re-derives it. So the hole that sinks `Reply` does not exist for `Tty`, and if typestate returns, the terminal is where it is free.

## The three routes

**Inversion of control.** The library owns the resource and the user writes callbacks or returns data, so nothing can be stepped twice and no substructural discipline has to be spoken. Surveyed in [channels/03](channels/03-session-spec.md) under *Session Types Without Sophistry*; it is what `/std/http` now does. It gives up incremental control: a handler that wants to stream cannot, because it never holds the connection.

**Indexed monads.** Atkey's parameterised monads, generalised dependently by Brady's `Control.ST` — everything that would need linearity becomes part of the monad's state, so the resource is never a value and there is nothing stale to hold. This is the complete answer and the only one that recovers incremental control. It is blocked on the language, not on the library: `/syn/Monad` is `Monad(M: (Type) -> Type)` and types `bind(m: M(A), f: (A) -> M(B)) -> M(B)`, while an indexed bind is `M(i, j, A) -> ((A) -> M(j, k, B)) -> M(i, k, B)`, which is not an instance of that shape — and `!` resolves sequencing through `Monad`, so an indexed monad would get no sugar. A second concept and `!` support for it are what this costs.

**Region parameters.** A phantom type parameter distinguishing one resource from another, in the shape of the `ST` trick, needing rank-2 polymorphism and no linearity. It is orthogonal to the other two: it does not track state at all, and it closes a hole neither of them does. Today two `Reply` values from two different connections have the same type and are interchangeable, so nothing refuses writing one connection's header onto another's socket.

## What is certain

Traced against `target/release/curios` built on 2026-09-07, each program on standard input under a memory cap.

- **Rank-2 region parameters elaborate.** A family carrying a phantom parameter, `induct Tagged(r: Type): (Phase) -> Type | at(@p: Phase): (p) end`, together with a consumer type quantifying over the region, `(@r: Type, Tagged(r, Phase/start())) -> Tagged(r, Phase/done())`, is admitted with no diagnostic, and a lambda binding the implicit region checks against it. Nothing about rank-2 argument types or phantom indices is an obstacle.
- **Two regions do not unify.** `let mix(@a: Type, @b: Type, x: Tagged(a, Phase/start())) -> Tagged(b, Phase/done()) = step(x);` is refused with `type mismatch / inferred: Tagged(a, /Phase/done) / expected: Tagged(b, /Phase/done)`. So the parameter really does separate resources, and the route is available whenever a consumer wants it.
- **An inductive parameter is implicit at the constructor.** `Tagged/at(@Phase/done())` fills the *family's* parameter rather than the index and is refused; the constructor takes both implicits, and both are solved from the expected type when neither is written. This is the surface rule `syntax.md` states, and it is what a first attempt gets wrong.

## What has to be decided

- **Whether incremental responses return at all**, and whether that is what forces the indexed-monad route rather than a nicety on top of it.
- **Whether region parameters land independently of the rest.** They are cheap, verified, and fix a different bug class; nothing about them waits on a decision about state.
- **Whether the same mechanism serves `Tty`**, whose index is sound as it stands and would need none of the machinery the reply case needs.
- **What the guarantee is documented as being.** The removed `Reply` claimed in prose that a header after the body "is refused where it is written", which the index did not deliver. Whatever returns states what it actually refuses.

## Deliberately not specified

Multiplicities or a linear fragment of the language, which would close the hole directly and is a far larger question than the two consumers that raised it. Any claim about protocols with more than one party, which is [channels/03](channels/03-session-spec.md)'s. Any static account of what a resource left undropped costs, which is the collector's.
