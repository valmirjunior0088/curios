# A channel owns its state, and a fiber parks with none

## Status

Designed and prototyped, not started. A complete channel with a non-committing selection was written against the compiler this checkout builds and runs today, needing no change to `/std` and no change to the compiler. This specification records which of the tree's own decisions eliminate most of the field, what the prototype measured, the one defect it found in shipped code, and the decisions to answer before a module is written.

## Why it exists

`/std/Async` has fibers, tasks, futures, a signal, scoped finalizers and a deadlock detector, and no way for two fibers to hand a value to one another. A fiber can wait for a task's single answer through `Future` and `join`, and it can wait for a flag through `Signal`, but a stream of values between two live fibers has no vocabulary at all. `/std/Tui` already hand-rolls the missing thing — a queue and an `Async/Signal` beside it — which is the demand stated in the corpus rather than inferred from peers.

## What the survey settled

Most of the field is eliminated by decisions already recorded here, so the survey's job is to say which decision removes which lineage, and what is left standing.

- **Go and occam** are removed by [syntax forms are closed, semantics extend by witness](../../design/language/syntax-forms-are-closed-semantics-extend-by-witness.md). There is no `chan T`, no `<-`, no `go`, and no `select` statement to add, and that decision's stated precedent is explicit: where no concept fits, add a *generic* concept any type could satisfy, never syntax special-cased to one type's operations.
- **Effect handlers** — OCaml 5's `Eio`, Koka, Effekt — are removed by [effects are descriptions, and the carrier has no eliminator](../../design/language/effects-are-descriptions-and-the-carrier-has-no-eliminator.md), which is not an open question: that design was implemented end to end on the `effects-system` branch and reverted whole, measured an order of magnitude worse at the same work. Its stated precondition for reopening is a written program the current design accepts whose author wanted it rejected, and a channel is not one.
- **A channel as a host primitive** is removed by [Curios owns the language, Rust owns the host](../../design/toolchain/curios-owns-the-language-rust-owns-the-host.md). The scheduler is already Curios; so is the channel. No `curios-abi` row, no runtime change.
- **Erlang's mailbox** is refused on operational grounds rather than typing ones. An unbounded mailbox accepts messages until the node runs out of memory, which is why the ecosystem grew `pobox` and why Hébert's *Handling Overload* exists; and selective receive is an O(n) scan per attempt during which the actor answers nothing else, so it *raises* deadlock risk rather than lowering it. `Task`, `cancel` and `using` already answer what links and monitors answer.
- **Concurrent ML** is what survives, and for one reason: Reppy's thesis that the synchronous operation should be a first-class value. The comparison that settles it is Wingo's — a built-in `select` and `events + choose + sync` have the same power *over channels*, but only the second also admits timeouts, condition variables and user-defined abstractions into the same selection without breaking their abstraction. What is **not** taken from CML is negative acknowledgement, which exists to un-commit an operation that committed before it learned it lost. Nothing here commits speculatively, so there is nothing to un-commit.
- **Tokio contributes the invariant**, arrived at independently by the prototype: an operation is safe to abandon exactly when all of its state lives in the shared structure rather than in the abandoning fiber. `Receiver::recv` is cancel-safe because the value stays in the channel until it is kept.

## What is certain

Measured against `target/release/curios` built on 2026-09-06, with prototypes on standard input, each under a memory cap, on an otherwise idle machine. Every program below compiled in about one second.

- **The whole layer is expressible with no change to `/std`.** A `Chan(A)` holding a `Cell(List(A))` and a `Cell(List(Signal))`, with `send` and `try_recv` in `Io`, a parking `recv` in `Async`, and a selection over a list of channels that subscribes one shared `Signal` to every source, compiles and runs. `Async/park` is subtree-private, but `Async/Signal` is public and is sufficient to build the same discipline, so the module's placement is a choice rather than a constraint.
- **Selection loses nothing when nothing is taken until it is kept.** Two channels each holding one value, selected between: the prototype answered with index 0 and value 10, and the losing channel still held its value afterwards.
- **`/std/Async/select` loses a value.** The same two channels through the shipped `select` answered index 0 and value 10, and *both* channels were empty afterwards — one value retrievable was expected, zero were. `select` forks a fiber per arm and cancels the losers after the winner fulfills, so both arms take before either wins, and `Future/fulfill` on an already-ready future is `ready(_) => Io/pure(())`, which discards the second answer. This is not hypothetical for programs written today: `select` over two `stream/Read` sockets both ready in one round drops the loser's bytes.
- **Cross-kind selection works today, at the cost of a fiber and a sentinel channel per arm.** A nine-second timer raced against a ready channel, with the timer spawned as a task and cancelled once the channel won, completed in 1.56s wall including compilation. Cancellation is immediate; nothing waits out a cancelled sleep.
- **A park costs an allocation per source per attempt** in the `Signal`-only shape: one `Signal` per parked selection, subscribed into every source's waiter list, discarded on wake. The scheduler's own park costs one `Waker` and one slot.
- **`choose` is a reserved word** — it is the guarded-ladder form ([syntax.md](../../syntax.md)) — so CML's name for the combinator is unavailable. `select` is free once the shipped one is replaced.

## The design

One sentence: **a channel owns every value it holds, and a fiber selecting over channels takes nothing until it keeps it.**

**The channel.** `Chan(A)` holds its queue and its waiters. `new(capacity: Nat)` is `Io`, since it only allocates and [an operation is `Io` unless it suspends](../../design/language/an-operation-is-io-unless-it-suspends.md); `send` and `recv` are `Async`, since either may park. `try_send` and `try_recv` are `Io` and are the non-committing halves the selection is built from.

**Capacity is not a tuning knob.** A capacity is required at construction, and `0` means rendezvous. Back-pressure is the channel's job, and the surveyed alternative has a published record of failing under load.

**`recv` answers an `Option`.** On a channel that is closed and drained there is no `A` to return at a generic `A`, so by [a partial primitive is totalized by a canonical extension, or it states its domain](../../design/language/a-partial-primitive-is-totalized-by-a-canonical-extension-or-it-states-its-domain.md) this is settled the way `List/get` is settled: there is no canonical extension, so the answer is `Async(Option(A))`. `none` means closed and drained, and it is the only end-of-stream signal.

**Selection is an offer and a retry, never a fork and a cancel.** An `Offer(A)` is a non-committing attempt and a registration — `try: Io(Option(A))` and `register: (Waker) -> Io({})`. `select` walks the offers, answers with the first that yields, and otherwise registers on all of them and parks, retrying on any wake. A value moves out of a channel only into the fiber that keeps it, so an arm that loses has taken nothing. `Offer/map` is CML's `wrap`; a guard is an ordinary lambda.

**Every readiness is an offer.** A channel's receive and its send, a `Signal`, a `Future`, a handle's readiness through a zero-timeout poll, and a timer. That uniformity is the whole point of preferring a first-class offer to a built-in selection, and it is what a built-in `select` cannot be extended to cover.

**What it replaces.** `Async/select`'s fork-and-cancel is deleted rather than left standing beside a correct one, and `select`, `race` and `timeout` are re-expressed over offers. A second selection primitive with different loss behaviour beside the first is the worse outcome.

## What has to be decided

Each with the recommendation first.

- **Where the module lives.** A package built on `Async/Signal`, proven, and moved into `/std/Async/Channel` afterwards; or `/std/Async/Channel` from the start, reaching `Async/park` directly. The prototype makes the first available and it is the lower-risk order, at the cost of one allocation per park until the move.
- **Whether the scheduler's parks become waker-shaped.** `Pause/wait` and `Pause/sleep` resume a fiber directly, so a handle and a timer cannot be offers without a helper fiber. Making both carry a `Waker` would make every readiness uniform and delete the helper. It is a rewrite of `Pause`, `Job`, `serve` and both wait queues, and the measurement above says it buys efficiency and uniformity rather than capability — cross-kind selection already works. Recommended as a second cut, not a first.
- **Sender and receiver as distinct types, or one type with two views.** Distinct, so that closing is expressible and so that [02](02-remote-spec.md) has one end to serialize.
- **What `send` does to a closed channel.** Recommend a refusal the caller can see rather than a silent drop; the shape is a decision this specification does not make.
- **The order `select` tries its offers in.** First match wins in list order is the simplest and is what the prototype does. It is not fair, and fairness is a decision rather than an implementation detail.
- **Whether spurious wakeups are permitted.** Recommend yes, since the retry loop makes them harmless and forbidding them costs bookkeeping in every source.

## Prerequisites

- **`Async/select` discards a losing arm's answer.** Reproduced above. It is fixed by this specification's design rather than beside it, and it is the reason the shipped `select` is deleted rather than kept. Until then, no `/std` function may be written that selects over a shared, consuming source.

## Deliberately not specified

Parallelism: the guest is single-threaded by construction, and nothing here anticipates otherwise. Priorities or a scheduler policy. A distributed failure detector, which is [02](02-remote-spec.md)'s subject. Protocol conformance, which is [03](03-session-spec.md)'s. Any timing.
