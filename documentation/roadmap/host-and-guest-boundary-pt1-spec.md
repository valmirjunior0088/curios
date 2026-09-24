# Host and guest boundary, part 1: guest coordination

Pending implementation specification. Replace rewritable cells and notification-based coordination with write-once cells, bounded channels and level waiting. This is the first of two efforts; [part 2](host-and-guest-boundary-pt2-spec.md) specifies host operations and outcomes. This part is independently implementable and can retire independently; part 2 reuses its `/sys/Option` declaration.

## Scope and existing foundations

Recursive knots currently use rewritable memoization storage that transitions from unforced to forcing to finished. Replace it with a write-once result cell and a capacity-one initializer channel, preserving release of the initializer closure and `Cycle` refusal on reentrant forcing. `/std/Async` uses program cells for scheduler state and maintains notification lists for futures, signals and channels. Abandoned selection registrations can leave a capacity-zero channel believing a receiver is waiting after that receiver has selected another offer.

Use the existing intrinsic signature roster, syntax registry, decided-bound mechanism, continuation representation and emitter helper libraries. Preserve `/std/Async`'s `Step` and `Pause` protocol, offers, cancellation, finalizers and list-order selection priority. The positive-capacity requirement follows [the decision for partial primitives](../design/language/a-partial-primitive-is-totalized-by-a-canonical-extension-or-it-states-its-domain.md).

This part moves `Option` into `/sys`, adds the channel carrier and outcomes, changes the cell API, rebuilds coordination and threads `Tui/Session` state. It does not change host rows, wire types, exit dispatch or host error decoding. `Result`, `Io/Error`, `Io/Chunk` and the other host vocabulary retain their current homes. Repository-wide import normalization and unrelated stale-comment cleanup are separate maintenance work; comments made inaccurate by this implementation are corrected with it.

Lightweight processes, cross-instance transport, unbounded channels, selection fairness changes and unrelated optimizer work are outside this specification.

## `Option` belongs to `/sys`

Declare the ordinary polymorphic induct directly at `/sys/Option`, preserving its parameters, universes and constructor order, `some` then `none`. Do not introduce an additional `/sys/Option/Option` nesting. Its functions, witnesses and derivations remain in `/std`.

The existing standard-library module explicitly re-exports `Option` through `pub use /sys/{Option};` and its constructors through `pub use /sys/Option/{some, none};`. Place these before ordinary imports, following the surrounding re-export convention; do not use a glob. Existing `/std/Option` paths remain valid.

Register compiler-needed family and constructor identities through the existing syntax registry. Check declarations and instantiate parameters and universes through the normal declaration machinery. Every `Produced::Fixed` result goes through ordinary type formation in both checkers; no separate coordination declaration validator is added. A registry entry is not a substitute for a correctly formed polymorphic declaration. Use `List` as an implementation comparison, and rebuild both prelude archives.

## Cells and channels

The following signatures are schematic: type parameters and their universe levels follow the existing intrinsic conventions. Each operation's Core signature and generated `/sys` declaration agree; the prelude build checks that agreement. Coordination outcomes are ordinary guest inductives, with no numeric wire status or boundary reconstruction involved.

| Operation | Inputs after the type parameter | Result |
| --- | --- | --- |
| `Cell/new` | None | `Io(Cell(A))` |
| `Cell/fill` | `Cell(A)`, `A` | `Io(Bool)` |
| `Cell/poll` | `Cell(A)` | `Io(Option(A))` |
| `Channel/new` | `capacity: Nat`, decided evidence that `0 < capacity` | `Io(Channel(A))` |
| `Channel/push` | `Channel(A)`, `A` | `Io(Channel/Push)` |
| `Channel/take` | `Channel(A)` | `Io(Channel/Take(A))` |
| `Channel/close` | `Channel(A)` | `Io({})` |
| `Channel/closed` | `Channel(A)` | `Io(Bool)` |
| `Channel/count` | `Channel(A)` | `Io(Nat)` |
| `Channel/capacity` | `Channel(A)` | `Io(Nat)` |

### Write-once cells and knot memoization

`Cell/new()` allocates an empty cell. `fill` stores its value and returns `true` exactly when the cell was empty. A later fill returns `false` and preserves the first value. `poll` returns `none` while empty and `some(value)` after filling. Delete initialized construction, `Cell/set` and `Cell/get`; introduce no temporary public constructor or compatibility layer.

Knots use the same write-once cells and bounded channels as guest coordination. Each computed member has a result cell and a capacity-one channel holding its initializer. Allocate all storage, bind the closures, and enqueue every initializer before any member can be forced. Forcing polls the result first; if empty, it takes the initializer, runs it and fills the result. An empty initializer channel with an empty result means reentrant forcing and reports `Cycle`. The forcing function captures only the storage, so taking the initializer removes the knot's persistent reference to its captures. Preserve lazy evaluation, caching and existing knot fixtures. Delete the private rewritable operations and state rows as well as the public `Cell/set` and `Cell/get`.

### A bounded queue with atomic outcomes

The carrier and operations live under `/sys/Channel`. Declare `/sys/Channel/Push` with constructors `taken`, `full`, `closed`, and polymorphic `/sys/Channel/Take(A)` with constructors `item(A)`, `empty`, `ended`. Keep their declaration and instantiation on the normal inductive path used for `Option`.

Creation requires a positive capacity through the existing decided-bound mechanism. The obligation is part of the raw intrinsic signature as well as its `/sys` wrapper, so directly constructing a Core application cannot bypass it. Carry the new carrier, operations and outcome identities through checking, traversal, erasure, continuation IR and emission.

A push onto an open channel with room appends the value and returns `taken`; an open full channel returns `full`; a closed channel returns `closed`. A take returns the oldest queued value as `item`, `empty` when open and empty, or `ended` when closed and drained. Each attempt commits and chooses its outcome atomically; callers do not infer an outcome by combining separate observations. These operations do not park and have no operation-specific refusal path.

Close is idempotent. Closing prevents later pushes and preserves queued values until taken. `count` reports the current queue length, `capacity` the original bound, and `closed` the closed flag; these queries do not change the channel. They are observations, not reservations for a later attempt.

Use a preallocated ring buffer with head, count, capacity and closed state. Maintain FIFO order and `count <= capacity`, including across wraparound. Clear each consumed slot so the queue no longer retains the removed value. Representation and allocation costs belong to the emitter; this contract does not promise that allocation cannot exhaust runtime resources.

### Standard-library ends and acknowledgement

Rebuild `/std/Async/Channel` on the intrinsic while preserving `Sender(A)`, `Receiver(A)`, their public operation results and their offers. Ends remain freely copied within an instance. Construction gains the positive-capacity obligation.

Remove capacity-zero rendezvous. Where a sender must wait until its message has been taken, use a buffered message carrying a reply cell. The receiver fills that cell after taking the message, and the sender waits for the acknowledgement. Test the distinction between enqueueing the message and acknowledging its receipt. No cross-instance meaning for cells or channel ends is introduced here.

## Level waiting and scheduler ownership

Make `Wait` opaque. Preserve the public `ready` and `elapsed` factories for host readiness and time, and add these safe factories:

| Factory | Readiness condition |
| --- | --- |
| `filled(cell)` | The cell is filled |
| `readable(receiver)` | The channel holds a value or is closed |
| `writable(sender)` | The channel has room or is closed |

Keep construction from an arbitrary `Io(Bool)` probe private to `/std/Async`. Its own probes only observe state; they neither consume values nor fill cells. Public callers cannot arrange for arbitrary actions to run on every idle round. Closed channels count as ready so an offer can resume and report its terminal outcome.

Thread runnable jobs, parked registrations, sleepers and handle waits through `drive` and its helpers as values. Fibers request scheduler changes through `Pause`; they do not capture rewritable queue cells.

Each parked job has one write-once claim cell shared by all of its wait alternatives. An alternative must claim the job before enqueueing it. A successful claim permits exactly one enqueue; other alternatives cannot enqueue it again. Prune registrations whose claims are taken or whose jobs are cancelled.

Scan probes on every idle round, before blocking on handles or timers and before declaring deadlock. A fill made inside a lifted `Io` action must therefore become observable without notification. A readiness observation is advisory: after resumption, an offer retries its committing attempt and reparks if another fiber consumed the opportunity. Preserve selection priority in list order.

Use cells for futures, cancellation, guard completion, root answers and park claims. Use capacity-one channels for signals, coalescing a signal when its slot is already full. Delete `Waker`, `on_waker`, `Wait/woken`, registration/listener APIs used for notification and their notification lists. Preserve cancellation, cleanup, finalizer and deadlock behavior while changing their implementation.

## Thread `Tui/Session` as a value

Session fields become values rather than rewritable cells. Return updated state through the existing effect carriers:

| Operation | New result |
| --- | --- |
| `draw` | `Async(Session)` |
| `read`, with event parameter `E` | `Async({Session, Option(List(Event(E)))})` |
| `size` | `Try(Async, Io/Error, {Session, Tty/Size})` |
| `last_size` | Unchanged: `Io(Tty/Size)` |

The record notation above denotes the updated session paired with the existing observation. Preserve `read`'s optional end-of-input result. The reader, ticker and drawing loops each thread their own session value through successive iterations. Forward size changes as resize events and update the drawing session before rendering. Retain input buffering across reads and drawing history across frames; preserve terminal cleanup on completion and cancellation. Do not introduce separate public reader, ticker or drawing role types.

Migrate every standard-library, example and test consumer of the removed cell and waker APIs, including fixtures that used rewriting only to hide a value from the optimizer. Use a fresh cell where a fixture needs a new write-once observation; do not retain a mutable escape hatch for tests.

## Implementation checkpoints

1. Move `Option` and its constructors to `/sys`, retain explicit `/std` re-exports, register their identities and rebuild the archives. Verify polymorphic declaration behavior and existing consumers before proceeding.
2. Develop cells, channels, the scheduler and session threading in that order, then land them together as one buildable change with all consumers migrated. Their dependency cycle through the standard library does not justify an intermediate public compatibility API.

Review and validate each checkpoint before accumulating unrelated changes. Run focused behavioral tests plus `cargo x clippy` and `cargo x fmt` between implementation checkpoints. Follow [the contributor validation rules](../../CLAUDE.md#build-and-validation), including the complete hand-off gate once implementation and permanent documentation are final. Unavailable prerequisites, timeouts and interrupted commands remain outstanding checks, not passes.

Immediately before and after the combined coordination checkpoint, measure `programs/monad_async.crs` and a repeatable Tui session using built-in profiling. Keep workload and profiling configuration consistent; record the workload, implementation stage and individual measurements, and investigate regressions before retirement. Keep measurement evidence in the Rust measurement test's documentation, not as a permanent copy of this specification.

## Verification and completion criteria

- [x] Existing `/std/Option` paths, constructor matching, witnesses and derived `Spell` retain their behavior. Both checkers accept normal instantiation, repeated generated type occurrences and higher-universe cases; consumers including `Cli` compile. Intrinsic results pass ordinary type formation.
- [x] Empty polling yields `none`; the first fill returns `true`; repeated filling returns `false` and retains the first value. Existing knot behavior and reentrant-knot `Cycle` refusals remain unchanged.
- [x] Channels cover FIFO order, wraparound, capacity bounds, every push/take outcome, idempotent close and close-and-drain. Verify consumed slots release their references and emitted channel helpers contain no operation-specific refusal path.
- [x] Zero capacity is rejected through the public constructor and the raw intrinsic checking path. Valid positive capacities work through both checkers.
- [x] Scheduler fixtures cover abandoned selections, two receivers competing for one value, stale readiness, fills inside lifted `Io`, cancellation, finalizers and deadlocks. A job cannot be enqueued twice through separate wait alternatives, and cancelled or claimed registrations are pruned.
- [x] The explicit rendezvous waits for acknowledgement after receipt, including when another selection alternative wins. Existing sender/receiver operation results and offers retain their behavior.
- [x] User code cannot construct an arbitrary probe. Signals coalesce and futures preserve their first result without notification lists.
- [x] Threaded Tui tests exercise incomplete input buffering, end of input, resize tracking, drawing history and cleanup.
- [x] Source searches and consumer compilation establish that initialized cell construction, `Cell/set`, `Cell/get`, wakers and notification registration APIs are gone. No temporary replacement API remains.
- [ ] The comparison measurements and full hand-off gate are complete, with any regression resolved or explicitly reviewed before retirement.

The implementation checkpoint is covered by `curios/src/tests/runtime/{std_tests,knot_tests,coordination_tests}.rs`, `curios/src/tests/scheduler.rs` and `scheduler/level_tests.rs`, and `curios/src/tests/tui.rs`. Raw capacity evidence is checked independently in both checkers' `intrinsic_tests`; `curios-ersd/src/into_cont/knot_tests.rs` pins publication, caching, reentry and captures; `curios-emit/src/into_wasm/aggregate_tests.rs` pins consumed-slot release. The comparison is recorded in `curios/src/tests/coordination.rs`; the full hand-off gate remains pending.

## Rejected alternatives

- **Expose a rewritable cell beside a write-once cell.** That preserves two public meanings for cells and leaves guest coordination depending on conventions about permitted writes. Knots can preserve memoization and initializer release through the planned cells and channels.
- **Build the shared queue from write-once cells alone.** Long-lived ends can retain message chains and require traversal from old positions. The bounded ring gives explicit storage and release behavior.
- **Use a mutable one-slot box as the primitive.** A capacity-one channel already covers that use, while the box does not provide a bounded queue.
- **Return `Bool` from push and `Option(A)` from take.** Those shapes merge full with closed, and empty with ended. A second query cannot recover the outcome of the original atomic attempt.
- **Repair notification lists with selection claims.** This retains registration bookkeeping and stale entries when readiness can be observed directly.
- **Expose arbitrary probes or use an offer's committing attempt as its probe.** Arbitrary probes can perform effects during idle scans; committing probes consume opportunities before the selected fiber resumes. Private observation and a retried attempt keep those responsibilities separate.
- **Add a scheduler-specific intrinsic probe representation.** Waits concern objects in the current instance; private library observations suffice without another compiler vocabulary.
- **Keep capacity-zero rendezvous.** A buffer bound should describe storage. A reply cell makes receipt acknowledgement explicit and independently testable.
- **Use unbounded queues or keep a general mutable reference solely for `/std` channels.** Neither establishes bounded storage as part of the coordination primitive.
- **Split `Session` into public role types.** Threading independent session values through the existing loops expresses their ownership without expanding the public vocabulary.

## Permanent documentation and retirement

This file owns the pending acceptance checklist. Update permanent claims in the implementation checkpoint that makes them true; do not duplicate their explanations here after implementation.

| Owner | Required durable information |
| --- | --- |
| [`curios-prelude-archive/README.md`](../../curios-prelude-archive/README.md) | `Option` ownership and explicit re-exports |
| Intrinsic roster and syntax registry documentation | Signatures, positive-capacity obligation and registered family identities |
| Knot documentation in `curios-cont` | Shared cell semantics and the forced empty-read invariant |
| [`curios-emit/README.md`](../../curios-emit/README.md) | Cell and ring representations, slot release and allocation costs |
| A language decision in `documentation/design/language/` | Cells, channels and level waiting, with the rationale and rejected alternatives |
| `/std/Async` and `/std/Tui` documentation beside their code | Public operations, scheduler invariants and session threading |
| Existing effect-perimeter documentation | Examples and evidence using the new cell API |

Correct the contributor invariant about compiler-emitted names to allow registered `/sys` declarations as well as `/std`. Replace the roadmap's old checked cell, waker and deadlock descriptions when their implementations change. Leave exit, panic and host-row decisions to part 2.

Retire this specification only after every completion criterion has evidence and each durable decision and rejected alternative has an authoritative home. Replace its pending roadmap entry with checked descriptions linking to those owners. Audit removed APIs and specification references, including part 2's dependency link, before deleting this file; verify replacement links and that no reference to its filename remains. Part 2 need not be implemented for this part to retire.
