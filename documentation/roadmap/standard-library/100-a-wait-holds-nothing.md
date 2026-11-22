# A wait holds nothing

**Adopted by the user on 2026-11-22; not refined yet.** At every wait, everything a branch has taken is either answered already or held by an owner that outlives the branch — a handle's queue, a box, a channel — so a branch cancelled where it waits loses nothing and tears nothing. Every move between owners happens in one step that cannot wait, and a step that cannot go ahead waits before it takes anything. [A wait is a suspension, and only the host waits](../../design/standard-library/a-wait-is-a-suspension-and-only-the-host-waits.md) states the rule for a take, a read, a join and a sleep ("an attempt takes nothing unless it answers") and for what outlives a wait ("kept in a `/std/Box`"); this specification makes it hold of every operation, so cancellation stays structural, `select` stays a driver that steps its branches, and `timeout` stays a bound on any wait.

## What is wrong today

- **A write waits holding the rest of its message.** `write_waiting` (`curios-prelude-build/std/internal/Handle.crs`) sends what the host accepts and, after a short write, waits again with the rest, so a `select` or `timeout` that drops it there leaves the peer holding part of a message, and the next write on the handle follows the cut bytes as though nothing was cut.
- **`read_all` and `copy` wait holding what they moved**, which `select`'s documentation (`curios-prelude-build/std/Io.crs`) states they lose where they are cancelled.
- **A value read from a box outlives a wait and is moved back as though current.** `let n = Box/held(b)!;`, a wait, then `Box/change(b, (_) => (n + 1, ()))` loses whatever another branch changed meanwhile, and lints clean; `held` reads a box apart from any change (`curios-prelude-build/std/Box.crs`).

## What is known

- **A step between two waits is atomic already.** A program's branches run one at a time, so a step that cannot wait is a transaction with no log to keep, and `/std/internal/until`, the loop every wait on the program's own state goes through, already runs an attempt that answers or declines naming what it waits on.
- **Commit follows from the rule rather than from a protocol.** A move never spans a wait, so a branch `select` drops has made whole moves or none, Concurrent ML's guarantee at the grain of a move: a branch cancelled between two writes has sent the first whole and none of the second, an action unfinished and no effect torn, as an event's post-commit `wrap` leaves one. Committing a choice at a branch's first effect was weighed on 2026-11-22 and set aside: it turns `select` from the first to finish into the first to act and leaves `timeout` unable to bound a begun write, which the decision's rejection of socket timeouts keeps.
- **Peers.** Haskell's STM composes a step over several variables and waits by `retry` until one it read changes, choosing by `orElse` ([Harris, Marlow, Peyton Jones and Herlihy, Composable Memory Transactions](https://www.microsoft.com/en-us/research/publication/composable-memory-transactions/)); Node.js's `stream.write` and Netty's outbound buffer take a whole message into the stream's queue; Erlang sends a message whole; [Trio](https://trio.readthedocs.io/en/stable/reference-io.html#trio.abc.SendStream.send_all) declares a stream cut mid-`send_all` broken, the state this rule keeps from arising; marking a cut write's half broken and shielding a region from cancellation were weighed on 2026-11-22 and set aside.
- **The decision's other choices stand on the rule.** Cancellation at any wait, `select` stepping in list order, `timeout` bounding any wait, readiness rather than completion, and its rejections of a public offer form (every operation keeps an offer's contract by construction), of a type for an action that loses nothing (none is needed) and of a box set by value. Its sentence that a value carried out of a box is data that has left it narrows to a snapshot written back on purpose, which no language rules out.

## Open questions

- Where a handle's outbound queue lives — the host, whose flush row already hands on what it holds, or a box a handle keeps — its bound, and what a full queue makes a write wait for.
- What a failure while a queue drains reports, and to which later write, flush or close.
- The public form of an attempt over several owners: its name, what it may read and change, and how it declines; and what replaces `held` for a program that only observes a box.

## Stages

**1. A write takes its whole message or nothing.** It waits until the handle's outbound queue has room, hands it the whole message in one step, then waits for the queue to drain; cancelled during the drain, the rest stays queued, and the next write, flush or close on the handle drains it first, close discarding it as close already does. Check: a `select` between a write to a slow peer and a sleep leaves the message whole on the wire, and the next write follows it whole; a `timeout` still bounds the write.

**2. A move between owners is one step.** `copy` waits until its source is readable and its destination has room, then moves a chunk in one step; `read_all` keeps what it has read in its reader's box; a transfer between channels waits for an item and for room, then moves it. Check: `read_all` and `copy` under `select` lose nothing.

**3. Shared state is read and changed in an attempt.** An attempt over several boxes and channels made public, `select` over attempts committing the one that answers, and `Box/held` withdrawn, so every write to shared state reads its current value. Check: the program above is unwritable as it stands; every reader of a box in `/std` and the corpus restated.

## Verification and retirement

Each stage's check, then `cargo xtask workspace lint` and the suite. Once stage 3 lands, the decision states the rule for every operation, `Io.crs`'s cancel-safety paragraph becomes a guarantee, and this file is deleted.
