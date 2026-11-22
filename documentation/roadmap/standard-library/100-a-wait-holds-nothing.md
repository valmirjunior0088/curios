# A wait holds nothing

**Adopted by the user on 2026-11-22 and refined the same day; its next step is a ready delivery.** At every wait, everything a branch has taken is either answered already or held by an owner that outlives the branch — a handle's queue, a box, a channel — so a branch cancelled where it waits loses nothing and tears nothing. Every move between owners happens in one step that cannot wait, and a step that cannot go ahead waits before it takes anything. [A wait is a suspension, and only the host waits](../../design/standard-library/a-wait-is-a-suspension-and-only-the-host-waits.md) states the rule for a take, a read, a join, a sleep and a write ("an attempt takes nothing unless it answers"; "a write is a take that never waits, and then a wait that holds nothing of its own") and for what outlives a wait ("kept in a `/std/Box`"); this specification makes it hold of every operation of `/std`, so cancellation stays structural, `select` stays a driver that steps its branches, and `timeout` stays a bound on any wait.

## What is wrong today

- **`Io/read_all` waits holding what it has read** (`curios-prelude-build/std/Io.crs`), so a cancel drops bytes taken from a stream its caller still holds: at `2efe3b57a`, a `read_all` of a child's output bounded by a `timeout` that passes between its `abc` and its `def`, then run again on the same pipe, answers `def`. `tcp/Socket/call` reads its caller's socket through it. `Io/Reader/rest` is the same read with what it has read kept in the reader's box: the same program through it answers `abcdef`.

## What is decided

- **`Io/read_all` is withdrawn**: `Io/Reader/rest` is the read to a stream's end that a cancel leaves whole. `File/read_all` and `Proc/run`, whose streams are theirs alone, read through one private loop, and `tcp/Socket/call`, which reads its caller's socket, is withdrawn with it: an exchange is a write and a `Reader/rest` of a reader over the socket.
- **`Box/held` stays**, an observation that holds until the branch waits, as a channel's length does.

## What is known

- **A step between two waits is atomic already.** A program's branches run one at a time, so a step that cannot wait is a transaction with no log to keep, and `/std/internal/until`, the loop every wait on the program's own state goes through, runs an attempt that answers or declines naming what it waits on.
- **Commit follows from the rule rather than from a protocol.** A move never spans a wait, so a branch `select` drops has made whole moves or none, Concurrent ML's guarantee at the grain of a move: a branch cancelled between two writes has sent the first whole and none of the second, an action unfinished and no effect torn, as an event's post-commit `wrap` leaves one. Committing a choice at a branch's first effect was weighed on 2026-11-22 and set aside: it turns `select` from the first to finish into the first to act and leaves `timeout` unable to bound a begun write, which the decision's rejection of socket timeouts keeps.
- **`select`'s statement of what an operation loses where it is cancelled becomes a guarantee** that `/std`'s lose nothing, once `read_all`, the last that does, is withdrawn.
- **A snapshot is a program's own.** A value read out of a box and written back after a wait loses what another branch changed meanwhile, through `Box/held` or through a `Box/change` that answers what it holds — at `2efe3b57a`, two branches that each bump a box from a snapshot taken before a `yield` leave it at 1 either way — as the decision's rejection of a box set by value states, and as Haskell's STM loses it between two `atomically` blocks ([Harris, Marlow, Peyton Jones and Herlihy, Composable Memory Transactions](https://www.microsoft.com/en-us/research/publication/composable-memory-transactions/)). A public attempt over several boxes and channels, with `Box/held` withdrawn, was weighed on 2026-11-22 and set aside: withdrawing `held` removes a spelling and not the loss, and an attempt a program writes is the offer whose contract the decision's rejection of a public offer form finds held by prose alone, which nothing in the tree consumes.
- **Peers.** tokio's `write_all_buf` advances the caller's buffer, an owner outside the choice, where `write_all` leaves a buffer partly written as `select!` drops it ([`AsyncWriteExt`](https://docs.rs/tokio/latest/tokio/io/trait.AsyncWriteExt.html)); `Io/Reader` keeps what it read ahead in a box the same way.

## Stages

**1. `read_all` withdrawn.** Its callers in `/std`, in the tests and in `documentation/language.md` read through `Io/Reader/rest`, or through the private loop where the stream is theirs alone; `tcp/Socket/call` goes with it. Check: a `Reader/rest` bounded by a `timeout` between two arrivals, then run again, answers both.

## Verification and retirement

The stage's check, then `cargo xtask workspace lint` and the suite. Once it lands, the decision states the rule for every operation of `/std`, `select`'s cancel-safety paragraph in `Io.crs` becomes a guarantee, the decision names `Box/held` the observation the `/sys` Box carrier's documentation already states, and this file is deleted.
