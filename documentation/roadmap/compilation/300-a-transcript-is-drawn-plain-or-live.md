# A transcript is drawn plain or live

**Ready to deliver, stages 1 and 2 in order.** Nothing here waits on parallel work: the compiler opens one subject at a time today, and both drawings hold any number open, so [A compilation is a graph of item tasks](301-item-tasks.md) plugs its workers into them and leaves the transcript alone.

## What is missing

The transcript is told as events, each naming its tree, drawn under one lock by whoever tells it, plain or live (`curios/src/report.rs`, `report/transcript.rs`; [A command narrates on one transcript and reports in blocks](../../design/tools/a-command-narrates-on-one-transcript-and-reports-in-blocks.md)), but live is today's tree (`report/tree.rs`), written for one open line at a time: every node is two writes to one line of standard error, its label before the work and its outcome after it. Once units are checked side by side, a second subject's label lands inside the first one's open line on a terminal, and nothing on it moves while a step runs. And nothing says what a running step is doing: a check of `/std` is one open line for as long as it takes, and no event says which of its items it is on or how many are left.

## The design

**One transcript, drawn twice.** The transcript is events, data until the edge that writes them, as a report is: a subject named, a step begun, a step ended and how, a line of detail, a line a test wrote, a test ended, a tally, a report raised, a line under no subject, the total. Every event names its subject, so none depends on the one before it, and carries its data as text, so it crosses threads and holds nothing of the compiler's. The editor's progress already draws `Progress` its own way (`curios-wonder/src/server.rs`). The command line draws the events plain or live, and the two say the same things each in its own form.

**Drawn where it is told.** The transcript is one value behind a lock, and whoever tells an event draws it, on its own thread, before the work it announces: a label reaches the reader before its work, as today, and a process that dies mid-step leaves its last line written, with nothing queued to lose. A worker of item tasks tells its events the same way and holds the lock for a line. The live drawing's clock is a thread of its own, taking the same lock to redraw the region. Every write the command makes on standard output or standard error goes through the transcript, so nothing lands between the region and what stands above it; where something writes past it — the program `run` executes, a child a test starts with a stream inherited, a test that takes the terminal — the region is cleared first.

**The plain drawing** is [What a command reports](../../usage.md#what-a-command-reports)'s, and why it stands alone the narration decision's.

**The live drawing keeps what is settled above what is open.**

- **Above, for good, the oldest open subject streams as today's narration does**: its line, each node once it closes, each line its tests write quoted beneath its node, and each report the moment it is raised. A subject opened later waits in the region until every subject opened before it has ended, then its closed nodes are printed and it streams in turn. The scrollback is today's narration in the order subjects opened, whatever order the work ran in, and with one subject open at a time the screen is today's narration exactly.
- **Beneath, a region redrawn** at most ten times a second, and only when something changed or a clock moved: the oldest subject's running node, then each other open subject's tree so far, in the order subjects opened, capped at a third of the terminal's height with `+ N more` for the rest, then a line counting what is done, `2 of 7 units; 2.4s`.
- **A running node reads as it will when it closes**, its time after `;`, and what it is processing behind `·`: of a check, how many of its items are done and the name it is on, or of several in flight the one that has run longest, ``├─ check; 2.4s · 416 of 1,040 · `/std/Map/insert` ``. A node without the `·` has ended. Nothing past the time reaches the scrollback.
- **Nothing to restore**: no alternate screen, no raw mode and no hidden cursor, so an interrupted command leaves its last frame. Every line of the region is cut to the terminal's width, read again for each frame from `curios-native`, the terminal's authority. The region is gone before the command writes standard output.

**Which drawing** is chosen as [What a command reports](../../usage.md#what-a-command-reports) states, by `--progress`, `CURIOS_PROGRESS` and the terminal; `live` draws today's tree until stage 1 lands. A test serve's runner page keeps what the drawing wrote for good, never a frame of the region, so it shows what the scrollback holds.

## Stages

1. **The live drawing**, for any number of open subjects, with the fold announcing the units it plans. Check: frame tests over a screen model of the four controls the drawing writes — carriage return, newline, cursor up and erase to the end of the screen —, on records opening one subject at a time and several at once: frames mid-run, the region's cap, the final screen equal to today's narration with one open at a time, a subject that ends behind an older one; a line cut to the width; the frame an interrupt leaves; the region cleared before a program's output.
2. **A check tells its items.** The demand driver tells each item as it begins and as it is published, and the running node shows them behind `·`. Check: a fixture's item events in the order the driver takes them, a voided attempt counted once; the frame of a running check.

## Decisions this overturns

- [A command narrates on one transcript and reports in blocks](../../design/tools/a-command-narrates-on-one-transcript-and-reports-in-blocks.md): nothing being erased or redrawn, and its rejection of "a live display beside the transcript … two renderers, one of which a pipe cannot show", which held while one subject was open at a time; restated as the live drawing lands, whose scrollback is still never rewritten.
- `curios/src/report.rs`'s *nothing written is taken back*.

## Peers

- cargo writes its status lines in a pipe and adds a redrawn bar on a terminal (`term.progress.when`); ninja overwrites one status line on a smart terminal and prints every one on a dumb one; BuildKit's `--progress=plain` heads every line with its step, and `tty` is live; Gradle's `--console=plain|rich`, Buck2's `--console` with its superconsole only where standard error is a terminal, and dune's `--display`, whose progress falls back to quiet off a terminal, each choose as this does.
- indicatif's `MultiProgress` draws under a lock, ticks from a thread of its own (`enable_steady_tick`), prints a line above its bars (`println`) and clears them around an outside writer (`suspend`): the shape taken here. Zig's `std.Progress` has workers update preallocated nodes lock-free and a thread sample and draw them, which suits a region, which may skip any state, and not a log, which may not. superconsole emits above a canvas it redraws on each render.
- pnpm's `append-only` reporter moves no cursor and is the one its documentation names for parallel runs; GNU make's `--output-sync` holds a recipe's output until it ends, which its manual describes as long silences and then bursts; Turborepo grouped its logs by default once interleaved ones made a task hard to follow; GNU parallel's `--keep-order` prints jobs in the order given, later ones waiting on earlier ones, which the live scrollback takes without the waiting being unseen, since the region shows it.
- GitHub CLI replaced its spinner with static text for screen readers, and Claude Code's screen-reader mode writes plain sequential text in place of redraws.

## Rejected

- **A subject waiting its turn in the live scrollback is no hold of its output**, which the narration decision rejects: the region shows it as it runs.
- **The live scrollback in the order subjects finish**: a test's lines then stand above a tree printed whole after them, so not even one subject at a time draws today's narration, and the scrollback's order is the schedule's.
- **A full-screen interface**, as Turborepo's since 2.0: some terminals have shown it blank or fallen back.
- **Building the live region with the executor**: one specification would carry both, and the drawing's behaviour with several subjects open would be first exercised by the change that starts opening them.

## Completion and retirement

Once stage 2 lands, the narration decision and [What a command reports](../../usage.md#what-a-command-reports), with `test`'s narration and its serve, state both drawings, [A compilation is a graph of item tasks](301-item-tasks.md) states its workers tell events as the sequential compiler does, and this specification is deleted.
