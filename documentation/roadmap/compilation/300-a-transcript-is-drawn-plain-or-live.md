# A transcript is drawn plain or live

**Ready to deliver, stages 1 to 4 in order.** Nothing here waits on parallel work: the compiler opens one subject at a time today, and both drawings hold any number open, so [A compilation is a graph of item tasks](301-item-tasks.md) plugs its workers into them and leaves the transcript alone.

## What is missing

The transcript is written for one open line at a time ([A command narrates on one transcript and reports in blocks](../../design/tools/a-command-narrates-on-one-transcript-and-reports-in-blocks.md)). `Progress` is sequential by construction (`curios-pipeline/src/compile.rs`): `Compiled`, `Filing` and `Filed` name no unit and mean whichever was announced last, and `Entry` names none. `Steps` folds those events onto trees (`curios/src/pipeline.rs`) whose every node is two writes to one line of standard error, its label before the work and its outcome after it (`Tree`, `curios/src/report.rs`), and a test's own writes go through its host straight to the stream it wrote on, ending the open line first (`TestHost`, `curios/src/test_runner/host.rs`). The narration's state is the cursor's: once units are checked side by side, a second subject's label lands inside the first one's open line, and no one stream serves a pipe, a log and a person watching a terminal alike. And nothing says what a running step is doing: a check of `/std` is one open line for as long as it takes, and no event says which of its items it is on or how many are left.

## The design

**One transcript, drawn twice.** The transcript is events, data until the edge that writes them, as a report is: a subject named, a step begun, a step ended and how, a line of detail, a line a test wrote, a test ended, a tally, a report raised, a line under no subject, the total. Every event names its subject, so none depends on the one before it, and carries its data as text, so it crosses threads and holds nothing of the compiler's. The editor's progress already draws `Progress` its own way (`curios-wonder/src/server.rs`). The command line draws the events plain or live, and the two say the same things each in its own form.

**Drawn where it is told.** The transcript is one value behind a lock, and whoever tells an event draws it, on its own thread, before the work it announces: a label reaches the reader before its work, as today, and a process that dies mid-step leaves its last line written, with nothing queued to lose. A worker of item tasks tells its events the same way and holds the lock for a line. The live drawing's clock is a thread of its own, taking the same lock to redraw the region. Every write the command makes on standard output or standard error goes through the transcript, so nothing lands between the region and what stands above it; where something writes past it — the program `run` executes, a child a test starts with a stream inherited, a test that takes the terminal — the region is cleared first.

**The plain drawing is a line an event, each standing alone**: a verb, the subject's name in backticks, then what the event carries. A step's two lines share its verb, its present participle where the step begins and its past where it ends, and how it ended stands after the subject where it did not simply finish:

```text
Checking `/app` with its tests
Checked `/app` with its tests in 0.1s
Testing `/app/draws_a_square`
`/app/draws_a_square`> ■■■■
Tested `/app/draws_a_square`: passed in 0.0s
Testing `/app/equality_fails`
`/app/equality_fails`> comparing
Tested `/app/equality_fails`: failed in 0.0s
  not equal
    expected: `5`
    obtained: `4`
Tested `/app`: 2 passed, 1 failed, 1 exited in 0.0s
```

- A test always says how it ended, `passed`, `failed`, `trapped` or `exited N`; a step that finished says its time alone.
- What a test writes is relayed as it writes it, on the stream it wrote on, each line headed by its test in backticks and `>`.
- A report, what a failed test found and a trap's text are blocks written whole, so no line of another subject splits one.
- A hang is a line that began and never ended.
- The lines of different subjects interleave in the order their events happen; one subject's lines keep its order.

**The live drawing keeps what is settled above what is open.**

- **Above, for good, the oldest open subject streams as today's narration does**: its line, each node once it closes, each line its tests write quoted beneath its node, and each report the moment it is raised. A subject opened later waits in the region until every subject opened before it has ended, then its closed nodes are printed and it streams in turn. The scrollback is today's narration in the order subjects opened, whatever order the work ran in, and with one subject open at a time the screen is today's narration exactly.
- **Beneath, a region redrawn** at most ten times a second, and only when something changed or a clock moved: the oldest subject's running node, then each other open subject's tree so far, in the order subjects opened, capped at a third of the terminal's height with `+ N more` for the rest, then a line counting what is done, `2 of 7 units; 2.4s`.
- **A running node reads as it will when it closes**, its time after `;`, and what it is processing behind `·`: of a check, how many of its items are done and the name it is on, or of several in flight the one that has run longest, ``├─ check; 2.4s · 416 of 1,040 · `/std/Map/insert` ``. A node without the `·` has ended. Nothing past the time reaches the scrollback.
- **Nothing to restore**: no alternate screen, no raw mode and no hidden cursor, so an interrupted command leaves its last frame. Every line of the region is cut to the terminal's width, read again for each frame from `curios-native`, the terminal's authority. The region is gone before the command writes standard output.

**Which drawing.** A flag, then an environment variable, then the terminal: live where standard error is a terminal, `TERM` is not `dumb` and `CI` is unset, plain everywhere else. Colour stays decided as it is today. A screen reader is served by the plain drawing on a terminal, which the flag and the variable choose. A test serve's runner page keeps what the drawing wrote for good, so it shows what the scrollback holds.

## Stages

1. **The events, drawn as today.** `Progress` names its unit on every event; `Steps`, `Tree`, the test relay, the reports a command writes and the lines under no subject tell events to the transcript, which draws them as today's narration, byte for byte. Check: every test pinning the narration passing unchanged — 88 lines across 6 files under `curios/tests/` naming a node or a tally, counted at `48e29a69e` —, the refactor's own check.
2. **The plain drawing and the choice.** The plain drawing, chosen as above, with its flag and variable; a terminal keeps today's tree for now. Check: the pins restated from a run's own output; one event record fed to both drawings, each fact found in each; a record opening several subjects at once drawn plain, each subject's lines in its own order; the flag and the variable each forcing its drawing on a terminal and in a pipe.
3. **The live drawing**, for any number of open subjects, with the fold announcing the units it plans. Check: frame tests over a screen model of the four controls the drawing writes — carriage return, newline, cursor up and erase to the end of the screen —, on records opening one subject at a time and several at once: frames mid-run, the region's cap, the final screen equal to today's narration with one open at a time, a subject that ends behind an older one; a line cut to the width; the frame an interrupt leaves; the region cleared before a program's output.
4. **A check tells its items.** The demand driver tells each item as it begins and as it is published, and the running node shows them behind `·`. Check: a fixture's item events in the order the driver takes them, a voided attempt counted once; the frame of a running check.

## Open

- The flag's and the variable's names: `--transcript=plain|live` and `CURIOS_TRANSCRIPT` are candidates.
- The plain verbs' casing and alignment: capitalized as sampled, and whether right-aligned as cargo aligns its own.
- How a node with no work between its label and its outcome is drawn plain — `serve`, `try`, a `write` whose place is its outcome: on one line, or on two as every other step.

## Decisions this overturns

- [A command narrates on one transcript and reports in blocks](../../design/tools/a-command-narrates-on-one-transcript-and-reports-in-blocks.md): the bytes being the same in a pipe as on a terminal, nothing being erased or redrawn, and its rejection of "a live display beside the transcript … two renderers, one of which a pipe cannot show", which held while one subject was open at a time; each restated as its stage lands, with the two drawings of one transcript, whose scrollback is still never rewritten. Its rejection of capturing a test's output until it fails stands.
- `curios-pipeline`: `Progress`'s *sequential by construction*; `curios/src/report.rs`'s *nothing written is taken back*.

## Peers

- cargo writes its status lines in a pipe and adds a redrawn bar on a terminal (`term.progress.when`); ninja overwrites one status line on a smart terminal and prints every one on a dumb one; BuildKit's `--progress=plain` heads every line with its step, and `tty` is live; Gradle's `--console=plain|rich`, Buck2's `--console` with its superconsole only where standard error is a terminal, and dune's `--display`, whose progress falls back to quiet off a terminal, each choose as this does.
- indicatif's `MultiProgress` draws under a lock, ticks from a thread of its own (`enable_steady_tick`), prints a line above its bars (`println`) and clears them around an outside writer (`suspend`): the shape taken here. Zig's `std.Progress` has workers update preallocated nodes lock-free and a thread sample and draw them, which suits a region, which may skip any state, and not a log, which may not. superconsole emits above a canvas it redraws on each render.
- pnpm's `append-only` reporter moves no cursor and is the one its documentation names for parallel runs; GNU make's `--output-sync` holds a recipe's output until it ends, which its manual describes as long silences and then bursts; Turborepo grouped its logs by default once interleaved ones made a task hard to follow; GNU parallel's `--keep-order` prints jobs in the order given, later ones waiting on earlier ones, which the live scrollback takes without the waiting being unseen, since the region shows it.
- GitHub CLI replaced its spinner with static text for screen readers, and Claude Code's screen-reader mode writes plain sequential text in place of redraws.

## Rejected

- **One stream for both**, closing a node left open with `…` and writing it again with its outcome: a stream of fragments on a terminal and in a log alike.
- **Holding a subject's output until it ends**: silences, then bursts, and a hang shows nothing. A subject waiting its turn in the live scrollback is no such hold: the region shows it as it runs.
- **A drawing thread fed through a channel**: every write becomes asynchronous, so a label no longer reaches the reader before its work, what is queued is lost where the process aborts or ends early — `wonder`'s answer ends the command where its reader has gone —, and a test's line can be overtaken by a child it starts with a stream inherited unless every such path waits for the queue.
- **The live scrollback in the order subjects finish**: a test's lines then stand above a tree printed whole after them, so not even one subject at a time draws today's narration, and the scrollback's order is the schedule's.
- **A full-screen interface**, as Turborepo's since 2.0: some terminals have shown it blank or fallen back.
- **Building the live region with the executor**: one specification would carry both, and the drawing's behaviour with several subjects open would be first exercised by the change that starts opening them.

## Completion and retirement

Once stage 4 lands, the narration decision and [What a command reports](../../usage.md#what-a-command-reports), with `test`'s narration and its serve, state both drawings, [A compilation is a graph of item tasks](301-item-tasks.md) states its workers tell events as the sequential compiler does, and this specification is deleted.
