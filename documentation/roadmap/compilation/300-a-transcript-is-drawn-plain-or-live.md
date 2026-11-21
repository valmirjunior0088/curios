# A transcript is drawn plain or live

**Ready to deliver, stage 1 then stage 2.** Nothing here waits on parallel work: the compiler opens one subject at a time today, and both drawings hold any number open, so [A compilation is a graph of item tasks](301-item-tasks.md) plugs its workers into them and leaves the transcript alone.

The transcript is written for one open line at a time ([A command narrates on one transcript and reports in blocks](../../design/tools/a-command-narrates-on-one-transcript-and-reports-in-blocks.md)). `Progress` is sequential by construction (`curios-pipeline/src/compile.rs`): it names a unit where the unit begins, and its later events mean whichever was announced last. `Steps` folds it onto trees (`curios/src/pipeline.rs`) whose every node is two writes to one line, its label before the work and its outcome after it (`Tree`, `curios/src/report.rs`). Once units are checked side by side, a second subject's label lands inside the first one's open line, and no one stream serves a pipe, a log and a person watching a terminal alike: holding a subject's output until it ends gives silences and then bursts, and closing an open line with a mark to write it again later gives a stream of fragments.

## The design

**One transcript, drawn twice.** The transcript is events, data until the edge that writes them, as a report is: a subject's step begun, a step ended and how, a line a test wrote, a report. Every event names its subject, so none depends on the one before it. The editor's progress already draws the same `Progress` events its own way (`curios-wonder/src/server.rs`). The command line draws them plain or live, and the two say the same things each in its own form.

**What a worker plugs into.** The drawing runs on a thread of its own, fed through a channel. An event carries its subject and its data as text, so the drawing holds nothing of the compiler's, and a worker sends the events the sequential compiler sends today. What the drawing does when several subjects are open is settled here and tested on event records that open several at once, before any compiles side by side.

**The plain drawing is a line an event, each standing alone**: a verb, the subject's name in backticks, then what the event carries. A step's two lines share its verb, its present participle where the step begins and its past where it ends, and how it ended stands after the subject where it did not simply finish:

```text
Checking `/lib` with its tests
Checked `/lib` with its tests in 0.2s
Checking `app`
Checked `app`: refused in 0.3s
Testing `/geometry/draws_a_square`
`/geometry/draws_a_square`> ■■■■
Tested `/geometry/draws_a_square`: passed in 0.1s
```

- A test always says its verdict, `passed` or `failed`; a step that finished says its time alone.
- What a test writes is relayed as it writes it, each line headed by its test in backticks and `>`.
- A report is a block written whole, so no line of another subject splits it; a failed test's findings are written whole beneath its `Tested` line.
- A hang is a line that began and never ended.
- The lines of different subjects interleave in the order their events happen; one subject's lines keep its order.

**The live drawing keeps what has finished above what is open.**

- **Above, for good**: each subject's tree as today's narration draws it, printed whole once its last node closes, in the order subjects finish; each report the moment it is raised; each line a test writes, as the plain drawing writes it.
- **Beneath, a region redrawn** at most ten times a second, and only when something changed or a clock moved: each open subject's tree so far, in the order subjects opened, capped at a third of the terminal's height with `+ N more open` for the rest, then a line counting what is done, `2 of 7 units; 2.4s`.
- **A running node reads as it will when it closes**, its time after `;`, and what it is processing behind `·`: how many of its items are done and the name it is on, or of several in flight the one that has run longest, ``├─ check; 2.4s · 416 of 1,040 · `/std/Map/insert` ``. A node without the `·` has ended. Nothing past the time reaches the scrollback.
- **A test program's open tree** holds its running tests, each with its time, and its tally as a running node, `└─ 1 passed, 0 failed; 0.4s`.
- **Nothing to restore**: no alternate screen, no raw mode and no hidden cursor, so an interrupted command leaves its last frame. Every line of the region is cut to the terminal's width, read again for each frame. The region is gone before the command writes standard output.
- With one subject open at a time, as today, the final screen is today's narration.

**Which drawing.** A flag, then an environment variable, then the terminal: live where standard error is a terminal, `TERM` is not `dumb` and `CI` is unset, plain everywhere else. Colour stays decided as it is today. A screen reader is served by the plain drawing on a terminal, which the flag and the variable choose.

## Stages

1. **The events and the plain drawing.** `Progress` names its subject on every event, `Steps` and the relay of what a test writes emit events, the drawing thread takes them through its channel, the plain drawing writes them, and the choice above decides the drawing, with its flag and variable. A terminal keeps today's tree for now. Check: the tests pinning the narration in a pipe (18 lines across 6 files under `curios/`) restated from a run's own output; one event record fed to both drawings, each fact found in each; a record opening several subjects at once drawn plain, each subject's lines in its own order; the flag and the variable each forcing its drawing on a terminal and in a pipe.
2. **The live drawing**, the region above for any number of open subjects. Check: frame tests over a screen model of the four controls the drawing writes — carriage return, newline, cursor up and erase to the end of the screen —, on records opening one subject at a time and several at once: frames mid-run, the region's cap, the final screen; a line cut to the width; the frame an interrupt leaves.

## Open

- The flag's and the variable's names: `--transcript=plain|live` and `CURIOS_TRANSCRIPT` are candidates.
- Whether a test program's final tree quotes each test's output again beneath its node, where the scrollback holds it already.
- The plain verbs' casing and alignment: capitalized as sampled, and whether right-aligned as cargo aligns its own.

## Decisions this overturns

- [A command narrates on one transcript and reports in blocks](../../design/tools/a-command-narrates-on-one-transcript-and-reports-in-blocks.md): the bytes being the same in a pipe as on a terminal, and its rejection of "a live display beside the transcript … two renderers, one of which a pipe cannot show", which held while one subject was open at a time; each restated as its stage lands, with the two drawings of one transcript. Its rejection of capturing a test's output until it fails stands.
- `curios-pipeline`: `Progress`'s *sequential by construction*.

## Peers

- cargo writes its status lines in a pipe and adds a redrawn bar on a terminal (`term.progress.when`); ninja overwrites one status line on a smart terminal and prints every one on a dumb one; BuildKit's `--progress=plain` heads every line with its step, and `tty` is live; Gradle's `--console=plain|rich`, Buck2's `--console` with its superconsole only where standard error is a terminal, and dune's `--display`, whose progress falls back to quiet off a terminal, each choose as this does.
- pnpm's `append-only` reporter moves no cursor and is the one its documentation names for parallel runs; GNU make's `--output-sync` holds a recipe's output until it ends, which its manual describes as long silences and then bursts; Turborepo grouped its logs by default once interleaved ones made a task hard to follow.
- GitHub CLI replaced its spinner with static text for screen readers, and Claude Code's screen-reader mode writes plain sequential text in place of redraws.

## Rejected

- **One stream for both**, closing a node left open with `…` and writing it again with its outcome: a stream of fragments on a terminal and in a log alike.
- **Holding a subject's output until it ends**: silences, then bursts, and a hang shows nothing.
- **A full-screen interface**, as Turborepo's since 2.0: some terminals have shown it blank or fallen back.
- **Building the live region with the executor**: one specification would carry both, and the drawing's behaviour with several subjects open would be first exercised by the change that starts opening them.

## Completion and retirement

Once stage 2 lands, the narration decision and [What a command reports](../../usage.md#what-a-command-reports) state both drawings, [A compilation is a graph of item tasks](301-item-tasks.md) states its workers send events as the sequential compiler does, and this specification is deleted.
