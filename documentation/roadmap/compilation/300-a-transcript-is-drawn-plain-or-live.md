# A transcript is drawn plain or live

**Ready to deliver.** Nothing here waits on parallel work: both drawings hold any number of open subjects, so [A compilation is a graph of item tasks](301-item-tasks.md) plugs its workers into them and leaves the transcript alone.

## What is missing

The transcript is told as events, each naming its tree, drawn under one lock by whoever tells it, plain or live ([What a command reports](../../usage.md#what-a-command-reports); `curios/src/report.rs`), and the live region redraws each running node with its time (`report/live.rs`). But nothing says what a running step is doing: a check of a large unit is one running node for as long as it takes, and no event says which of its declarations it is on or how many are left.

## The design

**A running node reads as it will when it closes**, its time after `;`, and what it is processing behind `·`: of a check, how many of its declarations are done and the one it is on, or of several in flight the one that has run longest, ``├─ check; 2.4s · 416 of 1,040 · `/std/Map/insert` ``. A node without the `·` has ended. Nothing past the time reaches the scrollback, and the plain drawing says nothing of it.

**The check tells them.** The elaborator's demand walk (`curios-elab/src/elaborate/module/demand.rs`) tells each declaration it asks for, and each it finishes — kept, taken from a cabinet, elaborated, refused or closed on a cycle (`Demand::close`) —, through a listener the pipeline installs on the context as it installs the reader's answer (`Context::set_reader`, from `curios-pipeline`'s `context_for`). A voided attempt never finishes, so it is counted once, when the attempt that stands does; the one it is on is the innermost declaration asked for; and the count is the declarations the walk holds: every one of a unit compiled whole, and over a baseline the ones the edit reached. The fold forwards each as a `Progress` event naming its unit, and the transcript tells it to the drawing.

## Stages

1. **A check tells its declarations.** Check: a fixture's events in the order the walk takes its declarations, a voided attempt counted once and the count the declarations it holds; the frame of a running check.

## Completion and retirement

Once stage 1 lands, [What a command reports](../../usage.md#what-a-command-reports) states the running check's count, [A compilation is a graph of item tasks](301-item-tasks.md) states its workers tell events as the sequential compiler does, and this specification is deleted.
