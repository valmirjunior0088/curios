# Profiling in the budget's own units

**Not refined yet.** This specification reserves what profiling has not answered since its first part landed — that a `profile` build files every span and event of whatever it ran, and that a declaration's fate in the optimizer is read from the compilation alone. It is not an implementation plan.

## The questions

- **What did checking a declaration cost?** `Consumption` per declaration — units and peak depth, in the reduction budget's own unit: machine-independent, reproducible, and priced identically by both checkers ([A reduction step costs what it builds](../design/toolchain/a-reduction-step-costs-what-it-builds.md)). A profile today reports durations, which are facts about the machine.
- **How often did each priced site run?** The compiler knows the price of every site; one execution supplies the counts. Counts, never durations: a count is a fact about the program and its input. Whether counts are wanted at all, and by which route, is open.
- **A buffered stream beside the written-through one.** A profile writes each row as it is made, so a run that aborts still leaves its rows; a run where that costs more than surviving an abort is worth wants a buffered stream instead.

## Refinement

Each question states its consumer, where its figure is recorded and how it is read (`cargo x profile` folds the stream today), and what it costs a build that records nothing.
