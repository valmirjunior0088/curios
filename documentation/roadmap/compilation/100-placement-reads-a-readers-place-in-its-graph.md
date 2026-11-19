# Placement reads a reader's place in its graph

[Every stage but reduction costs the graph it is handed](../../design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md) has a read visit a node once. Placement (`sink_computations`, `curios-cps/src/optimize/sink.rs`) reads where each reader of a computation stands off its census, never climbing the chain before it (`sink.rs`'s `//!`). What is left is the hold: `growth_slopes` (`curios/src/tests/instrumented/growth.rs`) has not yet counted `curios`'s `Growth::Loops`, a program of self-tail loops each called once from its entry, linear in every stage, which is what keeps placement to the rule.

The next step is that count, a measurement.

## What is known

- **Placement's climbs, counted.** By steps sampled per walk over `Growth::Loops`, compiled through `wonder --stage wasm` in a scratch copy of `3b03c7d7c` holding placement as it now is: the innermost holder's climbs take 3.0, 5.7 and 11.0 thousand steps at 128, 256 and 512 loops, `container` 19.8, 33.6 and 61.3 thousand lookups, and `site_in` 10.8, 18.1 and 32.7 thousand steps, where the chain before each reader cost `site_in` 1.44, 5.43 and 21.2 million and `between` 0.19, 0.72 and 2.81 million.

## Retirement

Once `growth_slopes` holds `Growth::Loops` among the growths every stage of which is linear (`HELD`), with its record restated at its commit, [An optimizer query costs what its answer uses](200-an-optimizer-query-costs-what-its-answer-uses.md) names this file no longer, and it is deleted. Where a stage shows the square, this file is restated to the climb that holds it.
