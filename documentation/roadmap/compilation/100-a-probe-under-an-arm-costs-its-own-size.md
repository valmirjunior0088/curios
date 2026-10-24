# A probe under an arm costs its own size

[Every stage but reduction costs the graph it is handed](../../design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md) has a judgment not pay its scope and a read visit a node once: a walk that outgrows its graph is a loop repeating it. The elaborator spells a probe under an arm as the kernel would read it, outside its metavariables' occurrences, and the filter in front of the arm's equations spells it again for each entry it is put to, where the kernel spells its candidate once for the lookup.

**Ready**: the one stage left is the next step.

## What is wrong

**The filter spells the probe again for each entry.** `could_settle` (`curios-elab/src/reduce.rs`) reads the probe as the kernel spells it (`Context::kernel_spelled`) for every entry `scan_settled` and `answered` put it to, since `a929d1f18`, and `kernel_spelled` files no spelling that holds an unsolved metavariable, so a probe that holds one is spelled once per entry and per pass. The kernel spells its candidate once per lookup and compares it with each key as filed (`curios-cert/src/kernel/scope.rs`).

**Where it shows.** In a scratch copy at `0fc2fc1bd`, counted, with the spellings passing over every metavariable occurrence: spelling the probe once for the lookup takes `ChainedMatches`' node looks at 400 links from 3.21 to 3.12 million, and its elaboration's allocations from 639.2 to 619.4 MB.

## What is known

- **The spellings no longer pay the scope.** A spelling reads a local outside every metavariable occurrence and gathers what to read from the locals a term names outside them (`locals_outside_spines`, `curios-elab/src/typing.rs`), so a probe's spelling costs the probe, as conversion's reading of a block's locals does.
- **What is left of the row is compilation 101's.** With both stages in a scratch copy at `0fc2fc1bd`, `refined_reduct`'s own allocations are linear (15.4, 31.1 and 62.7 MB at 100, 200 and 400 links), and the free-variable sets filled under it allocate 16.6, 52.3 and 179.0 MB from 6,514, 13,014 and 26,014 fills: a node rebuilt over a metavariable copies its spine's set ([A node carries its free variables in its own size](101-a-node-carries-its-free-variables-in-its-own-size.md)).
- **No verdict and no budget unit moves, and parity stands**: the probe is the same term however many times it is spelled, and the kernel spells it once.

## Stages

1. **The filter is handed the probe spelled once.** `could_settle` takes the probe as the kernel spells it, which `refined_reduct` spells where the filter first needs it and keeps for the lookup, as the kernel keeps its candidate: a settlement made between commits nothing, so the spelling stands. Check: `growth_slopes` passes, `ChainedMatches` reading looks within a tenth of 2.0 per doubling through `elaborate`, counted; `/std`'s units and image unchanged at one path, and its elaboration instructions counted before the stage that read the spellings outside metavariable occurrences and after this one.

## Verification and retirement

`cargo xtask workspace lint` and `curios-elab`'s tests, with the stage's check. At retirement `curios/src/tests/growth.rs`'s and `curios/src/tests/instrumented/growth.rs`'s sentences on `ChainedMatches` drop the cause they name here, keeping the bytes compilation 101 owns; the decision's *A walk passes over what it cannot change* names a reading by value's metavariable occurrence beside a capture's subterm and a materialization's; and this file is deleted. Figures it moves: `growth_slopes`' `ChainedMatches` row and `/std`'s elaboration instructions.
