# A counted growth is linear in its size

**Not refined yet.** The bench's growth slopes read past 3.0, a square's or a cube's where a form should cost its size, across erasure, parsing, the ANF optimizer and elaboration, and how many causes they are is not known. It waits on its first stage, which counts each growth on a wider grid.

## What is wrong today

- Read off `growth_slopes`' counts (`xbench/src/measurements/growth.rs`), run twice over the compiler of `439762f0a`, slopes past 3.0 in each, of which the measurement holds looks alone, the counts that repeat, and leaves the five growths whose looks grow past it unheld (`UNHELD`).
- At erasure: `let`s (3.95 looks, 6.36 allocations, 6.46 bytes), a family's values (3.92 looks, 3.95 allocations, 7.43 bytes), a product commuted (3.62 to 3.70 in every column), a family's constructors (3.80), a telescope's parameters (3.70), a function-type alias's readers (3.52, and 3.26 allocations and 3.64 bytes) and nested lambdas' looks (3.20).
- At parsing: nested parentheses (3.92, 3.95 bytes) and nested lists (3.77, 3.75 bytes).
- In the ANF optimizer: nested lambdas (4.56 allocations, 4.06 bytes).
- In elaboration: matches (3.45, 3.06 bytes) and matches each on the `let` before it (3.70 to 3.73 in every column), a growth the measurement holds in every stage.
- Over the two runs every look slope repeats, and allocations and bytes move.

## Known

- On a grid doubled twice past `growth_slopes`', a square's slope nears four where a logarithm's stays near three; `let`s' allocations at erasure read past six, nearer a cube's eight.

## Open

- How many causes the growths are, and which readings are a square and which a cube.

## Stages

1. **The wider grid.** Each growth counted at its steepest stage on a grid doubled twice past `growth_slopes`', on today's compiler, looks and allocations, the counts a busy machine leaves alone. Check: each growth read as a square, a cube or linear with a logarithm.
2. **The causes.** Each stage that holds a square profiled for its cause, and this specification split by cause into findings or specifications of their own.

## Verification and retirement

Retired once every growth `growth_slopes` counts is held under `HELD_SLOPE` with `UNHELD` empty, or has moved into an entry or a specification of its own; `growth.rs`'s doc of `UNHELD` then cites what holds each.
