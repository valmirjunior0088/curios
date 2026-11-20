# An arm's equation reaches every term conversion holds equal to its scrutinee

[Case equations and their key](../../design/soundness/case-equations-and-their-key.md) has an arm's equation answer the terms conversion holds equal to its scrutinee ([A term is one where conversion says so](../../design/arithmetic/a-term-is-one-where-conversion-says-so.md)). One reader stops short of that, in the refusing direction: an equation an outer arm settled is not read again under an inner arm on an expression. A question inside a question is no second: [A question reduction asks is answered by plain reduction](../../design/soundness/a-question-reduction-asks-is-answered-by-plain-reduction.md) rejects the third reduction it would take.

## What is wrong today

**An outer equation is not read again under an inner arm on an expression.** A guard's reduced spelling is settled with the equations inside it withheld, and an inner arm restates the guard only where its case solves a variable the guard reads ([Case equations and their key](../../design/soundness/case-equations-and-their-key.md)). Under `match match h(p) == h(q) | true => k(0) | false => k(1) end`, with `p, q : F(g(c))` and `F(b) = match b | true => {} | false => Nat end`, the inner arm `match g(c) | true => …` makes `p` and `q` units and the guard's scrutinee `k(0)`, and `Holds(k(0))` is refused there, at `ef6a83175`: the guard's spelling, settled without `g(c) = true`, is the stuck match. Split on `c` over `p, q : F(c)`, the same arm restates the guard and the fact holds.

## Settled

- **The reach is conversion's**, the decision's rule as written, with a question inside a question left out.
- **An inner arm on an expression restates** each equation in force whose settlement reads a local the arm's key names, read through types and values to the end as `curios_analysis::reads_solved` reads a solution, its instance pushed after the arm's own equation so its settlement sees it.

## Measurement

Counted, user-space instructions retired by a process and its threads (`perf_event_open` on the child, enabled at exec, inherited, the kernel excluded), and its peak resident memory: the prelude's build script, run alone into a fresh `OUT_DIR`, two rounds, alternating with the commit before.

## Stages

1. **An inner arm on an expression restates.** Check: `Holds(k(0))` in both checkers, the variable split unchanged, an inner arm whose key shares no local with the guard restating nothing; `/std` counted against the commit before it, a move past a few percent stopping the stage.

Each stage also takes `cargo xtask workspace lint`.

## Next step

Stage 1, a delivery.

## Retirement

As a stage lands, its rule goes to [Case equations and their key](../../design/soundness/case-equations-and-their-key.md) and it is removed here; once no stage remains and nothing cites this file, it is deleted.
