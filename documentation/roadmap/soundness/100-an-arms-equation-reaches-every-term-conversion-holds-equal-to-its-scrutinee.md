# An arm's equation reaches every term conversion holds equal to its scrutinee

[Case equations and their key](../../design/soundness/case-equations-and-their-key.md) has an arm's equation answer the terms conversion holds equal to its scrutinee ([A term is one where conversion says so](../../design/arithmetic/a-term-is-one-where-conversion-says-so.md)). Two readers stop short of that, each in the refusing direction: the filter in front of every equation reads a proof conversion does not read; and an equation an outer arm settled is not read again under an inner arm on an expression. A question inside a question is no fourth: [A question reduction asks is answered by plain reduction](../../design/soundness/a-question-reduction-asks-is-answered-by-plain-reduction.md) rejects the third reduction it would take.

## What is wrong today

**The filter reads a proof conversion does not read.** `curios_analysis::could_answer` passes over a binder that is a proof and an application of one, and reads past a binder the reduction of an argument erases, but reads any other proof as it reads any term, where conversion compares a part at a proposition by irrelevance and reads neither side. Under `match w(a, p1) < 5`, `w(a, lemma(a, c, p1)) < 5`, with `lemma(a, c, p) = match c | 0 => p | _ => p end` a proof of the bound stuck on `c`, converts with the guard at the top of an item and is refused `Holds` in the guard's `true` arm, at `756dcb782`. The decision states the shortfall as a caveat of the filter's paragraph, where [Writing documentation](../../../.claude/rules/documentation.md) has a gap be a roadmap item.

**An outer equation is not read again under an inner arm on an expression.** A guard's reduced spelling is settled with the equations inside it withheld, and an inner arm restates the guard only where its case solves a variable the guard reads ([Case equations and their key](../../design/soundness/case-equations-and-their-key.md)). Under `match match h(p) == h(q) | true => k(0) | false => k(1) end`, with `p, q : F(g(c))` and `F(b) = match b | true => {} | false => Nat end`, the inner arm `match g(c) | true => …` makes `p` and `q` units and the guard's scrutinee `k(0)`, and `Holds(k(0))` is refused there, at `ef6a83175`: the guard's spelling, settled without `g(c) = true`, is the stuck match. Split on `c` over `p, q : F(c)`, the same arm restates the guard and the fact holds.

## Settled

- **The reach is conversion's**, the decision's rule as written, with a question inside a question left out.
- **A proof is passed over wherever it stands**: a part the position it stands at types as a proposition — a spine's argument by its head's telescope, a constructor's payload and a struct literal's field by the declaration's, an intrinsic's operand by its signature — as conversion types it, each checker answering by its own lookup and sort.
- **An inner arm on an expression restates** each equation in force whose settlement reads a local the arm's key names, read through types and values to the end as `curios_analysis::reads_solved` reads a solution, its instance pushed after the arm's own equation so its settlement sees it.

## Measurement

Counted, user-space instructions retired by a process and its threads (`perf_event_open` on the child, enabled at exec, inherited, the kernel excluded), each run with a fresh `CURIOS_CACHE`: the prelude's build script, run alone into a fresh `OUT_DIR`; and `curios wonder -` over the doors, a web of `N` definitions `r{i} = both(r{i-1}, anyof(r{i-2}, r{i-1}))` over `base` (`x % 2 == 0`) and `other` (`x % 3 == 0`), `both` and `anyof` the pointwise `&&` and `||`, consumed scrutinized and proved as `scrutinee_refinement_measurements` consumes it, and compared, as `curios`'s `tests::unfolding` consumes it (`Consumed::Compared`), at `N` of 8, 14 and 20.

## Stages

1. **A proof is passed over wherever it stands.** Check: a row of `curios`'s `tests::respelling` respelling `w(a, p1) < 5` through a proof stuck on another binder, held in both checkers; the compared door and `/std` counted against the commit before it.
2. **An inner arm on an expression restates.** Check: `Holds(k(0))` in both checkers, the variable split unchanged, an inner arm whose key shares no local with the guard restating nothing.

Each stage also takes `cargo xtask workspace lint`.

## Next step

Stage 1, a delivery.

## Retirement

As a stage lands, its rule goes to [Case equations and their key](../../design/soundness/case-equations-and-their-key.md) and it is removed here; once no stage remains and nothing cites this file, it is deleted.
