# An arm's equation reaches every term conversion holds equal to its scrutinee

[Case equations and their key](../../design/soundness/case-equations-and-their-key.md) has an arm's equation answer the terms conversion holds equal to its scrutinee ([A term is one where conversion says so](../../design/arithmetic/a-term-is-one-where-conversion-says-so.md)). Two readers stop short of that, each in the refusing direction: the filter in front of every equation counts each binder a stuck form names, where conversion reads fewer; and an equation an outer arm settled is not read again under an inner arm on an expression. A question inside a question is no fourth: [A question reduction asks is answered by plain reduction](../../design/soundness/a-question-reduction-asks-is-answered-by-plain-reduction.md) rejects the third reduction it would take.

## What is wrong today

**The filter counts a binder conversion does not read.** `curios_analysis::could_reduce_to` puts a stuck form to an equation only where every binder it names, a binder that is itself a proof apart, is one the key names, since reduction introduces no local. Conversion reads fewer: it reduces an argument under a neutral head before comparing it, and reads no proof wherever one stands. Under `match f(b) < 10`, `f(b + 0 * c) < 10` and `f(Pair { left = b, right = c }.left) < 10`, and under `match w(a, p1) < 5`, `w(a, h(c)) < 5` with `h(c)` a proof that is no variable, each converts with the guard at the top of an item and is refused `Holds` in the guard's `true` arm, at `ef6a83175`; the first is the row `curios`'s `tests::respelling` states refused. The decision states the shortfall as a caveat of the filter's paragraph, where [Writing documentation](../../../.claude/rules/documentation.md) has a gap be a roadmap item.

The filter is no cost to drop. With it admitting every form, counted at `ef6a83175` by the protocol below, `/std`'s build retires 0.94% more, its elaborated Core byte-identical and the objects beside it 12,621 bytes larger; but the stress door retires 4.0×, 10.2× and 12.2× what it retires today at 8, 14 and 20 definitions, and at 20 is refused for its budget. Nothing holds the filter to that cost: the doors `scrutinee_refinement_measurements` times, scrutinized and proved, count the same either way, since their arms hold literals.

**An outer equation is not read again under an inner arm on an expression.** A guard's reduced spelling is settled with the equations inside it withheld, and an inner arm restates the guard only where its case solves a variable the guard reads ([Case equations and their key](../../design/soundness/case-equations-and-their-key.md)). Under `match match h(p) == h(q) | true => k(0) | false => k(1) end`, with `p, q : F(g(c))` and `F(b) = match b | true => {} | false => Nat end`, the inner arm `match g(c) | true => …` makes `p` and `q` units and the guard's scrutinee `k(0)`, and `Holds(k(0))` is refused there, at `ef6a83175`: the guard's spelling, settled without `g(c) = true`, is the stuck match. Split on `c` over `p, q : F(c)`, the same arm restates the guard and the fact holds.

## Settled

- **The reach is conversion's**, the decision's rule as written, with a question inside a question left out.
- **The filter stays, counting a binder only where conversion reads it**: at a rigid occurrence, an argument under a neutral head that holds a binder the key does not reduced plainly before it is counted, and a proof passed over wherever it stands. Its reduction is spent only on a form that names such a binder.
- **An inner arm on an expression restates** each equation in force whose settlement reads a local the arm's key names, read through types and values to the end as `curios_analysis::reads_solved` reads a solution, its instance pushed after the arm's own equation so its settlement sees it.

## Measurement

Counted, user-space instructions retired by a process and its threads (`perf_event_open` on the child, enabled at exec, inherited, the kernel excluded), each run with a fresh `CURIOS_CACHE`: the prelude's build script, run alone into a fresh `OUT_DIR`; and `curios wonder -` over the doors, a web of `N` definitions `r{i} = both(r{i-1}, anyof(r{i-2}, r{i-1}))` over `base` (`x % 2 == 0`) and `other` (`x % 3 == 0`), `both` and `anyof` the pointwise `&&` and `||`, consumed scrutinized and proved as `scrutinee_refinement_measurements` consumes it, and by the stress door, whose `match top(n)` arm states `top(m) || top(k)` and `top(m * 2) && top(k)` each equal to itself and `top(m + k)` equal to `top(k + m)` by `Eq/refl()`, and `Holds(top(n) || top(m))` by `True/qed()`, at `N` of 8, 14 and 20.

## Stages

1. **The filter reads what conversion reads.** Check: the grid's refused row restated held, rows for a projection and a proof term added held; a fixture holding the stress door at 20 within the default budget, which a filter admitting every form refuses; the stress door and `/std` counted against the commit before it.
2. **An inner arm on an expression restates.** Check: `Holds(k(0))` in both checkers, the variable split unchanged, an inner arm whose key shares no local with the guard restating nothing.

Each stage also takes `cargo xtask workspace lint`.

## Next step

Stage 1, a delivery.

## Retirement

As a stage lands, its rule goes to [Case equations and their key](../../design/soundness/case-equations-and-their-key.md) and it is removed here; once no stage remains and nothing cites this file, it is deleted.
