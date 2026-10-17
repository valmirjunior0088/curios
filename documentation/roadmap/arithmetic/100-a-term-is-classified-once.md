# A term is classified once

Two documents state rules the reducers' stuck folds break. [A term is one where conversion says so](../../design/arithmetic/a-term-is-one-where-conversion-says-so.md) classifies a term at a carrier "once, from its leaves to its root". [Every stage but reduction costs the graph it is handed](../../design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md) charges no walk to the budget and so holds every walk to the graph it is handed, a walk that outgrows it being "a loop repeating it — a table probed at every level of a spine". A stuck operation is classified again at every level of the spine it stands in, so classifying a sum of n operations, or a tower of n lines, walks n² nodes while the budget counts n, and a sum of n calls of one head puts some n³ pairs to its checker.

## What is wrong

**Each stuck fold classifies everything beneath it.** Both reducers' `refolded` (`curios-elab/src/reduce.rs`, `curios-cert/src/kernel/whnf.rs`) hand out a stuck operation's atoms (`atoms_within`) and take its fold again over them (`refold`, `curios-analysis/src/classify.rs`), each by a walk of every node beneath the operation into a `Classes` dropped when the call returns. A reduction folds a spine from its leaves, so the operation at each level walks the operations below it, each walked at its own level a moment before. The walk charges what it rebuilds and nothing for what it visits, so the budget never sees the repetition. `classable` pairs every non-variable atom with every other, and `Classes::of` puts every atom to its checker against every class it can meet, at every level.

A sum is already written over the chain it extends (`Nat::sum`, `int_sum`), so a reduct's graph is the graph of what was written: what is left is the walk.

| probe | size | looks | held | units |
| --- | --- | --- | --- | --- |
| tower | 10 | 617 467 | 16 200 | 54 493 |
| tower | 20 | 2 349 212 | 63 120 | 97 623 |
| tower | 30 | 5 472 421 | 140 640 | 145 153 |
| sum | 64 | 1 425 930 | 12 474 | 217 266 |
| sum | 128 | 5 132 426 | 49 530 | 560 978 |
| calls | 16 | 265 600 | — | 50 200 |
| calls | 32 | 1 384 960 | — | 113 464 |
| calls | 64 | 8 936 564 | — | 354 424 |

Counted on top of `827a06fc9` with `Nat::sum` and `int_sum` writing over the chain they extend, by the protocol below: the tower's and the sum's looks grow as the square, their units as their lines, and the calls' looks faster still. The calls' stuck folds put 405, 1 581 and 6 237 atoms to their checker at 16, 32 and 64 (`whnf::classable_fold`, three folds a summand), each against every class it can meet; the kernel's `convert::asked_classes` takes 57 ms, 474 ms and 3.76 s of the three compiles, timed in one round at a load of 20 to 40. Six of eight samples taken under gdb at 40 lines of the tower stand in the classification walk under the elaborator's `refolded`.

## Goal

A stuck fold's classification costs what is new at its operation: its operands' classifications are read, not walked again. A tower of n lines and a sum of n operations are classified in looks proportional to n, and a sum of n calls of one head puts each pair to its checker once, in both checkers, with no verdict, spelling, solution or stored byte moved.

## Settled

- **What a classification decides is unchanged.** Atoms classed by the checker's conversion, nodes held to the earlier nodes of their bucket by the readers alone, a representative the first met in the walk's order, a quotient one with the first of its kind over its operands, a stuck fold taken again at every stuck operation once every equation in force has been asked and kept only where it decides more ([A term is one where conversion says so](../../design/arithmetic/a-term-is-one-where-conversion-says-so.md)). A classification read from a table is the one the walk would have taken.
- **The classification calls no judgment.** The atoms are handed back and the checker classes them, as today's three calls do — `atoms_within`, the checker's `asked_classes`, `refold` — and the decision rejects a driver method the chain calls mid-way.
- **A classification is a judgment, filed as one**: by its term, for as long as what it read stands, nothing outliving a declaration, a hit free ([Every stage but reduction costs the graph it is handed](../../design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md), [A reduction step costs what it builds](../../design/soundness/a-reduction-step-costs-what-it-builds.md)).
- **No node is interned where it is built**, as the graph rule decides; a boundary conses (`Sharing`), and a fold hands back what it was given, a sum the chain it extends (`Nat::sum`).
- **No store decides anything the walk does not**: no congruence closure over a goal's terms, no canonical form per class.
- **Persistent maps are `curios-utilities`' own**, sealed, no external crate.

## Design

**A classification is filed by the operation it classifies.** It is the walk's state once the operation's operands are walked: the atoms handed out, indexed by what each can be one with; the classes and their representatives in the order opened; the nodes held, by bucket; the quotients spelled; the nodes walked, by identity; the operation as spelled. Each checker keeps a table from a stuck operation to its classification, in two lives. A classification no conversion class entered reads the term alone — the walk forces nothing (`Standing`) and the readers reduce through nothing — so it lives the declaration. One taken over classes the checker's conversion decided reads what plain conversion read, the equations in force and the types the binders stand at, so it lives as the reducts that name a local do: the kernel's cleared at `Memos::begin_equations`, the elaborator's wherever its scoped reducts are, and not kept across a question it declined.

**What two atoms can be is read off a key.** An atom's pairing key states `may_be_one` once: a variable and a metavariable have none; an application is keyed by its head up to universe instances and its arity, and meets its own key and every atom of no other kind; an intrinsic that reads every operand is keyed by its kind and meets its own; any other atom meets applications and atoms of its own kind. `classable` and `Classes::join` read the keys, so a new atom is put to its checker against the representatives it can meet, the first opened first, and no pair is read at every level.

**A stuck fold extends its operands' classifications.** The walk meets an operation's first operand before anything else and in that operand's own order, so the first operand's classification is exactly the walk's state once that operand's own operands are walked, and the parent takes the operand's node step — held, or handed out as a quotient — on top of it. Every later operand is walked against what stands, which is today's walk: a node the state walked already is met by its identity and not entered, so an operand that holds what came before it — a sum extended by a summand that names the sum — costs its new nodes. Where the operands before a filed classification are small beside it — `n - S`, `7 · Nat/min(…)` — that classification seeds the state instead, and what they make is prepended to it, every key it shares decided as the walk decides it: an atom put to no class it could meet, a node read by `agree` against the later nodes of its bucket, a quotient, a held node and a walked node standing as they stand in both. A prepending a decision would move falls back to the walk from the first operand, paid once for each spelling that moves; an operand with no classification filed is walked.

**A classification is extended without copying it.** The tables hold persistent maps (`curios-utilities`' `Persistent` and `Ordered`), so extending a classification costs the logarithm of what stands and shares what it extends. A classification copied at each level is a read the budget does not count, the square over a spine, and the loop this removes.

## Stages

1. **Classifications extended**, in `curios-analysis`: the classification type, the pairing keys, the extension and the prepending, both reducers' `refolded` reaching them with no classification filed, which is today's walk. Check: a differential, classification by extension against one walk of the whole operation — atoms, representatives, every node's spelling, the refold, what it rebuilt — over sums nested either way, the tower's operations, operands their readers hold one, quotients over one dividend, sums of calls of one head with some pairs one, and shapes whose first operand is small; `every_walk_answers_a_doubling_term_in_its_own_size` in `curios-analysis`.
2. **Classifications filed**, in both checkers' tables. Check: the probes linear in looks by the protocol; a stuck spine reduced alike with the kernel's memos and without (`curios-analysis/tests/driven.rs`); a sum of operations and a sum of calls of one head at a size the per-level loop does not answer at, beside `curios/src/tests/laws/nested.rs`'s fixtures; `a_sixty_line_bounded_tower_compiles`; the prelude image's digest unchanged.
3. **The conversion chain**, should it pay. The chain's pair classification (`atoms_of` and `classed`, `curios-analysis/src/conversion.rs`) and the case equations' (`case_equation.rs`) walk both sides whole, and a congruence descent classifies each operand pair afresh; a side that is a stuck reduct is its own forced form, so its classification is the table's. Taken where the `convert` spans of the protocol's probes and of `/std`'s elaboration show it; otherwise its figure is filed as a finding.

## Protocol

Counted: `term::looks` and `budget::consumed` as the elaborator samples them, and `classify::held`, read off the fold of `curios --profile <stream> wonder diagnostics -` through the release compiler built with `profile`. The probes: the tower of `Shape::Bounded` with `let tower` written `pub let tower` and no final term, at 10, 20 and 30 lines; `let summed(x1: Nat, y1: Nat, …) -> Eq()(Nat/and(x1, y1) + … + Nat/and(xn, yn), Nat/and(xn, yn) + … + Nat/and(x1, y1)) = Eq/refl();` over `use /std/{Nat, Eq, Io};` and a final `Io/pure(())`, at 64 and 128, and with each reversed operand swapped as well; and the same claim over `g(x1) + … + g(xn)` for `g: (Nat) -> Nat`, at 16, 32 and 64.

## Charges

A classification read from a table charges nothing, a hit being free, where each level today charges again the nodes its walk rebuilt and the readers' reads inside it. Charges only fall, alike in both checkers, so `tests::runaways`' fixtures hold; `runaway_measurements`, `nested_operation_measurements`, `tower_measurements` and the parity measurements are figures the stages may move.

## Rejected

- **Refolding only where a reduction is asked from outside**, one walk per reduction: an operand's reduct would then follow whether it was reached as an operand or at the root, and the rule settles where a stuck fold is taken again.
- **One classification per scope that every fold writes into**: a representative would be whatever was classified first in the scope, so a reduct's spelling would follow the order terms were reduced in.
- **Representatives by rank, so classifications join in any order**: it moves the settled rule's representative and every spelling written from one, where joining in the walk's order is exact.
- **A classification kept on the node, as its hash is**: it outlives its declaration, so a hit would let a neighbour pay for the readers' charged reads.
- **Interning every node where it is built**: a lookup at every node any stage builds, for what a fold gets by handing back the node it was given.
- **An external persistent-collections crate**: a dependency to pin and seal for one module's worth of tree.

## Retirement

The arithmetic decision's "classified once" states that a classification is filed by its term and extended by the fold above it, the graph rule's list of judgments gains the classification, and the tables' lives stay in the rustdoc of `Caches` and the kernel's `memos`. `curios/src/tests/laws/nested.rs` and anything else citing this specification are updated, and it is deleted.
