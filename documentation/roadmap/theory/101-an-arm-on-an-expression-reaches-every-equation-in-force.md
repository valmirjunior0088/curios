# An arm on an expression reaches every equation in force

Within an arm the scrutinee is definitionally the case's value, and an equation in force outside the arm holds inside it ([Case equations and their key](../../design/soundness/case-equations-and-their-key.md)). An arm on an expression restates only the equations whose settlement meets its key whole, read off their spellings as written or off the type or definition of a local they read (`curios_analysis::meets_one_of`, read by `curios-cert`'s `Scope::restate` and `curios-elab`'s `Context::equations_read_by`). Every other equation keeps the spelling it settled to outside the arm and is compared with the arm's equation withheld, so a guard's fact the arm's equation reaches any other way is refused, where the same matches nested the other way accept it. Shown at `c47494dfd`, each under a guard whose `true` arm matches again and states the fact in the innermost `true` arm:

- the guard a call: `pick(e, k, c)`, for a `pick` whose body is `match e(c) | true => k(0) | false => k(1) end`, then `match e(c)` and `Holds(k(0))`;
- the arm's key a call: that match written as the guard, then `match test(e, c)`, for a `test` whose body is `e(c)`, and `Holds(k(0))`, or the guard's own fact as written, whose `e(c)` the arm's equation answers;
- the guard's call in an argument conversion compares whole: `m(pick(e, k, c))`, then `match e(c)` and `Holds(m(k(0)))`;
- the arm's key respelled: `match a + b < 10 | true => k(0) | false => k(1) end` as the guard, then `match b + a < 10` and `Holds(k(0))`, or the guard's own fact as written;
- the guard meeting two arms in turn: `chain(e, j, k, c)`, for a `chain` whose body matches on `e(c)` and in its `true` arm on `j(c)`, then `match j(c)` and `match e(c)` in either order, and `Holds(k(0))`.

No program of `/std` is known to meet any of them.

The next step is stage 1.

## Decided

Settled by the user on 2026-11-20.

- **A refusal the theory does not make is a defect whether or not a program meets it**, so this waits on a design and not on a consumer.
- **No test chooses what an arm on an expression reaches: under it, every equation in force is settled again.** Which equations the arm's own would move is known only by settling them under it, and nothing is settled until a probe asks. A test on spellings as written misses the programs above; one on spellings as settled would settle every equation in force where an arm is entered, which the two-tier key exists to defer, and would be read by two checkers whose spellings settle at different moments; and one on the locals the key names reaches nearly every guard of a proof all the same. With none, the two checkers have no rule to differ by.
- **An equation is settled again from its own spelling**, the one its key reduced to with nothing inside it in view, settled first where no probe has asked for it. Reducing that on with the arm's equations in view is the key's settlement there, and where they answer no stuck part of it, one walk of a term already stuck. Settled from the key as written, a proof unfolds every guard again at every arm inside it. The proof `Int/DivMod/unique`, which `/std` held until `048516fdd`, compiled as a program of its own, costs its heaviest declaration 403,954 units as the tree stands and 500,650 so settled, and runs out of the certifier's budget of 30,000,000 settled from its keys (counted, the least `--budget` under which `wonder` accepts it, at `c47494dfd` with the rules prototyped).
- **An equation keeps its place and its value: an arm inside moves only what it is spelled as.** The equations answer innermost arm first, and an arm's own answers before any recorded outside it, whatever that one was settled under. An arm that restates a copy of an outer equation after its own lets the copy answer first, which shows in an arm no value reaches, where the two disagree: under `match +0 <= y | false => …`, a second `match +0 <= y` holds `Holds(+0 <= y)` in its `true` arm as the tree stands, and refuses it with every equation restated so. The proofs the elaborator writes for a bound rest on it, splitting a conjunction by a match on a comparison an arm around it decided, and `/std/http/Field/Value`'s `fixdate` stops elaborating without it.
- **It is settled in view of every equation in force but itself.** A guard that meets one arm's key and then another's needs both: settled with the arm's own equations in view and its neighbours withheld, as the elaborator settles a restated equation today, the last program above stays refused.
- **The recorded equation holds the spelling**, one for each height it is asked at, a height being the equations in view: settled in a view that withholds that equation alone, the others answering at the height below. No copy of it is pushed, none steps aside, and the elaborator opens no frame to restate in. Prototyped, it holds the five programs, two arms deep in either order, the unreachable arm and `/std`'s build, and refuses their near misses. A copy pushed after the arm's own, as an arm restates one today, answers ahead of it and reads what its position gives it, so each rule above is a patch on one: prototyped so, the copy answering at its recorded equation's place and settled from that one's spelling, the first four programs and `/std`'s build hold, and two arms deep is refused.
- Ground completion re-simplifies a rule a new rule rewrites, from the rule as it stands and never from the equation it began as (Bachmair, Tiwari and Vigneron's [abstract congruence closure](https://www.csl.sri.com/~tiwari/html/jar00.html)). Agda's `with` abstracts the occurrences visible where it is written and leaves the ones that appear later to the reader ([with-abstraction](https://agda.readthedocs.io/en/stable/language/with-abstraction.html)), which is the test on spellings as written.

## Stages

1. **The refusals as tests.** Rows of `curios/src/tests/respelling.rs`'s second-match grid for the five programs, stated refused in the arm and held with the matches nested the other way; near misses stated refused, the fact of the other case, an arm on another expression and the guard's `false` arm; and the second match in the arm no value reaches, held.
2. **The certifier holds a spelling for each height.** Each of `Scope`'s equations holds one, its view withholds one equation beside its limit, and an arm on an expression restates nothing. No verdict moves: the elaborator still refuses what the certifier would now accept.
3. **The elaborator holds a spelling for each height**, and the restating frame and `meets_one_of` go with their tests: the five rows turn held, the near misses stay refused and the unreachable arm stays held, the division proof above joins `curios`'s `tests::runaways` at a budget it answers inside, and [Case equations and their key](../../design/soundness/case-equations-and-their-key.md) is restated.

## Verification

- Each stage's rows and near misses on the side stated, each mutation-checked: settled from the key, the division proof runs out of its budget; answering by the arm that settled it, the unreachable arm and `/std`'s build are refused; its neighbours withheld, the fifth row is.
- `/std`'s build counted in user-space instructions retired against the tree as it stands, every proof of it within the default budget.

## Retirement

When stage 3 lands, this file is deleted: the rule is the decision's.
