# A chain of `Nat` or `Int` steps runs in a word

Working specification for holding a `Nat` in a machine word where a program never outgrows one and the representation analysis cannot see it, in the three places the analysis leaves a reference today. It takes back costs [Nat and Int are an i31 until they outgrow it](../../design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md) accepts, under that decision's one runtime form: what crosses a position is an i31, a `$big` or a word every value reaching it fits. Its next step is a bounded investigation, the census under *Stages*, which ends in the choice of the places taken.

## What this builds on

- **A bound is a power of two.** `Bound` (`curios-cps/src/bound.rs`) states what a natural lies below, up to `2³²`, past which it bounds nothing a register reads; an operation's result is `Intrinsic::result_bound`'s (`curios-cps/src/intrinsic.rs`): a sum one bit past its wider operand, a product the sum of their bits, a remainder by a literal below that literal, a monus and a quotient their first operand's, a byte below `2⁸` and a length below `2³²`. No `Int` operation has a bound, so an `Int` is a reference wherever it travels.
- **Representation reads the bound.** `represent.rs` holds a `Nat` in a word, `Repr::Word`, where everything reaching it lies below `2³⁰`, and in a word read unsigned, `Repr::Length`, below `2³²` ([`curios-cps`'s README](../../../curios-cps/README.md#representation-is-decided-wherever-every-party-is-visible)); elsewhere it is `Repr::Number`, a reference the emitter takes apart at every operation, a tag test an operand, the i31 fast path, and a `big/` helper past it.
- **A field is a reference.** A `Nat` or `Int` field is declared at `(ref null any)`, an i31 or a `$big` ([A field is declared at the carrier its shape names](../../design/compilation/a-field-is-declared-at-the-carrier-its-shape-names.md)); `curios`'s `boxed_field_read_measurements` (`curios/src/tests/codegen/shapes.rs`) prices a read at a median 2.25 ns, a seventh of a dispatch-heavy fold's element (at `9c543476a`).
- **A walk is not split by its scan state.** Per-tag call-pattern specialization of the scan state is declined on the specializer's growth budget (`step_specialization_extent`, [A value costs when it is kept, not when it is named](../../design/compilation/a-value-costs-when-it-is-kept-not-when-it-is-named.md)).

## The gap

**A `Nat` or `Int` no bound reaches is a reference between steps**, a box per result and a tag test per operand, in three places:

- **A counter counted down from an input.** A monus never passes its first operand, but an input is a reference whatever its size, so nothing bounds the seed or what is counted down from it: `programs/lcg`'s `k` and `programs/walk`'s.
- **A value a relation bounds and the bound lattice does not hold.** A UTF-8 character's `partial` takes six bits a continuation byte, at most three times, so it lies below `2²¹`; the lattice sees it multiplied by `0x40` round a loop and bounds it by nothing: `/std/Str/fold`'s `partial`. The relation is between its bits and the scan's count: `bits(partial) + 6·rem` is at most 21 at every step, a coefficient past the unit ones an octagon holds.
- **A `Nat` or `Int` field of a row**, a reference whatever reaches it. `/std/Map/internal/Node`'s `crit` is read at every level of a descent and taken apart by `/std/Map/bits/bit` (`crit / 9`, `crit % 9`) behind a tag test each: `bit` takes 19 of 85 samples of `programs/spines` at N = 15 000 000 (gdb over the `profile` feature's perf map, at `2c7bbb7d5`, a debug build whose collector runs Rust's debug assertions, so emitted code's share is if anything understated). A crit is a byte's position times nine and a bit, and a position is known only below `2³²`, so not even a field typed by everything stored in it reaches this one; a position held in a field of its own is one such a field could hold at a `Repr::Length`.

**Measured**, timed at `e8b8ed2d1`, release, x86-64 Linux, nine interleaved runs each, median and span: each program as written against itself with the value bounded by a literal remainder, which puts it in a word and pays one remainder the program does not ask for.

| Program | Bounded by hand | As written | Bounded |
| --- | --- | --- | --- |
| `programs/lcg`, `N = 100000000` | the seed, `n % 1000000000` | 283.9 ms [283.1–287.9] | 278.1 ms [276.5–283.9] |
| `programs/walk_mirror_baseline.crs`, `N = 1000000` | `partial % 0x200000` | 356.4 ms [348.2–357.8] | 325.4 ms [318.9–327.2] |

## Prior art

- **Lean 4's `Nat`** is this decision at 63 bits: a scalar tagged in the low bit while it lies below `SIZE_MAX >> 1`, an inline fast path when both operands are scalars, and a big-number call otherwise (`lean_nat_add`, [`lean.h`](https://raw.githubusercontent.com/leanprover/lean4/master/src/include/lean/lean.h)).
- **Interval range analysis** removes overflow checks where a value's interval stays inside the machine's: Rodrigues, Campos and Pereira's analysis for LLVM, which reads a value's future uses to stay precise cheaply ([CGO 2013](https://researchr.org/publication/RodriguesCP13)); this compiler's `Bound` is the interval lattice with its lower end fixed at zero and its upper end a power of two.
- **Relational domains** bound one variable by another: the octagon holds `±x ± y ≤ c` at a quadratic cost an element ([Miné, Higher-Order and Symbolic Computation 19(1), 2006](https://arxiv.org/abs/cs/0703084)), and a coefficient of six, as `partial`'s, needs the polyhedra past it.

## Decisions

- **No 64-bit runtime form.** A value between positions keeps the decision's two forms and the words the bound admits; a chain's intermediate may be a 64-bit register, never a field or a parameter ([Nat and Int are an i31 until they outgrow it](../../design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md), Rejected).
- **A step modular by intent is [arithmetic modulo a word](../arithmetic/301-arithmetic-modulo-a-word.md)'s**, which weighs this cost against a carrier of its own.
- **Each place is measured on `programs/` before it lands**, and a decision records the carrier it leaves.

## Open questions

- **How `partial` is bounded.** A relational fact between a loop's value and its counter, polyhedral by the coefficient; a copy of the walk per scan state, so each copy's bound is a constant, which the call-pattern specializer declines today; or `/std/Str/fold` writing the remainder the measurement wrote, a spelling of what the analysis should earn. Settled by a prototype of the first two over `programs/walk_mirror_baseline.crs`, measured against the hand-bounded figure above.
- **Whether a field is typed by what is stored in it.** A slot every construction of its row stores a word in, no host building the row, declared at an `i32`: a revision of [A field is declared at the carrier its shape names](../../design/compilation/a-field-is-declared-at-the-carrier-its-shape-names.md), and a pass in `curios-cps`, where the bound is, over rows the ANF door has already laid out. What it reaches is unknown: `Node`'s `crit` is outside it, as above.
- **Where a chain runs in 64 bits.** A single-use chain whose operands are references, computed in an `i64` behind one tag test an input and boxed once at its end, in the emitter, whose machine lowering sees a chain's uses, or in `curios-cps` behind a register of its own. No consumer is named: the chains of the programs measured here are bounded by a remainder already.
- **Whether a counter counted down from an input is taken at all.** It costs `programs/lcg` 2% above, and the one way to give it a word, a loop copied by a test of its seed, is declined in [`curios-cps`'s README](../../../curios-cps/README.md#representation-is-decided-wherever-every-party-is-visible). Recommended: out of scope unless the census finds more.

## Stages

1. **A census of what each place would hold in a word**, over `programs/` and `/std`'s tests: for each, the positions it would move from `Repr::Number` and the operations a run would stop taking apart, counted beside the timings of the programs it reaches. It ends in the choice of the places taken, each with its design from the questions above. Check: the census as a test of its own beside `curios`'s codegen census, counted and mixed figures named as such.
2. **One stage for each place chosen**, in the order of its measured gain. Check: the stage's program in `programs/` holds the value at `Repr::Word` or `Repr::Length` (a `represent` unit test and a codegen pin), the differential grid in `curios/src/tests/numeric` across the i31's edge, and the program's timing retaken against the hand-bounded figure.

## Verification

`cargo xtask workspace lint` and each stage's tests by name, with the corpus tests that reach `curios-cps` and `curios-emit`, `curios-prelude`'s, and `curios`'s `tests::numeric` for any change to an operation's lowering. The full gate runs once, after the last stage.

## Rejected

- **A 64-bit middle tier as a runtime form**, the carrier decision's own rejection: a third form in every test for a range the limbs cover.
- **A loop versioned by a test of its seed**, declined in [`curios-cps`'s README](../../../curios-cps/README.md#representation-is-decided-wherever-every-party-is-visible): it copies the loop.

## Completion and retirement

The carrier each place leaves goes to the decisions it revises, [Nat and Int are an i31 until they outgrow it](../../design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md), [A field is declared at the carrier its shape names](../../design/compilation/a-field-is-declared-at-the-carrier-its-shape-names.md) or [`curios-cps`'s README](../../../curios-cps/README.md#representation-is-decided-wherever-every-party-is-visible), and the census and the stages' tests stay. Once those are recorded and no reference to this specification remains, this file is deleted.
