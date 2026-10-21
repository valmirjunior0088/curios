# A chain of `Nat` or `Int` steps runs in a word

**Not refined yet.** This specification reserves the costs [Nat and Int are an i31 until they outgrow it](../../design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md) accepts, taken back where a program never outgrows a word. It is not an implementation plan.

## What is missing

**A `Nat` or `Int` no bound reaches is a reference between steps.** The representation analysis holds a value in a word wherever everything reaching it lies below `2³⁰`, or below `2³²` for a word read unsigned ([`curios-cont`'s README](../../../curios-cont/README.md#representation-is-decided-wherever-every-party-is-visible)), so a remainder by a literal, a counter counted down from one, a byte taken apart and the fields a scan carries ride words. What it cannot bound pays the costs the decision accepts, a box per result and a tag test per operand, in three places:

- **A counter counted down from an input.** A monus never passes its first operand, but an input is a reference whatever its size, so nothing bounds the seed or what is counted down from it: `programs/lcg`'s `k` and `programs/walk`'s.
- **A value a relation bounds and the bound lattice does not hold.** A UTF-8 character's `partial` takes six bits a continuation byte, at most three times, so it lies below `2²¹`; the lattice sees it multiplied by `0x40` round a loop and bounds it by nothing: `/std/Str/fold`'s `partial`.
- **A `Nat` or `Int` field of a row**, a reference whatever reaches it ([A field is declared at the carrier its shape names](../../design/compilation/a-field-is-declared-at-the-carrier-its-shape-names.md)), whose read `curios`'s `boxed_field_read_measurements` prices.

**Measured**, timed at `e8b8ed2d1`, release, x86-64 Linux, nine interleaved runs each, median and span: each program as written against itself with the value bounded by a literal remainder, which puts it in a word and pays one remainder the program does not ask for.

| Program | Bounded by hand | As written | Bounded |
| --- | --- | --- | --- |
| `programs/lcg`, `N = 100000000` | the seed, `n % 1000000000` | 283.9 ms [283.1–287.9] | 278.1 ms [276.5–283.9] |
| `programs/walk_mirror_baseline.crs`, `N = 1000000` | `partial % 0x200000` | 356.4 ms [348.2–357.8] | 325.4 ms [318.9–327.2] |

## Refinement

- **`Nat` and `Int` past a bound.** Compiling a single-use chain of steps as one 64-bit computation with overflow checks, falling back to the `big/` helpers only on overflow; bounding a value a relation bounds, as the scan state bounds `partial`; and typing a field at a word where every value it can be handed is one. Each is measured on `programs/` before it lands, and a decision records the carrier it leaves. A step that is modular by intent, a generator's or a digest's, is [arithmetic modulo a word](../arithmetic/300-arithmetic-modulo-a-word.md)'s, which weighs this cost against a carrier of its own.
