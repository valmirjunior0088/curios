# A range of literals is a pattern

**Not refined yet.** This specification reserves a pattern naming a range of naturals, `lo..=hi`, beside the literal by which a dispatch names one natural today ([Natural-number dispatch](../../language.md#natural-number-dispatch)). It waits for a consumer: no program is known that a range lets it write, and its one gain is a check the language does not make today. It is not an implementation plan: its lowering is named under *Refinement* and not drawn.

## What is missing

**A dispatch over ranges is an ordered list no check holds.** A natural dispatched by ranges is written as a match with no head whose arms are `Nat/in_range` guards over a subject repeated in each: 34 arms in `/std` — the byte classes of `/std/Str/utf8`, `/std/Str/Valid` and `/std/Tui/Input/decode`, the hex digits of `/std/Char`, `/std/http/read`, `/std/http/Url`, `/std/Toml/numbers` and `/std/Toml/scan`, and a control sequence's final byte in `/std/Tui/tests/render` — and four in each `programs/walk_mirror_*.crs`. Guards are tried in the order they are written ([An arm is taken by its patterns, then its guards](../../design/surface/an-arm-is-taken-by-its-patterns-then-its-guards.md)), so two that overlap are accepted and the first wins: `match | Nat/in_range(n, 0, 10) => 1 | Nat/in_range(n, 5, 15) => 2 | _ => 3 end` runs, and `7` takes the first arm. Literal arms are a set: `match n | 1 => 1 | 1 => 2 | _ => 3 end` is refused, its second arm unreachable past the first. So the disjointness a set of byte classes relies on is held by its author alone, and a final `| _ =>` stands for whatever the ranges leave, though they may leave nothing.

## What is known

- **A range arm learns nothing a guard does not.** An arm guarded by `Nat/in_range(n, lo, hi)` proves `Bool/Holds(Nat/in_range(n, lo, hi))` by `Bool/True/qed()`, and an arm after a lone guard proves its negation, as `/std/Str/utf8`'s proofs read them; a range arm holds those facts and needs no other.
- **Its gain is the set rule.** As a pattern a range is held to the rule patterns are held to: two ranges that meet without either containing the other are refused, one inside another is written as the exception before the general, and coverage is read off the ranges, a range with no upper end covering what the others leave.
- **Only literal bounds make a pattern.** A guard whose bounds are values stays a guard, as `/std/Str/utf8`'s `step` checks a continuation byte against the `lo` and `hi` its scan carries; of the 102 calls of `Nat/in_range` in `/std` and `programs/`, 85 have literal bounds and 54 of those stand as a guard.
- **A character is a natural here**, so `'a'..='z'` is the same pattern over `Char/to_nat`, as a character literal is a dispatch key today.
- **Prior art.** Rust writes `lo..=hi` and decides exhaustiveness over ranges of integers, but takes overlapping ranges in order and lints only two that share an endpoint (`overlapping_range_endpoints`); Swift writes `lo...hi`, and OCaml ranges of characters; Lean, Agda, Idris and Rocq have no range pattern and branch on a decidable condition, as a guard does here. A set of ranges is stricter than each.

## Refinement

- **The lowering, leaned toward and not decided.** A range arm lowers to the guarded arm written today, `Nat/in_range` over the column it stands in, and the elaborator, which already decides which arms a literal leaves unreachable, decides the set rule over ranges and literals together, so Core and the certifier hold nothing new. The alternative, a `Switch` whose keys are intervals, puts an interval's coverage in the trusted base.
- **To decide**: the spelling, `..=` or `...`, and whether an open end is written; how a range meets the rule that a match whose only literal is `0` is a case split; and how the formatter lays a range out.
- **Check**: the overlapping pair above is refused, `/std/Str/utf8`'s classifier is written with ranges and elaborates to the module it does today, and its proofs read the facts they read now.
