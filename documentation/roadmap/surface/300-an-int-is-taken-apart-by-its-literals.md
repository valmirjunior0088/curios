# An `Int` is taken apart by its literals

A column of `Int` takes no literal pattern: `| -1 =>` is refused where it is written, as a signed numeral no pattern takes (`curios-surface`'s `refuse_numeral_pattern`), and `| 1 =>` at an `Int` column by the match compiler, as a numeral at a column that is neither a `Nat` nor a `Byte` (`curios-elab`'s `MatrixLiteralColumn`). A test of an integer's value is a guard, `| i, i == -1 =>`, or a match on the comparison, which `/std` writes eight times, `match v.mantissa == +0 | true => … | false => … end` among them (`/std/Flt`, `/std/Flt/signals`, `/std/Flt/reduction`, `/std/time/iso8601`). Taken apart by its literals, an `Int` would be dispatched as a `Nat` is: each literal a case, a binder the rest, the arm refined to the value it takes.

## What is missing

- **A case split of `Int` in Core.** `curios_core::Cases` splits a `Bool`, a `Nat` by `Switch` or by its successor, a `List` or a `Bytes` by a run, and an inductive by its constructors. An `Int` is an intrinsic carrier with no eliminator, so a dispatch on its values has no Core form, nothing for either checker to type and refine, and no erased form.
- **A signed numeral as a pattern.** The grammar refuses one in a pattern's place; the surface tree, the printer, `from_core`, Core text and the tree-sitter grammar have no node for it.
- **An `Int` column in the match compiler.** `curios-elab`'s `naturals` reads a `Nat`, a `Byte` and a `Char` column; an `Int` column's literals, signed or not, would be read beside them, and a successor pattern refused there.

## Settled

- `Int` literal patterns are wanted. `Flt` literal patterns are not, for the reason [A literal is realized by its expected type](../../design/surface/a-literal-is-realized-by-its-expected-type.md) rejects them, and a `Char` and a `Str` column are read at the field their literal fills, which an `Int`, an intrinsic carrier, has none of.
- A literal is a pattern only at the column whose type its term realizes at ([A literal is realized by its expected type](../../design/surface/a-literal-is-realized-by-its-expected-type.md)): a signed numeral at an `Int` column, and an unsigned one at an `Int` column as at a `Nat` one.
- **The Core form is a case split of its own**, `Cases::IntSwitch`, keyed by `Integer` with a default arm: the twin of `Nat`'s `Cases::Switch`, held strictly ascending as its keys are, each arm refining the scrutinee to its value. It enters the trusted base — the certifier's typing and reduction rule, `recheck`, erasure, and a lowering through ANF and CPS to a branch on an `i31` with the boxed case beside it — as `Nat`'s did. Decided on 2026-11-16.

The next step is stage 1, a delivery.

## Stages

1. **Core and both checkers**: `Cases::IntSwitch`, its typing and reduction, `recheck`, erasure, the lowering and the emitter, each with its tests, and a differential case where reduction is stated twice; its rule written where the theory decisions state a case split's.
2. **The grammar**: a signed numeral as a pattern node, printed and read back, in Core text, and in the tree-sitter grammar with the Zed pin moved.
3. **The match compiler**: an `Int` column's literals as cases, tests put to both checkers, and the `/std` sites above rewritten as matches.

## Rejected

- **A dispatch through a view**, the integer's sign beside its magnitude, as a `Byte` column dispatches on `Byte/to_nat` and a `Char` column on its code point. It would need nothing of the certifier, but it refines the view and never the integer, where `Nat`, the carrier `Int` extends, has its own switch.

## Retirement

Once stage 3 lands, `documentation/language.md` and the literal decision state the `Int` column, and this specification is deleted.
