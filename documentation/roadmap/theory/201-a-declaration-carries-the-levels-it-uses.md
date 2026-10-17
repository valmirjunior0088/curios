# A declaration carries the levels it uses

**Waits for a consumer**: no `/std` signature names a method by its path at a level.

A declaration's universe parameters are the levels its signature holds where conversion compares two instances of it, read by the one alignment conversion reads two terms by ([A universe level is implicit, cumulative, and settles by where it stands](../../design/theory/a-universe-level-is-implicit-cumulative-and-settles-by-where-it-stands.md)). One reading still holds a level no two instances can be told apart by: a method named by its path, whose witness the alignment reads whole.

## What is wrong

**A method named by its path holds every level of the witness it is applied to.** A projection of a definition applied in full is read by the vector of the field it projects, so an operator, which elaborates to the projection, holds of a witness only what the method's value holds. A method named by its path elaborates to its wrapper applied to the witness instead, and the alignment reads that dictionary argument whole: over `concept Sized(T: Type): Type { Carrier: Type }` and `satisfy Sized(Nat) { Carrier = Str }`, `pub let named(x: Sized/Carrier(@Nat)) -> Nat = 0;` takes one parameter where the type is `Str`.

## Settled

- **Members of one cycle share their levels.** A top-level group is one cycle of what its members reach ([A declaration is a function of what it reads](../../design/compilation/a-declaration-is-a-function-of-what-it-reads.md)), and a member that reaches another takes that member's levels under any scheme, so in a cycle a level is every member's: `/std`'s parser groups (`Json/decode`, `Html/parse`, `Toml/values`) keep the level of the parser one member names. A level the members of a cycle hold for one another is one conversion tells their instances apart by, under the rule that two instances of a group are a level question ([Conversion recurrence](../../design/soundness/conversion-recurrence.md)); what would release it is a vector on a recursive member read by both checkers or level-polymorphic recursion, each a change to the trusted base that no program here asks for.
- **A dictionary a family takes as an argument is held.** `Parse`'s node keeps its `use Input(I)` argument and `Parse` is invariant in `Input`'s proposition level, so a parser at one level is no parser at another, and the level a `/std` parser's signature takes through `/std/Parse/text`'s witness stays.
- **A function's body is read as written.** An application stuck on a binder keeps the motives its body states, so two instances apart in a level those name are two terms; a value's levels read by its reduct would release nothing a stuck call does not show.

## Stage

1. **A method applied to its dictionary is read as the field it projects**, as a former applied in full is read as the node it builds, so an operator and a method named by its path are one reading. Check: `named` takes no parameter.

## Retirement

When the stage lands, the universe decision states the reading of a method named by its path, and this specification is deleted.
