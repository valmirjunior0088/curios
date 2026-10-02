# Coverage

**Assumes.** Every elimination enumerates its constructors, an arm omitted only where its case is provably impossible.

**Evidence.** Probed at the refusing rung and separately at the accepting one — an arm excused because its index target clashes with the scrutinee's, which the standard library relies on across `Str` and `Nat`. `tests::board::an_elimination_must_enumerate_its_constructors` is the refusing rung and `a_proposition_valued_index_cannot_excuse_an_omitted_arm` the accepting one; a family with no constructors exercises only the loop's absence.

- **The `Prop` condition is decided at depth.** Inversion decides it from the family of the values compared, wherever the recursion arrives, so a proposition inside a relevant constructor's payload is refused both as a vacuous elimination and as an omitted arm, while a relevant clash at the same depth still excuses.
- **Each source of `Impossible` is held.** A constructed index against a differing target at a proposition is not provably impossible ([Index inversion and K](index-inversion-and-k.md)); the same pair inside a tuple index decomposes to the variant arm per component, where the guard is, since a tuple arm carries none of its own ([What a proposition may carry](../formation/what-a-proposition-may-carry.md)); and the free-monoid peel answers an empty-window identity *equal*, never a length clash. The free-monoid arms are each checked at their case value.
