# A map intersects and subtracts

**Not refined yet.** This specification reserves `intersection` and `difference` over `/std/Map` and `/std/Set`, with their laws beside `Map/try_get` and `Set/has`. It is not an implementation plan. No module of `/std` consumes either yet; the first consumer would be a program that compares two key sets.

## What is missing

**`/std/Map` and `/std/Set` take a union and nothing else of two maps.** `Map/union` and `Set/union` combine two of one type; the entries of one map whose keys the other has, or lacks, are a `Map/filter` over `Map/has` a program writes itself, with no law stating what it answers.

## What is known

- **The laws read off `try_get`.** `try_get(intersection(a, b), k)` is `try_get(a, k)` where `b` has `k` and none otherwise, and `difference` the converse, the form `Map/try_get/of_filter` already proves of a filter, so either may be that filter at first and its laws a corollary of `of_filter`.
- **A walk of both tries is [A union walks two tries once](201-a-union-walks-two-tries-once.md)'s**, and each operation is a variant of the merge it reserves.

## Refinement

- **The names and their value types**: which map's values an intersection keeps, or a combining function's, as `union` keeps the second's.
- **Check**: the laws proved beside `Map/try_get` and `Set/has`, and a test in `curios-std`'s tests of each.
