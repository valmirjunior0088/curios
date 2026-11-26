# A tuple's witnesses are derived

**Not refined yet.** `/std/Tuple` writes by hand, for each number of fields, the witnesses the derivation writes for a struct: 2,632 lines at `52beb4c27`. It waits on its first stage, the struct's path tried at a tuple key.

## What is wrong today

- `curios-prelude-build/std/Tuple.crs` holds `Eql` for tuples of one to eight fields with a congruence helper for each (`same1` to `same8`), 670 lines, and `Ord` for the same eight with its five laws, 1,683 lines, each a comparison of projections and its laws proved a field at a time, as `curios-elab/src/derive.rs` writes a struct's (`congruent`, `Pieces`).
- Its `Hash` and `Hash/Key` for the tuples of zero to eight fields are written the same way, each encoding the one a derivation writes for a struct, and each key's `mono` the chain of `Hash/Key/then` and `last` a derived key's is. A derived tuple key would ask the derived order, which the written one is not, so the two would derive together.
- [A witness body may be written by the compiler](../../design/surface/a-witness-body-may-be-written-by-the-compiler.md) derives from "a registered `induct` or `struct`" and says tuples "take `/std/Tuple`'s per-arity ones", giving no reason; the laws are what made the per-arity witnesses long.
- The derivation reads its key as a registered `induct` or `struct` alone (`subject`), a tuple's type being a node of its own, and the struct's path is entered at nine places, reading a field by the struct's projection and building a value in two. From a program, the orphan rule refuses a tuple's witness before the derivation is asked.

## Open

- Whether a tuple key, whose telescope its type states, is read by the struct's path with a tuple's projection in the structure's place.
- How `Spell` and `Hash` spell a value with no name.

## Stages

1. **The struct's path at `{A, B}`.** Tried over a copy of the tree, the fix then stated with the lines it removes, or this specification dropped. Check: `satisfy (@A: Type, use Eql(A), @B: Type, use Eql(B)) => Eql({A, B}) { .. }` and its `Ord`, `Hash` and `Hash/Key` accepted by both checkers in the copy.
2. **`/std/Tuple` derives.** Each per-arity witness replaced by a derived one. Check: `/std` builds and `Tuple`'s tests pass.

## Verification and retirement

Retired once `/std/Tuple` derives its witnesses, with the derivation decision stating that a tuple key derives.
