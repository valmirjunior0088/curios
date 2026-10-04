# A certified sort and an `Ord`-keyed tree

**Not refined yet.** This specification reserves the two members of the collection tier still missing, each waiting for its consumer. It is not an implementation plan.

## The certified sort

`/std/List`'s `sort` is a program — stable by inserting before equals, written as the right fold a later proof can follow, and pinned by properties. The certified sort lands with both halves of its theorem: `Sorted`, decided over adjacent pairs, and a permutation, which has no decided form and is an inductive relation. It needs an antisymmetry hypothesis on the comparator, since `Sorted` alone is satisfied by an empty output and `Ord` cannot carry the law. It opens with a consumer that needs a sorted list proved rather than tested.

## The `Ord`-keyed tree

`/std/Map` is a crit-bit trie whose leaves hold their keys and whose shape is the `Key` encodings `/std/Hash` gives, so `Map(K, V)` and `Set(K)` hand keys back typed and order them by encoding. A tree keyed by `Ord` opens with a consumer whose keys no `Key` encoding serves, and follows the typed-key shape `Map` has: the key type and the dictionary named in the type.

## Refinement

Each item states its consumer, its laws and the proofs it owes, and the `/std` module it lands in.
