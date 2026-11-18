# A union walks two tries once

**Not refined yet.** This specification reserves building a map from two maps, or from one map's chosen entries, by one walk of their tries, where `/std/Map` inserts entry by entry today. It is not an implementation plan. No program measured spends its time there yet: `Map/union`'s one consumer in `/std` is `Set/union`, and no benchmark of `xbench` or program of `programs/` calls either.

## What is missing

**`union`, `filter` and `rekey` insert entry by entry.** Each is a `Map/fold` over one map's entries that inserts into the map it builds (`curios-prelude/std/Map.crs`): `union(a, b)` walks `b` and inserts each of its entries into `a`, each insertion a walk from `a`'s root that, where its key's twig is empty or a leaf holds another key, encodes a key it finds there to learn the position the two keys part at; `filter(m, pred)` builds its answer from the empty map the same way, though what it keeps is already a trie of the shape it builds; and `rekey` inserts every entry under its new dictionary, the one of the three a structural walk cannot spare, since its trie's shape is another dictionary's. A union of two maps of `n` and `m` entries is `m` insertions into a map of up to `n + m`. `intersection(a, b)` and `difference(a, b)` are filters of `a` that ask `has(b, k)` at each of its entries, a walk of `b` from its root per entry, where a walk of both tries at once meets each of `b`'s branches once.

## What is known

- **A merge walks both tries at once.** Okasaki and Gill's merge of two big-endian Patricia trees compares the two roots' branching bits and prefixes and descends into the one side the other belongs to, or merges side by side where they branch at one bit, in time linear in the two sizes, where inserting one map's entries into the other costs a root-to-leaf walk per entry (*Fast Mergeable Integer Maps*, Workshop on ML, 1998), the merge Haskell's [`Data.IntMap`](https://hackage.haskell.org/package/containers/docs/Data-IntMap.html) implements. A branch of `/std/Map`'s trie holds the position it forks at and no prefix, so a merge reads a twig's prefix off its first key's encoding, as `Node/insert` reads where a key parts from a branch, and two branches at one position merge twig by twig, each of the sixteen pairs a merge of its own.
- **The laws are stated already.** `Map/try_get/of_union`, `of_filter`, `of_intersection` and `of_difference` say what each answers at every key, so a merge is held to them and extensionality (`Map/try_get/injective`) makes any two implementations that meet them one map.
- **A filter keeps a trie's shape.** The entries a predicate keeps are a trie of the branches above them, each left one twig collapsed to it and each left none gone, which a walk builds without encoding a key.

## Refinement

- **The walk**: a merge of two sound nodes with its soundness proof built in its own recursion, as `Node/insert` builds its outcome's, and a filter that empties a branch's twigs without re-inserting.
- **Check**: `Map/try_get/of_union`, `of_filter`, `of_intersection` and `of_difference` proved of the new definitions; a program where a union dominates, timed against the fold it replaces before it lands.
