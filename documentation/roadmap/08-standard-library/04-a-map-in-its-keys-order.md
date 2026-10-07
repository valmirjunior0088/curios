# A map in its keys' order

**Not refined yet.** This specification reserves what stands between `/std/Map` and its keys' own order: the two key types whose encoding is not in their order, the operations an order would give a map, and a map for a key that has an order and no encoding. It records what is known, the two directions and what each waits on. It is not an implementation plan.

## What is known

- **`Map` enumerates in the order of its keys' encodings.** It is a crit-bit trie over the bytes `/std/Hash` gives each key, a proper prefix first and then byte by byte (`bit`, `curios-std/src/Map.crs`), which is the order `Bytes/ord` answers. `fold`, `entries`, `keys`, `Show` and `Spell` all follow it, and `Set`'s with them.
- **Five of the seven key types are already in their own order, and `Nat` and `Int` are not.** Counted at `bf21a9065` by building a map and comparing `Map/keys` with `List/sort` of it: keys of `Byte`, `Bool`, `Bits`, `Bytes` and `Str` come out as their `Ord` puts them; `Map(Nat, V)` over `256, 1, 2, 65536, 255, 257, 0, 513` enumerates `[0, 65536, 256, 1, 257, 513, 2, 255]`, and `Map(Int, V)` over `-2` to `+2` enumerates `[+0, +1, +2, -1, -2]`. `Hash(Nat)` is the minimal encoding with its least significant byte first, and `Hash(Int)` a sign byte before it. `Str`'s agreement holds by definition, `Str/ord` being `Bytes/ord` over a text's bytes.
- **A map has no operation that reads an order**: no least or greatest key, no keys between two.
- **Many types have an order and no encoding proved injective.** `Key` is witnessed at `Bytes`, `Str`, `Nat`, `Byte`, `Bool`, `Bits` and `Int`. `Char`, `List`, `Option`, `Result`, the tuples, `Dyadic` and every type with a derived `Ord` have an `Ord` and no `Key`; `Hash` derives and `Key` does not; and `Flt`'s witness is withheld on purpose (`Map.crs`).
- **Nothing consumes either.** `Map` is read by `/std/Toml` alone, at `Str`, and `Set` by nothing. `/std/Async` keeps its sleepers in a list and walks every registration on each idle turn, so an ordered queue of them would change no cost.
- **A function under both a `Key(K)` and an `Ord(K)` premise is refused by the kernel**, as [a finding](../02-theory/00-findings.md) says, which is the signature the first direction's law is stated in.

## The two directions

**The encodings in order, first.** `Hash(Nat)` and `Hash(Int)` become encodings whose bytes compare as the numbers do — a length before a magnitude written most significant byte first, the sign folded in so that the negatives come first — and the map that exists then enumerates every key type in its own order. It stays one trie with one shape for one set of entries, which [`curios-std`'s README](../../../curios-std/README.md#map-is-a-crit-bit-trie-and-its-shape-is-its-api) makes its API, and the order operations are walks of it. What it costs:

- `Key(Nat)`'s injectivity proved anew, and a law that an encoding is monotone, `Ord/ord(a, b)` being `Bytes/ord` of the two encodings. From that law and injectivity a key's order has its flip, its transitivity and its agreement with `Eq` by `Bytes/ord`'s own, proved once.
- The encoding `/std/Hash` documents as stable changes, for every value that holds a `Nat` or an `Int`.
- A `Nat` key grows by its length, a cost on `Map(Nat, V)` that `curios`'s `tests::codegen::map_wall` measures.
- A composite key needs each part's encoding to end where it ends: a text is a prefix of a longer one, so a pair of texts takes a terminator and an escape.

**A tree keyed by a comparator, for a consumer.** A key with an order and no encoding, or an order no encoding gives, takes a balanced search tree. It follows the typed shape `Map` has, the key type and the dictionary named in the type, and the dictionary it names is a lawful order as `Map`'s is a lawful encoding: under a comparator that is not transitive a search tree loses entries with no word, the failure `Key`'s law exists to exclude from the trie. Its shape follows its history, so two trees of the same entries are equal by their traversal and not by their nodes. Open with it: which balance, where the invariants live, and what is proved.

## Prior art

- **A trie over keys that compare as bytes is an ordered map.** The adaptive radix tree's authors define a binary-comparable key as a transformation under which `x < y` exactly when the two byte strings compare so, give it for unsigned and signed integers, floats, strings and compound keys, and note that a string must end in a value found nowhere else in it, since no key may be a prefix of another ([The Adaptive Radix Tree](https://db.in.tum.de/~leis/papers/ART.pdf), section IV). FoundationDB's tuple layer is the worked encoding: a type code that carries an integer's length and sign, a negative as its complement, and a byte string closed by a zero byte with its own zeros escaped ([`design/tuple.md`](https://github.com/apple/foundationdb/blob/main/design/tuple.md)). Haskell's `IntMap` is a big-endian Patricia trie that offers the least and greatest key, the neighbours of a key and ascending folds ([`Data.IntMap`](https://hackage.haskell.org/package/containers-0.7/docs/Data-IntMap-Strict.html)).
- **One shape per set of entries is what makes a map extensional.** Appel and Leroy's canonical binary tries give CompCert maps that are equal where their bindings are, and so are rewritten by plain equality ([Efficient Extensional Binary Tries](https://arxiv.org/abs/2110.05063)).
- **A map that only compares cannot have one shape and logarithmic operations.** Andersson and Ottmann show a uniquely represented dictionary over an ordered universe costs Θ(n^(1/3)) for a search or an update ([New tight bounds on uniquely represented dictionaries](https://user.it.uu.se/~arnea/abs/andetigh.html)).
- **A comparator-keyed map names its comparator in its type and asks its laws where they are used.** Lean's `Std.DTreeMap` is a size-balanced tree over a `cmp` parameter, bundled with a well-formedness proof, its operations defined on the raw tree and its lemmas asking `TransCmp` ([`Std/Data/DTreeMap/Basic.lean`](https://github.com/leanprover/lean4/blob/master/src/Std/Data/DTreeMap/Basic.lean)). Haskell's `Data.Map` is a tree of the same kind ([`Data.Map`](https://hackage.haskell.org/package/containers-0.7/docs/Data-Map-Strict.html)); Rocq's `MSetAVL` keeps the search-tree predicate apart from its trees and proves balance in a file of its own, functional correctness not needing it ([`MSets/MSetAVL.v`](https://github.com/rocq-prover/stdlib/blob/master/theories/MSets/MSetAVL.v)); Agda's `Data.Tree.AVL` puts height and key bounds in the type ([`Data/Tree/AVL.agda`](https://github.com/agda/agda-stdlib/blob/master/src/Data/Tree/AVL.agda)).
- **A search tree is verified through its in-order list.** Nipkow specifies insertion and deletion on the list an in-order traversal gives and proves eight kinds of tree against it with one small library ([Automatic Functional Correctness Proofs for Functional Search Trees](https://www21.in.tum.de/~nipkow/pubs/itp16.html)).

## What it waits on

- **The first direction** waits on [the numeric laws](../04-arithmetic/03-numeric-laws.md), whose last stage proves a run of bytes read as a number injective at one length and bounded by it, the two facts a length-prefixed encoding's order rests on; on the finding above; and on a quiet machine for `map_wall`.
- **The second direction** waits on a consumer: a program whose key has an order and no encoding, or that needs an order no encoding gives.

## Retirement

Refined into a working specification when the first direction's prerequisites have landed, the second kept reserved beside it until its consumer exists, or deleted with its roadmap entry if a later decision rules a direction out.
