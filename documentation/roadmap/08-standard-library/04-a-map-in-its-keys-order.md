# A map in its keys' order

Working specification for the order `/std/Map` enumerates its keys in: the encodings of `Nat` and `Int` written so that their bytes compare as the numbers do, the law that says so of every key type, and the two walks that read an order off the trie. A map for a key that has an order and no encoding stays reserved at the end, for a consumer.

It is independent of every other spec. Its first stage changes the bytes `/std/Hash` gives a `Nat` and an `Int`, which every map of either is shaped by, and retakes `curios`'s `tests::codegen::map_wall`.

## What this builds on

- **`Map` enumerates in the order of its keys' encodings.** It is a crit-bit trie over the bytes `/std/Hash` gives each key, a proper prefix first and then byte by byte (`curios-std/src/Map.crs`), which is the order `Bytes/ord` answers. `fold`, `entries`, `keys`, `Show` and `Spell` follow it, and `Set`'s with them.
- **Five of the seven key types are in their own order already.** Observed at `bf21a9065` by building a map and comparing `Map/keys` with `List/sort` of it: keys of `Byte`, `Bool`, `Bits`, `Bytes` and `Str` enumerate as their `Ord` puts them. `Map(Nat, V)` over `256, 1, 2, 65536, 255, 257, 0, 513` enumerates `[0, 65536, 256, 1, 257, 513, 2, 255]`, and `Map(Int, V)` over `-2` to `+2` enumerates `[+0, +1, +2, -1, -2]`: `Hash(Nat)` is the minimal encoding least significant byte first, and `Hash(Int)` a sign byte before it.
- **A function may stand under a `Key(K)` and an `Ord(K)` premise.** The law below is stated under both, and such a function elaborates, is certified and runs.
- **The order of bytes is a walk over two conses.** `Bytes/ord` reduces across a shared head by conversion, and a cons spread into an append is the cons of the append, so `Bytes/ord(x[..p, ..a], x[..p, ..b])` is `Bytes/ord(a, b)` by a fold on `p` whose step is its hypothesis. Two heads that differ are decided through `Nat/ord/of_lt`, not by conversion. `Bytes/ord/flips` is proved; that the order answers `eq` at one run is a claim in `curios-std/src/tests/aggregates/packed.crs` and not yet a law.
- **A recursion that divides by a literal is total.** A walk on `n / 256` under a guard that `n` is not zero stands in a type, as `Bytes/of_nat` does, so the number of bytes a number takes needs no count to descend on.

## The gap

**A map of numbers does not enumerate them in order**, so `Map/keys`, `Show` and `Spell` of a `Map(Nat, V)` or a `Map(Int, V)` read in an order no reader expects, and two maps a program prints are compared by eye against a sequence that is neither the insertion's nor the numbers'.

**A map has no operation that reads an order**: no least key and no greatest, though the trie's two spines are those keys once its encodings are in order.

**Nothing states that an encoding is in its key's order**, so a program that folds a map and relies on the order it met relies on a fact about one witness's bytes.

## Prior art

- **A trie over keys that compare as bytes is an ordered map.** The adaptive radix tree's authors define a binary-comparable key as a transformation under which `x < y` exactly when the two byte strings compare so, give it for unsigned and signed integers, floats, strings and compound keys, and note that a string must end in a value found nowhere else in it, since no key may be a prefix of another ([The Adaptive Radix Tree](https://db.in.tum.de/~leis/papers/ART.pdf), section IV). FoundationDB's tuple layer is the worked encoding: a type code that carries an integer's length and sign, a negative as its complement, and a byte string closed by a zero byte with its own zeros escaped ([`design/tuple.md`](https://github.com/apple/foundationdb/blob/main/design/tuple.md)). Haskell's `IntMap` is a big-endian Patricia trie that offers the least and greatest key, the neighbours of a key and ascending folds ([`Data.IntMap`](https://hackage.haskell.org/package/containers-0.7/docs/Data-IntMap-Strict.html)).
- **One shape per set of entries is what makes a map extensional.** Appel and Leroy's canonical binary tries give CompCert maps that are equal where their bindings are, and so are rewritten by plain equality ([Efficient Extensional Binary Tries](https://arxiv.org/abs/2110.05063)).
- **A map that only compares cannot have one shape and logarithmic operations.** Andersson and Ottmann show a uniquely represented dictionary over an ordered universe costs Θ(n^(1/3)) for a search or an update ([New tight bounds on uniquely represented dictionaries](https://user.it.uu.se/~arnea/abs/andetigh.html)).

## Decisions

Taken before stage 1, each with its reason, so a stage meets none of them as a fork.

1. **A natural is its length, then its bytes most significant first.** `Hash(Nat)` writes how many bytes the number takes and then those bytes, with no leading zero and none for zero. The length is written so that lengths compare as their codes do and no code begins another: a `0xFF` for every 255, then one byte for what is left. Every number below `256^255` therefore grows by one byte, and the encoding is total over an unbounded `Nat`, where one length byte would stop being injective at 2040 bits.
2. **An integer is its sign, then its natural, a negative's complemented.** `Hash(Int)` writes `0x01` and the natural for an integer at or above zero, and `0x00` and the bitwise complement of the natural `-1 - i` for one below, so every negative precedes every other and a more negative one comes first. The sign keeps a byte of its own, as it has one today.
3. **The law is a proposition under both premises, proved beside each witness.** `Map/Key/Monotone(K)` states that `Ord/ord(a, b)` is `Bytes/ord` of the two encodings, as `Ord/Flips` and `Ord/Transitive` state a comparator's laws; each of the seven key types proves it in a module beside its order, and a theorem about an ordered walk takes it as a hypothesis. `Key` gains no field: it has no order to state one in.
4. **Two walks land, the least key and the greatest.** `Map/least` and `Map/greatest` answer the entry at the end of the trie's zero spine and of its one spine, each with the theorem that no key of the map is below it, or above it, under the law. The neighbours of a key and the keys between two wait for a program that asks for them.
5. **`Hash`'s bytes change for a `Nat` and an `Int`.** A key's encoding is `Hash`'s by `Key`'s edge, with no second field to disagree with it, so the order is put where the bytes are made. A digest of a value that holds either changes with it.
6. **A composite key stays out.** No tuple or list has a `Key` witness, and a part that is a text needs a terminator and an escape before it can be followed by another part.

## Stages

1. **The order of bytes, as laws.** `Bytes/ord` answers `eq` at one run, answers through a shared prefix as it does past it, and answers `lt` across two conses whose heads are in order: the three facts every encoding's proof is made of.
2. **`Nat` in its order.** The number of bytes a number takes, by its quotient by 256; its bytes most significant first at a given length, a cons from the front by recursion on the length; the length's code. `Hash(Nat)` is the three, `Key(Nat)`'s injectivity is proved of them, and the law: a smaller number's code is below a larger one's, by the lengths where they differ and by the bytes where they agree.
3. **`Int` in its order.** `Hash(Int)` by decision 2, its injectivity, and the law, from `Nat`'s and from the complement reversing an order between two codes of which neither begins the other.
4. **The law at the five that hold it already.** `Byte`, `Bool`, `Bits`, `Bytes` and `Str` each prove `Monotone`, `Bytes` and `Str` by definition.
5. **The two walks.** `Map/least` and `Map/greatest`, and their theorems under the law.

## Verification

- Stage 2: `Map(Nat, V)` over `256, 1, 2, 65536, 255, 257, 0, 513` enumerates in number order, and so does one whose keys take 254, 255, 256, 509, 510 and 600 bytes, across the length code's own boundaries. `curios`'s `tests::codegen::map_wall` is retaken: a `Nat` key grows by a byte.
- Stage 3: `Map(Int, V)` over `-2` to `+2` and over `-65536`, `-257`, `-256`, `-255`, `+255`, `+256` enumerates in number order.
- Stage 4: a claim under both premises applies each type's law.
- Stage 5: `least` and `greatest` of a map of each key type are the first and last of `List/sort` of its keys, and `none` of the empty map.
- Every stage: the prelude build is measured before and after, naming the stage.

**To retake the measurements.** The encodings above, run as programs at `e506feab0` before any stage: a list of numbers sorted by `Bytes/ord` of their codes equals `List/sort` of it, for the naturals and the integers of the verification rows. `map_wall` is timed, and waits for a quiet machine.

## Rejected

- **One length byte.** It stops at 2040 bits, past which two numbers share a code.
- **A length of the length, recursively.** It is total too, and costs a second case in every proof for numbers no program holds.
- **The sign folded into the length's byte.** It saves a byte on an `Int` key and puts a case for the sign into every proof about a length.
- **The law as a field of `Key`.** `Key` would then need an order, and a type with an encoding and no order could not be a key.
- **Every order operation at once.** Nothing reads one today, and the two that land are the two the trie's shape already holds.

## Completion criteria

- A map of any key type enumerates in its key's order, by a law stated once and proved at each witness.
- The least and the greatest key of a map are read off it, each with its theorem.

## A tree keyed by a comparator, reserved

A key with an order and no encoding, or an order no encoding gives, takes a balanced search tree: the key type and the dictionary named in its type as `Map`'s are, the dictionary a lawful order as `Map`'s is a lawful encoding, since under a comparator that is not transitive a search tree loses entries with no word. Its shape follows its history, so two trees of the same entries are equal by their traversal and not by their nodes. It waits on a consumer: a program whose key has an order and no encoding. Lean's `Std.DTreeMap` is a size-balanced tree over a `cmp` parameter with a well-formedness proof, its lemmas asking `TransCmp` ([`Std/Data/DTreeMap/Basic.lean`](https://github.com/leanprover/lean4/blob/master/src/Std/Data/DTreeMap/Basic.lean)); Rocq's `MSetAVL` keeps the search-tree predicate apart from its trees ([`MSets/MSetAVL.v`](https://github.com/rocq-prover/stdlib/blob/master/theories/MSets/MSetAVL.v)); Nipkow specifies insertion and deletion on the list an in-order traversal gives ([Automatic Functional Correctness Proofs for Functional Search Trees](https://www21.in.tum.de/~nipkow/pubs/itp16.html)).

## Retirement

Move the encodings' contracts to `Hash`'s and `Map`'s module documentation and the decisions to [`curios-std`'s README](../../../curios-std/README.md#map-is-a-crit-bit-trie-and-its-shape-is-its-api); keep the reserved tree as this file, or delete the file with its roadmap entry if a later decision rules the tree out.
