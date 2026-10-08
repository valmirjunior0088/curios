# A map in its keys' order

Working specification for the order `/std/Map` enumerates its keys in: the law that holds every key type's encoding to its order, the encodings of `Nat` and `Int` rewritten to meet it, the proof that the trie walks its encodings in order, and the two walks that read an order off it.

It is independent of every other spec. Stages 3 and 4 change the bytes `/std/Hash` gives a `Nat` and an `Int`, which every map of either is shaped by.

## What this builds on

Each observed at `196ee2723`.

- **`Map` enumerates in the order of its keys' encodings.** It is a crit-bit trie over the bytes `/std/Hash` gives each key, nine bits a byte, the first saying the byte is there (`curios-std/src/Map.crs`), which is the order `Bytes/ord` answers: a proper prefix first, then byte by byte. `fold`, `entries`, `keys`, `Show` and `Spell` follow it, and `Set`'s with them.
- **Five of the seven key types are in their own order, and nothing holds them to it.** Building a map and comparing `Map/keys` with `List/sort` of it: `Byte`, `Bool`, `Bits`, `Bytes` and `Str` enumerate as their `Ord` puts them; `Map(Nat, V)` over `256, 1, 2, 65536, 255, 257, 0, 513` enumerates `[0, 65536, 256, 1, 257, 513, 2, 255]`, and `Map(Int, V)` over `-2` to `+2` enumerates `[+0, +1, +2, -1, -2]`. `Key` asks a witness for its encoding's injectivity and nothing of its order.
- **A concept holds an order and a law over it.** A `Key` with `use Hash(K)`, `use Ord(K)` and a field `monotone(a: K, b: K) -> Eq()(Ord/ord(a, b), Bytes/ord(Hash/hash(a), Hash/hash(b)))` elaborates, is certified and runs; its witnesses at `Bytes` and `Str` are `Eq/refl()`, at `Bool` and `Byte` a split on the comparison, at `Bits` a fold; a type declared under it reads the order through it; and `Ord/Flips` of every key's order is one lemma over the field and `Bytes/ord/flips`.
- **The order of bytes is a walk over two conses, and conversion reads a run by its cons.** `Bytes/ord` answers `eq` at one run, answers through a shared prefix as it does past it, and answers `lt` across two conses whose heads are in order, each a fold of three lines. `Bytes/get(x[h, ..t], 0)` is `h`, `Bytes/get(x[h, ..t], i + 1)` is `Bytes/get(t, i)` and `Bytes/len(x[h, ..t])` is `Bytes/len(t) + 1`, by conversion.
- **`Map/insert` may not stand in a type.** `crit`'s two walks count upward, so the totality obligation refuses a proposition that names a map built by `insert`: `'/std/Map/insert', which is not known to terminate`. `has`, `get`, `remove` and `keys` stand in one. With `crit` a walk on its two runs' structure and a ladder over a byte's eight places, the same trie's `insert` stands in a type and answers what it did.
- **A fact of every byte is decided by running it.** `Nat/shr` and `Nat/and` are atoms to the elaborator, so the trie's bit test `Nat/and(Nat/shr(x, 8 - o), 1)` is read by no proof ([what the carriers' algebra leaves undecided](../arithmetic/09-left-undecided-by-the-carriers-algebra.md)); a decision over every number below a bound, `True/qed()` at 256, with the lemma that it then holds of each, states the test at each of the eight places as the parity of a quotient, of every byte.
- **A map can hold what is true of its trie.** A structure under `use Key(K)` with a hidden proof over its nodes is built by `Map/empty` with nothing written, and a closed trie's reduces.
- **A `Bytes` of at most three bytes is an immediate, and one built from two that fit it is arithmetic**, whichever end it is built from ([A field is declared at the carrier its shape names](../../design/compilation/a-field-is-declared-at-the-carrier-its-shape-names.md); `emit_bin_append` and `emit_bin_concat`, `curios-emit/src/code_emitter.rs`). An insert into a `Map(Nat, V)` builds two keys, its own and that of the leaf it is compared with.

## The gap

**A map of numbers does not enumerate them in order**, so `Map/keys`, `Show` and `Spell` of a `Map(Nat, V)` or a `Map(Int, V)` read in an order no reader expects.

**A key is admitted whatever order its encoding is in.** `Key`'s one law is injectivity, which is how `Nat` and `Int` came to be keys out of order, and the next witness is held to no more.

**That the trie walks its encodings in order is nowhere a theorem, and cannot be stated.** `Map` carries no invariant, no proposition may name an `insert`, and the bit test is opaque to a proof, so the order a program meets in a fold is a fact about one function's code.

**A map has no operation that reads an order**: no least key and no greatest.

## Prior art

- **A trie over keys that compare as bytes is an ordered map, and the keys are made to.** The adaptive radix tree's authors define a binary-comparable key and give the transformation for integers, floats, strings and compound keys ([The Adaptive Radix Tree](https://db.in.tum.de/~leis/papers/ART.pdf), section IV). Cassandra's trie-indexed tables state the two requirements, comparison equivalence and prefix-freedom, and write an arbitrary-precision integer as a first byte `0x80 + (length - 1)` at or above zero and `0x7F - (length - 1)` below, a `0xFF` or a `0x00` ahead for every 128, then the digits; its current form writes a short integer's length as leading one bits of the first byte ([`ByteComparable.md`](https://github.com/apache/cassandra/blob/trunk/src/java/org/apache/cassandra/utils/bytecomparable/ByteComparable.md)). CockroachDB writes a small integer in its tag byte and a larger one's length there ([`encoding.go`](https://github.com/cockroachdb/cockroach/blob/master/pkg/util/encoding/encoding.go)); Google's OrderedCode writes a signed integer's length as leading one bits and a negative as the complement ([`orderedcode.go`](https://github.com/google/orderedcode/blob/master/orderedcode.go)); FoundationDB's tuple layer stops at 255 bytes ([`design/tuple.md`](https://github.com/apple/foundationdb/blob/main/design/tuple.md)).
- **A map over an injective encoding carries its invariant in its type.** Rocq's std++ `gmap` is a trie over a `Countable` type's code packed with the proof that makes its representation canonical, so equal maps are equal terms ([std++](https://gitlab.mpi-sws.org/iris/stdpp), `gmap.v`); Appel and Leroy's canonical binary tries are equal where their bindings are ([Efficient Extensional Binary Tries](https://arxiv.org/abs/2110.05063)); Lean's `Std.TreeMap` bundles its well-formedness and proves its operations against a list ([`Std.Data.TreeMap`](https://lean-lang.org/doc/api/Std/Data/TreeMap/Basic.html)), its order lemmas asking nothing of the map, `le_maxKeyD_of_contains` among them ([`DTreeMap/Lemmas.lean`](https://github.com/leanprover/lean4/blob/master/src/Std/Data/DTreeMap/Lemmas.lean)).
- **A Patricia trie has been proved in order, at ten lines a line.** The verification of Haskell's `Data.IntSet` specifies each operation by a relation that holds its well-formedness, its range and its denotation at once, takes the sorted list of elements as its second model, and needed 1 265 lines on integers and bits and 1 169 on dyadic intervals beside its proofs ([Ready, Set, Verify!](https://arxiv.org/abs/1803.06960)). Nipkow verifies a Patricia trie that stores its edges' labels by abstraction to the trie it compresses, with no invariant ([`Tries_Binary`](https://isabelle.in.tum.de/library/HOL/HOL-Data_Structures/Tries_Binary.html)). A crit-bit tree in C is verified against Bedrock2 ([Formally Verified Low-Level C Implementation of Crit-Bit Trees](https://research-collection.ethz.ch/handle/20.500.11850/685530)).
- **A map that only compares cannot have one shape and logarithmic operations.** Andersson and Ottmann show a uniquely represented dictionary over an ordered universe costs Θ(n^(1/3)) a search or an update ([New tight bounds on uniquely represented dictionaries](https://user.it.uu.se/~arnea/abs/andetigh.html)).

## Decisions

Taken before stage 1, each with its reason, so a stage meets none of them as a fork.

1. **The order is a key's law.** `Key(K)` takes `Ord(K)` as a superclass beside `Hash(K)` and a field `monotone`: `Ord/ord(a, b)` is `Bytes/ord` of the two encodings. A witness whose encoding is out of its order is then refused where it is declared, as one that collides is, and every theorem about an ordered walk stands under `use Key(K)` alone. A type with an encoding and no order of its own takes its encoding's in one line, the law being `Eq/refl()`. A key's order has its flip and its transitivity from the order of bytes, proved once.
2. **A natural's first byte holds its length and its top four bits.** `0x80 + 16 * rank + top`, where `rank` is how many bytes follow, below seven, and `top` the number those bytes leave, below sixteen; then the `rank` bytes, most significant first. A rank of seven or more writes a `0xFF` for every seven and then the same. A number below 16 is one byte, below 4 096 two and below `2^20` three, an immediate; the code is total over an unbounded `Nat`, and no code begins another.
3. **An integer at or above zero is its natural, and one below the complement of `-1 - i`'s.** No byte for the sign: a natural's first byte is in the upper half and a complement's in the lower, so every negative precedes every other and a more negative one comes first. `Hash` of `Nat/to_int(n)` is `Hash` of `n`, and an integer within `2^20` of zero is an immediate.
4. **A code is built from the back.** Its bytes are appended to its first, by recursion on the quotient by 256, so a code of three bytes is an immediate's arithmetic and the proof follows the number's own recursion with no power. The complement is a fold a byte at a time over `Byte/not`, a subtraction the elaborator reads.
5. **The codes' law is that the smaller is below the larger at a byte.** `Bytes/ord/below` decides that two runs part at a head with the first's the smaller, neither running out; it gives `Bytes/ord` its `lt`, stands under any two suffixes, and a complement reverses it. A code's injectivity is its law's corollary through `Nat/ord/eq` and `Int/ord/eq`.
6. **The map holds the proof that its trie is sound.** `Map` gains a hidden proof over its root: at every fork, the keys of the zero side have the fork's bit clear, those of the one side have it set, and all agree below it. `insert1` and `remove1` take their node's and hand back their outcome with what is true of it, built in their own recursion ([A certified function hands back its result with its proofs](../../../curios-std/README.md#a-certified-function-hands-back-its-result-with-its-proofs)); the proofs erase, and the erased program is the one that runs today. Where a conjunction of decisions states it, the conjunction is written as nested matches, as `List/ordered` writes its own.
7. **`crit` is a walk on its two runs, and the bit test stays as it is.** `crit` descends on both encodings' structure and reads the place two bytes part at off a ladder of eight shifts, so `insert` stands in a type; `bit` keeps its shift and its mask, and a proof reads it through the table of every byte.
8. **The order theorem is stated of `Map/keys`.** Under `use Key(K)`, the keys of a map are in strict order by `Ord/ord` at adjacent elements, and so at every pair. `Set/to_list`'s is its corollary.
9. **Two walks land, the least key and the greatest.** `Map/least` and `Map/greatest` answer the entry at the end of the trie's zero spine and of its one spine: the first and the last of `Map/entries`, with the theorem that no key the map has is below it, or above it. The neighbours of a key and the keys between two wait for a program that asks for them.
10. **`Hash`'s bytes change for a `Nat` and an `Int`.** A key's encoding is `Hash`'s by `Key`'s edge, with no second field to disagree with it, so the order is put where the bytes are made. A digest of a value that holds either changes with it.
11. **A composite key stays out.** No tuple or list has a `Key` witness. The two codes begin no other code, so either may be a part of one; a text needs a terminator and an escape before another part can follow it.

## Stages

1. **The order of bytes, as laws.** `Bytes/ord` answers `eq` at one run, carries across a middle run, and answers through a shared prefix as it does past it; `Bytes/ord/below` with its facts.
2. **The decision of every number below a bound**, with the lemma that it holds of each: the piece stage 7 reads the bit test through.
3. **`Nat` in its order.** `rank`, `top`, the first byte and the bytes appended to it; `Hash(Nat)` is the four; the law, by the first bytes where the ranks or the tops differ and by the bytes where they agree; `Key(Nat)`'s injectivity from it.
4. **`Int` in its order.** The complement; `Hash(Int)` by decision 3; the law, from `Nat`'s and from the complement reversing `below`; its injectivity.
5. **The law in `Key`.** `Key` moves to a module of its own under `/std/Map` with its seven witnesses, takes `Ord(K)` and `monotone`, and offers every key's flip and transitivity.
6. **`crit` on its runs.** The walk and the ladder, in place of the two counts.
7. **The trie's bits and the order of bytes.** The table of a byte's eight places; two encodings that agree below a bit and part at it are in the order of that bit, and `crit` answers the bit two encodings first part at.
8. **A sound trie.** The proof, `Map`'s field, and every operation that builds a map building it: `empty`, `insert`, `remove`, `map`, and through them `of`, `update`, `filter`, `union` and `rekey`.
9. **The order theorem**, and `Set`'s.
10. **The two walks**, and their theorems.

## Verification

- Stage 3: `Map(Nat, V)` over `256, 1, 2, 65536, 255, 257, 0, 513` enumerates in number order, and so does one over `15`, `16`, `4095`, `4096`, `2^20 - 1`, `2^20`, `16 * 256^6 - 1`, `16 * 256^6`, `16 * 256^13` and `16 * 256^14`, across the first byte's own boundaries; `curios/src/tests/derive.rs`'s three encodings are retaken.
- Stage 4: `Map(Int, V)` over each of those and its `-1 - n` enumerates in number order.
- Stage 5: `curios/src/tests/map.rs` refuses a witness whose encoding is out of its order as it refuses one that collides; a function under `use Key(K)` alone sorts its keys and states the sort's order.
- Stage 6: a claim in `curios-std/src/tests/data/map.crs` states what an `insert` answers, by `Eq/refl()`.
- Stage 8: `wonder stage wasm-optm` of `programs/spines/spines.crs` is the module it was before the stage, the proofs having erased.
- Stage 10: `least` and `greatest` of a map of each key type are the first and last of `List/sort` of its keys, and `none` of the empty map.
- Stages 3, 4, 6 and 8, what an insert costs (`timed`): `programs/spines/spines.crs` and `map_wall_key_share`'s loop compiled with `curios compile` by the compiler built before the stage and by the one after, the first run at 25 000 and at 75 000 inserts and the second at 1 000 000 and at 3 000 000 steps, the slope between them a median of at least eleven readings, each the least of three runs, the two compilers' programs interleaved in one sitting. A stage whose insert's span stands clear above the span before it stops; the key loop's reading says what part of an insert its two keys are. The figures `map_wall` records are read on a quiet machine.
- Every stage of `/std`: the prelude's build is measured before and after, naming the stage, and the table of stage 7 by its own count.

**Readings behind the decisions.** Timed at `196ee2723` on a machine under load, so each is a ratio. The codes: `programs/spines/spines.crs` with its map a `Map(Bytes, Nat)` keyed by the code under test, of the orbit's `y` and of `y * 15`, which is below `2^20`; each compiled with `curios compile` and run at 4 000 and at 12 000 inserts, the slope between them a median of eleven interleaved readings, each the least of three runs. Of `y`: `Bytes/of_nat` 2 424 ns an insert (1 253 to 6 258), a whole byte of length 1 599 (978 to 3 023), decision 2's code 1 319 (129 to 3 220). Of `y * 15`: 2 833 (1 850 to 5 041), 3 545 (613 to 7 756) and 1 945 (1 121 to 2 995). `crit`: the library's trie copied into a program over `Bytes` keys, with its two counts and with the walk and the ladder, the slope between 25 000 and 75 000 inserts over fifteen readings: 2 594 (845 to 3 376) and 2 404 (1 847 to 3 269).

**Readings at the gates.** Timed on a quiet machine, by the protocol above. Stage 3, `dc16bd507` against `a1f80f2ce`: an insert reads 699 ns (675 to 719) and 701 (677 to 726) over fifteen readings; a step of the key loop, which builds two keys, 29 ns (28 to 29) and 40 (40 to 42) over eleven, beside a control of 4 that builds none. A natural's code costs about five nanoseconds a key more than `Bytes/of_nat`'s cons, its first byte being arithmetic the cons has none of, and an insert's span does not show it.

## Rejected

- **The law as a proposition beside each witness.** A theorem would take it as a hypothesis and a caller find it by name, and a witness could still be declared without it, which is the defect.
- **A byte for the sign.** It costs every integer a byte, and an integer past 4 095 in magnitude its immediate.
- **A whole byte for the length.** The simplest proof, and it leaves a natural past 65 535 no immediate where one below `2^24` has one today.
- **The length as leading one bits, seven bits of the number a byte.** An immediate up to `2^21`, with the number's digits off the byte and a law over powers of 128.
- **A length of the length, recursively.** Total too, and a second case in every proof for numbers no program holds.
- **A ladder of literals for the short codes.** It builds a short code at once whatever a cons costs, and states the code twice.
- **`Bytes/not` for the complement.** An `xor` against a fill, which conversion does not carry across a cons.
- **The bit test as a quotient's parity.** A proof reads it with no table, in the walk an insert spends its time in.
- **The invariant as a predicate beside the map.** Every theorem would take it and every caller prove it of a map it did not build.
- **A trie that stores each edge's run.** Its invariant is local and its lookups compare a run a level, where a crit-bit fork tests a bit.
- **Keys read back from their paths.** A decoder for every key type, run at every enumeration.
- **A tree keyed by a comparator.** Its shape follows its history, and a map's is its API ([`curios-std`'s README](../../../curios-std/README.md#map-is-a-crit-bit-trie-and-its-shape-is-its-api)); a key with an order has an encoding in it or takes a dictionary that gives one.
- **Every order operation at once.** Nothing reads one today, and the two that land are the two the trie's shape already holds.

## Completion criteria

- A key type is declared with the proof that its encoding is in its order, and `Nat` and `Int` are.
- The keys of a map are in their order, by a theorem under `use Key(K)` alone.
- The least and the greatest key of a map are read off it, each with its theorem.
- An insert costs no more than it did before stage 3.

## Retirement

Move the encodings' contracts to `Hash`'s and `Map`'s module documentation, and the decisions to [`curios-std`'s README](../../../curios-std/README.md#map-is-a-crit-bit-trie-and-its-shape-is-its-api), whose `Map` decisions state the law, the soundness the map holds and the order theorem; what the sound trie is chosen to bear next, what `get` answers after an `insert` and that two maps of the same entries are one, opens as a line of its own where a program needs it.
