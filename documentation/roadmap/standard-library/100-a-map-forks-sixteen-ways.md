# A map forks sixteen ways

Adopted: `/std/Map` states its invariant where its reader looks for it and walks about a quarter of the levels it walks today. A fork holds its children in a list its bitmap counts, the map holds `Node/Sound` of its root itself, and the family that restates soundness with its bridges each way is deleted; a lookup takes half the time it takes today on the prototype measured below; and `List` and `Vec` gain an update in place, which `/std` has no form of.

Working specification for `/std/Map`'s trie forking on four bits of its key's encoding where it forks on one, its children held compressed in a list. It restates [`Map` is a crit-bit trie, and its shape is its API](../../../curios-prelude/README.md#map-is-a-crit-bit-trie-and-its-shape-is-its-api) and [A map holds the proof that its trie is sound](../../../curios-prelude/README.md#a-map-holds-the-proof-that-its-trie-is-sound); what a map promises — its keys in their type's order, one shape for one set of entries, `try_get`'s laws and extensionality — is unchanged.

Its next step is stage 1, a delivery. Stage 2 waits on [A type is read through what it computes](../theory/101-a-type-is-read-through-what-it-computes.md) and on the [compilation finding](../compilation/000-findings.md) that a walk that only reads a value it is handed in a loop owns it.

## What this builds on

- **Today's map.** A crit-bit trie over the encoding `Key` gives a key, nine bits a byte, the first saying the byte is there (`curios-prelude/std/Map/bits.crs`); one descent that decides on the way back (`Node/insert`, `Node/remove`, `curios-prelude/std/Map/internal/Node.crs`); the proof held as `Rooted` over `Node/Shaped`, a family restating `Node/Sound` with no function applied to the trie, read back through `Node/shaped`, `Node/read`, `grounded` and `rooted`, since `/std/Toml` holds itself through a map.
- **The rebuild in place** ([A construction beside a dying value of its row is rebuilt in place](../../../curios-cps/README.md#a-construction-beside-a-dying-value-of-its-row-is-rebuilt-in-place)) writes a unique row's changed slots, and a list is a value it writes nothing into.
- **A list is a rope whose flat run is one GC array** ([A rope is entered through its final shapes](../../../curios-emit/README.md#a-rope-is-entered-through-its-final-shapes)), and `Vec(T, n)` is a list with its count, which erases to the list.
- **A family is laid out at its widest constructor** ([A variant collapses when nothing needs to distinguish it](../../design/compilation/a-variant-collapses-when-nothing-needs-to-distinguish-it.md)), so a leaf beside a fork of seventeen slots is seventeen slots.
- **What stays stated over `try_get`'s laws**: [A union walks two tries once](200-a-union-walks-two-tries-once.md), and the [finding](000-findings.md) that adds intersection and difference as filters.

## What it costs today

Timed at `8ed961f9c`: child CPU time of compiled executables, the slope from 200,000 to 1,000,000 steps (2,000,000 for lookups), the median of seven interleaved rounds' slopes with their span. A step of `programs/spines/spines.crs`'s loop, and its mirrors with `Map` replaced by a hand-written crit-bit trie of the same shape and no proofs:

| Step | ns |
| --- | --- |
| `Map/insert` | 388.5 [385.6, 397.4] |
| the mirror over `Hash/hash`'s encoding, bits read as `/std/Map/bits/bit` reads them | 384.6 [376.7, 391.7] |
| the mirror over the `Nat` itself, a bit read by one shift | 243.0 [240.4, 249.6] |
| `Map/update` | 797.3 [785.4, 829.9] |
| `Map/try_get`, 65,537 keys | 306.3 [304.1, 310.8] |

The proofs cost nothing; reading a bit through the encoding costs about a third of an insert; the rest is the binary depth, seventeen levels over 65,537 keys. An update costs two inserts because the walk ahead of it makes the map shared (the compilation finding above).

## What is known

- **What the structures cost where nothing else does.** Timed with `rustc 1.99.1 -O` on the same machine and day, keys as heap byte strings as `Key`'s encodings are, each structure written in place as the rebuild writes a unique map, dense `Nat` keys and `"key/"` followed by the number, the protocol above with 1,000,000 steps for inserts and 2,000,000 for lookups:

  | Structure | Insert, `Nat` | Lookup, `Nat` | Insert, `Str` | Lookup, `Str` |
  | --- | --- | --- | --- | --- |
  | `HashMap` | 40.6 [38.6, 42.2] | 50.0 [47.6, 51.4] | 68.8 [66.4, 71.1] | 50.6 [50.0, 54.1] |
  | `BTreeMap` | 184.3 [181.0, 186.0] | 202.1 [200.8, 205.0] | 232.8 [228.4, 234.4] | 216.7 [209.7, 220.5] |
  | crit-bit, today's shape | 232.6 [227.8, 238.1] | 172.5 [170.2, 180.6] | 360.8 [353.8, 367.8] | 210.1 [205.2, 225.0] |
  | qp-trie, sixteen ways | 74.4 [72.9, 76.3] | 60.0 [58.9, 63.8] | 186.5 [181.7, 197.2] | 105.7 [104.6, 112.5] |
  | Merkle search tree, base 16 | 370.4 [366.5, 385.9] | 377.1 [373.2, 388.4] | 415.8 [404.9, 421.7] | 401.1 [383.9, 421.1] |

  The qp-trie (Finch, [qp](https://github.com/fanf2/qp)) is a crit-bit trie that forks on a nibble, its present children packed behind a bitmap; it is ordered and has one shape for one key set, as a crit-bit trie does, and Finch measures it 10% to 30% faster than crit-bit natively with half the depth. CHAMP keeps a hash trie canonical the same way ([Steindorfer and Vinju, OOPSLA 2015](https://blog.acolyer.org/2015/11/27/hamt/)); a Merkle search tree is a B-tree whose shape is its key set's, made for sync rather than speed.
- **What a sixteen-way trie costs in Curios today.** Timed and counted at `8ed961f9c`, generic over `Key` as the map is: with forks of sixteen fields, a lookup takes 154.3 ns [152.0, 157.3] where the map's takes 306.3; with children in a `List` built by slicing and concatenating, an insert takes 1,505.9 [1,485.9, 1,553.0] and a lookup 317.2 [313.7, 319.5], every child read and written through the rope. Forks of sixteen fields cost their own: an insert of sixteen arms in one pass compiles to an `ins` of 72,839 lines at `wasm-optm` and 14.5 s of `emit` against 1.1 s, and its frame weighs the cap of 1,024 against `Node/insert`'s 108, where the same in two passes weighs 112 and takes 2.7 s; every leaf takes the fork's eighteen slots; and an empty child written as a constructor is the [compilation finding](../compilation/000-findings.md) that a function naming constant data takes each constant as a parameter.
- **Persistent maps that run fast write in place when nothing else holds them**: Swift's `TreeDictionary` over CHAMP, Rust's `im` through `make_mut`, Clojure's transients by an owner token, and Koka's trees, within a quarter of C (Lorenzen, Leijen, Lindley and Swierstra). Curios proves the same at compile time.
- **The prototypes are not checked in.** Each is a program in the shape of `spines` over a generic trie, a lookup workload building 65,537 keys and probing them in the loop's order; the Rust bench is one program over the five structures, every one held to the same fold and, for the ordered four, the same keys in order.

## Decisions

- **A trie of sixteen ways over four positions a byte** of the key's encoding: the byte's presence, its high nibble and its low nibble, the fourth position no fork's, so a key that ends before another parts from it at a presence position as it does today, and the keys' order and the shape's canonicity are the crit-bit trie's. Decided on 2026-11-17.
- **A fork is `branch(at: Nat, bitmap: Nat, twigs: Vec(Node(K, V), Nat/ones(bitmap)))`**, its position, the digits present at it and one twig each in digit order, with no empty child; a leaf is `leaf(key: K, value: V)`. Decided on 2026-11-17.
- **`List` gains an update at an index**, written in place where the rebuild proves the list unique and a copy of its flat run otherwise, and `Vec/set` over it with its count. A list is the flat carrier already, and `Vec` its counted form. Decided on 2026-11-17.
- **An insert and a remove take two passes**: a probe that only reads finds where the key parts from the trie and the digit the trie's keys hold there, and a write that replaces, fills, wedges or collapses rebuilds each level it passes. Decided on 2026-11-17.
- **The map holds `Node/Sound` of its root** as a hidden field, and `Rooted`, `Node/Shaped`, `Node/Read`, `Node/shaped`, `Node/read`, `grounded` and `rooted` go. Decided on 2026-11-17.
- **`union`, `filter` and `rekey` stay folds over inserts**, and [A union walks two tries once](200-a-union-walks-two-tries-once.md) is restated over the new node. Decided on 2026-11-17.
- **The key's encoding is `Key`'s, unchanged**; what a key past three bytes costs is the standard library's uncertain [finding](000-findings.md). Decided on 2026-11-17.

## Questions

Settled before stage 2, each by the prototype above with the update of stage 1 in place.

- **A twig inserted or removed**: by slicing and concatenating the list, which the next update flattens once, recommended where an insertion of a new key costs no more than it does today; or by an insertion and a removal of their own beside the update.
- **`Nat/ones`**: a `/std` function over a table of nibbles, recommended where the walk does not spend its time there; or an intrinsic the emitter lowers to `i32.popcnt`.

## Stages

1. **`List` gains an update in place; `Vec/set` over it.** A `/sys` row typed by `Intrinsic::signature`, its reduction in both checkers, its erasure and its continuation form; `curios-cps/src/sharing.rs` reading a list's storage, a window sharing its base, so the rebuild writes an element of a list it proves unique; the emitter's write and its copy. Check: a `curios/src/tests/codegen/rebuild.rs` test that a loop of updates to a list nothing else holds allocates nothing, a differential that a list read after its update keeps its old element, and the update's laws beside `List/get` and `Vec/get`, folded and executed.
2. **The new node, and the map over it.** Every operation of `/std/Map` and `/std/Set` over the new node, their proofs with them, the decisions above, and the bridges deleted. Check: `/std/Map`'s, `/std/Set`'s, `/std/Toml`'s and `/std/Json`'s tests; every law `curios-prelude`'s README names proved of the new definitions — `try_get`'s, extensionality, `keys`' order, `least` and `greatest`; and the map's variance vector equal to today's.
3. **Measured before it lands.** The step of `spines`, `Map/update`, keys past three bytes, `Str` keys, a map shared with its previous version, lookups, and a remove beside an insert, each timed as above against today's map, and `emit` for `programs/spines/spines.crs` beside today's. It lands where no step is slower than today's beyond its span and lookups are faster.
4. **The documents.** `curios-prelude`'s README restates the two decisions and their **Rejected** paragraphs; `programs/README.md` restates what `spines` compares; `map_wall`'s documentation (`curios/src/tests/codegen/map_wall.rs`) restates the rationale it gives for the crit-bit trie, and its figures are retaken; [A union walks two tries once](200-a-union-walks-two-tries-once.md) is restated over the new node.

## Verification

`cargo xtask workspace lint` and the suites a stage reaches after each stage, its checks by name first; `curios-prelude`'s suite for the stages that touch `curios-cps` or `curios-emit`. Stored bytes that may change: `/std`'s image where `Map`, `Set`, `List` and `Vec` are declared, and the emitted module of every program that holds a map; every verdict of the corpus and of the tests stays equal. The full gate runs once, after the fourth stage.

## Rejected

- **The crit-bit trie kept, its bit read made cheaper**: bounded by its depth, Rust's own crit-bit taking three times its qp-trie on dense keys.
- **A fork of sixteen fields**: the code size, the frame weight and the leaf width counted above, and an empty child that is a constant every fork holds.
- **HAMT or CHAMP**: no order of the keys.
- **A Merkle search tree or a Prolly tree**: slowest of the five measured, a canonical B-tree comparing heap keys at every step.
- **An array carrier beside `List`**: a second storage abstraction where `List` is the flat one.
- **A one-pass insert reading a representative key in its empty child's case**: that read is the walk the compilation finding names, and it shares every child.

## Completion and retirement

The structure, its proof and what it rejects go to `curios-prelude`'s README, the update's contract to `List`'s and `Vec`'s documentation, and the laws to the tests above. Once no reference to this specification remains, it is deleted.
