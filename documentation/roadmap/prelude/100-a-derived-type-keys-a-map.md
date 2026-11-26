# A derived type keys a map

Adopted on 2026-11-25: a type a program declares keys a `Map` and fills a `Set` by derivation, as it derives `Eql`, `Ord` and `Hash` today, a `Date`, a tuple or an enumeration among them. Its approach is settled below, and its next step is delivery, the stages in order.

## What is wrong today

Each point is probed on standard input at `f76966ced`.

- **No type a program declares keys a map.** `Map/Key` (`curios-prelude-build/std/Map/Key.crs`) is written for `Bool`, `Byte`, `Bytes`, `Int`, `Nat`, `Str` and `Uuid` alone and has no derivation: `satisfy Map/Key(Point) { .. }` is refused, "`Map/Key` is not registered as derivable". `/std/Tuple` writes `Eql` and `Ord` for the tuples of zero to eight fields and no `Hash`, so no tuple keys a map either.
- **A derived `Hash` does not order as its type does.** A key's encoding is its `Hash`'s, by the concept's own statement (`monotone`: the order of two keys is the order of their encodings, as `Bytes/ord` reads them), and a derived `Hash` frames every part, its tag among them, behind its length (`Hash/tagged`, `curios-prelude-build/std/Hash.crs`, written by `hash_body`, `curios-elab/src/derive.rs`), so two encodings order by their parts' lengths before their contents: over a struct of a `Str` and a `Nat` with `Eql`, `Ord` and `Hash` derived, `("b", 1)` against `("ab", 1)` answers `gt` by `Ord/ord` and `lt` by `Bytes/ord` of the encodings.

## Settled

- **A key's encoding stays its `Hash`.** One encoding for each value, the one the concept already names. The carriers' own encodings do not move: `Nat`'s, `Str`'s, `Bytes`'s and the rest stay what they are.
- **A derived encoding is its parts', every part but the last delimited.** Where the type has more than one constructor the encoding opens with the constructor's ordinal as `Nat` encodes it, one byte below sixteen constructors; then each part's encoding in declaration order, a proof taking no part as today. Every part but the last is delimited: each zero byte written `0x00 0xFF` and the run closed by `0x00 0x01`, as CockroachDB's `EncodeBytesAscending` closes a key's byte strings and FoundationDB's tuple layer escapes them. A delimited run orders as the run did and parts at a byte from any other delimited run, so what follows a part decides only where the parts tie; `/std` proves that once, and no carrier proves anything for it. The last part stands as it is, so a type of one field encodes as its field.
- **The key concept's one law is named `mono`**, as `inj` was shortened, and so are `Nat/hash/monotone` and `Int/hash/monotone`, which its witnesses bind.
- **The concept lives at `/std/Hash/Key`.** It is `Hash` refined, and the delimited parts a derived key is proved through are `Hash`'s. No path is kept at `Map/Key`, and a module that names the concept writes it qualified, `Hash/Key(K)` and `Hash/Key/same`, in `Map` and `Set` as anywhere.
- **`Hash/tagged` and `Hash/framed` retire.** `Option`'s and `List`'s written witnesses take the derived shape, `Option`'s ordinal and then its payload and a list its elements each delimited, so each still encodes in its type's order; `Flt/internal/Dyadic`'s witness, which nothing resolves, goes.
- **A tuple's `Hash` and key are written**, for each tuple `/std/Tuple` orders, as its `Eql` and `Ord` are, since no derivation keys a tuple shape.

**Rejected.**

- **A law in the key concept that orders a part**, every key proving its encoding below a larger one's at a byte and a text meeting it by an escaped form. A derived `Hash` sees its parts through their `Hash` alone, so it could pick the parts to escape only by their types, and a derived `Hash` over a part with no key would have no law to ask for.
- **Delimiting the last part too.** Every encoding grows by a terminator and a type of one field no longer encodes as its field.
- **Refusing a derived key to a type whose text or bytes part is not its last**: a rule a program's author meets as a refusal about the order of its fields.
- **A derived `Hash` left framed and a key encoding of the concept's own beside it**: two encodings of one value, a reader asking which one a map holds, and a second path to one fact.

**Left out.** `OffsetDateTime` keys no map: its `Ord` compares the moment first and its derived `Hash` encodes its fields in declaration order, so its key is a written encoding in the order's own sequence. `Option` and `List` key no map, though their encodings order once they take the derived shape. Each is filed when this specification retires.

## Stages

3. **The concept moves to `/std/Hash/Key` and its law is `mono`.** Check: no path names `Map/Key` or `monotone`, and `/std` builds.
4. **Delimited parts, and the derived `Hash` writes them.** `/std/Hash`'s delimited run with its order law, written over a byte dispatch, which a proof reads past its key as [a stuck dispatch reads its keys' tests](../../design/soundness/case-equations-and-their-key.md); `hash_body` writing the ordinal where there are several constructors and every part but the last delimited; `tagged` and `framed` retired, `Option`'s and `List`'s witnesses rewritten and `Dyadic`'s removed. Check: `("ab", 1)` encodes below `("b", 1)` and `curios/src/tests/derive/hash.rs` pins the new bytes.
5. **The key derives.** A row of the `DerivationSyntax` roster with its three fills and the concept's vocabulary, and the body writer: `mono` over a pair of constructors where both reduce to their ordinals' order, and at one constructor through each part's `mono` and the delimited parts' law; the derive suites, the unused-declaration suite, `documentation/language.md` and [A witness body may be written by the compiler](../../design/surface/a-witness-body-may-be-written-by-the-compiler.md). Check: a struct of a `Str` and a `Nat`, an enumeration and a type under a premise derive their keys with no written proof, and a key derived over an order that is not the derived one is refused at its `satisfy`.
6. **`/std`'s own types derive their keys**: `Ordering`, `Flt/Rounding` and the `time` types whose `Ord` and `Hash` are derived. Check: `/std` builds and a map keyed by `Date` walks in date order.
7. **Tuples key maps.** `Hash` and the key written for the tuples of zero to eight fields. Check: a map keyed by a tuple of a `Str` and a `Nat` walks in its `Ord` order.

## What moves

Every derived encoding changes, and so do `Option`'s and `List`'s, so does every `Hash/of` digest over one, which `/std/Hash` already marks as unstable across compiler versions; nothing stores an encoding across runs, and no map keyed by a derived type exists before stage 5. The figures the change may move are the time `/std` takes to elaborate and the corpus's emitted bytes, named and not retaken; the insert cost of a longer key ([`prelude/`'s findings](000-findings.md), *A key past three bytes*) stands as filed.

## Verification and retirement

Check: a struct of a `Str` and a `Nat`, a tuple and an enumeration each key a `Map` whose walk answers their `Ord` order; `("ab", 1)` encodes below `("b", 1)`; a derived key's `mono` checks with no written proof. Once it lands, `/std/Hash/Key`'s statement and the derivation's documentation hold the rules, the two left-out keys are filed in [`prelude/`'s findings](000-findings.md), and this file is deleted.
