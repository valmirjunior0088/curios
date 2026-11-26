# A derived type keys a map

Adopted on 2026-11-25: a type a program declares keys a `Map` and fills a `Set` by derivation, as it derives `Eql`, `Ord` and `Hash` today, a `Date`, a tuple or an enumeration among them. Its approach is settled below, and its next step is delivery, the stages in order.

## What is wrong today

Each point is probed on standard input at `f76966ced`.

- **No type a program declares keys a map.** `Hash/Key` (`curios-prelude-build/std/Hash/Key.crs`) is written for `Bool`, `Byte`, `Bytes`, `Int`, `Nat`, `Str` and `Uuid` alone and has no derivation: `satisfy Hash/Key(Point) { .. }` is refused, "`Hash/Key` is not registered as derivable". `/std/Tuple` writes `Eql` and `Ord` for the tuples of zero to eight fields and no `Hash`, so no tuple keys a map either.

## Settled

- **A key's encoding stays its `Hash`**, the one [A witness body may be written by the compiler](../../design/surface/a-witness-body-may-be-written-by-the-compiler.md) states a derivation writes, so a derived key proves its law of the derived `Hash` and derived `Ord`.
- **A tuple's `Hash` and key are written**, for each tuple `/std/Tuple` orders, as its `Eql` and `Ord` are, since no derivation keys a tuple shape.

**Rejected.** Refusing a derived key to a type whose text or bytes part is not its last: a rule a program's author meets as a refusal about the order of its fields.

**Left out.** `OffsetDateTime` keys no map: its `Ord` compares the moment first and its derived `Hash` encodes its fields in declaration order, so its key is a written encoding in the order's own sequence. `Option` and `List` key no map, though their encodings order once they take the derived shape. Each is filed when this specification retires.

## Stages

5. **The key derives.** A row of the `DerivationSyntax` roster with its three fills and the concept's vocabulary, and the body writer: `mono` over a pair of constructors where both reduce to their ordinals' order, and at one constructor through each part's `mono` and the delimited parts' law; the derive suites, the unused-declaration suite, `documentation/language.md` and [A witness body may be written by the compiler](../../design/surface/a-witness-body-may-be-written-by-the-compiler.md). Check: a struct of a `Str` and a `Nat`, an enumeration and a type under a premise derive their keys with no written proof, and a key derived over an order that is not the derived one is refused at its `satisfy`.
6. **`/std`'s own types derive their keys**: `Ordering`, `Flt/Rounding` and the `time` types whose `Ord` and `Hash` are derived. Check: `/std` builds and a map keyed by `Date` walks in date order.
7. **Tuples key maps.** `Hash` and the key written for the tuples of zero to eight fields. Check: a map keyed by a tuple of a `Str` and a `Nat` walks in its `Ord` order.

## What moves

No map keyed by a derived type exists before stage 5. The figures the change may move are the time `/std` takes to elaborate and the corpus's emitted bytes, named and not retaken; the insert cost of a longer key ([`prelude/`'s findings](000-findings.md), *A key past three bytes*) stands as filed.

## Verification and retirement

Check: a struct of a `Str` and a `Nat`, a tuple and an enumeration each key a `Map` whose walk answers their `Ord` order; a derived key's `mono` checks with no written proof. Once it lands, `/std/Hash/Key`'s statement and the derivation's documentation hold the rules, the two left-out keys are filed in [`prelude/`'s findings](000-findings.md), and this file is deleted.
