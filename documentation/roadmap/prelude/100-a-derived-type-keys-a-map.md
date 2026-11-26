# A derived type keys a map

Adopted on 2026-11-25: a type a program declares keys a `Map` and fills a `Set` by derivation, as it derives `Eql`, `Ord` and `Hash` today, a `Date`, a tuple or an enumeration among them. Its approach is settled below, and its next step is delivery, the stages in order.

## What is wrong today

- **`/std`'s own types key no map.** `Hash/Key` derives ([A witness body may be written by the compiler](../../design/surface/a-witness-body-may-be-written-by-the-compiler.md)), and no type `/std` declares derives one: `Ordering`, `Flt/Rounding` and the `time` types derive their `Ord` and `Hash` and no key, so a `Date` keys no `Map`.
- **No tuple keys a map.** `/std/Tuple` writes `Eql` and `Ord` for the tuples of zero to eight fields and no `Hash` (probed on standard input at `f76966ced`).

## Settled

- **A tuple's `Hash` and key are written**, for each tuple `/std/Tuple` orders, as its `Eql` and `Ord` are, since no derivation keys a tuple shape: every field but the last delimited, as a derived encoding writes a struct's, and `mono` through `/std/Hash/Key/then` and `last`.

**Left out.** `OffsetDateTime` keys no map: its `Ord` compares the moment first and its derived `Hash` encodes its fields in declaration order, so its key is a written encoding in the order's own sequence. `Option` and `List` key no map, though their encodings order once they take the derived shape. Each is filed when this specification retires.

## Stages

6. **`/std`'s own types derive their keys**: `Ordering`, `Flt/Rounding` and the `time` types whose `Ord` and `Hash` are derived. Check: `/std` builds and a map keyed by `Date` walks in date order.
7. **Tuples key maps.** `Hash` and the key written for the tuples of zero to eight fields. Check: a map keyed by a tuple of a `Str` and a `Nat` walks in its `Ord` order.

## What moves

The figures the change may move are the time `/std` takes to elaborate and the corpus's emitted bytes, named and not retaken; the insert cost of a longer key ([`prelude/`'s findings](000-findings.md), *A key past three bytes*) stands as filed.

## Verification and retirement

Check: a tuple keys a `Map` whose walk answers its `Ord` order, and so does a `Date`. Once it lands, `/std/Hash/Key`'s statement and the derivation's documentation hold the rules, the two left-out keys are filed in [`prelude/`'s findings](000-findings.md), and this file is deleted.
