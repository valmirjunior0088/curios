# A derived type keys a map

Adopted on 2026-11-25: a type a program declares keys a `Map` and fills a `Set` by derivation, as it derives `Eql`, `Ord` and `Hash` today, a `Date`, a tuple or an enumeration among them. Its approach is settled below, and its next step is delivery, the stages in order.

## What is wrong today


## Settled

- **A tuple's `Hash` and key are written**, for each tuple `/std/Tuple` orders, as its `Eql` and `Ord` are, since no derivation keys a tuple shape: every field but the last delimited, as a derived encoding writes a struct's, and `mono` through `/std/Hash/Key/then` and `last`.

**Left out.** `OffsetDateTime` keys no map: its `Ord` compares the moment first and its derived `Hash` encodes its fields in declaration order, so its key is a written encoding in the order's own sequence. `Option` and `List` key no map, though their encodings order once they take the derived shape. Each is filed when this specification retires.

## Stages


## What moves

The figures the change may move are the time `/std` takes to elaborate and the corpus's emitted bytes, named and not retaken; the insert cost of a longer key ([`prelude/`'s findings](000-findings.md), *A key past three bytes*) stands as filed.

## Verification and retirement

Check: a tuple keys a `Map` whose walk answers its `Ord` order. Once it lands, `/std/Hash/Key`'s statement and the derivation's documentation hold the rules, the two left-out keys are filed in [`prelude/`'s findings](000-findings.md), and this file is deleted.
