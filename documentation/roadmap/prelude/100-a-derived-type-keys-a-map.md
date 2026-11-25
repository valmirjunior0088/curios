# A derived type keys a map

**Not refined yet.** Adopted on 2026-11-25: a type a program declares keys a `Map` and fills a `Set` by derivation, as it derives `Eql`, `Ord` and `Hash` today, a `Date`, a tuple or an enumeration among them. Its approach is settled under *Settled*; what is left to refine is under *Open*, and its next step is that refinement, a bounded design decision.

## What is wrong today

- **No type a program declares keys a map.** `Map/Key` (`curios-prelude-build/std/Map/Key.crs`) is written for `Bool`, `Byte`, `Bytes`, `Int`, `Nat`, `Str` and `Uuid` alone and has no derivation, so a type that derives `Hash` and `Ord` still keys no `Map`.
- **A derived `Hash` does not order as its type does.** A key's encoding is its `Hash`'s, by the concept's own statement (`monotone`: the order of two keys is the order of their encodings, as `Bytes/ord` reads them), and a derived `Hash` frames every part, its tag among them, behind its length (`Hash/tagged`, `curios-prelude-build/std/Hash.crs`, written by `hash_body`, `curios-elab/src/derive.rs`), so two encodings order by their parts' lengths before their contents: over a struct of a `Str` and a `Nat` with `Eql`, `Ord` and `Hash` derived, `("b", 1)` against `("ab", 1)` answers `gt` by `Ord/ord` and `lt` by `Bytes/ord` of the encodings (probed at `1fb7b00e9`).
- **What a hand-written witness needs is one law no concept states.** Over the parts' encodings joined with no frame, a key's witness composes from what `/std` holds, each checked on standard input at `1fb7b00e9`: for two `Nat` fields `monotone` is three cases in about forty-five lines (the first field smaller, by `Nat/hash/of_lt`, `Bytes/ord/below/onto` and `Bytes/ord/below/lt`; the first fields equal, by `Bytes/ord/past` and the second field's own `monotone`; the mirror, by `Ord/flips` at `Bytes`), and `inj` is eight lines from `monotone` and `Ord/eq`, the shape `Nat/hash/inj` has. What the composition asks of every part but the last is that a smaller value's encoding is below a larger one's at a byte (`Bytes/ord/below`): `Nat` proves it, the encodings of one width have it wherever they differ, as `Bool`'s, `Byte`'s and `Uuid`'s do, and `Str`'s and `Bytes`'s, their own bytes, do not, a text being a prefix of a longer one.

## Settled

- **A key's encoding stays its `Hash`.** One encoding for each value, the one the concept already names; a key encoding of its own beside `Hash` is rejected below.
- **`Map/Key` gains the law that orders a part**: a smaller value's encoding is below a larger one's at a byte.
- **A derived `Hash` joins its parts unframed**, behind a one-byte tag where the type has more than one constructor, each part's own encoding after it.
- **A text before the last part is escaped and terminated**, so a shorter text that begins a longer one encodes below it, as the order-preserving composite keys of FoundationDB's tuple layer, MyRocks and CockroachDB end each variable-length part.
- **The carriers' own encodings do not move**: `Nat`'s, `Str`'s, `Bytes`'s and the rest stay what they are, and only what a derivation writes changes.

**Rejected.** Refusing a derived key to a type whose text or bytes part is not its last: a rule a program's author meets as a refusal about the order of its fields. A derived `Hash` left framed and a key encoding of `Map/Key`'s own beside it: two encodings of one value, a reader asking which one a map holds, and a second path to one fact.

## Open

- **Where the law stands.** A text key at the last part needs no escaping and orders as it stands, so whether `Map/Key` states the law of every key and `Str`'s and `Bytes`'s witnesses meet it by an escaped form, or a part's law is stated apart from a whole key's, decides what a carrier proves.
- **The escape.** Which byte terminates a part, which escapes it inside one, and what the injectivity proof of an escaped part costs, `Hash/tagged`'s framing being today the whole of what a derived witness's injectivity rests on.
- **The derivation of `Map/Key`.** A derivation reaches the `DerivationSyntax` roster with its three fills, the concept's `/std` vocabulary, the derive suites and the unused-declaration suite (`.claude/rules/checking.md`); what the derived `monotone` and `inj` are written over, and how long they take to check on a type of several fields.
- **What moves.** Every derived `Hash` encoding changes, so whatever stores or compares one, a digest kept across runs or a map built over a derived key's hash, is listed and read again; and the insert cost of a longer key ([`prelude/`'s findings](000-findings.md), *A key past three bytes*) is the figure an escape may move.

## Stages

Stated once the refinement settles *Open*.

## Verification and retirement

Check, once refined: a struct of a `Str` and a `Nat`, a tuple and an enumeration each derive `Map/Key` and key a `Map` whose walk answers their `Ord` order; `("ab", 1)` encodes below `("b", 1)`; a derived key's `inj` and `monotone` check with no hand-written proof. Once it lands, `Map/Key`'s statement and the derivation's documentation hold the rule, and this file is deleted.
