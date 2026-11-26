# A codec proves it reads back what it writes

**Not refined yet.** Each codec of `/std` claims in prose that what it writes it reads back, and holds the claim by examples, though each is a total function over certified text and the claim a closed equation. It waits on its first stage, one small codec proved, which prices the others.

## What is missing today

- `Json`, `Toml`, `Html`, `http/Url`, `time/rfc3339` and `time/iso8601`, and `Flt`'s decimal and hex text each say in prose that `decode` reads back what `encode` writes, and each holds it by examples in its `tests` module; none states `decode(encode(v)) = success(v)` as a theorem. `time/Date`'s `of_days` and `to_days` are inverse by prose alone too.
- A closed instance is a theorem already: `Result/map(Parse/run(rfc3339/date(), "2024-02-29"), iso8601/write_date)` is `Result/success("2024-02-29")` by `Eq/refl()`, the checker running the parser over the literal, and `Parse/run(Parse/pure(a), s)` is `Result/success(a)` by `Eq/refl()` for every `a` and `s` (both checked on standard input at `1fb7b00e9`), so each example a codec's `tests` module holds could be an equation the build checks.
- No lemma of `/std` states a parser's `run` or the round trip of a number's text. A parser is a function from a position to a position and a value (`Parse/Of`), so `bind`, `map` and `pure` unfold, and the text primitives read `Str/At/try_next` at a position whose evidence is erased: the general theorem needs a statement of each primitive over those positions, and one of `iso8601`'s `fixed` over a number's padded text, neither written.

## Known

- EverParse proves in F* that each parser it generates inverts its serializer and that every message has one encoding, for the TLS, Bitcoin and CBOR formats (USENIX Security 2019; PulseParse and EverCDDL, 2025), and Narcissus derives such pairs in Coq.

## Open

- What a proof through `Parse`'s combinators costs, and whether a round-trip law for each combinator, parser and printer proved inverse a piece at a time as Narcissus proves them, makes each codec's theorem a composition, or each codec is proved by hand.
- Whether each codec's examples become equations the build checks, ahead of the general theorem.

## Stages

1. **One small codec.** `time/rfc3339`'s date proved through the combinators, what it took counted in lemmas and lines. Check: the theorem checked by both checkers in `/std`.
2. **The other codecs**, by the combinator laws or by hand as stage 1 decides. Check: each codec's round trip a theorem `/std` checks.

## Verification and retirement

Retired once each codec states its round trip as a theorem `/std` checks, its prose claim citing it.
