# A decided equality in scope is its equation

**[Equality is decided where a decider exists and is `Eq` everywhere, joined by one law](../../design/theory/equality-is-decided-where-a-decider-exists-and-is-eq-everywhere-joined-by-one-law.md) holds the bridge between the two equalities to be one lemma of the concept, and the elaborator crosses by it at both ends, as a program leaves the crossing to it; what stays is the order's half of the rule, each law `Ord` carries having one public name; its next step is stage 5, a ready delivery.** A carrier's order module publishes the proofs its `Ord` witness names, so each is a second public path to a law the concept carries, and a program may reach one fact by two names.

## What is wrong today

Each is probed at `bccdd3112`.

- **A carrier's order lemmas are a second public path to the laws `Ord` carries.** `Nat/ord`, `Int/ord`, `Bytes/ord` and `List/ord` hold the proofs their type's `Ord` witness names, each public, so `Nat/ord/flips` and `Ord/flips` at `Nat` name one fact. What keeps a lemma public is its witness alone, declared in the parent's file, which names no private declaration of a child module. `Bytes/ord`'s `past` and its `below` subtree are no law of `Ord`: a key's encoding is ordered through them ([`prelude/`'s findings](../prelude/000-findings.md)), and `Bytes/tests.crs` reads `past`. [`curios-prelude-build`'s README](../../../curios-prelude-build/README.md) states the order lemmas as the public proofs beside each order, `Str/ord/flips` among them, which `/std` does not declare, and names `Key/refl`, `Key/flips` and `Key/transitive`, which `Map/Key` does not declare either.

## What it simplifies

- **The order modules.** Every read of a carrier's order lemma that restates `Ord`'s law moves to the law with no declaration cycle, tried over a copy of the `std` package at `5763feb48`: the 27 reads in `Bytes/ord.crs`, `Int/hash.crs`, `Nat/hash.crs` and `Ord/by_tag.crs` as `Ord/eq`, `Ord/below`, `Ord/flips` and `Ord/refl`, and the five of `Int/tests.crs` and the two of `Bytes/tests.crs` with them. With `Nat`'s and `Int`'s witnesses declared in `Nat/ord.crs` and `Int/ord.crs` and every lemma there private, `/std` checks, and `gt`, `le` and `of_le` are then named by nothing.

## Goal

- **One public name a law.** No carrier's order module states a public lemma restating a law `Ord` carries. What `Ord` does not carry, `Bytes/ord`'s `past` and `below`, stays public.

## Stages

**5. A carrier's order lemmas are its witness's alone.** Each of the four witnesses is declared in its `ord` module beside the lemmas it names, which lose their `pub`; the reads above take `Ord`'s laws; the lemmas nothing then names are deleted; and `curios-prelude-build`'s README states an order's laws as `Ord`'s. It waits on nothing. Check: no `pub let` of the four modules but `ord`, `Bytes/ord/past` and `Bytes/ord`'s `below` subtree, and `/std` builds with its tests.

## Verification and retirement

The stage's check, then `cargo xtask format`, `cargo xtask compiler lint`, `cargo xtask jig lint`, the suite and `cargo xtask docs build`. The figures the work may move are the procedure's work across `/std`'s build, each `proved()` asking it where a call was written, and the runaway table (`curios/src/tests/instrumented/runaways.rs`). Once stage 5 has landed, this file is deleted.
