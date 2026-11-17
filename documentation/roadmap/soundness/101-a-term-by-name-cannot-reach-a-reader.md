# A term by name cannot reach a reader

**The benefit.** The certifier's reduction, conversion and classification read their terms by value, and a term that names a `let`-bound local cannot reach them: they run over a reading (`ByValue`), which only their doors make, each unfolding what it is handed, and which offers no binding, no typing and no recording while it stands, and carries plain reduction's mode ([`curios-cert`'s README](../../../curios-cert/README.md#a-let-is-bound-for-typing-and-read-by-value)). One guard beside the readings still holds by runtime state what the reading already holds by what it offers. The project has decided to adopt it.

Its next step is stage 1, a ready delivery. It waits on nothing.

## What is wrong today

- **A guard no path reaches.** `Positions::begin` raises a re-entrancy guard while an erased half is decided, so that the classification records no position or call of its own. Classification runs in a reading, which offers no recording, so the guard suppresses nothing.
- **One lookup written twice.** The remembered half is asked through two copies of one lookup, `Certifier::record_checked`'s and `Reader::erased`'s, each unfolding the type, asking the memo, classifying on a miss and filing what it found.

## Decisions

- **Classification records nothing because it cannot.** `Positions`' re-entrancy guard and `Positions::begin` go, and `Positions::settle` keeps only the first undecided classification.
- **The remembered half has one door.** `erased_half` asks the memo, classifies on a miss and files what it found, and `record_checked` and `Reader::erased` both ask it.

## Stages

1. **Classification records nothing because it cannot.**
   - Lands: the remembered half's one door, which `record_checked` and `Reader::erased` ask.
   - Goes: `Positions`' guard and `Positions::begin`.
   - Check: the verification below, with `curios-erase`'s suites, which read through `Reader`.

## Verification

`cargo xtask workspace lint` runs; it certifies all of `/std`. The workspace's suites run. `sys.rkyv`, `std.rkyv` and `documentation.rkyv`, which `curios-prelude`'s build writes, must digest the same as before the stage, built in the same checkout, since no verdict, key, reduct or position moves. `stored_prelude_measurements`' certification time (`timed`) may move; it is named, not retaken. The full gate runs once, after the stage.

## Retirement

Once the stage has landed and nothing cites this file, it is deleted.
