# A term by name cannot reach a reader

**The benefit.** The certifier's reduction, conversion and classification read their terms by value, and a term that names a `let`-bound local cannot reach them: they run over a reading (`ByValue`), which only their doors make, each unfolding what it is handed, and which offers no binding while it stands ([`curios-cert`'s README](../../../curios-cert/README.md#a-let-is-bound-for-typing-and-read-by-value)). Two modes and guards that ride beside the readings are still held by runtime state and checks where the reading could hold them by what it offers. The project has decided to adopt it.

Its next step is stage 1, a ready delivery. It waits on nothing.

## What this builds on

- **A reading is the receiver the readers run over.** `ByValue` (`curios-cert/src/certifier/by_value.rs`) holds the certifier borrowed, offers its lookups by shared reference and by name each capability a reading mutates, and is made only by `Certifier::read_by_value`, which every door enters by.
- **Reduction asks conversion, and conversion reduces.** A judgment's reduction puts four questions to its checker's conversion, each answered by plain reduction, which asks nothing ([A question reduction asks is answered by plain reduction](../../design/soundness/a-question-reduction-asks-is-answered-by-plain-reduction.md)); `convert.rs` holds the four together.

## What is wrong today

- **Plain reduction is a mode the scope carries.** `Certifier::plainly` and `ByValue::plainly` raise `Scope`'s `plain` field while a question is answered and while a shared analysis reads a term (`Env::force`). Only readings read it: the memo tables, a case equation's settled spellings and the question points. Typing reads it once, in `record_checked`'s assertion, made in every build, that nothing is typed under plain reduction.
- **A guard no path reaches.** `Positions::begin` raises a re-entrancy guard while an erased half is decided, so that the classification records no position or call of its own. Classification runs in a reading, which offers no recording, so the guard suppresses nothing. The remembered half is also asked through two copies of one lookup, `Certifier::record_checked`'s and `Reader::erased`'s.

## Decisions

- **Plain reduction is a mode of a reading.** The receiver carries it, set where a reading is made and where `ByValue::plainly` opens a plain one over the same certifier; `Env::force` on `Certifier` is a door that opens a plain reading. The scope's probes that read a settled spelling by mode take the mode as an argument, as the memo's tables already do. The scope keeps no mode, and typing has none to be in, so nothing can be typed under plain reduction.
- **Classification records nothing because it cannot.** `Positions`' re-entrancy guard and `Positions::begin` go, and `Positions::settle` keeps only the first undecided classification. The remembered half has one door, which `record_checked` and `Reader::erased` both ask.

## Stages

1. **Plain reduction is a reading's mode.**
   - Lands: `ByValue` carries the mode; `plainly` sets it on the reading; `Env::force` on `Certifier` opens a plain reading; `Scope::refinement_of_reduct`, `Scope::settled_refinements`, `Scope::unasked_refinement`, `Scope::reachable_refinements` and `Scope::settle_refinement` take the mode; the memo's hits, read off a reading, take it from the reading.
   - Goes: `Scope`'s `plain` field, `Scope::plain`, `Scope::begin_plain`, `Scope::end_plain`, `Certifier::plainly`, `Certifier::plain`, and `record_checked`'s assertion.
   - Restated: the comments that place the mode on the scope (`scope.rs`, the memo's module documentation).
   - Check: the verification below.
2. **Classification records nothing because it cannot.**
   - Lands: the remembered half's one door, which `record_checked` and `Reader::erased` ask.
   - Goes: `Positions`' guard and `Positions::begin`.
   - Check: the verification below, with `curios-erase`'s suites, which read through `Reader`.

## Verification

After each stage, `cargo xtask workspace lint` runs; it certifies all of `/std`. The workspace's suites run. `sys.rkyv`, `std.rkyv` and `documentation.rkyv`, which `curios-prelude`'s build writes, must digest the same as the previous stage's, built in the same checkout, since no verdict, key, reduct or position moves. `stored_prelude_measurements`' certification time (`timed`) may move; it is named, not retaken. The full gate runs once, after the last stage.

## Rejected

- **Two receiver types for the two reductions**, so that plain reduction could not ask a question at all. Every walk would be compiled twice for the few places the two reductions differ, where the walk is one statement for both, as the Π and Σ rules are one statement for a lookup and a judgment.

## Retirement

Once the two stages have landed and nothing cites this file, it is deleted.
