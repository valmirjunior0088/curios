# A unit in scope is a walk's output whole

Broken rule: [The soundness board](../../design/soundness/the-soundness-board.md) leaves "bytes no walk wrote" into the store outside the claim, and holds the board's histories to what a build reaches, yet the board withholds part of a record a walk filed and calls that inside the claim, and the certifier carries a path for a unit in scope beside a record no walk filed, which only the board and hand-built tests take. The repair is the boundary, not a guard: a unit in scope is what a walk returned, whole, and no path is left for anything else.

What reaches the certifier as a unit in scope is one of three: a unit a build compiled, beside the record its walk returned; a unit the store hands back, filed with such a record (`cache::put`, `curios-pipeline/src/compile.rs`); and the archived prelude, walked again by the build that embeds it (`curios-prelude/compile.rs`). `withhold` (`History::withhold`, `curios-verdict/src/test_support.rs`) is none of them, and neither is `already_judged` (`curios-cert/src/recheck/test_support.rs`), which mounts an empty record for fifteen suites of `recheck`. Agreed with the user on 2026-11-18, lifted from the findings as a delivery of its own.

The next step is stage 1, a delivery.

## What is known

- **The mount takes any record.** `Globals::mount` (`curios-cert/src/certifier/globals.rs`) takes a unit's definitions beside a record, `Certification::covers` tells one that classifies every definition from the rest, and coverage decides what is read: `Globals::classified`, `partial` and `applications` read a covering record alone, `unclassified` hands the reading walk an uncovered unit's items to classify from their terms, `DigestedUnit` digests a part by it for the stages outside the certifier, and adoption over a baseline (`recheck`) takes a covering baseline alone. A vector no record files is computed by the walk per name (`unvectored`), coverage aside.
- **The mount's callers.** The compile's scope (`globals_after`), the prelude's build and restore, and `baseline::complete` each hand a walk's record; `AlreadyJudged::globals` hands the empty one.
- **`covers` is a guard against one shape of forgery.** A forged record that covers its unit is believed — `adoption_tests`' `a_forged_total_verdict_mounted_in_scope_admits_a_looping_proof` admits a looping proof of an empty proposition through one, stating the boundary's trust — while one that covers less is classified afresh (`totality_tests`' `a_record_that_does_not_cover_its_unit_is_not_read`). Both are bytes no walk wrote, which the claim leaves outside alike.
- **Both tickets withholding found are flaws of the recomputation the guard feeds.** In `admission`'s `2026_11_05_a_read_of_a_unit_with_no_covering_record…` and `2026_11_05_a_family_of_a_unit_with_no_vector_on_record…`, `/lib` withheld, the reading walk classified or vectored it for itself, and the digest of what `/app`'s judgment read of it was alike before and after what it computed moved, so a verdict filed downstream stood.
- **The walk's own answer makes the state.** `Rechecked` (`curios-cert/src/recheck.rs`) holds a record beside its verdicts — an empty one where it refuses a module whole, for a redeclaration or a miscounted case, a full one where it refuses a declaration — and its three readers, the compile and the recompile (`curios-pipeline`) and `wonder`'s named query, take the record only once a refusal is ruled out, the recompile through `Rechecked::accepted`.
- **Histories stay the board's.** A history is what reaches what a walk files for a later walk to believe, which no single program put to the checkers can; mount, recompile and unmount are what a build does, and a unit written as Core text puts the certifier alone, which is the board's own question. Only `withhold` reaches a state no build does.

## Decided

- **The walk answers one or the other.** An accepted module, with its stated definitions, its record and its entry, or its verdicts alone; no record of a refused walk exists.
  - Rejected: keeping `Rechecked`'s fields and its `accepted` accessor, which leaves every reader to rule the refused record out by hand.
- **A mounted record answers for every name its unit holds**, which every walk's record does, read per name with that invariant stated where it is read: an `expect` naming it as a construction bug, as a registry entry mounted twice already is (`scope_tests`' `a_registry_entry_mounted_twice_is_a_construction_bug`). A store's damaged bytes are outside the claim with a forger's.
  - Rejected: an absent entry read in the refusing direction, which would hide a construction bug of the compiler's own as a refusal.
  - Rejected: a check of coverage where the store reads a record back, a guard against bytes the claim already leaves outside.
  - Rejected: a pair type carrying the invariant into the mount's signature: `curios-unit`, which holds a unit's record, stands on no edge to `curios-cert`, so the record stays a `curios-core` type a walk alone fills.
- **One reading path.** `covers`, `classified`, `unclassified`, `unvectored`, and the coverage the digests and adoption read go.
- **Hand-built environments mount the record a walk over them returns**, as `text_certified_over` already walks; the guard's own tests go with it — `a_record_that_does_not_cover_its_unit_is_not_read`, `scope_tests`' no-record half of `stored_verdicts`, `standing_tests`' `a_part_digest_reads_a_classification_where_the_certifiers_own_reads_one` — and the forged covering record, which states the claim's edge, stays.
- **`withhold` goes from the board**, and the two tickets are restated as the histories without it, each held to the refusal it names, and archived where one no longer reaches its flaw, with that reason.

## Stages

1. **The walk's answer.** `Rechecked` becomes the accepted module or the verdicts, its readers in `curios-pipeline`, `curios-wonder` and the board's put following. Check: `curios-cert`'s, `curios-pipeline`'s and `curios-verdict`'s suites; `cargo xboard report` passing. One commit.
2. **The per-name reading.** `Globals` reads each mounted record per name under its invariant, the coverage paths go, `already_judged` mounts a walk's record, and the guard's tests go. Check: the lint, which certifies `/std`; `curios-cert`'s suites with every hand-built environment over a walk's record; the forged covering record's test unchanged; `cargo xboard report` passing. One commit.
3. **The board.** `Step::Withhold` and `History::withhold` go, the two tickets are restated or archived, and the board's paragraph of [The soundness board](../../design/soundness/the-soundness-board.md) is restated so a believed record is a walk's output whole, a dropped, added or altered verdict alike the bytes no walk wrote. Check: `cargo xboard report` passing with the restated tickets; `xboard/README.md`'s history decision restated. One commit.

## Verification

`cargo xtask workspace lint` and the stage's suites by name at each stage; `cargo xboard report` at every stage; the whole workspace's tests once stage 3 has landed.

## Retirement

Once stage 3 lands, the rule lives in [The soundness board](../../design/soundness/the-soundness-board.md) and `curios-cert`'s `Globals` documentation, the two tickets in their restated or archived form, and this file is deleted.
