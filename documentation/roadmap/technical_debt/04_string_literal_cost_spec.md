# A string literal is checked once per use

This is the implementation specification for making a `Str` literal affordable at the size people actually write one.

## Status

A recorded shortcut, not a missing capability. `curios-text/src/into_core/lowerer.rs`'s `str_literal` states the trade in full under *What this does not fix, and what would*, names two remedies, and defers both. This document is that note promoted to a specification, with the cost measured rather than estimated.

Nothing here is a pricing defect. Price reduction perfectly — which is [A reduction step costs what it builds](01_priced_reduction_spec.md)'s job — and a six-kilobyte string literal still costs half a million steps, because the cost is *running a fold*, not building anything. The two specifications meet at exactly one point, recorded under *What spec 01 already removes*.

## What a literal costs today

A `Str` literal lowers to `Str { bytes = <Bytes>, valid = of_scan_eq(b, refl_scan(b)) }`. The proof is constant size — that was the earlier fix, and it worked — but checking it makes conversion decide `scan_from(lead, b) ≡ lead`, which runs `/syn/Str`'s scan over every byte: a `rec` unfold, a `Bytes` peel, a `Byte/to_nat`, `classify`'s `choose` ladder, and an inductive match, per byte.

Measured on `c58f0463`, by bisecting `--budget` over a literal of `n` identical characters:

| | steps per character |
| --- | --- |
| elaboration | 16 |
| whole compile, literal never used | 83 |
| whole compile, per additional use site | +83 |

So the cost is **`83 × length × (1 + uses)`**, and it is linear in all three. The model was checked at the boundary rather than extrapolated: a bare literal of 12 000 characters compiles at the default budget and one of 13 000 refuses, against a predicted 12 048.

Three conclusions follow.

**The ceiling is small enough that ordinary content reaches it.** About 12 000 characters for a literal nobody uses, about 6 000 for one used once, about 3 000 for one used three times. That is a help text, an embedded schema, a test fixture. The failure arrives as a kernel budget refusal that names nothing about string literals.

**The scaling defect is per *use*, not per literal, and nothing recorded it.** Each use site re-runs the whole scan. The elaborator does not — it is flat at 64 353 steps for a 4 000-character literal whether that literal is used zero, one, two or three times, because its cache hits are free. The kernel re-charges every one.

**It is the validity check and nothing else.** A `Bytes` literal of the same length costs 2 554 steps flat, because it carries no derivation. `Str` and `Bytes` differ here by three orders of magnitude at four kilobytes.

## The figure this corrects

`lowerer.rs` records "about 16 steps per byte against the derivation's 42, so a literal's ceiling against the default budget moves from roughly 23KiB to roughly 61KiB rather than away."

The 16 is exactly right and the 61KiB is not. Sixteen is the **elaborator's** share; the whole compile is 83, and the kernel's 67 was never in the figure. The ceiling it names is therefore five to ten times what a compiler actually accepts. Nothing was wrong when it was written — it measured a real thing and said which number it was, and then the number that decides whether a program compiles turned out to be a different one.

It is the failure mode `CLAUDE.md` names: a figure in prose with no probe beside it, designed against later. Whatever this specification lands, the replacement figure lives beside a test that reproduces it.

## What spec 01 already removes, and what it leaves

[A reduction step costs what it builds](01_priced_reduction_spec.md)'s Ma makes a `whnf`/`forced` memo hit spend nothing. A literal's scan is the same closed term at every use site, so **the `× (1 + uses)` multiplier goes away entirely** — the kernel becomes flat in use count, as the elaborator already is.

Whether the base 67 also falls is unmeasured and worth measuring first, because it decides how much of this specification is left. The kernel's 67 is about four times the elaborator's 16 for identical work, and the +83 per use proves the kernel does re-scan identical closed terms; if the base factor is the same phenomenon inside one declaration, Ma collapses it toward 17 and the ceiling lands near 60 000 characters — which would make `lowerer.rs`'s 61KiB true rather than wrong. If it is instead one scan costing four times as many transitions, Ma leaves it untouched.

**Measure that before scoping the rest.** It is the difference between this specification being about a 6 000-character ceiling and a 60 000-character one.

## What spec 01 measured, and the direction it moved

Both halves of spec 01 have since landed, and the answer is not the one either arm above predicted.

**The use multiplier is gone, as predicted.** Measured 2026-08-15 by bisecting `--budget` over a literal of `n` identical characters: the floor is flat across zero, one, two and three use sites — 33 853 to 33 934 units at `n = 500`, an increase of twenty-seven units per use where the model predicted the whole scan again. The kernel is now flat in use count, as the elaborator already was.

**The base did not fall, and pricing raised the ceiling's *cost* rather than lowering it.** A literal's UTF-8 derivation nests one reduction level per byte, and spec 01's frame row charges a new peak of reduction depth the native frame it takes — measured at 7 264 bytes, 1 024 units. So depth, which a literal has in proportion to its length, became the dominant term. Against the recalibrated 30 000 000-unit default a literal of about 2 000 characters compiles and one of 6 000 does not, where roughly 12 000 compiled before any of this work.

**That makes this specification's second lever the load-bearing one.** Shrinking the per-byte constant now buys depth as well as steps, because the two are the same walk. And it makes the third lever — an O(1) native scan — the only one that removes the length-dependence from both axes at once. What spec 01 changed is not the ceiling but which of these levers is worth pulling first.

The ceiling is not a calibration the budget default can fix: raising it far enough to restore 12 000 characters would give up the memory bound spec 01 exists to establish. `curios-elab`'s `DEFAULT_STEP_BUDGET` records that trade where the constant is.

What Ma cannot do under any reading is remove the *linearity*. The cost stays proportional to the literal's length with a non-zero constant, so a large enough literal always crosses any budget. Only an O(1) check changes that.

## The levers, and what each is worth

Three, and they compose rather than compete.

**Charge a computation once — spec 01's Ma.** Removes the use multiplier; possibly the 4×. Not this specification's work, and listed only so its share is not claimed twice.

**Shrink the per-byte constant.** Nobody has tried. Sixteen elaboration steps per ASCII byte buys a `rec` unfold, a packed peel, a `Byte/to_nat`, `classify`'s ladder of `Nat` range comparisons, and an inductive match. `classify` is written for clarity over a `Nat` carrier; whether it can be cheaper without becoming unreadable is an open question, and a cheap one to answer.

**Make the check O(1) — a native scan intrinsic.** The only lever that removes length-dependence, and the expensive one: it puts a UTF-8 validator in the trusted base. `lowerer.rs` deferred it on the grounds that this is the wrong direction while the kernel is being made load-bearing, and that judgment stands on its own. Worth stating fairly on both sides: the kernel already carries shared intrinsic folds for `Nat`, `Int` and `Bin`, so a scan is incremental rather than categorical — and [Intrinsic fold laws and the free-monoid peel](../../soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md) grades that row the weakest anywhere on the perimeter, so incremental is exactly where the exposure already is.

The fourth thing `lowerer.rs` names — full reflection, restating `Valid(b)` as `Eq(scan_from(lead, b), lead)` — is **not** a lever here. Checking a literal's proof still decides that same equation, so the scan remains. It removes the `of_scan_eq` bridge and helps the lemmas in `/std/Str/utf8` that traverse a derivation; it does not help a literal.

## Why this is worth deciding before something forces it

An `include_bytes!` analogue would be **free for `Bytes` and fatal for `Str`**, since one carries no derivation and the other pays per byte. A language feature whose cost depends that sharply on which of two closely related types you asked for is a feature that will be reported as a bug. Deciding the encoding first is cheaper than deciding it under a user's deadline.

## Milestones

### M0 — Measure what Ma leaves

- Re-take the per-character figures on a tree with spec 01's Ma landed, for a literal used zero, one and three times. This is the milestone that scopes the rest, and it may close this specification outright if the ceiling lands near 60 000 characters and nobody is asking for more.
- Record the figures beside an ignored probe carrying its command, date and profile, so the number that replaces `lowerer.rs`'s cannot decay the same way.
- Correct `lowerer.rs`'s note to say which half of the compiler its 16 measures, whatever else changes.

### M1 — The per-byte constant

- Account for the 16 elaboration steps a single ASCII byte costs, by reading `scan_from`, `step` and `classify` against the reduction they drive.
- Reduce it where that costs no clarity, and record what was tried and declined where it does.

### M2 — Decide the O(1) question

- Only if M0 and M1 leave a ceiling somebody is actually hitting. State the trusted-base cost of a native scan against the measured ceiling without it, and take the decision to [design.md](../../design.md) rather than settling it here — it is a judgment about the perimeter, not about strings.

## Acceptance

- The cost model is stated with a probe that reproduces it, and validated at its boundary rather than extrapolated.
- A `Str` literal costs the same whether it is used once or many times.
- `lowerer.rs` names which half of the compiler each figure it quotes belongs to.
- Whatever ceiling remains is written down in characters, beside the probe, where a user reaching it can find it.

## Refused alternatives

**Raising the default budget.** It is the only bound on reduction memory the compiler has, measured at roughly 2 KiB retained per transition with no payload constructed — so a ten-fold raise permits twenty gigabytes before a byte of payload is built. Spec 01 exists to replace that bound with a better one; until it does, raising the budget to fit a string literal trades a compile error for an out-of-memory kill.

**Skipping the check for literals the lowerer knows are valid.** The lowerer does know — the bytes came from a Rust `&str`. But the kernel exists precisely not to believe the stage above it, and an unchecked `Valid(b)` is a closed inhabitant of a proposition nobody proved. This is the trusted-base cost of the O(1) lever, arrived at by a route that hides it.

**Capping literal length in the parser.** It converts a budget refusal into an arbitrary limit with no principle behind its value, and leaves the per-use scaling untouched.
