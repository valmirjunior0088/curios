# An idiomatic string walk should cost what a hand-written one costs

`programs/parse_digits.crs` decodes a digit string through `/std/Str/fold`. `programs/parse_manual.crs` decodes the same string by indexing bytes with the accumulator threaded by hand. The second is much faster, and closing that distance — without asking anyone to write the second — is the whole of what this document is about.

The distance is not itself a target, because it is a *sum*. At least four costs separate those two programs, no measurement has ever divided them, and the corpus that would divide them has been sitting complete and unrun for the length of three roadmap items. So the first milestone here is the division, and every milestone after it is chosen by what the division says. **That order is the specification.** The mechanisms below are candidates for work that a measurement has not yet authorized.

## What is actually known

One figure is anchored. `programs/parse_digits.crs` runs 0.92–0.95 s of `user` time at N = 1 000 000, recorded beside the probe that reproduces it in `curios/src/tests/codegen/structural.rs`.

Read that workload before reusing it. N is read from stdin and the string decoded is `Nat/to_str(n)`, so the program decodes a **seven-character** string a million times; the ≈135 ns per character quoted beside the figure is derived from that, not from a walk over a million characters. A seven-link chain and a million-link one are different regimes — per-call overhead is amortized differently in each — and anything this campaign concludes about per-character cost must say which one it measured.

The control's figure is anchored nowhere. Neither is the ratio between the two, which circulates in prose here and in the roadmap as "roughly eightfold". A number that no probe reproduces is a number that will be designed against and cannot be checked, so this document quotes exactly one figure and names where it lives.

## The four suspects

| Cost | Who pays it | Status |
| --- | --- | --- |
| The UTF-8 scan, per codepoint | `Str/fold` decodes; `parse_manual` indexes raw bytes and skips it | **Not overhead.** Work the abstraction performs and the control declines to do, so it is a ceiling's worth of difference that no optimization should recover |
| One closure allocated per character | `/std/Str/fold`'s motive is a function, so the fold builds `step₀ ∘ … ∘ base` before anything runs | This document |
| One indirect call per character, through `f` | `Str/fold` is a shared combinator with two call sites, so its function parameter joins to a conflict and never devirtualizes | Its own roadmap item — see *Non-goals* |
| One transient `Option` per character | `/std/Nat/of_str` folds an `Option(Nat)` accumulator through postfix `!`, allocating a value the next step immediately destructs | Named as a suspect by `parse_digits.crs`'s own header and by `structural.rs`'s note; the nearest completed item reported one to two percent and said explicitly that it does not reach this |

The first row is why the gap is a bound and not a budget: some of it is *supposed* to be there.

## The ladder exists. The measurement does not.

`programs/parse_bindless.crs` is the middle rung, and its header states the design outright — the same `Str/fold` over the same codepoints, with `!` written out as an explicit match, so the scan is held constant and only the bind is dropped. Against `parse_digits` it isolates the bind; against `parse_manual` it isolates the closure plus the scan. The three programs already separate what one gap cannot.

Nobody has run them. `parse_bindless` and `parse_manual` appear in **zero** Rust files: no probe carries their numbers, nothing re-derives them when the compiler changes, and the separation the corpus was built for has never once been taken.

The consequence is visible in the roadmap. Two open items are each bounded above by the same undivided gap and each states that its own share of it is unknown; the item that closed nearest to it reported one to two percent while naming two costs it could not tell apart. That is not three cautious decisions. It is one missing measurement, deferred three times, and this document is the fourth deferral unless M1 comes first.

## Why the closure chain is there, and why it is a choice

`/std/Str/fold` is a right fold whose result is a function, so each step returns a lambda capturing the induction hypothesis, and folding a string builds the whole composition before applying it once — N allocations and N indirect calls ahead of any work.

**It is written that way for a reason.** The walk carries `valid : Utf8(sc, b)`, a proof indexed by the scan state, and a structural right fold cannot thread an accumulator left-to-right without making its motive a function of that accumulator. This is the standard encoding of a left fold as a right one.

**The reason does not reach as far as it looks.** Curios also has `rec` with size-change totality, and `/std/Str/utf8` already contains the pattern: `drop_width` recurses structurally on the byte tail and passes the totality check, while `peel_byte` is the transport that carries the scan witness across one byte. What that spelling needs is care rather than machinery — the recursion must go on the *relevant* argument, since `drop_valid`'s trick of recursing on the witness is unavailable to a fold whose result is relevant and whose witness erases — but it needs no compiler change at all.

So there are two spellings of the same walk, with the same meaning, and — if the chain turns out to matter — an order of magnitude between them. The library picked the expensive one and nothing warned it. That, and not the number of call sites, is the finding.

## The question a measurement cannot answer

Does Curios promise that both spellings cost the same?

If it does, a compiler transform is owed regardless of how many sites exist today, and the library fix is a stopgap that hides the cliff rather than removing it. If it does not, that is a stated limit of the cost model, and it belongs in `documentation/DESIGN.md` where a user can read it *before* choosing a spelling — not implicit in a pass that happens not to exist.

This is a language decision, not a pass-selection decision, and the difference matters for what triggers the work: **a corpus of thirteen programs written by one author who now knows which spelling is expensive will report one site forever.** Site-counting cannot answer the question, so it must not be what decides the answer.

## Milestones

- **M1 — divide the gap.** Time the three-program ladder, same tree, same profile, same input, from runs that check each program's output. *Acceptance:* an ignored measurement test carrying the command, the date, the profile and what it last printed, in the pattern `curios/src/tests/codegen/structural.rs` already uses; it reports a per-character breakdown across the four suspects and states which regime it measured. It asserts nothing. No figure it prints enters prose anywhere.
- **M2 — one spelling in the library.** Give `/std/Str` an accumulator-threaded walk and define `fold` over it, so the library offers one idiom rather than two. *Acceptance:* every `/std` and integration test passes; a fixture whose input is runtime-tainted — a closed term folds away before codegen — allocates no closure per character, asserted off `struct.new`/`array.new` counts and failing before the change; M1's record re-run and updated. **M2 lands even if M1 finds the chain is a small share**, because it is one function and it removes the cliff from the standard library; what M1 decides is whether anything *after* M2 is built.
- **M3 — the promise, in writing.** Decide whether Curios promises that a dependently-typed left fold costs what a left fold costs. *Acceptance:* the decision recorded in `documentation/DESIGN.md`, with its rationale and the rejected side. If the promise is made, M4 is owed and the promise is its trigger. If it is refused, the limit is documented, this specification retires, and its roadmap subitem becomes a checked, unlinked summary.
- **M4 — left-fold recovery, if M3 says it is owed.** A `FoldSequence` whose result is a function applied exactly once *is* a left fold written as a right one; recover the left fold. Eligibility, structural in ANF: both arms return a function reference bound in that same block; every tail of the step's function applies the hypothesis exactly once and in tail position; the base's function has the same arity; the fold's result is applied rather than stored; **and the arm blocks are effect-free outside the returned function**, because the original evaluates step blocks backward and the rewrite evaluates them forward, so a `Foreign` or `Cell` statement there would have its order reversed. *Acceptance:* a fixture whose fold returns a function every caller applies emits no per-element closure, failing before and passing after; a fixture shaped like `/std/Str/utf8/check`, which applies its hypothesis and then matches the result, is left alone and still runs; a fold whose result is stored rather than applied is left alone; the emitted module validates before Binaryen sees it; M1's record re-run.

## M4's placement is open, and the obvious answer may be the wrong one

The fold is *legible* in `curios-ersd` — `Rhs::FoldSequence` states the hypothesis binder outright — and that is the whole argument for putting the transform there. It is not the whole question, because the transform has to *emit* something.

Ersd has no forward loop form and no sequence destructuring: `SequenceOp` is length, equality, get, slice, append and concat, and `FoldSequence` is the only eliminator, documented as a right fold. So a worker written there must walk by slice — one O(1) rope view allocated per element, trading the closure for a cheaper allocation rather than removing one — or by index arithmetic, which is precisely the reindexing that was supposed to disqualify Cont.

By Cont the loop already *is* an index loop: `lower_fold_sequence` emits a backward walk `i = len … 0` with a get and a slice per step. Flipping its bounds and threading an accumulator through the loop continuation may well be less machinery than synthesizing a loop where none of the form exists.

Legibility is in Ersd, writability is in Cont, and M4 decides between them on that evidence rather than on which crate noticed the fold first.

## What must not happen

**Do not extend `rebase.rs`'s monoid oracle by analogy.** `Monoid` is keyed on `Operation`, its identity is a `Constant`, its combine builds an `Rhs::Operation`, and its targets are self-recursive functions bound by a single-member `Functions` statement. Composition is none of those. More decisively, it is not the same rewrite: the scalar rebase accumulates *inside* the monoid, whereas this one accumulates in the set the endomorphisms act on — actually composing into the accumulator would allocate exactly what the transform exists to delete. The shared algebra is a slogan, not a code path, and treating M4 as "one more row" will produce a design that reads finished and is not.

**Do not justify the linearity condition by citing the demand lattice.** `Demand::Applied(width)` in `curios-cont` is an *arity*, and uncurrying's `width >= 1` gate is about a thunk's effect timing. The condition M4 needs — the hypothesis applied exactly once — is linearity, which neither of those states. It stands on its own or not at all.

**Do not let a site count decide M3.** See above: the corpus cannot observe the user this question is about.

**Do not put a figure in prose.** Not in this document, not in the roadmap, not in a crate README. Beside the probe that reproduces it, or nowhere.

## Non-goals

- **Specializing `Str/fold`'s `f` parameter**, which its own roadmap item covers. It is a different mechanism and *it does not subsume this one*: devirtualizing the per-character call leaves the chain standing, because the two costs have different causes. M1 is what tells the two items apart, and both should read its result before either proceeds.
- **The UTF-8 scan.** If M1 says the residue is the scan, that is a different subject with a different document, and the reason `parse_manual` is a ceiling rather than an equivalent.
- **`FoldNat`**, which no corpus program instantiates with a function result.
- **Any change to what Core, the kernel, or Cont decide a program *means*.** Every milestone here is about cost.

## Retirement criteria

Before this specification is deleted: M1's record exists and is reproducible, and every figure this campaign produced lives beside it; M2's outcome is recorded either way; M3's decision is written in `documentation/DESIGN.md` with its rationale, since a refusal retires this document rather than failing it; M4, if built, states its eligibility conditions where the transform lives; the roadmap subitem is a checked, unlinked summary; and no reference to this filename, or to the one it replaced, remains.
