# Rebasing a fold out of the endomorphism monoid, so a string walk stops allocating per character

This document specifies removing the closure chain a fold builds when its result is a function — and recommends, on the strength of its own survey, that the library be reformulated before the pass is built.

It succeeds the uncurrying transform, which closed `programs/state_monad.crs` from about fortyfold to about 1.7× and left `programs/parse_digits.crs` untouched. This is what parse costs instead.

## Problem

`/std/Str/fold` allocates one closure per byte. Its erased form is the whole of it:

```text
function /std/Str/fold(s, init, f) {
    v440 = fold-seq[bin:x] s {
        empty => { functions fold/1; return fold/1 }
        step(h, t, ih) => { functions fold/2; return fold/2 }
    }
    v443 = apply v440(Scan/lead(), (init, 0))
    return project.0 v443
}
function fold/1(sc, acc) { return acc }
function fold/2(sc, acc) { … apply ih(sc′, acc′) … }
```

The fold's *result* is a function, so each step returns a lambda capturing the induction hypothesis. Folding a string builds `step₀ ∘ step₁ ∘ … ∘ base` and applies it once, which is N allocations and N indirect calls before any work happens. `programs/parse_digits.crs` runs 0.93 s against `programs/parse_manual.crs`'s 0.11 s at one million characters.

**It is written that way for a reason.** The motive is a function because the walk carries a proof, `valid : Utf8(sc, b)`, indexed by the scan state — and a structural right fold cannot thread an accumulator left-to-right without making the result a function of it. This is the standard encoding of a left fold as a right one, and here dependent typing forces the choice rather than style.

**It is also a monoid.** Functions under composition, with `λx. x` as identity, are exactly the shape `curios-ersd`'s existing worker/wrapper reassociates — `rebase_monoid_recursion` already turns `f(t) ⊕ k` into `w(t, acc ⊕ k)`. The endomorphism monoid is the one it does not carry.

## What the survey found, and why it changes the recommendation

Every corpus program was dumped at `ersd-optm` and every fold node classified.

| | |
| --- | --- |
| `fold-seq` nodes | 2, in every program that touches a string, always the same two |
| `fold-nat` nodes | 0, corpus-wide |
| `/std/Str/fold` | every arm tail-applies `ih` exactly once; the result is applied in the same function — **rebasable** |
| `/std/Str/utf8/check` | applies `ih` and then *matches its result*, and hands the function to its callers — **not rebasable by reassociation** |

**The reach is one site.** Not one per program: one, in `/std`, shared by every program that touches a string. `Str/utf8/check` is the counterexample that shows the shape is not even uniform within the same file — work after the recursive call is a deferred context, not a composition, and no reassociation reaches it.

That is what argues against building the pass first. The chain is a *source-level* choice in one library function, forced by how `Str/fold` threads its proof; a new arena phase built to serve exactly one call site in the standard library is a poor trade against reformulating that function. **Attempt the reformulation first**, and build what follows only if it cannot be done or if a second site appears.

The reformulation is not free either — threading `sc` directly means restating the `Utf8` invariant so the proof does not have to travel inside the accumulator, which is real dependent-typing work. But it is local, needs no compiler machinery, and its failure is the evidence that would justify the pass.

## Design, if it is built

**Extend the oracle, not the mechanism.** `Monoid` in `curios-ersd/src/optimize/rebase.rs` is a closed enum of scalar operators (`NatAdd`, `NatMul`, `NatOr`, and the `Int` rows). The endomorphism monoid is one more row, with composition as the operation and the identity function as the seed. Associativity plus an erasure-stable identity is the only algebraic fact the reassociation consumes, and composition supplies both.

**Extend the target set from self-recursive functions to `Rhs::FoldSequence`.** The existing rebase collects self-recursive functions bound by a `Functions` statement. A fold is already a first-class loop form, and its step arm binds the element, the suffix, and the accumulator — everything the rewrite needs, with no reindexing:

```text
fold-seq s { empty => base; step(h, t, ih) => λa. … ih(a′) … }
  ⟶  rec w(t, a) = match t { empty => base(a); h:t′ => w(t′, a′) }
```

**The eligibility, structural in ANF.** Both arms return a function reference bound in that same block; every tail of the step's returned function applies the hypothesis exactly once and in tail position; the base's function has the same arity; and the fold's own result is applied rather than stored. `/std/Str/fold` satisfies all four and `/std/Str/utf8/check` fails the third.

**Applied exactly once is the condition the scalar rows do not need.** A scalar addend is a value and may be duplicated or dropped freely. An endomorphism is a deferred computation, so rebasing it changes *how many times* the deferred work runs unless the application is unique. This is the third time that fact has been load-bearing — it is the `Applied` point on Cont's demand lattice, and the reason the uncurrying transform gates on `width >= 1`.

**Why the arena and not Cont.** The fold survives erasure as `Rhs::FoldSequence`, hypothesis binder and all. By Cont it has become an index loop recomputing its element with `BinGet`/`BinSlice`, so recovering the fold means a reindexing analysis rather than reading a node. The crate's own boundary says the same: structural and local optimization belongs to Cont, and the arena's leverage is semantic — this invokes a law about what a fold means and nothing else, which is precisely the inverse of the argument that put uncurrying in Cont.

**One justification does not transfer.** The existing rebase calls itself a correctness transform rather than an optimization, because without it the deferred-context corpus overflows the native stack. `FoldSequence` is already documented as O(1) native stack by construction, so this has no stack to save and must stand on the algebra alone.

## Milestones

- **M0 — reformulate `/std/Str/fold`.** Thread the scan state and accumulator directly, restating the `Utf8` invariant so the proof need not ride inside a function-valued accumulator. *Acceptance: `programs/parse_digits.crs` re-timed against `programs/parse_manual.crs` on the same tree, from a run that checks its output; the emitted module allocates no closure per character; every `/std` and integration test still passes.* **If this succeeds, the remaining milestones are not built and this specification is retired against the library change.**
- **M1 — the endomorphism row and the fold target.** The oracle row, the `FoldSequence` target set, and the four eligibility conditions, in one change. *Acceptance: a fixture whose fold returns a function every caller applies emits no per-element closure, failing before and passing after; a fixture that inspects the hypothesis's result is left alone.*
- **M2 — what it bought.** Re-time the parse pair. *Acceptance: the timing comes from a run that checks the program's output, in that same run.*

**The stopping rule.** `parse_manual` also skips the UTF-8 scan `Str/fold` performs, so the recoverable share is smaller than the eightfold gap and is unmeasured. **If M0 or M1 does not move `parse_digits` materially, the residue is surveyed before anything further is built** — the scan itself is then the cost, and it is a different subject.

## Non-goals

- **The argument boundary** — `/std/Str/fold`'s `f` parameter, called through once per character because two call sites disagree. It is its own roadmap item, it is a different mechanism, and *it does not subsume this one*: specializing `f` perfectly still leaves the chain, because the two costs have different causes.
- **A deferred context that is not a composition.** `/std/Str/utf8/check` applies the hypothesis and matches the result. Reassociation has nothing to move.
- `FoldNat`, which no corpus program instantiates with a function result.
- Any change to what Core, the kernel, or Cont decide.

## Rejected

- **Building the pass before trying the library.** One site does not pay for an arena phase, and the site is a shape the library chose.
- **Recovering the fold in Cont.** The element is a `BinGet` on an index by then, so the transform would need reindexing to prove what `FoldSequence` states outright.
- **A general defunctionalization of returned closures.** Retired once already by the uncurrying survey, and it does not reach a chain whose links are built before any is applied.

## Tests

- A fixture whose fold returns a function every caller applies, asserting no per-element closure allocation; it must fail before the milestone and pass after.
- A fixture that inspects the hypothesis's result, asserting it is left alone and still runs.
- A fold whose result is stored rather than applied, asserting the same.
- Inherited: the emitted module is validated before Binaryen sees it.

## Retirement criteria

Before this specification is deleted: M0's outcome is recorded either way, since a successful library reformulation retires this document rather than completing it; the endomorphism row, if built, is stated beside the scalar rows in the oracle that carries them; the survey's counts live with whatever probe reproduces them; the stopping rule is applied rather than deferred; the roadmap subitem is a checked unlinked summary; and no reference to this filename remains.
