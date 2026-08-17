# A variant travels as its fields behind a constant discriminant

## Status

This specification defines the cost contract, evidence gate, design boundaries and acceptance criteria for extending the recorded fields representation from exact products to variants: one scalar discriminant plus payload slots merged per representation class, so a variant value in compiler-coordinated flow stops paying a heap object and a dispatch re-read for a distinction one scalar carries.

It is the successor capability named twice by landed work: [A value costs when it is kept, not when it is named](../../design/toolchain/a-value-costs-when-it-is-kept-not-when-it-is-named.md) records the caller-side scan rebuild as a variant-width flow whose removal is "either variant-aware splitting with per-tag padding or a uniform-width variant lowering, each a measured decision not taken here", and [A variant collapses when nothing needs to distinguish it](../../design/toolchain/a-variant-collapses-when-nothing-needs-to-distinguish-it.md) records the split-return protocol's variant-width decline as the place the two decisions meet. This document adopts the first mechanism and gates the choice on M0's survey; the second is recorded below with its reinstate condition.

**M0, M1 and M3 have landed; M2 is refused on M0's measurement.** The spine ran `M0 → M1 → M3`: the census admitted the conditional annex and refused the return milestone, which is the gate working rather than the plan surviving.

Where the code corrected this document, each correction traceable to the instrument that forced it:

- **The discriminant is not a literal on every edge.** This specification says the origin analysis should learn that "a `Construct` of a known family is exact, and its discriminant is a literal on that edge". By the time a region reaches CPS the return protocol has already delivered constructors as fields, so a resume rebuilds the tuple with its tag in a *parameter*: seven of the scan region's seventeen constructions carry no literal at slot zero. An origin lattice demanding a tag-led construction would decline the one flow this campaign exists for. The fact merges construction *widths* and records no discriminant at all; constant discriminants are then found by the passes that already fold them.
- **The class merge is degenerate, so no width budget was selected.** `curios-cont` types every tuple field `(ref null any)`, so there is exactly one representation class and the class-merged width is the plain maximum arity. The largest observed is 4 against a `PARAM_SPLIT_GROWTH_LIMIT` of 16; inventing a second budget beside it would have been a constant with no measurement behind it.
- **M1's acceptance case needed M3.** The scan's flow class is *known-call*, not continuation-only. Splitting the loop parameter turned three per-arm rebuilds into one materialization at the loop's head, kept alive by `/syn/Str/step` taking the scan whole — a relocation, not a removal. Splitting `step`'s own parameter removed it, and the per-character path now allocates nothing.
- **A site may take a source apart only at a width its own flows settle on.** Projecting every edge at the region's widest compiled, verified, and trapped at run time where `Handle/Read`'s `eof()` rides a one-tuple beside `chunk(b)`'s two. A source that is itself a merged variant is declined at that site until it is split as a region of its own.

What M2 measured, and why it is refused, is below.

Nothing further is planned; this specification retires with the design decision it produced.

## Cost contract

An immutable variant does not require a heap object or a repeated dispatch read while its flow stays within control flow whose representation Curios can coordinate, even when its constructor is not statically decided: it travels as one scalar discriminant plus payload slots merged per representation class across the family's roster, with per-edge filler where a constructor is narrower than its class.

Materialization at an opaque boundary reconstructs the family's at-rest encoding — the one-way door's per-family decision, which this specification never changes. A Tagged family reboxes its tag and payload; a Collapsed family is width-uniform already and is this contract's landed degenerate case; an Immediate family reboxes only its boxed roster and rides the immediate constructor bare, per the encoding decision.

A match against a split region reads the discriminant slot with an ordinary switch — no `IsImmediate`, no tag projection — and binds payload slots per arm. The encoding-specific tests exist only where values rest.

Not promised: at-rest layout changes, field representation inside boxed payloads (`represent.rs`'s locals-only scope and its successor's subject), or equal cost for arbitrary extensionally equivalent spellings.

## Evidence

The one obligation the value-lifetime campaign left on the per-character path is this capability's motivating case: the UTF-8 loop's scan parameter mixes arity-1 interned constants with the resumes' arity-4 rebuilds — a variant-width shape no exact product describes — so continuation scalar replacement declined it, and each arm's resume rebuilds the scan it passes back, priced live beside `returned_scan_is_delivered_as_fields_and_rebuilt_by_callers` in `curios/src/tests/codegen/ladder.rs`.

The split-return protocol declines variant-width components, and the landed Immediate encoding widened that decline: any immediate family's return edges now mix a bare i31 with tuples, so no such family's component can split. The `SPLIT_RETURN` fixture in `curios/src/tests/codegen/structural.rs` keeps both its constructors wider than unary for exactly this reason, and its documentation names this successor.

The encoding decision deferred one at-rest item to "a successor with a measurement in hand": dropping the boxed side's written-but-never-read tag when the immediate test already discriminates. That item is annexed here only as an M0 census output; the rewrite, if any, belongs to the encoding decision, not to this specification.

What no instrument has measured yet: how many join parameters and return components carry variant-typed values outside the scan, by family, roster size and class-merged width. M0 owes that survey before M1 or M2 select anything.

## Existing substrate

This specification is thin because the value-lifetime campaign landed its hard parts as checked substrate.

The recorded fields representation and its verifier rule (`curios-cont`'s `cps/fields.rs`) already make a parameter *be* its fields, with every incoming jump and call held to that shape the way arities are held. A variant region extends the record with a discriminant slot and the family's roster; the decision stays data in the program, per the MLton lesson the product campaign adopted, and a second run of the rewrite finds nothing left to claim.

The forward origin analysis (`cps/origin.rs`) follows exact tuple origins through aliases, continuation edges and known calls. It must learn variant origins: a `Construct` of a known family is exact, and its discriminant is a literal on that edge.

`split_returns` and the return protocol's tail-call-component coordination remain the owner of results. This specification adds a variant-width class shape — one discriminant slot plus the class-merged payload slots — never a competing result ABI.

Interprocedural use demand already defers an argument's demand to the parameter that receives it, and the landed jump threading and interned-constant passes collapse a switch over a literal discriminant on their own, so this specification folds no dispatch and ships no new demand rule.

`family_encoding` at the one-way door (`curios-ersd`'s `into_cont.rs`) supplies the discriminant semantics per family, and `lower_match_variant`/`lower_immediate_match` are the match-side consumers taught to read the recorded representation where a region reaches them.

## Adopted precedents

[GHC's unboxed sums](https://gitlab.haskell.org/ghc/ghc/-/wikis/unpacked-sums) supply the slot model: unarisation merges payload slots by kind across the roster, carries a scalar tag beside them, and fills narrow alternatives per edge — the exact shape of the class-merged region here.

[Lean's compiler IR](https://lean-lang.org/doc/api/Lean/Compiler/IR/Basic.html) supplies the proof-assistant precedent for small linearly consumed sum results traveling unboxed, which supports pairing the region with the existing multi-result protocol rather than inventing a second one.

[Graal's partial escape analysis](https://ssw.jku.at/Research/Papers/Stadler14/Stadler2014-CGO-PEA.pdf) and [OCaml Flambda's recursive-specialization policy](https://ocaml.org/manual/4.04/flambda.html#s%3Aflambda-unboxing-specialised-args) carry over from the product campaign unchanged: materialize at the escaping branch alone, retain wrappers at other entries, bound growth, and refuse any rewrite that would inhibit tail calls.

## Common model

A variant region is a variant origin plus every parameter carrying it, traveling as a discriminant plus the class-merged slots downstream demand reads.

Eligibility mirrors the product rules — exact shape known in CPS, rewriteable receivers, per-edge field availability with an alias of the region's own parameter read as available, exclusive merges, tail-call preservation, and growth within a measured bound — plus one fact that always holds: the family's roster and encoding are closed at the door, because the lowering runs whole-program.

A nullary constructor's edge carries its literal discriminant and filler for every payload slot; an immediate-unary constructor's edge carries its payload in its class slot, and the region needs no `IsImmediate` because the discriminant already says which constructor traveled.

Materialization at an ineligible boundary reconstructs the at-rest encoding for the discriminant's constructor. A mixed-origin merge or unknown transfer is a materialization boundary, not a disqualification of the rest of the region; the first implementation may conservatively reject a candidate whose partial materialization needs path-sensitive state, and the tests must distinguish that implementation limit from the cost contract.

## Rejected pending measurement — uniform-width variant lowering

The alternative the design record names: pad every constructor of a family to one arity at the one-way door, so the at-rest encoding itself is width-uniform and the existing exact-product machinery splits it with no representation extension. It is rejected as the adopted mechanism because it pays at rest for a problem that exists in flight — every stored value of the family carries filler fields for its widest sibling, in exactly the storage the value-lifetime campaign structurally cannot reach. Reinstate condition: M0's survey showing a family whose values essentially never rest, where uniform width at the door is the cheaper spelling for that family alone — a per-family door decision, like every encoding.

## Measurement gate

M0 extends the aggregate-flow census (`aggregate_flow_census` in `curios/src/tests/codegen/census.rs`) with variant classification: variant-typed join parameters and return components over the optimized corpus and `/std`, by family, roster size, per-class merged width, and flow class — continuation-only, return, known-call, blocked.

Any width, growth or filler budget is selected from that survey and recorded beside the instrument that justifies it, never assumed.

Attribution corpora must include multi-byte text: the value-lifetime campaign's recorded lesson is that a digit corpus never executes the variant arms and reports their absence as yield.

Evidence that would stop the campaign: the survey showing variant-width flows rare outside `/std/Str`'s scan and the Immediate-widened return components, or the ladder showing the scan rebuild's share too small to justify the representation extension. Reaching it is a result rather than a failure.

## Milestones

The spine as written was `M0 → M1 → M2`, with M3 a conditional annex presumed stopped. The spine as run was `M0 → M1 → M3`: the census admitted the annex and refused the return milestone.

### M0 — Census extension — **landed**

- Extend the census with the variant flow classification above, over the corpus and `/std`, with multi-byte fixtures in the attribution set.

- Select the growth and width budgets from the observed candidates and record them beside the instrument.

- Report the known-call flow class explicitly: that report is M3's admission gate, and an empty one retires M3 without touching the spine.

- Report per-family populations whose values never rest: that report is the uniform-width alternative's reinstate gate.

- Measure the boxed-tag annex: how often an Immediate family's boxed constructor's tag field is written and never read, for the encoding decision's deferred item to consume.

What it reported is recorded beside `aggregate_flow_census` itself. In summary: thirty-eight variant regions over fourteen programs but only three distinct shapes, the class merge degenerate at one representation class, the known-call gate non-empty and naming the scan, and the return gate three functions of which none is evidence for a return mechanism.

### M1 — Join parameters — **landed**

- Extend the recorded fields representation with the discriminant slot and roster, and give the verifier the rule that holds every incoming edge to the region's shape.

- Teach the forward origin analysis variant origins, and split variant regions through continuation parameters and loop backedges — the central case, exactly as it was for products.

- Acceptance case: the caller-side scan rebuild leaves the per-character path — the loop's scan parameter becomes a discriminant plus three slots, the interned-constant edges carry filler, and the ladder's scan probe flips to pin the new shape.

- Evidence: the ladder's walk figures beside their probes, an encoding-level test pinning the recorded variant representation, and verifier tests for the new rule.

**What landed instead of the first bullet.** No discriminant or roster is recorded, because none is needed: a `FieldGroup` already says which parameters are one former aggregate's fields, and the verifier already holds every incoming edge to the continuation's arity. What the fact had to gain was the *set of widths* reaching a parameter, which lives in the origin lattice rather than in the record. The third bullet landed as a relocation rather than a removal — see M3.

### M2 — Returns — **refused on M0's measurement**

- Add variant-width classes to `split_returns`: one discriminant slot plus the class-merged payload slots per tail-call component, per-edge filler, coordinated over the same undirected components the protocol already owns.

- This retires the protocol's variant-width decline and the narrowed premise `SPLIT_RETURN` carries; the fixture returns to a shape that exercises the decline's absence, and its documentation records the premise change.

- Acceptance case: an Immediate-family component splits — the exact shape the landed encoding created and the protocol declines today.

**The population is three functions corpus-wide and none of them is evidence for the mechanism.** `/decode/1` and `/std/Nat/of_str/1` hand back `{Tuple(1), Tuple(2)}` and both *escape*, so the protocol declines them for the escaping reason and no width class would reach them. `/build` hands back `{Tuple(4), Bare}` — the immediate-family shape this milestone was written for — but a `binary_trees` node is stored in its parent, so its values rest: splitting that return would relocate the allocation into the caller rather than remove it, which is exactly the reboxing balance M3's own admission clause names as disqualifying. Building the mechanism would also cost more than the join-parameter one: an immediate family's at-rest encoding is not a tuple, so a resume would have to materialize it behind a switch on the discriminant, and nothing in the chain correlates that switch with the caller's own `IsImmediate` test.

Reinstate condition: a variant-width return component whose values do not rest, reported by the census's return gate — which is the instrument to re-run, not a judgement to re-make.

### M3 — Known-function variant workers — **landed, promoted into the spine**

- Admission-gated on M0's known-call report, exactly as the product campaign's worker milestone was gated, and additionally on the reboxing balance its adopted GHC precedent names: a split whose callers keep rebuilding the box costs more than the box it removed.

- Inherits the product worker rules wholesale — wrapper ABI for escaping callers, consistent rewrite of the recursive argument-flow component, tail-call preservation, growth bounds.

- The spine neither waits for it nor depends on its verdict.

**The last bullet is what the measurement overturned: the spine did depend on it.** M0's known-call report is non-empty and names the scan, so M1's own acceptance case — the rebuild leaving the per-character path — was M3's to finish. There is also no wrapper: the product rules assumed one for escaping callers, and refusing an escaping function outright is both narrower and sufficient, since a non-escaping function's every call site is a `Known` one the rewrite reaches.
