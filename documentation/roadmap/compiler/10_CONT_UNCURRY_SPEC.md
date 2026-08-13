# Moving an application into the function that returns it, so a monadic step stops allocating

This document specifies absorbing, into a function, the application its returned closure always receives — so the closure is never built and the call through it is never indirect.

It succeeds the return protocol, and stands on the copier that work needed: `82cb8ef7` through `729fadb1` made every body copy reproduce the definitions nested inside it, at a cost of 0.3% emitted size and no measurable time.

## Problem

A `State(S, A)` is a struct over `(S) -> {A, S}`, so an action *is* a closure and `bind` builds a new one. Threading a counter through twenty million steps costs **1.08 s** against the hand-threaded control's **0.01 s** — about fifty-four nanoseconds per `!`, and roughly a hundredfold. That is the largest cost this compiler has measured.

**It is not the witness, and not the desugaring — though the witness is real for longer than it looks.** Elaboration resolves which `satisfy` applies, but erasure still *materializes* it: `programs/state_monad.crs` reaches Ersd with the witness as a `product` of its fields, `bind` fetched from one of them, and the call through it indirect. Cont's constant propagation is what folds that away, so no `Monad` survives into the optimized CPS of any program here — the dictionary exists, and is already eliminated. Nor is `/syn/Monad/bind` itself the cost: `programs/parse_bindless.crs` folds exactly as `programs/parse_digits.crs` does with `let n = acc!` written out as a match, and the two are **0.93 s apiece** — identical. `Option/bind` is small enough to inline, so a `!` over it is already free. What costs is the closure, wherever the monad's carrier makes one.

**And the closure is never needed.** Surveying every function that returns a bare function reference, across four programs:

| Function | Call sites | Applied | Tail-forwarded | Otherwise used |
| --- | --- | --- | --- | --- |
| `/std/Handle/write` | 5 | 4 | 1 | **0** |
| `/std/Handle/write/1` | 1 | 1 | 0 | **0** |
| `go` | 3 | 1 | 2 | **0** |
| `go/1` | 1 | 1 | 0 | **0** |
| `/loop` | 2 | 1 | 1 | **0** |
| `/std/State/bind`, `/run` | 2 each | 1 | 1 | **0** |

Every non-tail use is an immediate application, with no exceptions in any program. Nothing stores a returned closure, passes it onward, or returns it other than by tail-forwarding. And the first four rows appear in *all four* programs — they are `Io`'s description machinery, so this is not a `State` quirk but a cost every program that performs I/O pays.

**What is not missing.** `rewrite_atoms` already promotes `CpsCallee::Closure(v)` to `Known(f)` once `v` is known, and inlining, contification, projection forwarding and the return protocol all run behind that promotion. The absent fact is that the application is unconditional.

## Design

**Absorb the application.** A function `f : A -> (B -> C)` whose every use applies the result becomes `f' : (A, B) -> C`, and each return edge `jump k[Fun(g)]` becomes a tail call `apply Known(g)[b]`. That turns `g` into a known callee, after which the existing chain takes the tower apart.

**It does not need to know which closure comes back**, and that is what makes it small. A function returning one of several actions — `/loop` returns `State/bind/1` or `State/pure/1` — is rewritten uniformly, because every branch is applied to the same argument. Nothing has to enumerate the possibilities or dispatch on them.

**One more point on the demand lattice.** The return protocol keys facts as `Unused < Projected(indices) < Opaque`; a closure callee currently joins `Opaque`, which is what hides this. Adding `Applied(arity)` beside `Projected` states it, and eligibility is then a lattice question rather than a bespoke walk.

**Per tail-call component, for the same reason as the return protocol.** Tail-forwarding is common — `go` forwards twice, `/loop` and `Handle/write` once each — and a function that returns what another returned must be uncurried with it. The undirected connected components of the tail-call graph are already what `return_call`'s result agreement forces, and the same partition serves here.

**Why Cont and not `curios-ersd`, which owns worker/wrapper.** Because the licensing fact is not visible upstream. At Ersd `loop`'s result is handed to `/std/State/run` rather than applied, and `bind` is not yet a known callee but a field fetched from a witness product — so *"every use of this result is an immediate application"* is simply false of the erased module, and becomes true only after Cont's constant propagation and projection forwarding have manufactured the shape. The crate's own boundary says the same thing from the other side: `curios-ersd`'s optimizer states that structural optimization belongs to Cont and the arena's leverage is semantic, and this transform invokes no law about what anything means. The worker/wrapper precedent does not transfer either — that one is the *monoid* rebasing, which is semantic, and shares only a name.

**The transform must not spend the tail position the closure return was providing.** Returning a closure is why the loop is flat: `loop(n)` hands one back in constant time and the caller drives the iteration, so no frame outlives a step. Absorbing the application turns that into a genuine call chain, and it stays flat only if the call becomes a tail call — which it does not automatically, because the continuation that received the closure is kept and still binds the continuation the application resumed into, so its body is a `LetCont` before a jump rather than the bare `ApplyCont` that `forward_continuations` collapses. A first attempt overflowed the stack at twenty million steps for exactly this reason while passing every test at twelve. **Either the class is declined, or the tail position is re-established before the rewrite is admitted; it is not optional.**

**Escaping functions are excluded**, because a closure reaches its target through the arity-keyed `clsr/{arity}` supertype, and uncurrying changes the arity. This is the return protocol's rule unchanged.

**Worker/wrapper covers a mixed use, and is expected to stay dead.** Where some use is not an application, the original is kept as a wrapper that builds the closure and the applying sites call the worker. No such case occurs anywhere in the corpus, so the wrapper should be pruned in every instance observed — it exists so the transformation is total rather than conditional on a survey continuing to hold.

## Milestones

- **M1 — `Applied`, and the rewrite it licenses.** The lattice point, the eligibility it defines, and the uncurrying itself, in one change. Not split: an analysis with no consumer cannot land without a dead-code suppression, which the return protocol's own decomposition discovered the hard way. *Acceptance: a fixture whose callee returns a closure that every caller applies emits no closure allocation and no `call_ref` for it, failing before and passing after; the emitted module validates before Binaryen sees it; size recorded.*
- **M2 — the tail-forwarded component.** Propagate the decision across tail-forwarding so a chain is uncurried together, and a member that cannot be leaves the whole class alone. *Acceptance: a chain whose members would otherwise disagree is decided together, in both directions.*
- **M3 — what it bought.** Re-time `programs/state_monad.crs` against `programs/state_manual.crs`, and `programs/rng_state.crs` against `programs/rng_manual.crs`, on the same tree with the pass toggled. Then the corpus: every program carries `Io`'s description machinery, so a figure that moves only the monadic benchmarks means the transformation is not reaching what the survey says it should. *Acceptance: every timing comes from a run that also checks the program's output, in that same run. A trapping program reports near-zero time, so a measurement that does not verify what it computed can report a hundredfold win for a crash — which is how the first attempt at this milestone read until its output was checked. The timings and the size delta are recorded beside a probe that reproduces them, never in prose.*

**The stopping rule.** `state_manual` and `rng_manual` exist to be diffed against their monadic twins. **If M3 does not close most of the hundredfold, the remaining milestones are not written and the residue is surveyed before anything else is built.** The gap is large enough that a small improvement means the mechanism was wrong, not that it needs tuning.

## Non-goals

- **Dictionary elimination — because it already happens, not because there is nothing there.** The erased module does build a witness product and does call `bind` indirectly through it; Cont's existing constant propagation and devirtualization remove both before emission. Stating it the other way round would tell a later reader that the language never builds one, which is false and would send them looking in the wrong stage.
- **Inlining `bind` harder.** Measured free where it inlines and already inlined where it matters; `State/bind` survives only as its inner lambda, which is the closure this document removes rather than a call this document could avoid.
- **The argument boundary** — a known function passed *into* a callee, which `/std/Str/fold`'s two disagreeing call sites need. It is a different mechanism against a smaller measured ceiling, and it is its own roadmap item.
- Defunctionalizing a closure that is not always applied.
- Re-keying the closure type families, which stay single-result and arity-keyed.
- Any change to what Core, Ersd, or the kernel decide.

## Rejected

- **Defunctionalizing the return instead**, giving each returned function a tag and dispatching at the call site. It was the previous specification's plan for this shape, and it is strictly larger: it needs a set-valued fact about which functions a call can return, plus a dispatch, to buy what an unconditional application gives for free. The survey is what retires it — with no use that is not an application, there is nothing to dispatch on.
- **Raising the inline budget.** `State/bind` is already inlined; what remains is the carrier's own lambda, which no budget removes.
- **A general control-flow analysis.** It would subsume this and the argument boundary both, and would still need this rewrite to act on what it found.

## Tests

- A fixture whose callee returns a closure that every caller immediately applies, asserting no closure is allocated for it and no `call_ref` remains. It must fail before the milestone and pass after.
- A fixture returning *different* closures from different branches, asserting the rewrite does not depend on knowing which — the property that separates this from defunctionalization.
- A tail-forwarding chain whose members would otherwise disagree, asserting they are decided together.
- A fixture whose returned closure is stored rather than applied, asserting the wrapper survives and the program still runs.
- An escaping function is left alone.
- Inherited: a value free in another function's body stays boxed, and the emitted module is validated before Binaryen sees it.

## Retirement criteria

Before this specification is deleted: the `Applied` lattice point and its eligibility are recorded where the demand lattice states its others; the component discipline is recorded beside the return protocol's, which it shares; M3's timings and size delta live beside a probe that reproduces them; the stopping rule is applied rather than deferred; the roadmap subitems are checked unlinked summaries; and no reference to this filename remains.
