# Iterative elaboration of deep spines

Working implementation specification for removing the last native-stack dependence on input depth in `curios-core`: `elaborate` recurses once per un-shared spine node, so a closed N-byte string literal still costs ~N native frames even with the elaboration cache collapsing every shared chain. This is the deferred completion recorded in [DESIGN.md — Deep shared terms are cached, not special-cased](../DESIGN.md); the landed halves of that decision (the elaboration cache and the reducer's `PendingMatch` scrutinee stack) are prerequisites and precedents, not part of this work.

## Objective

Elaboration depth bounded by binder nesting of the written program, never by data length. A literal or generated spine of any size elaborates on a default 2MB thread; the practical size ceiling becomes the reduction deadline and memory, not the stack. Measured status quo (2026-07-21, ≈27KB per elaborate level debug, ≈6.5KB release): the debug CLI at 2MB fails between 50 and 56 literal bytes, release at 8MB fails between 1000 and 1500. The cliffs bite freshly elaborated user-program literals (the `strings.rs` probe), not the prelude's: prelude literals elaborate once at compiler build (`build.rs`, on the build-script main thread), restore by rkyv deserialization rather than re-elaboration, and have their `valid` derivation erased before ersd, so a longer one stresses only the build, never the 2MB test or replay threads — the current longest, `Toml.crs`'s 84 bytes, already compiles.

## Problem shape

A lowered string literal's `valid` field is a right-nested constructor spine: `more(c, st, t, more(…))`, one link per byte, each `rest` argument occurring exactly once. The elaboration cache collapses the `Rc`-shared scan-state chains hanging off every link, but an un-shared spine node has no second occurrence to hit the cache, so the cycle `elaborate → elaborate_apply → check(rest) → elaborate` still nests one native level per link. The class is general — any right-nested constructor data, machine-generated argument spines, deep user terms — and the reducer already solved its instance of it: `reduce` runs match scrutinees on an explicit frame stack, probing the cache at frame push and recording at frame pop (`Context::cached_reduced` / `Context::reduce`), the defunctionalized form of the old bracket.

## Decision record

Chosen: defunctionalize the `elaborate → elaborate_apply → check` cycle onto an explicit frame stack inside `elaborate`'s entry — the reducer's `PendingMatch` move one level up. A frame captures the telescope-walk state of one application whose argument is being elaborated; a finished child result resolves against the frame stack instead of returning through native frames. `Context::get_or_init_elaborated`'s bracket tears into probe-at-push and record-at-pop halves exactly as the reduction cache's did, with the same purity condition (the `mutation_stamp` snapshot) carried per frame; record-at-pop fires when the frame finalizes (after its `Check`-mode `expect`) and stores the un-restamped result, so the per-occurrence span stamp stays outside the bracket exactly as today.

Rejected alternatives, recorded so they are not relitigated:

- Growing the stack (`stacker`-style segmented stacks, rustc's `ensure_sufficient_stack`): hides the depth instead of removing it, violating the invariant that recursive lowering works on default stacks; adds a native dependency to the pure pipeline; and degrades to a plain call on wasm32, so `curios-web` — the most stack-constrained product — would keep the cliff unchanged.
- Bigger threads or `RUST_MIN_STACK`: explicitly forbidden by AGENTS.md; the cliff would merely move with the configured size and the arena replay threads would need per-input sizing.
- A flat spine-shaped `Term` node (the `binding.rs` flatten-and-loop precedent, where let-chains became one `Let` with a `Vec` of bindings): let-chains are one syntactic form, but argument nesting is data-shaped and appears in any argument position of any head, so a flat representation either special-cases particular heads — the `Str`-shaped exception the owning DESIGN.md entry rejects — or burdens every `Term` consumer (conversion, erasure, printing) with a second application spelling.
- Emitter- or `/syn`-side evidence redesign: rejected in the owning DESIGN.md entry; the language may write deep certified data and the compiler must carry it.

## Feasibility and fidelity seams

The frame-state inventory is the feasibility gate, and the de-risking read of `elaborate_apply` discharges it. Its four re-entry points into checking — the all-auto saturation loop, the main telescope walk, the postponed-argument re-check loop, and the post-`expect` re-check — resume from owned, `Clone`, mostly `Rc`-backed data (telescope cursor, rebuilt-argument accumulator, plicity queues, postponement bookkeeping, mode); none of it borrows the context across the child check, and the hot path enters no context frame (`with_frame` lives only in `elaborate_func_type` and the once-per-struct `elaborate_struct`, off the spine), so the whole state moves to the heap with no borrow-checker or scoping obstacle. On the measured spine only the main telescope walk is even live — `more` is checked (the literal's `valid` field drives the spine in `Check` mode) and insertion-free (four explicit parameters, no implicit or witness slots), so the all-auto loop is skipped, no metavariable is minted (the link elaborates purely), and `blocked_on_metavar` is false everywhere, leaving nothing postponed — so the depth is entirely the last explicit slot's `check(rest)`, and the other three points carry load only for the general case the frame must still represent.

Two seams stay put by construction. Span restamping remains at the (now iterative) `elaborate` entry, outside the cache exactly as today. Error propagation becomes stack-drain-and-return instead of `?`-unwinding: `Error::at` is first-wins (an already-located error returns unchanged) and `with_span` is innermost-wins, so native unwinding reports the deepest node's span, and the drain must stamp innermost-frame-first to report the same one.

## Scope and staging

1. Frame-state inventory — complete (above): the gate passed, and on the measured spine only the main telescope walk's `check(rest)` is load-bearing.
2. Defunctionalize the ground all-explicit application: the iterative fast-path fires only when the term is ground and every slot is explicit — which provably neither inserts an implicit nor postpones an intro form — and every other application falls through to the unchanged native `elaborate_apply`. This is the whole measured surface: `utf8_derivation` is the only lowering that emits a data-driven constructor spine, and it is insertion-free. Other child positions — the application head (a bare `Var` on the string spine), tuple and struct fields, match arms, telescope types — keep native recursion, their depth tracking written binder nesting; the fast-path frame is shaped so one of them can adopt it later without redesign if evidence ever shows data-driven depth there.
3. Re-measure the cliffs with the existing `lit_N` probe method; grow the in-suite literal test (`curios/src/tests/strings.rs`) past the old cliffs; amend the DESIGN.md Deferred paragraph and the corresponding memory notes.

Independent companion, not gated on the above: the elaboration cache still clears on every top-level definition. The recorded relaxation is an insert-time gate — refuse to cache a result that names a not-yet-defined global (the name analogue of the reduction cache's unsolved-metavariable rule, sound because items elaborate in dependency order), read off the already-cached per-term `free_vars` sets — which lets entries survive `define` by dropping only its clear while the frame-exit and refinement clears stay. It can land before or after the iterative rewrite.

## Not in scope

- The `curios-ersd` exponential `!`-bind-chain defect — a separate investigation with its own cause.
- Reduce-side corners (`canonical_scrutinee`'s nested argument reduction on refined heads) — pre-existing, unmeasured, and untouched by elaboration changes.

## Validation

The standard gate, plus: the arena erasure tests keep passing on default stacks; the release `lit_2500` probe — today a stack overflow — must print; the debug 2MB cliff must move from ~50 bytes to deadline-bound (`convert` is worklist-based and `reduce` runs scrutinees on its own frame stack, with `Rc::ptr_eq` reflexivity collapsing the shared scan-state index, so no downstream pass reintroduces per-link native depth to relocate the cliff); and the postponement-heavy prelude modules (implicit-rich `/std` code) must elaborate identically, which the prelude archive rebuild checks at compiler build.
