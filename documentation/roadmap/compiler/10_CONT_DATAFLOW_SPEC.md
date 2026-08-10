# A dataflow substrate for `curios-cont`, and unboxed scalars as its first payoff

This document specifies extracting the dataflow machinery `curios-cont` already contains — one hand-rolled lattice welded to the specializer — into a substrate the whole optimizer shares, then proving it with two clients: a re-hosted constant propagation that must reproduce today's output exactly, and a representation analysis that lets scalars live in Wasm registers instead of round-tripping through `i31` references.

## Problem

Every value in an emitted module is a reference, including small integers. That is what makes the interfaces compose — one closure type per arity, one field shape per constructor — but it is paid for at every value, including inside a loop where nothing is interoperating with anything.

The `lcg` benchmark kernel is the clearest reading. Its loop body carries **38 reference-plumbing instructions** — 7 `ref.i31`, 9 `i31.get_u`, 10 `ref.cast`, 12 `ref.as_non_null` — plus 52 local loads and stores, against roughly six arithmetic operations. Worse, `75 * x` is computed in *64-bit* arithmetic with a shift and a conditional trap, purely to detect whether the product still fits the 31 bits an `i31` holds: a 32-bit multiply became five instructions and a branch because of how the value is stored. `lcg` has sat within half a percent of the same number across five benchmark runs and 416 commits, which is what a cost the compiler never addresses looks like.

The optimizer cannot currently reason about any of this, and that is the deeper problem. Its fifteen passes are syntactic rewrites over facts each recomputes per round; there is no shared notion of what is known at a program point, so every new analysis would re-implement a lattice, a worklist, and recursion handling from scratch.

## Constraints, verified

- **The lattice already exists, as exactly one instance.** `curios-cont/src/cps/specialize.rs:12` defines `Knowledge { Unknown, Known(CpsAtom), Conflict }` whose `join` is documented as "Lattice join for the SCC-invariant fixpoint, ordered `Unknown < Known(_) < Conflict`"; `invariant_fixpoint` (`:100`) is a worklist solver over call-site constraints; `scc_invariant_knowns` (`:46`) drives it, restricted by `eligible_sccs` to recursive components containing neither an escaping member nor the entry. It is a complete constant-propagation dataflow analysis, private to the specializer and reusable by nothing.
- **The call graph is rebuilt four times per round.** `analyze_calls` is invoked independently by `known_values` (`analysis.rs:254`), `inline.rs:15`, `contify.rs:11`, and `specialize.rs:154`, inside a fixpoint whose backstop is `ROUND_LIMIT = 1024`. Measured on `binary_trees`: 25 rounds; `ROUND_LIMIT`'s own documentation records corpus programs converging up to 191. `cont_optimize` is 72.4 ms of a 258.3 ms release compile of that program — 28%, and the largest block of the compiler whose internal distribution is unmeasured.
- **The representation demand is already fully written down, in the wrong place and the wrong form.** `LoadAs` (`into_wasm/context.rs:720`) and `WrapAs` (`into_wasm/code_emitter.rs:8`) are the coercion vocabulary, and there are **130 `LoadAs::` decision sites** — 97 in `code_emitter.rs`, 26 in `context.rs`, 6 in `expr_emitter.rs`, 1 in `module_emitter.rs`. Each states imperatively that a given operand must be a `Nat`, an `Int`, a `List`. Nothing joins them, and nothing upstream can read them.
- **`WrapAs` says how to wrap, never whether to.** The storage representation is not a choice: `Table::top_type` is the single spelling of "reference to anything", and its sites partition by scope — local declarations (`expr_emitter.rs:248,343`), the three function-type families (`module_emitter.rs:331–371`), struct fields (`:242,308`), and list elements, `Cell`, and globals (`:172,221,453`).
- **Block parameters are locals, not Wasm block parameters** (`block_param_locals`, `expr_emitter.rs:311`). Agreement at a join is therefore agreement on a local's declared type, needing no block-type machinery.
- **Information is discarded, not absent.** `machine.rs:27` has `MachineOperand::{Value, Literal}`; every operand position in the emission IR is an `EmissionValueName` (`into_wasm.rs:75`, and the jump, match, and call targets alongside it). `structurize` must therefore invent a binding for each literal operand, which codegen then materialises as box, store, load, cast, unbox.
- **Ersd and Cont already know every carrier exactly.** `curios-base/src/scalar.rs` fixes `Nat` as `u32`, `Int` as `i32`, `Flt` as binary32, and states that the runtime's `i31` envelope "appears nowhere in this module". No type information has to be recovered from Core's dependent types; the boxing is entirely an `into_wasm` decision.
- **The browser ships the unoptimized module.** `curios-web` deliberately excludes Binaryen, so whatever the emitter produces is what executes there. Native gains are additionally unquantified, because nothing parses Binaryen's output — that is [09_WASM_OPTM_STAGE_SPEC.md](09_WASM_OPTM_STAGE_SPEC.md)'s subject, and it is blocked behind a Wasm binary reader, so it is not a prerequisite this work can lean on.
- **Emission was not byte-reproducible, which is what made this specification's acceptance criteria checkable in the first place — _fixed before M1 landed._** The same binary on the same input emitted the closure type section in a different order each run, because `Table::clsrs` iterated a `HashMap` and Rust randomises that order per process. Measured on `lcg`: 92 differing lines, no instruction-level difference, the same multiset — a pure permutation, so no program was ever miscompiled. The cure is *not* `BTreeMap`: `curios-base/src/macros.rs` records that `name!` withholds `Ord` deliberately, because deriving it once let collation order become emitted tag order, so renaming a case silently renumbered its neighbours. The rule it states is to carry an explicit sequence where order is load-bearing — and one already existed, in `EmissionModule`'s own ordered `Vec`s. The iterating accessor is gone; the map stays an index; the emitter walks the source. Exactly one accessor was at fault: `consts` and `funcs` are looked up, never iterated.
- **The acceptance harness exists.** `curios/src/tests/codegen/structural.rs` compiles fixtures to the raw pre-Binaryen module and asserts structural properties, locating hot kernels by a baked constant (`65537` for `lcg`, `1000003` for `trees`) and never by source name. `curios-cont` carries 41 optimizer tests and 36 emitter tests beside it.

## Design

**The substrate.** A `Lattice` trait — bottom, join, and the laws the solver depends on — plus a solver that iterates a client's constraint system to its least fixpoint, in `curios-cont/src/cps/dataflow.rs`. It absorbs `invariant_fixpoint`'s loop and convergence test.

Facts are keyed by `CpsValueId` alone. The draft of this document listed value, function parameter, and continuation parameter as three key spaces; they are one, because a function's and a continuation's parameters *are* values. In CPS the keying is nearly free for the same reason the substrate stays small: a continuation's parameters are the join points and the term nesting is the dominator tree, so the structure a dataflow framework normally computes is already syntactic. Path-sensitive refinement — knowing a scrutinee's tag inside a `Switch` arm — is deliberately out of scope; it is a per-program-point extension the keying can grow into, and no client below needs it.

Two properties the substrate must preserve, both discovered by implementing against the existing analysis and both silent if broken:

- **Absence is not `bottom`.** `resolve_atom` reads an unseeded value as an unobservable runtime value forcing `Conflict`, distinct from a seeded value still sitting at `Unknown`. The solver therefore hands out its fact map rather than a total lookup that would answer `bottom` for both.
- **`Knowledge` carries two combining operations that are not the same function.** `merge` reads its `None` as "a caller I cannot observe" and lets it force `Conflict`; the lattice `join` treats `Unknown` as the identity. They disagree on exactly `(Unknown, None)`. Only `join` is the lattice operation; `merge` stays the client's observation step.

**What the substrate does not do.** Hoisting `analyze_calls` out of the per-round rebuild was specified here and is unsound: every pass takes `&mut CpsModule`, and `inline_known_calls` rewrites the call graph outright, so an analysis built at the top of a round is stale before `contify_calls` reads it. Sharing those rebuilds needs an invalidation scheme — a pass declaring whether it disturbed the call graph — which is real work on a stage worth 28% of a compile, and is named in the successor rather than smuggled in here. With that hoist gone, **M1 buys no measured performance at all**; its return is that every analysis after it stops carrying a private lattice, and the byte-identical criterion is what proves it cost nothing.

**Client one: constant propagation, re-hosted.** `known_values` and `scc_invariant_knowns` become instances of the substrate rather than bespoke code. This client is chosen precisely because it already exists and is already tested: it validates the substrate against known-good output before anything new depends on it.

**Client two: representation analysis.** The demand smeared across 130 emitter sites is lifted onto the `CpsIntrinsicOp` roster as data — each intrinsic declaring the representation it demands per operand and produces as its result — so it is stated once, where the optimizer can read it. The analysis is then a backward join over that table: a value may be raw when every use accepts its carrier and it does not escape into a field, a capture, a call argument, or a host call, with a forward agreement check across the incoming edges of each block-parameter local. `LoadAs` and `WrapAs` become genuine coercions between what a binding holds and what a use wants — frequently a no-op — and `expr_emitter`'s two local declarations take a computed type instead of `top_type`.

Scope is **locals only**. Nothing crosses a function boundary, so the closure type families, struct fields, and the host ABI are untouched, and no two parties must agree on a representation. That restriction is what keeps this client small; lifting it is the successor's subject.

**Client three: demand analysis.** A backward analysis of which parameters and results are actually consumed, subsuming `eliminate_dead_parameters` and standing as the prerequisite for the successor's constructed-product work.

## Sequencing and milestones

- **M1 — the substrate, with the fixpoint re-hosted. _Landed._** The `Lattice` trait, the solver, and `invariant_fixpoint` moved onto them. `known_values`'s remaining four steps stay client code and were never candidates: only one of its five steps is a fixpoint, the others being a syntactic scan, a non-iterated per-call-site join, a record step, and a chain collapse. *Acceptance, met: `ersd-optm` and `cont-optm` byte-identical over the benchmark corpus, `cont-optm` being the stage this milestone changes.* Landed as its own commit with no user-visible payoff, deliberately.
- **M2 — the demand table.** The per-operand representation demand lifted from the 130 emitter sites onto the `CpsIntrinsicOp` roster, with the emitter reading the table instead of restating it. *Acceptance: emitted Wasm byte-identical.*
- **M3 — representation analysis and unboxed locals.** The analysis, the coercion rework, and computed local types. *Acceptance: a `structural.rs` fixture asserting the `65537` kernel's loop carries no `ref.cast` and no 64-bit widening, plus the whole corpus unchanged behaviorally.*
- **M4 — demand analysis.** Reproducing `eliminate_dead_parameters`, then extending it.

M1 and M2 are refactors with mechanical acceptance criteria and no behavior change; M3 is the first milestone that alters emitted code. Splitting M2 from M3 is what keeps a representation regression attributable to one commit.

## Non-goals

- Unboxing across function boundaries, in struct fields, in `Cell`, in list elements, or in globals. Each requires two parties to agree on a representation, which is where layout enters a *signature* and becomes a type rather than a decision.
- Constructed-product results, Wasm multivalue returns, and multi-return continuations. They share this substrate and the worker/wrapper mechanism, and they are named in the successor below.
- Path-sensitive facts, as above.
- Any change to what Core, Ersd, or the kernel decide. This is a backend representation question throughout.

## Rejected

- **Layout types in the CPS IR, or in `curios-ersd`.** Representation is a property of the backend boundary, not of the semantic IR; `curios-ersd/src/optimize.rs` already states that all local and structural optimization belongs to Cont. Ersd additionally runs before CPS has decided what is a block parameter versus a call argument, which is exactly the distinction that separates this specification from its successor.
- **A peephole collapsing `ref.i31`/`i31.get_u` pairs during emission.** It treats the symptom at the last stage, informs no earlier decision, and leaves the demand restated at 130 sites — where the next operand position added recreates the problem.
- **Widening the emission IR's operand positions to a name-or-literal sum, alone.** It is the correct shape and it is subsumed: once bindings carry a representation, a literal operand is materialised as an immediate for the same reason every other raw value is. Doing it first would be a second, partial mechanism for one case of the general one.
- **Moving to a direct-style IR with explicit join points**, as GHC did in *Compiling without Continuations* (PLDI 2017). CPS already supplies join points and dominance syntactically, which is what that work had to add; *Compiling with continuations, or without? whatever* (ICFP 2019) then showed the two inter-translatable. There is nothing to buy.
- **Waiting on Binaryen measurements.** The browser path runs no Binaryen at all, so its benefit is unconditional, and 09 is not a cheap prerequisite.

## Tests

- M1: the 41 existing optimizer tests unchanged, and the emitted module byte-identical across the corpus — the substrate's whole claim is that it decides what the old code decided.
- M2: emitted Wasm byte-identical, asserting the lifted table reproduces the 130 sites exactly.
- M3: a `structural.rs` fixture on the `lcg` kernel asserting no `ref.cast` and no 64-bit widening in the loop; a `trees` fixture pinning that constructor payloads are *not* affected, which is what makes the locals-only scope observable rather than merely intended; the full codegen corpus behaviorally unchanged.
- Throughout: `verify`'s representation contract extended before the transformation that needs it, so a malformed rewrite fails loudly rather than miscompiling.

## Successor

Unboxing across boundaries, and the return-protocol family it shares a mechanism with. The design work is done and the open questions are named, so the successor specification begins from these rather than from a survey.

- Function-signature unboxing needs the three `func`/closure type families re-keyed. The closure supertype is shared by every closure of an arity, so a specialized return or parameter shape means keying by `(arity, shape)` — the "return-protocol split", and the reason multi-return is naturally restricted to known callees, since an indirect `call_ref` learns the shape only from the type.
- Constructed-product results with Wasm multivalue, and multi-return continuations after Shivers & Fisher's *Multi-return Function Call* (JFP 2006), which encodes a match-returning call without allocating its sum. `return_to` appears at 106 sites across 7 files and `return_cont` at 74 across 6.
- Four questions must be answered before that document claims a verified constraint: how `MachineTerminator::Return` lowers and how call sites consume results; whether `curios-wasm` models multi-value *function* types at all, independently of Binaryen accepting the feature; whether re-keying closure types disturbs the shapes `curios-runtime` and the `curios-web` bridge mirror structurally; and how the optimizer classifies effects, which bounds any reordering the substrate enables.

## Retirement criteria

- Before this specification is deleted: the substrate's contract — the lattice laws, the solver's direction and recursion handling, and the per-value keying with its stated limit — is recorded in `curios-cont`'s module documentation; the representation demand table is documented on the `CpsIntrinsicOp` roster it lives on; the locals-only scope and what lifts it are recorded in `curios-cont/README.md`; the roadmap subitems are checked unlinked summaries; the successor specification exists with its four questions answered or restated; and no reference to this filename remains.
