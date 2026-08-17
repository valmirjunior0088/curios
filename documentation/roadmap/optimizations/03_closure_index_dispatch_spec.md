# A closure carries its code as a table index rather than a funcref

## Status

This specification defines the cost contract, evidence, design boundaries and milestones for changing what a closure environment's code field holds: an `i32` index into a module-level funcref table, dispatched with `call_indirect`, replacing the `(ref null clsr/N)` funcref field read back and dispatched with `call_ref` today.

The Wasm-model substrate it needs — a table section, active element segments, and the `call_indirect`/`return_call_indirect` instructions, none of which `curios-wasm` models today — is in flight independently of this specification. The emitter change is not started.

## Cost contract

Constructing a closure stops paying the engine's funcref machinery: no `ref.func` materialization and no funcref-to-GC-heap conversion at the `struct.new`, because the field written is an ordinary `i32`. Dispatching through a closure stops paying the funcref field read-back and `call_ref`, paying an `i32` field read and `call_indirect`'s table access and type check instead.

Which programs trap does not change. A recursive shell dispatched before its back-patch traps today on the null funcref; under this contract its zero-initialized index field must reach an equally loud trap, which is a design boundary below, not an accident.

Not promised: fewer allocations from the swap itself (the environment struct and its captures are untouched; the one downstream exception is M3's constant-closure interning, an annex the swap enables and which carries its own gate), any devirtualization (a known callee is the specializers' and `rewrite_atoms`' subject, and the known-function-argument roadmap item's — this specification only re-prices the calls that stay unknown), and any figure on the browser engine (V8 is unmeasured; the contract there is correctness only).

## Evidence

A symbolicated profile of `programs/rng_state.crs` (2026-08-10, samply + wasmtime PerfMap, N=10,000,000, release, method in the profiling section of CLAUDE.md) attributed ~75% of the run to `ref.func` interning — `intern_func_ref_for_gc_heap` and its hashing and libcall entourage — beside 10.1% emitted code and 2.9% GC. Interning, not allocation and not collection, is what a monadic loop pays for.

The mechanism was priced in isolation twice, agreeing: first 2026-08-11 (113.88 vs 7.34 ns/iter, 15.5×), then reproduced 2026-08-17 with a six-arm decomposition — hand-written WAT pairs, wasmtime 46.0.1 (the `Cargo.lock` pin) with `curios-runtime`'s exact engine flags and `gc-copying`, x86-64 Linux, N=20,000,000, best of 5, every arm computing the identical checksum:

| arm | ns/iter |
| --- | ---: |
| funcref field, `ref.func` + `struct.new` + `call_ref` (what the emitter does today) | 116.38 |
| `i32` field + `call_indirect` (this specification) | 4.54 |
| funcref-field construction, direct call | 91.79 |
| struct built once, `call_ref` dispatch only | 14.38 |
| `ref.func` hoisted to a global, field still funcref | 58.62 |
| `i32`-field construction, direct call | 2.92 |

Three decisions fall out of the decomposition. The cost is construction (~89 of the 116 ns), split roughly evenly between `ref.func` materialization and the per-store intern. Dispatch alone still favors the index ~9× (14.38 vs ~1.6 ns), so closures built once and called hot benefit too. And the cheaper alternative — hoisting the loop-invariant `ref.func` into a global while the field stays a funcref — recovers only 2× of the 25.6×, because the intern is per store into the GC struct; the field type itself must change. That alternative is rejected with this measurement as its record, and its reinstate condition is an engine whose funcref-to-GC-heap conversion stops being per-store.

These arms lived in session scratchpads and are ephemeral; the figures above are the record of what decided the representation, with the recipe stated so the pair can be rebuilt. M2 owes durable product-level figures beside probes, per the measurement discipline.

Two questions are settled — do not re-derive them. `call_indirect (type $clsr/N)` accepts a table entry whose type is `sub final $clsr/N`, validated and executed, so the per-closure final subtypes stay exactly as they are. And this does not let `wasm_function_references` be dropped: the GC proposal is layered on it, and the flag also gates the non-nullable reference types the emitter uses everywhere.

## Design boundaries

The change is emitter-only above the Wasm model. The CPS IR, the machine IR, and every optimization pass are untouched; everything lives in `curios-cont/src/into_wasm/` plus the `curios-wasm` substrate.

One module-level funcref table holds every closure body, filled by one active element segment, indices assigned in the module's ordered closure walk so emission stays reproducible — never from a `HashMap` iteration, per the symbol table's own rule. `ClsrData` carries the index.

Indices are 1-based and table slot 0 is left null. A funcref table's uninitialized entry is null and `call_indirect` on it traps, so a recursive shell's `struct.new_default`-zeroed index field reaches the same loud trap the null funcref reaches today, with no stub function and no extra check.

The sites that change are exactly the funcref sites: construction (`ref.func` + fields + `struct.new`, `expr_emitter.rs`) writes `i32.const <index>`; the shell back-patch writes the index; dispatch (`call_indirect_instrs` in `context.rs` — the name is already waiting) swaps `struct.get`/`ref.as_non_null`/`call_ref` for `struct.get`/`call_indirect`, and its tail position swaps `return_call_ref` for `return_call_indirect`; the environment types (`module_emitter.rs`) declare the special field `i32` in both the per-arity supertype and every per-closure subtype; const closures built in the start function take the same `i32.const` spelling.

With no `ref.func` left in emitted code, the declarative element segment and `declare_func` retire — `module_emitter.rs:534` records that the segment exists only to make closure `ref.func` eligible, and the active segment now carries that role.

Binaryen must pass the table, the element segment and `call_indirect` through its optimization unchanged in meaning; that it may also devirtualize a constant-index `call_indirect` over an immutable table is a possible bonus, not a promise.

Per-arity typed tables — one table per `clsr/N` with element type `(ref null clsr/N)`, letting an engine discharge the type check statically — are rejected pending measurement: one table is simpler, the check replaced a hashing libcall and is not the cost. Reinstate condition: a profile at corpus scale showing `call_indirect`'s type check as an attributable share.

## Measurement gate

The mechanism harness priced one closure, one arity, a one-entry table and a perfectly predicted target. Two scale questions remain open from the design record and are owed before or beside M1, as cheap harness extensions or as measurements over the emitted corpus: `call_indirect` cost with many distinct final subtypes in one funcref table, and instantiation cost of a table at hundreds of entries. Neither can plausibly reverse 25.6×, so they gate nothing; they are recorded so their absence is a known gap rather than an assumed answer.

Evidence that would stop the work: the product-level figures in M2 showing no movement on the monadic-loop programs the profile attributed to interning — which would mean the profile misattributed, and the misattribution would itself be the finding.

## Milestones

The spine is `M0 → M1 → M2`. M3 stands beside it as an annex M1 enables; the spine neither waits for it nor depends on its verdict.

### M0 — Wasm-model substrate

- `curios-wasm` models a table section, active element segments, and `Instr::CallIndirect`/`Instr::ReturnCallIndirect`, each landing in the encoder, parser and printer with round-trip tests.

- This milestone is shared infrastructure, in flight independently; this specification consumes it and adds nothing to it.

### M1 — The swap

- The environment special field becomes `i32` in the per-arity supertype and every per-closure subtype; construction, back-patch and const-closure emission write the 1-based index; dispatch and its tail position go through `call_indirect`/`return_call_indirect`; the table and its element segment are emitted in ordered-walk index order with slot 0 null; the declarative segment and `declare_func` retire.

- Structural probes: the emitted module carries no `ref.func`, the closure ABI reads an `i32` and dispatches `call_indirect`, and a shell dispatched before back-patch still traps — pinned in `curios/src/tests/codegen/structural.rs` beside the existing closure-ABI fixtures.

- The change sits below the Ersd archive line, so the cross-stage corpus is its detector; the full gate plus `make curios/js` (the browser build must still validate and run, whatever V8's figures are).

### M2 — Figures beside probes

- Product-level before/after on the programs the evidence names — `programs/rng_state.crs`, `state_monad`, the ladder's closure-bearing rungs — and the benchmark pair, recorded beside the probes that reproduce them rather than in prose, per the measurement discipline.

- The two open scale questions above answered with their instrument, wherever they landed.

### M3 — Constant closures intern (annex)

- The swap dissolves the constant hoister's one exclusion: `hoist.rs` leaves closure values in place because hoisting one moves its `ref.func` into the start function — "no measured shape needs it yet". With the code field an ordinary `i32`, a closure whose fields are all interned constants is a constant aggregate like any `Tpl`, and the exclusion's rationale retires alongside the declarative segment.

- The change is one `ConstKey` arm — the closure's target plus its canonicalized field names — riding the existing interner and const-emission machinery unchanged; a capture-free or const-captured closure then materializes once per instantiation instead of allocating per construction. A shell'd recursive closure is naturally outside the condition — it captures itself, and itself is no constant — so the backpatch path is untouched.

- Admission: frequency, not product figures — a census count of closure constructions whose fields are all constant, over the optimized corpus and `/std`, recorded beside its instrument per the measurement discipline. The rewrite is cheap, so any nontrivial population admits it, and an empty one retires the annex without touching the spine.

- Structural probe: a capture-free closure constructed in a loop pins as one module const, with no per-iteration construction left in function code — beside the M1 probes in `curios/src/tests/codegen/structural.rs`.
