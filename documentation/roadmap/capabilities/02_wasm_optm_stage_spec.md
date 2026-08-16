# `Stage::WasmOptm` — observing the Binaryen-optimized module

This document specifies the pipeline's one dark stage becoming observable: `--print wasm-optm` renders the module Binaryen hands back, in the house rendering, diffable against `--print wasm`. The enabling artifact is a WebAssembly *binary reader* in `curios-wasm` — the binary writer's inverse, which does not exist today.

## Problem

Binaryen is the last transformation the emitted module undergoes (`curios-binaryen::optimize`, bytes in, bytes out), and nothing ever parses its output: the round-trip test checks the `\0asm` magic and that the module shrank, and `--print` stops at `wasm`. What closed-world GC optimization actually did — the inlining, layout, and dead-code decisions behind measured size and speed differences — is invisible, and past performance work had to reason about it blind.

## Constraints, verified

- `optimize` consumes and produces *binary* bytes. `curios-wasm`'s existing parser is a **text-format** parser (built on the shared text combinators), paired with `print.rs`; the binary side has only the encoder, `writer.rs`/`to_bytes`. Parsing Binaryen's output therefore means writing the encoder's inverse, not extending the wat parser — Binaryen's own text output is a folded s-expression dialect, a second grammar with no other consumer.
- The conformance target is bounded and explicit: `optimize` pins the feature set to exactly what the emitter produces and Wasmtime's engine enables — mutable globals, nontrapping float-to-int, bulk memory, sign extension, tail calls, reference types, multivalue, and GC — deliberately not `BinaryenFeatureAll`. The reader must cover *this envelope*, never all of WebAssembly: no threads, SIMD, exceptions, or memory64.
- `curios-pipeline` must not depend on Binaryen (the pure-pipeline invariant). The `Stage` enum is plain data, so the pipeline may *define* the variant while only the `curios` crate ever *constructs* it.
- The instruction roster already models the envelope's breadth in most places — `Select`, `BrTable`, the tail calls, the sign-extension and saturating-truncation families — and the writer already emits the full name custom section (function, local, type, and field names), which is what lets Binaryen preserve names so a parsed-back module displays readably rather than as bare indices.
- The known representation gaps are exactly [01_wasm_full_conformance_spec.md](01_wasm_full_conformance_spec.md)'s two umbrella items — full data-section support (active segments, `memory.init`/`data.drop`, the linear-memory load/store family) and full element/table support — forms bulk-memory-enabled optimization can legitimately rewrite into. The reader cannot land before the representation can hold them.

## Design

**The reader.** `curios-wasm/src/reader.rs`, entry point `from_bytes` — completing the crate's symmetry: `print.rs`/`parse.rs` are the text-format pair, `writer.rs`/`reader.rs` the binary-format pair, `to_bytes`/`from_bytes` the mirrored entries. An LEB/section walker in its own module (the text combinators do not apply to bytes), following the house layout (`reader.rs` declaring `reader/` submodules as it grows, tests in `reader/tests.rs`, flat crate-root re-export). It consumes the name section for display names, falling back to index-spelled names under the shared `~`-scheme for anything Binaryen minted or stripped.

Two laws anchor it:

- `from_bytes(to_bytes(m))` is structurally `m` — the binary counterpart of the text round trip.
- `from_bytes(optimize(to_bytes(m)))` succeeds for every module the emitter produces — the law the stage exists for, exercised over the codegen corpus.

The reader trusts shape rather than re-validating: its input is Binaryen's already-validated output on the compile path, so a malformed or out-of-envelope construct is a compiler invariant violation and asserts loudly, never a `Result` to plumb.

**The stage.** `Stage::WasmOptm` joins the enum in `curios-pipeline` and `wasm-optm` joins `Stage::NAMES`, but `compile_entrypoint` never emits it: the variant is constructed downstream, by the `curios` crate's `to_cwasm` path, after `optimize` — a deliberate, documented deviation from every other variant, which the enum's rustdoc states plainly (the enum is the vocabulary of observation points, not a promise that the pure pipeline emits each). The CLI parses the optimized bytes back and feeds the same stage printer only when `--print` requests `wasm-optm`; rendering goes through `display_within(100)` exactly as `Stage::Wasm` does. Native path only — the browser product neither optimizes nor needs the reader.

## Sequencing and milestones

- **M0 — envelope audit.** Walk the eight-feature envelope against the representation, writer, text parser, and printer; refine [01_wasm_full_conformance_spec.md](01_wasm_full_conformance_spec.md)'s umbrella into the concrete gap list (the two known section items, plus likely multivalue block types, plus whatever else falls out). Cheap, immediate, and the input that specification needs to stop being a placeholder.
- **M1 — representation fullness.** Implement the refined [01_wasm_full_conformance_spec.md](01_wasm_full_conformance_spec.md): sections, instructions, writer, text parser, printer, and round-trip tests for every gap M0 names.
- **M2 — the reader.** `reader.rs`/`from_bytes` over the complete representation, with both laws pinned and the name-section recovery tested.
- **M3 — the stage.** The enum variant, the `NAMES` entry, the `to_cwasm`-path construction, and the CLI wiring.

M0 can land today; M2 is the bulk; M3 is small and strictly last.

## Non-goals

- Parsing arbitrary third-party modules: the reader's contract is Binaryen's output for our envelope, stated rather than silently assumed.
- Validation: Binaryen validated; the reader asserts on surprises.
- The browser path: `curios-js` never reads binaries.
- Parsing Binaryen's text output, at any point.

## Rejected

- **A `wasmparser` dependency.** It already exists in the tree on the native side (via Wasmtime), but taking it into `curios-wasm` puts a foreign reader inside the representation crate the browser path builds, and splits ownership of the binary format between our writer and someone else's reader. The format knowledge already lives in `writer.rs`; the reader is its mirror.
- **Binaryen text output plus wat parsing.** A second grammar (folded dialect), name instability, and a wider surface than the binary format, which is the stable, versioned artifact.
- **Running Binaryen inside `curios-pipeline`.** The purity invariant is load-bearing for the browser build and the crate boundaries; the downstream-constructed variant costs nothing and preserves it.

## Tests

- Per-construct binary round trips (`from_bytes ∘ to_bytes`) as each M1 gap lands, beside the existing text round trips.
- The optimize round trip over the codegen corpus: every module the test suite emits, optimized then parsed, with the existing behavioral assertions untouched.
- Name recovery: a module with named functions, locals, types, and fields keeps those names through `optimize` and `from_bytes`; a Binaryen-minted local displays under the `~`-scheme fallback.
- CLI: `--print wasm-optm` renders; `--print wasm,wasm-optm` renders both; an unrequested `wasm-optm` parses nothing.

## Retirement criteria

- Before this specification is deleted: the reader's contract, envelope, and laws are recorded in `curios-wasm`'s documentation and tests; the downstream-constructed-stage deviation is recorded on the `Stage` enum; [01_wasm_full_conformance_spec.md](01_wasm_full_conformance_spec.md) is either implemented and retired or re-scoped by M0's findings; the roadmap subitem is a checked unlinked summary; and no reference to this filename remains.
