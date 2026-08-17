# Full-conformance Wasm memory, table, data, and element sections

This document specifies `curios-wasm` growing to represent, encode, print, and parse the complete memory/table/segment space of the pipeline's envelope — the eight features Binaryen's mask pins today, plus multi-memory and memory64, which M1 below adds to the mask. It covers both roadmap items linked to it: full data-section support and full element/table-section support.

## Status

The gap list below is a verified audit (2026-08-17) over `types.rs`, `expr.rs`, `module.rs`, `writer.rs`, `parse.rs`, and `print.rs`. Outside it the envelope is already modeled: the numeric core is complete (sign extension, saturating truncation, the full conversion family), multivalue is fully representable (`FuncType`'s result vectors, `BlockType::Concrete`), and the control, tail-call, and GC rosters cover everything but the segment-consuming instructions listed below.

Support for the two added features is verified where it must exist: the pinned wasmtime (46.0.1, the workspace's single row) enables `MULTI_MEMORY` and `MEMORY64` in its default feature set, so the engine, `validate`, and `precompile` need no change; the vendored Binaryen 130 header declares `BinaryenFeatureMultiMemory` and `BinaryenFeatureMemory64`, so the mask change is two extern rows and two OR terms in `curios-binaryen`. The executable check is the Binaryen pass-through tests below.

Nothing is started.

## Scope contract

The scope is envelope closure: every construct the envelope admits in the memory/table/segment space — memories and tables plural, each 32- or 64-bit addressed, every segment mode, and the complete instruction families over them.

This specification is representation-only. `curios-cont`'s emission does not change: program values stay GC references per [WebAssembly-GC is the only target](../../design/toolchain/webassembly-gc-is-the-only-target.md). Today that law is enforced twice — the design decision states it and the roster physically cannot express the alternative; after this lands only the decision enforces it, so the roster rustdoc must say plainly that the GC-only-for-program-values discipline is the emitter's, not the representation's.

Consumers are successors by name, never milestones here: closure index dispatch (envs holding an i32 table index, called through `call_indirect`) is an optimizations-kind specification of its own, measurement-gated in the variant-width mold, depending on M1 below. The one existing consumer of the memory lane, `curios-js/src/bridge.rs`, migrates inside M2.

## Gap list, verified

- **Memory**: modeled nowhere. The writer unconditionally emits one limitless empty memory (`writer.rs`'s `write_memory_section`), `Export::Memory` special-cases it, and no memory name, limits, address type, or import is representable.
- **Tables**: absent entirely — no table section, no name, no import/export.
- **Element segments**: one declarative func-index segment (`Module::elems` is `Vec<FuncName>`, flags `0x03` hardcoded). Missing: active and passive modes, expression-style element lists, per-segment element types, and a segment name for `table.init`/`elem.drop` to reference.
- **Data segments**: passive-only; no active mode.
- **Memory instructions**: 4 of the family exist (`i32.load8_u`, `i32.store8`, `memory.size`, `memory.grow`) with memargs hardcoded to align 0, offset 0 and no memory operand. Missing: the other 21 load/store forms, real memargs, `memory.copy`, `memory.fill`, `memory.init`, `data.drop`.
- **Table instructions**: all missing — `table.get`, `table.set`, `table.size`, `table.grow`, `table.fill`, `table.copy`, `table.init`, `elem.drop`, `call_indirect`, `return_call_indirect`.
- **Segment-consuming GC instructions**: `array.new_elem`, `array.init_data`, and `array.init_elem` are missing; only `array.new_data` exists.
- **Name section**: subsections exist for the module, functions, locals, types, and fields; none for globals, tables, memories, element segments, or data segments.
- **Encoder mechanics**: the 0xFC opcode prefix (first use — only GC's 0xFB exists today), section ids 4 and 5 as real sections, the elem-segment flag encodings, memarg encoding with the multi-memory index bit, the limits flag byte (has-max, 64-bit address), and three new maps in the writer's index `Table`.

## Design

**Names.** `MemName`, `TableName`, and `ElemName` join the `name!` roster in `names.rs`. Index spaces follow the existing discipline — insertion order, imports leading, panic-on-missing resolution in the writer's `Table`.

**Types** (`types.rs`). An `AddressType { I32, I64 }`, a `Limits { min: u64, max: Option<u64> }`, a `MemType { address_type: AddressType, limits: Limits }`, and a `TableType { address_type: AddressType, ref_type: RefType, limits: Limits }` — the memory64 proposal covers 64-bit tables, so the address type sits on both. Limits carry the format's raw units: pages for a memory, elements for a table.

**Module** (`module.rs`). Memories and tables are plural declared items: `add_memory(MemName, MemType)` and `add_table(TableName, Table)`, where `Table` holds its `TableType` and an optional init `Expr` (the function-references extension for non-defaultable element types), and the writer emits no memory section for a module that declares no memory. `Import` gains `Memory` and `Table` arms; `Export::Memory` takes its `MemName` and `Export` gains a `Table` arm. Element segments are named items with a mode and an element list: mode is `Active { table_name, offset: Expr }`, `Passive`, or `Declarative`; the list is either `Funcs(Vec<FuncName>)` (funcref element type implied) or `Exprs(RefType, Vec<Expr>)` (constant expressions, required for typed-ref tables). `DataSegment` gains a mode: `Passive` or `Active { mem_name, offset: Expr }`. `declare_func` survives as a convenience reimplemented over the general model, appending to one canonical declarative segment.

**Instructions** (`expr.rs`). A `MemArg { mem_name: MemName, align: u32, offset: u64 }` beside `Instr` — `align` carrying the binary format's log2 exponent, `offset` wide enough for a 64-bit memory, the memory carried symbolically like every other cross-reference. The four byte-lane instructions fold into the general memarg-carrying family — their rows leave the operand-less `mnemonics!` table for the hand-spelled printer and parser arms, like every operand-carrying form. The roster gains the complete load/store family (23 instructions, all four value types with their sized variants), `memory.size`/`memory.grow`/`memory.fill { mem_name }`, `memory.copy` with two memory names, `memory.init { mem_name, data_name }`, `data.drop { data_name }`, the table family (`table.get`/`set`/`size`/`grow`/`fill { table_name }`, `table.copy` with two table names, `table.init { table_name, elem_name }`, `elem.drop { elem_name }`), `call_indirect { table_name, type_name }` and `return_call_indirect { table_name, type_name }`, and `array.new_elem`/`array.init_elem` (`type_name`, `elem_name`) beside `array.init_data` (`type_name`, `data_name`). Address operands follow the referenced memory's or table's address type; the model carries no duplicate record of that fact, and wasmtime validates agreement.

**Constant expressions.** Segment offsets and table initializers reuse `Expr` like global initializers do, and all three stay within the base constant-expression grammar — a single `t.const`, `global.get`, `ref.null`, `ref.func`, or GC constructor — because extended constant expressions sit outside the envelope: wasmtime default-enables them, the Binaryen mask deliberately omits them, and the model's `Expr` cannot tell the two grammars apart. The restriction is a stated contract on module builders, and the Binaryen pass-through tests are its detector.

**Name section** (`writer.rs`). Every new index space joins the name custom section beside the existing subsections: table names (subsection 5), memory names (6), global names (7), element-segment names (8), and data-segment names (9) — global names included to close the one pre-existing hole. This is what keeps [spec 02](02_wasm_optm_stage_spec.md)'s name-recovery law meaningful over the new spaces once its reader exists.

**Encoder** (`writer.rs`). Memory and table sections in insertion order with the limits flag byte (has-max, 64-bit address) and the function-references explicit-init table form where an init expression is present; memarg encoding setting the alignment field's memory-index bit (0x40) with a trailing memory index whenever the named memory is not index 0; element section covering every flag encoding the model can reach, choosing the smallest correct encoding per segment, with the flag table stated in rustdoc; data section flags 0, 1, and 2 (active at memory 0, passive, active with explicit memory index); the 0xFC prefix for the bulk-memory and table families.

**Text form** (`print.rs`, `parse.rs`). The internal WAT dialect gains forms for every construct above, round-tripped like the rest. Memargs print only their non-default parts (memory omitted at the module's first memory, offset omitted at 0, align omitted at natural) and parse the same way.

**Feature mask** (`curios-binaryen`). `BinaryenFeatureMultiMemory` and `BinaryenFeatureMemory64` join the mask in `lib.rs` — two extern rows in `sys.rs`, two OR terms — keeping the mask's law intact: exactly what the emitter may produce and Wasmtime's engine enables, still deliberately not `BinaryenFeatureAll`. Spec 02's envelope statement flips to its landed form in the same change.

**Migrations.** `curios-js/src/bridge.rs` declares and names the memory it exports and passes explicit memargs at its four instruction uses. `curios-cont` needs no source change: its data segments are passive and `declare_func` keeps its signature; its emitted program modules stop carrying the empty memory section Binaryen already stripped, so the optimized artifact is unchanged.

## Sequencing and milestones

- **M1 — tables and element sections.** `TableName`/`ElemName`, `AddressType`/`Limits`/`TableType`, the table section with 64-bit tables, table import/export, every element-segment mode and list form, the table instruction family, `call_indirect`/`return_call_indirect`, `array.new_elem`/`array.init_elem`, `declare_func` over the general model, the table, element, and global name subsections, and the two-feature mask amendment, with text round-trips and validation probes. Front-loads the elem flag encoding — the riskiest part — and unblocks the closure-dispatch successor.
- **M2 — memory and data sections.** `MemName` and plural declared memories with address types and limits, memory import/export, memarg generalization and the full load/store family, the bulk-memory instructions and `data.drop`, the active data mode with its explicit-memory-index encoding, `array.init_data`, the memory and data name subsections, and the bridge migration, with text round-trips and validation probes.

M1 lands before M2; each is independently gated by the ordinary handoff sequence. This crate sits below Ersd, so a workspace check exercises none of it — the tests below are the detector.

## Non-goals

- Emitter changes: no `curios-cont` behavior changes, and no program value moves out of GC references.
- Threads, SIMD (fixed and relaxed), exceptions, extended constant expressions, custom page sizes, stack switching. The envelope's authority is `curios-binaryen`'s mask — what the emitter may produce — not wasmtime's default feature set, which is wider: it default-enables SIMD, relaxed SIMD, and extended constant expressions, so `validate` would accept forms `optimize` then aborts on, and the mask is what keeps the two ends agreeing.
- Spec-WAT compatibility or spec-testsuite parsing: the text form remains the internal dialect, per spec 02's non-goals.
- A binary reader (spec 02's M2 owns it) or a validator (wasmtime validates).

## Rejected

- **Keeping the implicit always-emitted memory.** Active data segments would target an item the model cannot see, limits and address types would stay unrepresentable, and the encoder would keep holding policy a symbolic model should state.
- **Keeping the four byte-lane instructions beside the general family.** A parallel abstraction for one alignment of one instruction family; the two consumers migrate instead.
- **Folding the closure-dispatch campaign in.** A measurement-gated optimization does not belong in a capabilities specification; it proceeds as its own specification depending on M1.

## Tests

- Text round-trips per construct in `curios-wasm`, beside the existing ones; one per reachable elem flag encoding and one per data flag, named for their flags.
- Validation probes in a new `curios/src/tests/wasm_conformance.rs`: per construct family, a builder-constructed module accepted by `curios_runtime::validate` — including a two-memory module and a 64-bit memory and table — the binary-side correctness check until spec 02's reader adds `from_bytes ∘ to_bytes`. They live in `curios` because `curios-wasm` must not name wasmtime (the single-row invariant).
- Binaryen pass-through: the optimize round-trip gains modules exercising the new surface — an active data segment, the bulk-memory instructions, a table called through `call_indirect`, a second memory, a 64-bit memory — confirming the amended mask admits everything this specification adds.
- The bridge migration is covered by the existing `make curios/js` gate.

## Retirement criteria

Before this specification is deleted: the roster, section, and encoding contracts are recorded in `curios-wasm`'s rustdoc with the flag tables beside the encoder; the relocated GC-only-for-program-values statement is on the roster doc; both roadmap items become checked plain-text summaries; spec 02's envelope statement, constraint bullets, and milestones naming this file are updated to state what landed; and no reference to this filename remains.
