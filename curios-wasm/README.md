# curios-wasm

The Curios WebAssembly-GC target: the symbolic module model, the WAT parser and the binary encoder — the pipeline's final stage. `curios-emit` lowers continuation IR into a `Module`, and `to_bytes` produces the binary that Wasmtime (`curios-runtime`), the browser (`curios-js`) and Binaryen's optimizer (`curios-binaryen`) consume. It models the whole feature envelope the pipeline pins, not the subset its consumers reach: that program values live in GC references is [WebAssembly-GC is the only target](../documentation/design/compilation/webassembly-gc-is-the-only-target.md), which this crate's model does not enforce, and how identities are spelled is [A printer states each fact once, where it is bound](../documentation/design/tools/a-printer-states-each-fact-once-where-it-is-bound.md). The type grammar, the instruction set and the encoder's section order and flag tables belong to the crate rustdoc.

## Design

### Everything is symbolic, and the index spaces exist only inside the encoder

**Decision.** Items and their cross-references use the `name!` newtypes in `names` — `TypeName`, `FuncName` and the rest. The numeric index spaces of the binary format are derived from declaration order at encoding time and exist nowhere else.

**Rationale.** An index is a fact about a finished module. Built with indices, every insertion can invalidate a reference already written, so the builder owes a renumbering pass and every caller owes it correctness — a bug that yields a valid module computing the wrong thing. With names, an unresolved reference is a lookup failure at encode time, before any bytes exist, and `curios-emit` emits items in whatever order its lowering finds natural.

**Rejected.** Carrying indices in the model and renumbering on mutation, a whole-module invariant at every mutation site.

### Nothing is emitted on a module's behalf

**Decision.** The encoder emits exactly the items the module declares: no memory a module gets for free, no element segment minted for it, no default table.

**Rationale.** A policy belonging to a consumer, written into the model, is one the model cannot state and the consumer cannot see: an always-emitted empty memory makes every program carry a section it never touches, and a minted declarative element segment encodes one lowering's need for `ref.func` into every module. Each builder declares what it needs where it knows why.

**Rejected.** Either as a convenience over the general model, which makes the encoder hold a whole-module policy the builder cannot inspect.

### A memarg names its memory; only the encoder knows one of them is index 0

**Decision.** `MemArg` carries a `MemName`, and the text form always spells it — `i32.load $m offset=4`. The binary encoding omits the memory index, and the alignment bit announcing it, whenever the resolved index is 0. Offset and alignment are omitted in text at their defaults: zero, and the natural log2 alignment of the access width.

**Rationale.** The implicit index exists because multi-memory extended an immediate with no room left, keeping a single-memory module byte-identical to a pre-proposal one, so it is the encoder's fact. The text form cannot borrow the default: a memarg inside a function body may precede every memory declaration, so "the first memory" is not yet known to the parser. Spelling it always keeps one spelling per model value.

**Rejected.** An `Option<MemName>` meaning the first memory, two ways to say one thing; resolving an omitted memory after the item list is folded, which makes parsing one instruction depend on the whole module.
