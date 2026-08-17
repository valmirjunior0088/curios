# curios-wasm

The Curios WebAssembly-GC target: the symbolic module model, the WAT parser, and the binary encoder — the pipeline's final stage. `curios-cont` lowers continuation IR into a `Module`, and `to_bytes` produces the binary that wasmtime (`curios-runtime`), the browser (`curios-js`), and `wasm-opt` (`curios-binaryen`) consume. It models the whole feature envelope the pipeline pins, not the subset its current consumers reach. How identities are spelled is the cross-cutting decision in [One naming scheme for compiler identities](../documentation/design/toolchain/one-naming-scheme-for-compiler-identities.md); that program values live in GC references rather than linear memory is [WebAssembly-GC is the only target](../documentation/design/toolchain/webassembly-gc-is-the-only-target.md), which this crate no longer enforces and never stated. The type grammar, the instruction set, and the encoder's section order and flag tables belong to the crate rustdoc.

## Design

### Everything is symbolic, and the index spaces exist only inside the encoder

**Decision.** Items and their cross-references use the `name!` newtypes in `names` — `TypeName`, `FuncName`, and the rest. The numeric index spaces the binary format is defined over are derived from declaration order at encoding time and exist nowhere else; nothing above the encoder can hold one.

**Rationale.** An index is a fact about a finished module, and the module is not finished while it is being built. Constructing one with indices means every insertion can invalidate a reference that is already written down, so the builder would owe a renumbering pass and every caller would owe it correctness — a class of bug that produces a *valid* module computing the wrong thing, which no validator catches and no test names. With names, an unresolved reference is a lookup failure at encode time, in one place, before any bytes exist.

It also keeps the emitter honest about ordering: because the encoder assigns indices from declaration order, `curios-cont` may emit items in whatever order its lowering finds natural, and the two concerns never negotiate.

**Rejected.** Carrying indices in the model and renumbering on mutation. It moves a whole-module invariant into every mutation site, and its failure mode is silent.

### Nothing is emitted on a module's behalf

**Decision.** The encoder emits exactly the items the module declares. There is no memory a module gets for free, no element segment minted for it, no default table: a module that declares no memory has no memory section, and a function that needs `ref.func` eligibility gets it from a declarative element segment its own builder added.

**Rationale.** Two such fabrications lived here, and they were one mistake twice — a policy belonging to a *consumer* written into the model, where the model could not state it and the consumer could not see it. The always-emitted empty memory existed for `curios-js`'s byte-copy lane, so every compiled program carried a memory section it never touched (and Binaryen quietly stripped), while an active data segment would have targeted an item nothing in the model named and no builder could resize. The minted declarative element segment existed because `curios-cont`'s closures are reached by `ref.func`, which validates only for a declared function — a fact about that lowering, not about wasm modules, and one no other consumer shares. With both gone, the bridge declares its memory and the lowering declares its closures, each stating why at the place that knows.

**Rejected.** Keeping either as a convenience over the general model. A convenience that fabricates an item makes the encoder hold a whole-module policy the builder cannot inspect, and its failure mode is a module that validates while meaning something other than what was built.

### A memarg names its memory; only the encoder knows one of them is index 0

**Decision.** `MemArg` carries a `MemName`, and the text form always spells it — `i32.load $m offset=4`. The binary encoding omits the memory index, and the alignment field's bit that announces it, whenever the resolved index is 0. Offset and alignment are the parts the text form omits at their defaults: zero, and the log2 alignment the access width makes natural.

**Rationale.** The binary format's implicit index exists because multi-memory had to extend an immediate with no room left in it, and leaving it out keeps a single-memory module byte-identical to a pre-proposal one. That is a fact about the encoding, so the encoder is the only place that holds it. The text form cannot borrow the same default: module items are read in written order, and a memarg appears inside a function body that may precede every memory declaration, so "the module's first memory" is not yet a thing the parser knows. Spelling it always also keeps one spelling per model value, which an omission would not.

**Rejected.** An `Option<MemName>` meaning "the first memory" — two ways to say one thing, which the encoder would then erase, so two models that print differently would encode identically. Also rejected: resolving an omitted memory in a pass after the item list is folded, which makes parsing one instruction depend on the whole module.
