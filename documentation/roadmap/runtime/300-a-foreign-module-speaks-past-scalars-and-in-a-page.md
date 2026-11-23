# A foreign module speaks past scalars, and in a page

**Not refined yet.** This specification reserves two independent deliveries for a WebAssembly module a package names to answer `foreign` declarations: marshalling the shapes past scalars and byte strings, and linking such a module where a row's `browser` table names it. It is not an implementation plan: each waits for its own consumer, a plugin that needs a shape past those and a program that needs a module in a page, and is refined for that consumer alone; neither waits on the other.

## What is known

- **The native link is the model.** `curios-native/src/plugin.rs` fills a module into `ForeignBindings` and copies between the guest's GC arrays and the module's linear memory host-side, through the module's own allocator: `alloc`, a byte-string result coming back as the `(ptr, len)` pair of a multi-value return, or the canonical ABI's `cabi_realloc`, a result that is no single scalar coming back as one pointer to an area laid out field by field and released through the export's post-return. It refuses a module that imports anything, naming every import, and instantiates one per row. `curios-abi` is the wire contract's source of truth, and the browser's port is JavaScript beside the harness's `checked` adapter, the two held to agree (`.claude/rules/host.md`).
- **The first hop is one rule already.** A value leaves a program at the wire type it crosses at, a row's operand and a test program's answer alike (`curios_abi::ENTRY`), lowered by one function (`wire_forced`, `curios-emit/src/context.rs`) and lifted by each host as an operand is; that hop carries lists, options and handles today. The ceiling below is the second hop, from the host into a module's linear memory, and a shape this specification adds there extends it without touching the first.
- **A row has one delivery a target** ([A foreign row states a delivery for each target](../../design/tooling/a-foreign-row-states-a-delivery-for-each-target.md)). A module that answers on both targets is named in both tables, so nothing chooses between a module and a gateway for one declaration.

## Shapes past scalars and byte strings

**The ceiling.** A `Handle`, a `List` and an `Option` are each refused where a signature is read (`crossing`, `curios-native/src/plugin.rs`), and a module with an allocator of its own answers one result or none: marshalling is the host copying between the guest's GC array and the plugin's linear memory, and only scalars and byte strings have a copy. The ceiling holds on either target.

**Refinement**, for the shape its consumer needs: its layout on both sides, the copy the host performs, the ABI row it adds, and the native and JavaScript implementations that must agree with it.

## A module in a page

**What is missing.** A browser delivery is a JavaScript gateway, so a module a package answers natively through a row's `native` table reaches a page only through a gateway the author writes, which instantiates the module and copies every byte string into and out of its memory by hand, against nothing that holds the copy to the row.

**What is not known.** No browser ships WebAssembly type reflection without a flag, so what `Plugin::of` checks of each export natively is checked where the page is assembled; how the compiler reads a module's export types there without compiling it with Cranelift is the first question its refinement answers.

## Completion and retirement

Each delivery records its contract in `curios-abi`, `curios-native`'s README and the harness as it lands and leaves only the other here; once neither remains and nothing cites this file, it is deleted.
