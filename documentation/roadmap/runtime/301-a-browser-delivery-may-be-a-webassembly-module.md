# A browser delivery may be a WebAssembly module

**Not refined yet.** This specification reserves linking a WebAssembly module that a `[foreign.browser]` table names. It is not an implementation plan: it waits on [A program runs in a browser](../300-a-program-runs-in-a-browser.md), which makes a browser delivery a JavaScript gateway, and on a program that needs a module in a page.

## What is missing

A module a package answers natively through `[foreign.native]` reaches a page only through a gateway the author writes, which instantiates the module and copies every byte string into and out of its memory by hand, against nothing that holds the copy to the row.

## What is known

- **The native link is the model.** `curios-native/src/plugin.rs` copies between the guest's GC arrays and the module's linear memory host-side, through the module's own allocator: `alloc`, a byte-string result coming back as the `(ptr, len)` pair of a multi-value return, or the canonical ABI's `cabi_realloc`, a result that is no single scalar coming back as one pointer to an area laid out field by field and released through the export's post-return. It refuses a module that imports anything, naming every import, and instantiates one per row. The browser's port is JavaScript beside the harness's `checked` adapter, and the two must agree (`.claude/rules/host.md`).
- **A page cannot read an export's types.** No browser ships WebAssembly type reflection without a flag, so what `Plugin::of` checks of each export natively is checked where the page is assembled. How the compiler reads a module's export types there without compiling it with Cranelift is not yet known.
- **A row has one delivery a target.** A module that answers on both targets is named in both tables, so nothing chooses between a module and a gateway for one declaration.
- **The ceiling stands.** What a module can take and answer is [Foreign calls past scalars and byte strings](300-foreign-calls-past-scalars.md)'s, on either target.
