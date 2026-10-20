# A foreign row states a delivery for each target

**Decision.** A `[[foreign]]` row states what each target links in a table of its own: `[foreign.native]` names the WebAssembly module the native target compiles and links, and `[foreign.browser]` the JavaScript gateway a browser imports, with `[foreign.browser.static]` the files its page serves beside it. Each table states its file as `path` or `url` beside the `hash` it is accepted against, and the row's one `[foreign.answers]` names the export of either that answers each declaration, since the declarations are the same whatever answers them. A row states either table or both, and a row with neither is refused where the manifest is read. A program compiled for a target reaching a declaration whose row lacks that target's table is refused where it is linked, naming the row and the table, before anything runs or is written. `curios pin foreign` writes one table, the one `--target` names, and `curate` fetches every table's file, since nothing it reads says which target a build will ask for. The target is spelled once, by `curios-package`'s `Target`, which `--target`, the tables and the store's entries all read.

**Rationale.**

- **Who answers a row is decided where it is linked** ([A host operation is a foreign declaration whose types are its whole contract](../runtime/a-host-operation-is-a-foreign-declaration-whose-types-are-its-whole-contract.md)), and a target is where it is linked: a native run links modules ahead of its host, and a page links gateways ahead of the browser's. Each target's delivery is a fact about that link, so it is that target's table.
- **A refusal is the author's to opt into.** A row stating only `[foreign.browser]` says the native target links nothing for it, and a program reaching it natively is refused by that statement, naming the table that would answer it, rather than by a guess about what the row meant.
- **One answers table, because the declarations are one.** The wire types are a row's whole contract, so an export of either kind answers the same declaration in the same types, and the harness's checked adapter holds a gateway's export to its row as the native link holds a module's.
- **Each delivery is pinned by itself.** A gateway is one file as a module is, so it is accepted against its own `f1:` hash, whichever way it arrived.

**Rejected.**

- **The delivery's keys directly under `[[foreign]]`**, with a `target` beside them or a second row per target: two rows for one provider each stating the same answers, or one table whose meaning depends on a field beside it.
- **A target inferred from the file's extension**, which makes a `.js` delivery the native target's refusal by a rule nobody wrote down.
- **A browser linking a row's native module where it states no gateway**, a fallback the author never chose, which would make a native table a browser's too.
- **A top-level `[browser]` table** for what a page serves: a page serves files for the rows that put them there, so they are a row's, and a manifest's top level is real estate for the package as a whole.
