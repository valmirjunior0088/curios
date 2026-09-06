# curios-document

Everything documentation: the record of a unit's interface that the text lowering builds and the unit carries, and the pages `curios document` writes from that record. What a page shows and why it is read off the compilation rather than the source is [A library is documented for its consumers, from the compilation that builds it](../documentation/design/toolchain/a-library-is-documented-for-its-consumers-from-the-compilation-that-builds-it.md); how the record is built belongs to `curios-text`, how it is obtained from a compilation or an archived unit to `curios-wonder`, and the record's shape and the pages' design to the crate rustdoc.

## Design

### The record and its renderer share a crate, and the lowering depends on it

**Decision.** This crate owns the record's types and the renderer over them, and depends on `curios-utilities`, `curios-archive` and Askama alone. `curios-text` depends on it for the representation its lowering constructs, `curios-wonder` for the record its query returns, and `curios` for the pages its subcommand writes.

**Rationale.** A lowering depends on the crate holding the representation it constructs, which is how every stage is laid out — `curios-text` depends on `curios-core` for the same reason — so the record could not stay in `curios-text` once anything other than that crate rendered it without every renderer depending on the whole lowering. The renderer lives beside the record because it is the record's one consumer today and brings its own weight, a template engine and the brand's fonts, which the compiler crate that links Binaryen and Cranelift had no reason to carry. The engine that produces a record from a compilation stays in `curios-wonder`, because producing one is a query over a compilation and this crate compiles nothing.

**Rejected.** A renderer-only crate with the record left in `curios-text`: the one renderer would depend on the whole lowering for eight type definitions. Rendering in the browser bundle: the pages are static files read from `file://`, and a bundle has nowhere to write them.

### The search index is a script every page loads, and a page is complete without it

**Decision.** The pages' search runs in the browser over `static/index.js`, one file per bundle written from the record — a row per module, named declaration and member, each its kind, its path and its address from the bundle's root — which every page loads with a deferred script tag ahead of the page script. The field, the results and the clear button are in the template but hidden, and the page script shows them; the tree, the checkbox and the chevron it toggles work without any script.

**Rationale.** The pages are read from `file://`, where a browser loads a script tag from a sibling file but refuses to fetch one, so a corpus that is fetched cannot be searched from disk and a corpus that is a script can; rustdoc and mdBook ship their indices the same way for the same reason. One file rather than a copy in every page, because the standard library's corpus is a thousand rows and a hundred pages, and a copy per page would outweigh the pages themselves. The rail lists another module's declarations nowhere, so a search over what is on the page would find a module and never a declaration of another one, which is what a search is for. Hidden markup shown by script rather than markup built by script keeps every element in the template, where the crate keeps its markup.

**Rejected.** A JSON index fetched on demand: not from `file://`. The corpus inlined in each page: three megabytes on a two-megabyte bundle. Filtering the tree in place: it holds only the current module's declarations. A search over the rail's rows alone: finds modules and nothing else.
