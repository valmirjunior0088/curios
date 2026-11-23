# curios-document

Everything documentation: the record of a unit's interface, made from the unit a compilation builds, and the pages `curios document` writes from that record. What a page shows and why it is read off the compilation rather than the source is [A library is documented for its consumers, from the compilation that builds it](../documentation/design/tooling/a-library-is-documented-for-its-consumers-from-the-compilation-that-builds-it.md); how the record is built belongs to `curios-module`, how it is obtained from a compilation to `curios-pipeline`'s fold and `curios-wonder`'s engine, where the prelude's are filed to `curios-prelude`, and the record's shape and the pages' design to the crate rustdoc.

## Design

### The record and its renderer share a crate, and the lowering depends on it

**Decision.** This crate owns the record's types and the renderer over them, and depends on `curios-common`, `curios-archive` and Askama alone. `curios-module` depends on it for the representation it constructs, `curios-prelude` and `curios-pipeline` for the records they hand on, `curios-wonder` for the record its query returns, and `curios` for the pages its subcommand writes.

**Rationale.** A stage depends on the crate holding the representation it constructs, which is how every stage is laid out — `curios-module` depends on `curios-core` for the same reason — so the record could not stay in `curios-module` once anything other than that crate rendered it without every renderer depending on the whole lowering. The renderer lives beside the record because it is the record's one consumer and brings its own weight, a template engine and the brand's fonts, which the compiler crate that links Binaryen and Cranelift had no reason to carry. Producing a record from a compilation stays above this crate, in the fold `curios-wonder`'s engine and `curios`'s build both ask, because it is a reading of a compilation and this crate compiles nothing.

**Rejected.** A renderer-only crate with the record left in `curios-module`: the one renderer would depend on the whole lowering for seven type definitions. Rendering in the browser bundle: the pages are static files read from `file://`, and a bundle has nowhere to write them.

### The pages hold no script

**Decision.** A bundle is HTML and CSS alone, and its index is its landing page and its rail: the rail lists every path that has a page, the branch the page is on opened, and what a module exposes is on its module's page. The tree sits behind a checkbox and a chevron, so at a narrow width it starts folded and opens with no script.

**Rationale.** Every path a bundle has a page for is in the rail, so a reader reaches any of them in a click or two, and a name a module exposes in one more. A search would be a second copy of the record for a lookup those already give: shipped as a script beside every page, since a page read from `file://` loads a sibling script and fetches nothing, with a second script to rank it. Without a script a page's rail opens at its top rather than at the page's own path.

**Rejected.** A search over an index written as a script: a second copy of every path, for a lookup the rail and the pages give. A page listing every name the unit exposes: a second rail, and the longest page of the bundle. A script to scroll the rail to the page's path alone: a script for one convenience.
