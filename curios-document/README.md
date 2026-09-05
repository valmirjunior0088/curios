# curios-document

Everything documentation: the record of a unit's interface that the text lowering builds and the unit carries, and the pages `curios document` writes from that record. What a page shows and why it is read off the compilation rather than the source is [A library is documented for its consumers, from the compilation that builds it](../documentation/design/toolchain/a-library-is-documented-for-its-consumers-from-the-compilation-that-builds-it.md); how the record is built belongs to `curios-text`, how it is obtained from a compilation or an archived unit to `curios-wonder`, and the record's shape and the pages' design to the crate rustdoc.

## Design

### The record and its renderer share a crate, and the lowering depends on it

**Decision.** This crate owns the record's types and the renderer over them, and depends on `curios-utilities`, `curios-archive` and Askama alone. `curios-text` depends on it for the representation its lowering constructs, `curios-wonder` for the record its query returns, and `curios` for the pages its subcommand writes.

**Rationale.** A lowering depends on the crate holding the representation it constructs, which is how every stage is laid out — `curios-text` depends on `curios-core` for the same reason — so the record could not stay in `curios-text` once anything other than that crate rendered it without every renderer depending on the whole lowering. The renderer lives beside the record because it is the record's one consumer today and brings its own weight, a template engine and the brand's fonts, which the compiler crate that links Binaryen and Cranelift had no reason to carry. The engine that produces a record from a compilation stays in `curios-wonder`, because producing one is a query over a compilation and this crate compiles nothing.

**Rejected.** A renderer-only crate with the record left in `curios-text`: the one renderer would depend on the whole lowering for eight type definitions. Rendering in the browser bundle: the pages are static files read from `file://`, and a bundle has nowhere to write them.
