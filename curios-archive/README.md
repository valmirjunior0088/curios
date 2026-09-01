# curios-archive

Zero-copy archiving for the workspace: the one crate that names rkyv, the `archived` attribute every stored type carries, the `Proxy`/`Via` adapter for a type rkyv cannot archive directly, and the four entry points — `to_bytes`, `from_bytes`, `access`, `deserialize` — with the error type fixed. How to annotate a type, what the keywords and field markers do, and what each entry point returns belong to the crate rustdoc; the macro's own decisions are `curios-archive-derive/README.md`'s.

## Design

### One crate is the authority for one external concern

**Decision.** rkyv is named in this manifest and reached through this crate's re-export by `curios-archive-derive`'s expansion, and nowhere else in the workspace: not a type, not a trait, not a function, not the helper attribute. The arrangement is [One crate is the authority for one external concern](../documentation/design/toolchain/one-crate-is-the-authority-for-one-external-concern.md), which `curios-num` and `curios-profile` follow for their dependencies. Checkable: grep the workspace for `rkyv`, and every hit is prose or a file name.

**Rationale.** The design entry's: a dependency that exists in exactly one manifest cannot be added elsewhere without someone writing the version down again, which is a question a reviewer will ask. A `[workspace.dependencies]` row shares configuration and concentrates no authority at all.

### A type rkyv cannot archive is described by a stand-in, once

**Decision.** `Proxy<Value>` states one conversion — this value converts to an archivable one, and back — and `Via<P>` supplies the three rkyv adapter impls (`ArchiveWith`, `SerializeWith`, `DeserializeWith`) from it. A namespace is a `u8`, a bignum its little-endian bytes, a hash map its sorted entries, a shared name the vector behind the `Rc`.

**Rationale.** Every hand-written field adapter in the workspace was that same three-impl shape around one idea. Written once here, a crate declaring a proxy names no rkyv trait, so the dependency stays inside this crate where the pin is. The stand-in is handed over borrowed rather than by value because the fixed prelude holds on the order of a hundred thousand qualifier occurrences over a couple of thousand distinct paths, and a by-value signature would clone the shared allocation once per occurrence on the way out.

### The entry points fix the error type

**Decision.** `to_bytes`, `from_bytes`, `access` and `deserialize` fix rkyv's error type to `rancor::Error`, hand back a `String`, and take rkyv's serializer, validator and deserializer bounds on themselves. `to_bytes` returns rkyv's aligned buffer behind a newtype rather than a `Vec<u8>`.

**Rationale.** Every call site instantiated all of that generality identically, so it bought nothing and cost each caller a `curios_archive::rkyv::` path in return. The aligned buffer stays because rkyv writes its archive expecting alignment for the largest type it contains, and converting to a `Vec` would copy every byte of an image that can run to megabytes.
