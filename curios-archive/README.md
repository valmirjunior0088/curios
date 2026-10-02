# curios-archive

Zero-copy archiving for the workspace: the one crate that names rkyv ([One crate is the authority for one external concern](../documentation/design/one-crate-is-the-authority-for-one-external-concern.md)), the `archived` attribute every stored type carries, the `Proxy`/`Via` adapter for a type rkyv cannot archive directly, and the four entry points — `to_bytes`, `from_bytes`, `access`, `deserialize` — with the error type fixed. rkyv is reached through this crate's re-export by `curios-archive-derive`'s expansion and nowhere else, so grepping the workspace for `rkyv` finds only prose and file names. How to annotate a type and what each entry point returns belong to the crate rustdoc; the macro's own decisions are `curios-archive-derive/README.md`'s.

## Design

### A type rkyv cannot archive is described by a stand-in, once

**Decision.** `Proxy<Value>` states one conversion — this value converts to an archivable one, and back — and `Via<P>` supplies the three rkyv adapter impls (`ArchiveWith`, `SerializeWith`, `DeserializeWith`) from it. A bignum is its little-endian bytes, a hash map its sorted entries, and an interned path or spelling the segments or text the process table already holds.

**Rationale.** A hand-written field adapter is that three-impl shape around one idea; written once here, a crate declaring a proxy names no rkyv trait. The stand-in is handed over borrowed rather than by value because the fixed prelude holds on the order of a hundred thousand qualifier occurrences over a couple of thousand distinct paths, and a by-value signature would clone once per occurrence on the way out.

### The entry points fix the error type

**Decision.** `to_bytes`, `from_bytes`, `access` and `deserialize` fix rkyv's error type to `rancor::Error`, hand back a `String`, and take rkyv's serializer, validator and deserializer bounds on themselves. `to_bytes` returns rkyv's aligned buffer behind a newtype rather than a `Vec<u8>`.

**Rationale.** Every call site instantiates that generality identically, so it buys nothing and costs each caller a `curios_archive::rkyv::` path. The aligned buffer stays because converting it to a `Vec` would copy every byte of an image that can run to megabytes; the workspace builds rkyv `unaligned`, so an archive is read wherever it lands.
